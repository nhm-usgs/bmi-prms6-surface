program test_get_value

  use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: status, print_1darray, isReal4EqualReal4, &
      isReal8EqualReal8, print_i_1darray

  implicit none

  character (len=*), parameter :: config_file = "control.simple1"
  type (bmi_prms_surface) :: m
  integer :: retcode

  !test r32 hru_area
  retcode = test1()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

  ! test logical value
  retcode = test2()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

    !test iScalar value cascade_flag
  retcode = test3()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

      !test i32 value hru_type
  retcode = test4()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

      !test r64 value basin_area_inv
  retcode = test5()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if
  
  retcode = test6()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

contains

  ! Test getting r32 hru_area.
  function test1() result(code)
    character (len=*), parameter :: &
         var_name = "hru_area"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
         expected = (/149514.2,5643.941,36182.93,37884.59,27400.33, &
        33827.8,29424.89,5031.483,17224.58,10224.35,4578.616, &
        558.906,6747.711,54949.04 /)
    real :: tval(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    call print_1darray(tval, shape)
    do i = 1, shape(1)
       write(*,*) tval(i)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(1)
       if (isreal4equalreal4(expected(i), tval(i)).ne..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test1

  ! Test getting logical val .
  function test2() result(code)
    character (len=*), parameter :: &
         var_name = "gsflow_mode"
    integer, parameter :: size = 1
    logical, parameter :: expected(size) = (/ .FALSE. /)
    integer :: val(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, val)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    write(*,*) val
    write(*,*) expected

    code = BMI_SUCCESS
    do i = 1, size
       if (val(i).ne.expected(i)) then
          code = BMI_FAILURE
       end if
    end do
  end function test2

  ! Test getting iScalar cascade_flag.
  function test3() result(code)
    character (len=*), parameter :: &
         var_name = "cascade_flag"
    integer, parameter :: size = 1
    integer, parameter :: expected(size) = (/ 0 /)
    integer :: val(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, val)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 3"
    write(*,*) val
    write(*,*) expected

    code = BMI_SUCCESS
    do i = 1, size
       if (val(i).ne.expected(i)) then
          code = BMI_FAILURE
       end if
    end do
  end function test3

    ! Test getting i32 hru_type.
  function test4() result(code)
    character (len=*), parameter :: &
         var_name = "hru_type"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    integer, parameter, dimension(shape(1)) :: &
         expected = (/1,1,1,1,1,1,1,1,1,1,1,1,1,1 /)
    integer :: tval(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 4"
    call print_i_1darray(tval, shape)
    do i = 1, shape(1)
       write(*,*) tval(i)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(1)
       if (tval(i).ne.expected(i)) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test4

  ! Test getting r64 basin_area_inv.
  function test5() result(code)
    character (len=*), parameter :: &
         var_name = "basin_area_inv"
    double precision, parameter :: size = 1
    double precision, parameter :: expected(size) = (/ 2.3855335D-006 /)
    double precision :: val(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, val)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 5"
    write(*,*) val
    write(*,*) expected

    code = BMI_SUCCESS
    do i = 1, size
       if (isreal8equalreal8(expected(i), val(i)).ne..TRUE.) then
          code = BMI_FAILURE
       end if
    end do
  end function test5

    ! Test getting r32 hru_area.
  function test6() result(code)
    character (len=*), parameter :: &
         var_name = "infil"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
         expected = (/0.176085,0.2061947,0.1826115,0.2881884, &
        0.1756179,0.2354067,0.2036093,0.2170401,0.2155075,0.4194541, &
        0.180874,0.2514547,0.1786138,0.1666235/)
    real :: tval(size)
    integer :: i, code

    status = m%initialize(config_file)
    do i =1,79
        status = m%update()
    enddo
    !status = m%update_until(68.0d0)
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 6"
    call print_1darray(tval, shape)
    do i = 1, shape(1)
       write(*,*) expected(i)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(1)
       if (isreal4equalreal4(expected(i), tval(i)).ne..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test6
end program test_get_value
