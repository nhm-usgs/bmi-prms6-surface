program test_get_value

  use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: status, print_1darray, isReal4EqualReal4, &
      isReal8EqualReal8, print_i_1darray, print_array, isintEqualint

  implicit none

  ! this config resides in bmi-test-projects repo in 
  ! bmi-test-projects\bmi-prms6-surface\pipestem
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

      !test r64 value dprst_stor_hru
  ! Delete Basin variables are going away in prms6
  !retcode = test5()
  !if (retcode.ne.BMI_SUCCESS) then
  !   stop BMI_FAILURE
  !end if
  
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
  ! basin variable are going away in prms6 so this test should be changed
  ! commented out above.
  function test5() result(code)
    character (len=*), parameter :: &
         var_name = "dprst_stor_hru"
    integer, parameter :: size = 14
    double precision, parameter :: expected(size) = (/ &
        0.277953696836656, 2.656665943150712E-002, 0.153268867485187, &
        0.141798932546227, 0.128282493483124, 0.214119862005884, &
        0.109451652923970, 3.359093199785438E-002, 3.231387813052020E-002, &
        7.310565607726262E-002, 1.943497908398673E-002, 0.000000000000000E+000, &
        3.852883300563383E-002, 0.152698339579570 /)
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

  function test6() result(code)
    character (len=*), parameter :: var_name = "dday_intcp"
    integer, parameter :: rank = 2
    !integer, parameter :: size = ‭168‬
    integer, parameter, dimension(rank) :: shape = (/ 14, 12 /)
    real, parameter, dimension(shape(1)*shape(2)) :: &
         expected = (/ &
        -10, -10, -10, -10, -10, -10, -10, -10, -10, -10, -10, -10, -10, -10, &
        -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, &
        -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, &
        -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, &
        -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, &
        -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, &
        -30, -30, -30, -30, -30, -30, -30, -30, -30, -30, -30, -30, -30, -30, &
        -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, -25, &
        -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, -20, &
        -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, -16, &
        -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, -13, &
        -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11, -11 &
            /)
    real :: tval(shape(1)*shape(2))
    integer :: i, code

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 6 get values"
    call print_array(tval, shape)
    write(*,*) "Test 6 expected values"
    call print_array(expected, shape)

    code = BMI_SUCCESS
    do i = 1, shape(1)
       if (isreal4equalreal4(expected(i), tval(i)).ne..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test6
  
    ! Test getting nhru:nmonth param.
  function test7() result(code)
    character (len=*), parameter :: &
         var_name = "dprst_stor_hru"
    integer, parameter :: size = 14
    double precision, parameter :: expected(size) = (/ &
        0.277953696836656, 2.656665943150712E-002, 0.153268867485187, &
        0.141798932546227, 0.128282493483124, 0.214119862005884, &
        0.109451652923970, 3.359093199785438E-002, 3.231387813052020E-002, &
        7.310565607726262E-002, 1.943497908398673E-002, 0.000000000000000E+000, &
        3.852883300563383E-002, 0.152698339579570 /)
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
  end function test7
end program test_get_value
