    program test_get_value_ptr

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmssurface
    use fixtures, only: status, print_1darray, print_i_1darray, &
        isreal4equalreal4

    implicit none

    character (len=*), parameter :: config_file = "control.simple1"
    type (bmi_prms_surface) :: m
    integer :: retcode

    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    retcode = test2()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    contains

! Test getting i32 hru_type.
    function test1() result(code)
    character (len=*), parameter :: &
        var_name = "hru_type"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    integer, parameter, dimension(shape(1)) :: &
        expected = (/1,1,1,1,1,1,1,1,1,1,1,1,1,1 /)
    integer, pointer :: tptr(:)
    integer :: i, code, status

    status = m%initialize(config_file)
    status = m%get_value_ptr(var_name, tptr)


    ! Visual inspection.
    write(*,*) "Test 1"
    call print_i_1darray(tptr, shape)
    do i = 1, shape(1)
        write(*,*) tptr(i)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(1)
        if (tptr(i).ne.expected(i)) then
            code = BMI_FAILURE
            exit
        end if
    end do

    code = m%finalize()

    end function test1

! Test getting r32 hru_area.
    function test2() result(code)
    character (len=*), parameter :: &
        var_name = "infil"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
        expected = (/0.176085,0.2061947,0.1826115,0.2881884, &
        0.1756179,0.2354067,0.2036093,0.2170401,0.2155075,0.4194541, &
        0.180874,0.2514547,0.1786138,0.1666235/)
    real, pointer :: tval(:)
    integer :: i, code

    status = m%initialize(config_file)
    do i =1,79
        status = m%update()
    enddo
    !status = m%update_until(68.0d0)
    status = m%get_value_ptr(var_name, tval)


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
    status = m%finalize()
    end function test2

  !function run_test() result(code)
  !  type (bmi_heat) :: m
  !  real, pointer :: tref(:)
  !  integer :: i, j
  !  integer :: code
  !
  !  status = m%initialize(config_file)
  !  status = m%get_value_ptr(var_name, tref)
  !
  !  ! Visual inspection.
  !  call print_array(tref, shape)
  !  do i = 1, shape(2)
  !     write(*,*) tref((i-1)*shape(1)+1)
  !  end do
  !
  !  code = BMI_SUCCESS
  !  do i = 1, shape(2)
  !     if (tref((i-1)*shape(1)+1).ne.expected(i)) then
  !        code = BMI_FAILURE
  !        exit
  !     end if
  !  end do
  !
  !  status = m%finalize()
  !end function run_test

end program test_get_value_ptr
