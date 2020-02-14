program test_get_value_at_indices

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmssurface
    use fixtures, only: config_file, status, print_1darray, print_i_1darray, &
        isreal4equalreal4

    implicit none

    type (bmi_prms_surface) :: m
    integer :: retcode


    retcode = test1()
    !if (retcode.ne.BMI_SUCCESS) then
    !    stop BMI_FAILURE
    !end if

    retcode = test2()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if
    
    retcode = test6()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if
    contains

! Test getting i32 hru_type.
    function test1() result(code)
    character (len=*), parameter :: &
        var_name = "hru_type"
    integer, parameter :: rank = 1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(7) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)
    integer, parameter, dimension(7) :: &
        expected = (/1,1,1,1,1,1,1 /)
    integer, allocatable :: tval(:)
    integer :: i, code

    allocate(tval(size))
    code = m%initialize(config_file)
    code = m%get_value_at_indices(var_name, tval, indices)
    code = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    call print_i_1darray(tval, shape)
    do i = 1, size
        write(*,*) tval(i)
    end do

    code = BMI_SUCCESS
    do i = 1, size
        if (tval(i).ne.expected(i)) then
            code = BMI_FAILURE
            exit
        end if
    end do

    end function test1

! Test getting r32 hru_area.
    function test2() result(code)
    character (len=*), parameter :: &
        var_name = "infil"
    integer, parameter :: rank = 1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(7) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)
    real, parameter, dimension(7) :: &
        expected = (/0.2061947,0.2881884, &
        0.2354067,0.2170401,0.4194541, &
        0.2514547,0.1666235/)
    real, allocatable :: tval(:)
    integer :: i, code
    
    allocate(tval(size))
    
    code = m%initialize(config_file)
    do i =1,79
        status = m%update()
    enddo
    !status = m%update_until(68.0d0)
    code = m%get_value_at_indices(var_name, tval, indices)
    code = m%finalize()


    ! Visual inspection.
    write(*,*) "Test 2"
    call print_1darray(tval, shape)
    do i = 1, size
        write(*,*) expected(i)
    end do

    code = BMI_SUCCESS
    do i = 1, size
        if (isreal4equalreal4(expected(i), tval(i)).neqv..TRUE.) then
            code = BMI_FAILURE
            exit
        end if
    end do
    end function test2
  function test6() result(code)
    character (len=*), parameter :: var_name = "dday_intcp"
    integer, parameter :: rank = 2
    !integer, parameter :: size = ‭168‬
    integer, parameter, dimension(rank) :: shape = (/ 14, 12 /)
    integer, parameter, dimension(shape(2)) :: &
       indices = (/ 1, 15, 29, 43, 57, 71, 85, 99, 113, 127, 141, 155 /)
    real, parameter, dimension(shape(2)) :: &
         expected = (/ -10.0, -11.0, -13.0, -16.0, -20.0, -25.0, -30.0, &
                       -25.0, -20.0, -16.0, -13.0, -11.0 /)
    real :: tval(shape(2))
    integer :: i, code

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_at_indices(var_name, tval, indices)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 6 get values"
    call print_1darray(tval, shape(2))
    write(*,*) "Test 6 expected values"
    call print_1darray(expected, shape(2))

    code = BMI_SUCCESS
    do i = 1, shape(2)
       if (isreal4equalreal4(expected(i), tval(i)).neqv..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test6
  
  !function run_test() result(code)
  !  type (bmi_heat) :: m
  !  real, allocatable :: tval(:)
  !  integer :: i
  !  integer :: code
  !
  !  allocate(tval(size(indices)))
  !
  !  status = m%initialize(config_file)
  !  status = m%get_value_at_indices(var_name, tval, indices)
  !  status = m%finalize()
  !
  !  ! Visual inspection.
  !  do i = 1, shape(2)
  !     write(*,*) indices(i), tval(i), expected(i)
  !  end do
  !
  !  code = BMI_SUCCESS
  !  do i = 1, shape(2)
  !     if (tval(i).ne.expected(i)) then
  !        code = BMI_FAILURE
  !        exit
  !     end if
  !  end do
  !
  !  deallocate(tval)
  !end function run_test

end program test_get_value_at_indices
