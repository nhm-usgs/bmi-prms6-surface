    program test_set_value

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmssurface
    use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint

    implicit none

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

    retcode = test3()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    retcode = test4()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    contains

  ! Test getting r32 hru_area.
  function test1() result(code)
    character (len=*), parameter :: &
         var_name = "hru_ppt"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
         setv = (/0.25,0.25,0.25,0.25,0.25, &
        0.25,0.25,0.25,0.25,0.25,0.25, &
        0.25,0.25,0.25 /)

    real :: tval(size), t2val(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%set_value(var_name, setv)
    status = m%update() !run update to check that values not over-ridden by read in prms
    status = m%get_value(var_name, tval)
    status = m%update()
    status = m%get_value(var_name, t2val)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    call print_1darray(tval, shape)
    do i = 1, shape(1)
       write(*,*) tval(i)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(1)
       if (isreal4equalreal4(setv(i), tval(i)).neqv..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test1

  function test2() result(code)
    character (len=*), parameter :: &
         var_name = "dprst_vol_open"
    integer, parameter :: size = 14
    double precision, parameter :: expected(size) = (/ &
        41558.0385172600, 149.940656219223, 5545.71725420578, 5371.99384589330, &
        3514.98316578602, 7243.20487164950, 3220.60291601327, 169.012200283425, &
        556.593107720004, 747.458071725632, 88.9853008034370, 0.00000000000000, &
        259.981427881226, 8390.62762281456 /)
    double precision :: setv(size) 
    double precision :: val(size)
    integer :: i, code
    
    setv = 1.5*expected

    status = m%initialize(config_file)
    status = m%get_value(var_name, val)
    status = m%set_value(var_name, setv)
    status = m%get_value(var_name, val)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test "
    write(*,*) val
    write(*,*) setv

    code = BMI_SUCCESS
    do i = 1, size
       if (isreal8equalreal8(setv(i), val(i)).neqv..TRUE.) then
          code = BMI_FAILURE
       end if
    end do
  end function test2

  function test3() result(code)
    character (len=*), parameter :: &
         var_name = "tmax"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
         setv = (/0.25,0.25,0.25,0.25,0.25, &
        0.25,0.25,0.25,0.25,0.25,0.25, &
        0.25,0.25,0.25 /)
    real, parameter, dimension(shape(1)) :: &
        getv = (/-17.63889, -17.63889, -17.63889, -17.63889, -17.63889, &
            -17.63889, -17.63889, -17.63889, -17.63889, -17.63889, &
            -17.63889, -17.63889, -17.63889, -17.63889 /)

    real :: tval(size), t2val(size)
    integer :: i, code

    !a little weird because input is in F but output is C 
    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%set_value(var_name, setv)
    status = m%update() !run update to check that values not over-ridden by read in prms
    status = m%get_value(var_name, tval)
    status = m%update()
    status = m%get_value(var_name, t2val)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 3"
    call print_1darray(tval, shape)
    do i = 1, shape(1)
       write(*,*) getv(i)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(1)
       if (isreal4equalreal4(tval(i), getv(i)).neqv..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test3

  function test4() result(code)
    character (len=*), parameter :: &
         var_name = "tmin"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
         setv = (/0.25,0.25,0.25,0.25,0.25, &
        0.25,0.25,0.25,0.25,0.25,0.25, &
        0.25,0.25,0.25 /)
    real, parameter, dimension(shape(1)) :: &
        getv = (/-17.63889, -17.63889, -17.63889, -17.63889, -17.63889, &
            -17.63889, -17.63889, -17.63889, -17.63889, -17.63889, &
            -17.63889, -17.63889, -17.63889, -17.63889 /)
    real :: tval(size), t2val(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%set_value(var_name, setv)
    status = m%update() !run update to check that values not over-ridden by read in prms
    status = m%get_value(var_name, tval)
    status = m%update()
    status = m%get_value(var_name, t2val)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 4"
    call print_1darray(tval, shape)
    do i = 1, shape(1)
       write(*,*) getv(i)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(1)
       if (isreal4equalreal4(tval(i), getv(i)).neqv..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test4
    end program test_set_value
