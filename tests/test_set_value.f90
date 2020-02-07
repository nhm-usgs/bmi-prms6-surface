    program test_set_value

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmssurface
    use fixtures, only: status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint

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

    real :: tval(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%set_value(var_name, setv)
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
       if (isreal4equalreal4(setv(i), tval(i)).ne..TRUE.) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test1

  function test2() result(code)
    character (len=*), parameter :: &
         var_name = "dprst_vol_open"
    double precision, parameter :: size = 14
    double precision, parameter :: expected(size) = (/ &
        0.277953696836656, 2.656665943150712E-002, 0.153268867485187, &
        0.141798932546227, 0.128282493483124, 0.214119862005884, &
        0.109451652923970, 3.359093199785438E-002, 3.231387813052020E-002, &
        7.310565607726262E-002, 1.943497908398673E-002, 0.000000000000000E+000, &
        3.852883300563383E-002, 0.152698339579570 /)
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
    write(*,*) "Test 5"
    write(*,*) val
    write(*,*) setv

    code = BMI_SUCCESS
    do i = 1, size
       if (isreal8equalreal8(setv(i), val(i)).ne..TRUE.) then
          code = BMI_FAILURE
       end if
    end do
  end function test2

  
end program test_set_value
