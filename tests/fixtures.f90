module fixtures
    use iso_fortran_env
  implicit none

  character (len=*), parameter :: config_file = ""

  integer :: status
  
    public :: isReal4EqualReal4
    public :: isReal4EqualReal8
    public :: isReal8EqualReal4
    public :: isReal8EqualReal8

    real(real32), parameter :: delta4 = 0.1
    real(real64), parameter :: delta8 = 0.000001

contains

  subroutine print_array(array, dims)
    integer :: dims(2)
    real, dimension(product(dims)) :: array
    integer :: i, j

    do j = 1, dims(1)
       do i = 1, dims(2)
          write (*,"(f6.1)", advance="no") array(j + dims(1)*(i-1))
       end do
       write (*,*)
    end do
  end subroutine print_array
    
  subroutine print_i_array(array, dims)
    integer :: dims(2)
    integer, dimension(product(dims)) :: array
    integer :: i, j

    do j = 1, dims(1)
       do i = 1, dims(2)
          write (*,"(i2)", advance="no") array(j + dims(1)*(i-1))
       end do
       write (*,*)
    end do
  end subroutine print_i_array

  subroutine print_1darray(array, dims)
    integer :: dims(1)
    real, dimension(dims(1)) :: array
    integer :: i, j

    do j = 1, dims(1)
       !do i = 1, dims(2)
          write (*,*) array(j)
       !end do
       !write (*,*)
    end do
    write (*,*)
    end subroutine print_1darray

    subroutine print_i_1darray(array, dims)
    integer :: dims(1)
    integer, dimension(dims(1)) :: array
    integer :: i, j

    do j = 1, dims(1)
       !do i = 1, dims(2)
          write (*,*) array(j)
       !end do
       !write (*,*)
    end do
    write (*,*)
    end subroutine print_i_1darray

        logical function isReal4EqualReal4(lhs, rhs) result(equal)
            real(real32), intent(in) :: lhs
            real(real32), intent(in) :: rhs
            equal = (abs(lhs - rhs) .le. delta4)
        end function isReal4EqualReal4

        logical function isReal4EqualReal8(lhs, rhs) result(equal)
            real(real32), intent(in) :: lhs
            real(real64), intent(in) :: rhs
            equal = (abs(lhs - real(rhs,4)) .le. delta4)
        end function isReal4EqualReal8

        logical function isReal8EqualReal4(lhs, rhs) result(equal)
            real(real64), intent(in) :: lhs
            real(real32), intent(in) :: rhs
            equal = isReal4EqualReal8(rhs, lhs)
        end function isReal8EqualReal4

        logical function isReal8EqualReal8(lhs, rhs) result(equal)
            real(real64), intent(in) :: lhs
            real(real64), intent(in) :: rhs
            equal = (dabs(lhs - rhs) .le. delta8)
        end function isReal8EqualReal8
        
        logical function isintEqualint(lhs, rhs) result(equal)
            integer, intent(in) :: lhs
            integer, intent(in) :: rhs
            equal = ((lhs - rhs) .eq. 0)
        end function isintEqualint

end module fixtures
