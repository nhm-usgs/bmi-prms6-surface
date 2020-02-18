program test_get_grid_y

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  ! integer, parameter :: ny = 1
  ! double precision, parameter, dimension(ny) :: expected_y = (/ 0.0 /)

  type (bmi_prms_surface) :: m
  integer :: grid_size
  double precision, allocatable :: grid_y(:)
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id, grid_size)
  allocate(grid_y(grid_size))
  status = m%get_grid_y(grid_id, grid_y)
  status = m%finalize()

  ! do i = 1, ny
  !    if (grid_y(i) /= expected_y(i)) then
  !       write(*,*) grid_y
  !       stop BMI_FAILURE
  !    end if
  ! end do

  deallocate(grid_y)
end program test_get_grid_y
