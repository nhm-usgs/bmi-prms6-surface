program test_get_grid_x

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: status

  implicit none
  character (len=*), parameter :: config_file = "./pipestem/control.simple1"
  integer, parameter :: grid_id_0 = 0
  integer, parameter :: grid_id_1 = 1
  ! integer, parameter :: nx = 1
  ! double precision, parameter, dimension(nx) :: expected_x = (/ 0.0 /)

  type (bmi_prms_surface) :: m
  integer :: grid_size
  double precision, allocatable :: grid_x(:), grid_x_1(:)
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id_0, grid_size)
  allocate(grid_x(grid_size))
  status = m%get_grid_x(grid_id_0, grid_x)
  status = m%finalize()

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id_1, grid_size)
  allocate(grid_x_1(grid_size))
  status = m%get_grid_x(grid_id_1, grid_x_1)
  status = m%finalize()


  ! do i = 1, nx
  !    if (grid_x(i) /= expected_x(i)) then
  !       write(*,*) grid_x
  !       stop BMI_FAILURE
  !    end if
  ! end do

  deallocate(grid_x)
end program test_get_grid_x
