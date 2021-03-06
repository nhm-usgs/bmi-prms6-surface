program test_get_grid_x

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: nx = 14
  integer, parameter, dimension(nx) :: expected_x = [ &
       -255338, -244755, -272161, -279352, -239341, -234624, -262778, &
       -260845, -253183, -252164, -242704, -238578, -226690, -238430 ]

  type (bmi_prms_surface) :: m
  integer :: grid_size
  double precision, allocatable :: grid_x(:)
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id, grid_size)
  allocate(grid_x(grid_size))
  status = m%get_grid_x(grid_id, grid_x)
  status = m%finalize()

  do i = 1, nx
     if (int(grid_x(i)) /= expected_x(i)) then
        write(*,*) grid_x
        stop BMI_FAILURE
     end if
  end do

  deallocate(grid_x)
end program test_get_grid_x
