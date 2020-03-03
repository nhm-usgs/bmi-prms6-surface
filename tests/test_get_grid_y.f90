program test_get_grid_y

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: ny = 14
  integer, parameter, dimension(ny) :: expected_y = [ &
       2703661, 2718133, 2714044, 2715510, 2702018, 2710288, &
       2715710, 2722800, 2722858, 2727314, 2707931, 2709141, &
       2695859, 2691322 ]

  type (bmi_prms_surface) :: m
  integer :: grid_size
  double precision, allocatable :: grid_y(:)
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id, grid_size)
  allocate(grid_y(grid_size))
  status = m%get_grid_y(grid_id, grid_y)
  status = m%finalize()

  do i = 1, ny
     if (int(grid_y(i)) /= expected_y(i)) then
        write(*,*) grid_y
        stop BMI_FAILURE
     end if
  end do

  deallocate(grid_y)
end program test_get_grid_y
