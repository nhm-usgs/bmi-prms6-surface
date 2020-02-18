program test_get_grid_size

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: expected_size = 14

  type (bmi_prms_surface) :: m
  integer :: grid_size

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id, grid_size)
  status = m%finalize()

  if (grid_size /= expected_size) then
     write(*,*) grid_size
     stop BMI_FAILURE
  end if
end program test_get_grid_size
