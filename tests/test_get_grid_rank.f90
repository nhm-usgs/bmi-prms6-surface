program test_get_grid_rank

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: expected_rank = 1

  type (bmi_prms_surface) :: m
  integer :: grid_rank

  status = m%initialize(config_file)
  status = m%get_grid_rank(grid_id, grid_rank)
  status = m%finalize()

  if (grid_rank /= expected_rank) then
     write(*,*) grid_rank
     stop BMI_FAILURE
  end if
end program test_get_grid_rank
