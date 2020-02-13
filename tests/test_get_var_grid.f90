program test_get_var_grid

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: var_name, config_file, status

  implicit none

  integer, parameter :: expected_id = 0

  type (bmi_prms_surface) :: m
  integer :: grid_id

  status = m%initialize(config_file)
  status = m%get_var_grid(var_name, grid_id)
  status = m%finalize()

  if (grid_id /= expected_id) then
     write(*,*) grid_id
     stop BMI_FAILURE
  end if
end program test_get_var_grid
