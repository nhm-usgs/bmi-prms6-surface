program test_get_var_units

  use bmif_2_0, only: BMI_FAILURE, BMI_MAX_UNITS_NAME
  use bmiprmssurface
  use fixtures, only: var_name, config_file, status

  implicit none

  character (len=*), parameter :: expected_units = "in"

  type (bmi_prms_surface) :: m
  character (len=BMI_MAX_UNITS_NAME) :: var_units

  status = m%initialize(config_file)
  status = m%get_var_units(var_name, var_units)
  status = m%finalize()

  if (var_units /= expected_units) then
     write(*,*) var_units
     stop BMI_FAILURE
  end if
end program test_get_var_units
