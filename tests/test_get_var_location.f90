program test_get_var_location

  use bmif_2_0, only: BMI_FAILURE, BMI_MAX_UNITS_NAME
  use bmiprmssurface
  use fixtures, only: var_name, config_file, status

  implicit none

  character (len=*), parameter :: expected_location = "node"

  type (bmi_prms_surface) :: m
  character (len=BMI_MAX_UNITS_NAME) :: var_location

  status = m%initialize(config_file)
  status = m%get_var_location(var_name, var_location)
  status = m%finalize()

  if (var_location /= expected_location) then
     write(*,*) var_location
     stop BMI_FAILURE
  end if
end program test_get_var_location
