program test_get_var_type

  use bmif_2_0, only: BMI_FAILURE, BMI_MAX_TYPE_NAME
  use bmiprmssurface
  use fixtures, only: var_name, config_file, status

  implicit none

  character (len=*), parameter :: expected_type = "real"

  type (bmi_prms_surface) :: m
  character (len=BMI_MAX_TYPE_NAME) :: var_type

  status = m%initialize(config_file)
  status = m%get_var_type(var_name, var_type)
  status = m%finalize()

  if (var_type /= expected_type) then
     write(*,*) var_type
     stop BMI_FAILURE
  end if
end program test_get_var_type
