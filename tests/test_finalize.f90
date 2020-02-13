program test_finalize

  use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: status, config_file

  implicit none

  type (bmi_prms_surface) :: m
  integer :: check_status

  status = m%initialize(config_file)
  check_status = m%finalize()
  if (check_status /= BMI_SUCCESS) then
     stop BMI_FAILURE
  end if
end program test_finalize
