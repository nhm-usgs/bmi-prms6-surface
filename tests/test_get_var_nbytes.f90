program test_get_var_nbytes

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: var_name, config_file, status

  implicit none

  integer, parameter :: expected_nbytes = 56

  type (bmi_prms_surface) :: m
  integer :: var_nbytes

  status = m%initialize(config_file)
  status = m%get_var_nbytes(var_name, var_nbytes)
  status = m%finalize()

  if (var_nbytes /= expected_nbytes) then
     write(*,*) var_nbytes
     stop BMI_FAILURE
  end if
end program test_get_var_nbytes
