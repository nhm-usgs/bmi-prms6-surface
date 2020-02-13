program test_get_var_itemsize

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: var_name, config_file, status

  implicit none

  integer, parameter :: expected_size = 4

  type (bmi_prms_surface) :: m
  integer :: item_size

  status = m%initialize(config_file)
  status = m%get_var_itemsize(var_name, item_size)
  status = m%finalize()

  if (item_size /= expected_size) then
     write(*,*) item_size
     stop BMI_FAILURE
  end if
end program test_get_var_itemsize
