program test_get_output_item_count

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: status

  implicit none

  integer, parameter :: expected = 87
  type (bmi_prms_surface) :: m
  integer :: count

  status = m%get_output_item_count(count)
  
  if (count /= expected) then
     stop BMI_FAILURE
  end if
end program test_get_output_item_count
