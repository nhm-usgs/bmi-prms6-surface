program test_get_input_var_names

  use bmif_2_0, only: BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmiprmssurface
  use fixtures, only: status

  implicit none

  type (bmi_prms_surface) :: m
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: i
  
  status = m%get_input_var_names(names)

  ! Visualize
  do i = 1, size(names)
     write(*,*) trim(names(i))
  end do
  
  if (status == BMI_FAILURE) then
     stop BMI_FAILURE
  end if
end program test_get_input_var_names
