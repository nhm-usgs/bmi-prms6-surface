program test_get_grid_face_count

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: expected = 0
  type (bmi_prms_surface) :: m
  integer :: count

  status = m%initialize(config_file)
  status = m%get_grid_face_count(grid_id, count)
  status = m%finalize()
  
  if (count /= expected) then
     write(*,*) count
     stop BMI_FAILURE
  end if
end program test_get_grid_face_count
