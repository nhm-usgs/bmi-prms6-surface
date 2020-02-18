program test_get_grid_face_edges

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 1
  integer, dimension(rank), parameter :: expected = [-1]

  type (bmi_prms_surface) :: m
  integer, dimension(rank) :: face_edges
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_face_edges(grid_id, face_edges)
  status = m%finalize()

  do i = 1, rank
     if (face_edges(i) /= expected(i)) then
        write(*,*) face_edges
        stop BMI_FAILURE
     end if
  end do
end program test_get_grid_face_edges
