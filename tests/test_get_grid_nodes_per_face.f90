program test_get_grid_nodes_per_face

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 1
  integer, dimension(rank), parameter :: expected = [-1]

  type (bmi_prms_surface) :: m
  integer, dimension(rank) :: nodes_per_face
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_nodes_per_face(grid_id, nodes_per_face)
  status = m%finalize()

  do i = 1, rank
     if (nodes_per_face(i) /= expected(i)) then
        write(*,*) nodes_per_face
        stop BMI_FAILURE
     end if
  end do
end program test_get_grid_nodes_per_face
