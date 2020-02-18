program test_get_grid_shape

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 1
  integer, dimension(rank), parameter :: expected_shape = [-1]

  type (bmi_prms_surface) :: m
  integer, dimension(rank) :: grid_shape
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_shape(grid_id, grid_shape)
  status = m%finalize()

  do i = 1, rank
     if (grid_shape(i) /= expected_shape(i)) then
        write(*,*) grid_shape
        stop BMI_FAILURE
     end if
  end do
end program test_get_grid_shape
