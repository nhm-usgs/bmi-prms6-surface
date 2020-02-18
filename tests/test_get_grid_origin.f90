program test_get_grid_origin

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 1
  double precision, dimension(rank), parameter :: &
       expected_origin = [-1.d0]

  type (bmi_prms_surface) :: m
  double precision, dimension(2) :: grid_origin
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_origin(grid_id, grid_origin)
  status = m%finalize()

  do i = 1, rank
     if (grid_origin(i).ne.expected_origin(i)) then
        write(*,*) grid_origin
        stop BMI_FAILURE
     end if
  end do
end program test_get_grid_origin
