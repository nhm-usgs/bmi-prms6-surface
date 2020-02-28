program test_get_grid_x

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssurface
  use fixtures, only: status

  implicit none
  character (len=*), parameter :: config_file = "./pipestem/control.simple1"
  integer, parameter :: grid_id_0 = 0
  integer, parameter :: grid_id_1 = 1
  integer, parameter :: grid_id_5 = 5
  ! integer, parameter :: nx = 1
  double precision, parameter, dimension(14) :: expected_x = (/ &
     -255338.765625000, -244755.015625000, -272161.750000000, -279352.531250000, &
     -239341.250000000, -234624.843750000, -262778.093750000, -260845.500000000, &
     -253183.812500000, -252164.890625000, -242704.531250000, -238578.578125000, &
     -226690.953125000, -238430.328125000 /)
  double precision, parameter, dimension(7) :: expected_x_seg =  ( [ &
    30113.00, 30114.00, 30115.00, 30116.00, 30117.00, 30118.00, 30119.00])
  double precision, parameter, dimension(168) :: expected_x_2d = ([ &
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0,  &
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0])

  type (bmi_prms_surface) :: m
  integer :: grid_size
  double precision, allocatable :: grid_x(:), grid_x_1(:), grid_x_5(:)
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id_0, grid_size)
  allocate(grid_x(grid_size))
  status = m%get_grid_x(grid_id_0, grid_x)
  status = m%finalize()
   
  do i = 1, grid_size
      if (grid_x(i) /= expected_x(i)) then
         write(*,*) grid_x
         stop BMI_FAILURE
      end if
  end do

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id_1, grid_size)
  allocate(grid_x_1(grid_size))
  status = m%get_grid_x(grid_id_1, grid_x_1)
  status = m%finalize()
  
  do i = 1, grid_size
      if (grid_x_1(i) /= expected_x_seg(i)) then
         write(*,*) grid_x
         stop BMI_FAILURE
      end if
  end do

  status = m%initialize(config_file)
  status = m%get_grid_size(grid_id_5, grid_size)
  allocate(grid_x_5(grid_size))
  status = m%get_grid_x(grid_id_5, grid_x_5)
  status = m%finalize()

   do i = 1, grid_size
      if (grid_x_5(i) /= expected_x_2d(i)) then
         write(*,*) grid_x
         stop BMI_FAILURE
      end if
   end do

  deallocate(grid_x)
  deallocate(grid_x_1)
  deallocate(grid_x_5)
end program test_get_grid_x
