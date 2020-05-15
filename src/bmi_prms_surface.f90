    module bmiprmssurface

    use m_prms_surface
    use bmif_2_0
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
    implicit none

    type, extends (bmi) :: bmi_prms_surface
        private
        type (prms_surface_model) :: model
    contains
    procedure :: get_component_name => prms_component_name
    procedure :: get_input_item_count => prms_input_item_count
    procedure :: get_output_item_count => prms_output_item_count
    procedure :: get_input_var_names => prms_input_var_names
    procedure :: get_output_var_names => prms_output_var_names
    procedure :: initialize => prms_initialize
    procedure :: finalize => prms_finalize
    procedure :: get_start_time => prms_start_time
    procedure :: get_end_time => prms_end_time
    procedure :: get_current_time => prms_current_time
    procedure :: get_time_step => prms_time_step
    procedure :: get_time_units => prms_time_units
    procedure :: update => prms_update
    procedure :: update_until => prms_update_until
    procedure :: get_var_grid => prms_var_grid
    procedure :: get_grid_type => prms_grid_type
    procedure :: get_grid_rank => prms_grid_rank
    procedure :: get_grid_shape => prms_grid_shape
    procedure :: get_grid_size => prms_grid_size
    procedure :: get_grid_spacing => prms_grid_spacing
    procedure :: get_grid_origin => prms_grid_origin
    procedure :: get_grid_x => prms_grid_x
    procedure :: get_grid_y => prms_grid_y
    procedure :: get_grid_z => prms_grid_z
    procedure :: get_grid_node_count => prms_grid_node_count
    procedure :: get_grid_edge_count => prms_grid_edge_count
    procedure :: get_grid_face_count => prms_grid_face_count
    procedure :: get_grid_edge_nodes => prms_grid_edge_nodes
    procedure :: get_grid_face_edges => prms_grid_face_edges
    procedure :: get_grid_face_nodes => prms_grid_face_nodes
    procedure :: get_grid_nodes_per_face => prms_grid_nodes_per_face
    procedure :: get_var_type => prms_var_type
    procedure :: get_var_units => prms_var_units
    procedure :: get_var_itemsize => prms_var_itemsize
    procedure :: get_var_nbytes => prms_var_nbytes
    procedure :: get_var_location => prms_var_location
    procedure :: get_value_int => prms_get_int
    procedure :: get_value_float => prms_get_float
    procedure :: get_value_double => prms_get_double
    generic :: get_value => &
         get_value_int, &
         get_value_float, &
         get_value_double
    procedure :: get_value_ptr_int => prms_get_ptr_int
    procedure :: get_value_ptr_float => prms_get_ptr_float
    procedure :: get_value_ptr_double => prms_get_ptr_double
    generic :: get_value_ptr => &
         get_value_ptr_int, &
         get_value_ptr_float, &
         get_value_ptr_double
    procedure :: get_value_at_indices_int => prms_get_at_indices_int
    procedure :: get_value_at_indices_float => prms_get_at_indices_float
    procedure :: get_value_at_indices_double => prms_get_at_indices_double
    generic :: get_value_at_indices => &
         get_value_at_indices_int, &
         get_value_at_indices_float, &
         get_value_at_indices_double
    procedure :: set_value_int => prms_set_int
    procedure :: set_value_float => prms_set_float
    procedure :: set_value_double => prms_set_double
    generic :: set_value => &
         set_value_int, &
         set_value_float, &
         set_value_double
    procedure :: set_value_at_indices_int => prms_set_at_indices_int
    procedure :: set_value_at_indices_float => prms_set_at_indices_float
    procedure :: set_value_at_indices_double => prms_set_at_indices_double
    generic :: set_value_at_indices => &
         set_value_at_indices_int, &
         set_value_at_indices_float, &
         set_value_at_indices_double
    !procedure :: print_model_info
    end type bmi_prms_surface

    private
    public :: bmi_prms_surface

    character (len=BMI_MAX_COMPONENT_NAME), target :: &
        component_name = "prms6-surface-BMI"

    ! Exchange items
    integer, parameter :: input_item_count = 51
    integer, parameter :: output_item_count = 83
    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(input_item_count) :: input_items =(/ &
        !climate forcings
        'tmax                ', &
        'tmin                ', &
        !potentially used for gsflow implementation?
        'hortonian_lakes     ', &
        !climateflow
        'soil_moist_max      ', &
        'soil_rechr_init_frac', &
        'soil_moist_init_frac', &
        'soil_rechr_max_frac ', &

        !intcp
        'covden_sum          ', &
        'snow_intcp          ', &
        'covden_win          ', &
        'wrain_intcp         ', &
        'srain_intcp         ', &
        !potet
        ! 'epan_coef           ', & !not yet implemented in prms6
        !potet_jh
        'jh_coef_hru         ', &
        'jh_coef             ', &
        !precipitation
        'tmax_allrain_offset ', &
        'tmax_allsnow        ', &
        'hru_ppt             ', &
        !precipitation_hru
        'adjmix_rain         ', &
        'rain_cbh_adj        ', &
        'snow_cbh_adj        ', &
        !runoff
        'va_open_exp         ', &
        'va_clos_exp         ', &
        'smidx_coef          ', &
        'smidx_exp           ', &
        'snowinfil_max       ', &
        'carea_max           ', &
        'imperv_stor_max     ', &
        'dprst_flow_coef     ', &
        'dprst_seep_rate_clos', &
        'dprst_depth_avg     ', &
        'dprst_area_max      ', &
        'dprst_frac          ', &
        'dprst_vol_clos      ', &
        'dprst_vol_open      ', &
        'hru_percent_imperv  ', &
        !snowcomp
        'snarea_curve        ', &
        'rad_trncf           ', &
        'cecn_coef           ', &
        'snarea_thresh       ', &
        !solar_radiation
        'radmax              ', &
        !solar_radiation_degday
        'dday_slope          ', &
        'tmax_index          ', &
        'dday_intcp          ', &
        !temperature
        'tmax_cbh_adj        ', &
        'tmin_cbh_adj        ', &
        !transp_tindex
        'transp_tmax         ', &
        !for soil2surfac
        'infil               ', &
        'sroff               ', &
        'soil_rechr          ', &
        'soil_moist          ', &
        'strm_seg_in         '/) ! this one not yet implemented in prms6 gwflow and soilzone
    
    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(output_item_count) :: &
        output_items  =(/ &
        !misc

        !control
        'gsflow_mode         ', & !logical by 1
        'dprst_flag          ', & !iscalar by 1
        'cascade_flag        ', & !iscalar by 1
        'cascadegw_flag      ', & !iscalar by 1
        'print_debug         ', & !iscalar by 1

        !basin
        'hru_type            ', & !i32by nhru
        'hru_area            ', & !r32 by nhru
        'cov_type            ', & !i32 by nhru
        'hru_route_order     ', & !i32 by count(active_mask)
        'active_hrus         ', & !i32 by 1
        'nlake               ', & !i32 by 1
        'nhru                ', & !i32 by 1
        'nhm_seg             ', & !i32 by nsegment
        'nhm_id              ', & !i32 by nhru
        'hru_lat             ', & !r32 by nhru
        'hru_lon             ', & !r32 by nhru
        'hru_area_dble       ', & ! r64 by nhru
        'nmonths             ', & !i32 by 1
        'active_mask         ', & !logical by nhru

        !climateflow
        'soil_moist_max      ', & !r32 by nhru
        'soil_moist          ', & !r32 by nhru
        'soil_rechr          ', & !r32 by nhru
        'soil_rechr_max      ', & !r32 by nhru
        'pkwater_equiv       ', & !r64 by nhru
    
        !potet
        'potet               ', & !r32 by nhru

        !transpiration
        'transp_on           ', & !logical by nhru

        !runoff
        ! 'soil_rechr_chg      ', & !r32 by nhru !conditional will leave out for now
        ! 'soil_moist_chg      ', & !r32 by nhru !conditional will leave out for now
        'hru_impervevap      ', & !r32 by nhru
        'hru_frac_perv       ', & !r32 by nhru
        'hru_area_perv       ', & !r32 by nhru
        ! 'hru_hortn_cascflow  ', & !r64 by hru !conditional will leave out for now
        'hru_percent_imperv  ', & !r32 by hru
        'hru_sroffi          ', & !r32 by hru
        'hru_sroffp          ', & !r32 by hru
        'hru_impervstor      ', & !r32 by nhru
        'infil               ', & !r32 by nhru
        'sroff               ', & !r32 by nhru
        'dprst_evap_hru      ', &  !r32 by nhru
        'dprst_seep_hru      ', & !r64 by nhru
        'dprst_area_clos     ', & !r32 by hru
        'dprst_area_max      ', & !r32 by hru
        'dprst_frac          ', & !r32 by hru
        'dprst_in            ', & !r64 by hru
        'dprst_insroff_hru   ', & !r32 by hru
        'dprst_sroff_hru     ', & !r64 by hru
        !'dprst_stor_ante     ', & !r64 by hru
        'dprst_vol_clos      ', & !r64 by hru
        'dprst_vol_open      ', & !r64 by hru
        'dprst_stor_hru      ', & !r64 by nhru
        !'imperv_stor_ante    ', & !r32 by hru
        'sro_to_dprst_perv   ', & !r32 by hru
        ! 'upslope_hortonian   ', & !r64 by hru !conditional not yet implemented
        'hortonian_flow      ', & !r32 by hru
        'use_sroff_transfer  ', & !logical by 1
        'hortonian_lakes     ', & !r64 by nhru
        'strm_seg_in         ', & !r64 by nsegment !conditional not yet implemented
        'srunoff_updated_soil', & !logical by 1
            
        !snow
        'snowcov_area        ', & !r32 by hru
        'snow_evap           ', & !r32 by hru
        'newsnow             ', & !i32 by hru
        'pkwater_ante        ', & !r64 by hru
        'pptmix              ', & !i32 by hru
        'pptmix_nopack       ', & !logical by hru
        'snowmelt            ', & !r32 by hru

        !intcp
        'hru_intcpevap       ', & !r32 by hru
        'hru_intcpstor       ', & !r32 by hru
        'use_transfer_intcp  ', & ! logical by 1
        'canopy_covden       ', & ! r32 by nhru
        !'gain_inches         ', & ! r32 by nhru
        'intcp_changeover    ', & !r32 by hru
        'intcp_evap          ', & !r32 by hru
        'intcp_stor          ', &  !r32 by hru
        !'intcp_stor_ante     ', & !r32 by hru
        !'net_apply           ', & !r32 by hru
        'net_ppt             ', & !r32 by hru
        'net_rain            ', & !r32 by hru
        'net_snow            ', & !r32 by hru
        'snow_intcp          ', & !r32 by hru
        'srain_intcp         ', &!r32 by hru

        'wrain_intcp         ', & !r32 by hru
        'last_intcp_stor     ', & !r64 by 1
    
        !precip
        'hru_snow            ', & !r32 by hru
        'hru_rain            ', & !r32 by hru
        'hru_ppt             ', & !r32 by hru

        'tmax                ', &
        'tmin                ', &

        !prms_time
        'nowtime             ', & !i32(6)
            
        !solrad

        'swrad               ', & ! r32 by nhru
        'dday_slope          ', & ! r32 by nhru:nmonths
        'dday_intcp          ', & ! r32 by nhru:nmonths
        'tmax_index          ' &! r32 by nhru:nmonths
        /)


    contains

    ! Get the name of the model.
    function prms_component_name(this, name) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
    end function prms_component_name

    ! Count the input variables.
    function prms_input_item_count(this, count) result (bmi_status)
        class (bmi_prms_surface), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = input_item_count
        bmi_status = BMI_SUCCESS
     end function prms_input_item_count

    ! Count the output variables.
    function prms_output_item_count(this, count) result (bmi_status)
        class (bmi_prms_surface), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = output_item_count
        bmi_status = BMI_SUCCESS
    end function prms_output_item_count

    ! List input variables.
    function prms_input_var_names(this, names) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    names => input_items
    bmi_status = BMI_SUCCESS
    end function prms_input_var_names

    ! List output variables.
    function prms_output_var_names(this, names) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    names => output_items
    bmi_status = BMI_SUCCESS
    end function prms_output_var_names

    ! BMI initializer.
    function prms_initialize(this, config_file) result (bmi_status)
    class (bmi_prms_surface), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
        call initialize_from_file(this%model, config_file)
    else
        !call initialize_from_defaults(this%model)
    end if
    bmi_status = BMI_SUCCESS
    end function prms_initialize

    ! BMI finalizer.
    function prms_finalize(this) result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_finalize

    ! Model start time.
    function prms_start_time(this, time) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    !time = this%model%model_simulation%model_time%Timestep
    bmi_status = BMI_SUCCESS
    end function prms_start_time

    ! Model end time.
    function prms_end_time(this, time) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Number_timesteps)
    bmi_status = BMI_SUCCESS
    end function prms_end_time

    ! Model current time.
    function prms_current_time(this, time) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Timestep)
    bmi_status = BMI_SUCCESS
    end function prms_current_time

    ! Model time step.
    function prms_time_step(this, time_step) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%model_simulation%model_time%Timestep_seconds)
    bmi_status = BMI_SUCCESS
    end function prms_time_step

    ! Model time units.
    function prms_time_units(this, units) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
    end function prms_time_units

    ! Advance model by one time step.
    function prms_update(this) result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_update

    ! Advance the model until the given time.
    function prms_update_until(this, time) result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    double precision, intent(in) :: time
    double precision :: current_time, end_time, dt
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s
    s = this%get_current_time(current_time)
    s = this%get_end_time(end_time)
    s = this%get_time_step(dt)
    if (time > current_time) then
        n_steps_real = (time - current_time)
        n_steps = floor(n_steps_real)
        do i = 1, n_steps
            s = this%update()
        end do
        !s = this%update_frac(n_steps_real - dble(n_steps))
    end if
    bmi_status = BMI_SUCCESS
    end function prms_update_until

    ! Get the grid id for a particular variable.
    function prms_var_grid(this, name, grid) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('tmax', 'tmin', 'hru_ppt', 'hru_rain', 'hru_snow', 'hru_x', &
        'hru_y', 'hru_elev', 'nhm_id', 'hru_lat', 'hru_lon', &
        'hru_actet', 'hortonian_lakes', &
        'cov_type', 'hru_area', 'hru_type', &
        'dprst_evap_hru', 'dprst_seep_hru', 'infil', &
        'sroff', 'potet', 'transp_on', 'hru_intcpevap', &
        'snow_evap', 'snowcov_area', 'soil_rechr', &
        'soil_rechr_max', 'soil_moist', &
        'active_mask', 'hru_area_perv', 'hru_frac_perv', &
        'hru_impervevap', 'soil_moist_chg', 'soil_rechr_chg', &
        'pkwater_equiv',  'dprst_stor_hru', &
        'hru_impervstor', 'swrad', 'hru_area_dble', &
        'newsnow', 'pkwater_ante', 'pptmix', 'pptmix_nopack', 'snowmelt', &
        'dprst_area_clos','dprst_area_max','dprst_frac','dprst_in', &
        'dprst_insroff_hru','dprst_sroff_hru','dprst_vol_clos', &
        'dprst_vol_open', 'hru_hortn_cascflow', 'hru_percent_imperv', &
        'hru_sroffi', 'hru_sroffp', 'sro_to_dprst_perv', &
        'upslope_hortonian', 'hortonian_flow', 'soil_rechr_init_frac', &
        'soil_rechr_max_frac', 'soil_moist_init_frac', 'covden_sum',  &
        'covden_win', 'jh_coef_hru', &
        !runoff
        
        !surface depression storage
        'va_clos_exp', &
        
        !intcp
        'snow_intcp', 'srain_intcp', 'wrain_intcp', 'canopy_covden', &
        'intcp_changeover', 'intcp_evap', &
        'intcp_stor', 'net_ppt', 'net_rain', &
        'net_snow', 'hru_intcpstor', &
        
        !climate_flow
        'soil_moist_max', &
        !runoff
        'va_open_exp', 'dprst_seep_rate_clos', 'smidx_exp', 'snowinfil_max', &
        'dprst_depth_avg', 'carea_max', 'va_close_exp', 'imperv_stor_max', 'dprst_flow_coef', &
        'smidx_coef', &
        !snowcomp
        'rad_trncf', 'snarea_thresh', &
        !solar radiation
        !temperature
        !transp_tindex
        'transp_tmax')
        grid = 0
        bmi_status = BMI_SUCCESS
    case('nhm_seg', 'strm_seg_in')
        grid = 1
        bmi_status = BMI_SUCCESS
    case('cascade_flag', 'dprst_flag', 'gsflow_mode', &
        'print_debug', 'nlake', 'active_hrus', &
        'srunoff_updated_soil', &
        'cascadegw_flag', 'nhru', 'use_transfer_intcp', 'last_intcp_stor', &
        'use_sroff_transfer', 'nmonths')
        grid = 2
        bmi_status = BMI_SUCCESS
    case('hru_route_order')
        grid = 3 ! nhru_active
        bmi_status = BMI_SUCCESS
    case('snarea_curve')
        grid = 4 ! ndepval
        bmi_status = BMI_SUCCESS
    case('epan_coef', 'jh_coef', 'tmax_allrain_offset', 'tmax_allsnow', 'adjmix_rain', &
        'rain_cbh_adj', 'snow_cbh_adj', 'cecn_coef', 'radmax', 'dday_slope', 'tmax_index', &
        'dday_intcp', 'tmax_cbh_adj', 'tmin_cbh_adj')
        grid = 5 ! by nhru, nmonth
        bmi_status = BMI_SUCCESS
    case('nowtime')
        grid = 6
        bmi_status = BMI_SUCCESS
    case default
        grid = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_grid

    ! The type of a variable's grid.
    function prms_grid_type(this, grid, type) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(1)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(2)
        type = 'scalar'
        bmi_status = BMI_SUCCESS
    case(3)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(4)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(5)
        type = "rectilinear"
        bmi_status = BMI_SUCCESS
    case(6)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_type

    ! The number of dimensions of a grid.
    function prms_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(1)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(2)
        rank = 0
        bmi_status = BMI_SUCCESS
    case(3)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(4)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(5)
        rank = 2
        bmi_status = BMI_SUCCESS
    case(6)
        rank = 1
        bmi_status = BMI_SUCCESS
    case default
        rank = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_rank

    ! The dimensions of a grid.
    function prms_grid_shape(this, grid, shape) result (bmi_status)
     class (bmi_prms_surface), intent(in) :: this
     integer, intent(in) :: grid
     integer, dimension(:), intent(out) :: shape
     integer :: bmi_status
    
     select case(grid)
     case(0)
         shape(:) = [this%model%model_simulation%model_basin%nhru]
         bmi_status = BMI_SUCCESS
     case(1)
         shape(:) = [this%model%model_simulation%model_basin%nsegment]
         bmi_status = BMI_SUCCESS
     case(2)
         shape(:) = [1]
         bmi_status = BMI_SUCCESS
     case(3)
         shape(:) = [count(this%model%model_simulation%model_basin%active_mask)]
         bmi_status = BMI_SUCCESS
     case(4)
         shape(:) = [this%model%model_simulation%snow%ndeplval]
         bmi_status = BMI_SUCCESS
     case(5)
         shape(:) = [this%model%model_simulation%model_basin%nmonths, &
            this%model%model_simulation%model_basin%nhru]
         bmi_status = BMI_SUCCESS
      case(6)
         shape(:) = [6]
         bmi_status = BMI_SUCCESS
    case default
        shape(:) = -1
        bmi_status = BMI_FAILURE
     end select
    end function prms_grid_shape
    
    ! The total number of elements in a grid.
    function prms_grid_size(this, grid, size) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
        size = this%model%model_simulation%model_basin%nhru
        bmi_status = BMI_SUCCESS
    case(1)
        size = this%model%model_simulation%model_basin%nsegment
        bmi_status = BMI_SUCCESS
    case(2)
        size = 1
        bmi_status = BMI_SUCCESS
    case(3)
        size = count(this%model%model_simulation%model_basin%active_mask)
        bmi_status = BMI_SUCCESS
    case(4)
        size = this%model%model_simulation%snow%ndeplval
        bmi_status = BMI_SUCCESS
    case(5) !for vars dim by nhru,nmonths
        size = this%model%model_simulation%model_basin%nmonths * &
            this%model%model_simulation%model_basin%nhru
        bmi_status = BMI_SUCCESS
     case(6)
        size = 6
        bmi_status = BMI_SUCCESS
   case default
        size = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_size

    ! The distance between nodes of a grid.
    function prms_grid_spacing(this, grid, spacing) result (bmi_status)
     class (bmi_prms_surface), intent(in) :: this
     integer, intent(in) :: grid
     double precision, dimension(:), intent(out) :: spacing
     integer :: bmi_status
    
     select case(grid)
     case default
        spacing(:) = -1.d0
        bmi_status = BMI_FAILURE
     end select
    end function prms_grid_spacing
    
    ! Coordinates of grid origin.
    function prms_grid_origin(this, grid, origin) result (bmi_status)
     class (bmi_prms_surface), intent(in) :: this
     integer, intent(in) :: grid
     double precision, dimension(:), intent(out) :: origin
     integer :: bmi_status
    
     select case(grid)
     case default
        origin(:) = -1.d0
        bmi_status = BMI_FAILURE
     end select
    end function prms_grid_origin
    
    ! X-coordinates of grid nodes.
    function prms_grid_x(this, grid, x) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status, i

    select case(grid)
    case(0)
        x = this%model%model_simulation%model_basin%hru_x
        bmi_status = BMI_SUCCESS
    case(1) 
        bmi_status = this%get_value('nhm_seg', x)
    case(2)
        x = -1.d0
        bmi_status = BMI_SUCCESS
    case(3)
        bmi_status = this%get_value('hru_route_order', x)
    case(4) 
        x = dble([1.0,2.0,3.0,4.0,5.0,6.0, &
            7.0,8.0,9.0,10.0,11.0])
        bmi_status = BMI_SUCCESS
    case(5)
        do i = 1, size(x)
           x(i) = i
        end do
        bmi_status = BMI_SUCCESS
    case(6)
        x = dble([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
        bmi_status = BMI_SUCCESS
    case default
        x(:) = -1.d0
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_x

    ! Y-coordinates of grid nodes.
    function prms_grid_y(this, grid, y) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status, i

    select case(grid)
    case(0)
        y = this%model%model_simulation%model_basin%hru_y
        bmi_status = BMI_SUCCESS
    case(1) 
        y(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(2)
        y = -1.d0
        bmi_status = BMI_SUCCESS
    case(3)
        y(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(4)
        y(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(5)
        do i = 1, size(y)
           y(i) = i
        end do
        bmi_status = BMI_SUCCESS
    case(6)
        y(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case default
        y(:) = -1.d0
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_y

    ! Z-coordinates of grid nodes.
    function prms_grid_z(this, grid, z) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(0)
        z = this%model%model_simulation%model_basin%hru_elev
        bmi_status = BMI_SUCCESS
    case(1) 
        z(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(2)
        z = -1.d0
        bmi_status = BMI_SUCCESS
    case(3)
        z(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(4)
        z(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(5)
        z(:) = -1.d0
        bmi_status = BMI_SUCCESS
     case(6)
        z(:) = -1.d0
        bmi_status = BMI_SUCCESS
   case default
        z(:) = -1.d0
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_z

    ! Get the number of nodes in an unstructured grid.
    function prms_grid_node_count(this, grid, count) result(bmi_status)
      class(bmi_prms_surface), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case(0:6)
         bmi_status = this%get_grid_size(grid, count)
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_node_count

    ! Get the number of edges in an unstructured grid.
    function prms_grid_edge_count(this, grid, count) result(bmi_status)
      class(bmi_prms_surface), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case (0:4)
         bmi_status = this%get_grid_node_count(grid, count)
         count = count - 1
      case (6)
         bmi_status = this%get_grid_node_count(grid, count)
         count = count - 1
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_edge_count

    ! Get the number of faces in an unstructured grid.
    function prms_grid_face_count(this, grid, count) result(bmi_status)
      class(bmi_prms_surface), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case (0:4)
         count = 0
         bmi_status = BMI_SUCCESS
      case (6)
         count = 0
         bmi_status = BMI_SUCCESS
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_count

    ! Get the edge-node connectivity.
    function prms_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
      class(bmi_prms_surface), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: edge_nodes
      integer :: bmi_status

      select case(grid)
      case default
         edge_nodes(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_edge_nodes

    ! Get the face-edge connectivity.
    function prms_grid_face_edges(this, grid, face_edges) result(bmi_status)
      class(bmi_prms_surface), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_edges
      integer :: bmi_status

      select case(grid)
      case default
         face_edges(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_edges

    ! Get the face-node connectivity.
    function prms_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
      class(bmi_prms_surface), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_nodes
      integer :: bmi_status

      select case(grid)
      case default
         face_nodes(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_nodes

    ! Get the number of nodes for each face.
    function prms_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
      class(bmi_prms_surface), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: nodes_per_face
      integer :: bmi_status

      select case(grid)
      case default
         nodes_per_face(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_nodes_per_face

    ! The data type of the variable, as a string.
    function prms_var_type(this, name, type) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case('tmax', 'tmin', "hru_ppt", "hru_snow", "hru_rain",  &
        'hru_lat', 'hru_lon', &
        "hru_actet", 'hru_area', 'dprst_evap_hru', 'infil', &
        'sroff', 'potet', 'hru_intcpevap', 'snow_evap', 'snowcov_area', &
        'soil_rechr', 'soil_rechr_max', 'soil_moist', 'soil_moist_max', &
        'hru_area_perv', 'hru_frac_perv', 'hru_impervevap', 'soil_moist_chg', &
        'hru_intcpstor', 'hru_impervstor', 'swrad', &
        'intcp_changeover', 'intcp_evap', &
        'intcp_stor', 'net_ppt', 'net_rain', &
        'net_snow', 'snowmelt', &
        'dprst_area_clos','dprst_area_max','dprst_frac', 'dprst_insroff_hru', &
        'hru_percent_imperv', 'hru_sroffi', 'hru_sroffp', &
        'sro_to_dprst_perv', 'soil_rechr_init_frac', &
        'soil_moist_init_frac', 'soil_rechr_max_frac', &
        'soil_rechr_chg', &
        !intcp
        'covden_sum', 'snow_intcp', 'covden_win', 'wrain_intcp', 'srain_intcp', &
        'canopy_covden', &
        !potet
        'epan_coef', 'jh_coef_hru', 'jh_coef', &
        !precipitation
        'tmax_allrain_offset', 'tmax_allsnow', 'adjmix_rain', 'rain_cbh_adj', 'snow_cbh_adj', &
        !runoff
        'va_open_exp', 'smidx_coef', 'dprst_seep_rate_clos', 'smidx_exp', 'snowinfil_max', &
        'dpsrst_depth_avg', 'carea_max', 'va_xlos_exp', 'import_stor_max', 'dprst_flow_coef', &
        'hortonian_flow', 'imperv_stor_max', 'dprst_depth_avg', &
        !snow_comp
        'snarea_curve', 'rad_trncf', 'cecn_coef', 'snarea_thresh', &
        !solar radiation
        'radmax', 'dday_slope', 'tmax_index', 'dday_intcp', &
        !temperature
        'tmax_cbh_adj', 'tmin_cbh_adj', &
        !transp_tindex
        'transp_tmax', &
        !surface depression storage
        'va_clos_exp')
        type = "real"
        bmi_status = BMI_SUCCESS
        case( &
        "hortonian_lakes", &
        'dprst_seep_hru', 'strm_seg_in', 'pkwater_equiv', 'dprst_stor_hru', &
        'last_intcp_stor', 'hru_area_dble', 'pkwater_ante', 'dprst_in', &
        'dprst_sroff_hru', 'dprst_vol_clos','dprst_vol_open', &
        'hru_hortn_cascflow', 'upslope_hortonian')
        type = "double precision"
        bmi_status = BMI_SUCCESS
        case("nlake", 'active_hrus', 'nowtime', 'cov_type', 'hru_type', &
        'hru_route_order', 'cascade_flag', 'dprst_flag', 'print_debug', &
        'cascadegw_flag', 'nhru', 'newsnow', 'pptmix', &
        'nmonths', 'nhm_seg', 'nhm_id')
        type = "integer"
        bmi_status = BMI_SUCCESS
    ! vars that are logical but cast as integers for BMI
    case('use_sroff_transfer', 'srunoff_updated_soil', 'transp_on', &
        'gsflow_mode', 'active_mask', 'pptmix_nopack', 'use_transfer_intcp')
        type = 'integer'
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_type

    ! The units of the given variable.
    function prms_var_units(this, name, units) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("hru_ppt", "hru_snow", "hru_rain", "hortonian_lakes", &
        "hru_actet","seg_gwflow", 'dprst_evap_hru', 'infil', &
        'sroff', 'potet', 'hru_intcpevap', 'snow_evap', &
        'soil_rechr', 'soil_rechr_max', 'soil_moist', 'soil_moist_max', &
        'soil_moist_chg', 'soil_rechr_chg', 'gwwsm_grav',  &
        'dprst_seep_hru', &
        'pkwater_equiv', 'hru_intcpstor', 'dprst_stor_hru', 'hru_impervstor', &
        'swrad', 'last_intcp_stor', 'intcp_changeover', 'intcp_evap', &
        'intcp_stor', 'net_ppt', 'net_rain', &
        'net_snow', 'pkwater_ante', &
        'snowmelt', 'dprst_insroff_hru', 'dprst_sroff_hru', &
        'hortonian_flow', 'hru_hortn_cascflow', 'hru_sroffi', 'hru_sroffp', &
        'imporv_stor_ante', 'upslope_ante', 'wrain_intcp', 'srain_intcp', 'imporv_stor_max', &
        'snarea_thresh', 'imperv_stor_max', 'dprst_depth_avg', 'upslope_hortonian', &
        'hru_impervevap')
        units = "in"
        bmi_status = BMI_SUCCESS
    case('dprst_area_clos', 'dprst_area_max', 'hru_area', 'hru_area_dble', &
        'hru_area_perv')
        units = 'acres'
        bmi_status = BMI_SUCCESS
    case('dprst_in', 'dprst_vol_clos', 'dprst_vol_open')
        units = 'acre-inches'
        bmi_status = BMI_SUCCESS
    case('covden_sum', 'covden_win', 'epan_coef', 'adjmix_rain', &
        'rain_cbh_adj', 'snow_cbh_adj', 'smidx_coef', &
        'radmax', 'va_clos_exp', 'snarea_curve', 'canopy_covden', &
        'soil_rechr_init_frac', 'soil_moist_init_frac', 'soil_rechr_max_frac', &
        'carea_max', 'dprst_frac', 'hru_percent_imperv', 'snowcov_area', &
        'hru_frac_perv', 'sro_to_dprst_perv')
        units = 'decimal-fraction'
        bmi_status = BMI_SUCCESS
    case('snow_intcp')
        units = 'fraction-inches'
        bmi_status = BMI_SUCCESS
    case('tmax', 'tmin', 'tmax_allrain_offset', 'tmax_allsnow')
        units = 'temp_units'
        bmi_status = BMI_SUCCESS
    case('dprst_seep_rate_clos', 'dprst_flow_coef')
        units = 'fraction/day'
        bmi_status = BMI_SUCCESS
    case('smidx_exp')
        units = '1/in'
        bmi_status = BMI_SUCCESS
    case('snowinfil_max')
        units = 'in/day'
        bmi_status = BMI_SUCCESS
    case('rad_trncf','cecn_coef')
        units = 'calories per degrees celcius > 0'
        bmi_status = BMI_SUCCESS
    case('dday_slope')
        units = 'degree-day/temp_units'
        bmi_status = BMI_SUCCESS
    case('tmax_index', 'tmax_cbh_adj', 'tmin_cbh_adj', 'transp_tmax')
        units = 'temp_units'
        bmi_status = BMI_SUCCESS
    case('dday_intcp')
        units = 'degree-day'
        bmi_status = BMI_SUCCESS
    case('hru_lat', 'hru_lon')
        units = 'decimal degrees'
        bmi_status = BMI_SUCCESS
    case('nmonths')
        units = 'months'
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        units = 'ft3 s-1'
        bmi_status = BMI_SUCCESS
    case('jh_coef_hru', 'jh_coef')
        units = 'per degrees Fahrenheit'
        bmi_status = BMI_SUCCESS
    case('cascade_flag', 'nhru', 'print_debug', 'cov_type', 'use_transfer_intcp', &
        'cascadegw_flag', 'newsnow', 'va_open_exp', 'pptmix_nopack', 'pptmix', &
        'nowtime', 'nlake', 'active_mask', 'dprst_flag', 'hru_type', 'gsflow_mode',&
        'srunoff_updated_soil', 'active_hrus', 'transp_on', 'hru_route_order', &
        'nhm_id', 'nhm_seg', 'use_sroff_transfer')
        units = '-'
        bmi_status = BMI_SUCCESS
    case default
        units = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_units

    ! Memory use per array element.
    function prms_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    case("nlake")
        size = sizeof(this%model%model_simulation%model_basin%nlake)
        bmi_status = BMI_SUCCESS
    case('active_hrus')
        size = sizeof(this%model%model_simulation%model_basin%active_hrus)
        bmi_status = BMI_SUCCESS
    case('nowtime')
        size = sizeof(this%model%model_simulation%model_time%nowtime(1))
        bmi_status = BMI_SUCCESS
    case('cov_type')
        size = sizeof(this%model%model_simulation%model_basin%cov_type(1))
        bmi_status = BMI_SUCCESS
    case('hru_type')
        size = sizeof(this%model%model_simulation%model_basin%hru_type(1))
        bmi_status = BMI_SUCCESS
    case('hru_route_order')
        size = sizeof(this%model%model_simulation%model_basin%hru_route_order(1))
        bmi_status = BMI_SUCCESS
    case('cascade_flag')
        size = sizeof(this%model%control_data%cascade_flag%value)
        bmi_status = BMI_SUCCESS
    case('cascadegw_flag')
        size = sizeof(this%model%control_data%cascadegw_flag%value)
        bmi_status = BMI_SUCCESS
	case('hru_area_dble')
        size = sizeof(this%model%model_simulation%model_basin%hru_area_dble(1))
		bmi_status = BMI_SUCCESS
    case('nhru')
        size = sizeof(this%model%model_simulation%model_basin%nhru)
        bmi_status = BMI_SUCCESS
    case('nhm_seg')
        size = sizeof(this%model%model_simulation%model_basin%nhm_seg(1))
        bmi_status = BMI_SUCCESS
    case('nhm_id')
        size = sizeof(this%model%model_simulation%model_basin%nhm_id(1))
        bmi_status = BMI_SUCCESS
    case('nmonths')
        size = sizeof(this%model%model_simulation%model_basin%nmonths)
        bmi_status = BMI_SUCCESS
    case('use_transfer_intcp')
        size = sizeof(this%model%model_simulation%intcp%use_transfer_intcp)
        bmi_status = BMI_SUCCESS
    case('use_sroff_transfer')
        size = sizeof(this%model%model_simulation%runoff%use_sroff_transfer)
        bmi_status = BMI_SUCCESS
    case('dprst_flag')
        size = sizeof(this%model%control_data%dprst_flag%value)
        bmi_status = BMI_SUCCESS
    case('print_debug')
        size = sizeof(this%model%control_data%print_debug%value)
        bmi_status = BMI_SUCCESS
    case('gsflow_mode')
        size = sizeof(this%model%control_data%gsflow_mode)
        bmi_status = BMI_SUCCESS
    case('srunoff_updated_soil')
        size = sizeof(this%model%model_simulation%runoff%srunoff_updated_soil)
        bmi_status = BMI_SUCCESS
    case('last_intcp_stor')
        size = sizeof(this%model%model_simulation%intcp%last_intcp_stor)
        bmi_status = BMI_SUCCESS
    case('transp_on')
        size = sizeof(this%model%model_simulation%transpiration%transp_on(1))
        bmi_status = BMI_SUCCESS
    case('active_mask')
        size = sizeof(this%model%model_simulation%model_basin%active_mask(1))
        bmi_status = BMI_SUCCESS
    case('hru_lat')
        size = sizeof(this%model%model_simulation%model_basin%hru_lat(1))
        bmi_status = BMI_SUCCESS
    case('hru_lon')
        size = sizeof(this%model%model_simulation%model_basin%hru_lon(1))
        bmi_status = BMI_SUCCESS
    case('tmax')
        size = sizeof(this%model%model_simulation%model_temp%tmax(1))
        bmi_status = BMI_SUCCESS
    case('tmin')
        size = sizeof(this%model%model_simulation%model_temp%tmin(1))
        bmi_status = BMI_SUCCESS
    case('hru_ppt')
        size = sizeof(this%model%model_simulation%model_precip%hru_ppt(1))
        bmi_status = BMI_SUCCESS
    case('hru_rain')
        size = sizeof(this%model%model_simulation%model_precip%hru_rain(1))
        bmi_status = BMI_SUCCESS
    case('hru_snow')
        size = sizeof(this%model%model_simulation%model_precip%hru_snow(1))
        bmi_status = BMI_SUCCESS
    case('hru_area')
        size = sizeof(this%model%model_simulation%model_basin%hru_area(1))
        bmi_status = BMI_SUCCESS
    case('dprst_evap_hru')
        size = sizeof(this%model%model_simulation%runoff%dprst_evap_hru(1))
        bmi_status = BMI_SUCCESS
    case('infil')
        size = sizeof(this%model%model_simulation%runoff%infil(1))
        bmi_status = BMI_SUCCESS
    case('sroff')
        size = sizeof(this%model%model_simulation%runoff%sroff(1))
        bmi_status = BMI_SUCCESS
    case('potet')
        size = sizeof(this%model%model_simulation%potet%potet(1))
        bmi_status = BMI_SUCCESS
    case('hru_intcpevap')
        size = sizeof(this%model%model_simulation%intcp%hru_intcpevap(1))
        bmi_status = BMI_SUCCESS
        
    case('intcp_changeover')
        size = sizeof(this%model%model_simulation%intcp%intcp_changeover(1))
        bmi_status = BMI_SUCCESS
    case('intcp_evap')
        size = sizeof(this%model%model_simulation%intcp%intcp_evap(1))
        bmi_status = BMI_SUCCESS
    case('intcp_stor')
        size = sizeof(this%model%model_simulation%intcp%intcp_stor(1))
        bmi_status = BMI_SUCCESS
    case('net_ppt')
        size = sizeof(this%model%model_simulation%intcp%net_ppt(1))
        bmi_status = BMI_SUCCESS
    case('net_rain')
        size = sizeof(this%model%model_simulation%intcp%net_rain(1))
        bmi_status = BMI_SUCCESS
    case('net_snow')
        size = sizeof(this%model%model_simulation%intcp%net_snow(1))
        bmi_status = BMI_SUCCESS
    case('canopy_covden')
        size = sizeof(this%model%model_simulation%intcp%canopy_covden(1))
        bmi_status = BMI_SUCCESS


    case('snow_evap')
        size = sizeof(this%model%model_simulation%snow%snow_evap(1))
        bmi_status = BMI_SUCCESS
    case('snowcov_area')
        size = sizeof(this%model%model_simulation%snow%snowcov_area(1))
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        size = sizeof(this%model%model_simulation%climate%soil_rechr(1))
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max')
        size = sizeof(this%model%model_simulation%climate%soil_rechr_max(1))
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        size = sizeof(this%model%model_simulation%climate%soil_moist(1))
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        size = sizeof(this%model%model_simulation%climate%soil_moist_max(1))
        bmi_status = BMI_SUCCESS
    case('hru_area_perv')
        size = sizeof(this%model%model_simulation%runoff%hru_area_perv(1))
        bmi_status = BMI_SUCCESS
    case('hru_impervevap')
        size = sizeof(this%model%model_simulation%runoff%hru_impervevap(1))
        bmi_status = BMI_SUCCESS
    case('soil_moist_chg')
        size = sizeof(this%model%model_simulation%runoff%soil_moist_chg(1))
        bmi_status = BMI_SUCCESS
    case('soil_rechr_chg')
        size = sizeof(this%model%model_simulation%runoff%soil_rechr_chg(1))
        bmi_status = BMI_SUCCESS
    case('hortonian_lakes')
        size = sizeof(this%model%model_simulation%runoff%hortonian_lakes(1))
        bmi_status = BMI_SUCCESS
    case('dprst_seep_hru')
        size = sizeof(this%model%model_simulation%runoff%dprst_seep_hru(1))
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            size = sizeof(this%model%model_simulation%runoff%strm_seg_in(1))
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_FAILURE
        endif
    case('pkwater_equiv') 
        size = sizeof(this%model%model_simulation%climate%pkwater_equiv(1))
        bmi_status = BMI_SUCCESS
    case('hru_intcpstor') 
        size = sizeof(this%model%model_simulation%intcp%hru_intcpstor(1))
        bmi_status = BMI_SUCCESS
    case('dprst_stor_hru') 
        size = sizeof(this%model%model_simulation%runoff%dprst_stor_hru(1))
        bmi_status = BMI_SUCCESS
    case('hru_impervstor') 
        size = sizeof(this%model%model_simulation%runoff%hru_impervstor(1))
        bmi_status = BMI_SUCCESS
    case('swrad')
        size = sizeof(this%model%model_simulation%solrad%swrad(1))
        bmi_status = BMI_SUCCESS
        
    case('newsnow')
        size = sizeof(this%model%model_simulation%snow%newsnow(1))
        bmi_status = BMI_SUCCESS
    case('pptmix')
        size = sizeof(this%model%model_simulation%snow%pptmix(1))
        bmi_status = BMI_SUCCESS
    case('snowmelt')
        size = sizeof(this%model%model_simulation%snow%snowmelt(1))
        bmi_status = BMI_SUCCESS
    case('pkwater_ante')
        size = sizeof(this%model%model_simulation%snow%pkwater_ante(1))
        bmi_status = BMI_SUCCESS
    case('pptmix_nopack')
        size = sizeof(this%model%model_simulation%snow%pptmix_nopack(1))
        bmi_status = BMI_SUCCESS
        
    case('dprst_area_clos')
        size = sizeof(this%model%model_simulation%runoff%dprst_area_clos(1))
        bmi_status = BMI_SUCCESS
    case('dprst_area_max')
        size = sizeof(this%model%model_simulation%runoff%dprst_area_max(1))
        bmi_status = BMI_SUCCESS
    case('dprst_frac')
        size = sizeof(this%model%model_simulation%runoff%dprst_frac(1))
        bmi_status = BMI_SUCCESS
    case('dprst_insroff_hru')
         size = sizeof(this%model%model_simulation%runoff%dprst_insroff_hru(1))
        bmi_status = BMI_SUCCESS
   case('hru_percent_imperv')
        size = sizeof(this%model%model_simulation%runoff%hru_percent_imperv(1))
        bmi_status = BMI_SUCCESS
    case('hru_sroffi')
        size = sizeof(this%model%model_simulation%runoff%hru_sroffi(1))
        bmi_status = BMI_SUCCESS
    case('hru_sroffp')
        size = sizeof(this%model%model_simulation%runoff%hru_sroffp(1))
        bmi_status = BMI_SUCCESS
    case('sro_to_dprst_perv')
        size = sizeof(this%model%model_simulation%runoff%sro_to_dprst_perv(1))
        bmi_status = BMI_SUCCESS
    case('hortonian_flow')
        size = sizeof(this%model%model_simulation%runoff%hortonian_flow(1))
        bmi_status = BMI_SUCCESS

    case('upslope_hortonian')
        size = sizeof(this%model%model_simulation%runoff%upslope_hortonian(1))
        bmi_status = BMI_SUCCESS
    case('dprst_in')
        size = sizeof(this%model%model_simulation%runoff%dprst_in(1))
        bmi_status = BMI_SUCCESS
    case('dprst_sroff_hru')
        size = sizeof(this%model%model_simulation%runoff%dprst_sroff_hru(1))
        bmi_status = BMI_SUCCESS
    case('dprst_vol_clos')
        size = sizeof(this%model%model_simulation%runoff%dprst_vol_clos(1))
        bmi_status = BMI_SUCCESS
    case('dprst_vol_open')
        size = sizeof(this%model%model_simulation%runoff%dprst_vol_open(1))
        bmi_status = BMI_SUCCESS
    case('hru_hortn_cascflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            size = sizeof(this%model%model_simulation%runoff%hru_hortn_cascflow(1))
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_SUCCESS
        endif
        bmi_status = BMI_SUCCESS
        
    !case('soil_moist_max') already here
        !climateflow module
    case('soil_rechr_init_frac')
        size = sizeof(this%model%model_simulation%climate%soil_rechr_init_frac)
        bmi_status = BMI_SUCCESS
    case('soil_moist_init_frac')
        size = sizeof(this%model%model_simulation%climate%soil_moist_init_frac)
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max_frac ')
        size = sizeof(this%model%model_simulation%climate%soil_rechr_max_frac)
        bmi_status = BMI_SUCCESS
        !intcp module
    case('covden_sum ')
        size = sizeof(this%model%model_simulation%intcp%covden_sum(1))
        bmi_status = BMI_SUCCESS
    case('snow_intcp ')
        size = sizeof(this%model%model_simulation%intcp%snow_intcp(1))
        bmi_status = BMI_SUCCESS
    case('covden_win ')
        size = sizeof(this%model%model_simulation%intcp%covden_win(1))
        bmi_status = BMI_SUCCESS
    case('wrain_intcp')
        size = sizeof(this%model%model_simulation%intcp%wrain_intcp(1))
        bmi_status = BMI_SUCCESS
    case('srain_intcp')
        size = sizeof(this%model%model_simulation%intcp%srain_intcp(1))
        bmi_status = BMI_SUCCESS

        !potet
    case('epan_coef')
        size = sizeof(this%model%model_simulation%potet%epan_coef(1,1))
        bmi_status = BMI_SUCCESS
    !    !potet_jh - !not implemented yet
    case('jh_coef_hru ')
        select type(potet => this%model%model_simulation%potet)
            type is(Potet_jh)
                size = sizeof(potet%jh_coef_hru(1))
        end select
        bmi_status = BMI_SUCCESS
    case('jh_coef ')
        select type(potet => this%model%model_simulation%potet)
            type is(Potet_jh)
                size = sizeof(potet%jh_coef(1,1))
        end select
        bmi_status = BMI_SUCCESS
        !precipitation
    case('tmax_allrain_offset')
        size = sizeof(this%model%model_simulation%model_precip%tmax_allrain_offset(1,1))
        bmi_status = BMI_SUCCESS
    case('tmax_allsnow')
        size = sizeof(this%model%model_simulation%model_precip%tmax_allsnow(1,1))
        bmi_status = BMI_SUCCESS
        !precipitation_hru - !not implemented yet
    case('adjmix_rain')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                size = sizeof(model_precip%adjmix_rain(1,1))
        end select
        bmi_status = BMI_SUCCESS
    case('rain_cbh_adj')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                size = sizeof(model_precip%rain_cbh_adj(1,1))
        end select
        bmi_status = BMI_SUCCESS
    case('snow_cbh_adj')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                size = sizeof(model_precip%snow_cbh_adj(1,1))
        end select
        bmi_status = BMI_SUCCESS
        
        !runoff
    case('va_open_exp')
        size = sizeof(this%model%model_simulation%runoff%va_open_exp(1))
        bmi_status = BMI_SUCCESS
    case('va_clos_exp')
        size = sizeof(this%model%model_simulation%runoff%va_clos_exp(1))
        bmi_status = BMI_SUCCESS
    case('smidx_coef')
        size = sizeof(this%model%model_simulation%runoff%smidx_coef(1))
        bmi_status = BMI_SUCCESS
    case('smidx_exp')
        size = sizeof(this%model%model_simulation%runoff%smidx_exp(1))
        bmi_status = BMI_SUCCESS
    case('dprst_seep_rate_clos')
        size = sizeof(this%model%model_simulation%runoff%dprst_seep_rate_clos(1))
        bmi_status = BMI_SUCCESS
    case('dprst_depth_avg')
        size = sizeof(this%model%model_simulation%runoff%dprst_depth_avg(1))
        bmi_status = BMI_SUCCESS
    case('dprst_flow_coef')
        size = sizeof(this%model%model_simulation%runoff%dprst_flow_coef(1))
        bmi_status = BMI_SUCCESS
    case('snowinfil_max')
        size = sizeof(this%model%model_simulation%runoff%snowinfil_max(1))
        bmi_status = BMI_SUCCESS
    case('carea_max')
        size = sizeof(this%model%model_simulation%runoff%carea_max(1))
        bmi_status = BMI_SUCCESS
    case('imperv_stor_max')
        size = sizeof(this%model%model_simulation%runoff%imperv_stor_max(1))
        bmi_status = BMI_SUCCESS
    case('hru_frac_perv')
        size = sizeof(this%model%model_simulation%runoff%hru_frac_perv(1))
        bmi_status = BMI_SUCCESS
        !snowcomp
    case('snarea_curve')
        size = sizeof(this%model%model_simulation%snow%snarea_curve(1))
        bmi_status = BMI_SUCCESS
    case('snarea_thresh')
        size = sizeof(this%model%model_simulation%snow%snarea_thresh(1))
        bmi_status = BMI_SUCCESS
    case('rad_trncf')
        size = sizeof(this%model%model_simulation%snow%rad_trncf(1))
        bmi_status = BMI_SUCCESS
    case('cecn_coef')
        size = sizeof(this%model%model_simulation%snow%cecn_coef(1,1))
        bmi_status = BMI_SUCCESS
        !solar radiation module
    case('radmax')
        size = sizeof(this%model%model_simulation%solrad%radmax(1,1))
        bmi_status = BMI_SUCCESS
        !solar_radition_degday module
    case('dday_slope')
        !size = sizeof(this%model%model_simulation%solrad%dday_slope)
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                size = sizeof(solrad%dday_slope(1,1))
        end select
        bmi_status = BMI_SUCCESS
    case('dday_intcp')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                size = sizeof(solrad%dday_intcp(1,1))
        end select
        bmi_status = BMI_SUCCESS
    case('tmax_index')
        !size = sizeof(this%model%model_simulation%solrad%tmax_index)
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                size = sizeof(solrad%tmax_index(1,1))
        end select
        bmi_status = BMI_SUCCESS
        !temperature_hru module
    case('tmax_cbh_adj')
        select type(model_temp => this%model%model_simulation%model_temp)
            type is(Temperature_Hru)
                size = sizeof(model_temp%tmax_cbh_adj(1,1))
        end select
        bmi_status = BMI_SUCCESS
    case('tmin_cbh_adj')
        select type(model_temp => this%model%model_simulation%model_temp)
            type is(Temperature_Hru)
                size = sizeof(model_temp%tmin_cbh_adj(1,1))
        end select
        bmi_status = BMI_SUCCESS
        !transp_tindex module
    case('transp_tmax')
        select type(transpiration => this%model%model_simulation%transpiration)
            type is(Transp_tindex)
                size = sizeof(transpiration%transp_tmax(1))
        end select
        bmi_status = BMI_SUCCESS
    case default
        size = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_itemsize
    
    ! The size of the given variable.
    function prms_var_nbytes(this, name, nbytes) result (bmi_status)
      class (bmi_prms_surface), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(out) :: nbytes
      integer :: bmi_status
      integer :: s1, s2, s3, type, grid_size, item_size
    
      s1 = this%get_var_grid(name, type)
      s2 = this%get_grid_size(type, grid_size)
      s3 = this%get_var_itemsize(name, item_size)
    
      if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
         nbytes = item_size * grid_size
         bmi_status = BMI_SUCCESS
      else
         nbytes = -1
         bmi_status = BMI_FAILURE
      end if
    end function prms_var_nbytes
    
  ! The location (node, face, edge) of the given variable.
    function prms_var_location(this, name, location) result (bmi_status)
        class (bmi_prms_surface), intent(in) :: this
        character (len=*), intent(in) :: name
        character (len=*), intent(out) :: location
        integer :: bmi_status

        select case(name)
        case default
           location = "node"
           bmi_status = BMI_SUCCESS
        end select
    end function prms_var_location
    
    ! Get a copy of a integer variable's values, flattened.
    function prms_get_int(this, name, dest) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status
        
    select case(name)
        !basin
    case("nlake")
        dest = [this%model%model_simulation%model_basin%nlake]
        bmi_status = BMI_SUCCESS
    case('active_hrus')
        dest = [this%model%model_simulation%model_basin%active_hrus]
        bmi_status = BMI_SUCCESS
    case('cov_type')
        dest = [this%model%model_simulation%model_basin%cov_type]
        bmi_status = BMI_SUCCESS
    case('hru_type')
        dest = [this%model%model_simulation%model_basin%hru_type]
        bmi_status = BMI_SUCCESS
    case('hru_route_order')
        dest = [this%model%model_simulation%model_basin%hru_route_order]
        bmi_status = BMI_SUCCESS
    case('nhru')
        dest = [this%model%model_simulation%model_basin%nhru]
        bmi_status = BMI_SUCCESS
    case('nhm_id')
        dest = [this%model%model_simulation%model_basin%nhm_id]
        bmi_status = BMI_SUCCESS
    case('nhm_seg')
        dest = [this%model%model_simulation%model_basin%nhm_seg]
        bmi_status = BMI_SUCCESS
    case('nmonths')
        dest = [this%model%model_simulation%model_basin%nmonths]
        bmi_status = BMI_SUCCESS
    case('active_mask')
        dest = [this%model%model_simulation%model_basin%active_mask]
        bmi_status = BMI_SUCCESS

        !prms_time
    case('nowtime')
        dest = [this%model%model_simulation%model_time%nowtime]
        bmi_status = BMI_SUCCESS
        
        !control
    case('dprst_flag')
        dest = [this%model%control_data%dprst_flag%value]
        bmi_status = BMI_SUCCESS
    case('gsflow_mode')
        dest = [this%model%control_data%gsflow_mode]
        bmi_status = BMI_SUCCESS
    case('print_debug')
        dest = [this%model%control_data%print_debug%value]
        bmi_status = BMI_SUCCESS
    case('cascade_flag')
        dest = [this%model%control_data%cascade_flag%value]
        bmi_status = BMI_SUCCESS
    case('cascadegw_flag')
        dest = [this%model%control_data%cascadegw_flag%value]
        bmi_status = BMI_SUCCESS
        
        !runoff
    case('srunoff_updated_soil')
        dest = [this%model%model_simulation%runoff%srunoff_updated_soil]
        bmi_status = BMI_SUCCESS
    case('use_sroff_transfer')
        dest = [this%model%model_simulation%runoff%use_sroff_transfer]
        bmi_status = BMI_SUCCESS
       
        !intcp
    case('use_transfer_intcp')
        dest = [this%model%model_simulation%intcp%use_transfer_intcp]
        bmi_status = BMI_SUCCESS

        !transpiration
    case('transp_on')
        dest = [this%model%model_simulation%transpiration%transp_on]
        bmi_status = BMI_SUCCESS
        
        !snow
    case('newsnow')
        dest = [this%model%model_simulation%snow%newsnow]
        bmi_status = BMI_SUCCESS
    case('pptmix')
        dest = [this%model%model_simulation%snow%pptmix]
        bmi_status = BMI_SUCCESS
    case('pptmix_nopack')
        dest = [this%model%model_simulation%snow%pptmix_nopack]
        bmi_status = BMI_SUCCESS
    case default
        dest = [-1]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_int
    
    ! Get a copy of a real variable's values, flattened.
    function prms_get_float(this, name, dest) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case('tmax')
        dest = [this%model%model_simulation%model_temp%tmax]
        bmi_status = BMI_SUCCESS
    case('tmin')
        dest = [this%model%model_simulation%model_temp%tmin]
        bmi_status = BMI_SUCCESS
        
        !precip
    case('hru_ppt')
        dest = [this%model%model_simulation%model_precip%hru_ppt]
        bmi_status = BMI_SUCCESS
    case('hru_rain')
        dest = [this%model%model_simulation%model_precip%hru_rain]
        bmi_status = BMI_SUCCESS
    case('hru_snow')
        dest = [this%model%model_simulation%model_precip%hru_snow]
        bmi_status = BMI_SUCCESS
        
        !basin
    case('hru_lat')
         dest = [this%model%model_simulation%model_basin%hru_lat]
         bmi_status = BMI_SUCCESS
    case('hru_lon')
         dest = [this%model%model_simulation%model_basin%hru_lon]
         bmi_status = BMI_SUCCESS
    case('hru_area')
        dest = [this%model%model_simulation%model_basin%hru_area]
        bmi_status = BMI_SUCCESS
        
        !runoff
    case('infil')
        dest = [this%model%model_simulation%runoff%infil]
        bmi_status = BMI_SUCCESS
    case('sroff')
        dest = [this%model%model_simulation%runoff%sroff]
        bmi_status = BMI_SUCCESS
    case('hru_area_perv')
        dest = [this%model%model_simulation%runoff%hru_area_perv]
        bmi_status = BMI_SUCCESS
    case('hru_impervevap')
        dest = [this%model%model_simulation%runoff%hru_impervevap]
        bmi_status = BMI_SUCCESS
    case('soil_moist_chg')
        dest = [this%model%model_simulation%runoff%soil_moist_chg]
        bmi_status = BMI_SUCCESS
    case('soil_rechr_chg')
        dest = [this%model%model_simulation%runoff%soil_rechr_chg]
        bmi_status = BMI_SUCCESS
    case('hru_frac_perv')
        dest = [this%model%model_simulation%runoff%hru_frac_perv]
        bmi_status = BMI_SUCCESS
    case('hru_impervstor')
        dest = [this%model%model_simulation%runoff%hru_impervstor]
        bmi_status = BMI_SUCCESS
    case('hru_percent_imperv')
        dest = [this%model%model_simulation%runoff%hru_percent_imperv]
        bmi_status = BMI_SUCCESS
    case('hru_sroffi')
        dest = [this%model%model_simulation%runoff%hru_sroffi]
        bmi_status = BMI_SUCCESS
    case('hru_sroffp')
        dest = [this%model%model_simulation%runoff%hru_sroffp]
        bmi_status = BMI_SUCCESS
    case('dprst_evap_hru')
        dest = [this%model%model_simulation%runoff%dprst_evap_hru]
        bmi_status = BMI_SUCCESS
    case('dprst_area_clos')
        dest = [this%model%model_simulation%runoff%dprst_area_clos]
        bmi_status = BMI_SUCCESS
    case('dprst_area_max')
        dest = [this%model%model_simulation%runoff%dprst_area_max]
        bmi_status = BMI_SUCCESS
    case('dprst_frac')
        dest = [this%model%model_simulation%runoff%dprst_frac]
        bmi_status = BMI_SUCCESS
    case('dprst_insroff_hru')
        dest = [this%model%model_simulation%runoff%dprst_insroff_hru]
        bmi_status = BMI_SUCCESS
    case('sro_to_dprst_perv')
        dest = [this%model%model_simulation%runoff%sro_to_dprst_perv]
        bmi_status = BMI_SUCCESS
    case('hortonian_flow')
        dest = [this%model%model_simulation%runoff%hortonian_flow]
        bmi_status = BMI_SUCCESS
        
        !potet
    case('potet')
        dest = [this%model%model_simulation%potet%potet]
        bmi_status = BMI_SUCCESS
        
        !intcp
    case('hru_intcpevap')
        dest = [this%model%model_simulation%intcp%hru_intcpevap]
        bmi_status = BMI_SUCCESS
    case('hru_intcpstor')
        dest = [this%model%model_simulation%intcp%hru_intcpstor]
        bmi_status = BMI_SUCCESS
    case('intcp_changeover')
        dest = [this%model%model_simulation%intcp%intcp_changeover]
        bmi_status = BMI_SUCCESS
    case('intcp_evap')
        dest = [this%model%model_simulation%intcp%intcp_evap]
        bmi_status = BMI_SUCCESS
    case('intcp_stor')
        dest = [this%model%model_simulation%intcp%intcp_stor]
        bmi_status = BMI_SUCCESS
    case('net_ppt')
        dest = [this%model%model_simulation%intcp%net_ppt]
        bmi_status = BMI_SUCCESS
    case('net_rain')
        dest = [this%model%model_simulation%intcp%net_rain]
        bmi_status = BMI_SUCCESS
    case('net_snow')
        dest = [this%model%model_simulation%intcp%net_snow]
        bmi_status = BMI_SUCCESS
    case('snow_intcp')
        dest = [this%model%model_simulation%intcp%snow_intcp]
        bmi_status = BMI_SUCCESS
    case('srain_intcp')
        dest = [this%model%model_simulation%intcp%srain_intcp]
        bmi_status = BMI_SUCCESS
    case('wrain_intcp')
        dest = [this%model%model_simulation%intcp%wrain_intcp]
        bmi_status = BMI_SUCCESS
    case('canopy_covden')
        dest = [this%model%model_simulation%intcp%canopy_covden]
        bmi_status = BMI_SUCCESS

        
        !snow
    case('snow_evap')
        dest = [this%model%model_simulation%snow%snow_evap]
        bmi_status = BMI_SUCCESS
    case('snowcov_area')
        dest = [this%model%model_simulation%snow%snowcov_area]
        bmi_status = BMI_SUCCESS
    case('snowmelt')
        dest = [this%model%model_simulation%snow%snowmelt]
        bmi_status = BMI_SUCCESS
        
        !climate
    case('soil_rechr')
        dest = [this%model%model_simulation%climate%soil_rechr]
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max')
        dest = [this%model%model_simulation%climate%soil_rechr_max]
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        dest = [this%model%model_simulation%climate%soil_moist]
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        dest = [this%model%model_simulation%climate%soil_moist_max]
        bmi_status = BMI_SUCCESS
        
        !solrad
    case('swrad')
        dest = [this%model%model_simulation%solrad%swrad]
        bmi_status = BMI_SUCCESS
    case('dday_intcp')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                dest = [solrad%dday_intcp]
        end select
        bmi_status = BMI_SUCCESS
    case('dday_slope')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                dest = [solrad%dday_slope]
        end select
        bmi_status = BMI_SUCCESS
    case('tmax_index')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                dest = [solrad%tmax_index]
        end select
        bmi_status = BMI_SUCCESS
    case default
        dest = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_float

    ! Get a copy of a double variable's values, flattened.
    function prms_get_double(this, name, dest) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
        !basin
    case('hru_area_dble')
        dest = [this%model%model_simulation%model_basin%hru_area_dble]
        bmi_status = BMI_SUCCESS

        !cliamte
    case('pkwater_equiv')
        dest = [this%model%model_simulation%climate%pkwater_equiv]
        bmi_status = BMI_SUCCESS

        !intcp
    case('last_intcp_stor')
        dest = [this%model%model_simulation%intcp%last_intcp_stor]
        bmi_status = BMI_SUCCESS
       
        !potet
        
        !runoff
    case('hortonian_lakes')
        dest = [this%model%model_simulation%runoff%hortonian_lakes]
        bmi_status = BMI_SUCCESS
    case('dprst_seep_hru')
        dest = [this%model%model_simulation%runoff%dprst_seep_hru]
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = [this%model%model_simulation%runoff%strm_seg_in]
            bmi_status = BMI_SUCCESS
        else
            dest = [-1.d0]
            bmi_status = BMI_FAILURE
        endif
    case('dprst_stor_hru')
        dest = [this%model%model_simulation%runoff%dprst_stor_hru]
        bmi_status = BMI_SUCCESS
    case('upslope_hortonian')
        dest = [this%model%model_simulation%runoff%upslope_hortonian]
        bmi_status = BMI_SUCCESS
    case('dprst_in')
        dest = [this%model%model_simulation%runoff%dprst_in]
        bmi_status = BMI_SUCCESS
    case('dprst_sroff_hru')
        dest = [this%model%model_simulation%runoff%dprst_sroff_hru]
        bmi_status = BMI_SUCCESS
    case('dprst_vol_clos')
        dest = [this%model%model_simulation%runoff%dprst_vol_clos]
        bmi_status = BMI_SUCCESS
    case('dprst_vol_open')
        dest = [this%model%model_simulation%runoff%dprst_vol_open]
        bmi_status = BMI_SUCCESS
    case('hru_hortn_cascflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = [this%model%model_simulation%runoff%hru_hortn_cascflow]
            bmi_status = BMI_SUCCESS
        else
            dest(:) = -1.d0
            bmi_status = BMI_SUCCESS
        endif   

        
        !snow
    case('pkwater_ante')
        dest = [this%model%model_simulation%snow%pkwater_ante]
        bmi_status = BMI_SUCCESS


    case default
        dest = [-1.d0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_double

    ! Get a reference to an integer-valued variable, flattened.
    function prms_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status, status
    type (c_ptr) :: src
    integer :: n_elements, gridid

    select case(name)
        !basin
    case('cov_type')
        src = c_loc(this%model%model_simulation%model_basin%cov_type(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_type')
        src = c_loc(this%model%model_simulation%model_basin%hru_type(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_route_order')
        src = c_loc(this%model%model_simulation%model_basin%hru_route_order(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS  
    case('hru_area_dble')
        src = c_loc(this%model%model_simulation%model_basin%hru_area_dble)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS

        !prms_time
        !control
        
        !runoff
        
        !intcp
        
        !transpiration
    case('transp_on')
        src = c_loc(this%model%model_simulation%transpiration%transp_on(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !snow
    case('newsnow')
        src = c_loc(this%model%model_simulation%snow%newsnow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pptmix')
        src = c_loc(this%model%model_simulation%snow%pptmix(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
 
    case default
        bmi_status = BMI_FAILURE
    end select

    end function prms_get_ptr_int

    ! Get a reference to a real-valued variable, flattened.
    function prms_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, gridid, status

    select case(name)
    case('tmax')
        src = c_loc(this%model%model_simulation%model_temp%tmax(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('tmin')
        src = c_loc(this%model%model_simulation%model_temp%tmin(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS

        !precip
    case('hru_ppt')
        src = c_loc(this%model%model_simulation%model_precip%hru_ppt(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_rain')
        src = c_loc(this%model%model_simulation%model_precip%hru_rain(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_snow')
        src = c_loc(this%model%model_simulation%model_precip%hru_snow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !basin
    case('hru_lat')
        src = c_loc(this%model%model_simulation%model_basin%hru_lat(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_lon')
        src = c_loc(this%model%model_simulation%model_basin%hru_lon(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_area')
        src = c_loc(this%model%model_simulation%model_basin%hru_area(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !runoff
    case('dprst_evap_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_evap_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('infil')
        src = c_loc(this%model%model_simulation%runoff%infil(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('sroff')
        src = c_loc(this%model%model_simulation%runoff%sroff(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_area_perv')
        src = c_loc(this%model%model_simulation%runoff%hru_area_perv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_impervevap')
        src = c_loc(this%model%model_simulation%runoff%hru_impervevap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_moist_chg')
        src = c_loc(this%model%model_simulation%runoff%soil_moist_chg(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_rechr_chg')
        src = c_loc(this%model%model_simulation%runoff%soil_rechr_chg(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_impervstor')
        src = c_loc(this%model%model_simulation%runoff%hru_impervstor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_area_clos')
        src = c_loc(this%model%model_simulation%runoff%dprst_area_clos(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_area_max')
        src = c_loc(this%model%model_simulation%runoff%dprst_area_max(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_frac')
        src = c_loc(this%model%model_simulation%runoff%dprst_frac(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_insroff_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_insroff_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
   case('hru_percent_imperv')
        src = c_loc(this%model%model_simulation%runoff%hru_percent_imperv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_sroffi')
        src = c_loc(this%model%model_simulation%runoff%hru_sroffi(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_sroffp')
        src = c_loc(this%model%model_simulation%runoff%hru_sroffp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('sro_to_dprst_perv')
        src = c_loc(this%model%model_simulation%runoff%sro_to_dprst_perv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hortonian_flow')
        src = c_loc(this%model%model_simulation%runoff%hortonian_flow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !potet
    case('potet')
        src = c_loc(this%model%model_simulation%potet%potet(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !intcp
    case('hru_intcpevap')
        src = c_loc(this%model%model_simulation%intcp%hru_intcpevap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_intcpstor')
        src = c_loc(this%model%model_simulation%intcp%hru_intcpstor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('intcp_changeover')
        src = c_loc(this%model%model_simulation%intcp%intcp_changeover(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('intcp_evap')
        src = c_loc(this%model%model_simulation%intcp%intcp_evap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('intcp_stor')
        src = c_loc(this%model%model_simulation%intcp%intcp_stor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('net_ppt')
        src = c_loc(this%model%model_simulation%intcp%net_ppt(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('net_rain')
        src = c_loc(this%model%model_simulation%intcp%net_rain(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('net_snow')
        src = c_loc(this%model%model_simulation%intcp%net_snow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('snow_intcp')
        src = c_loc(this%model%model_simulation%intcp%snow_intcp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('srain_intcp')
        src = c_loc(this%model%model_simulation%intcp%srain_intcp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('wrain_intcp')
        src = c_loc(this%model%model_simulation%intcp%wrain_intcp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('canopy_covden')
        src = c_loc(this%model%model_simulation%intcp%canopy_covden(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS

        !snow
    case('snow_evap')
        src = c_loc(this%model%model_simulation%snow%snow_evap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('snowcov_area')
        src = c_loc(this%model%model_simulation%snow%snowcov_area(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('snowmelt')
        src = c_loc(this%model%model_simulation%snow%snowmelt(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !climate
    case('soil_rechr')
        src = c_loc(this%model%model_simulation%climate%soil_rechr(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max')
        src = c_loc(this%model%model_simulation%climate%soil_rechr_max(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        src = c_loc(this%model%model_simulation%climate%soil_moist(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        src = c_loc(this%model%model_simulation%climate%soil_moist_max(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !solrad
    case('swrad')
        src = c_loc(this%model%model_simulation%solrad%swrad(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_float

    ! Get a reference to an double-valued variable, flattened.
    function prms_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, status, gridid

    select case(name)
        !basin
        
        !runoff
    case('hortonian_lakes')
        src = c_loc(this%model%model_simulation%runoff%hortonian_lakes(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_seep_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_seep_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%runoff%strm_seg_in(1))
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('dprst_stor_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_stor_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('upslope_hortonian')
        src = c_loc(this%model%model_simulation%runoff%upslope_hortonian)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_in')
        src = c_loc(this%model%model_simulation%runoff%dprst_in)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_sroff_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_sroff_hru)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_vol_clos')
        src = c_loc(this%model%model_simulation%runoff%dprst_vol_clos)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dprst_vol_open')
        src = c_loc(this%model%model_simulation%runoff%dprst_vol_open)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_hortn_cascflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%runoff%hru_hortn_cascflow)
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
        
        !potet

        
        !climate
    case('pkwater_equiv')
        src = c_loc(this%model%model_simulation%climate%pkwater_equiv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !snow
    case('pkwater_ante')
        src = c_loc(this%model%model_simulation%snow%pkwater_ante(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
        
        !intcp
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_double

    ! Get values of an integer variable at the given locations.
    function prms_get_at_indices_int(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    integer, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid

    select case(name)
        
        !basin
    case('cov_type')
        src = c_loc(this%model%model_simulation%model_basin%cov_type(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_type')
        src = c_loc(this%model%model_simulation%model_basin%hru_type(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1, size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_route_order')
        src = c_loc(this%model%model_simulation%model_basin%hru_route_order(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('active_mask')
        src = c_loc(this%model%model_simulation%model_basin%active_mask(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS

        
        !transpiration
    case('transp_on')
        src = c_loc(this%model%model_simulation%transpiration%transp_on(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        !snow
    case('newsnow')
        src = c_loc(this%model%model_simulation%snow%newsnow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pptmix')
        src = c_loc(this%model%model_simulation%snow%pptmix(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pptmix_nopack')
        src = c_loc(this%model%model_simulation%snow%pptmix_nopack(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS

    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_int

    ! Get values of a real variable at the given locations.
    function prms_get_at_indices_float(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid

    select case(name)
    case('tmax')
        src = c_loc(this%model%model_simulation%model_temp%tmax(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('tmin')
        src = c_loc(this%model%model_simulation%model_temp%tmin(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS

        !precip
    case('hru_ppt')
        src = c_loc(this%model%model_simulation%model_precip%hru_ppt(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_rain')
        src = c_loc(this%model%model_simulation%model_precip%hru_rain(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_snow')
        src = c_loc(this%model%model_simulation%model_precip%hru_snow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        !basin
    case('hru_lat')
        src = c_loc(this%model%model_simulation%model_basin%hru_lat(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_lon')
        src = c_loc(this%model%model_simulation%model_basin%hru_lon(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_area')
        src = c_loc(this%model%model_simulation%model_basin%hru_area(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        !runoff
    case('dprst_evap_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_evap_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('infil')
        src = c_loc(this%model%model_simulation%runoff%infil(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('sroff')
        src = c_loc(this%model%model_simulation%runoff%sroff(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_area_perv')
        src = c_loc(this%model%model_simulation%runoff%hru_area_perv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_impervevap')
        src = c_loc(this%model%model_simulation%runoff%hru_impervevap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist_chg')
        src = c_loc(this%model%model_simulation%runoff%soil_moist_chg(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_rechr_chg')
        src = c_loc(this%model%model_simulation%runoff%soil_rechr_chg(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_impervstor')
        src = c_loc(this%model%model_simulation%runoff%hru_impervstor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_area_clos')
        src = c_loc(this%model%model_simulation%runoff%dprst_area_clos(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_area_max')
        src = c_loc(this%model%model_simulation%runoff%dprst_area_max(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_frac')
        src = c_loc(this%model%model_simulation%runoff%dprst_frac(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_insroff_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_insroff_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
   case('hru_percent_imperv')
        src = c_loc(this%model%model_simulation%runoff%hru_percent_imperv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_sroffi')
        src = c_loc(this%model%model_simulation%runoff%hru_sroffi(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_sroffp')
        src = c_loc(this%model%model_simulation%runoff%hru_sroffp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('sro_to_dprst_perv')
        src = c_loc(this%model%model_simulation%runoff%sro_to_dprst_perv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hortonian_flow')
        src = c_loc(this%model%model_simulation%runoff%hortonian_flow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        
        !potet
    case('potet')
        src = c_loc(this%model%model_simulation%potet%potet(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        !intcp
    case('hru_intcpevap')
        src = c_loc(this%model%model_simulation%intcp%hru_intcpevap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1, n_elements
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('canopy_covden')
        src = c_loc(this%model%model_simulation%intcp%canopy_covden(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1, n_elements
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_intcpstor')
        src = c_loc(this%model%model_simulation%intcp%hru_intcpstor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1, n_elements
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('intcp_changeover')
        src = c_loc(this%model%model_simulation%intcp%intcp_changeover(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('intcp_evap')
        src = c_loc(this%model%model_simulation%intcp%intcp_evap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('intcp_stor')
        src = c_loc(this%model%model_simulation%intcp%intcp_stor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('net_ppt')
        src = c_loc(this%model%model_simulation%intcp%net_ppt(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('net_rain')
        src = c_loc(this%model%model_simulation%intcp%net_rain(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('net_snow')
        src = c_loc(this%model%model_simulation%intcp%net_snow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('snow_intcp')
        src = c_loc(this%model%model_simulation%intcp%snow_intcp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('srain_intcp')
        src = c_loc(this%model%model_simulation%intcp%srain_intcp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('wrain_intcp')
        src = c_loc(this%model%model_simulation%intcp%wrain_intcp(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        !snow
    case('snow_evap')
        src = c_loc(this%model%model_simulation%snow%snow_evap(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('snowcov_area')
        src = c_loc(this%model%model_simulation%snow%snowcov_area(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('snowmelt')
        src = c_loc(this%model%model_simulation%snow%snowmelt(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS

        !climate
    case('soil_rechr')
        src = c_loc(this%model%model_simulation%climate%soil_rechr(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max')
        src = c_loc(this%model%model_simulation%climate%soil_rechr_max(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        src = c_loc(this%model%model_simulation%climate%soil_moist(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1, n_elements
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        src = c_loc(this%model%model_simulation%climate%soil_moist_max(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        !solrad
    case('swrad')
        src = c_loc(this%model%model_simulation%solrad%swrad(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dday_intcp')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
            src = c_loc(solrad%dday_intcp(1,1))
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
        end select
        bmi_status = BMI_SUCCESS
    case('dday_slope')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
            src = c_loc(solrad%dday_slope(1,1))
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
        end select
        bmi_status = BMI_SUCCESS
    case('tmax_index')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
            src = c_loc(solrad%tmax_index(1,1))
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
        end select
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_float

    ! Get values of a double variable at the given locations.
    function prms_get_at_indices_double(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_surface), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid

    select case(name)
        
        !runoff
    case('hortonian_lakes')
        src = c_loc(this%model%model_simulation%runoff%hortonian_lakes(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_seep_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_seep_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%runoff%strm_seg_in(1))
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('dprst_stor_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_stor_hru(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('upslope_hortonian')
        src = c_loc(this%model%model_simulation%runoff%upslope_hortonian)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_in')
        src = c_loc(this%model%model_simulation%runoff%dprst_in)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_sroff_hru')
        src = c_loc(this%model%model_simulation%runoff%dprst_sroff_hru)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_vol_clos')
        src = c_loc(this%model%model_simulation%runoff%dprst_vol_clos)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_vol_open')
        src = c_loc(this%model%model_simulation%runoff%dprst_vol_open)
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_hortn_cascflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%runoff%hru_hortn_cascflow(1))
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
        
        !climate
    case('pkwater_equiv')
        src = c_loc(this%model%model_simulation%climate%pkwater_equiv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS

        !snow
    case('pkwater_ante')
        src = c_loc(this%model%model_simulation%snow%pkwater_ante(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_double

    ! Set new integer values.
    function prms_set_int(this, name, src) result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_int

    ! Set new real values.
    function prms_set_float(this, name, src) result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status, nmonths, nhru
    nmonths = this%model%model_simulation%model_basin%nmonths
    nhru = this%model%model_simulation%model_basin%nhru

    select case(name)
        !vars used in soil2gw - calculated in both soil and surface
    case('infil')
        this%model%model_simulation%runoff%infil = src
        bmi_status = BMI_SUCCESS
    case('sroff')
        this%model%model_simulation%runoff%sroff = src
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        this%model%model_simulation%climate%soil_rechr = src
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        this%model%model_simulation%climate%soil_moist = src
        bmi_status = BMI_SUCCESS
        
        !climateflow module
    case('soil_moist_max')
        this%model%model_simulation%climate%soil_moist_max = src
        bmi_status = BMI_SUCCESS
    case('soil_rechr_init_frac')
        this%model%model_simulation%climate%soil_rechr_init_frac = src
        bmi_status = BMI_SUCCESS
    case('soil_moist_init_frac')
        this%model%model_simulation%climate%soil_moist_init_frac = src
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max_frac ')
        this%model%model_simulation%climate%soil_rechr_max_frac = src
        bmi_status = BMI_SUCCESS
        
        !intcp module
    case('covden_sum')
        this%model%model_simulation%intcp%covden_sum = src
        bmi_status = BMI_SUCCESS
    case('snow_intcp')
        this%model%model_simulation%intcp%snow_intcp = src
        bmi_status = BMI_SUCCESS
    case('covden_win')
        this%model%model_simulation%intcp%covden_win = src
        bmi_status = BMI_SUCCESS
    case('wrain_intcp')
        this%model%model_simulation%intcp%wrain_intcp = src
        bmi_status = BMI_SUCCESS
    case('srain_intcp')
        this%model%model_simulation%intcp%srain_intcp = src
        bmi_status = BMI_SUCCESS
        
        !potet module
    case('epan_coef')
        this%model%model_simulation%potet%epan_coef = reshape(src, [nhru, nmonths])
        bmi_status = BMI_SUCCESS

        !potet_jh module 
    case('jh_coef')
        select type(potet => this%model%model_simulation%potet)
            type is(Potet_jh)
                potet%jh_coef = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
    case('jh_coef_hru')
        select type(potet => this%model%model_simulation%potet)
            type is(Potet_jh)
                potet%jh_coef_hru = src
        end select
        bmi_status = BMI_SUCCESS
        
        ! precipitation module
    case('tmax_allrain_offset')
        this%model%model_simulation%model_precip%tmax_allrain_offset = reshape(src, [nhru, nmonths])
        bmi_status = BMI_SUCCESS
    case('tmax_allsnow')
        this%model%model_simulation%model_precip%tmax_allsnow = reshape(src, [nhru, nmonths])
        bmi_status = BMI_SUCCESS
    case('tmax')
        this%model%model_simulation%model_temp%tmax = src
        this%model%bmitmax = .TRUE.
        bmi_status = BMI_SUCCESS
    case('tmin')
        this%model%model_simulation%model_temp%tmin = src
        this%model%bmitmin = .TRUE.
        bmi_status = BMI_SUCCESS
    case('hru_ppt')
        ! select type(model_precip => this%model%model_simulation%model_precip)
        !     type is(Precipitation_hru)
        !         model_precip%hru_ppt = src 
        ! end select
        this%model%model_simulation%model_precip%hru_ppt = src
        this%model%bmiprcp = .TRUE.
        bmi_status = BMI_SUCCESS
        
    case('adjmix_rain')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                model_precip%adjmix_rain = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
    case('rain_cbh_adj')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                model_precip%rain_cbh_adj = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
    case('snow_cbh_adj')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                model_precip%snow_cbh_adj = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
        
        !runoff
    case('va_open_exp')
        this%model%model_simulation%runoff%va_open_exp = src
        bmi_status = BMI_SUCCESS
    case('va_clos_exp')
        this%model%model_simulation%runoff%va_clos_exp = src
        bmi_status = BMI_SUCCESS
    case('smidx_coef')
        this%model%model_simulation%runoff%smidx_coef = src
        bmi_status = BMI_SUCCESS
    case('smidx_exp')
        this%model%model_simulation%runoff%smidx_exp = src
        bmi_status = BMI_SUCCESS
    case('snowinfil_max')
        this%model%model_simulation%runoff%snowinfil_max = src
        bmi_status = BMI_SUCCESS
     case('carea_max')
        this%model%model_simulation%runoff%carea_max = src
        bmi_status = BMI_SUCCESS
    case('imperv_stor_max')
        this%model%model_simulation%runoff%imperv_stor_max = src
        bmi_status = BMI_SUCCESS
    case('dprst_flow_coef')
        this%model%model_simulation%runoff%dprst_flow_coef = src
        bmi_status = BMI_SUCCESS
    case('dprst_seep_rate_clos')
        this%model%model_simulation%runoff%dprst_seep_rate_clos = src
        bmi_status = BMI_SUCCESS
    case('dprst_depth_avg')
        this%model%model_simulation%runoff%dprst_depth_avg = src
        bmi_status = BMI_SUCCESS
    case('dprst_area_max')
        this%model%model_simulation%runoff%dprst_area_max = src
        bmi_status = BMI_SUCCESS
    case('dprst_frac')
        this%model%model_simulation%runoff%dprst_frac = src
        bmi_status = BMI_SUCCESS
    case('hru_percent_imperv')
        this%model%model_simulation%runoff%hru_percent_imperv = src
        bmi_status = BMI_SUCCESS
        
        !snowcomp module
    case('snarea_curve')
        this%model%model_simulation%snow%snarea_curve = src
        bmi_status = BMI_SUCCESS
    case('snarea_thresh')
        this%model%model_simulation%snow%snarea_thresh = src
        bmi_status = BMI_SUCCESS
    case('rad_trncf')
        this%model%model_simulation%snow%rad_trncf = src
        bmi_status = BMI_SUCCESS
    case('cecn_coef')
        this%model%model_simulation%snow%cecn_coef = reshape(src, [nhru, nmonths])
        bmi_status = BMI_SUCCESS
        
        !solar radiation module
    case('radmax')
        this%model%model_simulation%solrad%radmax = reshape(src, [nhru, nmonths])
        bmi_status = BMI_SUCCESS
        
        !solar_radition_degday module
    case('dday_slope')
        !this%model%model_simulation%solrad%dday_slope = reshape(src, [nhru, nmonths])
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                solrad%dday_slope = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
    case('dday_intcp')
        !this%model%model_simulation%solrad%dday_intcp = reshape(src, [nhru, nmonths])
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                solrad%dday_intcp = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
    case('tmax_index')
        !this%model%model_simulation%solrad%tmax_index = reshape(src, [nhru, nmonths])
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
                solrad%tmax_index = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
        
        !temperature module
    case('tmax_cbh_adj')
        select type(model_temp => this%model%model_simulation%model_temp)
            type is(Temperature_Hru)
                model_temp%tmax_cbh_adj = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
    case('tmin_cbh_adj')
        select type(model_temp => this%model%model_simulation%model_temp)
            type is(Temperature_Hru)
                model_temp%tmin_cbh_adj = reshape(src, [nhru, nmonths])
        end select
        bmi_status = BMI_SUCCESS
        !transp_tindex module
    case('transp_tmax')
        select type(transpiration => this%model%model_simulation%transpiration)
            type is(Transp_tindex)
                transpiration%transp_tmax = src
        end select
        bmi_status = BMI_SUCCESS

    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_float

    ! Set new double values.
    function prms_set_double(this, name, src) result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
        !runoff
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            this%model%model_simulation%runoff%strm_seg_in = src
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('dprst_vol_clos')
        this%model%model_simulation%runoff%dprst_vol_clos = src
        bmi_status = BMI_SUCCESS
    case('dprst_vol_open')
        this%model%model_simulation%runoff%dprst_vol_open = src
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_double
    
    ! Set integer values at particular locations.
    function prms_set_at_indices_int(this, name, inds, src) &
        result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    integer, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_at_indices_int

    ! Set real values at particular locations.
    function prms_set_at_indices_float(this, name, inds, src) &
        result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    real, pointer :: dest_flattened(:)
    integer :: i, n_elements, status, gridid
    select case(name)
        !for soil2surfac
    case('infil')
        dest = c_loc(this%model%model_simulation%runoff%infil(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('sroff')
        dest = c_loc(this%model%model_simulation%runoff%sroff(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        dest = c_loc(this%model%model_simulation%climate%soil_rechr(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        dest = c_loc(this%model%model_simulation%climate%soil_moist(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
        
        !climateflow module
    case('soil_moist_max')
        dest = c_loc(this%model%model_simulation%climate%soil_moist_max(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('soil_rechr_init_frac')
        dest = c_loc(this%model%model_simulation%climate%soil_rechr_init_frac(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist_init_frac')
        dest = c_loc(this%model%model_simulation%climate%soil_moist_init_frac(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max_frac ')
        dest = c_loc(this%model%model_simulation%climate%soil_rechr_max_frac(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
        
        !intcp module
    case('covden_sum')
        dest = c_loc(this%model%model_simulation%intcp%covden_sum(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('snow_intcp')
        dest = c_loc(this%model%model_simulation%intcp%snow_intcp(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('covden_win')
        dest = c_loc(this%model%model_simulation%intcp%covden_win(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('wrain_intcp')
        dest = c_loc(this%model%model_simulation%intcp%wrain_intcp(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('srain_intcp')
        dest = c_loc(this%model%model_simulation%intcp%srain_intcp(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
        
        !potet module
    case('epan_coef')
        dest = c_loc(this%model%model_simulation%potet%epan_coef(1,1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
        
        !potet_jh module 
    case('jh_coef')
        select type(potet => this%model%model_simulation%potet)
            type is(Potet_jh)
                dest = c_loc(potet%jh_coef(1,1))
                status = this%get_var_grid(name, gridid)
                status = this%get_grid_size(gridid, n_elements)
                call c_f_pointer(dest, dest_flattened, [n_elements])
                do i = 1, size(inds)
                    dest_flattened(inds(i)) = src(i)
                end do
        end select
        bmi_status = BMI_SUCCESS
    case('jh_coef_hru')
        select type(potet => this%model%model_simulation%potet)
            type is(Potet_jh)
                dest = c_loc(potet%jh_coef_hru(1))
                status = this%get_var_grid(name, gridid)
                status = this%get_grid_size(gridid, n_elements)
                call c_f_pointer(dest, dest_flattened, [n_elements])
                do i = 1, size(inds)
                    dest_flattened(inds(i)) = src(i)
                end do
        end select
        bmi_status = BMI_SUCCESS
        
        ! precipitation module
    case('tmax_allrain_offset')
        dest = c_loc(this%model%model_simulation%model_precip%tmax_allrain_offset(1,1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('tmax_allsnow')
        dest = c_loc(this%model%model_simulation%model_precip%tmax_allsnow(1,1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS

    case('adjmix_rain')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                dest = c_loc(model_precip%adjmix_rain(1, 1))
                status = this%get_var_grid(name, gridid)
                status = this%get_grid_size(gridid, n_elements)
                call c_f_pointer(dest, dest_flattened, [n_elements])
                do i = 1, size(inds)
                    dest_flattened(inds(i)) = src(i)
                end do
        end select
        bmi_status = BMI_SUCCESS
    case('rain_cbh_adj')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                dest = c_loc(model_precip%rain_cbh_adj(1, 1))
                status = this%get_var_grid(name, gridid)
                status = this%get_grid_size(gridid, n_elements)
                call c_f_pointer(dest, dest_flattened, [n_elements])
                do i = 1, size(inds)
                    dest_flattened(inds(i)) = src(i)
                end do
        end select
        bmi_status = BMI_SUCCESS
    case('snow_cbh_adj')
        select type(model_precip => this%model%model_simulation%model_precip)
            type is(Precipitation_hru)
                dest = c_loc(model_precip%snow_cbh_adj(1, 1))
                status = this%get_var_grid(name, gridid)
                status = this%get_grid_size(gridid, n_elements)
                call c_f_pointer(dest, dest_flattened, [n_elements])
                do i = 1, size(inds)
                    dest_flattened(inds(i)) = src(i)
                end do
        end select
        bmi_status = BMI_SUCCESS
        
        !runoff
    case('va_open_exp')
        dest = c_loc(this%model%model_simulation%runoff%va_open_exp(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('va_clos_exp')
        dest = c_loc(this%model%model_simulation%runoff%va_clos_exp(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('smidx_coef')
        dest = c_loc(this%model%model_simulation%runoff%smidx_coef(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('smidx_exp')
        dest = c_loc(this%model%model_simulation%runoff%smidx_exp(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('snowinfil_max')
        dest = c_loc(this%model%model_simulation%runoff%snowinfil_max(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
     case('carea_max')
        dest = c_loc(this%model%model_simulation%runoff%carea_max(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('imperv_stor_max')
        dest = c_loc(this%model%model_simulation%runoff%imperv_stor_max(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_flow_coef')
        dest = c_loc(this%model%model_simulation%runoff%dprst_flow_coef(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_seep_rate_clos')
        dest = c_loc(this%model%model_simulation%runoff%dprst_seep_rate_clos(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_depth_avg')
        dest = c_loc(this%model%model_simulation%runoff%dprst_depth_avg(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_area_max')
        dest = c_loc(this%model%model_simulation%runoff%dprst_area_max(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_frac')
        dest = c_loc(this%model%model_simulation%runoff%dprst_frac(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('hru_percent_imperv')
        dest = c_loc(this%model%model_simulation%runoff%hru_percent_imperv(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS

        !snowcomp module
    case('snarea_curve')
        dest = c_loc(this%model%model_simulation%snow%snarea_curve(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('snarea_thresh')
        dest = c_loc(this%model%model_simulation%snow%snarea_thresh(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('rad_trncf')
        dest = c_loc(this%model%model_simulation%snow%rad_trncf(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('cecn_coef')
        dest = c_loc(this%model%model_simulation%snow%cecn_coef(1,1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
        
        !solar radiation module
    case('radmax')
        dest = c_loc(this%model%model_simulation%solrad%radmax(1,1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
        
        !solar_radition_degday module
    case('dday_slope')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
            dest = c_loc(solrad%dday_slope(1,1))
            status = this%get_var_grid(name, gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
        end select
        bmi_status = BMI_SUCCESS
    case('dday_intcp')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
            dest = c_loc(solrad%dday_intcp(1,1))
            status = this%get_var_grid(name, gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
        end select
        bmi_status = BMI_SUCCESS
    case('tmax_index')
        select type(solrad => this%model%model_simulation%solrad)
            type is(Solrad_degday)
            dest = c_loc(solrad%tmax_index(1,1))
            status = this%get_var_grid(name, gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
        end select
        bmi_status = BMI_SUCCESS
        
        !temperature module
    case('tmax_cbh_adj')
        select type(model_temp => this%model%model_simulation%model_temp)
            type is(Temperature_Hru)
            dest = c_loc(model_temp%tmax_cbh_adj(1,1))
            status = this%get_var_grid(name, gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
        end select
        bmi_status = BMI_SUCCESS
    case('tmin_cbh_adj')
        select type(model_temp => this%model%model_simulation%model_temp)
            type is(Temperature_Hru)
            dest = c_loc(model_temp%tmin_cbh_adj(1,1))
            status = this%get_var_grid(name, gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
        end select
        bmi_status = BMI_SUCCESS
    case('transp_tmax')
        select type(transpiration => this%model%model_simulation%transpiration)
            type is(Transp_tindex)
            dest = c_loc(transpiration%transp_tmax(1))
            status = this%get_var_grid(name, gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
        end select
        bmi_status = BMI_SUCCESS

    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_at_indices_float

    ! Set double values at particular locations.
    function prms_set_at_indices_double(this, name, inds, src) &
        result (bmi_status)
    class (bmi_prms_surface), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    double precision, pointer :: dest_flattened(:)
    integer :: i, n_elements, status, gridid

    select case(name)
        !runoff
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = c_loc(this%model%model_simulation%runoff%strm_seg_in(1))
            status = this%get_var_grid(name, gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('dprst_vol_clos')
        dest = c_loc(this%model%model_simulation%runoff%dprst_vol_clos(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('dprst_vol_open')
        dest = c_loc(this%model%model_simulation%runoff%dprst_vol_open(1))
        status = this%get_var_grid(name, gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS

    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_at_indices_double

    !! A non-BMI procedure for model introspection.
    !subroutine print_model_info(this)
    !  class (bmi_prms_surface), intent(in) :: this
    !
    !  call print_info(this%model)
    !end subroutine print_model_info

    end module bmiprmssurface
