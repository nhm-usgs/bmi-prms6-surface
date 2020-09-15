    module m_prms_surface
    use variableKind
    use prms_constants
    ! use, intrinsic :: iso_c_binding, only: c_sizeof
    use, intrinsic :: iso_fortran_env, only: output_unit
    use Control_class, only: Control
    use Simulation_class, only: Simulation
    ! provide access to child class members
    use SOLAR_RADIATION_DEGDAY, only: Solrad_degday
    use PRMS_TEMPERATURE_HRU, only: Temperature_hru
    use PRMS_POTET_JH, only: Potet_jh
    use PRMS_PRECIPITATION_HRU, only: Precipitation_hru
    use PRMS_BASIN, only: Basin
    use PRMS_TRANSP_TINDEX, only: Transp_tindex
    ! use ieee_arithmetic
    ! use ieee_features
    implicit none
    type :: prms_surface_model
        !character(len=:), allocatable :: control_filename
        !! Name of the control file
        type(Control) :: control_data
        !! Class of control file related parameters
        type(Basin) :: parameter_data
        !! Class of input parameters
        type(Simulation) :: model_simulation
        !! PRMS model simulation class

        !! test for accessing Solrad_degday
        !type(Solrad_degday) :: model_solrad_deg_day
        !type(Temperature_hru) :: model_temperature_hru
        
        integer(i64) :: start_rtc
        !! Starting system clock value
        integer(i64) :: end_rtc
        !! Ending system clock value
        integer(i64) :: max_rtc
        !! Maximum system clock ticks per second
        integer(i64) :: rate_rtc
        !! System clock ticks per second
        real(i64) :: delta_rtc_sec
        !! Elapsed system clock in seconds
        real(r64) :: start_ct
        !! Starting cpu time value
        real(r64) :: end_ct
        !! Ending cpu time value
        real(r64) :: dummy_r64
        real(r32) :: dummy_r32
        logical :: bmitmax, bmitmin, bmiprcp
    end type prms_surface_model

    contains

    subroutine initialize_from_file(model, config_file)
    use variableKind
    use prms_constants
    use Control_class, only: Control
    !use Parameters_class, only: Parameters
    use Simulation_class, only: Simulation
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    character (len=*), intent(in) :: config_file
    type(prms_surface_model), target, intent(out) :: model
    real(r64) :: dummy_r64
    real(r32) :: dummy_r32
    associate(start_rtc => model%start_rtc, end_rtc => model%end_rtc, max_rtc => model%max_rtc, &
        rate_rtc => model%rate_rtc, delta_rtc_sec => model%delta_rtc_sec,  &
        start_ct => model%start_ct, end_ct => model%end_ct, &
        control_data => model%control_data, &
        parameter_data => model%parameter_data, &
        model_simulation => model%model_simulation)
        ! , control_filename => model%control_filename)
        call system_clock(count=start_rtc, count_rate=rate_rtc, count_max=max_rtc)
        call cpu_time(time=start_ct)

        print *, 'CLOSEZERO, NEARZERO, DNEARZERO'
        print *, CLOSEZERO, NEARZERO, DNEARZERO

        print *, 'Ranges'
        print *, 'r32: ', range(dummy_r32)
        print *, 'smallest r32 value: ', tiny(dummy_r32)
        print *, 'minexponent of r32: ', minexponent(dummy_r32)
        print *, 'r64: ', range(dummy_r64)

        ! print *, 'r32 array memory footprint (109951 x 13505):', c_sizeof(arr_dummy_r32)
        write(output_unit, fmt='(a)') repeat('=', 72)
        !call get_control_filename(control_filename)

        !control_data = Control(config_file)
        call Control_data%init(config_file)

        ! TODO: Other stuff to consider
        ! - variable: kkiter; Current iteration in GSFLOW simulation (when model_mode=GSFLOW)
        ! - code behavior when init_vars_from_file==1
        ! * how to handle having different combinations of physics modules


        ! TODO: How to handle allocation and reading of parameter variables depending
        !       on which physics modules are selected?
        !parameter_data = Parameters(Control_data)

        ! TODO: Need routines for setting up output variables


        ! Initialize the simulation object
        !model_simulation = Simulation(Control_data, Parameter_data)
        call model_simulation%init(Control_data)
        
        ! 2019-08-08 PAN: This is rather kludgy...
        ! Close the parameter file
        call Control_data%param_file_hdl%close()
        
        !initialize logicals to track climate imput
        model%bmitmax = .false. 
        model%bmitmin = .false.
        model%bmiprcp = .false.

        write(output_unit, fmt='(a)') repeat('=', 72)
    end associate
    end subroutine

    subroutine cleanup(model)
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    type(prms_surface_model), intent(inout) :: model
    associate(start_rtc => model%start_rtc, end_rtc => model%end_rtc, max_rtc => model%max_rtc, &
        rate_rtc => model%rate_rtc, delta_rtc_sec => model%delta_rtc_sec,  &
        start_ct => model%start_ct, end_ct => model%end_ct, &
        model_simulation => model%model_simulation, &
        control_data => model%control_data)

        ! Cleanup everything
        write(output_unit, fmt='(a)') repeat('-', 72)
        write(output_unit, fmt='(a)') 'Cleaning up...'
        write(output_unit, fmt='(a)') repeat('-', 72)
        call model%model_simulation%cleanup(Control_data)

        call cpu_time(time=end_ct)
        call system_clock(count=end_rtc)

        if (Control_data%print_debug%value > -1) then
            delta_rtc_sec = real(end_rtc - start_rtc, r64) / real(rate_rtc, r64)

            write(output_unit, fmt='(a)') repeat('-', 72)
            write(output_unit, fmt='(a, 1x, f16.4, 1x, a)') 'Elapsed system clock:', delta_rtc_sec, 'seconds.'
            write(output_unit, fmt='(a, 1x, f16.4, 1x, a)') 'Elapsed cpu time:', end_ct - start_ct, 'seconds.'
        endif
        
    end associate
    end subroutine cleanup

    subroutine advance_in_time(model)
    implicit none
    type(prms_surface_model), intent(inout) :: model
    type(Control) :: ctl_data
    type(Simulation) :: this
    logical :: res
    associate(this=>model%model_simulation, &
                ctl_data => model%control_data)
        !if (.not. this%model_time%next(ctl_data, this%model_basin)) then
        !    call cleanup(model)
        !else
        !    call solve_prms(model) 
        !endif
    if(this%model_time%next()) then
        call solve_prms(model)
    endif
    !always assume prms reads climate forcing from .nc file
    model%bmitmax = .false.
    model%bmitmin = .false.
    model%bmiprcp = .false.
    end associate
    
    end subroutine advance_in_time
    
    subroutine solve_prms(model)
    use iso_fortran_env, only: output_unit
    implicit none
    type(prms_surface_model), intent(inout) :: model
    !! Name of the control file
    type(Control) :: ctl_data
    !! Class of control file related parameters
    !type(Parameters) :: param_data
    !! Class of input parameters
    type(Simulation) :: this
    !! PRMS model simulation class
    associate(  this =>model%model_simulation, &
        ctl_data => model%control_data)
        !if (.not. this%model_time%next(ctl_data, this%model_basin)) exit
        ! print *, this%model_time%Nowyear, this%model_time%Nowmonth, this%model_time%Nowday

        call this%model_basin%run(ctl_data, this%model_time)
        
        call this%model_temp%run(ctl_data, this%model_basin, this%model_time, this%model_summary, &
            model%bmitmax, model%bmitmin)
        ! print *, '1'
        call this%model_precip%run(ctl_data, this%model_basin, this%model_temp, this%model_time, &
            this%model_summary, model%bmiprcp)
        ! call this%climate_by_hru%run(ctl_data, param_data, this%model_time, &
        !                              this%model_basin, this%potet, this%model_temp, &
        !                              this%climate)
        ! print *, '2'
        call this%solrad%run(ctl_data, this%model_time, &
                             this%model_precip, this%model_basin, this%model_temp)

        ! print *, '3'
        call this%transpiration%run(ctl_data, this%model_time, &
                                    this%model_basin, this%model_temp)

        ! print *, '4'
        call this%potet%run(ctl_data, this%model_basin, this%model_time, &
                           this%solrad, this%model_temp)

        ! print *, '5'
        call this%intcp%run(ctl_data, this%model_basin, this%potet, &
                            this%model_precip, this%transpiration, this%climate, this%model_time)

        ! print *, '6'
        call this%snow%run(ctl_data, this%model_basin, this%model_time, this%climate, &
                           this%model_precip, this%model_temp, &
                           this%intcp, this%solrad, this%potet, this%transpiration)

        ! print *, '7'
        call this%runoff%run(ctl_data, this%model_basin, this%climate, &
                             this%potet, this%intcp, this%snow, this%model_time)

        ! print *, '8'

        if (ctl_data%outVarON_OFF%value == 1) then
          call this%model_summary%run(ctl_data, this%model_time, this%model_basin)
        end if
        
        if (ctl_data%print_debug%value == 1) then
          call this%model_waterbal%run(ctl_data, this%model_basin, &
                                       this%climate, this%groundwater, this%intcp, &
                                       this%model_precip, this%snow, this%soil, &
                                       this%runoff, this%model_time)
        endif
        end associate
    end subroutine solve_prms
    end module m_prms_surface
