#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_output_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about what output has been requested and sets up
!   the output profiles.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

PRIVATE ! private scope by default
PUBLIC init_output

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE init_output(nml_dir)

USE io_constants, ONLY: max_sdf_name_len, max_var_file, namelist_unit

USE datetime_mod, ONLY: datetime_str_len, datetime, datetime_from_string

USE string_utils_mod, ONLY: to_string

USE model_time_mod, ONLY: timestep_len, main_run_start, main_run_end

USE model_interface_mod, ONLY: identifier_len

USE output_mod, ONLY: dump_period, dump_period_unit,                           &
                      dump_period_year, dump_period_time,                      &
                      output_dir, run_id,                                      &
                      register_output_profile

USE init_river_out_grid_mod, ONLY: init_river_out_grid

USE jules_rivers_mod, ONLY: l_outflow_per_river

USE logging_mod, ONLY: log_info, log_warn, log_error, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

USE jules_print_mgr, ONLY: jules_message

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads information about what output has been requested and sets up
!   the output profiles.
!-----------------------------------------------------------------------------

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! Work variables
LOGICAL :: dir_exists  ! Used to check existence of output directory

TYPE(datetime) :: output_start_dt, output_end_dt
                       ! Datetime objects created from strings given in
                       ! output profiles

INTEGER :: i  ! Index variable.

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

LOGICAL :: any_river_out  ! Indicates that river grid output is required

LOGICAL :: l_outflow_per_river_check = .FALSE.
    ! Check that both ancillary and diagnostic/send field has been specified.

!-----------------------------------------------------------------------------
! Definition of the jules_output namelist
!-----------------------------------------------------------------------------
INTEGER :: nprofiles  ! The number of output profiles

NAMELIST  / jules_output /                                                     &
  output_dir, run_id, nprofiles, dump_period, dump_period_unit

!-----------------------------------------------------------------------------
! Definition of the JULES_OUTPUT_PROFILE namelist
!-----------------------------------------------------------------------------
CHARACTER(LEN=max_sdf_name_len) :: profile_name  ! The name of the profile
LOGICAL :: output_initial ! T - this profile should output initial data
                          !     for each section it is outputting
                          ! F - this profile should not output initial data
LOGICAL :: output_spinup  ! T - generate output during spinup
                          ! F - don't generate output during spinup
LOGICAL :: output_main_run  ! T - generate output for the part of
                            !     the main run specified by
                            !     output_start and output_end
                            ! F - don't generate output for any
                            !     of the main run
LOGICAL :: l_land_frac      ! T - Gridbox land fraction to be output
CHARACTER(LEN=datetime_str_len) :: output_start, output_end
                                      ! The start and end times for output
                                      ! during the main run as strings
                                      ! If not given, output_start is assumed
                                      ! to be the start of the main run and
                                      ! output_end is assumed to be the end
                                      ! of the main run
INTEGER :: output_period
                                      ! The period of output. The default is
                                      ! to output every timestep.
                                      ! This can be a special period
INTEGER :: sample_period
                                      ! The period for sampling data (s). The
                                      ! default is to sample every timestep.
                                      ! Must be a multiple of the model
                                      ! timestep length and a factor of the
                                      ! output period.
INTEGER :: file_period                ! The period for new files - unless this
                                      ! is a "special period" it is ignored.

INTEGER :: nvars  ! The number of variables in the output profile
CHARACTER(LEN=identifier_len) :: var(max_var_file)
                      ! The model identifiers for variables in the output
                      ! profile
CHARACTER(LEN=identifier_len) :: var_name(max_var_file)
                      ! The name to use for variables in output files
CHARACTER(LEN=1) :: output_type(max_var_file)
                      ! The type of output to use - this should be one of
                      ! 'S', 'A', 'M', 'N' or 'X'

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_OUTPUT'

NAMELIST  /jules_output_profile/ profile_name, output_initial, output_spinup,  &
                                output_main_run, output_start, output_end,     &
                                output_period, sample_period, file_period,     &
                                nvars, var, var_name, output_type, l_land_frac
!end of header
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
nprofiles = 0

any_river_out = .FALSE.

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'output.nml'),               &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file output.nml " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Read the JULES_OUTPUT namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_OUTPUT namelist...")
READ(namelist_unit, NML = jules_output, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_OUTPUT " //                     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

!-----------------------------------------------------------------------------
! Process the information from the JULES_OUTPUT namelist
!-----------------------------------------------------------------------------
! First check that a run_id was given
IF ( LEN_TRIM(run_id) == 0 ) CALL log_fatal(RoutineName, "No run_id given")

! Check that the output directory exists
! The Intel compiler requires a different form of the statement for directories,
! which we swap in with an ifdef
#if defined(INTEL_FORTRAN)
INQUIRE(directory = output_dir, EXIST = dir_exists)
#else
INQUIRE(FILE = output_dir, EXIST = dir_exists)
#endif
IF ( .NOT. dir_exists ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Output directory does not exist - " // TRIM(output_dir))
END IF

! Ensure dump_period is non-zero
IF ( dump_period == 0 ) dump_period = 1

! If dump_period_unit not recognised, use 'Y'
IF ( .NOT. ANY(dump_period_unit == [dump_period_year, dump_period_time]) ) THEN
  CALL log_warn(RoutineName,                                                   &
    "Using default dump_period_unit=" // dump_period_year //                   &
    " - as invalid value given (" // dump_period_unit // ")")
  dump_period_unit = dump_period_year
END IF

! Warn if no output is requested
! We do this after the directory check as we will always produce dumps
IF ( nprofiles < 1 ) THEN
  CALL log_warn(RoutineName,                                                   &
                "No output profiles given - output will not be " //            &
                "generated for this run")
  RETURN
END IF

!-----------------------------------------------------------------------------
! Read and process information about each output profile in turn
!-----------------------------------------------------------------------------
DO i = 1,nprofiles
  ! Set namelist values to their defaults before reading the next profile
  profile_name    = ""
  output_initial  = .FALSE.
  output_spinup   = .FALSE.
  output_main_run = .FALSE.
  l_land_frac     = .FALSE.
  output_start    = ""
  output_end      = ""
  output_period   = timestep_len
  sample_period   = timestep_len
  file_period     = 0
  nvars           = 0
  var(:)          = ''
  var_name(:)     = ''
  output_type(:)  = ''

  ! Read the namelist
  CALL log_info(RoutineName, "Reading JULES_OUTPUT_PROFILE namelist...")
  READ(namelist_unit, NML = jules_output_profile, IOSTAT = ERROR,              &
       IOMSG = iomessage)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Error reading namelist JULES_OUTPUT_PROFILE " //           &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")
  END IF

  !--------------------------------------------------------------------------
  ! Check that the variables requested are suitable for the current
  ! configuration.
  !---------------------------------------------------------------------------
  CALL check_output_vars( nvars, var, var_name, output_type, any_river_out,    &
                          l_outflow_per_river_check )

  !---------------------------------------------------------------------------
  ! If this profile is not providing any variables, we can skip it
  !---------------------------------------------------------------------------
  IF ( nvars < 1 ) THEN
    CALL log_error(RoutineName,                                                &
                   "Profile " // TRIM(profile_name) // " is not " //           &
                   "outputting any variables - ignoring")
    CYCLE
  END IF

  !---------------------------------------------------------------------------
  ! Set up the datetime objects for output start and end
  ! If not given, they default to start and end of main run respectively
  !---------------------------------------------------------------------------
  IF ( LEN_TRIM(output_start) == 0 ) THEN
    output_start_dt = main_run_start
  ELSE
    output_start_dt = datetime_from_string(output_start)
  END IF
  IF ( LEN_TRIM(output_end) == 0 ) THEN
    output_end_dt = main_run_end
  ELSE
    output_end_dt = datetime_from_string(output_end)
  END IF

  !---------------------------------------------------------------------------
  ! Register the output profile - this performs more error checking
  !---------------------------------------------------------------------------
  CALL register_output_profile(profile_name, output_initial,                   &
                               output_spinup, output_main_run, l_land_frac,    &
                               output_start_dt, output_end_dt,                 &
                               output_period, sample_period, file_period,      &
                               var(1:nvars), var_name(1:nvars),                &
                               output_type(1:nvars))

END DO
! If riv_number_file specified in error reset switch as no need to output
! rivers_outflow_rp to dump and could also probably reduce the allocation of
! rivers%rivers_outflow_number_rp to nominal size.
IF ( .NOT. l_outflow_per_river_check .AND. l_outflow_per_river ) THEN
  WRITE(jules_message,*)                                                       &
     "outflow_per_river not requested; " //                                    &
     "river number ancillary specified in error."
  CALL log_warn(RoutineName, jules_message)
  l_outflow_per_river = .FALSE.
END IF

!------------------------------------------------------------------------------
! Initialise the river output grid if required
!------------------------------------------------------------------------------
IF ( any_river_out ) THEN
  CALL init_river_out_grid()
END IF

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file output.nml " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

RETURN

END SUBROUTINE init_output

!-----------------------------------------------------------------------------

SUBROUTINE check_output_vars( nvars, var, var_name, output_type,               &
                              any_river_out, l_outflow_per_river_check )

USE ancil_info, ONLY: nsoilt

USE jules_model_environment_mod, ONLY: l_oasis_rivers

USE jules_snow_mod, ONLY: nsmax

USE jules_soil_biogeochem_mod, ONLY: soil_model_ecosse, soil_model_4pool,      &
      soil_bgc_model, l_label_frac_cs

USE jules_soil_ecosse_mod, ONLY: l_soil_N

USE jules_surface_mod, ONLY: l_urban2t

USE jules_vegetation_mod, ONLY: l_fapar_diag, l_fao_ref_evapotranspiration,    &
    photo_acclim_model, l_sugar, stomata_model, stomata_sox

USE jules_water_resources_mod, ONLY: l_have_groundwater, l_have_renew_gwater,  &
    l_have_surface_water, l_water_domestic, l_water_environment,               &
    l_water_industry, l_water_irrigation, l_water_livestock,                   &
    l_water_resources, l_water_transfers, no_model, nr_gwater_model

USE jules_rivers_mod, ONLY: l_minor_reservoirs, l_outflow_per_river, l_rivers, &
    l_riv_overbank, i_river_vn, rivers_camaflood, rivers_rfm, rivers_trip

USE jules_deposition_mod, ONLY: l_deposition, l_deposition_flux

USE logging_mod, ONLY: log_warn, log_info, log_fatal

USE model_interface_mod, ONLY: is_river_var

USE sf_diags_mod, ONLY: sf_diag

USE jules_urban_mod, ONLY: l_moruses

USE jules_print_mgr, ONLY: jules_message

USE string_utils_mod, ONLY: to_string

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks that all variables selected for output are available with the
!   current configuration, and removes any that are not. Also sets flags
!   if certain diagnostics are selected.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) ::                                                     &
  nvars
    ! The number of variables in the output profile.

CHARACTER(LEN=*), INTENT(IN OUT) ::                                            &
  var(:),                                                                      &
    ! The model identifiers for variables in the output profile.
  var_name(:),                                                                 &
    ! The name to use for variables in output files.
  output_type(:)
    ! The type of output for each variable.

LOGICAL, INTENT(IN OUT) ::                                                     &
  any_river_out,                                                               &
    ! Indicates if there is any river output required
  l_outflow_per_river_check
    ! Check that both ancillary and diagnostic/send field has been specified.

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  j,                                                                           &
    ! Index variable.
  nvars_in
    ! The number of variables that were requested for the profile currently
    ! being processed, before filtering.

LOGICAL ::                                                                     &
  remove_var
    ! Indicates if the current variable should be removed from the list as it
    ! is not allowed.

CHARACTER(LEN=200) :: message  !  Diagnostic message.

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_OUTPUT_VARS'

!end of header
!-----------------------------------------------------------------------------

!---------------------------------------------------------------------------
! First check output variables requiring extra ancillaries.
!---------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  ! Rivers is only called on the master task
  IF ( l_outflow_per_river ) THEN
    IF ( ANY( var(:) == 'outflow_per_river' ) .OR. l_oasis_rivers ) THEN
      l_outflow_per_river_check = .TRUE.
    END IF
  ELSE IF ( ANY( var(:) == 'outflow_per_river' ) ) THEN
    WRITE(jules_message,*)                                                     &
       'outflow_per_river requested without a river number ancillary specified.'
    CALL log_fatal(RoutineName, jules_message)
  END IF
END IF

!-----------------------------------------------------------------------------
! Remove any variables that are not allowed for the current configuration,
! warning about their removal. A variable can be flagged for removal by more
! than one section below, but only the last message will be reported.
!-----------------------------------------------------------------------------
nvars_in = nvars
nvars    = 0

DO j = 1,nvars_in
  ! Assume that we are going to keep this variable until we decide otherwise.
  remove_var = .FALSE.
  message    = ''

  !---------------------------------------------------------------------------
  ! Check if this variable is allowed for the current configuration.
  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! Coordinates are automatically included in output files and should not be
  ! requested separately.
  !---------------------------------------------------------------------------
  SELECT CASE ( var(j) )
  CASE ( 'latitude', 'longitude', 'projection_x_coord', 'projection_y_coord',  &
         'rivers_lat_rp', 'rivers_lon_rp', 'rivers_x_coord_rp',                &
         'rivers_y_coord_rp' )
    remove_var = .TRUE.
    message    = 'Coordinates are automatically included in files.'
  END SELECT

  !---------------------------------------------------------------------------
  ! Multilayer snow variables, and those derived from them, need nsmax>=1.
  !---------------------------------------------------------------------------
  IF ( nsmax < 1 ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'snow_ice_gb', 'snow_ice_tile',                                     &
           'snow_liq_gb', 'snow_liq_tile',                                     &
           'rgrainl', 'snow_ds', 'snow_ice', 'snow_liq', 'tsnow' )
      remove_var = .TRUE.
      message    = 'Multi-layer snow model not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! The gridbox mean of a soil-tiled variable in the case of nsoilt>1 is
  ! generally not allowed at present. In future we might decide to allow the
  ! calculation of some of these.
  !---------------------------------------------------------------------------
  IF ( nsoilt > 1 ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'b', 'c_bio', 'c_bio_gb', 'c_dpm', 'c_dpm_gb', 'c_hum',             &
           'c_hum_gb', 'c_rpm', 'c_rpm_gb', 'clay', 'cs', 'cs_gb',             &
           'depth_frozen', 'depth_frozen_sthf', 'depth_unfrozen',              &
           'depth_unfrozen_sthf', 'drain', 'esoil_gb', 'fch4_wetl',            &
           'fch4_wetl_cs', 'fch4_wetl_npp', 'fch4_wetl_resps', 'fprf',         &
           'frac_irrig', 'fsat', 'fsth', 'ftemp', 'fwetl', 'hcap', 'hcon',     &
           'n_amm', 'n_amm_gb', 'n_inorg_gb', 'n_nit', 'n_nit_gb',             &
           'n_soil_gb', 'qbase', 'qbase_zw', 'resp_s', 'resp_s_gb',            &
           'sat_excess_roff',  'satcon', 'sathh', 'sm_crit', 'sm_sat',         &
           'sm_wilt', 'smc_avail_top', 'smc_avail_tot', 'soil_CN',             &
           'soil_CN_gb', 'soil_ph', 'soil_wet', 'sthf', 'sthu', 'sthu_irr',    &
           'sthzw', 'swet_liq_tot', 'swet_tot', 'zw' )
      remove_var = .TRUE.
      message    = 'Gridbox mean of a soil-tiled variable is not ' //          &
                   'available with nsoilt>1.'
    END SELECT

    SELECT CASE ( var(j) )
    CASE ( 'n_leach' )
      IF ( soil_bgc_model == soil_model_4pool ) THEN
        remove_var = .TRUE.
        message    = 'Gridbox mean of a soil-tiled variable is not ' //        &
                     'available with nsoilt>1.'
      END IF
    END SELECT

  END IF  !  nsoilt>1

  !---------------------------------------------------------------------------
  ! Variables that are only allowed with a multi-pool soil C model.
  !---------------------------------------------------------------------------
  IF ( soil_bgc_model /= soil_model_4pool .AND.                                &
       soil_bgc_model /= soil_model_ecosse ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'c_bio',    'c_dpm',    'c_hum',    'c_rpm',                        &
           'c_bio_gb', 'c_dpm_gb', 'c_hum_gb', 'c_rpm_gb' )
      remove_var = .TRUE.
      message    = 'A suitable multi-pool soil C model is not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Variables that are only allowed with a soil N model.
  !---------------------------------------------------------------------------
  IF ( soil_bgc_model /= soil_model_4pool .AND.                                &
       .NOT. ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'n_bio',      'n_dpm',     'n_hum',    'n_rpm',                     &
           'n_bio_gb',   'n_dpm_gb',  'n_hum_gb', 'n_rpm_gb',                  &
           'n_inorg_gb', 'n_soil_gb', 'n_gas_gb', 'n_leach',  'ns', 'ns_gb' )
      remove_var = .TRUE.
      message    = 'A suitable soil N model is not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  !  ECOSSE variables - only allowed if ECOSSE selected.
  !  In this section we test variables that are always used with ECOSSE.
  !---------------------------------------------------------------------------
  IF ( soil_bgc_model /= soil_model_ecosse ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'plant_input_c_gb', 'soil_ph' )
      remove_var = .TRUE.
      message    = 'ECOSSE not used.'
    END SELECT
  END IF  !  not ECOSSE

  !---------------------------------------------------------------------------
  !  ECOSSE N variables - only allowed if ECOSSE + soil N selected.
  !---------------------------------------------------------------------------
  IF ( .NOT. ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'n_amm', 'n_amm_gb', 'n_nit', 'n_nit_gb', 'n_soil',                 &
           'no_soil_gb',  'n2_denitrif_gb', 'n2o_denitrif_gb',                 &
           'n2o_nitrif_gb', 'n2o_part_nitrif_gb', 'n2o_soil_gb',               &
           'n_denitrif_gb', 'n_nitrif_gb', 'plant_input_n_gb' )
      remove_var = .TRUE.
      message    = 'ECOSSE with Nitrogen is not being used.'
    END SELECT
  END IF  !  not ECOSSE with N

  !---------------------------------------------------------------------------
  ! Variables that are only allowed when l_label_frac_cs is true
  !---------------------------------------------------------------------------
  IF ( .NOT. l_label_frac_cs ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'cs_label', 'cs_label_gb', 'resp_label_cs', 'resp_label_cs_gb' )
      remove_var = .TRUE.
      message    = 'A fraction of the soil carbon is not labelled'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Variables that are only allowed with overbank inundation.
  !---------------------------------------------------------------------------
  IF ( l_riv_overbank ) THEN
    !-------------------------------------------------------------------------
    ! Variables that require the river output grid
    !-------------------------------------------------------------------------
    SELECT CASE ( var(j) )
    CASE ( 'frac_fplain_rp' )
      any_river_out = .TRUE.
    END SELECT
  ELSE
    SELECT CASE ( var(j) )
    CASE ( 'frac_fplain_lp', 'frac_fplain_rp' )
      remove_var = .TRUE.
      message    = 'Overbank inundation (l_riv_overbank) not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Variables that depend on river routing.
  !---------------------------------------------------------------------------
  IF ( l_rivers ) THEN

    !-------------------------------------------------------------------------
    ! Variables that are only allowed with CaMa-Flood
    !-------------------------------------------------------------------------
    IF ( i_river_vn /= rivers_camaflood ) THEN
      SELECT CASE ( var(j) )

      CASE ( 'channel_bed_elevation', 'channel_depth', 'channel_width',        &
             'flood_flow', 'flood_storage', 'river_channel_flow',              &
             'river_channel_storage', 'river_elevation', 'river_flow' )
        remove_var = .TRUE.
        message    = 'CaMa-Flood river routing not used.'
      END SELECT
    END IF

    !-------------------------------------------------------------------------
    ! Variables that are only allowed with RFM
    !-------------------------------------------------------------------------
    IF ( i_river_vn /= rivers_rfm ) THEN
      SELECT CASE ( var(j) )
      CASE ( 'rfm_bflowin_rp', 'rfm_flowin_rp','rfm_substore_rp',              &
             'rfm_surfstore_rp' )
        remove_var = .TRUE.
        message    = 'RFM river routing not used.'
      END SELECT
    END IF

    !-------------------------------------------------------------------------
    ! Variables that are only allowed with TRIP
    !-------------------------------------------------------------------------
    IF ( i_river_vn /= rivers_trip ) THEN
      SELECT CASE ( var(j) )
      CASE ( 'rivers_sto_rp' )
        remove_var = .TRUE.
        message    = 'TRIP river routing not used.'
      END SELECT
    END IF

    !-------------------------------------------------------------------------
    ! Detect variables that require the river output grid
    !-------------------------------------------------------------------------
    IF ( is_river_var(var(j)) ) THEN
      any_river_out = .TRUE.
    END IF

  ELSE

    !-------------------------------------------------------------------------
    ! Rivers are not selected.
    ! Check for variables that are only allowed with river routing.
    ! First we check for variables on the river output grid (which allow us to
    ! shortern the list by omitting those), then by name for any other river
    ! variable.
    !-------------------------------------------------------------------------
    IF ( is_river_var(var(j)) ) THEN
      remove_var = .TRUE.
      message    = 'River routing (l_rivers) not used.'
    ELSE
      SELECT CASE ( var(j) )
      CASE ( 'rflow', 'rrun' )
        remove_var = .TRUE.
        message    = 'River routing (l_rivers) not used.'
      END SELECT
    END IF  !  is_river_var

  END IF  !  l_rivers

  !----------------------------------------------------------------------------
  ! River 2D ancillary variables (including for overbank inundation) - these
  ! can be read in but not output (because code for 2D river grid outputs does
  ! not exist).
  !----------------------------------------------------------------------------
  SELECT CASE ( var(j) )
  CASE (  'area', 'channel_depth_grid', 'channel_width_grid',                  &
          'direction', 'hypso_quantiles_grid', 'latitude_2d', 'logn_mean',     &
          'logn_stdev', 'longitude_2d', 'mean_sea_level_grid',                 &
          'minor_res_capacity_grid', 'minor_res_frac_grid',                    &
          'river_distance_grid', 'river_elevation_grid', 'river_length_grid',  &
          'river_manning_grid', 'river_nextx_grid', 'river_nexty_grid',        &
          'rivers_xgrid', 'rivers_ygrid', 'sequence' )
    remove_var = .TRUE.
    message    = 'Variables on the full river grid are not available ' //      &
                 'for output.'
  END SELECT

  !----------------------------------------------------------------------------
  ! Variables that are only allowed with minor reservoirs.
  !----------------------------------------------------------------------------
  IF ( .NOT. l_minor_reservoirs ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'minor_res_capacity', 'minor_res_frac', 'minor_res_storage' )
      remove_var = .TRUE.
      message    = 'Minor reservoirs not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Thermal acclimation variables.
  !---------------------------------------------------------------------------
  IF ( photo_acclim_model == 0 ) THEN
    SELECT CASE ( var(j) )
    CASE (  't_home_gb', 't_growth_gb' )
      remove_var = .TRUE.
      message    = 'Thermal acclimation (photo_acclim_model) not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Water resource variables.
  !---------------------------------------------------------------------------
  ! Water resource variables that only require l_water_resources=T.
  IF ( .NOT. l_water_resources ) THEN
    SELECT CASE ( var(j) )
    CASE (  'conv_loss_frac', 'conveyance_loss', 'water_demand',               &
            'water_demand_unmet', 'water_removed' )
      remove_var = .TRUE.
      message    = 'Water resources (l_water_resources) not selected.'
    END SELECT
  END IF

  ! Water resource variables that also require another switch.

  ! Sector-specific switches (such as l_water_domestic) are FALSE if
  ! l_water_resources=F, so we don't need to test l_water_resources here.
  IF ( .NOT. l_water_domestic ) THEN
    SELECT CASE ( var(j) )
    CASE (  'demand_domestic', 'demand_rate_domestic', 'unmet_domestic' )
      remove_var = .TRUE.
      message    = 'l_water_domestic not selected.'
    END SELECT
  END IF

  IF ( .NOT. l_water_environment ) THEN
    SELECT CASE ( var(j) )
    CASE (  'demand_environment', 'unmet_environment' )
      remove_var = .TRUE.
      message    = 'l_water_environment not selected.'
    END SELECT
  END IF

  IF ( .NOT. l_water_industry ) THEN
    SELECT CASE ( var(j) )
    CASE (  'demand_industry', 'demand_rate_industry', 'unmet_industry' )
      remove_var = .TRUE.
      message    = 'l_water_industry not selected.'
    END SELECT
  END IF

  IF ( .NOT. l_water_irrigation ) THEN
    SELECT CASE ( var(j) )
    CASE (  'demand_irrigation', 'grid_area', 'unmet_irrigation' )
      remove_var = .TRUE.
      message    = 'l_water_irrigation not selected.'
    END SELECT
  END IF

  IF ( .NOT. l_water_livestock ) THEN
    SELECT CASE ( var(j) )
    CASE (  'demand_livestock', 'demand_rate_livestock','unmet_livestock' )
      remove_var = .TRUE.
      message    = 'l_water_livestock not selected.'
    END SELECT
  END IF

  IF ( .NOT. l_water_transfers ) THEN
    SELECT CASE ( var(j) )
    CASE (  'demand_transfers', 'unmet_transfers' )
      remove_var = .TRUE.
      message    = 'l_water_transfers not selected.'
    END SELECT
  END IF

  IF ( .NOT. l_have_renew_gwater ) THEN
    SELECT CASE ( var(j) )
    CASE (  'gw_abstracted', 'gw_avail' )
      remove_var = .TRUE.
      message    = 'No (renewable) groundwater is selected.'
    END SELECT
  END IF

  IF ( .NOT. l_water_resources .OR.                                            &
       ( l_water_resources .AND. nr_gwater_model == no_model ) ) THEN
    SELECT CASE ( var(j) )
    CASE (  'gw_nr_abstracted' )
      remove_var = .TRUE.
      message    = 'Non-renewable groundwater is represented.'
    END SELECT
  END IF

  IF ( .NOT. l_have_surface_water ) THEN
    SELECT CASE ( var(j) )
    CASE (  'sw_abstracted', 'sw_avail' )
      remove_var = .TRUE.
      message    = 'No surface water is selected.'
    END SELECT
  END IF

  IF ( .NOT. l_have_groundwater .OR. .NOT. l_have_surface_water ) THEN
    SELECT CASE ( var(j) )
    CASE (  'sfc_water_frac' )
      remove_var = .TRUE.
      message    = 'sfc_water_frac is not being used.'
    END SELECT
  END IF

  IF ( .NOT. l_water_resources .OR. .NOT. l_minor_reservoirs ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'minor_res_abstracted' )
      remove_var = .TRUE.
      message    = 'Water resources + minor reservoirs not used.'
    END SELECT
  END IF

  IF ( .NOT. l_water_resources .OR. .NOT. l_rivers ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'river_abstracted' )
      remove_var = .TRUE.
      message    = 'Water resources + rivers not used.'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Variables that are only allowed with atmospheric deposition
  !---------------------------------------------------------------------------
  IF ( .NOT. l_deposition ) THEN
    ! Remove deposition diagnostics if l_deposition = FALSE
    SELECT CASE ( var(j) )
    CASE ( 'deposition_ra', 'deposition_rb', 'deposition_rc',                  &
           'deposition_rc_stom', 'deposition_rc_nonstom',                      &
           'deposition_vd', 'deposition_lossrate', 'deposition_flux',          &
           'deposition_conc', 'level_separation', 'deposition_vd_gb')
      remove_var = .TRUE.
      message    = 'Atmospheric deposition is not selected.'
    END SELECT
  ELSE
    ! Remove deposition flux diagnostics if l_deposition = TRUE and
    ! l_deposition_flux = FALSE
    IF ( .NOT. l_deposition_flux ) THEN
      SELECT CASE ( var(j) )
      CASE ( 'deposition_flux', 'deposition_conc' )
        remove_var = .TRUE.
        message    = 'Atmospheric deposition flux is not selected.'
      END SELECT
    END IF  !  not Atmospheric Deposition
  END IF

  !---------------------------------------------------------------------------
  ! Urban morphology
  ! These are calculated when l_urban_empirical otherwise they will be the same
  ! as the input value.
  ! wrr   Repeating width ratio (or canyon fraction, W/R)
  ! hwr   Height-to-width ratio (H/W)
  ! hgt   Building height (H)
  !---------------------------------------------------------------------------
  SELECT CASE ( var(j) )
  CASE (  'hwr', 'hgt' )
    IF ( .NOT. l_moruses ) THEN
      remove_var = .TRUE.
      message    = 'No l_moruses_* parameterisations selected.'
    END IF
  CASE (  'wrr' )
    IF ( .NOT. l_urban2t ) THEN
      remove_var = .TRUE.
      message    = 'l_urban2t not selected.'
    END IF
  END SELECT

  !---------------------------------------------------------------------------
  ! Variables that are only allowed when l_sugar is true
  !---------------------------------------------------------------------------
  IF ( .NOT. l_sugar ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'f_nsc', 'growth_sug' )
      remove_var = .TRUE.
      message    = 'l_sugar not selected'
    END SELECT
  END IF

  !---------------------------------------------------------------------------
  ! Variables that only allowed when SOX is used
  !---------------------------------------------------------------------------
  IF ( stomata_model /= stomata_sox ) THEN
    SELECT CASE ( var(j) )
    CASE ( 'lwp_c' )
      remove_var = .TRUE.
      message    = 'SOX (stomata_model = 3) is not selected'
    END SELECT
  END IF

  IF ( remove_var ) THEN
    ! If the variable needs to be removed, issue a warning that we are doing
    ! this.
    CALL log_warn(RoutineName,                                                 &
                   "Variable " // TRIM(var(j)) // " is not available for " //  &
                   "output with the current configuration: "               //  &
                   TRIM(message) // " Removing from output.")
  ELSE
    ! Otherwise add the variable to the collapsed list - we can do this since
    ! nvars <= j so we are not overwriting unprocessed data
    nvars              = nvars + 1
    var(nvars)         = var(j)
    var_name(nvars)    = var_name(j)
    output_type(nvars) = output_type(j)
  END IF

END DO  !  variables

!-----------------------------------------------------------------------------
! Check for specific variables that require a flag to be set.
!-----------------------------------------------------------------------------
DO j = 1,nvars

  SELECT CASE ( var(j) )

    !---------------------------------------------------------------------------
    ! First we consider diagnostics found in sf_diags.
    !---------------------------------------------------------------------------
  CASE ( 'tau_gb' )
    sf_diag % l_tau_1 = .TRUE.

  CASE ( 'tau' )
    sf_diag % l_tau_surft = .TRUE.

  CASE ( 'et_stom' )
    sf_diag % l_et_stom_surft = .TRUE.

  CASE ( 'et_stom_gb' )
    sf_diag % l_et_stom = .TRUE.

  CASE ( 'fprf' )
    sf_diag % l_fprf = .TRUE.

  CASE ( 'fsth' )
    sf_diag % l_fsth = .TRUE.

  CASE ( 'ftemp' )
    sf_diag % l_ftemp = .TRUE.

  CASE ( 'latent_heat' )
    sf_diag % slh = .TRUE.

  CASE ( 'lw_down_surft', 'lw_up_surft', 'lw_net', 'lw_up' )
    sf_diag % l_lw_surft = .TRUE.

  CASE ( 'mu10m_n' )
    sf_diag % l_mu10m_n = .TRUE.

  CASE ( 'mv10m_n' )
    sf_diag % l_mv10m_n = .TRUE.

  CASE ( 'q1p5m', 'q1p5m_gb' )
    sf_diag % sq1p5 = .TRUE.

  CASE ( 'snice_smb_surft', 'snice_m_surft', 'snice_freez_surft',              &
         'snice_sicerate_surft', 'snice_sliqrate_surft',                       &
         'snice_runoff_surft' )
    sf_diag % l_snice = .TRUE.

  CASE ( 'snomlt_surf_htf' )
    sf_diag % smlt = .TRUE.

  CASE ( 't1p5m', 't1p5m_gb' )
    sf_diag % st1p5 = .TRUE.

  CASE ( 'u10m' )
    sf_diag % su10 = .TRUE.

  CASE ( 'u10m_n' )
    sf_diag % l_u10m_n = .TRUE.

  CASE ( 'v10m' )
    sf_diag % sv10 = .TRUE.

  CASE ( 'v10m_n' )
    sf_diag % l_v10m_n = .TRUE.

    !---------------------------------------------------------------------------
    ! Below here for diagnostics that are not in sf_diags.
    !---------------------------------------------------------------------------
  CASE ( 'fapar', 'apar', 'apar_gb' )
    l_fapar_diag = .TRUE.

  CASE ( 'fao_et0' )
    l_fao_ref_evapotranspiration = .TRUE.

  END SELECT

END DO  !  variables

END SUBROUTINE check_output_vars

END MODULE init_output_mod
#endif
