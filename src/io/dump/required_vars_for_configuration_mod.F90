#if !defined(RIVERS_ONLY)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE required_vars_for_configuration_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE required_vars_for_configuration(nvars, identifiers,                 &
                                           nvars_from_ancil, vars_from_ancil,  &
                                           l_output_mode,                      &
                                           total_snow, include_imogen_vars,    &
                                           read_or_write_dump)

USE update_mod, ONLY: l_imogen, have_prescribed_sthuf

USE jules_hydrology_mod, ONLY: l_top

USE jules_radiation_mod, ONLY: l_snow_albedo, l_embedded_snow

USE jules_soil_biogeochem_mod, ONLY: soil_model_ecosse, soil_model_4pool,      &
                                     soil_bgc_model, l_ch4_microbe,            &
                                     l_label_frac_cs

USE jules_soil_ecosse_mod, ONLY: l_soil_N

USE jules_vegetation_mod, ONLY: can_model, can_rad_mod,                        &
                                l_triffid, l_phenol, l_veg_compete,            &
                                l_crop, l_landuse,                             &
                                l_nitrogen, l_prescsow, l_trif_crop,           &
                                photo_acclim_model, photo_adapt,               &
                                photo_acclim, photo_adapt_acclim,              &
                                l_croprotate, l_trif_biocrop, l_sugar, l_red

USE jules_water_resources_mod, ONLY: l_water_irrigation, l_water_resources,    &
       nwater_use, partition_ancil, partition_method, use_environment

USE jules_irrig_mod, ONLY: l_irrig_dmd

USE jules_snow_mod, ONLY: nsmax

USE jules_surface_mod, ONLY: l_elev_land_ice, l_flake_model

USE jules_soil_mod, ONLY: l_bedrock, sm_levels, l_tile_soil

USE fire_mod,     ONLY: fire_cntl, l_fire

USE metstats_mod, ONLY: metstats_flag, l_metstats

USE update_mod, ONLY: l_daily_disagg, precip_disagg_method,                    &
                      prescribed_sthuf_levels

USE jules_rivers_mod, ONLY: l_rivers

USE logging_mod, ONLY: log_warn

USE imogen_run, ONLY: land_feed_ch4

USE prognostics, ONLY: l_broadcast_soilt

USE dump_mod, ONLY: ancil_dump_read

USE add_to_list_mod, ONLY: add_to_list

USE required_vars_for_rivers_mod, ONLY: required_vars_for_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns the identifiers of the prognostic variables for the current
!   configuration of the model
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN OUT) :: nvars  ! The number of variables
CHARACTER(LEN=*), INTENT(OUT) :: identifiers(:)
                               ! The model identifiers of the required
                               ! variables

INTEGER, INTENT(IN OUT)       :: nvars_from_ancil  !Number of ancil variables
CHARACTER(LEN=*), INTENT(OUT) :: vars_from_ancil(:)
                               ! The model identifiers of the required
                               ! ancil variables

LOGICAL, INTENT(IN)           :: l_output_mode
                               ! True if the intention of the call to this
                               ! routine is to get the variables to output
                               ! False if for input
                               ! This enables the broadcasting of non-soil
                               ! tiled initial conditions for soil tiling
                               ! runs

LOGICAL, INTENT(IN), OPTIONAL :: total_snow
                               ! T - return only the variables required if
                               !     snow layer values are to be set by the
                               !     model
                               ! F - return all snow variables required
                               !     by the current configuration
LOGICAL, INTENT(IN), OPTIONAL :: include_imogen_vars
                               ! T - include IMOGEN prognostics in the
                               !     returned list if they are needed
                               ! F - do not include IMOGEN prognostics in
                               !     the returned list, even if they are
                               !     needed
LOGICAL, INTENT(IN), OPTIONAL :: read_or_write_dump
                               ! T - include all variables that are needed
                               !     when reading or writing to dump files
                               ! F - don't include variables that are only
                               !     inputted or outputted in dump files

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'REQUIRED_VARS_FOR_CONFIGURATION'
    ! The name of this routine.

! Work variables
LOGICAL :: total_snow_local  ! Local version of total_snow
                             ! Defaults to FALSE if not present (i.e.
                             ! return all required snow vars)
LOGICAL :: inc_imogen_local  ! Local version of include_imogen_vars
                             ! Defaults to TRUE if not present, i.e.
                             ! return IMOGEN vars if they are required
LOGICAL :: read_or_write_dump_local
                             ! Local version of read_or_write_dump
                             ! Defaults to TRUE if not present
LOGICAL :: l_append_soilt
                             ! Controls whether vars are appended with _soilt
LOGICAL :: l_need_conveyance_loss
                             ! Flag indicating if a conveyance loss ancillary
                             ! field is required.
INTEGER :: i                 ! Loop counter.
INTEGER :: num_prescribed_sthuf_levels
                             ! Number of levels in the prescibed sthuf field


!-----------------------------------------------------------------------------

! Check for presence of optional arguments
total_snow_local          = .FALSE.
inc_imogen_local          = .TRUE.
read_or_write_dump_local  = .TRUE.

! Decide whether we need to append _soilt to variables
l_append_soilt = .FALSE.
IF (l_tile_soil) THEN
  IF (l_output_mode) THEN
    l_append_soilt = .TRUE.
  ELSE
    IF ( .NOT. l_broadcast_soilt) THEN
      l_append_soilt = .TRUE.
    END IF
  END IF
END IF

IF ( PRESENT(total_snow) )                                                     &
  total_snow_local         = total_snow
IF ( PRESENT(include_imogen_vars) )                                            &
  inc_imogen_local         = include_imogen_vars
IF ( PRESENT(read_or_write_dump) )                                             &
  read_or_write_dump_local = read_or_write_dump

! First, set up the array with variables that are required with every
! configuration.
! Subroutine add_to_list is called with the l_append_soilt for a variable that
! supports soil tiles.

! Variables without soil tiling.
CALL add_to_list( 'canopy',     nvars, identifiers )
! Note that for total_snow = T, snow_surft is the only required snow variable
! (in addition to rgrain, depending on the snow albedo switches).
CALL add_to_list( 'snow_tile',  nvars, identifiers )
CALL add_to_list( 'tstar_tile', nvars, identifiers )

! Variables with soil tiling.
CALL add_to_list( 'cs',         nvars, identifiers, l_append_soilt )
CALL add_to_list( 't_soil',     nvars, identifiers, l_append_soilt )

!-----------------------------------------------------------------------------
! Variables that are needed only if certain options set.
!-----------------------------------------------------------------------------
IF ( can_rad_mod == 1 ) THEN
  CALL add_to_list( 'gs', nvars, identifiers )
END IF

IF ( photo_acclim_model == photo_acclim .OR.                                   &
     photo_acclim_model == photo_adapt_acclim) THEN
  CALL add_to_list( 't_growth_gb', nvars, identifiers )
END IF

IF (ALLOCATED(prescribed_sthuf_levels)) THEN
  num_prescribed_sthuf_levels = SIZE(prescribed_sthuf_levels)
ELSE
  num_prescribed_sthuf_levels = 0
END IF

IF ( ( .NOT. have_prescribed_sthuf ) .OR.                                      &
     ( num_prescribed_sthuf_levels < sm_levels) ) THEN
  CALL add_to_list( 'sthuf', nvars, identifiers, l_append_soilt )
END IF

IF ( l_phenol ) THEN
  ! With phenology on, LAI is prognostic for all PFTs
  CALL add_to_list( 'lai', nvars, identifiers )
ELSE IF ( l_crop ) THEN
  ! Otherwise, if the crop scheme is on, LAI is prognostic for crop PFTs only
  CALL add_to_list( 'croplai', nvars, identifiers )
END IF

IF ( l_triffid ) THEN
  ! With TRIFFID on, canopy height is prognostic for all PFTs
  CALL add_to_list( 'canht', nvars, identifiers )
ELSE IF ( l_crop ) THEN
  ! Otherwise, if the crop scheme is on, canopy height is prognostic for crop
  ! PFTs only
  CALL add_to_list( 'cropcanht', nvars, identifiers )
END IF

IF ( l_triffid .AND. l_landuse ) THEN
  ! With TRIFFID on,  agricultural frac and WP pools are required for LUC
  CALL add_to_list( 'frac_agr_prev',  nvars, identifiers )
  CALL add_to_list( 'wood_prod_fast', nvars, identifiers )
  CALL add_to_list( 'wood_prod_med',  nvars, identifiers )
  CALL add_to_list( 'wood_prod_slow', nvars, identifiers )
  IF (l_trif_crop) THEN
    CALL add_to_list( 'frac_past_prev', nvars, identifiers )
  END IF
  IF (l_trif_biocrop) THEN
    CALL add_to_list( 'frac_biocrop_prev', nvars, identifiers )
  END IF

END IF

IF ( soil_bgc_model == soil_model_4pool .AND. l_nitrogen ) THEN
  CALL add_to_list( 'n_inorg', nvars, identifiers, l_append_soilt )
  CALL add_to_list( 'ns',      nvars, identifiers, l_append_soilt )
END IF

IF ( soil_bgc_model == soil_model_ecosse .AND. l_soil_N ) THEN
  CALL add_to_list( 'n_soil', nvars, identifiers, l_append_soilt )
END IF

IF ( l_label_frac_cs ) THEN
  CALL add_to_list( 'frac_c_label_pool', nvars, identifiers, l_append_soilt )
END IF

IF ( l_red ) THEN
  CALL add_to_list( 'plantNumDensity', nvars, identifiers )
END IF

IF ( l_ch4_microbe ) THEN
  CALL add_to_list( 'substr_ch4', nvars, identifiers, l_append_soilt )
  CALL add_to_list( 'mic_ch4', nvars, identifiers, l_append_soilt )
  CALL add_to_list( 'mic_act_ch4', nvars, identifiers, l_append_soilt )
  CALL add_to_list( 'acclim_ch4', nvars, identifiers, l_append_soilt )
END IF

IF ( l_sugar ) THEN
  ! With SUGAR on, NSC mass fraction is prognostic for all PFTs
  CALL add_to_list( 'f_nsc', nvars, identifiers )
END IF

! TOPMODEL variables.
IF ( l_top ) THEN
  ! Wetness in deep layer and depth to water table..
  CALL add_to_list( 'sthzw', nvars, identifiers, l_append_soilt )
  CALL add_to_list( 'zw',    nvars, identifiers, l_append_soilt )
END IF

! Additional crop scheme prognostics, required if crop scheme is on
IF ( l_crop ) THEN
  CALL add_to_list( 'cropdvi',      nvars, identifiers )
  CALL add_to_list( 'croprootc',    nvars, identifiers )
  CALL add_to_list( 'cropharvc',    nvars, identifiers )
  CALL add_to_list( 'cropreservec', nvars, identifiers )
END IF

IF ( l_irrig_dmd ) THEN
  CALL add_to_list( 'sthu_irr', nvars, identifiers, l_append_soilt )
END IF

! Additional deep soil temperature if bedrock is on
IF ( l_bedrock ) THEN
  CALL add_to_list( 'tsoil_deep', nvars, identifiers )
END IF

! FLake prognostics
IF (l_flake_model) THEN
  CALL add_to_list( 'lake_fetch_gb',  nvars, identifiers )
  CALL add_to_list( 'lake_t_mean_gb', nvars, identifiers )
  CALL add_to_list( 'lake_t_mxl_gb',  nvars, identifiers )
  CALL add_to_list( 'lake_h_mxl_gb',  nvars, identifiers )
  CALL add_to_list( 'lake_t_ice_gb',  nvars, identifiers )
  CALL add_to_list( 'lake_h_ice_gb',  nvars, identifiers )
  CALL add_to_list( 'lake_shape_factor_gb', nvars, identifiers )
END IF

!-----------------------------------------------------------------------------
! Add metstats prognostic variables if switched on
!-----------------------------------------------------------------------------
IF ( l_metstats ) THEN
  IF (metstats_flag%temp_max_00h) THEN
    CALL add_to_list( 'temp_max_00h',   nvars, identifiers )
    CALL add_to_list( 'temp_max_00h_r', nvars, identifiers )
  END IF

  IF (metstats_flag%temp_ave_00h) THEN
    CALL add_to_list( 'temp_ave_00h',   nvars, identifiers )
    CALL add_to_list( 'temp_ave_00h_r', nvars, identifiers )
  END IF

  IF (metstats_flag%temp_ave_nday) THEN
    CALL add_to_list( 'temp_ave_nday', nvars, identifiers )
  END IF

  IF (metstats_flag%temp_pnt_12h) THEN
    CALL add_to_list( 'temp_pnt_12h', nvars, identifiers )
  END IF

  IF (metstats_flag%prec_tot_00h) THEN
    CALL add_to_list( 'prec_tot_00h',   nvars, identifiers )
    CALL add_to_list( 'prec_tot_00h_r', nvars, identifiers )
  END IF

  IF (metstats_flag%prec_tot_12h) THEN
    CALL add_to_list( 'prec_tot_12h',   nvars, identifiers )
    CALL add_to_list( 'prec_tot_12h_r', nvars, identifiers )
  END IF

  IF (metstats_flag%rhum_min_00h) THEN
    CALL add_to_list( 'rhum_min_00h',   nvars, identifiers )
    CALL add_to_list( 'rhum_min_00h_r', nvars, identifiers )
  END IF

  IF (metstats_flag%rhum_pnt_12h) THEN
    CALL add_to_list( 'rhum_pnt_12h', nvars, identifiers )
  END IF

  IF (metstats_flag%dewp_ave_00h) THEN
    CALL add_to_list( 'dewp_ave_00h',   nvars, identifiers )
    CALL add_to_list( 'dewp_ave_00h_r', nvars, identifiers )
  END IF

  IF (metstats_flag%wind_ave_00h) THEN
    CALL add_to_list( 'wind_ave_00h',   nvars, identifiers )
    CALL add_to_list( 'wind_ave_00h_r', nvars, identifiers )
  END IF

  IF (metstats_flag%wind_pnt_12h) THEN
    CALL add_to_list( 'wind_pnt_12h', nvars, identifiers )
  END IF
END IF

!-----------------------------------------------------------------------------
! Add fire prognostic variables if switched on
!-----------------------------------------------------------------------------
IF ( l_fire ) THEN
  IF (fire_cntl%mcarthur%flag) THEN
    CALL add_to_list( 'fire_mcarthur_r_dr', nvars, identifiers )
    CALL add_to_list( 'fire_mcarthur_n_dr', nvars, identifiers )
  END IF

  IF (fire_cntl%canadian%flag) THEN
    CALL add_to_list( 'fire_canadian_ffmc',      nvars, identifiers )
    CALL add_to_list( 'fire_canadian_ffmc_mois', nvars, identifiers )
    CALL add_to_list( 'fire_canadian_dmc',       nvars, identifiers )
    CALL add_to_list( 'fire_canadian_dc',        nvars, identifiers )
  END IF

  IF (fire_cntl%nesterov%flag) THEN
    CALL add_to_list( 'fire_nesterov', nvars, identifiers )
  END IF
END IF !Fire

!-----------------------------------------------------------------------------
! Add IMOGEN variables if IMOGEN is on and they are requested
!-----------------------------------------------------------------------------
IF ( l_imogen .AND. inc_imogen_local ) THEN
  CALL add_to_list( 'co2_ppmv',        nvars, identifiers )
  CALL add_to_list( 'co2_change_ppmv', nvars, identifiers )
  CALL add_to_list( 'dtemp_o',         nvars, identifiers )
  CALL add_to_list( 'fa_ocean',        nvars, identifiers )
  CALL add_to_list( 'seed_rain',       nvars, identifiers )

  ! This should possibly be under an l_triffid switch, but is here for now
  ! so as not to change behaviour for non-TRIFFID runs
  CALL add_to_list( 'cv', nvars, identifiers )

  IF (land_feed_ch4) THEN
    CALL add_to_list( 'ch4_ppbv', nvars, identifiers )
  END IF
END IF

IF ( l_daily_disagg .AND. read_or_write_dump_local .AND.                       &
     (precip_disagg_method > 1) ) THEN
  CALL add_to_list( 'seed_rain', nvars, identifiers )
END IF

!-----------------------------------------------------------------------------
! Work out what snow variables are required
! If total_snow = T, only snow_surft is required, and is always required
! so if already in the list
! We just need to add the other required snow variables based on the scheme
! enabled if total_snow = F
!-----------------------------------------------------------------------------
IF ( .NOT. total_snow_local ) THEN
  ! Snow variables not specifically for the multi-layer model
  CALL add_to_list( 'rho_snow',   nvars, identifiers )
  CALL add_to_list( 'snow_depth', nvars, identifiers )

  IF ( can_model == 4 ) THEN
    CALL add_to_list( 'snow_grnd', nvars, identifiers )
  END IF

  ! Variables for the multi-layer snow model.
  IF ( nsmax > 0 ) THEN
    CALL add_to_list( 'nsnow',    nvars, identifiers )
    CALL add_to_list( 'snow_ds',  nvars, identifiers )
    CALL add_to_list( 'snow_ice', nvars, identifiers )
    CALL add_to_list( 'snow_liq', nvars, identifiers )
    CALL add_to_list( 'tsnow',    nvars, identifiers )

    IF ( l_snow_albedo .OR. l_embedded_snow ) THEN
      CALL add_to_list( 'rgrainl', nvars, identifiers )
    END IF
  END IF   ! nsmax

END IF  ! total_snow

! Snow surface grain size.
! Note this is required in addition to the layered variable rgrainl if
! using layered snow, because the surface grain size is used for radiative
! calculations. (But as rgrain is copied from rgrainl, possibly we
! should only require rgrainl for the layered model?)
IF ( l_snow_albedo .OR. l_embedded_snow ) THEN
  CALL add_to_list( 'rgrain', nvars, identifiers )
END IF

IF ( l_elev_land_ice) THEN
  CALL add_to_list( 'tsurf_elev_surft', nvars, identifiers )
END IF

!-----------------------------------------------------------------------------
! Work out which ancillary fields need to be in the dump file
! The logic here is slightly different from normal prognostic variables
!
! There are several cases to consider:
! -The var is not needed
! -The var should be populated by ancil and written to the dump (ie write only
!  as far as this module is concerned)
! -The var should be both read from the initial condition namelist and written
!  to the dump
!
! To get the information about which ancils will be read from file, we use
! ancil_dump_read%xyz:  Defaulting to false, set in init_ancillaries_mod
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Set up read/write details for latlon ancil.
! Note that latitude and longitude (and projection coordinates) are not
! handled here.
!-----------------------------------------------------------------------------
IF ( l_water_irrigation ) THEN
  CALL add_to_list( 'grid_area', nvars, identifiers )

  ! If not reading from dump, indicate variables come from an ancil file.
  IF ( .NOT. ancil_dump_read%latlon) THEN
    CALL add_to_list( 'grid_area', nvars_from_ancil, vars_from_ancil,          &
                                     l_append_soilt )
  END IF
END IF

!-----------------------------------------------------------------------------
!Set up read/write details for frac ancil
!This is a special case: with competing vegetation on, frac is prognostic

! frac should always be written to the dump
CALL add_to_list( 'frac', nvars, identifiers )

IF ( l_veg_compete ) THEN
  ! With competing veg on, frac is a prognostic and should always be read
  ! as an initial condition
  ancil_dump_read%frac  = .TRUE.
END IF

! If not reading from dump, indicate variables come from an ancil file.
IF ( .NOT. ancil_dump_read%frac ) THEN
  CALL add_to_list( 'frac', nvars_from_ancil, vars_from_ancil )
END IF

!-----------------------------------------------------------------------------
! Set up read/write details for vegetation ancil.

IF ( photo_acclim_model == photo_adapt .OR.                                    &
     photo_acclim_model == photo_adapt_acclim) THEN
  CALL add_to_list( 't_home_gb', nvars, identifiers )

  ! If not reading from dump, indicate variables come from an ancil file.
  IF ( .NOT. ancil_dump_read%vegetation_props) THEN
    CALL add_to_list( 't_home_gb', nvars_from_ancil, vars_from_ancil,          &
                                     l_append_soilt )
  END IF

END IF ! photo_acclim_model

!---------------------------------------------------------------------------
! Set up read/write details for soil properties ancils
! Most soil properties are always needed.
CALL add_to_list( 'b',       nvars, identifiers, l_append_soilt )
CALL add_to_list( 'sathh',   nvars, identifiers, l_append_soilt )
CALL add_to_list( 'satcon',  nvars, identifiers, l_append_soilt )
CALL add_to_list( 'sm_sat',  nvars, identifiers, l_append_soilt )
CALL add_to_list( 'sm_crit', nvars, identifiers, l_append_soilt )
CALL add_to_list( 'sm_wilt', nvars, identifiers, l_append_soilt )
CALL add_to_list( 'hcap',    nvars, identifiers, l_append_soilt )
CALL add_to_list( 'hcon',    nvars, identifiers, l_append_soilt )
CALL add_to_list( 'albsoil', nvars, identifiers, l_append_soilt )

IF ( soil_bgc_model == soil_model_4pool .OR.                                   &
     soil_bgc_model == soil_model_ecosse) THEN
  CALL add_to_list( 'clay', nvars, identifiers, l_append_soilt )
END IF

IF ( soil_bgc_model == soil_model_ecosse ) THEN
  CALL add_to_list( 'soil_ph', nvars, identifiers, l_append_soilt )
END IF

! If not reading from dump, indicate variables come from an ancil file.
IF ( .NOT. ancil_dump_read%soil_props ) THEN
  CALL add_to_list( 'b',       nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'sathh',   nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'satcon',  nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'sm_sat',  nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'sm_crit', nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'sm_wilt', nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'hcap',    nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'hcon',    nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  CALL add_to_list( 'albsoil', nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  IF ( soil_bgc_model == soil_model_4pool .OR.                                 &
       soil_bgc_model == soil_model_ecosse ) THEN
    CALL add_to_list( 'clay',  nvars_from_ancil, vars_from_ancil,              &
                               l_append_soilt )
  END IF
  IF ( soil_bgc_model == soil_model_ecosse ) THEN
    CALL add_to_list( 'soil_ph', nvars_from_ancil,                             &
                                 vars_from_ancil, l_append_soilt )
  END IF
END IF

!----------------------------------------------------------------------------
!Set up read/write details for topmodel ancils.
!Depends on the value of l_top

IF ( l_top ) THEN
  CALL add_to_list( 'fexp',    nvars, identifiers, l_append_soilt )
  CALL add_to_list( 'ti_mean', nvars, identifiers, l_append_soilt )
  CALL add_to_list( 'ti_sig',  nvars, identifiers, l_append_soilt )

  ! If not reading from dump, indicate variables come from an ancil file.
  IF ( .NOT. ancil_dump_read%top) THEN
    CALL add_to_list( 'fexp',    nvars_from_ancil,vars_from_ancil,             &
                                 l_append_soilt )
    CALL add_to_list( 'ti_mean', nvars_from_ancil,vars_from_ancil,             &
                                 l_append_soilt )
    CALL add_to_list( 'ti_sig',  nvars_from_ancil,vars_from_ancil,             &
                                 l_append_soilt )
  END IF
END IF !l_top

!----------------------------------------------------------------------------
!Set up read/write details for agric ancil namelist
!This is always needed

CALL add_to_list( 'frac_agr', nvars, identifiers )

! If not reading from dump, indicate variables come from an ancil file.
IF ( .NOT. ancil_dump_read%agric ) THEN
  CALL add_to_list( 'frac_agr', nvars_from_ancil, vars_from_ancil )
END IF

!---------------------------------------------------------------------------
!Set up read/write details for biocrop variables
!The vars required depend on l_trif_biocrop

IF ( l_trif_biocrop ) THEN
  CALL add_to_list( 'harvest_doy', nvars, identifiers )
  CALL add_to_list( 'years_since_harvest', nvars, identifiers )

  IF ( .NOT. ancil_dump_read%biocrop ) THEN
    CALL add_to_list( 'harvest_doy', nvars_from_ancil, vars_from_ancil )
  END IF

END IF

!---------------------------------------------------------------------------
!Set up read/write details for crop properites ancil namelist
!The vars required depend on a couple of switches

IF ( l_crop ) THEN
  CALL add_to_list( 'cropttveg', nvars, identifiers )
  CALL add_to_list( 'cropttrep', nvars, identifiers )
  IF ( l_prescsow ) THEN  !we additionally need the crop sowing date
    CALL add_to_list( 'cropsowdate', nvars, identifiers )
    !When l_croprotate is TRUE we need the latest possible harvest date but
    !it is also possible to use croplatestharvdate when l_croprotate is FALSE
    CALL add_to_list( 'croplatestharvdate', nvars, identifiers )
  END IF

  ! If not reading from dump, indicate variables come from an ancil file.
  IF ( .NOT. ancil_dump_read%crop_props ) THEN
    CALL add_to_list( 'cropttveg', nvars_from_ancil, vars_from_ancil )
    CALL add_to_list( 'cropttrep', nvars_from_ancil, vars_from_ancil )
    IF ( l_prescsow ) THEN  !we additionally need the crop sowing date
      CALL add_to_list( 'cropsowdate', nvars_from_ancil, vars_from_ancil )
      !When l_croprotate is TRUE we need the latest possible harvest date but
      !it is also possible to use croplatestharvdate when l_croprotate is
      !FALSE
      CALL add_to_list( 'croplatestharvdate', nvars_from_ancil,                &
                                              vars_from_ancil )
    END IF
  END IF

END IF !l_crop

  !---------------------------------------------------------------------------
  !Set up read/write details for irrigation ancil namelist
  !The vars required depend on l_irrig_dmd
IF ( l_irrig_dmd ) THEN
  CALL add_to_list( 'frac_irrig',         nvars, identifiers )
  CALL add_to_list( 'frac_irr_all_tiles', nvars, identifiers )
  !These only matter if frac_irrig_all_tiles is false, but can't gurantee that
  !that its value will be known when reading that variable from the dump
  CALL add_to_list( 'irrtiles',           nvars, identifiers )
  CALL add_to_list( 'nirrtile',           nvars, identifiers )
  CALL add_to_list( 'set_irrfrac_on_irrtiles', nvars, identifiers )
  CALL add_to_list( 'irrfrac_irrtiles',   nvars, identifiers )

  ! If not reading from dump, indicate variables come from an ancil file.
  IF ( .NOT. ancil_dump_read%irrig ) THEN
    CALL add_to_list( 'frac_irrig', nvars_from_ancil, vars_from_ancil )
    CALL add_to_list( 'frac_irr_all_tiles', nvars_from_ancil,                  &
                                            vars_from_ancil )
    !These only matter if frac_irrig_all_tiles is false, but can't gurantee that
    !that its value will be known when reading that variable from the dump
    CALL add_to_list( 'irrtiles',  nvars_from_ancil, vars_from_ancil )
    CALL add_to_list( 'nirrtile',  nvars_from_ancil, vars_from_ancil )
    CALL add_to_list( 'set_irrfrac_on_irrtiles', nvars_from_ancil,             &
                                                 vars_from_ancil )
    CALL add_to_list( 'irrfrac_irrtiles',        nvars_from_ancil,             &
                                                 vars_from_ancil )
  END IF

END IF

!---------------------------------------------------------------------------
!Set up read/write details for water resources properties ancil namelist.

IF ( l_water_resources ) THEN

  ! conv_loss_frac
  l_need_conveyance_loss = .FALSE.
  DO i = 1, nwater_use
    IF ( i /= use_environment ) THEN
      l_need_conveyance_loss = .TRUE.
    END IF
  END DO
  IF ( l_need_conveyance_loss ) THEN
    CALL add_to_list( 'conv_loss_frac', nvars, identifiers )
    ! If not reading from dump, indicate variables come from an ancil file.
    IF ( .NOT. ancil_dump_read%water_resources_props ) THEN
      CALL add_to_list( 'conv_loss_frac', nvars_from_ancil, vars_from_ancil )
    END IF
  END IF

  ! sfc_water_frac
  IF ( partition_method == partition_ancil ) THEN
    CALL add_to_list( 'sfc_water_frac', nvars, identifiers )
    IF ( .NOT. ancil_dump_read%water_resources_props ) THEN
      CALL add_to_list( 'sfc_water_frac',  nvars_from_ancil, vars_from_ancil )
    END IF
  END IF

END IF  !  l_water_resources

!---------------------------------------------------------------------------
!Set up read/write details for co2 ancil namelist
!Always required, even if it just carries around the default value set in
!the aero module
CALL add_to_list( 'co2_mmr', nvars, identifiers )

! If not reading from dump, indicate variable comes from another source.
IF ( .NOT. ancil_dump_read%co2 ) THEN
  CALL add_to_list( 'co2_mmr', nvars_from_ancil, vars_from_ancil )
END IF

!-----------------------------------------------------------------------------
!Set up read/write details for flake ancil namelist
!Required if l_flake_model = True, even if it just carries around the default
!value

IF ( l_flake_model ) THEN

  CALL add_to_list( 'lake_depth_gb', nvars, identifiers )

  IF ( .NOT. ancil_dump_read%flake ) THEN
    CALL add_to_list( 'lake_depth_gb', nvars_from_ancil, vars_from_ancil )
  END IF

END IF !l_flake_model

! ancil_dump_read%rivers_props functionality has not yet been implemented,
! however the ability to initialise River storage (TRIP) from a climatology
! ancil requires the vars from ancil functionality so this is an exception.
! Further work is required to add vars in the usual way.
IF ( l_rivers ) THEN
  CALL required_vars_for_rivers(nvars, identifiers,                            &
                                nvars_from_ancil, vars_from_ancil,             &
                                read_or_write_dump)
END IF

RETURN

END SUBROUTINE required_vars_for_configuration

!#############################################################################

END MODULE required_vars_for_configuration_mod
#endif
