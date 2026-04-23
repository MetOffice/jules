#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE init_ic_mod

USE model_interface_mod, ONLY: identifier_len
USE dump_mod,            ONLY: max_var_dump
USE jules_initial_mod,   ONLY: total_snow, dump_file
USE logging_mod,         ONLY: log_info, log_warn, log_error, log_fatal
USE string_utils_mod,    ONLY: to_string


IMPLICIT NONE

LOGICAL, PARAMETER :: l_output_mode = .FALSE.

CONTAINS

SUBROUTINE init_ic(nml_dir,crop_vars,psparms,toppdm,ainfo,trif_vars,           &
                   soilecosse,urban_param,progs,jules_vars,                    &
                          !fluxes, &
                          lake_vars,                                           &
                          !forcing, &
                          rivers                                               &
                          !veg3_parm, &
                          !veg3_field, &
                          !chemvars, &
                          )

  !Use in relevant subroutines
USE init_initial_mod,    ONLY: init_initial
USE jules_initial_mod,   ONLY: init_ic_shared
USE freeze_soil_mod,     ONLY: freeze_soil
USE flake_init_mod,      ONLY: flake_init
USE tilepts_mod,         ONLY: tilepts
USE total_snow_init_mod, ONLY: total_snow_init
USE topmodel_init_mod,   ONLY: topmodel_init
USE calc_fit_fsat_mod,   ONLY: calc_fit_fsat
USE init_layeredcn_mod, ONLY: init_layeredcn
USE soil_biogeochem_utils_mod, ONLY: calc_litc_norm

USE mpi, ONLY: mpi_integer
USE jules_vars_mod, ONLY: mpi_local_comm
USE um_types, ONLY: real_jlslsm

USE dictionary_mod, ONLY: dict

USE conversions_mod, ONLY: zerodegc

USE water_constants_mod, ONLY: rho_water, dpsidt

USE veg_param, ONLY: litc_norm

USE required_vars_for_configuration_mod, ONLY: required_vars_for_configuration

USE ecosse_init_mod, ONLY: ecosse_init

USE jules_rivers_mod, ONLY: np_rivers

USE jules_hydrology_mod, ONLY: l_top

USE jules_vegetation_mod, ONLY: l_phenol, l_triffid, frac_min, l_crop,         &
                                l_recon, l_nitrogen, l_croprotate

USE jules_irrig_mod, ONLY: l_irrig_dmd, l_irrig_limit

USE update_mod, ONLY: l_daily_disagg, precip_disagg_method

USE jules_surface_mod, ONLY: l_urban2t, l_anthrop_heat_use_wrr

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_ecosse, soil_model_4pool,                                         &
  ! imported scalar variables
  bio_hum_cn, l_layeredC, soil_bgc_model

USE jules_surface_mod, ONLY: l_elev_land_ice, l_flake_model, l_aggregate

USE jules_surface_types_mod, ONLY: ncpft, nnpft,                               &
                                    ice, urban_canyon, urban_roof

USE ancil_info, ONLY: land_pts, lice_pts, soil_pts, nsoilt,                    &
                      dim_cslayer

USE prognostics, ONLY: l_broadcast_soilt_in_mod => l_broadcast_soilt

USE crop_vars_mod, ONLY: startyr, startmon, startday,                          &
                         starttime

USE crop_utils_mod, ONLY: croplai_min, cropcanht_min, croprootc_min

USE jules_soil_mod, ONLY: dzsoil, sm_levels

USE assign_irrig_fraction_mod,  ONLY: assign_irrig_fraction

USE time_info_mod, ONLY: current_model_time

USE rivers_route_mod, ONLY: scatter_land_from_riv_field

USE soil_biogeochem_control_mod, ONLY:                                         &
  ! imported scalar parameters
  call_initial,                                                                &
  ! imported procedures
  increment_soil_drivers

USE parallel_mod, ONLY: master_task_id, is_master_task

!TYPE definitions
USE prognostics, ONLY: progs_type
USE crop_vars_mod, ONLY: crop_vars_type
USE p_s_parms,  ONLY: psparms_type
USE top_pdm, ONLY: top_pdm_type
USE ancil_info, ONLY: ainfo_type
USE trif_vars_mod, ONLY: trif_vars_type
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type
USE urban_param_mod, ONLY: urban_param_type
USE jules_vars_mod, ONLY: jules_vars_type
! USE fluxes, ONLY: fluxes_type
USE lake_mod, ONLY: lake_type
! USE forcing, ONLY: forcing_type
USE jules_rivers_mod, ONLY: rivers_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
! USE jules_chemvars_mod, ONLY: chemvars_type


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets up the initial conditions for the run
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(top_pdm_type), INTENT(IN OUT) :: toppdm
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
!TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
TYPE(lake_type), INTENT(IN OUT) :: lake_vars
!TYPE(forcing_type), INTENT(IN OUT) :: forcing
TYPE(rivers_type), INTENT(IN OUT) :: rivers
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field
!TYPE(chemvars_type), INTENT(IN OUT) :: chemvars


!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
     RoutineName = 'init_ic'   ! Name of this routine.

! Work variables
INTEGER :: nvars_required     ! The number of variables that are
                              ! required in this configuration

CHARACTER(LEN=identifier_len) :: required_vars(max_var_dump)
                               ! The variable identifiers of the required
                               ! variables

INTEGER :: nvars_from_ancil
CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                               ! The variable identifiers of the ancil
                               ! variables

TYPE(dict) :: default_values  ! Dictionary mapping identifier => default value
                              ! Values should be REAL
LOGICAL :: reset_done  ! Indicates if a reset of frac to frac_min was
                       ! performed

LOGICAL :: firstcall = .TRUE.

REAL(KIND=real_jlslsm) :: urban_fraction
                        ! Used in partitioning of urban fraction into
                        ! canyon and roof

REAL(KIND=real_jlslsm) :: rivers_sto_per_m2_rgrid(np_rivers)
          ! water storage on routing grid in kg m-2

REAL ::                                                                        &
 qbase_l_soilt_dummy(land_pts,nsoilt,sm_levels+1),                             &
    ! Lateral flux of water from each soil layer (kg m-2 s-1).
    ! Used as a dummy argument.
 w_flux_soilt_dummy(land_pts,nsoilt,0:sm_levels)
    ! Downward water flux at bottom of each soil layer (kg m-2 s-1).
    ! Used as a dummy argument.

INTEGER :: nstep_dummy        ! Used as a dummy argument.
LOGICAL :: l_run_model_dummy  ! Used as a dummy argument.

INTEGER :: i,j,l,n,m,ip  ! Loop counters

INTEGER :: n_counted ! Counter for assigning soil_pts

INTEGER :: year, month, day, TIME

INTEGER :: ERROR  ! Error indicator

!-----------------------------------------------------------------------------

CALL init_initial(nml_dir)

!-----------------------------------------------------------------------------
! Set up initial conditions using namelist values
!-----------------------------------------------------------------------------

! Set up the required variables - we get the list by calling a procedure in
! dump_mod that tells us the required prognostic variables for the current
! model configuration
! We indicate whether or not we are using total_snow, as this affects the
! required variables
! We also indicate that we do not want IMOGEN prognostics in the list, even
! if they are required, since they are initialised in init_imogen
! We do request the ancillaries
nvars_required   = 0
nvars_from_ancil = 0
CALL required_vars_for_configuration( nvars_required, required_vars,           &
                                      nvars_from_ancil, vars_from_ancil,       &
                                      l_output_mode,                           &
                                      total_snow, .FALSE., dump_file )

!Get a dictionary of default values in case they are needed
default_values = get_default_ic_values(total_snow)

CALL init_ic_shared ( nvars_required, required_vars,                           &
                      nvars_from_ancil, vars_from_ancil,                       &
                      default_values )

!*****************************************************************************
! Further processing depending on options specified
!*****************************************************************************

!-----------------------------------------------------------------------------
! Set up derived soil values
!-----------------------------------------------------------------------------
! Set surface values.
psparms%hcon_soilt(:,:,0)   = psparms%hcon_soilt(:,:,1)
psparms%satcon_soilt(:,:,0) = psparms%satcon_soilt(:,:,1)

! Check that psparms%sathh_soilt>=0 - a common error!
IF ( ANY( psparms%sathh_soilt(:,:,:) < 0.0 ) )                                 &
  CALL log_fatal("init_soil",                                                  &
                 "sathh < 0.0 detected - for JULES, sathh is " //              &
                 "abs(saturated head)")

! Check that clay_soilt>=0 and <=1 - a common error!
SELECT CASE ( soil_bgc_model )
CASE ( soil_model_ecosse, soil_model_4pool )
  IF ( ANY( psparms%clay_soilt(:,:,:) < 0.0 ) .OR.                             &
       ANY( psparms%clay_soilt(:,:,:) > 1.0 ) ) THEN
    CALL log_fatal("init_soil", "clay <0 and/or >1.0 detected - for JULES")
  END IF
END SELECT

!-----------------------------------------------------------------------------
! Detect soil points.
! If top layer saturation moisture content > 0, this is a soil point.
! Note that land ice points are no longer assigned here.
!-----------------------------------------------------------------------------
soil_pts        = 0
ainfo%soil_index(:)   = 0
ainfo%l_soil_point(:) = .FALSE.
DO l = 1,land_pts
  IF (nsoilt == 1) THEN
    m = 1
    !Use existing logic
    IF ( psparms%smvcst_soilt(l,m,1) >                                         &
         EPSILON(psparms%smvcst_soilt(l,m,1)) ) THEN
      soil_pts             = soil_pts + 1
      ainfo%soil_index(soil_pts) = l
      ainfo%l_soil_point(l)      = .TRUE.
    END IF
  ELSE
    !Use soil-tile aware logic

    !Test every soil tile for the smvcst condition
    n_counted = 0
    DO m = 1,nsoilt
      IF (psparms%smvcst_soilt(l,m,1) >                                        &
          EPSILON(psparms%smvcst_soilt(l,m,1))) THEN
        n_counted = n_counted + 1
      END IF
    END DO

    !If there are any tiles that pass the above, make sure it is all of them
    IF (n_counted == nsoilt) THEN
      soil_pts             = soil_pts + 1
      ainfo%soil_index(soil_pts) = l
      ainfo%l_soil_point(l)      = .TRUE.
    ELSE IF (n_counted > 0 .AND. n_counted /= nsoilt) THEN
      CALL log_fatal(RoutineName,                                              &
                     "Gridboxes must be entirely soil or land ice")
      !ELSE
        !It's not a soil point. Do nothing
    END IF
  END IF !nsoilt == 1
END DO !land_pts

CALL log_info(RoutineName,                                                     &
              "Number of soil points = " // TRIM(to_string(soil_pts)))

IF ( soil_pts <= 0 ) THEN
  CALL log_warn(RoutineName,                                                   &
                "There are no soil points - any land points are land ice")
END IF

!Set up up soilt tile points, index and frac
IF ( nsoilt == 1 ) THEN
  m = 1
  !Direct mapping of soil tile points to soil points
  ainfo%soilt_pts(1)     = soil_pts
  ainfo%soilt_index(:,m) = ainfo%soil_index

  !All fractions must equal 1 by definition
  ainfo%frac_soilt(:,m)  = 1.0

ELSE !nsoilt == nsurft

  DO m = 1,nsoilt

    !Set to zero initially to ensure we don't make a mess of gbm calculations
    ainfo%soilt_pts(m) = 0
    DO l = 1, land_pts
      ainfo%frac_soilt(l,m)  = 0.0
    END DO

    !Now work through each soil point, updating the number of points, index
    !and fraction as appropriate.
    !Assumes all non-zero fractions are non-negligible
    DO l = 1, soil_pts
      i = ainfo%soil_index(l)
      IF (ainfo%frac_surft(i,m) > EPSILON(ainfo%frac_surft(i,m))) THEN
        ainfo%soilt_pts(m)     = ainfo%soilt_pts(m) + 1
        ainfo%soilt_index(i,m) = i
        ainfo%frac_soilt(i,m)  = ainfo%frac_surft(i,m)
      END IF
    END DO

  END DO
END IF


! This accounts for some of the water that remains unfrozen when the soil is
! very cold
DO i = 1,soil_pts
  l = ainfo%soil_index(i)
  psparms%sthu_min_soilt(l,:,:) =                                              &
    ( dpsidt * zerodegc / psparms%sathh_soilt(l,:,:) )**                       &
    ( -1.0 / psparms%bexp_soilt(l,:,:) )
END DO

! Calculate soil moisture content from wetness
DO i = 1,sm_levels
  progs%smcl_soilt(:,:,i) = rho_water * dzsoil(i) *                            &
    jules_vars%sthuf_soilt(:,:,i) * psparms%smvcst_soilt(:,:,i)
END DO

!-----------------------------------------------------------------------------
! Set the normalisation factor for vertical profile of litter input.
!-----------------------------------------------------------------------------
IF (l_layeredc) THEN
  CALL calc_litc_norm(litc_norm)
END IF

!-----------------------------------------------------------------------------
! If using the two-tile urban schemes and only a combined urban fraction is
! given then split the fraction between the canyon and roof. This has to be
! done here instead of in init_urban to be consistent with triffid.
!-----------------------------------------------------------------------------
IF ( l_urban2t ) THEN
  CALL log_info(RoutineName,                                                   &
                "Either URBAN-2T or MORUSES is in use - splitting urban " //   &
                "tile into canyon/roof if needed")

  ! To maintain bit comparability in generate_anthropogenic_heat
  IF ( ANY( urban_param%wrr_gb(:) > 0.0 ) ) l_anthrop_heat_use_wrr = .TRUE.

  DO l = 1, land_pts
    IF ( ainfo%frac_surft(l,urban_canyon) >                                    &
                       EPSILON(ainfo%frac_surft(l,urban_canyon)) .AND.         &
         ainfo%frac_surft(l,urban_roof) == 0.0 ) THEN
      urban_fraction             = ainfo%frac_surft(l,urban_canyon)
      ainfo%frac_surft(l,urban_canyon) = urban_fraction * urban_param%wrr_gb(l)
      ainfo%frac_surft(l,urban_roof)   = urban_fraction -                      &
                                   ainfo%frac_surft(l,urban_canyon)
    ELSE IF ( ainfo%frac_surft(l,urban_canyon) >                               &
                       EPSILON(ainfo%frac_surft(l,urban_canyon)) .AND.         &
              ainfo%frac_surft(l,urban_roof) >                                 &
                       EPSILON(ainfo%frac_surft(l,urban_roof)) .AND.           &
              firstcall ) THEN
      CALL log_info(RoutineName,                                               &
         "URBAN-2T/M input being used to initialise. " //                      &
         " Splitting not required as roof fraction already exists")
      firstcall = .FALSE.
    END IF
  END DO
END IF

!-----------------------------------------------------------------------------
!   If using TRIFFID (with or without competing veg), ensure that fractions of
!   PFTs are not below minimum. Only do this over soil points - land ice
!   points should have zero fractions.
!-----------------------------------------------------------------------------
! Set up a flag to see if any points were reset to frac_min
reset_done = .FALSE.

IF ( l_triffid .AND. l_recon ) THEN
  DO j = 1,soil_pts
    i = ainfo%soil_index(j)
    IF ( ANY( ainfo%frac_surft(i,:) < frac_min ) ) THEN
      ! Reset all small values. Renormalisation is done later, but will fail
      ! if frac_min is sufficiently large. We only reset natural PFT tiles.
      WHERE ( ainfo%frac_surft(i,1:nnpft) < frac_min )
        ainfo%frac_surft(i,1:nnpft) = frac_min
      END WHERE
      reset_done = .TRUE.
    END IF
  END DO

  IF ( reset_done ) THEN
    CALL log_warn(RoutineName,                                                 &
                  "frac < frac_min at one or more points - reset to " //       &
                  "frac_min at those points")
  END IF
END IF

!-----------------------------------------------------------------------------
!   If using l_croprotate ensure that fractions of
!   crop PFTs are not below minimum. Only do this over soil points- land ice
!   points should have zero fractions.
!   This ensures that each crop fraction is initialised properly so it can be
!   used later. If a fraction is zero at the start it cannot be used later.
!-----------------------------------------------------------------------------

IF (l_croprotate) THEN
  DO j = 1, soil_pts
    i = ainfo%soil_index(j)
    IF ( ANY( ainfo%frac_surft(i,nnpft+1:nnpft + ncpft) < frac_min ) ) THEN
      ! Reset all small values. Renormalisation is done later, but will fail
      ! if frac_min is sufficiently large. We only reset crop PFT tiles.
      DO n = 1,ncpft
        WHERE ( ainfo%frac_surft(:,n + nnpft) < frac_min )
          ainfo%frac_surft(:,n + nnpft) = frac_min
        END WHERE
      END DO
      reset_done = .TRUE.
    END IF
  END DO

  IF ( reset_done ) THEN
    CALL log_warn(RoutineName,                                                 &
                "frac < frac_min at one or more points - reset to " //         &
                "frac_min at those points")
  END IF
END IF

!-----------------------------------------------------------------------------
! Initialise soil bio and hum pools from soil carbon.
!-----------------------------------------------------------------------------
!IF ( soil_bgc_model == soil_model_4pool ) THEN
!  !Triffid is incompatible with soil tiling at present. Hard code soil tile
!  !index to 1 using m = 1
!  m = 1
!  DO n = 1,dim_cslayer
!!    progs%ns_pool_gb(:,n,3) = progs%cs_pool_soilt(:,m,n,3) / bio_hum_cn
!    progs%ns_pool_gb(:,n,4) = progs%cs_pool_soilt(:,m,n,4) / bio_hum_cn
!  END DO
!END IF

!-----------------------------------------------------------------------------
! Reconfigure ECOSSE.
! For now we always call this (regardless of l_recon) so as to deal with
! zero (or tiny) pools.
!-----------------------------------------------------------------------------
IF ( soil_bgc_model == soil_model_ecosse ) THEN
  CALL ecosse_init(psparms%soil_pH_soilt,ainfo%frac_surft,progs%cs_pool_soilt)
END IF

!-----------------------------------------------------------------------------
! Initialise soil biogeochemistry drivers.
! At present this need only be done for ECOSSE.
!-----------------------------------------------------------------------------
IF ( soil_bgc_model == soil_model_ecosse ) THEN
  ! Most of these arguments are not used in this call (but must be present).
  ! However under some circumstances the values of sthf_soilt, sthu_soilt and
  ! t_soil_soilt passed here are used to set initial values - so give sensible
  ! values!
  CALL increment_soil_drivers( call_initial, land_pts, nstep_dummy,            &
                               l_run_model_dummy, trif_vars%deposition_n_gb,   &
                               qbase_l_soilt_dummy, psparms%sthf_soilt,        &
                               psparms%sthu_soilt, progs%t_soil_soilt,         &
                               w_flux_soilt_dummy,                             &
                               ainfo%soil_index,                               &
                               ! TYPES
                               soilecosse )
END IF

!-----------------------------------------------------------------------------
! If using crop model, fill lai and canht from croplai_cpft and cropcanht if
! lai and
! canht_pft were not prognostics (whether they're prognostics is specified in
! required_vars_for_configuration)
!-----------------------------------------------------------------------------
IF ( l_crop ) THEN
  IF ( l_phenol ) THEN
    DO n = 1,ncpft
      crop_vars%croplai_cpft(:,n) = progs%lai_pft(:,n + nnpft)
    END DO
  ELSE
    DO n = 1,ncpft
      progs%lai_pft(:,n + nnpft) = crop_vars%croplai_cpft(:,n)
    END DO
  END IF

  IF ( l_triffid ) THEN
    DO n = 1,ncpft
      crop_vars%cropcanht_cpft(:,n) = progs%canht_pft(:,n + nnpft)
    END DO
  ELSE
    DO n = 1,ncpft
      progs%canht_pft(:,n + nnpft) = crop_vars%cropcanht_cpft(:,n)
    END DO
  END IF

  ! ensure that crop leaf area index is not below minimum
  reset_done = .FALSE.
  DO i = 1,land_pts
    IF ( ANY( crop_vars%croplai_cpft(i,:) < croplai_min ) ) THEN
      ! Reset all small values. We only reset crop PFT tiles
      WHERE ( crop_vars%croplai_cpft(i,:) < croplai_min )
        crop_vars%croplai_cpft(i,:) = croplai_min
      END WHERE
      reset_done = .TRUE.
    END IF
  END DO

  IF ( reset_done ) THEN
    CALL log_warn(RoutineName,                                                 &
                "croplai_cpft < croplai_min at one or more points - " //       &
                "reset to croplai_min at those points")
    DO n = 1,ncpft
      progs%lai_pft(:,n + nnpft) = crop_vars%croplai_cpft(:,n)
    END DO
  END IF

  ! ensure that crop canopy height is not below minimum
  reset_done = .FALSE.
  DO i = 1,land_pts
    IF ( ANY( crop_vars%cropcanht_cpft(i,:) < cropcanht_min ) ) THEN
      ! Reset all small values. We only reset crop PFT tiles
      WHERE ( crop_vars%cropcanht_cpft(i,:) < cropcanht_min )
        crop_vars%cropcanht_cpft(i,:) = cropcanht_min
      END WHERE
      reset_done = .TRUE.
    END IF
  END DO

  IF ( reset_done ) THEN
    CALL log_warn(RoutineName,                                                 &
                "cropcanht < cropcanht_min at one or more points - " //        &
                "reset to cropcanht_min at those points")
    DO n = 1,ncpft
      progs%canht_pft(:,n + nnpft) = crop_vars%cropcanht_cpft(:,n)
    END DO
  END IF

  ! ensure that crop root carbon is not below minimum
  reset_done = .FALSE.
  DO i = 1,land_pts
    IF ( ANY( crop_vars%rootc_cpft(i,:) < croprootc_min ) ) THEN
      ! Reset all small values.
      WHERE ( crop_vars%rootc_cpft(i,:) < croprootc_min )
        crop_vars%rootc_cpft(i,:) = croprootc_min
      END WHERE
      reset_done = .TRUE.
    END IF
  END DO

  IF ( reset_done ) THEN
    CALL log_warn(RoutineName,                                                 &
                "rootc_cpft < croprootc_min at one or more points - " //       &
                "reset to croprootc_min at those points")
  END IF
END IF

!-----------------------------------------------------------------------------
! Check that frac_surft sums to 1.0 (with a bit of leeway).
!-----------------------------------------------------------------------------
IF ( l_recon .AND. .NOT. l_croprotate) THEN
  DO i = 1,land_pts
    ! If the discrepancy is big enough, bail
    IF ( ABS( SUM(ainfo%frac_surft(i,:)) - 1.0 ) >= 1.0e-2 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "frac does not sum to 1 at point " //                     &
                     TRIM(to_string(i)) //                                     &
                     " and the discrepancy is too big to be removed")
    ELSE IF ( ABS( SUM(ainfo%frac_surft(i,:)) - 1.0 ) > 1.0e-4 ) THEN
      ! Correct a small discrepancy
      CALL log_warn(RoutineName,                                               &
                    "frac does not sum to 1 at point " //                      &
                     TRIM(to_string(i)) //                                     &
                     " - removing small discrepancy")
      ! Ignore small discrepancies and (re)normalise
      ainfo%frac_surft(i,:) = ainfo%frac_surft(i,:) / SUM(ainfo%frac_surft(i,:))
    END IF

    ! Ignore any discrepancy below the threshold completely
  END DO
END IF

!-----------------------------------------------------------------------------
! Process the ice fraction field.
! Identify land ice points, only if the ice surface type is specified.
!-----------------------------------------------------------------------------
lice_pts        = 0
ainfo%lice_index(:)   = 0
ainfo%l_lice_point(:) = .FALSE.

IF ( ice > 0 .AND. .NOT. l_elev_land_ice ) THEN
  DO l = 1,land_pts
    IF ( ainfo%frac_surft(l,ice) > EPSILON(ainfo%frac_surft(l,ice)) ) THEN
      ! This is a land ice point.
      lice_pts             = lice_pts + 1
      ainfo%lice_index(lice_pts) = l
      ainfo%l_lice_point(l)      = .TRUE.

      ! If reconfiguring, ensure pools are set (mostly to zero) over ice.
      IF ( l_recon ) THEN
        IF ( l_triffid ) THEN
          progs%canht_pft(l,:)            = 0.0
          progs%lai_pft(l,:)              = 0.0
          ainfo%frac_surft(l,1:nnpft)     = 0.0
        END IF
        IF ( soil_bgc_model == soil_model_4pool ) THEN
          progs%n_inorg_gb(l)             = 0.0
          progs%n_inorg_soilt_lyrs(l,:,:) = 0.0
        END IF
        IF ( soil_bgc_model == soil_model_ecosse .OR.                          &
             soil_bgc_model == soil_model_4pool ) THEN
          progs%cs_pool_soilt(l,:,:,:)    = 1.0e-6
          progs%ns_pool_gb(l,:,:)         = 1.0e-6
        END IF
      END IF

      ! At present, land ice and soil points are mutually exclusive.
      ! Check this is not a soil point
      IF ( ANY(ainfo%soil_index == l) ) THEN
        CALL log_fatal(RoutineName,                                            &
                       "Land ice points and soil points are mutually " //      &
                       "exclusive")
      END IF

      ! Check that ice fraction is one (cannot have partial ice coverage).
      IF ( ABS(ainfo%frac_surft(l,ice) - 1.0) > EPSILON(1.0) ) THEN
        CALL log_error(RoutineName,                                            &
                       "Ice fraction must be 1 at an ice point")
      END IF

    END IF  !  land ice points
  END DO

END IF  !  ice > 0 .AND. .NOT.l_elev_land_ice

!-----------------------------------------------------------------------------
! fall back to the old smvcst (NOT soil) criteria when using the tiled land ice
! subsurface - consistency with the UM
!-----------------------------------------------------------------------------
IF ( l_elev_land_ice ) THEN
  !Only need to check the 1st soil tile as any problems will have been
  !picked up by the soil_pts test above. Set m = 1
  m = 1
  DO l = 1,land_pts
    IF ( psparms%smvcst_soilt(l,m,1) <=                                        &
         EPSILON(psparms%smvcst_soilt(l,m,1)) ) THEN
      lice_pts = lice_pts + 1
      ainfo%lice_index(lice_pts) = l
      ainfo%l_lice_point(l)      = .TRUE.
    END IF
    !
    !Reverse of soil criteria, so automatically exclusive.
    !Partial ice fractions (e.g in each elevation class) allowed for land ice
    !points here
    !
  END DO
END IF

CALL log_info(RoutineName,                                                     &
              "Number of land ice points = " // TRIM(to_string(lice_pts)))

!-----------------------------------------------------------------------------
! Check that all land points have been identified as either soil or ice
!-----------------------------------------------------------------------------
IF ( soil_pts + lice_pts /= land_pts ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "All points should be either soil or land ice points - " //   &
                 "have land_pts = " // TRIM(to_string(land_pts)) //            &
                 " and soil_pts + lice_pts = " //                              &
                 TRIM(to_string(soil_pts + lice_pts)))
END IF

! Additional check using ainfo%l_soil_point and ainfo%l_lice_point
DO l = 1, land_pts
  IF (ainfo%l_soil_point(l) .AND. ainfo%l_lice_point(l)) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Point " // TRIM(to_string(l)) // " has been assigned" //   &
                   " as both a soil and land ice point. Please review the"//   &
                   " consistency for the soil and frac ancils.")
  ELSE IF ( .NOT. ainfo%l_soil_point(l) .AND. .NOT. ainfo%l_lice_point(l)) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Point " // TRIM(to_string(l)) // " has been assigned" //   &
                   " as neither a soil or land ice point. Please review"  //   &
                   " the consistency for the soil and frac ancils.")
  END IF
END DO

!-----------------------------------------------------------------------------
! Set up tile index
!-----------------------------------------------------------------------------
CALL tilepts( land_pts, ainfo%frac_surft, ainfo%surft_pts, ainfo%surft_index,  &
              ainfo%l_lice_point, ainfo%l_lice_surft )

! For URBAN-2T or MORUSES: Check that urban canyons also have roofs
IF ( l_urban2t ) THEN
  IF ( ainfo%surft_pts(urban_canyon) /= ainfo%surft_pts(urban_roof) ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "URBAN-2T or MORUSES - # canyons /= # roofs")
  END IF
END IF

!-----------------------------------------------------------------------------
! Deal with "simple" initialisation of snow variables.
!-----------------------------------------------------------------------------
IF ( total_snow ) CALL total_snow_init(ainfo, progs)

!-----------------------------------------------------------------------------
! Calculate frozen and unfrozen fractions of soil moisture.
! freeze_soil assumes bexp_soilt, sathh_soilt, smvcst_soilt are
! constant in soil column, so loop around soil layers to allow
! depth varying soil properties and to maintain compatibility with the UM
!-----------------------------------------------------------------------------
DO i = 1,sm_levels
  DO m = 1,nsoilt
    CALL freeze_soil (land_pts, 1, psparms%bexp_soilt(:,m,i), dzsoil(i:i),     &
                      psparms%sathh_soilt(:,m,i), progs%smcl_soilt(:,m,i),     &
                      progs%t_soil_soilt(:,m,i), psparms%smvcst_soilt(:,m,i),  &
                      psparms%sthu_soilt(:,m,i), psparms%sthf_soilt(:,m,i))
  END DO
END DO
!-----------------------------------------------------------------------------
! Finish initialising TOPMODEL
!-----------------------------------------------------------------------------
IF ( l_top ) THEN
  !Calculate fitting parameters
  CALL calc_fit_fsat(toppdm,ainfo)
  CALL topmodel_init(psparms,toppdm,ainfo)
END IF

!-----------------------------------------------------------------------------
! Initialise FLake.
!-----------------------------------------------------------------------------

IF ( l_flake_model                                                             &
   .AND. ( .NOT. l_aggregate)                                                  &
   .AND. (land_pts > 0 ) ) THEN
  CALL flake_init(ainfo, progs, lake_vars)
END IF ! FLake

!---------------------------------------------------------------------------
! Finish initialising irrigation
! Process pft names to be assigned irr fraction (if not all)
!---------------------------------------------------------------------------
CALL current_model_time(year,month,day,TIME)
startyr   = year
startmon  = month
startday  = day
starttime = TIME

IF ( l_irrig_dmd ) THEN
  CALL assign_irrig_fraction(crop_vars,ainfo)
  IF ( l_irrig_limit ) THEN
    DO ip = 1, np_rivers
      rivers_sto_per_m2_rgrid(ip) = rivers%rivers_sto_rp(ip) /                 &
                                      rivers%rivers_boxareas_rp(ip)
    END DO
    CALL scatter_land_from_riv_field(rivers_sto_per_m2_rgrid,                  &
                                     rivers%rivers_sto_per_m2_on_landpts,      &
                                     rivers)
  END IF
END IF
!-----------------------------------------------------------------------------
! If we are using the disaggregator with random rainfall, initialise the
! random seed
!-----------------------------------------------------------------------------
IF ( l_daily_disagg .AND. precip_disagg_method > 1 ) THEN
  IF ( .NOT. dump_file ) THEN
    ! If not using a dump file, we need to initialise a new random seed on the
    ! master task and broadcast it to all other tasks
    IF ( is_master_task() ) THEN
      CALL RANDOM_SEED()
      CALL RANDOM_SEED( get = progs%seed_rain )
    END IF
    CALL mpi_bcast(progs%seed_rain, SIZE(progs%seed_rain), mpi_integer,        &
                   master_task_id, mpi_local_comm, ERROR)
  END IF

  ! Make sure that we put the seed, whether it came from the dump or was
  ! broadcast by the master task
  CALL RANDOM_SEED( put = progs%seed_rain )
END IF

IF (l_nitrogen .AND. l_layeredC) THEN
  IF ( soil_bgc_model == soil_model_4pool ) THEN
    ! Initialise n_inorg_avail_pft so it can be leached before the first call
    ! to TRIFFID.
    CALL init_layeredcn(ainfo%soil_index, progs)
  END IF
END IF

RETURN

END SUBROUTINE init_ic


FUNCTION get_default_ic_values(total_snow) RESULT(defaults_dict)

! USE statments
USE dictionary_mod, ONLY: dict, dict_set, dict_create

USE dump_mod,       ONLY: max_var_dump
USE required_vars_for_configuration_mod, ONLY: required_vars_for_configuration

USE fire_mod,        ONLY: fire_inis

USE lake_mod,        ONLY: lake_fetch_0, lake_h_mxl_0, lake_shape_0,           &
                           lake_T_mean_0, lake_T_mxl_0, lake_T_ice_0,          &
                           lake_H_ice_0

USE metstats_mod,    ONLY: metstats_inis

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Gets default values for initialisation
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
LOGICAL, INTENT(IN) :: total_snow

CHARACTER(LEN=identifier_len) :: identifiers(max_var_dump)
                             ! The variable identifiers of the required
                             ! variables

CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                             ! The variable identifiers of the ancil
                             ! variables (not used in this subroutine)

! Work variables
INTEGER :: i !Counter
INTEGER :: nvars_required
INTEGER :: nvars_from_ancil

! Return type
TYPE(dict) :: defaults_dict

!End of header
!-----------------------------------------------------------------------------

!Get the list of variables we need to have default variables for
nvars_required   = 0
nvars_from_ancil = 0
CALL required_vars_for_configuration(nvars_required, identifiers,              &
                                     nvars_from_ancil, vars_from_ancil,        &
                                     l_output_mode,                            &
                                     total_snow, .FALSE.)

!Create the dictionary containing the default values
defaults_dict = dict_create(nvars_required, 1.0)

!Set the default values one by one
DO i = 1, nvars_required
  SELECT CASE ( identifiers(i) )

    !      CASE ( 'canopy' )
    !      CASE ( 'cs' )
    !      CASE ( 'gs' )
    !      CASE ( 'snow_tile' )
    !      CASE ( 'sthuf' )
    !      CASE ( 't_soil' )
    !      CASE ( 'tstar_tile' )
    !      CASE ( 'lai' )
    !      CASE ( 'canht' )
    !      CASE ( 'frac' )
    !      CASE ( 'sthzw' )
    !      CASE ( 'zw' )
    !      CASE ( 'rgrain' )
    !      CASE ( 'plantNumDensity' )

          !Metstat module variables
  CASE ( 'temp_max_00h_r' )
    CALL dict_set(defaults_dict, 'temp_max_00h_r', metstats_inis%temp_max_00h)
  CASE ( 'temp_ave_00h_r' )
    CALL dict_set(defaults_dict, 'temp_ave_00h_r', metstats_inis%temp_ave_00h)
  CASE ( 'prec_tot_00h_r' )
    CALL dict_set(defaults_dict, 'prec_tot_00h_r', metstats_inis%prec_tot_00h)
  CASE ( 'prec_tot_12h_r' )
    CALL dict_set(defaults_dict, 'prec_tot_12h_r', metstats_inis%prec_tot_12h)
  CASE ( 'rhum_min_00h_r' )
    CALL dict_set(defaults_dict, 'rhum_min_00h_r', metstats_inis%rhum_min_00h)
  CASE ( 'dewp_ave_00h_r' )
    CALL dict_set(defaults_dict, 'dewp_ave_00h_r', metstats_inis%dewp_ave_00h)
  CASE ( 'wind_ave_00h_r' )
    CALL dict_set(defaults_dict, 'wind_ave_00h_r', metstats_inis%wind_ave_00h)
  CASE ( 'temp_max_00h' )
    CALL dict_set(defaults_dict, 'temp_max_00h',   metstats_inis%temp_max_00h)
  CASE ( 'temp_ave_00h' )
    CALL dict_set(defaults_dict, 'temp_ave_00h',   metstats_inis%temp_ave_00h)
  CASE ( 'temp_ave_nday' )
    CALL dict_set(defaults_dict, 'temp_ave_nday',  metstats_inis%temp_ave_nday)
  CASE ( 'temp_pnt_12h' )
    CALL dict_set(defaults_dict, 'temp_pnt_12h',   metstats_inis%temp_pnt_12h)
  CASE ( 'prec_tot_00h' )
    CALL dict_set(defaults_dict, 'prec_tot_00h',   metstats_inis%prec_tot_00h)
  CASE ( 'prec_tot_12h' )
    CALL dict_set(defaults_dict, 'prec_tot_12h',   metstats_inis%prec_tot_12h)
  CASE ( 'rhum_min_00h' )
    CALL dict_set(defaults_dict, 'rhum_min_00h',   metstats_inis%rhum_min_00h)
  CASE ( 'rhum_pnt_12h' )
    CALL dict_set(defaults_dict, 'rhum_pnt_12h',   metstats_inis%rhum_pnt_12h)
  CASE ( 'dewp_ave_00h' )
    CALL dict_set(defaults_dict, 'dewp_ave_00h',   metstats_inis%dewp_ave_00h)
  CASE ( 'wind_ave_00h' )
    CALL dict_set(defaults_dict, 'wind_ave_00h',   metstats_inis%wind_ave_00h)
  CASE ( 'wind_pnt_12h' )
    CALL dict_set(defaults_dict, 'wind_pnt_12h',   metstats_inis%wind_pnt_12h)

    !Fire module variables
  CASE ( 'fire_canadian_ffmc' )
    CALL dict_set(defaults_dict, 'fire_canadian_ffmc'     , fire_inis%canadian_ffmc )
  CASE ( 'fire_canadian_ffmc_mois' )
    CALL dict_set(defaults_dict, 'fire_canadian_ffmc_mois', fire_inis%canadian_ffmc_mois)
  CASE ( 'fire_canadian_dmc' )
    CALL dict_set(defaults_dict, 'fire_canadian_dmc'      , fire_inis%canadian_dmc )
  CASE ( 'fire_canadian_dc' )
    CALL dict_set(defaults_dict, 'fire_canadian_dc'       , fire_inis%canadian_dc )
  CASE ( 'fire_mcarthur_r_dr' )
    CALL dict_set(defaults_dict, 'fire_mcarthur_r_dr'     , fire_inis%mcarthur_r_dr )
  CASE ( 'fire_mcarthur_n_dr' )
    CALL dict_set(defaults_dict, 'fire_mcarthur_n_dr'     , fire_inis%mcarthur_n_dr)
  CASE ( 'fire_nesterov' )
    CALL dict_set(defaults_dict, 'fire_nesterov'          , fire_inis%nesterov_index )

    !FLake variables
  CASE ( 'lake_fetch_gb' )
    CALL dict_set(defaults_dict, 'lake_fetch_gb'       , lake_fetch_0)
  CASE ( 'lake_h_mxl_gb' )
    CALL dict_set(defaults_dict, 'lake_h_mxl_gb'       , lake_h_mxl_0)
  CASE ( 'lake_shape_factor_gb' )
    CALL dict_set(defaults_dict, 'lake_shape_factor_gb', lake_shape_0)
  CASE ( 'lake_t_mean_gb' )
    CALL dict_set(defaults_dict, 'lake_t_mean_gb'      , lake_T_mean_0)
  CASE ( 'lake_t_mxl_gb' )
    CALL dict_set(defaults_dict, 'lake_t_mxl_gb'       , lake_T_mxl_0)
  CASE ( 'lake_t_ice_gb' )
    CALL dict_set(defaults_dict, 'lake_t_ice_gb'       , lake_T_ice_0)
  CASE ( 'lake_h_ice_gb' )
    CALL dict_set(defaults_dict, 'lake_h_ice_gb'       , lake_H_ice_0)

    !Snow variables
    !      CASE ( 'rho_snow' )
    !      CASE ( 'snow_depth' )
    !      CASE ( 'snow_grnd' )
    !      CASE ( 'nsnow' )
    !      CASE ( 'snow_ds' )
    !      CASE ( 'snow_ice' )
    !      CASE ( 'snow_liq' )
    !      CASE ( 'tsnow' )

  CASE DEFAULT
    CALL log_info("get_default_ic_values",                                     &
                   "No default value defined. Value must be defined in namelist - " &
                   // TRIM(identifiers(i)))
  END SELECT
END DO

RETURN

END FUNCTION get_default_ic_values

END MODULE init_ic_mod
#endif
