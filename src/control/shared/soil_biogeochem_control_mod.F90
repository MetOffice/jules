#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology, 2017.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE soil_biogeochem_control_mod

!-----------------------------------------------------------------------------
! Description:
!   Control routine for soil biogeochemistry.
!   At present this is only called for the ECOSSE soil model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nsoilt, dim_soil_n_pool, dim_cs1, nz_soilc=>dim_cslayer

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE
PUBLIC call_initial, increment_soil_drivers, soil_biogeochem_control

! Module parameters.
INTEGER, PARAMETER ::                                                          &
  ! Values to indicate from where increment_soil_drivers has been called.
  ! These should be unique.
  call_initial = 0,                                                            &
    ! Value to indicate a call during model initialisation.
  call_before = 1,                                                             &
    ! Value to indicate a call before the soil model is (potentially) called.
  call_after  = 2
    ! Value to indicate a call after the soil model is (potentially) called.

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'SOIL_BIOGEOCHEM_CONTROL_MOD'

CONTAINS

!#############################################################################

SUBROUTINE soil_biogeochem_control( land_pts, triffid_call, vs_pts,            &
        vs_index, deposition_n_gb, frac_surft_start,                           &
        qbase_l_soilt, sthf_soilt, sthu_soilt, w_flux_soilt, t_soil_soilt,     &
    ! These arguments replace USE statements
        ! p_s_params
        bexp_soilt, clay_soilt, sathh_soilt, smvcst_soilt,                     &
        smvcwt_soilt, soil_ph_soilt,                                           &
        ! p_s_params_alloc
        dim_cslayer,                                                           &
        ! soil_ecosse_vars_mod
        co2_soil_gb, n2o_soil_gb, n2o_denitrif_gb,                             &
        n2o_nitrif_gb, n2o_partial_nitrif_gb,                                  &
        n2_denitrif_gb, n_denitrification_gb,                                  &
        n_leach_amm_gb, n_leach_nit_gb,                                        &
        n_nitrification_gb, n_soil_pool_soilt,                                 &
        no_soil_gb,                                                            &
        soil_c_add, soil_n_add,                                                &
        !trif_vars_mod
        lit_c_orig_pft, lit_n_orig_pft,                                        &
        minl_n_gb, minl_n_pot_gb,                                              &
        immob_n_gb, immob_n_pot_gb, resp_s_diag_gb,                            &
        resp_s_pot_diag_gb, fn_gb,                                             &
        n_fertiliser_add, n_fix_add, n_uptake_extract,                         &
        !prognostics
        cs_pool_soilt,                                                         &
        !ancil_info
        soil_index,                                                            &
        !TYPES
        soilecosse)

! TYPE Definitions
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type

USE ecosse_control_mod, ONLY:                                                  &
  ! imported procedures
  ecosse_control

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_ecosse,                                                           &
  ! imported scalars
  soil_bgc_model

USE jules_soil_mod, ONLY:                                                      &
  ! imported scalars
  sm_levels

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  dt_soilc, l_soil_N

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  ntype, npft

USE model_time_mod, ONLY:                                                      &
  ! imported scalars
  timestep_number

USE timestep_mod, ONLY:                                                        &
  ! imported scalars
  timestep

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Control routine for soil biogeochemistry.
!   At present it only deals with ECOSSE but other models could be added.
!-----------------------------------------------------------------------------

!TYPE Definitions
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  triffid_call,                                                                &
    ! Indicates if the dynamic vegetation model was called earlier in
    ! the current timestep.
    ! 0 = TRIFFID was called.
    ! 1 = TRIFFID was not called.
  vs_pts
    ! The number of points with vegetation and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of points with vegetation and/or soil.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  deposition_n_gb(land_pts),                                                   &
    ! Atmospheric deposition of N to land surface (kg m-2 s-1).
 frac_surft_start(land_pts,ntype),                                             &
    !  Fractional coverage of surface types at start of timestep.
  qbase_l_soilt(land_pts,nsoilt,sm_levels+1),                                  &
    ! Lateral flux of water from each soil layer (kg m-2 s-1).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content as a fraction of saturation.
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content as a fraction of saturation.
  w_flux_soilt(land_pts,nsoilt,0:sm_levels),                                   &
    ! Downward water flux at bottom of each soil layer (kg m-2 s-1).
  t_soil_soilt(land_pts,nsoilt,sm_levels)
    ! Subsurface temperature in each layer (K).

!-----------------------------------------------------------------------------
! These arguments replace USE statements
!-----------------------------------------------------------------------------
! p_s_params_alloc
INTEGER, INTENT(IN) :: dim_cslayer
! p_s_params
REAL(KIND=real_jlslsm), INTENT(IN) :: bexp_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: clay_soilt(land_pts,nsoilt,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN) :: sathh_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smvcst_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smvcwt_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: soil_ph_soilt(land_pts,nsoilt,dim_cslayer)
! soil_ecosse_vars_mod
REAL(KIND=real_jlslsm), INTENT(OUT) :: co2_soil_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_soil_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_denitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_nitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_partial_nitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2_denitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_denitrification_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_leach_amm_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_leach_nit_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_nitrification_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_soil_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_soil_n_pool)
    ! N in soil pools (kg m-2).
REAL(KIND=real_jlslsm), INTENT(OUT) :: no_soil_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: soil_c_add(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: soil_n_add(land_pts)
!trif_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN)  :: lit_c_orig_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN)  :: lit_n_orig_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: minl_n_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  minl_n_pot_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  immob_n_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  immob_n_pot_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  resp_s_diag_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  resp_s_pot_diag_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  fn_gb(land_pts,dim_cslayer)
! prognostics
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  cs_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1)
    ! Soil carbon (kg m-2).
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  n_fertiliser_add(land_pts,nsoilt,nz_soilc)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  n_fix_add(land_pts,nsoilt, nz_soilc)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  n_uptake_extract(land_pts,nsoilt,nz_soilc)

INTEGER, INTENT(IN) :: soil_index(land_pts)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'SOIL_BIOGEOCHEM_CONTROL'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  istep,                                                                       &
    ! Work.
  nstep
    ! Number of JULES timesteps per soil model timestep.

LOGICAL ::                                                                     &
  l_run_model   ! .TRUE. on timesteps when the soil model is run.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Work out if the soil model is to be called this timestep.
!-----------------------------------------------------------------------------
nstep = NINT( dt_soilc / timestep )
istep = MOD( timestep_number, nstep )
IF ( istep == 0 ) THEN
  l_run_model = .TRUE.
ELSE
  l_run_model = .FALSE.
END IF

!-----------------------------------------------------------------------------
! Update driving variables.
!-----------------------------------------------------------------------------
CALL increment_soil_drivers( call_before, land_pts, nstep, l_run_model,        &
                             deposition_n_gb, qbase_l_soilt, sthf_soilt,       &
                             sthu_soilt, t_soil_soilt, w_flux_soilt,           &
                             soil_index,                                       &
                             ! TYPES
                             soilecosse)

!-----------------------------------------------------------------------------
! If using ECOSSE, initialise increment for mass conservation.
! This value is retained on timesteps when ECOSSE is not called.
!-----------------------------------------------------------------------------
IF ( soil_bgc_model == soil_model_ecosse ) THEN
  soil_c_add(:) = 0.0
  IF ( l_soil_N ) soil_n_add(:) = 0.0
END IF

!-----------------------------------------------------------------------------
! Call the science control routine for the chosen model, if the model is to be
! called on this timestep.
! At present this can only be ECOSSE.
!-----------------------------------------------------------------------------
IF ( l_run_model ) THEN
  CALL ecosse_control( land_pts, triffid_call, vs_pts, vs_index,               &
                       frac_surft_start,                                       &
                    ! These arguments replace USE statements
                       ! p_s_params
                       bexp_soilt, clay_soilt, sathh_soilt, smvcst_soilt,      &
                       smvcwt_soilt, soil_ph_soilt,                            &
                       ! p_s_params_alloc
                       dim_cslayer, sm_levels,                                 &
                       ! soil_ecosse_vars_mod
                       co2_soil_gb, n2o_soil_gb, n2o_denitrif_gb,              &
                       n2o_nitrif_gb, n2o_partial_nitrif_gb,                   &
                       n2_denitrif_gb, n_denitrification_gb,                   &
                       n_leach_amm_gb, n_leach_nit_gb,                         &
                       n_nitrification_gb, n_soil_pool_soilt,                  &
                       no_soil_gb, soilecosse%qbase_l_driver,                  &
                       soilecosse%sthf_driver, soilecosse%sthu_driver,         &
                       soilecosse%tsoil_driver, soilecosse%wflux_driver,       &
                       soil_c_add, soil_n_add,                                 &
                       !trif_vars_mod
                       lit_c_orig_pft, lit_n_orig_pft,                         &
                       minl_n_gb, minl_n_pot_gb,                               &
                       immob_n_gb, immob_n_pot_gb, resp_s_diag_gb,             &
                       resp_s_pot_diag_gb, fn_gb,                              &
                       n_fertiliser_add, n_fix_add, n_uptake_extract,          &
                       !prognostics
                       cs_pool_soilt,                                          &
                       ! TYPES
                       soilecosse )
END IF

!-----------------------------------------------------------------------------
! Update driving variables.
!-----------------------------------------------------------------------------
CALL increment_soil_drivers( call_after, land_pts, nstep, l_run_model,         &
                             deposition_n_gb, qbase_l_soilt, sthf_soilt,       &
                             sthu_soilt, t_soil_soilt, w_flux_soilt,           &
                             soil_index,                                       &
                             ! TYPES
                             soilecosse)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_biogeochem_control

!#############################################################################

SUBROUTINE increment_soil_drivers(                                             &
       call_type, land_pts, nstep, l_run_model,                                &
       deposition_n_gb, qbase_l_soilt, sthf_soilt, sthu_soilt,                 &
       t_soil_soilt, w_flux_soilt, soil_index,                                 &
       ! TYPES
       soilecosse )

! Description:
!  Increment driver variables for a soil model.

! Module imports.
USE ancil_info, ONLY:                                                          &
  ! imported scalars
  soil_pts

USE ereport_mod, ONLY:                                                         &
  ! imported procedures
  ereport

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  l_driver_ave

USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type

USE jules_hydrology_mod, ONLY:                                                 &
  ! imported scalars
  l_top

USE jules_soil_mod, ONLY:                                                      &
  ! imported scalars
  sm_levels

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  call_type,                                                                   &
    ! Flag indicating where this routine has been called from.
  land_pts,                                                                    &
    ! Number of land points.
  nstep
    ! Number of JULES timesteps per soil model timestep.

TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse

LOGICAL, INTENT(IN) ::                                                         &
  l_run_model
    ! .TRUE. on timesteps when soil_model is to be run, else .FALSE.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  deposition_n_gb(land_pts),                                                   &
    ! Atmospheric deposition of N to land surface (kg m-2 s-1).
  qbase_l_soilt(land_pts,nsoilt,sm_levels+1),                                  &
    ! Lateral flux of water from each soil layer (kg m-2 s-1).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content as a fraction of saturation.
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content as a fraction of saturation.
  t_soil_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Subsurface temperature in each layer (K).
  w_flux_soilt(land_pts,nsoilt,0:sm_levels)
    ! Downward water flux at bottom of each soil layer (kg m-2 s-1).

INTEGER :: soil_index(land_pts)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INCREMENT_SOIL_DRIVERS'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  errorstatus,                                                                 &
    ! Value used for error.
  j,l
    ! Indices.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( call_type == call_initial ) THEN

  !---------------------------------------------------------------------------
  ! This is a call during initialisation.
  ! Set initial values of accumulated drivers to zero so that the
  ! accumulations can start correctly.
  !---------------------------------------------------------------------------
  soilecosse%deposition_n_driver(:) = 0.0
  soilecosse%qbase_l_driver(:,:,:)  = 0.0
  soilecosse%sthf_driver(:,:,:)     = 0.0
  soilecosse%sthu_driver(:,:,:)     = 0.0
  soilecosse%tsoil_driver(:,:,:)    = 0.0
  soilecosse%wflux_driver(:,:,:)    = 0.0
  IF ( .NOT. l_driver_ave ) THEN
    ! If the soil model is called every timestep (and hence l_driver_ave=F),
    ! the state-variable drivers still have their initial values when the
    ! model is first called. Replace these with the current state.
    ! These values will be overwritten later (correctly) if the model is
    ! not called on every timestep.
    soilecosse%sthf_driver(:,:,:)  = sthf_soilt(:,:,:)
    soilecosse%sthu_driver(:,:,:)  = sthu_soilt(:,:,:)
    soilecosse%tsoil_driver(:,:,:) = t_soil_soilt(:,:,:)
  END IF

ELSE IF ( call_type == call_before ) THEN

  !--------------------------------------------------------------------------
  ! This is call before the soil model is (potentially) run.
  !
  ! Increment fluxes, so the accumulations include values up to the end of
  ! the current timestep.
  !
  ! Note that we treat fluxes and state variables differently, so that the
  ! values passed to the soil model (averages or instantaneous values) are
  ! updated to the start of timestep for state variables, end of timestep for
  ! fluxes. The main advantage of this is that if the soil model is called
  ! every timestep, it uses state drivers (e.g. soil temperature) valid at
  ! the start of the timestep, and fluxes (e.g. water flux) that are valid
  ! over the timestep.
  !--------------------------------------------------------------------------

  DO j = 1,soil_pts
    l = soil_index(j)
    ! Deposition is accumulated on all timesteps so as to conserve mass
    ! across the atmos-land interface.
    soilecosse%deposition_n_driver(l) = soilecosse%deposition_n_driver(l)      &
                                        + deposition_n_gb(l)
    ! For instantaneous values (l_driver_ave=F) we only add to the
    ! accumulation on timesteps when the soil_model is to be run.
    IF ( l_driver_ave ) THEN
      ! We are averaging (l_driver_ave=T), so always accumulate.
      soilecosse%wflux_driver(l,:,:) = soilecosse%wflux_driver(l,:,:)          &
              + w_flux_soilt(l,:,:)
      ! We don't use the deepest layer of qbase_l_soilt.
      IF ( l_top ) THEN
        soilecosse%qbase_l_driver(l,:,:) = soilecosse%qbase_l_driver(l,:,:) +  &
                                qbase_l_soilt(l,:,1:sm_levels)
      END IF
    ELSE IF ( l_run_model ) THEN
      ! We're using instantaneous values and the model is about to be run.
      ! Get the flux over the current model timestep.
      soilecosse%wflux_driver(l,:,:) = w_flux_soilt(l,:,:)
      IF ( l_top ) soilecosse%qbase_l_driver(l,:,:) =                          &
              qbase_l_soilt(l,:,1:sm_levels)
    END IF
  END DO

  !-------------------------------------------------------------------------
  ! Calculate time averages if the soil_model will be run on this timestep.
  !-------------------------------------------------------------------------
  IF ( l_run_model ) THEN
    ! Deposition is always accumulated over all timesteps, so always
    ! calculate the average.
    soilecosse%deposition_n_driver(:) = soilecosse%deposition_n_driver(:)      &
            / REAL( nstep )
    ! Calculate other averages, if required. If averages are not being
    ! used, only one value will have been added to the accumulation (so no
    ! averaging is required).
    IF ( l_driver_ave ) THEN
      ! Calculate averages.
      soilecosse%sthf_driver(:,:,:)  = soilecosse%sthf_driver(:,:,:)           &
              / REAL( nstep )
      soilecosse%sthu_driver(:,:,:)  = soilecosse%sthu_driver(:,:,:)           &
              / REAL( nstep )
      soilecosse%tsoil_driver(:,:,:) = soilecosse%tsoil_driver(:,:,:)          &
              / REAL( nstep )
      soilecosse%wflux_driver(:,:,:) = soilecosse%wflux_driver(:,:,:)          &
              / REAL( nstep )
      IF ( l_top ) THEN
        soilecosse%qbase_l_driver(:,:,:) = soilecosse%qbase_l_driver(:,:,:)    &
                / REAL( nstep )
      END IF
    END IF
  END IF  !  l_run_model

ELSE IF ( call_type == call_after ) THEN

  ! This is a call after the soil model is (potentially) run.

  IF ( l_run_model ) THEN

    !-------------------------------------------------------------------------
    ! The soil model has already been run on this timestep.
    ! Reset accumulations.
    !-------------------------------------------------------------------------
    soilecosse%deposition_n_driver(:) = 0.0
    IF ( l_driver_ave ) THEN
      soilecosse%qbase_l_driver(:,:,:)  = 0.0
      soilecosse%sthf_driver(:,:,:)     = 0.0
      soilecosse%sthu_driver(:,:,:)     = 0.0
      soilecosse%tsoil_driver(:,:,:)    = 0.0
      soilecosse%wflux_driver(:,:,:)    = 0.0
    END IF

  END IF  !  l_run_model

  !-------------------------------------------------------------------------
  ! Increment state variables, so that on the next timestep the
  ! accumulations include values up to the start of that timestep.
  !-------------------------------------------------------------------------
  DO j = 1,soil_pts
    l = soil_index(j)
    IF ( l_driver_ave ) THEN
      ! Add to the accumulations.
      soilecosse%sthf_driver(l,:,:)  =                                         &
              soilecosse%sthf_driver(l,:,:)  + sthf_soilt(l,:,:)
      soilecosse%sthu_driver(l,:,:)  =                                         &
              soilecosse%sthu_driver(l,:,:)  + sthu_soilt(l,:,:)
      soilecosse%tsoil_driver(l,:,:) =                                         &
              soilecosse%tsoil_driver(l,:,:) + t_soil_soilt(l,:,:)
    ELSE
      ! Although we only need to set instantaneous state values at the end
      ! of the timestep BEFORE the soil model is run, for convenience we do
      ! it every timestep.
      soilecosse%sthf_driver(l,:,:)  = sthf_soilt(l,:,:)
      soilecosse%sthu_driver(l,:,:)  = sthu_soilt(l,:,:)
      soilecosse%tsoil_driver(l,:,:) = t_soil_soilt(l,:,:)
    END IF
  END DO

ELSE

  ! Unknown call_type.
  errorstatus = 101  !  A hard error.
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
                "Unrecognised value of call_type." )

END IF  !  call_type

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE increment_soil_drivers

!#############################################################################

END MODULE soil_biogeochem_control_mod
#endif
