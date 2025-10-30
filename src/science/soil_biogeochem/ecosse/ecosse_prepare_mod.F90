#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE ecosse_prepare_mod

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc=>dim_cslayer

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

!-----------------------------------------------------------------------------
! Description:
!   Module containing routines that prepare input for the ECOSSE model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  ! Private scope by default.
PUBLIC ecosse_prepare, stable_n_c

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'ECOSSE_PREPARE_MOD'

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE ecosse_prepare( land_pts, s, triffid_call, vs_pts,                  &
     vs_index, frac_surft_start, nlayer_nitrif, dt_input,                      &
     active_frac, l_add_plant_inputs, l_extract_n,                             &
     biohum_nc, residual_n, resp_frac_soil, f_root_pft,                        &
     smvc, sm_total, sm_unfrozen, sm_fc, sm_one_bar, sm_wilt, soilT_degC,      &
     water_flux_down, water_flux_lateral, veg_cover,                           &
     plant_input_c_dpm, plant_input_c_rpm,                                     &
     plant_input_n_dpm, plant_input_n_rpm,                                     &
  ! These arguments replace USE statements
     ! p_s_params
     bexp_soilt, clay_soilt, sathh_soilt, smvcst_soilt, smvcwt_soilt,          &
     soil_ph_soilt,                                                            &
     ! p_s_params_alloc
     nsoilt, dim_cslayer,                                                      &
     ! soil_ecosse_vars_mod
     qbase_l_driver, sthf_driver, sthu_driver, tsoil_driver, wflux_driver,     &
     !trif_vars_mod
     lit_c_orig_pft, lit_n_orig_pft,                                           &
     !TYPES
     soilecosse )

!-----------------------------------------------------------------------------
! Description:
!   Preparation (preprocessing) routine for ECOSSE soil biogeochemistry.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE conversions_mod, ONLY:                                                     &
  ! imported scalar parameters
  isec_per_day

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalar parameters
  psi_field_capac, psi_one_bar,                                                &
  ! imported scalars
  depth_nitrif

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported procedures
  get_residual_n, transfer_layers

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  l_soil_N,                                                                    &
  ! imported arrays
  dz_soilc

USE jules_soil_mod, ONLY:                                                      &
  ! imported scalars
  sm_levels,                                                                   &
  ! imported arrays
  dzsoil

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  npft, ntype

USE jules_vegetation_mod, ONLY:                                                &
  ! imported scalars
  triffid_period

USE pftparm, ONLY:                                                             &
  ! imported arrays
  rootd_ft

USE root_frac_mod, ONLY: root_frac

USE vmc_from_head_mod, ONLY:                                                   &
  ! imported procedures
  vmc_from_head

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  tm

!TYPES
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type

IMPLICIT NONE

!TYPES
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  s,                                                                           &
    ! Soil tile number.
  triffid_call,                                                                &
    ! Indicates if the dynamic vegetation model was called earlier in
    ! the current timestep.
    ! 0 = TRIFFID was called.
    ! 1 = TRIFFID was not called.
  vs_pts
   ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of points with veg and/or soil.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_surft_start(land_pts,ntype)
    ! Fractional coverage of surface types at start of timestep.

!-----------------------------------------------------------------------------
! Scalar arguments with intent(out).
!-----------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  nlayer_nitrif
    ! Number of layers in which nitrification and denitrification are
    ! allowed.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dt_input
    ! Timestep length for litter input and N uptake (s).

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  active_frac(nz_soilc),                                                       &
    ! The fraction of the inorganic nitrogen in a layer that can be involved
    ! in nitrification and denitrification (subject to the layer also being
    ! included in the nlayer_nitrif layers). This exists to deal with the
    ! case of a single soil layer that is much deeper than the depth to be
    ! considered for (de)nitrification.
  biohum_nc(land_pts,nz_soilc),                                                &
    ! Stable N:C ratio of biomass and humus pools.
  residual_n(land_pts,nz_soilc),                                               &
    ! Minimum-allowed (residual) inorganic N amount (kg m-2).
  resp_frac_soil(land_pts,nz_soilc),                                           &
    ! The fraction of decomposition that forms new biomass and
  f_root_pft(npft,nz_soilc),                                                   &
    ! Fraction of roots in each soil layer.
  smvc(land_pts,nz_soilc),                                                     &
    ! Volumetric soil moisture content (1).
  sm_total(land_pts,nz_soilc),                                                 &
    ! Volumetric soil moisture content, as a fraction of saturation.
  sm_unfrozen(land_pts,nz_soilc),                                              &
    ! Unfrozen volumetric soil moisture content, as a fraction of
    ! saturation.
  sm_fc(land_pts,nz_soilc),                                                    &
    ! Volumetric soil moisture content at field capacity,
    ! as a fraction of saturation.
  sm_one_bar(land_pts,nz_soilc),                                               &
    ! Volumetric soil moisture content when suction pressure is
    ! -100kPa (-1 bar), as a fraction of saturation.
  sm_wilt(land_pts,nz_soilc),                                                  &
    ! Volumetric soil moisture content at wilting point,
    ! as a fraction of saturation.
  soilT_degC(land_pts,nz_soilc),                                               &
    ! Soil temperature in ECOSSE layers (degC).
  water_flux_down(land_pts,0:nz_soilc),                                        &
    ! Downward water flux at bottom of each layer (kg m-2 s-1).
  water_flux_lateral(land_pts,nz_soilc),                                       &
    ! Lateral water flux from each soil layer (kg m-2 s-1).
  veg_cover(land_pts),                                                         &
    ! Indicator of vegetation coverage (0 to 1).
  plant_input_c_dpm(land_pts,nz_soilc),                                        &
    ! Carbon added to DPM pool by plant inputs (kg m-2).
  plant_input_c_rpm(land_pts,nz_soilc),                                        &
    ! Carbon added to RPM pool by plant inputs (kg m-2).
  plant_input_n_dpm(land_pts,nz_soilc),                                        &
    ! Nitrogen added to DPM pool by plant inputs (kg m-2).
  plant_input_n_rpm(land_pts,nz_soilc)
    ! Nitrogen added to RPM pool by plant inputs (kg m-2).

LOGICAL, INTENT(OUT) ::                                                        &
  l_add_plant_inputs,                                                          &
    ! Flag indicating if plant inputs to be added this timestep.
  l_extract_n
    ! Flag indicating if plant uptake of N is removed from soil this
    ! timestep.

!-----------------------------------------------------------------------------
! These arguments replace USE statements
!-----------------------------------------------------------------------------
! p_s_params_alloc
INTEGER, INTENT(IN) :: nsoilt
INTEGER, INTENT(IN) :: dim_cslayer
! p_s_params
REAL(KIND=real_jlslsm), INTENT(IN) :: bexp_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: clay_soilt(land_pts,nsoilt,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN) :: sathh_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smvcst_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smvcwt_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: soil_ph_soilt(land_pts,nsoilt,dim_cslayer)
! soil_ecosse_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN) :: qbase_l_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthf_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthu_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: tsoil_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: wflux_driver(land_pts,nsoilt,sm_levels)
!trif_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN) :: lit_c_orig_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN) :: lit_n_orig_pft(land_pts,npft)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
LOGICAL, PARAMETER ::                                                          &
  l_midpoint_true = .TRUE.,                                                    &
    ! Flag indicating that variables apply at mid-point of each layer,
    ! used with interpolation.
  l_midpoint_false = .FALSE.,                                                  &
    ! Flag used with water flux variables to indicate that the flux does
    ! not apply at the midpoint of each layer but rather at the layer
    ! bottom.
  l_interp_true = .TRUE.
    ! Switch controlling mapping of vertical water flux (w_flux) between
    ! soil moisture and ECOSSE layers.
    ! .TRUE. means interpolate.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ECOSSE_PREPARE'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, iz, j, n
    ! Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  convert_360_input,                                                           &
    ! Conversion from 360 days to litterfall input timestep.
  depth
    ! Depth to middle of a layer (m).

LOGICAL ::                                                                     &
  l_interp
    ! Switch controlling mapping between soil moisture and ECOSSE layers.

REAL(KIND=real_jlslsm) ::                                                      &
  dz_sc(0:nz_soilc),                                                           &
    ! Thicknesses of soil layers (m).
  dz_sm(0:sm_levels),                                                          &
    ! Thicknesses of soil moisture layers (m).
  sm_fc_sm_levs(land_pts,sm_levels),                                           &
    ! Soil moisture at one bar pressure, as fraction of saturation.
  sm_one_bar_sm_levs(land_pts,sm_levels),                                      &
    ! Soil moisture at one bar pressure, as fraction of saturation.
  sm_total_sm_levs(land_pts,sm_levels),                                        &
    ! Soil moisture, as fraction of saturation.
  sm_wilt_sm_levs(land_pts,sm_levels),                                         &
    ! Wilting point, as fraction of saturation.
  smvc_field_capac(land_pts,sm_levels),                                        &
    ! Volumetric soil moisture content at field capacity.
  smvc_one_bar(land_pts,sm_levels),                                            &
    ! Volumetric soil moisture content at one bar pressure.
  vmc_sm_levs(land_pts,sm_levels),                                             &
    ! Volumetric soil moisture content.
  f_root_pft_tmp(sm_levels)
    ! Temporary array for fraction of roots in each soil layer.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Add layer zero with thickness=0.
!-----------------------------------------------------------------------------
dz_sc(0)  = 0.0
dz_sc(1:) = dz_soilc(:)

dz_sm(0)  = 0.0
dz_sm(1:) = dzsoil(:)

!-----------------------------------------------------------------------------
! Set flags indicating how plant inputs and uptakes are to be handled.
! Some of this is currently overkill but provides flexibility that can be
! used with future developments.
!-----------------------------------------------------------------------------
l_add_plant_inputs = .FALSE.
l_extract_n        = .FALSE.
IF ( triffid_call == 0) THEN
  ! TRIFFID was called on this timestep.
  ! Inputs are calculated and added on veg model timesteps.
  l_add_plant_inputs = .TRUE.
  ! Get timestep length for plant inputs and uptake.
  dt_input = FLOAT( triffid_period * isec_per_day )
  ! Get conversion from 360 days to the veg model timestep.
  convert_360_input = FLOAT( triffid_period ) / 360.0
  ! Uptake is removed on the N model timestep.
  ! At present (with n_model_triffid) this is done on TRIFFID timestep.
  l_extract_n = .TRUE.
END IF
! For clarity, reset flag if N not modelled.
IF ( .NOT. l_soil_N ) l_extract_n = .FALSE.

!-----------------------------------------------------------------------------
! Calculate the root fraction in each ECOSSE layer.
! This is not needed every timestep, but easier to calculate anyway.
! When/if the crop model is added, this will vary with location too.
!
! Note that f_root_pft is normalised (so the sum is 1) meaning that it is the
! fraction of all roots in the ECOSSE soil column that is in each layer. For
! an ECOSSE column that is smaller than the soil moisture column, some roots
! might extend below the soil moisture column but these are ignored by the
! normalisation. This is not currently an issue, just beware in future!
!-----------------------------------------------------------------------------
DO n = 1,npft
  CALL root_frac( n, nz_soilc, dz_soilc(:), rootd_ft(n), f_root_pft_tmp )
  f_root_pft(n,:) = f_root_pft_tmp(:)
END DO

!-----------------------------------------------------------------------------
! Calculate plant inputs to each layer.
!-----------------------------------------------------------------------------
IF ( l_add_plant_inputs ) THEN
  CALL distribute_plant_inputs( land_pts, vs_pts,                              &
                                convert_360_input, dt_input,                   &
                                vs_index, frac_surft_start, f_root_pft,        &
                                plant_input_c_dpm, plant_input_c_rpm,          &
                                plant_input_n_dpm, plant_input_n_rpm,          &
                            ! These arguments replace USE statements
                                !trif_vars_mod
                                lit_c_orig_pft, lit_n_orig_pft,                &
                                !TYPES
                                soilecosse )
END IF

!-----------------------------------------------------------------------------
! Calculate volumetric soil moisture contents at given hydraulic heads.
!-----------------------------------------------------------------------------
! Field capacity.
CALL vmc_from_head( land_pts, vs_pts, sm_levels, psi_field_capac,              &
                    vs_index, bexp_soilt(:,s,:), sathh_soilt(:,s,:),           &
                    smvcst_soilt(:,s,:),                                       &
                    smvc_field_capac )
! One bar.
CALL vmc_from_head( land_pts, vs_pts, sm_levels, psi_one_bar,                  &
                    vs_index, bexp_soilt(:,s,:), sathh_soilt(:,s,:),           &
                    smvcst_soilt(:,s,:),                                       &
                    smvc_one_bar )

!-----------------------------------------------------------------------------
! Calculate variables on soil moisture layers.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,sm_levels
    ! Total soil wetness.
    sm_total_sm_levs(i,iz)    = sthu_driver(i,s,iz) + sthf_driver(i,s,iz)
    ! Volumetric moisture content.
    vmc_sm_levs(i,iz) = sm_total_sm_levs(i,iz) *  smvcst_soilt(i,s,iz)
    ! Soil wetness at defined moisture levels.
    sm_wilt_sm_levs(i,iz)     = smvcwt_soilt(i,s,iz) / smvcst_soilt(i,s,iz)
    sm_one_bar_sm_levs (i,iz) = smvc_one_bar(i,iz)   / smvcst_soilt(i,s,iz)
    sm_fc_sm_levs(i,iz)       = smvc_field_capac(i,iz) /                       &
                                smvcst_soilt(i,s,iz)
  END DO  !  iz
END DO  !  points

!-----------------------------------------------------------------------------
! Put soil moisture and temperature variables onto ECOSSE layers.
!-----------------------------------------------------------------------------
! Decide whether to interpolate variables or use an average.
IF ( nz_soilc == 1 ) THEN
  ! Use depth-weighted average.
  l_interp = .FALSE.
ELSE
  ! Interpolate.
  l_interp = .TRUE.
END IF

! All the variables in this block are valid at the mid-layer depth.
smvc(:,:)        = transfer_layers( land_pts, vs_pts, sm_levels, nz_soilc,     &
                                    l_midpoint_true, l_interp, vs_index,       &
                                    dz_sm(1:),dz_sc(1:),                       &
                                    vmc_sm_levs )
! Impose a minimum water content, to avoid dividing by zero.
smvc(:,:)        = MAX( smvc(:,:), 0.001 )

sm_total(:,:)    = transfer_layers( land_pts, vs_pts, sm_levels, nz_soilc,     &
                                    l_midpoint_true, l_interp, vs_index,       &
                                    dz_sm(1:),dz_sc(1:),                       &
                                    sm_total_sm_levs )
sm_unfrozen(:,:) = transfer_layers( land_pts, vs_pts, sm_levels, nz_soilc,     &
                                    l_midpoint_true, l_interp, vs_index,       &
                                    dz_sm(1:),dz_sc(1:),                       &
                                    sthu_driver(:,s,:) )
sm_wilt(:,:)     = transfer_layers( land_pts, vs_pts, sm_levels, nz_soilc,     &
                                    l_midpoint_true, l_interp, vs_index,       &
                                    dz_sm(1:),dz_sc(1:),                       &
                                    sm_wilt_sm_levs )
sm_fc(:,:)       = transfer_layers( land_pts, vs_pts, sm_levels, nz_soilc,     &
                                    l_midpoint_true, l_interp, vs_index,       &
                                    dz_sm(1:) , dz_sc(1:),                     &
                                    sm_fc_sm_levs )
sm_one_bar(:,:)  = transfer_layers( land_pts,vs_pts, sm_levels, nz_soilc,      &
                                    l_midpoint_true, l_interp, vs_index,       &
                                    dz_sm(1:), dz_sc(1:),                      &
                                    sm_one_bar_sm_levs )
soilT_degC(:,:)  = transfer_layers( land_pts, vs_pts, sm_levels, nz_soilc,     &
                                    l_midpoint_true, l_interp, vs_index,       &
                                    dz_sm(1:),dz_sc(1:),                       &
                                    tsoil_driver(:,s,:) ) - tm
water_flux_lateral(:,:) =  transfer_layers( land_pts, vs_pts, sm_levels,       &
                                            nz_soilc, l_midpoint_true,         &
                                            l_interp, vs_index,                &
                                            dz_sm(1:), dz_sc(1:),              &
                                            qbase_l_driver(:,s,:) )

!-----------------------------------------------------------------------------
! Vertical fluxes at the bottom of soil layers (l_midpoint_false).
! We include layer #0 (top boundary).
!-----------------------------------------------------------------------------
water_flux_down(:,:) = transfer_layers( land_pts, vs_pts, sm_levels+1,         &
                                        nz_soilc+1, l_midpoint_false,          &
                                        l_interp_true, vs_index,               &
                                        dz_sm(0:), dz_sc(0:),                  &
                                        wflux_driver(:,s,0:sm_levels) )

!-----------------------------------------------------------------------------
! Set indicator of vegetation cover.
!-----------------------------------------------------------------------------
veg_cover(:) = 0.0
DO j = 1,vs_pts
  i = vs_index(j)
  veg_cover(i) = veg_cover(i) + SUM( frac_surft_start(i,1:npft) )
END DO

!----------------------------------------------------------------------------
! Calculate the fractions of decomposition going to biomass and humus.
!----------------------------------------------------------------------------
CALL calc_decomp_frac( land_pts,vs_pts,vs_index, clay_soilt(:,s,:),            &
                       resp_frac_soil )

IF ( l_soil_N ) THEN
  !-------------------------------------------------------------------------
  ! Calculate the stable N:C ratio of biomass and humus.
  !-------------------------------------------------------------------------
  CALL stable_n_c( land_pts, vs_pts,vs_index, soil_pH_soilt(:,s,:),            &
                   biohum_nc )

  !-------------------------------------------------------------------------
  ! Calculate the minimum-allowed amount of inorganic N.
  !-------------------------------------------------------------------------
  CALL get_residual_n( land_pts, vs_pts, vs_index, residual_n )

  !---------------------------------------------------------------------------
  ! Calculate the number of layers in which nitrification and
  ! denitrification are allowed. The top layer is always considered so as to
  ! allow runs with a single layer. We compare depth to middle of layer with
  ! the threshold (depth_nitrif).
  !---------------------------------------------------------------------------
  nlayer_nitrif = 1
  depth = 0.5 * dz_soilc(1)
  DO iz = 2,nz_soilc
    depth = depth + 0.5 * ( dz_soilc(iz-1) + dz_soilc(iz) )
    IF ( depth <= depth_nitrif ) nlayer_nitrif = iz
  END DO
  ! If a single-layer model is used only allow some of this to be involved in
  ! nitrification and denitrification (which tend to occur mainly near the
  ! surface). However if the single layer has a thickness (the
  ! "representative" or notional depth in this case) that is less than
  ! the depth for nitrification we revert to using all the nitrogen (though
  ! the use of a single, shallow soil layer is an unlikely configuration).
  IF ( nz_soilc == 1 .AND. dz_soilc(1) >= depth_nitrif ) THEN
    ! If we scale by the total soil depth we are ignoring the
    ! possibility that the inorganic nitrogen is relatively abundant near
    ! the surface (where deposition occurs). For now we use an arbitrary
    ! fraction.
    active_frac(1) = 0.25
  ELSE
    ! Show that all layers are liable to (de)nitrification - subject to also
    ! being in one of the nlayer_nitrif layers.
    active_frac(:) = 1.0
  END IF

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ecosse_prepare

!#############################################################################
!#############################################################################

SUBROUTINE distribute_plant_inputs( land_pts, vs_pts,                          &
             convert_360_input, dt_input,                                      &
             vs_index, frac_surft_start, f_root_pft,                           &
             plant_input_c_dpm, plant_input_c_rpm,                             &
             plant_input_n_dpm, plant_input_n_rpm,                             &
         ! These arguments replace USE statements
             !trif_vars_mod
             lit_c_orig_pft, lit_n_orig_pft,                                   &
             !TYPES
             soilecosse )

! Description:
!   Paritions plant organic matter inputs between soil layers.

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc=>dim_cslayer

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  l_soil_N

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  npft, ntype

USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type

USE trif, ONLY:                                                                &
  ! imported arrays
  dpm_rpm_ratio

IMPLICIT NONE

!Type Arguments
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  convert_360_input,                                                           &
    ! Conversion from flux over 360 days to per litterfall input timestep.
  dt_input
    ! Timestep length for litter inputs and N uptake (s).

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN)  ::                                                        &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_surft_start(land_pts,ntype),                                            &
    ! Fractional coverage of each land type at start of timestep.
  f_root_pft(npft,nz_soilc)
    ! Fraction of roots in each soil layer.

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  plant_input_c_dpm(land_pts,nz_soilc),                                        &
    ! Carbon added to DPM pool by plant inputs (kg m-2).
  plant_input_c_rpm(land_pts,nz_soilc),                                        &
    ! Carbon added to RPM pool by plant inputs (kg m-2).
  plant_input_n_dpm(land_pts,nz_soilc),                                        &
    ! Nitrogen added to DPM pool by plant inputs (kg m-2).
  plant_input_n_rpm(land_pts,nz_soilc)
    ! Nitrogen added to RPM pool by plant inputs (kg m-2).
!-----------------------------------------------------------------------------
! These arguments replace USE statements
!-----------------------------------------------------------------------------
!trif_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN) ::lit_c_orig_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN) ::lit_n_orig_pft(land_pts,npft)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'DISTRIBUTE_PLANT_INPUTS'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j, n   ! loop counters/indices

REAL(KIND=real_jlslsm) ::                                                      &
  dpm_frac(npft),                                                              &
    ! The fraction of litterfall that is decomposable plant material (DPM).
  input_frac(npft,nz_soilc)
    ! The proportion of plant inputs going into each soil layer.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate the vertical distribution of inputs.
!-----------------------------------------------------------------------------
CALL profile_plant_inputs( f_root_pft, input_frac )

!-----------------------------------------------------------------------------
! Calculate fraction of litter that is DPM, from DPM:RPM ratio.
!-----------------------------------------------------------------------------
dpm_frac(:) = dpm_rpm_ratio(:) / ( 1.0 + dpm_rpm_ratio(:) )

!-----------------------------------------------------------------------------
! Calculate the input of C to each soil layer.
!-----------------------------------------------------------------------------
plant_input_c_dpm(:,:) = 0.0
plant_input_c_rpm(:,:) = 0.0
DO n = 1,npft
  DO iz = 1,nz_soilc
    DO j = 1,vs_pts
      i = vs_index(j)
      plant_input_c_dpm(i,iz) = plant_input_c_dpm(i,iz) +                      &
                                lit_c_orig_pft(i,n) * dpm_frac(n) *            &
                                input_frac(n,iz) * frac_surft_start(i,n) *     &
                                convert_360_input
      plant_input_c_rpm(i,iz) = plant_input_c_rpm(i,iz) +                      &
                                lit_c_orig_pft(i,n) * (1.0 - dpm_frac(n)) *    &
                                input_frac(n,iz) * frac_surft_start(i,n) *     &
                                convert_360_input
    END DO
  END DO  !  iz
END DO  !  n (PFT)

IF ( l_soil_N ) THEN
  !---------------------------------------------------------------------------
  ! Calculate the input of N to each soil layer.
  !---------------------------------------------------------------------------
  plant_input_n_dpm(:,:) = 0.0
  plant_input_n_rpm(:,:) = 0.0
  DO n = 1,npft
    DO iz = 1,nz_soilc
      DO j = 1,vs_pts
        i = vs_index(j)
        plant_input_n_dpm(i,iz) = plant_input_n_dpm(i,iz) +                    &
                                  lit_n_orig_pft(i,n) * dpm_frac(n) *          &
                                  input_frac(n,iz) * frac_surft_start(i,n)     &
                                  * convert_360_input
        plant_input_n_rpm(i,iz) = plant_input_n_rpm(i,iz) +                    &
                                  lit_n_orig_pft(i,n) * (1.0 - dpm_frac(n))    &
                                  * input_frac(n,iz) *                         &
                                  frac_surft_start(i,n) * convert_360_input
      END DO
    END DO  !  iz
  END DO  !  n (PFT)
END IF

!-----------------------------------------------------------------------------
! Calculate plant input diagnostics, summing over layers and converting to
! rates.
!-----------------------------------------------------------------------------
soilecosse%plant_input_c_gb(:) =                                               &
        SUM( plant_input_c_dpm + plant_input_c_rpm, 2 ) / dt_input

IF ( l_soil_N ) THEN
  soilecosse%plant_input_n_gb(:) =                                             &
        SUM( plant_input_n_dpm + plant_input_n_rpm, 2 ) / dt_input
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE distribute_plant_inputs

!#############################################################################
!#############################################################################

SUBROUTINE profile_plant_inputs( f_root_pft, input_frac )

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalars
  tau_lit

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalar parameters
  plant_input_roots,                                                           &
  ! imported scalars
  plant_input_profile,                                                         &
  ! imported arrays
  dz_soilc

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  npft

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  pi_sfc_depth, pi_sfc_frac

USE veg_param, ONLY:                                                           &
  ! imported scalars
  litc_norm

IMPLICIT NONE

! Description:
!   Calculates the vertical distribution of plant organic matter inputs to
!   the soil for each PFT.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) :: f_root_pft(npft,nz_soilc)
    ! Fraction of roots in each soil layer.

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: input_frac(npft,nz_soilc)
    ! The proportion of plant inputs going into each soil layer.

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'PROFILE_PLANT_INPUTS'

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: iz, p    ! Indices.
REAL(KIND=real_jlslsm) :: dzSum       ! Accumulated depth (m).
REAL(KIND=real_jlslsm) :: piRemains   ! Remaining fraction of inputs.

REAL(KIND=real_jlslsm) ::                                                      &
  zmid(nz_soilc)
    ! Depth (from surface) to mid-point of each ECOSSE soil layer (m).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise outputs.
!-----------------------------------------------------------------------------
input_frac(:,:) = 0.0

IF ( nz_soilc == 1 ) THEN
  !---------------------------------------------------------------------------
  ! Deal with the trivial case of a single soil layer separately.
  ! All inputs are added to this layer.
  !---------------------------------------------------------------------------
  input_frac(:,:) = 1.0

ELSE

  !---------------------------------------------------------------------------
  ! Calculate depths to middle of ECOSSE layers.
  !---------------------------------------------------------------------------
  zmid(1) = 0.5 * dz_soilc(1)
  DO iz = 2,nz_soilc
    zmid(iz) = zmid(iz-1) + 0.5 * ( dz_soilc(iz-1) + dz_soilc(iz) )
  END DO

  !---------------------------------------------------------------------------
  ! Calculate distribution of inputs.
  !---------------------------------------------------------------------------
  IF ( plant_input_profile == plant_input_roots ) THEN

    !-------------------------------------------------------------------------
    ! Fraction pi_sfc_frac of inputs is added to a surface layer of depth
    ! pi_sfc_depth (or the topmost layer, whichever is larger). The remaining
    ! inputs are distributed between all layers (including the top) according
    ! to roots.
    !-------------------------------------------------------------------------
    DO p = 1,npft

      ! Calculate the inputs to the surface layer.
      dzSum = 0.0
      DO iz = 1,nz_soilc
        IF ( iz == 1 .OR. zmid(iz) <= pi_sfc_depth ) THEN
          input_frac(p,iz) = pi_sfc_frac * dz_soilc(iz)
          dzSum            = dzSum + dz_soilc(iz)
        ELSE
          EXIT  !  leave loop over layers
        END IF
      END DO
      !  Normalise by total depth within surface layer.
      input_frac(p,:) = input_frac(p,:) / dzSum

      !-----------------------------------------------------------------------
      ! Partition remaining litter between all layers (including surface)
      ! according to root distribution.
      !-----------------------------------------------------------------------
      piRemains = 1.0 - SUM( input_frac(p,:) )
      DO iz = 1,nz_soilc
        input_frac(p,iz) = input_frac(p,iz) + piRemains * f_root_pft(p,iz)
      END DO

    END DO  !  PFT

  ELSE

    !-------------------------------------------------------------------------
    ! plant_input_profile = plant_input_exp
    ! Calculate dz * exp(-tau*z) for mid-point depth in each layer and
    ! include the normalisation factor.
    !-------------------------------------------------------------------------
    DO iz = 1,nz_soilc
      input_frac(:,iz) = dz_soilc(iz) * EXP( -tau_lit * zmid(iz) )             &
                         / litc_norm
    END DO

  END IF   !  plant_input_profile

END IF  !  number of layers

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE profile_plant_inputs

!#############################################################################
!#############################################################################

SUBROUTINE calc_decomp_frac( land_pts, vs_pts, vs_index, clay,                 &
                             resp_frac_soil )

! Description:
!   Calculates the fraction of decomposition that forms new biomass and humus,
!   i.e. the fraction that remains in the soil.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! Number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) :: clay(land_pts,nz_soilc)
    ! Fraction of soil that is clay.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
    resp_frac_soil(land_pts,nz_soilc)
    ! The fraction of decomposition that forms new biomass and humus, i.e.
    ! the fraction that remains in the soil.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_DECOMP_FRAC'

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j    ! Loop counters/indices

REAL(KIND=real_jlslsm) ::                                                      &
  ratio_co2_bh
    ! Ratio of C in CO2 produced during decomposition to C going to
    ! biomass+humus.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialisation.
resp_frac_soil(:,:) = 0.0

DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nz_soilc

    ! Note that this is essentially a recoding of the resp_frac equation
    ! from VEG2. For now this is left separate but in time should likely be
    ! combined.
    ratio_co2_bh         = 1.67 * ( 1.85 + 1.60                                &
                                    * EXP( -7.86 * clay(i,iz) ) )
    resp_frac_soil(i,iz) = 1.0 / ( 1.0 + ratio_co2_bh )

  END DO  !  layers
END DO  !  points

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE calc_decomp_frac

!#############################################################################
!#############################################################################

SUBROUTINE stable_n_c( land_pts, vs_pts, vs_index, soil_pH, biohum_nc )

! Description:
!   Calculates the stable N:C ratio of biomass and humus pools as a function
!   of soil pH.
!   Reference: Smith, J., et al., 2010, Climate Research, 45: 179-192.
!              doi:10.3354/cr00899.

!-----------------------------------------------------------------------------

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  bacteria_min_frac, bacteria_max_frac,                                        &
  bacteria_min_frac_pH, bacteria_max_frac_pH,                                  &
  cn_bacteria, cn_fungi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  vs_pts
    ! Number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) :: soil_pH(land_pts,nz_soilc)
    ! pH of each layer.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: biohum_nc(land_pts,nz_soilc)
    ! Stable N:C ratio of biomass and humus.

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'STABLE_N_C'

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j  ! Loop counters/indices

REAL(KIND=real_jlslsm) ::                                                      &
  frac_bacteria,                                                               &
    ! Fraction of decomposer community that is bacteria.
    ! Fraction 1-frac_bacteria is fungi.
  max_minus_min_frac,                                                          &
    ! Max-min fraction of bacteria
  max_minus_min_pH
    ! pH difference for max-min fraction of bacteria

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate differences.
!-----------------------------------------------------------------------------
max_minus_min_frac = bacteria_max_frac - bacteria_min_frac
max_minus_min_pH   = bacteria_max_frac_pH - bacteria_min_frac_pH

!-----------------------------------------------------------------------------
! Caculate the fraction of bacteria, then the overall N:C ratio.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nz_soilc

    ! Fraction of bacteria at this soil pH.
    ! Eqn. 8 of Smith et al. (2010).
    IF ( soil_pH(i,iz) >= bacteria_max_frac_pH ) THEN
      frac_bacteria = bacteria_max_frac
    ELSE IF ( soil_pH(i,iz) > bacteria_min_frac_pH ) THEN
      frac_bacteria = bacteria_min_frac + max_minus_min_frac *                 &
                      ( soil_pH(i,iz) - bacteria_min_frac_pH ) /               &
                      max_minus_min_pH
    ELSE
      frac_bacteria = bacteria_min_frac
    END IF

    ! Calculate weighted N:C ratio.
    ! Eqn. 7 of Smith et al. (2010).
    biohum_nc(i,iz) = 1.0 /                                                    &
                      ( frac_bacteria * cn_bacteria +                          &
                      ( 1.0 - frac_bacteria) * cn_fungi )

  END DO  !  layers
END DO  !  points

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE stable_n_c

!#############################################################################
!#############################################################################

END MODULE ecosse_prepare_mod
#endif
