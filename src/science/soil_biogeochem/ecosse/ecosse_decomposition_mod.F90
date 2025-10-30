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

MODULE ecosse_decomposition_mod

!-----------------------------------------------------------------------------
! Description:
!   Calculate aerobic decomposition of soil organic matter, and associated
!   mineralisation and immobilisation of N.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC calc_soil_decomposition

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'ECOSSE_DECOMPOSITION_MOD'

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE calc_soil_decomposition( land_pts, vs_pts, vs_index,                &
          biohum_nc, residual_n, resp_frac_soil,                               &
          sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                             &
          soil_pH, soilT_degC, veg_cover,                                      &
          c_bio, c_hum, c_dpm, c_rpm,                                          &
          n_bio, n_hum, n_dpm, n_rpm, n_nit, n_amm, co2_from_decomp,           &
      ! These arguments replace USE statements
          ! trif_vars_mod
          minl_n_gb, minl_n_pot_gb, immob_n_gb,                                &
          immob_n_pot_gb, resp_s_diag_gb, resp_s_pot_diag_gb, fn )

! Description:
!   Calculate soil decomposition (aka respiration).
!   Note that several variables declared and/or used here have counterparts
!   in the 4-pool soil C code with the same name but often different units.


USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc=>dim_cslayer, dim_cs1, dim_cslayer

USE ecosse_param_mod, ONLY:                                                    &
  ! imported arrays
  decomp_rate

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported parameters
  nt

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  dt_soilc, l_decomp_slow, l_soil_N

USE veg_param, ONLY:                                                           &
  ! imported parameters
  secs_per_360days

USE ereport_mod, ONLY:                                                         &
  ! imported procedures
   ereport

USE string_utils_mod, ONLY:                                                    &
  ! imported procedures
  to_string

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  biohum_nc(land_pts,nz_soilc),                                                &
    ! Stable N:C ratio of the BIO and HUM pools (kgN/kgC).
  residual_n(land_pts,nz_soilc),                                               &
    ! N in the minimum allowed (residual) nitrate or ammonium amount (kg m-2).
  resp_frac_soil(land_pts,nz_soilc),                                           &
    ! The fraction of decomposition that forms new biomass and humus, i.e.
    ! the fraction that remains in the soil.
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
  soil_pH(land_pts,nz_soilc),                                                  &
    ! Soil pH.
  soilT_degC(land_pts,nz_soilc),                                               &
    ! Soil temperature (degC).
  veg_cover(land_pts)
    ! Indicator of vegetation coverage (0 to 1).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  c_bio(land_pts,nz_soilc,nt),                                                 &
    ! C in soil biomass (kg m-2).
  c_hum(land_pts,nz_soilc,nt),                                                 &
    ! C in soil humus (kg m-2).
  c_dpm(land_pts,nz_soilc,nt),                                                 &
    ! C in decomposable plant material (kg m-2).
  c_rpm(land_pts,nz_soilc,nt),                                                 &
    ! C in resistant plant material (kg m-2).
  n_bio(land_pts,nz_soilc,nt),                                                 &
    ! N in soil biomass (kg m-2).
  n_hum(land_pts,nz_soilc,nt),                                                 &
    ! N in soil humus (kg m-2).
  n_dpm(land_pts,nz_soilc,nt),                                                 &
    ! N in decomposable plant material (kg m-2).
  n_rpm(land_pts,nz_soilc,nt),                                                 &
    ! N in resistant plant material (kg m-2).
  n_nit(land_pts,nz_soilc,nt),                                                 &
    ! N in soil nitrate (kg m-2).
  n_amm(land_pts,nz_soilc,nt)
    ! N in soil ammonium (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  co2_from_decomp(land_pts,nz_soilc)
    ! CO2 emitted from soil, expressed as carbon (kg m-2).

!-----------------------------------------------------------------------------
! These arguments replace USE statements
!-----------------------------------------------------------------------------
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
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  fn(land_pts,dim_cslayer)

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  decomp_bio_frac = 0.46,                                                      &
    ! The fraction of the decomposing C that is retained in the soil
    ! (aka resp_frac) that becomes new biomass.
  decomp_hum_frac = 1.0 - decomp_bio_frac
    ! The fraction of the decomposing C that is retained in the soil
    ! (aka resp_frac) that becomes new humus.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  error_status,                                                                &
    ! Error status.
  i, iz, l, p
    ! Loop counters and indices.

REAL(KIND=real_jlslsm) ::                                                      &
  bio_rate_dt,                                                                 &
    ! Rate constant for decomposition of biomass (per timestep).
  hum_rate_dt,                                                                 &
    ! Rate constant for decomposition of humus (per timestep).
  dpm_rate_dt,                                                                 &
    ! Rate constant for decomposition of decomposable plant matter
    ! (per timestep).
  rpm_rate_dt,                                                                 &
    ! Rate constant for decomposition of resistant plant matter
    ! (per timestep).
  dt_to_360d
    ! Multiplier to convert from per ECOSSE timestep to per 360 days.

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  cn(land_pts,nz_soilc,dim_cs1),                                               &
    ! C:N of soil pools.
  immob_n(land_pts,nz_soilc,dim_cs1+1),                                        &
    ! Immobilised N (kg m-2).
    ! Element dim_cs1+1 holds the total over all pools.
  immob_n_pot(land_pts,nz_soilc,dim_cs1+1),                                    &
    ! Immobilised N assuming no N limit on decomposition (kg m-2).
    ! Element dim_cs1+1 holds the total over all pools.
  minl_n(land_pts,nz_soilc,dim_cs1+1),                                         &
    ! Mineralised N (kg m-2).
    ! Element dim_cs1+1 holds the total over all pools.
  minl_n_pot(land_pts,nz_soilc,dim_cs1+1),                                     &
    ! Mineralised N assuming no N limit on decomposition (kg m-2).
    ! Element dim_cs1+1 holds the total over all pools.
  n_amm_avail(land_pts,nz_soilc),                                              &
    ! Amount of ammonium-N available for immobilisation (kg m-2).
  n_nit_avail(land_pts,nz_soilc),                                              &
    ! Amount of nitrate-N available for immobilisation (kg m-2).
  net_immobilised_n(land_pts,nz_soilc),                                        &
    ! Net immobilisation of N in a layer (kg m-2).
  rate_factor(land_pts,nz_soilc),                                              &
    ! Overall (combined) rate modification factor.
  resp_frac_soil_local(land_pts,nz_soilc),                                     &
    ! The fraction of decomposition that forms new biomass and humus, i.e.
    ! the fraction that remains in the soil. This will be reduced if
    ! l_decomp_slow=F and there is insufficient N to support decomposition
    ! at the potential rate.
  resp_s(land_pts,nz_soilc,dim_cs1+1),                                         &
    ! Soil respiration (kg m-2).
    ! Element dim_cs1+1 holds the total over all pools.
  resp_s_pot(land_pts,nz_soilc,dim_cs1+1),                                     &
    ! Soil respiration assuming no N limit on decomposition (kg m-2).
    ! Element dim_cs1+1 holds the total over all pools.
  total_n_avail(land_pts,nz_soilc)
    ! Total N available for immobilisation (kg m-2).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_SOIL_DECOMPOSITION'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
! Initialise fluxes.
co2_from_decomp(:,:) = 0.0
immob_n(:,:,:)       = 0.0
immob_n_pot(:,:,:)   = 0.0
minl_n(:,:,:)        = 0.0
minl_n_pot(:,:,:)    = 0.0
resp_s(:,:,:)        = 0.0
resp_s_pot(:,:,:)    = 0.0
! Initialise resp_frac (unchanged if l_decomp_slow=T).
resp_frac_soil_local(:,:) = resp_frac_soil(:,:)
! Initialise nitrogen limitation factor to indicate no limitation.
fn(:,:)         = 1.0
IF ( l_soil_N ) THEN
  ! Calculate N available for immobilisation.
  DO i = 1,vs_pts
    l = vs_index(i)
    DO iz = 1,nz_soilc
      n_amm_avail(l,iz)   = MAX( n_amm(l,iz,1) - residual_n(l,iz), 0.0 )
      n_nit_avail(l,iz)   = MAX( n_nit(l,iz,1) - residual_n(l,iz), 0.0 )
      total_n_avail(l,iz) = n_amm_avail(l,iz) + n_nit_avail(l,iz)
    END DO
  END DO
END IF

!----------------------------------------------------------------------------
! Convert rate constants from per second to per timestep.
!----------------------------------------------------------------------------
dpm_rate_dt = decomp_rate(1) * dt_soilc
rpm_rate_dt = decomp_rate(2) * dt_soilc
bio_rate_dt = decomp_rate(3) * dt_soilc
hum_rate_dt = decomp_rate(4) * dt_soilc

!-----------------------------------------------------------------------------
! Calculate the rate modifying factor for each layer.
!-----------------------------------------------------------------------------
CALL rate_modifier_decomposition( land_pts, nz_soilc, vs_pts, vs_index,        &
          sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                             &
          soil_pH, soilT_degC, veg_cover,                                      &
          rate_factor )

!-----------------------------------------------------------------------------
! The following code is intentionally similar to that used with the 4-pool C
! model, meaning in future they might be combined.
!-----------------------------------------------------------------------------
DO i = 1,vs_pts
  l = vs_index(i)

  DO iz = 1,nz_soilc

    ! Calculate respiration assuming sufficient N.
    resp_s_pot(l,iz,1) = dpm_rate_dt * rate_factor(l,iz) * c_dpm(l,iz,1)
    resp_s_pot(l,iz,2) = rpm_rate_dt * rate_factor(l,iz) * c_rpm(l,iz,1)
    resp_s_pot(l,iz,3) = bio_rate_dt * rate_factor(l,iz) * c_bio(l,iz,1)
    resp_s_pot(l,iz,4) = hum_rate_dt * rate_factor(l,iz) * c_hum(l,iz,1)
    resp_s_pot(l,iz,5) = SUM( resp_s_pot(l,iz,1:4) )

    IF ( l_soil_n ) THEN
      ! Calculate soil C:N.
      cn(l,iz,1) = c_dpm(l,iz,1) / n_dpm(l,iz,1)
      cn(l,iz,2) = c_rpm(l,iz,1) / n_rpm(l,iz,1)
      cn(l,iz,3) = 1.0 / biohum_nc(l,iz)
      cn(l,iz,4) = 1.0 / biohum_nc(l,iz)

      ! Calculate mineralisation and immobilisation for each pool.
      DO p = 1,4
        minl_n_pot(l,iz,p)  = resp_s_pot(l,iz,p) / cn(l,iz,p)
        immob_n_pot(l,iz,p) = decomp_bio_frac * resp_frac_soil_local(l,iz)     &
                              * resp_s_pot(l,iz,p) / cn(l,iz,3)                &
                              + decomp_hum_frac                                &
                                * resp_frac_soil_local(l,iz)                   &
                                * resp_s_pot(l,iz,p) / cn(l,iz,4)
      END DO

      ! Get totals over all pools.
      minl_n_pot(l,iz,5)  = SUM( minl_n_pot(l,iz,1:4) )
      immob_n_pot(l,iz,5) = SUM( immob_n_pot(l,iz,1:4) )

      !-----------------------------------------------------------------------
      ! If soil N demand is greater than the available N, alter decomposition.
      !-----------------------------------------------------------------------
      IF ( immob_n_pot(l,iz,5) - minl_n_pot(l,iz,5)                            &
           > total_n_avail(l,iz) ) THEN

        IF ( l_decomp_slow ) THEN
          fn(l,iz) = ( minl_n_pot(l,iz,3) + minl_n_pot(l,iz,4)                 &
                      - immob_n_pot(l,iz,3) - immob_n_pot(l,iz,4)              &
                      + total_n_avail(l,iz) )                                  &
                     /                                                         &
                     ( immob_n_pot(l,iz,1) + immob_n_pot(l,iz,2)               &
                       - minl_n_pot(l,iz,1) - minl_n_pot(l,iz,2) )

          ! Check for impossible values.
          ! This will likely only be used during code development.
          IF ( fn(l,iz) < -1.0e-3 .OR. fn(l,iz) > 1.001 ) THEN
            error_status = 101  !  a fatal error
            CALL ereport( RoutineName, error_status,                           &
                         'fn<0 or >1 at point ' // to_string(l) //             &
                         'fn='// to_string(fn(l,iz)) )
          END IF

          ! Avoid rounding issues by limiting the range.
          fn(l,iz) = MIN( MAX(fn(l,iz), 0.0), 1.0)

          ! Calculate actual fluxes given the available N.
          ! Decomposition of the DPM and RPM pools is slowed.
          resp_s(l,iz,1:2)  = fn(l,iz) * resp_s_pot(l,iz,1:2)
          minl_n(l,iz,1:2)  = fn(l,iz) * minl_n_pot(l,iz,1:2)
          immob_n(l,iz,1:2) = fn(l,iz) * immob_n_pot(l,iz,1:2)
          ! Conversions of BIO and HUM pools are at the potential rates.
          resp_s(l,iz,3:4)  = resp_s_pot(l,iz,3:4)
          minl_n(l,iz,3:4)  = minl_n_pot(l,iz,3:4)
          immob_n(l,iz,3:4) = immob_n_pot(l,iz,3:4)

        ELSE

          ! l_decomp_slow = .FALSE.
          fn(l,iz) = ( minl_n_pot(l,iz,5) + total_n_avail(l,iz) )              &
                     / immob_n_pot(l,iz,5)

          ! Check for impossible values.
          ! This will likely only be used during code development.
          IF ( fn(l,iz) < -1.0e-3 .OR. fn(l,iz) > 1.001 ) THEN
            error_status = 101  !  a fatal error
            CALL ereport( RoutineName, error_status,                           &
                         'fn<0 or >1 at point ' // to_string(l) //             &
                         'fn='// to_string(fn(l,iz)) )
          END IF

          ! Avoid rounding issues by limiting the range.
          fn(l,iz) = MIN( MAX(fn(l,iz), 0.0), 1.0)

          ! Calculate actual fluxes given the available N.
          ! Decomposition (respiration) and mineralisation continue at the
          ! potential rate.
          ! Immobilisation is reduced (more C is released to the atmosphere).
          DO p = 1,4
            resp_s(l,iz,p)  = resp_s_pot(l,iz,p)
            minl_n(l,iz,p)  = minl_n_pot(l,iz,p)
            immob_n(l,iz,p) = fn(l,iz) * immob_n_pot(l,iz,p)
          END DO

          ! Reduce resp_frac.
          resp_frac_soil_local(l,iz) = resp_frac_soil(l,iz) * fn(l,iz)

        END IF  !  l_decomp_slow

      ELSE

        ! There is sufficient N available to allow decomposition at the
        ! potential rate.
        resp_s(l,iz,1:4)  = resp_s_pot(l,iz,1:4)
        minl_n(l,iz,1:4)  = minl_n_pot(l,iz,1:4)
        immob_n(l,iz,1:4) = immob_n_pot(l,iz,1:4)

      END IF  !  lack of N restricts decomposition

      ! Calculate totals over all pools.
      minl_n(l,iz,5)  = SUM( minl_n(l,iz,1:4) )
      immob_n(l,iz,5) = SUM( immob_n(l,iz,1:4) )
      resp_s(l,iz,5)  = SUM( resp_s(l,iz,1:4) )

      net_immobilised_n(l,iz) = immob_n(l,iz,5) - minl_n(l,iz,5)

    ELSE

      ! .NOT. l_soil_n
      ! Copy N-umlimited respiration into actual respiration.
      resp_s(l,iz,:) = resp_s_pot(l,iz,:)

    END IF  !  l_soil_n

    !-------------------------------------------------------------------------
    ! Calculate the CO2 released due to biological activity.
    !-------------------------------------------------------------------------
    co2_from_decomp(l,iz) = ( 1.0 - resp_frac_soil_local(l,iz) )               &
                            * resp_s(l,iz,5)

    !-------------------------------------------------------------------------
    ! Update C pool sizes (i.e. after decomposition).
    !-------------------------------------------------------------------------
    c_dpm(l,iz,nt) = c_dpm(l,iz,nt) - resp_s(l,iz,1)
    c_rpm(l,iz,nt) = c_rpm(l,iz,nt) - resp_s(l,iz,2)
    c_bio(l,iz,nt) = c_bio(l,iz,nt) - resp_s(l,iz,3)                           &
                     + decomp_bio_frac * resp_frac_soil_local(l,iz)            &
                       * resp_s(l,iz,5)
    c_hum(l,iz,nt) = c_hum(l,iz,nt) - resp_s(l,iz,4)                           &
                     + decomp_hum_frac * resp_frac_soil_local(l,iz)            &
                       * resp_s(l,iz,5)

    IF ( l_soil_N ) THEN

      !-----------------------------------------------------------------------
      ! Update N pool sizes.
      ! We know that sufficient N was available for immobilisation.
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      ! Update ammonimum and nitrate stores.
      !-----------------------------------------------------------------------
      IF ( net_immobilised_n(l,iz) > n_amm_avail(l,iz) ) THEN
        !---------------------------------------------------------------------
        ! The ammount of N immobilised exceeds the N available as ammonium.
        ! Deplete ammonimum to the minimum allowed and take the remaining
        ! mineralised N from the nitrate pool.
        !---------------------------------------------------------------------
        n_nit(l,iz,nt) = n_nit(l,iz,nt) + n_amm_avail(l,iz)                    &
                         - net_immobilised_n(l,iz)
        n_amm(l,iz,nt) = residual_n(l,iz)
      ELSE
        !---------------------------------------------------------------------
        ! The amount of N immobilised can all be taken from the ammonium
        ! pool. If immobilisation is less than zero (mineralisation) this is
        ! augmenting the ammonium pool (ammonification).
        !---------------------------------------------------------------------
        n_amm(l,iz,nt) = n_amm(l,iz,nt) - net_immobilised_n(l,iz)
      END IF

      !-----------------------------------------------------------------------
      ! Update organic N pool sizes.
      !-----------------------------------------------------------------------
      n_dpm(l,iz,nt) = n_dpm(l,iz,nt) - minl_n(l,iz,1)
      n_rpm(l,iz,nt) = n_rpm(l,iz,nt) - minl_n(l,iz,2)
      n_bio(l,iz,nt) = n_bio(l,iz,nt) - minl_n(l,iz,3)                         &
                       + decomp_bio_frac * immob_n(l,iz,5)

      n_hum(l,iz,nt) = n_hum(l,iz,nt) - minl_n(l,iz,4)                         &
                       + decomp_hum_frac * immob_n(l,iz,5)

    END IF  !  l_soil_N

  END DO  !  layers

END DO  !  points

!-----------------------------------------------------------------------------
! Save diagnostic values.
! These are converted into the units used by TRIFFID and 4-pool C model.
! As the conversion is not needed by ECOSSE, an alternative would be to do
! this in the output code.
!-----------------------------------------------------------------------------
! Get conversion from ECOSSE timestep to 360 days.
dt_to_360d                = secs_per_360days / dt_soilc
minl_n_pot_gb(:,:,:)      = minl_n_pot(:,:,:)    * dt_to_360d
minl_n_gb(:,:,:)          = minl_n(:,:,:)        * dt_to_360d
immob_n_pot_gb(:,:,:)     = immob_n_pot(:,:,:)   * dt_to_360d
immob_n_gb(:,:,:)         = immob_n(:,:,:)       * dt_to_360d
resp_s_pot_diag_gb(:,:,:) = resp_s_pot(:,:,:)    * dt_to_360d
resp_s_diag_gb(:,:,:)     = resp_s(:,:,:)        * dt_to_360d

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE calc_soil_decomposition

!#############################################################################
!#############################################################################

SUBROUTINE rate_modifier_decomposition( land_pts, nlayer, vs_pts, vs_index,    &
             sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                          &
             soil_pH, soilT_degC, veg_cover,                                   &
             rate_factor )

! Description:
!  Subroutine to calculate the rate modifying factors for decomposition.

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  decomp_wrate_min_smith

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported parameters
  temp_mod_q10, temp_mod_smith, water_mod_clark, water_mod_smith,              &
  ! imported scalars
  temp_modifier, water_modifier

USE ecosse_rate_modifier_mod, ONLY:                                            &
  ! imported procedures
  rate_modifier_q10_temp, rate_modifier_clark_water,                           &
  rate_modifier_smith_ph,   rate_modifier_smith_temp,                          &
  rate_modifier_smith_water

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  nlayer,                                                                      &
    ! The number of layers input.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
  ! Indices of veg/soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sm_unfrozen(land_pts,nlayer),                                                &
    ! Unfrozen volumetric soil moisture content, as a fraction of saturation.
  sm_fc(land_pts,nlayer),                                                      &
    ! Volumetric soil moisture content at field capacity,
    ! as a fraction of saturation.
  sm_one_bar(land_pts,nlayer),                                                 &
    ! Volumetric soil moisture content when suction pressure is
    ! -100kPa (-1 bar), as a fraction of saturation.
  sm_wilt(land_pts,nlayer),                                                    &
    ! Volumetric soil moisture content at wilting point,
    ! as a fraction of saturation.
  soil_ph(land_pts,nlayer),                                                    &
     ! soil pH
  soilT_degC(land_pts,nlayer),                                                 &
    ! Soil temperature (degC).
  veg_cover(land_pts)
    ! Indicator of vegetation coverage (0 to 1).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: rate_factor(land_pts,nlayer)
    ! Overall (combined) rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, il, j    ! Indices.

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  veg_rate(land_pts),                                                          &
    ! Rate factor accounting for vegetation cover.
  ph_rate(land_pts,nlayer),                                                    &
    ! pH rate modifier.
  temp_rate(land_pts,nlayer),                                                  &
    ! Temperature rate modifying factor.
  water_rate(land_pts,nlayer)
    ! Moisture rate modifier.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'RATE_MODIFIER_DECOMPOSITION'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Crop (vegetation) modification factor.
! Decomposition is slower with veg cover. This is a linear decline from 1 at
! veg_cover=0 to 0.6 at veg_cover=1.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  veg_rate(i) = 1.0 - 0.4 * veg_cover(i)
END DO

!-----------------------------------------------------------------------------
! Moisture modification factors.
!-----------------------------------------------------------------------------
SELECT CASE ( water_modifier )

CASE ( water_mod_smith )
  ! Pass the same minimum rate twice - for use with both dry and wet soil.
  CALL rate_modifier_smith_water( land_pts, nlayer, vs_pts,                    &
                                  decomp_wrate_min_smith,                      &
                                  decomp_wrate_min_smith, vs_index,            &
                                  sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,     &
                                  water_rate )

CASE ( water_mod_clark )
  CALL rate_modifier_clark_water( land_pts, nlayer, vs_pts, vs_index,          &
                                  sm_unfrozen, sm_wilt,                        &
                                  water_rate )
END SELECT

!-----------------------------------------------------------------------------
! Temperature modification factor
!-----------------------------------------------------------------------------
SELECT CASE ( temp_modifier )

CASE ( temp_mod_smith )
  CALL rate_modifier_smith_temp( land_pts, nlayer, vs_pts,                     &
                                 vs_index, soilT_degC,                         &
                                 temp_rate )
CASE ( temp_mod_q10 )
  CALL rate_modifier_q10_temp( land_pts, nlayer, vs_pts,                       &
                                 vs_index, soilT_degC,                         &
                                 temp_rate )
END SELECT

!-----------------------------------------------------------------------------
! pH modification factor.
!-----------------------------------------------------------------------------
CALL rate_modifier_smith_ph( land_pts, nlayer, vs_pts,                         &
                             vs_index, soil_ph,                                &
                             ph_rate )

!-----------------------------------------------------------------------------
! Calculate the overall rate modification factor.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO il = 1,nlayer
    rate_factor(i,il) = veg_rate(i) * water_rate(i,il) * temp_rate(i,il)       &
                        * ph_rate(i,il)
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE rate_modifier_decomposition

!#############################################################################
!#############################################################################

END MODULE ecosse_decomposition_mod
#endif
