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

MODULE ecosse_nitrification_mod

!-----------------------------------------------------------------------------
! Description:
!   Calculate nitrification.
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

PRIVATE
PUBLIC calc_nitrification

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'ECOSSE_NITRIFICATION_MOD'

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE calc_nitrification( land_pts, nlayer_nitrif, vs_pts,                &
         vs_index, active_frac, residual_N,                                    &
         sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                              &
         soil_pH, soilT_degC, n_amm, n_nit,                                    &
         n_nitrification, no_soil, n2o_full_nitrif, n2o_partial_nitrif )

!----------------------------------------------------------------------------
! Reference:
!  Bell et al. (2012), "Simulation of soil nitrogen, nitrous oxide emissions
!  and mitigation scenarios at 3 European cropland sites using the ECOSSE
!  model", Nutr Cycl Agroecosyst, 92: 161-181,
!  http://doi.org/10.1007/s10705-011-9479-4.
!----------------------------------------------------------------------------

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc=>dim_cslayer

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported parameters
  nt

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  dt_soilc,                                                                    &
  ! imported arrays
  dz_soilc

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  nitrif_frac_n2o_fc, nitrif_frac_gas, nitrif_frac_no, nitrif_rate,            &
  nitrif_max_factor

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  nlayer_nitrif,                                                               &
    ! The number of layers in which nitrification is allowed.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  active_frac(nz_soilc),                                                       &
    ! The fraction of the inorganic nitrogen in a layer that can be involved
    ! in nitrification and denitrification (subject to the layer also being
    ! included in the nlayer_nitrif layers).
  residual_N(land_pts,nz_soilc),                                               &
   ! N in minimum allowed (residual) nitrate or ammonium amount (kg m-2).
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
  soil_ph(land_pts,nz_soilc),                                                  &
     ! soil pH
  soilT_degC(land_pts,nz_soilc)
    ! Soil temperature (degC).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_amm(land_pts,nz_soilc,nt),                                                 &
    ! N in soil ammonium (kg m-2).
  n_nit(land_pts,nz_soilc,nt)
    !  N in soil nitrate (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  n_nitrification(land_pts),                                                   &
    ! Rate of nitrification, expressed as N (kg m-2 s-1).
  no_soil(land_pts),                                                           &
    ! N in NO flux from soil to atmosphere (kg m-2 s-1).
  n2o_full_nitrif(land_pts),                                                   &
    ! N in N2O lost by (full) nitrification (kg m-2 s-1).
  n2o_partial_nitrif(land_pts)
    ! N in N2O lost by partial nitrification (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j ! work

REAL(KIND=real_jlslsm) ::                                                      &
  anit,                                                                        &
    ! Ammonium N nitrified (kg m-2).
  frac_lost_n2o_partial_nitrif,                                                &
    ! Proportion of nitrified N that is lost as N2O through partial
    ! nitrification.
  nitrif_rate_dt,                                                              &
    ! Rate constant for nitrification (per timestep).
  n_n2o,                                                                       &
    ! N in N2O lost by (full) nitrification in a layer (kg m-2).
  n_no,                                                                        &
    ! N in NO lost by nitrification in a layer (kg m-2).
  n_n2o_partial,                                                               &
    ! N in N2O lost by partial nitrification in a layer (kg m-2).
  no_frac,                                                                     &
    ! The fraction of nitrified N that becomes NO.
  n2o_frac,                                                                    &
    ! The fraction of nitrified N that becomes N2O.
  recip_dt_soilc
    ! The reciprocal of the timestep length (s-1).

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: rate_factor(land_pts,nz_soilc)
    ! Overall (combined) rate modification factor.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_NITRIFICATION'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise fluxes.
!-----------------------------------------------------------------------------
n_nitrification(:)    = 0.0
n2o_full_nitrif(:)    = 0.0
n2o_partial_nitrif(:) = 0.0
no_soil(:)            = 0.0

!-----------------------------------------------------------------------------
! Convert rate constant from s-1 to per timestep, and calculate other
! constants.
!-----------------------------------------------------------------------------
nitrif_rate_dt = nitrif_rate * dt_soilc
no_frac        = nitrif_frac_gas * nitrif_frac_no
n2o_frac       = nitrif_frac_gas * ( 1.0 - nitrif_frac_no )
recip_dt_soilc = 1.0 / dt_soilc

!-----------------------------------------------------------------------------
! Get rate modifying factors for layers in which nitrification is allowed.
!-----------------------------------------------------------------------------
CALL rate_modifier_nitrification( land_pts, nlayer_nitrif, vs_pts, vs_index,   &
           sm_unfrozen(:,1:nlayer_nitrif),                                     &
           sm_fc(:,1:nlayer_nitrif),                                           &
           sm_one_bar(:,1:nlayer_nitrif),                                      &
           sm_wilt(:,1:nlayer_nitrif),                                         &
           soil_pH(:,1:nlayer_nitrif), soilT_degC(:,1:nlayer_nitrif),          &
           rate_factor(:,1:nlayer_nitrif) )

!-----------------------------------------------------------------------------
! Calculate nitrification.
! Modified from Eq.1 of Bell et al. (2012).
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nlayer_nitrif

    anit = nitrif_rate_dt * rate_factor(i,iz) * n_amm(i,iz,1)                  &
           * active_frac(iz)                                                   &
           ! Below is a saturation function to represent the restriction of
           ! the activity of nitrifiers at high concentrations of ammonium.
           * ( 1.0 - n_amm(i,iz,1)                                             &
                     / ( nitrif_max_factor * dz_soilc(iz) + n_amm(i,iz,1) ) )

    !-------------------------------------------------------------------------
    ! Prevent NH4 from going below the minimum allowed value.
    !-------------------------------------------------------------------------
    IF ( anit > n_amm(i,iz,nt) - residual_N(i,iz) ) THEN
      anit = n_amm(i,iz,nt) - residual_N(i,iz)
    END IF

    !-------------------------------------------------------------------------
    ! Update the ammonium pool.
    !-------------------------------------------------------------------------
    n_amm(i,iz,nt) = n_amm(i,iz,nt) - anit

    !-------------------------------------------------------------------------
    ! Calculate the gaseous losses associated with nitrification.
    ! Eq.6 of Bell et al. (2012).
    !-------------------------------------------------------------------------
    frac_lost_n2o_partial_nitrif = nitrif_frac_n2o_fc                          &
                                   * sm_unfrozen(i,iz) / sm_fc(i,iz)

    n_n2o_partial = frac_lost_n2o_partial_nitrif * anit
    n_no          = no_frac  * anit
    n_n2o         = n2o_frac * anit

    !-------------------------------------------------------------------------
    ! Ensure that mass is conserved. Although in most cases the gas loss
    ! will be a small fraction of nitrification, it is possible that the
    ! initial calculation exceeds nitrification because of a large
    ! contribution from partial nitrification (if the soil moisture is much
    ! above field capacity, which is unlikely, and/or nitrif_frac_n2o_fc is
    ! large). (Note that during initialisation we have checked that
    ! nitrif_frac_gas <= 1). In that case we reduce partial nitrification to
    ! conserve mass. This is an extreme case (all nitrified N is lost as
    ! gas)!
    !-------------------------------------------------------------------------
    IF ( n_n2o_partial + n_no + n_n2o > anit ) THEN
      n_n2o_partial = anit - n_no - n_n2o
    END IF

    !-------------------------------------------------------------------------
    ! Add remaining nitrified N to the nitrate pool.
    !-------------------------------------------------------------------------
    n_nit(i,iz,nt) = n_nit(i,iz,nt) + anit - n_n2o_partial - n_no - n_n2o

    !-------------------------------------------------------------------------
    ! Add fluxes to column totals, converting to rates.
    !-------------------------------------------------------------------------
    n_nitrification(i)    = n_nitrification(i) + anit * recip_dt_soilc
    n2o_partial_nitrif(i) = n2o_partial_nitrif(i)                              &
                            + n_n2o_partial * recip_dt_soilc
    n2o_full_nitrif(i)    = n2o_full_nitrif(i) + n_n2o * recip_dt_soilc
    no_soil(i)            = no_soil(i)         + n_no * recip_dt_soilc

  END DO  ! layers
END DO  ! points

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE calc_nitrification

!#############################################################################
!#############################################################################

SUBROUTINE rate_modifier_nitrification( land_pts, nlayer, vs_pts,              &
       vs_index,  sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                     &
       soil_pH, soilT_degC,rate_factor )

USE conversions_mod, ONLY:                                                     &
  ! imported scalar parameters
  pi

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported parameters
  temp_mod_q10, temp_mod_smith, water_mod_clark, water_mod_smith,              &
  ! imported scalars
  temp_modifier, water_modifier

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  nitrif_wrate_min

USE ecosse_rate_modifier_mod, ONLY:                                            &
  ! imported procedures
  rate_modifier_q10_temp, rate_modifier_clark_water,                           &
  rate_modifier_smith_temp, rate_modifier_smith_water

! Description:
!  Subroutine to calculate the rate modifying factors for nitrification.

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  nlayer,                                                                      &
    ! The number of layers input.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: vs_index(land_pts)
    ! Indices of veg/soil points.

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
  soilT_degC(land_pts,nlayer)
    ! Soil temperature (degC).

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: rate_factor(land_pts,nlayer)
    ! Overall (combined) rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i      ! work
INTEGER :: iz     ! loop counter
INTEGER :: j      ! loop counter

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
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

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'RATE_MODIFIER_NITRIFICATION'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Moisture modification factors.
!-----------------------------------------------------------------------------
SELECT CASE ( water_modifier )

CASE ( water_mod_smith )
  ! Pass zero to the argument rate_min_wet to set the rate modifier to zero
  ! when soil is water saturated.
  CALL rate_modifier_smith_water( land_pts, nlayer, vs_pts,                    &
           nitrif_wrate_min, 0.0, vs_index,                                    &
           sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                            &
           water_rate )

CASE ( water_mod_clark )
  ! Note that this uses the same parameter as is used for mineralisation.
  CALL rate_modifier_clark_water( land_pts, nlayer, vs_pts, vs_index,          &
                              sm_unfrozen, sm_wilt,                            &
                              water_rate )
END SELECT

!-----------------------------------------------------------------------------
! Temperature modification factor.
!-----------------------------------------------------------------------------
SELECT CASE ( temp_modifier )

CASE ( temp_mod_smith )
  ! Note that this uses the same coefficients as are used for
  ! mineralisation.
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
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nlayer

    ! Eq.5 of Bell et al. (2012).
    ph_rate(i,iz) = 0.56 + ( ATAN( pi * 0.45 * ( soil_pH(i,iz) - 5.0 ) ) )     &
                           / pi
    ph_rate(i,iz) = MAX( ph_rate(i,iz), 0.0 )
    ph_rate(i,iz) = MIN( ph_rate(i,iz), 1.5 )

  END DO  !  layers
END DO  !  points

!-----------------------------------------------------------------------------
! Calculate the overall rate modification factor.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nlayer
    rate_factor(i,iz) = water_rate(i,iz) * temp_rate(i,iz) * ph_rate(i,iz)
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE rate_modifier_nitrification

!#############################################################################
!#############################################################################

END MODULE ecosse_nitrification_mod
#endif
