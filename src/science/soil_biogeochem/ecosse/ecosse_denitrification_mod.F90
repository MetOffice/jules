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

MODULE ecosse_denitrification_mod

!-----------------------------------------------------------------------------
! Description:
!   Calculate denitrification.
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
PUBLIC calc_denitrification

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'ECOSSE_DENITRIFICATION_MOD'

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE calc_denitrification( land_pts, nlayer_nitrif, vs_pts, vs_index,    &
                   active_frac, residual_n, co2_from_decomp,                   &
                   sm_unfrozen, sm_fc, sm_wilt,                                &
                   n_nit, n_denitrification, n2_denitrif, n2o_denitrif )

! Calculate denitrification.

USE ancil_info, ONLY:                                                          &
  ! imported scalars
  nz_soilc=>dim_cslayer

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  denit_bio_factor, denit_frac_n2_fc, denit_nitrate_equal

USE ecosse_utils_mod, ONLY:                                                    &
  ! imported parameters
  nt

USE jules_soil_ecosse_mod, ONLY:                                               &
  ! imported scalars
  dt_soilc,                                                                    &
  ! imported arrays
  dz_soilc

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
  residual_n(land_pts,nz_soilc),                                               &
    ! N in minimum allowed (residual) nitrate or ammonium amount (kg m-2).
  co2_from_decomp(land_pts,nz_soilc),                                          &
    ! C in CO2 produced by aerobic decomposition (kg m-2).
  sm_unfrozen(land_pts,nz_soilc),                                              &
    ! Unfrozen volumetric soil moisture content, as a fraction of
    ! saturation.
  sm_fc(land_pts,nz_soilc),                                                    &
    ! Volumetric soil moisture content at field capacity,
    ! as a fraction of saturation.
  sm_wilt(land_pts,nz_soilc)
    ! Volumetric soil moisture content at wilting point,
    ! as a fraction of saturation.

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_nit(land_pts,nz_soilc,nt)
    ! N in soil nitrate (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  n_denitrification(land_pts),                                                 &
    ! Rate of denitrification, expressed as N (kg m-2 s-1).
  n2_denitrif(land_pts),                                                       &
    ! N in N2 lost from soil by denitrification (kg m-2 s-1).
  n2o_denitrif(land_pts)
    ! N in N2O lost from soil by denitrification (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j ! work

REAL(KIND=real_jlslsm) ::                                                      &
  dlayer,                                                                      &
    ! N lost by denitrification from a layer (kg m-2).
  dlayer_dt,                                                                   &
    ! N lost by denitrification, expressed as a rate (kg m-2 s-1).
  part_h2o,                                                                    &
    ! Factor related to soil moisture.
  part_no3,                                                                    &
    ! Factor related to NO3.
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

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_DENITRIFICATION'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise fluxes.
!-----------------------------------------------------------------------------
n_denitrification(:) = 0.0
n2_denitrif(:)       = 0.0
n2o_denitrif(:)      = 0.0

! Calculate a constant.
recip_dt_soilc       = 1.0 / dt_soilc

!-----------------------------------------------------------------------------
! Get rate modifying factors for layers in which denitrification is allowed.
!-----------------------------------------------------------------------------
CALL rate_modifier_denitrification( land_pts, nlayer_nitrif, vs_pts,           &
       vs_index, dz_soilc(1:nlayer_nitrif),                                    &
       sm_unfrozen(:,1:nlayer_nitrif), sm_fc(:,1:nlayer_nitrif),               &
       sm_wilt(:,1:nlayer_nitrif), n_nit(:,1:nlayer_nitrif,1),                 &
       rate_factor(:,1:nlayer_nitrif) )

!-----------------------------------------------------------------------------
! Calculate denitrification.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nlayer_nitrif

    !-------------------------------------------------------------------------
    ! Calculate the N denitrified.
    ! Modified from Eq.7 of Bell et al. (2012),
    ! http://doi.org/10.1007/s10705-011-9479-4
    ! Note that the effect of model timestep length comes in via the modelled
    ! CO2 flux (co2_from_decomp).
    !-------------------------------------------------------------------------
    dlayer = co2_from_decomp(i,iz) * denit_bio_factor * n_nit(i,iz,1)          &
                                   * active_frac(iz) * rate_factor(i,iz)

    !-------------------------------------------------------------------------
    ! Prevent NO3 from going below the minimum allowed value.
    !-------------------------------------------------------------------------
    IF ( dlayer > n_nit(i,iz,nt) - residual_n(i,iz) ) THEN
      dlayer = n_nit(i,iz,nt) - residual_n(i,iz)
    END IF

    !-------------------------------------------------------------------------
    ! Partition denitrification losses into N2 and N2O.
    ! Eq.11-14 of Bell et al. (2012),
    ! http://doi.org/10.1007/s10705-011-9479-4
    !-------------------------------------------------------------------------
    IF ( dlayer > 0.0 ) THEN

      ! Convert mass to a rate.
      dlayer_dt = dlayer * recip_dt_soilc

      ! Effect of soil moisture.
      IF ( sm_unfrozen(i,iz) < sm_fc(i,iz) ) THEN
        part_h2o = denit_frac_n2_fc                                            &
                   * ( sm_unfrozen(i,iz)  - sm_wilt(i,iz) )                    &
                   / ( sm_fc(i,iz) - sm_wilt(i,iz) )
      ELSE
        part_h2o = denit_frac_n2_fc
      END IF

      ! Effect of nitrate amount.
      part_no3 = 1.0 - n_nit(i,iz,1)                                           &
                       / ( denit_nitrate_equal * dz_soilc(iz)                  &
                           + n_nit(i,iz,1) )

      n2_denitrif(i)  = n2_denitrif(i)                                         &
                        + part_h2o * part_no3 * dlayer_dt
      n2o_denitrif(i) = n2o_denitrif(i)                                        &
                        + (1.0 - part_h2o * part_no3 ) * dlayer_dt

      !-----------------------------------------------------------------------
      ! Update the nitrate pool and add to column flux.
      !-----------------------------------------------------------------------
      n_nit(i,iz,nt)       = n_nit(i,iz,nt) - dlayer
      n_denitrification(i) = n_denitrification(i) + dlayer_dt

    END IF  !  dlayer > 0

  END DO  !  layers
END DO  !  points

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE calc_denitrification

!#############################################################################
!#############################################################################

SUBROUTINE rate_modifier_denitrification( land_pts, nlayer, vs_pts,            &
             vs_index, dz_soilc, sm_unfrozen, sm_fc, sm_wilt, n_nit,           &
             rate_factor )

! Description:
!  Subroutine to calculate the rate modifying factors for denitrification.

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  denit50,                                                                     &
  ! imported arrays
  denit_water_coeff

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! The number of land points.
  nlayer,                                                                      &
    ! The number of layers present.
  vs_pts
    ! The number of points with veg and/or soil.

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz_soilc(nlayer),                                                            &
    ! Thicknesses of soil layers (m).
  sm_unfrozen(land_pts,nlayer),                                                &
    ! Unfrozen volumetric soil moisture content, as a fraction of
    ! saturation.
  sm_fc(land_pts,nlayer),                                                      &
    ! Volumetric soil moisture content at field capacity,
    ! as a fraction of saturation.
  sm_wilt(land_pts,nlayer),                                                    &
    ! Volumetric soil moisture content at wilting point,
    ! as a fraction of saturation.
  n_nit(land_pts,nlayer)
    !  N in soil nitrate (kg m-2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: rate_factor(land_pts,nlayer)
    ! Overall (combined) rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, iz, j  ! Indices.

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  nitrate_rate(land_pts,nlayer),                                               &
    ! Nitrate amount rate modifier.
  water_rate(land_pts,nlayer)
    ! Moisture rate modifier.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'RATE_MODIFIER_DENITRIFICATION'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Moisture modification factor.
! Rate is constant (1) above field capacity.
! Eq.9 of Bell et al. (2012), http://doi.org/10.1007/s10705-011-9479-4
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nlayer
    water_rate(i,iz) = ( MIN( sm_unfrozen(i,iz), sm_fc(i,iz) )                 &
                         - sm_wilt(i,iz) )                                     &
                       / ( sm_fc(i,iz) - sm_wilt(i,iz) )
    water_rate(i,iz) = MAX( ( water_rate(i,iz) - denit_water_coeff(1) )        &
                            / denit_water_coeff(2),                            &
                            0.0 )
    water_rate(i,iz) = water_rate(i,iz)** denit_water_coeff(3)
  END DO
END DO

!-----------------------------------------------------------------------------
! Modifier based on amount of nitrate.
! Eq.8 of Bell et al. (2012), doi: 10.1007/s10705-011-9479-4
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nlayer
    nitrate_rate(i,iz) = n_nit(i,iz)                                           &
                         / ( denit50 * dz_soilc(iz) + n_nit(i,iz) )
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate the overall rate modification factor.
!-----------------------------------------------------------------------------
DO j = 1,vs_pts
  i = vs_index(j)
  DO iz = 1,nlayer
    rate_factor(i,iz) = water_rate(i,iz) * nitrate_rate(i,iz)
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE rate_modifier_denitrification

!#############################################################################
!#############################################################################

END MODULE ecosse_denitrification_mod
#endif
