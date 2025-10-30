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

MODULE ecosse_rate_modifier_mod

!-----------------------------------------------------------------------------
! Description:
!   Rate modifiers for the ECOSSE model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------
! References:
! Clark, D.B., et al., 2011, Geoscientific Model Development, 4: 701-722,
!    doi:10.5194/gmd-4-701-2011.
! Smith, J., et al., 2010, Climate Research, 45: 179-192.
!             doi:10.3354/cr00899.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  public scope by default
PUBLIC rate_modifier_q10_temp, rate_modifier_clark_water,                      &
       rate_modifier_smith_ph,   rate_modifier_smith_temp,                     &
       rate_modifier_smith_water

CONTAINS

!#############################################################################
!#############################################################################

SUBROUTINE rate_modifier_smith_ph( land_pts, nlayer, vs_pts,                   &
                                   vs_index, soil_ph,                          &

! Description:
!   Subroutine to calculate the soil pH rate modifying factor using the
!   4-pool soil C formulation: Eqn.3 of Smith et al. (2010).
                                ph_rate )

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  decomp_ph_rate_min, decomp_ph_max, decomp_ph_min

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
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  soil_ph(land_pts,nlayer)
    ! Soil pH.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  ph_rate(land_pts,nlayer)
    ! Rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, il, j    ! Indices.
REAL(KIND=real_jlslsm) :: one_minus_min  ! One minus a minimum rate.

!-----------------------------------------------------------------------------
!end of header

one_minus_min = 1.0 - decomp_ph_rate_min
DO j = 1,vs_pts
  i = vs_index(j)
  DO il = 1,nlayer

    IF ( soil_ph(i,il) <= decomp_ph_min ) THEN
      ph_rate(i,il) = decomp_ph_rate_min
    ELSE IF ( soil_ph(i,il) > decomp_ph_min .AND.                              &
              soil_ph(i,il) < decomp_ph_max ) THEN
      ph_rate(i,il) = decomp_ph_rate_min + one_minus_min                       &
                      * ( soil_ph(i,il) - decomp_ph_min )                      &
                      / ( decomp_ph_max - decomp_ph_min )
    ELSE
      ph_rate(i,il) = 1.0
    END IF

  END DO  !  layers
END DO  !  points

END SUBROUTINE rate_modifier_smith_ph

!#############################################################################
!#############################################################################
SUBROUTINE rate_modifier_smith_temp( land_pts, nlayer, vs_pts,                 &
                                     vs_index, soilT_degC,                     &
                                     temp_rate )

! Description:
!   Subroutine to calculate the temperature rate modifying factor:
!   Eqn.4 of Smith et al. (2010).

USE ecosse_param_mod, ONLY:                                                    &
  ! imported arrays
  decomp_temp_coeff_smith

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
INTEGER, INTENT(IN) ::                                                         &
  vs_index(land_pts)
    ! Indices of veg/soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  soilT_degC(land_pts,nlayer)
    ! Soil temperature (degC).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  temp_rate(land_pts,nlayer)
    ! Rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, il, j ! Indices.

!-----------------------------------------------------------------------------
!end of header

DO j = 1,vs_pts
  i = vs_index(j)
  DO il = 1,nlayer

    IF ( soilT_degC(i,il) < -10.0 ) THEN
      temp_rate(i,il) = 0.0
    ELSE
      temp_rate(i,il) = decomp_temp_coeff_smith(1) /                           &
                        ( 1.0 + EXP( decomp_temp_coeff_smith(2) /              &
                          ( soilT_degC(i,il) + decomp_temp_coeff_smith(3) )    &
                        ) )
    END IF

  END DO  !  layers
END DO  !  points

END SUBROUTINE rate_modifier_smith_temp

!#############################################################################
!#############################################################################

SUBROUTINE rate_modifier_smith_water( land_pts, nlayer, vs_pts,                &
             rate_min_dry, rate_min_wet, vs_index,                             &
             sm_unfrozen, sm_fc, sm_one_bar, sm_wilt,                          &
             water_rate )

! Description:
!   Subroutine to calculate the water rate modifying factor:
!   Eqn.1 of Smith et al. (2010).
!   By providing two minima this can also be used for nitrification.

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

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rate_min_dry,                                                                &
    ! Value of water_rate modifier when soil moisture less than wilting point.
  rate_min_wet
    ! Value of water_rate modifier when soil moisture above field capacity.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
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
  sm_wilt(land_pts,nlayer)
    ! Volumetric soil moisture content at wilting point,
    ! as a fraction of saturation.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: water_rate(land_pts,nlayer)
    ! Rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, il, j    ! Indices.
REAL(KIND=real_jlslsm) :: one_minus_min_dry, one_minus_min_wet
                                              ! One minus various rates.

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Calculate constants.
one_minus_min_dry = 1.0 - rate_min_dry
one_minus_min_wet = 1.0 - rate_min_wet

DO j = 1,vs_pts
  i = vs_index(j)
  DO il = 1,nlayer

    IF ( sm_unfrozen(i,il) > sm_fc(i,il) ) THEN
      ! Wetter than field capacity: linear decrease from 1 to a minimum at
      ! saturation.
      water_rate(i,il) = 1.0 - one_minus_min_wet                               &
                         * ( sm_unfrozen(i,il) - sm_fc(i,il) )                 &
                         / ( 1.0 - sm_fc(i,il) )
    ELSE IF ( sm_unfrozen(i,il) >= sm_one_bar(i,il) ) THEN
      ! Field capacity to 1 bar.
      water_rate(i,il) = 1.0
    ELSE IF ( sm_unfrozen(i,il) > sm_wilt(i,il) ) THEN
      ! 1 bar to wilting point: linear decrease from 1 to minimum at
      ! wilting point.
      water_rate(i,il) = rate_min_dry + one_minus_min_dry                      &
                         * ( sm_unfrozen(i,il) - sm_wilt(i,il) )               &
                         / ( sm_one_bar(i,il) - sm_wilt(i,il) )
    ELSE
      ! Below wilting point.
      water_rate(i,il) =  rate_min_dry
    END IF

  END DO  !  layers
END DO  !  points

END SUBROUTINE rate_modifier_smith_water

!#############################################################################
!#############################################################################

SUBROUTINE rate_modifier_q10_temp( land_pts, nlayer, vs_pts, vs_index,         &
                                     soilT_degC, temp_rate )

! Description:
!   Subroutine to calculate the temperature rate modifying factor using the
!   JULES formulation: Eqn. 65 of Clark et al. (2011).

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalars
  q10_soil

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
    ! Indices of veg/soil points.
REAL(KIND=real_jlslsm), INTENT(IN) :: soilT_degC(land_pts,nlayer)
    ! Soil temperature (degC).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: temp_rate(land_pts,nlayer)
    ! Rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, il, j    ! Indices.

!-----------------------------------------------------------------------------
!end of header

DO j = 1,vs_pts
  i = vs_index(j)
  DO il = 1,nlayer
    temp_rate(i,il) = q10_soil ** ( 0.1 * ( soilT_degC(i,il) - 9.25 ) )
  END DO
END DO

END SUBROUTINE rate_modifier_q10_temp

!#############################################################################
!#############################################################################

SUBROUTINE rate_modifier_clark_water( land_pts,nlayer,vs_pts,vs_index,         &
                                sm_unfrozen, sm_wilt,                          &
                                water_rate )

! Description:
!   Subroutine to calculate the water rate modifying factor using the
!   formulation used in JULES: Eqn. 67 of Clark et al. (2011). Note however
!   that only the unfrozen water is considered here.

USE ecosse_param_mod, ONLY:                                                    &
  ! imported scalars
  decomp_wrate_min_clark

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
  sm_wilt(land_pts,nlayer)
    ! Volumetric soil moisture content at wilting point,
    ! as a fraction of saturation.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: water_rate(land_pts,nlayer)
    ! Rate modification factor.

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER  ::                                          &
  min_factor = 1.7
    ! Factor to scale wilting point to get resp_min at 25 deg C and optimum
    ! soil moisture (/s).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER :: i, il, j    ! Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  one_minus_min,                                                               &
    ! One minus the minimum rate.
  sm_opt,                                                                      &
    ! Fractional soil moisture at which respiration is maximum.
  sm_resp_min
    ! Fractional soil moisture at or below which which respiration is
    ! minimum.

!-----------------------------------------------------------------------------
!end of header

! Calculate a constant.
one_minus_min = 1.0 - decomp_wrate_min_clark

DO j = 1,vs_pts
  i = vs_index(j)
  DO il = 1,nlayer

    ! Calculate fractional soil moisture at which respiration is maximum.
    sm_opt = 0.5 * ( 1.0 + sm_wilt(i,il) )

    ! Calculate the soil wetness for minimum respiration.
    sm_resp_min = sm_wilt(i,il) * min_factor

    ! For large values of the wilting point the response function is
    ! discontinuous at the optimal wetness (because sm_resp_min > sm_opt).
    ! For min_factor=1.7 the threshold wilting point is 0.4167 (as a fraction
    ! of saturation).

    IF ( sm_unfrozen(i,il) > sm_resp_min .AND.                                 &
         sm_unfrozen(i,il) < sm_opt ) THEN
      ! Minimum to optimal wetness: linear increase from minimum to optimum
      ! rate.
      water_rate(i,il) = decomp_wrate_min_clark +  one_minus_min               &
                         * ( (sm_unfrozen(i,il) - sm_resp_min )                &
                         / (sm_opt - sm_resp_min ) )
    ELSE IF ( sm_unfrozen(i,il) > sm_opt ) THEN
      ! Wetter than optimal point: linear decrease to minimum rate.
      water_rate(i,il) = 1.0 - one_minus_min * ( sm_unfrozen(i,il)             &
                                                 - sm_opt )
    ELSE
      ! At or below dry point: minimum rate.
      ! This is also used in cases with sm_resp_min > sm_opt when soil is
      ! drier than the optimal point.
      water_rate(i,il) = decomp_wrate_min_clark
    END IF

  END DO  !  layers
END DO  !  points

END SUBROUTINE rate_modifier_clark_water

!#############################################################################
!#############################################################################
END MODULE ecosse_rate_modifier_mod
#endif
