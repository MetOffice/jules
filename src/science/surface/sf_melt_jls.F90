! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sf_melt_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_MELT_MOD'

CONTAINS
!-----------------------------------------------------------------------
! Purpose : Calculates surface melting (snow and sea-ice) and increments
!           surface fluxes to satisfy energy balance.
!           Sub-surface snowmelt is calculated and snowdepth incremented
!           by melt and sublimation in P251.
!-----------------------------------------------------------------------
SUBROUTINE sf_melt (                                                           &
 points,pts_index                                                              &
,surft_index,surft_pts,fld_sea                                                 &
,alpha1,ashtf_prime,dtrdz_1                                                    &
,fracs,resft,rhokh_1,tile_frac,timestep,r_gamma                                &
,ei_surft,fqw_1,ftl_1,fqw_surft,ftl_surft                                      &
,tstar_surft,snow_surft,snowdepth                                              &
,melt_surft,snowinc_surft                                                      &
 )

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE planet_constants_mod, ONLY: cp

USE jules_snow_mod, ONLY: maskd, rho_snow_const,                               &
                          frac_snow_subl_melt, l_snowdep_surf

USE jules_science_fixes_mod, ONLY: l_fix_snow_frac, i_fix_neg_snow,            &
                          ip_fix_neg_snow_none, ip_fix_neg_snow_none_corr,     &
                          ip_fix_neg_snow_v1,                                  &
                          ip_fix_neg_snow_v2, ip_fix_neg_snow_v3

USE water_constants_mod, ONLY:                                                 &
 lc, lf, rho_water, tm

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER ::                                                                     &
 points                                                                        &
                      ! IN Total number of points.
,pts_index(points)                                                             &
                      ! IN Index of points.
,surft_index(points)                                                           &
!                           ! IN Index of tile points.
,surft_pts             ! IN Number of tile points.

REAL(KIND=real_jlslsm) ::                                                      &
fld_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
!                           ! IN Fraction of land or sea.
,alpha1(points)                                                                &
!                           ! IN Gradients of saturated specific
!                           !    humidity with respect to temp.
!                           !    between the bottom model layer
!                           !    and surface.
,ashtf_prime(points)                                                           &
!                           ! IN Adjusted SEB coefficient
,dtrdz_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
!                           ! IN -g.dt/dp for surface layer
,fracs(points)                                                                 &
                            ! IN Fraction subject to sublimation or deposition
,resft(points)                                                                 &
                            ! IN Resistance factor.
,rhokh_1(points)                                                               &
!                           ! IN Surface exchange coefficient.
,tile_frac(points)                                                             &
!                           ! IN Tile fractions.
,timestep                                                                      &
                      ! IN Timestep (sec).
,r_gamma              ! IN implicit weight in level 1

REAL(KIND=real_jlslsm) ::                                                      &
 ei_surft(points)                                                              &
!                           ! INOUT Sublimation for tile (kg/m2/s)
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
!                           ! INOUT GBM surface moisture flux (kg/m2/s).
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
!                           ! INOUT GBM surface sens. heat flux (W/m2).
,fqw_surft(points)                                                             &
!                           ! INOUT FQW for tile.
,ftl_surft(points)                                                             &
!                           ! INOUT FTL for tile.
,tstar_surft(points)                                                           &
!                           ! INOUT Tile surface temperatures (K).
,snow_surft(points)                                                            &
!                           ! INOUT Lying snow on tile (kg/m2).
,snowdepth(points)
!                           ! INOUT Depth of snow on tile (m).

REAL(KIND=real_jlslsm) ::                                                      &
 melt_surft(points)                                                            &
!                           ! OUT Surface snowmelt on tiles (kg/m2/s).
,snowinc_surft(points)
!                           ! OUT Total snow increment on tiles (kg/m2/TS)
!                           !     used to avoid rounding errors in the
!                           !     snow mass budget

REAL(KIND=real_jlslsm) ::                                                      &
 dfqw                                                                          &
                      ! Moisture flux increment.
,dftl                                                                          &
                      ! Sensible heat flux increment.
,dtstar                                                                        &
                      ! Surface temperature increment.
,lcmelt                                                                        &
                      ! Temporary in melt calculations.
,lsmelt                                                                        &
                      ! Temporary in melt calculations.
,rhokh1_prime                                                                  &
                      ! Modified forward time-weighted
!                           ! transfer coefficient.
,snow_new                                                                      &
                      ! Snow available for melting.
,snow_density                                                                  &
                      ! Density of snow on input
,ei_cnv                                                                        &
                      ! Rate of sublimation consistent with mass balance in
                      ! expressed in terms of increments to the snow and
                      ! the rate of melting.
,dfqw_cnv
                      ! The numerical increment to the sublimation to balance
                      ! the mass budget.
INTEGER ::                                                                     &
 i,j                                                                           &
                      ! Loop counter - full horizontal field.
,k                                                                             &
                      ! Loop counter - tile field
,l
                      ! Loop counter - land field.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_MELT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

melt_surft(:) = 0.0

!-----------------------------------------------------------------------
!  Melt snow on tile if TSTAR_SURFT is greater than TM.
!-----------------------------------------------------------------------

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(k,j,i,l,snow_density,snow_new,rhokh1_prime,lcmelt,lsmelt,dtstar, &
!$OMP         dftl,dfqw,ei_cnv,dfqw_cnv)
DO k = 1,surft_pts
  l = surft_index(k)
  j=(pts_index(l) - 1) / t_i_length + 1
  i = pts_index(l) - (j-1) * t_i_length
  !
  IF (snowdepth(l) > SQRT(TINY(1.0))) THEN
    snow_density = snow_surft(l) / snowdepth(l)
  ELSE
    snow_density = rho_snow_const
  END IF
  !
  ! The l_snowdep_surf if-test below is temporary to preserve
  ! bit comparison. It should be removed when this switch becomes
  ! default .TRUE.
  !
  IF (l_snowdep_surf) THEN
    snow_density = MAX(rho_snow_const,snow_density)
    snow_density = MIN(rho_water     ,snow_density)
  ELSE
    snow_density = MAX(   1.0,snow_density)
    snow_density = MIN(1000.0,snow_density)
  END IF
  !
  SELECT CASE (i_fix_neg_snow)
  CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_v1)
    snowinc_surft(l) = - MIN(snow_surft(l), ei_surft(l) * timestep )
    snow_new = MAX(0.0, snow_surft(l) + snowinc_surft(l))
  CASE (ip_fix_neg_snow_none_corr)
    snowinc_surft(l) = - ei_surft(l) * timestep
    snow_new = snow_surft(l) - ei_surft(l) * timestep
  CASE (ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
    snowinc_surft(l) = - MIN(snow_surft(l), ei_surft(l) * timestep )
    snow_new = snow_surft(l) + snowinc_surft(l)
  END SELECT
  rhokh1_prime = 1.0 / ( 1.0 / rhokh_1(l)                                      &
                     + r_gamma * dtrdz_1(i,j) )
  IF ( snow_new >  0.0 .AND. tstar_surft(l) >  tm ) THEN
    SELECT CASE (i_fix_neg_snow)
    CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_none_corr)
      lcmelt = (cp + lc * alpha1(l) * resft(l)) * rhokh1_prime                 &
               + ashtf_prime(l)
      lsmelt = lcmelt + lf * alpha1(l) * rhokh1_prime
    CASE (ip_fix_neg_snow_v1, ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
      ! This fix renders lcmelt superfluous, but to avoid more complicated
      ! logic it is clearest to retain it for now and remove it once the old
      ! functionality is no longer needed.
      lsmelt = (cp + (lf * fracs(l) + lc * resft(l)) * alpha1(l)) *            &
               rhokh1_prime + ashtf_prime(l)
      lcmelt = lsmelt
    END SELECT
    !   Note the use of lcmelt in the following IF-block. In line with the
    !   preceeding comment, this should be lsmelt, but lcmelt is used to
    !   maintain existing behaviour, aside from the loss of bit-comparison
    !   by moving to increments.
    !   Note also that the calculation of the increment to snow from
    !   sublimation must anticipate the adjustment of ei_surft. This was
    !   missed in version 1 of the fix for negative snow
    IF (frac_snow_subl_melt == 1) THEN
      IF (l_fix_snow_frac .AND.                                                &
          snow_new / snow_density  <=  SQRT(2.0*EPSILON(snow_new))/maskd) THEN
        ! Use linear expansion of exponential if non-linear term
        ! is of order EPSILON (i.e., x^2.0/2.0 ~ EPSILON)
        SELECT CASE (i_fix_neg_snow)
        CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_v1)
          snowinc_surft(l) = - MIN(snow_surft(l),                              &
            (lcmelt * (tstar_surft(l) - tm) *                                  &
            maskd * snow_new / snow_density / lf +                             &
            ei_surft(l)) * timestep)
        CASE (ip_fix_neg_snow_none_corr,                                       &
              ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
          snowinc_surft(l) = - MIN(snow_surft(l),                              &
            ( (lcmelt / lf + alpha1(l) * resft(l) * rhokh1_prime) *            &
              (tstar_surft(l) - tm) *                                          &
            maskd * snow_new / snow_density +                                  &
            ei_surft(l)) * timestep)
        END SELECT
      ELSE
        SELECT CASE (i_fix_neg_snow)
        CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_v1)
          snowinc_surft(l) = - MIN(snow_surft(l),                              &
            (lcmelt * (tstar_surft(l) - tm) *                                  &
            (1.0 - EXP(-maskd * snow_new / snow_density)) / lf +               &
            ei_surft(l)) * timestep)
        CASE (ip_fix_neg_snow_none_corr, ip_fix_neg_snow_v2,                   &
              ip_fix_neg_snow_v3)
          snowinc_surft(l) = - MIN(snow_surft(l),                              &
            ( (lcmelt / lf + alpha1(l) * resft(l) * rhokh1_prime) *            &
              (tstar_surft(l) - tm) *                                          &
            (1.0 - EXP(-maskd * snow_new / snow_density)) +                    &
            ei_surft(l)) * timestep)
        END SELECT
      END IF
    ELSE
      SELECT CASE (i_fix_neg_snow)
      CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_v1)
        snowinc_surft(l) = - MIN(snow_surft(l),                                &
          (lcmelt * (tstar_surft(l) - tm) / lf + ei_surft(l)) * timestep)
      CASE (ip_fix_neg_snow_none_corr, ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
        snowinc_surft(l) = - MIN(snow_surft(l),                                &
          ( (lcmelt / lf + alpha1(l) * resft(l) * rhokh1_prime) *              &
            (tstar_surft(l) - tm) + ei_surft(l)) * timestep)
      END SELECT
    END IF
    SELECT CASE (i_fix_neg_snow)
    CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_v1)
      melt_surft(l) = - snowinc_surft(l) / timestep - ei_surft(l)
    CASE (ip_fix_neg_snow_none_corr, ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
      melt_surft(l) = - (snowinc_surft(l) / timestep + ei_surft(l)) /          &
          (1.0 - alpha1(l) * resft(l) * rhokh1_prime * lf / lsmelt)
    END SELECT
    dtstar = - lf * melt_surft(l) / lsmelt
    SELECT CASE (i_fix_neg_snow)
    CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_none_corr,                     &
          ip_fix_neg_snow_v1)
      tstar_surft(l) = tstar_surft(l) + dtstar
    END SELECT
    dftl = cp * rhokh1_prime * dtstar
    dfqw = alpha1(l) * resft(l) * rhokh1_prime * dtstar
    SELECT CASE (i_fix_neg_snow)
    CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_none_corr,                     &
          ip_fix_neg_snow_v1)
      ftl_surft(l) = ftl_surft(l) + dftl
      fqw_surft(l) = fqw_surft(l) + dfqw
      ei_surft(l) = ei_surft(l) + dfqw
      !-----------------------------------------------------------------------
      !  Update gridbox-mean quantities
      !-----------------------------------------------------------------------
      dftl = tile_frac(l) * dftl
      dfqw = tile_frac(l) * dfqw
      ftl_1(i,j) = ftl_1(i,j) + fld_sea(i,j) * dftl
      fqw_1(i,j) = fqw_1(i,j) + fld_sea(i,j) * dfqw
    CASE (ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
      ! No action.
    END SELECT
  ELSE
    SELECT CASE (i_fix_neg_snow)
    CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_none_corr, ip_fix_neg_snow_v1)
      ! No action.
    CASE (ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
      dtstar = 0.0
      dfqw   = 0.0
      dftl   = 0.0
    END SELECT
  END IF

  SELECT CASE (i_fix_neg_snow)
  CASE (ip_fix_neg_snow_none, ip_fix_neg_snow_none_corr, ip_fix_neg_snow_v1)
    ! The following refinement was not made in the
    ! original version of the fix.
  CASE (ip_fix_neg_snow_v2, ip_fix_neg_snow_v3)
    !-----------------------------------------------------------------------
    !  At this stage we have the final increment to the snow, the rate of
    !  melting, and will have the rate of sublimation after applying dfqw
    !  to ei_surft. Algebraically, the increment to the snow will then be
    !  consistent with the rates of melting and sublimation.
    !  However, when the increments are applied in the snow code, a loss
    !  of numerical significance will compromise the long-term mass budget.
    !  To remove this, we make a further adjustment to the sublimation to
    !  make the rates of melting and sublimation consistent with the mass
    !  budget. To ensure conservation of energy, we adjust the sensible
    !  heat flux to compensate for the change in the latent heat flux
    !  implied by the adjustment to the moisture flux. These numerical
    !  adjustments are at the level of the truncation error and are not
    !  derived from the equation for the canopy temperature.
    !-----------------------------------------------------------------------
    ei_cnv = ( snow_surft(l) - (snow_surft(l) + snowinc_surft(l)) ) /          &
               timestep - melt_surft(l)
    dfqw_cnv = ei_cnv - (ei_surft(l) + dfqw)
    dftl   = dftl - (lc + lf) * dfqw_cnv
    dfqw   = dfqw + dfqw_cnv
    !-----------------------------------------------------------------------
    !  Apply the updates to the tiled fluxes
    !-----------------------------------------------------------------------
    tstar_surft(l) = tstar_surft(l) + dtstar
    ftl_surft(l) = ftl_surft(l) + dftl
    fqw_surft(l) = fqw_surft(l) + dfqw
    ei_surft(l) = ei_surft(l) + dfqw
    !-----------------------------------------------------------------------
    !  Update gridbox-mean quantities
    !-----------------------------------------------------------------------
    dftl = tile_frac(l) * dftl
    dfqw = tile_frac(l) * dfqw
    ftl_1(i,j) = ftl_1(i,j) + fld_sea(i,j) * dftl
    fqw_1(i,j) = fqw_1(i,j) + fld_sea(i,j) * dfqw
  END SELECT

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_melt
END MODULE sf_melt_mod
