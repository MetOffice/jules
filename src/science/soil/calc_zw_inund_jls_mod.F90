#if !defined(RECON)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   Subroutine CALC_ZW_INUND-------------------------------------------

!   Purpose: To calculate the mean water table depth from the soil
!            moisture deficit as described in Koster et al., 2000.,
!            using the Newton-Raphson method.

! Documentation: UNIFIED MODEL DOCUMENTATION PAPER NO 25

MODULE calc_zw_inund_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CALC_ZW_INUND_MOD'

CONTAINS

SUBROUTINE calc_zw_inund(npnts, nshyd, soil_pts, soil_index, zdepth,           &
                         bexp, sathh, smclsat, smcl, sthu, sthzw,              &
                         zw, zw_inund, wutot)

USE jules_hydrology_mod,  ONLY: zw_max

USE parkind1,             ONLY: jprb, jpim
USE yomhook,              ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts
    ! Number of soil points.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  bexp(npnts,nshyd),                                                           &
    ! Brooks & Corey exponent.
  sathh(npnts,nshyd),                                                          &
    ! Saturated soil water pressure (m).
  smcl(npnts,nshyd),                                                           &
    ! Total soil moisture contents of each layer (kg/m2).
  smclsat(npnts,nshyd),                                                        &
    ! Soil moisture contents of each layer at saturation (kg/m2).
  sthu(npnts,nshyd),                                                           &
    ! Unfrozen soil moisture content of each layer as a fraction of saturation.
  sthzw(npnts),                                                                &
    ! Fraction soil moisture content in deep layer.
  zdepth(0:nshyd)
    ! Soil layer depth at lower boundary (m).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  zw(npnts)
    ! Water table depth (m).

!-----------------------------------------------------------------------------
! Aray arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  zw_inund(npnts),                                                             &
    ! Adjusted water table depth (m).
  wutot(npnts)
    ! UNFROZEN to TOTAL fraction at ZW.

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n
    ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  sthuk,                                                                       &
    ! fractional unfrozen soil moisture interpolated to water table.
  sthfk,                                                                       &
    ! fractional frozen soil moisture interpolated to water table.
  sthzwu,                                                                      &
    ! Estimated fractional unfrozen soil moisture content in deep
    ! LSH/TOPMODEL layer (assuming sthf in this layer=sthf(nshyd))
  zw_old,                                                                      &
    ! zw from last timestep (m).
  psisat,                                                                      &
    ! Saturated soil water pressure (m) (negative).
  fac1,                                                                        &
    ! Soil moisture related variable.
  fac2,                                                                        &
    ! Soil moisture related variable.
  facb,                                                                        &
    ! fac^bexp.
  unfroz_frac_avg,                                                             &
    ! Weighted average of unfroz_frac.
  zwest_mean,                                                                  &
    ! Weighted average of zwest_l.
  zwdef
    ! Water table depth related parameter.

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  unfroz_frac(npnts,nshyd+1),                                                  &
    !  Fractional space available for unfrozen soil moisture in soil layer.
  zwest_l(nshyd),                                                              &
    !  Water estimate with Newton-Raphson iteration.
  psi(nshyd),                                                                  &
    ! soil pressure in soil layer.
  sthf(npnts,nshyd),                                                           &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  avgzd(nshyd+1)
    ! Soil layer depth in centre (m).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_ZW_INUND'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate depth of centre point of each layer:
!-----------------------------------------------------------------------------
DO n = 1,nshyd
  avgzd(n) = 0.5 * (zdepth(n-1) + zdepth(n))
END DO
avgzd(nshyd+1) = 0.5 * (zw_max - zdepth(nshyd)) + zdepth(nshyd)

!-----------------------------------------------------------------------------
! Calculate the available space for unfrozen soil moisture in each soil layer:
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)

  DO n = 1,nshyd
    IF (smclsat(i,n) > EPSILON(1.0)) THEN
      sthf(i,n) = smcl(i,n) / smclsat(i,n) - sthu(i,n)
    ELSE
      sthf(i,n) = 0.0
    END IF

    unfroz_frac(i,n) = 1.0 - sthf(i,n)
    unfroz_frac(i,n) = MAX(unfroz_frac(i,n),0.0)
    unfroz_frac(i,n) = MIN(unfroz_frac(i,n),1.0)
  END DO

  unfroz_frac(i,nshyd+1) = 1.0 - sthf(i,nshyd)
  unfroz_frac(i,nshyd+1) = MAX(unfroz_frac(i,nshyd+1),0.0)
  unfroz_frac(i,nshyd+1) = MIN(unfroz_frac(i,nshyd+1),1.0)
END DO

!-----------------------------------------------------------------------------
! Add code to allow for the fact that the water table isn't well estimated
! using the method in calc_zw_jls.F90 when one layer is significantly
! frozen (see documentation).
! Instead use the same fundamental equations above but for each
! layer containing or above the provisional water table
! and weight accordingly:
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)

  zw_inund(i)     = zw(i)
  wutot(i)        = 1.0
  unfroz_frac_avg = 0.0
  DO n = 1,nshyd
    unfroz_frac_avg = unfroz_frac_avg                                          &
                      + (unfroz_frac(i,n) * (zdepth(n) - zdepth(n-1)))
  END DO
  unfroz_frac_avg   = unfroz_frac_avg                                          &
                      + (unfroz_frac(i,nshyd+1) * (zw_max - zdepth(nshyd)))

  unfroz_frac_avg   = unfroz_frac_avg / zw_max

  IF (1.0 - unfroz_frac_avg > EPSILON(1.0) .AND. zw(i) < zw_max) THEN
    zw_old = zw(i)
    DO n = 1,nshyd
      zwest_l(n) = 0.0
    END DO
    zwest_mean = 0.0
    zwdef = zw_old
    IF (zw_old > zdepth(nshyd)) THEN
      zwdef = zdepth(nshyd)
    END IF

    !-------------------------------------------------------------------------
    ! Interpolate thetaf to zw and use this in psisat
    ! and include frozen soil into calc of psisat
    !-------------------------------------------------------------------------
    IF (zw(i) < avgzd(1)) THEN
      sthfk = sthf(i,1)
      fac1 = MAX(MIN(1.0 - sthfk, 1.0), 0.0)
      facb = MAX(fac1**bexp(i,1), EPSILON(1.0))
      psisat = -sathh(i,1) / facb
    END IF

    DO n = 1,nshyd-1
      IF (zw(i) >= avgzd(n) .AND. zw(i) < avgzd(n+1)) THEN
        sthfk = (sthf(i,n) * (avgzd(n+1) - zw(i))                              &
                +sthf(i,n+1) * (zw(i) - avgzd(n))) / (avgzd(n+1) - avgzd(n))
        fac1 = MAX(MIN(1.0 - sthfk, 1.0), 0.0)
        facb = MAX(fac1**bexp(i,n), EPSILON(1.0))
        psisat = -sathh(i,n) / facb
      END IF
    END DO

    IF (zw(i) >= avgzd(nshyd)) THEN
      sthfk = sthf(i,nshyd)
      fac1 = MAX(MIN(1.0 - sthfk, 1.0), 0.0)
      facb = MAX(fac1**bexp(i,nshyd), EPSILON(1.0))
      psisat = -sathh(i,nshyd) / facb
    END IF

    DO n = 1,nshyd

      fac2 = MAX(MIN(sthu(i,n), 1.0), 0.0)
      facb = MAX(fac2**bexp(i,n), EPSILON(1.0))
      psi(n) = -sathh(i,n) / facb

      zwest_l(n) = psisat - psi(n) + (0.5 * (zdepth(n) + zdepth(n-1)))
      zwest_l(n) = MAX(zwest_l(n),0.0)
      zwest_l(n) = MIN(zwest_l(n),zw_max)

      IF (zwdef <  zdepth(n) .AND. zwdef >  zdepth(n-1)) THEN
        zwest_mean = zwest_mean + (zwest_l(n) * (zwdef - zdepth(n-1)))
      END IF

      IF (zwdef >= zdepth(n)) THEN
        zwest_mean = zwest_mean + (zwest_l(n) * (zdepth(n) - zdepth(n-1)))
      END IF

    END DO

    zwest_mean = zwest_mean / zwdef
    zwest_mean = MAX(zwest_mean,0.0)
    zwest_mean = MIN(zwest_mean,zw_max)

    IF (zwest_mean > zw_old) THEN
      zw_inund(i) = (unfroz_frac_avg * zw_old)                                 &
                    + ((1.0 - unfroz_frac_avg) * zwest_mean)
    END IF

  END IF
END DO

!-----------------------------------------------------------------------------
! wutot needs to be based on corrected zw due to partially
! frozen soil moisture fraction:
! interpolate thetas to zw_inund and use this in wutot
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)

  IF (zw_inund(i) < avgzd(1)) THEN
    sthfk = sthf(i,1)
    sthuk = sthu(i,1)
  END IF

  DO n = 1,nshyd-1
    IF (zw_inund(i) >= avgzd(n) .AND. zw_inund(i) < avgzd(n+1)) THEN
      sthfk = ((sthf(i,1) * (avgzd(n+1) - zw_inund(i)))                        &
              + (sthf(i,n+1) * (zw_inund(i) - avgzd(n))))                      &
              / (avgzd(n+1) - avgzd(n))

      sthuk = ((sthu(i,n) * (avgzd(n+1) - zw_inund(i)))                        &
              + (sthu(i,n+1) * (zw_inund(i) - avgzd(n))))                      &
              / (avgzd(n+1) - avgzd(n))
    END IF
  END DO

  sthzwu  = sthzw(i) - sthf(i,nshyd)
  sthzwu  = MAX(sthzwu,0.0)

  IF (zw_inund(i) >= avgzd(nshyd)) THEN
    sthfk = sthf(i,nshyd)
  END IF

  IF (zw_inund(i) >= avgzd(nshyd) .AND. zw_inund(i) < avgzd(nshyd+1)) THEN
    sthuk = ((sthu(i,nshyd) * (avgzd(nshyd+1) - zw_inund(i)))                  &
            + (sthzwu * (zw_inund(i) - avgzd(nshyd))))                         &
            / (avgzd(nshyd+1) - avgzd(nshyd))
  END IF

  IF (zw_inund(i) >= avgzd(nshyd+1)) THEN
    sthuk = sthzwu
  END IF

  IF (sthuk + sthfk > EPSILON(1.0)) THEN
    wutot(i) = sthuk / (sthuk + sthfk)
  END IF

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE calc_zw_inund
END MODULE calc_zw_inund_mod
#endif
