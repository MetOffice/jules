! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE CALC_FSAT----------------------------------------------

! Description:
!     Calculates the surface saturation fraction.

MODULE calc_fsat_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CALC_FSAT_MOD'

CONTAINS

SUBROUTINE calc_fsat( npnts, soil_pts, l_gamtot, soil_index,                   &
                      ti_mean, ti_sig, top_crit, wutot,                        &
                      gamtot, fsat, fwetl )

USE c_topog,              ONLY: dti, sigma_logk
USE jules_hydrology_mod,  ONLY: ti_wetl

USE parkind1,             ONLY: jprb, jpim
USE yomhook,              ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! No. of land points.
  soil_pts
    ! Number of soil points.

LOGICAL, INTENT(IN) ::                                                         &
  l_gamtot
    ! TRUE if need to calculate GAMTOT

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  ti_mean(npnts),                                                              &
    ! Gridbox mean topographic index.
  ti_sig(npnts),                                                               &
    ! Standard deviation in topographic index.
  top_crit(npnts),                                                             &
    ! Critical topographic index required to calculate the surface
    ! saturation fraction.
  wutot(npnts)
    ! UNFROZEN to TOTAL fraction at ZW.

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  gamtot(npnts),                                                               &
    ! Integrated complete Gamma function.
  fsat(npnts),                                                                 &
    ! Surface saturation fraction.
  fwetl(npnts)
    ! Wetland fraction.

!-----------------------------------------------------------------------------
! Local parameters:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: log_ti_sc_const = LOG(2.7)
    ! log_ti_sc_const Constant required to ensure that there is no underflow
    ! or overflow in the pdf calculation. (See Documentation).

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j,                                                                        &
    ! Loop counter for points.
  nti,                                                                         &
    ! Loop counter for topographic index.
  mti,                                                                         &
    ! Max loop count for topographic index.
  mti_w
    ! Max loop count for topographic index.

REAL(KIND=real_jlslsm) ::                                                      &
  alf,                                                                         &
    ! Parameter in incomplete Gamma function.
  alf_ksat,                                                                    &
    ! Parameter in incomplete Gamma function including horizontal ksat
    ! variability.
  cum,                                                                         &
    ! Integrated incomplete Gamma fn.
  ti_sc,                                                                       &
    ! Incremented Topographic index. Scaled by ti_mean.
  dti_sc,                                                                      &
    ! Scale increment in TI.
  calc,                                                                        &
    ! Incremental value for pdf calculation.
  ticr_sc,                                                                     &
    ! Critical topographic index. (Any value above this gives surface
    ! saturation). Scaled by TI_MEAN.
  ticr_sc_w,                                                                   &
    ! As above, but for wetland fraction calc.
  fbox_s,                                                                      &
    ! Fraction of box for integration.
  fbox_w
    ! Fraction of box for integration.

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  ti_sig_use(npnts),                                                           &
    ! Standard deviation in topographic index with a minimum value set.
  ti_max_use(npnts),                                                           &
    ! TI_MAX dependent on ti_sig.
  dti_use(npnts)
    ! increment dependent on ti_sig.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_FSAT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Set up maximum topographic index to integrate to and increment
! factor. These are topography dependent to maximise accuracy and
! minimise runtime:
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  ! Set a minimum possible value for TI_SIG for safety.
  ti_sig_use(i) = MAX(ti_sig(i),0.5)
  ti_max_use(i) = ti_mean(i) + 5.0 * ti_sig_use(i)
  dti_use(i)    = dti / (ti_sig_use(i) * ti_sig_use(i))
END DO

!-----------------------------------------------------------------------------
! Calculate the total integral under the Gamma function. Carried
! out via the reconfiguration:
!-----------------------------------------------------------------------------
IF (l_gamtot) THEN

  DO j = 1,soil_pts
    i = soil_index(j)

    mti      = NINT(ti_max_use(i) / dti_use(i))
    dti_sc   = dti_use(i) / ti_mean(i)

    alf      = (ti_mean(i) / ti_sig_use(i))**2
    alf_ksat = 1.0 / (1.0 / alf + (sigma_logk / ti_mean(i))**2)

    DO nti = 1,mti
      ti_sc = (nti-0.5) * dti_sc
      IF ((alf_ksat-1.0) * LOG(ti_sc) > LOG(TINY(ti_sc))) THEN
        calc      = (alf_ksat-1.0) * (LOG(ti_sc) + log_ti_sc_const)            &
                    - (alf_ksat * ti_sc)
        gamtot(i) = gamtot(i) + EXP(calc)
      END IF
    END DO
  END DO

ELSE

  !---------------------------------------------------------------------------
  ! Calculate the integral under the incomplete Gamma function for the
  ! saturation surface fraction:
  !---------------------------------------------------------------------------
  DO j = 1,soil_pts
    i = soil_index(j)

    IF (top_crit(i) <  ti_max_use(i)) THEN

      ticr_sc   = top_crit(i) / ti_mean(i) + 1.0
      ticr_sc_w = ticr_sc + ti_wetl / ti_mean(i)

      IF (ticr_sc * ti_mean(i) <  ti_max_use(i)) THEN

        alf      = (ti_mean(i) / ti_sig_use(i))**2
        alf_ksat = 1.0 / (1.0 / alf + (sigma_logk / ti_mean(i))**2)

        dti_sc   = dti_use(i) / ti_mean(i)
        mti      = INT(ticr_sc / dti_sc)

        cum = 0.0
        DO nti = 1,mti
          ti_sc = (nti-0.5) * dti_sc
          IF ((alf_ksat-1.0) * LOG(ti_sc) > LOG(TINY(ti_sc))) THEN
            calc = (alf_ksat-1.0) * (LOG(ti_sc) + log_ti_sc_const)             &
                   - (alf_ksat * ti_sc)
            cum  = cum + EXP(calc)
          END IF
        END DO

        ! Include the fractional increment:
        fbox_s = (ticr_sc - mti * dti_sc) / dti_sc
        ti_sc  = (mti+0.5 * fbox_s) * dti_sc
        IF ((alf_ksat-1.0) * LOG(ti_sc) > LOG(TINY(ti_sc))) THEN
          calc = (alf_ksat-1.0) * (LOG(ti_sc) + log_ti_sc_const)               &
                 - (alf_ksat * ti_sc)
          cum  = cum + EXP(calc) * fbox_s
        END IF

        fsat(i) = 1.0 - cum / gamtot(i)

        !---------------------------------------------------------------------
        ! Calculate the integral under the incomplete Gamma function for the
        ! wetland fraction:
        !---------------------------------------------------------------------
        IF (ticr_sc_w * ti_mean(i) <  ti_max_use(i)) THEN

          mti_w = INT(ticr_sc_w / dti_sc)

          ! Include the fractional increment:
          IF (mti_w == mti) THEN
            fbox_w = (ticr_sc_w - mti * dti_sc) / dti_sc
          ELSE
            fbox_w = 1.0
          END IF

          ti_sc = (mti+0.5 * fbox_w) * dti_sc
          IF ((alf_ksat-1.0) * LOG(ti_sc) > LOG(TINY(ti_sc))) THEN
            calc = (alf_ksat-1.0) * (LOG(ti_sc) + log_ti_sc_const)             &
                   - (alf_ksat * ti_sc)
            cum = cum + EXP(calc) * (fbox_w - fbox_s)
          END IF

          DO nti = mti+2,mti_w
            ti_sc = (nti-0.5) * dti_sc
            IF ((alf_ksat-1.0) * LOG(ti_sc) > LOG(TINY(ti_sc))) THEN
              calc = (alf_ksat-1.0) * (LOG(ti_sc) + log_ti_sc_const)           &
                     - (alf_ksat * ti_sc)
              cum  = cum + EXP(calc)
            END IF
          END DO

          ! Include the fractional increment:
          fbox_w = (ticr_sc_w - mti_w * dti_sc) / dti_sc
          ti_sc  = (mti_w+0.5 * fbox_w) * dti_sc
          IF ((alf_ksat-1.0) * LOG(ti_sc) > LOG(TINY(ti_sc))) THEN
            calc = (alf_ksat-1.0) * (LOG(ti_sc) + log_ti_sc_const)             &
                   - (alf_ksat * ti_sc)
            cum  = cum + EXP(calc) * fbox_w
          END IF

          fwetl(i) = -1.0 + cum / gamtot(i) + fsat(i)
        END IF

        IF (fwetl(i) <  0.0)     fwetl(i) = 0.0
        IF (fsat(i) <  0.0)      fsat(i)  = 0.0
        IF (fwetl(i) >  fsat(i)) fwetl(i) = fsat(i)

        !---------------------------------------------------------------------
        ! Assume that in partially frozen water, flow is inhibited, therefore
        ! the critical flow for wetland area is no longer valid:
        !---------------------------------------------------------------------
        fwetl(i) = fwetl(i) * wutot(i) + fsat(i) * (1.0 - wutot(i))

      END IF
    END IF

  END DO

END IF  !  l_gam_tot

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_fsat
END MODULE calc_fsat_mod
!-----------------------------------------------------------------------------
