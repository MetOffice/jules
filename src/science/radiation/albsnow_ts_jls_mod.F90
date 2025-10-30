! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Routine to calculate spectral snow albedos for JULES, using a
! two-stream model.

! *********************************************************************
MODULE albsnow_ts_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ALBSNOW_TS_MOD'
CONTAINS
SUBROUTINE albsnow_ts (land_pts,snow_pts,snow_index,cosz_gb,                   &
                       albudir,albudif,rgrain,snowmass,soot_gb,alb_snow)

USE jules_snow_mod,       ONLY: cnr_g, cnr_om, dce
USE water_constants_mod,  ONLY: rho_ice

USE jules_science_fixes_mod, ONLY: l_fix_albsnow_ts

USE parkind1,             ONLY: jprb, jpim
USE yomhook,              ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Subroutine arguments
!   Scalar arguments with intent(in):
INTEGER, INTENT(IN) :: land_pts
                             ! Number of land points.
INTEGER, INTENT(IN) :: snow_pts
                             ! Number of snow-covered points

!   Array arguments with intent(in):
INTEGER, INTENT(IN) :: snow_index(land_pts)
                             ! Index of land points with snow cover

REAL(KIND=real_jlslsm), INTENT(IN) :: albudir(land_pts,2)
                             ! Direct albedos of the substrate
REAL(KIND=real_jlslsm), INTENT(IN) :: albudif(land_pts,2)
                             ! Diffuse albedos of the substrate
REAL(KIND=real_jlslsm), INTENT(IN) :: cosz_gb(land_pts)
                             ! Cosine of the zenith angle.
REAL(KIND=real_jlslsm), INTENT(IN) :: rgrain(land_pts)
                             ! Snow grain size (microns).
REAL(KIND=real_jlslsm), INTENT(IN) :: snowmass(land_pts)
                             ! Mass loading of snow
REAL(KIND=real_jlslsm), INTENT(IN) :: soot_gb(land_pts)
                              ! Snow soot content (kg/kg).

!   Array arguments with intent(out):
REAL(KIND=real_jlslsm), INTENT(OUT) :: alb_snow(land_pts,4)
!                              Snow albedo.
!                              (:,1) - Direct beam visible
!                              (:,2) - Diffuse visible
!                              (:,3) - Direct beam near-IR
!                              (:,4) - Diffuse near-IR

! Local scalars:
REAL(KIND=real_jlslsm) :: deff
                             ! Effective dimension of snow crystals
REAL(KIND=real_jlslsm) :: log_deff
                             ! LOG(deff)
REAL(KIND=real_jlslsm) :: tau
                             ! Optical depth of snow pack
REAL(KIND=real_jlslsm) :: omega
                             ! Albedo fo single scattering of snowpack
REAL(KIND=real_jlslsm) :: asym
                             ! Asymmetry of snow crystals
REAL(KIND=real_jlslsm) :: frwd
                             ! Forward scattering in the snowpack
REAL(KIND=real_jlslsm) :: beta
                             ! Diffuse backward scattering coefficient
REAL(KIND=real_jlslsm) :: beta_s
                             ! Solar backward scattering coefficient
REAL(KIND=real_jlslsm), PARAMETER :: diff = 2.0
                             ! Diffusivity factor
REAL(KIND=real_jlslsm) :: alpha_1
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: alpha_2
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: alpha_3
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: alpha_4
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: lmb
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: gamma2
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: pp
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: sigma_1
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: sigma_2
                             ! Two-stream coefficient
REAL(KIND=real_jlslsm) :: rdd
                             ! Diffuse reflection coefficient
REAL(KIND=real_jlslsm) :: tdd
                             ! Diffuse transmission coefficient
REAL(KIND=real_jlslsm) :: tss
                             ! Solar transmission coefficient
REAL(KIND=real_jlslsm) :: rsd
                             ! Direct-diffuse reflection coefficient
REAL(KIND=real_jlslsm) :: tsd
                             ! Direct-diffuse transmission coefficient

INTEGER :: band,j,l          ! Loop counters.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALBSNOW_TS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! 2-Band scheme, VIS and NIR.
DO band = 1, 2
!$OMP PARALLEL DO IF(snow_pts>1) DEFAULT(NONE) SCHEDULE(STATIC)                &
!$OMP PRIVATE(j,l,deff,log_deff,tau,asym,omega,frwd,beta,beta_s,alpha_1,rsd,   &
!$OMP alpha_2,alpha_3,alpha_4,lmb,gamma2,pp,rdd,tdd,tss,sigma_1,sigma_2,tsd)   &
!$OMP SHARED(snow_pts,snow_index,alb_snow,band,albudir,snowmass,cosz_gb,       &
!$OMP rgrain,cnr_g,cnr_om,soot_gb,albudif,l_fix_albsnow_ts,dce)
  DO j = 1,snow_pts
    l = snow_index(j)

    !   Provisionally set albedos for snow layer to albedos for
    !   ground to cover the case of no snow.
    alb_snow(l,2 * band-1) = albudir(l,band)
    alb_snow(l,2 * band)   = albudif(l,band)

    !   Recalculate where snow is present.
    IF ( (snowmass(l) > 0.0) .AND. (cosz_gb(l) > EPSILON(1.0)) ) THEN

      !     Radiative effective dimension, recalling that rgrain
      !     is in microns.
      deff     = 2.0 * rgrain(l) * 1.0e-6
      log_deff = LOG(deff)

      !     Single scattering properties
      tau   = 3.0 * snowmass(l) / (rho_ice * deff)
      asym  = cnr_g(1,band) + cnr_g(2,band) * log_deff
      omega = 1.0 / (1.0 + cnr_om(1,1,band) + log_deff *                       &
                    (cnr_om(2,1,band) + log_deff * cnr_om(3,1,band)) +         &
                    deff * cnr_om(2,2,band) * soot_gb(l) / dce)

      !     Rescale
      frwd  = asym * asym
      asym  = (asym - frwd) / (1.0 - frwd)
      tau   = (1.0 - omega * frwd) * tau
      omega = (1.0 - frwd) * omega / (1.0 - omega * frwd)

      !     Two stream coefficients (PIFM85)
      beta    = (4.0 - frwd - 3.0 * asym) / 8.0
      beta_s  = 0.5 - 0.75 * asym * cosz_gb(l)
      alpha_1 = diff * (1.0 - omega * (1.0 - beta))
      alpha_2 = diff * omega * beta
      alpha_3 = omega * beta_s
      alpha_4 = omega * (1.0 - beta_s)

      !     Reflection and Transmission coefficients
      lmb    = SQRT(alpha_1 * alpha_1 - alpha_2 * alpha_2)
      gamma2 = alpha_2 / (alpha_1 + lmb)
      pp     = EXP( -lmb * tau )
      rdd    = gamma2 * (1.0 - pp * pp) / (1.0 - (gamma2 * pp)**2)
      tdd    = pp * (1.0 - gamma2 * gamma2) / (1.0 - (gamma2 * pp)**2)
      tss    = EXP( -tau / cosz_gb(l) )

      !     Calculate explicit solar transmission and reflection
      !     unless very close to the solar beam.
      IF ( ABS(lmb * cosz_gb(l) - 1.0) > 100.0 * EPSILON(1.0) ) THEN
        sigma_1 = ( cosz_gb(l) *                                               &
          (alpha_1 * alpha_3 + alpha_2 * alpha_4) - alpha_3) /                 &
          ( (lmb * cosz_gb(l))**2-1.0)
        sigma_2 = ( cosz_gb(l) *                                               &
          (alpha_1 * alpha_4 + alpha_2 * alpha_3) + alpha_4) /                 &
          ( (lmb * cosz_gb(l))**2-1.0)
        IF (l_fix_albsnow_ts) THEN
          rsd = sigma_1 * (1.0 - tdd * tss) - sigma_2 * rdd
        ELSE
          rsd = sigma_1 * (1.0 - tdd) - sigma_2 * rdd
        END IF
        tsd = sigma_2 * (tss - tdd) - sigma_1 * rdd * tss
      ELSE
        rsd = rdd
        tsd = tdd
      END IF
      !
      !     Overall reflection coefficients
      alb_snow(l,2 * band-1) = rsd + tdd *                                     &
        (albudif(l,band) * tsd + albudir(l,band) * tss) /                      &
        (1.0 - albudif(l,band) * rdd)
      alb_snow(l,2 * band)   = rdd +                                           &
        albudif(l,band) * tdd * tdd /                                          &
        (1.0 - albudif(l,band) * rdd)
      alb_snow(l,2 * band-1) = MAX(0.0, MIN(alb_snow(l,2 * band-1), 1.0))
      alb_snow(l,2 * band)   = MAX(0.0, MIN(alb_snow(l,2 * band),   1.0))
      !
    END IF
  END DO
!$OMP END PARALLEL DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE albsnow_ts
END MODULE albsnow_ts_mod
