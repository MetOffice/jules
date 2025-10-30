! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Description:
! Calculates the soil respiration based on a simplified version of the
! model of Raich et al. (1991).

! **********************************************************************
MODULE microbe_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: modulename = 'MICROBE_MOD'

CONTAINS

SUBROUTINE microbe (land_pts,dim_cs1,l_q10,cs,                                 &
                    sthu,sthf,v_sat,v_wilt,tsoil,resp_s,veg_frac,              &
                    sf_diag,l_soil_point)

USE jules_soil_biogeochem_mod, ONLY:                                           &
! imported scalar parameters
   soil_model_1pool, soil_model_4pool,                                         &
! imported scalar variables (IN)
   kaps, l_layeredC, l_soil_resp_lev2, q10 => q10_soil, soil_bgc_model,        &
   tau_resp,                                                                   &
! imported array variables (IN)
   kaps_4pool

USE jules_soil_mod, ONLY:                                                      &
 dzsoil,sm_levels

USE sf_diags_mod, ONLY: strnewsfdiag

USE ancil_info, ONLY: dim_cslayer

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                            ! IN Number of land points to be
!                                 !    processed.
,dim_cs1                    ! IN soil carbon dimensions

LOGICAL, INTENT(IN OUT) ::                                                     &
 l_q10                      ! TRUE if using Q10 for soil resp

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 cs(land_pts,dim_cslayer,dim_cs1)                                              &
                            ! IN Soil carbon (kg C/m2).
!                                 !    For 4-pool soil C (dim_cs1=4), the pools
!                                 !    are DPM, RPM, biomass and humus.
,veg_frac(land_pts)                                                            &
                            ! IN vegetated fraction.
,sthu(land_pts,sm_levels)                                                      &
                            ! IN unfrozen soil moisture as a
!                                 !    fraction of saturation (m3/m3).
,sthf(land_pts,sm_levels)                                                      &
                            ! IN frozen soil moisture as a
!                                 !    fraction of saturation (m3/m3).
,v_sat(land_pts,sm_levels)                                                     &
                            ! IN Volumetric soil moisture
!                                 !    concentration at saturation
!                                 !    (m3 H2O/m3 soil).
,v_wilt(land_pts,sm_levels)                                                    &
                            ! IN Volumetric soil moisture
!                                 !    concentration below which
!                                 !    stomata close (m3 H2O/m3 soil).
,tsoil(land_pts,sm_levels)
                            ! IN Soil temperature (K).

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 resp_s(land_pts,dim_cslayer,dim_cs1)
                            ! OUT Soil respiration (kg C/m2/s).

LOGICAL, INTENT(IN) :: l_soil_point(land_pts)

REAL(KIND=real_jlslsm) ::                                                      &
 sth_soil(land_pts,sm_levels)                                                  &
                            ! WORK Soil moisture as a
!                                 !    fraction of saturation (m3/m3).
,fsth(land_pts,sm_levels)                                                      &
                            ! WORK Factors describing the...
,fprf(land_pts)                                                                &
                            !     ...influence of soil moisture,...
,ftemp(land_pts,sm_levels)                                                     &
                            !     ...vegetation cover and
!                           !     soil temperature respectively
!                           !     on the soil respiration.
,sth_resp_min                                                                  &
                            ! WORK soil moist giving min. resp
,sth_opt                                                                       &
                            ! WORK Fractional soil moisture at
!                                 !      which respiration is maximum.
,sth_wilt                   ! WORK Wilting soil moisture as a
!                                 !      fraction of saturation.
INTEGER ::                                                                     &
 l,n,j                      ! Loop counters

!-----------------------------------------------------------------------
! Local parameters
!-----------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 min_factor = 1.7          ! FACTOR to scale WILT to get RESP_MIN
!                                ! at 25 deg C and optimum soil
!                                ! moisture (/s).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='MICROBE'

IF (lhook) CALL dr_hook(modulename//':'//RoutineName,zhook_in,zhook_handle)

! initialise INTENT(OUT) variable
resp_s(:,:,:) = 0.0

IF ( soil_bgc_model == soil_model_1pool ) THEN
  l_q10      = .TRUE.
  min_factor = 1.0
END IF

! FSTH

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(l,n,j,sth_wilt,sth_opt,sth_resp_min)

DO j = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    fsth(l,j) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

IF (l_soil_resp_lev2) THEN
  DO j = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      sth_soil(l,j) = sthu(l,j) + sthf(l,j)
    END DO
    !OMP END PARALLEL DO
  END DO
ELSE
  DO j = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      sth_soil(l,j) = sthu(l,j)
    END DO
!$OMP END DO NOWAIT
  END DO
END IF

DO j = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    IF (l_soil_point(l)) THEN !Replaces previous test against sm_sat
      sth_wilt      = v_wilt(l,j) / v_sat(l,j)
      sth_opt       = 0.5 * (1 + sth_wilt)
      sth_resp_min  = sth_wilt * min_factor
      fsth(l,j)     = 0.2

      IF (sth_soil(l,j) > sth_resp_min .AND.                                   &
            sth_soil(l,j) <= sth_opt) THEN
        fsth(l,j) = 0.2 + (0.8 * ((sth_soil(l,j) - sth_resp_min)               &
                          /  (sth_opt - sth_resp_min)))
      ELSE IF (sth_soil(l,j) > sth_opt) THEN
        fsth(l,j) = 1 - (0.8 * (sth_soil(l,j) - sth_opt))
      END IF

    END IF
  END DO
!$OMP END DO NOWAIT
END DO

! FTEMP
IF (l_q10) THEN
  ! use original HadCM3LC Q10 formula
  DO j = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      ftemp(l,j) = q10 ** (0.1 * (tsoil(l,j) - 282.4))
    END DO
!$OMP END DO NOWAIT
  END DO
ELSE
  ! use 4-pool temperature formula (using TSOIL for now...)
  DO j = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      ftemp(l,j) = 0.0
      IF (tsoil(l,j) > 263.15)                                                 &
        ftemp(l,j) = 47.9 / (1.0 + EXP(106.0 / (tsoil(l,j) - 254.85)))
    END DO
!$OMP END DO NOWAIT
  END DO
END IF

! FPRF - plant retainment factor
!          =0.6 for fully vegetated
!          =1.0 for bare soil
!        only set for 4-pool soil C runs.

IF ( soil_bgc_model == soil_model_4pool ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    fprf(l) = 0.6 + 0.4 * (1 - veg_frac(l))
  END DO
!$OMP END DO NOWAIT
END IF

! set 1-D or 4-D soil resp depending on whether using 4-pools or not
IF (l_layeredC) THEN
  IF ( soil_bgc_model == soil_model_4pool ) THEN
    DO n = 1,dim_cs1
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        resp_s(l,1,n) = kaps_4pool(n) * cs(l,1,n) * fsth(l,1) * ftemp(l,1) *   &
                        fprf(l) * (EXP(-0.5 * dzsoil(1) * tau_resp))
        DO j = 2,dim_cslayer
          resp_s(l,j,n) = kaps_4pool(n) * cs(l,j,n) * fsth(l,j) * ftemp(l,j) * &
                          fprf(l) * (EXP( - (SUM(dzsoil(1:(j-1)))              &
                          + 0.5 * dzsoil(j)) * tau_resp))
        END DO
      END DO
!$OMP END DO NOWAIT
    END DO
  ELSE
    !   1-pool model
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      resp_s(l,1,1) = kaps * cs(l,1,1) * fsth(l,1) * ftemp(l,1) *              &
                      (EXP(-0.5 * dzsoil(1) * tau_resp))
      DO j = 2,dim_cslayer
        resp_s(l,j,1) = kaps * cs(l,j,1) * fsth(l,j) * ftemp(l,j) *            &
                        (EXP( - (SUM(dzsoil(1:(j-1))) +                        &
                        0.5 * dzsoil(j)) * tau_resp))
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF  !  soil_bgc_model

ELSE


  !  .NOT. l_layeredC
  IF ( soil_bgc_model == soil_model_4pool ) THEN
    DO n = 1,dim_cs1
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        IF (l_soil_resp_lev2) THEN
          resp_s(l,1,n) = kaps_4pool(n) * cs(l,1,n) * fsth(l,2) *              &
                          ftemp(l,2) * fprf(l)
        ELSE
          resp_s(l,1,n) = kaps_4pool(n) * cs(l,1,n) * fsth(l,1) *              &
                          ftemp(l,1) * fprf(l)
        END IF
      END DO
!$OMP END DO NOWAIT
    END DO
  ELSE
    !   1-pool model
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      IF (l_soil_resp_lev2) THEN
        resp_s(l,1,1) = kaps * cs(l,1,1) * fsth(l,2) * ftemp(l,2)
      ELSE
        resp_s(l,1,1) = kaps * cs(l,1,1) * fsth(l,1) * ftemp(l,1)
      END IF
    END DO
!$OMP END DO NOWAIT
  END IF  !  soil_bgc_model

END IF  !  l_layeredC

!$OMP END PARALLEL

! Copy rate modifiers to SF_DIAG for outputting via DIAGNOSTICS_BL.F90

IF (sf_diag%l_ftemp) THEN
  DO l = 1,land_pts
    DO j = 1,sm_levels
      sf_diag%ftemp(l,j) = ftemp(l,j)
    END DO
  END DO
END IF

IF (sf_diag%l_fsth) THEN
  DO l = 1,land_pts
    DO j = 1,sm_levels
      sf_diag%fsth(l,j) = fsth(l,j)
    END DO
  END DO
END IF

IF (sf_diag%l_fprf) THEN
  DO l = 1,land_pts
    sf_diag%fprf(l) = fprf(l)
  END DO
END IF


IF (lhook) CALL dr_hook(modulename//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE microbe

END MODULE microbe_mod
