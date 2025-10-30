#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!   Subroutine CALC_FIT_FSAT-------------------------------------------
!
!   Purpose: To speed up the large scale hydrology code (LTOP=TRUE)
!            dramatically. This is done by fitting exponential
!            functions to the incomplete gamma function for each grid
!            box and the complete range of possible "water table"
!            (top_crit) cases - see documentation.
!            Estimates the fitted parameters for Fsat=function(ZW)
!            and  Fwet=function(ZW) for each land grid point.
!            (Calculating the incomplete gamma function for each grid
!            box at each time step was very time consuming).
!                                                             !
! Documentation: UNIFIED MODEL DOCUMENTATION PAPER NO 25
!
! History:
! Version   Date     Comment
! -------   ----     -------
!  6.4    10/01/07   New Deck         Nic Gedney
!
! Code Description:
!   Language: FORTRAN 77 + common extensions.

MODULE calc_fit_fsat_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE calc_fit_fsat(toppdm,ainfo)

  !Use in relevant subroutines
USE calc_fsat_mod, ONLY: calc_fsat

!Use in relevant varaibles
USE ancil_info,           ONLY:                                                &
  land_pts, soil_pts, nsoilt

USE jules_hydrology_mod,  ONLY:                                                &
  l_top, zw_max, nfita

USE jules_soil_mod,       ONLY:                                                &
  dzsoil, sm_levels

USE jules_print_mgr,      ONLY:                                                &
  jules_message, jules_print

USE um_types, ONLY: real_jlslsm

! Type Definitions
USE top_pdm, ONLY: top_pdm_type
USE ancil_info,    ONLY: ainfo_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Subroutine arguments
! TYPE Definitions
TYPE(top_pdm_type), INTENT(IN OUT) :: toppdm
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo

!-----------------------------------------------------------------------------

! Work variables
REAL(KIND=real_jlslsm) :: zdepth           ! Standard Soil model DEPTH.

! Local scalars:
INTEGER, PARAMETER :: nzw = 100  ! Number of ZW values used in fit.
                                 ! Maximum value for a significant
                                 ! improvement in the fit.

INTEGER :: i,j,iz,n,ifita,m  ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  dzw,                                                                         &
                 ! WORK ZW increment ; defined by zw_max and nzw.
  rms,                                                                         &
                 ! WORK rms errors for given fsat fit values.
  rmsw,                                                                        &
                 ! WORK rms errors for given fwet fit values.
  rmsold,                                                                      &
                 ! WORK rms errors for given fsat fit values.
                 !      for best fit so far.
  rmswold,                                                                     &
                 ! WORK rms errors for given fwet fit values
                 !      for best fit so far.
  cfit,                                                                        &
                 ! WORK CFit value for given loop.
  cfitmax
                 ! Maximum possible value for Cfit.

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  thr_err = 5.0e-3,                                                            &
                 ! Error threshold value

  cfitmin = 0.0
                 ! Minimum possible value for Cfit.

! Local arrays:
REAL(KIND=real_jlslsm) ::                                                      &
  fsat_calc_soilt(land_pts,nsoilt,nzw),                                        &
                            ! WORK Surface saturation fraction.
  fsat_fit(nzw),                                                               &
                            ! WORK Fitted surface saturation fraction.
  fwet_calc_soilt(land_pts,nsoilt,nzw),                                        &
                            ! WORK Wetland fraction.
  fwet_fit(nzw),                                                               &
                            ! WORK Fitted wetland fraction.
  dumzw(nzw),                                                                  &
                            ! WORK Dummy water table depth (m).
  dumfsat_soilt(land_pts,nsoilt),                                              &
                            ! WORK Dummy surface saturation fraction.
  dumfwetl_soilt(land_pts,nsoilt)
                            ! WORK Dummy wetland fraction.
! DBC We could use this local version of gamtot in this version,
!     but using the module version to match other versions of code.
!     &,toppdm%gamtot_soilt(land_pts)  ! WORK Integrated complete Gamma function

REAL(KIND=real_jlslsm) ::                                                      &
  top_crit_soilt(land_pts,nsoilt,nzw),                                         &
                            ! WORK LOG(QBASE_MAX/QBASE) -see document.
  top_crit1z_soilt(land_pts,nsoilt),                                           &
                            ! WORK As above but for an individual zw.
  top_min_soilt(land_pts,nsoilt),                                              &
                            ! WORK value for when zw=zw_max.
  wutot_soilt(land_pts,nsoilt)           ! WORK Dummy (set to 1.0).

INTEGER :: errorstatus
CHARACTER(LEN=80) :: cmessage
CHARACTER(LEN=13) :: routinename


!-----------------------------------------------------------------------------
routinename='CALC_FIT_FSAT'

IF ( l_top ) THEN

  cfitmax = 0.15 * nfita

  ! Define the water table depths to be used in the fitting process:
  dzw = 1.0 / REAL(nzw) * zw_max
  DO iz = 1,nzw
    dumzw(iz) = REAL(iz-1) * dzw
  END DO

  ! Calculate total soil depth
  zdepth = SUM(dzsoil(1:sm_levels))

  ! Calculate Gamtot
!$OMP PARALLEL DO IF(nsoilt > 1) DEFAULT(NONE) SCHEDULE(STATIC)                &
!$OMP SHARED(nsoilt, toppdm, land_pts, soil_pts, ainfo, top_crit1z_soilt,      &
!$OMP        wutot_soilt, dumfsat_soilt, dumfwetl_soilt)                       &
!$OMP PRIVATE(m)
  DO m = 1,nsoilt
    wutot_soilt(:,m)    = 1.0
    dumfsat_soilt(:,m)  = 0.0
    dumfwetl_soilt(:,m) = 0.0

    toppdm%gamtot_soilt(:,m) = 0.0
    CALL calc_fsat( land_pts, soil_pts, .TRUE., ainfo%soil_index,              &
                  toppdm%ti_mean_soilt(:,m), toppdm%ti_sig_soilt(:,m),         &
                  top_crit1z_soilt(:,m), wutot_soilt(:,m),                     &
                  toppdm%gamtot_soilt(:,m),                                    &
                  dumfsat_soilt(:,m), dumfwetl_soilt(:,m))
  END DO
!$OMP END PARALLEL DO

  ! Calculate top_crit for the water table depths:
!$OMP PARALLEL IF(nsoilt > 1) DEFAULT(NONE)                                    &
!$OMP SHARED(nsoilt, soil_pts, ainfo, fsat_calc_soilt, fwet_calc_soilt, dumzw, &
!$OMP       top_crit_soilt, toppdm, top_min_soilt, zw_max, zdepth, land_pts,   &
!$OMP       top_crit1z_soilt, wutot_soilt, dumfsat_soilt, dumfwetl_soilt, dzw) &
!$OMP PRIVATE(i, j, m, iz)
  DO iz = 1, nzw
!$OMP DO SCHEDULE(STATIC)
    DO m = 1, nsoilt
      DO j = 1, soil_pts
        i = ainfo%soil_index(j)

        fsat_calc_soilt(i,m,iz) = 0.0
        fwet_calc_soilt(i,m,iz) = 0.0
        top_crit_soilt(i,m,iz)  = 0.0

        IF ( toppdm%ti_mean_soilt(i,m) > 0.0 .AND.                             &
             toppdm%ti_sig_soilt(i,m) > 0.0 ) THEN
          top_min_soilt(i,m) = 1.0 / toppdm%fexp_soilt(i,m)                    &
                              * EXP(-toppdm%fexp_soilt(i,m) * (zw_max - zdepth))

          IF ( dumzw(iz) <= zdepth ) THEN
            top_crit1z_soilt(i,m) = LOG(zdepth+1.0                             &
                               / toppdm%fexp_soilt(i,m) - top_min_soilt(i,m))  &
                               - LOG(zdepth - dumzw(iz) + 1.0                  &
                               / toppdm%fexp_soilt(i,m) - top_min_soilt(i,m))
          ELSE
            top_crit1z_soilt(i,m) = LOG(zdepth+1.0                             &
                                    / toppdm%fexp_soilt(i,m)                   &
                                    - top_min_soilt(i,m))                      &
                                    - LOG(1 / toppdm%fexp_soilt(i,m)           &
                                    * EXP(-toppdm%fexp_soilt(i,m)              &
                                    * (dumzw(iz) - zdepth))                    &
                                    - top_min_soilt(i,m))
          END IF
        END IF
      END DO !soil_pts

      ! Calculate FSAT and FWET for one ZW at all soil land_pts:
      CALL calc_fsat( land_pts, soil_pts, .FALSE., ainfo%soil_index,           &
                      toppdm%ti_mean_soilt(:,m), toppdm%ti_sig_soilt(:,m),     &
                      top_crit1z_soilt(:,m), wutot_soilt(:,m),                 &
                      toppdm%gamtot_soilt(:,m),                                &
                      dumfsat_soilt(:,m), dumfwetl_soilt(:,m) )

      DO j = 1,soil_pts
        i = ainfo%soil_index(j)

        IF ( toppdm%ti_mean_soilt(i,m) > 0.0 .AND.                             &
             toppdm%ti_sig_soilt(i,m) > 0.0 ) THEN
          fsat_calc_soilt(i,m,iz) = dumfsat_soilt(i,m)
          fwet_calc_soilt(i,m,iz) = dumfwetl_soilt(i,m)
          top_crit_soilt(i,m,iz)  = top_crit1z_soilt(i,m)

          IF ( dumzw(iz) <  dzw ) THEN ! Values at zw=0m
            toppdm%a_fsat_soilt(i,m) = fsat_calc_soilt(i,m,iz)
            toppdm%a_fwet_soilt(i,m) = fwet_calc_soilt(i,m,iz)
          END IF
        END IF
      END DO
    END DO !nsoilt
!$OMP END DO NOWAIT
  END DO                  !ZW calc_fsat loop
!$OMP END PARALLEL

  ! Now carry out fit for FSAT, where FSAT=function(ZW). (Likewise FWET)
  DO m = 1, nsoilt
    DO j = 1,soil_pts
      i = ainfo%soil_index(j)

      IF ( toppdm%ti_mean_soilt(i,m) > 0.0 .AND.                               &
           toppdm%ti_sig_soilt(i,m) > 0.0 ) THEN
        rmsold  = 1.0e10
        rmswold = 1.0e10

        DO ifita = 1,nfita
          cfit = cfitmax * (ifita) / REAL(nfita)

          rms  = 0.0
          rmsw = 0.0
          ! top_crit=TI_MAX when zw=zw_max
          DO iz = 1,nzw
            fsat_fit(iz) = toppdm%a_fsat_soilt(i,m)                            &
                            * EXP(-cfit * top_crit_soilt(i,m,iz))
            fwet_fit(iz) = toppdm%a_fwet_soilt(i,m)                            &
                            * EXP(-cfit * top_crit_soilt(i,m,iz))
            rms  = rms  + (fsat_calc_soilt(i,m,iz) - fsat_fit(iz))**2
            rmsw = rmsw + (fwet_calc_soilt(i,m,iz) - fwet_fit(iz))**2
          END DO         !ZW

          rms  = SQRT(rms) / REAL(nzw)
          rmsw = SQRT(rmsw) / REAL(nzw)

          IF ( rms < rmsold ) THEN
            rmsold = rms
            toppdm%c_fsat_soilt(i,m) = cfit
          END IF
          IF ( rmsw < rmswold ) THEN
            rmswold = rmsw
            toppdm%c_fwet_soilt(i,m) = cfit
          END IF
        END DO

        DO iz = 1,nzw
          fsat_fit(iz) = toppdm%a_fsat_soilt(i,m) *                            &
                         EXP(-toppdm%c_fsat_soilt(i,m) *                       &
                         top_crit_soilt(i,m,iz))
          fwet_fit(iz) = toppdm%a_fwet_soilt(i,m) *                            &
                         EXP(-toppdm%c_fwet_soilt(i,m) *                       &
                         top_crit_soilt(i,m,iz))
        END DO            !ZW

        IF ( rmsold >= thr_err ) THEN
          IF ( toppdm%c_fsat_soilt(i,m) <= cfitmin .OR.                        &
              toppdm%c_fsat_soilt(i,m) >= cfitmax ) THEN

            WRITE(jules_message,*) 'ERROR cfit FSAT', i,                       &
                toppdm%c_fsat_soilt(i,m), cfitmin, cfitmax
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*)                                             &
              'If c_fsat=cfitmax try increasing nfita'
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) 'fsat_calc=',                               &
                fsat_calc_soilt(i,m,1), fsat_calc_soilt(i,m,3),                &
                fsat_calc_soilt(i,m,5)
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) 'fsat_fit=',                                &
                fsat_fit(1), fsat_fit(3), fsat_fit(5)
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) 'rms=', rmsold
            CALL jules_print('calc_fit_fsat',jules_message)
            ErrorStatus = 35
            WRITE(CMessage, *) 'Error in cfit FSAT in LSH model setup'
          END IF
        END IF

        IF ( rmswold >= thr_err ) THEN
          IF ( toppdm%c_fwet_soilt(i,m) <= cfitmin .OR.                        &
              toppdm%c_fwet_soilt(i,m) >= cfitmax ) THEN

            WRITE(jules_message,*) 'ERROR cfit FWET', i,                       &
                toppdm%c_fwet_soilt(i,m), cfitmin, cfitmax
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) 'fwet_calc=',                               &
                fwet_calc_soilt(i,m,1), fwet_calc_soilt(i,m,3),                &
                fwet_calc_soilt(i,m,5)
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) 'fwet_fit=',                                &
                fwet_fit(1), fwet_fit(3), fwet_fit(5)
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) 'rmsw=', rmswold
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) '(fsat_calc=)',                             &
                fsat_calc_soilt(i,m,1), fsat_calc_soilt(i,m,3),                &
                fsat_calc_soilt(i,m,5)
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) '(fsat_fit=)',                              &
                fsat_fit(1), fsat_fit(3), fsat_fit(5)
            CALL jules_print('calc_fit_fsat',jules_message)
            WRITE(jules_message,*) '(rms=)', rmsold
            CALL jules_print('calc_fit_fsat',jules_message)
            ErrorStatus = 35
            ErrorStatus = 40
            WRITE(CMessage,*) 'Error in cfit FWET in LSH model setup'
          END IF
        END IF

        IF ( rmsold > thr_err ) THEN
          WRITE(jules_message,*) 'Warning LSH rms Error in fit:',              &
              rmsold, rmswold
          CALL jules_print('calc_fit_fsat',jules_message)
        END IF
      END IF

    END DO                  ! land_pts
  END DO !nsoilt
ELSE
  DO m = 1,nsoilt
    DO j = 1,soil_pts
      i = ainfo%soil_index(j)

      toppdm%a_fsat_soilt(i,m) = 0.0
      toppdm%c_fsat_soilt(i,m) = 0.0
      toppdm%a_fwet_soilt(i,m) = 0.0
      toppdm%c_fwet_soilt(i,m) = 0.0
      toppdm%gamtot_soilt(i,m) = 0.0
    END DO
  END DO
END IF

RETURN

END SUBROUTINE calc_fit_fsat
END MODULE calc_fit_fsat_mod
#endif
