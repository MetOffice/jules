!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey, 2024.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE wtrac_correct_jls_mod

!-------------------------------------------------------------------------------
! Description:
!   Correct water tracers in JULES land stores for small numerical errors
!   which can cause the water tracer to diverge from normal water.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!-------------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

USE jules_water_tracers_mod, ONLY: wtrac_calc_ratio_fn_jules

IMPLICIT NONE

! Remove water tracer if water or water tracer is less than min_value
REAL(KIND=real_jlslsm), PARAMETER :: min_value = 1.0e-20

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WTRAC_CORRECT_JLS_MOD'

CONTAINS

SUBROUTINE wtrac_correct_sno(land_pts, nsurft, nsmax, n_wtrac_jls,             &
                            surft_pts, surft_index,                            &
                            snow_surft, snow_grnd_surft, sice_surft,           &
                            sliq_surft,                                        &
                            snow_surft_wtrac, snow_grnd_surft_wtrac,           &
                            sice_surft_wtrac, sliq_surft_wtrac)

! Description:
!   Wrapper routine to correct snow stores.

USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook


IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts
INTEGER, INTENT(IN) :: nsurft
INTEGER, INTENT(IN) :: nsmax
INTEGER, INTENT(IN) :: n_wtrac_jls

INTEGER, INTENT(IN) :: surft_pts(nsurft)
INTEGER, INTENT(IN) :: surft_index(land_pts,nsurft)   ! Index of tile points

! Water prognostics
REAL(KIND=real_jlslsm), INTENT(IN) :: snow_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: snow_grnd_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: sice_surft(land_pts,nsurft,nsmax)
REAL(KIND=real_jlslsm), INTENT(IN) :: sliq_surft(land_pts,nsurft,nsmax)

! Equivalent water tracer prognostics
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: snow_surft_wtrac(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: snow_grnd_surft_wtrac                &
                                             (land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: sice_surft_wtrac                     &
                                             (land_pts,nsurft,nsmax)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: sliq_surft_wtrac                     &
                                             (land_pts,nsurft,nsmax)

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CORRECT_SNO'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Canopy snow
CALL wtrac_correct_tiles(land_pts, nsurft, 1, n_wtrac_jls, surft_pts,          &
                         surft_index, snow_surft, snow_surft_wtrac)

! Ground snow
CALL wtrac_correct_tiles(land_pts, nsurft, 1, n_wtrac_jls, surft_pts,          &
                         surft_index, snow_grnd_surft, snow_grnd_surft_wtrac)

! Snow layer ice mass
CALL wtrac_correct_tiles(land_pts, nsurft, nsmax, n_wtrac_jls, surft_pts,      &
                         surft_index, sice_surft, sice_surft_wtrac)

! Snow layer liquid mass
CALL wtrac_correct_tiles(land_pts, nsurft, nsmax, n_wtrac_jls, surft_pts,      &
                         surft_index, sliq_surft, sliq_surft_wtrac)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_correct_sno

! ---------------------------------------------------------------------------

SUBROUTINE wtrac_correct_hyd(land_pts, nsurft, nsoilt, sm_levels, n_wtrac_jls, &
                            soil_pts, soil_index, surft_pts, surft_index,      &
                            canopy_surft, sthu_soilt,                          &
                            sthf_soilt, smcl_soilt, sthzw_soilt,               &
                            canopy_surft_wtrac, sthu_soilt_wtrac,              &
                            sthf_soilt_wtrac, smcl_soilt_wtrac,                &
                            sthzw_soilt_wtrac)


! Description:
!  Wrapper routine to correct hydrological stores.
! (Note, this code is assuming that nsoilt=1.)

USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook


IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts
INTEGER, INTENT(IN) :: nsurft
INTEGER, INTENT(IN) :: nsoilt
INTEGER, INTENT(IN) :: sm_levels
INTEGER, INTENT(IN) :: n_wtrac_jls

INTEGER, INTENT(IN) :: soil_pts
INTEGER, INTENT(IN) :: soil_index(land_pts)
INTEGER, INTENT(IN) :: surft_pts(nsurft)
INTEGER, INTENT(IN) :: surft_index(land_pts,nsurft)   ! Index of tile points

! Water prognostics
REAL(KIND=real_jlslsm), INTENT(IN) :: canopy_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthu_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthf_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smcl_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthzw_soilt(land_pts,nsoilt)

! Equivalent water tracer prognostics
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: canopy_surft_wtrac                   &
                                        (land_pts,nsurft,n_wtrac_jls)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: sthu_soilt_wtrac                     &
                                        (land_pts,nsoilt,sm_levels,n_wtrac_jls)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: sthf_soilt_wtrac                     &
                                        (land_pts,nsoilt,sm_levels,n_wtrac_jls)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: smcl_soilt_wtrac                     &
                                        (land_pts,nsoilt,sm_levels,n_wtrac_jls)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: sthzw_soilt_wtrac                    &
                                        (land_pts,nsoilt,n_wtrac_jls)


INTEGER :: soilt_pts(nsoilt)
INTEGER :: soilt_index(land_pts,nsoilt)


CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CORRECT_HYD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set up 'tiled' soil points index (assuming nsoilt=1) so that the routine
! wtrac_correct_tiles can be used for testing soil fields
! (To be able to use nsoilt>1 in this routine, then these fields need to be
! passed in.)

soilt_pts(1)     = soil_pts
soilt_index(:,1) = soil_index(:)

! Canopy
CALL wtrac_correct_tiles(land_pts, nsurft, 1, n_wtrac_jls, surft_pts,          &
                         surft_index, canopy_surft, canopy_surft_wtrac)

! Unfrozen soil as fraction of saturation
CALL wtrac_correct_tiles(land_pts, nsoilt, sm_levels, n_wtrac_jls, soilt_pts,  &
                         soilt_index, sthu_soilt, sthu_soilt_wtrac)

! Frozen soil as fraction of saturation
CALL wtrac_correct_tiles(land_pts, nsoilt, sm_levels, n_wtrac_jls, soilt_pts,  &
                         soilt_index, sthf_soilt, sthf_soilt_wtrac)

! Soil moisture
CALL wtrac_correct_tiles(land_pts, nsoilt, sm_levels, n_wtrac_jls, soilt_pts,  &
                         soilt_index,  smcl_soilt, smcl_soilt_wtrac)

! Soil moisture fraction in deep-zw layer
CALL wtrac_correct_tiles(land_pts, nsoilt, 1, n_wtrac_jls, soilt_pts,          &
                         soilt_index, sthzw_soilt, sthzw_soilt_wtrac)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_correct_hyd

! ---------------------------------------------------------------------------

SUBROUTINE wtrac_correct_tiles(land_pts, nsurft, nlayer, n_wtrac_jls,          &
                               surft_pts, surft_index, q, q_wtrac)

!
! Description:
!   Generic routine to apply correction to tiled fields

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts
INTEGER, INTENT(IN) :: nsurft
INTEGER, INTENT(IN) :: nlayer
INTEGER, INTENT(IN) :: n_wtrac_jls

INTEGER, INTENT(IN) :: surft_pts(nsurft)
INTEGER, INTENT(IN) :: surft_index(land_pts,nsurft)   ! Index of tile points

! Water field:
REAL(KIND=real_jlslsm), INTENT(IN) :: q(land_pts,nsurft,nlayer)

! Water tracer field:
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: q_wtrac                              &
                                          (land_pts,nsurft,nlayer,n_wtrac_jls)

! Local

INTEGER :: i, n, k, ilyr, i_wt             ! Loop counters

REAL(KIND=real_jlslsm) :: diff_q, ratio_q, q_wtrac_norm

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CORRECT_TILES'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(n,k,i,ilyr,i_wt,diff_q,q_wtrac_norm,ratio_q)                     &
!$OMP SHARED(nsurft,surft_pts,surft_index,nlayer,n_wtrac_jls,q,q_wtrac)
DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    IF (surft_pts(n) > 0) THEN
      i = surft_index(k,n)

      DO ilyr = 1, nlayer

        ! This check is currently not used due to negative snow values.
        ! Once the water tracers are coded for the new option of fixing
        ! negative snow values (i.e. l_fix_neg_snow=T), then this check
        ! should be reinstated
        ! IF (q(i,n,ilyr) > min_value) THEN
        ! If there is water present, then correct water tracers

        ! Find difference between normal water tracer and water
        diff_q  = q(i,n,ilyr) - q_wtrac(i,n,ilyr,1)

        ! Store normal water value for ratio calculation
        q_wtrac_norm = q_wtrac(i,n,ilyr,1)

        DO i_wt = 1, n_wtrac_jls
          ! Calculate ratios - use the normal water tracer for this
          ratio_q = wtrac_calc_ratio_fn_jules(i_wt, q_wtrac(i,n,ilyr,i_wt),    &
                                              q_wtrac_norm)

          ! Apply correction
          q_wtrac(i,n,ilyr,i_wt) = q_wtrac(i,n,ilyr,i_wt) + ratio_q*diff_q

          ! Ensure there are no very small values - ignore conservation here
          ! as dealing with such small values

          ! See note above as to why this code is commented out for now.
          !            IF (q_wtrac(i,n,ilyr,i_wt) < min_value) THEN
          !              q_wtrac(i,n,ilyr,i_wt) = 0.0
          !            END IF

        END DO
        ! ELSE

        ! If there is no water present, set water tracer fields to zero
        !          DO i_wt = 1, n_wtrac_jls
        !            q_wtrac(i,n,ilyr,i_wt) = 0.0
        !          END DO

        !        ENDIF

      END DO
    END IF
  END DO
!$OMP END DO NOWAIT
END DO
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE wtrac_correct_tiles

! ---------------------------------------------------------------------------

SUBROUTINE wtrac_correct_grid(row_length, rows, n_wtrac_jls, q, q_wtrac)
!
! Description:
!   Generic routine to apply correction to 2D gridded fields

USE missing_data_mod, ONLY: rmdi

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) :: row_length
INTEGER, INTENT(IN) :: rows
INTEGER, INTENT(IN) :: n_wtrac_jls

! Water field:
REAL(KIND=real_jlslsm), INTENT(IN) :: q(row_length,rows)

! Water tracer field:
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: q_wtrac(row_length,rows,n_wtrac_jls)


! Local

INTEGER :: i, j, i_wt                           ! Loop counters

REAL(KIND=real_jlslsm) :: diff_q, ratio_q, q_wtrac_norm

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CORRECT_GRID'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP PRIVATE(i,j,i_wt,diff_q,q_wtrac_norm,ratio_q)                            &
!$OMP SHARED(rows,row_length,n_wtrac_jls,q,q_wtrac)
DO j = 1, rows
  DO i = 1, row_length

    IF (q(i,j) /= rmdi) THEN
      ! (Note, twatstor contains rmdi values which shouldn't be corrected)

      IF (q(i,j) > min_value) THEN
        ! If there is water present, then correct water tracers

        ! Find difference between normal water tracer and water
        diff_q  = q(i,j) - q_wtrac(i,j,1)

        ! Store normal water value for ratio calculation
        q_wtrac_norm = q_wtrac(i,j,1)

        DO i_wt = 1, n_wtrac_jls
          ! Calculate ratios - use the normal water tracer for this
          ratio_q = wtrac_calc_ratio_fn_jules(i_wt, q_wtrac(i,j,i_wt),         &
                                              q_wtrac_norm)

          ! Apply correction
          q_wtrac(i,j,i_wt) = q_wtrac(i,j,i_wt) + ratio_q*diff_q

          ! Ensure there are no very small values - ignore conservation here
          ! as dealing with such small values
          IF (q_wtrac(i,j,i_wt) < min_value) THEN
            q_wtrac(i,j,i_wt) = 0.0
          END IF

        END DO
      ELSE

        ! If there is no water present, set water tracer fields to zero
        DO i_wt = 1, n_wtrac_jls
          q_wtrac(i,j,i_wt) = 0.0
        END DO
      END IF

    END IF ! q /= rmdi

  END DO
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE wtrac_correct_grid

END MODULE wtrac_correct_jls_mod




