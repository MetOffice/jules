!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey and University of Bristol, 2024.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE wtrac_checks_jls_mod

!-------------------------------------------------------------------------------
! Description:
!   Compare the normal water tracer prognostics in JULES with the model water
!   fields, and output warning or abort model if difference is over specified
!   values.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!-------------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

#if defined(UM_JULES)
USE timestep_mod, ONLY: timestep_number
#else
USE model_time_mod, ONLY: timestep_number
#endif

IMPLICIT NONE

! Output warning if  max diff > max_diff_warning and
!           relative diff of max diff > max_rel_diff_warning
! Abort run if relative diff > max_rel_diff_abort

REAL(KIND=real_jlslsm), PARAMETER :: max_diff_warning = 1.0e-15
REAL(KIND=real_jlslsm), PARAMETER :: max_rel_diff_warning = 1.0e-13
REAL(KIND=real_jlslsm), PARAMETER :: max_rel_diff_abort   = 1.0e-12

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WTRAC_CHECKS_JLS_MOD'

CONTAINS

SUBROUTINE wtrac_checks_sno(land_pts, nsurft, nsmax, surft_pts, surft_index,   &
                            snow_surft, snow_grnd_surft, sice_surft,           &
                            sliq_surft,                                        &
                            snow_surft_wtrac, snow_grnd_surft_wtrac,           &
                            sice_surft_wtrac, sliq_surft_wtrac)

! Test snow prognostics

USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook


IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts
INTEGER, INTENT(IN) :: nsurft
INTEGER, INTENT(IN) :: nsmax

INTEGER, INTENT(IN) :: surft_pts(nsurft)
INTEGER, INTENT(IN) :: surft_index(land_pts,nsurft)   ! Index of tile points

! Water prognostics
REAL(KIND=real_jlslsm), INTENT(IN) :: snow_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: snow_grnd_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: sice_surft(land_pts,nsurft,nsmax)
REAL(KIND=real_jlslsm), INTENT(IN) :: sliq_surft(land_pts,nsurft,nsmax)

! Equivalent water tracer prognostics
REAL(KIND=real_jlslsm), INTENT(IN) :: snow_surft_wtrac(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: snow_grnd_surft_wtrac(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: sice_surft_wtrac(land_pts,nsurft,nsmax)
REAL(KIND=real_jlslsm), INTENT(IN) :: sliq_surft_wtrac(land_pts,nsurft,nsmax)

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CHECKS_SNO'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Canopy snow
CALL wtrac_checks_tiles(land_pts, nsurft, 1, surft_pts, surft_index,           &
                            snow_surft, snow_surft_wtrac, 'snow_surft')

! Ground snow
CALL wtrac_checks_tiles(land_pts, nsurft, 1, surft_pts, surft_index,           &
                    snow_grnd_surft, snow_grnd_surft_wtrac, 'snow_grnd_surft')

! Snow layer ice mass
CALL wtrac_checks_tiles(land_pts, nsurft, nsmax, surft_pts, surft_index,       &
                            sice_surft, sice_surft_wtrac, 'sice_surft')

! Snow layer liquid mass
CALL wtrac_checks_tiles(land_pts, nsurft, nsmax, surft_pts, surft_index,       &
                            sliq_surft, sliq_surft_wtrac, 'sliq_surft')

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_checks_sno

! ---------------------------------------------------------------------------

SUBROUTINE wtrac_checks_hyd(land_pts, nsurft, nsoilt, sm_levels, soil_pts,     &
                            soil_index, surft_pts, surft_index,                &
                            canopy_surft, sthu_soilt,                          &
                            sthf_soilt, smcl_soilt, sthzw_soilt,               &
                            canopy_surft_wtrac, sthu_soilt_wtrac,              &
                            sthf_soilt_wtrac, smcl_soilt_wtrac,                &
                            sthzw_soilt_wtrac)


! Test hydrology prognostics
! (Note, this code is assuming that nsoilt=1.)

USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook


IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts
INTEGER, INTENT(IN) :: nsurft
INTEGER, INTENT(IN) :: nsoilt
INTEGER, INTENT(IN) :: sm_levels

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
REAL(KIND=real_jlslsm), INTENT(IN) :: canopy_surft_wtrac(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthu_soilt_wtrac                         &
                                               (land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthf_soilt_wtrac                         &
                                               (land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smcl_soilt_wtrac                         &
                                               (land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthzw_soilt_wtrac(land_pts,nsoilt)


INTEGER :: soilt_pts(nsoilt)
INTEGER :: soilt_index(land_pts,nsoilt)


CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CHECKS_HYD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set up 'tiled' soil points index (assuming nsoilt=1) so that the routine
! wtrac_check_tiles can be used for testing soil fields
! (To be able to use nsoilt>1 in this routine, then these fields need to be
! passed in.)

soilt_pts(1)     = soil_pts
soilt_index(:,1) = soil_index(:)

! Canopy
CALL wtrac_checks_tiles(land_pts, nsurft, 1, surft_pts, surft_index,           &
                            canopy_surft, canopy_surft_wtrac, 'canopy_surft')

! Unfrozen soil as fraction of saturation
CALL wtrac_checks_tiles(land_pts, nsoilt, sm_levels, soilt_pts, soilt_index,   &
                            sthu_soilt, sthu_soilt_wtrac, 'sthu_soilt')

! Frozen soil as fraction of saturation
CALL wtrac_checks_tiles(land_pts, nsoilt, sm_levels, soilt_pts, soilt_index,   &
                            sthf_soilt, sthf_soilt_wtrac, 'sthf_soilt')

! Soil moisture
CALL wtrac_checks_tiles(land_pts, nsoilt, sm_levels, soilt_pts, soilt_index,   &
                            smcl_soilt, smcl_soilt_wtrac, 'smcl_soilt')

! Soil moisture fraction in deep-zw layer
CALL wtrac_checks_tiles(land_pts, nsoilt, 1, soilt_pts, soilt_index,           &
                            sthzw_soilt, sthzw_soilt_wtrac, 'sthzw_soilt')


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE wtrac_checks_hyd

! ---------------------------------------------------------------------------

SUBROUTINE wtrac_checks_tiles(land_pts, nsurft, nlayer, surft_pts,             &
                              surft_index, q, q_wtrac, info_txt)

!
! Description:
!   Compare the normal water tracer to the water field for a single source
!   and output the largest error if larger than a specified relative value.
!   Also abort model if error is too large.
!

USE Ereport_mod,             ONLY: ereport
USE jules_print_mgr,         ONLY: jules_message, jules_print, newline

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts
INTEGER, INTENT(IN) :: nsurft
INTEGER, INTENT(IN) :: nlayer

INTEGER, INTENT(IN) :: surft_pts(nsurft)
INTEGER, INTENT(IN) :: surft_index(land_pts,nsurft)   ! Index of tile points

! Water field:
REAL(KIND=real_jlslsm), INTENT(IN) :: q(land_pts,nsurft,nlayer)

! Water tracer field:
REAL(KIND=real_jlslsm), INTENT(IN) :: q_wtrac(land_pts,nsurft,nlayer)

CHARACTER(LEN=*), INTENT(IN) :: info_txt      ! Text for error message


! Local

INTEGER :: i, n, k, ilyr                      ! Loop counters
INTEGER :: nmax_q, imax_q, kmax_q             ! Location of largest q error
INTEGER :: ilyrmax_q, ilyrmax_q_out           ! Layer location of largest error

INTEGER :: ErrorStatus        ! Error code

REAL(KIND=real_jlslsm) :: diff_q
REAL(KIND=real_jlslsm) :: largest_error_q
REAL(KIND=real_jlslsm) :: q_error

CHARACTER(LEN=256) :: message

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CHECKS_TILES'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

largest_error_q   = 0.0

! --------------------------------------------------------------------------
! Find largest error
! --------------------------------------------------------------------------

DO n = 1,nsurft
  DO k = 1,surft_pts(n)
    IF (surft_pts(n) > 0) THEN
      i = surft_index(k,n)

      DO ilyr = 1, nlayer
        diff_q  = q_wtrac(i,n,ilyr) - q(i,n,ilyr)

        IF ( ABS(diff_q) > ABS(largest_error_q) ) THEN
          IF (ABS(q(i,n,ilyr)) > 0.0) THEN
            largest_error_q = diff_q
            q_error         = q(i,n,ilyr)
          ELSE
            largest_error_q = diff_q
            q_error         = q_wtrac(i,n,ilyr)
          END IF
          nmax_q = n
          kmax_q = k
          imax_q = i
          ilyrmax_q = ilyr
        END IF
      END DO
    END IF
  END DO
END DO

! --------------------------------------------------------------------------
! Print out large relative errors if greater than specified values
! --------------------------------------------------------------------------

IF (ABS(largest_error_q) > max_diff_warning .AND.                              &
    ABS(largest_error_q/q_error) > max_rel_diff_warning) THEN

  IF (nlayer > 1) THEN
    ilyrmax_q_out = ilyrmax_q
  ELSE
    ! If the field doesn't contain layers, set output value to 0 reduce
    ! confusion
    ilyrmax_q_out = 0
  END IF

  WRITE(jules_message,'(3a,4g20.12,a,5(i0,1x))')                               &
  newline// '*********WATER TRACER PROBLEM IN JULES*********' //newline//      &
  'Large relative difference between tracer and water for ',info_txt,          &
  newline//                                                                    &
  ' q, q_wtrac, max diff, frac diff = ' //newline,                             &
  q(imax_q,nmax_q,ilyrmax_q),q_wtrac(imax_q,nmax_q,ilyrmax_q),                 &
  largest_error_q, largest_error_q/q_error,                                    &
  newline// ' at n,i,k,layer,it=', nmax_q,imax_q,kmax_q,ilyrmax_q_out,         &
  timestep_number

  CALL jules_print(RoutineName, jules_message)

  IF (ABS(largest_error_q/q_error) > max_rel_diff_abort) THEN
    ! Abort model
    ErrorStatus = 100
    message = 'Problem with water tracers in JULES.'
  ELSE
    ! Output warning
    ErrorStatus = -100
    message = 'Problem with water tracers in JULES.'
  END IF

  CALL Ereport(RoutineName, ErrorStatus, message)

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE wtrac_checks_tiles

! ---------------------------------------------------------------------------

SUBROUTINE wtrac_checks_grid(row_length, rows, q, q_wtrac, info_txt)
!
! Description:
!   Compare the normal water tracer to the water field for river storage
!   and output the largest error if larger than a specified relative value.
!   Also abort model if error is too large.
!

USE Ereport_mod,             ONLY: ereport
USE jules_print_mgr,         ONLY: jules_message, jules_print, newline

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

INTEGER, INTENT(IN) :: row_length
INTEGER, INTENT(IN) :: rows

! Water field:
REAL(KIND=real_jlslsm), INTENT(IN) :: q(row_length,rows)

! Water tracer field:
REAL(KIND=real_jlslsm), INTENT(IN) :: q_wtrac(row_length,rows)

CHARACTER(LEN=*), INTENT(IN) :: info_txt      ! Text for error message


! Local

INTEGER :: i, j                               ! Loop counters
INTEGER :: imax_q, jmax_q                     ! Location of largest q error

INTEGER :: ErrorStatus        ! Error code

REAL(KIND=real_jlslsm) :: diff_q
REAL(KIND=real_jlslsm) :: largest_error_q
REAL(KIND=real_jlslsm) :: q_error

CHARACTER(LEN=256) :: message

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_CHECKS_GRID'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

largest_error_q   = 0.0

! --------------------------------------------------------------------------
! Find largest error
! --------------------------------------------------------------------------

DO j = 1, rows
  DO i = 1, row_length
    diff_q  = q_wtrac(i,j) - q(i,j)

    IF ( ABS(diff_q) > ABS(largest_error_q) ) THEN
      IF (ABS(q(i,j)) > 0.0) THEN
        largest_error_q = diff_q
        q_error         = q(i,j)
      ELSE
        largest_error_q = diff_q
        q_error         = q_wtrac(i,j)
      END IF
      imax_q = i
      jmax_q = j
    END IF
  END DO
END DO

! --------------------------------------------------------------------------
! Print out large relative errors if greater than specified values
! --------------------------------------------------------------------------

IF (ABS(largest_error_q) > max_diff_warning .AND.                              &
    ABS(largest_error_q/q_error) > max_rel_diff_warning) THEN

  WRITE(jules_message,'(3a,4g20.12,a,3(i0,1x))')                               &
  newline// '*********WATER TRACER PROBLEM IN JULES*********' //newline//      &
  'Large relative difference between tracer and water for ',info_txt,          &
  newline//                                                                    &
  ' q, q_wtrac, max diff, frac diff = ' //newline,                             &
  q(imax_q,jmax_q),q_wtrac(imax_q,jmax_q),                                     &
  largest_error_q, largest_error_q/q_error,                                    &
  newline// ' at i,j,it=', imax_q,jmax_q,timestep_number

  CALL jules_print(RoutineName, jules_message)

  IF (ABS(largest_error_q/q_error) > max_rel_diff_abort) THEN
    ! Abort model
    ErrorStatus = 100
    message = 'Problem with water tracers in JULES.'
  ELSE
    ! Output warning
    ErrorStatus = -100
    message = 'Problem with water tracers in JULES.'
  END IF

  CALL Ereport(RoutineName, ErrorStatus, message)

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE wtrac_checks_grid

END MODULE wtrac_checks_jls_mod




