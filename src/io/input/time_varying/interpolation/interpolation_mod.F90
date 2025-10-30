#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE interpolation_mod

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
CHARACTER(LEN=2), PARAMETER ::                                                 &
  interp_ave_backward = 'b',                                                   &
     ! backward time average, i.e. time average ending at given time
     ! (in GSWP2 this is L)
  interp_ave_centred = 'c',                                                    &
     ! centred time average, i.e. time average centred on given time
     ! (GSWP2 C)
  interp_ave_forward = 'f',                                                    &
     ! forward time average, i.e. time average starting at given time
     ! (GSWP2 N)
  interp_instant = 'i',                                                        &
     ! instantaneous value at given time (interpolation will be
     ! linear in time)(GSWP2 I)
  no_interp_end = 'nb',                                                        &
     ! no interpolation, value is valid over time interval ending
     ! at given time (not in GSWP2)
  no_interp_centred = 'nc',                                                    &
     ! no interpolation, value is valid over time interval centred
     ! on given time  (GSWP2 0)
  no_interp_start = 'nf'
     ! no interpolation, value is valid over time interval starting
     ! at given time (GSWP2 default)

! Note that interpolation for b,c and f will generate values that, generally,
! lie outside the range of the input averages


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC get_required_time_bounds, interpolate


CONTAINS


! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.

SUBROUTINE get_required_time_bounds(interp_flags, lower_bound, upper_bound)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given a list of interpolation flags, returns the common lower bound and
!   upper bound required to use all the given interpolation schemes in the
!   same file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: interp_flags(:)

! Return types
INTEGER, INTENT(OUT) :: lower_bound, upper_bound


! Work variables
LOGICAL :: required_times(-1:2)  ! Array indicating the required times
INTEGER :: i  ! Loop variable

!-----------------------------------------------------------------------------

required_times(:) = .FALSE.

DO i = 1,SIZE(interp_flags)

  ! If all the times are already required, we can exit early
  IF ( ALL(required_times) ) EXIT

  ! Otherwise, set the required times based on the interpolation flag
  SELECT CASE ( interp_flags(i) )
  CASE ( interp_ave_backward )
    required_times(0:2) = .TRUE.

  CASE ( interp_ave_centred )
    required_times(-1:2) = .TRUE.

  CASE ( interp_ave_forward )
    required_times(-1:1) = .TRUE.

  CASE ( interp_instant )
    required_times(0:1) = .TRUE.

  CASE ( no_interp_end )
    required_times(1) = .TRUE.

  CASE ( no_interp_centred )
    required_times(0:1) = .TRUE.

  CASE ( no_interp_start )
    required_times(0) = .TRUE.

  CASE DEFAULT
    CALL log_fatal("get_required_times",                                       &
                   "Unrecognised interpolation flag - " // interp_flags(i))
  END SELECT

END DO

! The required lower bound is the first element for which required_times=T
DO i = -1,2
  IF ( required_times(i) ) THEN
    lower_bound = i
    EXIT
  END IF
END DO

! The required upper bound is the last element for which required_times=T
! So step backwards through the required_times array to detect it
DO i = 2,-1,-1
  IF ( required_times(i) ) THEN
    upper_bound = i
    EXIT
  END IF
END DO

RETURN

END SUBROUTINE get_required_time_bounds

FUNCTION interpolate(DATA, interp_flag, tsteps_in_period, tstep)               &
                                                     RESULT(interpolated_data)

USE data_cube_mod, ONLY: data_cube,                                            &
                          OPERATOR ( * ), OPERATOR ( + ), OPERATOR ( / ),      &
                          cube_safe_copy, cube_free

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns data interpolated to the current time as a data cube
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
#if defined(SUN_FORTRAN)
TYPE(data_cube), POINTER :: DATA(:)
#else
TYPE(data_cube), POINTER, INTENT(IN) :: DATA(:)
#endif
                    ! The data to interpolate, a data_cube for each time
                    ! Marked as a pointer to preserve array indexing
                    ! The SUN compiler, for whatever reason, doesn't like
                    ! the INTENT and POINTER attributes together, so we
                    ! remove the INTENT for it
CHARACTER(LEN=*), INTENT(IN) :: interp_flag
                    ! The interpolation scheme to use
INTEGER, INTENT(IN) :: tsteps_in_period(-1:1)
                    ! The number of timesteps in the interpolated data
                    ! for each data timestep
                    ! I.e. tsteps_in_period(-1) is the number of interpolated
                    !      (i.e. model) timesteps between data(-1) and data(0)
                    !      tsteps_in_period(0) is the number of interpolated
                    !      timesteps between data(0) and data(1)
                    ! etc...
INTEGER, INTENT(IN) :: tstep
                    ! The interpolated (i.e. model) timestep that we want data for
                    ! This is a number from 0 to (tsteps_in_period(0) - 1)


! Return type
TYPE(data_cube) :: interpolated_data
                    ! The interpolated data for the current time

! Work variables
REAL :: weights(-1:2)  ! The weights for each time

! Used in CASE ( INTERP_AVE_BACKWARD ) and CASE ( INTERP_AVE_FORWARD )
REAL :: n1, n2  ! The number of timesteps in the first and second intervals

! Used in CASE ( INTERP_AVE_CENTRED )
REAL :: n  ! The number of timesteps in the interval

! Used in all INTERP_AVE_* cases
TYPE(data_cube) :: weighted_data
TYPE(data_cube) :: numer
TYPE(data_cube) :: denom

! Temporary cubes used for preventing memory leaks
TYPE(data_cube) :: temp1, temp2, temp3, temp4

REAL :: t  ! Real version of tstep


!-----------------------------------------------------------------------------

! Reject values that we can't interpolate for - it only makes sense to give
! values for times between current and next data
IF ( tstep < 0 .OR. tsteps_in_period(0) <= tstep )                             &
  CALL log_fatal("interpolate",                                                &
                 "tstep to interpolate to is invalid for given data " //       &
                 "periods")


SELECT CASE ( interp_flag )
CASE ( interp_ave_backward )
  !-----------------------------------------------------------------------------
  ! Inputs are backward time averages, i.e. time average ending at given time
  !
  ! This is a slightly modified version of the scheme in JULES v3.0. It has
  ! been modified to attempt to provide sensible values for varying length
  ! data timesteps (e.g. monthly data) - the previous scheme was limited to
  ! fixed size data timesteps. This extension should be considered experimental.
  !
  ! In the case of fixed length timesteps, the new scheme reduces to exactly
  ! the scheme in JULES v3.0, and so can be safely used in this case.
  !-----------------------------------------------------------------------------
  n1 = REAL(tsteps_in_period(0))
  n2 = REAL(tsteps_in_period(1))
  t  = REAL(tstep)

  weights(1) = 1.0 - ( ABS(t + (n2 / n1) * t - n1 + 1.0) / (n1 + n2) )
  weights(0) = MAX(1.0 - ( (t + (n2 / n1) * t + n1 + 1.0) / (n1 + n2) ), 0.0)
  weights(2) = MAX(1.0 - ((n1+2.0 * n2 - t - (n2 / n1) * t - 1.0) / (n1 + n2)), 0.0)

  temp1 = DATA(0) * weights(0)
  temp2 = DATA(1) * weights(1)
  temp3 = temp1 + temp2
  temp4 = DATA(2) * weights(2)
  weighted_data = temp3 + temp4
  CALL cube_free(temp1)
  CALL cube_free(temp2)
  CALL cube_free(temp3)
  CALL cube_free(temp4)

  temp1 = DATA(0) + DATA(2)
  temp2 = temp1 * 0.5
  temp3 = DATA(1) * 3.0
  denom = temp2 + temp3
  CALL cube_free(temp1)
  CALL cube_free(temp2)
  CALL cube_free(temp3)

  numer = DATA(1) * 4.0

  temp1 = weighted_data * numer
  interpolated_data = temp1 / denom
  CALL cube_free(temp1)
  CALL cube_free(weighted_data)
  CALL cube_free(numer)
  CALL cube_free(denom)


CASE ( interp_ave_centred )
  !-----------------------------------------------------------------------------
  ! Inputs are centred time averages, i.e. time average centred on given time
  !-----------------------------------------------------------------------------
  n = REAL(tsteps_in_period(0))
  t = REAL(tstep)

  weights(0)  = 1.0 - ( (2.0 * t + 1.0) / (2.0 * n) )
  weights(1)  = 1.0 - weights(0)

  temp1 = DATA(0) * weights(0)
  temp2 = DATA(1) * weights(1)
  weighted_data = temp1 + temp2
  CALL cube_free(temp1)
  CALL cube_free(temp2)

  IF ( t > n/2 ) THEN
    temp1 = DATA(0) + DATA(2)
    temp2 = temp1 * 0.5
    temp3 = DATA(1) * 3.0
    denom = temp2 + temp3
    CALL cube_free(temp1)
    CALL cube_free(temp2)
    CALL cube_free(temp3)

    numer = DATA(1) * 4.0
  ELSE
    temp1 = DATA(-1) + DATA(1)
    temp2 = temp1 * 0.5
    temp3 = DATA(0) * 3.0
    denom = temp2 + temp3
    CALL cube_free(temp1)
    CALL cube_free(temp2)
    CALL cube_free(temp3)

    numer = DATA(0) * 4.0
  END IF

  temp1 = weighted_data * numer
  interpolated_data = temp1 / denom
  CALL cube_free(temp1)
  CALL cube_free(weighted_data)
  CALL cube_free(numer)
  CALL cube_free(denom)


CASE ( interp_ave_forward )
  !-----------------------------------------------------------------------------
  ! Inputs are forward time averages, i.e. time average starting at given time
  !
  ! This is a slightly modified version of the scheme in JULES v3.0. It has
  ! been modified to attempt to provide sensible values for varying length
  ! data timesteps (e.g. monthly data) - the previous scheme was limited to
  ! fixed size data timesteps. This extension should be considered experimental.
  !
  ! In the case of fixed length timesteps, the new scheme reduces to exactly
  ! the scheme in JULES v3.0, and so can be safely used in this case.
  !-----------------------------------------------------------------------------
  n1 = REAL(tsteps_in_period(-1))
  n2 = REAL(tsteps_in_period(0))
  t  = REAL(tstep)

  weights(0)  = 1.0 - ( ABS(t + (n1 / n2) * t - n1+1) / (n1 + n2) )
  weights(-1) = MAX(1.0 - ( (t + (n1 / n2) * t + n1+1) / (n1 + n2) ), 0.0)
  weights(1)  = MAX(1.0 - ((n1+2 * n2 - t - (n1 / n2) * t-1) / (n1 + n2)), 0.0)

  temp1 = DATA(-1) * weights(-1)
  temp2 = DATA(0) * weights(0)
  temp3 = temp1 + temp2
  temp4 = DATA(1) * weights(1)
  weighted_data = temp3 + temp4
  CALL cube_free(temp1)
  CALL cube_free(temp2)
  CALL cube_free(temp3)
  CALL cube_free(temp4)


  temp1 = DATA(-1) + DATA(1)
  temp2 = temp1 * 0.5
  temp3 = DATA(0) * 3.0
  denom = temp2 + temp3
  CALL cube_free(temp1)
  CALL cube_free(temp2)
  CALL cube_free(temp3)

  numer = DATA(0) * 4.0

  temp1 = weighted_data * numer
  interpolated_data = temp1 / denom
  CALL cube_free(temp1)
  CALL cube_free(weighted_data)
  CALL cube_free(numer)
  CALL cube_free(denom)


CASE ( interp_instant )
  !-----------------------------------------------------------------------------
  ! Inputs are instantaneous values at given times - interpolation is linear
  ! between times
  !-----------------------------------------------------------------------------
  ! Calculate the weights based on how far through the time period we are
  ! We use tstep + 1 because tstep is 0-indexed, and we want to end up with
  ! weights = [0, 1] when tstep = tsteps_in_period - 1
  weights(0) = REAL(tsteps_in_period(0) - (tstep+1)) / REAL(tsteps_in_period(0))
  weights(1) = 1.0 - weights(0)

  ! Calculate the data to return
  temp1 = DATA(0) * weights(0)
  temp2 = DATA(1) * weights(1)
  interpolated_data = temp1 + temp2
  CALL cube_free(temp1)
  CALL cube_free(temp2)


CASE ( no_interp_end )
  !-----------------------------------------------------------------------------
  ! No interpolation, (current) value is valid over time interval ending at
  ! given time
  !-----------------------------------------------------------------------------
  CALL cube_safe_copy(interpolated_data, DATA(1))


CASE ( no_interp_centred )
  !-----------------------------------------------------------------------------
  ! No interpolation, (current) value is valid over time interval centred on
  ! given time
  !-----------------------------------------------------------------------------
  IF ( tstep < (tsteps_in_period(0) / 2) ) THEN
    ! If we are closer to current data than next data, use the current data
    CALL cube_safe_copy(interpolated_data, DATA(0))
  ELSE
    ! Otherwise use the next data
    CALL cube_safe_copy(interpolated_data, DATA(1))
  END IF


CASE ( no_interp_start )
  !-----------------------------------------------------------------------------
  ! No interpolation, (current) value is valid over time interval starting at
  ! given time
  !-----------------------------------------------------------------------------
  CALL cube_safe_copy(interpolated_data, DATA(0))


CASE DEFAULT
  CALL log_fatal("interpolate",                                                &
                 "Unrecognised interpolation flag - " // interp_flag)
END SELECT

RETURN

END FUNCTION interpolate

END MODULE interpolation_mod
#endif
