#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ts_seek_to_datetime(FILE, dt)

USE io_constants, ONLY: mode_read, mode_write

USE precision_mod, ONLY: int_64

USE datetime_utils_mod, ONLY: days_in_month

USE datetime_mod, ONLY: period_day, period_month, period_year, l_360, l_leap,  &
                        datetime_create, datetime_clone, datetime_advance,     &
                        datetime_diff, datetime_to_string

USE templating_mod, ONLY: tpl_substitute_datetime

USE file_gridded_mod, ONLY: file_gridded_seek

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Seeks the file_ts to a particular datetime (i.e. the next time values are
!   read from the file_ts using file_ts_read_var, they will be read at
!   the requested datetime)
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ts), INTENT(IN OUT) :: FILE  ! The file to seek
TYPE(datetime), INTENT(IN) :: dt        ! The datetime to seek to


! Work variables
TYPE(datetime) :: dt_local  ! Local version of dt that is possibly modified
                            ! to deal with climatology

CHARACTER(LEN=max_file_name_len) :: file_name ! The name of the file
                                              ! containing the requested
                                              ! timestep
TYPE(datetime) :: file_start, next_file_start ! The start times of the file
                                              ! containing the requested
                                              ! timestep and the next file
INTEGER :: file_index                         ! The index of the file
                                              ! containing the requested
                                              ! timestep in the files
                                              ! array on the file object
INTEGER :: tsteps_from_start                  ! How far into the file is
INTEGER(KIND=int_64) :: secs_from_start      ! the requested timestep?

INTEGER :: i  ! Loop counters


!-----------------------------------------------------------------------------

! If file is still in define mode, error out
IF ( FILE%define_mode )                                                        &
  CALL log_fatal("file_ts_seek_to_datetime",                                   &
                 "Cannot seek while file is in define mode")

!-----------------------------------------------------------------------------
! Arbitrary seeks are only allowed in read mode
! Only seeking to the beginning of the data is permitted in write mode
!-----------------------------------------------------------------------------
IF ( FILE%mode /= mode_read .AND. dt /= FILE%data_start )                      &
  CALL log_fatal("file_ts_seek_to_datetime",                                   &
                 "Seeking to arbitrary datetime is only allowed in " //        &
                 "read mode")

!-----------------------------------------------------------------------------
! If we are using a climatology, create a new datetime to seek to where
! the year is the same as the year that the data runs for
! Otherwise we seek to the supplied datetime
!-----------------------------------------------------------------------------
dt_local = datetime_clone(dt)
IF ( FILE%is_climatology ) THEN
  dt_local%year = FILE%data_start%year
  ! We might need to adjust the day - if dt is in a leap year but the year we
  ! have data for is not a leap year and we are seekeing to somewhere in Feb 29
  dt_local%day = MIN(                                                          &
    dt_local%day, days_in_month(dt_local%year, dt_local%month, l_360, l_leap)  &
  )
END IF

!-----------------------------------------------------------------------------
! Check that the datetime to seek to is appropriate for special periods
!-----------------------------------------------------------------------------
! Monthly data is assumed to apply at 00:00:00 on the first of the month
! Annual data adds the additional constraint that the month must be January.
! Note there is no consideration of period_day as that is not allowed as a
! data period (use 86400 instead).
IF ( FILE%data_period == period_month .OR.                                     &
     FILE%data_period == period_year       ) THEN

  IF ( dt_local%TIME /= 0 .OR. dt_local%day /= 1 )                             &
    CALL log_fatal("file_ts_seek_to_datetime",                                 &
                   "Cannot seek to datetime - data_period=" //                 &
                   TRIM(to_string(FILE%data_period)) //                        &
                   " assumes data apply at 00:00:00 on 1st of month " //       &
                   "(given " // datetime_to_string(dt) // ")")

  IF ( FILE%data_period == period_year .AND. dt_local%month /= 1 )             &
    CALL log_fatal("file_ts_seek_to_datetime",                                 &
                   "Cannot seek to datetime - data_period=" //                 &
                   TRIM(to_string(FILE%data_period)) //                        &
                   " assumes data apply at 00:00:00 on 1st of January " //     &
                   "(given " // datetime_to_string(dt) // ")")

END IF

!-----------------------------------------------------------------------------
! Check that it is actually possible to seek to the requested datetime
!-----------------------------------------------------------------------------
IF ( dt_local < FILE%data_start .OR. FILE%data_end <= dt_local )               &
  CALL log_fatal("file_ts_seek_to_datetime",                                   &
                 "No data for datetime - out of range")

!-----------------------------------------------------------------------------
! We need to get the name and start time of the file containing the datetime
! that has been asked for
! This is done differently depending on whether we are using time templating
! or not
!-----------------------------------------------------------------------------
! Initialise file_index - note that this is never used when time templating
! is in use
file_index = 0

IF ( FILE%use_time_template ) THEN
  ! Time templating is in use
  ! We already know that the requested datetime is within the data range, so
  ! get the name of the file to open by substituting values from the given
  ! datetime into the template
  file_name = tpl_substitute_datetime(FILE%template, dt_local)

  ! Get the start and end time for the file based on the constraints imposed
  ! by the templating period being used
  SELECT CASE ( FILE%tpl_period )

  CASE ( period_day )
    ! The first data in a daily file must apply at 00:00:00 on that day.
    file_start = datetime_create(dt_local%year, dt_local%month, dt_local%day,  &
                                 0, 0, 0)

  CASE ( period_month )
    ! The first data in a monthly file must apply at 00:00:00 on the 1st of the
    ! month
    file_start = datetime_create(dt_local%year, dt_local%month, 1, 0, 0, 0)

  CASE ( period_year )
    ! The first data in a yearly file must apply at 00:00:00 on the 1st of Jan
    file_start = datetime_create(dt_local%year, 1, 1, 0, 0, 0)

  CASE DEFAULT
    CALL log_fatal("file_ts_seek_to_datetime",                                 &
                   "No code for tpl_period=" //                                &
                   TRIM(to_string(FILE%tpl_period)))
  END SELECT

  ! In all cases, the start of the next file is one templating period on from
  ! the start of the file we are opening
  next_file_start = datetime_advance(file_start, FILE%tpl_period)
ELSE
  ! No time templating - use the list of files instead

  ! Assume we are in the last file unless we discover otherwise while scanning
  ! below
  file_index = FILE%nfiles

  ! Get the index of the file containing the correct date and time
  DO i = 1,FILE%nfiles-1
    IF ( FILE%file_times(i) <= dt_local .AND.                                  &
         dt_local < FILE%file_times(i+1)     ) THEN
      file_index = i
      EXIT
    END IF
  END DO

  file_name  = FILE%files(file_index)
  file_start = FILE%file_times(file_index)

  ! Get the start time for the next file
  ! If we are in the last file (i.e. there is no next file), then set this to
  ! 1 second past the end of the data so it never gets hit
  IF ( file_index == FILE%nfiles) THEN
    next_file_start = datetime_advance(FILE%data_end, 1)
  ELSE
    next_file_start = FILE%file_times(file_index+1)
  END IF
END IF

!-----------------------------------------------------------------------------
! Calculate where in the new file we need to seek to.
! Note there is no consideration of period_day as that is not allowed as a
! data period (use 86400 instead).
!-----------------------------------------------------------------------------
SELECT CASE ( FILE%data_period )
CASE ( 1: )
  ! Any positive number is interpreted as a period in seconds

  !-----------------------------------------------------------------------------
  ! Get the timestep in the file that we want to seek to
  !-----------------------------------------------------------------------------
  secs_from_start = datetime_diff(file_start, dt_local)

  ! Check that the requested time is a whole number of timesteps into the file
  IF ( MOD(secs_from_start, INT(FILE%data_period, int_64)) /= 0 )              &
    CALL log_fatal("file_ts_seek_to_datetime",                                 &
                   "Cannot seek to datetime " //                               &
                   datetime_to_string(dt) //                                   &
                   " - it is not a valid timestep in file " //                 &
                   TRIM(file_name))

  ! Integer division gives us the number of timesteps into the file that we
  ! need for the requested time
  ! To avoid cluttering the code with INT calls, we rely on implicit conversion
  ! betweeen int64 and the default integer kind to do the right thing, i.e.
  !    file%data_period => int64
  !    division in 64-bit,
  !    result => default real kind
  ! For gfortran, we can confirm this using -Wconversion
  ! For intel, results confirm that this is what is happening
  tsteps_from_start = ( secs_from_start / FILE%data_period ) + 1

CASE ( period_month )
  ! Monthly data is only allowed when every file starts at 00:00:00 on the 1st
  ! of some month - when using a list of files this is verified in
  ! file_ts_open, and when using time templating it is true by construction
  ! This means we can calculate the timestep in the file to use just by
  ! comparing the year and month of file_start and dt
  tsteps_from_start = ( ( dt_local%year - file_start%year ) * 12 )             &
                    + ( dt_local%month - file_start%month )                    &
                    + 1

CASE ( period_year )
  ! Yearly data is only allowed when there is one file for the whole span of
  ! the data and the data starts at 00:00:00 on 1st January for some year
  ! - this is verified in file_ts_open
  ! This means we can calculate the timestep in the file to use just by comparing
  ! the year of file_start and dt
  tsteps_from_start = ( dt_local%year - file_start%year ) + 1

CASE DEFAULT
  CALL log_fatal("file_ts_seek_to_datetime",                                   &
                 "No code for data_period = " //                               &
                 TRIM(to_string(FILE%data_period)))
END SELECT

!-----------------------------------------------------------------------------
! Open the file that we have found
!-----------------------------------------------------------------------------
CALL file_ts_internal_open_file(FILE, file_name)


! If we are in read mode, then seek the file to the correct record
! If we are in write mode, then we know (due to the check at the top of the
! file) that we are seeking to the start of the file(s) and so no further
! seeking is necessary
IF ( FILE%mode == mode_read)                                                   &
  CALL file_gridded_seek(FILE%open_file, tsteps_from_start)

! Update properties on the file_ts object
FILE%current_datetime = dt_local
FILE%open_file_index  = file_index
FILE%next_file_start  = next_file_start

! Indicate that the timestamp for this file timestep has not been written yet
FILE%timestamp_written = .FALSE.

RETURN

END SUBROUTINE file_ts_seek_to_datetime
#endif
