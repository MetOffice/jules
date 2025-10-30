#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE seek_all_to_current_datetime()

USE precision_mod, ONLY: int_64

USE datetime_mod, ONLY: period_month, period_year,                             &
                         datetime_clone, datetime_diff,                        &
                         datetime_advance, datetime_subtract

USE model_time_mod, ONLY: current_time, timestep_len

USE file_ts_mod, ONLY: file_ts_seek_to_datetime, file_ts_read_var,             &
                        file_ts_advance

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets up the input files so that they are able to provide data for the
!   current timestep next time they are asked for data
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
TYPE(datetime) :: data_dt  ! The datetime for data that applies for
                           ! current_dt
                           ! See step 1 in algorithm description below
TYPE(datetime) :: seek_dt  ! The datetime to seek the underlying file to
                           ! See step 2 of algorithm below

INTEGER(KIND=int_64) :: diff_secs
    ! Used as return value from calls to datetime_diff (hence why an int64)

INTEGER :: time_into_period
    ! For the 1: case, this is the number of seconds into a data period

INTEGER :: i,j,k  ! Loop counters

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Overview of algorithm (for each file):
!
!   1. Find the data time for the current datetime (i.e. the closest
!      time going backwards from current_dt that data exists for)
!   2. Depending what times are needed for interpolation, seek the underlying
!      file to the appropriate time
!   3. Read the timesteps required for interpolation
!   4. Calculate the data times for all times from -1 to 2, even if they are
!      not required (and even if they are not actually part of the data)
!   5. Calculate the number of model timesteps in each data period and the
!      number of timesteps into the current interpolation period we are
!
!-----------------------------------------------------------------------------


DO i = 1,nfiles
  !-----------------------------------------------------------------------------
  ! Step 1 - find the data time for the requested datetime for this file
  !-----------------------------------------------------------------------------
  SELECT CASE ( files(i)%fh%data_period )
  CASE ( 1: )
    ! In the case of a period of seconds >= 1, we need to know how many seconds
    ! past the closest data time current_dt is, and take that off current_dt

    ! The IBM XL Fortran compiler requires explicit conversion between
    ! the default integer kind and int64:
    !    diff_secs is int64
    !    files(i)%fh%data_period => int64
    !    MOD in 64-bit
    !    result => default real kind for time_into_period
    ! Some compilers apply implicit conversion:
    ! For gfortran, we can confirm this using -Wconversion
    ! For intel, results confirm that this is what is happening
    diff_secs = datetime_diff(files(i)%fh%data_start, current_time)
    time_into_period = MOD(diff_secs, INT(files(i)%fh%data_period, int_64))
    data_dt = datetime_subtract(current_time, time_into_period)

  CASE ( period_month )
    ! In the case of a monthly period, we know that the data applies from the
    ! start of the month
    data_dt = datetime_clone(current_time)
    data_dt%day  = 1
    data_dt%TIME = 0

  CASE ( period_year )
    ! In the case of a yearly period, we know that the data will apply from the
    ! start of the year
    data_dt = datetime_clone(current_time)
    data_dt%month = 1
    data_dt%day   = 1
    data_dt%TIME  = 0
  END SELECT

  ! Update the time of the current data
  files(i)%data_times(0) = data_dt


  !-----------------------------------------------------------------------------
  ! Step 2 - depending on the interpolation in use for this file, seek the
  !          underlying timeseries file to the correct place
  !-----------------------------------------------------------------------------
  seek_dt = datetime_clone(data_dt)
  ! Because data_period could be a special period, which we can't meaningfully
  ! multiply, we do this by looping
  DO j = 1,ABS(files(i)%times_lbound)
    IF ( files(i)%times_lbound < 0 ) THEN
      seek_dt = datetime_subtract(seek_dt, files(i)%fh%data_period)
    ELSE
      seek_dt = datetime_advance(seek_dt, files(i)%fh%data_period)
    END IF
  END DO

  CALL file_ts_seek_to_datetime(files(i)%fh, seek_dt)


  !-----------------------------------------------------------------------------
  ! Step 3 - read the number of timesteps required for interpolation for
  !          each field
  !-----------------------------------------------------------------------------
  DO j = files(i)%times_lbound,files(i)%times_ubound
    DO k = 1,files(i)%nfields
      ! Read the slab of data for this time
      files(i)%fields(k)%DATA(j) = file_ts_read_var(                           &
        files(i)%fh, files(i)%fields(k)%file_id,                               &
      ! Subgrid information from input_mod
                use_subgrid, subgrid                                           &
              )
    END DO  ! fields

    ! If we need to read more data now, advance the file
    IF ( j /= files(i)%times_ubound ) CALL file_ts_advance(files(i)%fh)
  END DO  ! times


  !-----------------------------------------------------------------------------
  ! Step 4 - Populate data_times for all times -1 to 2
  !-----------------------------------------------------------------------------
  ! We know data_times(0), so populate the other times from that
  files(i)%data_times(-1) = datetime_subtract(                                 &
    files(i)%data_times(0), files(i)%fh%data_period                            &
  )

  DO j = 1,2
    files(i)%data_times(j) = datetime_advance(                                 &
      files(i)%data_times(j-1), files(i)%fh%data_period                        &
    )
  END DO

  !-----------------------------------------------------------------------------
  ! Step 5 - Populate tsteps_in_data_period and current_tstep
  !-----------------------------------------------------------------------------
  ! Populate tsteps_in_data_period from the data times
  DO j = -1,1
    diff_secs = datetime_diff(                                                 &
      files(i)%data_times(j), files(i)%data_times(j+1)                         &
    )

    IF ( MOD(diff_secs, INT(timestep_len, int_64)) /= 0 )                      &
      CALL log_fatal("seek_all_to_current_datetime",                           &
                     "Data should be a whole number of model timesteps apart")

    files(i)%tsteps_in_data_period(j) = REAL(diff_secs) / REAL(timestep_len)
  END DO

  ! Work out how many model timesteps into the interpolation period we are
  diff_secs = datetime_diff(files(i)%data_times(0), current_time)

  IF ( MOD(diff_secs, INT(timestep_len, int_64)) /= 0 )                        &
    CALL log_fatal("seek_all_to_current_datetime",                             &
                   "Data should be a whole number of model timesteps apart")

  files(i)%current_tstep = REAL(diff_secs) / REAL(timestep_len)

END DO  ! files

RETURN

END SUBROUTINE seek_all_to_current_datetime
#endif
