#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE advance_all()

USE precision_mod, ONLY: int_64

USE datetime_mod, ONLY: period_month, period_year,                             &
                         datetime_advance, datetime_diff, datetime_clone

USE data_cube_mod, ONLY: cube_safe_copy, cube_free

USE model_time_mod, ONLY: is_spinup, spinup_start, spinup_end, timestep_len

USE file_ts_mod, ONLY: file_ts_advance, file_ts_seek_to_datetime,              &
                        file_ts_read_var

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Advances all the input files so that they are able to provide data for
!   the current model time when the timestep has been advanced normally (i.e.
!   by one model timestep)
!   If the current model time has been set abnormally (i.e. resetting to the
!   start of spinup period), use seek_to_current_datetime instead
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
TYPE(datetime) :: corrected_dt  ! Next data time corrected for end of spinup
                                ! if required
INTEGER :: secs_into_period  ! The number of seconds into a data_period
                             ! that spinup_start is

INTEGER(KIND=int_64) :: diff_secs
                      ! Used in calculation of tsteps_in_data_period
                      ! The number of seconds between two times

! Work variables
INTEGER :: i, j, k  ! Loop counters

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Overview of algorithm (for each file) in psuedocode:
!
!   IF ( read is not required ) exit
!
!   advance underlying file
!
!   FOREACH variable IN file
!     shift data back one timestep
!
!     read next slab of data into last time slot (vacated by the shift above)
!   END FOREACH
!
!   adjust data times
!
!-----------------------------------------------------------------------------


DO i = 1,nfiles
  ! Every time we advance, we need to increase the counter for number of model
  ! timesteps into a data period we are
  files(i)%current_tstep = files(i)%current_tstep+1

  ! Check if the file is required to read more data (i.e. we have read all the
  ! timesteps in this data period)
  ! If not, then there is nothing more to do
  IF ( files(i)%current_tstep < files(i)%tsteps_in_data_period(0) ) CYCLE


  !-----------------------------------------------------------------------------
  ! If we get to here, then we need to read the next data for this file
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! First, update the metadata for data times etc.
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Update data_times as if we are proceeding normally
  !-----------------------------------------------------------------------------
  ! We know that on entering this routine, data_times(-1:2) were all correctly
  ! populated (since a seek will have been done before)
  ! This must be the case for the new timestep when this routine exits

  ! The shift means that all times in the range -1:1 are correctly populated
  ! for the new timestep
  files(i)%data_times = EOSHIFT(files(i)%data_times, shift = 1,                &
  ! Since data_times is an array of derived type objects, Intel insists on
  ! boundary being provided (makes sense since it doesn't know what to put
  ! there!). We don't actually care what it is since it will be overwritten later
                                boundary = datetime_clone(files(i)%data_times(1)) &
      )
  ! Populate data_times(2) from data_times(1)
  files(i)%data_times(2) = datetime_advance(                                   &
    files(i)%data_times(1), files(i)%fh%data_period                            &
  )

  !-----------------------------------------------------------------------------
  ! Update tsteps_in_data_period in a similar fashion using the calculated
  ! data times
  !-----------------------------------------------------------------------------
  ! We know that on entering this routine, tsteps_in_data_period(-1:1) were
  ! all correctly populated, so after the shift, -1:0 will be correct for the
  ! new timestep
  files(i)%tsteps_in_data_period = EOSHIFT(files(i)%tsteps_in_data_period, shift = 1)

  ! All that remains (in a normal situation) is to calculate the value for
  ! index 1 from the new data_times
  diff_secs = datetime_diff(files(i)%data_times(1), files(i)%data_times(2))

  IF ( MOD(diff_secs, INT(timestep_len, int_64)) /= 0 )                        &
    CALL log_fatal("advance_all",                                              &
                   "Data should be a whole number of model timesteps apart")

  files(i)%tsteps_in_data_period(1) = REAL(diff_secs) / REAL(timestep_len)


  !-----------------------------------------------------------------------------
  ! If we are coming to the end of a spinup cycle, we want to correct the
  ! data_times (and tsteps_in_data_period) so that the data ramp nicely across
  ! the break in time, rather than changing instantaneously
  !
  ! Note that this corrects the data times for times in the future, even
  ! when they are not required. Hence we check below to see if the required
  ! data times are out of sync with the underlying file - if they are, we need
  ! to seek the file
  !
  ! This assumes that we will be starting a new cycle of spinup
  ! In the case of the start of the main run, the files are seeked to the
  ! start of the main run with no ramping in next_time
  ! This means that running a spinup and starting from the end dump of that
  ! is equivalent to running a spinup and then carrying on with the main run
  !-----------------------------------------------------------------------------
  IF ( is_spinup .AND. files(i)%data_times(2) >= spinup_end ) THEN
    ! Adjust data_times(2) so that the time wraps around to the first data time
    ! after spinup start
    ! To do this, we have to find the first data time after spinup start
    SELECT CASE ( files(i)%fh%data_period )
    CASE ( 1: )
      ! In the case of a period of seconds >= 1, we start by assuming that the
      ! corrected datetime will be exactly spinup start
      corrected_dt = spinup_start

      ! Work out how many seconds into a data period that datetime is

      ! The IBM XL Fortran compiler requires explicit conversion between
      ! the default integer kind and int64:
      !    datetime_diff returns int64
      !    files(i)%fh%data_period => int64
      !    MOD in 64-bit
      !    result => default real kind for secs_into_period
      ! Some compilers apply implicit conversion:
      ! For gfortran, we can confirm this using -Wconversion
      ! For intel, results confirm that this is what is happening
      secs_into_period = MOD(                                                  &
        datetime_diff(files(i)%fh%data_start, corrected_dt),                   &
        INT(files(i)%fh%data_period, int_64)                                   &
      )

      ! If we guessed the wrong time, make a correction by adding on enough
      ! seconds to take us to the next data time
      IF ( secs_into_period > 0 )                                              &
        corrected_dt = datetime_advance(                                       &
          corrected_dt, files(i)%fh%data_period - secs_into_period             &
        )

    CASE ( period_month )
      ! In the case of a monthly period, we know that we can get the start of the
      ! data period containing spinup_start just by setting the day and time to the
      ! beginning of the month
      corrected_dt = datetime_clone(spinup_start)
      corrected_dt%day  = 1
      corrected_dt%TIME = 0

      ! Only if that time is strictly less than spinup_start do we need to correct by
      ! adding on a data period
      IF ( corrected_dt < spinup_start )                                       &
        corrected_dt = datetime_advance(corrected_dt, period_month)

    CASE ( period_year )
      ! In the case of a yearly period, we know that we can get the start of the
      ! data period containing spinup_start just by setting the month, day and time
      ! to the beginning of the year
      corrected_dt = datetime_clone(spinup_start)
      corrected_dt%month = 1
      corrected_dt%day   = 1
      corrected_dt%TIME  = 0

      ! Only if that time is strictly less than spinup_start do we need to correct by
      ! adding on a data period
      IF ( corrected_dt < spinup_start )                                       &
        corrected_dt = datetime_advance(corrected_dt, period_year)
    END SELECT

    files(i)%data_times(2) = corrected_dt

    ! Adjust tsteps_in_data_period so that the number of timesteps in the affected
    ! period is the number until the end of spinup + the number from start of
    ! spinup until the next data
    diff_secs = datetime_diff(files(i)%data_times(1), spinup_end)              &
              + datetime_diff(spinup_start, files(i)%data_times(2))

    IF ( MOD(diff_secs, INT(timestep_len, int_64)) /= 0 )                      &
      CALL log_fatal("advance_all",                                            &
                     "Each spinup cycle must contain a whole number of " //    &
                     "timesteps")

    files(i)%tsteps_in_data_period(1) = REAL(diff_secs) / REAL(timestep_len)
  END IF

  ! We are now in the first timestep of a new data period
  files(i)%current_tstep = 0


  !-----------------------------------------------------------------------------
  ! We need to make sure that the file is at times_ubound before we update the
  ! data
  !
  ! In the majority of cases this will be a regular advance, but in the case
  ! of rolling over a spinup cycle, it will require a seek
  !-----------------------------------------------------------------------------
  ! To determine if we need a seek, we compare data_times(times_ubound) to
  ! data_times(times_ubound - 1)
  IF ( files(i)%data_times(files(i)%times_ubound) >                            &
       files(i)%data_times(files(i)%times_ubound-1) ) THEN
    ! If we are moving forward in time normally, then assume that we just require
    ! an advance
    CALL file_ts_advance(files(i)%fh)
  ELSE
    ! Otherwise seek
    CALL file_ts_seek_to_datetime(                                             &
      files(i)%fh, files(i)%data_times(files(i)%times_ubound)                  &
    )
  END IF


  !-----------------------------------------------------------------------------
  ! Update the actual data
  !-----------------------------------------------------------------------------
  DO j = 1,files(i)%nfields
    ! Shift data back one timestep
    DO k = files(i)%times_lbound,(files(i)%times_ubound-1)
      ! Copy the state from the next timestep into the current timestep
      CALL cube_safe_copy(                                                     &
        files(i)%fields(j)%DATA(k), files(i)%fields(j)%DATA(k+1)               &
      )
    END DO

    ! Read next slab of data into the last timeslot
    CALL cube_free(files(i)%fields(j)%DATA(files(i)%times_ubound))
    files(i)%fields(j)%DATA(files(i)%times_ubound) = file_ts_read_var(         &
      files(i)%fh, files(i)%fields(j)%file_id,                                 &
    ! Subgrid information from input_mod
            use_subgrid, subgrid                                               &
          )
  END DO  ! fields

END DO  ! files

RETURN

END SUBROUTINE advance_all
#endif
