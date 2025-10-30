#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE output_data()

USE io_constants, ONLY: mdi

USE datetime_mod, ONLY: datetime_advance

USE data_cube_mod, ONLY: data_cube, cube_safe_copy, cube_free,                 &
                          OPERATOR ( + ), OPERATOR ( * ), OPERATOR ( / )

USE model_time_mod, ONLY: current_time, is_spinup, spinup_start, timestep_len

USE file_ts_mod, ONLY: file_ts_write_var, file_ts_advance

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   For each profile, decide if we need to output data at this timestep, and
!   output it if we do
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------
! Work variables
TYPE(datetime) :: next_time  ! The time at the next timestep

TYPE(data_cube) :: data_for_output  ! The data cube to output

INTEGER :: i,j  ! Loop counter

!------------------------------------------------------------------------------

next_time = datetime_advance(current_time, timestep_len)

DO i = 1,nprofiles

  !----------------------------------------------------------------------------
  ! River profiles are only output from the master task.
  !----------------------------------------------------------------------------
  IF ( profiles(i)%is_river_profile .AND. .NOT. is_master_task() ) THEN
    CYCLE
  END IF

  !----------------------------------------------------------------------------
  ! Check if the profile is currently active
  !----------------------------------------------------------------------------
  IF ( is_spinup .AND. .NOT. profiles(i)%output_spinup ) THEN
    ! If we are in spinup but profile is not outputting, we don't need to do
    ! anything
    CYCLE
  END IF

  IF ( .NOT. is_spinup .AND.                                                   &
       ( .NOT. profiles(i)%output_main_run .OR.                                &
         ( current_time < profiles(i)%output_start .OR.                        &
           profiles(i)%output_end < current_time ) ) ) THEN
    ! If we are in the main run but either not outputting any of the main run
    ! or the time is out of the range of times the profile is outputting, we
    ! don't need to do anything
    CYCLE
  END IF

  !----------------------------------------------------------------------------
  ! Open the next output file for the profile, if required
  !
  ! This is the case if:
  !
  !   - The profile is outputting during spinup and we are at in the first
  !     timestep of a spinup cycle
  !
  !   - The profile is outputting part of the main run and we are in the
  !     timestep in the main run when output starts
  !
  ! We do this check in two stages for readability
  !----------------------------------------------------------------------------
  IF ( is_spinup .AND. profiles(i)%output_spinup .AND.                         &
       current_time == spinup_start ) THEN
    CALL internal_next_output_file(profiles(i))
  END IF

  IF ( .NOT. is_spinup .AND. profiles(i)%output_main_run .AND.                 &
       current_time == profiles(i)%output_start ) THEN
    CALL internal_next_output_file(profiles(i))
  END IF

  !-----------------------------------------------------------------------------
  ! If we are in the last timestep of an output period (i.e. the time at the
  ! start of the next model timestep is equal to (or greater than) the start of
  ! the next output interval), output data
  !-----------------------------------------------------------------------------
  IF ( next_time >= profiles(i)%next_output_time ) THEN

    DO j = 1,profiles(i)%nfields

      SELECT CASE ( profiles(i)%fields(j)%field_type )

      CASE ( output_snapshot, output_min, output_max )
        ! For snapshot, min or max variables, just copy the data array
        CALL cube_safe_copy(data_for_output,                                   &
                            profiles(i)%fields(j)%field_data)
        ! Once data has been output, reset it to mdi
        profiles(i)%fields(j)%field_data%values(:) = mdi

      CASE ( output_accum )
        ! In the case of an accumulation, we output the accumulated value
        ! multiplied by the number of timesteps in a sample period. This
        ! adjusts the accumulation for any intermittent sampling and is
        ! designed so that the time accumulation of a flux (e.g. kg s-1) can
        ! easily be converted to a total (e.g. kg) by subsequently multiplying
        ! by the model timestep length during post processing.
        ! Note that we don't have to worry about using cube_safe_copy since the
        ! multiplication creates a new cube.
        data_for_output = profiles(i)%fields(j)%field_data                     &
                * ( REAL(profiles(i)%sample_period) / REAL(timestep_len) )

      CASE ( output_mean )
        ! In the case of a mean, divide the accumulated value by the number of
        ! samples taken.
        ! Note that we don't have to worry about using cube_safe_copy since the
        ! division creates a new cube.
        data_for_output = profiles(i)%fields(j)%field_data                     &
                        / REAL(profiles(i)%samples_in_period)
        ! Zero the field's data property and start gathering again
        profiles(i)%fields(j)%field_data%values(:) = 0.0
      END SELECT

      ! Write the data to file
      IF ( profiles(i)%is_river_profile ) THEN
        CALL file_ts_write_var(                                                &
               profiles(i)%fh, profiles(i)%fields(j)%file_id, data_for_output, &
               ! No subgrid used for river variables
              .FALSE. )
      ELSE
        ! All other variables are written in parallel
        CALL file_ts_write_var(                                                &
               profiles(i)%fh, profiles(i)%fields(j)%file_id, data_for_output, &
               ! Subgrid information (for writing a slab of the output grid in
               ! parallel mode)
               use_subgrid, subgrid )
      END IF

      ! Free the temporary cube
      CALL cube_free(data_for_output)

    END DO  ! fields

    !--------------------------------------------------------------------------
    ! Update tracking variables and, if this is not the last output for the
    ! profile, advance the file
    !--------------------------------------------------------------------------
    profiles(i)%samples_in_period = 0

    profiles(i)%current_output_time = profiles(i)%next_output_time
    profiles(i)%next_output_time =                                             &
      datetime_advance(profiles(i)%next_output_time,                           &
                       profiles(i)%output_period)

    ! Check if this is the last output time for the currently open output file
    ! and if this task has an open file. If it is not, advance the file.
    IF ( profiles(i)%has_open_file .AND.                                       &
         profiles(i)%current_output_time < profiles(i)%fh%data_end ) THEN
      CALL file_ts_advance(profiles(i)%fh)
    END IF

  END IF  ! Last timestep of output period

END DO

RETURN

END SUBROUTINE output_data
#endif
