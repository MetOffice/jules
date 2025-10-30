#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE sample_data()

USE datetime_mod, ONLY: datetime_advance

USE data_cube_mod, ONLY: data_cube, OPERATOR ( + ), cube_safe_copy,            &
                         cube_free, cube_min, cube_max

USE model_time_mod, ONLY: current_time, is_spinup, spinup_start

USE model_interface_mod, ONLY: extract_var

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   For each profile, decide if we need to sample data
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------
! Work variables
TYPE(data_cube) :: var_data, accum
               ! Workspace cubes that can be deallocated to avoid memory leaks

INTEGER :: i,j  ! Loop counter

!------------------------------------------------------------------------------

DO i = 1,nprofiles

  !----------------------------------------------------------------------------
  ! River profiles are only sampled on the master task.
  !----------------------------------------------------------------------------
  IF ( profiles(i)%is_river_profile .AND. .NOT. is_master_task() ) THEN
    CYCLE
  END IF

  !----------------------------------------------------------------------------
  ! Check if the profile is currently active
  !----------------------------------------------------------------------------
  IF ( is_spinup .AND. .NOT. profiles(i)%output_spinup ) THEN
    ! If we are in spinup but profile is not outputting, we don't need to do
    ! anything.
    CYCLE
  END IF

  IF ( .NOT. is_spinup .AND. ( .NOT. profiles(i)%output_main_run .OR.          &
       ( current_time < profiles(i)%output_start .OR.                          &
         profiles(i)%output_end < current_time ) ) ) THEN
    ! If we are in the main run but either not outputting any of the main run
    ! or the time is out of the range of times the profile is outputting, we
    ! don't need to do anything.
    CYCLE
  END IF

  !----------------------------------------------------------------------------
  ! Initialise the profile fields, if required
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
    CALL internal_init_profile_vars(profiles(i))
  END IF

  IF ( .NOT. is_spinup .AND. profiles(i)%output_main_run .AND.                 &
       current_time == profiles(i)%output_start ) THEN
    CALL internal_init_profile_vars(profiles(i))
  END IF

  !-----------------------------------------------------------------------------
  ! If we are in the first timestep of a sample period (i.e. current time is
  ! now equal to (or greater than) the next sample time), then sample the data
  !-----------------------------------------------------------------------------
  IF ( profiles(i)%next_sample_time <= current_time ) THEN

    DO j = 1,profiles(i)%nfields

      SELECT CASE ( profiles(i)%fields(j)%field_type )

      CASE ( output_snapshot )
        ! In the first timestep of an output period, capture snapshot values.
        ! Free the previous cube
        CALL cube_free(profiles(i)%fields(j)%field_data)
        ! Extract the data from the variable and set it as the current cube
        profiles(i)%fields(j)%field_data =                                     &
          extract_var(profiles(i)%fields(j)%var_id)

      CASE ( output_accum, output_mean )
        ! Extract current values.
        var_data = extract_var(profiles(i)%fields(j)%var_id)
        ! Collect the accumulated value in the work cube
        accum = profiles(i)%fields(j)%field_data + var_data
        ! Copy the accumulated value safely into the data cube
        CALL cube_safe_copy(profiles(i)%fields(j)%field_data, accum)
        ! Free the work cubes
        CALL cube_free(var_data)
        CALL cube_free(accum)

      CASE ( output_min, output_max )
        ! If this is the first sample of a new output period, just use the data
        IF ( profiles(i)%samples_in_period == 0 ) THEN
          CALL cube_free(profiles(i)%fields(j)%field_data)
          profiles(i)%fields(j)%field_data =                                   &
                                 extract_var(profiles(i)%fields(j)%var_id)
        ELSE
          ! Otherwise, compare the data for this sample to the previous data
          ! and store appropriately
          var_data = extract_var(profiles(i)%fields(j)%var_id)
          SELECT CASE ( profiles(i)%fields(j)%field_type )
          CASE ( output_min )
            accum = cube_min(profiles(i)%fields(j)%field_data, var_data)
            CALL cube_safe_copy(profiles(i)%fields(j)%field_data, accum)
          CASE ( output_max )
            accum = cube_max(profiles(i)%fields(j)%field_data, var_data)
            CALL cube_safe_copy(profiles(i)%fields(j)%field_data, accum)
          END SELECT
          ! Free the work cubes
          CALL cube_free(var_data)
          CALL cube_free(accum)
        END IF

      END SELECT
    END DO

    ! Increment the sample count
    profiles(i)%samples_in_period = profiles(i)%samples_in_period+1

    ! Work out when we will next take a sample
    profiles(i)%next_sample_time = datetime_advance(                           &
      profiles(i)%next_sample_time, profiles(i)%sample_period                  &
    )
  END IF

END DO  !  profiles

RETURN

END SUBROUTINE sample_data
#endif
