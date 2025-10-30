#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE output_initial_data()

USE datetime_mod, ONLY: datetime_subtract

USE data_cube_mod, ONLY: cube_free

USE model_time_mod, ONLY: is_spinup, spinup_start, main_run_start,             &
                           spinup_cycle, current_time

USE file_ts_mod, ONLY: file_ts_write_var, file_ts_close

USE model_interface_mod, ONLY: extract_var

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   For each profile, decide if we need to output a file containing initial
!   data for a section of the run
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

! Work variables
TYPE(file_ts) :: FILE  ! The initial data file

CHARACTER(LEN=max_file_name_len) :: file_name
                                        ! The name for the initial data file

TYPE(datetime) :: file_start  ! The start time for the file

TYPE(grid_info) :: out_grid  ! The output grid (land points or river vector)

TYPE(data_cube) :: data_for_output  ! Cube to hold data for output

INTEGER :: i,j  ! Loop counter

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'OUTPUT_INITIAL_DATA'

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! IF we are not in the first timestep of a section, there is nothing to do
! for any profile
!------------------------------------------------------------------------------
IF ( is_spinup .AND. current_time /= spinup_start ) THEN
  RETURN
END IF

IF ( .NOT. is_spinup .AND. current_time /= main_run_start ) THEN
  RETURN
END IF

DO i = 1,nprofiles

  !----------------------------------------------------------------------------
  ! Check if we need to output initial data for this profile
  !----------------------------------------------------------------------------
  IF ( .NOT. profiles(i)%output_initial ) THEN
    ! IF initial state was not asked for, there is nothing to do
    CYCLE
  END IF

  IF ( .NOT. ANY( profiles(i)%fields(1:profiles(i)%nfields)%field_type         &
                  == output_snapshot ) ) THEN
    ! IF there is no snapshot data in the profile, there is nothing to output
    ! here.
    CYCLE
  END IF

  IF ( is_spinup .AND. .NOT. profiles(i)%output_spinup ) THEN
    ! IF we are at the start of a spinup cycle but profile is not outputting
    ! during spinup, we don't need to do anything
    CYCLE
  END IF

  IF ( .NOT. is_spinup ) THEN

    ! We are at the start of the main run
    IF ( .NOT. profiles(i)%output_main_run ) THEN
      ! If the profile is not outputting for the main run, there is nothing to
      ! do.
      CYCLE
    END IF

    IF ( profiles(i)%output_start /= main_run_start ) THEN
      ! If the profile does not start outputting until later in the run, there
      ! is nothing to do.
      CYCLE
    END IF

    !--------------------------------------------------------------------------
    ! River profiles are only output from the master task.
    !--------------------------------------------------------------------------
    IF ( profiles(i)%is_river_profile .AND. .NOT. is_master_task() ) THEN
      CYCLE
    END IF

  END IF

  !----------------------------------------------------------------------------
  ! Open an initial data file for the profile
  !----------------------------------------------------------------------------
  ! All file names start with the run id followed by the profile name
  file_name = TRIM(run_id) // "." // TRIM(profiles(i)%profile_name)

  IF ( is_spinup ) THEN
    ! IF we are in a spinup cycle, add that to the file name
    file_name = TRIM(file_name) // "." // "spin" //                            &
                TRIM(to_string(spinup_cycle))
  END IF

  ! Indicate that it is an initial data file
  file_name = TRIM(file_name) // ".initial"

  ! Add an extension based on the output format
  SELECT CASE ( output_format )
  CASE ( format_ascii )
    IF ( profiles(i)%is_river_profile ) THEN
      CALL log_fatal(RoutineName,                                              &
                     "Cannot create river grid output in ASCII format")
    END IF
    file_name = TRIM(file_name) // ".asc"

  CASE ( format_ncdf )
    file_name = TRIM(file_name) // ".nc"

  CASE DEFAULT
    CALL log_fatal(RoutineName,                                                &
                   "Unrecognised file format - " // TRIM(output_format))
  END SELECT

  ! Prepend the output directory
  file_name = TRIM(output_dir) // "/" // TRIM(file_name)

  ! The timestamp in files is the end of the output period, so we create a file
  ! with exactly one output period ending at the current time.
  ! We don't much care about time_bounds since the only real valid output is
  ! snapshot variables, for which the timestamp is the important thing.
  ! So we create a file that ends at the current time, starts 1 second before
  ! and has an output period of 1.
  file_start = datetime_subtract(current_time, 1)

  !----------------------------------------------------------------------------
  ! Check if we want land or river output
  !----------------------------------------------------------------------------
  IF ( profiles(i)%is_river_profile ) THEN
    out_grid = river_out_grid
  ELSE
    out_grid = grid
  END IF

  FILE = internal_open_output_file(                                            &
           file_start, current_time, 1, .FALSE., profiles(i)%is_river_profile, &
           file_name, out_grid, profiles(i)%fields(1:profiles(i)%nfields),     &
           profiles(i)%l_land_frac)

  !----------------------------------------------------------------------------
  ! Populate any snapshot variables in the file
  !----------------------------------------------------------------------------
  DO j = 1,profiles(i)%nfields

    ! Ignore variables that are not snapshot variables
    IF ( profiles(i)%fields(j)%field_type /= output_snapshot ) THEN
      CYCLE
    END IF

    ! Extract the data to output
    data_for_output = extract_var(profiles(i)%fields(j)%var_id)

    ! Output the data to file
    IF ( profiles(i)%is_river_profile ) THEN
      CALL file_ts_write_var(                                                  &
             FILE, profiles(i)%fields(j)%file_id, data_for_output,             &
             ! No subgrid used for river variables
             .FALSE. )
    ELSE
      ! All other variables are written in parallel
      CALL file_ts_write_var(                                                  &
             FILE, profiles(i)%fields(j)%file_id, data_for_output,             &
             ! Subgrid information (for writing a slab of the output grid in
             ! parallel mode)
             USE_subgrid, subgrid )
    END IF

    ! Free the temporary cube
    CALL cube_free(data_for_output)
  END DO

  !----------------------------------------------------------------------------
  ! Close the file.
  !----------------------------------------------------------------------------
  CALL file_ts_close(FILE)

END DO

RETURN

END SUBROUTINE output_initial_data
#endif
