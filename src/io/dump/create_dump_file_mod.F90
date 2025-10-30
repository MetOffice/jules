! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE create_dump_file_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE create_dump_file(nvars, identifiers, FILE, var_ids)

USE io_constants, ONLY:                                                        &
  max_file_name_len, max_dim_var, mode_write, max_sdf_name_len, format_ascii,  &
  format_ncdf

USE parallel_mod, ONLY:                                                        &
  is_master_task

USE model_interface_mod, ONLY:                                                 &
  identifier_len

USE dictionary_mod, ONLY:                                                      &
  dict, dict_create, dict_set, dict_get, dict_has_key, dict_free

USE string_utils_mod, ONLY:                                                    &
  to_string

USE file_mod, ONLY:                                                            &
  file_handle, file_open, file_def_dim, file_def_var,                          &
  file_enddef

USE output_mod, ONLY:                                                          &
  output_dir, run_id

USE model_time_mod, ONLY:                                                      &
  current_time, is_spinup, spinup_cycle

USE get_dim_info_mod,  ONLY: get_dim_info
USE dump_mod,          ONLY: max_dim_dump, max_var_dump, dump_format
USE logging_mod,       ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a dump file for the current timestep
!   Note that the writing of the dump is done by the master task with the
!   values gathered from other tasks. This means that dumps written with
!   different amounts of tasks should be interchangable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
INTEGER, INTENT(IN) :: nvars  ! The number of variables we are processing

CHARACTER(LEN=identifier_len), INTENT(IN) :: identifiers(:)
                                    ! The model identifiers for the variables
                                    ! to put in the dump

TYPE(file_handle), INTENT(OUT) :: FILE  ! The dump file

INTEGER, INTENT(OUT) :: var_ids(:)
                      ! The ids of the variables in the dump file

! Local parameters.
LOGICAL, PARAMETER :: l_reading_false = .FALSE.
  ! A value of .FALSE. that is passed to argument l_reading of subroutine
  ! get_dim_info to show that it is being called in connection with writing
  ! (not reading) a dump.

! Work variables
CHARACTER(LEN=max_file_name_len) :: file_name
                                    ! The filename to use for the dump file
CHARACTER(LEN=max_file_name_len) :: dt_string
                                    ! The datetime string to use in the file
                                    ! name

! Variables used when defining dimensions
TYPE(dict) :: file_dim_ids  ! Dictionary of the dimensions that have been
                            ! defined
                            ! Maps dim_name => dim_id

INTEGER :: ndims  ! The number of levels dims for the current variable
CHARACTER(LEN=max_sdf_name_len) :: dim_names(max_dim_var)
                  ! The levels dimension names for the current variable
INTEGER :: dim_sizes(max_dim_var)
                  ! The sizes of the levels dims for the current variable
INTEGER :: dim_ids(max_dim_var)
                  ! The ids in file of the levels dims for the current
                  ! variable
INTEGER :: i, j   ! Loop counters

!-----------------------------------------------------------------------------
! In the master task only, we open a new file and define the required
! dimensions and variables
!-----------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  !---------------------------------------------------------------------------
  ! Generate the file name that we want to use and open the file
  !---------------------------------------------------------------------------
  ! File name starts with run id + indicator of a dump file
  file_name = TRIM(run_id) // ".dump."

  ! Include the current spinup cycle if there is one
  IF ( is_spinup )                                                             &
    file_name = TRIM(file_name) //                                             &
                "spin" // TRIM(to_string(spinup_cycle)) // "."

  ! Then current date and time
  WRITE(dt_string, '(I4.4,I2.2,I2.2)') current_time%year,                      &
                                       current_time%month,                     &
                                       current_time%day
  dt_string = TRIM(dt_string) // "." // TRIM(to_string(current_time%TIME))
  file_name = TRIM(file_name) // TRIM(dt_string)

  ! Add the extension based on dump format
  SELECT CASE ( dump_format )
  CASE ( format_ascii )
    file_name = TRIM(file_name) // ".asc"

  CASE ( format_ncdf )
    file_name = TRIM(file_name) // ".nc"

  CASE DEFAULT
    CALL log_fatal("create_dump_file",                                         &
                   "Unrecognised file format - " // TRIM(dump_format))
  END SELECT

  ! Prepend the output directory
  file_name = TRIM(output_dir) // "/" // TRIM(file_name)

  ! We use the lowest level file API here, as we don't want to impose a grid
  FILE=file_open(file_name, mode_write)

  !---------------------------------------------------------------------------
  ! Create the dimensions and variables
  !---------------------------------------------------------------------------
  file_dim_ids = dict_create(max_dim_dump, INT(1))

  DO i = 1,nvars

    !------------------------------------------------------------------------
    ! Get information about the dimensions used by the variable.
    ! The argument l_reading_false shows that we are writing (not reading) a
    ! dump.
    !------------------------------------------------------------------------
    CALL get_dim_info( l_reading_false, identifiers(i), ndims,  dim_sizes,     &
                       dim_names )

    !-------------------------------------------------------------------------
    ! Define the dimensions if they have not already been defined
    ! We use a dictionary to keep track of defined dimension ids
    !
    ! At the same time, gather up the dimension ids needed by the current
    ! variable.
    !-------------------------------------------------------------------------
    DO j = 1,ndims
      ! If it has not yet been defined, define the dimension, storing its id
      IF ( .NOT. dict_has_key(file_dim_ids, dim_names(j)) )                    &
        CALL dict_set(                                                         &
          file_dim_ids, dim_names(j),                                          &
          file_def_dim(FILE, dim_names(j), dim_sizes(j))                       &
        )

      ! Get the dimension id from the dict and add it to the list for this
      ! variable.
      CALL dict_get(file_dim_ids, dim_names(j), dim_ids(j))
    END DO

    !-------------------------------------------------------------------------
    ! Define the variable, saving the id in the file for later
    !-------------------------------------------------------------------------
    var_ids(i) = file_def_var(FILE, identifiers(i), dim_ids(1:ndims),          &
                              .FALSE.)

  END DO

  !---------------------------------------------------------------------------
  ! We have finished defining things
  !---------------------------------------------------------------------------
  CALL file_enddef(FILE)
  CALL dict_free(file_dim_ids)

END IF  ! MASTER TASK

RETURN

END SUBROUTINE create_dump_file
END MODULE create_dump_file_mod
