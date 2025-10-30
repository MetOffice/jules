! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE read_dump_shared_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE read_dump_shared(file_name, identifiers, nvars, FILE, var_ids)

!Others
USE io_constants, ONLY:                                                        &
  mode_read, max_dim_var, max_sdf_name_len

USE parallel_mod, ONLY:                                                        &
  is_master_task

USE string_utils_mod, ONLY:                                                    &
  to_string

USE file_mod, ONLY:                                                            &
  file_handle, file_open, file_introspect,                                     &
  file_inquire_dim, file_inquire_var

USE logging_mod,      ONLY: log_fatal, log_warn
USE get_dim_info_mod, ONLY: get_dim_info

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Check that the given file is a dump compatible with the current
!   run, and read the given identifiers from it. This is code shared by both
!   standalone JULES & Rivers.
!   Note that the reading of the JULES dump is done by the master task and the
!   results scattered to other tasks. This means that dumps written with
!   different amounts of tasks should be interchangable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*) :: file_name  ! The dump file
CHARACTER(LEN=*) :: identifiers(:)  ! The model identifiers for the
                                    ! variables to define
INTEGER :: nvars
  ! The number of variables we are processing

TYPE(file_handle) :: FILE
  ! The opened file

INTEGER :: var_ids(SIZE(identifiers))
  ! The ids of the variables in the dump file

! Local parameters.
LOGICAL, PARAMETER :: l_reading_true = .TRUE.
  ! A value of .TRUE. that is passed to argument l_reading of subroutine
  ! get_dim_info to show that it is being called in connection with reading
  ! (rather than writing) a dump.

! Work variables
INTEGER :: dim_size_file
  ! The size of the dimension currently being
  ! processed in the file

! Used when defining dimensions and variables
INTEGER :: ndims
  ! The number of dimensions the current variable has
CHARACTER(LEN=max_sdf_name_len) :: dim_names(max_dim_var)
  ! The dimension names the current variable should use
INTEGER :: dim_sizes(max_dim_var)
  ! The dimension sizes for the current variable
INTEGER :: dim_ids(max_dim_var)
  ! The dimension ids for the current variable as
  ! calculated from file_inquire_dim
LOGICAL :: is_record_dim
  ! Detects if the current dimension is a record dim
INTEGER :: ndims_file
  ! The number of dimensions the variable has in file
  ! Compared to ndims above for each variable
INTEGER :: dim_ids_file(max_dim_var)
  ! The ids of the dimensions the variable has in the file
  ! Compared to dim_ids above to verify the variable has the
  ! correct dimensions

LOGICAL :: is_record_var
  ! Indicates if a variable uses the record dimension

LOGICAL :: l_read_from_dump
  ! Used to bypass checking the dimensions of ancil variables that are
  ! not to be read from the dump.

INTEGER :: i, j  ! Loop counters

nvars = SIZE(identifiers)

!-----------------------------------------------------------------------------
! In the master task only, we open the file and check that the correct
! dimensions exist and are of a size compatible with this run
!-----------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  !-----------------------------------------------------------------------------
  ! We use the lowest level file API here, as we don't want to impose the input
  ! grid
  !-----------------------------------------------------------------------------
  FILE=file_open(file_name, mode_read)

  ! We want to auto-detect the dimensions and variables in the file
  CALL file_introspect(FILE)

  DO i = 1,nvars

    !-----------------------------------------------------------------------
    ! Get information about the dimensions used by the variable.
    ! The argument l_reading_true shows that we are reading (not writing) a
    ! dump.
    !-----------------------------------------------------------------------
    CALL get_dim_info( l_reading_true, identifiers(i), ndims,  dim_sizes,      &
                       dim_names, l_read_from_dump )

    !-------------------------------------------------------------------------
    ! Check the dimensions exist and have the correct size
    !-------------------------------------------------------------------------
    IF ( l_read_from_dump ) THEN
      DO j = 1,ndims
        ! Retrive information about the dimension from the file we store the id
        ! for use outside this loop
        CALL file_inquire_dim(                                                 &
          FILE, dim_names(j), dim_ids(j), dim_size_file, is_record_dim         &
        )

        ! Check that we found a dimension
        IF ( dim_ids(j) < 0 )                                                  &
          CALL log_fatal("read_dump_shared",                                   &
                         "Could not find expected dimension '" //              &
                         TRIM(dim_names(j)) // "' in dump file")

        ! Check that the dimension is not a record dimension (there shouldn't
        ! be one in dump files).
        IF ( is_record_dim )                                                   &
          CALL log_fatal("read_dump_shared",                                   &
                         "Dimension '" // TRIM(dim_names(j)) // "' is a " //   &
                         "record dimension - should not exist in dump file")

        ! Check that the dimension has the correct size
        IF ( dim_size_file /= dim_sizes(j) )                                   &
          CALL log_fatal("read_dump_shared",                                   &
                         "Dimension '" // TRIM(dim_names(j)) // "' has " //    &
                         "size incompatible with current run (required: " //   &
                         TRIM(to_string(dim_sizes(j))) // ", found: " //       &
                         TRIM(to_string(dim_size_file)) // ")")
      END DO  ! dims

      !-----------------------------------------------------------------------
      ! Check that the variable exists and has the correct dimensions
      !-----------------------------------------------------------------------
      ! Retrieve information about the variable from the file
      CALL file_inquire_var(                                                   &
        FILE, identifiers(i), var_ids(i), ndims_file, dim_ids_file,            &
        is_record_var                                                          &
      )

      ! Check that we found a variable
      IF ( var_ids(i) < 1 ) THEN
        SELECT CASE ( identifiers(i) )
        CASE ( 'rivers_outflow_rp' )
          CALL log_warn("read_dump_shared",                                    &
             "rivers_outflow_rp is not in the dump so will be initialised " // &
             "to zero.")
        CASE DEFAULT
          CALL log_fatal("read_dump_shared",                                   &
                         "Failed to find requested variable '" //              &
                         TRIM(identifiers(i)) // "' in dump file")
        END SELECT
      ELSE
          ! Check that the number of dimensions match
        IF ( ndims_file /= ndims )                                             &
           CALL log_fatal("read_dump_shared",                                  &
                          "Variable '" // TRIM(identifiers(i)) // "' has " //  &
                          "incorrect number of dimensions in dump file (" //   &
                          "expected: " // TRIM(to_string(ndims)) // ", " //    &
                          "found: " // TRIM(to_string(ndims_file)) // ")")

        ! Check that the dimension ids match
        IF ( .NOT. ALL(dim_ids(1:ndims) == dim_ids_file(1:ndims)) )            &
           CALL log_fatal("read_dump_shared",                                  &
                          "Variable '" // TRIM(identifiers(i)) // "' has " //  &
                          "incorrect dimensions in dump file")
      END IF ! ( var_ids(i) < 1 )
    END IF  !  l_read_from_dump

  END DO  ! vars

END IF  ! MASTER TASK

RETURN

END SUBROUTINE read_dump_shared
END MODULE read_dump_shared_mod
