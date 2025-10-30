#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ncdf_def_var(FILE, var_name, dims_in, is_record) RESULT(var_id)

USE io_constants, ONLY: max_sdf_name_len, max_dim_var, mode_read, mode_write

USE netcdf, ONLY: nf90_inq_varid, nf90_inquire_variable, nf90_def_var,         &
                   nf90_float

#if !defined(MPI_DUMMY)
! Extra imports required to make parallel variable access collective
USE netcdf, ONLY: nf90_collective, nf90_var_par_access
#endif

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines a variable in the given file, returning the variable id
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ncdf), INTENT(IN OUT) :: FILE
                                  ! The file to define the variable in
CHARACTER(LEN=*), INTENT(IN) :: var_name
                                  ! The name of the variable
INTEGER, INTENT(IN) :: dims_in(:) ! The ids of the NON-RECORD dimensions of
                                  ! the variable
LOGICAL, INTENT(IN) :: is_record  ! Indicates whether the variable uses the
                                  ! record dimension

! Return type
INTEGER :: var_id                 ! The variable id - this is the index
                                  ! in the vars array of the file_ncdf object
                                  ! of the variable


! Work variables
INTEGER :: var_ncid  ! The NetCDF id of the variable

INTEGER :: ndims_in_file  ! The number of dimensions that the variable
                           ! has in the NetCDF file
                           ! Only used when in read mode for verification
                           ! purposes

INTEGER :: ndims     ! The number of dimensions supplied INCLUDING RECORD DIM

INTEGER, ALLOCATABLE :: dims_in_file(:) ! The dimension ids of the
                                        ! variable in the file

INTEGER :: n          ! Loop counters
INTEGER :: ERROR      ! Error indicator


!-----------------------------------------------------------------------------
! Check that a variable name has been provided.
IF ( LEN_TRIM(var_name) < 1 )                                                  &
  CALL log_fatal( "file_ncdf_def_var",                                         &
                  TRIM(FILE%NAME) // ": No variable name provided." )

! The record dimension needs to be defined before any variables can use it
IF ( is_record .AND. FILE%record_dim < 0 )                                     &
  CALL log_fatal("file_ncdf_def_var",                                          &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "var_name =" // TRIM(var_name) // ": " //                     &
                 "Record dimension must be defined before defining a " //      &
                 "variable as using it")

! Check that none of the given dimensions is the record dimension
! (note that we don't have to check first that a record dim has been defined,
! since file%record_dim takes the value -1 if not defined and no valid dimension
! should take this value anyway)
IF ( ANY(dims_in == FILE%record_dim) )                                         &
  CALL log_fatal("file_ncdf_def_var",                                          &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "var_name =" // TRIM(var_name) // ": " //                     &
                 "Record dimension is in dimension list - use of the " //      &
                 "record dimension should be specified using 'is_record'")

! Work out how many dimensions the variable has in total
ndims = SIZE(dims_in)
IF ( ndims > max_dim_var )                                                     &
  CALL log_fatal("file_ncdf_def_var",                                          &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "var_name =" // TRIM(var_name) // ": " //                     &
                 "Variable has too many dimensions - code only exists " //     &
                 "for variables with up to " //                                &
                 TRIM(to_string(max_dim_var)) // " dimensions")

IF ( is_record ) ndims = ndims + 1


SELECT CASE ( FILE%mode )
CASE ( mode_read )
  !-----------------------------------------------------------------------------
  ! In read mode, we just get the id of the variable and validate that it has
  ! the correct dimensions
  !-----------------------------------------------------------------------------

  ! Retrieve the variable id by its name
  ERROR = nf90_inq_varid(FILE%id, var_name, var_ncid)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_var",                                   &
                        TRIM(FILE%NAME) // ": " //                             &
                        "Error getting variable id for variable: " //          &
                        TRIM(var_name), ERROR)

  ! Get the number of dimensions that the variable has in the file
  ERROR = nf90_inquire_variable(FILE%id, var_ncid, ndims = ndims_in_file)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_var",                                   &
                        TRIM(FILE%NAME) // ": " //                             &
                        "var_name =" // TRIM(var_name) // ": " //              &
                        "Error getting number of dimensions for variable",     &
                        ERROR)

  ! Check that the number of dimensions match
  IF ( ndims_in_file /= ndims )                                                &
    CALL log_fatal("file_ncdf_def_var",                                        &
                   TRIM(FILE%NAME) // ": " //                                  &
                   "var_name =" // TRIM(var_name) // ": " //                   &
                   "Dimensions mismatch - number in file: " //                 &
                   TRIM(to_string(ndims_in_file)) // ", expected: " //         &
                   TRIM(to_string(ndims)))

  ! Retrieve the dimension ids
  ALLOCATE(dims_in_file(ndims_in_file))
  ERROR = nf90_inquire_variable(FILE%id, var_ncid, dimids = dims_in_file)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_var",                                   &
                        TRIM(FILE%NAME) // ": " //                             &
                        "var_name =" // TRIM(var_name) // ": " //              &
                        "Error getting dimensions for variable", ERROR)

  ! Check that the given dimension ids match
  DO n = 1,SIZE(dims_in)
    IF ( .NOT. ANY(dims_in_file == dims_in(n)) )                               &
      CALL log_fatal("file_ncdf_def_var",                                      &
                     TRIM(FILE%NAME) // ": " //                                &
                     "var_name =" // TRIM(var_name) // ": " //                 &
                     "Given dimension ids do not match file")
  END DO

  ! Check that the variable is defined with the record dimension if it is
  ! supposed to be
  IF ( is_record .AND. ( .NOT. ANY(dims_in_file == FILE%record_dim) ) )        &
    CALL log_fatal("file_ncdf_def_var",                                        &
                   TRIM(FILE%NAME) // ": " //                                  &
                   "var_name =" // TRIM(var_name) // ": " //                   &
                   "Variable not defined with record dimension in file")

  ! Verification is over - the variable is defined as we were told it was
  DEALLOCATE(dims_in_file)

CASE ( mode_write )
  !-----------------------------------------------------------------------------
  ! In write mode, we need to define the variable
  !-----------------------------------------------------------------------------

  ! Construct a new dimensions array that has the record dimension appended
  ! if required
  ALLOCATE(dims_in_file(ndims))
  dims_in_file(1:SIZE(dims_in)) = dims_in(:)
  IF ( is_record ) dims_in_file(ndims) = FILE%record_dim

  ! Define the variable
  ERROR = nf90_def_var(                                                        &
    FILE%id, var_name, nf90_float, dims_in_file, var_ncid                      &
  )
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_var",                                   &
                        TRIM(FILE%NAME) // ": " //                             &
                        "var_name =" // TRIM(var_name) // ": " //              &
                        "Error defining variable", ERROR)

  DEALLOCATE(dims_in_file)

#if !defined(MPI_DUMMY)
  ! If in parallel mode, make variable access collective
  IF ( FILE%PARALLEL ) THEN
    ERROR = nf90_var_par_access(FILE%id, var_ncid, nf90_collective)
    IF ( ERROR /= nf90_noerr )                                                 &
      CALL log_fatal_ncdf("file_ncdf_def_var",                                 &
                          TRIM(FILE%NAME) // ": " //                           &
                          "var_name =" // TRIM(var_name) // ": " //            &
                          "Error setting collective access", ERROR)
  END IF
#endif

  ! No default case as we already know that mode_read and mode_write are
  ! the only options
END SELECT

!-----------------------------------------------------------------------------
! Set up the var_ncdf object corresponding to this variable and store it
!-----------------------------------------------------------------------------
IF ( FILE%nvars >= max_var_file )                                              &
  CALL log_fatal("file_ncdf_def_var",                                          &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "Too many variables in file - try increasing max_var_file")

FILE%nvars = FILE%nvars + 1

! The return value is the index of the variable in the vars array on the
! file_ncdf object
var_id = FILE%nvars
FILE%vars(var_id)%id        = var_ncid
FILE%vars(var_id)%ndims     = SIZE(dims_in)
FILE%vars(var_id)%is_record = is_record

RETURN

END FUNCTION file_ncdf_def_var
#endif
