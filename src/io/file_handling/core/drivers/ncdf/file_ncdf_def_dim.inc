#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ncdf_def_dim(FILE, dim_name, dim_len) RESULT(dim_id)

USE io_constants, ONLY: max_sdf_name_len, mode_read, mode_write

USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension, nf90_def_dim

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines a dimension on the given file, returning the dimension id
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
                                ! The file to define the dimension on
CHARACTER(LEN=*), INTENT(IN) :: dim_name
                                ! The name of the dimension
INTEGER, INTENT(IN) :: dim_len  ! The length of the dimension

! Return type
INTEGER :: dim_id               ! The dimension id

! Work variables
INTEGER :: ERROR  ! The current error code (if any)
INTEGER :: len_in_file  ! The length of the dimension in the file (only
                        ! used in read mode to validate dimension size)


!-----------------------------------------------------------------------------


! Check the dimension has a sensible size (i.e. > 0)
IF ( dim_len < 1 )                                                             &
  CALL log_fatal("file_ncdf_def_dim",                                          &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "Dimension size should be >= 1 - size of " //                 &
                 TRIM(to_string(dim_len)) // " given for dimension " //        &
                 TRIM(dim_name))

! We do different things depending on whether we are open for reading
! or writing
SELECT CASE ( FILE%mode )
CASE ( mode_read )
  ! In read mode, we just get the id of the dimension and validate its size
  ERROR = nf90_inq_dimid(FILE%id, dim_name, dim_id)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_dim",                                   &
                        TRIM(FILE%NAME) // ": " //                             &
                        "Error getting dimension id for dimension: " //        &
                        TRIM(dim_name), ERROR)

  ERROR = nf90_inquire_dimension(FILE%id, dim_id, LEN = len_in_file)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_dim",                                   &
                        TRIM(FILE%NAME) // ": " //                             &
                        "dim_name =" // TRIM(dim_name) // ": " //              &
                        "Error getting dimension size", ERROR)
  IF ( dim_len /= len_in_file )                                                &
    CALL log_fatal("file_ncdf_def_dim",                                        &
                   TRIM(FILE%NAME) // ": " //                                  &
                   "dim_name =" // TRIM(dim_name) // ": " //                   &
                   "Dimension length mismatch - length in file: " //           &
                   TRIM(to_string(len_in_file)) // ", expected: " //           &
                   TRIM(to_string(dim_len)))

CASE ( mode_write )
  ! In write mode, we define the dimension
  ERROR = nf90_def_dim(FILE%id, dim_name, dim_len, dim_id)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_dim",                                   &
                        TRIM(FILE%NAME) // ": " //                             &
                        "dim_name =" // TRIM(dim_name) // ": " //              &
                        "Error defining dimension", ERROR)

  ! No default case as we already know that mode_read and mode_write are
  ! the only options
END SELECT

RETURN

END FUNCTION file_ncdf_def_dim
#endif
