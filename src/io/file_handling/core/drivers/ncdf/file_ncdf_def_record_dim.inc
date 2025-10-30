#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ncdf_def_record_dim(FILE, dim_name) RESULT(dim_id)

USE io_constants, ONLY: max_sdf_name_len, mode_read, mode_write

USE netcdf, ONLY: nf90_inq_dimid, nf90_def_dim, nf90_unlimited

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines the record dimension on the given file, returning the dimension id
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

! Return type
INTEGER :: dim_id               ! The dimension id

! Work variables
INTEGER :: ERROR  ! The current error code (if any)


!-----------------------------------------------------------------------------

! We do different things depending on whether we are open for reading
! or writing
SELECT CASE ( FILE%mode )
CASE ( mode_read )
  ! In read mode, we just get the id of the named dimension
  ! We don't actually care whether it is a 'proper' unlimited dimension or not,
  ! since we won't be writing to it
  ERROR = nf90_inq_dimid(FILE%id, dim_name, dim_id)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_record_dim",                            &
                        TRIM(FILE%NAME) // ": " //                             &
                        "dim_name =" // TRIM(dim_name) // ": " //              &
                        "Error getting dimension id", ERROR)

CASE ( mode_write )
  ! In write mode, we define an unlimited dimension
  ERROR = nf90_def_dim(FILE%id, dim_name, nf90_unlimited, dim_id)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_def_record_dim",                            &
                        TRIM(FILE%NAME) // ": " //                             &
                        "dim_name =" // TRIM(dim_name) // ": " //              &
                        "Error defining unlimited dimension", ERROR)

  ! No default case as we already know that mode_read and mode_write are
  ! the only options
END SELECT

! Store the id of the record dimension for future use
FILE%record_dim = dim_id

RETURN

END FUNCTION file_ncdf_def_record_dim
#endif
