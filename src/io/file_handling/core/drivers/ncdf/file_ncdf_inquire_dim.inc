#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_inquire_dim(FILE, dim_name, dim_id, dim_len, is_record_dim)

USE netcdf, ONLY: nf90_inq_dimid, nf90_inquire_dimension

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given a dimension name, returns its id and its size
!   If the returned id < 0, the dimension doesn't exist
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ncdf), INTENT(IN) :: FILE  ! The file to inspect
CHARACTER(LEN=*), INTENT(IN) :: dim_name  ! The name of the dimension to
                                          ! inquire about
INTEGER, INTENT(OUT) :: dim_id  ! The id of the dimension in the file
INTEGER, INTENT(OUT) :: dim_len  ! The length of the dimension in the
LOGICAL, INTENT(OUT) :: is_record_dim  ! Indicates if the named dimension
                                       ! is the record dimension for the file

! Work variables
INTEGER :: ERROR  ! The current error code (if any)


!-----------------------------------------------------------------------------


is_record_dim = .FALSE.

! To recover all the required information about a dimension in a NetCDF file
! needs two steps

! First, recover the dimension id from the name
ERROR = nf90_inq_dimid(FILE%id, dim_name, dim_id)
IF ( ERROR /= nf90_noerr ) THEN
  ! If there was an error retrieving the dimension id, return a non-existent
  ! value
  dim_id = -1
  RETURN
END IF

! If the dimension is the record dimension, we can return early
IF ( dim_id == FILE%record_dim ) THEN
  is_record_dim = .TRUE.
  RETURN
END IF

! If we have a non-record dim, then recover the dim size from the id
ERROR = nf90_inquire_dimension(FILE%id, dim_id, LEN = dim_len)
IF ( ERROR /= nf90_noerr )                                                     &
  CALL log_fatal_ncdf("file_ncdf_inquire_dim",                                 &
                      TRIM(FILE%NAME) // ": "     //                           &
                      "dim_name =" // TRIM(dim_name) // ": " //                &
                      "Error recovering dimension size", ERROR)

RETURN

END SUBROUTINE file_ncdf_inquire_dim
#endif
