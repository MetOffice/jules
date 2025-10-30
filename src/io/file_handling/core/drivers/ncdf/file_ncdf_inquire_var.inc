#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_inquire_var(FILE, var_name, var_id, ndims, dim_ids, is_record)

USE netcdf, ONLY: nf90_inq_varid, nf90_inquire_variable

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given a variable name, returns its id and information about dimensions
!   If the returned id < 0, the variable doesn't exist
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
CHARACTER(LEN=*), INTENT(IN) :: var_name  ! The name of the variable to
                                          ! inquire about
INTEGER, INTENT(OUT) :: var_id  ! The id of the variable in the file
INTEGER, INTENT(OUT) :: ndims  ! The number of dimensions that
                               ! the variable has
INTEGER, INTENT(OUT) :: dim_ids(:)  ! The dimension ids
LOGICAL, INTENT(OUT) :: is_record  ! Indicates if the variable
                                   ! uses the record dimension

! Work variables
INTEGER :: var_id_nc  ! The variable id in the NetCDF file

INTEGER :: i, j  ! Index variables

INTEGER :: ERROR  ! The current error code (if any)


!-----------------------------------------------------------------------------

! The default is to assume that we won't find the variable
var_id = -1

! To recover all the required information about a variable in a NetCDF file
! needs two steps (assuming we have been provided with enough space in the
! dim_ids array)

! First, recover the variable id in the file from the name
ERROR = nf90_inq_varid(FILE%id, var_name, var_id_nc)
! If there was an error retrieving the variable id, then the variable doesn't
! exist in the underlying file and we have nothing more to do
IF ( ERROR /= nf90_noerr ) RETURN

! The actual returned var_id is the index in the file%vars array
! So convert it now, in case the variable we just found is one that we are
! not 'technically' aware of (i.e. it has not been specified using file_def_var)
DO i = 1,FILE%nvars
  IF ( FILE%vars(i)%id == var_id_nc ) THEN
    var_id = i
    EXIT
  END IF
END DO

! If we are not aware of the variable, we are done
IF ( var_id < 1 ) RETURN

! Now we can get the information about dimensions
! We assume we have been allocated enough space to return the dimension ids
ERROR = nf90_inquire_variable(FILE%id, var_id_nc, ndims = ndims, dimids = dim_ids)
IF ( ERROR /= nf90_noerr )                                                     &
  CALL log_fatal_ncdf("file_ncdf_inquire_var",                                 &
                      TRIM(FILE%NAME) // ": " //                               &
                      "var_name =" // TRIM(var_name) // ": " //                &
                      "Error recovering variable dimensions", ERROR)

! If the record dimension appears in the list of dimension ids, then we
! need to remove it
j = 0
DO i = 1,ndims
  IF ( dim_ids(i) == FILE%record_dim ) CYCLE

  j = j + 1
  dim_ids(j) = dim_ids(i)
END DO
! We will have j dimensions left
ndims = j

! Lastly, indicate if the variable uses the record dimension
is_record = FILE%vars(var_id)%is_record

RETURN

END SUBROUTINE file_ncdf_inquire_var
#endif
