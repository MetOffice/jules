#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_inquire_var(FILE, var_name, var_id, ndims, dim_ids, is_record)

USE io_constants, ONLY: max_dim_file, max_file_name_len

USE driver_ascii_mod, ONLY: file_ascii_inquire_var
USE driver_ncdf_mod, ONLY: file_ncdf_inquire_var

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
TYPE(file_handle), INTENT(IN) :: FILE  ! The file to inspect
CHARACTER(LEN=*), INTENT(IN) :: var_name  ! The name of the variable to
                                          ! inquire about
INTEGER, INTENT(OUT) :: var_id  ! The id of the variable in the file
INTEGER, INTENT(OUT), OPTIONAL :: ndims  ! The number of dimensions that
                                         ! the variable has
INTEGER, INTENT(OUT), OPTIONAL :: dim_ids(:)  ! The dimension ids
LOGICAL, INTENT(OUT), OPTIONAL :: is_record  ! Indicates if the variable
                                             ! uses the record dimension


! Work variables
! Local versions of the optional arguments to pass to the underlying
! implementations
INTEGER :: ndims_local
INTEGER :: dim_ids_local(max_dim_file)
LOGICAL :: is_record_local
! Other local variables.
CHARACTER(LEN=max_file_name_len) :: file_name

!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  file_name = FILE % ascii % NAME
  CALL file_ascii_inquire_var(                                                 &
    FILE%ascii, var_name, var_id,                                              &
    ndims_local, dim_ids_local, is_record_local                                &
  )

CASE ( driver_ncdf )
  file_name = FILE % ncdf % NAME
  CALL file_ncdf_inquire_var(                                                  &
    FILE%ncdf, var_name, var_id,                                               &
    ndims_local, dim_ids_local, is_record_local                                &
  )

CASE DEFAULT
  CALL log_fatal("file_inquire_var",                                           &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

! Check that the variable was found.
IF ( var_id < 1 ) CALL log_fatal( "file_inquire_var",                          &
      "Variable " // TRIM(var_name) // " not in file " // TRIM(file_name) )

! Copy the returned values into the optional arguments if present
IF ( PRESENT(ndims) ) ndims = ndims_local
IF ( PRESENT(dim_ids) ) dim_ids(1:ndims) = dim_ids_local(1:ndims)
IF ( PRESENT(is_record) ) is_record = is_record_local

RETURN

END SUBROUTINE file_inquire_var
#endif
