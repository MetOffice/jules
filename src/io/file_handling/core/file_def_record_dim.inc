#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_def_record_dim(FILE, dim_name) RESULT(dim_id)

USE io_constants, ONLY: max_sdf_name_len

USE driver_ascii_mod, ONLY: file_ascii_def_record_dim
USE driver_ncdf_mod, ONLY: file_ncdf_def_record_dim

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
TYPE(file_handle), INTENT(IN OUT) :: FILE
                                ! The file to define the dimension on
CHARACTER(LEN=*), INTENT(IN) :: dim_name
                                ! The name of the record dimension

! Return type
INTEGER :: dim_id               ! The dimension id


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  dim_id = file_ascii_def_record_dim(FILE%ascii, dim_name)

CASE ( driver_ncdf )
  dim_id = file_ncdf_def_record_dim(FILE%ncdf, dim_name)

CASE DEFAULT
  CALL log_fatal("file_def_record_dim",                                        &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END FUNCTION file_def_record_dim
#endif
