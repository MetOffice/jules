#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_gridded_def_record_dim(FILE, dim_name) RESULT(dim_id)

USE file_mod, ONLY: file_def_record_dim

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines a record dimension on the given file, returning the dimension id
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_gridded), INTENT(IN OUT) :: FILE
                                ! The file to define the dimension on
CHARACTER(LEN=*), INTENT(IN) :: dim_name
                                ! The name of the record dimension

! Return type
INTEGER :: dim_id               ! The dimension id


!-----------------------------------------------------------------------------

! Just defer to the underlying file
dim_id = file_def_record_dim(FILE%fh, dim_name)

RETURN

END FUNCTION file_gridded_def_record_dim
#endif
