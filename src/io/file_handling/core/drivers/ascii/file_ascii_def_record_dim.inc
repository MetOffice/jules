#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ascii_def_record_dim(FILE, dim_name) RESULT(dim_id)

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
TYPE(file_ascii), INTENT(IN OUT) :: FILE
                                ! The file to define the dimension on
CHARACTER(LEN=*), INTENT(IN) :: dim_name
                                ! The name of the record dimension

! Return type
INTEGER :: dim_id               ! The dimension id


!-----------------------------------------------------------------------------

IF ( .NOT. FILE%define_mode )                                                  &
  CALL log_fatal("file_ascii_def_record_dim",                                  &
                 "Cannot define record dimension - file is not in define mode")

! Just indicate that the file has a record dimension
FILE%has_record_dim = .TRUE.
FILE%record_dim_name = dim_name

! Return -1 as the dimension id, since the return value should never be used
! and -1 can never be a valid dimension id for a non-record dimension
dim_id = -1

RETURN

END FUNCTION file_ascii_def_record_dim
#endif
