#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ts_def_time_dim(FILE, dim_name) RESULT(dim_id)

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
TYPE(file_ts), INTENT(IN OUT) :: FILE
                                ! The file to define the dimension on
CHARACTER(LEN=*), INTENT(IN) :: dim_name
                                ! The name of the dimension

! Return type
INTEGER :: dim_id               ! The dimension id


!-----------------------------------------------------------------------------

! If we are not in define mode, error out
IF ( .NOT. FILE%define_mode )                                                  &
  CALL log_fatal("file_ts_def_time_dim",                                       &
                 "Cannot define time dimension - file is not in define mode")

! A file can only have one time dimension
IF ( FILE%has_time_dim )                                                       &
  CALL log_fatal("file_ts_def_time_dim",                                       &
                 "Time dimension has already been defined")

!-----------------------------------------------------------------------------
! Store the dimension attributes so that they can be used later to define
! the dimension on actual file(s)
!-----------------------------------------------------------------------------
FILE%has_time_dim = .TRUE.

FILE%time_dim%NAME   = dim_name
FILE%time_dim%length = -1  ! The time dimension does not have a length

! Return -1 as the dimension id, since the return value should never be used
! and -1 can never be a valid dimension id for a non-time dimension
dim_id = -1

RETURN

END FUNCTION file_ts_def_time_dim
#endif
