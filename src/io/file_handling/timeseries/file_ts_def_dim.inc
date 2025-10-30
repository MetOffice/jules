#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ts_def_dim(FILE, dim_name, dim_len) RESULT(dim_id)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines a dimension on the given timeseries file, returning the dimension id
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
INTEGER, INTENT(IN) :: dim_len  ! The length of the dimension

! Return type
INTEGER :: dim_id               ! The dimension id


!-----------------------------------------------------------------------------

! If we are not in define mode, error out
IF ( .NOT. FILE%define_mode )                                                  &
  CALL log_fatal("file_ts_def_dim",                                            &
                 "Cannot define dimension - file is not in define mode")

! If adding another dimension will cause us to have too many dimensions,
! error out
IF ( FILE%ndims >= max_dim_file )                                              &
  CALL log_fatal("file_ts_def_dim",                                            &
                 "Too many dimensions in file - try increasing max_dim_file")

!-----------------------------------------------------------------------------
! Store the dimension attributes so that they can be used later to define
! dimensions on actual file(s)
!-----------------------------------------------------------------------------
FILE%ndims = FILE%ndims + 1

! The returned dimension id is just the index in the dims array on the file object
dim_id = FILE%ndims

FILE%dims(dim_id)%NAME   = dim_name
FILE%dims(dim_id)%length = dim_len

RETURN

END FUNCTION file_ts_def_dim
#endif
