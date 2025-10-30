#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_gridded_def_dim(FILE, dim_name, dim_len) RESULT(dim_id)

USE file_mod, ONLY: file_def_dim

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines a non-grid (levels) dimension on the given file
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
    ! The name of the dimension
INTEGER, INTENT(IN) :: dim_len
    ! The size of the dimension

! Return type
INTEGER :: dim_id  ! The dimension id


!-----------------------------------------------------------------------------


! Check if we already have the maximum number of dimensions
IF ( FILE%ndims >= max_dim_file )                                              &
  CALL log_fatal("file_gridded_def_dim",                                       &
                 "Too many dimensions in file - try increasing max_dim_file")

! Define the dimension on the underlying file and store its id for later use
FILE%ndims = FILE%ndims + 1

! The returned dimension id is just the index in the dim_ids array
dim_id = FILE%ndims

FILE%dim_ids(dim_id)   = file_def_dim(FILE%fh, dim_name, dim_len)
FILE%dim_sizes(dim_id) = dim_len

RETURN

END FUNCTION file_gridded_def_dim
#endif
