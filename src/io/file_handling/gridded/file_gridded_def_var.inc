#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_gridded_def_var(FILE, var_name, levels_dims, is_record)          &
                                                                RESULT(var_id)

USE file_mod, ONLY: file_def_var

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines a variable in the given file, returning the variable id
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
                                ! The file to define the variable in
CHARACTER(LEN=*), INTENT(IN) :: var_name
                                  ! The name of the variable
INTEGER, INTENT(IN), OPTIONAL :: levels_dims(:)
                                  ! The ids of the dimensions to use for the
                                  ! vertical levels of the variable, if
                                  ! required
                                  ! If not given or an array of 0 length is
                                  ! given, no vertical levels are used
LOGICAL, INTENT(IN) :: is_record  ! Indicates whether the variable uses the
                                  ! record dimension

! Return type
INTEGER :: var_id               ! The variable id


! Work variables
INTEGER :: ndims_levs  ! The number of levels dimensions that the variable has
INTEGER :: ndims       ! The total number of dimensions that the variable has
INTEGER, ALLOCATABLE :: dim_ids(:)  ! The ids in the underlying file of the
                                    ! dimensions of the variable
                                    ! The variable has:
                                    !   * 1 or 2 dimensions for grid
                                    !   * 0 or more levels

INTEGER :: i  ! Loop index


!-----------------------------------------------------------------------------


! Check if we already have the maximum number of variables
IF ( FILE%nvars >= max_var_file )                                              &
  CALL log_fatal("file_gridded_def_var",                                       &
                 "Too many variables in file - try increasing max_var_file")

!-----------------------------------------------------------------------------
! Construct the array of dimension ids for the variable
!-----------------------------------------------------------------------------
! How many levels dimensions do we have
ndims_levs = 0
IF ( PRESENT(levels_dims) ) ndims_levs = SIZE(levels_dims)

! Construct the array of dimension ids
! Grid dimensions come first
IF ( FILE%grid%is_1d ) THEN
  ALLOCATE(dim_ids(1 + ndims_levs))
  ndims = 1
  dim_ids(1) = FILE%grid_dim_id
ELSE
  ALLOCATE(dim_ids(2 + ndims_levs))
  ndims = 2
  dim_ids(1:2) = [ FILE%grid_x_dim_id, FILE%grid_y_dim_id ]
END IF

DO i = 1,ndims_levs
  ndims = ndims + 1
  ! Each levels dim is an index in file%dim_ids, as returned by file_gridded_def_dim
  dim_ids(ndims) = FILE%dim_ids(levels_dims(i))
END DO

!-----------------------------------------------------------------------------
! Define the variable in the underlying file and store details for future use
!-----------------------------------------------------------------------------
FILE%nvars = FILE%nvars + 1

! The returned variable id is just the index in the var_ids array of the file
var_id = FILE%nvars

FILE%vars(var_id)%id = file_def_var(                                           &
  FILE%fh, var_name, dim_ids(1:ndims), is_record                               &
)

! Note that we can do this even when ndims_levs = 0 - we just get a zero-sized
! array
! This actually simplifies things in file_gridded_read/write_var as well
ALLOCATE(FILE%vars(var_id)%lev_sizes(ndims_levs))
DO i = 1,ndims_levs
  FILE%vars(var_id)%lev_sizes(i) = FILE%dim_sizes(levels_dims(i))
END DO

DEALLOCATE(dim_ids)

RETURN

END FUNCTION file_gridded_def_var
#endif
