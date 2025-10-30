#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_gridded_def_grid(FILE, grid)

USE file_mod, ONLY: file_def_dim

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines the grid used by variables in the file
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
    ! The file to define the grid on

TYPE(grid_info), INTENT(IN) :: grid  ! The grid to define


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Define the dimensions for the grid on the underlying file and store the
! dimension ids for later use
!-----------------------------------------------------------------------------
FILE%grid = grid

IF ( grid%is_1d ) THEN
  FILE%grid_dim_id  = file_def_dim(FILE%fh, grid%dim_name, grid%nx)
ELSE
  FILE%grid_x_dim_id = file_def_dim(FILE%fh, grid%x_name, grid%nx)
  FILE%grid_y_dim_id = file_def_dim(FILE%fh, grid%y_name, grid%ny)
END IF

RETURN

END SUBROUTINE file_gridded_def_grid
#endif
