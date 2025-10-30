#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ts_def_grid(FILE, grid)

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
TYPE(file_ts), INTENT(IN OUT) :: FILE
    ! The file to define the grid on

TYPE(grid_info), INTENT(IN) :: grid  ! The grid to define


!-----------------------------------------------------------------------------


! If we are not in define mode, error out
IF ( .NOT. FILE%define_mode )                                                  &
  CALL log_fatal("file_ts_def_grid",                                           &
                 "Cannot define grid - file is not in define mode")

FILE%grid = grid


RETURN

END SUBROUTINE file_ts_def_grid
#endif
