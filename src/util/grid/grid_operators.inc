#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


LOGICAL FUNCTION grid_eq(grid, other_grid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Determines if two grids are equal. Currently, two grids are considered
!   equal if they have the same dimensions (i.e. dimension names are not
!   taken into account).
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(grid_info), INTENT(IN) :: grid, other_grid  ! The grids to compare


!-----------------------------------------------------------------------------


grid_eq = ( grid%nx == other_grid%nx ) .AND. ( grid%ny == other_grid%ny )

RETURN

END FUNCTION grid_eq



LOGICAL FUNCTION grid_ne(grid, other_grid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Determines if two grids are not equal.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(grid_info), INTENT(IN) :: grid, other_grid  ! The grids to compare


!-----------------------------------------------------------------------------


grid_ne = .NOT. grid_eq(grid, other_grid)

RETURN

END FUNCTION grid_ne
#endif
