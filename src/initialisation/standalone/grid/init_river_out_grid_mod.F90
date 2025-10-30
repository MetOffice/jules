#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_river_out_grid_mod

IMPLICIT NONE

PRIVATE ! private scope by default
PUBLIC init_river_out_grid

CONTAINS

SUBROUTINE init_river_out_grid()
!-----------------------------------------------------------------------------
! Description:
!   Defines the river output grid
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE output_mod, ONLY: river_out_grid

USE jules_rivers_mod, ONLY: np_rivers

USE grid_utils_mod, ONLY: grid_create

IMPLICIT NONE

! Create a 1-d output grid of length np_rivers
! We don't need x and y dimensions
river_out_grid = grid_create( .TRUE., "np_rivers", np_rivers, "", 0, "", 0)

END SUBROUTINE init_river_out_grid

END MODULE init_river_out_grid_mod
#endif
