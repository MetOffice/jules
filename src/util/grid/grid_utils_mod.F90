#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE grid_utils_mod

USE io_constants, ONLY: max_sdf_name_len

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Type definitions
!-----------------------------------------------------------------------------
TYPE :: grid_info
  !-----------------------------------------------------------------------------
  ! This type encapsulates information about a grid
  !-----------------------------------------------------------------------------
  LOGICAL :: is_1d = .FALSE.

  CHARACTER(LEN=max_sdf_name_len) :: dim_name = ""
                               ! Used if is_1d = T
                               ! The name of the single grid dimension
  CHARACTER(LEN=max_sdf_name_len) :: x_name = "", y_name = ""
                               ! Used if is_1d = F
                               ! The names of the x and y dimensions of
                               ! the grid respectively

  INTEGER :: nx = 0, ny = 0  ! The sizes of the x and y dimensions of the grid
                             ! For 1d grids, ny = 1

END TYPE grid_info


TYPE :: subgrid_info

  ! Always set
  TYPE(grid_info) :: parent  ! The grid that this is a subgrid of

  INTEGER :: nx = 0, ny = 0  ! The size of the subgrid
                             ! For 1D subgrids, ny = 1

  ! Used if subgrid is constructed by specifying a region
  INTEGER :: x_start = -1, y_start = -1  ! The indices of the corner of the
                                         ! region
                                         ! The region consists of the area
                                         ! enclosed by the points
                                         !   (x_start,          y_start)
                                         !   (x_start + nx - 1, y_start)
                                         !   (x_start,          y_start + ny - 1)
                                         !   (x_start + nx - 1, y_start + ny - 1)

  ! Used if subgrid is constructed from point indices
  INTEGER, POINTER :: points(:,:) => NULL()
                                   ! For each point in the subgrid, this
                                   ! specifies the corresponding index in
                                   ! the parent grid
                                   ! For 2D grids, the first row is represented
                                   ! by indices 1:nx, the second by nx+1:2nx,
                                   ! the third by 2nx+1:3nx etc.

END TYPE subgrid_info

!-----------------------------------------------------------------------------
! Operator declarations
!-----------------------------------------------------------------------------
INTERFACE OPERATOR ( == )
  MODULE PROCEDURE grid_eq
END INTERFACE

INTERFACE OPERATOR ( /= )
  MODULE PROCEDURE grid_ne
END INTERFACE

!-----------------------------------------------------------------------------
! Overloads
!-----------------------------------------------------------------------------
! There are several ways to create a subgrid - we want to use them all by the
! same name externally
INTERFACE subgrid_create
  MODULE PROCEDURE subgrid_create_mask, subgrid_create_region,                 &
                   subgrid_create_points
END INTERFACE subgrid_create

! A subgrid can also be further restricted in several ways - we also want to
! use them all be the same name
INTERFACE subgrid_restrict
  MODULE PROCEDURE subgrid_restrict_mask, subgrid_restrict_region,             &
                   subgrid_restrict_points, subgrid_restrict_subgrid
END INTERFACE subgrid_restrict

!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                         &
! Public types
    grid_info, subgrid_info,                                                   &
! Public operators
    OPERATOR ( == ), OPERATOR ( /= ),                                          &
! Public procedures
    grid_create, subgrid_create, subgrid_restrict, subgrid_extract


CONTAINS


! Includes for grid functionality
#include "grid_create.inc"
#include "grid_operators.inc"

! Includes for subgrid functionality
#include "subgrid_create.inc"
#include "subgrid_restrict.inc"
#include "subgrid_extract.inc"

END MODULE grid_utils_mod
#endif
