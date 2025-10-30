#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE model_grid_mod

USE grid_utils_mod, ONLY: grid_info

IMPLICIT NONE

LOGICAL :: l_coord_latlon = .FALSE.
  ! Switch indicating the coordinate system for the full grid.
  ! TRUE: Coordinates are latitude and longitude
  ! FALSE: Other projection coordinates (not lat-lon).
  !   A rotated grid (which uses latitude and longitude relative to a rotated
  !   pole) should be indicated with l_coord_latlon=F.

!-----------------------------------------------------------------------------
! Variables for the sizes of the full grid
!-----------------------------------------------------------------------------
TYPE(grid_info), SAVE :: model_grid  ! The full model grid (i.e. combined grid
                                     ! for all MPI tasks)
                                     ! gfortran requires the SAVE attribute

INTEGER :: global_land_pts  ! The number of land points in the full model
                            ! grid

!-----------------------------------------------------------------------------
! Variables defined on the FULL model grid (i.e. all tasks combined)
!-----------------------------------------------------------------------------
LOGICAL, ALLOCATABLE :: global_land_mask(:,:)
                                      ! Land mask for all points in the
                                      ! FULL model grid (i.e. all tasks
                                      ! combined)

!-----------------------------------------------------------------------------
! Variables defined on only the points modelled by the current task
!-----------------------------------------------------------------------------
REAL, ALLOCATABLE :: latitude(:,:)  ! The latitude of model points for the
                                    ! current task
REAL, ALLOCATABLE :: longitude(:,:) ! The longitude of model points for the
                                    ! current task
REAL, ALLOCATABLE :: grid_area_ij(:,:) ! The area of each gridbox (m2).
REAL, ALLOCATABLE :: projection_x_coord(:,:)
                                    ! Projection coordinate in the x
                                    ! direction for each model point.
REAL, ALLOCATABLE :: projection_y_coord(:,:)
                                    ! Projection coordinate in the y
                                    ! direction for each model point.
REAL, ALLOCATABLE :: projection_x_coord_land(:)
  ! Projection x coordindates values of land points for the current task.
REAL, ALLOCATABLE :: projection_y_coord_land(:)
  ! Projection y coordindates values of land points for the current task.

  ! Note: If the model grid is defined in terms of geographic coordinates
  !   (latitude and longitude; as indicated by l_coord_latlon=TRUE), the
  !   so-called projection coordinate variables above will in fact hold copies
  !   of the geographic coordinates. The distinction only becomes important in
  !   the case of l_coord_latlon=FALSE.

END MODULE model_grid_mod
#endif
