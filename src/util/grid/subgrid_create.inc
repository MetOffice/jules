#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION subgrid_create_region(parent, x_start, y_start, nx, ny) RESULT(subgrid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a subgrid_info object representing the specified region
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(grid_info), INTENT(IN) :: parent  ! The parent grid

INTEGER, INTENT(IN) :: x_start, y_start, nx, ny
                                       ! The start and extent of the region
                                       ! the subgrid will cover

! Return type
TYPE(subgrid_info) :: subgrid  ! The subgrid object

! Work variables
INTEGER :: vertices(4,2)  ! The x/y coordinates of each of the 4 vertices
                          ! of the region, as determined by the starts and
                          ! extents


!-----------------------------------------------------------------------------


! Set the subgrid parent in all cases
subgrid%parent = parent

!-----------------------------------------------------------------------------
! If negative or 0 nx/ny have been specified, this results in an empty
! subgrid (i.e. return the subgrid in its default state)
!-----------------------------------------------------------------------------
IF ( nx < 1 .OR. ny < 1 ) RETURN

!-----------------------------------------------------------------------------
! Check that the subgrid lies within the parent grid
!-----------------------------------------------------------------------------
! Construct the vertices that define the region
vertices(1,:) = [ x_start,          y_start ]
vertices(2,:) = [ x_start + nx - 1, y_start ]
vertices(3,:) = [ x_start,          y_start + ny - 1 ]
vertices(4,:) = [ x_start + nx - 1, y_start + ny - 1 ]

! Check that all the values are within the bounds of the parent grid
IF ( ANY(vertices < 1) .OR.                                                    &
     ANY(vertices(:,1) > parent%nx) .OR. ANY(vertices(:,2) > parent%ny) )      &
  CALL log_fatal("subgrid_create_region",                                      &
                 "Specified region contains points beyond the bounds " //      &
                 "of the parent grid")

!-----------------------------------------------------------------------------
! Build and return the subgrid
!-----------------------------------------------------------------------------
subgrid%nx      = nx
subgrid%ny      = ny

subgrid%x_start = x_start
subgrid%y_start = y_start

RETURN

END FUNCTION subgrid_create_region


!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************


FUNCTION subgrid_create_points(parent, points) RESULT(subgrid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a subgrid_info object from the specified points
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(grid_info), INTENT(IN) :: parent  ! The parent grid

INTEGER, INTENT(IN) :: points(:,:)  ! The indices in the parent grid of
                                    ! the points in the subgrid
                                    ! The shape of this argument will
                                    ! determine the shape of the subgrid

! Return type
TYPE(subgrid_info) :: subgrid  ! The subgrid object


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that the subgrid lies within the parent grid
!-----------------------------------------------------------------------------
! Check that all the values are within the bounds of the parent grid
IF ( ANY(points < 1) .OR. ANY(points > (parent%nx * parent%ny)) )              &
  CALL log_fatal("subgrid_create_points",                                      &
                 "Points beyond the bounds of the parent grid have " //        &
                 "been specified")

!-----------------------------------------------------------------------------
! Build and return the subgrid
!-----------------------------------------------------------------------------
subgrid%parent  = parent

subgrid%nx      = SIZE(points, 1)
subgrid%ny      = SIZE(points, 2)

ALLOCATE(subgrid%points(subgrid%nx,subgrid%ny))
subgrid%points(:,:) = points(:,:)

RETURN

END FUNCTION subgrid_create_points


!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************


FUNCTION subgrid_create_mask(parent, mask, force_1d_grid) RESULT(subgrid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a subgrid_info object from the specified mask
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(grid_info), INTENT(IN) :: parent  ! The parent grid

LOGICAL, INTENT(IN) :: mask(:,:)  ! Mask indicating which points in the
                                  ! parent grid to use
                                  ! This should be the same size as the
                                  ! parent grid

LOGICAL, INTENT(IN) :: force_1d_grid  ! T - model grid is 1D
                          ! F - model grid is 1D unless input grid is 2D
                          !     and model grid is the whole input grid

! Return type
TYPE(subgrid_info) :: subgrid  ! The subgrid object


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! When using a mask to create a subgrid, we would like to use a region
! if possible
! So that the logic to detect this is contained in one place, we create a
! "null subgrid" - a subgrid specified as a region that covers the whole
! grid - and just pass that to subgrid_restrict with the same mask
!-----------------------------------------------------------------------------
! By specifying x/y_start = 1 and nx/ny the same as the parent grid, we create
! a "null subgrid"
subgrid = subgrid_restrict(                                                    &
  subgrid_create(parent, 1, 1, parent%nx, parent%ny), mask, force_1d_grid      &
)

RETURN

END FUNCTION subgrid_create_mask
#endif
