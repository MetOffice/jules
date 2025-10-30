#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION subgrid_restrict_region(parent, x_start, y_start, nx, ny) RESULT(subgrid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a subgrid_info object and returns a subgrid_info object that is
!   equivalent to extracting the specified region from the original subgrid
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(subgrid_info), INTENT(IN) :: parent  ! The parent subgrid

INTEGER, INTENT(IN) :: x_start, y_start, nx, ny
                                       ! The start and extent of the region
                                       ! the subgrid will cover

! Return type
TYPE(subgrid_info) :: subgrid  ! The restricted subgrid object

! Work variables
INTEGER :: vertices(4,2)  ! The x/y coordinates of each of the 4 vertices
                          ! of the region, as determined by the starts and
                          ! extents


!-----------------------------------------------------------------------------


! Set the subgrid parent to be the actual grid that is the parent of the
! parent subgrid
subgrid%parent = parent%parent

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
  CALL log_fatal("subgrid_restrict_region",                                    &
                 "Specified region contains points beyond the bounds " //      &
                 "of the parent grid")

!-----------------------------------------------------------------------------
! Build and return the subgrid
!-----------------------------------------------------------------------------
subgrid%nx = nx
subgrid%ny = ny

IF ( ASSOCIATED(parent%points) ) THEN
  ! If the parent subgrid is specified using points, then our new subgrid can
  ! only be specified using points
  ! We extract the points from the parent subgrid associated with the specified
  ! region
  ALLOCATE(subgrid%points(subgrid%nx,subgrid%ny))
  subgrid%points(:,:) = parent%points(vertices(1,1):vertices(4,1),             &
                                      vertices(1,2):vertices(4,2))
ELSE
  ! If the parent subgrid is specified using a region, then we can just restrict
  ! that region by offsetting the given x/y_start by the parent x/y_start
  subgrid%x_start = parent%x_start + x_start - 1
  subgrid%y_start = parent%y_start + y_start - 1
END IF

RETURN

END FUNCTION subgrid_restrict_region


!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************


FUNCTION subgrid_restrict_points(parent, points) RESULT(subgrid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a subgrid_info object and returns a subgrid_info object that is
!   equivalent to extracting the specified points from the original subgrid
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(subgrid_info), INTENT(IN) :: parent  ! The parent subgrid

INTEGER, INTENT(IN) :: points(:,:)  ! The indices in the parent subgrid of
                                    ! the points in the restricted subgrid
                                    ! The shape of this argument will
                                    ! determine the shape of the restricted
                                    ! subgrid

! Return type
TYPE(subgrid_info) :: subgrid  ! The restricted subgrid object

! Work variables
INTEGER :: i, j, x, y  ! Index variables


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that the subgrid lies within the parent subgrid
!-----------------------------------------------------------------------------
! Check that all the values are within the bounds of the parent grid
IF ( ANY(points < 1) .OR. ANY(points > (parent%nx * parent%ny)) )              &
  CALL log_fatal("subgrid_restrict_points",                                    &
                 "Points beyond the bounds of the parent grid have " //        &
                 "been specified")

!-----------------------------------------------------------------------------
! Build and return the subgrid
!-----------------------------------------------------------------------------
! The parent of the new subgrid is the same as the old subgrid
subgrid%parent  = parent%parent

! The size of the new subgrid is determined by the size of the points array
subgrid%nx = SIZE(points, 1)
subgrid%ny = SIZE(points, 2)

ALLOCATE(subgrid%points(subgrid%nx,subgrid%ny))

IF ( ASSOCIATED(parent%points) ) THEN
  ! If the parent subgrid is specified using points, then we construct our new
  ! subgrid by extracting those points from the parent subgrid
  DO j = 1,subgrid%ny
    DO i = 1,subgrid%nx
      ! Convert the 1D index in points into x/y indices in the parent subgrid
      y = (points(i,j) - 1) / parent%nx + 1
      x = points(i,j) - (y-1) * parent%nx

      subgrid%points(i,j) = parent%points(x,y)
    END DO
  END DO
ELSE
  ! If the parent subgrid is specified using a region, we construct our subgrid
  ! by offsetting each specified point using the parent subgrid's x_start and
  ! y_start
  DO j = 1,subgrid%ny
    DO i = 1,subgrid%nx
      ! Convert the 1D index in points into x/y indices in the parent subgrid
      y = (points(i,j) - 1) / parent%nx + 1
      x = points(i,j) - (y-1) * parent%nx

      ! Offset those indices by the parent subgrid's x/y_start to get the x/y indices
      ! in the full parent grid
      x = x + parent%x_start - 1
      y = y + parent%y_start - 1

      ! Convert back to a 1D index in the full parent grid
      subgrid%points(i,j) = (y-1) * subgrid%parent%nx + x
    END DO
  END DO
END IF

RETURN

END FUNCTION subgrid_restrict_points


!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************


FUNCTION subgrid_restrict_mask(parent, mask, force_1d_grid) RESULT(subgrid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a subgrid_info object and returns a subgrid_info object that is
!   equivalent to extracting the points from the original subgrid where
!   mask is .TRUE.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(subgrid_info), INTENT(IN) :: parent  ! The parent subgrid

LOGICAL, INTENT(IN) :: mask(:,:)  ! Mask indicating which points in the
                                  ! parent grid to use
                                  ! This should be the same size as the
                                  ! parent grid

LOGICAL, INTENT(IN) :: force_1d_grid  ! T - model grid is 1D
                          ! F - model grid is 1D unless input grid is 2D
                          !     and model grid is the whole input grid

! Return type
TYPE(subgrid_info) :: subgrid  ! The subgrid object

! Work variables
INTEGER :: nx, ny  ! The dimensions of the subgrid

INTEGER :: x_start, y_start  ! The index of the first column/row to contain
                             ! a .TRUE. value

INTEGER :: x_end, y_end  ! The index of the last column/row to contain
                         ! a .TRUE. value

INTEGER, ALLOCATABLE :: points(:,:)  ! The indices of the .TRUE. points
                                     ! in mask

INTEGER :: i, j, n  ! Index variables


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! First, verify that the mask is applicable to the parent subgrid
!-----------------------------------------------------------------------------
IF ( SIZE(mask,1) /= parent%nx .OR. SIZE(mask,2) /= parent%ny )                &
  CALL log_fatal("subgrid_restrict_mask",                                      &
                 "Mask must have the same dimensions as the grid it is " //    &
                 "being applied to")

!-----------------------------------------------------------------------------
! If mask contains no .TRUE. points, we return a 0 size subgrid by specifying
! a region with nx = ny = 0
!-----------------------------------------------------------------------------
IF ( .NOT. ANY(mask) ) THEN
  subgrid = subgrid_restrict(parent, 1, 1, 0, 0)
  RETURN
END IF

!-----------------------------------------------------------------------------
! Try to detect if the mask specifies a region - we do this because using a
! subgrid is more efficient (particularly when reading/writing subgrids
! during I/O)
!
! The fallback is to use a list of points
!-----------------------------------------------------------------------------
! Find the first column that contains a .TRUE. in mask
DO n = 1,parent%nx
  IF ( ANY(mask(n,:)) ) THEN
    x_start = n
    EXIT
  END IF
END DO

! Find the last column that contains a .TRUE.
DO n = parent%nx,1,-1
  IF ( ANY(mask(n,:)) ) THEN
    x_end = n
    EXIT
  END IF
END DO

! Find the first row that contains a .TRUE. in mask
DO n = 1,parent%ny
  IF ( ANY(mask(:,n)) ) THEN
    y_start = n
    EXIT
  END IF
END DO

! Find the last row that contains a .TRUE.
DO n = parent%ny,1,-1
  IF ( ANY(mask(:,n)) ) THEN
    y_end = n
    EXIT
  END IF
END DO

!-----------------------------------------------------------------------------
! We have found the extents of the .TRUE. points in mask (i.e. mask has no
! .TRUE. points outside of the bounds we have found
!
! If all the points within the bounds we have found are also .TRUE., we have
! a region
! Otherwise, we fall back to a list of points
!-----------------------------------------------------------------------------
IF ( ALL(mask(x_start:x_end,y_start:y_end)) .AND.                              &
    (parent%ny == 1 .OR. ( .NOT. force_1d_grid )) ) THEN
  ! Calculate the nx/ny we will pass to the region overload of subgrid_restrict
  ! to create the subgrid
  ! This is inclusive at both ends
  nx = x_end - x_start + 1
  ny = y_end - y_start + 1

  subgrid = subgrid_restrict(parent, x_start, y_start, nx, ny)
ELSE
  ! Build a list of points to pass to the points overload of subgrid_restrict
  nx = COUNT(mask)
  ny = 1

  ALLOCATE(points(nx,ny))

  n = 0
  DO j = 1,parent%ny
    DO i = 1,parent%nx
      IF ( mask(i,j) ) THEN
        n = n + 1
        points(n,1) = (j-1) * parent%nx + i
      END IF
    END DO
  END DO

  subgrid = subgrid_restrict(parent, points)

  DEALLOCATE(points)
END IF

RETURN

END FUNCTION subgrid_restrict_mask


!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************
!*****************************************************************************


FUNCTION subgrid_restrict_subgrid(parent, subgrid) RESULT(new_subgrid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes two subgrid objects and combines them into a new subgrid. The size
!   of the parent grid of the second subgrid must match the size of the first
!   subgrid
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(subgrid_info), INTENT(IN) :: parent  ! The parent subgrid

TYPE(subgrid_info), INTENT(IN) :: subgrid  ! The subgrid to use to restrict
                                           ! the parent subgrid

! Return type
TYPE(subgrid_info) :: new_subgrid  ! The combined subgrid object

! Work variables


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! First, verify that the combination of subgrids can be done
!-----------------------------------------------------------------------------
IF ( subgrid%parent%nx /= parent%nx .OR. subgrid%parent%ny /= parent%ny )      &
  CALL log_fatal("subgrid_restrict_subgrid",                                   &
                 "Grid dimensions are incompatible")

!-----------------------------------------------------------------------------
! From this point, we can defer to one of the other subgrid_restrict_*
! variants, depending on whether the second subgrid is restricting by
! region or points
!-----------------------------------------------------------------------------
IF ( ASSOCIATED(subgrid%points) ) THEN
  new_subgrid = subgrid_restrict(parent, subgrid%points)
ELSE
  new_subgrid = subgrid_restrict(                                              &
    parent, subgrid%x_start, subgrid%y_start, subgrid%nx, subgrid%ny           &
  )
END IF

RETURN

END FUNCTION subgrid_restrict_subgrid
#endif
