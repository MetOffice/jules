#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION subgrid_extract(subgrid, grid_data) RESULT(subgrid_data)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes data on the parent grid and a subgrid_info object, extracts the
!   data on the subgrid and returns it
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(subgrid_info), INTENT(IN) :: subgrid
                   ! The subgrid to extract
REAL, INTENT(IN) :: grid_data(subgrid%parent%nx,subgrid%parent%ny)
                   ! The data on the parent grid to extract from

! Return type
REAL :: subgrid_data(subgrid%nx,subgrid%ny)

! Work variables
INTEGER :: i, j, x, y  ! Index variables

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! How we extract data depends on whether the subgrid is defined as a region
! or a set of points
!-----------------------------------------------------------------------------
IF ( ASSOCIATED(subgrid%points) ) THEN
  DO j = 1,subgrid%ny
    DO i = 1,subgrid%nx
      ! Convert the point indices in subgrid%points into x/y coords in the parent grid
      y = (subgrid%points(i,j) - 1) / subgrid%parent%nx + 1
      x = subgrid%points(i,j) - (y-1) * subgrid%parent%nx

      ! Set the subgrid point to the value of the corresponding point in the parent grid
      subgrid_data(i,j) = grid_data(x,y)
    END DO
  END DO
ELSE
  ! If the subgrid is specified using a region, just extract that region from the
  ! parent grid
  subgrid_data(:,:) = grid_data(                                               &
    subgrid%x_start:(subgrid%x_start + subgrid%nx-1),                          &
    subgrid%y_start:(subgrid%y_start + subgrid%ny-1)                           &
  )
END IF

RETURN

END FUNCTION subgrid_extract
#endif
