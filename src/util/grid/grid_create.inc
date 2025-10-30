#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION grid_create(grid_is_1d, dim_name, npoints, x_name, nx, y_name, ny)    &
                                                                  RESULT(grid)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Creates a grid_info object encapsulating the specified information
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
LOGICAL, INTENT(IN) :: grid_is_1d
    ! T - define a 1d grid (i.e. a vector)
    ! F - define a 2d grid

CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: dim_name
    ! ONLY USED IF is_1d=T
    ! The name of the single grid dimension
INTEGER, INTENT(IN), OPTIONAL :: npoints
    ! ONLY USED IF is_1d=T
    ! The size of the single grid dimension

CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: x_name, y_name
    ! ONLY USED IF is_1d=F
    ! The names of the x and y dimensions of the grid respectively
INTEGER, INTENT(IN), OPTIONAL :: nx, ny
    ! ONLY USED IF is_1d=F
    ! The sizes of the x and y dimensions respectively

! Return type
TYPE(grid_info) :: grid  ! The grid_info object


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check an appropriate combination of variables has been given
!-----------------------------------------------------------------------------
IF ( grid_is_1d ) THEN
  IF ( ( .NOT. PRESENT(dim_name) ) .OR. ( .NOT. PRESENT(npoints) ) )           &
    CALL log_fatal("grid_create",                                              &
                   "To create a 1D grid, dim_name and npoints are required")
ELSE
  IF ( ( .NOT. PRESENT(x_name) ) .OR. ( .NOT. PRESENT(nx) ) .OR.               &
       ( .NOT. PRESENT(y_name) ) .OR. ( .NOT. PRESENT(ny) ) )                  &
    CALL log_fatal("grid_create",                                              &
                   "To create a 2D grid, x_name, y_name, nx and ny are " //    &
                   "all required")
END IF

!-----------------------------------------------------------------------------
! Build the grid_info object
!-----------------------------------------------------------------------------
grid%is_1d = grid_is_1d

IF ( grid_is_1d ) THEN
  grid%dim_name = dim_name

  grid%nx = npoints
  grid%ny = 1
ELSE
  grid%x_name = x_name
  grid%y_name = y_name

  grid%nx = nx
  grid%ny = ny
END IF

RETURN

END FUNCTION grid_create
#endif
