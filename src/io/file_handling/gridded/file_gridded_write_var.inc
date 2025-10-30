#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_gridded_write_var(FILE, var_id, cube, write_subgrid, subgrid)

USE grid_utils_mod, ONLY: subgrid_info, OPERATOR( /= )

USE file_mod, ONLY: file_write_var

USE data_cube_mod, ONLY: data_cube

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes data given as a data cube to the given variable in the given file
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
                               ! The file to write data to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
TYPE(data_cube), INTENT(IN) :: cube
                               ! The values to write to the file, contained
                               ! in a data cube
                               ! The first 2 dimensions of the cube should
                               ! be the grid dimensions OR the first 1
                               ! dimension should be the vector dimension
                               ! The rest of the dimensions will be taken to
                               ! be levels dimensions
LOGICAL, INTENT(IN) :: write_subgrid
                               ! T - write to a subgrid of the full
                               !     grid of the file
                               ! F - write the full grid
TYPE(subgrid_info), OPTIONAL :: subgrid  ! The subgrid to write


! Work variables
TYPE(var_gridded) :: var  ! The variable we are writing to

REAL, ALLOCATABLE :: values_3d(:,:,:)  ! 3D version of the cube values to
                                       ! simplify calculations using subgrids

INTEGER :: nx, ny  ! The sizes of the grid dimensions

INTEGER :: i, j, x, y  ! Index variables


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check if a subgrid has been requested and that the parent grid is the same
! as the grid the file is on
!-----------------------------------------------------------------------------
IF ( write_subgrid ) THEN
  IF ( .NOT. PRESENT(subgrid) )                                                &
    CALL log_fatal("file_gridded_write_var",                                   &
                   "Writing a subgrid has been requested but no " //           &
                   "subgrid has been given")

  ! Check that the subgrid's parent grid matches the file's grid
  IF ( subgrid%parent /= FILE%grid )                                           &
    CALL log_fatal("file_gridded_write_var",                                   &
                   "Subgrid is inconsistent with grid in file")
END IF  ! extract_subgrid

!-----------------------------------------------------------------------------
! Extract information on the variable being read into a local variable for
! convenience
!-----------------------------------------------------------------------------
var = FILE%vars(var_id)

!-----------------------------------------------------------------------------
! Check that the given cube has the correct size
!-----------------------------------------------------------------------------
IF ( FILE%grid%is_1d ) THEN
  ! The cube should have one vector dimension plus the correct number of levels
  ! dimensions
  IF ( SIZE(cube%SHAPE) /= (1 + SIZE(var%lev_sizes)) )                         &
    CALL log_fatal("file_gridded_write_var",                                   &
                   "Given cube has incorrect number of dimensions")
  ! Check that the cube has the shape we are expecting
  IF ( write_subgrid ) THEN
    nx = subgrid%nx
  ELSE
    nx = FILE%grid%nx
  END IF
  IF ( .NOT. ALL(cube%SHAPE == [ nx, var%lev_sizes ]) )                        &
    CALL log_fatal("file_gridded_write_var",                                   &
                   "At least one dimension of given cube has incorrect size")
ELSE
  ! The cube should have two grid dimensions plus the correct number of levels
  ! dimensions
  IF ( SIZE(cube%SHAPE) /= (2 + SIZE(var%lev_sizes)) )                         &
    CALL log_fatal("file_gridded_write_var",                                   &
                   "Given cube has incorrect number of dimensions")
  ! Check that the cube has the shape we are expecting
  IF ( write_subgrid ) THEN
    nx = subgrid%nx
    ny = subgrid%ny
  ELSE
    nx = FILE%grid%nx
    ny = FILE%grid%ny
  END IF
  IF ( .NOT. ALL(cube%SHAPE == [ nx, ny, var%lev_sizes ]) )                    &
    CALL log_fatal("file_gridded_write_var",                                   &
                   "At least one dimension of given cube has incorrect size")
END IF

!-----------------------------------------------------------------------------
! Actually write the data
!
! The decision process for how to write to file is as follows:
!   * If no subgrid has been specified, write everything (obviously!)
!
!   * If a subgrid has been specified using a region, then write only that
!     region (easy to specify with start and count)
!
!   * If a subgrid has been specified using points, then write the values
!     to file one at a time. Note that the optimisation used in read_var
!     when the number of points in the subgrid is small compared to the
!     full grid does not apply here.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! If we are not writing a subgrid, just write all the data and return
!-----------------------------------------------------------------------------
IF ( .NOT. write_subgrid ) THEN
  IF ( FILE%grid%is_1d ) THEN
    CALL file_write_var(                                                       &
      FILE%fh, var%id, cube%values,                                            &
    ! We want to write the whole of the grid and levels dimensions, so set start
    ! and count accordingly
    ! For a 1D grid, we only need to consider the x axis of the grid
            [ 1,            (1, i = 1,SIZE(var%lev_sizes)) ],                  &
            [ FILE%grid%nx, var%lev_sizes ]                                    &
          )
  ELSE
    CALL file_write_var(                                                       &
      FILE%fh, var%id, cube%values,                                            &
    ! We want to write the whole of the grid and levels dimensions, so set start
    ! and count accordingly
    ! For a 2D grid, we only need to consider both axes of the grid
            [ 1,            1,            (1, i = 1,SIZE(var%lev_sizes)) ],    &
            [ FILE%grid%nx, FILE%grid%ny, var%lev_sizes ]                      &
          )
  END IF

  RETURN
END IF

!-----------------------------------------------------------------------------
! If we get to here, we know we have to write a subgrid
!-----------------------------------------------------------------------------
IF ( .NOT. ASSOCIATED(subgrid%points) ) THEN

  ! The subgrid is specified using a region - we just write a single slab
  ! using start/count appropriately

  IF ( FILE%grid%is_1d ) THEN
    CALL file_write_var(                                                       &
      FILE%fh, var%id, cube%values,                                            &
    ! We want to write the whole of the levels dimensions, but only the correct
    ! chunk of the grid dimensions, so set start and count accordingly
    ! For a 1D grid, we only need to consider the 'x axis' of the grid
            [ subgrid%x_start, (1, i = 1,SIZE(var%lev_sizes)) ],               &
            [ subgrid%nx,      var%lev_sizes ]                                 &
          )
  ELSE
    CALL file_write_var(                                                       &
      FILE%fh, var%id, cube%values,                                            &
    ! We want to read the whole of the levels dimensions, but only the correct
    ! chunk of the grid dimensions, so set start and count accordingly
    ! For a 1D grid, we only need to consider the 'x axis' of the grid
            [ subgrid%x_start, subgrid%y_start, (1, i = 1,SIZE(var%lev_sizes)) ], &
            [ subgrid%nx,      subgrid%ny,      var%lev_sizes ]                &
          )
  END IF

ELSE

  ! The subgrid is specified using points - just write the points one at a time
  ! directly to file

  ! For this section, it is easier to deal with the data as if it has two grid
  ! dimensions and a combined z dimension that represents all the levels combined
  ALLOCATE(values_3d(subgrid%nx, subgrid%ny, PRODUCT(var%lev_sizes)))
  values_3d(:,:,:) = RESHAPE(cube%values, SHAPE(values_3d))

  DO j = 1,subgrid%ny
    DO i = 1,subgrid%nx
      ! Translate the index in subgrid%points into x and y coordinates in the file grid
      y = (subgrid%points(i,j) - 1) / FILE%grid%nx + 1
      x = subgrid%points(i,j) - (y-1) * FILE%grid%nx

      IF ( FILE%grid%is_1d ) THEN
        CALL file_write_var(FILE%fh, var%id, values_3d(i,j,:),                 &
        ! For a 1D grid, we only need to specify the x index
                                      [ x, (1, i = 1,SIZE(var%lev_sizes)) ],   &
                                      [ 1, var%lev_sizes ])
      ELSE
        CALL file_write_var(FILE%fh, var%id, values_3d(i,j,:),                 &
        ! For a 2D grid, we specify the x and y indices
                                      [ x, y, (1, i = 1,SIZE(var%lev_sizes)) ],&
                                      [ 1, 1, var%lev_sizes ])
      END IF
    END DO
  END DO

  DEALLOCATE(values_3d)

END IF  ! subgrid specified using region or points


RETURN

END SUBROUTINE file_gridded_write_var
#endif
