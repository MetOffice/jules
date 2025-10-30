#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION map_to_land(cube_grid) RESULT(cube_land)

USE data_cube_mod, ONLY: data_cube, cube_create

USE ancil_info, ONLY: land_pts

USE jules_fields_mod, ONLY: ainfo

USE theta_field_sizes, ONLY: t_i_length, t_j_length

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes a cube of data on the full model grid and maps it to land points only
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube_grid  ! The data on the full model grid
                                          ! This cube should have two grid
                                          ! dimensions with all other dimensions
                                          ! being levels dimensions
                                          ! Vertical levels are preserved in
                                          ! the mapping


! Return type
TYPE(data_cube) :: cube_land  ! The data mapped onto land points
                              ! This cube will have a 'land points' dimension
                              ! plus all the levels dimensions from the gridded
                              ! data


! Work variables
REAL, ALLOCATABLE :: data_land(:,:), data_grid(:,:,:)
                              ! Variables to hold the data that use a
                              ! 'collapsed levels dimension', i.e. all the
                              ! vertical levels dimensions are represented
                              ! by one dimension

INTEGER :: nlevs  ! The size of the combined levels dimension

! Work variables
INTEGER :: i, j, l  ! Indexing variables


!-----------------------------------------------------------------------------


! Check the data is on the full model grid
IF ( cube_grid%SHAPE(1) /= t_i_length .OR. cube_grid%SHAPE(2) /= t_j_length )  &
  CALL log_fatal("map_to_land",                                                &
                 "Input data must be on the full model grid")

! Allocate the data_land and data_grid arrays
nlevs = PRODUCT(cube_grid%SHAPE(3:))
ALLOCATE(data_land(land_pts, nlevs))
ALLOCATE(data_grid(t_i_length, t_j_length, nlevs))

! Copy the data from the cube into the data array
! Note that we can't use cube_get_data as our array has the combined z dimension
data_grid(:,:,:) = RESHAPE(cube_grid%values, SHAPE(data_grid))

! Do the mapping
DO l = 1,land_pts
  j = (ainfo%land_index(l) - 1) / t_i_length + 1
  i = ainfo%land_index(l) - (j-1) * t_i_length

  data_land(l,:) = data_grid(i,j,:)
END DO

! Copy the land data into the return cube
cube_land = cube_create([ land_pts, cube_grid%SHAPE(3:) ])
cube_land%values(:) = RESHAPE(data_land, [ SIZE(data_land) ])

! Deallocate the arrays
DEALLOCATE(data_land)
DEALLOCATE(data_grid)

RETURN

END FUNCTION map_to_land
#endif
