#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_model_grid_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_model_grid()

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE init_latlon_mod, ONLY: l_const_coord

USE io_constants, ONLY: max_file_name_len, namelist_unit, points_file_unit

USE jules_rivers_mod, ONLY: l_rivers

USE string_utils_mod, ONLY: to_string

USE grid_utils_mod, ONLY: subgrid_info, grid_create, subgrid_create,           &
                           subgrid_extract, subgrid_restrict

USE input_mod, ONLY: grid_in => grid,                                          &
! Variables to do with the extraction of subgrids if specified
! use_subgrid does not have an _in so that it can appear as use_subgrid in the
! namelist
                        use_subgrid, subgrid_in => subgrid

USE output_mod, ONLY: grid_out => grid,                                        &
! Variables to do with writing subgrids if multiple tasks are used
                         use_subgrid_out => use_subgrid,                       &
                         subgrid_out => subgrid

USE parallel_mod, ONLY: decompose_domain

USE jules_surface_mod, ONLY: l_point_data

USE jules_model_environment_mod, ONLY: lsm_id, rivers

USE model_grid_mod, ONLY: model_grid, global_land_pts, global_land_mask,       &
                          grid_area_ij, latitude, longitude, l_coord_latlon,   &
                          projection_x_coord, projection_y_coord

USE coastal, ONLY: flandg

USE atm_fields_bounds_mod, ONLY: atm_fields_bounds_init
USE theta_field_sizes, ONLY: t_i_length, t_j_length,                           &
                             u_i_length,u_j_length,                            &
                             v_i_length,v_j_length

USE ancil_info, ONLY: land_pts, row_length, rows, n_rows

USE nlsizes_namelist_mod, ONLY: bl_levels, jules_nlsizes

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about how the model grid is defined, sets it up and
!   checks for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Work variables
REAL :: x_coord(grid_in%nx,grid_in%ny)
  ! x co-ordinates of points on the input grid.

LOGICAL :: input_mask(grid_in%nx, grid_in%ny)
                                      ! Defined on the full input grid
                                      !   T - the point will be modelled
                                      !   F - the point will not be modelled
REAL(KIND=real_jlslsm) :: point_x, point_y
                              ! x and y coordinates of points read from
                              ! points file. If l_coord_latlon=T these are
                              ! longitude and latitude respectively.

TYPE(subgrid_info) :: task_subgrid  ! The subgrid that the current MPI task
                                    ! will model, as a subgrid of the full
                                    ! model grid

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  global_area(:,:), global_lat(:,:), global_lon(:,:), global_land_frac(:,:),   &
  global_xcoord(:,:), global_ycoord(:,:)
                             ! Latitude, longitude, land fraction and more on
                             ! the full model grid.
                             ! These are not required outside of this
                             ! routine.

INTEGER :: ERROR, error_sum  ! Error indicators
CHARACTER(LEN=errormessagelength) :: iomessage

INTEGER :: i, i2, istart, j, j2 ! Index variables
INTEGER :: np, np_previous      ! Counters of values set in mask.

LOGICAL :: l_bounds_180
  ! TRUE if bounds values are taken to be in [-180,180], otherwise FALSE.
LOGICAL :: l_lon_180
  ! TRUE if longitude values are taken to be in [-180,180], otherwise FALSE.
LOGICAL :: l_missing
  ! TRUE if a coordinate pair cannot be matched to a location in the input
  !   grid, when reading a list f coordinate pairs.


!-----------------------------------------------------------------------------
! Definition of the jules_model_grid namelist - this combines variables
! from input_mod with some local variables
!-----------------------------------------------------------------------------
LOGICAL :: land_only      ! T - only model land points
                          ! F - model all specified points


LOGICAL :: force_1d_grid  ! T - model grid is 1D
                          ! F - model grid is 1D unless input grid is 2D
                          !     and model grid is the whole input grid

LOGICAL :: l_bounds       ! T - subgrid is to be selected with coordinate bounds
                          ! F - subgrid is to be selected using a list of
                          !     coordinates
REAL(KIND=real_jlslsm) :: y_bounds(2)   ! USED IF l_bounds=T
REAL(KIND=real_jlslsm) :: x_bounds(2)
                        ! Upper and lower bounds for latitude and longitude

INTEGER :: npoints = 0 ! The number of points to read from file
CHARACTER(LEN=max_file_name_len) :: points_file
                        ! The file to read latitudes and longitudes for
                        ! specified points

NAMELIST  / jules_model_grid/ land_only, use_subgrid, l_bounds,                &
                              y_bounds, x_bounds, npoints, points_file,        &
                              force_1d_grid


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
land_only = .TRUE.  ! Default is to model only land points from whatever
                    ! points are selected
force_1d_grid = .FALSE. ! Default is model grid is 1D unless input grid is 2D
                        ! and model grid is the whole input grid
points_file = ''    ! Empty file name.

!-----------------------------------------------------------------------------
! Read the init_model_grid namelist.
!-----------------------------------------------------------------------------
CALL log_info("init_model_grid", "Reading JULES_MODEL_GRID namelist...")

READ(namelist_unit, NML = jules_model_grid, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal("init_model_grid",                                            &
                 "Error reading namelist JULES_MODEL_GRID " //                 &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

IF ( lsm_id /= rivers ) THEN
  !-----------------------------------------------------------------------------
  ! Read the jules_nlsizes namelist.
  !-----------------------------------------------------------------------------
  CALL log_info("init_model_grid", "Reading JULES_NLSIZES namelist...")

  READ(namelist_unit, NML = jules_nlsizes, IOSTAT = ERROR, IOMSG = iomessage)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal("init_model_grid",                                          &
                   "Error reading namelist JULES_NLSIZES " //                  &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")
  END IF
END IF

!-----------------------------------------------------------------------------
! Set up the model grid variables
!-----------------------------------------------------------------------------
CALL log_info("init_model_grid", "Setting up model grid variables...")

!*****************************************************************************
!*****************************************************************************
! Work out what points in the input grid will make up the model grid
!
! By the time we get here, we know that we have latitude, longitude and
! land fraction on the full input grid already read in
! Use these in combination with the values in the jules_model_grid namelist
! to work out what points to use
!*****************************************************************************
!*****************************************************************************
! To start with, assume we are using all the points
input_mask(:,:) = .TRUE.

! Copy x coordinates of input grid.
x_coord(:,:) = projection_x_coord(:,:)

IF ( use_subgrid ) THEN
  IF ( l_bounds ) THEN
    !--------------------------------------------------------------------------
    ! The subgrid will be selected using coordinate bounds.
    !--------------------------------------------------------------------------
    CALL log_info("init_model_grid",                                           &
                  "Subgrid will be selected using coordinate bounds")
    CALL log_info("init_model_grid",                                           &
                  "y range - " // TRIM(to_string(y_bounds(1))) //              &
                  " to " // TRIM(to_string(y_bounds(2))))
    CALL log_info("init_model_grid",                                           &
                  "x range - " // TRIM(to_string(x_bounds(1))) //              &
                  " to " // TRIM(to_string(x_bounds(2))))
    ! Check that the bounds are given in increasing order.
    IF ( x_bounds(2) < x_bounds(1) .OR. y_bounds(2) < y_bounds(1) ) THEN
      CALL log_fatal("init_model_grid",                                        &
                     "Bounds must be given in increasing order.")
    END IF

    !-------------------------------------------------------------------------
    ! Deal with the case when longitude bounds cross the edge of the input
    ! grid - e.g. longitude values are given in [0,360] but a regional subgrid
    ! for [-10,10] is required. In this case we adjust the longitude values to
    ! fall within [-180,180].
    !-------------------------------------------------------------------------
    IF ( l_coord_latlon ) THEN
      ! Establish the ranges used for longitude and bounds.
      ! A value <0 is taken to indicate that values are in [-180,180];
      ! otherwise [0,360] is assumed.
      l_lon_180    = MINVAL( x_coord) < 0.0
      l_bounds_180 = x_bounds(1) < 0.0
      ! If ranges are different, change longitude values.
      IF ( l_lon_180 .NEQV. l_bounds_180 ) THEN
        IF ( l_bounds_180 ) THEN
          ! Change longitude to [-180,180].
          DO j = 1,grid_in%ny
            DO i = 1,grid_in%nx
              IF ( x_coord(i,j) > 180.0 ) THEN
                x_coord(i,j) = x_coord(i,j) - 360.0
              END IF
            END DO
          END DO
        ELSE
          ! l_lon_180=T. Change longitude to [0,360].
          DO j = 1,grid_in%ny
            DO i = 1,grid_in%nx
              IF ( x_coord(i,j) < 0.0 ) THEN
                x_coord(i,j) = x_coord(i,j) + 360.0
              END IF
            END DO
          END DO
        END IF
      END IF  !  l_lon_180 .NEQV. l_bounds_180
    END IF  !  l_coord_latlon

    ! Set mask at locations within coordinate bounds.
    ! Here we use projection_y_coord and x_coord (rather than
    ! projection_x_coord) which includes any modification to fit with the
    ! bounds.
    input_mask = ( y_bounds(1) <= projection_y_coord ) .AND.                   &
                 ( projection_y_coord <= y_bounds(2) ) .AND.                   &
                 ( x_bounds(1) <= x_coord ) .AND.                              &
                 ( x_coord <= x_bounds(2) )
  ELSE
    !--------------------------------------------------------------------------
    ! The subgrid will be selected using a list of coordinates from the given
    ! file.
    !--------------------------------------------------------------------------
    CALL log_info("init_model_grid",                                           &
                  "Subgrid will be selected using a list of points " //        &
                  "from " // TRIM(points_file))

    ! Check that a file name was provided.
    IF ( LEN_TRIM(points_file) == 0 ) THEN
      CALL log_fatal("init_model_grid", "No points file provided" )
    END IF

    ! No points have been specified yet, so set input_mask accordingly
    input_mask(:,:) = .FALSE.

    !--------------------------------------------------------------------------
    ! Read the point coordinates from file.
    !--------------------------------------------------------------------------
    OPEN(points_file_unit, FILE=points_file,                                   &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
    IF ( ERROR /= 0 ) THEN
      CALL log_fatal("init_model_grid",                                        &
                     "Error opening points file " //                           &
                     "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //      &
                     TRIM(iomessage) // ")")
    END IF

    ! Initialise to show no points found and none missing.
    np_previous = 0
    l_missing   = .FALSE.

    DO i = 1,npoints
      ! Try to read the next coordinate pair
      READ(points_file_unit, *, IOSTAT = ERROR, IOMSG = iomessage) point_y,    &
                                                                   point_x
      IF ( ERROR /= 0 ) THEN
        CALL log_fatal("init_model_grid",                                      &
                       "Error reading coordinate pair from points file " //    &
                       "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //    &
                       TRIM(iomessage) // ")")
      END IF

      ! Set input_mask to .TRUE. wherever coordinates match.
      ! We assume that at most 1 location matches each coordinate pair - which
      ! will be true as long as grid spacing is larger than EPSILON.
      WHERE ( ABS(projection_y_coord - point_y) < EPSILON(1.0) .AND.           &
              ABS(projection_x_coord - point_x) < EPSILON(1.0) )
        input_mask = .TRUE.
      END WHERE

      ! Count the number of locations selected.
      np = COUNT( input_mask )

      ! Warn if no new location was identified.
      IF ( np == np_previous ) THEN
        l_missing = .TRUE.
        CALL log_warn("init_model_grid",                                       &
                       "No location found to match (x,y)="                 //  &
                       TRIM(to_string(point_x)) // ' '                     //  &
                       TRIM(to_string(point_y)))
      END IF

      ! Set counter ready for next loop.
      np_previous = np

    END DO  !  points

    CLOSE(points_file_unit, IOSTAT = ERROR, IOMSG = iomessage)
    IF ( ERROR /= 0 ) THEN
      CALL log_fatal("init_model_grid",                                        &
                     "Error closing points file " //                           &
                     "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //      &
                     TRIM(iomessage) // ")")
    END IF

    ! Raise an error if any locations were not matched.
    IF ( l_missing ) THEN
      CALL log_fatal("init_model_grid",                                        &
                     "One or more locations in list are not in the model " //  &
                     "input grid. See earlier messages.")
    END IF

  END IF  ! l_bounds
END IF  ! use_subgrid

IF ( land_only ) THEN
  !----------------------------------------------------------------------------
  ! If requested, select land points only from the points we have left
  !----------------------------------------------------------------------------
  CALL log_info("init_model_grid",                                             &
                "From the points specified, only land points will be " //      &
                "modelled")

  input_mask = input_mask .AND. ( flandg > 0.0 )
END IF


!*****************************************************************************
!-----------------------------------------------------------------------------
! At this point, the .TRUE. points in input_mask will comprise the FULL
! model grid
! Issue an error if all points have been excluded
!-----------------------------------------------------------------------------
IF ( .NOT. ANY(input_mask) ) THEN
  CALL log_fatal("init_model_grid",                                            &
                 "All points in input grid have been excluded from " //        &
                 "model grid")
END IF
!*****************************************************************************

!-----------------------------------------------------------------------------
! Build a subgrid from the input grid and mask
!-----------------------------------------------------------------------------
subgrid_in = subgrid_create(grid_in, input_mask, force_1d_grid)

! Construct the model grid to be the same size as the input subgrid
! It doesn't matter about dimension names as it will not be used for I/O
! It is always created as a 2D grid
model_grid = grid_create(                                                      &
  .FALSE., x_name = "", y_name = "", nx = subgrid_in%nx, ny = subgrid_in%ny    &
)

!-----------------------------------------------------------------------------
! Store the latitude, longitude and land fraction on the full model grid
!-----------------------------------------------------------------------------
ALLOCATE(global_area(model_grid%nx,model_grid%ny))
global_area(:,:) = subgrid_extract(subgrid_in, grid_area_ij)

ALLOCATE(global_lat(model_grid%nx,model_grid%ny))
global_lat(:,:) = subgrid_extract(subgrid_in, latitude)

ALLOCATE(global_lon(model_grid%nx,model_grid%ny))
global_lon(:,:) = subgrid_extract(subgrid_in, longitude)

ALLOCATE(global_land_frac(model_grid%nx,model_grid%ny))
global_land_frac(:,:) = subgrid_extract(subgrid_in, flandg)

ALLOCATE(global_xcoord(model_grid%nx,model_grid%ny))
global_xcoord(:,:) = subgrid_extract(subgrid_in, projection_x_coord)

ALLOCATE(global_ycoord(model_grid%nx,model_grid%ny))
global_ycoord(:,:) = subgrid_extract(subgrid_in, projection_y_coord)

! Work out how many land points are in the full model grid and set the land
! mask
ALLOCATE(global_land_mask(model_grid%nx,model_grid%ny))
global_land_mask(:,:) = ( global_land_frac(:,:) > EPSILON(1.0) )
global_land_pts = COUNT(global_land_mask)

!-----------------------------------------------------------------------------
! Check that we do not have any repeated coordinates.
! The model cannot deal with duplicate points if river routing is active - the
! mapping between land and river grids will not work. Most runs without river
! routing will also not have duplicate points - but the code will function
! with duplicates, which can be useful if a user is deliberately repeating a
! point (e.g. looking at the effect of different land covers at one location,
! in a single run).
! As the check can be slow for larger grids, we only do it if one or more of
! the coordinate variables (e.g. latitude and longitude) has been set using a
! constant, as this is the most likely way that duplicates could arise
! inadvertently. By contrast, values read from an ancillary file are assumed
! to be correct.
! In truth the limited cases in which this test is run rather reduces its
! usefulness, but it is here to help a user who stumbles into such a situation
! and might otherwise be stuck.
!-----------------------------------------------------------------------------
IF ( l_const_coord ) THEN
  CALL log_info( "init_model_grid",                                            &
                 "Checking for duplicate coordinates..." )
  DO i = 1,model_grid%nx
    DO j = 1,model_grid%ny

      ! Compare with this row and higher rows.
      DO j2 = j,model_grid%ny

        IF ( j2 == j ) THEN
          ! Compare with the remainder of this row.
          istart = i + 1
        ELSE
          ! Scan the full row.
          istart = 1
        END IF

        DO i2 = istart,model_grid%nx
          IF ( global_ycoord(i,j) == global_ycoord(i2,j2) .AND.                &
               global_xcoord(i,j) == global_xcoord(i2,j2) ) THEN
            IF ( l_rivers ) THEN
              ! We cannot cope with duplicates when translating to the river
              ! grid.
              CALL log_fatal( "init_model_grid",                               &
                              "Duplicate coordinates have been specified." //  &
                              " y=" // to_string(global_ycoord(i,j))       //  &
                              " x= " // to_string(global_xcoord(i,j)) )
            ELSE
              ! This is possible, if unusual. Proceed with caution!
              CALL log_warn( "init_model_grid",                                &
                             "Duplicate coordinates have been specified."  //  &
                             "Only proceed if this is intentional!"        //  &
                             " y=" // to_string(global_ycoord(i,j))        //  &
                             " x=" // to_string(global_xcoord(i,j)) )
            END IF
          END IF
        END DO  !  i2
      END DO  !  j2

    END DO  !  j
  END DO  !  i

END IF  !  l_const_coord

!-----------------------------------------------------------------------------
! Decompose the model domain
!
! Returns the subgrid of the full model grid that the current task will model
!-----------------------------------------------------------------------------
task_subgrid = decompose_domain(model_grid)

!-----------------------------------------------------------------------------
! Set up the extraction of the input grid
!-----------------------------------------------------------------------------
! First, we combine the task subgrid with the subgrid generated from input_mask
! to get the input subgrid for this task
subgrid_in = subgrid_restrict(subgrid_in, task_subgrid)

! Do we actually have a subgrid, or does the subgrid cover the whole input grid?
use_subgrid = ( subgrid_in%nx /= grid_in%nx ) .OR.                             &
              ( subgrid_in%ny /= grid_in%ny )

IF ( use_subgrid ) THEN
  ! Regrid the latitude and longitude onto the model grid for this task

  DEALLOCATE(grid_area_ij)
  ALLOCATE(grid_area_ij(task_subgrid%nx,task_subgrid%ny))
  grid_area_ij(:,:) = subgrid_extract(task_subgrid, global_area)

  DEALLOCATE(latitude)
  ALLOCATE(latitude(task_subgrid%nx,task_subgrid%ny))
  latitude(:,:) = subgrid_extract(task_subgrid, global_lat)

  DEALLOCATE(longitude)
  ALLOCATE(longitude(task_subgrid%nx,task_subgrid%ny))
  longitude(:,:) = subgrid_extract(task_subgrid, global_lon)

  DEALLOCATE(flandg)
  ALLOCATE(flandg(task_subgrid%nx,task_subgrid%ny))
  flandg(:,:) = subgrid_extract(task_subgrid, global_land_frac)

  DEALLOCATE(projection_x_coord)
  ALLOCATE(projection_x_coord(task_subgrid%nx,task_subgrid%ny))
  projection_x_coord(:,:) = subgrid_extract(task_subgrid, global_xcoord)

  DEALLOCATE(projection_y_coord)
  ALLOCATE(projection_y_coord(task_subgrid%nx,task_subgrid%ny))
  projection_y_coord(:,:) = subgrid_extract(task_subgrid, global_ycoord)

END IF  !  use_subgrid

! Deallocate some arrays that are no longer required
DEALLOCATE(global_area)
DEALLOCATE(global_lat)
DEALLOCATE(global_lon)
DEALLOCATE(global_land_frac)
DEALLOCATE(global_xcoord)
DEALLOCATE(global_ycoord)

!-----------------------------------------------------------------------------
! Set up the output grid
!
! The full output grid is the same as the full model grid
! The subgrid that this task is responsible for outputting is the same as
! the subgrid it is responsible for modelling
!-----------------------------------------------------------------------------
grid_out    = model_grid
subgrid_out = task_subgrid

! Do we actually have a subgrid, or does the subgrid cover the whole output
! grid?
use_subgrid_out = ( subgrid_out%nx /= grid_out%nx ) .OR.                       &
                  ( subgrid_out%ny /= grid_out%ny )

!-----------------------------------------------------------------------------
! Set up the model grid for this task, i.e. the grid that the science code
! knows about. Only control code has to deal with the concept of a 'task
! model grid' and a 'full model grid'
!-----------------------------------------------------------------------------
t_i_length = task_subgrid%nx
t_j_length = task_subgrid%ny
v_i_length = task_subgrid%nx
v_j_length = task_subgrid%ny
u_i_length = task_subgrid%nx
u_j_length = task_subgrid%ny

! At the moment, JULES uses no halos, and the t, p, u and v grids are the same
CALL atm_fields_bounds_init(0, 0, 0, 0, t_i_length, t_j_length, t_j_length, 1, &
                            bl_levels_opt = bl_levels)
! Copy the values into row_length, rows and n_rows for legacy purposes
row_length = t_i_length
rows   = t_j_length
n_rows = t_j_length

CALL log_info("init_model_grid",                                               &
              "Size of model grid - " // TRIM(to_string(t_i_length)) //        &
              " x " // TRIM(to_string(t_j_length)))

! Calculate the number of land points
land_pts = COUNT(flandg > EPSILON(1.0))
CALL log_info("init_model_grid",                                               &
              "Selected grid contains " // TRIM(to_string(land_pts)) //        &
              " land points")

CALL log_info("init_model_grid",                                               &
              "bl_levels = " // TRIM(to_string(bl_levels)))

! Warn if using l_point_data for a non-single-point run
IF ( l_point_data .AND. t_i_length * t_j_length > 1 ) THEN
  CALL log_warn("init_model_grid",                                             &
                "l_point_data is selected but there is more than one point")
END IF


RETURN

END SUBROUTINE init_model_grid

END MODULE init_model_grid_mod
#endif
