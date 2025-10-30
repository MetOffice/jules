!##############################################################################
!##############################################################################

MODULE rivers_regrid_mod

!------------------------------------------------------------------------------
! Description:
!   Contains river routing regridding/remapping functions for standalone JULES.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC get_xy_pos, calc_grid_index, landpts_to_rivpts,                         &
       calc_map_river_to_land_points, rivpts_to_landpts,                       &
       twod_to_rp, rp_to_twod

CONTAINS

!##############################################################################

SUBROUTINE get_xy_pos( i, nx, ny, ix, iy )

!------------------------------------------------------------------------------
! Description:
!    Given a point number and the extents (size) of a 2-D (xy) grid, returns
!    the x and y indices (coords) of the point. All coords are relative to
!    (1,1) at bottom left of grid, with numbering running left to right,
!    bottom to top.
!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
                       i,                                                      &
                   !  the point number
                       nx,                                                     &
                   !  the extent (size) of the grid in the x direction
                       ny
                   !  the extent (size) of the grid in the y direction
                   !  NB ny is only required for (cursory) error checking.

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(OUT):
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::  ix,iy   !  the x and y coordinates of the point

!end of header
!------------------------------------------------------------------------------

! Assume that i>=1!
iy = (i - 1) / nx + 1
ix = i - (iy - 1) * nx

! If locations are out of range (grid is too small for this point number),
! set to -1.
IF ( ix > nx .OR. iy > ny ) THEN
  ix = -1
  iy = -1
END IF

RETURN
END SUBROUTINE get_xy_pos

!##############################################################################

SUBROUTINE calc_map_river_to_land_points( global_land_pts,                     &
                                 x_coord_of_land, y_coord_of_land,             &
                                 rivers_x_coord_rp, rivers_y_coord_rp,         &
                                 map_river_to_land_points )

!------------------------------------------------------------------------------
!
! Description:
!   Computes mapping between land points and river points.
!   Only used when rivers_regrid=F.
!   For each river point, this identifies a land point (if it exists) with
!   the same coordinate values, and returns the number of that point.
!
!------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY: l_trivial_mapping, np_rivers

#if defined(UM_JULES)
USE um_parallel_mod, ONLY: is_master_task
#else
USE parallel_mod, ONLY: is_master_task
#endif

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! The number of land points in the full model grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  x_coord_of_land(global_land_pts),                                            &
    ! x coordinates of land points.
  y_coord_of_land(global_land_pts),                                            &
    ! y coordinates of land points.
  rivers_x_coord_rp(np_rivers),                                                &
    ! x coordinates of river points.
  rivers_y_coord_rp(np_rivers)
    ! y coordinates of river points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) :: map_river_to_land_points(:)
  ! List of coincident land point numbers, on river points.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  iland, l, rp,                                                                &
    ! Loop counters and indices
  nmatch
    ! Counter of locations for which a trivial mapping applies, i.e the n-th
    ! river point matches the n-th land point.

!end of header
!------------------------------------------------------------------------------

IF ( is_master_task() ) THEN

  ! Initialise with zero to show that there is no matching land point.
  map_river_to_land_points(:) = 0

  ! Initialise index so first loop will start at first land point, and
  ! initialise counter of points with trivial mapping.
  iland = 0
  nmatch = 0

  DO rp = 1,np_rivers
    ! Search all land points to find a match. A river point without matching
    ! land retains the initial value set above.
    DO l = 1,global_land_pts
      ! We start at the land point next to the last match found (from previous
      ! river point).
      iland = iland + 1
      IF ( iland > global_land_pts ) THEN
        ! Continue the search from the first land point.
        iland = 1
      END IF
      IF ( rivers_x_coord_rp(rp) == x_coord_of_land(iland) .AND.               &
           rivers_y_coord_rp(rp) == y_coord_of_land(iland) ) THEN
        map_river_to_land_points(rp) = iland
        ! Check for trivial mapping.
        IF ( rp == iland ) THEN
          nmatch = nmatch + 1
        END IF
        EXIT  ! leave land point loop
      END IF
    END DO  !  l (land points)

  END DO  !  rp (river points)

  ! If the numbers of river and land points are equal, and all have a trivial
  ! mapping (they occur in the same order), set flag.
  IF ( np_rivers == global_land_pts .AND. nmatch == np_rivers ) THEN
    l_trivial_mapping = .TRUE.
  END IF

END IF  !  is_master_task

RETURN
END SUBROUTINE calc_map_river_to_land_points

!##############################################################################

SUBROUTINE calc_grid_index( npts, nx, ny, dx, dy, frac_toler, x1, y1, x_coord, &
                            y_coord, grid_index )

!-----------------------------------------------------------------------------
! Description:
!   Calculate the mapping of land points to a 2D grid, returning an index
!   of location in the grid.
!------------------------------------------------------------------------------

! Modules used:
USE ereport_mod, ONLY: ereport

USE jules_print_mgr, ONLY:                                                     &
   jules_message,                                                              &
   jules_print

IMPLICIT NONE


!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npts,                                                                        &
    ! The number of points.
  nx,                                                                          &
    ! Number of columns in grid.
  ny
    ! Number of rows in grid.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dx,                                                                          &
    ! Gridbox size in x.
  dy,                                                                          &
    ! Gridbox size in y.
  frac_toler,                                                                  &
    ! Tolerance (fraction of a gridbox) when testing whether points lie on
    ! grid.
  x1,                                                                          &
    ! x coordinate of point in "SW" (lower left) corner of grid.
  y1
    ! y coordinate of point in "SW" (lower left) corner of grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  x_coord(npts),                                                               &
    ! x coordinates of points.
  y_coord(npts)
    ! y coordinates of points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) :: grid_index(:)
  ! List of indices for the grid. For every input point, this is the location
  ! in a 2-D grid, counting row-wise from the bottom left.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  errcode,                                                                     &
    ! Error code.
  i, j,                                                                        &
    ! Locations on grid.
  l
    ! Loop counter.

REAL(KIND=real_jlslsm) ::                                                      &
  one_minus_toler, nx_plus_toler, ny_plus_toler,                               &
    ! Constants.
  rix, riy
    ! Locations on grid.

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_GRID_INDEX'

!end of header
!------------------------------------------------------------------------------

! Initialise to zero to indicate that points are not in grid.
grid_index(:) = 0

! Calculate some constants.
one_minus_toler = 1.0  - frac_toler
nx_plus_toler   = nx + frac_toler
ny_plus_toler   = ny + frac_toler

DO l = 1,npts

  ! Calculate location on grid.
  rix = (x_coord(l) - x1) / dx + 1.0
  riy = (y_coord(l) - y1) / dy + 1.0

  ! Raise an error if this location lies beyond the edge of the grid.
  IF ( rix < one_minus_toler .OR. rix > nx_plus_toler .OR.                     &
       riy < one_minus_toler .OR. riy > ny_plus_toler ) THEN
    WRITE(jules_message,'(a,f0.3,tr1,f0.3)' )                                  &
      'Location lies beyond edge of grid: ', x_coord(l), y_coord(l)
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode, jules_message)
  END IF

  ! Check that this location lies on the grid - raise an error if the location
  ! does not match the expected grid to within a non-negligible fraction of a
  ! gridbox. Essentially we are checking that this location appears to be
  ! "sufficiently close" to the grid (where "sufficiently close" seeks to
  ! make allowance for the finite precision of the calculation).
  IF ( ABS( rix - NINT(rix) ) > frac_toler .OR.                                &
       ABS( riy - NINT(riy) ) > frac_toler ) THEN
    WRITE(jules_message,'(a,f0.3,tr1,f0.3)' )                                  &
      'Location does not sit on the grid: ', x_coord(l), y_coord(l)
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode, jules_message)
  END IF

  ! Calculate location on the grid.
  ! Note that the use of NINT here means this is a relatively forgiving
  ! calculation - it "snaps" to the nearest grid location.
  i = NINT(rix)
  j = NINT(riy)

  ! Calculate index.
  grid_index(l) = (j-1) * nx + i

END DO

RETURN
END SUBROUTINE calc_grid_index

!##############################################################################

SUBROUTINE landpts_to_rivpts( land_pts, riv_pts,                               &
                              map_river_to_land_points, global_land_index,     &
                              rivers_index_rp, var_land,                       &
                              var_riv )

!------------------------------------------------------------------------------
! Description:
!   Checks what method of conversion required between land_pts and riv_pts, and
!   invokes regridding or remapping for a given input variable.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
   l_trivial_mapping, rivers_regrid

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts  ! Number of land points.
INTEGER, INTENT(IN) :: riv_pts   ! Number of river points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: map_river_to_land_points(:)
  ! List of coincident land point numbers, on river points.
INTEGER, INTENT(IN) :: global_land_index(:)
  ! List of indices for the land model grid.
INTEGER, INTENT(IN) :: rivers_index_rp(:)
  ! Index of points where routing is calculated.

REAL(KIND=real_jlslsm), INTENT(IN) :: var_land(land_pts)
  ! Input variable on land_pts.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: var_riv(riv_pts)
  ! Output variable on riv_pts.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: ip
  ! Local counter

!end of header
!------------------------------------------------------------------------------

IF ( rivers_regrid ) THEN

  ! Call regridding routine.
  CALL regrid_land_to_rivers_ctl( land_pts, riv_pts, global_land_index,        &
                                rivers_index_rp, var_land,  var_riv )

ELSE IF ( l_trivial_mapping ) THEN

  ! Land and river points occur in the same order, simply copy values.
  DO ip = 1,riv_pts
    var_riv(ip)  = var_land(ip)
  END DO

ELSE

  ! Translate from land points to river points.
  ! A river point that does not match to a land point has
  ! map_river_to_land_points=0 and hence does nothing here.
  DO ip = 1,riv_pts
    IF (map_river_to_land_points(ip) > 0) THEN
      var_riv(ip) = var_land(map_river_to_land_points(ip))
    END IF
  END DO

END IF

RETURN
END SUBROUTINE landpts_to_rivpts

!##############################################################################

SUBROUTINE rivpts_to_landpts( land_pts, riv_pts, map_river_to_land_points,     &
                              global_land_index, rivers_index_rp,              &
                              var_riv, var_land )

!------------------------------------------------------------------------------
! Description:
!   Checks what method of conversion required between land_pts and riv_pts, and
!   invokes regridding or remapping for a given input variable.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: l_trivial_mapping, rivers_regrid

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts  ! Number of land points.
INTEGER, INTENT(IN) :: riv_pts   ! Number of river points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: map_river_to_land_points(:)
  ! List of land point numbers for river points.
INTEGER, INTENT(IN) :: global_land_index(:)
  ! List of indices for the land model grid.
INTEGER, INTENT(IN) :: rivers_index_rp(:)
  ! Index of points where routing is calculated.

REAL(KIND=real_jlslsm), INTENT(IN) :: var_riv(riv_pts)
  ! Input variable on riv_pts.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: var_land(land_pts)
  ! output variable on land_pts.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: ip
  ! Local counter

!end of header
!------------------------------------------------------------------------------

IF ( rivers_regrid ) THEN

  ! Call regridding routine.
  CALL regrid_rivers_to_land_ctl( land_pts, riv_pts, global_land_index,        &
                                  rivers_index_rp, var_riv, var_land )

ELSE IF ( l_trivial_mapping ) THEN

  ! Land and river points occur in the same order, simply copy values.
  DO ip = 1,riv_pts
    var_land(ip) = var_riv(ip)
  END DO

ELSE

  ! Translate from river points to land points.
  ! A river point that does not match to a land point has
  ! map_river_to_land_points=0 and hence does nothing here.
  DO ip = 1,riv_pts
    IF (map_river_to_land_points(ip) > 0) THEN
      var_land(map_river_to_land_points(ip)) = var_riv(ip)
    END IF
  END DO

END IF

RETURN
END SUBROUTINE rivpts_to_landpts

!##############################################################################

SUBROUTINE regrid_land_to_rivers_ctl( land_pts, riv_pts, global_land_index,    &
                                      rivers_index_rp, runoff_land,            &
                                      runoff_riv )

!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid to a target grid (the rivers grid).
!   Both grids must be regular in latitude and longitude.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars with intent(in)
     nx_rivers,ny_rivers, nx_land_grid, ny_land_grid

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts  ! Number of land points.
INTEGER, INTENT(IN) :: riv_pts   ! Number of river points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: global_land_index(:)
  ! List of indices for the land model grid.
INTEGER, INTENT(IN) :: rivers_index_rp(:)
  ! Index of points where routing is calculated.

REAL(KIND=real_jlslsm), INTENT(IN) :: runoff_land(land_pts)
!  runoff rate on model grid (kg m-2 s-1)

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: runoff_riv(riv_pts)
!  runoff rate on rivers grid (kg m-2 s-1)

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: maxlin, l, ip, ix, iy

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: runoff_grid(nx_land_grid,ny_land_grid)
REAL(KIND=real_jlslsm) :: runoff_out(nx_rivers,ny_rivers)

!end of header
!------------------------------------------------------------------------------

! Convert from land_pts to full grid
runoff_grid(:,:) = 0.0
DO l = 1,land_pts
  CALL get_xy_pos( global_land_index(l), nx_land_grid, ny_land_grid, ix, iy )
  runoff_grid(ix,iy) = runoff_land(l)
END DO

! Call to lat/lon grid-based regridding routine
maxlin = ( nx_land_grid + nx_rivers ) * ( ny_land_grid + ny_rivers )
CALL regrid_land_to_rivers( maxlin, runoff_grid, runoff_out )

! Convert from rivers grid to riv_pts
DO ip = 1,riv_pts
  CALL get_xy_pos(rivers_index_rp(ip),nx_rivers,ny_rivers,ix,iy)
  runoff_riv(ip) = runoff_out(ix,iy)
END DO

RETURN
END SUBROUTINE regrid_land_to_rivers_ctl

!##############################################################################

SUBROUTINE regrid_rivers_to_land_ctl( land_pts, riv_pts, global_land_index,    &
                                      rivers_index_rp, runoff_riv,             &
                                      runoff_land )

!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid to a target grid (the land grid).
!   Both grids must be regular in latitude and longitude.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars with intent(in)
     nx_rivers,ny_rivers, nx_land_grid, ny_land_grid

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts  ! Number of land points.
INTEGER, INTENT(IN) :: riv_pts   ! Number of river points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: global_land_index(:)
  ! List of indices for the land model grid.
INTEGER, INTENT(IN) :: rivers_index_rp(:)
  ! Index of points where routing is calculated.

REAL(KIND=real_jlslsm), INTENT(IN) :: runoff_riv(riv_pts)
  ! Runoff rate on rivers grid (kg m-2 s-1).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT):
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: runoff_land(land_pts)
  ! Runoff rate on model grid (kg m-2 s-1).

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: maxlin, l, ip, ix, iy

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: runoff_grid(nx_land_grid,ny_land_grid)
REAL(KIND=real_jlslsm) :: runoff_out(nx_rivers,ny_rivers)

!end of header
!------------------------------------------------------------------------------

! Convert from riv_pts to rivers grid
runoff_out(:,:) = 0.0
DO ip = 1,riv_pts
  CALL get_xy_pos(rivers_index_rp(ip),nx_rivers,ny_rivers,ix,iy)
  runoff_out(ix,iy) = runoff_riv(ip)
END DO

! Call to lat/lon grid-based regridding routine
maxlin = ( nx_land_grid + nx_rivers ) * ( ny_land_grid + ny_rivers )
CALL regrid_rivers_to_land( maxlin, runoff_out, runoff_grid )

! Convert from full grid to land_pts
DO l = 1,land_pts
  CALL get_xy_pos( global_land_index(l), nx_land_grid, ny_land_grid, ix, iy )
  runoff_land(l) = runoff_grid(ix,iy)
END DO

RETURN
END SUBROUTINE regrid_rivers_to_land_ctl

!##############################################################################

SUBROUTINE regrid_land_to_rivers( maxlin, runoff_grid, runoff_out )
!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid (the model grid) to a target grid (the
!   rivers grid).
!   Both grids must be regular in latitude and longitude.
!
!------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars with intent(in)
     nx_rivers, ny_rivers, rivers_dlat=>rivers_dy, rivers_dlon=>rivers_dx,     &
     rivers_lat1=>rivers_y1, rivers_lon1=>rivers_x1, nx_land_grid,             &
     ny_land_grid, reg_dlat=>land_dy, reg_dlon=>land_dx,                       &
     reg_lat1=>y1_land_grid, reg_lon1=>x1_land_grid

USE areaver_mod, ONLY: pre_areaver, do_areaver

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------

INTEGER, INTENT(IN) :: maxlin  !  a vector length

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) :: runoff_grid(nx_land_grid,ny_land_grid)
                               !  runoff rate on model grid (kg m-2 s-1)

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: runoff_out(nx_rivers,ny_rivers)
                               !  runoff rate on rivers grid (kg m-2 s-1)

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER :: adjust       !  mode (option number) of area averaging
INTEGER :: ix           !  loop counter
INTEGER :: iy           !  loop counter
INTEGER :: maxl         !  number of entries in output from pre_areaver

LOGICAL :: cyclic_srce  !  TRUE source (model) grid is cyclic in x
LOGICAL :: cyclic_targ  !  TRUE target (model) grid is cyclic in x
LOGICAL :: invert_srce  !  TRUE source (model) grid runs N to S
                        !  FALSE source grid runs S to N
LOGICAL :: spherical    !  TRUE  coordinates are lat/lon on a sphere
                        !  FALSE Cartesian axes.
LOGICAL :: want         !  Value of masks at locations for which data are
                        !  required

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
INTEGER :: count_targ(nx_rivers,ny_rivers)
                        !  number of model gridboxes that
                        !  contribute to each rivers gridbox
INTEGER :: base_targ(nx_rivers,ny_rivers)
                        !  base (starting) index for each rivers gridbox
                        !  (i.e. location of first element in lists)
INTEGER :: index_srce(maxlin)
                        !  list of source (model) gridboxes
                        !  that contribute to each rivers gridbox

REAL(KIND=real_jlslsm) :: sourcelat(ny_land_grid+1)
                        !  latitudes of edges of source (model) gridboxes.
                        !  First value is the S edge of first gridbox,
                        !  all other values are the N edge of each gridbox.
REAL(KIND=real_jlslsm) :: sourcelon(nx_land_grid+1)
                        !  longitudes of edges of source (model) gridboxes
                        !  First value is the W edge of first gridbox, all
                        !  other values are the E edge of each gridbox.
REAL(KIND=real_jlslsm) :: targetlat(ny_rivers+1)
                        !  latitudes of edges of target (rivers) gridboxes.
                        !  First value is the S edge of first gridbox, all
                        !  other values are the N edge of each gridbox.
REAL(KIND=real_jlslsm) :: targetlon(nx_rivers+1)
                        !  longitudes of edges of target (rivers) gridboxes
                        !  First value is the W edge of first gridbox, all
                        !  other values are the E edge of each gridbox.

REAL(KIND=real_jlslsm) :: adjust_targ(nx_rivers,ny_rivers)
                        !  adjustment factors (not used)
REAL(KIND=real_jlslsm) :: weight(maxlin)
                        !  lists of weights for each source (model) gridbox

LOGICAL :: mask_srce(nx_land_grid,ny_land_grid)
LOGICAL :: mask_targ(nx_rivers,ny_rivers)

!------------------------------------------------------------------------------

! Set values

spherical = .TRUE.    !  Calculations are for lat/lon coordinates.
adjust    = 0         !  "normal" area averaging
maxl      = maxlin

! Decide if grids run S-N or N-S in latitude

IF ( reg_dlat > 0.0) THEN
  invert_srce = .FALSE.                   !  model grid runs S to N
ELSE
  invert_srce = .TRUE.                    !  model grid runs N to S
END IF

! Decide if grids are cyclic in longitude.

IF ( REAL(nx_land_grid) * reg_dlon > 359.9 ) THEN
  cyclic_srce = .TRUE.
ELSE
  cyclic_srce = .FALSE.
END IF
IF ( REAL(nx_rivers) * rivers_dlon > 359.9 ) THEN
  cyclic_targ = .TRUE.
ELSE
  cyclic_targ = .FALSE.
END IF

!------------------------------------------------------------------------------
! Set coordinates of edges of model gridboxes
!------------------------------------------------------------------------------
! Note that in standalone JULES we are recalculating coordinates which were
! originally read from an ancillary file, which generally results in values
! that are slightly different from those read in. This could make a difference
! for high resolution runs with large grids.

DO ix = 1,nx_land_grid+1
  sourcelon(ix) = reg_lon1 + (REAL(ix-1) - 0.5) * reg_dlon
END DO
DO iy = 1,ny_land_grid+1
  sourcelat(iy) = reg_lat1 + (REAL(iy-1) - 0.5) * reg_dlat
END DO

!------------------------------------------------------------------------------
! Set coordinates of edges of rivers gridboxes
!------------------------------------------------------------------------------

DO ix = 1,nx_rivers+1
  targetlon(ix) = rivers_lon1 + (REAL(ix-1) - 0.5) * rivers_dlon
END DO
DO iy = 1,ny_rivers+1
  targetlat(iy) = rivers_lat1 + (REAL(iy-1) - 0.5) * rivers_dlat
END DO

!------------------------------------------------------------------------------
! Set masks to indicate that all points in both grids are to be used
!------------------------------------------------------------------------------

want           = .TRUE.
mask_srce(:,:) = want
mask_targ(:,:) = want

!------------------------------------------------------------------------------
! Call setup routing for averaging
!------------------------------------------------------------------------------

CALL pre_areaver( nx_land_grid, sourcelon, ny_land_grid, sourcelat,            &
                  cyclic_srce, nx_land_grid, want, mask_srce,                  &
                  nx_rivers, targetlon, ny_rivers, targetlat,                  &
                  cyclic_targ, spherical, maxl, count_targ, base_targ,         &
                  index_srce, weight )

!------------------------------------------------------------------------------
! Call averaging routine
!------------------------------------------------------------------------------

CALL do_areaver( nx_land_grid, ny_land_grid, nx_land_grid,                     &
                 invert_srce, runoff_grid, nx_rivers, ny_rivers,               &
                 count_targ, base_targ, nx_rivers, want, mask_targ,            &
                 index_srce, weight, adjust, runoff_out, nx_land_grid, 2,      &
                 adjust_targ )

RETURN
END SUBROUTINE regrid_land_to_rivers

!##############################################################################

SUBROUTINE regrid_rivers_to_land( maxlin, runoff_out, runoff_grid )

!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid (the river grid) to a target grid
!   (the model grid).
!   Both grids must be regular in latitude and longitude.
!------------------------------------------------------------------------------

! Modules used:

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars with intent(in)
     nx_rivers, ny_rivers, rivers_dlat=>rivers_dy, rivers_dlon=>rivers_dx,     &
     rivers_lat1=>rivers_y1, rivers_lon1=>rivers_x1, nx_land_grid,             &
     ny_land_grid, reg_dlat=>land_dy, reg_dlon=>land_dx,                       &
     reg_lat1=>y1_land_grid, reg_lon1=>x1_land_grid

USE areaver_mod, ONLY: pre_areaver, do_areaver

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) :: maxlin    !  a vector length

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) :: runoff_out(nx_rivers,ny_rivers)
                                 !  runoff rate on rivers grid (kg m-2 s-1)

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: runoff_grid(nx_land_grid,ny_land_grid)
                                 !  runoff rate on model grid (kg m-2 s-1)

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER :: adjust        !  mode (option number) of area averaging
INTEGER :: ix            !  loop counter
INTEGER :: iy            !  loop counter
INTEGER :: maxl          !  number of entries in output from pre_areaver

LOGICAL :: cyclic_srce   !  TRUE source (model) grid is cyclic in x
LOGICAL :: cyclic_targ   !  TRUE target (model) grid is cyclic in x
LOGICAL :: invert_srce   !  TRUE source (model) grid runs N to S
                         !  FALSE source grid runs S to N
LOGICAL :: spherical     !  TRUE  coordinates are lat/lon on a sphere
                         !  FALSE Cartesian axes.
LOGICAL :: want
                         !  Value of masks at locations for which data
                         !  are required

!------------------------------------------------------------------------------
! Local array variables
!------------------------------------------------------------------------------
INTEGER :: count_targ(nx_land_grid,ny_land_grid)
                         !  number of model gridboxes that contribute to
                         !  each rivers gridbox
INTEGER :: base_targ(nx_land_grid,ny_land_grid)
                         !  base (starting) index for each rivers gridbox
                         !  (i.e. location of first element in lists)
INTEGER :: index_srce(maxlin)
                         !  list of source (model) gridboxes that contribute
                         !  to each rivers gridbox

REAL(KIND=real_jlslsm) :: sourcelat(ny_rivers+1)
                         !  latitudes of edges of target (model) gridboxes.
                         !  First value is the S edge of first gridbox, all
                         !  other values are the N edge of each gridbox.
REAL(KIND=real_jlslsm) :: sourcelon(nx_rivers+1)
                         !  longitudes of edges of target (model) gridboxes
                         !  First value is the W edge of first gridbox, all
                         !  other values are the E edge of each gridbox.
REAL(KIND=real_jlslsm) :: targetlat(ny_land_grid+1)
                         !  latitudes of edges of source (rivers) gridboxes.
                         !  First value is the S edge of first gridbox, all
                         !  other values are the N edge of each gridbox.
REAL(KIND=real_jlslsm) :: targetlon(nx_land_grid+1)
                         !  longitudes of edges of source (rivers) gridboxes
                         !  First value is the W edge of first gridbox, all
                         !  other values are the E edge of each gridbox.

REAL(KIND=real_jlslsm) :: adjust_targ(nx_land_grid,ny_land_grid)
                         !  adjustment factors (not used)
REAL(KIND=real_jlslsm) :: weight(maxlin)
                         !  lists of weights for each source (model) gridbox

LOGICAL :: mask_srce(nx_rivers,ny_rivers)
LOGICAL :: mask_targ(nx_land_grid,ny_land_grid)

!------------------------------------------------------------------------------

! Set values
spherical   = .TRUE.    !  Calculations are for lat/lon coordinates.
adjust      = 0         !  "normal" area averaging
maxl        = maxlin
invert_srce = .FALSE.   !  rivers grid runs S to N

! Decide if grids are cyclic in longitude

IF ( REAL(nx_rivers) * rivers_dlon > 359.9 ) THEN
  cyclic_srce = .TRUE.
ELSE
  cyclic_srce = .FALSE.
END IF
IF ( REAL(nx_land_grid) * reg_dlon > 359.9 ) THEN
  cyclic_targ = .TRUE.
ELSE
  cyclic_targ = .FALSE.
END IF

!------------------------------------------------------------------------------
! Set coordinates of edges of rivers gridboxes
!------------------------------------------------------------------------------

DO ix = 1,nx_rivers+1
  sourcelon(ix) = rivers_lon1 + (REAL(ix-1) - 0.5) * rivers_dlon
END DO
DO iy = 1,ny_rivers+1
  sourcelat(iy) = rivers_lat1 + (REAL(iy-1) - 0.5) * rivers_dlat
END DO

!------------------------------------------------------------------------------
! Set coordinates of edges of model gridboxes
!------------------------------------------------------------------------------

DO ix = 1,nx_land_grid+1
  targetlon(ix) = reg_lon1 + (REAL(ix-1) - 0.5) * reg_dlon
END DO
DO iy = 1,ny_land_grid+1
  targetlat(iy) = reg_lat1 + (REAL(iy-1) - 0.5) * reg_dlat
END DO

!------------------------------------------------------------------------------
! Set masks to indicate that all points in both grids are to be used
!------------------------------------------------------------------------------

want           = .TRUE.
mask_srce(:,:) = want
mask_targ(:,:) = want

!------------------------------------------------------------------------------
! Call setup rivers for averaging.
!------------------------------------------------------------------------------

CALL pre_areaver( nx_rivers, sourcelon, ny_rivers, sourcelat, cyclic_srce,     &
                  nx_rivers, want, mask_srce, nx_land_grid, targetlon,         &
                  ny_land_grid, targetlat, cyclic_targ, spherical, maxl,       &
                  count_targ, base_targ, index_srce, weight )

!------------------------------------------------------------------------------
! Call averaging routine.
!------------------------------------------------------------------------------

CALL do_areaver( nx_rivers, ny_rivers, nx_rivers, invert_srce, runoff_out,     &
                 nx_land_grid, ny_land_grid, count_targ, base_targ,            &
                 nx_land_grid, want, mask_targ, index_srce, weight, adjust,    &
                 runoff_grid, nx_rivers, 2, adjust_targ )

RETURN
END SUBROUTINE regrid_rivers_to_land
!##############################################################################

SUBROUTINE twod_to_rp( field_2d, field_rp, rivers )
!------------------------------------------------------------------------------
! Description:
!   Converts from a 2D global grid to a 1D rivers grid
!   with no remapping or interpolation.
!
!------------------------------------------------------------------------------
! Modules used:
USE jules_rivers_mod, ONLY: nx_rivers, ny_rivers, np_rivers, rivers_type

IMPLICIT NONE

REAL, INTENT(IN) :: field_2d(nx_rivers,ny_rivers)
                        !  Input 2D global field
REAL, INTENT(OUT) :: field_rp(np_rivers)
                        !  Output 1D river points field
TYPE(rivers_type), INTENT(IN) :: rivers
                        !  Extra river information

! Local scalar variables

INTEGER :: ip            !  loop counter
INTEGER :: ilon          !  longitude index
INTEGER :: ilat          !  latitude index

DO ip = 1,np_rivers
  ilon = rivers%rivers_ilon_rp(ip)
  ilat = rivers%rivers_ilat_rp(ip)
  field_rp(ip) = field_2d(ilon,ilat)
END DO

END SUBROUTINE twod_to_rp

!###############################################################################

SUBROUTINE rp_to_twod( field_rp, field_2d, rivers )
!------------------------------------------------------------------------------
! Description:
!   Converts from a 1D rivers grid to a 2D global grid
!   with no remapping or interpolation.
!
!------------------------------------------------------------------------------
! Modules used:
USE jules_rivers_mod, ONLY: nx_rivers, ny_rivers, np_rivers, rivers_type

IMPLICIT NONE

REAL, INTENT(IN) :: field_rp(np_rivers)
                        !  Input 1D river points field
REAL, INTENT(OUT) :: field_2d(nx_rivers,ny_rivers)
                        !  Output 2D global field
TYPE(rivers_type), INTENT(IN) :: rivers
                        !  Extra river information

! Local scalar variables

INTEGER :: ip            !  loop counter
INTEGER :: ilon          !  longitude index
INTEGER :: ilat          !  latitude index

field_2d(:,:) = 0.0

DO ip = 1,np_rivers
  ilon = rivers%rivers_ilon_rp(ip)
  ilat = rivers%rivers_ilat_rp(ip)
  field_2d(ilon,ilat) = field_rp(ip)
END DO

END SUBROUTINE rp_to_twod


END MODULE rivers_regrid_mod
