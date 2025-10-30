! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE add_dump_coords_for_rivers_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE add_dump_coords_for_rivers(nvars, identifiers)

USE model_grid_mod, ONLY: l_coord_latlon

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Add latitude and longitude of river points, and possibly projection
!   coordinates, to the list to help offline inspection of the dump file, ie
!   they are diagnostics, not prognostics or ancillaries. Projection
!   coordinates are only included if they are not latitude and longitude -
!   primarily to preserve existing results.
!
!   Lat & lon (and projection coords) will not be read in from the dump to
!   prevent confusion with the model grid namelists.

! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN OUT) :: nvars  ! The number of variables
CHARACTER(LEN=*), INTENT(IN OUT) :: identifiers(:)
                               ! The model identifiers of the required
                               ! variables

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'ADD_DUMP_COORDS_FOR_RIVERS'
    ! The name of this routine.

!-----------------------------------------------------------------------------

! Add latitude and longitude of river points.
nvars = nvars + 1
identifiers(nvars) = 'rivers_lat_rp'
nvars = nvars + 1
identifiers(nvars) = 'rivers_lon_rp'
IF ( .NOT. l_coord_latlon ) THEN
  nvars = nvars + 1
  identifiers(nvars) = 'rivers_x_coord_rp'
  nvars = nvars + 1
  identifiers(nvars) = 'rivers_y_coord_rp'
END IF

RETURN

END SUBROUTINE add_dump_coords_for_rivers
END MODULE add_dump_coords_for_rivers_mod
