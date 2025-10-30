#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE fill_model_grid_arrays_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE fill_model_grid_arrays( ainfo_data, coastal_data )

USE model_grid_mod, ONLY: projection_x_coord, projection_x_coord_land,         &
                          projection_y_coord, projection_y_coord_land,         &
                          latitude, longitude

USE coastal, ONLY: flandg

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE ancil_info, ONLY: land_pts

USE datetime_mod,  ONLY: l_local_solar_time

!TYPE definitions
USE ancil_info,    ONLY: ainfo_data_type
USE coastal,       ONLY: coastal_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Fills previously allocated model grid arrays with data
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
!Arguments
!TYPES containing field data (IN OUT)
TYPE(ainfo_data_type),   INTENT(IN OUT) :: ainfo_data
TYPE(coastal_data_type), INTENT(IN OUT) :: coastal_data

INTEGER :: i, j, l  ! Index variables

ainfo_data%land_mask(:,:) = ( flandg(:,:) > EPSILON(1.0) )

ainfo_data%latitude(:,:)  = latitude(:,:)
IF (l_local_solar_time) THEN
  ! If local time given instead of UTC, overwrite longitude to remove any
  ! longitude-dependent offsets, so that diagnostics are in local time as well
  ! (as done in the UM for single column model with local_time)
  ainfo_data%longitude(:,:) = 0.0
ELSE
  ainfo_data%longitude(:,:) = longitude(:,:)
END IF

ALLOCATE( projection_x_coord_land(land_pts) )
ALLOCATE( projection_y_coord_land(land_pts) )

l = 0
DO j = 1,t_j_length
  DO i = 1,t_i_length
    IF ( ainfo_data%land_mask(i,j) ) THEN
      l = l + 1
      ainfo_data%land_index(l)   = (j-1) * t_i_length + i
      coastal_data%fland(l)      = flandg(i,j)
      projection_x_coord_land(l) = projection_x_coord(i,j)
      projection_y_coord_land(l) = projection_y_coord(i,j)
    END IF
  END DO
END DO

! Initialise ocn_cpl_point as false for standalone JULES
! (i.e. not coupled to ocean)
ainfo_data%ocn_cpl_point(:,:) = .FALSE.

RETURN

END SUBROUTINE fill_model_grid_arrays

END MODULE fill_model_grid_arrays_mod
#endif
