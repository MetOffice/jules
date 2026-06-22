! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!     Driver and science routines for calculating river flow routing
!     using a kinematic wave model
!     RFM: see Bell et al. 2007 Hydrol. Earth Sys. Sci. 11. 532-549
!     TRIP: see Oki et al 1999 J.Met.Soc.Japan, 77, 235-255.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

MODULE rivers_route_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CONTAINS

!##############################################################################

SUBROUTINE rivers_route_rp(rivers)

!------------------------------------------------------------------------------
!
! Description:
!   Perform the routing of surface and sub-surface runoff defined on riverpts
!   This passes the total runoff in the river grid to the appropriate routines
!   to be routed.
!
!------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars with intent(in)
     i_river_vn, rivers_camaflood, rivers_rfm, rivers_trip, np_rivers,         &
     rivers_type

USE rivers_route_camaflood_mod, ONLY:                                          &
!  imported procedures
     rivers_route_camaflood

USE rivers_route_rfm_mod, ONLY:                                                &
!  imported procedures
     rivers_route_rfm

USE rivers_route_trip_mod, ONLY:                                               &
!  imported procedures
     rivers_route_trip

USE jules_print_mgr, ONLY:                                                     &
  jules_message,                                                               &
  jules_print

IMPLICIT NONE

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER :: ip                !  loop counter

!------------------------------------------------------------------------------
! Local array variables
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: baseflow(np_rivers)
                             !  rate of channel base flow leaving
                             !  gridbox (kg m-2 s-1)

!end of header

!------------------------------------------------------------------------------
! Calculate total runoff diagnostic.
!------------------------------------------------------------------------------
DO ip = 1, np_rivers
  rivers%rrun_rp(ip) = rivers%rrun_surf_rp(ip) + rivers%rrun_sub_surf_rp(ip)
END DO

!------------------------------------------------------------------------------
! Call the routing science routine.
!------------------------------------------------------------------------------
SELECT CASE ( i_river_vn )

CASE ( rivers_camaflood )
  CALL rivers_route_camaflood( rivers%rivers_next_rp, rivers%mean_sea_level,   &
                               rivers%rivers_boxareas_rp,                      &
                               rivers%river_elevation, rivers%sea_level,       &
                               rivers%rrun_sub_surf_rp, rivers%rrun_surf_rp,   &
                               rivers%flood_flow, rivers%rflow_rp,             &
                               rivers%river_channel_flow, rivers%river_depth,  &
                               rivers%rivers_outflow_rp )

CASE ( rivers_rfm )
  CALL rivers_route_rfm( rivers%rrun_surf_rp, rivers%rrun_sub_surf_rp,         &
                         rivers%rflow_rp, baseflow,                            &
                        !  imported river arrays
                         rivers)
CASE ( rivers_trip )
  CALL rivers_route_trip( rivers%rrun_surf_rp, rivers%rrun_sub_surf_rp,        &
                          rivers%rflow_rp, baseflow, rivers%rivers_outflow_rp, &
                          rivers%rivers_next_rp, rivers%rivers_seq_rp,         &
                          rivers%rivers_sto_rp,rivers%rivers_boxareas_rp,      &
                          rivers%rivers_lat_rp, rivers%rivers_lon_rp,          &
                          rivers%inland_outflow_rp, rivers%land_fraction_rp  )
CASE DEFAULT
  WRITE(jules_message,*) 'ERROR: rivers_drive: ' //                            &
                         'do not recognise i_river_vn=', i_river_vn
  CALL jules_print('rivers_route_drive',jules_message)

END SELECT

END SUBROUTINE rivers_route_rp

!##############################################################################

SUBROUTINE scatter_land_from_riv_field( global_var_riv, var_land,              &
                                        rivers )
!------------------------------------------------------------------------------
! Description:
!   Uses to the river variable on global river points array (e.g. a prognostic)
!   to fill a variable on land points array, scattered across all processors
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars with intent(in)
    np_rivers, rivers_type
USE rivers_regrid_mod, ONLY: rivpts_to_landpts

#if defined(UM_JULES)
USE um_parallel_mod, ONLY: is_master_task, scatter_land_field
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts
#else
USE parallel_mod, ONLY: is_master_task, scatter_land_field
USE model_grid_mod, ONLY: global_land_pts
#endif

USE ancil_info, ONLY: land_pts
USE ereport_mod, ONLY: ereport

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) :: global_var_riv(np_rivers)
                             ! Input variable on riverpoints

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) :: var_land(land_pts)
                             ! Input variable on riverpoints

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER:: ERROR, errorstatus    ! Error status from ALLOCATE call
INTEGER:: l                     ! Loop counter

!------------------------------------------------------------------------------
! Local array variables
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE :: global_rivers_var_on_landpts(:)
          ! River routing output variable defined on global model land points

TYPE(rivers_type), INTENT(IN) :: rivers

!------------------------------------------------------------------------------
! Initialise
DO l = 1, land_pts
  var_land(l) = 0.0
END DO

!------------------------------------------------------------------------------
! Allocate global variable on land points
IF ( is_master_task() ) THEN
  ALLOCATE(global_rivers_var_on_landpts(global_land_pts), STAT = ERROR)
  IF ( ERROR == 0 ) THEN
    DO l = 1, global_land_pts
      global_rivers_var_on_landpts(l) = 0.0
    END DO
  END IF
ELSE
  ALLOCATE(global_rivers_var_on_landpts(1), STAT = ERROR)
  IF ( ERROR == 0 ) THEN
    global_rivers_var_on_landpts = 0.0
  END IF
END IF

IF ( ERROR /= 0 ) THEN
  errorstatus = 10
  CALL ereport( 'scatter_land_from_riv_field', errorstatus,                    &
                "Error related to allocation in scatter_land_from_riv." )
END IF


!------------------------------------------------------------------------------
! Regrid/remap variable from rivers to land grid
IF ( is_master_task() ) THEN

  CALL rivpts_to_landpts( global_land_pts, np_rivers,                          &
                          rivers%map_river_to_land_points,                     &
                          rivers%global_land_index, rivers%rivers_index_rp,    &
                          global_var_riv, global_rivers_var_on_landpts )

END IF

!------------------------------------------------------------------------------
! Scatter land variable onto separate processors
CALL scatter_land_field(global_rivers_var_on_landpts, var_land,                &
                        rivers%global_land_index)

DEALLOCATE(global_rivers_var_on_landpts)

END SUBROUTINE scatter_land_from_riv_field

!##############################################################################

SUBROUTINE adjust_routestore(rivers_adj_on_landpts,                            &
                             rivers )
!------------------------------------------------------------------------------
! Description:
!   Uses to the river adjustment factor on land points (which accounts for
!   water extracted from the river storage for irrigation)
!   to adjust the river storage on river points array (a prognostic).
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars with intent(in)
    np_rivers, rivers_type

USE rivers_regrid_mod, ONLY: landpts_to_rivpts

#if defined(UM_JULES)
USE um_parallel_mod, ONLY: is_master_task, gather_land_field
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts
#else
USE parallel_mod, ONLY: is_master_task, gather_land_field
USE model_grid_mod, ONLY: global_land_pts
#endif

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) :: rivers_adj_on_landpts(:)

TYPE(rivers_type), INTENT(IN) :: rivers

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: ip ! array indices

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE :: global_rivers_adj_on_landpts(:)
          ! rivers adjustment factor on global model land points

REAL(KIND=real_jlslsm) :: rivers_adj_rgrid(np_rivers)
          ! rivers adjustment factor on routing grid

!------------------------------------------------------------------------------

IF ( is_master_task() ) THEN
  ALLOCATE(global_rivers_adj_on_landpts(global_land_pts))
ELSE
  ALLOCATE(global_rivers_adj_on_landpts(1))
END IF

CALL gather_land_field(rivers_adj_on_landpts, global_rivers_adj_on_landpts,    &
                       rivers%global_land_index)

IF ( is_master_task() ) THEN

  DO ip = 1, np_rivers
    rivers_adj_rgrid(ip) = 1.0
  END DO

  !----------------------------------------------------------------------------
  !   Regrid from land to rivers grid
  !----------------------------------------------------------------------------
  CALL landpts_to_rivpts( global_land_pts, np_rivers,                          &
                          rivers%map_river_to_land_points,                     &
                          rivers%global_land_index, rivers%rivers_index_rp,    &
                          global_rivers_adj_on_landpts,                        &
                          rivers_adj_rgrid )

  !----------------------------------------------------------------------------
  ! Apply the correction factor to the route storage, which is a prognostic
  !----------------------------------------------------------------------------
  DO ip = 1,np_rivers
    rivers%rivers_sto_rp(ip) = rivers_adj_rgrid(ip) * rivers%rivers_sto_rp(ip)
  END DO

END IF

DEALLOCATE(global_rivers_adj_on_landpts)

END SUBROUTINE adjust_routestore

!##############################################################################

END MODULE rivers_route_mod
