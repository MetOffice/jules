! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!     Science routines for calculating river flow routing
!     using the RFM kinematic wave model
!     see Bell et al. 2007 Hydrol. Earth Sys. Sci. 11. 532-549
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

MODULE rivers_route_rfm_mod

CONTAINS

!#############################################################################
! subroutine rivers_route_rfm
!
!-----------------------------------------------------------------------------
! Description:
!   Perform the kinematic wave routing of surface and sub-surface runoff
!   Calculates river outflow (kg m-2 s-1) and baseflow (kg m-2 s-1) for the
!   RFM kinematic wave river routing model.
!
! Method:
!   See Bell et al. 2007 Hydrol. Earth Sys. Sci. 11. 532-549
!
! Author: V.A.Bell, CEH Wallingford, 21.08.03
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
! Code History:
!    Modified from river2a.f by sjd 13/05/05
!    Modified from UM routine riv_rout-river2a.F90 by hl 13/04/14
!    Updated to route runoff on riv_pts vector only by hl 24/04/14
!
!  MODEL            MODIFICATION HISTORY FROM MODEL VERSION 5.5:
! VERSION  DATE
!   6.0   12/09/03  Change DEF from A20 to A26. D. Robinson
!   6.0   12.09.03  Routing code added. V.A.Bell
!   x.x   02/05/12  Additional implementation and developments S. Dadson
!   x.x   06/01/15  Formal implementation within JULES code base H. Lewis
!
! NOTE ON UNITS:
!   This routine, based on Simon Dadson's work, includes a modification to the
!   'standard' RFM routines which assume a regular x/y-grid based
!   implementation to account for potential variable grid box areas (e.g. from
!   lat/lon grid).
!   Stores are calculated in units of m x m2 rather than mm, and flows are
!   initially calculated in units of m3/s.
!   For consistency with other routines (for now), the output is converted
!   again to a flux density kg/m2/s

SUBROUTINE rivers_route_rfm( sfc_runoff, sub_sfc_runoff, outflow, baseflow,    &
                             abstracted_res_rp, rivers )

USE jules_rivers_mod, ONLY:                                                    &
!  imported scalars
       nstep_rivers, np_rivers, river_mouth                                    &
       ,rivers_first, rivers_length, runoff_factor, cland, criver, cbland      &
       ,cbriver, retl, retr, rfm_land, rfm_river, rfm_sea,  l_reservoirs,      &
! imported type
        rivers_type

USE jules_water_resources_mod, ONLY: sw_river_source

USE timestep_mod, ONLY: timestep

USE water_constants_mod, ONLY: rho_water

USE jules_print_mgr, ONLY:                                                     &
   jules_message,                                                              &
   jules_print

!-----------------------------------------------------------------------------

USE route_reservoirs_mod, ONLY:                                                &
! imported procedures
     route_reservoirs

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! IN Arguments

! Array arguments with intent(in)
REAL(KIND=real_jlslsm), INTENT(IN) :: sfc_runoff(np_rivers)
       !  average rate of surface runoff since last rivers call (kg m-2 s-1)
       !  This includes any abstraction of water for water resources.
REAL(KIND=real_jlslsm), INTENT(IN) :: sub_sfc_runoff(np_rivers)
       !  average rate of sub-surface runoff since last call (kg m-2 s-1)
REAL(KIND=real_jlslsm), INTENT(IN) :: abstracted_res_rp(np_rivers)
       !  Water abstracted from reservoirs over river timestep (kg).

REAL(KIND=real_jlslsm), INTENT(OUT) :: outflow(np_rivers)
       !  rate of channel surface flow leaving gridbox (kg m-2 s-1)
REAL(KIND=real_jlslsm), INTENT(OUT) :: baseflow(np_rivers)
       !  rate of channel base flow leaving gridbox (kg m-2 s-1)

! internal variables
INTEGER ::                                                                     &
     landtype                                                                  &
       !  local for land type
     ,rn                                                                       &
       !  local co-ords of downstream point
     ,ip
       !  co-ordinate counters in do loops

REAL(KIND=real_jlslsm) ::                                                      &
   landtheta, rivertheta                                                       &
       !  surface wave speed factors
   ,sublandtheta, subrivertheta                                                &
       !  sub-surface wave speed factors
   ,returnflow                                                                 &
       !  returnflow (m3 per timestep)
   ,flowobs1_m3s                                                               &
       !  initial river flow [m3/s]
   ,dt                                                                         &
       !  river routing model timestep (s)
   ,dx                                                                         &
       !  distance between midpoints of neighbouring cells (m)
   ,reservoir_flow
       !  flow in and out of reservoir (kg s-1)

REAL(KIND=real_jlslsm) ::                                                      &
   substore_n(np_rivers)                                                       &
       !   subsurface store at next timestep (m3 per timestep)
   ,surfstore_n(np_rivers)                                                     &
       !   surface store at next timestep (m3 per timestep)
   ,flowin_n(np_rivers)                                                        &
       !   surface lateral inflow next time (m3 per timestep)
   ,bflowin_n(np_rivers)                                                       &
       !   sub-surface lateral inflow next time (m3 per timestep)
   ,surf_roff(np_rivers)                                                       &
       !   INTERNAL surf_runoff (m3 per timestep)
   ,sub_surf_roff(np_rivers)
       !   INTERNAL sub_surf_runoff (m3 per timestep)

! Rivers Arrays
TYPE(rivers_type), INTENT(IN OUT) :: rivers


!-----------------------------------------------------------------------------
! Set up rivers parameters
!-----------------------------------------------------------------------------

! Calculate rivers model timestep length.
dt = REAL(nstep_rivers) * timestep

! Wave speed factors (dimensionless)
rivertheta    = criver  * dt / rivers_length
landtheta     = cland   * dt / rivers_length
sublandtheta  = cbland  * dt / rivers_length
subrivertheta = cbriver * dt / rivers_length

! Check condition for numerical stability
IF (landtheta > 1.0 .OR. sublandtheta > 1.0 .OR.                               &
    rivertheta > 1.0 .OR. subrivertheta > 1.0) THEN

  WRITE(jules_message,*)'WARNING: rivers_route_rfm: ' //                       &
                        'Finite difference method will be unstable in RFM,'    &
                        // ' setting thetas to zero'
  CALL jules_print('rivers_route_rfm',jules_message)

  rivertheta    = 0.0
  landtheta     = 0.0
  sublandtheta  = 0.0
  subrivertheta = 0.0
END IF

!-----------------------------------------------------------------------------
! Initialise variables at first timestep
!-----------------------------------------------------------------------------
IF (rivers_first) THEN

  DO ip = 1, np_rivers
    ! Initialise surface and sub-surface stores using flow observations if
    ! available.
    IF ( rivers%rfm_flowobs1_rp(ip) > 0.0 ) THEN
      flowobs1_m3s = rivers%rfm_flowobs1_rp(ip) *                              &
        rivers%rivers_boxareas_rp(ip) / rho_water
      rivers%rfm_surfstore_rp(ip) = flowobs1_m3s * dt / rivertheta
      rivers%rfm_substore_rp(ip) = flowobs1_m3s * dt / subrivertheta
    END IF
  END DO

END IF   ! end rivers_first

!-----------------------------------------------------------------------------
! Processing for each timestep
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Convert runoff from (kg m-2 s-1) to (m3 per gridcell per timestep)
!-----------------------------------------------------------------------------
DO ip = 1,np_rivers
  ! Historically RFM has ignored negative runoff fluxes, but that is not
  ! appropriate if water is abstracted from rivers for water resource purposes
  ! (when the configuration includes sw_river_source > 0). It would be
  ! preferable not to use water resource information in here (and for RFM to
  ! always route what it is given).
  IF ( (sw_river_source == 0 .AND. sfc_runoff(ip) >= 0.0 .AND.                 &
        sub_sfc_runoff(ip) >= 0.0)                                             &
       .OR. sw_river_source > 0 ) THEN

    surf_roff(ip) = runoff_factor * sfc_runoff(ip) *                           &
                          dt * rivers%rivers_boxareas_rp(ip) / rho_water
    sub_surf_roff(ip) = runoff_factor * sub_sfc_runoff(ip) *                   &
                          dt * rivers%rivers_boxareas_rp(ip) / rho_water
  ELSE
    ! ignore no data (-1.00e20) and other negative values
    surf_roff(ip) = 0.0
    sub_surf_roff(ip) = 0.0
  END IF

  !---------------------------------------------------------------------------
  ! Initialise accumulated inflows and stores for the next timestep
  !---------------------------------------------------------------------------
  flowin_n(ip)    = 0.0
  bflowin_n(ip)   = 0.0
  surfstore_n(ip) = 0.0
  substore_n(ip)  = 0.0
END DO

!-----------------------------------------------------------------------------
! Rivers runoff using simple kinematic wave model (see Lewis et al. 2018,
! Appx.B).
!-----------------------------------------------------------------------------
DO ip = 1,np_rivers

  rn = rivers%rivers_next_rp(ip)
  landtype = rivers%rfm_land_rp(ip)

  !-------------------------------------------------------------------------------
  !   If reservoirs are considered and capacity > 0, route through reservoirs.
  !-------------------------------------------------------------------------------
  IF (l_reservoirs) THEN
    IF (rivers%res_cap_current(ip) > 0.0) THEN
      reservoir_flow = rivers%rfm_flowin_rp(ip) * 1000.0 / dt
      CALL route_reservoirs(dt, abstracted_res_rp(ip),                         &
                                  rivers%res_cap_current(ip),                  &
                                  rivers%res_catch(ip),                        &
                                  rivers%res_critical(ip),                     &
                                  rivers%res_flood(ip),                        &
                                  rivers%res_emergency(ip),                    &
                                  rivers%res_normal_release(ip),               &
                                  rivers%res_flood_release(ip),                &
                                  rivers%res_storage(ip), reservoir_flow)
      rivers%rfm_flowin_rp(ip) = reservoir_flow * dt / 1000.0
    END IF
  END IF

  IF (landtype == rfm_land) THEN  !Gridcell is land

    ! land surface (Lewis et al. 2018:eqnB4)
    surfstore_n(ip) = (1.0 - landtheta) * rivers%rfm_surfstore_rp(ip) +        &
                             rivers%rfm_flowin_rp(ip) + surf_roff(ip)

    ! land subsurface (Lewis et al. 2018:eqnB4)
    substore_n(ip) = (1.0 - sublandtheta) * rivers%rfm_substore_rp(ip) +       &
                             rivers%rfm_bflowin_rp(ip) + sub_surf_roff(ip)

    ! return flow
    IF (retl > 0) THEN
      returnflow = MAX( ABS( substore_n(ip) * retl ), 0.0 )
    ELSE
      returnflow = -1.0 * MAX( ABS( surfstore_n(ip) * retl ), 0.0 )
    END IF

    substore_n(ip)  = substore_n(ip)  - returnflow
    surfstore_n(ip) = surfstore_n(ip) + returnflow

    rivers%rfm_rivflow_rp(ip)  = rivers%rfm_surfstore_rp(ip) *                 &
                                   (landtheta    / dt)
    rivers%rfm_baseflow_rp(ip) = rivers%rfm_substore_rp(ip)  *                 &
                                   (sublandtheta / dt)

    IF ( rn > 0 ) THEN
      ! Add to inflow to the next point downstream.
      flowin_n(rn)  = flowin_n(rn)  + landtheta   * rivers%rfm_surfstore_rp(ip)
      bflowin_n(rn) = bflowin_n(rn) + sublandtheta * rivers%rfm_substore_rp(ip)
    END IF

  ELSE IF (landtype == rfm_river) THEN  !Gridcell is river

    ! river subsurface (Lewis et al. 2018:eqnB4)
    substore_n(ip) = (1.0 - subrivertheta) * rivers%rfm_substore_rp(ip) +      &
                          rivers%rfm_bflowin_rp(ip) + sub_surf_roff(ip)

    ! river surface (Lewis et al. 2018:eqnB4)
    surfstore_n(ip) = (1.0 - rivertheta) * rivers%rfm_surfstore_rp(ip) +       &
                            rivers%rfm_flowin_rp(ip) + surf_roff(ip)

    ! return flow
    IF (retr > 0) THEN
      returnflow = MAX( ABS( substore_n(ip) * retr ), 0.0 )
    ELSE
      returnflow = -1.0 * MAX( ABS( surfstore_n(ip) * retr ), 0.0 )
    END IF
    substore_n(ip)  = substore_n(ip)  - returnflow
    surfstore_n(ip) = surfstore_n(ip) + returnflow

    rivers%rfm_rivflow_rp(ip)  = rivers%rfm_surfstore_rp(ip) *                 &
                                  (rivertheta    / dt)
    rivers%rfm_baseflow_rp(ip) = rivers%rfm_substore_rp(ip)  *                 &
                                  (subrivertheta / dt)

    IF ( rn > 0 ) THEN
      ! Add to inflow to the next point downstream.
      flowin_n(rn)  = flowin_n(rn)  + rivertheta    *                          &
                        rivers%rfm_surfstore_rp(ip)
      bflowin_n(rn) = bflowin_n(rn) + subrivertheta *                          &
                        rivers%rfm_substore_rp(ip)
    END IF

  END IF ! land or river

END DO !end of rivers loop, ip

!-----------------------------------------------------------------------------
! Save the outflow going into the sea.
!-----------------------------------------------------------------------------
DO ip = 1,np_rivers
  IF ( rivers%rivers_next_rp(ip) == river_mouth ) THEN
    rivers%rivers_outflow_rp(ip) = rivers%rfm_rivflow_rp(ip)  * rho_water
  END IF
END DO

!-----------------------------------------------------------------------------
! Housekeeping for next timestep
!-----------------------------------------------------------------------------
DO ip = 1,np_rivers
  ! keep inflows for next timestep
  rivers%rfm_flowin_rp(ip)  = flowin_n(ip)
  rivers%rfm_bflowin_rp(ip) = bflowin_n(ip)

  ! keep rivers stores for next timestep (m3)
  rivers%rfm_surfstore_rp(ip) = surfstore_n(ip)
  rivers%rfm_substore_rp(ip)  = substore_n(ip)

  !---------------------------------------------------------------------------
  ! Return flows in flux density units kg/m2/s
  !---------------------------------------------------------------------------
  outflow(ip)  = rivers%rfm_rivflow_rp(ip)  *                                  &
                  rho_water / rivers%rivers_boxareas_rp(ip)
  baseflow(ip) = rivers%rfm_baseflow_rp(ip) *                                  &
                  rho_water / rivers%rivers_boxareas_rp(ip)
END DO

END SUBROUTINE rivers_route_rfm

!#############################################################################
!#############################################################################

END MODULE rivers_route_rfm_mod
