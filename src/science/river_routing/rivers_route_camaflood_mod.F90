! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!   Science routines for calculating river flow routing using the CaMa-Flood
!   local inertial equation method.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

MODULE rivers_route_camaflood_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  ! Private scope by default
PUBLIC rivers_route_camaflood

CONTAINS

!##############################################################################

SUBROUTINE rivers_route_camaflood( rivers_next_rp, mean_sea_level,             &
               rivers_boxareas_rp, river_elevation, sea_level,                 &
               sub_sfc_runoff, sfc_runoff,                                     &
               flood_flow, outflow, river_channel_flow, river_depth,           &
               rivers_outflow_rp )

USE jules_rivers_mod, ONLY: dt_rivers, np_rivers, np_rivers_upstream,          &
               nstep_rivers, river_mouth

USE timestep_mod, ONLY: timestep

USE water_constants_mod, ONLY: rho_water

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  rivers_next_rp(np_rivers)
    ! Index (river point number) of the next downstream point.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  mean_sea_level(np_rivers),                                                   &
     ! Mean sea level at river mouth, relative to top of river channel (m).
  rivers_boxareas_rp(np_rivers),                                               &
    ! Gridbox area of each river grid pixel (m2).
  river_elevation(np_rivers),                                                  &
   ! Elevation of top of river channel (m).
  sea_level(np_rivers),                                                        &
     ! Sea level at river mouth, relative to top of river channel (m).
  sub_sfc_runoff(np_rivers),                                                   &
    ! Average rate of sub-surface runoff since last call (kg m-2 s-1).
  sfc_runoff(np_rivers)
    ! Average rate of surface runoff since last call (kg m-2 s-1)

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  flood_flow(np_rivers),                                                       &
    ! Flow on floodplain (m3 s-1). This is the flow over the last sub-timestep
    ! in the river period.
  outflow(np_rivers),                                                          &
    ! Total flow (channel + floodplain) leaving gridbox (kg m-2 s-1).
    ! This is the average flow over the river period. Units are m3 s-1 while
    ! the flow is accumulated over the river period, before conversion to
    ! kg m-2 s-1 at the end of the river period.
  river_channel_flow(np_rivers),                                               &
   ! Flow in river channel (m3 s-1). This is the flow over the final           &
   ! sub-timestep in the current river period.
  river_depth(np_rivers),                                                      &
    ! Depth of water in river channel (m).
  rivers_outflow_rp(np_rivers)
    ! River outflow into the ocean (kg s-1), averaged over the river period.

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Loop counter.
  istep,                                                                       &
    ! Loop counter.
  nstep
    ! Number of sub-timesteps within overall river timestep.

REAL(KIND=real_jlslsm) ::                                                      &
  dt,                                                                          &
    ! Sub-timestep length (s).
  dt_over_rho,                                                                 &
    ! Timestep divided by density (m3 s kg-1).
  rho_over_nstep,                                                              &
    ! Density divided by number of sub-timesteps in routing period (kg m-3).
  river_period
    ! The river routing period, that is the length of time between calls to
    ! river routing (s). Within this there can be sub-timesteps of length dt.

!------------------------------------------------------------------------------
! Local array variables
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  boundary_elevation(np_rivers),                                               &
    ! Water level at downstream boundaries (m).
  runoff(np_rivers)
    ! Volume of runoff over a sub-timestep (m3).

!end of header

!------------------------------------------------------------------------------
! Set the water level at boundary points (mouths and inland drainage points).
!------------------------------------------------------------------------------
CALL set_boundary_level( rivers_next_rp, mean_sea_level,                       &
                         river_elevation, sea_level, boundary_elevation )

!------------------------------------------------------------------------------
! Set river timestep length and calculate number of sub-timesteps within this
! call.
!------------------------------------------------------------------------------
! Calculate the river routing period.
river_period = REAL(nstep_rivers) * timestep

! Initialise timestep length to the maximum value. This is <= river_period.
dt = dt_rivers

! If using the adaptive timestep scheme, decide if a shorter step is required.
! This has not yet been included in this code.

! Calculate number of sub-timesteps in the river period.
nstep= NINT( river_period / dt )
! Recalculate timestep length - to ensure that REAL(nstep)*dt = river_period.
dt = river_period / REAL( nstep )

! Calculate some constants.
dt_over_rho    = dt / rho_water
rho_over_nstep = rho_water / REAL( nstep )

!------------------------------------------------------------------------------
! Calculate runoff volume over each sub-timestep (converting kg m-2 s-1 to
! m3). Also initialise output fluxes.
!------------------------------------------------------------------------------
DO i=1,np_rivers
  runoff(i) = ( sfc_runoff(i) + sub_sfc_runoff(i) ) * dt_over_rho              &
              * rivers_boxareas_rp(i)
  outflow(i)           = 0.0
  rivers_outflow_rp(i) = 0.0
  ! Temporary initialisation during development - to avoid compiler complaints.
  flood_flow(i)         = 0.0
  river_channel_flow(i) = 0.0
  river_depth(i)        = 0.0
END DO

!------------------------------------------------------------------------------
! Advance the model by one or more timesteps within the river timestep.
!------------------------------------------------------------------------------
DO istep = 1,nstep

  ! CaMa-Flood physics to be added here.
  ! Variables returned will include river_channel_flow and flood_flow.

  ! Add to the accumulating average flow over the river routing period.
  ! The units of outflow are currently m3 s-1.
  DO i = 1, np_rivers
    outflow(i) = outflow(i) + river_channel_flow(i) + flood_flow(i)
  END DO

END DO  !  istep

!------------------------------------------------------------------------------
! Save the outflow going into the sea. Here we convert units from m3 s-1
! accumulated over the river period to kg s-1.
!------------------------------------------------------------------------------
DO i = np_rivers_upstream+1, np_rivers
  IF ( rivers_next_rp(i) == river_mouth ) THEN
    rivers_outflow_rp(i) = outflow(i) * rho_over_nstep
  END IF
END DO

!------------------------------------------------------------------------------
! Return flows in flux density units kg m-2 s-1 (converted from m3 s-1).
!------------------------------------------------------------------------------
DO i = 1, np_rivers
  outflow(i) = outflow(i) * rho_over_nstep / rivers_boxareas_rp(i)
END DO

END SUBROUTINE rivers_route_camaflood

!##############################################################################


SUBROUTINE set_boundary_level( rivers_next_rp, mean_sea_level,                 &
                               river_elevation, sea_level, boundary_elevation )

! Update the boundary condition: water level at river mouths and inland
! termination points.

USE jules_rivers_mod, ONLY: l_sea_level, l_vary_sea_level, np_rivers,          &
                            np_rivers_upstream, river_mouth

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  rivers_next_rp(np_rivers)
    ! Index (river point number) of the next downstream point.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  mean_sea_level(np_rivers),                                                   &
    ! Mean sea level elevation at river mouth, relative to top of river channel
    ! (m).
  river_elevation(np_rivers),                                                  &
    ! Elevation of top of river channel (m).
  sea_level(np_rivers)
    ! Sea level at river mouth, relative to top of river channel (m).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  boundary_elevation(np_rivers)
    ! Water level at downstream boundaries (m). Values are not set at other
    ! locations.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: i
    ! Loop counter.

!end of header
!------------------------------------------------------------------------------

! Set default values at river mouths and inland termination points.
! The default is to set water surface height equal to the elevation at the top
! of the river channel. This is the final value for inland termination points.
DO i = np_rivers_upstream+1, np_rivers
  boundary_elevation(i) = river_elevation(i)
END DO

! Now deal with options that can alter river mouth values.
IF ( l_sea_level .OR. l_vary_sea_level ) THEN

  ! Loop over points that are river mouths or inland termination points.
  DO i = np_rivers_upstream+1, np_rivers

    ! Check if this is a river mouth.
    IF ( rivers_next_rp(i) == river_mouth ) THEN

      IF ( l_sea_level ) THEN
        ! Add the (relative) mean sea level.
        boundary_elevation(i) = boundary_elevation(i) + mean_sea_level(i)
      END IF

      IF ( l_vary_sea_level ) THEN
        ! Add the time-varying contribution.
        boundary_elevation(i) = boundary_elevation(i) + sea_level(i)
      END IF

    END IF  !  river mouth

  END DO

END IF   !  l_sea_level .OR. l_vary_sea_level

END SUBROUTINE set_boundary_level

!##############################################################################

END MODULE rivers_route_camaflood_mod
