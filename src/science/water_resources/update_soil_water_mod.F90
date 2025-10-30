#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE update_soil_water_mod

!------------------------------------------------------------------------------
! Description:
!   Code to update soil and groundwater stores with increments from water
!   resource management.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
! Code Description:
!   Language: Fortran 90.
!------------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC update_soil_water

! Module parameters.
CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'UPDATE_SOIL_WATER_MOD'

CONTAINS

!##############################################################################
!##############################################################################

SUBROUTINE update_soil_water( conveyance_loss, demand_irrig,                   &
                              demand_irrig_soilt, demand_irrig_layer,          &
                              frac_irr_soilt, frac_soilt, gw_abstracted,       &
                              gw_avail_start_soilt, gw_avail_start, land_area, &
                              return_flow_gw, smvcst_soilt, sthf_soilt,        &
                              supply_irrig, smcl_soilt, sthu_irr_soilt,        &
                              sthu_soilt, sthzw_soilt, sub_surf_roff,          &
                              irrig_water_gb )

!------------------------------------------------------------------------------
! Description:
!   Update soil and groundwater stores with increments from water resource
!   management.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts, nsoilt

USE irrigation_mod, ONLY:nlayer_irrig

USE irrigation_water_mod, ONLY: add_irrigation_to_soil

USE jules_soil_mod, ONLY: sm_levels

USE jules_water_resources_mod, ONLY: l_have_renew_gwater,                      &
                                     l_water_irrigation

USE timestep_mod, ONLY: timestep_len=>timestep

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  conveyance_loss(land_pts),                                                   &
    ! Water that is lost during conveyance (kg).
  demand_irrig(land_pts),                                                      &
    ! Demand for irrigation water (kg).
  demand_irrig_soilt(land_pts,nsoilt),                                         &
    ! Demand for irrigation water per soil tile (kg m-2).
  demand_irrig_layer(land_pts,nsoilt,nlayer_irrig),                            &
    ! Demand for irrigation water in each layer (kg m-2).
  frac_soilt(land_pts,nsoilt),                                                 &
    !  Fraction of gridbox for each soil tile.
  frac_irr_soilt(land_pts,nsoilt),                                             &
    ! Irrigation fraction.
  gw_abstracted(land_pts),                                                     &
    ! Water abstracted from groundwater (kg).
  gw_avail_start_soilt(land_pts,nsoilt),                                       &
    ! Groundwater that is available for abstraction at start of timestep, on
    ! soil tiles (kg).
  gw_avail_start(land_pts),                                                    &
    ! Groundwater that is available for abstraction at start of timestep (kg).
  land_area(land_pts),                                                         &
    ! Area of land in each gridbox (m2).
  return_flow_gw(land_pts),                                                    &
    ! Water that is returned to groundwater after use (kg).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  supply_irrig(land_pts)
    ! Water supplied for irrigation (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg m-2).
  sthu_irr_soilt(land_pts,nsoilt,sm_levels),                                   &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation in irrigated fraction.
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.
  sthzw_soilt(land_pts,nsoilt),                                                &
     ! Soil moisture fraction in deep TOPMODEL layer.
  sub_surf_roff(land_pts)
     ! Sub-surface runoff (kg m-2 s-1).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  irrig_water_gb(land_pts)
    ! Water added to soil via irrigation (kg m-2 s-1).

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  l, m
    ! Loop counters.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm)              ::                                         &
  water_added(land_pts)
    ! Water added to groundwater (kg).

!------------------------------------------------------------------------------
!end of header

!------------------------------------------------------------------------------
! Add irrigation to the soil store.
! This should be done before any other water is added to the soil so as to
! avoid any chance of supersaturation - the irrigation requirement was
! calculated for the current soil wetness.
!------------------------------------------------------------------------------
IF ( l_water_irrigation ) THEN
  CALL add_irrigation_to_soil( demand_irrig, demand_irrig_soilt,               &
                               demand_irrig_layer,                             &
                               frac_irr_soilt, land_area, smvcst_soilt,        &
                               sthf_soilt, supply_irrig,                       &
                               smcl_soilt, sthu_irr_soilt, sthu_soilt,         &
                               irrig_water_gb )
END IF

!------------------------------------------------------------------------------
! Update groundwater stores.
!------------------------------------------------------------------------------
! Calculate the total water addition - this is the loss during transport
! (conveyance) and part of the return flow.
water_added(:) = conveyance_loss(:) + return_flow_gw(:)

IF ( l_have_renew_gwater ) THEN
  ! Add water to renewable groundwater.
  CALL update_groundwater( frac_soilt, gw_abstracted, water_added,             &
                           gw_avail_start_soilt, gw_avail_start, land_area,    &
                           smvcst_soilt, sthzw_soilt, sub_surf_roff )
ELSE
  ! Without renewable groundwater, the water added is added to sub-surface
  ! runoff. It might be better to add to soil moisture, but that has not been
  ! coded.
  DO l = 1, land_pts
    sub_surf_roff(l) = sub_surf_roff(l) + water_added(l)                       &
                                          / ( land_area(l) * timestep_len )
  END DO
END IF

END SUBROUTINE update_soil_water

!##############################################################################
!##############################################################################

SUBROUTINE update_groundwater( frac_soilt, gw_abstracted, gw_added,            &
                               gw_avail_start_soilt, gw_avail_start,           &
                               land_area, smvcst_soilt, sthzw_soilt,           &
                               sub_surf_roff )

!------------------------------------------------------------------------------
! Description:
!   Update groundwater stores given abstraction and addition terms.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts, nsoilt

USE jules_hydrology_mod, ONLY: l_top, zw_max

USE jules_soil_mod, ONLY: dzsoil, sm_levels

USE timestep_mod, ONLY: timestep_len=>timestep

USE water_constants_mod, ONLY: rho_water

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_soilt(land_pts,nsoilt),                                                 &
    !  Fraction of gridbox for each soil tile.
  gw_abstracted(land_pts),                                                     &
    ! Water abstracted from groundwater (kg).
  gw_added(land_pts),                                                          &
    ! Water added to groundwater (kg).
  gw_avail_start_soilt(land_pts,nsoilt),                                       &
    ! Groundwater that is available for abstraction at start of timestep, on
    ! soil tiles (kg).
  gw_avail_start(land_pts),                                                    &
    ! Groundwater that is available for abstraction at start of timestep (kg).
  land_area(land_pts),                                                         &
    ! Area of land in gridbox (m2).
  smvcst_soilt(land_pts,nsoilt,sm_levels)
    ! Volumetric saturation point (m3/m3 of soil).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  sthzw_soilt(land_pts,nsoilt),                                                &
     ! Soil moisture fraction in deep TOPMODEL layer.
  sub_surf_roff(land_pts)
     ! Sub-surface runoff (kg m-2 s-1).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  gw_avail_min = 1.0e-10
    ! Minimum amount of water below which calculations are not performed (kg).

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'UPDATE_GROUNDWATER'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  l, m
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  abstracted,                                                                  &
    ! Amount of water abstracted (kg).
  dsmcl,                                                                       &
    ! Increment in soil moisture (kg m-2).
  dz_rho,                                                                      &
    ! The product of a thickness and a density (kg m-2).
  smcl,                                                                        &
    ! Soil moisture (lg m-2).
  smcl_sat
    ! Soil moisture at saturation (lg m-2).

!------------------------------------------------------------------------------
!end of header

! At present we only include TOPMODEL water.
IF ( l_top ) THEN

  ! Remove and add water from/to the TOPMODEL "water table" layer.
  ! Water added is split between soil tiles in proportion to area.
  ! Water abstracted is split between tiles in proportion to the available
  ! water. Because the total abstraction is <= the total available water,
  ! no tile should become over-depleted. If a tile becomes saturated, any
  ! excess is passed out of this routine.

  ! Calculate thickness of "water table" layer * density.
  dz_rho = ( zw_max - SUM(dzsoil(1:sm_levels)) ) * rho_water

  DO m = 1,nsoilt
    DO l = 1,land_pts

      ! Calculate amount to be abstracted from this tile. If there was
      ! only a tiny amount available we ignore extraction, which will
      ! result in a similarly tiny lack of conservation of water.
      IF ( gw_avail_start(l) > gw_avail_min ) THEN
        abstracted = gw_abstracted(l)                                          &
                     * gw_avail_start_soilt(l,m) / gw_avail_start(l)
      ELSE
        abstracted = 0.0
      END IF
      ! Calculate net amount to be added (kg m-2).
      dsmcl = ( gw_added(l) * frac_soilt(l,m) - abstracted ) / land_area(l)

      ! Calculate storage currently and at saturation.
      smcl     = sthzw_soilt(l,m) * smvcst_soilt(l,m,sm_levels) * dz_rho
      smcl_sat = smvcst_soilt(l,m,sm_levels) * dz_rho

      ! Update storage. We restrict values to be >=0 to avoid creating small
      ! (absolute value) negative amounts. These small amounts of water will
      ! not be conserved by this procedure. Note that even if no water is
      ! being added or removed, this will remove any (unphysical) negative
      ! wetness values that are input.
      smcl = MAX( smcl + dsmcl, 0.0 )
      IF ( smcl > smcl_sat ) THEN
        ! Add excess water to subsurface runoff.
        sub_surf_roff(l) = sub_surf_roff(l)                                    &
                           + ( smcl - smcl_sat ) * frac_soilt(l,m)             &
                             / timestep_len
        smcl = smcl_sat
      END IF
      ! Update the prognostic variable.
      sthzw_soilt(l,m) = smcl / smcl_sat

    END DO
  END DO

END IF  !  l_top

RETURN
END SUBROUTINE update_groundwater

!##############################################################################
!##############################################################################

END MODULE update_soil_water_mod
#endif
