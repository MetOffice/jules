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

MODULE water_resources_drive_mod

!------------------------------------------------------------------------------
! Description:
!   Driver code for water resource management.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
! Code Description:
!   Language: Fortran 90.
!------------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC water_resources_drive

! Module parameters.
CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'WATER_RESOURCES_DRIVE_MOD'

CONTAINS

!#############################################################################

SUBROUTINE water_resources_drive( global_land_pts, priority_order,             &
                                  conv_loss_frac, demand_accum,                &
                                  demand_unmet, gw_abstracted, gw_avail,       &
                                  gw_nr_abstracted, sfc_water_frac,            &
                                  sw_abstracted, sw_avail, water_removed,      &
                                  conveyance_loss, return_flow_gw,             &
                                  return_flow_sw, supply_irrig )

!------------------------------------------------------------------------------
! Description:
!   Driver routine for water resource management.
!------------------------------------------------------------------------------

USE abstract_local_mod, ONLY: abstract_local, abstract_local_gw

USE jules_water_resources_mod, ONLY: l_have_groundwater, l_water_irrigation,   &
      n_sw_source, nwater_use, partition_calc_from_stores, partition_method,   &
      use_environment, use_irrigation

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! Number of land points in the full model grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  priority_order(global_land_pts,nwater_use)
    ! Priorities of water demands at each gridpoint, in order of decreasing
    ! priority. Values are the index in multi-sector arrays.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  conv_loss_frac(global_land_pts),                                             &
    ! Fraction of water that is lost during conveyance from source to user.
  demand_accum(global_land_pts,nwater_use)
    ! Demands for water accumulated over the water resource timestep (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
! Note that many of these are completely calculated inside this subroutine but
! are marked as IN OUT to clarify that they are initialised before being passed
! in.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  demand_unmet(global_land_pts,nwater_use),                                    &
    ! The part of the demand for water that is not satisfied (kg).
  gw_abstracted(global_land_pts),                                              &
    ! Water abstracted from renewable groundwater (kg).
  gw_avail(global_land_pts),                                                   &
    ! Groundwater that is available for abstraction (kg).
    ! This does not include "non-renewable" groundwater.
  gw_nr_abstracted(global_land_pts),                                           &
    ! Water abstracted from non-renewable groundwater (kg).
  sfc_water_frac(global_land_pts),                                             &
    ! Target for the fraction of demand to be met from surface water.
    ! This is INTENT(IN) in some configurations, INTENT(OUT) in others.
  sw_abstracted(global_land_pts,n_sw_source),                                  &
    ! Water abstracted from each surface water source (kg).
  sw_avail(global_land_pts,n_sw_source),                                       &
    ! Surface water that is available for abstraction (kg).
  water_removed(global_land_pts)
    ! Water that is removed from the system during use, e.g. incorporated into
    ! manufactured goods (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  conveyance_loss(global_land_pts),                                            &
    ! Water that is lost during conveyance (kg).
  return_flow_gw(global_land_pts),                                             &
    ! Water that is returned to renewable groundwater after use (kg).
  return_flow_sw(global_land_pts),                                             &
    ! Water that is returned to rivers after use (kg).
  supply_irrig(global_land_pts)
    ! Water supplied for irrigation (kg).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'WATER_RESOURCES_DRIVE'

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, l
    ! Loop counters.

!------------------------------------------------------------------------------
! Local array variables
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  abstracted(global_land_pts,nwater_use),                                      &
    ! Water abstracted for each use (kg).
  conveyance_loss_use(global_land_pts,nwater_use),                             &
    ! Water that is lost during conveyance, for each water use (kg).
  demand_sw(global_land_pts,nwater_use),                                       &
    ! Demand for water from surface water, for each water use (kg).
  demand_gw(global_land_pts,nwater_use)
    ! Demand for water from groundwater, for each water use (kg).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Calculate the target for the fraction of demand to be met from surface
! water, if required.
!------------------------------------------------------------------------------
IF ( partition_method == partition_calc_from_stores ) THEN
  CALL calc_target_split( global_land_pts, gw_avail, sw_avail,                 &
                          sfc_water_frac )
END IF

!------------------------------------------------------------------------------
! Split the water demand by source (surface water, groundwater).
! Note that if we only have one of these (surface or groundwater) we are
! essentially splitting the demand (into 0 and 100%) and setting the unmet
! demand to equal the one (100%) component.
!------------------------------------------------------------------------------
CALL split_demands( global_land_pts, demand_accum, sfc_water_frac, demand_sw,  &
                    demand_gw )

!------------------------------------------------------------------------------
! Initialise the unmet demand to equal the total demand for local abstraction.
! At present the code only supports local abstraction - but in future it is
! expected that water can be demanded from remote surface water, in which case
! demand_sw would be amended here to only include the demand for local
! abstraction.
!------------------------------------------------------------------------------
demand_unmet(:,:) = demand_gw(:,:) + demand_sw(:,:)

!------------------------------------------------------------------------------
! Abstract from local water.
!------------------------------------------------------------------------------
CALL abstract_local( global_land_pts, priority_order, demand_gw,               &
                     demand_unmet, gw_abstracted, gw_avail,                    &
                     gw_nr_abstracted, sw_abstracted, sw_avail )

!------------------------------------------------------------------------------
! In future implicit transfers will abstract from non-local water at this
! point in the code.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Try to meet any remaining demand from local groundwater.
! If we don't have a groundwater model there is nothing to do here.
! Previously we have abstracted from groundwater while respecting the target
! surface:groundwater ratio; now we are simply looking to meet any shortfall
! from groundwater.
!------------------------------------------------------------------------------
IF ( l_have_groundwater ) THEN
  CALL abstract_local_gw( global_land_pts, priority_order, demand_unmet,       &
                          gw_abstracted, gw_avail,                             &
                          gw_nr_abstracted )
END IF

!------------------------------------------------------------------------------
! Calculate the water abstracted to meet each demand.
! We exclude the notional "abstraction" from the river for environmental flow
! requirements.
!------------------------------------------------------------------------------
DO i = 1, nwater_use
  IF ( i == use_environment ) THEN
    abstracted(:,i) = 0.0
  ELSE
    abstracted(:,i) = demand_accum(:,i) - demand_unmet(:,i)
  END IF
END DO

!------------------------------------------------------------------------------
! Calculate conveyance loss.
!------------------------------------------------------------------------------
CALL calc_conveyance_loss( global_land_pts, abstracted, conv_loss_frac,        &
                           conveyance_loss, conveyance_loss_use )

!---------------------------------------------------------------------------
! Calculate return flows.
!------------------------------------------------------------------------------
CALL calc_return_flow( global_land_pts, abstracted, conveyance_loss_use,       &
                       return_flow_gw, return_flow_sw, water_removed )

!------------------------------------------------------------------------------
! Calculate the water supplied for irrigation.
!------------------------------------------------------------------------------
IF ( l_water_irrigation ) THEN
  DO l = 1, global_land_pts
    ! Water supplied is the water abstracted, minus conveyance loss.
    supply_irrig(l) = abstracted(l,use_irrigation)                             &
                      - conveyance_loss_use(l,use_irrigation)
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_drive

!#############################################################################
!#############################################################################

SUBROUTINE calc_target_split( global_land_pts, gw_avail, sw_avail,             &
                              sfc_water_frac )

!------------------------------------------------------------------------------
! Description:
!   Calculate the target for the fraction of demand to be met by surface
!   water.
!------------------------------------------------------------------------------

USE jules_water_resources_mod, ONLY:                                           &
  n_sw_source, sfc_water_factor

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! Number of land points in the full model grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  gw_avail(global_land_pts),                                                   &
    ! Groundwater that is available for abstraction (kg).
    ! This does not include "non-renewable" groundwater.
  sw_avail(global_land_pts,n_sw_source)
    ! Surface water that is available for abstraction (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
   ! Demands for water accumulated over the water resource timestep (kg).
  sfc_water_frac(global_land_pts)
    ! Fraction of demand to be met from surface water.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  l
    ! Loop counter.

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  denom,                                                                       &
    ! Denominator (kg).
  surface_water
    ! Available surface water, multiplied by weighting factor (kg).

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  water_min = 1.0e-10
    ! A minimum amount of water below which calculations are not performed
    ! (kg).

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_TARGET_SPLIT'

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO l = 1, global_land_pts
  surface_water = sfc_water_factor * SUM(sw_avail(l,:))
  denom         = surface_water + gw_avail(l)
  IF ( denom > water_min ) THEN
    sfc_water_frac(l) = surface_water / denom
  ELSE
    ! There is very little water. Arbitrarily target surface water.
    sfc_water_frac(l) = 1.0
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_target_split

!#############################################################################
!#############################################################################

SUBROUTINE split_demands( global_land_pts, demand_accum, sfc_water_frac,       &
                          demand_sw, demand_gw)

!------------------------------------------------------------------------------
! Description:
!   Split the demand into the amounts to be taken from surface and groundwater
!   sources.
!------------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport

USE jules_water_resources_mod, ONLY:                                           &
  l_have_groundwater, l_have_surface_water, l_water_environment,               &
  l_water_transfers, nwater_use, use_environment, use_transfers

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! Number of land points in the full model grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  demand_accum(global_land_pts,nwater_use),                                    &
   ! Demands for water accumulated over the water resource timestep (kg).
  sfc_water_frac(global_land_pts)
    ! Fraction of demand to be met from surface water.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  demand_sw(global_land_pts,nwater_use),                                       &
    ! Demand for water from surface water, for each water use (kg).
  demand_gw(global_land_pts,nwater_use)
    ! Demand for water from groundwater, for each water use (kg).

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, l
  ! Loop counters and indices.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  sw_frac(global_land_pts,nwater_use)
    ! Surface water fraction for each water use.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'SPLIT_DEMANDS'

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Set surface water fraction for each sector.
!------------------------------------------------------------------------------
IF ( l_have_groundwater .AND. l_have_surface_water ) THEN

  DO i = 1, nwater_use
    ! Indicate that environmental and transfer demands can only be met by
    ! surface water.
    IF ( l_water_environment .AND. i == use_environment ) THEN
      sw_frac(:,i) = 1.0
    ELSE IF ( l_water_transfers .AND. i == use_transfers ) THEN
      sw_frac(:,i) = 1.0
    ELSE
      ! Use the provided fraction.
      sw_frac(:,i) = sfc_water_frac(:)
    END IF
  END DO

ELSE IF ( l_have_groundwater ) THEN

  ! We have a groundwater model but no surface water, hence all demands must
  ! be met by groundwater.
  sw_frac(:,:) = 0.0

ELSE IF ( l_have_surface_water ) THEN

  ! We have surface water but no groundwater, hence all demands must be met by
  ! surface waters. Note that this assumes that it is acceptable to use
  ! surface water for all water uses.
  sw_frac(:,:) = 1.0

END IF

!------------------------------------------------------------------------------
! Separate the total demand into surface and groundwater components.
!------------------------------------------------------------------------------
IF ( l_have_groundwater ) THEN
  DO i = 1, nwater_use
    DO l = 1, global_land_pts
      demand_gw(l,i) = demand_accum(l,i) * (1.0 - sw_frac(l,i))
    END DO
  END DO
ELSE
  demand_gw(:,:) = 0.0
END IF

IF ( l_have_surface_water ) THEN
  DO i = 1, nwater_use
    DO l = 1, global_land_pts
      demand_sw(l,i) = demand_accum(l,i) * sw_frac(l,i)
    END DO
  END DO
ELSE
  demand_sw(:,:) = 0.0
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE split_demands

!#############################################################################
!#############################################################################

SUBROUTINE calc_conveyance_loss( global_land_pts, abstracted, conv_loss_frac,  &
                                 conveyance_loss, conveyance_loss_use )

!------------------------------------------------------------------------------
! Description:
!   Calculate conveyance loss (i.e. water lost during transit).
!   This is all assumed to be lost in the gridbox from which the demand
!   originates, which is clearly unrealistic if water has been transferred
!   from a remote location to meet the demand.
!------------------------------------------------------------------------------

USE jules_water_resources_mod, ONLY:                                           &
  nwater_use, use_environment

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! Number of land points in the full model grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  abstracted(global_land_pts,nwater_use),                                      &
    ! Water abstracted to meet each use (kg).
  conv_loss_frac(global_land_pts)
    ! Fraction of water that is lost during conveyance from source to user.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  conveyance_loss(global_land_pts),                                            &
    ! Water that is lost during conveyance (kg).
  conveyance_loss_use(global_land_pts,nwater_use)
    ! Water that is lost during conveyance, for each water use (kg).

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, l
    ! Loop counters and indices.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  loss_frac(global_land_pts)
    ! Fraction of water that is lost during conveyance.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_CONVEYANCE_LOSS'

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise conveyance losses.
conveyance_loss(:)       = 0.0
conveyance_loss_use(:,:) = 0.0

DO i = 1, nwater_use

  !---------------------------------------------------------------------------
  ! Set conveyance loss fraction for this water use, or cycle if conveyance
  ! loss is zero.
  !---------------------------------------------------------------------------
  IF ( i == use_environment ) THEN
    ! No conveyance loss. Nothing more to do for this water use.
    CYCLE
  ELSE
    ! All other uses use the given loss value.
    loss_frac(:) = conv_loss_frac(:)
  END IF

  !---------------------------------------------------------------------------
  ! Calculate conveyance loss for this use and add to total.
  !---------------------------------------------------------------------------
  DO l = 1, global_land_pts
    conveyance_loss_use(l,i) = loss_frac(l) * abstracted(l,i)
    conveyance_loss(l)       = conveyance_loss(l) + conveyance_loss_use(l,i)
  END DO

END DO  !  water uses

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_conveyance_loss

!#############################################################################
!#############################################################################

SUBROUTINE calc_return_flow( global_land_pts, abstracted, conveyance_loss_use, &
                             return_flow_gw, return_flow_sw, water_removed )

!------------------------------------------------------------------------------
! Description:
!   Calculate return flows (i.e. water that is returned after use) and water
!   that is removed from the system during use.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: l_rivers

USE jules_water_resources_mod, ONLY:                                           &
  l_have_groundwater, l_have_surface_water, nwater_use, rf_domestic,           &
  rf_livestock, rf_industry, use_environment, use_domestic, use_industry,      &
  use_irrigation, use_livestock, use_transfers

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! Number of land points in the full model grid.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  abstracted(global_land_pts,nwater_use),                                      &
    ! Water abstracted to meet each use (kg).
  conveyance_loss_use(global_land_pts,nwater_use)
    ! Water that is lost during conveyance, for each water use (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  return_flow_gw(global_land_pts),                                             &
    ! Water that is returned to renewable groundwater after use (kg).
    ! If there is no renewable groundwater, this water is later added to
    ! the runoff flux.
  return_flow_sw(global_land_pts),                                             &
    ! Water that is returned to rivers after use (kg).
  water_removed(global_land_pts)
    ! Water that is removed from the system during use, e.g. incorporated into
    ! manufactured goods (kg).

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, l
  ! Loop counters and indices.

REAL(KIND=real_jlslsm) ::                                                      &
  delivered,                                                                   &
    ! Water delivered to meet a particular demand (kg).
  removed_frac,                                                                &
    ! Fraction of water that is removed from the system during use, e.g.
    ! incorporated into manufactured goods.
  return_flow_frac_gw,                                                         &
    ! Fraction of the water delivered that is then returned to renewable
    ! groundwater.
  return_flow_frac_sw
    ! Fraction of the water delivered that is then returned to rivers.

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_RETURN_FLOW'

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise return flows and water removed.
return_flow_gw(:) = 0.0
return_flow_sw(:) = 0.0
water_removed(:)  = 0.0

DO i = 1, nwater_use

  !---------------------------------------------------------------------------
  ! Set fraction of flow that is returned for this water use. Each use returns
  ! water to either renewable groundwater or river water, but if that sink is
  ! not modelled the other is used. We know that at least one of l_rivers and
  ! l_have_groundwater is always TRUE when water resources are modelled. If
  ! the only groundwater is "non-renewable", any groundwater return is instead
  ! later added to runoff (not groundwater).
  !---------------------------------------------------------------------------
  IF ( i == use_domestic ) THEN
    ! Domestic water is returned to rivers, if those are modelled.
    IF ( l_rivers ) THEN
      return_flow_frac_gw  = 0.0
      return_flow_frac_sw = rf_domestic
    ELSE
      return_flow_frac_gw  = rf_domestic
      return_flow_frac_sw = 0.0
    END IF
    removed_frac     = 1.0 - rf_domestic
  ELSE IF ( i == use_industry ) THEN
    ! Industrial water is returned to rivers, if those are modelled.
    IF ( l_rivers ) THEN
      return_flow_frac_gw  = 0.0
      return_flow_frac_sw = rf_industry
    ELSE
      return_flow_frac_gw  = rf_industry
      return_flow_frac_sw = 0.0
    END IF
    removed_frac     = 1.0 - rf_industry
  ELSE IF ( i == use_livestock ) THEN
    ! Livestock water is returned to groundwater, if that is modelled.
    IF ( l_have_groundwater ) THEN
      return_flow_frac_gw  = rf_livestock
      return_flow_frac_sw = 0.0
    ELSE
      return_flow_frac_gw  = 0.0
      return_flow_frac_sw = rf_livestock
    END IF
    removed_frac     = 1.0 - rf_livestock
  ELSE
    ! For all other uses no water is returned and none is removed (in that the
    ! water remains in the system and is accounted for). There is nothing more
    ! to do here.
    CYCLE   !  to next use
  END IF

  !---------------------------------------------------------------------------
  ! Calculate return flow and water removed for this use and add to totals.
  !---------------------------------------------------------------------------
  DO l = 1, global_land_pts

    ! Calculate the water delivered to meet this demand in this gridbox.
    ! This is the water abstracted, minus conveyance loss.
    delivered = abstracted(l,i) - conveyance_loss_use(l,i)

    ! Add to totals. Both GW and SW return flows are always calculated, though
    ! either can be zero if those sources are not represented.
    return_flow_gw(l) = return_flow_gw(l) + return_flow_frac_gw * delivered
    return_flow_sw(l) = return_flow_sw(l) + return_flow_frac_sw * delivered
    water_removed(l)  = water_removed(l) + removed_frac * delivered

  END DO

END DO  !  water uses

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_return_flow

END MODULE water_resources_drive_mod
#endif
