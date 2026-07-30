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

MODULE water_resources_control_mod

!------------------------------------------------------------------------------
! Description:
!   Control-level code for water resource management.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  Private scope by default.
PUBLIC water_resources_control

! Module parameters.

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'WATER_RESOURCES_CONTROL_MOD'

!------------------------------------------------------------------------------
! Scalar variables.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! "Global" arrays - to be dimensioned with global_land_pts.
! These hold "global" (full domain) versions of land point variables.
! These are needed at full size only on the master task.
!------------------------------------------------------------------------------
INTEGER, ALLOCATABLE ::                                                        &
  priority_order_global(:,:)
    ! Priorities of water demands at each gridpoint, in order of decreasing
    ! priority. Values are the index in multi-sector arrays. This is a 2-D
    ! array to allow for spatial variation of priorities (not yet supported).

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  conveyance_loss_global(:),                                                   &
    ! Water that is lost during conveyance (kg).
  conv_loss_frac_global(:),                                                    &
    ! Fraction of water that is lost during conveyance from source to user.
  demand_accum_global(:,:),                                                    &
    ! Demands for water accumulated over the water resource timestep (kg).
  demand_unmet_global(:,:),                                                    &
    ! The part of the demand for water that is not satisfied (kg).
  grid_area_global(:),                                                         &
    ! Gridbox area (m2).
  gw_abstracted_global(:),                                                     &
    ! Water abstracted from renewable groundwater (kg).
  gw_avail_global(:),                                                          &
    ! Groundwater that is available for abstraction (kg).
    ! This does not include "non-renewable" groundwater.
  gw_nr_abstracted_global(:),                                                  &
    ! Water abstracted from non-renewable groundwater (kg).
  minor_res_storage_global(:),                                                 &
    ! Water stored in minor reservoirs, on land points (kg).
  return_flow_gw_global(:),                                                    &
    ! Water that is returned to renewable groundwater after use (kg).
  return_flow_river_global(:),                                                 &
    ! Water that is returned to rivers after use (kg).
  river_storage_global(:),                                                     &
    ! Water in rivers, on land points (kg).
  sfc_water_frac_global(:),                                                    &
    ! Fraction of demand to be met from surface water.
  supply_irrig_global(:),                                                      &
    ! Water supplied for irrigation (kg).
  sw_abstracted_global(:,:),                                                   &
    ! Water that is abstracted from surface water sources (kg).
  sw_avail_global(:,:),                                                        &
    ! Surface water that is available for abstraction from each source (kg).
  sw_avail_total_start_global(:),                                              &
    ! Surface water that is available for abstraction at the start of the
    ! timestep, summed over sources (kg).
  water_removed_global(:)
    ! Water that is removed from the system during use, e.g. incorporated into
    ! manufactured goods (kg).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CONTAINS

!##############################################################################

SUBROUTINE water_resources_control( global_land_pts,                           &
             global_land_index, land_index, map_river_to_land_points,          &
             rivers_index_rp, con_rain_ij, con_snow_ij,                        &
             conv_loss_frac, demand_rate_domestic, demand_rate_industry,       &
             demand_rate_livestock, demand_rate_transfers, dvi_cpft,           &
             flandg, frac_irr_soilt, frac_soilt,                               &
             frac_surft, grid_area_ij,                                         &
             ls_rain_ij, ls_snow_ij, lw_down, minor_res_storage,               &
             rfm_surfstore_rp, rivers_sto_rp, smvccl_soilt,                    &
             smvcst_soilt, smvcwt_soilt, sthf_soilt,                           &
             sw_surft, tl_1_ij, tstar_surft,                                   &
             icntmax_gb, plant_n_gb, demand_accum,                             &
             prec_1_day_av_gb, prec_1_day_av_use_gb,                           &
             rn_1_day_av_gb,rn_1_day_av_use_gb, sfc_water_frac, smcl_soilt,    &
             sthu_irr_soilt, sthu_soilt,                                       &
             sthzw_soilt, sub_surf_roff, tl_1_day_av_gb,                       &
             tl_1_day_av_use_gb,                                               &
             priority_order, abstracted_minor_res_global,                      &
             net_abstracted_river_global, gw_abstracted, irrig_water_gb,       &
             abstracted_minor_res, abstracted_river, conveyance_loss,          &
             demand_unmet, gw_avail_start, gw_nr_abstracted, sw_abstracted,    &
             sw_avail_total, water_removed )

!------------------------------------------------------------------------------
! Description:
!   Top-level control routine for water resource management.
!   It contains code that is potentially useful to alternative
!   parameterisations, such as accumulating demands. It then calls a driver
!   routine specific to each parameterisation.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts, nsoilt, nsurft

USE atm_fields_bounds_mod, ONLY: pdims_s

USE calc_avail_water_mod, ONLY:                                                &
  calc_avail_groundwater, calc_avail_surface_water

USE crop_date_mod, ONLY: calc_crop_date

USE crop_vars_mod, ONLY: ndpy, nyav

USE irrigation_water_mod, ONLY: calc_irrigation_demand

USE jules_irrig_mod, ONLY: irr_crop_doell, irr_crop

USE jules_rivers_mod, ONLY: l_minor_reservoirs, np_rivers

USE jules_soil_mod, ONLY: sm_levels

USE jules_surface_types_mod, ONLY: ncpft, ntype

USE jules_water_resources_mod, ONLY:                                           &
  l_have_groundwater, l_have_surface_water, l_water_irrigation,                &
  nstep_water_res, n_sw_source, nwater_use, sw_minor_res_source,               &
  sw_river_source, use_irrigation, water_res_count

USE parallel_mod, ONLY: is_master_task

USE theta_field_sizes, ONLY: row_length=>t_i_length, rows=>t_j_length

#if defined(UM_JULES)
USE timestep_mod, ONLY: timestep_number
#else
USE model_time_mod, ONLY: timestep_number
#endif

USE update_soil_water_mod, ONLY: update_soil_water

USE water_resources_drive_mod, ONLY: water_resources_drive

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! Number of land points (summed over all tasks).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_index(global_land_pts),                                          &
    ! List of indices for the land model grid.
  land_index(land_pts),                                                        &
    ! Index of land points.
  map_river_to_land_points(np_rivers),                                         &
    ! List of coincident land point numbers, on river points.
  rivers_index_rp(np_rivers)
    ! Index of points where routing is calculated.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  con_rain_ij(row_length,rows),                                                &
    ! Convective rainfall rate (kg m-2 s-1).
  con_snow_ij(row_length,rows),                                                &
    ! Convective snowfall rate (kg m-2 s-1).
  conv_loss_frac(land_pts),                                                    &
    ! Fraction of water that is lost during conveyance from source to user.
  demand_rate_domestic(land_pts),                                              &
    ! Demand for water for domestic use (kg s-1).
  demand_rate_industry(land_pts),                                              &
    ! Demand for water for industrial use (kg s-1).
  demand_rate_livestock(land_pts),                                             &
    ! Demand for water for livestock (kg s-1).
  demand_rate_transfers(land_pts),                                             &
    ! Demand for water for (explicit) transfers (kg s-1).
  dvi_cpft(land_pts,ncpft),                                                    &
    ! Development index for crop tiles.
  flandg(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),         &
    ! Land fraction.
  frac_irr_soilt(land_pts,nsoilt),                                             &
    ! Irrigation fraction.
  frac_soilt(land_pts,nsoilt),                                                 &
    !  Fraction of gridbox for each soil tile.
  frac_surft(land_pts,ntype),                                                  &
    ! Fractions of surface types.
  grid_area_ij(row_length,rows),                                               &
    ! Area of each gridbox (m2).
  ls_rain_ij(row_length,rows),                                                 &
    ! Large-scale rainfall rate (kg m-2 s-1).
  ls_snow_ij(row_length,rows),                                                 &
    ! Large-scale snowfall rate (kg m-2 s-1).
  lw_down(row_length,rows),                                                    &
    ! Surface downward longwave radiation (W m-2).
  minor_res_storage(np_rivers),                                                &
    ! Water stored in minor reservoirs (kg).
  rfm_surfstore_rp(np_rivers),                                                 &
    ! River surface storage (m3).
  rivers_sto_rp(np_rivers),                                                    &
    ! River water storage (kg).
  smvccl_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Critical volumetric SMC (cubic m per cubic m of soil).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  smvcwt_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric wilting point (m3/m3 of soil).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  sw_surft(land_pts,nsurft),                                                   &
    ! Surface net shortwave radiation on land tiles (W m-2).
  tl_1_ij(row_length,rows),                                                    &
    ! Ice/liquid water temperature (K).
  tstar_surft(land_pts,nsurft)
    ! Surface temperature (K).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) ::                                                     &
  icntmax_gb(land_pts),                                                        &
    ! Counter for start date for non-rice crops.
  plant_n_gb(land_pts)
    ! Best planting date for non-rice crops.

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  demand_accum(land_pts,nwater_use),                                           &
    ! Demands for water accumulated over the water resource timestep (kg).
    ! On timesteps on which the water resource model is run, the values
    ! output include the conveyance loss (that is, the demand is inflated
    ! to allow for the loss during transport).
  prec_1_day_av_gb(land_pts),                                                  &
    ! Average precipitation rate for the current day (kg m-2 s-1).
  prec_1_day_av_use_gb(land_pts,ndpy,nyav),                                    &
    ! Daily average precipitation rate (kg m-2 s-1).
  rn_1_day_av_gb(land_pts),                                                    &
    ! Average net radiation for the current day (W m-2).
  rn_1_day_av_use_gb(land_pts,ndpy,nyav),                                      &
    ! Daily average net radiation (W m-2).
  sfc_water_frac(land_pts),                                                    &
    ! Fraction of demand to be met from surface water.
    ! This is INTENT(IN) in some configurations, INTENT(OUT) in others.
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg/m2).
  sthu_irr_soilt(land_pts,nsoilt,sm_levels),                                   &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation in irrigated fraction.
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.
  sthzw_soilt(land_pts,nsoilt),                                                &
     ! Soil moisture fraction in deep TOPMODEL layer.
  sub_surf_roff(land_pts),                                                     &
     ! Sub-surface runoff (kg m-2 s-1).
  tl_1_day_av_gb(land_pts),                                                    &
    ! Average air temperature for the current day (K).
  tl_1_day_av_use_gb(land_pts,ndpy,nyav)
    ! Daily average air temperature (K).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  priority_order(land_pts,nwater_use)
    ! Priorities of water demands at each gridpoint, in order of decreasing
    ! priority. Values are the index in multi-sector arrays.

! Coupling variables and diagnostics.
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  !----------------------------------------------------------------------------
  ! Coupling variables
  !----------------------------------------------------------------------------
  abstracted_minor_res_global(global_land_pts),                                &
    ! Water abstracted from minor reservoirs, on global land points (kg).
    ! This is used to couple to minor reservoirs.
    ! Note that this has reduced size if it is not required.
  net_abstracted_river_global(global_land_pts),                                &
    ! Net abstraction from river, on global land points (kg m-2).
    ! Note that this has reduced size if it is not required.
  !----------------------------------------------------------------------------
  ! Variables that are passed out for diagnostic purposes.
  !----------------------------------------------------------------------------
  gw_abstracted(land_pts),                                                     &
    ! Water abstracted from renewable groundwater (kg). This is for coupling
    ! to a groundwater model - though that is currently done internally to
    ! the water resources code.
  irrig_water_gb(land_pts),                                                    &
    ! Water added to soil via irrigation (kg m-2 s-1). This is for coupling
    ! to a soil model - though that is currently done internally to the water
    ! resources code.
  abstracted_minor_res(land_pts),                                              &
    ! Water abstracted from minor reservoirs (kg). Diagnostic only.
  abstracted_river(land_pts),                                                  &
    ! Water abstracted from rivers (kg). Diagnostic only.
  conveyance_loss(land_pts),                                                   &
    ! Water that is lost during conveyance (kg).
  demand_unmet(land_pts,nwater_use),                                           &
    ! The part of the demand for water that is not satisfied (kg).
  gw_avail_start(land_pts),                                                    &
    ! Groundwater that is available for abstraction at start of timestep (kg).
  gw_nr_abstracted(land_pts),                                                  &
    ! Water abstracted from non-renewable groundwater (kg).
  sw_abstracted(land_pts,n_sw_source),                                         &
    ! Water that is abstracted from surface waters (kg).
  sw_avail_total(land_pts),                                                    &
    ! Surface water that is available for abstraction at start of timestep,    &
    ! summed over sources (kg).
  water_removed(land_pts)
    ! Water that is removed from the system during use, e.g. incorporated into
    ! manufactured goods (kg).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'WATER_RESOURCES_CONTROL'

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, l
    ! Loop counters and indices.

LOGICAL ::                                                                     &
  l_allocate,                                                                  &
    ! Flag indicating if arrays are to be allocated (T) or deallocated (F).
  l_water_res_call
    ! TRUE on timesteps when the water resource model is to be run,
    ! FALSE otherwise.

!------------------------------------------------------------------------------
! Local array variables
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  demand_irrig_layer(land_pts,nsoilt,sm_levels),                               &
    ! Demand for irrigation water for each soil layer (kg m-2).
    ! This has sm_levels layers (though we only need nlayer_irrig) for ease of
    ! ensuring compatability with the code used with subroutine
    ! irrigation_demand when l_water_resources=F.
  demand_irrig_soilt(land_pts,nsoilt),                                         &
    ! Demand for water for irrigation for each soil tile (kg m-2).
  grid_area_lp(land_pts),                                                      &
    ! Area of gridbox (m2).
  gw_avail_start_soilt(land_pts,nsoilt),                                       &
    ! Groundwater that is available for abstraction at start of timestep from
    ! each soil tile (kg).
  land_area(land_pts),                                                         &
    ! Area of land in gridbox (m2).
  return_flow_gw(land_pts),                                                    &
    ! Water that is returned to renewable groundwater after use (kg).
  return_flow_river(land_pts),                                                 &
    ! Water that is returned to rivers after use (kg).
  supply_irrig(land_pts)
    ! Water supplied for irrigation (kg).

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! On the first call to this routine, initialise further variables.
!------------------------------------------------------------------------------
IF ( timestep_number == 1 ) THEN
  CALL initialise_water_resources( priority_order )
END IF

!------------------------------------------------------------------------------
! Work out if the water resource model is to be called this timestep.
!------------------------------------------------------------------------------
! Increment timestep counter.
water_res_count = water_res_count + 1

IF ( water_res_count == nstep_water_res ) THEN
  l_water_res_call = .TRUE.
ELSE
  l_water_res_call = .FALSE.
END IF

!------------------------------------------------------------------------------
! Initialise coupling fluxes and abstractions.
! Any flux that is passed out (up) from this subroutine should be initialised
! here because on timesteps on which the water resource model is not called
! they are potentially still used for coupling to other components and/or as
! diagnostics. As these fluxes are currently passed as increments (e.g. kg)
! rather than as rates (e.g. kg s-1) they need to be zero on these
! intermediate timesteps.
!------------------------------------------------------------------------------
! Initialise coupling fluxes.
! These "global" arrays are only allocated at full size on the master task
! (see water_resources_alloc), so they must only be accessed there.
IF ( is_master_task() ) THEN
  IF ( l_minor_reservoirs ) THEN
    abstracted_minor_res_global(:) = 0.0
  END IF
  IF ( sw_river_source > 0 ) THEN
    net_abstracted_river_global(:) = 0.0
  END IF
END IF
! Initialise abstractions.
gw_abstracted(:)    = 0.0
gw_nr_abstracted(:) = 0.0
sw_abstracted(:,:)  = 0.0
! Initialise other fluxes.
conveyance_loss(:)  = 0.0
demand_unmet(:,:)   = 0.0
water_removed(:)    = 0.0
! Initialise diagnostics.
IF ( l_minor_reservoirs ) THEN
  abstracted_minor_res(:) = 0.0
END IF
IF ( sw_river_source > 0 ) THEN
  abstracted_river(:) = 0.0
END IF

!------------------------------------------------------------------------------
! Add to the accumulated demands (only for prescribed demands).
! Initialise to zero at the start of the water resource timestep.
!------------------------------------------------------------------------------
IF ( water_res_count == 1 ) THEN
  demand_accum(:,:) = 0.0
END IF

CALL accumulate_demand( demand_rate_domestic, demand_rate_industry,            &
                        demand_rate_livestock, demand_rate_transfers,          &
                        demand_accum)

IF ( l_water_res_call ) THEN

  !----------------------------------------------------------------------------
  ! Water resources are calculated on this timestep.
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  ! Get the area and area of land in each land gridbox.
  !----------------------------------------------------------------------------
  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    grid_area_lp(l) = grid_area_ij(i,j)
    land_area(l)    = grid_area_ij(i,j) * flandg(i,j)
  END DO

  !----------------------------------------------------------------------------
  ! Calculate the demand for water for irrigation.
  !----------------------------------------------------------------------------
  IF ( l_water_irrigation ) THEN
    CALL calc_irrigation_demand( plant_n_gb, dvi_cpft, frac_irr_soilt,         &
         frac_soilt, land_area, smcl_soilt, smvccl_soilt, smvcst_soilt,        &
         sthf_soilt, sthu_irr_soilt, sthu_soilt,                               &
         demand_accum(:,use_irrigation), demand_irrig_layer,                   &
         demand_irrig_soilt )
  END IF

  !----------------------------------------------------------------------------
  ! Increase demands to account for conveyance loss.
  !----------------------------------------------------------------------------
  CALL add_conveyance_loss( conv_loss_frac, demand_accum )

  !----------------------------------------------------------------------------
  ! Calculate the renewable groundwater available for abstraction.
  !----------------------------------------------------------------------------
  ! If we only have non-renewable groundwater we still call this routine so
  ! that values are set to zero.
  IF ( l_have_groundwater ) THEN
    CALL calc_avail_groundwater( frac_soilt, land_area, smvcst_soilt,          &
                                 smvcwt_soilt, sthzw_soilt,                    &
                                 gw_avail_start, gw_avail_start_soilt )
  END IF

  !----------------------------------------------------------------------------
  ! Depending on the configuration the water resource code could be run
  ! independently on multiple processors, or it requires a single processor.
  ! For simplicity we always use a single processor.
  !----------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  ! Allocate global arrays.
  !----------------------------------------------------------------------------
  l_allocate = .TRUE.
  CALL allocate_global_water( l_allocate )

  !----------------------------------------------------------------------------
  ! Gather information from all processors.
  ! Fields on land_pts need to be gathered into global equivalents.
  !----------------------------------------------------------------------------
  CALL gather_global_water( priority_order, conv_loss_frac, demand_accum,      &
                            grid_area_lp, gw_avail_start, sfc_water_frac )

  IF ( is_master_task() ) THEN

    !----------------------------------------------------------------------------
    ! Calculate the surface water available for abstraction.
    ! As this can involve the river grid, we only do this on the master task.
    !----------------------------------------------------------------------------
    IF ( l_have_surface_water ) THEN

      !------------------------------------------------------------------------
      ! Regrid river variables onto land points.
      ! Global arrays should have been allocated before this routine is called.
      !------------------------------------------------------------------------
      CALL regrid_to_land( global_land_index, map_river_to_land_points,        &
                           rivers_index_rp, minor_res_storage,                 &
                           rfm_surfstore_rp, rivers_sto_rp,                    &
                           minor_res_storage_global, river_storage_global )

      ! Calculate water available for abstraction.
      CALL calc_avail_surface_water( minor_res_storage_global,                 &
                                     river_storage_global,                     &
                                     sw_avail_global )

      ! Save the total at start of timestep for diagnostic purposes.
      sw_avail_total_start_global(:) = SUM(sw_avail_global,2)
    END IF

    !--------------------------------------------------------------------------
    ! Call the top-level routine for the chosen model.
    ! Initially only one model is available.
    !--------------------------------------------------------------------------
    CALL water_resources_drive( global_land_pts, priority_order_global,        &
           conv_loss_frac_global, demand_accum_global,                         &
           demand_unmet_global, gw_abstracted_global, gw_avail_global,         &
           gw_nr_abstracted_global, sfc_water_frac_global,                     &
           sw_abstracted_global, sw_avail_global, water_removed_global,        &
           conveyance_loss_global, return_flow_gw_global,                      &
           return_flow_river_global, supply_irrig_global )

    ! If minor reservoirs are modelled, save abstraction in a new variable.
    IF ( sw_minor_res_source > 0 ) THEN
      abstracted_minor_res_global(:)                                           &
        = sw_abstracted_global(:,sw_minor_res_source)
    END IF

    !--------------------------------------------------------------------------
    ! Decide where return flows should go.
    ! This is essentially part of coupling to other parts of the model but the
    ! current code structure requires this bit to be done now (before net
    ! abstraction from rivers is calculated).
    !--------------------------------------------------------------------------
    CALL redirect_return_flows( global_land_pts, return_flow_gw_global,        &
                                return_flow_river_global )

    !--------------------------------------------------------------------------
    ! Calculate the net abstraction from rivers.
    !--------------------------------------------------------------------------
    IF ( sw_river_source > 0 ) THEN
      CALL calc_river_flux( global_land_pts, grid_area_global,                 &
                            return_flow_river_global,                          &
                            sw_abstracted_global(:,sw_river_source),           &
                            net_abstracted_river_global )
    END IF

  END IF  !  is_master_task

  !----------------------------------------------------------------------------
  ! Scatter information across tasks. Groundwater fluxes are scattered so that
  ! each task can update groundwater stores. Diagnostics are also scattered.
  ! There are no prognostic variables to be scattered.
  !----------------------------------------------------------------------------
  CALL scatter_global_water( conveyance_loss, demand_unmet, gw_abstracted,     &
                             abstracted_minor_res, abstracted_river,           &
                             gw_nr_abstracted, return_flow_gw,                 &
                             return_flow_river, sfc_water_frac, supply_irrig,  &
                             sw_abstracted, sw_avail_total, water_removed )

  !----------------------------------------------------------------------------
  ! Deallocate global arrays.
  !----------------------------------------------------------------------------
  l_allocate = .FALSE.
  CALL allocate_global_water( l_allocate )

  !----------------------------------------------------------------------------
  ! Update soil and groundwater stores.
  !----------------------------------------------------------------------------
  CALL update_soil_water( conveyance_loss, demand_accum(:,use_irrigation),     &
                          demand_irrig_soilt, demand_irrig_layer,              &
                          frac_irr_soilt, frac_soilt, gw_abstracted,           &
                          gw_avail_start_soilt, gw_avail_start, land_area,     &
                          return_flow_gw, smvcst_soilt, sthf_soilt,            &
                          supply_irrig, smcl_soilt, sthu_irr_soilt,            &
                          sthu_soilt, sthzw_soilt, sub_surf_roff,              &
                          irrig_water_gb )

  ! Reset timestep counter.
  water_res_count = 0

END IF  !  l_water_res_call

!------------------------------------------------------------------------------
! Update crop/irrigation-related information.
! This is called every timestep.
!------------------------------------------------------------------------------
IF ( l_water_irrigation .AND. irr_crop == irr_crop_doell ) THEN
  CALL calc_crop_date(land_index, land_pts, row_length, rows,                  &
                      nsurft, frac_surft,                                      &
                      sw_surft, tstar_surft, lw_down, tl_1_ij,                 &
                      con_rain_ij, ls_rain_ij, con_snow_ij, ls_snow_ij,        &
                      prec_1_day_av_gb, prec_1_day_av_use_gb,                  &
                      rn_1_day_av_gb, rn_1_day_av_use_gb,                      &
                      tl_1_day_av_gb, tl_1_day_av_use_gb,                      &
                      icntmax_gb, plant_n_gb)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_control

!##############################################################################
!##############################################################################

SUBROUTINE initialise_water_resources( priority_order )

!------------------------------------------------------------------------------
! Description:
!   Initialise further aspects of the water resource management code.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts

USE ereport_mod, ONLY: ereport

USE jules_water_resources_mod, ONLY:                                           &
  l_prioritise, name_domestic, name_environment,                               &
  name_industry, name_irrigation, name_livestock, name_transfers,              &
  nwater_use, priority, use_domestic, use_environment,                         &
  use_industry, use_irrigation, use_livestock, use_transfers

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  priority_order(land_pts,nwater_use)
    ! Water demands at each gridpoint, in order of decreasing priority.
    ! Values are the index in multi-sector arrays.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INITIALISE_WATER_RESOURCES'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  error_status,                                                                &
    ! Error status.
  i
    ! Loop counter.

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_prioritise ) THEN
  ! Set sector priorities at each location.
  ! At present these are the same at all locations and it is simply a case
  ! of setting values based on the priority variable.
  ! In future this information might come from an ancillary file.
  ! The ancillary could list the sector names and use grids of numerical
  ! values [indicating the index in the name variable]. The names can be
  ! checked against those known to this code, to ensure the ancil uses a
  ! scheme that is consistent with this code.
  DO i = 1,nwater_use
    SELECT CASE ( priority(i) )
    CASE ( name_domestic )
      priority_order(:,i) = use_domestic
    CASE ( name_environment )
      priority_order(:,i) = use_environment
    CASE ( name_industry )
      priority_order(:,i) = use_industry
    CASE ( name_irrigation )
      priority_order(:,i) = use_irrigation
    CASE ( name_livestock )
      priority_order(:,i) = use_livestock
    CASE ( name_transfers )
      priority_order(:,i) = use_transfers
    CASE DEFAULT
      ! Set error status to show a fatal error.
      error_status = 101
      CALL ereport ( RoutineName, error_status,                                &
                     "Priority name not valid: " // TRIM(priority(i)) )
    END SELECT
  END DO

END IF  !  l_prioritise

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE initialise_water_resources

!##############################################################################
!##############################################################################

SUBROUTINE accumulate_demand( demand_rate_domestic,                            &
             demand_rate_industry, demand_rate_livestock,                      &
             demand_rate_transfers, demand_accum )

!------------------------------------------------------------------------------
! Description:
!   Add to the accumulated demand in each sector over the water resource
!   timestep, converting from kg s-1 to kg. We only do this for demands that
!   are prescribed rather than calculated within the model.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts

USE jules_water_resources_mod, ONLY:                                           &
  l_water_domestic, l_water_industry, l_water_livestock, l_water_transfers,    &
  nwater_use, use_domestic, use_industry, use_livestock, use_transfers

USE timestep_mod, ONLY: timestep_len=>timestep

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  demand_rate_domestic(land_pts),                                              &
    ! Demand for water for domestic use (kg s-1).
  demand_rate_industry(land_pts),                                              &
    ! Demand for water for industrial use (kg s-1).
  demand_rate_livestock(land_pts),                                             &
    ! Demand for water for livestock (kg s-1).
  demand_rate_transfers(land_pts)
    ! Demand for water for (explicit) transfers (kg s-1).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  demand_accum(land_pts,nwater_use)
    ! Demands for water accumulated over the water resource timestep (kg).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ACCUMULATE_DEMAND'

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_water_domestic ) THEN
  demand_accum(:,use_domestic) = demand_accum(:,use_domestic)                  &
                                 + demand_rate_domestic(:) * timestep_len
END IF

IF ( l_water_industry ) THEN
  demand_accum(:,use_industry) = demand_accum(:,use_industry)                  &
                                 + demand_rate_industry(:) * timestep_len
END IF

IF ( l_water_livestock ) THEN
  demand_accum(:,use_livestock) = demand_accum(:,use_livestock)                &
                                  + demand_rate_livestock(:) * timestep_len
END IF

IF ( l_water_transfers ) THEN
  demand_accum(:,use_transfers) = demand_accum(:,use_transfers)                &
                                  + demand_rate_transfers(:) * timestep_len
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE accumulate_demand

!##############################################################################
!##############################################################################

SUBROUTINE add_conveyance_loss( conv_loss_frac, demand_accum )

!------------------------------------------------------------------------------
! Description:
!   Increase the demand in each sector to account for conveyance loss.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts

USE jules_water_resources_mod, ONLY:                                           &
  nwater_use, use_environment, use_transfers

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  conv_loss_frac(land_pts)
    ! Fraction of water that is lost during conveyance from source to user.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  demand_accum(land_pts,nwater_use)
    ! Demand for water (kg).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ADD_CONVEYANCE_LOSS'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: i
  !  Loop counter.

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Include conveyance loss for all relevant sectors.
DO i = 1, nwater_use
  ! There is no conveyance loss for environmental use.
  IF ( i == use_environment ) THEN
    CYCLE
  END IF
  ! Include an allowance for conveyance loss for all other sectors.
  demand_accum(:,i) = demand_accum(:,i) * ( 1.0 + conv_loss_frac(:) )
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE add_conveyance_loss

!##############################################################################
!##############################################################################

SUBROUTINE allocate_global_water( l_allocate )

!------------------------------------------------------------------------------
! Description:
!   Allocate or deallocate fields on global land points for water resources
!   code.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts  ! for the current task

USE ereport_mod, ONLY: ereport

USE jules_rivers_mod, ONLY: l_minor_reservoirs, l_rivers

USE jules_water_resources_mod, ONLY: l_have_groundwater, l_have_surface_water, &
  l_prioritise, l_water_irrigation, n_sw_source, nwater_use

USE model_grid_mod, ONLY: global_land_pts

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(in)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN) ::                                                         &
  l_allocate
    ! TRUE indicates this call will allocate arrays.
    ! FALSE indicates this call wil ldeallocate arrays.

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ALLOCATE_GLOBAL_WATER'

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR,                                                                       &
    ! Error flag.
  errorstatus,                                                                 &
    ! Error code.
  error_sum,                                                                   &
    ! Accumulated error flag.
  land_size,                                                                   &
    ! Size for arrays.
  land_size_irrig,                                                             &
    ! Size for irrigation arrays.
  land_size_gw,                                                                &
    ! Size for groundwater arrays.
  land_size_minor_res,                                                         &
    ! Size for minor reservoir arrays.
  land_size_rivers,                                                            &
    ! Size for river arrays.
  land_size_sw
    ! Size for surface water arrays.

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_allocate ) THEN

  !----------------------------------------------------------------------------
  ! Allocate arrays.
  ! These are allocated at full size only on the master task, and only if a
  ! parameterisation is selected - otherwise allocate minimum size.
  !----------------------------------------------------------------------------
  IF ( is_master_task() ) THEN
    land_size = global_land_pts
    IF ( l_have_groundwater ) THEN
      land_size_gw = global_land_pts
    ELSE
      land_size_gw = 1
    END IF
    IF ( l_have_surface_water ) THEN
      land_size_sw = global_land_pts
    ELSE
      land_size_sw = 1
    END IF
    IF ( l_rivers ) THEN
      land_size_rivers = global_land_pts
    ELSE
      land_size_rivers = 1
    END IF
    IF ( l_minor_reservoirs ) THEN
      land_size_minor_res = global_land_pts
    ELSE
      land_size_minor_res = 1
    END IF
    IF ( l_water_irrigation ) THEN
      land_size_irrig = global_land_pts
    ELSE
      land_size_irrig = 1
    END IF
  ELSE
    ! Allocate minimum size on all other tasks.
    land_size       = 1
    land_size_irrig = 1
    land_size_gw    = 1
    land_size_minor_res = 1
    land_size_rivers = 1
    land_size_sw    = 1
  END IF

  error_sum = 0
  ALLOCATE(conveyance_loss_global(land_size), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(conv_loss_frac_global(land_size), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(demand_accum_global(land_size,nwater_use), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(demand_unmet_global(land_size,nwater_use), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(gw_abstracted_global(land_size_gw), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(gw_avail_global(land_size_gw), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(gw_nr_abstracted_global(land_size_gw), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(minor_res_storage_global(land_size_minor_res), STAT = ERROR)
  error_sum = error_sum + ERROR

  IF ( l_prioritise ) THEN
    ALLOCATE(priority_order_global(land_size,nwater_use), STAT = ERROR)
  ELSE
    ALLOCATE(priority_order_global(1,1), STAT = ERROR)
  END IF
  error_sum = error_sum + ERROR

  ALLOCATE(river_storage_global(land_size_rivers), STAT = ERROR)
  error_sum = error_sum + ERROR

  ! Return flows to groundater and rivers are both needed at full size, even
  ! if either source is not active.
  ALLOCATE(return_flow_gw_global(land_size), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(return_flow_river_global(land_size), STAT = ERROR)
  error_sum = error_sum + ERROR

  IF ( l_have_groundwater .AND. l_have_surface_water ) THEN
    ALLOCATE(sfc_water_frac_global(land_size), STAT = ERROR)
  ELSE
    ALLOCATE(sfc_water_frac_global(1), STAT = ERROR)
  END IF
  error_sum = error_sum + ERROR

  ALLOCATE(supply_irrig_global(land_size_irrig), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(sw_abstracted_global(land_size_sw,n_sw_source), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(sw_avail_global(land_size_sw,n_sw_source), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(sw_avail_total_start_global(land_size_sw), STAT = ERROR)
  error_sum = error_sum + ERROR
  ALLOCATE(water_removed_global(land_size), STAT = ERROR)
  error_sum = error_sum + ERROR

  ALLOCATE(grid_area_global(land_size_rivers), STAT = ERROR)
  error_sum = error_sum + ERROR

  IF ( error_sum == 0 ) THEN
    ! Initialise arrays.
    conveyance_loss_global(:)  = 0.0
    conv_loss_frac_global(:)   = 0.0
    demand_accum_global(:,:)   = 0.0
    demand_unmet_global(:,:)   = 0.0
    gw_abstracted_global(:)    = 0.0
    gw_avail_global(:)         = 0.0
    gw_nr_abstracted_global(:) = 0.0
    minor_res_storage_global(:) = 0.0
    priority_order_global(:,:) = 0
    return_flow_gw_global(:)   = 0.0
    return_flow_river_global(:) = 0.0
    river_storage_global(:)    = 0.0
    sfc_water_frac_global(:)   = 0.0
    supply_irrig_global(:)     = 0.0
    sw_abstracted_global(:,:)  = 0.0
    sw_avail_global(:,:)       = 0.0
    sw_avail_total_start_global(:) = 0.0
    water_removed_global(:)    = 0.0
    grid_area_global(:)        = 0.0
  ELSE
    errorstatus = 10
    CALL ereport( RoutineName, errorstatus,                                    &
                  "Error related to allocation of variables." )
  END IF

ELSE

  !----------------------------------------------------------------------------
  ! l_allocate = .FALSE.
  ! Deallocate arrays, in opposite order to the allocation.
  !----------------------------------------------------------------------------
  IF ( ALLOCATED(grid_area_global) )  DEALLOCATE(grid_area_global)
  IF ( ALLOCATED(water_removed_global) )  DEALLOCATE(water_removed_global)
  IF ( ALLOCATED(sw_avail_total_start_global) ) THEN
    DEALLOCATE(sw_avail_total_start_global)
  END IF
  IF ( ALLOCATED(sw_avail_global) )       DEALLOCATE(sw_avail_global)
  IF ( ALLOCATED(sw_abstracted_global) )  DEALLOCATE(sw_abstracted_global)
  IF ( ALLOCATED(supply_irrig_global) )   DEALLOCATE(supply_irrig_global)
  IF ( ALLOCATED(sfc_water_frac_global) ) DEALLOCATE(sfc_water_frac_global)
  IF ( ALLOCATED(river_storage_global) )  DEALLOCATE(river_storage_global)
  IF ( ALLOCATED(return_flow_gw_global) ) DEALLOCATE(return_flow_gw_global)
  IF ( ALLOCATED(return_flow_river_global) ) THEN
    DEALLOCATE(return_flow_river_global)
  END IF
  IF ( ALLOCATED(priority_order_global) ) DEALLOCATE(priority_order_global)
  IF ( ALLOCATED(minor_res_storage_global) ) THEN
    DEALLOCATE(minor_res_storage_global)
  END IF
  IF ( ALLOCATED(gw_nr_abstracted_global)) DEALLOCATE(gw_nr_abstracted_global)
  IF ( ALLOCATED(gw_avail_global) )       DEALLOCATE(gw_avail_global)
  IF ( ALLOCATED(gw_abstracted_global) )  DEALLOCATE(gw_abstracted_global)
  IF ( ALLOCATED(demand_unmet_global) )   DEALLOCATE(demand_unmet_global)
  IF ( ALLOCATED(demand_accum_global) )   DEALLOCATE(demand_accum_global)
  IF ( ALLOCATED(conv_loss_frac_global) ) DEALLOCATE(conv_loss_frac_global)
  IF ( ALLOCATED(conveyance_loss_global) ) DEALLOCATE(conveyance_loss_global)

END IF  !  l_allocate

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE allocate_global_water

!##############################################################################
!##############################################################################

SUBROUTINE gather_global_water( priority_order, conv_loss_frac, demand_accum,  &
                                grid_area_lp, gw_avail_start, sfc_water_frac )

!------------------------------------------------------------------------------
! Description:
!   Gather fields onto global land points for water resources code.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts  ! for the current task

USE jules_water_resources_mod, ONLY: l_have_groundwater, l_prioritise,         &
      nwater_use, partition_ancil, partition_method, sw_river_source

USE model_grid_mod, ONLY: global_land_pts

USE parallel_mod, ONLY: gather_land_field

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  priority_order(land_pts,nwater_use)
    ! Priorities of water demands at each gridpoint, in order of decreasing
    ! priority. Values are the index in multi-sector arrays.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  conv_loss_frac(land_pts),                                                    &
    ! Fraction of water that is lost during conveyance from source to user.
  demand_accum(land_pts,nwater_use),                                           &
    ! Demands for water accumulated over the water resource timestep (kg).
  grid_area_lp(land_pts),                                                      &
    ! Area of gridbox (m2).
  gw_avail_start(land_pts),                                                    &
    ! Groundwater that is available for abstraction at start of timestep (kg).
  sfc_water_frac(land_pts)
    ! Fraction of demand to be met from surface water.

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'GATHER_GLOBAL_WATER'

!------------------------------------------------------------------------------
! Local variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i ! Loop counter.

REAL(KIND=real_jlslsm) ::                                                      &
  rland(land_pts),                                                             &
    ! Workspace for a field on land_pts.
  rland_global(global_land_pts)
    ! Workspace for a field on global_land_pts.

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gather_land_field( conv_loss_frac, conv_loss_frac_global )

DO i = 1, nwater_use
  CALL gather_land_field( demand_accum(:,i), demand_accum_global(:,i) )
END DO

IF ( l_have_groundwater ) THEN
  CALL gather_land_field( gw_avail_start, gw_avail_global )
END IF

IF ( l_prioritise ) THEN
  DO i = 1, nwater_use
    ! gather_land_field requires REAL arguments, so provide those.
    rland(:) = REAL( priority_order(:,i) )
    CALL gather_land_field( rland, rland_global )
    priority_order_global(:,i) = INT( rland_global(:) )
  END DO
END IF

! We only need to gather sfc_water_frac if that was read from a file.
IF ( partition_method == partition_ancil ) THEN
  CALL gather_land_field( sfc_water_frac, sfc_water_frac_global )
END IF

! We only need to gather grid_area_lp if there is abstraction from rivers.
IF ( sw_river_source > 0 ) THEN
  CALL gather_land_field( grid_area_lp, grid_area_global )
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE gather_global_water

!##############################################################################
!##############################################################################

SUBROUTINE scatter_global_water( conveyance_loss, demand_unmet, gw_abstracted, &
                                 abstracted_minor_res, abstracted_river,       &
                                 gw_nr_abstracted, return_flow_gw,             &
                                 return_flow_river, sfc_water_frac,            &
                                 supply_irrig, sw_abstracted, sw_avail_total,  &
                                 water_removed )

!------------------------------------------------------------------------------
! Description:
!   Scatter global land fields across tasks.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts  ! for the current task

USE jules_rivers_mod, ONLY: l_minor_reservoirs, l_rivers

USE jules_water_resources_mod, ONLY: l_have_groundwater, l_have_surface_water, &
      l_water_irrigation, n_sw_source, nwater_use,                             &
      partition_calc_from_stores, partition_method, sw_minor_res_source,       &
      sw_river_source

USE model_grid_mod, ONLY: global_land_pts

USE parallel_mod, ONLY: scatter_land_field

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  conveyance_loss(land_pts),                                                   &
    ! Water that is lost during conveyance (kg).
  demand_unmet(land_pts,nwater_use),                                           &
    ! The part of the demand for water that is not satisfied (kg).
  gw_abstracted(land_pts),                                                     &
    ! Water abstracted from renewable groundwater (kg).
  gw_nr_abstracted(land_pts),                                                  &
    ! Water abstracted from non-renewable groundwater (kg).
  abstracted_minor_res(land_pts),                                              &
    ! Water abstracted from minor reservoirs (kg).
  abstracted_river(land_pts),                                                  &
    ! Water abstracted from rivers (kg).
  return_flow_gw(land_pts),                                                    &
    ! Water that is returned to renewable groundwater after use (kg).
  return_flow_river(land_pts),                                                 &
    ! Water that is returned to rivers after use (kg).
  sfc_water_frac(land_pts),                                                    &
    ! Target fraction of demand to be met by surface water.
  supply_irrig(land_pts),                                                      &
    ! Water supplied for irrigation (kg).
  sw_abstracted(land_pts,n_sw_source),                                         &
    ! Water abstracted from surface waters (kg).
  sw_avail_total(land_pts),                                                    &
    ! Surface water that is available for abstraction at start of timestep,
    ! summed over sources (kg).
  water_removed(land_pts)
    ! Water that is removed from the system during use, e.g. incorporated into
    ! manufactured goods (kg).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'SCATTER_GLOBAL_WATER'

!------------------------------------------------------------------------------
! Local scalar variables
!------------------------------------------------------------------------------
INTEGER :: i  !  loop counter

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Fields that are always required.
CALL scatter_land_field( conveyance_loss_global, conveyance_loss )
CALL scatter_land_field( water_removed_global, water_removed )
DO i = 1, nwater_use
  CALL scatter_land_field( demand_unmet_global(:,i), demand_unmet(:,i) )
END DO

! Fields that are only required if we are modelling groundwater.
IF ( l_have_groundwater ) THEN
  CALL scatter_land_field( gw_abstracted_global, gw_abstracted )
  CALL scatter_land_field( gw_nr_abstracted_global, gw_nr_abstracted )
END IF
! Note that return_flow_gw is always used, even if groundwater is not
! modelled (in that case it is added to runoff).
CALL scatter_land_field( return_flow_gw_global, return_flow_gw )

! Return flow to rivers is always calculated (but can be zero). However
! it is only required further if we have rivers.
IF ( l_rivers ) THEN
  CALL scatter_land_field( return_flow_river_global, return_flow_river )
END IF

! Fields that are only required if we are modelling surface water sources.
IF ( l_have_surface_water ) THEN

  ! Surface water fraction only needs to be scattered if it was calculated by
  ! the master task. If it was read as an ancillary field, nothing to do here.
  IF ( partition_method == partition_calc_from_stores ) THEN
    CALL scatter_land_field( sfc_water_frac_global, sfc_water_frac )
  END IF

  DO i = 1, n_sw_source
    CALL scatter_land_field( sw_abstracted_global(:,i), sw_abstracted(:,i) )
  END DO
  CALL scatter_land_field( sw_avail_total_start_global, sw_avail_total )

  ! Save diagnostic of abstraction from minor reservoirs.
  IF ( l_minor_reservoirs ) THEN
    CALL scatter_land_field( sw_abstracted_global(:,sw_minor_res_source),      &
                             abstracted_minor_res )
  END IF

  ! Save diagnostic of abstraction from rivers.
  IF ( sw_river_source > 0 ) THEN
    CALL scatter_land_field( sw_abstracted_global(:,sw_river_source),          &
                             abstracted_river )
  END IF

END IF  !  l_have_surface_water

! Fields for irrigation.
IF ( l_water_irrigation ) THEN
  CALL scatter_land_field( supply_irrig_global, supply_irrig )
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE scatter_global_water

!##############################################################################
!##############################################################################

SUBROUTINE regrid_to_land( global_land_index, map_river_to_land_points,        &
                           rivers_index_rp, minor_res_storage,                 &
                           rfm_surfstore_rp, rivers_sto_rp,                    &
                           minor_res_storage_global, river_storage_global )

!------------------------------------------------------------------------------
! Description:
!   Regrid water resource-related variables from river grid to land grid.
!------------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport

USE jules_rivers_mod, ONLY: i_river_vn, l_minor_reservoirs, l_rivers,          &
                            np_rivers, rivers_rfm, rivers_trip

USE model_grid_mod, ONLY: global_land_pts

USE rivers_regrid_mod, ONLY: rivpts_to_landpts

USE water_constants_mod, ONLY: rho_water


IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN).
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_index(global_land_pts),                                          &
    ! List of indices for the land model grid.
  map_river_to_land_points(np_rivers),                                         &
    ! List of coincident land point numbers, on river points.
  rivers_index_rp(np_rivers)
    ! Index of points where routing is calculated.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  minor_res_storage(np_rivers),                                                &
    ! Water stored in minor reservoirs (kg).
  rfm_surfstore_rp(np_rivers),                                                 &
    ! River surface storage (m3).
  rivers_sto_rp(np_rivers)
    ! River water storage (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  minor_res_storage_global(global_land_pts),                                   &
    ! Water stored in minor reservoirs, on land points (kg).
  river_storage_global(global_land_pts)
    ! Water in rivers, on land points (kg).

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'REGRID_TO_LAND'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  errorstatus
    ! Error value.

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_rivers ) THEN

  !----------------------------------------------------------------------------
  ! Convert river storage to a variable on land points.
  ! Select code for the current river model as variables differ between models.
  ! This is later used to define the river water available for abstraction,
  ! which informs the approach taken below for rivers_rfm.
  !----------------------------------------------------------------------------

  ! Initialise.
  river_storage_global(:) = 0.0

  SELECT CASE ( i_river_vn )

  CASE ( rivers_rfm )
    ! We will only include the surface store in the available water.
    CALL rivpts_to_landpts( global_land_pts, np_rivers,                        &
                            map_river_to_land_points,global_land_index,        &
                            rivers_index_rp, rfm_surfstore_rp,                 &
                            river_storage_global )
    ! Convert units from m3 to kg.
    river_storage_global(:) = river_storage_global(:) * rho_water

  CASE ( rivers_trip )
    CALL rivpts_to_landpts( global_land_pts, np_rivers,                        &
                            map_river_to_land_points, global_land_index,       &
                            rivers_index_rp, rivers_sto_rp,                    &
                            river_storage_global )

  CASE DEFAULT

    errorstatus = 101  !  a fatal error
    CALL ereport(RoutineName, errorstatus, 'Unknown value of i_river_vn.')

  END SELECT  !  i_river_vn

END IF  !  l_rivers

!------------------------------------------------------------------------------
! Get minor reservoir storage onto land points.
!------------------------------------------------------------------------------
IF ( l_minor_reservoirs ) THEN
  ! Initialise.
  minor_res_storage_global(:) = 0.0
  CALL rivpts_to_landpts( global_land_pts, np_rivers,                          &
                          map_river_to_land_points, global_land_index,         &
                          rivers_index_rp, minor_res_storage,                  &
                          minor_res_storage_global )
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE regrid_to_land

!##############################################################################
!##############################################################################

SUBROUTINE redirect_return_flows( global_land_pts,                             &
                                  return_flow_gw_global,                       &
                                  return_flow_river_global )

!------------------------------------------------------------------------------
! Description:
!   Change the destination of return flows depending on what sinks are
!   available.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: l_rivers
USE jules_water_resources_mod, ONLY: l_have_renew_gwater

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! The number of land points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  return_flow_gw_global(global_land_pts),                                      &
    ! Water that is returned to renewable groundwater after use (kg).
  return_flow_river_global(global_land_pts)
    ! Water that is returned to rivers after use (kg).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'REDIRECT_RETURN_FLOWS'

!------------------------------------------------------------------------------
! Local variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  l ! Loop counter.

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Return flows to groundwater and rivers have been calculated assuming those
! sinks are available. If the preferred sink is not modelled the other is used
! (if available). If neither is available we add to the groundwater return flow
! because later code will divert that to runoff.
! Note that if both rivers and renewable groundwater are modelled nothing is
! changed in this subroutine.
!------------------------------------------------------------------------------
IF ( l_rivers .AND. .NOT. l_have_renew_gwater ) THEN

  ! River are modelled but renewable groundwater is not.
  ! Direct all return flows to rivers.
  DO l=1,global_land_pts
    return_flow_river_global(l) = return_flow_river_global(l)                  &
                                  +  return_flow_gw_global(l)
    return_flow_gw_global(l)    = 0.0
  END DO

ELSE IF ( .NOT. l_rivers ) THEN

  ! Rivers are not modelled. Groundwater might or might not be modelled.
  ! Direct all return flows to groundwater (for now).
  DO l=1,global_land_pts
    return_flow_gw_global(l)    = return_flow_gw_global(l)                     &
                                  +  return_flow_river_global(l)
    return_flow_river_global(l) = 0.0
  END DO

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE redirect_return_flows

!##############################################################################
!##############################################################################
SUBROUTINE calc_river_flux( global_land_pts, grid_area_global,                 &
                            return_flow_river_global,                          &
                            sw_abstracted_river_global,                        &
                            net_abstracted_river_global )

!------------------------------------------------------------------------------
! Description:
!   Calculate the net abstraction of water from rivers.
!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts
    ! The number of land points.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  grid_area_global(global_land_pts),                                           &
    ! Area of gridbox (m2).
  return_flow_river_global(global_land_pts),                                   &
    ! Water that is returned to rivers after use (kg).
  sw_abstracted_river_global(global_land_pts)
    ! Water abstracted from river (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  net_abstracted_river_global(global_land_pts)
    ! Net abstraction from river (kg m-2).

!------------------------------------------------------------------------------
! Local parameters
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_RIVER_FLUX'

!------------------------------------------------------------------------------
! Local variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  l ! Loop counter.

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Calculate net abstraction and change units from kg to kg m-2.
DO l=1,global_land_pts
  net_abstracted_river_global(l) = ( sw_abstracted_river_global(l)             &
                                      - return_flow_river_global(l) )          &
                                   / grid_area_global(l)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_river_flux

!##############################################################################

END MODULE water_resources_control_mod
#endif
