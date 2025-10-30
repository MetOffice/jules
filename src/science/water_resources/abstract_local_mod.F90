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

MODULE abstract_local_mod

!------------------------------------------------------------------------------
! Description:
!   Calculate abstraction of water from local water sources.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC abstract_local, abstract_local_gw

!------------------------------------------------------------------------------
! Module parameters.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  water_min = 1.0e-10
    ! A minimum amount of water (either a demand or available water) at or
    ! below which some calculations are not performed (kg). This is introduced
    ! primarily so as to avoid very small values in denominators, but this also
    ! prevents resources being spent calculating tiny fluxes that are
    ! physically insignificant. Ignoring these values can mean that (for
    ! example) a small demand is not met when there is water available, but the
    ! amounts are physically insignificant - e.g. a demand of 1.0e-10 kg (for
    ! the gridbox) which is neglected once per day for 100 years means 4e-6kg
    ! of demand is ignored in total.

CONTAINS

!##############################################################################

SUBROUTINE abstract_local( global_land_pts, priority_order, demand_gw,         &
                           demand_unmet, gw_abstracted, gw_avail,              &
                           gw_nr_abstracted, sw_abstracted, sw_avail )

USE jules_water_resources_mod, ONLY:                                           &
  nwater_use, l_have_groundwater, l_prioritise, l_water_environment,           &
  nr_gwater_model, nr_gwater_last, nr_gwater_use, n_sw_source,                 &
  sw_river_source, use_environment, use_transfers

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
! Abstract water demand from local groundwater and surface water sources
! according to the target ratio.
!------------------------------------------------------------------------------

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
  demand_gw(global_land_pts,nwater_use)
    ! Demand for water from local groundwater, for each water use (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  demand_unmet(global_land_pts,nwater_use),                                    &
    ! Unmet demands for water from local abstractions (kg).
  gw_abstracted(global_land_pts),                                              &
    ! Water abstracted from renewable groundwater (kg).
  gw_avail(global_land_pts),                                                   &
    ! Groundwater that is available for abstraction (kg).
    ! This does not include "non-renewable" groundwater.
  gw_nr_abstracted(global_land_pts),                                           &
    ! Water abstracted from non-renewable groundwater (kg).
  sw_abstracted(global_land_pts,n_sw_source),                                  &
    ! Water abstracted from surface waters (kg).
  sw_avail(global_land_pts,n_sw_source)
    ! Surface water that is available for abstraction (kg).

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  j, k, l, n, n2, s,                                                           &
    ! Loop counters and indices.
  nsource
    ! Number of sources.

REAL(KIND=real_jlslsm) ::                                                      &
  demand_tmp,                                                                  &
    ! A demand (kg).
  env_demand_tmp,                                                              &
    ! Remaining environmental demand (kg).
  gw_abs,                                                                      &
    ! Amount of groundwater extracted (kg).
  gw_avail_ratio,                                                              &
    ! Ratio of available groundwater to demand for groundwater (1).
  tot_gw_demand, tot_sw_demand,                                                &
    ! Total mass of water to be abstracted from groundwater, surface water
    ! respectively (kg).
  sw_abs,                                                                      &
    ! Amount of surface water extracted (kg).
  sw_avail_to_use,                                                             &
    ! Amount of surface water available for a particular use (kg).
  tot_sw_avail
    ! Total mass of available surface water (kg).

LOGICAL ::                                                                     &
  l_abstract
    ! Flag indicating if water is to be abstracted (or simply reserved).

!------------------------------------------------------------------------------
!end of header

DO l = 1, global_land_pts

  IF ( l_have_groundwater ) THEN

    !--------------------------------------------------------------------------
    ! Abstract from groundwater.
    !--------------------------------------------------------------------------

    ! Calculate total amount of water to be abstracted from groundwater by
    ! summing over sectors.
    tot_gw_demand = SUM( demand_gw(l,:) )

    ! If there is demand, try abstracting.
    IF ( tot_gw_demand > 0.0 ) THEN

      IF ( tot_gw_demand <= gw_avail(l) ) THEN

        !----------------------------------------------------------------------
        ! There is sufficient (renewable) groundwater to meet the demand.
        ! Abstraction of non-renewable groundwater is zero.
        !----------------------------------------------------------------------
        gw_abstracted(l)  = tot_gw_demand
        gw_avail(l)       = gw_avail(l) - gw_abstracted(l)
        ! The remaining demand in each sector is the surface water part.
        demand_unmet(l,:) = demand_unmet(l,:) - demand_gw(l,:)

      ELSE IF ( nr_gwater_model == nr_gwater_use ) THEN

        !----------------------------------------------------------------------
        ! There is insufficient renewable groundwater to meet demand but
        ! non-renewable groundwater can be used.
        ! Note that the use of non-renewable groundwater "as a last resort" is
        ! dealt with elsewhere.
        !----------------------------------------------------------------------
        ! Abstract all renewable groundwater and use non-renewable water to
        ! meet the remaining demand.
        gw_abstracted(l)     = gw_avail(l)
        gw_nr_abstracted(l)  = tot_gw_demand - gw_abstracted(l)
        gw_avail(l)          = 0.0
        ! The remaining demand in each sector is the surface water part.
        demand_unmet(l,:) = demand_unmet(l,:) - demand_gw(l,:)

      ELSE IF ( gw_avail(l) > 0.0 ) THEN

        ! There is groundwater available, but insufficient to meet demand.

        IF ( l_prioritise ) THEN
          !--------------------------------------------------------------------
          ! There is insufficient groundwater to meet total demand,
          ! non-renewable groundwater is not available, and prioritisation is
          ! used. Abstract the available water in priority order.
          !--------------------------------------------------------------------
          DO j = 1, nwater_use
            ! Get the index for the next highest priority sector.
            k = priority_order(l,j)
            IF ( demand_gw(l,k) < gw_avail(l) ) THEN
              ! Demand from this sector can be fully satisfied by renewable
              ! groundwater.
              gw_abs           = demand_gw(l,k)
              gw_abstracted(l) = gw_abstracted(l) + gw_abs
              gw_avail(l)      = gw_avail(l) - gw_abs
              ! The remaining demand in this sector is the surface water part.
              demand_unmet(l,k) = demand_unmet(l,k) - demand_gw(l,k)
            ELSE
              ! Demand from this sector cannot be fully satisfied by renewable
              ! groundwater alone. Extract all available water and don't
              ! consider any further water uses.
              gw_abs            = gw_avail(l)
              gw_abstracted(l)  = gw_abstracted(l) + gw_abs
              demand_unmet(l,k) = demand_unmet(l,k) - gw_abs
              gw_avail(l)       = 0.0
              EXIT
            END IF

          END DO  !  uses

        ELSE

          !--------------------------------------------------------------------
          ! There is insufficient groundwater to meet total demand,
          ! non-renewable groundwater is not available, and prioritisation is
          ! not used. Abstract the available water in proportion to each
          ! demand.
          !--------------------------------------------------------------------
          IF ( tot_gw_demand > water_min ) THEN
            gw_avail_ratio = gw_avail(l) / tot_gw_demand
            DO j = 1, nwater_use
              gw_abs            = demand_gw(l,j) * gw_avail_ratio
              gw_abstracted(l)  = gw_abstracted(l) + gw_abs
              demand_unmet(l,j) = demand_unmet(l,j) - gw_abs
              gw_avail(l)       = gw_avail(l) - gw_abs
            END DO
          END IF

          ! If the total demand <= water_min we didn't use the code above to
          ! extract water, so as to avoid a small value in denominator. This
          ! will leave a small unmet demand but will also leave a small amount
          ! of available water. We could add further code to deal with this but
          ! instead opt for simplicity - and note that the full (but small)
          ! demand could not anyway be met fully by groundwater.

        END IF  !  l_prioritise

      END IF  !  tot_gw_demand v. gw_avail

    END IF  !  tot_gw_demand > 0.0

  END IF  !  l_have_groundwater

  !############################################################################
  !----------------------------------------------------------------------------
  ! Abstraction from surface water.
  !----------------------------------------------------------------------------

  ! Calculate the total remaining demand, to be abstracted from local surface
  ! water. Under some circumstances this is updated below.
  ! Note that the approach used here can mean that either surface or
  ! groundwater can be completely exhausted while there is still water
  ! available in the other source.
  tot_sw_demand = SUM( demand_unmet(l,:) )

  ! Calculate the total available surface water. Under some circumstances this
  ! is updated below.
  tot_sw_avail = SUM( sw_avail(l,:) )

  ! If there is demand and available water, try abstracting.
  IF ( tot_sw_demand > water_min .AND. tot_sw_avail > water_min ) THEN

    IF ( l_prioritise ) THEN

      !------------------------------------------------------------------------
      ! Demands are prioritised.
      !------------------------------------------------------------------------
      DO j = 1, nwater_use

        ! Get the index for the next highest priority sector.
        k = priority_order(l,j)

        ! Set the number of sources that can be used to meet this demand, and
        ! the index of the first source. Also establish if water is abstracted
        ! or merely set aside (for environmental flow).
        IF ( k == use_environment ) THEN
          ! Environmental demand can only be met from the river.
          nsource    = 1
          n          = sw_river_source
          l_abstract = .FALSE.
        ELSE
          ! Consider all sources.
          nsource    = n_sw_source
          n          = 1
          l_abstract = .TRUE.
        END IF
        ! Calculate index of final source.
        n2 = n + nsource - 1

        ! Calculate the total water available for this use. Note that this sum
        ! over a continuous range of sources works because we are considering
        ! either a single source (for environmental use) or all sources.
        sw_avail_to_use = SUM( sw_avail(l,n:n2) )

        ! If there is negligible water available to this use, move to the next
        ! use. (Again this also helps avoid a small value in a denominator.)
        IF ( sw_avail_to_use < water_min ) CYCLE

        ! Loop over sources for this demand.
        ! Abstract in proportion to the available water in each source.

        ! Copy the unmet demand into a new variable, so unmet demand can be
        ! updated inside the loop.
        demand_tmp = demand_unmet(l,k)

        DO s = 1, nsource
          sw_abs = MIN( demand_tmp * sw_avail(l,n) / sw_avail_to_use,          &
                        sw_avail(l,n) )
          sw_avail(l,n)      = sw_avail(l,n) - sw_abs
          demand_unmet(l,k)  = demand_unmet(l,k) - sw_abs
          ! Add to abstraction (uness water is just being reserved).
          IF ( l_abstract ) THEN
            sw_abstracted(l,n) = sw_abstracted(l,n) + sw_abs
          END IF
          ! Set the index for the next source.
          n = n + 1
        END DO  !  sources

      END DO  !  j (water uses)

    ELSE

      !------------------------------------------------------------------------
      ! Demands are not prioritised.
      !------------------------------------------------------------------------

      IF ( l_water_environment ) THEN
        ! In this case (unprioritised demands including an environmental
        ! demand) we need to treat the environmental demand differently.

        IF ( tot_sw_demand <= tot_sw_avail ) THEN
          !--------------------------------------------------------------------
          ! There is enough surface water for all demands.
          !--------------------------------------------------------------------
          IF ( demand_unmet(l,use_environment) < sw_avail(l,sw_river_source) ) &
            THEN
            ! There is enough river water for environmental demand.
            ! Abstract environmental demand from the river.
            sw_abs = demand_unmet(l,use_environment)
          ELSE
            ! There is not enough river water for environmental demand.
            ! Abstract as much as possible from the river.
            sw_abs = sw_avail(l,sw_river_source)
          END IF

        ELSE
          !--------------------------------------------------------------------
          ! There is not enough surface water for the total demand.
          !--------------------------------------------------------------------
          ! Calculate the part of the environmental demand that we will
          ! attempt to meet. (Note that we have previously ensured that
          ! tot_sw_demand is not very small.)
          env_demand_tmp = demand_unmet(l,use_environment)                     &
                           * tot_sw_avail / tot_sw_demand

          IF ( env_demand_tmp < sw_avail(l,sw_river_source) ) THEN
            ! This part of the environmental demand can be met from river.
            sw_abs = env_demand_tmp
          ELSE
            ! Demand can't be met by river, but abstract as much as possible.
            sw_abs = sw_avail(l,sw_river_source)
          END IF

        END IF  !  tot_sw_demand

        sw_avail(l,sw_river_source) = sw_avail(l,sw_river_source) - sw_abs
        ! Note there is no need to update sw_abstracted as this water is
        ! not actually abstracted.
        ! Calculate the remaining environmental demand.
        env_demand_tmp = demand_unmet(l,use_environment) - sw_abs

        ! Set the remaining environmental demand to zero so that it is not
        ! considered with the remaining demands below. This demand is
        ! reintroduced again later in this subroutine.
        demand_unmet(l,use_environment) = 0.0

        ! Recalculate the available water and total demand.
        tot_sw_avail  = SUM( sw_avail(l,:) )
        tot_sw_demand = SUM( demand_unmet(l,:) )

      END IF  !  l_water_environment

      !------------------------------------------------------------------------
      ! Now we consider the remaining demands, having met any environmental
      ! demand (either completely or in part) above.
      ! If environmental demand has been considered:
      ! (i) tot_sw_demand no longer includes an environmental contrubution and
      ! so can be used to calculate an abstraction
      ! (ii) tot_sw_demand and tot_sw_avail have been recalculated and hence
      ! need to be checked again to ensure we avoid small denominators.
      !------------------------------------------------------------------------
      IF ( tot_sw_demand > water_min .AND. tot_sw_avail > water_min ) THEN

        IF ( tot_sw_demand <= tot_sw_avail ) THEN

          !--------------------------------------------------------------------
          ! There is sufficient water to meet all demands.
          ! Abstract from sources in proportion to each source.
          !
          ! Note that we have previously ensured that tot_sw_avail is not very
          ! small.
          !--------------------------------------------------------------------
          DO n = 1, n_sw_source
            sw_abs = tot_sw_demand * sw_avail(l,n) / tot_sw_avail
            sw_avail(l,n)      = sw_avail(l,n) - sw_abs
            sw_abstracted(l,n) = sw_abstracted(l,n) + sw_abs
          END DO

          ! Set the remaining demand in each sector to zero.
          demand_unmet(l,:) = 0.0

        ELSE

          !--------------------------------------------------------------------
          ! tot_sw_demand > tot_sw_avail
          ! There is insufficient water to meet all demands.
          ! Meet a fraction of each demand.
          ! (Note that we have previously ensured that tot_sw_demand is not
          !  very small.)
          !--------------------------------------------------------------------
          demand_unmet(l,:) = demand_unmet(l,:) *                              &
                              (1.0 - tot_sw_avail / tot_sw_demand )
          ! All sources of water have been exhausted.
          DO n = 1, n_sw_source
            sw_abstracted(l,n) = sw_abstracted(l,n) + sw_avail(l,n)
            sw_avail(l,n)      = 0.0
          END DO

        END IF  !  tot_sw_demand v. tot_sw_avail

      END IF  !  tot_sw_demand and tot_sw_avail > water_min

      !------------------------------------------------------------------------
      ! If considering environmental demands, reset the remaining demand
      ! (having previously set it to zero while other abstractions were
      ! calculated).
      !------------------------------------------------------------------------
      IF ( l_water_environment ) THEN
        demand_unmet(l,use_environment) = env_demand_tmp
      END IF

    END IF  !  l_prioritise

  END IF  !  tot_sw_demand and tot_sw_avail > water_min

END DO  !  points

RETURN
END SUBROUTINE abstract_local

!##############################################################################
!##############################################################################

SUBROUTINE abstract_local_gw( global_land_pts, priority_order, demand_unmet,   &
                              gw_abstracted, gw_avail,                         &
                              gw_nr_abstracted )

USE jules_water_resources_mod, ONLY:                                           &
  nwater_use, l_prioritise, l_water_environment, l_water_transfers,            &
  use_environment, use_transfers, nr_gwater_model, nr_gwater_last,             &
  nr_gwater_use

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
! After initial abstractions from groundwater and (local and non-local)
! surface water, abstract any remaining demand from local groundwater (except
! for sectors that cannot use groundwater).
!------------------------------------------------------------------------------

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

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  demand_unmet(global_land_pts,nwater_use),                                    &
    ! Unmet demands for water (kg).
  gw_abstracted(global_land_pts),                                              &
    ! Water abstracted from renewable groundwater (kg).
  gw_avail(global_land_pts),                                                   &
    ! Groundwater that is available for abstraction (kg).
    ! This does not include non-renewable groundwater.
  gw_nr_abstracted(global_land_pts)
    ! Demand that is abstracted from non-renewable groundwater (kg).

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: j, k, l
  ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  gw_abs,                                                                      &
    ! Amount of groundwater extracted (kg).
  gw_avail_ratio,                                                              &
    ! Ratio of available groundwater to demand for groundwater (1).
  tot_gw_demand
    ! Total amount of water to be abstracted from groundwater (kg).

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_use_gw(nwater_use)
    ! Flag to indicate sectors that can use groundwater.
    ! TRUE=can use groundwater, F=cannot use groundwater

!------------------------------------------------------------------------------
!end of header

!------------------------------------------------------------------------------
! Set flags to indicate which water uses can be addressed by groundwater.
!------------------------------------------------------------------------------
l_use_gw(:) = .TRUE.
IF ( l_water_environment ) THEN
  l_use_gw(use_environment) = .FALSE.
END IF
IF ( l_water_transfers ) THEN
  l_use_gw(use_transfers) = .FALSE.
END IF

DO l = 1, global_land_pts

  !----------------------------------------------------------------------------
  ! Calculate the total remaining demand for water from groundwater by summing
  ! the demand over sectors that can use groundwater.
  !----------------------------------------------------------------------------
  tot_gw_demand = 0.0
  DO j = 1, nwater_use
    IF ( l_use_gw(j) ) THEN
      tot_gw_demand = tot_gw_demand + demand_unmet(l,j)
    END IF
  END DO

  ! If there is demand, try abstracting.
  IF ( tot_gw_demand > 0.0 ) THEN

    IF ( tot_gw_demand <= gw_avail(l) ) THEN
      !------------------------------------------------------------------------
      ! There is sufficient (renewable) groundwater to meet demand.
      !------------------------------------------------------------------------
      gw_abstracted(l)  = gw_abstracted(l) + tot_gw_demand
      gw_avail(l)       = gw_avail(l) - tot_gw_demand
      ! Set remaining demand to zero for sectors that can use groundwater.
      DO j = 1, nwater_use
        IF ( l_use_gw(j) ) THEN
          demand_unmet(l,j) = 0.0
        END IF
      END DO

    ELSE IF ( nr_gwater_model == nr_gwater_use .OR.                            &
              nr_gwater_model == nr_gwater_last ) THEN
      !------------------------------------------------------------------------
      ! There is insufficient renewable groundwater to meet demand but
      ! non-renewable groundwater can be used. Meet as much of the demand as
      ! possible from the renewable source, and use non-renewable for the
      ! remainder.
      !------------------------------------------------------------------------
      gw_abstracted(l)    = gw_abstracted(l) + gw_avail(l)
      gw_nr_abstracted(l) = gw_nr_abstracted(l)                                &
                            + ( tot_gw_demand - gw_avail(l) )
      gw_avail(l)         = 0.0
      ! Set remaining demand to zero for sectors that can use groundwater.
      DO j = 1, nwater_use
        IF ( l_use_gw(j) ) THEN
          demand_unmet(l,j) = 0.0
        END IF
      END DO

    ELSE IF ( gw_avail(l) > 0.0 ) THEN
      ! We only need to do more if there is groundwater available.

      IF ( l_prioritise ) THEN
        !----------------------------------------------------------------------
        ! There is insufficient groundwater to meet total demand, non-renewable
        ! groundwater is not available, and prioritisation is used.
        ! Abstract available water in priority order.
        !----------------------------------------------------------------------
        DO j = 1, nwater_use
          k = priority_order(l,j)
          IF ( l_use_gw(k) ) THEN
            IF ( demand_unmet(l,k) < gw_avail(l) ) THEN
              ! Demand from this sector can be fully satisfied by renewable
              ! groundwater.
              gw_abs           = demand_unmet(l,k)
              gw_avail(l)      = gw_avail(l) - gw_abs
              demand_unmet(l,k) = 0.0
            ELSE
              ! Demand from this sector cannot be met by renewable groundwater
              ! alone. Extract all available water.
              gw_abs = gw_avail(l)
              demand_unmet(l,k) = demand_unmet(l,k) - gw_abs
              gw_avail(l)       = 0.0
            END IF
            ! Add to total abstraction.
            gw_abstracted(l) = gw_abstracted(l) + gw_abs
          END IF  !  mask_use
        END DO  !  water use

      ELSE

        !----------------------------------------------------------------------
        ! There is insufficient groundwater to meet total demand, non-renewable
        ! groundwater is not available, and prioritisation is not used.
        ! Abstract the available water in proportion to each demand.
        !----------------------------------------------------------------------
        IF ( tot_gw_demand > water_min ) THEN
          gw_avail_ratio = gw_avail(l) / tot_gw_demand
          DO j = 1, nwater_use
            IF ( l_use_gw(j) ) THEN
              gw_abs            = demand_unmet(l,j) * gw_avail_ratio
              gw_abstracted(l)  = gw_abstracted(l) + gw_abs
              demand_unmet(l,j) = demand_unmet(l,j) - gw_abs
              gw_avail(l)       = gw_avail(l) - gw_abs
            END IF
          END DO
        END IF

        ! If the total demand <= water_min we didn't use the code above to
        ! extract water, so as to avoid a small value in denominator. This will
        ! leave a small amount of water available but also a small unmet
        ! demand. We could add further code to deal with this but instead opt
        ! for simplicity - and note that the full (but small) demand could not
        ! anyway be met fully by groundwater.

      END IF  !  l_prioritise
    END IF  !  tot_gw_demand <v.gw_avail
  END IF  !  tot_gw_demand > 0.0

END DO  !  points

RETURN
END SUBROUTINE abstract_local_gw

!##############################################################################

END MODULE abstract_local_mod

