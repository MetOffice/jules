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

MODULE irrigation_water_mod

!-----------------------------------------------------------------------------
! Description:
!   Irrigation code for use with water resource management.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC add_irrigation_to_soil, calc_irrigation_demand

! Module parameters.
CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'IRRIGATION_WATER_MOD'

CONTAINS

!#############################################################################


SUBROUTINE calc_irrigation_demand( plant_n_gb, dvi_cpft, frac_irr_soilt,       &
             frac_soilt, land_area, smcl_soilt, smvccl_soilt,                  &
             smvcst_soilt,                                                     &
             sthf_soilt, sthu_irr_soilt, sthu_soilt, demand_irrig,             &
             demand_irrig_layer, demand_irrig_soilt )

USE ancil_info, ONLY: land_pts, nsoilt

USE irrigation_mod, ONLY:                                                      &
  ! imported variables
  nlayer_irrig,                                                                &
  ! imported procedures
  irrigation_demand

USE jules_soil_mod, ONLY: sm_levels

USE jules_surface_types_mod, ONLY: ncpft

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  plant_n_gb(land_pts)
    ! Best planting date for non-rice crops.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dvi_cpft(land_pts,ncpft),                                                    &
    ! Development index for crop tiles
  frac_irr_soilt(land_pts,nsoilt),                                             &
    ! Irrigation fraction.
  frac_soilt(land_pts,nsoilt),                                                 &
    !  Fraction of gridbox for each soil tile.
  land_area(land_pts),                                                         &
    ! Area of land in each gridbox (m2).
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg/m2).
  smvccl_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Critical volumetric SMC (cubic m per cubic m of soil).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  sthu_irr_soilt(land_pts,nsoilt,sm_levels),                                   &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation in irrigated fraction
  sthu_soilt(land_pts,nsoilt,sm_levels)
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  demand_irrig(land_pts),                                                      &
    ! Demand for water for irrigation accumulated over the water resource
    ! timestep (kg).
  demand_irrig_layer(land_pts,nsoilt,sm_levels),                               &
    ! Demand for irrigation water for each soil layer (kg m-2).
  demand_irrig_soilt(land_pts,nsoilt)
    ! Demand for water for irrigation for each soil tile (kg m-2).

!-----------------------------------------------------------------------------
! Local scalar variables
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l, m
    ! Loop counters.

!-----------------------------------------------------------------------------
! Local array variables
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  smcl_target(land_pts,nsoilt,nlayer_irrig),                                   &
    ! The target values of soil moisture content of each layer (kg/m2).
  sthu_irr_target(land_pts,nsoilt,nlayer_irrig),                               &
    ! The target value of unfrozen soil moisture content of each layer as a
    ! fraction of saturation in irrigated fraction.
  sthu_target(land_pts,nsoilt,nlayer_irrig)
    ! The target value of unfrozen soil moisture content of each layer as a
    ! fraction of saturation.

LOGICAL ::                                                                     &
  l_irrigate_now(land_pts,nsoilt)
    ! TRUE when a tile can be irrigated on this timestep.
    ! FALSE at other times and tiles.

!-----------------------------------------------------------------------------
!end of header

! Initialise demand to be zero.
demand_irrig_layer(:,:,:) = 0.0
! demand_irrig is passed out to the accumulated demand, but we can initialise
! it here because the demand is calculated once every water resource timestep
! - effectively this is the accumulated demand.
demand_irrig(:) = 0.0

! Calculate water required for irrigation.
! First load soil moisture variables into dummy "target" variables; on return
! these have the soil wetness assuming sufficient water is available for
! irrigation, but are not needed further.
smcl_target(:,:,:)     = smcl_soilt(:,:,1:nlayer_irrig)
sthu_irr_target(:,:,:) = sthu_irr_soilt(:,:,1:nlayer_irrig)
sthu_target(:,:,:)     = sthu_soilt(:,:,1:nlayer_irrig)
CALL irrigation_demand( land_pts, sm_levels, plant_n_gb,                       &
         frac_irr_soilt, sthf_soilt, smvccl_soilt, smvcst_soilt, dvi_cpft,     &
         demand_irrig_layer, smcl_target, sthu_irr_target,                     &
         sthu_target, l_irrigate_now )

! Get the total demand over all layers, converting from kg m-2 to kg.
DO m = 1,nsoilt
  DO l = 1,land_pts
    ! Sum over soil layers.
    demand_irrig_soilt(l,m) = SUM( demand_irrig_layer(l,m,1:nlayer_irrig) )
    ! Add to gridbox total.
    demand_irrig(l) = demand_irrig(l)                                          &
                      + demand_irrig_soilt(l,m) * frac_soilt(l,m)              &
                        * land_area(l)
  END DO
END DO

END SUBROUTINE calc_irrigation_demand

!#############################################################################
!#############################################################################

SUBROUTINE add_irrigation_to_soil( demand_irrig, demand_irrig_soilt,           &
                                   demand_irrig_layer,                         &
                                   frac_irr_soilt, land_area, smvcst_soilt,    &
                                   sthf_soilt, supply_irrig,                   &
                                   smcl_soilt, sthu_irr_soilt, sthu_soilt,     &
                                   irrig_water_gb )

!-----------------------------------------------------------------------------
! Description:
!   Add irrigation water to the soil.
!-----------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts, nsoilt

USE irrigation_mod, ONLY: nlayer_irrig

USE jules_soil_mod, ONLY: dzsoil, sm_levels

USE jules_water_resources_mod, ONLY: nstep_water_res

USE timestep_mod, ONLY: timestep

USE water_constants_mod, ONLY: rho_water

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Array arguments with intent(in).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  demand_irrig(land_pts),                                                      &
    ! Demand for irrigation water (kg).
  demand_irrig_soilt(land_pts,nsoilt),                                         &
    ! Demand for irrigation water per soil tile (kg m-2).
  demand_irrig_layer(land_pts,nsoilt,nlayer_irrig),                            &
    ! Demand for irrigation water in each layer (kg m-2).
  frac_irr_soilt(land_pts,nsoilt),                                             &
    ! Irrigation fraction.
  land_area(land_pts),                                                         &
    ! Area of land in each gridbox (m2).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  supply_irrig(land_pts)
    ! Water supplied for irrigation (kg).

!-----------------------------------------------------------------------------
! Array arguments with intent(in out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg m-2).
  sthu_irr_soilt(land_pts,nsoilt,sm_levels),                                   &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation in irrigated fraction.
  sthu_soilt(land_pts,nsoilt,sm_levels)
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.

!-----------------------------------------------------------------------------
! Array arguments with intent(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  irrig_water_gb(land_pts)
    ! Water added to soil via irrigation (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l,m,n
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  supplied,                                                                    &
    ! Water supplied by irrigation (kg m-2).
  supplied_layer
    ! Water supplied to a layer by irrigation (kg m-2).

!-----------------------------------------------------------------------------
! End of header

!-----------------------------------------------------------------------------
! Initialise a diagnostic.
!-----------------------------------------------------------------------------
irrig_water_gb(:) = 0.0

! Loop over soil tiles
DO m = 1,nsoilt
  ! Loop over land points
  DO l = 1,land_pts

    ! Note that all the demand variables used here are consistent in that they
    ! have all been increased to account for the efficiency of irrigation.
    ! This is important because they are considered in ratios.

    ! We only proceed if water is available to the gridbox.
    IF ( supply_irrig(l) > 0.0 ) THEN

      ! The supply is split between tiles in proportion to the demand from
      ! each. Convert the water supplied from kg to kg m-2 over the irrigated
      ! area. A factor of land_area in both numerator and deniminator cancel
      ! here.
      supplied = supply_irrig(l) * demand_irrig_soilt(l,m)                     &
                 / ( demand_irrig(l) * frac_irr_soilt(l,m) )

      ! Loop over layers.
      DO n = 1,nlayer_irrig

        IF ( demand_irrig_layer(l,m,n) > 0.0 ) THEN
          ! The available water is partitioned between layers according to the
          ! demand from each layer.
          supplied_layer = ( demand_irrig_layer(l,m,n)                         &
                             / demand_irrig_soilt(l,m) ) * supplied

          ! Update the soil wetness in the irrigated area.
          ! We assume that no other processes have increased soil moisture
          ! since the irrigation demand was calculated, and that the effects
          ! of rounding errors through the calculations will be small,
          ! meaning we do not need to check again for supersaturation.
          sthu_irr_soilt(l,m,n) = sthu_irr_soilt(l,m,n)                        &
            + supplied_layer / (smvcst_soilt(l,m,n) * dzsoil(n) * rho_water )

          ! Update the tile-mean moisture.
          smcl_soilt(l,m,n) = smcl_soilt(l,m,n)                                &
                              + supplied_layer * frac_irr_soilt(l,m)

          ! Diagnose the mean wetness.
          sthu_soilt(l,m,n) = smcl_soilt(l,m,n)                                &
                              / (rho_water * dzsoil(n) * smvcst_soilt(l,m,n))  &
                              - sthf_soilt(l,m,n)

        END IF  !  demand_irrig_layer > 0

      END DO  !  layers

      ! Add to the gridbox diagnostic, converting to kg m-2 s-1.
      irrig_water_gb(l) = irrig_water_gb(l) + supply_irrig(l)                  &
                          / ( land_area(l) * timestep * REAL(nstep_water_res) )

    END IF  !  supply>0

  END DO  !  points

END DO  !  soil tiles

END SUBROUTINE add_irrigation_to_soil

END MODULE irrigation_water_mod
#endif
