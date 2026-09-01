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

MODULE calc_avail_water_mod

!------------------------------------------------------------------------------
! Description:
!   Calculate water available for abstraction.
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
PUBLIC calc_avail_groundwater, calc_avail_surface_water

! Module parameters.
CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'CALC_AVAIL_WATER_MOD'

CONTAINS

!##############################################################################
!##############################################################################

SUBROUTINE calc_avail_groundwater( frac_soilt, land_area, smvcst_soilt,        &
                                   smvcwt_soilt, sthzw_soilt,                  &
                                   gw_avail, gw_avail_soilt  )

!------------------------------------------------------------------------------
! Description:
!   Calculate the amount of (renewable) groundwater available for abstraction.
!------------------------------------------------------------------------------

USE ancil_info, ONLY: land_pts, nsoilt

USE jules_hydrology_mod, ONLY: l_top, zw_max

USE jules_soil_mod, ONLY: dzsoil, sm_levels

USE water_constants_mod, ONLY: rho_water

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_soilt(land_pts,nsoilt),                                                 &
    !  Fraction of gridbox for each soil tile.
  land_area(land_pts),                                                         &
    ! Area of land in gridbox (m2).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  smvcwt_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric wilting point (m3/m3 of soil).
  sthzw_soilt(land_pts,nsoilt)
     ! Soil moisture fraction in deep TOPMODEL layer.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  gw_avail(land_pts),                                                          &
    ! Groundwater that is available for abstraction (kg).
  gw_avail_soilt(land_pts,nsoilt)
    ! Groundwater that is available for abstraction, on soil tiles (kg).

!------------------------------------------------------------------------------
! Local scalar parameters.
!------------------------------------------------------------------------------
LOGICAL, PARAMETER :: l_min_zero = .FALSE.
  ! TRUE means all water is available for abstraction.
  ! FALSE means only water above the wilting point is available.
!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  l,m
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  dz_rho,                                                                      &
    ! The product of a thickness and a density (kg m-2).
  smc_avail
    ! Available water in the "water table" layer (kg m-2).

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  smvc_min(land_pts)
    ! The minimum-allowed volumetric moisture content (m3/m3 of soil).

!------------------------------------------------------------------------------
! Initialise values.
!------------------------------------------------------------------------------
gw_avail(:)         = 0.0
gw_avail_soilt(:,:) = 0.0

! At present we only include TOPMODEl water.
! In future we might consider also including the RFM sub-surface store here.
IF ( l_top ) THEN

  ! Add water available in the TOPMODEL "water table" layer.
  ! First calculate thickness of "water table" layer * density.
  dz_rho = ( zw_max - SUM(dzsoil(1:sm_levels)) ) * rho_water

  ! Assume water in all soil tiles is available.
  DO m = 1,nsoilt

    ! Set the minimum-allowed soil moisture. We either consider that all
    ! water is available (unrealistic) or only water above the wilting point
    ! is available (a rather arbitrary reference in this context).
    IF ( l_min_zero ) THEN
      DO l = 1,land_pts
        smvc_min(l) = 0.0
      END DO
    ELSE
      ! All water above the wilting point is available.
      DO l = 1,land_pts
        smvc_min(l) = smvcwt_soilt(l,m,sm_levels)
      END DO
    END IF

    DO l = 1,land_pts
      IF ( sthzw_soilt(l,m) >= 0.0 ) THEN
        smc_avail = MAX( ( sthzw_soilt(l,m) * smvcst_soilt(l,m,sm_levels)      &
                           - smvc_min(l) )* dz_rho, 0.0 )
      ELSE
        smc_avail = 0.0
      END IF

      gw_avail_soilt(l,m) = smc_avail * frac_soilt(l,m) * land_area(l)
      gw_avail(l)         = gw_avail(l) + gw_avail_soilt(l,m)
    END DO
  END DO

END IF  !  l_top

END SUBROUTINE calc_avail_groundwater

!##############################################################################
!##############################################################################

SUBROUTINE calc_avail_surface_water( res_storage_global,                       &
                                     river_storage_global,                     &
                                     sw_avail_global )

!------------------------------------------------------------------------------
! Description:
!   Calculate the amount of surface water available for abstraction from
!   each source.
!------------------------------------------------------------------------------

USE model_grid_mod, ONLY: global_land_pts

USE jules_rivers_mod, ONLY: l_reservoirs, l_rivers

USE jules_water_resources_mod, ONLY:  n_sw_source,                             &
                                      sw_res_source, sw_river_source

IMPLICIT NONE

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  river_storage_global(global_land_pts),                                       &
    ! Water in rivers, on land points (kg).
  res_storage_global(global_land_pts)
    ! Water stored in reservoirs, on land points (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  sw_avail_global(global_land_pts,n_sw_source)
    ! Surface water that is available for abstraction (kg).

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_AVAIL_SURFACE_WATER'

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Initialise output values.
!------------------------------------------------------------------------------
sw_avail_global(:,:) = 0.0

IF ( l_rivers ) THEN

  ! Include water available from rivers.
  sw_avail_global(:,sw_river_source) = river_storage_global(:)

  IF ( l_reservoirs ) THEN
    ! Include water stored in reservoirs.
    sw_avail_global(:,sw_res_source) = res_storage_global(:)
  END IF

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_avail_surface_water

!##############################################################################
!##############################################################################

SUBROUTINE calc_avail_river_water( global_land_index, map_river_to_land_points,&
                                   rivers_index_rp, rfm_surfstore_rp,          &
                                   rivers_sto_rp, river_avail_global )

!------------------------------------------------------------------------------
! Description:
!   Calculate water available from river storage, on the land grid.
!------------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport

USE jules_rivers_mod, ONLY: i_river_vn, np_rivers, rivers_rfm, rivers_trip

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
  rfm_surfstore_rp(np_rivers),                                                 &
    ! River surface storage (m3).
  rivers_sto_rp(np_rivers)
    ! River water storage (kg).

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  river_avail_global(global_land_pts)
    ! Available river water, on land points (kg).

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CALC_AVAIL_RIVER_WATER'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  errorstatus
    ! Error value.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!------------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!------------------------------------------------------------------------------
! Initialise values.
!------------------------------------------------------------------------------
river_avail_global(:) = 0.0

!------------------------------------------------------------------------------
! Select code for the current river model: variables differ between models.
! Convert a variable on river points to one on land points.
!------------------------------------------------------------------------------
SELECT CASE ( i_river_vn )

CASE ( rivers_rfm )
  ! We will only include the surface store in the available water.
  CALL rivpts_to_landpts( global_land_pts, np_rivers, map_river_to_land_points,&
                          global_land_index, rivers_index_rp,                  &
                          rfm_surfstore_rp, river_avail_global )
  ! Convert units from m3 to kg.
  river_avail_global(:) = river_avail_global(:) * rho_water

CASE ( rivers_trip )
  CALL rivpts_to_landpts( global_land_pts, np_rivers, map_river_to_land_points,&
                          global_land_index, rivers_index_rp,                  &
                          rivers_sto_rp, river_avail_global )

CASE DEFAULT

  errorstatus = 101  !  a fatal error
  CALL ereport(RoutineName, errorstatus, 'Unknown value of i_river_vn.')

END SELECT  !  i_river_vn

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_avail_river_water

!##############################################################################

END MODULE calc_avail_water_mod
#endif
