! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE wtrac_check_options_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE wtrac_check_options(ERROR)
!-----------------------------------------------------------------------------
! Description:
!   Checks that the enabled science schemes are compatible with the water
!   tracer scheme.
!   Note, water tracers are not currently available in standalone mode.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_hydrology_mod,           ONLY: l_wetland_unfrozen, l_pdm
USE jules_irrig_mod,               ONLY: l_irrig_dmd
USE jules_rivers_mod,              ONLY: lake_water_conserve_method
USE jules_sea_seaice_mod,          ONLY: l_tstar_sice_new, nice, nice_use
USE jules_snow_mod,                ONLY: nsmax
USE jules_soil_mod,                ONLY: l_tile_soil, l_holdwater,             &
                                         l_soil_sat_down
USE jules_surface_mod,             ONLY: l_flake_model, l_point_data
USE jules_vegetation_mod,          ONLY: l_use_pft_psi
USE jules_water_resources_mod,     ONLY: l_water_resources

USE ereport_mod,                   ONLY: ereport
USE jules_print_mgr,               ONLY: jules_print

IMPLICIT NONE

INTEGER, INTENT(IN OUT) :: ERROR  ! Error indicator

CHARACTER(LEN=*), PARAMETER :: routinename='WTRAC_CHECK_OPTIONS'

! Check for compatibility (Note, this routine is only called if l_wtrac_jls=T)

! Snow related
IF (nsmax == 0) THEN
  ! Code exists but has not been tested so stop use
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers require the multiple snow layer model (nsmax > 0)")
END IF

! Hydrology related
IF ( l_irrig_dmd ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers not presently compatible with irrigation (l_irrig_dmd)")
END IF

IF ( l_tile_soil ) THEN
  ! Code exists but has not been tested so stop use
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers not presently compatible with soil tiling (l_tile_soil)")
END IF

IF ( l_pdm ) THEN
  ! Code exists but has not been fully tested so stop use
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers not presently compatible with PDM scheme (l_pdm)")
END IF

IF ( l_wetland_unfrozen ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers not presently compatible with l_wetland_unfrozen=T")
END IF

IF ( .NOT. l_soil_sat_down ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
      "Water tracers not presently compatible l_soil_sat_down=F")
END IF

IF ( l_holdwater ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers not presently compatible l_holdwater=T")
END IF

IF ( lake_water_conserve_method  /=  2 ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers require lake_water_conserve_method = 2")
END IF

IF ( l_flake_model ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers are not compatible with the FLAKE model (l_flake_model)")
END IF

! Water resources related
IF ( l_water_resources ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers are not compatible with the water resource model"          // &
  " (l_water_resources)")
END IF

! Surface related
IF ( l_use_pft_psi ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers are not presently compatible with l_use_pft_psi=T")
END IF

IF (l_point_data) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers are not compatible with point rainfall data (l_point_data=T)")
END IF

IF ( .NOT. l_tstar_sice_new ) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers are not presently compatible with l_tstar_sice_new=F")
END IF

IF (nice  /=  nice_use) THEN
  ERROR = 1
  CALL jules_print(routinename,                                                &
  "Water tracers require full use of sea ice categories (nice=nice_use)")
END IF

RETURN
END SUBROUTINE wtrac_check_options

END MODULE wtrac_check_options_mod
