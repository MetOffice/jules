#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_soil_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_soil(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the soil namelist items and checks them for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_soil_mod, ONLY: jules_soil, sm_levels, dzsoil, dzsoil_elev,          &
                          l_vg_soil, l_soil_sat_down, soilhc_method,           &
                          l_bedrock, l_tile_soil, l_broadcast_ancils,          &
                          check_jules_soil

USE jules_surface_mod, ONLY: l_elev_land_ice

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! First, we read the soil namelist
!-----------------------------------------------------------------------------
CALL log_info("init_soil", "Reading JULES_SOIL namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_soil.nml'),           &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_soil",                                                  &
                 "Error opening namelist file jules_soil.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_soil, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_soil",                                                  &
                 "Error reading namelist JULES_SOIL " //                       &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_soil",                                                  &
                 "Error closing namelist file jules_soil.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CALL check_jules_soil()

!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options
!-----------------------------------------------------------------------------
CALL log_info("init_soil",                                                     &
              "Using " // TRIM(to_string(sm_levels)) // " soil levels")

CALL log_info("init_soil",                                                     &
              "Soil levels: " // TRIM(to_string(dzsoil)))

IF ( l_elev_land_ice )                                                         &
CALL log_info("init_soil",                                                     &
            "Tiled ice subsurface depth: " // TRIM(to_string(dzsoil_elev)))

IF ( l_vg_soil )                                                               &
  CALL log_info("init_soil", "van Genuchten model will be used")

IF ( l_soil_sat_down ) THEN
  CALL log_info("init_soil",                                                   &
                "l_soil_sat_down = T - excess water is pushed down")
ELSE
  CALL log_info("init_soil",                                                   &
                "l_soil_sat_down = F - excess water is pushed up")
END IF

IF ( soilHc_method == 1 ) THEN
  CALL log_info("init_soil",                                                   &
                "soilHc_method = 1 - following Cox et al (1999)")
ELSE IF ( soilHc_method == 2 ) THEN
  CALL log_info("init_soil",                                                   &
                "soilHc_method = 2 - following simplified Johansen (1975)")
ELSE
  CALL log_info("init_soil",                                                   &
                "soilHc_method = 3 - Chadburn et al. (2015)")
END IF

IF ( l_bedrock )                                                               &
  CALL log_info("init_soil", "Bedrock will be included at base of soil")

IF ( l_tile_soil ) THEN
  CALL log_info("init_soil",                                                   &
    "l_tile_soil = T. Soil tiling is switched on: nsoilt = nsurft")
  IF ( l_broadcast_ancils ) THEN
    CALL log_info("init_soil",                                                 &
    "l_broadcast_ancils = T. GB mean ancils will be broadcast to all tiles")
  ELSE
    CALL log_info("init_soil",                                                 &
    "l_broadcast_ancils = F. Soil ancillaries must be defined for all tiles")
  END IF
ELSE
  CALL log_info("init_soil",                                                   &
    "l_tile_soil = F. Soil tiling is switched off: nsoilt = 1")
END IF

RETURN
END SUBROUTINE init_soil

END MODULE init_soil_mod
#endif
