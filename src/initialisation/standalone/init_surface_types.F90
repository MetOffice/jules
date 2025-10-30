#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_surface_types_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_jules_surface_types(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the JULES surface types namelist items and checks them for
!   consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
USE string_utils_mod, ONLY: to_string

USE jules_surface_types_mod, ONLY:                                             &
    check_jules_surface_types, set_derived_variables_jules_surface_types,      &
    read_nml_jules_surface_types, print_nlist_jules_surface_types,             &
    ice, lake, ncpft, nnpft, nnvg, soil, urban, urban_canyon, urban_roof

USE land_tile_ids_mod, ONLY: set_surface_type_ids

USE logging_mod, ONLY: log_info


IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CALL read_nml_jules_surface_types(nml_dir)
CALL set_derived_variables_jules_surface_types()
CALL print_nlist_jules_surface_types()
! Set the surface_type_ids array and carry out additional checks
CALL set_surface_type_ids ()
CALL check_jules_surface_types()

! Report some info about the surface types
! This list is probably redundant and need not be added to in future, the print
! nlist routine prints the namelist.
CALL log_info("init_jules_surface_types",                                      &
              "Using " // TRIM(to_string(nnpft)) // " natural PFTs and " //    &
              TRIM(to_string(ncpft)) // " crop PFTs")

CALL log_info("init_jules_surface_types",                                      &
              "Using " // TRIM(to_string(nnvg)) // " non-veg surface types")

CALL log_info("init_jules_surface_types",                                      &
              "Soil is type #" // TRIM(to_string(soil)))

IF ( lake > 0 ) THEN
  CALL log_info("init_jules_surface_types",                                    &
                "Lake (inland water) is type #" // TRIM(to_string(lake)))
ELSE
  CALL log_info("init_jules_surface_types",                                    &
                "No lake (inland water) type specified")
END IF

IF ( ice > 0 ) THEN
  CALL log_info("init_jules_surface_types",                                    &
                "Land ice is type #" // TRIM(to_string(ice)))
ELSE
  CALL log_info("init_jules_surface_types",                                    &
                "No land ice type specified")
END IF

IF ( urban > 0 ) THEN
  CALL log_info("init_jules_surface_types",                                    &
                "Urban is type #" // TRIM(to_string(urban)))
ELSE
  CALL log_info("init_jules_surface_types",                                    &
                "No urban type specified (URBAN-1T)")
END IF

IF ( urban_canyon > 0 ) THEN
  CALL log_info("init_jules_surface_types",                                    &
                "Urban canyon is type #" // TRIM(to_string(urban_canyon)))
ELSE
  CALL log_info("init_jules_surface_types",                                    &
                "No canyon type specified (URBAN-2T or MORUSES)")
END IF

IF ( urban_roof > 0 ) THEN
  CALL log_info("init_jules_surface_types",                                    &
                "Urban roof is type #" // TRIM(to_string(urban_roof)))
ELSE
  CALL log_info("init_jules_surface_types",                                    &
                "No roof type specified (URBAN-2T or MORUSES)")
END IF

RETURN
END SUBROUTINE init_jules_surface_types

!-----------------------------------------------------------------------------

SUBROUTINE init_cable_surface_types(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the CABLE surface types namelist items and checks them for
!   consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in CABLE SCIENCE
!-----------------------------------------------------------------------------
USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE cable_surface_types_mod, ONLY: cable_surface_types,                        &
    check_cable_surface_types, nnpft_cable, nnvg_cable, ncpft_cable,           &
    ice_cable, lakes_cable, barren_cable, urban_cable,                         &
    set_derived_variables_cable_surface_types

USE jules_surface_types_mod, ONLY: soil

USE logging_mod, ONLY: log_info, log_error, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! First, read the surface types namelist
!-----------------------------------------------------------------------------
CALL log_info("init_cable_surface_types", "Reading CABLE_SURFACE_TYPES namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'cable_surface_types.nml'),  &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cable_surface_types",                                   &
                 "Error opening namelist file cable_surface_types.nml " //     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = cable_surface_types, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cable_surface_types",                                   &
                 "Error reading namelist CABLE_SURFACE_TYPES " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_cable_surface_types",                                   &
                 "Error closing namelist file cable_surface_types.nml " //     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CALL set_derived_variables_cable_surface_types()
CALL check_cable_surface_types()


! Report some info about the surface types
CALL log_info("init_cable_surface_types",                                      &
              "Using " // TRIM(to_string(nnpft_cable)) //                      &
              " natural PFTs and " // TRIM(to_string(ncpft_cable)) //          &
              " crop PFTs")

CALL log_info("init_cable_surface_types",                                      &
              "Using " // TRIM(to_string(nnvg_cable)) //                       &
              " non-veg surface types")

IF ( lakes_cable > 0 ) THEN
  CALL log_info("init_cable_surface_types",                                    &
                "Lakes (inland water) is type #" //                            &
                TRIM(to_string(lakes_cable)))
ELSE
  CALL log_info("init_cable_surface_types",                                    &
                "No lakes (inland water) type specified")
END IF

IF ( ice_cable > 0 ) THEN
  CALL log_info("init_cable_surface_types",                                    &
                "Land ice for CABLE is type #" // TRIM(to_string(ice_cable)))
ELSE
  CALL log_info("init_cable_surface_types",                                    &
                "No land ice type specified")
END IF

IF ( barren_cable > 0 ) THEN
  CALL log_info("init_cable_surface_types",                                    &
                "Barren is type #" // TRIM(to_string(barren_cable)))
ELSE
  CALL log_info("init_cable_surface_types",                                    &
                "No barren type specified")
END IF

IF ( urban_cable > 0 ) THEN
  CALL log_info("init_cable_surface_types",                                    &
                "Urban is type #" // TRIM(to_string(urban_cable)))
ELSE
  CALL log_info("init_cable_surface_types",                                    &
                "No urban type specified")
END IF

RETURN
END SUBROUTINE init_cable_surface_types

END MODULE init_surface_types_mod
#endif
