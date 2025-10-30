#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_snow_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_snow(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the snow namelist items and checks them for consistency
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

USE jules_snow_mod, ONLY: jules_snow, check_jules_snow, nsmax,                 &
   print_nlist_jules_snow

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

USE land_tile_ids_mod, ONLY: set_ml_snow_type_ids

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! First, read the snow namelist
!-----------------------------------------------------------------------------
CALL log_info("init_snow", "Reading JULES_SNOW namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_snow.nml'),           &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_snow",                                                  &
                 "Error opening namelist file jules_snow.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_snow, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_snow",                                                  &
                 "Error reading namelist JULES_SNOW " //                       &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_snow",                                                  &
                 "Error closing namelist file jules_snow.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CALL print_nlist_jules_snow()
CALL check_jules_snow()
! Sets multilayer snow indices for headers
CALL set_ml_snow_type_ids()

IF ( nsmax > 0 ) THEN
  CALL log_info("init_snow",                                                   &
                "Using multi-layer snow scheme with " //                       &
                TRIM(to_string(nsmax)) // " levels")
ELSE
  CALL log_info("init_snow", "Using 'old' snow scheme")
END IF

RETURN
END SUBROUTINE init_snow

END MODULE init_snow_mod
#endif
