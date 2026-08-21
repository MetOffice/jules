#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_inferno_mod

IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------
! Description:
!   Reads in the inferno fire namelist items and checks them for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
SUBROUTINE init_inferno(nml_dir)

USE jules_inferno_mod, ONLY: print_nlist_jules_inferno, check_jules_inferno,   &
  l_inferno, l_trif_fire, flam_sm_up, flam_sm_low, flam_rhum_up,               &
  flam_rhum_low, flam_fuel_up, flam_fuel_low, flam_rain_const,                 &
  triffire_ccdpm_max, triffire_ccdpm_min, triffire_ccrpm_max,                  &
  triffire_ccrpm_min, ignition_method, ignition_constant,                      &
  ignition_vary_natural, ignition_vary_natural_human

USE io_constants,     ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod,      ONLY: log_info,log_fatal

USE fire_mod,         ONLY: fire_cntl, l_fire_weather_index

USE fire_allocate_mod,ONLY: fire_allocate

USE fire_init_mod,    ONLY: fire_init

USE metstats_mod,     ONLY: l_metstats


IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

INTEGER :: ERROR  ! Error indicators

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_INFERNO'

!-----------------------------------------------------------------------------
! Definition of the jules_inferno namelist
!-----------------------------------------------------------------------------
NAMELIST  / jules_inferno/                                                     &
  l_trif_fire,                                                                 &
  l_inferno,                                                                   &
  ignition_method,                                                             &
  flam_sm_low,                                                                 &
  flam_sm_up,                                                                  &
  flam_rhum_low,                                                               &
  flam_rhum_up,                                                                &
  flam_rain_const,                                                             &
  flam_fuel_low,                                                               &
  flam_fuel_up,                                                                &
  triffire_ccdpm_min,                                                          &
  triffire_ccdpm_max,                                                          &
  triffire_ccrpm_min,                                                          &
  triffire_ccrpm_max
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Read the namelist
!-----------------------------------------------------------------------------

! Open the fire namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'fire.nml'),                 &
    STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName, "Error opening namelist file fire.nml " //       &
    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")


! There is one namelist to read from this file for jules inferno
CALL log_info(RoutineName, "Reading JULES_INFERNO namelist...")
READ(namelist_unit, NML = jules_inferno, IOSTAT = ERROR)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
    "Error reading namelist jules_inferno " //                                 &
    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
    "Error closing namelist file fire.nml " //                                 &
    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")

! CALL check_jules_inferno()

CALL print_nlist_jules_inferno()

! If not running inferno, we can bail after namelist has been read.
IF ( .NOT. l_inferno ) RETURN


!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options
! does this ahve to be ehrer?
!-----------------------------------------------------------------------------

IF ( l_inferno ) THEN
  CALL log_info(RoutineName,                                                   &
                "Interactive fires and emissions (INFERNO) will be diagnosed")
  IF (ignition_method == ignition_constant ) THEN
    CALL log_info(RoutineName,                                                 &
                  "Constant or ubiquitous ignitions (INFERNO)")
  ELSE IF (ignition_method == ignition_vary_natural ) THEN
    CALL log_info(RoutineName,                                                 &
                  "Constant human ignitions, varying lightning (INFERNO)")
  ELSE IF (ignition_method == ignition_vary_natural_human ) THEN
    CALL log_info(RoutineName,                                                 &
                  "Fully prescribed ignitions (INFERNO)")
  END IF
END IF


RETURN

END SUBROUTINE init_inferno
END MODULE init_inferno_mod
#endif
