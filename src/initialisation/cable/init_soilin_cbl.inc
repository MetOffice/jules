#if !defined(UM_JULES)
!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

SUBROUTINE init_soilin_cbl(nml_dir)

!-----------------------------------------------------------------------------
! Description:
!   Initialises the non-vegetation parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string
USE errormessagelength_mod, ONLY: errormessagelength

USE cable_fields_mod,       ONLY: pars_io_data_cbl

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

! CABLE constant, to be replaced once compilation with CABLE
! library works. ccc25
INTEGER, PARAMETER :: nsoil_max = 9 ! # of soil types

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_SOILIN_CABLE'

!-----------------------------------------------------------------------------

REAL ::                                                                        &
      silt_io(nsoil_max),                                                      &
      clay_io(nsoil_max),                                                      &
      sand_io(nsoil_max),                                                      &
      swilt_io(nsoil_max),                                                     &
      sfc_io(nsoil_max),                                                       &
      ssat_io(nsoil_max),                                                      &
      bch_io(nsoil_max),                                                       &
      hyds_io(nsoil_max),                                                      &
      sucs_io(nsoil_max),                                                      &
      rhosoil_io(nsoil_max),                                                   &
      css_io(nsoil_max)

!-----------------------------------------------------------------------------
! Namelist definition
!-----------------------------------------------------------------------------
NAMELIST / cable_soilparm/ silt_io, clay_io, sand_io, swilt_io,                &
          sfc_io, ssat_io, bch_io, hyds_io, sucs_io, rhosoil_io, css_io

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------
silt_io(:nsoil_max)       = rmdi
clay_io(:nsoil_max)    = rmdi
sand_io(:nsoil_max)    = rmdi
swilt_io(:nsoil_max)   = rmdi
sfc_io(:nsoil_max)     = rmdi
ssat_io(:nsoil_max)    = rmdi
bch_io(:nsoil_max)     = rmdi
hyds_io(:nsoil_max)    = rmdi
sucs_io(:nsoil_max)    = rmdi
rhosoil_io(:nsoil_max) = rmdi
css_io(:nsoil_max)     = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading CABLE_SOILPARM namelist...")

! Open the CABLE soil parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' //                              &
               'cable_soilparm.nml'),                                          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file cable_soilparm.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = cable_soilparm, IOSTAT = ERROR,                      &
                 IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist CABLE_SOILPARM " //                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file cable_soilparm.nml " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Copy values from dedicated I/O arrays into the soil parameter data type
pars_io_data_cbl%soilin_silt(1:nsoil_max)    = silt_io(1:nsoil_max)
pars_io_data_cbl%soilin_clay(1:nsoil_max)    = clay_io(1:nsoil_max)
pars_io_data_cbl%soilin_sand(1:nsoil_max)    = sand_io(1:nsoil_max)
pars_io_data_cbl%soilin_swilt(1:nsoil_max)   = swilt_io(1:nsoil_max)
pars_io_data_cbl%soilin_sfc(1:nsoil_max)     = sfc_io(1:nsoil_max)
pars_io_data_cbl%soilin_ssat(1:nsoil_max)    = ssat_io(1:nsoil_max)
pars_io_data_cbl%soilin_bch(1:nsoil_max)     = bch_io(1:nsoil_max)
pars_io_data_cbl%soilin_hyds(1:nsoil_max)    = hyds_io(1:nsoil_max)
pars_io_data_cbl%soilin_sucs(1:nsoil_max)    = sucs_io(1:nsoil_max)
pars_io_data_cbl%soilin_rhosoil(1:nsoil_max) = rhosoil_io(1:nsoil_max)
pars_io_data_cbl%soilin_css(1:nsoil_max)     = css_io(1:nsoil_max)

!-----------------------------------------------------------------------------
! Check that all variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( ABS( pars_io_data_cbl%soilin_silt(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for silt")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_clay(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for clay")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_sand(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for sand")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_swilt(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for swilt")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_sfc(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for sfc")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_ssat(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ssat")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_bch(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for bch")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_hyds(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for hyds")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_sucs(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for sucs")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_rhosoil(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rhosoil")
END IF
IF ( ANY( ABS( pars_io_data_cbl%soilin_css(1:nsoil_max) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for css")
END IF

IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

RETURN

END SUBROUTINE init_soilin_cbl

#endif
