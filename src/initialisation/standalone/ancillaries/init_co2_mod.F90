#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_co2_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_co2()

USE io_constants, ONLY: namelist_unit

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE aero, ONLY: co2_mmr_mod => co2_mmr

USE dump_mod, ONLY: ancil_dump_read

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in the Read the JULES_CO2 namelist
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage
CHARACTER(LEN=*), PARAMETER :: RoutineName='init_co2'

!-----------------------------------------------------------------------------
! Definition of the jules_co2 namelist
!-----------------------------------------------------------------------------
LOGICAL :: read_from_dump = .FALSE.
REAL(KIND=real_jlslsm)    :: co2_mmr

NAMELIST  / jules_co2 / read_from_dump, co2_mmr

!Copy across the default value set up in the module.
co2_mmr = co2_mmr_mod

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_CO2 namelist...")
READ(namelist_unit, NML = jules_co2, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_CO2 " //                        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

ancil_dump_read%co2 = read_from_dump

IF ( .NOT. ancil_dump_read%co2) THEN
  co2_mmr_mod = co2_mmr
ELSE
  ! We read from the dump file.
  CALL log_info(RoutineName,                                                   &
                "co2_mmr will be read from the dump file.  " //                &
                "Namelist value ignored")
END IF

RETURN

END SUBROUTINE init_co2
END MODULE init_co2_mod
#endif
