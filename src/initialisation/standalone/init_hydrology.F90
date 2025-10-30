#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_hydrology_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_hydrology(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the hydrology namelist items and checks them for consistency
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

USE jules_hydrology_mod, ONLY: jules_hydrology, check_jules_hydrology,         &
                                l_top, l_pdm

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
! First, read the hydrology namelist
!-----------------------------------------------------------------------------
CALL log_info("init_hydrology", "Reading JULES_HYDROLOGY namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_hydrology.nml'),      &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_hydrology",                                             &
                 "Error opening namelist file jules_hydrology.nml " //         &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_hydrology, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_hydrology",                                             &
                 "Error reading namelist JULES_HYDROLOGY " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_hydrology",                                             &
                 "Error closing namelist file jules_hydrology.nml " //         &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


CALL check_jules_hydrology


IF ( l_pdm ) THEN
  CALL log_info("init_hydrology", "PDM is on")
ELSE IF ( l_top ) THEN
  CALL log_info("init_hydrology", "TOPMODEL is on")
ELSE
  CALL log_info("init_hydrology", "No large-scale hydrology selected")
END IF

RETURN
END SUBROUTINE init_hydrology

END MODULE init_hydrology_mod
#endif
