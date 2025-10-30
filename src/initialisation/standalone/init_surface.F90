#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE init_surface_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_surface(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the surface namelist items and checks them for consistency
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

USE jules_surface_mod, ONLY: jules_surface, l_aggregate, check_jules_surface,  &
   print_nlist_jules_surface

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
! First, we read the surface namelist
!-----------------------------------------------------------------------------
CALL log_info("init_surface", "Reading JULES_SURFACE namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_surface.nml'),        &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal("init_surface",                                               &
                 "Error opening namelist file jules_surface.nml " //           &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

READ(namelist_unit, NML = jules_surface, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal("init_surface",                                               &
                 "Error reading namelist JULES_SURFACE " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal("init_surface",                                               &
                 "Error closing namelist file jules_surface.nml " //           &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

CALL print_nlist_jules_surface()
CALL check_jules_surface()

!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options
!-----------------------------------------------------------------------------
IF ( l_aggregate ) THEN
  CALL log_info("init_surface",                                                &
                "Aggregate surface scheme has been selected")
END IF

RETURN

END SUBROUTINE init_surface

END MODULE init_surface_mod
#endif
