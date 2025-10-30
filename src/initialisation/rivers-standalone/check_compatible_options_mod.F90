! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE check_compatible_options_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE check_compatible_options()
!-----------------------------------------------------------------------------
! Description:
!   Rivers-only version of routine which checks that the enabled science
!   options are compatible. Refer to the JULES user manual for more information.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE model_time_mod,  ONLY: max_spinup_cycles


USE ereport_mod,     ONLY: ereport
USE jules_print_mgr, ONLY: jules_print, jules_message

IMPLICIT NONE

! Work variables
INTEGER :: ERROR  ! Error indicator

CHARACTER(LEN=*), PARAMETER :: routinename='CHECK_COMPATIBLE_OPTIONS'

ERROR = 0

! jules_spinup
IF ( max_spinup_cycles > 0 ) THEN
  ERROR = 1
  WRITE(jules_message, *)                                                      &
     "Spin up cannot be used with Rivers-only: max_spinup_cycles = ",          &
     max_spinup_cycles
  CALL jules_print(routinename, jules_message)
END IF

IF ( ERROR /= 0 )                                                              &
   CALL ereport(routinename, ERROR,                                            &
                  "Error(s) found in enabled science options and/or " //       &
                  "parameter choice(s) - see job.out for error message(s)")

RETURN
END SUBROUTINE check_compatible_options
END MODULE check_compatible_options_mod
