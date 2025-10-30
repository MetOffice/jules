#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE ereport_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Implements the same interface as the UM ereport_mod to allow error
!   reporting to use a common interface in the UM and standalone
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

PRIVATE
PUBLIC ereport

CONTAINS


SUBROUTINE ereport(proc_name, error_status, message)

USE logging_mod, ONLY: log_info, log_warn, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Subroutine for reporting errors that replicates the UM interface but uses
!   the JULES logging module under the hood
!
!   An error status of > 0 corresponds to a log_fatal, < 0 to a log_warn and
!   0 to a log_info
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Arguments
CHARACTER(LEN=*), INTENT(IN)   :: proc_name
INTEGER, INTENT(IN OUT)         :: error_status
CHARACTER(LEN=*), INTENT(IN)   :: message

!-----------------------------------------------------------------------------

IF ( error_status > 0 ) THEN
  CALL log_fatal(TRIM(proc_name), TRIM(message))
ELSE IF ( error_status < 0 ) THEN
  CALL log_warn(TRIM(proc_name), TRIM(message))
ELSE
  CALL log_info(TRIM(proc_name), TRIM(message))
END IF

! reset error_status
error_status = 0

RETURN
END SUBROUTINE ereport

END MODULE ereport_mod
#endif
