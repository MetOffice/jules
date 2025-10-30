#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_enddef(FILE)

USE driver_ascii_mod, ONLY: file_ascii_enddef
USE driver_ncdf_mod, ONLY: file_ncdf_enddef

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Takes the file out of definition mode - no more dimensions or variables
!   may be defined after this
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_handle), INTENT(IN OUT) :: FILE


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  CALL file_ascii_enddef(FILE%ascii)

CASE ( driver_ncdf )
  CALL file_ncdf_enddef(FILE%ncdf)

CASE DEFAULT
  CALL log_fatal("file_enddef",                                                &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END SUBROUTINE file_enddef
#endif
