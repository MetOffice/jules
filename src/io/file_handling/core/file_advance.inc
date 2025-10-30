#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_advance(FILE)

USE driver_ascii_mod, ONLY: file_ascii_advance
USE driver_ncdf_mod, ONLY: file_ncdf_advance

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Advances the file by one record
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_handle), INTENT(IN OUT) :: FILE  ! The file to seek


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  CALL file_ascii_advance(FILE%ascii)

CASE ( driver_ncdf )
  CALL file_ncdf_advance(FILE%ncdf)

CASE DEFAULT
  CALL log_fatal("file_advance",                                               &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END SUBROUTINE file_advance
#endif
