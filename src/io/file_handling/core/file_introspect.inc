#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_introspect(FILE)

USE driver_ascii_mod, ONLY: file_ascii_introspect
USE driver_ncdf_mod, ONLY: file_ncdf_introspect

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given an open file_handle in read mode, try to detect the dimensions and
!   variables in the file and define them on the file_handle
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_handle), INTENT(IN OUT) :: FILE  ! The file to detect items in


!-----------------------------------------------------------------------------

SELECT CASE ( FILE%driver )
CASE ( driver_ascii )
  CALL file_ascii_introspect(FILE%ascii)

CASE ( driver_ncdf )
  CALL file_ncdf_introspect(FILE%ncdf)

CASE DEFAULT
  CALL log_fatal("file_introspect",                                            &
                 "Unrecognised driver - see docs for available drivers")
END SELECT

RETURN

END SUBROUTINE file_introspect
#endif
