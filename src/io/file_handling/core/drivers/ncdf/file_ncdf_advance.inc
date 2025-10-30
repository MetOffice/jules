#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_advance(FILE)

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to advance


!-----------------------------------------------------------------------------

IF ( FILE%record_dim < 0 )                                                     &
  CALL log_fatal("file_ncdf_advance",                                          &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "Cannot advance - no record dimension defined")

FILE%current_record = FILE%current_record + 1

RETURN

END SUBROUTINE file_ncdf_advance
#endif
