#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_enddef(FILE)

USE io_constants, ONLY: mode_write

USE netcdf, ONLY: nf90_enddef

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
TYPE(file_ncdf), INTENT(IN OUT) :: FILE

INTEGER :: ERROR      ! Error indicator


!-----------------------------------------------------------------------------

! We are only genuinely in define mode if the file is open in write mode
IF ( FILE%mode == mode_write ) THEN
  ERROR = nf90_enddef(FILE%id)
  IF ( ERROR /= nf90_noerr )                                                   &
    CALL log_fatal_ncdf("file_ncdf_enddef",                                    &
                        "Error exiting define mode", ERROR)
END IF

RETURN

END SUBROUTINE file_ncdf_enddef
#endif
