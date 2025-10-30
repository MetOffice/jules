#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_close(FILE)

USE netcdf, ONLY: nf90_close
USE io_constants, ONLY: mode_write

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Closes and frees any resources consumed by the given file object
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ncdf), INTENT(IN OUT) :: FILE ! The file to close


! Work variables
INTEGER :: ERROR ! Error code for any errors that occur

!-----------------------------------------------------------------------------

CALL log_info("file_ncdf_close",                                               &
              "Closing file " // TRIM(FILE%NAME))

IF ( FILE%mode == mode_write )                                                 &
  CALL file_ncdf_sync(FILE)

ERROR = nf90_close(FILE%id)
IF ( ERROR /= nf90_noerr )                                                     &
  CALL log_fatal_ncdf("file_ncdf_close",                                       &
                      "Error closing file " // TRIM(FILE%NAME), ERROR)

RETURN

END SUBROUTINE file_ncdf_close
#endif
