#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_seek(FILE, RECORD)

USE io_constants, ONLY: mode_read

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Seeks the file to a particular record (i.e. the next time values are read
!   from the file using file_read_var, they will be read from the requested
!   record)
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ncdf), INTENT(IN OUT) :: FILE  ! The file to seek
INTEGER, INTENT(IN) :: RECORD           ! The record number to seek to


!-----------------------------------------------------------------------------

IF ( FILE%mode /= mode_read )                                                  &
  CALL log_fatal("file_ncdf_seek",                                             &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "Arbitrary seeking is only allowed in read mode")

IF ( FILE%record_dim < 0 )                                                     &
  CALL log_fatal("file_ncdf_seek",                                             &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "Cannot seek - no record dimension defined")

IF ( RECORD < 1 )                                                              &
  CALL log_fatal("file_ncdf_seek",                                             &
                 TRIM(FILE%NAME) // ": " //                                    &
                 "Record number must be > 0")

FILE%current_record = RECORD

RETURN

END SUBROUTINE file_ncdf_seek
#endif
