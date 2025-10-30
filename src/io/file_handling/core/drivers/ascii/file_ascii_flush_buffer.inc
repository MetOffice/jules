#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_flush_buffer(FILE)

USE io_constants, ONLY: mode_write, mdi
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   INTERNAL PROCEDURE - flushes the current buffer to file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ascii), INTENT(IN OUT) :: FILE


! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------

! We can't flush the buffer until enddef has been called
IF ( FILE%define_mode )                                                        &
  CALL log_fatal("file_ascii_flush_buffer",                                    &
                 "Cannot flush buffer - file is still in define mode")

IF ( FILE%mode /= mode_write )                                                 &
  CALL log_fatal("file_ascii_flush_buffer",                                    &
                 "Cannot write to file - file is not in write mode")

! Just write the current buffer to the file
WRITE(FILE%UNIT, out_format_str, IOSTAT = ERROR, IOMSG = iomessage)            &
     FILE%buffer(:)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("file_ascii_flush_buffer",                                    &
                 "Error writing to file " //                                   &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Reset buffer values to missing data value
FILE%buffer(:) = mdi
! Buffer is clean again (i.e. has no writes that have not been committed to
! file)
FILE%buffer_is_dirty = .FALSE.

RETURN

END SUBROUTINE file_ascii_flush_buffer
#endif
