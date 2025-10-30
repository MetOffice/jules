#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_close(FILE)

USE io_constants, ONLY: mode_write
USE errormessagelength_mod, ONLY: errormessagelength
USE file_ascii_generic_sync_mod, ONLY: file_sync
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
TYPE(file_ascii), INTENT(IN OUT) :: FILE ! The file to close


! Work variables
INTEGER :: ERROR ! Error code for any errors that occur
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------

CALL log_info("file_ascii_close",                                              &
              "Closing file " // TRIM(FILE%NAME))

! Flush the buffer to disk only if it has uncommitted writes
IF ( FILE%mode == mode_write ) THEN

  IF ( FILE%buffer_is_dirty ) THEN
    CALL file_ascii_flush_buffer(FILE)
  END IF

  CALL file_sync(FILE%UNIT)

END IF

! Deallocate the memory associated with the buffer
DEALLOCATE(FILE%buffer)
NULLIFY(FILE%buffer)

CLOSE(FILE%UNIT, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("file_ascii_close",                                           &
                 "Error closing file " // TRIM(FILE%NAME) //                   &
                 " (IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //         &
                 TRIM(iomessage) // ")")

RETURN

END SUBROUTINE file_ascii_close
#endif
