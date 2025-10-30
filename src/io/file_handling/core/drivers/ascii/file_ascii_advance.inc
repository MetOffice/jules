#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_advance(FILE)

USE io_constants, ONLY: mode_read, mode_write

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
TYPE(file_ascii), INTENT(IN OUT) :: FILE  ! The file to advance


!-----------------------------------------------------------------------------

! We can't advance the file if it's in define mode
IF ( FILE%define_mode )                                                        &
  CALL log_fatal("file_ascii_advance",                                         &
                 "Cannot advance - file is still in define mode")

! We can only advance if there is a record dimension
IF ( .NOT. FILE%has_record_dim )                                               &
  CALL log_fatal("file_ascii_advance",                                         &
                 "Cannot advance file - no record dimension defined")


! We do different things depending on mode
SELECT CASE ( FILE%mode )
CASE ( mode_read )
  ! In read mode, we just fill the buffer with the next record of data, so that
  ! the next set of reads will use that data
  CALL file_ascii_fill_buffer(FILE)

CASE ( mode_write )
  ! In write mode, we just flush the buffer, so we can start populating it
  ! with the next records worth of data
  CALL file_ascii_flush_buffer(FILE)

END SELECT

RETURN

END SUBROUTINE file_ascii_advance
#endif
