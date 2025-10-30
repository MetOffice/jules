#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_fill_buffer(FILE)

USE io_constants, ONLY: mode_read
USE errormessagelength_mod, ONLY: errormessagelength
USE string_utils_mod, ONLY: str_starts_with

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   INTERNAL PROCEDURE - reads the next data from file, skipping any comment
!   lines, and fills the buffer with it
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
CHARACTER(LEN=FILE%record_len) :: line
CHARACTER(LEN=errormessagelength) :: iomessage

LOGICAL :: is_comment  ! Indicates if the current line is a comment line
INTEGER :: i  ! Loop counter

INTEGER :: ERROR  ! Error indicator


!-----------------------------------------------------------------------------

! We can't fill the buffer until enddef has been called
IF ( FILE%define_mode )                                                        &
  CALL log_fatal("file_ascii_fill_buffer",                                     &
                 "Cannot fill buffer - file is still in define mode")

IF ( FILE%mode /= mode_read )                                                  &
  CALL log_fatal("file_ascii_fill_buffer",                                     &
                 "Cannot read from file - file is not in read mode")

! Read lines from the file until we get a non-comment line
DO
  READ(FILE%UNIT, "(A)", IOSTAT = ERROR, IOMSG = iomessage) line
  IF ( ERROR /= 0 )                                                            &
    CALL log_fatal("file_ascii_fill_buffer",                                   &
                   "Error reading from file " //                               &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")

  ! Remove any leading spaces
  line = ADJUSTL(line)

  ! We skip over empty lines
  IF ( LEN_TRIM(line) == 0 ) CYCLE

  ! If the line does not start with a comment char, we have found a line of data
  ! and can exit the loop
  is_comment = .FALSE.
  DO i = 1,SIZE(comment_chars)
    IF ( str_starts_with(line, comment_chars(i)) ) THEN
      is_comment = .TRUE.
      EXIT
    END IF
  END DO
  IF ( .NOT. is_comment ) EXIT
END DO

! Fill the buffer from the line of data
READ(line, *) FILE%buffer(:)

RETURN

END SUBROUTINE file_ascii_fill_buffer
#endif
