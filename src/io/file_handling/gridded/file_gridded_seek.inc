#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_gridded_seek(FILE, RECORD)

USE file_mod, ONLY: file_seek

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Seeks the file to before a particular record (i.e. the next time a
!   values are read from the file using file_read_var, they will be read from
!   the requested record)
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_gridded), INTENT(IN OUT) :: FILE  ! The file to seek
INTEGER, INTENT(IN) :: RECORD              ! The record number to seek to


!-----------------------------------------------------------------------------

! We just need to defer to the underlying file
CALL file_seek(FILE%fh, RECORD)

RETURN

END SUBROUTINE file_gridded_seek
#endif
