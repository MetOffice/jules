#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_gridded_advance(FILE)

USE file_mod, ONLY: file_advance

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
TYPE(file_gridded), INTENT(IN OUT) :: FILE  ! The file to seek


!-----------------------------------------------------------------------------

! Just defer to the underlying file
CALL file_advance(FILE%fh)

RETURN

END SUBROUTINE file_gridded_advance
#endif
