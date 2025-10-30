#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE errormessagelength_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines the errormessagelength variable, for use in standalone JULES when
!   the UM module of the same name is unavailable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

PRIVATE

! The length of error messages. This is used for IOMSG, and so needs to be
! long enough to contain a full path and filename with space left over for the
! error message, currently (filename length x 2)
INTEGER, PUBLIC :: errormessagelength = 512

END MODULE errormessagelength_mod
#endif
