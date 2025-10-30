#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE precision_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This module defines the appropriate kinds for 32 and 64 bit integers and
!   reals
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, PARAMETER ::                                                          &
  real_32 = SELECTED_REAL_KIND(6, 37),                                         &
      ! Kind for 32-bit reals
  real_64 = SELECTED_REAL_KIND(15, 307),                                       &
      ! Kind for 64-bit reals
  int_32 = SELECTED_INT_KIND(9),                                               &
      ! Kind for 32-bit integers
  int_64 = SELECTED_INT_KIND(15)
      ! Kind for 64-bit integers

END MODULE precision_mod
#endif
