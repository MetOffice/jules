#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
! ******************************** COPYRIGHT *********************************

! ****************************************************************************
! This is a stripped-down version of UM file nlsizes_namelist_mod.F90 for use
! with standalone JULES only!
! ****************************************************************************

MODULE nlsizes_namelist_mod

IMPLICIT NONE

! Public scope by default.

SAVE

INTEGER :: bl_levels = 1
  ! Number of boundary layer levels.
  ! In this standalone version we default to a single level.
  ! At present this value is only used for dry deposition code.

NAMELIST  / jules_nlsizes/ bl_levels

END MODULE nlsizes_namelist_mod

#endif
