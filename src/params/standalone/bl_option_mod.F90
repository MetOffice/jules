#if !defined(UM_JULES)
! *****************************COPYRIGHT********************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT********************************

! **********************************************************************
! THIS VERSION OF bl_option_mod IS STRICTLY FOR USE WITH
! STANDALONE JULES!
! **********************************************************************

!  Data module for switches/options concerned with the BL scheme.
! Description:
!   Module containing runtime options/data used by the boundary
!   layer scheme.

! Method:
!   Switches and associated data values used by the boundary layer
!   scheme are defined here and assigned default values. These may
!   be overridden by namelist input.

!   Any routine wishing to use these options may do so with the 'USE'
!   statement.

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Boundary Layer

! Code Description:
!   Language: FORTRAN 90

MODULE bl_option_mod

! Declarations:

IMPLICIT NONE

REAL, PARAMETER :: max_stress_grad = 0.05
                       ! Maximum implied stress gradient across the
                       ! boundary layer, used to limit the explicit
                       ! stress applied in non-local scalings (m/s2)

! logical for whether to skip calculations based on start of timestep
! quantities when using semi-lagrangian cycling with Endgame
! This is hard-wired to true in readlsta_4a so Endgame always uses it,
! otherwise it is false
! N.B. results should bit-compare whether this logical is true or false
! and so changing it will be a good test of whether new code has been
! added correctly
LOGICAL :: l_quick_ap2 = .FALSE.

END MODULE bl_option_mod

#endif
