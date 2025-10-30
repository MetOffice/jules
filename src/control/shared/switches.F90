! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!
! Module containing miscellaneous switches, mostly non-UM
!

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE switches

IMPLICIT NONE

LOGICAL ::                                                                     &
  l_360 = .FALSE.
      ! Switch for setting a 360 day year
      ! This remains in this module for UM compatibility
      ! When running in the UM, this is set from the UM variable
      ! lcal360
      ! When running standalone it is read in the JULES_TIME namelist


#if !defined(UM_JULES)
!-----------------------------------------------------------------------
! Switches that are always .FALSE.
!-----------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_mr_physics = .FALSE.,                                                      &
      ! TRUE if mixing ratios used in boundary layer code
  l_spec_z0 = .FALSE.,                                                         &
      ! T if using prescribed sea surface roughness lengths
  l_co2_interactive = .FALSE.,                                                 &
      ! Switch for 3D CO2 field
  l_dust = .FALSE.,                                                            &
      ! Switch for mineral dust
  l_z0_orog = .FALSE.
      ! T to use orog.roughness in surface calcs
#endif

END MODULE switches
