#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! A module containing constants/parameters for Chemistry
!
MODULE chemistry_constants_mod

IMPLICIT NONE

! Description:
!   This module contains constants for chemistry
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code description:
!   Language: Fortran 2003
!   This code is written to UMDP3 standards.
!----------------------------------------------------------------------

! No. of molecules in 1 mole
REAL, PARAMETER :: avogadro = 6.022e23

! Boltzmanns constant (J K-1)
REAL, PARAMETER :: boltzmann = 1.3804e-23

! Mean Free Path
REAL, PARAMETER :: mfp_ref = 6.6e-8 ! Ref value (m)
REAL, PARAMETER :: tref_mfp = 293.15 ! Ref temperature (K)
REAL, PARAMETER :: pref_mfp = 1.01325e5 ! Ref pressure (Pa)

END MODULE chemistry_constants_mod
#endif
