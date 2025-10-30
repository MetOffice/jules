#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  Water related physical constants

MODULE water_constants_mod

! Description:
!        Water related physical constants

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Top Level

! Code Description:
!   Language: FORTRAN 95
!   This code is written to UMDP3 v8 programming standards.

IMPLICIT NONE

! tfs, temperature at which sea water freezes (K)
REAL, PARAMETER :: tfs                 = 271.35
! tm, temperature at which fresh water freezes and ice melts (K)
REAL, PARAMETER :: tm                  = 273.15

! density of pure water (kg/m3)
REAL, PARAMETER :: rho_water           = 1000.0
! density of sea water (kg/m3)
REAL, PARAMETER :: rhosea              = 1026.0
! Density of ice (kg/m3)
REAL, PARAMETER :: rho_ice             = 917

! latent heat of condensation of water at 0degc (J kg-1)
REAL, PARAMETER :: lc                  = 2.501e6
! latent heat of fusion of water at 0degc (J kg-1)
REAL, PARAMETER :: lf                  = 0.334e6

! Specific heat capacity of water vapour (J/kg/K)
REAL, PARAMETER :: hcapv               = 1850.0
! Specific heat capacity of water (J/kg/K)
REAL, PARAMETER :: hcapw               = 4180.0
! Specific heat capacity of ice (J/kg/K)
REAL, PARAMETER :: hcapi               = 2100.0

! Rate of change of ice potential with temperature
! RHO_ICE*LF/ZERODEGC*1/(RHO_WATER*G) (m/K)
REAL, PARAMETER :: dpsidt              = 114.3

END MODULE water_constants_mod
#endif
