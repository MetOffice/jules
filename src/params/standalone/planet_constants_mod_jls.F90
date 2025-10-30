#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  Global physical constants

MODULE planet_constants_mod

! Description:
!       Global physical constants

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Top Level

! Code Description:
!   Language: FORTRAN 95
!   This code is written to UMDP3 v8 programming standards.


! This is a stripped down version for the JULES standalone code.
! The UM version defines parameters such as earth_g and then sets g = earth_g
! if the chosen planet is the Earth. For JULES we use the parameters to
! hold the Earth-specific values.

IMPLICIT NONE

! Von Karman's constant
REAL, PARAMETER :: vkman               = 0.4

! r, the Gas constant for dry air
REAL :: r                   = 287.05

! cp, specific heat of dry air at constant pressure
REAL :: cp                  = 1005.0

! pref, reference surface pressure
REAL :: pref                = 100000.0

! repsilon, ratio of molecular weights of water and dry air
REAL :: repsilon            = 0.62198

! the planet radius (m)
REAL :: planet_radius       = 6371229.0

! Mean acceleration due to gravity at the Earth's surface (m s-2).
REAL :: g             = 9.80665

! planet equatorial radius 'a' (m)
! from Appendix A of Oki and Sud, 1998, Earth Interactions, Vol.2, Paper 1.
REAL, PARAMETER :: planet_eq_radius    = 6378136.0

! eccentricity of planet spheroid
!         (spheroid - lines of constant latitude are circular,
!         meridional cross-section is an ellipse). From Appendix A of
!                 Oki and Sud, 1998, Earth Interactions, Vol.2, Paper 1.
REAL, PARAMETER :: eccen               = 0.08181974


! Derived parameters:
REAL :: kappa               = 287.05 / 1005.0   ! r/cp
REAL :: p_zero              = 100000.0        ! pref
REAL :: c_virtual           = 1.0 / 0.62198 - 1.0 ! 1.0/repsilon-1.0
REAL :: one_minus_epsilon   = 1.0 - 0.62198     ! 1.0 - repsilon
REAL :: grcp                = 9.80665 / 1005.0  ! g/cp
REAL, PARAMETER :: rv                  = 287.05 / 0.62198  ! r/repsilon
REAL, PARAMETER :: eccensq             = 0.00669447      ! eccen*eccen

END MODULE planet_constants_mod
#endif
