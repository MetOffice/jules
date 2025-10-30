#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE solpos_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE solpos(day,year,sindec,scs)

USE conversions_mod, ONLY: pi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!  Unified model deck SOLPOS, containing only routine SOLPOS.
!    This is part of logical component P233, performing the
!  calculations of the earth's orbit described in the first page of
!  the "Calculation of incoming insolation" section of UMDP 23, i.e.
!  from the day of the year (and, in forecast mode, whether it is a
!  leap year) and the orbital "constants" (which vary over
!  "Milankovitch" timescales) it calculates the sin of the solar
!  declination and the inverse-square scaling factor for the solar
!  "constant".  It is thus intrinsically scalar.  The FORTRAN code
!  present depends on whether *DEF CAL360 is set during UPDATE: this
!  replaces the Julian calendar with the climate-mode 360-day calendar
!    Written in FORTRAN 77, with the addition of "!" comments and
!  underscores in variable names.
!    Written to comply with 12/9/89 version of UMDP 4 (meteorological
!  standard).
!   Author:    William Ingram  22/3/89
!                      Reviewer: Clive Wilson Winter 1989/90
!  First version.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  day,                                                                         &
         !  Day-number in the year
  year   !  Calendar year

REAL, INTENT(OUT) ::                                                           &
  sindec,                                                                      &
         !  sin(solar declination)
  scs    !  solar constant scaling factor

! This routine has no dynamically allocated work areas and no
!  significant structure.  It calls the intrinsic functions REAL, SIN
!  & COS, but no user functions or subroutines.
!
REAL ::                                                                        &
! Basic orbital constants
      r_gamma,                                                                 &
             ! Supplement of the longitude of the perihelion
      e,                                                                       &
             ! Eccentricity of the orbit
      tau0,                                                                    &
             ! True date of perihelion
      sinobl,                                                                  &
             ! Sin (obliquity of the orbit)
! Derived orbital constants
      tau1,                                                                    &
             !
      e1,e2,                                                                   &
             ! Coefficients for 3.1.2
      e3,                                                                      &
      e4,                                                                      &
             ! Constant for 3.1.4
      twopi  ! 2 * pi

REAL :: diny      ! Number of days in the year
REAL :: m,v       ! Mean & true anomaly

!-----------------------------------------------------------------
! Set up parameters
!-----------------------------------------------------------------
PARAMETER( twopi = 2.0 * pi )
PARAMETER(                                                                     &
  r_gamma  = 1.352631,                                                         &
  e      = 0.0167,                                                             &
  tau0   = 2.5,                                                                &
  sinobl = 0.397789                                                            &
)

! Calculate the coefficients in the equation of the centre and
! thence derive the true anomaly.
PARAMETER (                                                                    &
  e1 = e * (2.0-0.25 * e * e),                                                 &
  e2 = 1.25 * e * e,                                                           &
  e3 = e * e * e * 13.0 / 12.0,                                                &
  e4 = ((1.0 + e * e * 0.5) / (1.0 - e * e))**2                                &
)

!  In climate mode, DINY=360 always, and as well as applying 3.3.1,
!  TAU1 is modified so as to include the conversion of day-ordinal into
!  fractional-number-of-days-into-the-year-at-12-Z-on-this-day.
PARAMETER(                                                                     &
  diny = 360.0,                                                                &
  tau1 = tau0 * diny / 365.25 + 0.71 + 0.5                                     &
)



m = twopi * (REAL(day) - tau1) / diny                        ! Eq 3.1.1
v = m + e1 * SIN(m) + e2 * SIN(2.0 * m) + e3 * SIN(3.0 * m)  ! Eq 3.1.2
scs = e4 * ( 1.0 + e * COS(v) )**2                           ! Eq 3.1.4

! sin(solar declination) (UMDP23 Eq 3.1.5)
! Solar declination is related to true longitude of earth (lambda) by:
!  sindec = sin(obliquity) * sin(lambda)
! Lambda is counted counterclockwise from vernal equinox
!  and is related to v (the true anomaly) through
!  lambda = v + (longitude of perihelion)
sindec = sinobl * SIN (v - r_gamma)                          ! Eq 3.1.6

RETURN

END SUBROUTINE solpos
END MODULE solpos_mod
#endif
