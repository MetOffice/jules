#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  Global standard conversions

MODULE conversions_mod

! Description:
!       Model and code section invariant physical constants

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Top Level

! Code Description:
!   Language: FORTRAN 95
!   This code is written to UMDP3 v8 programming standards.

! Stripped down version for standalone code

IMPLICIT NONE

REAL,    PARAMETER :: rsec_per_day        = 86400.0
                        ! Number of seconds in a day (24 hours).
INTEGER, PARAMETER :: isec_per_day        = 86400
                        ! Number of seconds in a day (24 hours).
REAL,    PARAMETER :: rsec_per_hour       = 3600.0
                        ! Number of seconds in an hour.
INTEGER, PARAMETER :: isec_per_hour       = 3600
                        ! Number of seconds in an hour.
INTEGER, PARAMETER :: isec_per_min        = 60
                        ! Number of seconds in a minute.
REAL,    PARAMETER :: rhour_per_day       = 24.0
                        ! Number of hours per day.
INTEGER, PARAMETER :: ihour_per_day       = 24
                        ! Number of hours per day.
REAL,    PARAMETER :: rhour_per_sec       = 1.0 / rsec_per_hour
                        ! Reciprocal of seconds in an hour.

! imin_per_hour is NOT in the UM version of this module.
INTEGER, PARAMETER :: imin_per_hour = 60
                        ! Number of minutes in an hour.

REAL, PARAMETER :: pi                  = 3.14159265358979323846

!Conversion factor degrees to radians
REAL, PARAMETER :: pi_over_180         = pi / 180.0
!Conversion factor radians to degrees
REAL, PARAMETER :: recip_pi_over_180   = 180.0 / pi


! zerodegc is a conversion between degrees centigrade and kelvin
REAL, PARAMETER :: zerodegc            = 273.15

!Conversion from metres/second to kilometers/hour
REAL, PARAMETER ::  mps_to_kph = (60.0 * 60.0) / 1000.0 !m/s to km/h

! mm_2_m is NOT in the UM version of this module.
! Conversion mm to m
REAL, PARAMETER :: mm_2_m = 1000.0

! below are used in imogen
! Converts global emission of C (Gt) to change in atmospheric CO2 (ppm)
REAL, PARAMETER :: conv_gtc_to_ppm = 0.471

! converts ppm to ppb multiplier
REAL, PARAMETER :: ppm_to_ppb = 1.0e3

! converts Pg to kg multiplier
REAL, PARAMETER :: pg_to_kg = 1.0e12

! converts ppm to mmr multiplier (g/g)
! mol mass co2 / mol mass dry air * 1e-6
REAL, PARAMETER :: ppm_to_mmr = 44.0 / 28.97 * 1.0e-6

! Ocean area (m2)
REAL, PARAMETER :: ocean_area = 3.627e14


END MODULE conversions_mod
#endif
