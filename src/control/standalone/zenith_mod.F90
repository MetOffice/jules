#if !defined(UM_JULES)
MODULE zenith_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This module contains constants and routines for calculating solar related
!   quantities (i.e. solar declination angle, solar zenith angle and
!   photoperiod
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
REAL,PARAMETER ::                                                              &
  earth_tilt  = 23.4,                                                          &
      ! Earth's axial tilt in degrees
  phase_shift = 10.0
      ! Approx number of days from December solstice to Jan 1

CONTAINS

FUNCTION solar_declination_angle() RESULT(sd)

USE conversions_mod, ONLY: pi, pi_over_180

USE datetime_utils_mod, ONLY: day_of_year, days_in_year

USE time_info_mod, ONLY: l_360, l_leap, current_model_time

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the solar declination angle for the current day assuming a
!   circular earth orbit
!
! Method:
!   Gets the current model time and works out what day of the year it is
!   before calculating the solar declination angle for that day
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
REAL :: sd

! Work variables
INTEGER :: day_of_yr, days_in_yr
INTEGER :: year, month, day

!-----------------------------------------------------------------------------

CALL current_model_time(year, month, day)
day_of_yr = day_of_year(year, month, day, l_360, l_leap)
days_in_yr = days_in_year(year, l_360, l_leap)

sd = -earth_tilt * pi_over_180 *                                               &
      COS(2.0 * pi * (REAL(day_of_yr) +  phase_shift) / REAL(days_in_yr) )

END FUNCTION solar_declination_angle


!-----------------------------------------------------------------------------


SUBROUTINE zenith(cosz)

USE conversions_mod, ONLY: pi, pi_over_180

USE conversions_mod, ONLY: rsec_per_day

USE time_info_mod, ONLY: current_model_time
USE datetime_mod,  ONLY: l_local_solar_time

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE model_grid_mod, ONLY: latitude, longitude

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the cosine of the zenith angle
!
! Method:
!   Uses the solar declination angle for the current day and the hour angle
!   to get the cosine of the zenith angle
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

REAL, INTENT(OUT) :: cosz(t_i_length, t_j_length) ! Cosine of zenith angle

! Local variables
REAL    ::  hh        ! Hour angle of sun relative to solar noon (radians)
REAL    ::  sd        ! Solar declination angle (radians)

INTEGER ::  i,j       ! Loop counters

INTEGER ::  TIME      ! Current time of day in seconds


!-----------------------------------------------------------------------------

CALL current_model_time(TIME = TIME)

sd = solar_declination_angle()

DO j = 1, t_j_length
  DO i = 1, t_i_length

    IF (l_local_solar_time) THEN
      hh = pi * ((2.0 * (REAL(TIME) / rsec_per_day)) - 1.0)
    ELSE
      hh = pi * ((2.0 * (REAL(TIME) / rsec_per_day))                           &
           + (longitude(i,j) / 180.0) - 1.0)
    END IF

    cosz(i,j) = SIN(pi_over_180 * latitude(i,j)) * SIN(sd)                     &
            + COS(pi_over_180 * latitude(i,j)) * COS(sd) * COS(hh)

    IF ( cosz(i,j) < 0.0 ) cosz(i,j) = 0.0
  END DO
END DO

END SUBROUTINE zenith


!-----------------------------------------------------------------------------


SUBROUTINE photoperiod(npoints, phot, dphotdt)

USE conversions_mod, ONLY: pi, pi_over_180

USE conversions_mod, ONLY: rhour_per_day

USE datetime_utils_mod, ONLY: day_of_year, days_in_year

USE time_info_mod, ONLY: l_360, l_leap, current_model_time

USE theta_field_sizes, ONLY: t_i_length

USE model_grid_mod, ONLY: latitude

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the photoperiod in hours and rate of change of photoperiod in
!   hours per day
!   This is required for the crop model
!
! Method:
!   Uses the solar decination angle and calculates the cosine of the hour
!   angle at sunrise/sunset and then uses this to obtain the photoperiod and
!   rate of change of photoperiod
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
INTEGER, INTENT(IN) :: npoints         ! Number of points
REAL, INTENT(OUT) :: phot(npoints)     ! Photoperiod (hours)
REAL, INTENT(OUT) :: dphotdt(npoints)  ! Rate of change of photoperiod
                                       ! in hours per day

! Local variables
REAL :: sd     ! Solar declination angle (radians)
REAL :: cosho  ! cos of hour angle at sunrise/sunset
INTEGER :: day_of_yr, days_in_yr
INTEGER :: year, month, day
INTEGER :: i, j, l

!-----------------------------------------------------------------------------

CALL current_model_time(year, month, day)
day_of_yr = day_of_year(year, month, day, l_360, l_leap)
days_in_yr = days_in_year(year, l_360, l_leap)

sd = solar_declination_angle()

DO l = 1,npoints
  j = (l-1) / t_i_length + 1
  i = l - (j-1) * t_i_length

  cosho = -TAN( latitude(i,j) * pi_over_180 ) * TAN(sd)

  IF ( cosho < -1.0 ) THEN
    phot(l)    = rhour_per_day
    dphotdt(l) = 0.0
  ELSE IF ( cosho > 1.0 ) THEN
    phot(l)    = 0.0
    dphotdt(l) = 0.0
  ELSE
    phot(l) = ACOS(cosho) * rhour_per_day / pi

    ! This expression can be found from differentiating the expression for phot
    ! with respect to time (i.e. day_of_year)
    ! The dependence of phot on day_of_year is via the solar declination angle sd
    dphotdt(l) = earth_tilt * pi_over_180 * 2.0 * rhour_per_day                &
               / REAL(days_in_yr)                                              &
               * TAN(latitude(i,j) * pi_over_180)                              &
               * SIN(2.0 * pi *                                                &
                     (REAL(day_of_yr) +  phase_shift) / REAL(days_in_yr))      &
               / MAX( ABS( SIN( ACOS(cosho) ) ), EPSILON(1.0) )                &
               / COS ( sd )** 2.0
  END IF
END DO

END SUBROUTINE photoperiod

END MODULE zenith_mod
#endif
