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

MODULE sunny_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE sunny(                                                              &
  daynumber,jday,points,year,lat,lon,sun,time_max                              &
)

USE solpos_mod, ONLY: solpos
USE solang_mod, ONLY: solang

USE conversions_mod, ONLY: pi, pi_over_180, rhour_per_day

USE datetime_mod, ONLY: secs_in_day

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Routine to calculate the normalised solar radiation at each time and
!   the time of the daily maximum temperature (UTC).
!
!   Written by Peter Cox (March 1996)
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER ::                                                                     &
  daynumber,                                                                   &
            ! IN Day of the year.
  jday,                                                                        &
            ! IN Number of timesteps in the day.
  points,                                                                      &
            ! IN Number of spatial points.
  year      ! IN Calender year.

REAL ::                                                                        &
  lat(points),                                                                 &
            ! IN Latitude (degrees).
  lon(points),                                                                 &
            ! IN Longitude (degrees).
  sun(points,jday),                                                            &
            ! OUT Normalised solar radiation at each time.
  time_max(points),                                                            &
            ! OUT Time (UTC) at which temperature is maximum (hrs).
  cosdec,                                                                      &
            ! WORK COS (solar declination).
  coslat,                                                                      &
            ! WORK COS (latitude).
  cosz(points),                                                                &
            ! WORK Timestep mean COSZ.
  coszm(points),                                                               &
            ! WORK Daily mean COSZ.
  latrad,                                                                      &
            ! WORK Latitude (radians).
  lit(points),                                                                 &
            ! WORK Sunlit fraction of the day
  lonrad(points),                                                              &
            ! WORK Longitude (radians).
  scs,                                                                         &
            ! WORK Factor for TOA solar.
  sindec,                                                                      &
            ! WORK SIN (solar declination).
  sinlat(points),                                                              &
            ! WORK SIN (latitude).
  tandec,                                                                      &
            ! WORK TAN (solar declination).
  tanlat,                                                                      &
            ! WORK TAN (latitude).
  tantan,                                                                      &
            ! WORK TANDEC*TANLAT.
  omega_up,omega_down,                                                         &
            ! WORK Solar angle of sunrise and sunset (radians).
  time_up,time_down,                                                           &
            ! WORK Time (UTC) of sunrise and sunset (hrs).
  timestep,                                                                    &
            ! WORK Timestep (s).
  timeday

INTEGER :: i,j    ! WORK Loop counter.


CALL solpos (daynumber, year, sindec, scs)

DO i = 1,points
  latrad     = pi_over_180 * lat(i)
  lonrad(i)  = pi_over_180 * lon(i)
  sinlat(i)  = SIN(latrad)
  coszm(i)   = 0.0
END DO

cosdec   = SQRT(1 - sindec**2)
tandec   = sindec / cosdec
timestep = REAL(secs_in_day) / REAL(jday)

!----------------------------------------------------------------------
! Calculate the COSZ at each time
!----------------------------------------------------------------------
DO j = 1,jday

  timeday = (j-1) * timestep
  CALL solang(                                                                 &
    sindec,timeday,timestep,sinlat,lonrad,points,lit,cosz                      &
  )

  DO i = 1,points
    sun(i,j) = cosz(i) * lit(i)
    coszm(i) = coszm(i) + sun(i,j) / REAL(jday)
  END DO

END DO

!----------------------------------------------------------------------
! Calculate the normalised solar radiation
!----------------------------------------------------------------------
DO j = 1,jday
  DO i = 1,points

    IF (coszm(i) > EPSILON(1.0)) THEN
      sun(i,j) = sun(i,j) / coszm(i)
    ELSE
      sun(i,j) = 0.0
    END IF

  END DO
END DO

!----------------------------------------------------------------------
! Calculate the time of maximum temperature. Assume this occurs 0.15
! of the daylength after local noon (guess !).
!----------------------------------------------------------------------
DO i = 1,points

  coslat = SQRT(1 - sinlat(i)**2)
  tanlat = sinlat(i) / coslat
  tantan = tanlat * tandec

  IF (ABS(tantan) <= 1.0) THEN      ! Sun sets and rises

    omega_up   = -ACOS(-tantan)
    time_up    = 0.5 * rhour_per_day                                           &
               * ((omega_up - lonrad(i)) / pi + 1.0)
    omega_down = ACOS(-tantan)
    time_down  = 0.5 * rhour_per_day                                           &
               * ((omega_down - lonrad(i)) / pi + 1.0)

  ELSE                             ! Perpertual day or night
    time_up   = 0.0
    time_down = 0.0
  END IF

  time_max(i) = 0.5 * (time_up + time_down)                                    &
              + 0.15 * (time_down - time_up)

END DO

RETURN

END SUBROUTINE sunny
END MODULE sunny_mod
#endif
