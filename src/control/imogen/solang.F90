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

MODULE solang_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE solang(sindec,t,dt,sinlat,longit,k,lit,cosz)

USE conversions_mod, ONLY: pi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!  Unified model deck SOLANG, containing only routine SOLANG.
!    This is part of logical component P233, performing the
!  calculations of the earth's orbit described in the second page of
!  the "Calculation of incoming insolation" section of UMDP 23, i.e.
!  from the sin of the solar  declination, the position of each point
!  and the time limits it calculates how much sunlight, if any, it
!  receives.
!    Written in FORTRAN 77 with the addition of "!" comments and
!  underscores in variable names.
!    Written to comply with 12/9/89 version of UMDP 4 (meteorological
!  standard).
!    Author:    William Ingram  21/3/89
!                      Reviewer: Clive Wilson Winter 1989/90
!  First version.
! A01_2A selected for SCM use equivalent to frozen version
! physics 1/3/93 J.Lean
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  k     ! Number of points

REAL, INTENT(IN) ::                                                            &
  sindec,                                                                      &
        ! Sin(solar declination)
  t,dt,                                                                        &
        ! Start time (UTC) & timestep
  sinlat(k),longit(k)
        ! sin(latitude) & longitude of each point

REAL, INTENT(OUT) ::                                                           &
  lit(k),                                                                      &
        ! Sunlit fraction of the timestep
  cosz(k)
        ! Mean cos(solar zenith angle) during the sunlit
        ! fraction

! This routine has no dynamically allocated work areas.  It calls the
! intrinsic functions SQRT, ACOS & SIN, but no user functions or
! subroutines.  The only structure is a loop over all the points to be
! dealt with, with IF blocks nested inside to cover the various
! possibilities.

INTEGER :: j  ! Loop counter over points

REAL ::                                                                        &
  twopi,                                                                       &
        ! 2*pi
  s2r,                                                                         &
        ! Seconds-to-radians converter
  sinsin,coscos,                                                               &
        ! Products of the sines and of the cosines
        ! of solar declination and of latitude.
  hld,coshld,                                                                  &
        ! Half-length of the day in radians (equal to the
        ! hour-angle of sunset, and minus the hour-angle of
        ! sunrise) & its cosine.
  hat,                                                                         &
        ! Local hour angle at the start time.
  omegab,omegae,omega1,omega2,omegas,                                          &
        ! Beginning and end of the timestep and of the
        ! period over which cosz is integrated, and sunset
        ! - all measured in radians after local sunrise,
        ! not from local noon as the true hour angle is.
  difsin,diftim,                                                               &
        ! A difference-of-sines intermediate value and the
        ! corresponding time period
  trad, dtrad
        ! These are the start-time and length of the
        ! timestep (T & DT) converted to radians after
        ! midday UTC, or equivalently, hour angle of the
        ! sun on the Greenwich meridian.

!-----------------------------------------------------------------
! Set up parameter values
!-----------------------------------------------------------------
PARAMETER( twopi = 2.0 * pi, s2r = pi / 43200.0)



trad = t * s2r - pi
dtrad = dt * s2r

!DIR$ IVDEP
DO j = 1,k                          ! Loop over points

  ! Logically unnecessary statement without which the
  ! CRAY compiler will not vectorize this code
  hld = 0.0

  sinsin = sindec * sinlat(j)
  coscos = SQRT( (1.0 - sindec**2) * (1.0 - sinlat(j)**2) )
  coshld = sinsin / coscos
  IF (coshld < -1.0) THEN             ! Perpetual night
    lit(j)  = 0.0
    cosz(j) = 0.0
  ELSE
    hat = longit(j) + trad           ! (3.2.2)
    IF (coshld > 1.0) THEN            !   Perpetual day - hour
      omega1 = hat                   ! angles for (3.2.3) are
      omega2 = hat + dtrad           ! start & end of timestep
    ELSE                                !   At this latitude some
      ! points are sunlit, some not.  Different ones need different treatment.
      hld = ACOS(-coshld)               ! (3.2.4)
      ! The logic seems simplest if one takes all "times" - actually hour
      ! angles - relative to sunrise (or sunset), but they must be kept in the
      ! range 0 to 2pi for the tests on their orders to work.
      omegab = hat + hld
      IF (omegab < 0.0)    omegab = omegab + twopi
      IF (omegab >= twopi) omegab = omegab - twopi
      IF (omegab >= twopi) omegab = omegab - twopi
      !            !  Line repeated - otherwise could have failure if
      !            !  longitudes W are > pi rather than < 0.
      omegae = omegab + dtrad
      IF (omegae > twopi)  omegae = omegae - twopi
      omegas = 2.0 * hld

      ! Now that the start-time, end-time and sunset are set in terms of hour
      ! angle, can set the two hour-angles for (3.2.3).  The simple cases are
      ! start-to-end-of-timestep, start-to-sunset, sunrise-to-end and sunrise-
      ! -to-sunset, but two other cases exist and need special treatment.
      IF (omegab <= omegas .OR. omegab < omegae) THEN
        omega1 = omegab - hld
      ELSE
        omega1 = - hld
      END IF

      IF (omegae <= omegas) THEN
        omega2 = omegae - hld
      ELSE
        omega2 = omegas - hld
      END IF

      !  Put in an arbitrary marker for the case when the sun does not rise
      !  during the timestep (though it is up elsewhere at this latitude).
      !  (Cannot set COSZ & LIT within the ELSE ( COSHLD < 1 ) block
      !  because 3.2.3 is done outside this block.)
      IF (omegae > omegab .AND. omegab > omegas)                               &
        omega2 = omega1

    END IF           ! This finishes the ELSE (perpetual day) block

    difsin = SIN(omega2) - SIN(omega1)             ! Begin (3.2.3)
    diftim = omega2 - omega1

    ! Next, deal with the case where the sun sets and then rises again
    ! within the timestep.  There the integration has actually been done
    ! backwards over the night, and the resulting negative DIFSIN and DIFTIM
    ! must be combined with positive values representing the whole of the
    ! timestep to get the right answer, which combines contributions from
    ! the two separate daylit periods.  A simple analytic expression for the
    ! total sun throughout the day is used.  (This could of course be used
    ! alone at points where the sun rises and then sets within the timestep)
    IF (diftim < 0.0) THEN
      difsin = difsin + 2.0 * SQRT(1.0 - coshld**2)
      diftim = diftim + 2.0 * hld
    END IF

    IF (diftim == 0.0) THEN
      ! Pick up the arbitrary marker for night points at a partly-lit latitude
      cosz(j) = 0.0
      lit(j)  = 0.0
    ELSE
      cosz(j) = difsin * coscos / diftim + sinsin     ! (3.2.3)
      lit(j) = diftim / dtrad
    END IF
  END IF            ! This finishes the ELSE (perpetual night) block
END DO

RETURN

END SUBROUTINE solang
END MODULE solang_mod
#endif
