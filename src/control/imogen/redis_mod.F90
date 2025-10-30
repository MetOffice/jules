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

MODULE redis_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE redis(                                                              &
  nsdmax,step_day,max_precip_rate,prec_loc,n_event_local,                      &
  n_tally                                                                      &
)

!-----------------------------------------------------------------------------
! Description:
!   Routine to redistribute rainfall if maximum precipitation rate is
!   exceeded.
!
!   Written by Chris Huntingford (September 2001)
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER ::                                                                     &
  step_day,                                                                    &
          !IN Calculated number of timesteps per day
  nsdmax  !IN Maximum possible number of sub-daily timesteps.

REAL ::                                                                        &
  max_precip_rate,                                                             &
          !IN Maximum allowed precip. rate allowed within
          !   each sub-daily timestep (mm/day). (This only
          !   applies when STEP_DAY >= 2)
  prec_loc(nsdmax),                                                            &
          !INOUT Temporary value of rainfall for each gridbox.
  prec_tot,                                                                    &
          !WORK Total precip during entire day (mm/day
  prec_tot_adj,                                                                &
          !WORK Temporary adjusted total (mm/day)
  prec_change,                                                                 &
          !WORK The amount of precipitation to be
          !     redistributed within day (mm/day)
  extra_per_real
          !WORK Number of extra periods of rainfall
          !     (expressed as a real number) that are required.

INTEGER ::                                                                     &
  n_event_local(nsdmax),                                                       &
          ! 1: if rains/snows during timestep period
          ! 0: otherwise
  n_tally,                                                                     &
          !IN Number of precipitation periods before
          !   redistribution.
  i,                                                                           &
          ! WORK Looping parameter
  first_period,                                                                &
          ! First period of precipitation
  last_period,                                                                 &
          ! Last period of precipitation
  extra_per_int
          ! WORK Integer value of EXTRA_PER_REAL



! Recalculate the total precipitation for the day (in units of mm)
prec_tot = 0.0
DO i = 1,step_day
  prec_tot = prec_tot + (1.0 / REAL(step_day)) * prec_loc(i)
END DO

! Now limit the possible precipitation rate
DO i = 1,step_day
  prec_loc(i) = MIN(prec_loc(i), max_precip_rate)
END DO

! Calculate the new total precipitation for the day
prec_tot_adj = 0.0
DO i = 1,step_day
  prec_tot_adj = prec_tot_adj                                                  &
               + (1.0 / REAL(step_day)) * prec_loc(i)
END DO

! Now scatter remaining amount across other periods.
! First revisit the hours where the precipitation may be placed.
! This includes initially before the rainfall event, and then
! after. Start by finding the last_period.
DO i = 1,step_day
  IF (n_event_local(i) == 1) last_period = i
END DO

first_period = last_period - n_tally + 1

prec_change = prec_tot - prec_tot_adj

! Calculate the number of extra periods required. Ideally this
! number is less than STEP_DAY-N_TALLY

! Check that only looking at case when prec_change is non-zero
IF (prec_change > 0.0) THEN

  extra_per_real = (prec_change * REAL(step_day))                              &
                 / max_precip_rate
  extra_per_int = INT(extra_per_real)

  ! First case where it is not possible to distribute all the rainfall.
  IF (extra_per_int >= step_day - n_tally) THEN
    DO i = 1,step_day
      IF (n_event_local(i) == 0) prec_loc(i) = max_precip_rate
    END DO

    ! Now fill in the time periods before and after the storm.
    ! Periods are calculated and are moving away from first_period
    ! and last_period. Options are as follows:
    !
    ! 1) No periods available before storm, hence all distributed
    ! after the storm.
    !
    ! 2) All may be accommodated before the storm and nothing after
    !
    ! 3) Then case 3, whereby some of the rain must fall after the
    ! storm period.
    !

    ! Do case (1); first_period = 1, hence all precipitation is
    ! after the storm.
  ELSE IF (first_period == 1) THEN
    DO i = last_period+1,last_period + extra_per_int
      prec_loc(i) = max_precip_rate
    END DO
    prec_loc(last_period + extra_per_int+1) =                                  &
       max_precip_rate * (extra_per_real - REAL(extra_per_int))

    ! Now do the case where all rain may be accommodated before storm.
  ELSE IF ((extra_per_int+1) <= first_period-1) THEN
    DO i = first_period - 1,first_period - extra_per_int,-1
      prec_loc(i) = max_precip_rate
    END DO
    prec_loc(first_period - extra_per_int-1) =                                 &
       max_precip_rate * (extra_per_real - REAL(extra_per_int))

    ! Now do case where some rain falls after the storm, and all periods
    ! before the sotrm it rains.
  ELSE
    DO i = first_period - 1,1,-1
      prec_loc(i) = max_precip_rate
    END DO

    DO i = last_period+1,                                                      &
           last_period + extra_per_int - (first_period-1)
      prec_loc(i) = max_precip_rate
    END DO

    prec_loc(last_period + extra_per_int - (first_period-1) + 1) =             &
       max_precip_rate * (extra_per_real - REAL(extra_per_int))
  END IF      ! End of distribution options.
END IF        ! End of check whether redistribution is required.

RETURN

END SUBROUTINE redis
END MODULE redis_mod
#endif
