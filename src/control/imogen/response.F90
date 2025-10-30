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

SUBROUTINE response(ncallyr,year_run,rs)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the response function of the ocean heat uptake
!    A Greens function
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER ::                                                                     &
  i,                                                                           &
          !WORK looping parameter
  year_run,                                                                    &
          !IN Number of years in the simulation
  ncallyr
          !IN Number of calls per year to the ocean routine
          !   (with CO2 conc. fixed)

REAL ::                                                                        &
  time_rs,                                                                     &
          !IN Time delay required by the response function (yr)
  rs(ncallyr * year_run)
          !OUT Value of the response function (.)


DO i = 1,ncallyr * year_run
  time_rs = REAL(i) / REAL(ncallyr)
  IF (time_rs >= 1.0) THEN
    rs(i) = 0.014819 + 0.70367 * EXP(-time_rs / 0.70177)                       &
                     + 0.24966 * EXP(-time_rs / 2.3488)                        &
                     + 0.066485 * EXP(-time_rs / 15.281)                       &
                     + 0.038344 * EXP(-time_rs / 65.359)                       &
                     + 0.019439 * EXP(-time_rs / 347.55)
  ELSE
    rs(i) = 1.0 - 2.2617 * time_rs + 14.002 * (time_rs**2)                     &
                - 48.770 * (time_rs**3) + 82.986 * (time_rs**4)                &
                - 67.527 * (time_rs**5) + 21.037 * (time_rs**6)
  END IF
END DO

RETURN

END SUBROUTINE response
#endif
