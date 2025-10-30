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

MODULE imogen_time

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!  Define basic timestepping variables
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER ::                                                                     &
  mm,                                                                          &
            ! Number of months in year
  md,                                                                          &
            ! Number of days in (GCM) month
  step_day,                                                                    &
            ! Number of daily timesteps
            ! This is derived from the timestep
  nsdmax
            ! Maximum number of possible subdaily increments

PARAMETER(mm = 12)
            ! Number of months in year
PARAMETER(md = 30)
            ! At present hardwired as 30 days per month (as in GCM)
            ! l_360 is forced to be true at the control level
PARAMETER(nsdmax = 24)
            ! minimum timestep is 1 hour
            ! maximum number of timesteps in 1 day is 24

END MODULE imogen_time
#endif
