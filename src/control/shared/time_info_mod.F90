! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE time_info_mod

! Information about the 360 day year comes from different places in the UM
! and standalone
! We unify them and expose them as a variable in this module
#if defined(UM_JULES)
USE nlstcall_mod, ONLY: l_360 => lcal360
#else
USE datetime_mod, ONLY: l_360, l_leap, l_local_solar_time
#endif

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module for unifying access to date / time information in the UM and
!   standalone
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

#if defined(UM_JULES)
LOGICAL :: l_leap = .TRUE.
  ! In the UM, we always have leap years with 365 day calendar
LOGICAL :: l_local_solar_time = .FALSE.
  ! In the UM, time is always UTC.
#endif


CONTAINS


SUBROUTINE current_model_time(year, month, day, TIME)

! Note: Standalone JULES and the UM define their model time information in
! modules which have the same name (their content is different) - include the
! appropriate module here
#if defined(UM_JULES)
USE model_time_mod, ONLY: i_year, i_month, i_day, i_hour, i_minute,            &
                           i_second
USE conversions_mod, ONLY: isec_per_hour, isec_per_min
#else
USE model_time_mod, ONLY: current_time
#endif

IMPLICIT NONE

INTEGER, OPTIONAL, INTENT(OUT) :: year, month, day, TIME
    ! The time components as integers

!-----------------------------------------------------------------------------
! Description:
!   Gets the current model time as integer parts
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

#if defined(UM_JULES)
IF ( PRESENT(year) ) year = i_year
IF ( PRESENT(month) ) month = i_month
IF ( PRESENT(day) ) day = i_day
IF ( PRESENT(TIME) ) TIME = isec_per_hour * i_hour                             &
                            + isec_per_min * i_minute + i_second
#else
IF ( PRESENT(year) ) year = current_time%year
IF ( PRESENT(month) ) month = current_time%month
IF ( PRESENT(day) ) day = current_time%day
IF ( PRESENT(TIME) ) TIME = current_time%TIME
#endif

END SUBROUTINE current_model_time

END MODULE time_info_mod
