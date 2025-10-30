! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE datetime_utils_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module containing useful date/time routines that can be used in the UM
!   and standalone
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CONTAINS


LOGICAL FUNCTION is_leap_year(year)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns .TRUE. if the year of the given year is a leap year
!   Returns .FALSE. otherwise
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: year

is_leap_year = ( MOD(year, 4) == 0 .AND. MOD(year, 100) /= 0 ) .OR.            &
               ( MOD(year, 400) == 0 )

RETURN

END FUNCTION is_leap_year


INTEGER FUNCTION days_in_year(year, l_360, l_leap)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns number of days in the year based on whether it is a leap year
!   or if a 360 day calendar is in use
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: year
LOGICAL, INTENT(IN) :: l_360  ! T for 360-day calendar
                              ! F for Gregorian
LOGICAL, INTENT(IN) :: l_leap  ! T for leap years
                               ! F for no leap years


! Default number of days in year is (obviously) 365
days_in_year = 365

IF ( l_leap .AND. is_leap_year(year) ) days_in_year = 366

IF ( l_360 ) days_in_year = 360

RETURN

END FUNCTION days_in_year


INTEGER FUNCTION days_in_month(year, month, l_360, l_leap)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns the number of days in the given month for the given year
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) :: year, month
LOGICAL, INTENT(IN) :: l_360  ! T for 360-day calendar
                              ! F for Gregorian
LOGICAL, INTENT(IN) :: l_leap  ! T for leap years
                               ! F for no leap years


! If using 360 day year, every month has 30 days
IF ( l_360 ) THEN
  days_in_month = 30
  RETURN
END IF

! If using 'normal' year then we need to do different things for each month
SELECT CASE ( month )
  ! First the awkward month, Feb
CASE ( 2 )
  IF ( l_leap .AND. is_leap_year(year) ) THEN
    days_in_month = 29
  ELSE
    days_in_month = 28
  END IF
  ! Then months with 30 days (Apr, Jun, Sep, Nov)
CASE ( 4, 6, 9, 11 )
  days_in_month = 30
  ! Every other month has 31 days
CASE DEFAULT
  days_in_month = 31
END SELECT

RETURN

END FUNCTION days_in_month


INTEGER FUNCTION day_of_year(year, month, day, l_360, l_leap)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns the day of the year that corresponds to year, month and day
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: year, month, day
LOGICAL, INTENT(IN) :: l_360  ! T for 360-day calendar
                              ! F for Gregorian
LOGICAL, INTENT(IN) :: l_leap  ! T for leap years
                               ! F for no leap years

! Work variables
INTEGER :: m  ! Month counter


day_of_year = 0
m = 0
! Count through the full months worth of days we need to add
DO
  m = m + 1
  IF ( m >= month ) EXIT

  day_of_year = day_of_year + days_in_month(year, m, l_360, l_leap)
END DO

! Add the remaining days
day_of_year = day_of_year + day

RETURN

END FUNCTION day_of_year

END MODULE datetime_utils_mod
