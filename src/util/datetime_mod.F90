#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE datetime_mod

USE conversions_mod, ONLY:                                                     &
  secs_in_min => isec_per_min,   mins_in_hour => imin_per_hour,                &
  hours_in_day => ihour_per_day, secs_in_hour => isec_per_hour,                &
  secs_in_day => isec_per_day

USE logging_mod, ONLY: log_info, log_debug, log_warn, log_error, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module containing type definitions and routines for manipulating datetime
!   objects
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
! 'Special' periods.
! Note that although days are considered to be of equal length (unlike months
! or years) and as such don't need to use a "special period", the use of a
! special period allows the use of time templating.
INTEGER, PARAMETER ::                                                          &
  period_day   = -3,                                                           &
  period_month = -1,                                                           &
  period_year  = -2

INTEGER, PARAMETER ::                                                          &
  datetime_str_len = 19

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_360 = .FALSE.,                                                             &
                     ! .TRUE. to use 360 day year
                     ! .FALSE. to use 'normal' year
  l_leap = .TRUE.,                                                             &
                     ! .TRUE. to use leap years
                     ! .FALSE. to ignore leap years
                     ! This is not used if l_360 = T
  l_local_solar_time = .FALSE.
                     ! Assume time throughout the model and in the
                     ! driving data is actually local solar time,
                     ! disregarding any attributes of the driving data


!-----------------------------------------------------------------------------
! Type definitions
!-----------------------------------------------------------------------------
TYPE :: datetime

  INTEGER :: year, month, day  ! Integer day of month, month and year

  INTEGER :: TIME  ! Number of seconds into the day

END TYPE datetime

!-----------------------------------------------------------------------------
! Overload the comparison operators for dates and times
!-----------------------------------------------------------------------------
INTERFACE OPERATOR ( == )
  MODULE PROCEDURE datetime_eq
END INTERFACE

INTERFACE OPERATOR ( /= )
  MODULE PROCEDURE datetime_ne
END INTERFACE

INTERFACE OPERATOR ( < )
  MODULE PROCEDURE datetime_lt
END INTERFACE

INTERFACE OPERATOR ( > )
  MODULE PROCEDURE datetime_gt
END INTERFACE

INTERFACE OPERATOR ( <= )
  MODULE PROCEDURE datetime_le
END INTERFACE

INTERFACE OPERATOR ( >= )
  MODULE PROCEDURE datetime_ge
END INTERFACE


CONTAINS

!#############################################################################

FUNCTION datetime_create(year, month, day, hour, minute, sec) RESULT(dt)

USE string_utils_mod, ONLY: to_string

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function creates a new datetime object and checks it for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: year, month, day, hour, minute, sec

! Return type
TYPE(datetime) :: dt


dt%year  = year
dt%month = month
dt%day   = day
dt%TIME  = (hour * secs_in_hour) + (minute * secs_in_min) + sec

IF ( .NOT. datetime_is_valid(dt) ) THEN
  CALL log_fatal("datetime_create",                                            &
                 "year="     // TRIM(to_string(year))   // ", " //             &
                 "month="    // TRIM(to_string(month))  // ", " //             &
                 "day="      // TRIM(to_string(day))    // ", "  //            &
                 "hour="     // TRIM(to_string(hour))   // ", "  //            &
                 "minute="   // TRIM(to_string(minute)) // ", "  //            &
                 "sec="      // TRIM(to_string(sec))    // " "  //             &
                 "is not a valid date")
END IF

RETURN

END FUNCTION datetime_create

!#############################################################################

TYPE(datetime) FUNCTION datetime_clone(dt)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function creates a new datetime object from an existing one
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(datetime) :: dt


datetime_clone%year  = dt%year
datetime_clone%month = dt%month
datetime_clone%day   = dt%day
datetime_clone%TIME  = dt%TIME

END FUNCTION datetime_clone

!#############################################################################

FUNCTION datetime_is_valid(dt) RESULT(is_valid)

USE datetime_utils_mod, ONLY: days_in_month

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function determines if a datetime represents a real (valid) date
!   and time
!
! Method:
!   Checks components of a datetime are consistent
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

TYPE(datetime), INTENT(IN) :: dt  ! The datetime to check for validity

LOGICAL :: is_valid   ! RETURN VALUE
                      ! .TRUE. if dt is a valid datetime,
                      ! .FALSE. otherwise


is_valid = .TRUE.

! We let any year go, so validation starts with month
IF ( dt%month < 1 .OR. dt%month > 12 ) THEN
  is_valid = .FALSE.
  RETURN
END IF

IF ( dt%day < 1 .OR.                                                           &
     dt%day > days_in_month(dt%year, dt%month, l_360, l_leap) ) THEN
  is_valid = .FALSE.
  RETURN
END IF

is_valid = ( 0 <= dt%TIME .AND. dt%TIME < secs_in_day )

RETURN

END FUNCTION datetime_is_valid

!#############################################################################

FUNCTION datetime_advance(dt, period) RESULT(new_dt)

USE datetime_utils_mod, ONLY: days_in_month

USE string_utils_mod, ONLY: to_string

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function returns a new datetime that is period seconds ahead of the
!   given datetime
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Types for arguments
TYPE(datetime), INTENT(IN) :: dt ! The datetime to advance from

INTEGER, INTENT(IN) :: period ! The number of seconds to advance by, or a
                              ! 'special period'

! Return type
TYPE(datetime) :: new_dt ! The advanced datetime

! Work variables
INTEGER :: year, month, day, TIME
INTEGER :: month_days


! Initialise variables using the passed in datetime
year  = dt%year
month = dt%month
day   = dt%day
TIME  = dt%TIME

! Special periods can be handled differently for efficiency, rather than
! calculating the number of seconds to advance and using the normal
! algorithm
SELECT CASE ( period )
CASE ( 0: )
  ! The 'normal' case - period given in seconds

  ! First just advance the time by the required amount
  TIME = TIME + period

  IF ( TIME >= secs_in_day ) THEN
    ! Advance the day as required, and peg the time back into a valid
    ! range. Here we use the fact that integer division truncates the
    ! fractional part.
    day = day + ( TIME / secs_in_day )
    TIME = MOD(TIME, secs_in_day)

    IF ( day /= dt%day ) THEN
      ! If the day has changed, check if we need to update the month
      ! Advance the month and year until day is within the valid range
      DO
        month_days = days_in_month(year, month, l_360, l_leap)

        ! If day is in the valid range for the current year and month
        ! then there is nothing left to do.
        IF ( day <= month_days ) EXIT

        ! If day is bigger than the valid range for the current month,
        ! then advance the month and knock the appropriate number
        ! of days off
        month = month + 1
        day   = day - month_days

        ! We might now need to adjust the year
        IF ( month > 12 ) THEN
          year  = year + 1
          month = 1
        END IF
      END DO
    END IF  ! time > secs_in_day
  END IF  ! day changed

CASE ( period_day )
  ! Advance the day by 1 and check if we need to advance month and year.
  day = day + 1
  month_days = days_in_month(year, month, l_360, l_leap)
  ! If day is in the valid range for the current year and month, there is nothing
  ! left to do.
  IF ( day > month_days ) THEN
    ! Day is bigger than the valid range for the current month; advance the month
    ! and set the day to 1.
    month = month + 1
    day   = 1
    ! We might now need to adjust the year.
    IF ( month > 12 ) THEN
      year  = year + 1
      month = 1
    END IF
  END IF  !  day > month_days

CASE ( period_month )
  ! Advance the month by one and check if we need to advance the year
  month = month + 1
  IF ( month > 12 ) THEN
    year  = year + 1
    month = 1
  END IF

  ! If we are past the end of the month we just moved in to, set the day
  ! to the last day in the month
  day = MIN(day, days_in_month(year, month, l_360, l_leap))

CASE ( period_year )
  ! Advance the year by one
  year  = year + 1

  ! If we are past the end of the month we just moved in to, set the day
  ! to the last day in the month (this will only happen when advancing
  ! a year from 29th Feb in a leap year).
  IF ( .NOT. l_360 .AND. l_leap .AND. month == 2 .AND. day == 29 ) THEN
    day = 28
  END IF

CASE DEFAULT
  CALL log_fatal("datetime_advance",                                           &
                 "Unsupported period = " // TRIM(to_string(period)))

END SELECT

! Copy variables into return value
new_dt%year  = year
new_dt%month = month
new_dt%day   = day
new_dt%TIME  = TIME

RETURN

END FUNCTION datetime_advance

!#############################################################################

FUNCTION datetime_subtract(dt, period) RESULT(new_dt)

USE datetime_utils_mod, ONLY: days_in_month

USE string_utils_mod, ONLY: to_string

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function returns a new datetime that is period seconds behind the
!   given datetime
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Types for arguments
TYPE(datetime), INTENT(IN) :: dt ! The datetime to subtract from

INTEGER, INTENT(IN) :: period ! The number of seconds to subtract, or a
                              ! 'special period'

! Return type
TYPE(datetime) :: new_dt ! The new datetime

! Work variables
INTEGER :: year, month, day, TIME
INTEGER :: period_local  ! A modifiable local version of period


! Initialise variables using the passed in datetime
year  = dt%year
month = dt%month
day   = dt%day
TIME  = dt%TIME

period_local = period

! Special periods can be handled differently for efficiency, rather than
! calculating the number of seconds to subtract and using the normal
! algorithm
SELECT CASE ( period_local )
CASE ( 0: )
  ! The 'normal' case - period given in seconds
  IF ( period_local <= TIME ) THEN
    TIME = TIME - period_local
  ELSE
    period_local = period_local - TIME

    TIME = MOD(secs_in_day - MOD(period_local, secs_in_day),                   &
               secs_in_day)

    ! Due to the behaviour of the integer division, we need to treat any
    ! periods that mean we end up at midnight specially.
    IF ( TIME == 0 ) THEN
      day = day - (period_local / secs_in_day)
    ELSE
      day = day - 1 - (period_local / secs_in_day)
    END IF

    DO
      IF ( day > 0 ) EXIT

      ! We know that we have to move backwards a month if we get to here
      month = month - 1
      IF ( month < 1 ) THEN
        year = year - 1
        month = 12
      END IF

      day = days_in_month(year, month, l_360, l_leap) + day
    END DO
  END IF

CASE ( period_day )
  ! Reduce the day by one and check if we need to reduce the month and year.
  day = day - 1
  IF ( day < 1 ) THEN
    ! Move to the last day of the previous month.
    month = month - 1
    IF ( month < 1 ) THEN
      year = year - 1
      month = 12
    END IF
    day = days_in_month(year, month, l_360, l_leap)
  END IF  !  day < 1

CASE ( period_month )
  ! Reduce the month by one and check if we need to reduce the year
  month = month - 1
  IF ( month < 1 ) THEN
    year  = year - 1
    month = 12
  END IF

  ! If we are past the end of the month we just moved in to, set the day
  ! to the last day in the month.
  day = MIN(day, days_in_month(year, month, l_360, l_leap))

CASE ( period_year )
  ! Reduce the year by one
  year  = year - 1

  ! If we are past the end of the month we just moved in to, set the day
  ! to the last day in the month (this will only happen when subtracting
  ! a year from 29th Feb in a leap year).
  IF ( .NOT. l_360 .AND. l_leap .AND. month == 2 .AND. day == 29 ) THEN
    day = 28
  END IF

CASE DEFAULT
  CALL log_fatal("datetime_subtract",                                          &
                 "Unsupported period = " // TRIM(to_string(period)))

END SELECT

! Copy variables into return value
new_dt%year  = year
new_dt%month = month
new_dt%day   = day
new_dt%TIME  = TIME

RETURN

END FUNCTION datetime_subtract

!#############################################################################

FUNCTION datetime_diff(dt1_arg, dt2_arg) RESULT(diff)

USE precision_mod, ONLY: int_64

USE datetime_utils_mod, ONLY: days_in_year, day_of_year

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function returns the number of seconds difference between dt1 and dt2
!   It is always positive
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Types for arguments
TYPE(datetime), INTENT(IN) :: dt1_arg, dt2_arg

! Return type
INTEGER(KIND=int_64) :: diff

! Work variables
TYPE(datetime) :: dt1, dt2  ! The two datetime objects to get the
                            ! difference between
                            ! If necessary, these are swapped, so that
                            ! dt1 <= dt2 at all times

INTEGER :: i  ! Loop counter

! Make sure dt1 <= dt2
IF ( dt1_arg <= dt2_arg) THEN
  dt1 = dt1_arg
  dt2 = dt2_arg
ELSE
  dt1 = dt2_arg
  dt2 = dt1_arg
END IF

IF ( dt1%year == dt2%year ) THEN
  IF ( dt1%month == dt2%month ) THEN
    IF ( dt1%day == dt2%day ) THEN
      ! If dates are equal, just diff the times
      diff = dt2%TIME - dt1%TIME
      RETURN
    END IF

    ! If months are equal, we need to factor in the days
    diff = ((dt2%day-1) * secs_in_day + dt2%TIME)                              &
         - ((dt1%day-1) * secs_in_day + dt1%TIME)
    RETURN
  END IF

  ! If only years are equal, we need to factor in months and days
  diff = ((day_of_year(dt2%year, dt2%month, dt2%day, l_360, l_leap) - 1)       &
                                                            * secs_in_day      &
          + dt2%TIME)                                                          &
       - ((day_of_year(dt1%year, dt1%month, dt1%day, l_360, l_leap) - 1)       &
                                                            * secs_in_day      &
          + dt1%TIME)
  RETURN
END IF

! If the dates are in different years, we do something different
! Intialise the difference as the number of seconds into its year that
! dt2 is.
diff = (day_of_year(dt2%year, dt2%month, dt2%day, l_360, l_leap) - 1)          &
                                                            * secs_in_day      &
     + dt2%TIME                                                                &
! Add on the number of seconds from dt1 until the end of it's year
! We do this by first adding on the number of seconds in the days up to the
! end of the year
     + (days_in_year(dt1%year, l_360, l_leap) -                                &
        day_of_year(dt1%year, dt1%month, dt1%day, l_360, l_leap))              &
                                                          * secs_in_day        &
! Then add on the number of seconds until the end of that day
     + secs_in_day - dt1%TIME

! Then add on a years worth of seconds for each whole year between the
! dates.
DO i = (dt1%year+1),(dt2%year-1)
  diff = diff + days_in_year(i, l_360, l_leap) * secs_in_day
END DO

RETURN

END FUNCTION datetime_diff


!#############################################################################

! ****************************************************************************
! Implementation of operators for datetime type
! ****************************************************************************
LOGICAL FUNCTION datetime_eq(dt, other_dt)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function determines if two datetimes are equal
!
! Method:
!   Compares components of two datetimes to determine if they are equal
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

TYPE(datetime), INTENT(IN) :: dt, other_dt


datetime_eq = ( dt%year  == other_dt%year )   .AND.                            &
              ( dt%month == other_dt%month )  .AND.                            &
              ( dt%day   == other_dt%day )    .AND.                            &
              ( dt%TIME  == other_dt%TIME )

RETURN

END FUNCTION datetime_eq

!#############################################################################

LOGICAL FUNCTION datetime_ne(dt, other_dt)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function determines if two datetimes are not equal
!
! Method:
!   Compares components of two datetimes to determine if they are not equal
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

TYPE(datetime), INTENT(IN) :: dt, other_dt


datetime_ne = ( dt%year  /= other_dt%year )   .OR.                             &
              ( dt%month /= other_dt%month )  .OR.                             &
              ( dt%day   /= other_dt%day )    .OR.                             &
              ( dt%TIME  /= other_dt%TIME )

RETURN

END FUNCTION datetime_ne

!#############################################################################

LOGICAL FUNCTION datetime_lt(dt, other_dt)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function determines if one datetime is less than another
!
! Method:
!   Compares components of two datetimes to determine if dt represents a
!   time strictly before other_dt
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

TYPE(datetime), INTENT(IN) :: dt, other_dt


datetime_lt = .TRUE.

IF ( dt%year < other_dt%year ) RETURN
IF ( dt%year > other_dt%year ) THEN
  datetime_lt = .FALSE.
  RETURN
END IF

! Years are equal so check month
IF ( dt%month < other_dt%month ) RETURN
IF ( dt%month > other_dt%month ) THEN
  datetime_lt = .FALSE.
  RETURN
END IF

! Years and months are equal, so check day
IF ( dt%day < other_dt%day ) RETURN
IF ( dt%day > other_dt%day ) THEN
  datetime_lt = .FALSE.
  RETURN
END IF

! Dates are equal, so just check time
datetime_lt = dt%TIME < other_dt%TIME

RETURN

END FUNCTION datetime_lt

!#############################################################################

LOGICAL FUNCTION datetime_le(dt, other_dt)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function determines if one datetime is less than or equal to another
!
! Method:
!   Compares components of two datetimes to determine if dt represents a
!   time before or equal to other_dt
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

TYPE(datetime), INTENT(IN) :: dt, other_dt


datetime_le = datetime_lt(dt, other_dt) .OR. datetime_eq(dt, other_dt)

RETURN

END FUNCTION datetime_le

!#############################################################################

LOGICAL FUNCTION datetime_gt(dt, other_dt)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function determines if one datetime is greater than another
!
! Method:
!   Compares components of two datetimes to determine if dt represents a
!   time strictly after other_dt
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

TYPE(datetime), INTENT(IN) :: dt, other_dt


datetime_gt = .NOT. datetime_le(dt, other_dt)

RETURN

END FUNCTION datetime_gt

!#############################################################################

LOGICAL FUNCTION datetime_ge(dt, other_dt)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This function determines if one datetime is greater than or equal to
!   another
!
! Method:
!   Compares components of two datetimes to determine if dt represents a
!   time equal to or after other_dt
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

TYPE(datetime), INTENT(IN) :: dt, other_dt


datetime_ge = .NOT. datetime_lt(dt, other_dt)

RETURN

END FUNCTION datetime_ge

!#############################################################################

! ****************************************************************************
! Functions for the marshalling of datetimes to and from strings
! ****************************************************************************
FUNCTION datetime_to_string(dt) RESULT(dt_string)

IMPLICIT NONE

TYPE(datetime), INTENT(IN) :: dt

CHARACTER(LEN=datetime_str_len) :: dt_string ! Return type

CHARACTER(LEN=10) :: date_string
CHARACTER(LEN=8)  :: time_string

INTEGER :: year, month, day, hour, minute, sec


year  = dt%year
month = dt%month
day   = dt%day
! These calculations use the fact that fractional part is truncated by
! integer division.
hour   = dt%TIME / secs_in_hour
minute = MOD(dt%TIME, secs_in_hour) / secs_in_min
sec = MOD(dt%TIME, secs_in_min)

WRITE(date_string, '(I4.4, A, I2.2, A, I2.2)') year, '-', month, '-', day

WRITE(time_string, '(I2.2, A, I2.2, A, I2.2)') hour, ':', minute, ':',         &
                                               sec

dt_string = date_string // ' ' // time_string

RETURN

END FUNCTION datetime_to_string

!#############################################################################

FUNCTION datetime_from_string(dt_string) RESULT(dt)

IMPLICIT NONE

CHARACTER(LEN=*), INTENT(IN) :: dt_string

TYPE(datetime) :: dt ! Return type

! Work variables
INTEGER :: year, month, day, hour, minute, sec
INTEGER :: ERROR

!-----------------------------------------------------------------------------

    ! This function expects datetime strings of the form yyyy-mm-dd hh:mm:ss
READ(dt_string,                                                                &
     '(I4.4, 1X, I2.2, 1X, I2.2, 1X, I2.2, 1X, I2.2, 1X, I2.2)',               &
     IOSTAT = ERROR) year, month, day, hour, minute, sec

! If there was an error extracting the values, log an error
IF ( ERROR /= 0 ) THEN
  CALL log_fatal("datetime_from_string",                                       &
                 "'" // TRIM(dt_string) // "' is not a valid datetime")
END IF

dt = datetime_create(year, month, day, hour, minute, sec)

RETURN

END FUNCTION datetime_from_string

!#############################################################################

END MODULE datetime_mod
#endif
