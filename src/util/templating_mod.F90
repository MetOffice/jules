#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE templating_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
! Strings used to indicate templated parts of strings
! These are replaced by functions in this module to create other strings
CHARACTER(LEN=3), PARAMETER ::                                                 &
  tpl_yr_2digit  = '%y2',                                                      &
             !  2-digit year
  tpl_yr_4digit  = '%y4',                                                      &
             !  4-digit year
  tpl_mon        = '%m1',                                                      &
             !  1- or 2-digit month
  tpl_mon_2digit = '%m2',                                                      &
             !  2-digit month
  tpl_mon_abbr   = '%mc',                                                      &
             !  3-character month abbreviation
  tpl_day_2digit   = '%d2',                                                    &
             !  2-digit day of month
  tpl_var_name   = '%vv'
             !  Name of a variable

CHARACTER(LEN=3) :: month_abbreviations(12)

DATA month_abbreviations / 'jan', 'feb', 'mar', 'apr', 'may', 'jun',           &
                           'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /


CONTAINS

! ****************************************************************************
! Variable name templating
! ****************************************************************************

LOGICAL FUNCTION tpl_has_var_name(tpl)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks if a template includes variable name templating
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: tpl  ! The string to check for variable
                                     ! name templating

!-----------------------------------------------------------------------------

! We just check for the variable name templating string in the given template
tpl_has_var_name = ( INDEX(tpl, tpl_var_name) > 0 )

RETURN

END FUNCTION tpl_has_var_name

!##############################################################################

FUNCTION tpl_substitute_var(tpl, var_name) RESULT(file_name)

USE io_constants, ONLY: max_file_name_len

USE string_utils_mod, ONLY: str_replace

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Replaces all occurences of the variable name template string with the
!   given variable name
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: tpl  ! The string to replace variable
                                     ! name templates in
CHARACTER(LEN=*), INTENT(IN) :: var_name  ! The variable name to replace
                                          ! them with

! Return type
CHARACTER(LEN=max_file_name_len) :: file_name  ! The resulting file name


!-----------------------------------------------------------------------------

file_name = str_replace(tpl, tpl_var_name, var_name)

RETURN

END FUNCTION tpl_substitute_var

!##############################################################################

! ****************************************************************************
! Time templating
! ****************************************************************************

FUNCTION tpl_detect_period(tpl) RESULT(tpl_period)

USE datetime_mod, ONLY: period_year, period_month, period_day

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Determines from the template the period of time over which files will
!   apply
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: tpl  ! The template to determine period
                                     ! for

! Return type
INTEGER :: tpl_period  ! The detected templating period

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Currently, we only have code for handling templating periods of a day, a
! month, or a year, so just look for those.
!-----------------------------------------------------------------------------

! Return a templating period of 0 if there are no time templating strings.
tpl_period = 0

IF ( INDEX(tpl, tpl_yr_2digit) > 0 .OR. INDEX(tpl, tpl_yr_4digit) > 0 ) THEN
  ! If the template contains any year strings, then the templating period is
  ! at least yearly.
  tpl_period = period_year

  ! We can't have monthly templating without first having yearly templating...
  IF ( INDEX(tpl, tpl_mon) > 0        .OR.                                     &
       INDEX(tpl, tpl_mon_2digit) > 0 .OR.                                     &
       INDEX(tpl, tpl_mon_abbr) > 0 ) THEN
    ! If the template contains any month strings, then the templating period is
    ! at least monthly.
    tpl_period = period_month

    ! We can't have daily templating without first having monthly templating...
    IF ( INDEX(tpl, tpl_day_2digit) > 0 ) THEN
      ! If the template contains any month strings, then the templating period
      ! is at least daily.
      tpl_period = period_day
    END IF  !  day

  END IF  !  month

END IF  ! year

RETURN

END FUNCTION tpl_detect_period

!##############################################################################

FUNCTION tpl_substitute_datetime(tpl, dt) RESULT(file_name)

USE io_constants, ONLY: max_file_name_len

USE string_utils_mod, ONLY: str_replace

USE datetime_mod, ONLY: datetime

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Replaces all occurences of the time templating strings with values from
!   the given datetime
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: tpl
                          ! The template to replace templating strings in
TYPE(datetime), INTENT(IN) :: dt
                          ! The datetime to get values from

! Return type
CHARACTER(LEN=max_file_name_len) :: file_name
                          ! The resulting file name


! Work variables
CHARACTER(LEN=4) :: formatted_str  ! The formatted string to pass to
                                   ! the replace function

!-----------------------------------------------------------------------------

! Just replace each time templating string appropriately

! 4 digit years
WRITE(formatted_str, "(I4.4)") dt%year
file_name = str_replace(tpl, tpl_yr_4digit, formatted_str)

! 2 digit years
WRITE(formatted_str, "(I2.2)") MOD(dt%year, 100)
file_name = str_replace(file_name, tpl_yr_2digit, TRIM(formatted_str))

! 1 or 2 digit months
IF ( dt%month < 10 ) THEN
  WRITE(formatted_str, "(I1.1)") dt%month
ELSE
  WRITE(formatted_str, "(I2.2)") dt%month
END IF
file_name = str_replace(file_name, tpl_mon, TRIM(formatted_str))

! 2 digit month
WRITE(formatted_str, '(I2.2)') dt%month
file_name = str_replace(file_name, tpl_mon_2digit, TRIM(formatted_str))

! 3 character string month
formatted_str = month_abbreviations(dt%month)
file_name = str_replace(file_name, tpl_mon_abbr, TRIM(formatted_str))

! 2 digit day of month.
WRITE(formatted_str, '(I2.2)') dt%day
file_name = str_replace(file_name, tpl_day_2digit, TRIM(formatted_str))

RETURN

END FUNCTION tpl_substitute_datetime

END MODULE templating_mod
#endif
