#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_ts_open(mode, data_start, data_end, data_period, is_climatology, &
                      use_time_template, template, files, file_times,          &
                      comm, info)                                              &
                              RESULT(FILE)

USE io_constants, ONLY: mode_read, mode_write

USE datetime_mod, ONLY: period_day, period_month, period_year, datetime_advance

USE templating_mod, ONLY: tpl_detect_period

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Opens a timeseries file and returns a file_ts object representing it
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: mode ! The mode to open the file
                            ! One of mode_read or mode_write

TYPE(datetime), INTENT(IN) :: data_start
                            ! The date and time of the first data
TYPE(datetime), INTENT(IN) :: data_end
                            ! The date and time of the last data
INTEGER, INTENT(IN) :: data_period
                            ! The period of the data
                            ! (in seconds or a 'special' period)
LOGICAL, INTENT(IN) :: is_climatology
                            ! .TRUE. - the data is a climatology
                            ! .FALSE. - the data is not a climatology

LOGICAL, INTENT(IN) :: use_time_template
                            ! .TRUE. - use time templating
                            ! .FALSE. - use lists of file names and times
                            !           of first data in each file

! With time templating
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: template
                            ! The time template to use

! With a file list
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: files(:)
                            ! List of file names
TYPE(datetime), OPTIONAL, INTENT(IN) :: file_times(:)
                            ! Time of first data for each file

! MPI variables
INTEGER, OPTIONAL, INTENT(IN) :: comm  ! MPI communicator to use for parallel
                                       ! I/O
                                       ! If not given, serial I/O is used
INTEGER, OPTIONAL, INTENT(IN) :: info  ! MPI info object to use for parallel
                                       ! I/O
                                       ! If not given, serial I/O is used

! Return type
TYPE(file_ts) :: FILE


! Work variables
INTEGER :: nfiles ! The number of files in the files/file_times lists
INTEGER :: tpl_period ! The detected templating period

TYPE(datetime) :: prev_file_start  ! The start time of the previous file
                                   ! in the list
                                   ! Used when checking that files
                                   ! given in a list are in chronological
                                   ! order
LOGICAL :: file_exists  ! Indicates if the file exists
                        ! Used when checking that files given in a list
                        ! exist

INTEGER :: i ! Loop counter

CHARACTER(LEN=max_file_name_len) :: fname   !  Name of file or template, for
                                            !  error message.

!-----------------------------------------------------------------------------


! Initialise nfiles - this is not used when time-templating is enabled
nfiles = -1

! Load name of file or template, for error messages.
fname = ''  !  initialise, in case optional argument not provided
IF ( use_time_template ) THEN
  IF ( PRESENT(template) ) fname = 'template name: ' // TRIM(template)
ELSE
  IF ( PRESENT(files) ) THEN
    IF ( SIZE(files) > 0 ) fname = files(1)    ! Give name of first file.
    IF ( SIZE(files) > 1 ) fname = TRIM(fname) // ' and related files'
  END IF
END IF

!*****************************************************************************
! Check that a valid combination of options has been supplied
!*****************************************************************************
!-----------------------------------------------------------------------------
! Check that the start and end times for the data make sense
!-----------------------------------------------------------------------------
! Obviously, the data must start before they end, so bail if not true
IF ( data_start >= data_end )                                                  &
  CALL log_fatal("file_ts_open",                                               &
                 TRIM(fname) // ": " //                                        &
                 "Data start must be strictly before data end.")

! Check that the data period is allowed
SELECT CASE ( data_period )
CASE ( 1:, period_month, period_year )
  ! These are the allowed data periods
  CALL log_info("file_ts_open",                                                &
                "Opening time series with data_period=" //                     &
                TRIM(to_string(data_period)))

CASE DEFAULT
  CALL log_fatal("file_ts_open",                                               &
                 TRIM(fname) // ": " //                                        &
                 "Data period must be > 0 or a 'special' period (1 month " //  &
                 "or 1 year) (supplied " // TRIM(to_string(data_period))   //  &
                 ")")
END SELECT

! Check that the start time is appropriate if using a special period
IF ( data_period == period_month .OR. data_period == period_year ) THEN
  IF ( data_start%TIME /= 0 .OR. data_start%day /= 1 )                         &
    CALL log_fatal("file_ts_open",                                             &
                   TRIM(fname) // ": " //                                      &
                   "When using data_period=" //                                &
                   TRIM(to_string(data_period)) //                             &
                   ", data must start at 00:00:00 on 1st of month")

  IF ( data_period == period_year .AND. data_start%month /= 1 )                &
    CALL log_fatal("file_ts_open",                                             &
                   TRIM(fname) // ": " //                                      &
                   "When using data_period=" //                                &
                   TRIM(to_string(data_period)) //                             &
                   ", data must start at 00:00:00 on 1st of January")
END IF

!-----------------------------------------------------------------------------
! Check that we have appropriate arguments to be able to locate the
! correct files to use
!-----------------------------------------------------------------------------
IF ( use_time_template ) THEN
  ! If using time templating, we need a template to use
  IF ( .NOT. PRESENT(template) )                                               &
    CALL log_fatal("file_ts_open",                                             &
                   "Time templating selected but no template given")

  ! Check that template is not an empty string
  IF ( LEN_TRIM(template) <= 0 )                                               &
    CALL log_fatal("file_ts_open",                                             &
                   "Time templating selected but template is empty string")

  tpl_period = tpl_detect_period(template)

  ! If the period is not one that has code written for it, abort
  SELECT CASE ( tpl_period )
  CASE ( period_day, period_month, period_year )
    ! Fine - just log some information
    CALL log_info("file_ts_open",                                              &
                  "Detected period=" // TRIM(to_string(tpl_period)) //         &
                  " for template " // TRIM(template))

  CASE DEFAULT
    CALL log_fatal("file_ts_open",                                             &
                   "Could not detect supported templating period " //          &
                   "for template " // TRIM(template))
  END SELECT
ELSE
  ! If using lists of files and start times, check that they are present and
  ! consistent
  IF ( .NOT. PRESENT(files) )                                                  &
    CALL log_fatal("file_ts_open",                                             &
                   "Time templating is not selected - a list of files " //     &
                   "must be given")

  IF ( .NOT. PRESENT(file_times) )                                             &
    CALL log_fatal("file_ts_open",                                             &
                   TRIM(fname) // ": " //                                      &
                   "Time templating is not selected - a list of file " //      &
                   "times must be given")

  nfiles = SIZE(files)

  IF ( nfiles <= 0 )                                                           &
    CALL log_fatal("file_ts_open",                                             &
                   "Time templating is not selected - list must contain" //    &
                   " at least one file")

  IF ( SIZE(file_times) /= nfiles )                                            &
    CALL log_fatal("file_ts_open",                                             &
                   TRIM(fname) // ": " //                                      &
                   "'files' and 'file_times' must have the same " //           &
                   "number of entries")

  ! If using yearly data, there must be one file containing all the data
  ! We know from previous checks on file_times(1) and data_start that this file
  ! starts on the 1st Jan for some year
  IF ( data_period == period_year .AND. nfiles > 1 )                           &
    CALL log_fatal("file_ts_open",                                             &
                   TRIM(fname) // ": " //                                      &
                   "Yearly data must be contained in a single file for " //    &
                   "the entirety of the data")

  ! Do checks on the individual files
  DO i = 1,nfiles
    IF ( mode == mode_read ) THEN
      ! By this point, we know that we don't have variable name or time templating,
      ! so we should check if the given files exist
      INQUIRE(FILE = files(i), EXIST = file_exists)
      IF ( .NOT. file_exists )                                                 &
        CALL log_fatal("file_ts_open",                                         &
                       "Given file '" // TRIM(files(i)) // "' does not exist")
    ELSE
      ! If we are in write mode, just check that a non-empty file name has been
      ! given
      IF ( LEN_TRIM(files(i)) <= 0 )                                           &
        CALL log_fatal("file_ts_open",                                         &
                       "List of files given, but one or more file names" //    &
                       " are the empty string")
    END IF

    ! Check that the start times of the files are suitable
    IF ( i == 1 ) THEN
      ! Check that the first file starts at data start
      IF ( file_times(i) /= data_start )                                       &
        CALL log_fatal("file_ts_open",                                         &
                       TRIM(files(i)) // ": " //                               &
                       "Start time for the first file must match data_start")

      prev_file_start = file_times(i)
    ELSE
      ! Check that all subsequent files start after the previous file
      IF ( file_times(i) <= prev_file_start )                                  &
        CALL log_fatal("file_ts_open",                                         &
                       TRIM(files(i)) // ": " //                               &
                       "Files must be given in chronological order")

      prev_file_start = file_times(i)
    END IF

    ! If using monthly data, files must start at midnight on the 1st of some
    ! month (but not necessarily only contain data for one month)
    IF ( data_period == period_month ) THEN
      IF ( file_times(i)%TIME /= 0 .OR. file_times(i)%day /= 1 )               &
        CALL log_fatal("file_ts_open",                                         &
                       TRIM(files(i)) // ": " //                               &
                       "When using monthly data with a list of files, " //     &
                       "all files must start at 00:00:00 on 1st of " //        &
                       "some month")
    END IF
  END DO
END IF  !  use_time_template

!-----------------------------------------------------------------------------
! If we have a climatology, check that the given arguments and detected
! properties make sense
!-----------------------------------------------------------------------------
IF ( is_climatology ) THEN
  ! It makes no sense to specify is_climatology if in write mode
  IF ( mode == mode_write )                                                    &
    CALL log_fatal("file_ts_open",                                             &
                   "Cannot open a file in write mode as a climatology.")

  ! The data must start at the beginning of the year
  IF ( data_start%TIME /= 0 .OR.                                               &
       data_start%day /= 1 .OR. data_start%month /= 1 )                        &
    CALL log_fatal("file_ts_open",                                             &
                   TRIM(fname) // ": " //                                      &
                   "When using data as a climatology, data must start " //     &
                   "at 00:00:00 on 1st of January")

  ! The data must apply for exactly one year
  IF ( datetime_advance(data_start, period_year) /= data_end )                 &
    CALL log_fatal("file_ts_open",                                             &
                   TRIM(fname) // ": " //                                      &
                   "When using data as a climatology, exactly one year " //    &
                   "of data must be given.")

  ! Warn the user if they have supplied yearly data - this means the same data
  ! will be used every year, and is the same as having fixed data
  IF ( data_period == period_year )                                            &
    CALL log_warn("file_ts_open",                                              &
                  "When using a climatology, a data period of a year " //      &
                  "is equivalent to having fixed data")
END IF

!-----------------------------------------------------------------------------
! Check that the MPI variables are either specified together or not at all
!-----------------------------------------------------------------------------
IF ( PRESENT(comm) .NEQV. PRESENT(info) )                                      &
  CALL log_fatal("file_ts_open",                                               &
                 "Only one of comm and info is present - either give a " //    &
                 "value for both MPI variables for parallel access or " //     &
                 "omit both for serial access")


!-----------------------------------------------------------------------------
! Now we are happy that the arguments are consistent, we can set up the
! file_ts object
!-----------------------------------------------------------------------------
FILE%mode = mode

! Store info about MPI-IO settings
IF ( PRESENT(comm) ) THEN
  FILE%use_mpiio = .TRUE.
  FILE%comm = comm
  FILE%info = info
END IF

FILE%data_start  = data_start
FILE%data_end    = data_end
FILE%data_period = data_period
FILE%is_climatology = is_climatology

FILE%use_time_template = use_time_template

IF ( use_time_template ) THEN
  ! With time templating
  FILE%template   = template
  FILE%tpl_period = tpl_period
ELSE
  ! With a file list
  FILE%nfiles = nfiles

  ! Allocate space for the files and file_times arrays
  ALLOCATE(FILE%files(nfiles))
  ALLOCATE(FILE%file_times(nfiles))

  FILE%files(:)      = files(:)
  FILE%file_times(:) = file_times(:)
END IF

RETURN

END FUNCTION file_ts_open
#endif
