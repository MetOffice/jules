#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE logging_mod

USE jules_vars_mod, ONLY: mpi_local_comm

USE ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT

IMPLICIT NONE

! Log levels - these can be combined using bitwise operators to indicate
! any combination of log levels
INTEGER, PARAMETER ::                                                          &
  log_level_info  = 1,                                                         &
  log_level_debug = 2,                                                         &
  log_level_warn  = 4,                                                         &
  log_level_error = 8,                                                         &
  log_level_fatal = 16

! Determines what log levels are printed to log_unit - this is a bitwise
! combination of values from above.
! The default (31) is to print everything
INTEGER, PARAMETER :: log_print_level = 31

! Determines what log levels cause a program to stop - this is a bitwise
! combination of values from above.
! Fatal errors will always cause the program to stop, by definition.
! The default (0) is to stop only for fatal errors
! Setting this to 15 (i.e. stop for everything, even info) or 14 (i.e. stop
! for everything except info) are useful options for debugging
INTEGER, PARAMETER :: log_stop_level = 0

! The maximum line length for log entries. Any message larger than this is
! truncated
INTEGER, PARAMETER :: log_max_line_len = 500

! The calculated number of tasks id and task prefix
! These are cached the first time they are calculated to avoid calling the
! MPI routines every time a log message is written
INTEGER :: ntasks = 0, task_id = 0
CHARACTER(LEN=20) :: task_prefix = ""

! Named Options for who writes
INTEGER, PARAMETER            :: outputAll    = 1     ! all tasks
INTEGER, PARAMETER            :: outputZero   = 2     ! Task 0 only
INTEGER                       :: prnt_writers = outputAll

NAMELIST  / jules_prnt_control/ prnt_writers

! Visibilities
PRIVATE
PUBLIC :: log_info, log_debug, log_warn, log_error, log_fatal,                 &
          init_prnt_control

CONTAINS

SUBROUTINE write_to_log(log_level, proc_name, message)

#if defined(INTEL_FORTRAN)
USE ifcore, ONLY: tracebackqq
#endif

USE string_utils_mod, ONLY: to_string

!-----------------------------------------------------------------------------
! Description:
!   Logs the given message at the given level and performs any necessary
!   action
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: log_level
  ! The level at which to log the given message
CHARACTER(LEN=*), INTENT(IN) :: proc_name
  ! The name of the originating routine/function
CHARACTER(LEN=*), INTENT(IN) :: message
  ! The message to log

! Work variables
CHARACTER(LEN=log_max_line_len) :: full_message
INTEGER :: ERROR

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Construct the full log message
!-----------------------------------------------------------------------------
! Prepend a fragment to the message depending on the log level
SELECT CASE ( log_level )
CASE ( log_level_info )
  full_message = "[INFO]"

CASE ( log_level_debug )
  full_message = "[DEBUG]"

CASE ( log_level_warn )
  full_message = "[WARNING]"

CASE ( log_level_error )
  full_message = "[ERROR]"

CASE ( log_level_fatal )
  full_message = "[FATAL ERROR]"

CASE DEFAULT
  ! This should never happen since the only public access to write_to_log is
  ! through the log_* routines defined below
  CALL log_fatal("write_to_log", "Unknown log level")
END SELECT

! Combine the name of the originating procedure with the message
full_message = TRIM(full_message) // " " // TRIM(proc_name) // ': ' // message

! Calculate the number of tasks and task prefix if we have not already
IF ( ntasks <= 0 ) THEN
  CALL mpi_comm_size(mpi_local_comm, ntasks, ERROR)
  CALL mpi_comm_rank(mpi_local_comm, task_id, ERROR)
  IF ( ntasks > 1 ) THEN
    task_prefix = "{MPI Task " // TRIM(to_string(task_id)) // "}"
  END IF
END IF

! Prepend the task prefix
full_message = ADJUSTL(TRIM(task_prefix) // " " // full_message)

!-----------------------------------------------------------------------------
! Use a bitwise and to check if we want to print log messages for the
! given log level
!-----------------------------------------------------------------------------
IF ( IAND(log_print_level, log_level) > 0 ) THEN
  IF ( log_level <= log_level_debug ) THEN
    ! Limit what task writes the output if desired
    IF ( prnt_writers == outputAll .OR.                                        &
       ( prnt_writers == outputZero .AND. task_id == 0 ) ) THEN
      ! Print info and debug to stdout
      WRITE(OUTPUT_UNIT, "(A)") TRIM(full_message)
    END IF
  ELSE
    ! Print errors to stdout
    WRITE(ERROR_UNIT, "(A)") TRIM(full_message)
  END IF
END IF

!-----------------------------------------------------------------------------
! Check if we need to stop the program
!-----------------------------------------------------------------------------
IF ( IAND(log_stop_level, log_level) > 0 .OR.                                  &
     log_level == log_level_fatal ) THEN
  ! If stopping abnormally, attempt to print a stack trace
#if defined(GNU_FORTRAN)
  ! Currently, the default Met Office version of gfortran is too old to support
  ! BACKTRACE, as is the default gfortran version in some common OS package
  ! systems
  ! We'll add this back in when gfortran 4.8 is fully available
  !      CALL BACKTRACE()
#elif defined(INTEL_FORTRAN)
  CALL tracebackqq(user_exit_code = -1)
#endif

  ! Abort MPI with a non-zero exit code
  CALL mpi_abort(mpi_local_comm, 1, ERROR)
END IF

END SUBROUTINE write_to_log


SUBROUTINE log_info(proc_name, message)

!-----------------------------------------------------------------------------
! Description:
!   Utility function to write a message to the log at level info
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: proc_name
CHARACTER(LEN=*), INTENT(IN) :: message

CALL write_to_log(log_level_info, proc_name, message)

END SUBROUTINE log_info


SUBROUTINE log_debug(proc_name, message)

!-----------------------------------------------------------------------------
! Description:
!   Utility function to write a message to the log at level debug
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: proc_name
CHARACTER(LEN=*), INTENT(IN) :: message

CALL write_to_log(log_level_debug, proc_name, message)

END SUBROUTINE log_debug


SUBROUTINE log_warn(proc_name, message)

!-----------------------------------------------------------------------------
! Description:
!   Utility function to write a message to the log at level warn
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: proc_name
CHARACTER(LEN=*), INTENT(IN) :: message

CALL write_to_log(log_level_warn, proc_name, message)

END SUBROUTINE log_warn


SUBROUTINE log_error(proc_name, message)

!-----------------------------------------------------------------------------
! Description:
!   Utility function to write a message to the log at level error
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: proc_name
CHARACTER(LEN=*), INTENT(IN) :: message

CALL write_to_log(log_level_error, proc_name, message)

END SUBROUTINE log_error


SUBROUTINE log_fatal(proc_name, message)

!-----------------------------------------------------------------------------
! Description:
!   Utility function to write a message to the log at level fatal
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: proc_name
CHARACTER(LEN=*), INTENT(IN) :: message

CALL write_to_log(log_level_fatal, proc_name, message)

END SUBROUTINE log_fatal


SUBROUTINE read_nml_jules_prnt_control(nml_dir)

! Description:
!  Read the JULES_PRNT_CONTROL namelist (standalone)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE errormessagelength_mod, ONLY: errormessagelength


IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_PRNT_CONTROL'

CALL log_info(RoutineName, "Reading JULES_PRNT_CONTROL namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'jules_prnt_control.nml'),   &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file jules_prnt_control.nml " //      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_prnt_control, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_PRNT_CONTROL " //               &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file jules_prnt_control.nml " //      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_prnt_control


SUBROUTINE check_jules_prnt_control()

USE errormessagelength_mod, ONLY: errormessagelength

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_PRNT_CONTROL namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errorstatutus
CHARACTER(LEN=errormessagelength) :: iomessage
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_PRNT_CONTROL'

SELECT CASE ( prnt_writers )
CASE ( outputAll )
  ! Initial value so no change to inform about; output to all tasks.
CASE ( outputZero )
  WRITE(iomessage, '(A,I0)')                                                   &
     "Further informative messages will be written to Task 0: " //             &
     "prnt_writers = ", prnt_writers
  CALL log_info(RoutineName, iomessage)
CASE DEFAULT
  WRITE(iomessage, '(A,I0)')                                                   &
     "prnt_writers is not a recognised value: prnt_writers = ", prnt_writers
  CALL log_fatal(RoutineName, iomessage)
END SELECT

END SUBROUTINE check_jules_prnt_control


SUBROUTINE init_prnt_control(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in the jules_prnt_control namelist items, prints and performs checks
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CALL read_nml_jules_prnt_control(nml_dir)
CALL check_jules_prnt_control()

END SUBROUTINE init_prnt_control

END MODULE logging_mod
#endif
