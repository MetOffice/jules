#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_open(NAME, mode, comm, info) RESULT(FILE)

USE driver_ascii_mod, ONLY: extensions_ascii => extensions, file_ascii_open
USE driver_ncdf_mod, ONLY: extensions_ncdf => extensions, file_ncdf_open

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Opens a file and returns a file_handle object representing it
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*), INTENT(IN) :: NAME
                            ! The name of the file
INTEGER, INTENT(IN) :: mode ! The mode to open the file
                            ! One of mode_read or mode_write
INTEGER, OPTIONAL, INTENT(IN) :: comm  ! MPI communicator to use for parallel
                                       ! I/O
                                       ! If not given, serial I/O is used
INTEGER, OPTIONAL, INTENT(IN) :: info  ! MPI info object to use for parallel
                                       ! I/O
                                       ! If not given, serial I/O is used

! Return type
TYPE(file_handle) :: FILE


! Work variables
INTEGER :: last_dot  ! The index of the last . (dot) in the file name
                     ! Used to extract the file extension
CHARACTER(LEN=15) :: extension  ! The extension for the file name


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that the MPI variables are either specified together or not at all
!-----------------------------------------------------------------------------
IF ( PRESENT(comm) .NEQV. PRESENT(info) )                                      &
  CALL log_fatal("file_open",                                                  &
                 "Only one of comm and info is present - either give a " //    &
                 "value for both MPI variables for parallel access or " //     &
                 "omit both for serial access")

!-----------------------------------------------------------------------------
! Get the extension for the file name (i.e. everything after the last dot)
!-----------------------------------------------------------------------------
last_dot = INDEX(NAME, '.', back = .TRUE.)
extension = NAME(last_dot+1:)

! Select a driver based on the file extension
IF ( ANY(extensions_ascii == extension) ) THEN
  ! Assign the correct driver
  FILE%driver = driver_ascii
  ! Initialise the ASCII representation of the file
  IF ( PRESENT(comm) ) THEN
    FILE%ascii = file_ascii_open(NAME, mode, comm, info)
  ELSE
    FILE%ascii = file_ascii_open(NAME, mode)
  END IF

ELSE IF ( ANY(extensions_ncdf == extension) ) THEN
  ! Assign the correct driver
  FILE%driver = driver_ncdf
  ! Initialise the NetCDF representation of the file
  IF ( PRESENT(comm) ) THEN
    FILE%ncdf = file_ncdf_open(NAME, mode, comm, info)
  ELSE
    FILE%ncdf = file_ncdf_open(NAME, mode)
  END IF

ELSE
  ! File type not recognised
  CALL log_fatal("file_open",                                                  &
                 "Unrecognised file extension for file " // TRIM(NAME) //      &
                 " - see docs for supported file types")
END IF

RETURN

END FUNCTION file_open
#endif
