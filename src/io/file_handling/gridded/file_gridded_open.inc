#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION file_gridded_open(NAME, mode, comm, info) RESULT(FILE)

USE file_mod, ONLY: file_open

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Opens a gridded file and returns a file_gridded object representing it
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
TYPE(file_gridded) :: FILE


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check that the MPI variables are either specified together or not at all
!-----------------------------------------------------------------------------
IF ( PRESENT(comm) .NEQV. PRESENT(info) )                                      &
  CALL log_fatal("file_gridded_open",                                          &
                 "Only one of comm and info is present - either give a " //    &
                 "value for both MPI variables for parallel access or " //     &
                 "omit both for serial access")

! All we need to do is open the underlying file
IF ( PRESENT(comm) ) THEN
  FILE%fh = file_open(NAME, mode, comm, info)
ELSE
  FILE%fh = file_open(NAME, mode)
END IF

RETURN

END FUNCTION file_gridded_open
#endif
