! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE read_dump_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE read_dump(file_name, identifiers)

!Others
USE parallel_mod, ONLY:                                                        &
  is_master_task

USE file_mod, ONLY:                                                            &
  file_handle, file_close

USE read_dump_shared_mod,     ONLY: read_dump_shared
USE read_dump_var_rivers_mod, ONLY: read_dump_var_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Check that the given file is a Rivers dump compatible with the current
!   run, and read the given identifiers from it.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*) :: file_name  ! The dump file
CHARACTER(LEN=*) :: identifiers(:)  ! The model identifiers for the
                                    ! variables to define

! Work variables
TYPE(file_handle) :: FILE
  ! The opened file

INTEGER :: nvars
  ! The number of variables we are processing

INTEGER :: var_ids(SIZE(identifiers))
  ! The ids of the variables in the dump file

INTEGER :: i  ! Loop counters

!-----------------------------------------------------------------------------

! Check that the given file is a dump compatible with the current
! run, and read the given identifiers from it.
CALL read_dump_shared(file_name, identifiers, nvars, FILE, var_ids)

!-----------------------------------------------------------------------------
! Set the requested variables from the file
!
! We assume that if the file passed all the checks on dimensions above, then
! it will be fine to fill variables here (i.e. we don't check the dimensions
! associated with the variables)
!-----------------------------------------------------------------------------

IF ( is_master_task() ) THEN
  !---------------------------------------------------------------------------
  ! In the master task, read the data from file
  !---------------------------------------------------------------------------
  DO i = 1,nvars
    ! Cases for river routing variables plus log fatal for unknown identifier
    CALL read_dump_var_rivers( identifiers(i), FILE, var_ids(i) )
  END DO
  CALL file_close(FILE)
END IF  ! MASTER TASK

RETURN

END SUBROUTINE read_dump
END MODULE read_dump_mod
