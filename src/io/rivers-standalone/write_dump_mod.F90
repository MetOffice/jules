! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE write_dump_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE write_dump()

!Science variables
! TYPE Definitions
USE jules_fields_mod, ONLY: rivers

USE jules_rivers_mod, ONLY:                                                    &
  np_rivers


USE parallel_mod, ONLY:                                                        &
  is_master_task

USE model_interface_mod, ONLY:                                                 &
  identifier_len

USE file_mod, ONLY:                                                            &
  file_handle, file_write_var, file_close

USE dump_mod,                       ONLY: max_var_dump
USE logging_mod,                    ONLY: log_fatal, log_info
USE required_vars_for_rivers_mod,   ONLY: required_vars_for_rivers
USE create_dump_file_mod,           ONLY: create_dump_file
USE write_dump_var_rivers_mod,      ONLY: write_dump_var_rivers
USE add_dump_coords_for_rivers_mod, ONLY: add_dump_coords_for_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a dump file for the current timestep
!   Note that the writing of the dump is done by the master task with the
!   values gathered from other tasks. This means that dumps written with
!   different amounts of tasks should be interchangable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
CHARACTER(LEN=identifier_len) :: identifiers(max_var_dump)
                                    ! The model identifiers for the variables
                                    ! to put in the dump
CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                             ! The variable identifiers of the ancil
                             ! variables (not used in this subroutine)

TYPE(file_handle) :: FILE  ! The dump file

INTEGER :: nvars  ! The number of variables we are processing
INTEGER :: nvars_from_ancil

INTEGER :: var_ids(max_var_dump)
                      ! The ids of the variables in the dump file

INTEGER :: i, j ! Loop counter

LOGICAL, PARAMETER :: l_output_mode = .TRUE.

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Get the list of identifiers that we are going to output
!-----------------------------------------------------------------------------
nvars = 0
nvars_from_ancil = 0
CALL required_vars_for_rivers( nvars, identifiers,                             &
                               nvars_from_ancil, vars_from_ancil,              &
                               l_output_mode )

! Add latitude and longitude to the list to help offline inspection of the
! dump file, ie they are diagnostics, not prognostics or ancillaries.
! Lat & lon will not be read in from the dump to prevent confusion with the
! model grid namelists

CALL add_dump_coords_for_rivers(nvars, identifiers)

CALL create_dump_file(nvars, identifiers, FILE, var_ids)

IF ( is_master_task() ) THEN
  !---------------------------------------------------------------------------
  ! In the master task, write the data to file
  !---------------------------------------------------------------------------
  DO i = 1,nvars
    ! Cases for river routing variables plus log fatal for unknown identifier
    CALL write_dump_var_rivers( identifiers(i), FILE, var_ids(i) )
  END DO

  ! We are done with the file and dictionaries
  CALL file_close(FILE)
END IF  ! MASTER TASK

RETURN

END SUBROUTINE write_dump
END MODULE write_dump_mod
