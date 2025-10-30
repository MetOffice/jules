#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_ancillaries_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_ancillaries( nml_dir, rivers, rivers_data )

USE io_constants, ONLY: namelist_unit

USE logging_mod, ONLY: log_fatal

USE string_utils_mod, ONLY: to_string

USE init_rivers_props_mod, ONLY: init_rivers_props

USE errormessagelength_mod, ONLY: errormessagelength

USE jules_rivers_mod, ONLY: rivers_type,rivers_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about model ancillaries required by Standalone Rivers
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

CHARACTER(LEN=errormessagelength) :: iomessage

!TYPES containing field data (IN OUT)
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT) :: rivers_data

! Work variables
INTEGER :: ERROR  ! Error indicator

CHARACTER(LEN=*), PARAMETER :: RoutineName='init_ancillaries'

!-----------------------------------------------------------------------------
! Open the ancillaries namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'ancillaries.nml'),          &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file ancillaries.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

! Defer to specialist routines to process each namelist (has to be the same
! order as specified in namelist files)

CALL init_rivers_props(rivers, rivers_data)

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file ancillaries.nml " //             &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

RETURN

END SUBROUTINE init_ancillaries

END MODULE init_ancillaries_mod

#endif
