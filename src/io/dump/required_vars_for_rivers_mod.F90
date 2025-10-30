! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE required_vars_for_rivers_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE required_vars_for_rivers( nvars, identifiers,                       &
                                     nvars_from_ancil, vars_from_ancil,        &
                                     read_or_write_dump )

USE jules_rivers_mod, ONLY: l_outflow_per_river, i_river_vn, rivers_camaflood, &
                            rivers_rfm, rivers_trip, l_init_storage

USE logging_mod, ONLY: log_warn

USE add_to_list_mod, ONLY: add_to_list


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns the identifiers of the prognostic variables for the current
!   configuration of the model
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN OUT) :: nvars  ! The number of variables
CHARACTER(LEN=*), INTENT(IN OUT) :: identifiers(:)
                               ! The model identifiers of the required
                               ! variables

INTEGER, INTENT(IN OUT)       :: nvars_from_ancil  !Number of ancil variables
CHARACTER(LEN=*), INTENT(OUT) :: vars_from_ancil(:)
                               ! The model identifiers of the required
                               ! ancil variables

LOGICAL, INTENT(IN), OPTIONAL :: read_or_write_dump
                               ! T - include all variables that are needed
                               !     when reading or writing to dump files
                               ! F - don't include variables that are only
                               !     inputted or outputted in dump files

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'REQUIRED_VARS_FOR_RIVERS'
    ! The name of this routine.

! Work variables
LOGICAL :: read_or_write_dump_local
                             ! Local version of read_or_write_dump
                             ! Defaults to TRUE if not present

!-----------------------------------------------------------------------------


! ancil_dump_read%rivers_props functionality has not yet been implemented,
! however the ability to initialise River storage (TRIP) from a climatology
! ancil requires the vars from ancil functionality so this is an exception.
! Further work is required to add vars in the usual way.

! Check for presence of optional arguments
read_or_write_dump_local  = .TRUE.
IF ( PRESENT(read_or_write_dump) )                                             &
  read_or_write_dump_local = read_or_write_dump

IF ( l_outflow_per_river ) THEN
  IF ( read_or_write_dump_local ) THEN
    CALL add_to_list( 'rivers_outflow_rp', nvars, identifiers )
  ELSE
    CALL log_warn( RoutineName,                                                &
                  "rivers_outflow_rp will be initialised to zero.")
  END IF
END IF

SELECT CASE( i_river_vn )

CASE ( rivers_camaflood )
  IF ( read_or_write_dump_local ) THEN
    CALL add_to_list( 'flood_flow', nvars, identifiers )
    CALL add_to_list( 'flood_flow_prev', nvars, identifiers )
    CALL add_to_list( 'flood_storage', nvars, identifiers )
    CALL add_to_list( 'flood_storage_prev', nvars, identifiers )
    CALL add_to_list( 'river_channel_flow', nvars, identifiers )
    CALL add_to_list( 'river_channel_storage', nvars, identifiers )
    CALL add_to_list( 'river_depth_prev', nvars, identifiers )
    CALL add_to_list( 'river_flow_prev', nvars, identifiers )
  ELSE
    CALL log_warn( RoutineName,                                                &
                  "CaMa-Flood river prognostics will be initialised to zero.")
  END IF

CASE ( rivers_rfm )
  IF ( read_or_write_dump_local ) THEN
    CALL add_to_list( 'rfm_bflowin_rp',   nvars, identifiers )
    CALL add_to_list( 'rfm_flowin_rp',    nvars, identifiers )
    CALL add_to_list( 'rfm_substore_rp',  nvars, identifiers )
    CALL add_to_list( 'rfm_surfstore_rp', nvars, identifiers )
  ELSE
    CALL log_warn( RoutineName,                                                &
                  "RFM river prognostics will be initialised to zero.")
  END IF

CASE ( rivers_trip )
  IF ( read_or_write_dump_local .OR. l_init_storage ) THEN
    CALL add_to_list( 'rivers_sto_rp', nvars, identifiers )
    IF ( l_init_storage ) THEN
      CALL add_to_list( 'rivers_sto_rp',  nvars_from_ancil, vars_from_ancil )
    END IF
  ELSE
    CALL log_warn( RoutineName,                                                &
                  "TRIP river prognostic will be initialised to zero.")
  END IF

END SELECT

RETURN

END SUBROUTINE required_vars_for_rivers
END MODULE required_vars_for_rivers_mod
