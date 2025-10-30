! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE read_dump_var_rivers_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE read_dump_var_rivers( identifier, FILE, var_id )

!Science variables
! TYPE Definitions
USE jules_fields_mod, ONLY: rivers

USE model_interface_mod, ONLY:                                                 &
  identifier_len

USE file_mod, ONLY:                                                            &
  file_handle, file_read_var

USE logging_mod, ONLY: log_fatal, log_warn

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads the rivers variable from the dump file.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
CHARACTER(LEN=identifier_len) :: identifier
                                    ! The model identifier for the variable
                                    ! to put in the dump

TYPE(file_handle) :: FILE  ! The dump file

INTEGER :: var_id
                      ! The id of the variable in the dump file

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_DUMP_VAR_RIVERS'

!-----------------------------------------------------------------------------

SELECT CASE ( identifier )
  ! Cases for river routing variables

CASE ( 'flood_flow' )
  CALL file_read_var(FILE, var_id, rivers%flood_flow)

CASE ( 'flood_flow_prev' )
  CALL file_read_var(FILE, var_id, rivers%flood_flow_prev)

CASE ( 'flood_storage' )
  CALL file_read_var(FILE, var_id, rivers%flood_storage)

CASE ( 'flood_storage_prev' )
  CALL file_read_var(FILE, var_id, rivers%flood_storage_prev)

CASE ( 'rfm_bflowin_rp' )
  CALL file_read_var(FILE, var_id, rivers%rfm_bflowin_rp)

CASE ( 'rfm_flowin_rp' )
  CALL file_read_var(FILE, var_id, rivers%rfm_flowin_rp)

CASE ( 'rfm_substore_rp' )
  CALL file_read_var(FILE, var_id, rivers%rfm_substore_rp)

CASE ( 'rfm_surfstore_rp' )
  CALL file_read_var(FILE, var_id, rivers%rfm_surfstore_rp)

CASE ( 'river_channel_flow' )
  CALL file_read_var(FILE, var_id, rivers%river_channel_flow)

CASE ( 'river_channel_storage' )
  CALL file_read_var(FILE, var_id, rivers%river_channel_storage)

CASE ( 'river_depth_prev' )
  CALL file_read_var(FILE, var_id, rivers%river_depth_prev)

CASE ( 'river_flow_prev' )
  CALL file_read_var(FILE, var_id, rivers%river_flow_prev)

CASE ( 'rivers_outflow_rp' )
  CALL file_read_var(FILE, var_id, rivers%rivers_outflow_rp)

CASE ( 'rivers_sto_rp' )
  CALL file_read_var(FILE, var_id, rivers%rivers_sto_rp)

CASE DEFAULT
  CALL log_fatal( RoutineName,                                                 &
                 "Unexpected variable in dump - " // TRIM(identifier) )
END SELECT

RETURN

END SUBROUTINE read_dump_var_rivers
END MODULE read_dump_var_rivers_mod
