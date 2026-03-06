#if defined(LFRIC)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE check_unavailable_options_mod

! This contains options unavailable to LFRic apps which have had the namelist
! items plumbed through jules_physics_init. It does not contain those with
! hardwired values.

IMPLICIT NONE

CONTAINS

SUBROUTINE check_unavailable_options()

USE ereport_mod, ONLY: ereport
USE log_mod,     ONLY: log_event, log_scratch_space, log_level_warning

USE jules_surface_mod, ONLY: l_anthrop_heat_src, anthrop_heat_option, dukes,   &
                             l_flake_model, l_aggregate, l_elev_land_ice,      &
                             l_elev_lw_down, l_point_data

IMPLICIT NONE

!Local variables
INTEGER :: errcode, error_sum
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_UNAVAILABLE_OPTIONS'

error_sum = 0

! jules_surface
IF ( l_anthrop_heat_src .AND. anthrop_heat_option /= dukes ) THEN
  error_sum = error_sum + 1
  WRITE(log_scratch_space,'(I0,A,I0,A)') error_sum,                            &
     ": Only the DUKES (0) anthopogenic heat option is available. " //         &
     "anthrop_heat_option = ", anthrop_heat_option,                            &
     ". Please see LFRic apps ticket #1009 for details."
  CALL log_event(RoutineName//": "//TRIM(log_scratch_space), log_level_warning)
END IF

IF ( l_flake_model ) THEN
  error_sum = error_sum + 1
  WRITE(log_scratch_space,'(I0,A,L1)') error_sum,                              &
     ": FLake is not available to LFRic. l_flake_model = ", l_flake_model
  CALL log_event(RoutineName//": "//TRIM(log_scratch_space), log_level_warning)
END IF

IF ( l_aggregate ) THEN
  error_sum = error_sum + 1
  WRITE(log_scratch_space,'(I0,A,L1)') error_sum,                              &
     ": The aggregate tile is deprecated and not available to LFRic. " //      &
     "l_aggregate = ", l_aggregate
  CALL log_event(RoutineName//": "//TRIM(log_scratch_space), log_level_warning)
END IF

IF ( l_elev_land_ice ) THEN
  error_sum = error_sum + 1
  WRITE(log_scratch_space,'(I0,A,L1)') error_sum,                              &
     ": Elevated land ice tiles are not available to LFRic. " //               &
     "l_elev_land_ice = ", l_elev_land_ice
  CALL log_event(RoutineName//": "//TRIM(log_scratch_space), log_level_warning)
END IF

IF ( l_elev_lw_down ) THEN
  error_sum = error_sum + 1
  WRITE(log_scratch_space,'(I0,A,L1)') error_sum,                              &
     ": Downward adjustment of longwave radiation for elevated tiles " //      &
     "is not available to LFRic. l_elev_lw_down = ", l_elev_lw_down
  CALL log_event(RoutineName//": "//TRIM(log_scratch_space), log_level_warning)
END IF

IF ( l_point_data ) THEN
  error_sum = error_sum + 1
  WRITE(log_scratch_space,'(I0,A,L1)') error_sum,                              &
     ": It is not possible to use point rainfall data with LFRic. " //         &
     "l_point_data = ", l_point_data
  CALL log_event(RoutineName//": "//TRIM(log_scratch_space), log_level_warning)
END IF

! Defining errors ends here. Now issue FATAL ereport.
IF ( error_sum > 0 ) THEN
  errcode = 10
  WRITE(log_scratch_space,'(A,I0,A)') ": One or more JULES options (",         &
     error_sum,                                                                &
     ") have been incorrectly set for use in LFRic apps." //                   &
     NEW_LINE('A') // "Please see job output for details."
  CALL ereport(RoutineName, errcode, log_scratch_space)
END IF


RETURN
END SUBROUTINE check_unavailable_options
END MODULE check_unavailable_options_mod
#endif
