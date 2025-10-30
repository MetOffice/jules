MODULE oasis_rivers_control_mod
! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! Please refer to the OASIS-MCT version 4.0 user guide for details on the implementation:
!  https://oasis.cerfacs.fr/wp-content/uploads/sites/114/2021/12/GLOBC_TR_oasis3mct_UserGuide_4.0_final_122021.pdf
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************
USE string_utils_mod, ONLY: to_string
USE logging_mod, ONLY: log_info, log_warn, log_fatal

IMPLICIT NONE

PRIVATE

! OASIS variables
INTEGER                   :: compid, il_part_id, il_part_id_1d
!
CHARACTER(LEN=5), PARAMETER :: comp_name='river'

! Public subroutines
PUBLIC :: oasis_init, oasis_partition, oasis_grid, oasis_variables,            &
          oasis_send, oasis_receive, oasis_finalise

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='OASIS_RIVERS_CONTROL_MOD'

CONTAINS

#if defined(RIVER_CPL)
SUBROUTINE oasis_init()

USE mod_oasis,                   ONLY: oasis_init_comp, oasis_get_localcomm
USE jules_vars_mod,              ONLY: mpi_local_comm
USE jules_model_environment_mod, ONLY: l_oasis_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the oasis coupler and creates local MPI communicator
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local variables
INTEGER :: ierror, ierror2

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_INIT'

!------------------------------------------------------------------------------
! Coupling initialisation
CALL oasis_init_comp(compid, comp_name, ierror)

IF (ierror == 0) THEN
  ! Get the communicator allocated to this model component.
  CALL oasis_get_localcomm(mpi_local_comm, ierror2)
END IF

! Log messages need to be called after mpi_local_comm is initialized
IF (ierror /= 0) THEN
  CALL log_fatal(RoutineName,                                                  &
               "oasis_init_comp Error " //                                     &
               "(IOSTAT=" // TRIM(to_string(ierror)) // ")")
ELSE
  CALL log_info(RoutineName,"oasis_init_comp OK" )
END IF

IF (ierror2 /= 0) THEN
  CALL log_fatal(RoutineName,                                                  &
               "oasis_get_localcomm Error " //                                 &
               "(IOSTAT=" // TRIM(to_string(ierror2)) // ")")
ELSE
  CALL log_info(RoutineName,"oasis_get_localcomm OK" )
END IF

! Indicate that river coupling is taking place
l_oasis_rivers = .TRUE.

RETURN

END SUBROUTINE oasis_init
!
SUBROUTINE oasis_partition()

USE mod_oasis,        ONLY: oasis_def_partition
USE oasis_rivers_mod, ONLY: l_runoff_1d
USE jules_rivers_mod, ONLY: n_rivers, nx_rivers, ny_rivers
!
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Defines the oasis grid partition
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local variables
INTEGER, DIMENSION(3) :: ig_paral
INTEGER :: ierror

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_PARTITION'

!------------------------------------------------------------------------------
! Use serial partition, as rivers runs using only one processor
ig_paral(1) = 0
ig_paral(2) = 0

IF (l_runoff_1d) THEN
  ! Define the river outflow partition
  ig_paral(3) = n_rivers

  ! Coupling initialisation
  CALL oasis_def_partition(il_part_id_1d, ig_paral, ierror)

  IF (ierror /= 0) THEN
    CALL log_fatal(RoutineName,                                                &
                "oasis_def_partition 1d Error " //                             &
                "(IOSTAT=" // TRIM(to_string(ierror)) // ")")
  ELSE
    CALL log_info(RoutineName,"oasis_def_partition 1d OK" )
  END IF
END IF

ig_paral(3) = nx_rivers * ny_rivers

! Coupling initialisation
CALL oasis_def_partition(il_part_id, ig_paral, ierror)

IF (ierror /= 0) THEN
  CALL log_fatal(RoutineName,                                                  &
               "oasis_def_partition Error " //                                 &
               "(IOSTAT=" // TRIM(to_string(ierror)) // ")")
ELSE
  CALL log_info(RoutineName,"oasis_def_partition OK" )
END IF

RETURN

END SUBROUTINE oasis_partition
!
SUBROUTINE oasis_grid()
!
USE mod_oasis,        ONLY: oasis_start_grids_writing, oasis_write_grid,       &
                            oasis_write_corner, oasis_write_area,              &
                            oasis_write_mask, oasis_terminate_grids_writing
USE jules_rivers_mod, ONLY: nx_rivers, ny_rivers, rivers_dlat=>rivers_dy,      &
                            rivers_dlon=>rivers_dx
USE jules_fields_mod, ONLY: rivers
USE conversions_mod,  ONLY: pi_over_180
!
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes to file the oasis grid
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local variables
REAL    :: area(nx_rivers,ny_rivers),CornerLon(nx_rivers,ny_rivers,4),         &
           CornerLat(nx_rivers,ny_rivers,4)
INTEGER :: mask(nx_rivers,ny_rivers)
INTEGER :: ix, iy, ierror
REAL    :: lat_radians

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_GRID'

!------------------------------------------------------------------------------
! Create grid file
CALL oasis_start_grids_writing(ierror)
!

DO ix = 1, nx_rivers
  DO iy = 1, ny_rivers

    ! Calculate corner longitudes and latitudes starting in the
    ! Southwest corner and moving anticlockwise
    CornerLon(ix,iy,1) = rivers%rivers_lon2d(ix,iy) - 0.5 * rivers_dlon
    CornerLon(ix,iy,2) = rivers%rivers_lon2d(ix,iy) + 0.5 * rivers_dlon
    CornerLon(ix,iy,3) = rivers%rivers_lon2d(ix,iy) + 0.5 * rivers_dlon
    CornerLon(ix,iy,4) = rivers%rivers_lon2d(ix,iy) - 0.5 * rivers_dlon

    CornerLat(ix,iy,1) = rivers%rivers_lat2d(ix,iy) - 0.5 * rivers_dlat
    CornerLat(ix,iy,2) = rivers%rivers_lat2d(ix,iy) - 0.5 * rivers_dlat
    CornerLat(ix,iy,3) = rivers%rivers_lat2d(ix,iy) + 0.5 * rivers_dlat
    CornerLat(ix,iy,4) = rivers%rivers_lat2d(ix,iy) + 0.5 * rivers_dlat

    ! Calculate area in square radians
    lat_radians = rivers%rivers_lat2d(ix,iy) * pi_over_180
    area(ix,iy) = (rivers_dlat*pi_over_180) *                                  &
                  (rivers_dlon*pi_over_180) *                                  &
                  COS(lat_radians)

    ! 1 = masked, 0 = unmasked
    mask(ix,iy) = 0
  END DO
END DO

!
CALL oasis_write_grid('rivT',nx_rivers,ny_rivers,                              &
                      rivers%rivers_lon2d, rivers%rivers_lat2d,il_part_id)
CALL oasis_write_corner('rivT',nx_rivers,ny_rivers,4,CornerLon,CornerLat,il_part_id)
CALL oasis_write_area('rivT',nx_rivers,ny_rivers,area,il_part_id)
CALL oasis_write_mask('rivT',nx_rivers,ny_rivers,mask,il_part_id)
!
! Close grid file
CALL oasis_terminate_grids_writing()

RETURN

END SUBROUTINE oasis_grid
!
SUBROUTINE oasis_variables()
!
USE mod_oasis,        ONLY: oasis_def_var, oasis_enddef, oasis_in, oasis_out,  &
                            oasis_real
USE oasis_rivers_mod, ONLY: np_receive, np_send, cpl_receive, cpl_send
USE jules_rivers_mod, ONLY: n_rivers, nx_rivers, ny_rivers
!
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Declares all coupling variables
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local variables
INTEGER, PARAMETER :: var_nodims_1d(2) = [1,1 ]
INTEGER, PARAMETER :: var_nodims_2d(2) = [2,1 ]
INTEGER            :: var_actual_shape_1d(2)
INTEGER            :: var_actual_shape_2d(4)
INTEGER :: i, ierror

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_VARIABLES'

var_actual_shape_1d(:) = [1,n_rivers]
var_actual_shape_2d(:) = [1,nx_rivers,1,ny_rivers]

!------------------------------------------------------------------------------
! Declare oasis send fields
DO i = 1, np_send
  IF (TRIM(cpl_send(i)%field_name) == "outflow_per_river") THEN
    CALL oasis_def_var(cpl_send(i)%field_id, TRIM(cpl_send(i)%field_name),     &
                       il_part_id_1d, var_nodims_1d, oasis_out, var_actual_shape_1d, &
                       oasis_real, ierror)
  ELSE
    CALL oasis_def_var(cpl_send(i)%field_id, TRIM(cpl_send(i)%field_name),     &
                       il_part_id, var_nodims_2d, oasis_out, var_actual_shape_2d, &
                       oasis_real, ierror)
  END IF
  IF (ierror /= 0) THEN
    CALL log_fatal(RoutineName,                                                &
                 "oasis_def_var Error " //                                     &
                 "(IOSTAT=" // TRIM(to_string(ierror)) // "): " //             &
                 "when defining variable " // TRIM(cpl_send(i)%field_name))
  END IF
  IF (cpl_send(i)%field_id == -1) THEN
    CALL log_fatal(RoutineName,                                                &
                 "oasis_def_var Error " //                                     &
                 "(IOSTAT=" // TRIM(to_string(ierror)) // "): " //             &
                 "variable " // TRIM(cpl_send(i)%field_name) //                &
                 " not found in the namcouple file")
  END IF
END DO

! Declare oasis receive fields
DO i = 1, np_receive
  CALL oasis_def_var(cpl_receive(i)%field_id, TRIM(cpl_receive(i)%field_name), &
                     il_part_id, var_nodims_2d, oasis_in, var_actual_shape_2d, &
                     oasis_real, ierror)
  IF (ierror /= 0) THEN
    CALL log_fatal(RoutineName,                                                &
                 "oasis_def_var Error " //                                     &
                 "(IOSTAT=" // TRIM(to_string(ierror)) // "): " //             &
                 "when defining variable " // TRIM(cpl_receive(i)%field_name))
  END IF
  IF (cpl_receive(i)%field_id == -1) THEN
    CALL log_fatal(RoutineName,                                                &
                 "oasis_def_var Error " //                                     &
                 "(IOSTAT=" // TRIM(to_string(ierror)) // "): " //             &
                 "variable " // TRIM(cpl_send(i)%field_name) //                &
                 " not found in the namcouple file")
  END IF
END DO

! Finish oasis definitions
CALL oasis_enddef(ierror)

IF (ierror /= 0) THEN
  CALL log_fatal(RoutineName,                                                  &
               "oasis_enddef Error " //                                        &
               "(IOSTAT=" // TRIM(to_string(ierror)) // ")")
ELSE
  CALL log_info(RoutineName,"oasis_variables OK" )
END IF

RETURN

END SUBROUTINE oasis_variables
!
SUBROUTINE oasis_send(rivers, TIME)

USE mod_oasis,        ONLY: oasis_put, oasis_sent, oasis_loctrans,             &
                            oasis_torest, oasis_output, oasis_sentout,         &
                            oasis_torestout, oasis_waitgroup, oasis_ok
USE oasis_rivers_mod, ONLY: np_send, cpl_send
USE jules_rivers_mod, ONLY: rivers_type, n_rivers,                             &
                            l_outflow_per_river, calc_outflow_per_river
USE missing_data_mod, ONLY: imdi
!
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sends fields to other models via oasis
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Arguments with intent(in)
TYPE(rivers_type), INTENT(IN) :: rivers
INTEGER, INTENT(IN) :: TIME

! Local variables
REAL, ALLOCATABLE :: outflow_per_river(:)
INTEGER :: i, ip, ierror, ERROR

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_SEND'

!------------------------------------------------------------------------------
! Calculate the total runoff if necessary. This is done here so that other send
! variables do not have to wait for this to be calculated in the following loop
IF (l_outflow_per_river) THEN
  ! Calculate river outflow for each river. This can be done like this here
  ! because the river executable runs in only one processor
  ALLOCATE(outflow_per_river(n_rivers), STAT = ERROR)
  outflow_per_river = calc_outflow_per_river(rivers)
END IF

! Loop in all variables that are sent via coupling
DO i = 1, np_send
  SELECT CASE(TRIM(cpl_send(i)%field_name))
    ! send the right field
  CASE ('outflow_per_river')
    CALL oasis_put(cpl_send(i)%field_id,TIME,outflow_per_river,ierror)
  END SELECT

  IF (ierror /= oasis_sent .AND. ierror /= oasis_loctrans .AND.                &
      ierror /= oasis_torest .AND. ierror /= oasis_output  .AND.               &
      ierror /= oasis_sentout .AND. ierror /= oasis_torestout .AND.            &
      ierror /= oasis_waitgroup .AND. ierror /= oasis_ok) THEN
    CALL log_fatal(RoutineName,                                                &
                 "oasis_put Error " //                                         &
    "(IOSTAT=" // TRIM(to_string(ierror)) // "): " //                          &
    "when sending variable " // TRIM(cpl_send(i)%field_name))
  END IF
END DO

IF (l_outflow_per_river) THEN
  DEALLOCATE(outflow_per_river)
END IF

RETURN

END SUBROUTINE oasis_send
!
SUBROUTINE oasis_receive(rivers, TIME)

USE mod_oasis,        ONLY: oasis_get, oasis_recvd, oasis_fromrest,            &
                            oasis_input, oasis_recvout, oasis_fromrestout,     &
                            oasis_ok
USE oasis_rivers_mod, ONLY: np_receive, cpl_receive
USE jules_rivers_mod, ONLY: rivers_type, nx_rivers, ny_rivers
USE rivers_regrid_mod, ONLY: twod_to_rp

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Receives fields from other models via oasis
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Arguments with intent(in)
TYPE(rivers_type), INTENT(IN) :: rivers
INTEGER, INTENT(IN) :: TIME

! Local variables
INTEGER :: i, ierror
REAL    :: received_field_2d(nx_rivers, ny_rivers)

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_RECEIVE'

!------------------------------------------------------------------------------
! Loop in all variables that are received via coupling
DO i = 1, np_receive
  SELECT CASE(TRIM(cpl_receive(i)%field_name))
    ! receive the right field
  CASE ('sub_surf_roff')
    CALL oasis_get(cpl_receive(i)%field_id,TIME,received_field_2d,ierror)
    CALL twod_to_rp(received_field_2d, rivers%sub_surf_roff_rp, rivers)
  CASE ('surf_roff')
    CALL oasis_get(cpl_receive(i)%field_id,TIME,received_field_2d,ierror)
    CALL twod_to_rp(received_field_2d, rivers%surf_roff_rp, rivers)
  END SELECT

  IF (ierror /= oasis_recvd .AND. ierror /= oasis_fromrest .AND.               &
      ierror /= oasis_input .AND. ierror /= oasis_recvout  .AND.               &
      ierror /= oasis_fromrestout .AND. ierror /= oasis_ok ) THEN
    CALL log_fatal(RoutineName,                                                &
                 "oasis_get Error " //                                         &
    "(IOSTAT=" // TRIM(to_string(ierror)) // "): " //                          &
    "when receiving variable " // TRIM(cpl_receive(i)%field_name))
  END IF
END DO

RETURN

END SUBROUTINE oasis_receive
!
SUBROUTINE oasis_finalise()
!
USE mod_oasis,        ONLY: oasis_terminate
!
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Finalises the oasis coupler
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local variables
INTEGER :: ierror

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_FINALISE'

!------------------------------------------------------------------------------
! Coupling finalisation
CALL oasis_terminate(ierror)

IF (ierror /= 0) THEN
  CALL log_fatal(RoutineName,                                                  &
               "oasis_terminate Error " //                                     &
               "(IOSTAT=" // TRIM(to_string(ierror)) // ")")
ELSE
  CALL log_info(RoutineName,"oasis_terminate OK" )
END IF

RETURN

END SUBROUTINE oasis_finalise
#else
SUBROUTINE oasis_init()

USE jules_model_environment_mod, ONLY: l_oasis_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description: Stub routine for oasis_init.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Indicate that river coupling is not taking place
l_oasis_rivers = .FALSE.

END SUBROUTINE oasis_init

SUBROUTINE oasis_partition()
!
IMPLICIT NONE

! Local variables
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_PARTITION_STUB'

!-----------------------------------------------------------------------------
! Description: Stub routine for oasis_partition.
!              Run time controls should mean this routine is never
!              actually called.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CALL log_fatal(RoutineName, "**ERROR**: oasis_partition unavailable. " //      &
                         "Check RIVER_CPL fpp_defs key is set")

END SUBROUTINE oasis_partition

SUBROUTINE oasis_grid()
!
IMPLICIT NONE

! Local variables
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_GRID_STUB'

!-----------------------------------------------------------------------------
! Description: Stub routine for oasis_grid.
!              Run time controls should mean this routine is never
!              actually called.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CALL log_fatal(RoutineName, "**ERROR**: oasis_grid unavailable. " //           &
                         "Check RIVER_CPL fpp_defs key is set")

END SUBROUTINE oasis_grid

SUBROUTINE set_oasis_fields()
!
IMPLICIT NONE

! Local variables
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'SET_OASIS_FIELDS'

!-----------------------------------------------------------------------------
! Description: Stub routine for set_oasis_fields.
!              Run time controls should mean this routine is never
!              actually called.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CALL log_fatal(RoutineName, "**ERROR**: set_oasis_fields unavailable. " //     &
                         "Check RIVER_CPL fpp_defs key is set")

END SUBROUTINE set_oasis_fields

SUBROUTINE oasis_variables()
!
IMPLICIT NONE

! Local variables
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_VARIABLES'

!-----------------------------------------------------------------------------
! Description: Stub routine for oasis_variables.
!              Run time controls should mean this routine is never
!              actually called.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CALL log_fatal(RoutineName, "**ERROR**: oasis_variables unavailable. " //      &
                         "Check RIVER_CPL fpp_defs key is set")


END SUBROUTINE oasis_variables

SUBROUTINE oasis_send(rivers,TIME)
!
USE jules_rivers_mod, ONLY: rivers_type,rivers_data_type
!
IMPLICIT NONE

! Arguments with intent(in)
TYPE(rivers_type), INTENT(IN OUT) :: rivers
INTEGER, INTENT(IN) :: TIME

! Local variables
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_SEND'

!-----------------------------------------------------------------------------
! Description: Stub routine for oasis_send.
!              Run time controls should mean this routine is never
!              actually called.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CALL log_fatal(RoutineName, "**ERROR**: oasis_send unavailable. " //           &
                         "Check RIVER_CPL fpp_defs key is set")

END SUBROUTINE oasis_send

SUBROUTINE oasis_receive(rivers,TIME)
!
USE jules_rivers_mod, ONLY: rivers_type,rivers_data_type
!
IMPLICIT NONE

! Arguments with intent(in)
TYPE(rivers_type), INTENT(IN OUT) :: rivers
INTEGER, INTENT(IN) :: TIME

! Local variables
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_RECEIVE'

!-----------------------------------------------------------------------------
! Description: Stub routine for oasis_receive.
!              Run time controls should mean this routine is never
!              actually called.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CALL log_fatal(RoutineName, "**ERROR**: oasis_receive unavailable. " //        &
                         "Check RIVER_CPL fpp_defs key is set")

END SUBROUTINE oasis_receive

SUBROUTINE oasis_finalise()
!
IMPLICIT NONE

! Local variables
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'OASIS_FINALISE'

!-----------------------------------------------------------------------------
! Description: Stub routine for oasis_finalise.
!              Run time controls should mean this routine is never
!              actually called.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CALL log_fatal(RoutineName, "**ERROR**: oasis_finalise unavailable. " //       &
                         "Check RIVER_CPL fpp_defs key is set")

END SUBROUTINE oasis_finalise
#endif

END MODULE oasis_rivers_control_mod
