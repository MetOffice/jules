! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE oasis_rivers_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains river coupling options and a namelist for setting them
!   This currently holds both "physics" and control variables
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi
USE io_constants, ONLY: max_file_name_len

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module types
!-----------------------------------------------------------------------------
TYPE                :: cpl_field
  CHARACTER(LEN=20) :: field_name          ! Name of the coupling field
  INTEGER           :: field_id            ! Field ID
END TYPE cpl_field
!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
INTEGER, PARAMETER        :: maxFields = 2      ! Maximum number of coupling fields
!-----------------------------------------------------------------------------
! Logical parameters.
!-----------------------------------------------------------------------------
! Is 1d runoff coupling partition used?
LOGICAL, PUBLIC           :: l_runoff_1d = .FALSE.
!-----------------------------------------------------------------------------
! Array parameters.
!-----------------------------------------------------------------------------
! Coupling fields
TYPE(cpl_field), DIMENSION(maxFields), PUBLIC :: cpl_receive, cpl_send
!-----------------------------------------------------------------------------
! Items set in namelist
!-----------------------------------------------------------------------------
INTEGER, PUBLIC ::                                                             &
   np_receive = imdi,                                                          &
                            ! Number of oasis fields to receive
   np_send = imdi,                                                             &
                            ! Number of oasis fields to send
   cpl_freq = imdi
                            ! Coupling frequency in seconds
CHARACTER(LEN=20), PUBLIC ::                                                   &
   send_fields(maxFields)='',                                                  &
                            ! List of fields sent via coupling
   receive_fields(maxFields)=''
                            ! List of fields received via coupling
!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------

NAMELIST  / oasis_rivers /                                                     &
  np_receive, np_send, cpl_freq, send_fields, receive_fields

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='OASIS_RIVERS_MOD'

!-----------------------------------------------------------------------------

CONTAINS

SUBROUTINE check_oasis_rivers()

USE jules_rivers_mod,            ONLY: l_outflow_per_river
USE jules_model_environment_mod, ONLY: l_oasis_rivers
USE model_time_mod,              ONLY: timestep_len
USE ereport_mod,                 ONLY: ereport
USE jules_print_mgr,             ONLY: jules_message

!-----------------------------------------------------------------------------
! Description:
!   Checks OASIS_RIVERS namelist for consistency, and:
!   Finds what fields need to be coupled
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: i, errcode

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_OASIS_RIVERS'

IF ( .NOT. l_oasis_rivers ) THEN
  IF ( cpl_freq /= imdi ) THEN
    errcode = 101
    CALL ereport(RoutineName, errcode,                                         &
       "Rivers-standalone: cpl_freq should not been set")
  END IF
ELSE
  ! Check that a valid number of send and receive fields was given
  IF (np_receive == imdi) THEN
    errcode = 102
    CALL ereport(RoutineName, errcode, 'np_receive has not been set')
  ELSE IF (np_receive < 0) THEN
    errcode = 103
    CALL ereport(RoutineName, errcode, 'np_receive must be >= 0')
  ELSE IF (np_receive > 2) THEN
    errcode = 104
    CALL ereport(RoutineName, errcode, 'np_receive must be <= 2')
  ELSE IF (np_receive /=2) THEN
    errcode = 105
    CALL ereport(RoutineName, errcode, 'only fully coupled mode ' //           &
                 '(np_receive=2) is supported')
  END IF
  IF (np_send == imdi) THEN
    errcode = 106
    CALL ereport(RoutineName, errcode, 'np_send has not been set')
  ELSE IF (np_send < 0) THEN
    errcode = 107
    CALL ereport(RoutineName, errcode, 'np_send must be >= 0')
  ELSE IF (np_send > 1) THEN
    errcode = 108
    CALL ereport(RoutineName, errcode, 'np_send must be ' //                   &
                 'in the range 0:1')
  END IF

  ! Check that at least one coupling field is exchanged
  IF ( np_receive == 0 .AND. np_send == 0 ) THEN
    errcode = 109
    CALL ereport(RoutineName, errcode,                                         &
                    'no send or receive coupling fields')
  END IF

  ! Check that a valid integer coupling frequency was given
  IF ( cpl_freq == imdi ) THEN
    errcode = 110
    CALL ereport(RoutineName, errcode, 'cpl_freq has not been set')
  ELSE IF ( cpl_freq <= 0 ) THEN
    errcode = 111
    CALL ereport(RoutineName, errcode, 'cpl_freq must be > 0')
  ELSE IF ( (MOD(cpl_freq, timestep_len) /= 0 ) ) THEN
    errcode = 112
    CALL ereport(RoutineName, errcode,                                         &
                 'cpl_freq must be a multiple of the timestep length')
  END IF

  ! Check that correct number of send fields have been given
  IF ( COUNT( LEN_TRIM(send_fields(:)) /= 0 ) /= np_send ) THEN
    errcode = 113
    WRITE(jules_message,*)                                                     &
       "Number of expected send fields (np_send) " //                          &
       "do not match those given (send_fields)."
    CALL ereport(RoutineName, errcode, jules_message)
  END IF

  ! Check that correct number of receive fields have been given
  IF ( COUNT( LEN_TRIM(receive_fields(:)) /= 0 ) /= np_receive ) THEN
    errcode = 114
    WRITE(jules_message,*)                                                     &
       "Number of expected receive fields (np_receive) " //                    &
       "do not match those given (receive_fields)."
    CALL ereport(RoutineName, errcode, jules_message)
  END IF

  ! Check send fields
  DO i = 1, np_send
    IF ( COUNT( send_fields(1:np_send) == send_fields(i) ) /= 1 ) THEN
      errcode = 115
      CALL ereport(RoutineName, errcode,                                       &
                   "Duplicated send coupled field " // TRIM(send_fields(i)) )
    END IF

    SELECT CASE(TRIM(send_fields(i)))
    CASE ('outflow_per_river')
      cpl_send(i)%field_name = TRIM(send_fields(i))
      l_runoff_1d         = .TRUE. ! 1d runoff partition required
      l_outflow_per_river = .TRUE. ! Check river number ancillary is given
    CASE DEFAULT
      errcode = 117
      CALL ereport(RoutineName, errcode,                                       &
                "The requested send coupled field '" //                        &
                TRIM(send_fields(i)) // "' is not recognized" )
    END SELECT
  END DO

  ! Check receive fields
  DO i=1, np_receive
    IF ( COUNT(receive_fields(1:np_receive) == receive_fields(i) ) /= 1) THEN
      errcode = 118
      CALL ereport(RoutineName, errcode,                                       &
                   "Duplicated receive coupled field " //                      &
                   TRIM(receive_fields(i)) )
    END IF

    SELECT CASE(TRIM(receive_fields(i)))
    CASE ('sub_surf_roff', 'surf_roff')
      cpl_receive(i)%field_name = TRIM(receive_fields(i))
    CASE DEFAULT
      errcode = 119
      CALL ereport(RoutineName, errcode,                                       &
                   "The requested receive coupled field '" //                  &
                   TRIM(receive_fields(i)) // "' is not recognized" )
    END SELECT
  END DO

END IF

RETURN

END SUBROUTINE check_oasis_rivers

SUBROUTINE print_nlist_oasis_rivers()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE
INTEGER :: i
CHARACTER(LEN=50000) :: lineBuffer

CHARACTER(LEN=*), PARAMETER :: RoutineName='PRINT_NLIST_OASIS_RIVERS'

CALL jules_print('oasis_rivers_mod',                                           &
   'Contents of namelist oasis_rivers')

WRITE(lineBuffer,*)' cpl_freq = ', cpl_freq
CALL jules_print('oasis_rivers',lineBuffer)
WRITE(lineBuffer,*)' send_fields = ', (TRIM(send_fields(i))//',',i=1,maxFields)
CALL jules_print('oasis_rivers',lineBuffer)
WRITE(lineBuffer,*)' receive_fields = ',                                       &
                                   (TRIM(receive_fields(i))//',',i=1,maxFields)
CALL jules_print('oasis_rivers',lineBuffer)

CALL jules_print('oasis_rivers_mod',                                           &
   '- - - - - - end of namelist - - - - - -')

RETURN

END SUBROUTINE print_nlist_oasis_rivers

#if !defined(UM_JULES)
SUBROUTINE read_nml_oasis_rivers(nml_dir)

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_OASIS_RIVERS'

CALL log_info("oasis_rivers", "Reading OASIS_RIVERS namelist...")

OPEN(namelist_unit,                                                            &
     FILE=(TRIM(nml_dir) // '/' // 'oasis_rivers.nml'),                        &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file oasis_rivers.nml " //            &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

READ(namelist_unit, NML = oasis_rivers, IOSTAT = ERROR,                        &
   IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist OASIS_RIVERS " //                     &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file oasis_rivers.nml " //            &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF


END SUBROUTINE read_nml_oasis_rivers
#endif

END MODULE oasis_rivers_mod

!-----------------------------------------------------------------------------
