! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_model_environment_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains model interface options. Not all JULES options are available to all
!   environments in which JULES is run e.g. standalone, UM, CABLE, LIS, MONC.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi
#if !defined(UM_JULES) && !defined(LFRIC) && !defined(RIVERS_ONLY)
USE update_mod, ONLY: l_imogen
USE imogen_run, ONLY: l_daily_metdata_climatol
#endif

IMPLICIT NONE

PRIVATE
! Please do not make this namelist public. The only place this namelist
! should be used is in this module. Please see the JULES Working Practices.
! If required new items added can be made public, but as a default it should be
! private; l_jules_parent should not have science options attached.
PUBLIC :: print_nlist_jules_model_environment,                                 &
#if !defined(LFRIC)
          read_nml_jules_model_environment,                                    &
#endif
          check_jules_model_environment


INTEGER :: l_jules_parent = imdi ! Switch to identify UM-JULES environment
INTEGER, PUBLIC :: lsm_id = imdi ! Switch to identify land surface model
INTEGER, PARAMETER, PUBLIC :: jules = 1
INTEGER, PARAMETER, PUBLIC :: cable = 2
INTEGER, PARAMETER, PUBLIC :: rivers = 3
CHARACTER(LEN=6), DIMENSION(3) :: lsm_name = [ 'JULES ', 'CABLE ', 'RIVERS' ]

LOGICAL, PUBLIC :: l_oasis_rivers  = .FALSE.
                             ! Internal switch to indicate OASIS-Rivers coupling

NAMELIST  / jules_model_environment / l_jules_parent, lsm_id

#if !defined(UM_JULES) && !defined(LFRIC) && !defined(RIVERS_ONLY)
NAMELIST  / imogen_onoff_switch / l_imogen, l_daily_metdata_climatol
#endif

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_MODEL_ENVIRONMENT_MOD'

CONTAINS

SUBROUTINE check_jules_model_environment()

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_MODEL_ENVIRONMENT namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_message, jules_print

IMPLICIT NONE

! Options for defining JULES parent models
INTEGER, PARAMETER ::                                                          &
  jules_standalone = 0,                                                        &
  um_jules         = 1,                                                        &
  oasis_coupler    = 2

INTEGER :: errcode   ! error code to pass to ereport.
CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_MODEL_ENVIRONMENT'

!-----------------------------------------------------------------------------

! Check that l_jules_parent is consistent with UM_JULES ifdef.
! MONC_JULES (jules:#347) should be added here when available.
#if defined(UM_JULES)
IF ( l_jules_parent /= um_jules ) THEN
  errcode = 10
  WRITE(jules_message,'(A,I0)')                                                &
     "l_jules_parent should be 1 for UM run. l_jules_parent = ",               &
     l_jules_parent
  CALL ereport(RoutineName, errcode, jules_message )
END IF
#elif defined(RIVER_CPL)
IF ( l_jules_parent /= oasis_coupler ) THEN
  errcode = 20
  WRITE(jules_message,'(A,I0)')                                                &
     "l_jules_parent should be 2 for OASIS coupler. l_jules_parent = ",        &
     l_jules_parent
  CALL ereport(RoutineName, errcode, jules_message )
END IF
#else
IF ( l_jules_parent /= jules_standalone ) THEN
  errcode = 30
  WRITE(jules_message,'(A,I0)')                                                &
     "l_jules_parent should be 0 for standalone. l_jules_parent = ",           &
     l_jules_parent
  CALL ereport(RoutineName, errcode, jules_message )
END IF
#endif

IF ( l_jules_parent == imdi ) THEN
  errcode = 40
  WRITE(jules_message,'(A,I0)') "l_jules_parent is currently unset."
  CALL ereport(RoutineName, errcode, jules_message )
END IF

!-----------------------------------------------------------------------------
! Check if land surface model id is valid
!-----------------------------------------------------------------------------
SELECT CASE ( lsm_id )
CASE ( jules, cable, rivers )
  ! Allowed LSMs
  WRITE(jules_message,'(A)') "Land surface model selected is " //              &
     lsm_name(lsm_id)
  CALL jules_print(RoutineName, jules_message)
  IF ( l_jules_parent == oasis_coupler ) THEN
    IF ( lsm_id /= rivers ) THEN
      errcode = 50
      WRITE(jules_message,'(A)')                                               &
         "OASIS coupler can only be used with Rivers-only."
      CALL ereport(RoutineName, errcode, jules_message )
    ELSE IF ( .NOT. l_oasis_rivers ) THEN
      errcode = 60
      WRITE(jules_message,'(A)')                                               &
         "Internal switch l_oasis_rivers has been set incorrectly"
      CALL ereport(RoutineName, errcode, jules_message )
    END IF
  END IF
CASE DEFAULT
  errcode = 70
  WRITE(jules_message,'(A,I0)') "Invalid LSM ID provided. lsm_id = ", lsm_id
  CALL ereport(RoutineName, errcode, jules_message )
END SELECT

END SUBROUTINE check_jules_model_environment

!-----------------------------------------------------------------------------

SUBROUTINE print_nlist_jules_model_environment()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000)        :: lineBuffer
CHARACTER(LEN=*), PARAMETER :: RoutineName='PRINT_NLIST_JULES_MODEL_ENVIRONMENT'

CALL jules_print(RoutineName, 'Contents of namelist jules_model_environment')

WRITE(lineBuffer, '(A,I0)') 'l_jules_parent = ', l_jules_parent
CALL jules_print(RoutineName, lineBuffer)

WRITE(lineBuffer, '(A,I0)') 'lsm_id = ', lsm_id
CALL jules_print(RoutineName, lineBuffer)

CALL jules_print(RoutineName, '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_model_environment

!-----------------------------------------------------------------------------

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_model_environment (unitnumber)

! Description:
!  Read the jules_model_environment namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_MODEL_ENVIRONMENT'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 1
INTEGER, PARAMETER :: n_int = 2

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: l_jules_parent
  INTEGER :: lsm_id
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_model_environment,                      &
        IOSTAT = errorstatus, IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_model_environment", iomessage)

  my_nml % l_jules_parent = l_jules_parent
  my_nml % lsm_id         = lsm_id

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  l_jules_parent          = my_nml % l_jules_parent
  lsm_id                  = my_nml % lsm_id

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_model_environment
#endif

#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_model_environment(nml_dir)

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
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_MODEL_ENVIRONMENT'

CALL log_info(RoutineName, "Reading JULES_MODEL_ENVIRONMENT namelist...")

OPEN(namelist_unit,                                                            &
     FILE=(TRIM(nml_dir) // '/' // 'model_environment.nml'),                   &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file model_environment.nml " //       &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = jules_model_environment, IOSTAT = ERROR,             &
   IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
   CALL log_fatal(RoutineName,                                                 &
                 "Error reading namelist JULES_MODEL_ENVIRONMENT " //          &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
   CALL log_fatal(RoutineName,                                                 &
                 "Error closing namelist file model_environment.nml " //       &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")


! read imogen switch - not relevant for UM/LFric
#if !defined(LFRIC)  && !defined(RIVERS_ONLY)
CALL log_info(RoutineName, "Reading IMOGEN_ONOFF_SWITCH namelist...")

OPEN(namelist_unit,                                                            &
     FILE=(TRIM(nml_dir) // '/' // 'imogen.nml'),                              &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR,           &
     IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file imogen.nml " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

READ(namelist_unit, NML = imogen_onoff_switch, IOSTAT = ERROR,                 &
   IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
   CALL log_fatal(RoutineName,                                                 &
                 "Error reading namelist IMOGEN_ONOFF_SWITCH " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
   CALL log_fatal(RoutineName,                                                 &
                 "Error closing namelist file imogen.nml " //                  &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
#endif
RETURN
END SUBROUTINE read_nml_jules_model_environment
#endif

END MODULE jules_model_environment_mod
