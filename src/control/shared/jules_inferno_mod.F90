! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_inferno_mod

! -----------------------------------------------------------------------------
! Description:
!   Contains switches and other inputs for l_inferno and l_trif_fire options
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
! -----------------------------------------------------------------------------

USE um_types,         ONLY: real_jlslsm
USE missing_data_mod, ONLY: rmdi, imdi

IMPLICIT NONE

LOGICAL ::                                                                     &
  l_inferno = .FALSE.,                                                         &
      ! Switch used to control whether inferno fire scheme is used to calculate
      !   flammability and burnt area
  l_trif_fire = .FALSE.
      ! Switch used to control whether fire is part of the carbon cycle
      !   T => if l_inferno is also true, g_burn is calculated in INFERNO
      !   passed to TRIFFID to calculate emissions and vegetation dynamics
      !   T => if l_inferno is false, interactive fire is calculated via
      !   ancillary if provided, and is 0 if not provided
      !   F => g_burn is calculated via ancillary if provided, and is 0 if
      !   not provided


INTEGER ::                                                                     &
  ignition_method = imdi,                                                      &
      ! Switch for the calculation method of INFERNO fire ignitions
      !   IGNITION_METHOD=1:Constant (1.67 per km2 per s)
      !   IGNITION_METHOD=2:Constant (Human - 1.5 per km2 per s)
      !                     Varying  (Lightning - see Pechony & Shindell,2009)
      !   IGNITION_METHOD=3:Vary Human and Lightning (Pechony & Shindell,2009)
  flam_sm_func = 1
      ! Switch for relationship between INFERNO fire
      !      flammability and soil moisture
      ! FLAM_SM_FUNC=1:Linear (doesnt require flam_sm_low / flam_sm_up)
      ! FLAM_SM_FUNC=2:Exponential


REAL(KIND=real_jlslsm) ::                                                      &
  flam_sm_low = rmdi,                                                          &
    ! Below this soil moisture, flammability is 1.0 (flam_sm_func=2)
    ! Expressed as a fraction of saturation (between 0 and 1)
  flam_sm_up = rmdi,                                                           &
    ! Exponential decay parameter for relationship between soil moisture
    ! and flammability (flam_sm_func=2)
  flam_rhum_low = rmdi,                                                        &
    ! Lower boundary to the relative humidity (%,  between 0 and 100 %)
  flam_rhum_up = rmdi,                                                         &
    ! Upper boundary to the relative humidity (%,  between 0 and 100 %)
  flam_rain_const = rmdi,                                                      &
    ! Precipitation factor (2(day/mm)*(kg/m2/s)  WRONG UNITS)
  flam_fuel_low = rmdi,                                                        &
    ! Lower boundary to the fuel density (UNITS)
  flam_fuel_up = rmdi,                                                         &
    ! Upper boundary to the fuel density (UNITS)
  ccdpm_min = rmdi,                                                            &
    ! Minimum decomposable plant material burn fraction (0 <= fraction <= 1)
  ccdpm_max = rmdi,                                                            &
    ! Decomposable Plant Material burn fraction (0 <= fraction <= 1)
  ccrpm_min = rmdi,                                                            &
    ! Minimum resistant plant material urn fraction (0 <= fraction <= 1)
  ccrpm_max = rmdi,                                                            &
    ! Resistant Plant Material burn fraction (0 <= fraction <= 1)
  z_burn_max = rmdi
    ! Parameter setting maximum depth of burn (m)

INTEGER, PARAMETER :: ignition_constant = 1
INTEGER, PARAMETER :: ignition_vary_natural = 2
INTEGER, PARAMETER :: ignition_vary_natural_human = 3

!-----------------------------------------------------------------------
! Set up a namelist to allow switches to be set.
!-----------------------------------------------------------------------
NAMELIST  / jules_inferno/                                                     &
  l_trif_fire, l_inferno, ignition_method, flam_sm_func,                       &
  flam_sm_low, flam_sm_up, flam_rhum_low, flam_rhum_up,                        &
  flam_rain_const, flam_fuel_low, flam_fuel_up,                                &
  ccdpm_min, ccdpm_max,  ccrpm_min, ccrpm_max, z_burn_max

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_INFERNO_MOD'

CONTAINS

SUBROUTINE check_jules_inferno()


!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_INFERNO namelist for consistency and calculates some
!   derived values.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!-----------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_print, jules_message

USE logging_mod, ONLY: log_info

IMPLICIT NONE

INTEGER :: errorstatus


CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_INFERNO'

errorstatus = 101

IF ( l_inferno ) THEN
  ! Check a suitable ignition_method was given
  IF ( ignition_method /= ignition_constant .AND.                              &
      ignition_method /= ignition_vary_natural .AND.                           &
      ignition_method /= ignition_vary_natural_human ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
              'ignition_method must be 1, 2 or 3')
  END IF

  ! Check a suitable flam_sm_func was given
  IF ( flam_sm_func /= 1 .AND. flam_sm_func /= 2 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
              'flam_sm_func must be 1 or 2')
  END IF

  IF ( flam_sm_func == 2 ) THEN
    IF ( ABS(flam_sm_low - rmdi) < EPSILON(rmdi) ) THEN
      CALL ereport( TRIM(RoutineName), errorstatus,                            &
                  "flam_sm_low needs to be specified.")
    ELSE IF ( flam_sm_low < 0.0 .OR. flam_sm_low > 1.0 ) THEN
      CALL ereport( TRIM(RoutineName), errorstatus,                            &
                  "flam_sm_low must be >= 0.0 and <= 1.0.")
    END IF

    IF ( ABS(flam_sm_up - rmdi) < EPSILON(rmdi) ) THEN
      CALL ereport( TRIM(RoutineName), errorstatus,                            &
                  "flam_sm_up needs to be specified.")
    ELSE IF ( flam_sm_up < 0.0 ) THEN
      CALL ereport( TRIM(RoutineName), errorstatus,                            &
                  "flam_sm_up must be >= 0.0")
    END IF
  END IF

  IF ( ABS(flam_rhum_low - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_rhum_low needs to be specified.")
  ELSE IF ( flam_rhum_low < 0.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_rhum_low must be >= 0.0.")
  END IF

  IF ( ABS(flam_rhum_up - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_rhum_up needs to be specified.")
  ELSE IF ( flam_rhum_up <= flam_rhum_low ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_rhum_up must be >= flam_rhum_low")
  ELSE IF ( flam_rhum_up > 100.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_rhum_up must be <= 100.0.")
  END IF

  IF ( ABS(flam_fuel_low - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_fuel_low needs to be specified.")
  ELSE IF ( flam_fuel_low < 0.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_fuel_low must be >= 0.0.")
  END IF

  IF ( ABS(flam_fuel_up - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_fuel_up needs to be specified.")
  ELSE IF ( flam_fuel_up < flam_fuel_low ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_fuel_up must be > flam_fuel_low")
  ELSE IF ( flam_fuel_up > 1.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_fuel_up must be <= 1.0.")
  END IF

  IF ( ABS(flam_rain_const - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_rain_const needs to be specified.")
  ELSE IF ( flam_rain_const < 0.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "flam_rain_const must be >= 0.0.")
  END IF
END IF ! end of l_inferno check


IF ( l_trif_fire ) THEN
  IF ( ABS(ccdpm_min - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccdpm_min needs to be specified.")
  ELSE IF ( ccdpm_min < 0.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccdpm_min must be >= 0.0.")
  END IF

  IF ( ABS(ccdpm_max - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccdpm_max needs to be specified.")
  ELSE IF ( ccdpm_max < ccdpm_min ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccdpm_max must be >= ccdpm_min")
  ELSE IF ( ccdpm_max > 1.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccdpm_max must be < 1.0.")
  END IF

  IF ( ABS(ccrpm_min - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccrpm_min needs to be specified.")
  ELSE IF ( ccrpm_min < 0.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccrpm_min must be >= 0.0.")
  END IF

  IF ( ABS(ccrpm_max - rmdi) < EPSILON(rmdi) ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccrpm_max needs to be specified.")
  ELSE IF ( ccrpm_max < ccrpm_min ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccrpm_max must be >= ccrpm_min")
  ELSE IF ( ccrpm_max > 1.0 ) THEN
    CALL ereport( TRIM(RoutineName), errorstatus,                              &
                "ccrpm_max must be < 1.0.")
  END IF

END IF  ! end of l_trif_fire check


END SUBROUTINE check_jules_inferno


SUBROUTINE print_nlist_jules_inferno()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_inferno', 'Contents of namelist jules_inferno')

CALL jules_print('jules_inferno_mod',                                          &
                 'Contents of namelist jules_inferno')

WRITE(lineBuffer,*)' l_trif_fire = ',l_trif_fire
CALL jules_print('jules_inferno_mod',lineBuffer)

WRITE(lineBuffer,*)' l_inferno = ',l_inferno
CALL jules_print('jules_inferno_mod',lineBuffer)

IF ( l_inferno ) THEN
  WRITE(lineBuffer,*)' ignition_method = ',ignition_method
  CALL jules_print('jules_inferno_mod',lineBuffer)

  IF (ignition_method == ignition_constant ) THEN
    WRITE(lineBuffer,*)'Constant or ubiquitous ignitions (l_inferno=T)'
  ELSE IF (ignition_method == ignition_vary_natural ) THEN
    WRITE(lineBuffer,*)'Constant human ignitions, varying lightning (l_inferno=T)'
  ELSE IF (ignition_method == ignition_vary_natural_human ) THEN
    WRITE(lineBuffer,*)'Fully prescribed ignitions (l_inferno=T)'
  END IF

  WRITE(lineBuffer,*)' flam_sm_func = ',flam_sm_func
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' flam_sm_low = ',flam_sm_low
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' flam_sm_up = ',flam_sm_up
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' flam_rhum_low = ',flam_rhum_low
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' flam_rhum_up = ',flam_rhum_up
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' flam_rain_const = ',flam_rain_const
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' flam_fuel_low = ',flam_fuel_low
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' flam_fuel_up = ',flam_fuel_up
  CALL jules_print('jules_inferno_mod',lineBuffer)
END IF

IF ( l_trif_fire ) THEN
  WRITE(lineBuffer,*)' ccdpm_min = ',ccdpm_min
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' ccdpm_max = ',ccdpm_max
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' ccrpm_min = ',ccrpm_min
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer,*)' ccrpm_max = ',ccrpm_max
  CALL jules_print('jules_inferno_mod',lineBuffer)

  WRITE(lineBuffer, *) ' z_burn_max = ', z_burn_max
  CALL jules_print('jules_inferno_mod', lineBuffer)
END IF

CALL jules_print('jules_inferno_mod',                                          &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_inferno

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_inferno (unitnumber)

! Description:
!  Read the JULES_INFERNO namelist

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
INTEGER :: errorstatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*),   PARAMETER :: RoutineName='READ_NML_JULES_INFERNO'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 2
INTEGER, PARAMETER :: n_real = 12
INTEGER, PARAMETER :: n_log = 2

TYPE :: my_namelist
  SEQUENCE
  REAL(KIND=real_jlslsm) :: flam_sm_low
  REAL(KIND=real_jlslsm) :: flam_sm_up
  REAL(KIND=real_jlslsm) :: flam_rhum_low
  REAL(KIND=real_jlslsm) :: flam_rhum_up
  REAL(KIND=real_jlslsm) :: flam_rain_const
  REAL(KIND=real_jlslsm) :: flam_fuel_low
  REAL(KIND=real_jlslsm) :: flam_fuel_up
  REAL(KIND=real_jlslsm) :: ccdpm_min
  REAL(KIND=real_jlslsm) :: ccdpm_max
  REAL(KIND=real_jlslsm) :: ccrpm_min
  REAL(KIND=real_jlslsm) :: ccrpm_max
  REAL(KIND=real_jlslsm) :: z_burn_max
  LOGICAL :: l_trif_fire
  LOGICAL :: l_inferno
  INTEGER :: ignition_method
  INTEGER :: flam_sm_func
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_inferno, IOSTAT = errorstatus,          &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_inferno", iomessage)

  my_nml % ignition_method = ignition_method
  my_nml % flam_sm_func    = flam_sm_func
  my_nml % l_trif_fire     = l_trif_fire
  my_nml % l_inferno       = l_inferno
  my_nml % flam_sm_low     = flam_sm_low
  my_nml % flam_sm_up      = flam_sm_up
  my_nml % flam_rhum_low   = flam_rhum_low
  my_nml % flam_rhum_up    = flam_rhum_up
  my_nml % flam_rain_const = flam_rain_const
  my_nml % flam_fuel_low   = flam_fuel_low
  my_nml % flam_fuel_up    = flam_fuel_up
  my_nml % ccdpm_min = ccdpm_min
  my_nml % ccdpm_max = ccdpm_max
  my_nml % ccrpm_min = ccrpm_min
  my_nml % ccrpm_max = ccrpm_max
  my_nml % z_burn_max         = z_burn_max
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  ignition_method = my_nml % ignition_method
  flam_sm_func    = my_nml % flam_sm_func
  l_trif_fire     = my_nml % l_trif_fire
  l_inferno       = my_nml % l_inferno
  flam_sm_low     = my_nml % flam_sm_low
  flam_sm_up      = my_nml % flam_sm_up
  flam_rhum_low   = my_nml % flam_rhum_low
  flam_rhum_up    = my_nml % flam_rhum_up
  flam_rain_const = my_nml % flam_rain_const
  flam_fuel_low   = my_nml % flam_fuel_low
  flam_fuel_up    = my_nml % flam_fuel_up
  ccdpm_min = my_nml % ccdpm_min
  ccdpm_max = my_nml % ccdpm_max
  ccrpm_min = my_nml % ccrpm_min
  ccrpm_max = my_nml % ccrpm_max
  z_burn_max         = my_nml % z_burn_max
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_inferno
#endif

#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_inferno(nml_dir)

!-----------------------------------------------------------------------------
! Description:
!  Read the JULES_INFERNO namelist (standalone)
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!-----------------------------------------------------------------------------

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

! Open the fire namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'fire.nml'),                 &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_inferno", "Error opening namelist file fire.nml " //    &
                 "Error opening namelist file fire.nml " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! There is one namelist to read from this file for jules inferno
CALL log_info("init_inferno", "Reading JULES_INFERNO namelist...")
READ(namelist_unit, NML = jules_inferno, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_inferno",                                               &
                 "Error reading namelist JULES_INFERNO " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_inferno",                                               &
                 "Error closing namelist file fire.nml " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_inferno
#endif

END MODULE jules_inferno_mod
