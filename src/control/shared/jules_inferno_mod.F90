! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_inferno_mod

USE missing_data_mod, ONLY: rmdi, imdi

!-----------------------------------------------------------------------------
! Description:
!   Contains inferno and l_trif_fire options and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------

! Items set in namelist jules_inferno.

INTEGER ::                                                                     &
  ignition_method = 1,                                                         &
      ! Switch for the calculation method of INFERNO fire ignitions
      ! IGNITION_METHOD=1:Constant (1.67 per km2 per s)
      ! IGNITION_METHOD=2:Constant (Human - 1.5 per km2 per s)
      !                   Varying  (Lightning - see Pechony and Shindell,2009)
      ! IGNITION_METHOD=3:Vary Human and Lightning (Pechony and Shindell,2009)
  flam_sm_func = 1
      ! Switch for the calculation method of INFERNO fire flammability
      ! FLAM_SM_FUNC=1:Linear
      ! FLAM_SM_FUNC=2:Exponential


LOGICAL ::                                                                     &
  l_inferno = .FALSE.,                                                         &
      ! Switch used to control whether the Interactive fire scheme is used
  l_trif_fire = .FALSE.
      ! Switch used to control whether interactive fire is used
      !   T => if l_inferno is also true, g_burn is calculated in INFERNO
!   and passed to TRIFFID to calculate emissions and vegetation
       !   dynamics
       !   T => if l_inferno is false, interactive fire is calculated via
!   ancillary if provided, and is 0 if not provided
       !   F => g_burn is calculated via ancillary if provided, and is 0 if
!   not provided

INTEGER, PARAMETER :: ignition_constant = 1
INTEGER, PARAMETER :: ignition_vary_natural = 2
INTEGER, PARAMETER :: ignition_vary_natural_human = 3


INTEGER :: errcode   ! error code to pass to ereport.

REAL(KIND=real_jlslsm) ::                                                      &
  flam_sm_low = rmdi,                                                          &
    ! Lower boundary to the soil moisture
  flam_sm_up = rmdi,                                                           &
    ! Upper boundary to the soil moisture
  flam_rhum_low = rmdi,                                                        &
    ! Lower boundary to the relative humidity
  flam_rhum_up = rmdi,                                                         &
    ! Upper boundary to the relative humidity
  flam_rain_const = rmdi,                                                      &
    ! Precipitation factor (-2(day/mm)*(kg/m2/s))
  flam_fuel_low = rmdi,                                                        &
    ! Lower boundary to the fuel density
  flam_fuel_up = rmdi,                                                         &
    ! Upper boundary to the fuel density
  triffire_ccdpm_min = rmdi,                                                   &
    ! Minimum decomposable plant material burn fraction
  triffire_ccdpm_max = rmdi,                                                   &
    ! Decomposable Plant Material burns between 80 to 100 %
  triffire_ccrpm_min = rmdi,                                                   &
    ! Minimum resistant plant material burn fraction
  triffire_ccrpm_max = rmdi,                                                   &
    ! Resistant Plant Material burns between 0 to 20 %
  z_burn_max = rmdi
    ! Parameter setting maximum depth of burn

!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------
NAMELIST  / jules_inferno/                                                     &
  l_trif_fire,                                                                 &
  l_inferno,                                                                   &
  ignition_method,                                                             &
  flam_sm_func, &
  flam_sm_low,                                                                 &
  flam_sm_up,                                                                  &
  flam_rhum_low,                                                               &
  flam_rhum_up,                                                                &
  flam_rain_const,                                                             &
  flam_fuel_low,                                                               &
  flam_fuel_up,                                                                &
  triffire_ccdpm_min,                                                          &
  triffire_ccdpm_max,                                                          &
  triffire_ccrpm_min,                                                          &
  triffire_ccrpm_max

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_INFERNO_MOD'

CONTAINS

SUBROUTINE check_jules_inferno()

USE ereport_mod, ONLY: ereport

USE jules_print_mgr, ONLY: jules_message
!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_INFERNO namelist for consistency and calculates some
!   derived values.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Local scalar parameters.
INTEGER :: errorstatus

CHARACTER(LEN=*), PARAMETER ::                                                 &
  RoutineName = 'CHECK_JULES_INFERNO'   ! Name of this procedure.

! Set error status to show a fatal error for all checks.
errorstatus = 101

! Check options that depend on TRIFFID if it is not enabled
!if l_triffie

IF ( .NOT. l_inferno .AND. .NOT. l_trif_fire ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "l_inferno needs to be specified if l_trif_fire is true.")
END IF

IF ( ABS(flam_sm_low - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_sm_low needs to be specified.")
ELSE IF ( flam_sm_low < 0.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_sm_low must be >= 0.0.")
END IF

IF ( ABS(flam_sm_up - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_sm_up needs to be specified.")
ELSE IF ( flam_sm_up < flam_sm_low ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_sm_up must be >= flam_sm_low")
ELSE IF ( flam_sm_up > 1.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_sm_up must be <= 1.0.")
END IF

IF ( ABS(flam_rhum_low - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_rhum_low needs to be specified.")
ELSE IF ( flam_rhum_low < 0.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_rhum_low must be >= 0.0.")
END IF

IF ( ABS(flam_rhum_up - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_rhum_up needs to be specified.")
ELSE IF ( flam_rhum_up < flam_rhum_low ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_rhum_up must be >= flam_rhum_low")
ELSE IF ( flam_rhum_up > 1.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_rhum_up must be <= 1.0.")
END IF

IF ( ABS(flam_fuel_low - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_fuel_low needs to be specified.")
ELSE IF ( flam_fuel_low < 0.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_fuel_low must be >= 0.0.")
END IF

IF ( ABS(flam_fuel_up - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_fuel_up needs to be specified.")
ELSE IF ( flam_fuel_up < flam_fuel_low ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_fuel_up must be >= flam_fuel_low")
ELSE IF ( flam_fuel_up > 1.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_fuel_up must be <= 1.0.")
END IF

IF ( ABS(triffire_ccdpm_min - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccdpm_min needs to be specified.")
ELSE IF ( triffire_ccdpm_min < 0.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccdpm_min must be >= 0.0.")
END IF

IF ( ABS(triffire_ccdpm_max - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccdpm_max needs to be specified.")
ELSE IF ( triffire_ccdpm_max < triffire_ccdpm_min ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccdpm_max must be >= triffire_ccdpm_min")
ELSE IF ( triffire_ccdpm_max > 1.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccdpm_max must be < 1.0.")
END IF

IF ( ABS(triffire_ccrpm_min - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccrpm_min needs to be specified.")
ELSE IF ( triffire_ccrpm_min < 0.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccrpm_min must be >= 0.0.")
END IF

IF ( ABS(triffire_ccrpm_max - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccrpm_max needs to be specified.")
ELSE IF ( triffire_ccrpm_max < triffire_ccrpm_min ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccrpm_max must be >= triffire_ccrpm_min")
ELSE IF ( triffire_ccrpm_max > 1.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "triffire_ccrpm_max must be < 1.0.")
END IF

IF ( ABS(flam_rain_const - rmdi) < EPSILON(rmdi) ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_rain_const needs to be specified.")
ELSE IF ( flam_rain_const < 0.0 ) THEN
  CALL ereport( TRIM(RoutineName), errorstatus,                                &
               "flam_rain_const must be >= 0.0.")
END IF

! Check a suitable ignition_method was given
IF ( ignition_method /= ignition_constant .AND.                                &
     ignition_method /= ignition_vary_natural .AND.                            &
     ignition_method /= ignition_vary_natural_human ) THEN
  errcode = 101
  CALL ereport("check_jules_vegetation", errcode,                              &
               'ignition_method must be 1, 2 or 3')
END IF

! check value of z_burn_max with l_layeredc
!IF ( l_layeredc ) THEN
!!  IF ( ABS( z_burn_max - rmdi ) > EPSILON(1.0) ) THEN
 !   IF ( z_burn_max <= 0.0 .OR. z_burn_max > 10.0 ) THEN
 !     CALL ereport(RoutineName, errorstatus,                                   &
 !                  "z_burn_max must be positive & less than 10 meters")
 !   END IF
 ! END IF
!END IF


END SUBROUTINE check_jules_inferno

SUBROUTINE print_nlist_jules_inferno()

USE jules_print_mgr, ONLY: jules_print
USE jules_surface_types_mod, ONLY: npft
! USE jules_soil_biogeochem_mod, ONLY: l_layeredc !ejb check that it is set or move this trap

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_inferno_mod',                                          &
                 'Contents of namelist jules_inferno')

WRITE(lineBuffer,*)' l_trif_fire = ',l_trif_fire
CALL jules_print('jules_inferno_mod',lineBuffer)

WRITE(lineBuffer,*)' l_inferno = ',l_inferno
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

WRITE(lineBuffer,*)' triffire_ccdpm_min = ',triffire_ccdpm_min
CALL jules_print('jules_inferno_mod',lineBuffer)

WRITE(lineBuffer,*)' triffire_ccdpm_max = ',triffire_ccdpm_max
CALL jules_print('jules_inferno_mod',lineBuffer)

WRITE(lineBuffer,*)' triffire_ccrpm_min = ',triffire_ccrpm_min
CALL jules_print('jules_inferno_mod',lineBuffer)

WRITE(lineBuffer,*)' triffire_ccrpm_max = ',triffire_ccrpm_max
CALL jules_print('jules_inferno_mod',lineBuffer)

WRITE(lineBuffer, *) ' z_burn_max = ', z_burn_max
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

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
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_INFERNO'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 2
INTEGER, PARAMETER :: n_real = 11
INTEGER, PARAMETER :: n_log = 2

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: ignition_method
  INTEGER :: flam_sm_func
  REAL(KIND=real_jlslsm) :: flam_sm_low
  REAL(KIND=real_jlslsm) :: flam_sm_up
  REAL(KIND=real_jlslsm) :: flam_rhum_low
  REAL(KIND=real_jlslsm) :: flam_rhum_up
  REAL(KIND=real_jlslsm) :: flam_rain_const
  REAL(KIND=real_jlslsm) :: flam_fuel_low
  REAL(KIND=real_jlslsm) :: flam_fuel_up
  REAL(KIND=real_jlslsm) :: triffire_ccdpm_min
  REAL(KIND=real_jlslsm) :: triffire_ccdpm_max
  REAL(KIND=real_jlslsm) :: triffire_ccrpm_min
  REAL(KIND=real_jlslsm) :: triffire_ccrpm_max
  REAL(KIND=real_jlslsm) :: z_burn_max
  LOGICAL :: l_trif_fire
  LOGICAL :: l_inferno
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
  my_nml % flam_sm_func = flam_sm_func
  my_nml % l_trif_fire     = l_trif_fire
  my_nml % l_inferno       = l_inferno
  my_nml % flam_sm_low     = flam_sm_low
  my_nml % flam_sm_up      = flam_sm_up
  my_nml % flam_rhum_low   = flam_rhum_low
  my_nml % flam_rhum_up    = flam_rhum_up
  my_nml % flam_rain_const = flam_rain_const
  my_nml % flam_fuel_low   = flam_fuel_low
  my_nml % flam_fuel_up    = flam_fuel_up
  my_nml % triffire_ccdpm_min = triffire_ccdpm_min
  my_nml % triffire_ccdpm_max = triffire_ccdpm_max
  my_nml % triffire_ccrpm_min = triffire_ccrpm_min
  my_nml % triffire_ccrpm_max = triffire_ccrpm_max
  my_nml % z_burn_max       = z_burn_max
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
  triffire_ccdpm_min = my_nml % triffire_ccdpm_min
  triffire_ccdpm_max = my_nml % triffire_ccdpm_max
  triffire_ccrpm_min = my_nml % triffire_ccrpm_min
  triffire_ccrpm_max = my_nml % triffire_ccrpm_max
  z_burn_max       = my_nml % z_burn_max
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_inferno
#endif

END MODULE jules_inferno_mod
