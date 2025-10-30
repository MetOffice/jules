#if !defined(UM_JULES)
!-----------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

MODULE read_cable_progs_mod

!------------------------------------------------------------------------------
! Description:
!   Reads in information about CABLE prognostic variables for their
!   initialisation
!
! This MODULE is USEd in:
!      init_cable_progs.F90
!
! This MODULE contains 1 public Subroutine:
!      read_cable_progs
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in CABLE SCIENCE
!------------------------------------------------------------------------------

IMPLICIT NONE

PUBLIC :: read_cable_progs
PRIVATE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_CABLE_PROGS_MOD'

CONTAINS

SUBROUTINE read_cable_progs()

! Description:
!   Nothing further to add to the module description

USE errormessagelength_mod, ONLY: errormessagelength

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE string_utils_mod, ONLY: to_string

USE templating_mod, ONLY: tpl_has_var_name, tpl_substitute_var

USE model_interface_mod, ONLY: identifier_len, populate_var, get_var_id

USE ancil_info, ONLY: land_pts

USE missing_data_mod, ONLY: rmdi

USE logging_mod, ONLY: log_info, log_warn, log_fatal

IMPLICIT NONE

! Work variables
INTEGER, PARAMETER :: max_cable_vars  = 10
       ! The maximum number of CABLE model variables that can be given

INTEGER :: nvars_required      ! The number of prognostic variables that are
                               ! required in this configuration

CHARACTER(LEN=identifier_len) :: required_vars(max_cable_vars)
                               ! The variable identifiers of the required
                               ! variables

INTEGER :: nvars_file       ! The number of variables that will be set
                            ! from the given file (template?)

INTEGER :: i,l  ! Index variables

INTEGER :: ERROR  ! Error indicator

!! Variables passed to fill_variables_from_file
!CHARACTER(LEN=identifier_len) :: file_var(max_cable_vars)
!                      ! The variable identifiers of the variables to set
!                      ! from file
!CHARACTER(LEN=max_sdf_name_len) :: file_var_name(max_cable_vars)
!                      ! The name of each variable in the file
!
!CHARACTER(LEN=max_sdf_name_len) :: file_tpl_name(max_cable_vars)
!                      ! The name to substitute in a template for each
!                      ! variable


!-----------------------------------------------------------------------------
! Definition of the cable_progs namelist
!-----------------------------------------------------------------------------
CHARACTER(LEN=max_file_name_len) :: FILE
                      ! The name of the file (or variable name template) to
                      ! use for variables that need to be filled from file

INTEGER :: nvars      ! The number of variables in this section

CHARACTER(LEN=identifier_len) :: var(max_cable_vars)
                      ! The variable identifiers of the variables
CHARACTER(LEN=max_sdf_name_len) :: var_name(max_cable_vars)
                      ! The name of each variable in the file
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(max_cable_vars)
                      ! The name to substitute in a template for each
                      ! variable
LOGICAL :: use_file(max_cable_vars)
                      !   T - the variable uses the file
                      !   F - the variable is set using a constant value
CHARACTER(LEN=errormessagelength) :: iomessage
                      ! I/O error message string
REAL :: const_val(max_cable_vars)
!INTEGER:: iconst_val(max_cable_vars)
                      ! The constant value to use for each variable if
                      ! use_file = F for that variable
NAMELIST  / cable_progs/ FILE, nvars, var, var_name, use_file, tpl_name,       &
                        const_val

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
nvars_required = 0
nvars_file     = 0
nvars          = 0
use_file(:)    = .TRUE.  ! Default is for every variable to be read from file
FILE(:)        = ''      ! Empty file names
var_name(:)    = ''      ! Empty variable names
tpl_name(:)    = ''      ! Empty template string
const_val(:)   = rmdi

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info("init_cable_progs", "Reading CABLE_PROGS namelist...")
OPEN(namelist_unit, FILE=('cable_prognostics.nml'),                            &
             STATUS='old', POSITION='rewind', ACTION='read', IOSTAT  = ERROR,  &
             IOMSG = iomessage)


! First, we read the cable_progs namelist
READ(namelist_unit, NML = cable_progs, IOSTAT = ERROR)

IF ( ERROR /= 0 )                                                              &
   CALL log_fatal("init_cable_progs",                                          &
                  "Error reading namelist CABLE_PROGS" //                      &
                  "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //         &
                  TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Set up CABLE prognostics using namelist values
!-----------------------------------------------------------------------------
! Set up the required variables
! All the CABLE variables are always required for CABLE runs
nvars_required = max_cable_vars
required_vars(:) = [                                                           &
                        'ThreeLayerSnowFlag_CABLE',                            &
                        'OneLyrSnowDensity_CABLE ',                            &
                        'SnowAge_CABLE           ',                            &
                        'SnowDensity_CABLE       ',                            &
                        'SnowMass_CABLE          ',                            &
                        'SnowDepth_CABLE         ',                            &
                        'SnowTemp_CABLE          ',                            &
                        'FrozenSoilFrac_CABLE    ',                            &
                        'SoilMoisture_CABLE      ',                            &
                        'SoilTemp_CABLE          '                             &
                        ]
!-------------------------------------------------------------------------
! Check that variable identifiers are not empty.
! Although we might later decide that the identifier is not required, for
! clarity we check here whether the claimed amount of information was
! provided.
!-------------------------------------------------------------------------
DO i = 1,nvars
  IF ( LEN_TRIM(var(i)) == 0 )                                                 &
    CALL log_fatal("init_cable_progs",                                         &
                    "Insufficient values for var. " //                         &
                    "No name provided for var at position #" //                &
                    TRIM(to_string(i)) )
END DO

!-----------------------------------------------------------------------------
! Check that all the required variables are there
!-----------------------------------------------------------------------------

DO i = 1,nvars_required
  IF ( .NOT. ANY(var(1:nvars) == TRIM(required_vars(i))) )                     &
    CALL log_fatal("init_cable_progs",                                         &
                   "No value given for required variable '" //                 &
                   TRIM(required_vars(i)) // "'")
END DO


!-----------------------------------------------------------------------------
! Check which variables we will be using and partition them into variables
! set to constant values and variables set from file
!-----------------------------------------------------------------------------
DO i = 1,nvars
  !-----------------------------------------------------------------------------
  ! If the variable is one of the required vars, then we will be using it
  !-----------------------------------------------------------------------------
  IF ( ANY(required_vars(1:nvars_required) == var(i)) ) THEN
    IF ( use_file(i) ) THEN
      CALL log_info("init_cable_progs",                                        &
                    "'" // TRIM(var(i)) // "' will be read from file")

      ! If the variable will be filled from file, register it here
      nvars_file = nvars_file + 1
      var(nvars_file) = var(i)
      var_name(nvars_file) = var_name(i)
      tpl_name(nvars_file) = tpl_name(i)
    ELSE
      ! If the variable is being set as a constant, just populate it here
        ! First check that a value has been provided.
      IF ( ABS( const_val(i) - rmdi ) < EPSILON(1.0) )                         &
      CALL log_fatal("init_cable_progs",                                       &
                     "No constant value provided for variable '"               &
                     // TRIM(var(i)) // "'" )

      CALL log_info("init_cable_progs",                                        &
                  "'" // TRIM(var(i)) // "' will be set to a " //              &
                  "constant = " // to_string(const_val(i)))

      CALL populate_var(get_var_id(var(i)), const_val = const_val(i))
    END IF
  ELSE
    ! If the variable is not a required variable, warn about not using it
    CALL log_warn("init_cable_progs",                                          &
                  "Provided variable '" // TRIM(var(i)) //                     &
                  "' is not required, so will be ignored")
  END IF
END DO

!-----------------------------------------------------------------------------
! Set variables from file
!-----------------------------------------------------------------------------
IF ( nvars_file > 0 ) THEN
    ! Check that a file name was provided.
  IF ( LEN_TRIM(FILE) == 0 )                                                   &
    CALL log_fatal("init_cable_progs", "No file name provided")

  IF ( tpl_has_var_name(FILE) ) THEN
    ! We are using a file name template, so loop through the variables setting
    ! one from each file
    DO i = 1,nvars_file
      ! If using a variable name template, check that a template string was
      !provided for the current variable
      IF ( LEN_TRIM(tpl_name(i)) == 0 )                                        &
      CALL log_fatal("init_cable_progs",                                       &
                     "No variable name template substitution " //              &
                     "(tpl_name) provided for " // TRIM(var(i)))


      CALL fill_variables_from_file(                                           &
        tpl_substitute_var(FILE, tpl_name(i)),                                 &
        [ var(i) ], [ var_name(i) ], is_climatology = [ .FALSE. ]              &
      )
    END DO
  ELSE
    ! We are not using a file name template, so set all variables from the same
    ! file

    CALL fill_variables_from_file(                                             &
      FILE, var(1:nvars_file), var_name(1:nvars_file),                         &
      is_climatology = [ (.FALSE., i=1, nvars_file) ]                          &
      )
  END IF
END IF

RETURN

END SUBROUTINE read_cable_progs
END MODULE read_cable_progs_mod
#endif
