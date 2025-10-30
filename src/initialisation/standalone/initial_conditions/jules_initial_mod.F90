#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE jules_initial_mod

USE io_constants,        ONLY: max_sdf_name_len, max_file_name_len
USE model_interface_mod, ONLY: identifier_len
USE dump_mod,            ONLY: max_var_dump
USE um_types,            ONLY: real_jlslsm
USE logging_mod,         ONLY: log_info, log_warn, log_fatal
USE missing_data_mod,    ONLY: imdi, rmdi
USE string_utils_mod,    ONLY: to_string

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Definition of the jules_initial namelist
!-----------------------------------------------------------------------------
LOGICAL :: dump_file = .FALSE. ! T - the given file is a dump file
                               ! F - the given file is not a dump file

LOGICAL :: total_snow = .FALSE.
                      ! Switch indicating how the snow model is initialised
                      !   T - only snow_tile is needed
                      !       If nsmax>0, the layer values are determined
                      !       by the model
                      !   F - all snow variables are supplied directly (what
                      !       these are depends on nsmax)

CHARACTER(LEN=max_file_name_len) :: FILE = ''
                      ! The name of the file (or variable name template) to
                      ! use for variables that need to be filled from file

INTEGER :: nvars = imdi ! The number of variables in this section
CHARACTER(LEN=identifier_len) :: var(max_var_dump) = ''
                      ! The variable identifiers of the variables
LOGICAL :: use_file(max_var_dump) = .TRUE.
                      !   T - the variable uses the file
                      !   F - the variable is set using a constant value
                      ! Defaults to T for every variable
CHARACTER(LEN=max_sdf_name_len) :: var_name(max_var_dump) = ''
                      ! The name of each variable in the file
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(max_var_dump) = ''
                      ! The name to substitute in a template for each
                      ! variable
REAL(KIND=real_jlslsm) :: const_val(max_var_dump) = rmdi
                      ! The constant value to use for each variable if
                      ! use_file = F for that variable
                      ! This might later be replaced by a default value from
                      ! a dictionary.
LOGICAL :: l_broadcast_soilt = .FALSE.
                      !IO local variable.
                      !Value then held in prognostics module.
                      !Switch to broadcast model state around all soil tiles.
                      !Only has an effect if l_tile_soil is true.
                      !Does not do anything with ancils- this is done by
                      !l_broadcast_ancils

NAMELIST /jules_initial/ total_snow, dump_file, FILE,                          &
                         nvars, var, use_file, var_name, tpl_name,             &
                         const_val, l_broadcast_soilt

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_INITIAL_MOD'

CONTAINS

SUBROUTINE read_nml_jules_initial(nml_dir)

USE io_constants,           ONLY: namelist_unit
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage
CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_INITIAL'

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_INITIAL namelist...")

OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'initial_conditions.nml'),   &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error opening namelist file initial_conditions.nml " //      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

READ(namelist_unit, NML = jules_initial, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_INITIAL " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error closing namelist file initial_conditions.nml " //      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

END SUBROUTINE read_nml_jules_initial

SUBROUTINE check_jules_initial()

USE prognostics,    ONLY: l_broadcast_soilt_in_mod => l_broadcast_soilt
USE jules_soil_mod, ONLY: l_tile_soil

IMPLICIT NONE

INTEGER :: i ! Loop counters
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CHECK_JULES_INITIAL'

!Copy across _io variable to its proper place
l_broadcast_soilt_in_mod = l_broadcast_soilt

!-----------------------------------------------------------------------------
! Check switches against soil tiling
!-----------------------------------------------------------------------------
IF ( l_broadcast_soilt ) THEN
  IF ( l_tile_soil ) THEN
    CALL log_info(RoutineName,                                                 &
                  "Non-soil tiled initial conditions will be broadcast " //    &
                  "to all soil tiles. Users should consider appropriate" //    &
                  " spinup period")
  ELSE
    CALL log_warn(RoutineName,                                                 &
                  "l_broadcast_soilt will have no effect: l_tile_soilt = F")
  END IF
END IF

!-----------------------------------------------------------------------------
! Check that variable identifiers are not empty.
! Although we might later decide that the identifier is not required, for
! clarity we check here whether the claimed amount of information was
! provided.
!-----------------------------------------------------------------------------
DO i = 1,nvars
  IF ( LEN_TRIM(var(i)) == 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   "Insufficient values for var. " //                          &
                   "No name provided for var at position #" //                 &
                   TRIM(to_string(i)) )
  END IF
END DO

END SUBROUTINE check_jules_initial


SUBROUTINE init_ic_shared ( nvars_required, required_vars,                     &
                            nvars_from_ancil, vars_from_ancil,                 &
                            default_values )

USE dictionary_mod,      ONLY: dict, dict_get, dict_free
USE model_interface_mod, ONLY: populate_var, get_var_id
USE templating_mod,      ONLY: tpl_has_var_name, tpl_substitute_var
USE read_dump_mod,       ONLY: read_dump

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

IMPLICIT NONE

! Arguments
INTEGER, INTENT(IN) :: nvars_required   ! The number of variables that are
                                        ! required in this configuration

INTEGER, INTENT(IN) :: nvars_from_ancil !Number of ancil variables

CHARACTER(LEN=identifier_len), INTENT(IN) :: required_vars(:)
                               ! The variable identifiers of the required
                               ! variables

CHARACTER(LEN=identifier_len), INTENT(IN) :: vars_from_ancil(:)
                               ! The model identifiers of the required
                               ! ancil variables

TYPE(dict), INTENT(IN OUT) :: default_values
            ! Dictionary mapping identifier => default value
            ! Values should be REAL

! Local variables
INTEGER :: i  ! Loop counter

INTEGER :: nvars_file   ! The number of variables that will be set
                        ! from the given file (template?)

REAL(KIND=real_jlslsm) :: default_value
                       ! Variable to contain values read from the dict

LOGICAL :: in_init_nml, loaded_from_ancil
                      !Used for checking whether all req vars have been
                      !accounted for

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_IC_SHARED'

! If we are initialising from a dump and no variables were specified, then
! we assume that all variables will be initialised from the dump file
IF ( dump_file .AND. nvars < 1 ) THEN
  CALL log_info(RoutineName,                                                   &
                "No variables given - will attempt to initialise all " //      &
                "required variables from specified dump file")
  nvars = nvars_required
  var(1:nvars) = required_vars(1:nvars)
  ! Every variable will use the file
  use_file(:) = .TRUE.
  ! We don't need to set var_name, tpl_name or const_val since they are never
  ! used in the case of a dump file anyway

ELSE
  !Check that all required variables are accounted for and fill in with
  !default values as needed
  !** NB- Default values are not available when using a dump file **
  DO i = 1, nvars_required

    !Test if the required variable is in the initial conditions namelist
    IF ( ANY(var(1:nvars) == required_vars(i)) ) THEN
      in_init_nml = .TRUE.
    ELSE
      in_init_nml = .FALSE.
    END IF

    !Test if the required variable has been loaded from an ancil
    IF ( ANY(vars_from_ancil(1:nvars_from_ancil) == required_vars(i)) ) THEN
      loaded_from_ancil = .TRUE.
    ELSE
      loaded_from_ancil = .FALSE.
    END IF

    !Log our assessment of whether the variable is accounted for
    IF ( in_init_nml .OR. loaded_from_ancil ) THEN
      CALL log_info(RoutineName,                                               &
           "'" // TRIM(required_vars(i)) // "' accounted for- OK")
    ELSE
      CALL log_info(RoutineName,                                               &
           "'" // TRIM(required_vars(i)) // "' not accounted for- " //         &
           "will attempt to set to default value")

      !Add the req variable to the var array along with with its default value
      nvars            = nvars + 1
      var(nvars)       = required_vars(i)
      use_file(nvars)  = .FALSE.
      CALL dict_get(default_values, required_vars(i), default_value)
      const_val(nvars) = default_value

    END IF !in_init_nml .OR. loaded_from_ancil
  END DO !nvars_required
END IF !dump_file .AND. nvars < 1

!-----------------------------------------------------------------------------
! Check which variables we will be using and partition them into variables
! set to constant values and variables set from file
!-----------------------------------------------------------------------------
nvars_file     = 0
DO i = 1,nvars
  !---------------------------------------------------------------------------
  ! If the variable is one of the required vars, then we will be using it
  !---------------------------------------------------------------------------
  IF ( ANY(required_vars(1:nvars_required) == var(i)) ) THEN
    IF ( use_file(i) ) THEN
      CALL log_info(RoutineName,                                               &
                    "'" // TRIM(var(i)) // "' will be read from file")

      ! If the variable will be filled from file, register it here
      nvars_file = nvars_file + 1
      ! Since nvars_file <= i (so we will not overwrite unprocessed values)
      ! and we do not need the values from these arrays for any non-file
      ! variables from now on, we can just compress them down onto variables
      ! that are in the file.
      var(nvars_file) = var(i)
      var_name(nvars_file) = var_name(i)
      tpl_name(nvars_file) = tpl_name(i)
    ELSE
      ! If the variable is being set as a constant, populate it here.
      ! First check that a value has been provided.
      IF ( ABS( const_val(i) - rmdi ) < EPSILON(1.0) ) THEN
        CALL log_fatal(RoutineName,                                            &
                       "No constant value provided for variable '"             &
                       // TRIM(var(i)) // "'" )
      END IF

      CALL log_info(RoutineName,                                               &
                    "'" // TRIM(var(i)) // "' will be set to a " //            &
                    "constant = " // to_string(const_val(i)))

      CALL populate_var(get_var_id(var(i)), const_val = const_val(i))
    END IF  !  use_file
  ELSE
    ! If the variable is not a required variable, warn about not using it
    CALL log_warn(RoutineName,                                                 &
                  "Provided variable '" // TRIM(var(i)) //                     &
                  "' is not required, so will be ignored")
  END IF
END DO

!-----------------------------------------------------------------------------
! Set variables from file
!-----------------------------------------------------------------------------
IF ( nvars_file > 0 ) THEN
  !   Check that a file name was provided.
  IF ( LEN_TRIM(FILE) == 0 ) THEN
    CALL log_fatal(RoutineName, "No file name provided")
  END IF

  IF ( dump_file ) THEN
    ! If we are using a dump file, use read_dump to fill the variables
    CALL read_dump(FILE, var(1:nvars_file))
  ELSE IF ( tpl_has_var_name(FILE) ) THEN
    ! If we are using a non-dump file with a variable name template, loop
    ! through the variables setting one from each file.
    DO i = 1,nvars_file
      ! Check that a template string was provided for this variable.
      IF ( LEN_TRIM(tpl_name(i)) == 0 ) THEN
        CALL log_fatal( RoutineName,                                           &
                        "No variable name template substitution " //           &
                        "provided for " // TRIM(var(i)) )
      END IF
      CALL fill_variables_from_file(                                           &
        tpl_substitute_var(FILE, tpl_name(i)),                                 &
        [ var(i) ], [ var_name(i) ], is_climatology = [ .FALSE. ]              &
      )
    END DO
  ELSE
    ! We are not using a file name template, so set all variables from the
    ! same file.
    CALL fill_variables_from_file(                                             &
      FILE, var(1:nvars_file), var_name(1:nvars_file),                         &
      is_climatology = [ (.FALSE., i=1, nvars_file) ]                          &
      )
  END IF  !  dump_file
END IF  !  nvars_file

! Free the dictionary of default values as it is no longer required
CALL dict_free(default_values)

END SUBROUTINE init_ic_shared

END MODULE jules_initial_mod
#endif
