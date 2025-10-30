#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE init_ic_mod

USE model_interface_mod, ONLY: identifier_len
USE dump_mod,            ONLY: max_var_dump
USE jules_initial_mod,   ONLY: total_snow, dump_file
USE logging_mod,         ONLY: log_info, log_warn, log_error, log_fatal
USE string_utils_mod,    ONLY: to_string


IMPLICIT NONE

CONTAINS

SUBROUTINE init_ic( nml_dir, rivers )

  !Use in relevant subroutines
USE init_initial_mod,    ONLY: init_initial
USE jules_initial_mod,   ONLY: init_ic_shared

USE required_vars_for_rivers_mod, ONLY: required_vars_for_rivers

USE um_types, ONLY: real_jlslsm

USE dictionary_mod, ONLY: dict

!TYPE definitions
USE jules_rivers_mod, ONLY: rivers_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Sets up the initial conditions for the run
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!TYPES containing field data (IN OUT)
TYPE(rivers_type), INTENT(IN OUT) :: rivers

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                                 &
     RoutineName = 'init_ic'   ! Name of this routine.

! Work variables
INTEGER :: nvars_required     ! The number of variables that are
                              ! required in this configuration

CHARACTER(LEN=identifier_len) :: required_vars(max_var_dump)
                               ! The variable identifiers of the required
                               ! variables

INTEGER :: nvars_from_ancil
CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                               ! The variable identifiers of the ancil
                               ! variables

TYPE(dict) :: default_values  ! Dictionary mapping identifier => default value
                              ! Values should be REAL

!-----------------------------------------------------------------------------

CALL init_initial(nml_dir)

!-----------------------------------------------------------------------------
! Set up initial conditions using namelist values
!-----------------------------------------------------------------------------

! Set up the required variables - we get the list by calling a procedure in
! dump_mod that tells us the required prognostic variables for the current
! model configuration
nvars_required   = 0
nvars_from_ancil = 0
CALL required_vars_for_rivers( nvars_required, required_vars,                  &
                               nvars_from_ancil, vars_from_ancil,              &
                               dump_file )

!Get a dictionary of default values in case they are needed
default_values = get_default_ic_values()

CALL init_ic_shared ( nvars_required, required_vars,                           &
                      nvars_from_ancil, vars_from_ancil,                       &
                      default_values )

RETURN

END SUBROUTINE init_ic


FUNCTION get_default_ic_values() RESULT(defaults_dict)

! USE statments
USE dictionary_mod, ONLY: dict, dict_set, dict_create

USE dump_mod,       ONLY: max_var_dump
USE required_vars_for_rivers_mod, ONLY: required_vars_for_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Gets default values for initialisation
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CHARACTER(LEN=identifier_len) :: identifiers(max_var_dump)
                             ! The variable identifiers of the required
                             ! variables

CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                             ! The variable identifiers of the ancil
                             ! variables (not used in this subroutine)

! Work variables
INTEGER :: i !Counter
INTEGER :: nvars_required
INTEGER :: nvars_from_ancil

! Return type
TYPE(dict) :: defaults_dict

!End of header
!-----------------------------------------------------------------------------

!Get the list of variables we need to have default variables for
nvars_required   = 0
nvars_from_ancil = 0
CALL required_vars_for_rivers(nvars_required, identifiers,                     &
                              nvars_from_ancil, vars_from_ancil,               &
                              .FALSE.)

!Create the dictionary containing the default values
defaults_dict = dict_create(nvars_required, 1.0)

!Set the default values one by one
DO i = 1, nvars_required
  SELECT CASE ( identifiers(i) )
    ! No defaults set
  CASE DEFAULT
    CALL log_info("get_default_ic_values",                                     &
              "No default value defined. Value must be defined in namelist - " &
                   // TRIM(identifiers(i)))
  END SELECT
END DO

RETURN

END FUNCTION get_default_ic_values

END MODULE init_ic_mod
#endif
