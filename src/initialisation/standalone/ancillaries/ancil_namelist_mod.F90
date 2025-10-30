!******************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************
!
! Description:
!   Utility routines to assist in the processing of ancillary namelist values.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

MODULE ancil_namelist_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE  ! Private scope by default
PUBLIC check_namelist_values, initialise_namelist_vars, set_constant_vars,     &
       set_file_vars

CONTAINS

!#############################################################################
!##############################################################################

SUBROUTINE initialise_namelist_vars( nvars, nvars_const, nvars_file,           &
                                     nvars_required, l_file_list,              &
                                     file_nml, const_val, use_file, use_var,   &
                                     file_name, tpl_name, var, var_name,       &
                                     nvars_optional, optional_vars,            &
                                     is_climatology )

! Description:
!   Initialise variables that appear in many ancillary namelists.

USE missing_data_mod, ONLY: rmdi

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(OUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
  nvars,                                                                       &
    ! Number of variables.
  nvars_const,                                                                 &
    ! The number of variables to be set using constant values.
  nvars_file,                                                                  &
    ! The number of variables to read from file.
  nvars_required
    ! The number of variables that are required in the current configuration.

LOGICAL, INTENT(OUT) ::                                                        &
  l_file_list
    ! Flag indicating if we have a list of files either as variable-name
    ! templating is being used or we have supplied a list of files.

CHARACTER(LEN=*), INTENT(OUT) ::                                               &
  file_nml
    ! The name of a file (or variable name template), as read from namelist.

!------------------------------------------------------------------------------
! Array arguments with INTENT(OUT)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  const_val(:)
    ! The constant value to use for each variable that has use_file = F.

LOGICAL, INTENT(OUT) ::                                                        &
  use_file(:),                                                                 &
    ! Flag indicating if a variable is to be read from file.
    !   T - read from file
    !   F - set using a constant value
  use_var(:)
    ! Flag indicating if a variable is required.

CHARACTER(LEN=*), INTENT(OUT) ::                                               &
  file_name(:),                                                                &
    ! The name of the file to use for each variable.
  tpl_name(:),                                                                 &
    ! The name to substitute in a template, for each variable.
  var(:),                                                                      &
    ! Identifiers for variables.
  var_name(:)
    ! The name of the variable to be read from file, for each variable.

!------------------------------------------------------------------------------
! Optional arguments.
!------------------------------------------------------------------------------
! Optional scalar arguments with INTENT(OUT).
INTEGER, OPTIONAL, INTENT(OUT) ::                                              &
  nvars_optional
    ! The number of variables that are optional in the current configuration.

CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) ::                                     &
  optional_vars(:)
    ! Identifiers for optional variables.

LOGICAL, OPTIONAL, INTENT(OUT) ::                                              &
  is_climatology(:)
    ! Indicates whether the ancillary input is a 12-month climatology. This
    ! currently assumes that there are 12 monthly values valid on 16th of the
    ! month.

! End of header.
!------------------------------------------------------------------------------

! Initialise variables that will be read from a namelist.
nvars          = 0
file_nml       = ''      ! Empty file name.
const_val(:)   = rmdi    ! Missing data value.
use_file(:)    = .TRUE.  ! Default is for every var to be read from file
tpl_name(:)    = ''      ! Empty template name.
var(:)         = ''      ! Empty identifiers.
var_name(:)    = ''      ! Empty variable names.

! Initialise variables that are derived and/or do not come from namelist.
nvars_const    = 0
nvars_file     = 0
nvars_required = 0
IF ( PRESENT( nvars_optional ) ) THEN
  nvars_optional = 0
  IF ( PRESENT( optional_vars ) ) THEN
    optional_vars(:) = ''  ! Empty identifiers.
  END IF
END IF
IF ( PRESENT( is_climatology ) ) THEN
  is_climatology(:) = .FALSE.
END IF
! Initially assume any file name does not include variable-name templating.
l_file_list     = .FALSE.
use_var(:)      = .FALSE. ! Variable is not required.
file_name(:)    = ''      ! Empty file name.

END SUBROUTINE initialise_namelist_vars

!#############################################################################
!#############################################################################

SUBROUTINE check_namelist_values( nvars, nvars_max, nvars_required,            &
                                  calling_routine, file_nml, const_val,        &
                                  use_file, read_list, required_vars,          &
                                  tpl_name, var, nvars_const, nvars_file,      &
                                  l_file_list, use_var, file_name,             &
                                  var_name, nvars_optional, optional_vars )

! Description:
!   Check that variables in the namelist have been provided and set up other
!   variables.

USE errormessagelength_mod, ONLY: errormessagelength
USE io_constants, ONLY: file_list_unit
USE logging_mod, ONLY: log_error, log_info, log_warn, log_fatal
USE missing_data_mod, ONLY: rmdi
USE string_utils_mod, ONLY: to_string
USE templating_mod, ONLY: tpl_has_var_name, tpl_substitute_var
USE jules_print_mgr, ONLY: newline

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_END

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nvars,                                                                       &
    ! Number of variables.
  nvars_max,                                                                   &
    ! The maximum-allowed number of variables.
  nvars_required
    ! The number of variables that are required in the current configuration.

CHARACTER(LEN=*), INTENT(IN) ::                                                &
  calling_routine,                                                             &
    ! The name of the program unit from which this routine has been called.
  file_nml
    ! The name of a file (or variable name template), as read from namelist.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  const_val(nvars_max)
    ! The constant value to use for each variable that has use_file = F.

LOGICAL, INTENT(IN) ::                                                         &
  use_file(nvars_max),                                                         &
    ! Flag indicating if a variable is to be read from file.
    !   T - read from file
    !   F - set using a constant value
  read_list
    ! T - The given file contains a list of file names
    ! F - Ancillary data should be read directly from the given file

CHARACTER(LEN=*), INTENT(IN) ::                                                &
  required_vars(nvars_max),                                                    &
    ! The identifier of each required variable.
  tpl_name(nvars_max),                                                         &
    ! The name to substitute in a template, for each variable.
  var(nvars_max)
    ! Identifiers for variables.

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(INOUT)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) ::                                                     &
  nvars_const,                                                                 &
    ! The number of variables to be set using constant values.
  nvars_file
    ! The number of variables to read from file.

LOGICAL, INTENT(IN OUT) ::                                                     &
  l_file_list
    ! Flag indicating if we have a list of files.

!------------------------------------------------------------------------------
! Array arguments with INTENT(INOUT)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN OUT) ::                                                     &
  use_var(nvars_max)
    ! Flag indicating if a variable is required.

CHARACTER(LEN=*), INTENT(IN OUT) ::                                            &
  file_name(nvars_max),                                                        &
    ! The name of the file to use for each variable.
  var_name(nvars_max)
    ! The name of the variable to be read from file, for each variable.

!------------------------------------------------------------------------------
! Optional arguments.
!------------------------------------------------------------------------------
! Optional scalar arguments with INTENT(IN).
INTEGER, OPTIONAL, INTENT(IN) ::                                               &
  nvars_optional
    ! The number of variables that are optional in the current configuration.

! Optional array arguments with INTENT(IN).
CHARACTER(LEN=*), OPTIONAL, INTENT(IN) ::                                      &
  optional_vars(nvars_max)
    ! The identifier of each optional variable.

!------------------------------------------------------------------------------
! Local scalars.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Loop counter.
  n
    ! Counter.

INTEGER :: ERROR  ! Error indicator

LOGICAL ::                                                                     &
  l_have_optional
    ! Flag indicating if optional variables are present.

CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CHECK_NAMELIST_VALUES'

! End of header.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Check for optional arguments.
!------------------------------------------------------------------------------
l_have_optional = .FALSE.
IF ( PRESENT(nvars_optional) ) THEN

  IF ( nvars_optional > 0 ) THEN
    ! Set flag indicating that we have optional variables.
    l_have_optional = .TRUE.

    ! Check that neither or both optional arguments are present.
    IF ( PRESENT(nvars_optional) .NEQV. PRESENT(optional_vars) ) THEN
      CALL log_fatal(RoutineName,                                              &
                     TRIM(calling_routine)  //                                 &
                     ": nvars_optional and optional_vars should either " //    &
                     "both be present or neither should be present." )
    END IF
  END IF  ! nvars_optional > 0

END IF  !  present

!------------------------------------------------------------------------------
! Check that we have exactly the same number of values given for each variable.
! This aims to prevent situations when a user has updated some entries but not
! all - e.g. reduced nvars but not removed an item from const_val, which could
! potentially result in an incorrect and unintended value being used.
! Later checks detect any missing values (as might happen if a user increases
! nvars between runs); here we check that no extra values are provided (as
! might happen if nvars has been reduced).
! Although some of these situations are potentially harmless we improve clarity
! by rejecting any inconsistencies.
!------------------------------------------------------------------------------
n = 0  ! Initialise counter.
DO i = nvars+1,nvars_max

  IF ( var(i) /= '' ) THEN
    CALL log_error(RoutineName,                                                &
                   TRIM(calling_routine)                                    // &
                   ": extra value provided for var: "                       // &
                   TRIM(var(i)) )
    n = n + 1
  END IF

  IF ( var_name(i) /= '' ) THEN
    CALL log_error(RoutineName,                                                &
                   TRIM(calling_routine)                                    // &
                   ": extra value provided for var_name: "                  // &
                   TRIM(var_name(i)) )
    n = n + 1
  END IF

  IF ( tpl_name(i) /= '' ) THEN
    CALL log_error(RoutineName,                                                &
                   TRIM(calling_routine)                                    // &
                   ": extra value provided for tpl_name: "                  // &
                   TRIM(tpl_name(i)) )
    n = n + 1
  END IF

  IF ( ABS( const_val(i) - rmdi ) > EPSILON(1.0) ) THEN
    CALL log_error(RoutineName,                                                &
                   TRIM(calling_routine)                                    // &
                   ": extra value provided for const_val: "                 // &
                   TRIM(to_string(const_val(i))) )
    n = n + 1
  END IF

END DO

IF ( n /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 TRIM(calling_routine)                                      // &
                 ": one or more namelist arrays has more values than the"   // &
                 " expected " // TRIM(to_string(nvars))                     // &
                 " (see separate messages)")
END IF

!------------------------------------------------------------------------------
! Check that the variable identifiers are not empty.
! Although we might later decide that the identifier is not required, for
! clarity we check here whether the claimed amount of information was
! provided.
!------------------------------------------------------------------------------
DO i = 1,nvars
  IF ( LEN_TRIM(var(i)) == 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   TRIM(calling_routine)                     //                &
                   ": Insufficient values for var. "         //                &
                   "No name provided for var at position #"  //                &
                   TRIM(to_string(i)) )
  END IF
END DO

!------------------------------------------------------------------------------
! Check that all the required variables are provided.
!------------------------------------------------------------------------------
DO i = 1,nvars_required
  IF ( .NOT. ANY(var(1:nvars) == required_vars(i)) ) THEN
    CALL log_fatal(RoutineName,                                                &
                   TRIM(calling_routine)                      //               &
                   ": No value given for required variable '" //               &
                   TRIM(required_vars(i)) // "'")
  END IF
END DO

!------------------------------------------------------------------------------
! Read list of files if present and ensure templating is not used
!------------------------------------------------------------------------------
IF ( read_list ) THEN
  l_file_list = .TRUE.
  IF ( LEN_TRIM(file_nml) == 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   TRIM(calling_routine)                      //               &
                   ": No file name given to read file list from.")
  END IF
  CALL log_info(RoutineName,                                                   &
                TRIM(calling_routine)                                       // &
                ": Reading list of ancillary file names from file "         // &
                TRIM(file_nml))
  ! Open the file
  OPEN(file_list_unit, FILE=file_nml, STATUS='old', POSITION='rewind',         &
                        ACTION='read', IOSTAT = ERROR, IOMSG = iomessage)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   TRIM(calling_routine)                        //             &
                   ": Error opening file " // TRIM(file_nml) // " " //         &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")
  END IF

  ! Read up to nvars_max file names, one per line.
  ! We allow nvars_max so that we can check for the case when too many names
  ! have been provided (as that could be a mistake).
  n = 0  ! initialise counter
  DO i = 1,nvars_max
    READ(file_list_unit, *, IOSTAT = ERROR, IOMSG = iomessage) file_name(i)
    ! Deal with reaching the end of the file.
    IF ( ERROR == IOSTAT_END ) THEN
      EXIT
    ELSE IF ( ERROR /= 0 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     TRIM(calling_routine)                  //                 &
                     ": Error reading file name from file " //                 &
                     "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //      &
                     TRIM(iomessage) // ")")
    END IF
    ! Update number of lines read.
    n = n + 1
    ! Check file name is provided. An empty name is acceptable if the variable
    ! will not be read from file.
    IF ( use_file(i) .AND. LEN_TRIM(file_name(i)) == 0 ) THEN
      CALL log_fatal(RoutineName,                                              &
                     TRIM(calling_routine)                                  // &
                     ": Empty file name in list.")
    END IF
    ! Check for templating.
    IF ( tpl_has_var_name( file_name(i) ) ) THEN
      CALL log_fatal(RoutineName,                                              &
                     TRIM(calling_routine)                  //                 &
                     ": File name in list cannot contain template.")
    END IF
  END DO

  ! Check file list has the correct number of entries
  IF ( n /= nvars ) THEN
    CALL log_fatal(RoutineName,                                                &
                   TRIM(calling_routine)                                    // &
                   ": File list should have nvars entries, found "          // &
                   TRIM(to_string(n)))
  END IF

  ! Close the file
  CLOSE(file_list_unit, IOSTAT = ERROR, IOMSG = iomessage)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal(RoutineName,                                                &
                   TRIM(calling_routine)                  //                   &
                   ": Error closing file " // TRIM(file_nml) // " " //         &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")
  END IF
END IF

!------------------------------------------------------------------------------
! Loop though the list of identifiers provided, checking which variables we
! will be using and partition them into those that will be set using a constant
! and those to be read from file.
!------------------------------------------------------------------------------
DO i = 1,nvars

  !----------------------------------------------------------------------------
  ! Decide if this variable is required or optional.
  !----------------------------------------------------------------------------
  IF ( ANY( required_vars(1:nvars_required) == TRIM(var(i)) ) ) THEN
    ! Indicate that this variable is required.
    use_var(i) = .TRUE.
  END IF
  ! We use a separate IF for optional variables to avoid array access errors
  ! if there are no optional variables.
  IF ( l_have_optional ) THEN
    IF ( ANY(optional_vars(1:nvars_optional) == TRIM(var(i)) ) ) THEN
      ! Indicate that this variable is required.
      use_var(i) = .TRUE.
    END IF
  END IF

  IF ( use_var(i) ) THEN

    IF ( use_file(i) ) THEN
      !------------------------------------------------------------------------
      ! The variable will be filled from file.
      !------------------------------------------------------------------------

      ! Increment counter.
      nvars_file = nvars_file + 1

      IF ( .NOT. read_list ) THEN
        ! If this is the first variable to read from file, perform further
        ! checks.
        ! This is not required if file names have been read from a list as this
        ! has already been done. Variable name templating is not allowed.
        IF ( nvars_file == 1 ) THEN
          ! Check that a file name was provided.
          IF ( LEN_TRIM(file_nml) == 0 ) THEN
            CALL log_fatal(RoutineName, "No file name provided")
          END IF

          ! Check if a file name template is present.
          !   Indicate that we will have a list of files to read.
          IF ( tpl_has_var_name(file_nml) ) THEN
            l_file_list = .TRUE.
          END IF
        END IF  !  nvars_file = 1

        IF ( l_file_list ) THEN
          ! Check that a template string was provided for this variable.
          IF ( LEN_TRIM(tpl_name(i)) == 0 ) THEN
            CALL log_fatal( RoutineName,                                       &
                            TRIM(calling_routine)                    //        &
                            ": No variable name template substitution "  //    &
                            "provided for " // TRIM(var(i)) )
          END IF
          ! Substitute into the template.
          file_name(i) = tpl_substitute_var( file_nml, tpl_name(i) )

        ELSE
          ! We are not using a template nor reading a list, therefore
          ! use the same file name for all variables.
          file_name(i) = file_nml
        END IF
      END IF ! .NOT. read_list

      ! If no name is given for the variable to be read, use the identifier.
      IF ( LEN_TRIM( var_name(i) ) == 0 ) THEN
        var_name(i) = var(i)
      END IF

      CALL log_info(RoutineName,                                               &
                    "'" // TRIM(var(i)) // "' will be read from file " //      &
                    "variable '" // TRIM(var_name(i)) // "' from file "        &
                    //newline// file_name(i) )

    ELSE

      !------------------------------------------------------------------------
      ! The variable is being set as a constant.
      !------------------------------------------------------------------------

      ! Increment counter.
      nvars_const = nvars_const + 1

      ! Check a value has been provided.
      IF ( ABS( const_val(i) - rmdi ) < EPSILON(1.0) ) THEN
        CALL log_fatal(RoutineName,                                            &
                       TRIM(calling_routine)                        //         &
                       ": No constant value provided for variable '" //        &
                       TRIM(var(i)) // "'" )
      END IF

      CALL log_info(RoutineName,                                               &
                    "'" // TRIM(var(i)) // "' will be set to a " //            &
                    "constant = " // to_string(const_val(i)))

    END IF  !  use_file

  ELSE

    !--------------------------------------------------------------------------
    ! The variable is not required. Warn about not using it.
    !--------------------------------------------------------------------------
    CALL log_warn(RoutineName,                                                 &
                  "Provided variable '" // TRIM(var(i)) //                     &
                  "' is not required, so will be ignored")

  END IF  !  use_var

END DO  !  variables

RETURN
END SUBROUTINE check_namelist_values

!##############################################################################
!##############################################################################

SUBROUTINE set_constant_vars( nvars, nvars_const, nvars_max, const_val,        &
                              use_file, use_var, var )

! Description:
!   Set variables that are using a constant value.

USE model_interface_mod, ONLY: identifier_len, populate_var, get_var_id

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nvars,                                                                       &
    ! Number of variables.
  nvars_const,                                                                 &
    ! The number of variables to be set using constant values.
  nvars_max
    ! The maximum-allowed number of variables.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  const_val(nvars_max)
    ! The constant value to use for each variable that has use_file = F.

LOGICAL, INTENT(IN) ::                                                         &
  use_file(nvars_max),                                                         &
    ! Flag indicating if a variable is to be read from file.
    !   T - read from file
    !   F - set using a constant value
  use_var(nvars_max)
    ! Flag indicating if a variable is required.

CHARACTER(LEN=*), INTENT(IN) ::                                                &
  var(nvars_max)
    ! The name of the variable to be read from file, for each variable.

!------------------------------------------------------------------------------
! Local scalars.
!------------------------------------------------------------------------------
INTEGER :: i  !  Loop counter.

! End of header.
!------------------------------------------------------------------------------

! If there is nothing to do, return.
IF ( nvars_const == 0 ) RETURN

DO i = 1,nvars

  !----------------------------------------------------------------------------
  ! Check if the variable is required and is to be set using a constant.
  !----------------------------------------------------------------------------
  IF ( use_var(i) .AND. .NOT. use_file(i) ) THEN

    ! Set this variable using a constant.
    CALL populate_var(get_var_id(var(i)), const_val = const_val(i))

  END IF

END DO  !  variables

END SUBROUTINE set_constant_vars

!##############################################################################
!##############################################################################

SUBROUTINE set_file_vars( nvars, nvars_file, nvars_max, l_file_list,           &
                          calling_routine, use_file, use_var, file_name,       &
                          var, var_name, is_climatology )

! Description:
!   Set variables that are to be read from a file.

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nvars,                                                                       &
    ! Number of variables.
  nvars_file,                                                                  &
    ! The number of variables to read from file.
  nvars_max
    ! The maximum-allowed number of variables.

LOGICAL, INTENT(IN) ::                                                         &
  l_file_list
    ! Flag indicating if we have a list of files.

CHARACTER(LEN=*), INTENT(IN) ::                                                &
  calling_routine
    ! The name of the program unit from which this routine has been called.

!------------------------------------------------------------------------------
! Array arguments with INTENT(IN)
!------------------------------------------------------------------------------
LOGICAL, INTENT(IN) ::                                                         &
  use_file(nvars_max),                                                         &
    ! Flag indicating if a variable is to be read from file.
    !   T - read from file
    !   F - set using a constant value
  use_var(nvars_max)
    ! Flag indicating if a variable is required.

CHARACTER(LEN=*), INTENT(IN) ::                                                &
  file_name(nvars_max),                                                        &
    ! The name of the file to use for each variable.
  var(nvars_max),                                                              &
    ! Identifiers for variables.
  var_name(nvars_max)
    ! The name of the variable to be read from file, for each variable.

LOGICAL, INTENT(IN) ::                                                         &
  is_climatology(nvars_max)
    ! Indicates whether the ancillary input is a 12-month climatology. This
    ! currently assumes that there are 12 monthly values valid on 16th of the
    ! month.
!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER :: i, j  !  Loop counters.

!------------------------------------------------------------------------------
! Local array variables.
!------------------------------------------------------------------------------
CHARACTER(LEN=max_file_name_len) ::                                            &
  file_name_use(nvars_max)
    ! The name of the file to use for each variable.

CHARACTER(LEN=LEN(var)) ::                                                     &
  file_var(nvars_max)
    ! Identifiers of the variables to be set from a file.

CHARACTER(LEN=LEN(var_name)) ::                                                &
  file_var_name(nvars_max)
    ! The name of each variable in the file.

LOGICAL ::                                                                     &
  file_is_climatology(nvars_max)
    ! Indicates whether the ancillary input is a 12-month climatology. This
    ! currently assumes that there are 12 monthly values valid on 16th of the
    ! month.

! Name of this routine.
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'SET_FILE_VARS'

! End of header.
!------------------------------------------------------------------------------

! If there is nothing to do, return.
IF ( nvars_file == 0 ) RETURN

!------------------------------------------------------------------------------
! Create lists containing only variables to be read from file.
!------------------------------------------------------------------------------
j = 0
DO i = 1,nvars
  IF ( use_var(i) .AND. use_file(i) ) THEN
    j = j + 1
    file_var(j)            = var(i)
    file_var_name(j)       = var_name(i)
    file_name_use(j)       = file_name(i)
    file_is_climatology(j) = is_climatology(i)
  END IF
END DO

IF ( j /= nvars_file ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 TRIM(calling_routine)                    //                   &
                 ": Unexpected mismatch in number of variables from file.")
END IF


IF ( l_file_list ) THEN
  ! We have a list of files. Loop through the variables setting one from
  ! each file.
  DO i = 1,nvars_file
    CALL fill_variables_from_file( file_name_use(i), [ file_var(i) ],          &
                                 [ file_var_name(i) ],                         &
                                 [ file_is_climatology(i) ] )

  END DO  !  vars
ELSE
  ! We are not using a file name template. Set all variables from the same
  ! file.

  ! In this case all values of file_name_use are identical.
  CALL fill_variables_from_file( file_name_use(1), file_var(1:nvars_file),     &
                                 file_var_name(1:nvars_file),                  &
                                 file_is_climatology(1:nvars_file) )
END IF  !  l_file_list

END SUBROUTINE set_file_vars

END MODULE ancil_namelist_mod
