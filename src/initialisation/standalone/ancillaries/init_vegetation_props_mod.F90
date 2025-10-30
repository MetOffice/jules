#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE init_vegetation_props_mod

IMPLICIT NONE

CONTAINS

!##############################################################################

SUBROUTINE init_vegetation_props()

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE io_constants, ONLY: max_file_name_len, max_sdf_name_len, namelist_unit

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE model_interface_mod, ONLY: identifier_len

USE jules_vegetation_mod, ONLY: photo_adapt, photo_adapt_acclim,               &
                                photo_acclim_model

USE dump_mod, ONLY: ancil_dump_read

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   Reads in ancils required for vegetation and checks them for consistency.
!------------------------------------------------------------------------------

! Local parameters.
INTEGER, PARAMETER :: nvars_max = 1
  ! The maximum possible number of variables that can be given.

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_VEGETATION_PROPS'

! Local scalar variables.
INTEGER ::                                                                     &
  ERROR,                                                                       &
    ! Error indicator.
  i,                                                                           &
    ! Loop counter.
  nvars_const,                                                                 &
    ! The number of variables to be set using constant values.
  nvars_file,                                                                  &
    ! The number of variables that will be set from the given file.
  nvars_required
    ! The number of variables that are required in this configuration.

LOGICAL ::                                                                     &
  l_file_list
    ! Flag indicating if we have a list of files either as variable-name
    ! templating is being used or we have supplied a list of files.

CHARACTER(LEN=errormessagelength) :: iomessage
    ! I/O error message string.

! Local array variables.
LOGICAL ::                                                                     &
  use_var(nvars_max),                                                          &
    ! Flag indicating if a variable is required.
  is_climatology(nvars_max) = .FALSE.
    ! Flag to indicate var is from a 12-month climatology, requiring
    ! interpolation to the correct time.

CHARACTER(LEN=max_file_name_len) :: file_name(nvars_max)
    ! The name of the file to use for each variable.

CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
  ! The variable identifiers of the required variables.

CHARACTER(LEN=identifier_len) :: file_var(nvars_max)
    ! Identifiers of the variables to be set from a file.
CHARACTER(LEN=max_sdf_name_len) :: file_var_name(nvars_max)
    ! The name of each variable in the file.
CHARACTER(LEN=max_sdf_name_len) :: file_tpl_name(nvars_max)
    ! The name to substitute in a template for each variable.

!------------------------------------------------------------------------------
! Variables for and definition of the jules_vegetation_props namelist.
!------------------------------------------------------------------------------
INTEGER :: nvars
  ! The number of variables listed.

LOGICAL :: read_from_dump
  ! T means read variables from the dump file (not an ancillary file).

LOGICAL :: read_list = .FALSE.
    ! T - The given file contains a list of file names
    ! F - Ancillary data should be read directly from the given file or template
CHARACTER(LEN=max_file_name_len) :: FILE
    ! The file to use for variables that need to be filled from a file.
    ! Exact usage depends on read_list:
    ! read_list = T
    ! Contains a list of files; cannot contain variable name templating.
    ! read_list = F
    ! The name of the file (or variable name template).

REAL(KIND=real_jlslsm) :: const_val(nvars_max)
  ! The constant value to use for a variable if use_file = F for that
  ! variable.

LOGICAL :: use_file(nvars_max)
  ! T - the variable is in the file
  ! F - the variable is set using a constant value.

CHARACTER(LEN=identifier_len) :: var(nvars_max)
  ! The variable identifiers of the variables.

CHARACTER(LEN=max_sdf_name_len) :: var_name(nvars_max)
  ! The name of each variable in the file.

CHARACTER(LEN=max_sdf_name_len) :: tpl_name(nvars_max)
  ! The name to substitute in a template for each variable.

NAMELIST  /jules_vegetation_props/ read_from_dump, FILE, nvars, var, use_file, &
   read_list, var_name, tpl_name, const_val

!------------------------------------------------------------------------------
! If thermal adaptation is not selected, we have nothing more to do.
!------------------------------------------------------------------------------
IF ( photo_acclim_model /= photo_adapt .AND.                                   &
     photo_acclim_model /= photo_adapt_acclim ) RETURN

!------------------------------------------------------------------------------
! Initialise
!------------------------------------------------------------------------------
! Initialise some common variables.
CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,                 &
                               nvars_required, l_file_list,                    &
                               FILE, const_val, use_file, use_var,             &
                               file_name, tpl_name, var, var_name )
! Further initialisation.
read_from_dump = .FALSE.

!------------------------------------------------------------------------------
! Read namelist
!------------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_VEGETATION_PROPS namelist...")

READ(namelist_unit, NML = jules_vegetation_props, IOSTAT = ERROR,              &
     IOMSG = iomessage)
IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_VEGETATION_PROPS " //           &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

ancil_dump_read%vegetation_props = read_from_dump

IF ( .NOT. ancil_dump_read%vegetation_props) THEN
  !----------------------------------------------------------------------------
  ! Set properties using namelist values.
  !----------------------------------------------------------------------------
  ! Set up the required variables.
  ! All the variables are required.
  nvars_required   = nvars_max
  required_vars(:) = [ 't_home_gb' ]

  !----------------------------------------------------------------------------
  ! Check that variables in the namelist have been provided and set up other
  ! variables.
  !----------------------------------------------------------------------------
  CALL check_namelist_values( nvars, nvars_max, nvars_required,                &
                              RoutineName, FILE, const_val,                    &
                              use_file, read_list, required_vars,              &
                              tpl_name, var, nvars_const, nvars_file,          &
                              l_file_list, use_var, file_name, var_name )

  !----------------------------------------------------------------------------
  ! Set variables that are using a constant value.
  !----------------------------------------------------------------------------
  CALL set_constant_vars( nvars, nvars_const, nvars_max, const_val,            &
                          use_file, use_var, var )

  !----------------------------------------------------------------------------
  ! Set variables that are to be read from a file.
  !----------------------------------------------------------------------------
  CALL set_file_vars( nvars, nvars_file, nvars_max, l_file_list,               &
                      RoutineName, use_file, use_var, file_name, var,          &
                      var_name, is_climatology )

ELSE

  !----------------------------------------------------------------------------
  ! We read from the dump file
  !----------------------------------------------------------------------------
  CALL log_info(RoutineName,                                                   &
                "Veg property ancils will be read from the dump file.  " //    &
                "Namelist values ignored")

END IF !  ancil_dump_read%vegetation_props

RETURN

END SUBROUTINE init_vegetation_props
END MODULE init_vegetation_props_mod
#endif
