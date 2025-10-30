#if !defined(UM_JULES)
MODULE init_flake_ancils_mod

! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT****************************************
IMPLICIT NONE

PRIVATE

PUBLIC :: init_flake_ancils

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_FLAKE_ANCILS_MOD'

CONTAINS

!===============================================================================
! Public subroutine
!===============================================================================

SUBROUTINE init_flake_ancils()

!Module imports

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE dump_mod, ONLY: ancil_dump_read

USE errormessagelength_mod, ONLY: errormessagelength

USE io_constants, ONLY: max_file_name_len, max_sdf_name_len, namelist_unit

USE jules_surface_mod, ONLY: l_flake_model

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE model_interface_mod, ONLY: identifier_len

USE string_utils_mod, ONLY: to_string

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Subroutine for importing FLake ancillaries.
!
! Current Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
INTEGER, PARAMETER :: nvars_max = 1
       ! The maximum number of FLake model variables that can be given
       ! (lake depth is the only parameter set from namelist at the moment
       ! other paremeters, e.g. fetch, may be added in the future).

INTEGER :: nvars_required      ! The number of variables that are
                               ! required in this configuration

CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
                               ! The variable identifiers of the required
                               ! variables

INTEGER :: nvars_const
    ! The number of variables to be set using constant values.

INTEGER :: nvars_file          ! The number of variables that will be set
                               ! from the given file (template?)

INTEGER :: ERROR  ! Error indicator

INTEGER :: i      ! Loop counter

LOGICAL ::                                                                     &
  l_file_list
    ! Flag indicating if we have a list of files either as variable-name
    ! templating is being used or we have supplied a list of files.

LOGICAL ::                                                                     &
  use_var(nvars_max),                                                          &
    ! Flag indicating if a variable is required.
  is_climatology(nvars_max) = .FALSE.
    ! Flag to indicate var is from a 12-month climatology, requiring
    ! interpolation to the correct time.

CHARACTER(LEN=max_file_name_len) :: file_name(nvars_max)
    ! The name of the file to use for each variable.

!-----------------------------------------------------------------------------
! Definition of the jules_flake namelist
!-----------------------------------------------------------------------------

LOGICAL :: read_from_dump
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
INTEGER :: nvars      ! The number of variables in this section
CHARACTER(LEN=identifier_len) :: var(nvars_max)
                      ! The variable identifiers of the variables
CHARACTER(LEN=max_sdf_name_len) :: var_name(nvars_max)
                      ! The name of the lake depth variable in the file
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(nvars_max)
                      ! The name to substitute in a template for each
                      ! variable
LOGICAL :: use_file(nvars_max)
                      !   T - the variable uses the file
                      !   F - the variable is set using a constant value
CHARACTER(LEN=errormessagelength) :: iomessage
                      ! I/O error message string
REAL :: const_val(nvars_max)
                      ! The constant value to use for lake depth if
                      ! use_file = F

NAMELIST  /jules_flake/ read_from_dump, FILE, nvars, var, var_name, use_file,  &
                       read_list, tpl_name, const_val

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_FLAKE_ANCILS'

!-----------------------------------------------------------------------------

! If FLake model is not on, we have nothing to do
IF ( .NOT. l_flake_model ) RETURN

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
! Initialise some common variables.
CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,                 &
                               nvars_required, l_file_list,                    &
                               FILE, const_val, use_file, use_var,             &
                               file_name, tpl_name, var, var_name )
! Further initialisation.
read_from_dump = .FALSE.

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_FLAKE namelist...")

READ(namelist_unit, NML = jules_flake, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_FLAKE " //                      &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &

                TRIM(iomessage) // ")")

ancil_dump_read%flake = read_from_dump

IF ( .NOT. ancil_dump_read%flake ) THEN
  !---------------------------------------------------------------------------
  ! Set up FLake properties using namelist values
  !---------------------------------------------------------------------------
  ! Set up the required variables
  ! All the FLake model variables are always required
  nvars_required = nvars_max
  required_vars(:) = [ 'lake_depth' ]

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
                      var_name, is_climatology)

ELSE !We read from the dump file
  CALL log_info(RoutineName,                                                   &
                "flake ancils will be read from the dump file.  " //           &
                "Namelist values ignored")

END IF !.NOT. ancil_dump_read%flake


RETURN
END SUBROUTINE init_flake_ancils
END MODULE init_flake_ancils_mod
#endif
