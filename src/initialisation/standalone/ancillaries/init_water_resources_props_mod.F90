#if !defined(UM_JULES)

! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Reads in JULES_WATER_RESOURCES_PROPS namelist if necessary

MODULE init_water_resources_props_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_water_resources_props

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE dump_mod, ONLY: ancil_dump_read

USE io_constants, ONLY: max_file_name_len, max_sdf_name_len, namelist_unit

USE jules_water_resources_mod, ONLY: l_water_resources, l_water_irrigation,    &
       nwater_use, partition_ancil, partition_method, use_environment

USE model_interface_mod, ONLY: identifier_len

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!    Reads ancillary fields related to water resource modelling.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
INTEGER, PARAMETER :: nvars_max = 2
  ! The maximum possible number of variables that can be given.

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_WATER_RESOURCES_PROPS'

!------------------------------------------------------------------------------
! Local scalar variables:
!------------------------------------------------------------------------------

INTEGER :: nvars_const
                               ! The number of variables to be set using
                               ! constant values.
INTEGER :: nvars_required      ! The number of water resource variables that
                               ! are required in this configuration
INTEGER :: nvars_file       ! The number of variables that will be set
                            ! from the given file

INTEGER :: ERROR  ! Error indicator

INTEGER :: i  ! Loop counter

LOGICAL ::                                                                     &
  l_file_list
    ! Flag indicating if we have a list of files either as variable-name
    ! templating is being used or we have supplied a list of files.

LOGICAL ::                                                                     &
  l_need_conveyance_loss
    ! Flag indicating if a conveyance loss ancillary field is required.

!------------------------------------------------------------------------------
! Local array variables:
!------------------------------------------------------------------------------
LOGICAL ::                                                                     &
  use_var(nvars_max),                                                          &
    ! Flag indicating if a variable is required.
  is_climatology(nvars_max) = .FALSE.
    ! Flag to indicate var is from a 12-month climatology, requiring
    ! interpolation to the correct time.

CHARACTER(LEN=max_file_name_len) :: file_name(nvars_max)
    ! The name of the file to use for each variable.

CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
                               ! The variable identifiers of the required
                               ! variables
CHARACTER(LEN=identifier_len) :: file_var(nvars_max)
                      ! The variable identifiers of the variables to set
                      ! from file
CHARACTER(LEN=max_sdf_name_len) :: file_var_name(nvars_max)
                      ! The name of each variable in the file

CHARACTER(LEN=max_sdf_name_len) :: file_tpl_name(nvars_max)
                      ! The name to substitute in a template for each
                      ! variable

!------------------------------------------------------------------------------
! Variables that are in the namelist.
!------------------------------------------------------------------------------
INTEGER :: nvars
  ! The number of variables to be set.

LOGICAL :: read_from_dump

REAL(KIND=real_jlslsm) :: const_val(nvars_max)
  ! The constant value to use for each variable if use_file = F for that
  ! variable.

LOGICAL :: use_file(nvars_max)
  ! T - the variable uses the file
  ! F - the variable is set using a constant value

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

CHARACTER(LEN=identifier_len) :: var(nvars_max)
  ! The identifiers of the variables.
CHARACTER(LEN=max_sdf_name_len) :: var_name(nvars_max)
  ! The name of each variable in the file.
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(nvars_max)
  ! The name to substitute in a template for each variable.

NAMELIST  /jules_water_resources_props/ read_from_dump, FILE, nvars, var,      &
                            use_file, read_list, var_name, tpl_name, const_val

!------------------------------------------------------------------------------
!end of header

! Nothing to do if water resource model is not selected.
IF ( .NOT. l_water_resources ) RETURN

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
! Read the namelist.
!------------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_WATER_RESOURCES_PROPS namelist...")

READ(namelist_unit, NML = jules_water_resources_props, IOSTAT = ERROR)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                "Error reading namelist JULES_WATER_RESOURCES_PROPS " //       &
                "(IOSTAT=" // TRIM(to_string(ERROR)) // ")")
END IF

ancil_dump_read%water_resources_props = read_from_dump

IF ( .NOT. ancil_dump_read%water_resources_props) THEN
  !----------------------------------------------------------------------------
  ! Read from the ancil file.
  !----------------------------------------------------------------------------

  ! Conveyance loss is required for all sectors other than environmental.
  l_need_conveyance_loss = .FALSE.
  DO i = 1, nwater_use
    IF ( i /= use_environment ) THEN
      l_need_conveyance_loss = .TRUE.
    END IF
  END DO
  IF ( l_need_conveyance_loss ) THEN
    nvars_required = 1
    required_vars(nvars_required) = 'conv_loss_frac'
  END IF

  IF ( partition_method == partition_ancil ) THEN
    nvars_required = nvars_required + 1
    required_vars(nvars_required) =  'sfc_water_frac'
  END IF

  IF ( nvars_required > 0 ) THEN

    !--------------------------------------------------------------------------
    ! Check that variables in the namelist have been provided and set up other
    ! variables.
    !--------------------------------------------------------------------------
    CALL check_namelist_values( nvars, nvars_max, nvars_required,              &
                                RoutineName, FILE, const_val,                  &
                                use_file, read_list, required_vars,            &
                                tpl_name, var, nvars_const, nvars_file,        &
                                l_file_list, use_var, file_name, var_name )

    !--------------------------------------------------------------------------
    ! Set variables that are using a constant value.
    !--------------------------------------------------------------------------
    CALL set_constant_vars( nvars, nvars_const, nvars_max, const_val,          &
                            use_file, use_var, var )

    !--------------------------------------------------------------------------
    ! Set variables that are to be read from a file.
    !--------------------------------------------------------------------------
    CALL set_file_vars( nvars, nvars_file, nvars_max, l_file_list,             &
                        RoutineName, use_file, use_var, file_name, var,        &
                        var_name, is_climatology )

  END IF  !  nvars_required

ELSE

  !----------------------------------------------------------------------------
  ! ancil_dump_read%water_resources_props = .TRUE.
  ! Values will be read from the dump file.
  !----------------------------------------------------------------------------
  CALL log_info(RoutineName,                                                   &
                "water resources properties will be read from the dump " //    &
                "file. Namelist values will be ignored.")

END IF ! ancil_dump_read%water_resources_props

END SUBROUTINE init_water_resources_props
END MODULE init_water_resources_props_mod
#endif
