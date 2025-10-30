#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_soil_props_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_soil_props()

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE model_interface_mod, ONLY: identifier_len

USE dump_mod, ONLY: ancil_dump_read

USE errormessagelength_mod, ONLY: errormessagelength

USE jules_soil_biogeochem_mod, ONLY:                                           &
 ! imported scalar parameters
   soil_model_ecosse, soil_model_4pool,                                        &
 ! imported scalar variables (IN)
   soil_bgc_model

USE jules_soil_mod, ONLY: soil_props_const_z, l_tile_soil, l_broadcast_ancils

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   Reads in the soil properties and checks them for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

! Work variables
INTEGER, PARAMETER :: nvars_max = 11 ! The maximum possible number of
                                    ! variables that can be given

INTEGER :: nvars_const         ! The number of variables to be set using
                               ! constant values.
INTEGER :: nvars_required      ! The number of soil variables that are
                               ! required in this configuration
CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
                               ! The variable identifiers of the required
                               ! variables

INTEGER :: nvars_file       ! The number of variables that will be set
                            ! from the given file (template?)

INTEGER :: i    ! Index variables

INTEGER :: ERROR  ! Error indicator

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
! Definition of the jules_soil_props namelist
!-----------------------------------------------------------------------------
LOGICAL :: read_from_dump
LOGICAL :: const_z            ! T - the same properties are used for each
                              !     soil layer
                              ! F - properties for each layer are read from
                              !     file
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
LOGICAL :: use_file(nvars_max)
                      !   T - the variable uses the file
                      !   F - the variable is set using a constant value
CHARACTER(LEN=max_sdf_name_len) :: var_name(nvars_max)
                      ! The name of each variable in the file
CHARACTER(LEN=max_sdf_name_len) :: tpl_name(nvars_max)
                      ! The name to substitute in a template for each
                      ! variable
CHARACTER(LEN=errormessagelength) :: iomessage
                      ! I/O error message string
REAL(KIND=real_jlslsm) :: const_val(nvars_max)
                      ! The constant value to use for each variable if
                      ! use_file = F for that variable

NAMELIST  /jules_soil_props/ read_from_dump, const_z, FILE, nvars, var,        &
                            use_file, read_list, var_name, tpl_name, const_val

CHARACTER(LEN=*), PARAMETER :: RoutineName='init_soil_props'

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
const_z        = .FALSE. ! Default is to read a value for each soil level

!------------------------------------------------------------------------------
! Read namelist
!------------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_SOIL_PROPS namelist...")

READ(namelist_unit, NML = jules_soil_props, IOSTAT = ERROR, IOMSG = iomessage)

IF ( ERROR /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist JULES_SOIL_PROPS " //                 &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")
END IF

ancil_dump_read%soil_props = read_from_dump

IF ( .NOT. ancil_dump_read%soil_props) THEN
  !----------------------------------------------------------------------------
  ! Set soil properties using namelist values.
  !----------------------------------------------------------------------------

  ! Set up the required variables.

  ! First, set up the variables that are always required.
  nvars_required = 9
  required_vars(1:nvars_required) = [ 'b      ', 'sathh  ', 'satcon ',         &
                                       'sm_sat ', 'sm_crit', 'sm_wilt',        &
                                       'hcap   ', 'hcon   ', 'albsoil' ]

  ! If 4-pool soil C is selected, clay is required.
  IF ( soil_bgc_model == soil_model_4pool ) THEN
    nvars_required                = nvars_required + 1
    required_vars(nvars_required) = 'clay'
    ! In older versions the soil clay content was set to zero instead of
    ! being read in. If it is not available in ancillaries give a stern
    ! warning and set it to zero.
    IF ( .NOT. ANY(var(1:nvars) == 'clay') ) THEN
      CALL log_warn(RoutineName,                                               &
                    "No value given for soil clay content. "              //   &
                    "Soil clay content is required with 4-pool C model. " //   &
                    "It will be set to 0.0 as for previous versions. "    //   &
                    "This is WRONG - please try and find values for clay.")
      ! Add clay to the list of variables, so that it will be set to zero.
      nvars                     = nvars + 1
      var(nvars)                = 'clay'
      use_file(nvars_required)  = .FALSE.
      const_val(nvars_required) = 0.0
    END IF
  END IF

  ! Variables used with ECOSSE.
  IF ( soil_bgc_model == soil_model_ecosse ) THEN
    nvars_required                = nvars_required + 1
    required_vars(nvars_required) = 'clay'
    nvars_required                = nvars_required + 1
    required_vars(nvars_required) = 'soil_ph'
  END IF

  !----------------------------------------------------------------------------
  ! Determine whether to append _soilt as well to tell model_interface_mod
  ! that the variables read in will have a soil tile dimension.
  ! This is required when l_tile_soil = T, and l_broadcast_ancils = F.
  !----------------------------------------------------------------------------
  IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils ) THEN
    DO i = 1,nvars
      var(i) = TRIM(var(i)) // "_soilt"
    END DO

    DO i = 1,nvars_required
      required_vars(i) = TRIM(required_vars(i)) // "_soilt"
    END DO
  END IF

  !---------------------------------------------------------------------------
  ! Constant Z (i.e. spatially varying but constant through vertical levels)
  ! is implemented by having a separate input variable in
  ! model_interface_mod called <var>_const_z that has no vertical levels.
  ! Hence, once the previous check is done, we add _const_z to both
  ! required and provided variable identifiers if asked for.
  !---------------------------------------------------------------------------
  soil_props_const_z = const_z
  IF ( soil_props_const_z ) THEN
    DO i = 1,nvars
      ! Don't change variables that do not have multiple levels.
      IF ( var(i) /= 'albsoil' ) THEN
        var(i) = TRIM(var(i)) // "_const_z"
      END IF
    END DO

    DO i = 1,nvars_required
      ! Don't change variables that do not have multiple levels.
      IF ( required_vars(i) /= 'albsoil' ) THEN
        required_vars(i) = TRIM(required_vars(i)) // "_const_z"
      END IF
    END DO
  END IF  !  soil_ancil_const_z

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
  ! ancil_dump_read%soil_props = .TRUE.
  ! Values will be read from the dump file.
  !----------------------------------------------------------------------------
  CALL log_info(RoutineName,                                                   &
                "soil properties will be read from the dump file.  " //        &
                "Namelist values ignored")

END IF  ! ancil_dump_read%soil_props

RETURN

END SUBROUTINE init_soil_props
END MODULE init_soil_props_mod
#endif
