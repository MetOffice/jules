#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_urban_props_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_urban_props(ainfo,urban_param)

!Use in subroutines
USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE tilepts_mod, ONLY: tilepts

USE urban_empirical_morphology_mod, ONLY: urban_empirical_morphology
USE calc_urban_aero_fields_mod,     ONLY: calc_urban_aero_fields

!Use in variables
USE io_constants, ONLY: max_sdf_name_len, max_file_name_len, namelist_unit

USE model_interface_mod, ONLY: identifier_len

USE ancil_info, ONLY: land_pts

USE jules_surface_types_mod, ONLY: urban_canyon, urban_roof

USE jules_urban_mod, ONLY: l_urban_empirical, l_moruses_macdonald, l_moruses

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

!TYPE definitions
USE ancil_info,    ONLY: ainfo_type
USE urban_param_mod, ONLY: urban_param_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises urban parameters and properties
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!Arguments

TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param

! Work variables
INTEGER, PARAMETER :: nvars_max = 9
       ! The maximum possible number of TOPMODEL variables that can be given

INTEGER :: nvars_required      ! The number of variables that are
                               ! required in this configuration
CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
                               ! The variable identifiers of the required
                               ! variables
INTEGER :: nvars_const
    ! The number of variables to be set using constant values.

INTEGER :: nvars_file       ! The number of variables that will be set
                            ! from the given file (template?)

INTEGER :: ERROR  ! Error indicator

LOGICAL ::                                                                     &
  l_file_list
    ! Flag indicating if we have a list of files either as variable-name
    ! templating is being used or we have supplied a list of files.

CHARACTER(LEN=errormessagelength) :: iomessage

LOGICAL ::                                                                     &
  use_var(nvars_max),                                                          &
    ! Flag indicating if a variable is required.
  is_climatology(nvars_max) = .FALSE.
    ! Flag to indicate var is from a 12-month climatology, requiring
    ! interpolation to the correct time.

CHARACTER(LEN=max_file_name_len) :: file_name(nvars_max)
    ! The name of the file to use for each variable.

!-----------------------------------------------------------------------------
! Definition of the urban_properties namelist - this specifies how urban
! properties are set
!-----------------------------------------------------------------------------
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
REAL(KIND=real_jlslsm) :: const_val(nvars_max)
                      ! The constant value to use for each variable if
                      ! use_file = F for that variable
CHARACTER(LEN=*), PARAMETER :: RoutineName='init_urban_props'

NAMELIST  /urban_properties/ FILE, nvars, var, use_file, read_list, var_name,  &
   tpl_name, const_val

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
! Initialise some common variables.
CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,                 &
                               nvars_required, l_file_list,                    &
                               FILE, const_val, use_file, use_var,             &
                               file_name, tpl_name, var, var_name )

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading URBAN_PROPERTIES namelist...")
READ(namelist_unit, NML = urban_properties, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                 "Error reading namelist URBAN_PROPERTIES " //                 &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Process the namelist values and set derived variables
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Check that the run actually has urban and that urban schemes are not run
! in error
!-----------------------------------------------------------------------------
CALL tilepts(land_pts, ainfo%frac_surft, ainfo%surft_pts, ainfo%surft_index,   &
             ainfo%l_lice_point, ainfo%l_lice_surft)
IF ( ainfo%surft_pts(urban_canyon) == 0 )                                      &
  CALL log_warn(RoutineName,                                                   &
                "URBAN-2T or MORUSES is selected but there are no " //         &
                "urban land points - extra calculations may be being " //      &
                "performed that will not impact on results")

!-----------------------------------------------------------------------------
! Read values for urban properties
!-----------------------------------------------------------------------------
! Set up the required variables
! First, variables that are always required and can't be derived
nvars_required = 4
required_vars(1:nvars_required) = [ 'albwl', 'albrd', 'emisw', 'emisr' ]

IF ( l_moruses ) THEN
  IF ( .NOT. l_urban_empirical ) THEN
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'wrr'
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'hwr'
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'hgt'
  END IF
  IF ( .NOT. l_moruses_macdonald ) THEN
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'ztm'
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'disp'
  END IF
ELSE
  ! For urban2t, we only need wrr if there are no urban_roof points. This
  ! indicates that total urban fraction is contained in frac(urban_canyon)
  IF ( ainfo%surft_pts(urban_roof) == 0 .AND.                                  &
       ainfo%surft_pts(urban_canyon) > 0 ) THEN
    CALL log_info(RoutineName,                                                 &
       "wrr is a required variable as total urban fraction supplied")
    nvars_required = 1
    required_vars(1) = 'wrr'
  END IF
END IF

!------------------------------------------------------------------------------
! Check that variables in the namelist have been provided and set up other
! variables.
!------------------------------------------------------------------------------
CALL check_namelist_values( nvars, nvars_max, nvars_required,                  &
                            RoutineName, FILE, const_val,                      &
                            use_file, read_list, required_vars,                &
                            tpl_name, var, nvars_const, nvars_file,            &
                            l_file_list, use_var, file_name, var_name )

!------------------------------------------------------------------------------
! Set variables that are using a constant value.
!------------------------------------------------------------------------------
CALL set_constant_vars( nvars, nvars_const, nvars_max, const_val,              &
                        use_file, use_var, var )

!------------------------------------------------------------------------------
! Set variables that are to be read from a file.
!------------------------------------------------------------------------------
CALL set_file_vars( nvars, nvars_file, nvars_max, l_file_list,                 &
                    RoutineName, use_file, use_var, file_name, var,            &
                    var_name, is_climatology )

IF ( l_urban_empirical ) THEN
  CALL urban_empirical_morphology(ainfo%frac_surft, ainfo%surft_index,         &
                                  ainfo%surft_pts, urban_param)
END IF

IF ( l_moruses_macdonald ) THEN
  CALL calc_urban_aero_fields(land_pts, urban_param%wrr_gb,                    &
                              urban_param%hwr_gb, urban_param%hgt_gb,          &
                              urban_param%ztm_gb, urban_param%disp_gb)
END IF

RETURN

END SUBROUTINE init_urban_props
END MODULE init_urban_props_mod
#endif
