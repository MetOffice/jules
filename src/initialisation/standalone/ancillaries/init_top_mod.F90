#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_top_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_top()

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE io_constants, ONLY: max_file_name_len, max_sdf_name_len, namelist_unit

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE model_interface_mod, ONLY: identifier_len

USE jules_hydrology_mod, ONLY: l_top

USE jules_soil_mod, ONLY: l_tile_soil, l_broadcast_ancils

USE dump_mod, ONLY: ancil_dump_read

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in the TOPMODEL properties and checks them for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
INTEGER, PARAMETER :: nvars_max = 3
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

INTEGER :: i  ! Loop counter

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
CHARACTER(LEN=identifier_len) :: file_var(nvars_max)
    ! Identifiers of the variables to be set from a file.
CHARACTER(LEN=max_sdf_name_len) :: file_var_name(nvars_max)
    ! The name of each variable in the file.
CHARACTER(LEN=max_sdf_name_len) :: file_tpl_name(nvars_max)
    ! The name to substitute in a template for each variable.

!-----------------------------------------------------------------------------
! Definition of the jules_top namelist - this combines local variables with
! some from c_topog
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
NAMELIST  /jules_top/ read_from_dump, FILE, nvars, var, use_file, read_list,   &
   var_name, tpl_name, const_val


CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_TOP'

!-----------------------------------------------------------------------------

! If TOPMODEL is not on, we have nothing to do
IF ( .NOT. l_top ) RETURN


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
CALL log_info("init_top", "Reading JULES_TOP namelist...")

READ(namelist_unit, NML = jules_top, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_top",                                                   &
                 "Error reading namelist JULES_TOP " //                        &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

ancil_dump_read%top = read_from_dump

IF ( .NOT. ancil_dump_read%top) THEN !we read from the ancil file
  !---------------------------------------------------------------------------
  ! Set up TOPMODEL properties using namelist values
  !---------------------------------------------------------------------------
  ! Set up the required variables
  ! All the TOPMODEL variables are always required
  nvars_required = nvars_max
  ! ti_skew may be available in later releases
  required_vars(:) = [ 'fexp   ', 'ti_mean', 'ti_sig ' ]

  !---------------------------------------------------------------------------
  ! Determine whether to append _soilt as well to tell model_interface_mod
  ! whether the variables read in will have a soil tile dimension.
  ! This is required when l_tile_soil = T, and l_broadcast_ancils = F.
  !---------------------------------------------------------------------------
  IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils ) THEN
    DO i = 1,nvars
      var(i) = TRIM(var(i)) // "_soilt"
    END DO

    DO i = 1,nvars_required
      required_vars(i) = TRIM(required_vars(i)) // "_soilt"
    END DO
  END IF

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

ELSE !We read from the dump file
  CALL log_info("init_top",                                                    &
                "topmodel ancils will be read from the dump file.  " //        &
                "Namelist values ignored")

END IF !.NOT. ancil_dump_read%top

RETURN

END SUBROUTINE init_top
END MODULE init_top_mod
#endif
