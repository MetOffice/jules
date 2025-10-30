#if !defined(UM_JULES)

! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Reads in JULES_CROP_PROPS namelist if necessary

MODULE init_crop_props_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_crop_props()

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE io_constants, ONLY: max_file_name_len, max_sdf_name_len, namelist_unit

USE model_interface_mod, ONLY: identifier_len

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE string_utils_mod, ONLY: to_string

USE jules_vegetation_mod, ONLY: l_crop, l_prescsow, l_croprotate

USE dump_mod, ONLY: ancil_dump_read

USE errormessagelength_mod, ONLY: errormessagelength

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!    Reads in TTveg, TTrep
!    Reads in the sowing date for each of the crop pfts if l_prescsow=T
!    and the latest possible harvest date for the crop pfts if provided
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
INTEGER, PARAMETER :: nvars_max = 4
       ! The maximum possible number of crop variables that can be given
       ! includes the croplatestharvdate which is only a required variable
       ! when l_croprotate=T and optional when l_croprotate=F.

INTEGER :: nvars_required      ! The number of variables that are
                               ! required in this configuration

CHARACTER(LEN=identifier_len), ALLOCATABLE :: required_vars(:)
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

!-----------------------------------------------------------------------------
! Definition of the jules_crop_props namelist
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
CHARACTER(LEN=errormessagelength) :: iomessage
                      ! Error message string for I/O errors
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
NAMELIST  /jules_crop_props/                                                   &
                     read_from_dump, FILE, nvars, var, use_file, read_list,    &
                     var_name, tpl_name, const_val

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_CROP_PROPS'

!-----------------------------------------------------------------------------

! Nothing to do if crop model is not on
IF ( .NOT. l_crop ) RETURN

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
CALL log_info(RoutineName, "Reading JULES_CROP_PROPS namelist...")

READ(namelist_unit, NML = jules_crop_props, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(RoutineName,                                                  &
                "Error reading namelist JULES_CROP_PROPS " //                  &
                "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //           &
                TRIM(iomessage) // ")")

ancil_dump_read%crop_props = read_from_dump

IF ( .NOT. ancil_dump_read%crop_props) THEN !we read from the ancil file

  !-------------------------------------------------------------------------
  ! Set up crop properties using namelist values
  !-------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  ! Check that variable identifiers are not empty.
  ! Although we might later decide that the identifier is not required, for
  ! clarity we check here whether the claimed amount of information was
  ! provided.
  !-------------------------------------------------------------------------
  DO i = 1,nvars
    IF ( LEN_TRIM(var(i)) == 0 )                                               &
      CALL log_fatal(RoutineName,                                              &
                     "Insufficient values for var. " //                        &
                     "No name provided for var at position #" //               &
                     TRIM(to_string(i)) )
  END DO

  ! Set up the required variables

  IF (l_prescsow) THEN
    IF ( (l_croprotate) .AND.                                                  &
         .NOT. (ANY(var(1:nvars) == 'croplatestharvdate')) )                   &

      CALL log_fatal(RoutineName,                                              &
                     "If l_croprotate is T then value for "    //              &
                     "latestharvdate must be provided"         //              &
                     TRIM(to_string(i)) )

    IF (ANY(var(1:nvars) == 'croplatestharvdate')) THEN
      nvars_required = nvars_max
      ALLOCATE(required_vars(nvars_required))
      required_vars(:) = [ 'cropsowdate       ', 'cropttveg         ',         &
                            'cropttrep         ', 'croplatestharvdate'     ]
    ELSE
      nvars_required = 3
      ALLOCATE(required_vars(nvars_required))
      required_vars(:) = [ 'cropsowdate', 'cropttveg  ', 'cropttrep  ' ]
    END IF
  ELSE
    nvars_required = 2
    ALLOCATE(required_vars(nvars_required))
    required_vars(:) = [ 'cropttveg', 'cropttrep' ]
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

  DEALLOCATE(required_vars)

ELSE !We read from the dump file
  CALL log_info(RoutineName,                                                   &
                "crop properties will be read from the dump file.  " //        &
                "Namelist values ignored")

END IF !.NOT. ancil_dump_read%crop_props

END SUBROUTINE init_crop_props
END MODULE init_crop_props_mod
#endif
