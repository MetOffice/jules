#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE register_input_file(data_start, data_end, data_period,              &
                               is_climatology, use_time_template, template,    &
                               file_names, file_times, identifiers,            &
                               sdf_names, interp_flags)

USE io_constants, ONLY: max_file_name_len, mode_read, max_sdf_name_len,        &
                         max_dim_file, max_dim_var

USE model_time_mod, ONLY: run_min_time, run_max_time, timestep_len

USE datetime_mod, ONLY: period_day, period_month, period_year, datetime_advance

USE dictionary_mod, ONLY: dict, dict_create, dict_get, dict_set,               &
                           dict_has_key, dict_free

USE file_ts_mod, ONLY: file_ts_open, file_ts_def_grid, file_ts_def_dim,        &
                        file_ts_def_time_dim, file_ts_def_var,                 &
                        file_ts_enddef

USE model_interface_mod, ONLY: get_var_id, get_var_levs_dims

USE interpolation_mod, ONLY: get_required_time_bounds

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Registers a file or group of files as providing time-varying data for the
!   model variables specified by the given identifiers
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Argument types
!-----------------------------------------------------------------------------
TYPE(datetime), INTENT(IN) :: data_start
                            ! The date and time of the first data
TYPE(datetime), INTENT(IN) :: data_end
                            ! The date and time of the last data
INTEGER, INTENT(IN) :: data_period
                            ! The period of the data
                            ! (in seconds or a 'special' period)
LOGICAL, INTENT(IN) :: is_climatology
                            ! .TRUE. - the data is a climatology
                            ! .FALSE. - the data is not a climatology

LOGICAL, INTENT(IN) :: use_time_template
                            ! .TRUE. - use time templating
                            ! .FALSE. - use lists of file names and times
                            !           of first data in each file

! With time templating
CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: template
                            ! The time template to use

! With a file list
CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file_names(:)
                            ! List of file names
TYPE(datetime), INTENT(IN), OPTIONAL :: file_times(:)
                            ! Time of first data for each file

CHARACTER(LEN=*), INTENT(IN) :: identifiers(:)
                            ! The model identifiers of the variables
                            ! this file(s) provides data for
CHARACTER(LEN=*), INTENT(IN) :: sdf_names(:)
                            ! The name of each variable in the file(s)
CHARACTER(LEN=*), INTENT(IN) :: interp_flags(:)
                            ! The type of interpolation to use for each
                            ! variable


!-----------------------------------------------------------------------------
! Work variables
!-----------------------------------------------------------------------------
! Local variables passed to file_ts_open
! In the case where optional variables are not given, these are set to values
! that will cause errors if they are required, but will be ignored if they
! are not required
CHARACTER(LEN=max_file_name_len) :: template_local
CHARACTER(LEN=max_file_name_len), ALLOCATABLE :: file_names_local(:)
TYPE(datetime), ALLOCATABLE :: file_times_local(:)

INTEGER :: nvars  ! The number of variables in the file

TYPE(dict) :: file_dim_ids  ! Dictionary containing the dimension ids defined
                            ! so far
                            ! Maps dim_name => dim_id

! Used to define dimensions and variables in file
INTEGER :: ndims  ! The number of levels dimensions the current variable has
CHARACTER(LEN=max_sdf_name_len) :: dim_names(max_dim_var)
                  ! The levels dimension names for the current variable
INTEGER :: dim_sizes(max_dim_var)
                  ! The sizes of the levels dimensions for the current variable
INTEGER :: dim_ids(max_dim_var)
                  ! The dimension ids in file of the levels dimensions for the
                  ! current variable

INTEGER :: dummy  ! This is used to store the result from defining the time
                  ! dimension - we don't need to keep it
CHARACTER(LEN=LEN(sdf_names)) :: sdf_name_local
                  ! A local copy of a value from sdf_names, for alteration.

INTEGER :: i, j  ! Index variables


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Check arguments
!-----------------------------------------------------------------------------
! Check that we have space to register another file
IF ( nfiles >= nfiles_max )                                                    &
  CALL log_fatal("register_input_file",                                        &
                 "Too many files registered - try increasing NFILES_MAX")

! Check that we have an sdf_name and an interpolation flag for each variable
nvars = SIZE(identifiers)
IF ( nvars /= SIZE(sdf_names) )                                                &
  CALL log_fatal("register_input_file",                                        &
                 "identifiers and sdf_names must have the same number " //     &
                 "of elements")
IF ( nvars /= SIZE(interp_flags) )                                             &
  CALL log_fatal("register_input_file",                                        &
                 "identifiers and interp_flags must have the same " //         &
                 "number of elements")


!-----------------------------------------------------------------------------
! Check that data will be provided for the whole run at an appropriate
! timestep
!-----------------------------------------------------------------------------
! If not using a climatology, data must be provided for the whole run
! If using a climatology, the data must cover a whole year, but this is checked
! by file_ts_open
IF ( .NOT. is_climatology .AND.                                                &
     ( data_start > run_min_time .OR. data_end < run_max_time ) )              &
  CALL log_fatal("register_input_file",                                        &
                 "Each input file must provide data for the entire run")

! Unless data is given on a 'special' period (a month or year), the data period
! must be a whole number of model timesteps. We explicitly test for the special
! period period_day as the rest of the code cannot support that as a data
! period (only as a file period for templating) - but the user can be directed
! to use data_period=86400s in that case.
IF ( data_period /= period_month .AND. data_period /= period_year .AND.        &
     MOD(data_period, timestep_len) /= 0 ) THEN
  IF ( data_period == period_day ) THEN
    CALL log_fatal("register_input_file",                                      &
                   "Data period cannot be the special period period_day. " //  &
                   "Use data period = 86400 seconds to represent daily data.")
  ELSE
    CALL log_fatal("register_input_file",                                      &
                   "Data period must be a special period (1 month or " //      &
                   "1 year) or a multiple of timestep length")
  END IF
END IF


!-----------------------------------------------------------------------------
! Set up the optional arguments depending on what is available
!-----------------------------------------------------------------------------
IF ( PRESENT(template) ) THEN
  template_local = template
ELSE
  ! If template is not provided, we provide an empty string
  ! This will cause an error unless a file list is specified
  template_local = ""
END IF

IF ( PRESENT(file_names) ) THEN
  ! If file_names is present, copy its values into the local counterpart
  ! that has fixed length strings
  ALLOCATE(file_names_local(SIZE(file_names)))
  file_names_local(:) = file_names(:)
ELSE
  ! If file_names is not provided, we provide one file that doesn't exist
  ! This will cause an error unless time templating is specified
  ALLOCATE(file_names_local(1))
  file_names_local(1) = ""
END IF

IF ( PRESENT(file_times) ) THEN
  ! If file_times is present, copy its values into the local counterpart
  ALLOCATE(file_times_local(SIZE(file_times)))
  file_times_local(:) = file_times(:)
ELSE
  ! If file_times is not provided, we provide one time that is not equal to
  ! data_start
  ! This will cause an error unless time templating is specified
  ALLOCATE(file_times_local(1))
  file_times_local(1) = datetime_advance(data_start, 1)
END IF


!-----------------------------------------------------------------------------
! Open the file handle
!-----------------------------------------------------------------------------
nfiles = nfiles + 1

files(nfiles)%fh = file_ts_open(mode_read, data_start, data_end,               &
                                data_period, is_climatology,                   &
                                use_time_template, template_local,             &
                                file_names_local, file_times_local)

! We have finished with the local pointers
DEALLOCATE(file_names_local)
DEALLOCATE(file_times_local)


!-----------------------------------------------------------------------------
! Define the grid
!-----------------------------------------------------------------------------
CALL file_ts_def_grid(files(nfiles)%fh, grid)


!-----------------------------------------------------------------------------
! Define the time dimension
!-----------------------------------------------------------------------------
dummy = file_ts_def_time_dim(files(nfiles)%fh, time_dim_name)


!-----------------------------------------------------------------------------
! Allocate space for the input fields
!-----------------------------------------------------------------------------
files(nfiles)%nfields = nvars
ALLOCATE(files(nfiles)%fields(nvars))


!-----------------------------------------------------------------------------
! Get the upper and lower bounds to use for the time dimension based on what
! interpolation each variable is using
!-----------------------------------------------------------------------------
CALL get_required_time_bounds(                                                 &
  interp_flags, files(nfiles)%times_lbound, files(nfiles)%times_ubound         &
)


!-----------------------------------------------------------------------------
! Define the required variables
!-----------------------------------------------------------------------------
! Define a dictionary to gather the dimension ids
file_dim_ids = dict_create(max_dim_file, INT(1))

DO i = 1,nvars

  ! Get the integer id from model_interface_mod for the identifier
  files(nfiles)%fields(i)%var_id = get_var_id(identifiers(i))

  ! Get the levels dims used by this variable - we only care about the names
  ! used in input files
  ! The functionality to initialise some initial conditions from a climatology
  ! has been added. For this purpose the input fields require a time
  ! dimension to be added by get_var_levs_dim. This is not required here and
  ! hence the following is_climatology argument must be .FALSE.
  CALL get_var_levs_dims(files(nfiles)%fields(i)%var_id,                       &
                         is_climatology = .FALSE., ndims = ndims,              &
                         dim_names_in = dim_names, dim_sizes = dim_sizes)

  DO j = 1,ndims
    ! If it has not yet been defined, define the dimension, storing its id
    IF ( .NOT. dict_has_key(file_dim_ids, dim_names(j)) )                      &
      CALL dict_set(                                                           &
        file_dim_ids, dim_names(j),                                            &
        file_ts_def_dim(files(nfiles)%fh, dim_names(j), dim_sizes(j))          &
      )

    ! Get the dimension id from the dict and add it to the list for this variable
    CALL dict_get(file_dim_ids, dim_names(j), dim_ids(j))
  END DO

  ! If sdf_names is empty, use the identifier.
  sdf_name_local = sdf_names(i)
  IF ( LEN_TRIM(sdf_name_local) == 0 ) THEN
    !     Check variable is long enough.
    !     Note that a fatal error will not occur as long as the code declares
    !     sdf_names with lengths >= those of identifiers.
    IF ( LEN_TRIM(identifiers(i)) > LEN(sdf_name_local) )                      &
      CALL log_fatal("register_input_file",                                    &
                     "identifier too long for sdf_name. " //                   &
                     "identifier: " // TRIM(identifiers(i)) )
    sdf_name_local = identifiers(i)
  END IF

  ! Create the variable and store its id
  files(nfiles)%fields(i)%file_id = file_ts_def_var(                           &
    files(nfiles)%fh, sdf_name_local, dim_ids(1:ndims), .TRUE.                 &
  )

  ! Allocate space for the data cubes for the field
  ALLOCATE(files(nfiles)%fields(i)%DATA(                                       &
    files(nfiles)%times_lbound:files(nfiles)%times_ubound                      &
  ))

  ! Store the interpolation flag for the field
  files(nfiles)%fields(i)%interp_flag = interp_flags(i)

END DO


!-----------------------------------------------------------------------------
! We are done - clean up
!-----------------------------------------------------------------------------
! Free the dimensions dictionary
CALL dict_free(file_dim_ids)

! Take the file out of define mode
CALL file_ts_enddef(files(nfiles)%fh)

RETURN

END SUBROUTINE register_input_file
#endif
