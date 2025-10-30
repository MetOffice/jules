#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE fill_variables_from_file_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE fill_variables_from_file(file_name, identifiers, sdf_names,         &
                                    is_climatology)

USE input_mod, ONLY: grid, use_subgrid, subgrid

USE io_constants, ONLY: mode_read, max_sdf_name_len, max_dim_file, max_dim_var

USE dictionary_mod, ONLY: dict, dict_create, dict_get, dict_set,               &
                                 dict_has_key, dict_free

USE data_cube_mod, ONLY: data_cube, cube_free

USE model_interface_mod, ONLY: get_var_id, get_var_levs_dims, populate_var

USE file_gridded_mod, ONLY: file_gridded, file_gridded_open,                   &
                             file_gridded_def_grid, file_gridded_def_dim,      &
                             file_gridded_def_var, file_gridded_enddef,        &
                             file_gridded_read_var, file_gridded_close

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Fills the model variables specified by the given identifiers using
!   variables from the given file specified by sdf_names
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: file_name
    ! The name of the file to open
CHARACTER(LEN=*), INTENT(IN) :: identifiers(:)
    ! The model identifiers of the variables to fill
CHARACTER(LEN=*), INTENT(IN) :: sdf_names(:)
    ! The name of the variable in the file for each identifier
LOGICAL, INTENT(IN) :: is_climatology(:)
    ! Indicates whether the ancillary input is a climatology

!-----------------------------------------------------------------------------
! Work variables
!-----------------------------------------------------------------------------
! Variables that need to be carried over from the definition phase to the
! reading phase
INTEGER :: nvars  ! The number of variables requested
INTEGER :: var_ids(SIZE(identifiers))
                  ! The variable ids as used by model_interface_mod
INTEGER :: var_file_ids(SIZE(identifiers))
                  ! The ids of the variables in the opened file
CHARACTER(LEN=LEN(sdf_names)) :: sdf_name_local
                  ! A local copy of a value from sdf_names, for alteration.
TYPE(file_gridded) :: in_file  ! The file object that we are reading from


! Variables used during definition loop
INTEGER :: ndims  ! The number of levels dimensions the current variable has
CHARACTER(LEN=max_sdf_name_len) :: dim_names(max_dim_var)
                  ! The names of the dimensions for the current variable
INTEGER :: dim_sizes(max_dim_var)
                  ! The sizes of the dimensions for the current variable
INTEGER :: dim_ids(max_dim_var)
                  ! The ids of the dimensions for the current variable
TYPE(dict) :: file_dim_ids  ! Dictionary containing the dimension ids in file
                            ! Maps dim_name => dim_id

! Workspace cube that can be deallocated to avoid memory leaks
TYPE(data_cube) :: DATA

INTEGER :: i, j  ! Loop counters

!-----------------------------------------------------------------------------
! Initialise
!-----------------------------------------------------------------------------
file_dim_ids = dict_create(max_dim_file, INT(1))

!-----------------------------------------------------------------------------
! Check arguments
!-----------------------------------------------------------------------------
nvars = SIZE(identifiers)
IF ( nvars /= SIZE(sdf_names) )                                                &
  CALL log_fatal("fill_variables_from_file",                                   &
                 "identifiers and sdf_names must have the same number " //     &
                 "of elements")

IF ( nvars /= SIZE(is_climatology) ) THEN
  CALL log_fatal("fill_variables_from_file",                                   &
                 "is_climatology does not have nvars elements")
END IF

!-----------------------------------------------------------------------------
! Map the string identifiers to their integer ids
!-----------------------------------------------------------------------------
DO i = 1,nvars
  var_ids(i) = get_var_id(identifiers(i))
END DO

!-----------------------------------------------------------------------------
! Open the given file and define its grid
!-----------------------------------------------------------------------------
in_file = file_gridded_open(file_name, mode_read)

! Define the grid
CALL file_gridded_def_grid(in_file, grid)

!-----------------------------------------------------------------------------
! Define the required dimensions and variables
!-----------------------------------------------------------------------------
DO i = 1,nvars

  !   If sdf_names is empty, use the identifier.
  sdf_name_local = sdf_names(i)
  IF ( LEN_TRIM(sdf_name_local) == 0 ) THEN
    !     Check variable is long enough.
    !     Note that a fatal error will not occur as long as the code declares
    !     sdf_names with lengths >= those of identifiers.
    IF ( LEN_TRIM(identifiers(i)) > LEN(sdf_name_local) )                      &
      CALL log_fatal("fill_variables_from_file",                               &
                     "identifier too long for sdf_name." //                    &
                     " file: " // TRIM(file_name) //                           &
                     " identifier: " // TRIM(identifiers(i)) )
    sdf_name_local = identifiers(i)
  END IF

  ! Get the levels dims used by this variable - we only care about the names
  ! used in input files
  CALL get_var_levs_dims(var_ids(i), is_climatology(i), ndims = ndims,         &
                         dim_names_in = dim_names, dim_sizes = dim_sizes)

  DO j = 1,ndims
    ! If it has not yet been defined, define the dimension, storing its id
    IF ( .NOT. dict_has_key(file_dim_ids, dim_names(j)) )                      &
      CALL dict_set(                                                           &
        file_dim_ids, dim_names(j),                                            &
        file_gridded_def_dim(in_file, dim_names(j), dim_sizes(j))              &
      )

    ! Get the dimension id from the dict and add it to the list for this
    ! variable
    CALL dict_get(file_dim_ids, dim_names(j), dim_ids(j))
  END DO

  ! Create the variable and store its id
  var_file_ids(i) = file_gridded_def_var(                                      &
    in_file, sdf_name_local, dim_ids(1:ndims), .FALSE.                         &
  )
END DO

!-----------------------------------------------------------------------------
! We have finished defining things!
!-----------------------------------------------------------------------------
CALL dict_free(file_dim_ids)
CALL file_gridded_enddef(in_file)

!-----------------------------------------------------------------------------
! Read each variable and fill the appropriate model variable
!-----------------------------------------------------------------------------
DO i = 1,nvars
  ! Read the data cube from file and pass it to populate_var
  DATA = file_gridded_read_var(in_file, var_file_ids(i),                       &
                               use_subgrid, subgrid)
  CALL populate_var(var_ids(i), DATA)
  CALL cube_free(DATA)
END DO

CALL file_gridded_close(in_file)

RETURN

END SUBROUTINE fill_variables_from_file
END MODULE fill_variables_from_file_mod
#endif
