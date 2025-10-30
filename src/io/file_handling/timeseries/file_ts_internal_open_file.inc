#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ts_internal_open_file(FILE, file_name)

USE io_constants, ONLY: mode_write, attr_global

USE datetime_mod, ONLY: l_360, l_leap, datetime_to_string, l_local_solar_time

USE dictionary_mod, ONLY: dict_key_len, dict_char_val_len, dict_get

USE data_cube_mod, ONLY: data_cube, cube_safe_copy, cube_free,                 &
                         cube_collapse_dimension

USE file_gridded_mod, ONLY: file_gridded_open, file_gridded_def_grid,          &
                             file_gridded_def_dim,                             &
                             file_gridded_def_record_dim,                      &
                             file_gridded_def_var, file_gridded_def_attr,      &
                             file_gridded_enddef, file_gridded_close

USE file_mod, ONLY: file_def_dim, file_def_var, file_def_attr

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   INTERNAL PROCEDURE TO file_ts_mod
!   Sets the currently open file to that specified by file_name, including
!   defining dimensions, variables etc.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ts), INTENT(IN OUT) :: FILE  ! The file to update
CHARACTER(LEN=*), INTENT(IN) :: file_name  ! The file to open and set as
                                           ! current file


! Work variables
INTEGER :: attr_var_id    ! Variable ID to use when defining attributes

CHARACTER(LEN=dict_key_len) :: key  ! Used when iterating over attribute
INTEGER :: int_val                  ! dictionaries
REAL :: real_val
CHARACTER(LEN=dict_char_val_len) :: char_val

INTEGER, ALLOCATABLE :: zero_element_array(:)  ! This array is allocated
                                               ! to have size 0 - used to
                                               ! indicate passing no
                                               ! dimensions to file_def_var
                                               ! for the time index


INTEGER :: n_non_time_vars  ! The number of non-time-varying variables
INTEGER :: nx, ny  ! Number of x and y points in the grid - used to
                   ! dimension array below
TYPE(data_cube), ALLOCATABLE :: non_time_varying_data(:)
                         ! Array to hold the data cubes for non-time-varying
                         ! variables from the previous file until they can
                         ! be copied into the next file
TYPE(data_cube) :: temp  ! Temporary cube to read data in case we need to
                         ! collapse dimensions
INTEGER :: var_nlevs  ! The size of the vertical levels dimension for the
                      ! variable currently being processed
                      ! Only use when copying non-time-varying variables
                      ! from one file to another

INTEGER :: dim_id  ! Id of dimension for time_bounds

INTEGER :: i,j  ! Loop counters


!-----------------------------------------------------------------------------


IF ( FILE%has_open_file ) THEN
  IF ( FILE%mode == mode_write ) THEN
    !-----------------------------------------------------------------------------
    ! If we are in write mode, we want to copy the values of all the variables
    ! not using the time dimensions into the new file
    ! To do that, we have to grab their values here
    !-----------------------------------------------------------------------------
    ! Only bother doing this if we have non-time-varying variables
    n_non_time_vars = COUNT( .NOT. FILE%vars(1:FILE%nvars)%use_time)
    IF ( n_non_time_vars > 0 ) THEN
      ! We allocate (potentially) too much data, since we only need it for this routine
      ALLOCATE(non_time_varying_data(n_non_time_vars))

      ! Gather the data for the non-time varying variables
      j = 1
      DO i = 1,FILE%nvars
        IF ( .NOT. FILE%vars(i)%use_time ) THEN

          ! Read into a temporary cube, which is always a 2d grid
          ! We are not using a subgrid, so give extract_subgrid=F
          temp = file_ts_read_var(FILE, i, .FALSE.)

          IF ( FILE%grid%is_1d ) THEN
            ! If necessary, collapse the degenerate y dimension to get
            ! a cube with a 1d grid
            ! We know that this is dimension number 2
            CALL cube_collapse_dimension(non_time_varying_data(j), temp, 2)
          ELSE
            ! Otherwise, just copy the cube
            CALL cube_safe_copy(non_time_varying_data(j), temp)
          END IF

          ! Advance the non-time variable counter
          j = j + 1
        END IF
      END DO
    END IF
  END IF

  !-----------------------------------------------------------------------------
  ! Free the final temporary cube
  !-----------------------------------------------------------------------------
  CALL cube_free(temp)

  !-----------------------------------------------------------------------------
  ! Close the currently open file
  !-----------------------------------------------------------------------------
  CALL file_gridded_close(FILE%open_file)
END IF

!-----------------------------------------------------------------------------
! Open the new file
!-----------------------------------------------------------------------------
IF ( FILE%use_mpiio ) THEN
  ! If we are using parallel I/O, then pass MPI variables to file_gridded_open
  FILE%open_file = file_gridded_open(                                          &
    file_name, FILE%mode, FILE%comm, FILE%info                                 &
  )
ELSE
  ! Otherwise omit the MPI variables
  FILE%open_file = file_gridded_open(file_name, FILE%mode)
END IF
FILE%has_open_file = .TRUE.

!-----------------------------------------------------------------------------
! Define the grid on the newly opened file
!-----------------------------------------------------------------------------
CALL file_gridded_def_grid(FILE%open_file, FILE%grid)

!-----------------------------------------------------------------------------
! Define the vertical level dimensions and update the dimension ids to the
! ones used by the newly opened file
!-----------------------------------------------------------------------------
DO i = 1,FILE%ndims
  FILE%dims(i)%current_id = file_gridded_def_dim(                              &
    FILE%open_file, FILE%dims(i)%NAME, FILE%dims(i)%length                     &
  )
END DO

!-----------------------------------------------------------------------------
! Define the time dimension as a record dimension on the open file and update
! the stored id to the one used by the newly opened file
!-----------------------------------------------------------------------------
FILE%time_dim%current_id = file_gridded_def_record_dim(                        &
  FILE%open_file, FILE%time_dim%NAME                                           &
)

!-----------------------------------------------------------------------------
! Define the time indexes if we are in write mode
!
! We define a variable time_bounds that defines the start and end of each
! "time cell". This is associated with the time coordinate using the
! "bounds" attribute.
!-----------------------------------------------------------------------------
! Since the time bounds and index do not want grid semantics, we have to go
! down to the raw file routines
IF ( FILE%mode == mode_write ) THEN
  !-----------------------------------------------------------------------------
  ! Create the time_bounds variable
  !-----------------------------------------------------------------------------
  ! Before creating the variable, we need to create a dimension of size 2 to use
  dim_id = file_def_dim(FILE%open_file%fh, "nt", 2)
  FILE%time_bounds_var_id = file_def_var(                                      &
    FILE%open_file%fh, "time_bounds", [ dim_id ], .TRUE.                       &
  )

  ! Define it's attributes
  CALL file_def_attr(                                                          &
    FILE%open_file%fh, FILE%time_bounds_var_id,                                &
    "long_name", "Time bounds for each time stamp"                             &
  )
  CALL file_def_attr(                                                          &
    FILE%open_file%fh, FILE%time_bounds_var_id, "units",                       &
    "seconds since " // datetime_to_string(FILE%data_start)                    &
  )

  !-----------------------------------------------------------------------------
  ! Create the time index
  !-----------------------------------------------------------------------------
  ! We pass an array with 0 elements as the non-record dimensions, but indicate
  ! we want to use the record dim
  ALLOCATE(zero_element_array(0))
  FILE%time_index_var_id = file_def_var(                                       &
    FILE%open_file%fh, time_index_var_name, zero_element_array, .TRUE.         &
  )
  DEALLOCATE(zero_element_array)

  ! Define it's name and units
  CALL file_def_attr(                                                          &
    FILE%open_file%fh, FILE%time_index_var_id, "standard_name", "time"         &
  )
  CALL file_def_attr(                                                          &
    FILE%open_file%fh, FILE%time_index_var_id, "long_name", "Time of data"     &
  )
  CALL file_def_attr(                                                          &
    FILE%open_file%fh, FILE%time_index_var_id, "units",                        &
    "seconds since " // datetime_to_string(FILE%data_start)                    &
  )
  CALL file_def_attr(                                                          &
    FILE%open_file%fh, FILE%time_index_var_id, "bounds", "time_bounds"         &
  )

  ! Add the calendar attribute
  IF ( l_360 ) THEN
    CALL file_def_attr(                                                        &
      FILE%open_file%fh, FILE%time_index_var_id, "calendar", "360_day"         &
    )
  ELSE IF ( l_leap ) THEN
    CALL file_def_attr(                                                        &
      FILE%open_file%fh, FILE%time_index_var_id, "calendar", "standard"        &
    )
  ELSE
    CALL file_def_attr(                                                        &
      FILE%open_file%fh, FILE%time_index_var_id, "calendar", "365_day"         &
    )
  END IF

  ! Add the attribute of local solar time
  IF ( l_local_solar_time ) THEN
    CALL file_def_attr(                                                        &
      FILE%open_file%fh, FILE%time_index_var_id, "Time Convention",            &
        "Local Solar Time"                                                     &
    )
  END IF

END IF

!-----------------------------------------------------------------------------
! Define the variables and update the variable ids to the ones used by the
! newly opened file
!-----------------------------------------------------------------------------
DO i = 1,FILE%nvars
  ! Define the variable, indicating whether to use the record dimension
  FILE%vars(i)%current_id = file_gridded_def_var(                              &
    FILE%open_file, FILE%vars(i)%NAME,                                         &
  ! We have to convert the dimension ids to ids in the underlying file
        FILE%dims( FILE%vars(i)%levels_dims )%current_id,                      &
        FILE%vars(i)%use_time                                                  &
      )

  ! Define the attributes for the variable
  ! First the real valued attributes
  DO j = 1,FILE%vars(i)%attrs_real%length
    key = FILE%vars(i)%attrs_real%keys(j)
    CALL dict_get(FILE%vars(i)%attrs_real, key, real_val)

    CALL file_gridded_def_attr(                                                &
      FILE%open_file, FILE%vars(i)%current_id, key, real_val                   &
    )
  END DO

  ! Next, integer valued attributes
  DO j = 1,FILE%vars(i)%attrs_int%length
    key = FILE%vars(i)%attrs_int%keys(j)
    CALL dict_get(FILE%vars(i)%attrs_int, key, int_val)

    CALL file_gridded_def_attr(                                                &
      FILE%open_file, FILE%vars(i)%current_id, key, int_val                    &
    )
  END DO

  ! Lastly, character valued attributes
  DO j = 1,FILE%vars(i)%attrs_char%length
    key = FILE%vars(i)%attrs_char%keys(j)
    CALL dict_get(FILE%vars(i)%attrs_char, key, char_val)

    CALL file_gridded_def_attr(                                                &
      FILE%open_file, FILE%vars(i)%current_id, key, char_val                   &
    )
  END DO
END DO

!-----------------------------------------------------------------------------
! Lastly, define global attributes
!-----------------------------------------------------------------------------
! First the real valued attributes
DO i = 1,FILE%attrs_real%length
  key = FILE%attrs_real%keys(i)
  CALL dict_get(FILE%attrs_real, key, real_val)

  CALL file_gridded_def_attr(FILE%open_file, attr_global, key, real_val)
END DO

! Next, integer valued attributes
DO i = 1,FILE%attrs_int%length
  key = FILE%attrs_int%keys(i)
  CALL dict_get(FILE%attrs_int, key, int_val)

  CALL file_gridded_def_attr(FILE%open_file, attr_global, key, int_val)
END DO

! Lastly, character valued attributes
DO i = 1,FILE%attrs_char%length
  key = FILE%attrs_char%keys(i)
  CALL dict_get(FILE%attrs_char, key, char_val)

  CALL file_gridded_def_attr(FILE%open_file, attr_global, key, char_val)
END DO

!-----------------------------------------------------------------------------
! Take the file out of define mode, so that it is ready to read from/write to
!-----------------------------------------------------------------------------
CALL file_gridded_enddef(FILE%open_file)

!-----------------------------------------------------------------------------
! Now everything is defined on the new file, we write the data that we
! gathered for non-time-varying fields before we closed the old file
!-----------------------------------------------------------------------------
IF ( ALLOCATED(non_time_varying_data) ) THEN
  ! Write the data for the non-time varying variables
  j = 1
  DO i = 1,FILE%nvars
    IF ( .NOT. FILE%vars(i)%use_time ) THEN
      ! We are not using a subgrid, so give write_subgrid=F
      CALL file_ts_write_var(FILE, i, non_time_varying_data(j), .FALSE.)
      ! Advance the non-time variable counter
      j = j + 1
    END IF
  END DO

  DEALLOCATE(non_time_varying_data)
END IF

RETURN

END SUBROUTINE file_ts_internal_open_file
#endif
