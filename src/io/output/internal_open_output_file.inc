#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


FUNCTION internal_open_output_file(                                            &
  file_start, file_end, output_period, is_time_template, is_1d_file,           &
  file_name, out_grid, fields, l_land_frac                                     &
) RESULT(out_file)

USE mpi, ONLY: mpi_info_null
USE jules_vars_mod, ONLY: mpi_local_comm

USE io_constants, ONLY: mode_write, max_dim_file, attr_global

USE dictionary_mod, ONLY: dict, dict_create, dict_free

USE data_cube_mod, ONLY: cube_get_data, cube_free

USE file_ts_mod, ONLY: file_ts_open, file_ts_def_grid, file_ts_def_time_dim,   &
                        file_ts_def_attr, file_ts_enddef, file_ts_write_var

USE model_interface_mod, ONLY: get_var_id, extract_var

USE model_grid_mod, ONLY: l_coord_latlon

USE grid_utils_mod, ONLY: grid_info

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   Opens a file_ts object for a new output file using the supplied information
!   and returns it
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------
! Argument types
TYPE(datetime), INTENT(IN) :: file_start  ! The start time of the file
TYPE(datetime), INTENT(IN) :: file_end    ! The end time of the file

INTEGER, INTENT(IN) :: output_period  ! The output period of the file

LOGICAL, INTENT(IN) :: is_time_template  ! Indicates if file_name includes
                                         ! time templating strings, or
                                         ! whether it is a single file name
LOGICAL, INTENT(IN) :: is_1d_file        ! Indicates if file is defined on
                                         ! a vector
LOGICAL, INTENT(IN) :: l_land_frac       ! Include land_fraction data
CHARACTER(LEN=max_file_name_len), INTENT(IN) :: file_name
                                         ! The name or time template to use
                                         ! for the file

TYPE(grid_info), INTENT(IN OUT) :: out_grid  ! The the output grid

TYPE(output_field), INTENT(IN OUT) :: fields(:)
                                         ! The fields we are defining in the
                                         ! file
                                         ! These are INOUT because the
                                         ! file_id field will be set to the
                                         ! id of the variable in the file

! Return type
TYPE(file_ts) :: out_file


! Work variables
INTEGER :: dummy  ! Dummy variable to receive dimension id of time dim

TYPE(dict) :: defined_dims  ! Dictionary of dimensions defined in the file
                            ! so far

TYPE(data_cube) :: lat_cube, lon_cube, land_frac_cube, xcoord_cube, ycoord_cube
  ! Cubes for coordinate and land fraction data.

REAL :: point_lat, point_lon, point_xcoord, point_ycoord
  ! Real values for coordinates of single point for ASCII files.

INTEGER :: lat_id, lon_id, land_frac_id, xcoord_id, ycoord_id
  ! IDs of coordinate and land_frac variables in file for NetCDF files

CHARACTER(LEN=15) :: cell_methods_val  ! Value of cell_methods attribute
                                       ! for the current variable

INTEGER :: i  ! Loop variable


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Open a new file using the properties we were given
!-----------------------------------------------------------------------------
IF ( is_1d_file ) THEN
  IF ( is_master_task() ) THEN
    out_file = file_ts_open(mode_write, file_start, file_end,                  &
                            output_period, .FALSE.,                            &
                            ! If is_time_template = T, file_name will be used
                            ! as a time template
                            is_time_template, file_name,                       &
                            ! If is_time_template = F, file_names and
                            ! file_times will be used. We give a single file
                            ! name starting at the start of the file (i.e. one
                            ! file for all output)
                            [ file_name ], [ file_start ])
  END IF
ELSE
  out_file = file_ts_open(mode_write, file_start, file_end,                    &
                          output_period, .FALSE.,                              &
                          ! If is_time_template = T, file_name will be used
                          ! as a time template
                          is_time_template, file_name,                         &
                          ! If is_time_template = F, file_names and
                          ! file_times will be used. We give a single file
                          ! name starting at the start of the file (i.e. one
                          ! file for all output)
                          [ file_name ], [ file_start ],                       &
                          ! Pass MPI variables to file_ts_open so that
                          ! parallel I/O will be used.
                          mpi_local_comm, mpi_info_null)
END IF

!-----------------------------------------------------------------------------
! Define the grid
!
! Before passing the grid_info object, overide the dimension names that will
! be used
!-----------------------------------------------------------------------------
out_grid%dim_name = grid_dim_name
out_grid%x_name   = x_dim_name
out_grid%y_name   = y_dim_name
CALL file_ts_def_grid(out_file, out_grid)


!-----------------------------------------------------------------------------
! Define the time dimension
!-----------------------------------------------------------------------------
dummy = file_ts_def_time_dim(out_file, time_dim_name)


!-----------------------------------------------------------------------------
! Create a dictionary to store the levels dimensions defined so far
!    dim_name => dim_id
!-----------------------------------------------------------------------------
defined_dims = dict_create(max_dim_file, INT(1))


!-----------------------------------------------------------------------------
! Get coordinate data cubes, choosing the correct variable for the required
! grid.
!-----------------------------------------------------------------------------
IF ( is_1d_file ) THEN
  ! ASCII format is only allowed on a 1 x 1 grid, so we know we cannot make
  ! vector output (because currently the only vector output is river routing)
  IF ( output_format == format_ascii ) THEN
    CALL log_fatal("internal_open_output_file",                                &
                   "Cannot produce vector output for ascii files")
  END IF
  lat_cube = extract_var(get_var_id('rivers_lat_rp'))
  lon_cube = extract_var(get_var_id('rivers_lon_rp'))
  IF ( .NOT. l_coord_latlon ) THEN
    ! Get projection coordinate values.
    xcoord_cube = extract_var(get_var_id('rivers_x_coord_rp'))
    ycoord_cube = extract_var(get_var_id('rivers_y_coord_rp'))
  END IF
ELSE
  lat_cube = extract_var(get_var_id('latitude'))
  lon_cube = extract_var(get_var_id('longitude'))
  IF ( l_land_frac ) THEN
    land_frac_cube = extract_var(get_var_id('land_fraction'))
  END IF
  IF ( .NOT. l_coord_latlon ) THEN
    ! Get projection coordinate values.
    xcoord_cube = extract_var(get_var_id('projection_x_coord'))
    ycoord_cube = extract_var(get_var_id('projection_y_coord'))
  END IF
END IF


!-----------------------------------------------------------------------------
! Do the things that need to be done differently for ASCII and NetCDF
!
! In particular, if we are using ASCII files every variable must have a time
! dimension. However, the grid is restricted to 1 x 1 - so we use global
! attributes for latitude and longitude
!-----------------------------------------------------------------------------
SELECT CASE ( output_format )
CASE ( format_ascii )

  ! For ASCII, populate the lat/lon attributes
  CALL cube_get_data(lat_cube, point_lat)
  CALL cube_get_data(lon_cube, point_lon)

  CALL file_ts_def_attr(out_file, attr_global, 'latitude', point_lat)
  CALL file_ts_def_attr(out_file, attr_global, 'longitude', point_lon)

  ! Populate projection coordinates.
  IF ( .NOT. l_coord_latlon ) THEN
    CALL cube_get_data(xcoord_cube, point_xcoord)
    CALL cube_get_data(ycoord_cube, point_ycoord)
    CALL file_ts_def_attr(out_file, attr_global, 'projection_x_coord',         &
                          point_xcoord)
    CALL file_ts_def_attr(out_file, attr_global, 'projection_y_coord',         &
                          point_ycoord)
  END IF

CASE ( format_ncdf )
  ! For NetCDF, create non-time-varying lat/lon variables
  CALL internal_define_var( out_file, defined_dims, get_var_id('latitude'),    &
                            'latitude', .FALSE., lat_id )
  CALL internal_define_var( out_file, defined_dims, get_var_id('longitude'),   &
                            'longitude', .FALSE., lon_id )
  IF ( l_land_frac ) THEN
    CALL internal_define_var( out_file, defined_dims,                          &
                              get_var_id('land_fraction'), 'land_fraction',    &
                             .FALSE., land_frac_id )
  END IF

  ! Populate projection coordinates.
  IF ( .NOT. l_coord_latlon ) THEN
    CALL internal_define_var( out_file, defined_dims,                          &
                              get_var_id('projection_x_coord'),                &
                             'projection_x_coord', .FALSE., xcoord_id )
    CALL internal_define_var( out_file, defined_dims,                          &
                              get_var_id('projection_y_coord'),                &
                             'projection_y_coord', .FALSE., ycoord_id )
  END IF

  ! No default case, so that using a format other than those defined is a
  ! definite error.
END SELECT


!-----------------------------------------------------------------------------
! Set up the output variables in the file
!-----------------------------------------------------------------------------
DO i = 1,SIZE(fields)

  ! Define the variable in the file (inc. dimensions and attributes)
  CALL internal_define_var(                                                    &
    out_file, defined_dims,                                                    &
    fields(i)%var_id, fields(i)%output_name, .TRUE., fields(i)%file_id         &
  )

  ! Add the CF convention "cell_methods" attribute to indicate the field type
  ! of output.
  SELECT CASE ( fields(i)%field_type )
  CASE ( output_snapshot )
    cell_methods_val = "time : point"

  CASE ( output_accum )
    cell_methods_val = "time : sum"

  CASE ( output_mean )
    cell_methods_val = "time : mean"

  CASE ( output_min )
    cell_methods_val = "time : minimum"

  CASE ( output_max )
    cell_methods_val = "time : maximum"

  END SELECT

  CALL file_ts_def_attr(                                                       &
    out_file, fields(i)%file_id, "cell_methods", cell_methods_val              &
  )

END DO

! We have finished defining things on the file handle
! For 1d files this is only done by the master task, for all other files
! it is done by all tasks
IF ( ( is_1d_file .AND.  is_master_task() ) .OR. ( .NOT. is_1d_file ) ) THEN
  CALL file_ts_enddef(out_file)
END IF

! We no longer need the defined_dims dictionary
CALL dict_free(defined_dims)


!-----------------------------------------------------------------------------
! Fill the coordinate variables if we need to
!-----------------------------------------------------------------------------
SELECT CASE ( output_format )
CASE ( format_ascii )
  ! For ASCII files, we do nothing - we use a select statement so we get an
  ! error if someone has added a file type but not considered it's consequences
  ! here

CASE ( format_ncdf )

  IF ( is_1d_file ) THEN
    ! 1d files do not correspond to the model grid, so must be written by
    ! the master task.
    IF ( is_master_task() ) THEN
      ! For all these calls we pass .FALSE. to cover subgrid information (for
      ! writing a slab of the output grid in parallel mode).
      CALL file_ts_write_var( out_file, lat_id, lat_cube,                      &
                              .FALSE. )
      CALL file_ts_write_var( out_file, lon_id, lon_cube,                      &
                              .FALSE. )
      IF ( .NOT. l_coord_latlon ) THEN
        CALL file_ts_write_var( out_file, xcoord_id, xcoord_cube,              &
                                .FALSE. )
        CALL file_ts_write_var( out_file, ycoord_id, ycoord_cube,              &
                                .FALSE. )
      END IF
    END IF  !  is_master_task

  ELSE
    ! 2d files can be written per task
    ! For all these calls we pass subgrid information (use_subgrid and subgrid)
    ! to allow writing a slab of the output grid in parallel mode.
    CALL file_ts_write_var( out_file, lat_id, lat_cube,                        &
                            use_subgrid, subgrid )
    CALL file_ts_write_var( out_file, lon_id, lon_cube,                        &
                            use_subgrid, subgrid )

    IF ( l_land_frac ) THEN
      CALL file_ts_write_var( out_file, land_frac_id, land_frac_cube,          &
                              use_subgrid, subgrid )
    END IF
    IF ( .NOT. l_coord_latlon ) THEN
      CALL file_ts_write_var( out_file, xcoord_id, xcoord_cube,                &
                              use_subgrid, subgrid )
      CALL file_ts_write_var( out_file, ycoord_id, ycoord_cube,                &
                              use_subgrid, subgrid )
    END IF

  END IF ! .NOT. is_1d_file

  ! No default case, so that using a format other than those defined is a
  ! definite error
END SELECT


!-----------------------------------------------------------------------------
! Deallocate the lat/lon data cubes
!-----------------------------------------------------------------------------
CALL cube_free(lat_cube)
CALL cube_free(lon_cube)
IF ( l_land_frac ) THEN
  CALL cube_free(land_frac_cube)
END IF
IF ( .NOT. l_coord_latlon ) THEN
  CALL cube_free(xcoord_cube)
  CALL cube_free(ycoord_cube)
END IF


RETURN

END FUNCTION internal_open_output_file
#endif
