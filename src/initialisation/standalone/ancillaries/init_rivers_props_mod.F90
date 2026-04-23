#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

!------------------------------------------------------------------------------
! Description:
!   Reads river ancillary namelist and data, and calls code to convert
!   ancillary fields to the required river point information.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!------------------------------------------------------------------------------

MODULE init_rivers_props_mod

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC init_rivers_props

CONTAINS

!##############################################################################

SUBROUTINE init_rivers_props(rivers, rivers_data)

USE ancil_namelist_mod, ONLY: check_namelist_values, initialise_namelist_vars, &
                              set_constant_vars, set_file_vars

USE fill_variables_from_file_mod, ONLY: fill_variables_from_file

USE grid_utils_mod, ONLY: grid_create, grid_info

USE input_mod, ONLY:                                                           &
    od_grid => grid,                                                           &
    dummy_grid => grid,                                                        &
    use_subgrid

USE jules_overbank_props_mod, ONLY:                                            &
    allocate_overbank_vars_grid, deallocate_overbank_props

USE init_rivers_process_data_mod, ONLY: process_rivers_data

USE io_constants, ONLY: max_file_name_len, namelist_unit

USE jules_rivers_mod, ONLY:                                                    &
  i_river_vn, l_rivers, l_minor_reservoirs, l_sea_level,                       &
  rivers_camaflood, rivers_rfm,                                                &
  rivers_trip, y1_land_grid, l_riv_overbank, l_outflow_per_river,              &
  l_init_storage,                                                              &
  ! types
  rivers_type, rivers_data_type

USE jules_rivers_props_mod, ONLY:                                              &
   jules_rivers_props, check_jules_rivers_props, allocate_river_vars_grid,     &
   deallocate_river_props,                                                     &
   nvars_max, nvars, var, read_list, FILE, const_val, use_file, tpl_name,      &
   var_name, l_use_area, nx_rivers, ny_rivers, file_name_coords,               &
   x_dim_name, y_dim_name, is_climatology

USE jules_water_resources_mod, ONLY:                                           &
  l_water_resources

USE logging_mod, ONLY: log_fatal, log_info

USE model_grid_mod, ONLY: global_land_pts, l_coord_latlon

USE model_interface_mod, ONLY: identifier_len

USE overbank_inundation_mod, ONLY:                                             &
  init_rosgen_vars, overbank_model, overbank_simple_rosgen,                    &
  overbank_hypsometric, overbank_quantiles

USE parallel_mod, ONLY: is_master_task, master_task_id

USE mpi, ONLY: mpi_logical
USE jules_vars_mod, ONLY: mpi_local_comm

USE string_utils_mod, ONLY: to_string

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!    Top-level routine that controls reading of river routing grid and river
!    and properties, and allocation and setting of river point values.
!
!    When running the river executable in coupled mode, no regridding between
!    the land and river grids is necessary, so we can skip the calculation of
!    some variables that will not be used.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Subroutine arguments.
!------------------------------------------------------------------------------
! Types
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

!------------------------------------------------------------------------------
! Local parameters.
!------------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'INIT_RIVERS_PROPS'

LOGICAL ::                                                                     &
  use_var(nvars_max)
    ! Flag indicating if a variable is required.

CHARACTER(LEN=identifier_len) :: required_vars(nvars_max)
    ! The variable identifiers of the variables

!------------------------------------------------------------------------------
! Local scalar variables (not in namelist).
!------------------------------------------------------------------------------
INTEGER :: ERROR               ! Error indicators

INTEGER :: nvars_required
  ! The number of routing variables that are required in this configuration.

INTEGER :: nvars_const         ! The number of variables to be set using
                               ! constant values.
INTEGER :: nvars_file          ! The number of variables that will be set
                               ! from the given file (template?)

INTEGER :: nvars_optional      ! The number of optional routing variables
                               ! in this configuration
CHARACTER(LEN=identifier_len) :: optional_vars(nvars_max)
                               ! The variable identifiers of any optional
                               ! variables
LOGICAL ::                                                                     &
  l_file_list,                                                                 &
    ! Flag indicating if we have a list of files either as variable-name
    ! templating is being used or we have supplied a list of files.
  l_use_direction = .FALSE.,                                                   &
    ! Flag indicating type of flow direction ancillary inputs.
    ! TRUE  = a flow direction field is provided
    ! FALSE = fields of nextx and nexty (indices of the location of the next
    !         downstream point) are provided.
  use_sub_local
    ! A local copy of use_subgrid.

!------------------------------------------------------------------------------
! Local array variables (not in namelist).
!------------------------------------------------------------------------------
CHARACTER(LEN=max_file_name_len) :: file_name(nvars_max)
    ! The name of the file to use for each variable.

!------------------------------------------------------------------------------
! Types
!------------------------------------------------------------------------------
TYPE(grid_info) :: local_grid

!end of header
!------------------------------------------------------------------------------

! If rivers are not on, there is nothing to do.
IF ( .NOT. l_rivers ) RETURN

! If this run does not have any land points we cannot do river routing.
IF ( global_land_pts == 0 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                 "River routing not appropriate for runs with no land "     // &
                 "points." )
END IF

! Check for a consistency between the river scheme and the coordinate system.
! This does not involve river ancillaries, but at present there is no obviously
! better location at which to do this.
! TRIP needs rivers_dx and rivers_dy to be defined in terms of latitude and
! longitude for use when calculating distances in SUBROUTINE get_rivers_len_rp.
! At present that is only the case when the grid is defined in terms of
! latitude and longitude.
IF ( i_river_vn == rivers_trip .AND. .NOT. l_coord_latlon ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 'TRIP can only be used with coordinates of latitude and '  // &
                 'longitude.')
END IF

!------------------------------------------------------------------------------
! Most tasks need only be done on the master task, which is where the river
! routing code will run. If the JULES_RIVERS_PROPS namelist were to contain
! values that are needed by other tasks, all tasks would have to read it.
!------------------------------------------------------------------------------
IF ( is_master_task() ) THEN

  !----------------------------------------------------------------------------
  ! Get ready for reading the river routing properties namelist.
  !----------------------------------------------------------------------------
  ! Initialisation of some common variables related to namelists.
  CALL initialise_namelist_vars( nvars, nvars_const, nvars_file,               &
                                 nvars_required, l_file_list,                  &
                                 FILE, const_val, use_file, use_var,           &
                                 file_name, tpl_name, var, var_name,           &
                                 nvars_optional, optional_vars,                &
                                 is_climatology )

  !----------------------------------------------------------------------------
  ! Read river routing properties namelist
  !----------------------------------------------------------------------------
  CALL log_info( RoutineName, "Reading JULES_RIVERS_PROPS namelist..." )

  READ(namelist_unit, NML = jules_rivers_props, IOSTAT = ERROR)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "Error reading namelist JULES_RIVERS_PROPS "            // &
                    "(IOSTAT=" // TRIM(to_string(ERROR)) // ")" )
  END IF

  !----------------------------------------------------------------------------
  ! Check values from the namelist.
  !----------------------------------------------------------------------------
  CALL check_jules_rivers_props()

  !----------------------------------------------------------------------------
  ! Set up the list of required & optional ancillary variables.
  !----------------------------------------------------------------------------
  ! Add variables that depend only on the chosen routing model.
  SELECT CASE ( i_river_vn )

  CASE ( rivers_camaflood )

    nvars_required = 6
    required_vars(1:nvars_required) =                                          &
      [ 'channel_depth_grid  ', 'channel_width_grid  ',                        &
        'river_distance_grid ', 'river_elevation_grid',                        &
        'river_length_grid   ', 'river_manning_grid  ' ]

  CASE ( rivers_trip )

    nvars_required = 1
    required_vars(1:nvars_required) = [ 'sequence ' ]
    nvars_optional = 1
    optional_vars(1:nvars_optional) = [ 'rivers_storage' ]

  END SELECT

  !----------------------------------------------------------------------------
  ! Set up river overbank inundation properties
  !----------------------------------------------------------------------------
  IF ( overbank_model == overbank_hypsometric ) THEN
    nvars_required = nvars_required + 2
    required_vars(nvars_required-1:nvars_required) =                           &
       [ 'logn_mean ', 'logn_stdev' ]
  ELSE IF ( overbank_model == overbank_quantiles ) THEN
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'hypso_quantiles_grid'
  END IF

  ! Add required variables that depend on the coordinates used.
  ! If the river grid is not a lat-lon grid, we need lat and lon variables.
  ! These are needed for output files, and for calculating gridbox areas
  ! under some configurations. The river grid is defined using the same
  ! coordinate system (type) as the main model grid, hence we test using
  ! l_coord_latlon here.
  IF ( .NOT. l_coord_latlon ) THEN
    nvars_required = nvars_required + 2
    required_vars(nvars_required-1:nvars_required) =                           &
      [ 'latitude_2d ', 'longitude_2d' ]
  END IF

  ! Add variables that are required by some configurations.

  ! All models require some sort of flow direction information. At present
  ! the type of input is determined by the river routing model, but in future
  ! this could be a choice.
  SELECT CASE ( i_river_vn )
  CASE ( rivers_camaflood )
    ! CaMa-Flood uses nextx and nexty fields.
    l_use_direction = .FALSE.
    nvars_required  = nvars_required + 2
    required_vars(nvars_required-1:nvars_required) =                           &
      [ 'river_nextx_grid', 'river_nexty_grid' ]
  CASE DEFAULT
    ! All other models use a flow direction field.
    l_use_direction = .TRUE.
    nvars_required  = nvars_required + 1
    required_vars(nvars_required) = 'direction'
  END SELECT

  IF ( i_river_vn == rivers_camaflood .AND. l_sea_level ) THEN
    ! Add mean sea level.
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'mean_sea_level_grid'
  END IF

  IF ( i_river_vn == rivers_rfm .AND. l_use_area ) THEN
    ! Add upstream drainage area.
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'area'
  END IF

  !----------------------------------------------------------------------------
  ! Add variables for minor reservoirs.
  !----------------------------------------------------------------------------
  IF ( l_minor_reservoirs ) THEN
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'minor_res_capacity_grid'
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'minor_res_frac_grid'
  END IF

  ! OASIS-Rivers:
  !   l_outflow_per_river is set before now by check_oasis_rivers and is
  !   true when outflow_per_river is present in send_fields; requires the
  !   rivers_outflow_number identifier.
  IF ( l_outflow_per_river ) THEN
    nvars_required = nvars_required + 1
    required_vars(nvars_required) = 'rivers_outflow_number'
  END IF

  nvars_optional = nvars_optional + 1
  optional_vars(nvars_optional) = 'rivers_outflow_number'

  !----------------------------------------------------------------------------
  ! Check that variables in the namelist have been provided and set up other
  ! variables.
  !----------------------------------------------------------------------------
  CALL check_namelist_values( nvars, nvars_max, nvars_required,                &
                              RoutineName, FILE, const_val,                    &
                              use_file, read_list, required_vars,              &
                              tpl_name, var, nvars_const, nvars_file,          &
                              l_file_list, use_var, file_name, var_name,       &
                              nvars_optional = nvars_optional,                 &
                              optional_vars = optional_vars )

  !----------------------------------------------------------------------------
  ! Set up the regular routing input grid.
  !----------------------------------------------------------------------------
  use_sub_local = use_subgrid
  use_subgrid   = .FALSE.

  ! Temporarily copy saved grid (set for full model grid) to a local variable
  ! before overwriting to define the river routing grid
  local_grid = grid_create( dummy_grid%is_1d, dummy_grid%dim_name,             &
                            dummy_grid%nx, dummy_grid%x_name, dummy_grid%nx,   &
                            dummy_grid%y_name, dummy_grid%ny )

  !----------------------------------------------------------------------------
  ! Read y dimension from file.
  od_grid = grid_create( grid_is_1d = .TRUE.,                                  &
                         dim_name = TRIM(y_dim_name), npoints = ny_rivers )
  ALLOCATE(rivers_data%rivers_ygrid(ny_rivers), STAT = ERROR)
  rivers%rivers_ygrid => rivers_data%rivers_ygrid
  CALL fill_variables_from_file(file_name_coords,                              &
                                [ 'rivers_ygrid' ],[ TRIM(y_dim_name) ],       &
                                is_climatology = [ .FALSE. ])

  ! Read x dimension from file.
  od_grid = grid_create( grid_is_1d = .TRUE.,                                  &
                         dim_name = TRIM(x_dim_name), npoints = nx_rivers)
  ALLOCATE(rivers_data%rivers_xgrid(nx_rivers), STAT = ERROR)
  rivers%rivers_xgrid => rivers_data%rivers_xgrid
  CALL fill_variables_from_file(file_name_coords,                              &
                                [ 'rivers_xgrid' ],[ TRIM(x_dim_name) ],       &
                                is_climatology = [ .FALSE. ])

  !----------------------------------------------------------------------------
  ! Build the routing grid object from the namelist values
  !----------------------------------------------------------------------------
  dummy_grid = grid_create( .FALSE., "", 0, x_dim_name, nx_rivers, y_dim_name, &
                            ny_rivers)

  !----------------------------------------------------------------------------
  ! Allocate gridded river ancillary variables.
  !----------------------------------------------------------------------------
  CALL allocate_river_vars_grid( global_land_pts, nx_rivers, ny_rivers,        &
                                 l_use_direction, rivers, rivers_data )
  CALL allocate_overbank_vars_grid( nx_rivers, ny_rivers )

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

END IF  !  is_master_task

! Broadcast l_init_storage to all tasks to ensure that `rivers_sto_rp` has a
! source on all tasks in init_ic.
CALL mpi_bcast( l_init_storage, 1, mpi_logical, master_task_id,                &
                mpi_local_comm, ERROR )

!------------------------------------------------------------------------------
! Process the ancillary fields to identify river points and populate fields
! on river points.
!------------------------------------------------------------------------------
CALL process_rivers_data( l_use_direction, rivers, rivers_data )

!------------------------------------------------------------------------------
! Dellocate arrays that are not needed after this.
!------------------------------------------------------------------------------
CALL deallocate_river_props()

IF ( l_riv_overbank ) THEN
  CALL deallocate_overbank_props()
END IF

IF ( is_master_task() ) THEN

  !----------------------------------------------------------------------------
  ! Calculate derived variables for inundation.
  !----------------------------------------------------------------------------
  IF ( l_riv_overbank .AND. overbank_model == overbank_simple_rosgen ) THEN
    CALL init_rosgen_vars( rivers )
  END IF

  !----------------------------------------------------------------------------
  ! Reset saved JULES grid from river to full land model
  !----------------------------------------------------------------------------
  dummy_grid = grid_create(local_grid%is_1d,local_grid%dim_name,               &
                           local_grid%nx,local_grid%x_name,local_grid%nx,      &
                           local_grid%y_name,local_grid%ny)
  use_subgrid = use_sub_local

END IF  !  is_master_task

RETURN
END SUBROUTINE init_rivers_props

!##############################################################################

END MODULE init_rivers_props_mod
#endif
