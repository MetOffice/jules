#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE output_mod

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len,                   &
                         format_len, format_ascii, format_ncdf

USE string_utils_mod, ONLY: to_string

USE datetime_mod, ONLY: datetime,                                              &
! Also import the comparison operators on the datetime type
                           OPERATOR( == ), OPERATOR( /= ), OPERATOR( < ),      &
                           OPERATOR( > ), OPERATOR( <= ), OPERATOR( >= )

USE grid_utils_mod, ONLY: grid_info, subgrid_info

USE data_cube_mod, ONLY: data_cube

USE file_ts_mod, ONLY: file_ts

USE logging_mod, ONLY: log_info, log_warn, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: nprofiles_max = 50

! Constants for dump period unit
CHARACTER(LEN=1), PARAMETER ::                                                 &
  ! Calendar year, default
  dump_period_year  = 'Y',                                                     &
  ! Calendar second of day
  dump_period_time  = 'T'

! Constants for types of output available
CHARACTER(LEN=1), PARAMETER ::                                                 &
  output_snapshot = 'S',                                                       &
      ! Indicates output should be a snapshot every output period
  output_accum    = 'A',                                                       &
      ! Indicates output should be an accumulation over the output period
  output_mean     = 'M',                                                       &
      ! Indicates output should be a mean over the output period
  output_max      = 'X',                                                       &
      ! Indicates output should be a max over the output period
  output_min      = 'N'
      ! Indicates output should be a min over the output period

CHARACTER(LEN=1), PARAMETER, DIMENSION(5) :: allowed_output_types = [          &
  output_snapshot, output_accum, output_mean, output_max, output_min           &
]

! Dimension names for the output grid
! USED ONLY FOR 1D GRID
CHARACTER(LEN=max_sdf_name_len), PARAMETER :: grid_dim_name = "points"
    ! The dimension name of the single grid dimension

! USED ONLY FOR 2D GRID
CHARACTER(LEN=max_sdf_name_len), PARAMETER ::                                  &
  x_dim_name = "x",                                                            &
  y_dim_name = "y"

CHARACTER(LEN=max_sdf_name_len) :: time_dim_name = "time"
                       ! The dimension name for the time dimension

#if defined(NCDF_DUMMY)
CHARACTER(LEN=format_len), PARAMETER :: output_format = format_ascii
#else
CHARACTER(LEN=format_len), PARAMETER :: output_format = format_ncdf
#endif
                                         ! The file format to use for output


!-----------------------------------------------------------------------------
! Module types
!-----------------------------------------------------------------------------
TYPE :: output_profile

  CHARACTER(LEN=max_sdf_name_len) :: profile_name  ! The name of the profile

  LOGICAL :: has_open_file = .FALSE.  ! Indicates if the profile is currently
                                      ! open

  LOGICAL :: is_river_profile = .FALSE.  ! Indicates if this profile is for
                                         ! variables on the river grid.

  TYPE(file_ts) :: fh  ! Handle to the underlying file

  LOGICAL :: output_initial ! T - this profile should output initial data
                            !     for each section it is outputting
                            ! F - this profile should not output initial data

  LOGICAL :: output_spinup  ! T - this profile is outputting during spinup
                            ! F - this profile is not outputting during spinup

  LOGICAL :: output_main_run  ! T - this profile is outputting (part of) the
                              !     main run
                              ! F - this profile is not outputting any of
                              !     the main run

  LOGICAL :: l_land_frac = .FALSE. ! Include land_fraction data

  TYPE(datetime) :: output_start, output_end
                            ! The start and end times for output during the
                            ! main run

  INTEGER :: file_period    ! The period to use for files for this profile
  INTEGER :: output_period  ! The output period for this profile
  INTEGER :: sample_period  ! The sampling period for this profile (s).
                            ! Note that "special" periods, such as monthly,
                            ! are not allowed because time accumulations
                            ! need to know how many timesteps went into the
                            ! sample, which is less straightforward for
                            ! special periods (which generally don't have
                            ! constant length; a further counter would
                            ! likely be required to deal with that case).

  TYPE(datetime) :: next_sample_time  ! The time that the data will next be
                                      ! sampled
  TYPE(datetime) :: current_output_time  ! The time that the current output
                                         ! period started
  TYPE(datetime) :: next_output_time  ! The time that the next output period
                                      ! starts

  INTEGER :: samples_in_period  ! The number of times that data has been
                                ! sampled in this output period

  INTEGER :: nfields = 0  ! The number of fields in the profile
  TYPE(output_field), POINTER :: fields(:) => NULL()  ! The fields in the profile

END TYPE output_profile


TYPE :: output_field

  INTEGER :: var_id  ! The variable id of the model variable that
                     ! this field outputs - see model_interface_mod

  CHARACTER(LEN=max_sdf_name_len) :: output_name
                     ! The name of the variable in output files

  CHARACTER(LEN=1) :: field_type  ! The type of output to use

  INTEGER :: file_id  ! The id of the variable in the underlying file

  TYPE(data_cube) :: field_data ! The data collected for output as a cube

END TYPE output_field


!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
CHARACTER(LEN=max_file_name_len) :: output_dir = "./output"
                                         ! The directory to use for output

CHARACTER(LEN=max_sdf_name_len) :: run_id = ""  ! Identifier for the run

TYPE(grid_info), SAVE :: grid  ! The output grid
                               ! gfortran requires the SAVE attribute
                               ! Other compilers don't care either way

LOGICAL :: use_subgrid = .FALSE.
    ! T => the model grid is a subset of the output grid
    ! F => the model grid is the output grid

TYPE(subgrid_info), SAVE :: subgrid  ! Used if use_subgrid=T
                                     ! Information about the subgrid to write
                                     ! gfortran requires the SAVE attribute
                                     ! Other compilers don't care either way

TYPE(grid_info), SAVE :: river_out_grid ! The river output grid
                                        ! Used if any river variables are
                                        ! requested for output
                                        ! gfortran requires the SAVE attribute


INTEGER :: nprofiles = 0  ! The number of output profiles requested
TYPE(output_profile), SAVE :: profiles(nprofiles_max)  ! The requested profiles

INTEGER :: dump_period = 1
CHARACTER(LEN=1) :: dump_period_unit = dump_period_year
!   The dump period, and its unit (calendar year or second of calendar day)
!   In calendar year mode, the number of years between model dumps.
!   Note that the calendar year (date) is used to determine whether a dump
!   is written, not the number of years simulated so far.
!   In second of calendar day mode, the number of seconds between model dumps.
!   Note that this is calculated using the number of seconds into the day,
!   not the number seconds simulated so far.
!   Dumps are also written at the start and end of the main run, and at the
!   start of each cycle of any spin up.

!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                         &
! Constants
    dump_period_year, dump_period_time,                                        &
! Variables
    output_dir, run_id, grid, use_subgrid, subgrid, river_out_grid,            &
    dump_period, dump_period_unit,                                             &
! Routines
    register_output_profile, sample_data, output_data, close_all,              &
    output_initial_data


CONTAINS


! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "output_initial_data.inc"
#include "register_output_profile.inc"
#include "sample_data.inc"
#include "output_data.inc"
#include "close_all.inc"

! Internal procedures
#include "internal_init_profile_vars.inc"
#include "internal_next_output_file.inc"
#include "internal_open_output_file.inc"
#include "internal_define_var.inc"

END MODULE output_mod
#endif
