#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE file_ts_mod

USE io_constants, ONLY: max_file_name_len, max_sdf_name_len,                   &
                         max_attr_val_len, max_dim_file, max_var_file,         &
                         max_attr_file

USE datetime_mod, ONLY: datetime,                                              &
! Also import the comparison operators on the datetime type
                           OPERATOR( == ), OPERATOR( /= ), OPERATOR( < ),      &
                           OPERATOR( > ), OPERATOR( <= ), OPERATOR( >= )

USE grid_utils_mod, ONLY: grid_info

USE string_utils_mod, ONLY: to_string

USE file_gridded_mod, ONLY: file_gridded

USE dictionary_mod, ONLY: dict

USE logging_mod, ONLY: log_info, log_warn, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Constants
!-----------------------------------------------------------------------------
CHARACTER(LEN=max_sdf_name_len) :: time_index_var_name = 'time'

!-----------------------------------------------------------------------------
! Type definitions
!-----------------------------------------------------------------------------
! Internal type containing information about a dimension
TYPE :: dim_ts

  INTEGER :: current_id  ! The id of the dimension in the currently open file
  CHARACTER(LEN=max_sdf_name_len) :: NAME ! The name of the dimension
  INTEGER :: length  ! The length of the dimension

END TYPE dim_ts


! Internal type containing information about a variable
TYPE :: var_ts

  INTEGER :: current_id  ! The id of the variable in the currently open file

  CHARACTER(LEN=max_sdf_name_len) :: NAME  ! The name of the variable in the
                                           ! file(s)

  INTEGER, POINTER :: levels_dims(:)  ! The ids of the vertical levels dimensions
                                      ! (as indices in the dims array on the
                                      ! file_ts object)

  LOGICAL :: use_time   ! Indicates whether the variable uses the time dimension

  ! Dictionaries to hold attribute values
  TYPE(dict) :: attrs_real  ! The values for real valued attributes
                            ! Indexed by attribute name
  TYPE(dict) :: attrs_int  ! The values for integer valued attributes
                           ! Indexed by attribute name
  TYPE(dict) :: attrs_char  ! The values for char valued attributes
                            ! Indexed by attribute name

END TYPE var_ts


TYPE :: file_ts
  !-----------------------------------------------------------------------------
  ! This type and associated functions and subroutines are intended to provide
  ! access to timeseries data in a unified way, whether that data is contained
  ! in a single file or multiple files
  !-----------------------------------------------------------------------------
  INTEGER :: mode ! The mode in which to open files
                  ! This can be one of mode_read or mode_write
  LOGICAL :: use_mpiio = .FALSE.  ! T - Pass values for MPI_Comm and MPI_Info
                                  !     down the chain to open files for
                                  !     parallel access
                                  ! F - Open files for serial access
  INTEGER :: comm = -1  ! MPI communicator to use
  INTEGER :: info = -1  ! MPI info type to use


  !-----------------------------------------------------------------------------
  ! Properties defining the characteristics of the data
  !-----------------------------------------------------------------------------
  TYPE(datetime) :: data_start ! The date and time of the first data
  TYPE(datetime) :: data_end   ! The date and time of the last data
  INTEGER :: data_period ! The period of the data in the files
                         ! (in seconds or the 'special' periods period_month and
                         !  period_year).
                         ! NOTE: that this means all data in a file must
                         ! have the same period
  LOGICAL :: is_climatology = .FALSE. ! Indicates whether the data represents
                                      ! a climatology.
                                      ! In this case, the same data are
                                      ! reused for every year


  !-----------------------------------------------------------------------------
  ! Properties containing information about the dimensions, variables and
  ! attributes contained in each file - this will need to be redefined for
  ! each file that is opened
  !-----------------------------------------------------------------------------
  LOGICAL :: define_mode = .TRUE.  ! Indicates whether object is in
                                   ! 'define mode', i.e. if we can define
                                   ! dimensions and variables

  TYPE(grid_info) :: grid  ! The grid that variables in the file(s) are on

  LOGICAL :: has_time_dim = .FALSE.  ! Indicates whether the time dimension
                                     ! has been defined
  TYPE(dim_ts) :: time_dim ! The time dimension for the file(s)

  INTEGER :: ndims = 0  ! The number of non-grid, non-time dimensions
                        ! that have been defined
  TYPE(dim_ts) :: dims(max_dim_file) ! The non-grid, non-time dimensions
                                     ! in the file(s)

  INTEGER :: nvars = 0  ! The number of variables that have been defined
  TYPE(var_ts) :: vars(max_var_file) ! The variables in the file(s)

  ! Dictionaries to hold global attribute values
  TYPE(dict) :: attrs_real  ! The values for real valued attributes
                                ! Indexed by attribute name
  TYPE(dict) :: attrs_int  ! The values for integer valued attributes
                               ! Indexed by attribute name
  TYPE(dict) :: attrs_char  ! The values for char valued attributes
                                ! Indexed by attribute name

  INTEGER :: time_index_var_id  ! The variable id for the time index
                                ! in the currently open file
  INTEGER :: time_bounds_var_id ! The variable id for the time bounds
                                ! in the currently open file
                                ! Note that since the time index and time
                                ! bounds do not have a grid, we have to
                                ! use the file_handle underlying
                                ! the file_gridded object to manipulate
                                ! it, hence these are variable ids in the
                                ! file_handle object, i.e. file_ts%open_file%fh


  !-----------------------------------------------------------------------------
  ! Properties used to determine what file to open
  !-----------------------------------------------------------------------------
  LOGICAL :: use_time_template

  ! With time templating
  CHARACTER(LEN=max_file_name_len) :: template
  INTEGER :: tpl_period

  ! With a file list
  INTEGER :: nfiles ! The number of files
  CHARACTER(LEN=max_file_name_len), POINTER :: files(:) => NULL()
                     ! List of file names
  TYPE(datetime), POINTER :: file_times(:) => NULL()
                     ! Time of first data for each file


  !-----------------------------------------------------------------------------
  ! Properties related to the currently open file
  !-----------------------------------------------------------------------------
  LOGICAL :: has_open_file = .FALSE.
                            ! T - the file in open_file represents an
                            !     actual open file
                            ! F - the file object in open_file is uninitialised

  INTEGER :: open_file_index ! The index in the files/file_times arrays
                             ! of the currently open file

  TYPE(file_gridded) :: open_file ! The currently open file

  TYPE(datetime) :: next_file_start ! The start time of the next file

  TYPE(datetime) :: current_datetime ! The datetime that this file_ts
                                     ! object is currently pointing to
                                     ! NOTE: it is possible for this datetime
                                     ! to contain a different value to
                                     ! the current model datetime - it is
                                     ! maintained to ensure the correct file
                                     ! is kept open

  LOGICAL :: timestamp_written = .FALSE.  ! Used in write mode only
                                          ! Indicates if the timestamp has
                                          ! been written for the current
                                          ! timestep
                                          ! It is written on first write

END TYPE file_ts


! Overloads for file_ts_def_attr
INTERFACE file_ts_def_attr
  MODULE PROCEDURE file_ts_def_attr_real, file_ts_def_attr_int,                &
                   file_ts_def_attr_char
END INTERFACE file_ts_def_attr


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                         &
! Types
         file_ts,                                                              &
! Routines for opening and closing
         file_ts_open, file_ts_close,                                          &
! Routines for definitions
         file_ts_def_grid, file_ts_def_dim,                                    &
         file_ts_def_time_dim, file_ts_def_var,                                &
         file_ts_def_attr, file_ts_enddef,                                     &
! Routines for seeking
         file_ts_seek_to_datetime, file_ts_advance,                            &
! Routines for reading and writing
         file_ts_read_var, file_ts_write_var


CONTAINS


! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "file_ts_open.inc"
#include "file_ts_def_grid.inc"
#include "file_ts_def_dim.inc"
#include "file_ts_def_time_dim.inc"
#include "file_ts_def_var.inc"
#include "file_ts_def_attr.inc"
#include "file_ts_enddef.inc"
#include "file_ts_seek_to_datetime.inc"
#include "file_ts_advance.inc"
#include "file_ts_read_var.inc"
#include "file_ts_write_var.inc"
#include "file_ts_close.inc"

! Utility routines
#include "file_ts_internal_open_file.inc"

END MODULE file_ts_mod
#endif
