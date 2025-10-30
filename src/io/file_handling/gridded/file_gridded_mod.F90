#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE file_gridded_mod

USE io_constants, ONLY: max_dim_file, max_var_file, max_dim_var

USE grid_utils_mod, ONLY: grid_info

USE file_mod, ONLY: file_handle

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE


! Type definitions
TYPE :: var_gridded
  !-----------------------------------------------------------------------------
  ! INTERNAL TYPE
  !
  ! This type contains information about the variables in a gridded file
  !-----------------------------------------------------------------------------
  INTEGER :: id  ! The id of the variable in the underlying file

  INTEGER, POINTER :: lev_sizes(:) => NULL() ! The sizes of the levels dimensions
END TYPE var_gridded


TYPE :: file_gridded

  TYPE(file_handle) :: fh  ! Handle to the underlying file

  !-----------------------------------------------------------------------------
  ! Information about the grid to use
  !-----------------------------------------------------------------------------
  TYPE(grid_info) :: grid  ! The grid that we are using

  INTEGER :: grid_dim_id = -1  ! Dimension id of the single grid dimension
                               ! if using a 1d grid
  INTEGER :: grid_x_dim_id = -1  ! Dimension ids of the x and y dimensions
  INTEGER :: grid_y_dim_id = -1  ! if using a 2d grid

  !-----------------------------------------------------------------------------
  ! Information about the 'levels' dimensions that have been defined
  ! (i.e. dimensions that define the number of vertical levels a variable has)
  ! Each variable can have at most one 'levels' dimension
  !-----------------------------------------------------------------------------
  INTEGER :: ndims = 0  ! The number of 'levels' dimensions defined
  INTEGER :: dim_ids(max_dim_file)  ! The ids of the 'levels' dimensions
                                    ! in the file
  INTEGER :: dim_sizes(max_dim_file)  ! The sizes of the 'levels' dimensions

  !-----------------------------------------------------------------------------
  ! Information about the number of levels that variable in the file have
  !-----------------------------------------------------------------------------
  INTEGER :: nvars = 0  ! The number of variables in the file
  TYPE(var_gridded) :: vars(max_var_file)  ! The variables in the file

END TYPE file_gridded


! Overloads for file_gridded_def_attr
INTERFACE file_gridded_def_attr
  MODULE PROCEDURE file_gridded_def_attr_real, file_gridded_def_attr_int,      &
                   file_gridded_def_attr_char
END INTERFACE file_gridded_def_attr


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                         &
! Types
         file_gridded,                                                         &
! Routines for opening and closing
         file_gridded_open, file_gridded_close,                                &
! Routines for definitions
         file_gridded_def_grid, file_gridded_def_dim,                          &
         file_gridded_def_record_dim, file_gridded_def_var,                    &
         file_gridded_def_attr, file_gridded_enddef,                           &
! Routines for seeking
         file_gridded_seek, file_gridded_advance,                              &
! Routines for reading and writing
         file_gridded_read_var, file_gridded_write_var


CONTAINS

! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "file_gridded_open.inc"
#include "file_gridded_def_grid.inc"
#include "file_gridded_def_dim.inc"
#include "file_gridded_def_record_dim.inc"
#include "file_gridded_def_var.inc"
#include "file_gridded_def_attr.inc"
#include "file_gridded_enddef.inc"
#include "file_gridded_seek.inc"
#include "file_gridded_advance.inc"
#include "file_gridded_read_var.inc"
#include "file_gridded_write_var.inc"
#include "file_gridded_close.inc"

END MODULE file_gridded_mod
#endif
