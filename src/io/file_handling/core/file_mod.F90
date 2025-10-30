#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE file_mod

USE driver_ascii_mod, ONLY: file_ascii
USE driver_ncdf_mod, ONLY: file_ncdf

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

! Constants for available drivers
INTEGER, PARAMETER :: driver_ascii = 1
INTEGER, PARAMETER :: driver_ncdf  = 2


! Type definitions
TYPE :: file_handle
  !-----------------------------------------------------------------------------
  ! This type and associated functions and subroutines are intended to provide
  ! a standard interface for different types of file
  !-----------------------------------------------------------------------------
  INTEGER :: driver ! The driver to use to process this file_handle object
                    ! This should be one of the drivers with a constant
                    ! defined above

  ! Only one of these representations is used for any given file_handle object
  TYPE(file_ascii) :: ascii ! The ASCII representation of the file
  TYPE(file_ncdf) :: ncdf   ! The NetCDF representation of the file

END TYPE file_handle


! Interface definition for overloads of def_attr for different types of
! variables
INTERFACE file_def_attr
  MODULE PROCEDURE file_def_attr_real, file_def_attr_int, file_def_attr_char
END INTERFACE file_def_attr

! Interface definitions for the overloads of read/write var for variables
! of different ranks
INTERFACE file_read_var
  MODULE PROCEDURE file_read_var_scalar, file_read_var_1d,                     &
                   file_read_var_2d,     file_read_var_3d,                     &
                   file_read_var_4d,     file_read_var_5d,                     &
                   file_read_var_6d,     file_read_var_7d
END INTERFACE file_read_var

INTERFACE file_write_var
  MODULE PROCEDURE file_write_var_scalar, file_write_var_1d,                   &
                   file_write_var_2d,     file_write_var_3d,                   &
                   file_write_var_4d,     file_write_var_5d,                   &
                   file_write_var_6d,     file_write_var_7d
END INTERFACE file_write_var


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC                                                                         &
! Types
         file_handle,                                                          &
! Routines for opening and closing
         file_open, file_close,                                                &
! Routines for definitions
         file_def_dim, file_def_record_dim, file_def_var, file_def_attr,       &
         file_enddef,                                                          &
! Routines for seeking
         file_seek, file_advance,                                              &
! Routines for reading and writing
         file_read_var, file_write_var,                                        &
! Routines for inquiry
         file_introspect, file_inquire_dim, file_inquire_var


CONTAINS

! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "file_open.inc"
#include "file_def_dim.inc"
#include "file_def_record_dim.inc"
#include "file_def_var.inc"
#include "file_def_attr.inc"
#include "file_enddef.inc"
#include "file_seek.inc"
#include "file_advance.inc"
#include "file_read_var.inc"
#include "file_write_var.inc"
#include "file_close.inc"

! Inquiry routines
#include "file_inquire_dim.inc"
#include "file_inquire_var.inc"
#include "file_introspect.inc"

END MODULE file_mod
#endif
