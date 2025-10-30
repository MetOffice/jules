#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE io_constants

IMPLICIT NONE

! File formats
INTEGER, PARAMETER :: format_len = 3
CHARACTER(LEN=format_len), PARAMETER ::                                        &
  format_ascii = 'asc',                                                        &
    ! Indicates an ASCII file
  format_ncdf = 'nc'
    ! Indicates a NetCDF file


! Modes for opening files
INTEGER, PARAMETER ::                                                          &
  mode_read  = 1,                                                              &
  mode_write = 2


! 'Special' units
INTEGER, PARAMETER ::                                                          &
  unit_stdin       = 5,                                                        &
  unit_stdout      = 6,                                                        &
  namelist_unit    = 1,                                                        &
  points_file_unit = 2,                                                        &
  file_list_unit   = 3,                                                        &
  imogen_unit      = 99


! Constant to specify that an attribute is global (passed in place of var_id)
INTEGER, PARAMETER :: attr_global = -1


REAL, PARAMETER :: mdi = -1.0e20  ! Missing data indicator for output files


! Various maximums for quantities required in IO
INTEGER, PARAMETER ::                                                          &
  max_file_name_len = 500,                                                     &
  max_sdf_name_len = 55,                                                       &
  max_attr_val_len = 200,                                                      &
  max_dim_file = 40,                                                           &
  max_var_file = 150,                                                          &
  max_attr_file = 700,                                                         &
  max_dim_var = 7,                                                             &
  max_attr_var = 10

END MODULE io_constants
#endif
