#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE dump_mod

USE io_constants, ONLY: format_len, format_ascii, format_ncdf

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------

INTEGER, PARAMETER :: max_dim_dump = 14
INTEGER, PARAMETER :: max_var_dump = 85

#if defined(NCDF_DUMMY)
CHARACTER(LEN=format_len), PARAMETER :: dump_format = format_ascii
#else
CHARACTER(LEN=format_len), PARAMETER :: dump_format = format_ncdf
#endif

  !Create a defined type to store the flags for which ancils are read
  !from the dump file
TYPE :: ancil_flags
  LOGICAL :: latlon
  LOGICAL :: frac
  LOGICAL :: vegetation_props
  LOGICAL :: soil_props
  LOGICAL :: top
  LOGICAL :: agric
  LOGICAL :: crop_props
  LOGICAL :: irrig
  LOGICAL :: biocrop
  LOGICAL :: rivers_props
  LOGICAL :: water_resources_props
  LOGICAL :: co2
  LOGICAL :: flake
END TYPE ancil_flags

TYPE(ancil_flags) :: ancil_dump_read

!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC  max_dim_dump, max_var_dump, ancil_dump_read, dump_format

END MODULE dump_mod
#endif
