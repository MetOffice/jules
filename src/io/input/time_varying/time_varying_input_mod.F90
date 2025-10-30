#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE time_varying_input_mod

USE io_constants, ONLY: max_sdf_name_len

USE data_cube_mod, ONLY: data_cube

USE file_ts_mod, ONLY: file_ts

USE datetime_mod, ONLY: datetime,                                              &
! Also import the comparison operators on the datetime type
                           OPERATOR( == ), OPERATOR( /= ), OPERATOR( < ),      &
                           OPERATOR( > ), OPERATOR( <= ), OPERATOR( >= )

USE input_mod, ONLY: grid, use_subgrid, subgrid

USE logging_mod, ONLY: log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: nfiles_max = 50  ! The maximum number of file_ts objects
                                       ! that we can read data from

!-----------------------------------------------------------------------------
! Module types
!-----------------------------------------------------------------------------
TYPE :: input_file

  TYPE(file_ts) :: fh  ! The underlying timeseries file

  INTEGER :: times_lbound, times_ubound  ! The bounds on the times required
                                         ! to fulfill required interpolation
                                         !  -1 - previous data
                                         !   0 - current data
                                         !   1 - next data
                                         !   2 - 2 data steps forward

  TYPE(datetime) :: data_times(-1:2)  ! The times that data apply from
                                      !   -1 - the time that the previous
                                      !        data in file apply from
                                      !    0 - the time that the current
                                      !        data in file apply from
                                      !    1 - the time that the next data
                                      !        in file apply from
                                      !    2 - the time that the data after
                                      !        the next data in file apply
                                      !        from
                                      ! It is guaranteed that data_times(0)
                                      ! and data_times(1) will always be
                                      ! populated, even if not required
                                      ! Other times will only be populated
                                      ! if required

  INTEGER :: tsteps_in_data_period(-1:1)
                                      ! The number of model timesteps in
                                      ! each data period
                                      !   -1 - the number of model timesteps
                                      !        between data_times(-1) and
                                      !        data_times(0)
                                      !    0 - the number of model timesteps
                                      !        between data_times(0) and
                                      !        data_times(1)
                                      !    1 - the number of model timesteps
                                      !        between data_times(1) and
                                      !        data_times(2)

  INTEGER :: current_tstep = 0        ! The number of model timesteps since
                                      ! data_times(0) (i.e. how far into the
                                      ! current interpolation period we are)

  INTEGER :: nfields  ! The number of fields in the file
  TYPE(input_field), POINTER :: fields(:) => NULL()  ! The fields in this file

END TYPE input_file


TYPE :: input_field

  INTEGER :: var_id  ! The id of the model variable that this field
                     ! populates - see model_interface_mod

  INTEGER :: file_id  ! The id of the variable in the underlying file

  CHARACTER(LEN=2) :: interp_flag  ! The type of interpolation to use

  TYPE(data_cube), POINTER :: DATA(:) => NULL()
                      ! The current data cubes for the field
                      ! One cube per time required for interpolation

END TYPE input_field

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
INTEGER :: nfiles = 0  ! The number of files registered as containing time
                       ! varying input
TYPE(input_file), SAVE :: files(nfiles_max)  ! The registered files

CHARACTER(LEN=max_sdf_name_len) :: time_dim_name = "time"
                       ! The dimension name for the time dimension in input
                       ! files


!-----------------------------------------------------------------------------
! Visibility declarations
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC time_dim_name,                                                          &
       register_input_file, advance_all, seek_all_to_current_datetime,         &
       update_model_variables, close_all


CONTAINS


!-----------------------------------------------------------------------------
! Module procedures
!-----------------------------------------------------------------------------
! Fortran INCLUDE statements would be preferred, but (at least) the pgf90
! compiler objects to their use when the included file contains pre-processor
! directives. At present, such directives are used to exclude files from
! the UM build, so are required. This may change in the future.
#include "register_input_file.inc"
#include "advance_all.inc"
#include "seek_all_to_current_datetime.inc"
#include "update_model_variables.inc"
#include "time_varying_close_all.inc"

END MODULE time_varying_input_mod
#endif
