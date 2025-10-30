#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ts_write_var(FILE, var_id, cube, write_subgrid, subgrid)

USE precision_mod, ONLY: int_64

USE grid_utils_mod, ONLY: subgrid_info

USE file_gridded_mod, ONLY: file_gridded_write_var

USE datetime_mod, ONLY: datetime_advance, datetime_diff

USE file_mod, ONLY: file_write_var

USE data_cube_mod, ONLY: data_cube

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes data given as a data cube to the given variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
TYPE(file_ts), INTENT(IN OUT) :: FILE
                               ! The file to write data to
INTEGER, INTENT(IN) :: var_id  ! The id of the variable to write to
TYPE(data_cube), INTENT(IN) :: cube
                               ! The values to write to the file, contained
                               ! in a data cube
                               ! The first 2 dimensions of the cube should
                               ! be the grid dimensions OR the first 1
                               ! dimension should be the vector dimension
                               ! The rest of the dimensions will be taken to
                               ! be levels dimensions
LOGICAL, INTENT(IN) :: write_subgrid
                               ! T - write to a subgrid of the full
                               !     grid of the file
                               ! F - write the full grid
TYPE(subgrid_info), OPTIONAL :: subgrid  ! The subgrid to write


! Work variables
INTEGER :: actual_var_id  ! The id of the variable in the underlying file

INTEGER(KIND=int_64) :: interval_start, interval_end
                          ! The start and end time of the current interval
                          ! in seconds since the start of the data file


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! If this is the first write since an advance, then write the timestamp
! information
!
! If there are no writes between advancing a file and closing it, nothing
! will be written for the final time
!-----------------------------------------------------------------------------
IF ( .NOT. FILE%timestamp_written ) THEN
  ! Work out the start and end of the current interval
  interval_start = datetime_diff(FILE%data_start, FILE%current_datetime)
  interval_end = datetime_diff(                                                &
    FILE%data_start,                                                           &
    datetime_advance(FILE%current_datetime, FILE%data_period)                  &
  )

  ! Write the index variable to be the end of the current interval
  CALL file_write_var(                                                         &
    FILE%open_file%fh, FILE%time_index_var_id, REAL(interval_end)              &
  )

  ! Write the bounds variable to indicate how long the interval lasts for
  CALL file_write_var(                                                         &
    FILE%open_file%fh, FILE%time_bounds_var_id,                                &
    REAL([ interval_start, interval_end ])                                     &
  )

  FILE%timestamp_written = .TRUE.
END IF

!-----------------------------------------------------------------------------
! Defer the actual writing to the file_gridded routine
!-----------------------------------------------------------------------------
actual_var_id = FILE%vars(var_id)%current_id
IF ( PRESENT(subgrid) ) THEN
  CALL file_gridded_write_var(                                                 &
    FILE%open_file, actual_var_id, cube, write_subgrid, subgrid                &
  )
ELSE
  CALL file_gridded_write_var(                                                 &
    FILE%open_file, actual_var_id, cube, write_subgrid                         &
  )
END IF

RETURN

END SUBROUTINE file_ts_write_var
#endif
