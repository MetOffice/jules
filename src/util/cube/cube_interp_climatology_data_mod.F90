#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE cube_interp_climatology_data_mod

IMPLICIT NONE

!------------------------------------------------------------------------------
! Description:
!   Processes climatology data during initialisation for initial conditions.
!   Takes a 12-month climatology field and returns the appropriate field for
!   the required initialisation time.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!------------------------------------------------------------------------------

! We want to use a single name to read data from the cube in different
! dimensionalities. As other routines are required they can be added here.
INTERFACE cube_interp_climatology_data
  MODULE PROCEDURE cube_interp_climatology_data_2d
END INTERFACE cube_interp_climatology_data

CONTAINS

!##############################################################################

SUBROUTINE cube_interp_climatology_data_2d(cube, values)

USE logging_mod, ONLY: log_fatal

USE data_cube_mod, ONLY: data_cube

USE datetime_mod, ONLY: datetime

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Identifies the months before and after the model start date and extracts
!   the corresponding arrays from the cube. Performs a weighted average over
!   the two months.
!-----------------------------------------------------------------------------
! Argument types
TYPE(data_cube), INTENT(IN) :: cube
                     ! The cube containing the data to be interpolated

REAL, INTENT(OUT) :: values(:,:)  ! The array to put values in

! Work variables
INTEGER :: shape2D(2), shape3D(3) ! Constant size array required by RESHAPE
                                  ! intrinsic

REAL, ALLOCATABLE :: values3D(:,:,:)  ! The array to put values in

TYPE(datetime) :: date1, date2 ! Dates to interpolate between

REAL :: weight ! Interpolation weight

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CUBE_INTERP_CLIMATOLOGY_DATA_2D'

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Check that values is the right shape for the data in the cube
!-----------------------------------------------------------------------------
! First make sure that cube%shape has the correct number of dimensions
IF ( SIZE(cube%SHAPE) /= 3 ) THEN
  CALL log_fatal(RoutineName,                                                  &
     "To interpolate in time to a 2D field, the input cube needs to be 3D")
END IF

! Check that the dimensions of values have the correct size
IF ( .NOT. ALL(SHAPE(values) == cube%SHAPE(1:2)) ) THEN
  CALL log_fatal("RoutineName",                                                &
     "values is not the same shape as the cube, excluding the time dimension")
END IF

! Check that the last dimension of the cube has the correct 12-month size
IF ( cube%SHAPE(3) /= 12 ) THEN
  CALL log_fatal("RoutineName",                                                &
     "The time (last) dimension should be 12 for a 12-month climatology")
END IF

!-----------------------------------------------------------------------------
! Reshape the cube's data to the expected shape
!-----------------------------------------------------------------------------
shape3D(1:3) = cube%SHAPE(1:3)
ALLOCATE(values3D(shape3D(1),shape3D(2),shape3D(3)))
values3D = RESHAPE(cube%values, shape3D)

! Get the dates to interpolate between and the weight for the current date
CALL get_interp_dates(date1, date2, weight)

! Calulate values at the current date from the climatology
values = ( 1.0 - weight ) * values3D(:,:,date1%month) +                        &
                 weight   * values3D(:,:,date2%month)

DEALLOCATE(values3D)

RETURN
END SUBROUTINE cube_interp_climatology_data_2d

!##############################################################################

SUBROUTINE get_interp_dates(date1, date2, weight)

USE model_time_mod, ONLY: current_time
USE datetime_mod, ONLY: datetime, datetime_diff, datetime_clone, OPERATOR( <= )

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Identifies the months before and after the model start date and calculates
!   the weights for the weighted average over the two months.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(OUT) :: weight
TYPE(datetime), INTENT(OUT) :: date1, date2

! Climatology datestamp is assumed to be the middle of the month i.e. the 16th
! This is different to the JULES inputs assumption that the datestamp is
! assumed to the the 1st.
! This could be a namelist input for flexibility
INTEGER, PARAMETER :: clim_datestamp = 16

! The 16th of the month either side of the model start date
date1     = datetime_clone(current_time)
date1%day = clim_datestamp
date2     = datetime_clone(date1)

IF ( date1 <= current_time ) THEN
  date2%month = date1%month+1
ELSE
  date2%month = date1%month-1
END IF
SELECT CASE (date2%month)
CASE ( 13 )
  date2%month = 1
CASE ( 0 )
  date2%month = 12
END SELECT

weight = REAL ( datetime_diff(date1, current_time) ) /                         &
         REAL ( datetime_diff(date1, date2) )

END SUBROUTINE get_interp_dates

END MODULE cube_interp_climatology_data_mod
#endif
