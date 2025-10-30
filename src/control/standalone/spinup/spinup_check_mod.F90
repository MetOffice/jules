#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE spinup_check_mod

IMPLICIT NONE

CONTAINS

LOGICAL FUNCTION spinup_check(progs,trifctltype)

USE mpi, ONLY: mpi_logical, mpi_land
USE jules_vars_mod, ONLY: mpi_local_comm

USE string_utils_mod, ONLY: to_string

! TYPE Definitions
USE trifctl, ONLY: trifctl_type

USE prognostics, ONLY: progs_type

USE spinup_mod, ONLY: nvars, spinup_vars

USE logging_mod, ONLY: log_info, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Checks if spinup was successful for the whole model
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments

TYPE(progs_type), INTENT(IN) :: progs
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype

! Local parameters
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'spinup_check'

! Work variables
INTEGER :: data_shape(3)
  ! The shape of the data.
REAL, ALLOCATABLE :: DATA(:,:,:)
  ! The data we will be checking against.
REAL, ALLOCATABLE :: tol(:,:,:)
  ! The actual tolerance we will be using for each element of the data
  ! (after % tolerances have been resolved).
LOGICAL, ALLOCATABLE :: passed(:,:,:)
  ! Mask that will be .TRUE. at points that passed for the current variable,
  ! and .FALSE. otherwise.
LOGICAL, ALLOCATABLE :: passed_pts(:)
  ! Passed collapsed down onto one dimension.

LOGICAL :: local_check  ! Indicates if the current task has passed it's
                        ! local spinup

INTEGER :: i,l  ! Loop counter

INTEGER :: ERROR  ! MPI error indicator
                  ! NOTE that this is not checked since the default
                  !      behaviour of most (all?) MPI implementations is
                  !      to abort on error

!-----------------------------------------------------------------------------
!end of header

!-----------------------------------------------------------------------------
! Note that each task does its own spinup check on the points that it owns.
! The results from each task are then collected, and spinup for the model as
! a whole is judged to have passed if all the tasks have passed
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Work out if the current task passes spinup
!-----------------------------------------------------------------------------
! Assume we will pass until a variable fails
local_check = .TRUE.

! Loop over each variable and check if that variable passes spinup
DO i = 1,nvars

  ! First, allocate the data and tolerance arrays for this variable
  data_shape(:) = SHAPE(spinup_vars(i)%DATA)
  ALLOCATE(DATA(data_shape(1), data_shape(2), data_shape(3)))
  ALLOCATE(tol(data_shape(1), data_shape(2), data_shape(3)))
  ALLOCATE(passed(data_shape(1), data_shape(2), data_shape(3)))
  ALLOCATE(passed_pts(SIZE(passed, 1)))

  ! Fill the data array based on the variable
  SELECT CASE ( spinup_vars(i)%identifier )

  CASE ( 'c_soil' )
    ! Sum the C pools in each layer.
    DATA(:,:,:) = SUM( progs%cs_pool_soilt(:,:,:,:), 4 )

  CASE ( 'c_veg' )
    DATA(:,1,1) = trifctltype%cv_gb(:)

  CASE ( 'smcl' )
    DATA(:,:,:) = progs%smcl_soilt(:,:,:)

  CASE ( 't_soil' )
    DATA(:,:,:) = progs%t_soil_soilt(:,:,:)

  CASE DEFAULT
    CALL log_fatal(RoutineName,                                                &
                   "Unrecognised variable for spinup - " //                    &
                   TRIM(spinup_vars(i)%identifier))
  END SELECT

  ! Work out what tolerance we will use for each element
  IF ( spinup_vars(i)%use_percent ) THEN
    tol(:,:,:) = ABS(DATA(:,:,:)) * spinup_vars(i)%tolerance / 100.0
  ELSE
    tol(:,:,:) = spinup_vars(i)%tolerance
  END IF

  ! Indicate a failure of spinup if one has occured
  passed(:,:,:) = ABS(spinup_vars(i)%DATA(:,:,:) - DATA(:,:,:)) <= tol(:,:,:)
  ! Collapse the vertical levels into one value per point only
  passed_pts(:) = [ ( ALL(passed(l,:,:)), l = 1,SIZE(passed_pts) ) ]
  IF ( .NOT. ALL(passed_pts) ) THEN
    local_check = .FALSE.
    CALL log_info("spinup_check",                                              &
                  "Variable '" // TRIM(spinup_vars(i)%identifier) //           &
                  "' has not spunup at " //                                    &
                  TRIM(to_string(COUNT( .NOT. passed_pts))) // " points")
  END IF

  ! Store the data for next time we need to check
  spinup_vars(i)%DATA(:,:,:) = DATA(:,:,:)

  DEALLOCATE(DATA)
  DEALLOCATE(tol)
  DEALLOCATE(passed)
  DEALLOCATE(passed_pts)
END DO

!-----------------------------------------------------------------------------
! Exchange results with the other tasks
!
! Spinup passes only if all tasks have passed, so a reduction using the .AND.
! operation gives us what we want
!-----------------------------------------------------------------------------
CALL mpi_allreduce(                                                            &
  local_check, spinup_check, 1, mpi_logical, mpi_land, mpi_local_comm, ERROR   &
)

RETURN

END FUNCTION spinup_check
END MODULE spinup_check_mod
#endif
