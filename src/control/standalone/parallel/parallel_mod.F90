#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

!-----------------------------------------------------------------------------
! Description:
!   This module provides utilities used for running in parallel using MPI
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

MODULE parallel_mod

USE logging_mod, ONLY: log_info, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module parameters
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: master_task_id = 0

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
! These are set by calling decompose_domain
INTEGER :: ntasks = 0   ! The number of MPI tasks available
INTEGER :: task_id = -1  ! The ID of this task

INTEGER :: mpi_type_global_col  ! MPI type for columns in the full grid
INTEGER :: mpi_type_local_col   ! MPI type for columns in the task local grid

! Used in scatter/gather calls
INTEGER, ALLOCATABLE :: counts(:)  ! The number of blocks sent to each
                                   ! task. This is 1 for every task
INTEGER, ALLOCATABLE :: offsets(:)  ! The offset, in numbers of blocks,
                                    ! for each task's block in the global
                                    ! array

!-----------------------------------------------------------------------------
! Visibilities
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC master_task_id, is_master_task, decompose_domain,                       &
                       scatter_land_field, gather_land_field

CONTAINS

!-----------------------------------------------------------------------------
! Description:
!   Returns .TRUE. if the current task is the master task, .FALSE. otherwise
!-----------------------------------------------------------------------------
LOGICAL FUNCTION is_master_task()

USE jules_vars_mod, ONLY: mpi_local_comm

IMPLICIT NONE

! Work variables
INTEGER :: task_id  ! The id of this task

INTEGER :: ERROR  ! Error indicator for MPI calls
                  ! This is ignored as (most) MPI implementations fail
                  ! rather than returning actual error codes


!-----------------------------------------------------------------------------

CALL mpi_comm_rank(mpi_local_comm, task_id, ERROR)

is_master_task = (task_id == master_task_id)

RETURN

END FUNCTION is_master_task

!-----------------------------------------------------------------------------
! Description:
!   Decomposes the given grid across the available MPI tasks
!   Returns a subgrid object representing the part of the grid that the
!   current task will be responsible for
!-----------------------------------------------------------------------------
FUNCTION decompose_domain(grid) RESULT(task_subgrid)

USE mpi, ONLY: mpi_address_kind, mpi_real
USE jules_vars_mod, ONLY: mpi_local_comm

USE grid_utils_mod, ONLY: grid_info, subgrid_info, subgrid_create

USE string_utils_mod, ONLY: to_string

IMPLICIT NONE

! Argument types
TYPE(grid_info), INTENT(IN) :: grid  ! The grid to decompose

! Return type
TYPE(subgrid_info) :: task_subgrid  ! The subgrid this task is responsible for

! Work variables
INTEGER :: ntasks_x, ntasks_y  ! The size of the "task grid" (i.e. the grid
                               ! will be split into ntasks_x by ntasks_y
                               ! blocks

INTEGER :: task_nx, task_ny    ! The size of the block for the current task

INTEGER :: task_x, task_y  ! The x and y coordinates in the "task grid"
                           ! of the current task

INTEGER :: x_start, y_start  ! The x/y coordinates in the grid of the start
                             ! of the task subgrid

LOGICAL :: found_decomposition  ! T - we found a usable decomposition
                                ! F - we did not find a usable decomposition

INTEGER :: leftover  ! The remainder when distributing columns along a task row

INTEGER :: mpi_type  ! Holds intermediate MPI datatype before the extent
                     ! is adjusted
INTEGER(KIND=mpi_address_kind) :: mpi_real_lb, mpi_real_extent
                     ! The lower bound and extent for the mpi_real type

INTEGER(KIND=mpi_address_kind), PARAMETER :: mpi_zero = 0
                     ! A 'zero' of the correct kind to be used as an MPI address

INTEGER, ALLOCATABLE :: counts_2d(:,:), offsets_2d(:,:)
                     ! Used when calculating the counts and offsets on the
                     ! task grid

INTEGER :: ERROR  ! Error indicator for MPI calls
                  ! This is ignored as (most) MPI implementations fail
                  ! rather than returning actual error codes

INTEGER :: i, j, n  ! Index variables


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! This routine currently implements a very naive decomposition
!
! The main concern when decomposing the grid is I/O, not MPI communication
! I.E. the grid must be split into contiguous regions that can be written to
! file in one write statement by specifying appropriate start and count
!-----------------------------------------------------------------------------

! First get the number of available tasks and the id of this task
CALL mpi_comm_size(mpi_local_comm, ntasks, ERROR)
CALL mpi_comm_rank(mpi_local_comm, task_id, ERROR)

CALL log_info("decompose_domain",                                              &
              "Decomposing domain across " // TRIM(to_string(ntasks)) //       &
              " available tasks")

! We can only utilise at most 1 task per point
IF ( ntasks > grid%nx * grid%ny )                                              &
  CALL log_fatal("decompose_domain",                                           &
                 "More tasks are available than points in the model grid")

!-----------------------------------------------------------------------------
! Work out the decomposition, subject to the following rules:
!
!   * Each task must have the same number of grid rows, but can have a
!     varying number of columns
!
!   * Each row of the task grid must have at most grid%nx tasks
!
!   * Each row of the task grid must have the same number of tasks
!
!-----------------------------------------------------------------------------
! This is the minimum number of rows we need in the task grid to ensure that
! each row has <= grid%nx tasks
ntasks_y = (ntasks-1) / grid%nx + 1

! Loop until we find a suitable number of rows for the task grid
! Limited testing found that using as many rows of tasks as possible resulted
! in the most efficient decomposition more of the time (I realise that sounds
! a bit woolly!)
DO n = grid%ny,ntasks_y,-1
  found_decomposition =     ( MOD(grid%ny, n) == 0 ) & ! Each task gets the same number of grid rows
                      .AND. ( MOD(ntasks, n) == 0 )     ! Each row of the task grid has the same number of tasks

  IF ( found_decomposition ) THEN
    ntasks_x = ntasks / n
    ntasks_y = n
    EXIT
  END IF
END DO

! If we could not find a suitable decomposition, suggest changing the number
! of available processes
IF ( .NOT. found_decomposition )                                               &
  CALL log_fatal("decompose_domain",                                           &
                 "Unable to find a suitable decomposition - try " //           &
                 "using a different number of tasks")

CALL log_info("decompose_domain",                                              &
              "Tasks are arranged as a grid of size " //                       &
              TRIM(to_string(ntasks_x)) // " x " // TRIM(to_string(ntasks_y)))

! Each task has the same number of rows, which we can now calculate
task_ny = grid%ny / ntasks_y

!-----------------------------------------------------------------------------
! Build the MPI datatype that allows us to scatter to and gather from
! global arrays in blocks of size 1 x task_ny
!-----------------------------------------------------------------------------
! Get the lower bound and extent for the mpi_real type
CALL mpi_type_get_extent(mpi_real, mpi_real_lb, mpi_real_extent, ERROR)

! Define a MPI type that selects columns from the full grid
CALL mpi_type_vector(task_ny, 1, grid%nx, mpi_real, mpi_type, ERROR)

! Restrict the extent of the datatype to 1 real value for use in offset
! calculations
CALL mpi_type_create_resized(                                                  &
  mpi_type, mpi_zero, mpi_real_extent, mpi_type_global_col, ERROR              &
)

! Commit the datatype
CALL mpi_type_commit(mpi_type_global_col, ERROR)

!-----------------------------------------------------------------------------
! Work out how many grid columns each task in the task grid will have
!-----------------------------------------------------------------------------
ALLOCATE(counts_2d(ntasks_x,ntasks_y))
! Work out how many columns (most of) the tasks will get
counts_2d(:,:) = grid%nx / ntasks_x
! Distribute any leftover columns
leftover = MOD(grid%nx, ntasks_x)
IF ( leftover > 0 ) THEN
  DO n = 1,leftover
    counts_2d(n,:) = counts_2d(n,:) + 1
  END DO
END IF

! The counts used in MPI calls are a flattened version of this
ALLOCATE(counts(ntasks))
counts(:) = RESHAPE(counts_2d, [ ntasks ])

!-----------------------------------------------------------------------------
! Calculate the offsets for each task
!
! Because we adjusted the extent of the column type, our offsets are
! calculated in actual grid cells
!-----------------------------------------------------------------------------
ALLOCATE(offsets_2d(ntasks_x,ntasks_y))
! Note that MPI offsets must start from 0, not 1!
DO j = 1,ntasks_y
  DO i = 1,ntasks_x
    ! First, sum along each row in the task grid to get the offsets within the row
    offsets_2d(i,j) = SUM(counts_2d(1:i-1,j))
  END DO
  ! Then for each row of tasks, add the offset to the start of that row
  offsets_2d(:,j) = offsets_2d(:,j) + (j-1) * grid%nx * task_ny
END DO

! The offsets used in MPI calls are a flattened version of this
ALLOCATE(offsets(ntasks))
offsets(:) = RESHAPE(offsets_2d, [ ntasks ])

!-----------------------------------------------------------------------------
! Construct the subgrid that this task is responsible for
!-----------------------------------------------------------------------------
! Work out where the current task sits in the task grid
! Remember that task ids start from 0, not 1!
task_y = task_id / ntasks_x + 1
task_x = (task_id+1) - (task_y-1) * ntasks_x

! From that, we can retrieve the number of columns the current task is
! reponsible for
! We already know how many rows the task is responsible for
task_nx = counts_2d(task_x,task_y)

! Now we work out the position of the start of our grid in the full grid
! Don't forget that these offsets start from 1, not 0!
! We can get our x_start by summing the counts for the previous columns
x_start = SUM(counts_2d(1:task_x-1,task_y)) + 1
! Since each task has the same number of rows, getting y_start is easier
y_start = (task_y-1) * task_ny + 1

task_subgrid = subgrid_create(grid, x_start, y_start, task_nx, task_ny)

!-----------------------------------------------------------------------------
! Now we know the size of the subgrid, we define a MPI datatype that selects
! columns in the subgrid for the current task
!-----------------------------------------------------------------------------
CALL mpi_type_vector(                                                          &
  task_subgrid%ny, 1, task_subgrid%nx, mpi_real, mpi_type, ERROR               &
)
! Again, we set the extent of the type to one real value for offset
! calculations
CALL mpi_type_create_resized(                                                  &
  mpi_type, mpi_zero, mpi_real_extent, mpi_type_local_col, ERROR               &
)
CALL mpi_type_commit(mpi_type_local_col, ERROR)

DEALLOCATE(counts_2d)
DEALLOCATE(offsets_2d)

RETURN

END FUNCTION decompose_domain

!-----------------------------------------------------------------------------
! Description:
!   Takes a field defined on the land points of the full model grid in the
!   master task and scatters it onto the land points for each task, in
!   accordance with the decomposition performed by decompose_domain
!-----------------------------------------------------------------------------
SUBROUTINE scatter_land_field(field_global_land, field_local_land,             &
                              global_land_index)

USE jules_vars_mod, ONLY: mpi_local_comm

USE missing_data_mod, ONLY: rmdi

USE ancil_info, ONLY: land_pts

USE jules_fields_mod, ONLY: ainfo

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE model_grid_mod, ONLY: model_grid, global_land_pts, global_land_mask

IMPLICIT NONE

! Interface definition is only required if using the dummy MPI library - it
! allows the mpi_scatterv implementation to use assumed-shape arrays (without
! being in a module), which it needs to as it doesn't track MPI type information
! and relies purely on the shape of the array
#if defined(MPI_DUMMY)
INTERFACE
  SUBROUTINE mpi_scatterv(sendbuf, sendcnts, displs, sendtype,                 &
                          recvbuf, recvcnt, recvtype,                          &
                          root, comm, ERROR)

  REAL, INTENT(IN) :: sendbuf(:,:)
  INTEGER, INTENT(IN) :: sendcnts(:)
  INTEGER, INTENT(IN) :: displs(:)
  INTEGER, INTENT(IN) :: sendtype

  REAL, INTENT(OUT) :: recvbuf(:,:)
  INTEGER, INTENT(IN) :: recvcnt
  INTEGER, INTENT(IN) :: recvtype

  INTEGER, INTENT(IN) :: root
  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: ERROR

  END SUBROUTINE mpi_scatterv
END INTERFACE
#endif

! Argument types
REAL, INTENT(IN) :: field_global_land(:)
                                     ! The field on the global land points
                                     ! This only has to be properly defined
                                     ! for the master task
REAL, INTENT(OUT) :: field_local_land(land_pts)
                                     ! The field on the land points for the
                                     ! current task
                                     ! This must be defined for all tasks
INTEGER, INTENT(IN), OPTIONAL :: global_land_index(:)
                                     ! Included to allow consistency between
                                     ! calls from UM & standalone. However,
                                     ! it is not required by standalone.

! Work variables
REAL, ALLOCATABLE :: field_global_2d(:,:)
                                ! The global field on the full 2d model grid
                                ! Only allocated in master task
REAL :: field_local_2d(t_i_length,t_j_length)
                                ! The local field on the model grid for the
                                ! current task
                                ! Used in all tasks

INTEGER :: i,j,l  ! Indexing variables

INTEGER :: ERROR  ! Error indicator for MPI calls
                  ! This is ignored as (most) MPI implementations fail
                  ! rather than returning actual error codes


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! In the master task, convert the full land points array into a 2d array
! on the full model grid
!-----------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  ! Check that the global field is properly defined in the master task
  IF ( SIZE(field_global_land) /= global_land_pts )                            &
    CALL log_fatal("scatter_land_field",                                       &
                   "Input field should be on global land points")

  ! We then need to map back onto the full model grid before scattering
  ALLOCATE(field_global_2d(model_grid%nx,model_grid%ny))
  field_global_2d(:,:) = rmdi
  field_global_2d(:,:) = UNPACK(                                               &
    field_global_land, global_land_mask, field_global_2d                       &
  )
ELSE
  ALLOCATE(field_global_2d(1,1))
END IF

!-----------------------------------------------------------------------------
! Scatter the field into local 2d parts using the block datatype and
! calculated counts and offsets
!-----------------------------------------------------------------------------
CALL mpi_scatterv(field_global_2d, counts, offsets, mpi_type_global_col,       &
                  field_local_2d,  t_i_length,      mpi_type_local_col,        &
                  master_task_id, mpi_local_comm, ERROR)

!-----------------------------------------------------------------------------
! Convert the local 2d parts into land point arrays
!-----------------------------------------------------------------------------
DO l = 1,land_pts
  j = (ainfo%land_index(l) - 1) / t_i_length + 1
  i = ainfo%land_index(l) - (j-1) * t_i_length

  field_local_land(l) = field_local_2d(i,j)
END DO

!-----------------------------------------------------------------------------
! Deallocate the global field at the end
!-----------------------------------------------------------------------------
IF ( ALLOCATED(field_global_2d) ) DEALLOCATE(field_global_2d)

RETURN

END SUBROUTINE scatter_land_field


SUBROUTINE gather_land_field(field_local_land, field_global_land,              &
                             global_land_index)

USE jules_vars_mod, ONLY: mpi_local_comm

USE missing_data_mod, ONLY: rmdi

USE ancil_info, ONLY: land_pts

USE jules_fields_mod, ONLY: ainfo

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE model_grid_mod, ONLY: model_grid, global_land_pts, global_land_mask

IMPLICIT NONE

! Interface definition is only required if using the dummy MPI library - it
! allows the mpi_gatherv implementation to use assumed-shape arrays (without
! being in a module), which it needs to as it doesn't track MPI type information
! and relies purely on the shape of the array
#if defined(MPI_DUMMY)
INTERFACE
  SUBROUTINE mpi_gatherv(sendbuf, sendcnt, sendtype,                           &
                         recvbuf, recvcnts, displs, recvtype,                  &
                         root, comm, ERROR)

  REAL, INTENT(IN) :: sendbuf(:,:)
  INTEGER, INTENT(IN) :: sendcnt
  INTEGER, INTENT(IN) :: sendtype

  REAL, INTENT(OUT) :: recvbuf(:,:)
  INTEGER, INTENT(IN) :: recvcnts(:)
  INTEGER, INTENT(IN) :: displs(:)
  INTEGER, INTENT(IN) :: recvtype

  INTEGER, INTENT(IN) :: root
  INTEGER, INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: ERROR

  END SUBROUTINE mpi_gatherv
END INTERFACE
#endif

!-----------------------------------------------------------------------------
! Description:
!   Takes a field defined on the land points of each task and gathers it
!   onto the land points of the full model grid in the master task, in
!   accordance with the decomposition performed by decompose_domain
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: field_local_land(land_pts)
                                     ! The field on the land points for the
                                     ! current task
                                     ! This must be defined for all tasks
REAL, INTENT(OUT) :: field_global_land(:)
                                     ! The field on the global land points
                                     ! This only has to be properly defined
                                     ! for the master task
INTEGER, INTENT(IN), OPTIONAL :: global_land_index(:)
                                     ! Included to allow consistency between
                                     ! calls from UM & standalone. However,
                                     ! it is not required by standalone.

! Work variables
REAL, ALLOCATABLE :: field_global_2d(:,:)
                                ! The global field on the full 2d model grid
                                ! Only allocated in master task
REAL :: field_local_2d(t_i_length,t_j_length)
                                ! The local field on the model grid for the
                                ! current task
                                ! Used in all tasks

INTEGER :: i,j,l  ! Indexing variables

INTEGER :: ERROR  ! Error indicator for MPI calls
                  ! This is ignored as (most) MPI implementations fail
                  ! rather than returning actual error codes


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Verify that the global land points field is the right size
! We do this at runtime to avoid forcing it to be defined at that size for
! non-master tasks
! We also allocate the global 2d array at this point, for the same reason
!-----------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  IF ( SIZE(field_global_land) /= global_land_pts )                            &
    CALL log_fatal("gather_land_field",                                        &
                   "Output field should be on global land points")

  ALLOCATE(field_global_2d(model_grid%nx,model_grid%ny))
ELSE
  ALLOCATE(field_global_2d(1,1))
END IF

!-----------------------------------------------------------------------------
! Convert the local land point array into a 2d array on the model grid for
! the task
!-----------------------------------------------------------------------------
field_local_2d(:,:) = rmdi
DO l = 1,land_pts
  j = (ainfo%land_index(l) - 1) / t_i_length + 1
  i = ainfo%land_index(l) - (j-1) * t_i_length

  field_local_2d(i,j) = field_local_land(l)
END DO

!-----------------------------------------------------------------------------
! Gather the values from each task into the global field in the main task
! using the block datatype and calculated counts and offsets
!-----------------------------------------------------------------------------
CALL mpi_gatherv(field_local_2d,  t_i_length,      mpi_type_local_col,         &
                 field_global_2d, counts, offsets, mpi_type_global_col,        &
                 master_task_id, mpi_local_comm, ERROR)

!-----------------------------------------------------------------------------
! In the master task, convert the 2d array on the full model grid into
! an array on the global land points
!-----------------------------------------------------------------------------
IF ( is_master_task() )                                                        &
  field_global_land(:) = PACK(field_global_2d, global_land_mask)

!-----------------------------------------------------------------------------
! Deallocate the global field at the end
!-----------------------------------------------------------------------------
IF ( ALLOCATED(field_global_2d) ) DEALLOCATE(field_global_2d)

RETURN

END SUBROUTINE gather_land_field

END MODULE parallel_mod
#endif
