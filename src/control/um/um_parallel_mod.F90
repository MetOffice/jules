#if defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE um_parallel_mod

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   This module provides equivalent utilities to standalone used for running
!   in parallel with MetUM. Most routines are simple wrappers of UM routines
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------
INTEGER :: master_task_id = 0

!-----------------------------------------------------------------------------
! Visibilities
!-----------------------------------------------------------------------------
PRIVATE
PUBLIC master_task_id, is_master_task,                                         &
       um_scatter_field, scatter_land_field, scatter_land2d_field,             &
       um_gather_field, gather_land_field, gather_land2d_field

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='UM_PARALLEL_MOD'

CONTAINS

!-----------------------------------------------------------------------------
LOGICAL FUNCTION is_master_task()

  ! Processor number from MetUM control
USE um_parcore,               ONLY:    mype

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Returns .TRUE. if the current task is the master task, .FALSE. otherwise
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

is_master_task = (mype == master_task_id)

RETURN

END FUNCTION is_master_task

!-----------------------------------------------------------------------------
SUBROUTINE um_scatter_field(field_global, field_local)

USE UM_ParVars,   ONLY: glsize, lasize
USE UM_ParParams, ONLY: halo_type_no_halo
USE Field_Types,  ONLY: fld_type_p
USE scatter_field_mod, ONLY: scatter_field

USE parkind1,     ONLY: jprb, jpim
USE yomhook,      ONLY: lhook, dr_hook

IMPLICIT NONE

! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types

REAL, INTENT(IN)  :: field_global(glsize(1,fld_type_p) * glsize(2,fld_type_p))
                                     ! The field on the global 2D grid
REAL, INTENT(OUT) :: field_local(lasize(1,fld_type_p,halo_type_no_halo),       &
                                 lasize(2,fld_type_p,halo_type_no_halo))
                                       ! The field for the current task
                                       ! This must be defined for all tasks

! Local variables
INTEGER :: i,j   ! Loop counters

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'UM_SCATTER_FIELD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise output to zero
DO j = 1, lasize(2,fld_type_p,halo_type_no_halo)
  DO i = 1, lasize(1,fld_type_p,halo_type_no_halo)
    field_local(i,j) = 0.0
  END DO
END DO

CALL scatter_field(field_local,field_global,                                   &
 lasize(1,fld_type_p,halo_type_no_halo),                                       &
 lasize(2,fld_type_p,halo_type_no_halo),                                       &
 glsize(1,fld_type_p),                                                         &
 glsize(2,fld_type_p),                                                         &
 fld_type_p,halo_type_no_halo,                                                 &
 master_task_id)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE um_scatter_field

!-----------------------------------------------------------------------------
SUBROUTINE scatter_land_field(field_global_land, field_local_land,             &
                              global_land_index)

USE ancil_info, ONLY: land_pts
USE atm_fields_mod, ONLY: ainfo
USE theta_field_sizes, ONLY: t_i_length, t_j_length
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Takes a field defined on the land points of the full model grid in the
!   master task and scatters it onto the land points for each task, in
!   accordance with the decomposition performed by decompose_domain
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: global_land_index(global_land_pts)
                                     ! List of indices for the land grid. For
                                     ! every land point, this gives the
                                     ! location in a 2-D grid.
REAL, INTENT(IN) :: field_global_land(global_land_pts)
                                     ! The field on the global land points
                                     ! This only has to be properly defined
                                     ! for the master task
REAL, INTENT(OUT) :: field_local_land(land_pts)
                                     ! The field on local land_pts for the
                                     ! current task
                                     ! This must be defined for all tasks
! Work variables
REAL :: field_local_2d(t_i_length, t_j_length)
                                ! The local field on the current task

INTEGER :: i,j,l  ! Indexing variables

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'SCATTER_LAND_FIELD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Scatter global land field to a 2D local grid
!-----------------------------------------------------------------------------
CALL scatter_land2d_field(field_global_land, field_local_2d,                   &
                          global_land_index)

!-----------------------------------------------------------------------------
! Convert the local 2d parts into land point arrays
!-----------------------------------------------------------------------------
DO l = 1,land_pts
  j = (ainfo%land_index(l) - 1) / t_i_length + 1
  i = ainfo%land_index(l) - (j-1) * t_i_length

  field_local_land(l) = field_local_2d(i,j)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE scatter_land_field

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE scatter_land2d_field(field_global_land, field_local_2d,             &
                                global_land_index)

USE UM_ParVars,   ONLY: glsize
USE Field_Types,  ONLY: fld_type_p
USE theta_field_sizes, ONLY: t_i_length, t_j_length
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Takes a field defined on the land points of the full model grid in the
!   master task and scatters it onto the land points for each task, in
!   accordance with the decomposition performed by decompose_domain
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: global_land_index(global_land_pts)
                                     ! List of indices for the land grid. For
                                     ! every land point, this gives the
                                     ! location in a 2-D grid.
REAL, INTENT(IN) :: field_global_land(global_land_pts)
                                     ! The field on the global land points
                                     ! This only has to be properly defined
                                     ! for the master task
REAL, INTENT(OUT) :: field_local_2d(t_i_length, t_j_length)
                                     ! The field on the 2D model grid for the
                                     ! current task
                                     ! This must be defined for all tasks
! Work variables
REAL :: field_global(glsize(1,fld_type_p) * glsize(2,fld_type_p))
                                ! The global field on the full 2d model grid
                                ! Only allocated in master task
INTEGER :: i,j,l,gf  ! Indexing variables

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'SCATTER_LAND2D_FIELD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
DO j = 1, t_j_length
  DO i = 1, t_i_length
    field_local_2d(i,j) = 0.0
  END DO
END DO

!-----------------------------------------------------------------------------
! In the master task, convert the full land points array into a 2d array
! on the full model grid
!-----------------------------------------------------------------------------
DO i = 1, glsize(1,fld_type_p) * glsize(2,fld_type_p)
  field_global(i) = 0.0
END DO

IF ( is_master_task() ) THEN
  DO l = 1,global_land_pts
    j = (global_land_index(l) - 1) / glsize(1,fld_type_p) + 1
    i = global_land_index(l) - (j-1) * glsize(1,fld_type_p)
    gf = i + (j-1) * glsize(1,fld_type_p)

    field_global(gf) = field_global_land(l)
  END DO
END IF

!-----------------------------------------------------------------------------
! Scatter the field into local 2d parts using UM function
!-----------------------------------------------------------------------------
CALL um_scatter_field(field_global, field_local_2d)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE scatter_land2d_field

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE um_gather_field(field_local, field_global)

USE UM_ParVars,   ONLY: glsize, lasize
USE UM_ParParams, ONLY: halo_type_no_halo
USE Field_Types,  ONLY: fld_type_p
USE gather_field_mod, ONLY: gather_field
USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
REAL, INTENT(IN) :: field_local(t_i_length, t_j_length)
                                         ! The field for the current task
                                         ! This must be defined for all tasks
REAL, INTENT(OUT) :: field_global(glsize(1,fld_type_p) * glsize(2,fld_type_p))
                                     ! The field on the global 2D grid

! Local variables
INTEGER :: i  ! Loop counter

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'UM_GATHER_FIELD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO i = 1, glsize(1,fld_type_p) * glsize(2,fld_type_p)
  field_global(i) = 0.0
END DO

CALL gather_field(field_local,field_global,                                    &
 lasize(1,fld_type_p,halo_type_no_halo),                                       &
 lasize(2,fld_type_p,halo_type_no_halo),                                       &
 glsize(1,fld_type_p),                                                         &
 glsize(2,fld_type_p),                                                         &
 fld_type_p,halo_type_no_halo,                                                 &
 master_task_id)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE um_gather_field

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE gather_land_field(field_local_land, field_global_land,              &
                             global_land_index)

USE ancil_info, ONLY: land_pts
USE atm_fields_mod, ONLY: ainfo
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts
USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Takes a field defined on the land points of each task and gathers it
!   onto the land points of the full model grid in the master task, in
!   accordance with the decomposition performed by decompose_domain
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: global_land_index(global_land_pts)
                                     ! List of indices for the land grid. For
                                     ! every land point, this gives the
                                     ! location in a 2-D grid.
REAL, INTENT(IN) :: field_local_land(land_pts)
                                     ! The field on the land points for the
                                     ! current task
                                     ! This must be defined for all tasks
REAL, INTENT(OUT) :: field_global_land(:)
                                     ! The field on the global land points
                                     ! This only has to be properly defined
                                     ! for the master task
! Work variables
REAL :: field_local_2d(t_i_length,t_j_length)
                                ! The local field on the model grid for the
                                ! current task
                                ! Used in all tasks
INTEGER :: i,j,l  ! Indexing variables

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'GATHER_LAND_FIELD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Convert the local land point array into a 2d array on the model grid for
! the task
!-----------------------------------------------------------------------------
DO j = 1, t_j_length
  DO i = 1, t_i_length
    field_local_2d(i,j) = 0.0
  END DO
END DO

DO l = 1,land_pts
  j = (ainfo%land_index(l) - 1) / t_i_length + 1
  i = ainfo%land_index(l) - (j-1) * t_i_length

  field_local_2d(i,j) = field_local_land(l)
END DO

!-----------------------------------------------------------------------------
! Gather the values from each task into the global field in the main task
! using the block datatype and calculated counts and offsets
!-----------------------------------------------------------------------------

CALL gather_land2d_field(field_local_2d, field_global_land,                    &
                         global_land_index)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE gather_land_field

!-----------------------------------------------------------------------------
SUBROUTINE gather_land2d_field(field_local_2d, field_global_land,              &
                               global_land_index)

USE UM_ParVars,   ONLY: glsize
USE Field_Types,  ONLY: fld_type_p
USE theta_field_sizes, ONLY: t_i_length, t_j_length
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Takes a field defined on the 2d local grid of each task and gathers it
!   onto the land points of the full model grid in the master task, in
!   accordance with the decomposition performed by decompose_domain
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: global_land_index(global_land_pts)
                                     ! List of indices for the land grid. For
                                     ! every land point, this gives the
                                     ! location in a 2-D grid.
REAL, INTENT(IN) :: field_local_2d(t_i_length,t_j_length)
                                     ! The local field on the model grid
                                     ! for the current task
                                     ! This must be defined for all tasks
REAL, INTENT(OUT) :: field_global_land(:)
                                     ! The field on the global land points
                                     ! This only has to be properly defined
                                     ! for the master task
! Work variables
REAL :: field_global(glsize(1,fld_type_p) * glsize(2,fld_type_p))
                                ! The global field on the full 2d model grid
                                ! Only allocated in master task
INTEGER :: i,j,l,gf  ! Indexing variables

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'GATHER_LAND2D_FIELD'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialise output variable to zero
!-----------------------------------------------------------------------------
field_global_land(:) = 0.0

!-----------------------------------------------------------------------------
! Gather the values from each task into the global field in the main task
! using the block datatype and calculated counts and offsets
!-----------------------------------------------------------------------------
CALL um_gather_field(field_local_2d, field_global)

!-----------------------------------------------------------------------------
! In the master task, convert the 2d array on the full model grid into
! an array on the global land points
!-----------------------------------------------------------------------------
IF ( is_master_task() ) THEN

  DO l = 1,global_land_pts
    j = (global_land_index(l) - 1) / glsize(1,fld_type_p) + 1
    i = global_land_index(l) - (j-1) * glsize(1,fld_type_p)
    gf = i + (j-1) * glsize(1,fld_type_p)

    field_global_land(l) = field_global(gf)
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE gather_land2d_field

END MODULE um_parallel_mod
#endif
