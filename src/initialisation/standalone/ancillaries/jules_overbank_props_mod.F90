#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE jules_overbank_props_mod

USE missing_data_mod, ONLY: rmdi

IMPLICIT NONE

PRIVATE  !  private scope by default
PUBLIC allocate_overbank_vars_grid, allocate_overbank_vars_rp,                 &
       deallocate_overbank_props

CONTAINS

!#############################################################################

SUBROUTINE allocate_overbank_vars_grid( nx_rivers, ny_rivers )

!------------------------------------------------------------------------------
! Description:
!   Allocates overbank inundation variables on the river input grid.
!------------------------------------------------------------------------------

USE logging_mod, ONLY: log_fatal

USE overbank_inundation_mod, ONLY:                                             &
  hypsometric_quantiles_grid, logn_mean, logn_stdev, nquantile_hypso,          &
  overbank_hypsometric, overbank_model, overbank_quantiles

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nx_rivers, ny_rivers
    ! Size of the river input grid.

CHARACTER(LEN=*), PARAMETER :: RoutineName='allocate_overbank_vars_grid'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR, error_sum,                                                            &
    ! Error values.
  nx, ny
    ! Sizes used in allocation.

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! This routine is only called by the master task. All variables are allocated,
! but they are only allocated at full size when required by the run
! configuration.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Variables for overbank_model == overbank_quantiles.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_quantiles ) THEN
  nx = nx_rivers
  ny = ny_rivers
ELSE
  nx = 1
  ny = 1
END IF

ALLOCATE( hypsometric_quantiles_grid(nx,ny,nquantile_hypso), STAT = ERROR )
error_sum = ERROR

!------------------------------------------------------------------------------
! Variables for overbank_model == overbank_hypsometric.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_hypsometric ) THEN
  nx = nx_rivers
  ny = ny_rivers
ELSE
  nx = 1
  ny = 1
END IF

ALLOCATE( logn_mean(nx, ny), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( logn_stdev(nx, ny), STAT = ERROR )
error_sum = error_sum + ERROR

IF ( error_sum /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error allocating grid arrays")
END IF

!------------------------------------------------------------------------------
! Initialise to missing data.
!------------------------------------------------------------------------------
hypsometric_quantiles_grid(:,:,:) = rmdi
logn_mean(:,:)  = rmdi
logn_stdev(:,:) = rmdi

RETURN
END SUBROUTINE allocate_overbank_vars_grid

!##############################################################################

SUBROUTINE allocate_overbank_vars_rp( land_pts, np_rivers )

!------------------------------------------------------------------------------
! Description:
!   Allocates overbank inundation variables on river points. Also allocates
!   inundation variables on land points.
!------------------------------------------------------------------------------

USE logging_mod, ONLY: log_fatal

USE overbank_inundation_mod, ONLY:                                             &
  hypsometric_quantiles, logn_mean_rp, logn_stdev_rp, nquantile_hypso,         &
  overbank_hypsometric, overbank_model, overbank_quantiles,                    &
  overbank_simple_rosgen, frac_fplain_lp, frac_fplain_rp, qbf, dbf, wbf

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  np_rivers
    ! Number of river points.

CHARACTER(LEN=*), PARAMETER :: RoutineName='allocate_overbank_vars_rp'

!------------------------------------------------------------------------------
! Local scalar variables.
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  ERROR, error_sum,                                                            &
    ! Error values.
  np_rivers_tmp,                                                               &
    ! Number of river points to allocate for.
  nquantile_tmp
    ! Number of quantiles to allocate for.

!end of header
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Allocate inundation variables defined on river routing points.
! All variables are allocated (on all tasks), but they are only allocated at
! full size on the master task and when required by the run configuration.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Allocate river point variables that are used by all overbank models.
!------------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

ALLOCATE(frac_fplain_rp(np_rivers_tmp), STAT = ERROR)
error_sum = ERROR

!------------------------------------------------------------------------------
! Allocate river point variables that are only used with the option
! overbank_hypsometric.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_hypsometric .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

ALLOCATE(logn_mean_rp(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(logn_stdev_rp(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

!------------------------------------------------------------------------------
! Allocate river point variables that are only used with the option
! overbank_quantiles.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_quantiles .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
  nquantile_tmp = nquantile_hypso
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
  nquantile_tmp = 1
END IF

ALLOCATE(hypsometric_quantiles(np_rivers_tmp,nquantile_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

!------------------------------------------------------------------------------
! Allocate river point variables that are only used with the option
! overbank_simple_rosgen.
!------------------------------------------------------------------------------
IF ( overbank_model == overbank_simple_rosgen .AND. is_master_task() ) THEN
  ! Full size.
  np_rivers_tmp = np_rivers
ELSE
  ! Minimum size.
  np_rivers_tmp = 1
END IF

! Note that these variables are part of the workspace required by the
! parameterisation.
ALLOCATE(qbf(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(dbf(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR
ALLOCATE(wbf(np_rivers_tmp), STAT = ERROR)
error_sum = error_sum + ERROR

!-------------------------------------------------------------------------
! Allocate inundation variables defined on land points.
! This is required at full size on all tasks.
!-------------------------------------------------------------------------
ALLOCATE(frac_fplain_lp(land_pts), STAT = ERROR)
error_sum = error_sum + ERROR

IF ( error_sum /= 0 ) THEN
  CALL log_fatal(RoutineName,                                                  &
                 "Error allocating arrays)")
END IF

! Initialise array values
frac_fplain_rp(:) = rmdi
logn_mean_rp(:)   = rmdi
logn_stdev_rp(:)  = rmdi
hypsometric_quantiles(:,:) = rmdi
qbf(:)            = rmdi
dbf(:)            = rmdi
wbf(:)            = rmdi
frac_fplain_lp(:) = 0.0

RETURN
END SUBROUTINE allocate_overbank_vars_rp

!##############################################################################

SUBROUTINE deallocate_overbank_props()

!------------------------------------------------------------------------------
! Description:
!   Deallocate overbank inundation variables that are not needed any further.
!------------------------------------------------------------------------------

USE overbank_inundation_mod, ONLY:                                             &
  hypsometric_quantiles_grid, logn_mean, logn_stdev

IMPLICIT NONE

!------------------------------------------------------------------------------
! Variables that are only allocated on master task - hence first check if
! allocated.
!------------------------------------------------------------------------------
IF ( ALLOCATED( hypsometric_quantiles_grid ) ) THEN
  DEALLOCATE( hypsometric_quantiles_grid )
END IF

IF ( ALLOCATED( logn_mean ) ) THEN
  DEALLOCATE( logn_mean )
END IF

IF ( ALLOCATED( logn_stdev ) ) THEN
  DEALLOCATE( logn_stdev )
END IF

RETURN
END SUBROUTINE deallocate_overbank_props

!##############################################################################
END MODULE jules_overbank_props_mod
#endif
