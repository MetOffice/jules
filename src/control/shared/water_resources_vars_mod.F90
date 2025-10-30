!******************************COPYRIGHT**************************************
! (c) UK Centre for Ecology & Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

!-----------------------------------------------------------------------------
! Description:
!   Contains variables and field types for water resource modelling.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

MODULE water_resources_vars_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!------------------------------------------------------------------------------
! Implementation for field variables
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are
! generally not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines
!------------------------------------------------------------------------------

TYPE :: water_resources_data_type

  INTEGER, ALLOCATABLE ::                                                      &
    priority_order(:,:)
      ! Priorities of water demands at each gridpoint, in order of decreasing
      ! priority. Values are the index in multi-sector arrays.
      ! e.g. priority_order(l,1) = 3 indicates that the first priority use is
      !      in slice 3 of multi-sector arrays.

  REAL(KIND=real_jlslsm), ALLOCATABLE ::                                       &
    !--------------------------------------------------------------------------
    ! Ancillary fields.
    !--------------------------------------------------------------------------
    conv_loss_frac(:),                                                         &
      ! Fraction of water that is lost during conveyance from source to user.
    sfc_water_frac(:),                                                         &
      ! Target for the fraction of demand that will be met from surface water
      ! (as opposed to groundwater).
    !--------------------------------------------------------------------------
    ! Demands that can be prescribed.
    ! These do not include any allowance for conveyance loss.
    !--------------------------------------------------------------------------
    demand_rate_domestic(:),                                                   &
      ! Demand for water for domestic use (kg s-1).
    demand_rate_industry(:),                                                   &
      ! Demand for water for industrial use (kg s-1).
    demand_rate_livestock(:),                                                  &
      ! Demand for water for livestock (kg s-1).
    demand_rate_transfers(:),                                                  &
      ! Demand for water for (explicit) transfers (kg s-1).
    !--------------------------------------------------------------------------
    ! Flux for coupling to rivers.
    !--------------------------------------------------------------------------
    net_abstracted_river(:),                                                   &
      ! Net abstraction from river (kg).
    !--------------------------------------------------------------------------
    ! Diagnostics (also used internally).
    ! All abstractions are gross (not net of returns) fluxes unless stated
    ! otherwise.
    !--------------------------------------------------------------------------
    demand_accum(:,:),                                                         &
      ! Demands for water accumulated over the water resource timestep (kg).
      ! These include allowance for any conveyance loss.
      ! Note that in general this should be written to restart files (dumps)
      ! but this is not done yet.
    demand_unmet(:,:),                                                         &
      ! The part of the demand for water that is not satisfied (kg).
    gw_abstracted(:),                                                          &
      ! Water abstracted from renewable groundwater (kg).
    gw_avail(:),                                                               &
      ! Groundwater that is available for abstraction at the start of the
      ! timestep (kg). This does not include "non-renewable" groundwater.
    gw_nr_abstracted(:),                                                       &
      ! Water that has been abstracted from non-renewable groundwater (kg).
    sw_abstracted(:,:),                                                        &
      ! Water abstracted from surface water sources (kg).
    sw_avail_total(:),                                                         &
      ! Surface water that is available for abstraction at start of timestep,
      ! summed over sources (kg).
    water_removed(:)
      ! Water that is removed from the system during use, e.g. incorporated
      ! into manufactured goods (kg).

END TYPE

!##############################################################################

TYPE :: water_resources_type

  INTEGER, POINTER :: priority_order(:,:)
  REAL(KIND=real_jlslsm), POINTER :: conv_loss_frac(:)
  REAL(KIND=real_jlslsm), POINTER :: sfc_water_frac(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_domestic(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_industry(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_livestock(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_rate_transfers(:)
  REAL(KIND=real_jlslsm), POINTER :: net_abstracted_river(:)
  REAL(KIND=real_jlslsm), POINTER :: demand_accum(:,:)
  REAL(KIND=real_jlslsm), POINTER :: demand_unmet(:,:)
  REAL(KIND=real_jlslsm), POINTER :: gw_abstracted(:)
  REAL(KIND=real_jlslsm), POINTER :: gw_avail(:)
  REAL(KIND=real_jlslsm), POINTER :: gw_nr_abstracted(:)
  REAL(KIND=real_jlslsm), POINTER :: sw_abstracted(:,:)
  REAL(KIND=real_jlslsm), POINTER :: sw_avail_total(:)
  REAL(KIND=real_jlslsm), POINTER :: water_removed(:)

END TYPE

!##############################################################################

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WATER_RESOURCES_VARS_MOD'

CONTAINS

!##############################################################################

SUBROUTINE water_resources_alloc( land_pts, n_sw_source, nwater_use,           &
             l_have_groundwater, l_have_surface_water,                         &
             l_water_domestic, l_water_industry, l_water_irrigation,           &
             l_water_livestock, l_water_resources, l_water_transfers,          &
             water_resources_data )

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points (current task).
  n_sw_source,                                                                 &
    ! Number of surface water sources.
  nwater_use
    ! Number of water resource sectors that are considered.

LOGICAL, INTENT(IN) ::                                                         &
  l_have_groundwater,                                                          &
    ! Flag indicating if we have a model of groundwater (renewable or
    ! non-renewable).
  l_have_surface_water,                                                        &
    ! Flag indicating if we have surface water represented (e.g. rivers).
    ! TRUE means n_sw_source > 0.
  l_water_domestic,                                                            &
    ! Switch to consider demand for water for domestic use.
  l_water_industry,                                                            &
    ! Switch to consider demand for water for industrial use.
  l_water_irrigation,                                                          &
    ! Switch to consider demand for water for irrigation.
  l_water_livestock,                                                           &
    ! Switch to consider demand for water for livestock.
  l_water_resources,                                                           &
    ! Switch to select water resource management modelling.
  l_water_transfers
    ! Switch to consider (explicit) water transfers.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(water_resources_data_type), INTENT(IN OUT) :: water_resources_data

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------
INTEGER ::                                                                     &
  land_pts_dim, land_pts_gw, land_pts_sw, n_sw_source_dim, nwater_use_dim
    ! Sizes used when allocating arrays.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_ALLOC'

!End of header
!------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Arrays are always allocated, but with minimal size if the science is not
! selected. Decide on sizes.
!-----------------------------------------------------------------------------
! Set default sizes that are used if water resources (or a particular part of
! the parameterisation) are not selected.
land_pts_dim    = 1
land_pts_gw     = 1
land_pts_sw     = 1
n_sw_source_dim = 1
nwater_use_dim  = 1
IF ( l_water_resources ) THEN
  land_pts_dim    = land_pts
  n_sw_source_dim = n_sw_source
  nwater_use_dim  = nwater_use
  IF ( l_have_groundwater ) THEN
    land_pts_gw  = land_pts
  END IF
  IF ( l_have_surface_water ) THEN
    land_pts_sw  = land_pts
  END IF
END IF

!-----------------------------------------------------------------------------
! Priority order.
!-----------------------------------------------------------------------------
ALLOCATE( water_resources_data%priority_order(land_pts_dim,nwater_use_dim) )

!-----------------------------------------------------------------------------
! Ancillary fields.
! Although these are not required for every configuration, they are commonly
! required and hence for convenience we always allocate them.
!-----------------------------------------------------------------------------
ALLOCATE( water_resources_data%conv_loss_frac(land_pts_dim) )
ALLOCATE( water_resources_data%sfc_water_frac(land_pts_dim) )

!-----------------------------------------------------------------------------
! Individual demands (which can be prescibed).
! We allocate a minimum size if a sector is not being used.
!-----------------------------------------------------------------------------
IF ( l_water_domestic ) THEN
  ALLOCATE( water_resources_data%demand_rate_domestic(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_domestic(1) )
END IF

IF ( l_water_industry ) THEN
  ALLOCATE( water_resources_data%demand_rate_industry(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_industry(1) )
END IF

IF ( l_water_livestock ) THEN
  ALLOCATE( water_resources_data%demand_rate_livestock(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_livestock(1) )
END IF

IF ( l_water_transfers ) THEN
  ALLOCATE( water_resources_data%demand_rate_transfers(land_pts_dim) )
ELSE
  ALLOCATE( water_resources_data%demand_rate_transfers(1) )
END IF

!-----------------------------------------------------------------------------
! Coupling to rivers - which are always modelled if l_have_surface_water=T.
!-----------------------------------------------------------------------------
ALLOCATE( water_resources_data%net_abstracted_river(land_pts_sw) )

!-----------------------------------------------------------------------------
! Other variables.
!-----------------------------------------------------------------------------
ALLOCATE( water_resources_data%demand_accum(land_pts_dim,nwater_use_dim) )
ALLOCATE( water_resources_data%demand_unmet(land_pts_dim,nwater_use_dim) )
ALLOCATE( water_resources_data%water_removed(land_pts_dim) )
! Groundwater variables.
ALLOCATE( water_resources_data%gw_abstracted(land_pts_gw) )
ALLOCATE( water_resources_data%gw_avail(land_pts_gw) )
ALLOCATE( water_resources_data%gw_nr_abstracted(land_pts_gw) )
! Surface water variables.
ALLOCATE( water_resources_data%sw_abstracted(land_pts_sw,n_sw_source_dim) )
ALLOCATE( water_resources_data%sw_avail_total(land_pts_sw) )

!-----------------------------------------------------------------------------
! Initialise arrays.
!-----------------------------------------------------------------------------
water_resources_data%priority_order(:,:)      = 0
water_resources_data%conv_loss_frac(:)        = 0.0
water_resources_data%sfc_water_frac(:)        = 0.0
water_resources_data%demand_rate_domestic(:)  = 0.0
water_resources_data%demand_rate_industry(:)  = 0.0
water_resources_data%demand_rate_livestock(:) = 0.0
water_resources_data%demand_rate_transfers(:) = 0.0
water_resources_data%net_abstracted_river(:)  = 0.0
water_resources_data%demand_accum(:,:)        = 0.0
water_resources_data%demand_unmet(:,:)        = 0.0
water_resources_data%water_removed(:)         = 0.0
water_resources_data%gw_abstracted(:)         = 0.0
water_resources_data%gw_avail(:)              = 0.0
water_resources_data%gw_nr_abstracted(:)      = 0.0
water_resources_data%sw_abstracted(:,:)       = 0.0
water_resources_data%sw_avail_total(:)        = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_alloc

!##############################################################################

SUBROUTINE water_resources_dealloc(water_resources_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(water_resources_data_type), INTENT(IN OUT) :: water_resources_data

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_DEALLOC'

!End of header
!------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE( water_resources_data%priority_order )
DEALLOCATE( water_resources_data%conv_loss_frac )
DEALLOCATE( water_resources_data%sfc_water_frac )
DEALLOCATE( water_resources_data%demand_rate_domestic )
DEALLOCATE( water_resources_data%demand_rate_industry )
DEALLOCATE( water_resources_data%demand_rate_livestock )
DEALLOCATE( water_resources_data%demand_rate_transfers )
DEALLOCATE( water_resources_data%net_abstracted_river )
DEALLOCATE( water_resources_data%demand_accum )
DEALLOCATE( water_resources_data%demand_unmet )
DEALLOCATE( water_resources_data%water_removed )
DEALLOCATE( water_resources_data%gw_abstracted )
DEALLOCATE( water_resources_data%gw_avail )
DEALLOCATE( water_resources_data%gw_nr_abstracted )
DEALLOCATE( water_resources_data%sw_abstracted )
DEALLOCATE( water_resources_data%sw_avail_total )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE water_resources_dealloc

!##############################################################################

SUBROUTINE water_resources_assoc(water_resources,water_resources_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!------------------------------------------------------------------------------
! Arguments with INTENT(IN)
!------------------------------------------------------------------------------
TYPE(water_resources_data_type), TARGET, INTENT(IN) :: water_resources_data
  ! Instance of the data type we are associating to.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(water_resources_type), INTENT(IN OUT) :: water_resources
  ! Instance of the pointer type we are associating.

!------------------------------------------------------------------------------
! Local variables
!------------------------------------------------------------------------------

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_ASSOC'

!End of header
!------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL water_resources_nullify(water_resources)

water_resources%priority_order => water_resources_data%priority_order
water_resources%conv_loss_frac => water_resources_data%conv_loss_frac
water_resources%sfc_water_frac => water_resources_data%sfc_water_frac
water_resources%demand_rate_domestic                                           &
                => water_resources_data%demand_rate_domestic
water_resources%demand_rate_industry                                           &
                => water_resources_data%demand_rate_industry
water_resources%demand_rate_livestock                                          &
                => water_resources_data%demand_rate_livestock
water_resources%demand_rate_transfers                                          &
                => water_resources_data%demand_rate_transfers
water_resources%net_abstracted_river                                           &
                => water_resources_data%net_abstracted_river
water_resources%demand_accum     => water_resources_data%demand_accum
water_resources%demand_unmet     => water_resources_data%demand_unmet
water_resources%water_removed    => water_resources_data%water_removed
water_resources%gw_abstracted    => water_resources_data%gw_abstracted
water_resources%gw_avail         => water_resources_data%gw_avail
water_resources%gw_nr_abstracted => water_resources_data%gw_nr_abstracted
water_resources%sw_abstracted    => water_resources_data%sw_abstracted
water_resources%sw_avail_total   => water_resources_data%sw_avail_total

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_assoc

!##############################################################################

SUBROUTINE water_resources_nullify(water_resources)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

TYPE(water_resources_type), INTENT(IN OUT) :: water_resources
  ! Instance of the pointer type we are associating.

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WATER_RESOURCES_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY( water_resources%priority_order )
NULLIFY( water_resources%conv_loss_frac )
NULLIFY( water_resources%sfc_water_frac )
NULLIFY( water_resources%demand_rate_domestic )
NULLIFY( water_resources%demand_rate_industry )
NULLIFY( water_resources%demand_rate_livestock )
NULLIFY( water_resources%demand_rate_transfers )
NULLIFY( water_resources%net_abstracted_river )
NULLIFY( water_resources%demand_accum )
NULLIFY( water_resources%demand_unmet )
NULLIFY( water_resources%water_removed )
NULLIFY( water_resources%gw_abstracted )
NULLIFY( water_resources%gw_avail )
NULLIFY( water_resources%gw_nr_abstracted )
NULLIFY( water_resources%sw_abstracted )
NULLIFY( water_resources%sw_avail_total )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE water_resources_nullify

END MODULE water_resources_vars_mod
