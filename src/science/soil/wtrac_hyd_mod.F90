! *****************************COPYRIGHT*******************************
! (c) British Antarctic Survery, 2023.
! All rights reserved.
!
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
!
! *****************************COPYRIGHT*******************************

MODULE wtrac_hyd_mod

!-----------------------------------------------------------------------------
! Description:
!    Derived type definition for water tracer working arrays used in the
!    hydrology scheme plus related allocation and deallocation routines.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

TYPE :: wtrac_hy_type

  REAL(KIND=real_jlslsm), ALLOCATABLE :: w_flux_soilt(:,:,:,:)
    ! Fluxes of water tracer between layers (kg/m2/s)
    ! (Note, the water version of this field is output from hydrol and passed
    ! into veg_control for use in standalone options)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dsmc_dt_soilt(:,:,:)
    ! Rate of change of water tracer soil moisture due to water falling onto
    ! the surface after surface runoff (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: qbase_soilt(:,:,:)
    ! Water tracer base flow (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: qbase_l_soilt(:,:,:,:)
    ! Water tracer base flow from each level (kg/m2/s).
    ! (Note, the water version of this field is output from hydrol and passed
    !  into veg_control for use in standalone options)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_roff_soilt(:,:,:)
    ! Soil-tiled contributions to water tracer surface runoff (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_roff_inc_soilt(:,:,:)
    ! Increment to tiled surface water tracer runoff (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sub_surf_roff_soilt(:,:,:)
    ! Soil-tiled contributions to subsurface water tracer runoff (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: canopy_gb(:,:)
    ! Gridbox canopy water tracer content (kg/m2).


END TYPE wtrac_hy_type

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WTRAC_HYD_MOD'

CONTAINS

! Subroutine Interface:
SUBROUTINE wtrac_alloc_hyd(land_pts, nsoilt, sm_levels, n_wtrac_jls, wtrac_hy)

!
! Description:
!  Allocate arrays used by water tracers in hydrology scheme and initialise
!  fields
!

USE jules_water_tracers_mod, ONLY: l_wtrac_jls

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts                   ! No. of land points
INTEGER, INTENT(IN) :: nsoilt                     ! No. of soil tiles
INTEGER, INTENT(IN) :: sm_levels                  ! No. of soil levels
INTEGER, INTENT(IN) :: n_wtrac_jls                ! No. of water tracers

TYPE(wtrac_hy_type), INTENT (IN OUT) :: wtrac_hy  ! Water tracer working arrays

INTEGER :: i, m ,n, i_wt                          ! Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_ALLOC_HYD'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate water tracer arrays
IF (l_wtrac_jls) THEN
  ALLOCATE(wtrac_hy%w_flux_soilt(land_pts,nsoilt,0:sm_levels,n_wtrac_jls))
  ALLOCATE(wtrac_hy%dsmc_dt_soilt(land_pts,nsoilt,n_wtrac_jls))
  ALLOCATE(wtrac_hy%qbase_soilt(land_pts,nsoilt,n_wtrac_jls))
  ALLOCATE(wtrac_hy%qbase_l_soilt(land_pts,nsoilt,sm_levels+1,n_wtrac_jls))
  ALLOCATE(wtrac_hy%surf_roff_soilt(land_pts,nsoilt,n_wtrac_jls))
  ALLOCATE(wtrac_hy%surf_roff_inc_soilt(land_pts,nsoilt,n_wtrac_jls))
  ALLOCATE(wtrac_hy%sub_surf_roff_soilt(land_pts,nsoilt,n_wtrac_jls))
  ALLOCATE(wtrac_hy%canopy_gb(land_pts,n_wtrac_jls))

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(i,m,n,i_wt)                                                      &
!$OMP SHARED(land_pts,wtrac_hy,n_wtrac_jls,nsoilt,sm_levels)
  DO i_wt = 1, n_wtrac_jls

    DO m = 1, nsoilt
      DO n = 0, sm_levels     ! Note, starting at 0
!$OMP DO SCHEDULE(STATIC)
        DO i = 1, land_pts
          wtrac_hy%w_flux_soilt(i,m,n,i_wt) = 0.0
        END DO
!$OMP END DO NOWAIT
      END DO

      DO n = 1, sm_levels+1
!$OMP DO SCHEDULE(STATIC)
        DO i = 1, land_pts
          wtrac_hy%qbase_l_soilt(i,m,n,i_wt) = 0.0
        END DO
!$OMP END DO NOWAIT
      END DO

!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        wtrac_hy%dsmc_dt_soilt(i,m,i_wt)       = 0.0
        wtrac_hy%qbase_soilt(i,m,i_wt)         = 0.0
        wtrac_hy%surf_roff_soilt(i,m,i_wt)     = 0.0
        wtrac_hy%surf_roff_inc_soilt(i,m,i_wt) = 0.0
        wtrac_hy%sub_surf_roff_soilt(i,m,i_wt) = 0.0
      END DO
!$OMP END DO NOWAIT
    END DO  ! m

!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      wtrac_hy%canopy_gb(i, i_wt) = 0.0
    END DO
!$OMP END DO NOWAIT

  END DO  ! i_wt
!$OMP END PARALLEL

ELSE
  ALLOCATE(wtrac_hy%w_flux_soilt(1,1,1,1))
  ALLOCATE(wtrac_hy%dsmc_dt_soilt(1,1,1))
  ALLOCATE(wtrac_hy%qbase_soilt(1,1,1))
  ALLOCATE(wtrac_hy%qbase_l_soilt(1,1,1,1))
  ALLOCATE(wtrac_hy%surf_roff_soilt(1,1,1))
  ALLOCATE(wtrac_hy%surf_roff_inc_soilt(1,1,1))
  ALLOCATE(wtrac_hy%sub_surf_roff_soilt(1,1,1))
  ALLOCATE(wtrac_hy%canopy_gb(1,1))
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE wtrac_alloc_hyd

!-----------------------------------------------------------------------------

! Subroutine Interface:
SUBROUTINE wtrac_dealloc_hyd(wtrac_hy)

!
! Description:
!  Deallocate arrays used by water tracers in hydrology scheme
!

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(wtrac_hy_type), INTENT (IN OUT) :: wtrac_hy  ! Water tracer working arrays

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_DEALLOC_HYD'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Deallocate water tracer arrays
DEALLOCATE(wtrac_hy%canopy_gb)
DEALLOCATE(wtrac_hy%sub_surf_roff_soilt)
DEALLOCATE(wtrac_hy%surf_roff_inc_soilt)
DEALLOCATE(wtrac_hy%surf_roff_soilt)
DEALLOCATE(wtrac_hy%qbase_l_soilt)
DEALLOCATE(wtrac_hy%qbase_soilt)
DEALLOCATE(wtrac_hy%dsmc_dt_soilt)
DEALLOCATE(wtrac_hy%w_flux_soilt)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE wtrac_dealloc_hyd

END MODULE wtrac_hyd_mod
