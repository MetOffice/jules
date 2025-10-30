! *****************************COPYRIGHT*******************************
! (c) British Antarctic Survey, 2023.
! All rights reserved.
!
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
!
! *****************************COPYRIGHT*******************************

MODULE wtrac_snow_mod

!-----------------------------------------------------------------------------
! Description:
!   Derived type definition for water tracer working arrays used in the
!   snow scheme plus related allocation/initialisation and deallocation
!   routines.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Snow
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

TYPE :: wtrac_sn_type

  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_can(:,:,:)
        ! Water tracer canopy snow load (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snowmass(:,:)
        ! Water tracer snow mass on the ground for current tile (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice_sl(:,:,:)
        ! Water tracer ice content of snow layers for current tile (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sliq_sl(:,:,:)
        ! Water tracer liquid content of snow layers for current tile (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snowfall(:,:)
        ! Water tracer total frozen precip reaching the ground in timestep
        ! (kg/m2) - includes any canopy unloading.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: graupfall(:,:)
        ! Water tracer graupel reaching the ground in timestep (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice0(:,:)
        ! Water tracer ice content of fresh snow (kg/m2).
        ! Where nsnow=0, sice0 is the mass of the snowpack.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: infiltration(:,:)
        ! Infiltration of water tracer rainfall into snow pack on the
        ! current tile (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: infil_rate_con_gbm(:,:)
        ! GBM rate of water tracer infiltration of convective rainfall into
        ! snowpack (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: infil_rate_ls_gbm(:,:)
        ! GBM rate of water tracer infiltration of large-scale rainfall into
        ! snowpack (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: infil_ground_con_gbm(:,:)
        ! GBM rate of water tracer infiltration of convective rainfall into
        ! ground (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: infil_ground_ls_gbm(:,:)
        ! GBM rate of water tracer infiltration of large-scale rainfall into
        ! ground (kg/m2/s).

END TYPE wtrac_sn_type

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WTRAC_SNOW_MOD'

CONTAINS

! Subroutine Interface:
SUBROUTINE wtrac_alloc_snow(land_pts, nsurft, nsmax, n_wtrac_jls, wtrac_sn)

!
! Description:
!  Allocate arrays used by water tracers in snow scheme and initialise
!

USE jules_water_tracers_mod, ONLY: l_wtrac_jls

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts                   ! No. of land points
INTEGER, INTENT(IN) :: nsurft                     ! No. of land tiles
INTEGER, INTENT(IN) :: nsmax                      ! Max no. of snow layers
INTEGER, INTENT(IN) :: n_wtrac_jls                ! No. of water tracers

TYPE(wtrac_sn_type), INTENT (IN OUT) :: wtrac_sn  ! Water tracer working
                                                  ! arrays

INTEGER :: i, n, ns, i_wt                      ! Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_ALLOC_SNOW'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate water tracer arrays
IF (l_wtrac_jls) THEN
  ALLOCATE(wtrac_sn%snowmass(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%snow_can(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(wtrac_sn%sice_sl(land_pts,nsmax,n_wtrac_jls))
  ALLOCATE(wtrac_sn%sliq_sl(land_pts,nsmax,n_wtrac_jls))
  ALLOCATE(wtrac_sn%snowfall(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%graupfall(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%sice0(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%infiltration(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%infil_rate_con_gbm(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%infil_rate_ls_gbm(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%infil_ground_con_gbm(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_sn%infil_ground_ls_gbm(land_pts,n_wtrac_jls))


!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(i,n,ns,i_wt)                                                     &
!$OMP SHARED(land_pts,wtrac_sn,n_wtrac_jls,nsurft,nsmax)
  DO i_wt = 1, n_wtrac_jls

    DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        wtrac_sn%snow_can(i,n,i_wt) = 0.0
      END DO
!$OMP END DO
    END DO

    DO ns = 1, nsmax
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        wtrac_sn%sice_sl(i,ns,i_wt) = 0.0
        wtrac_sn%sliq_sl(i,ns,i_wt) = 0.0
      END DO
!$OMP END DO
    END DO

!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      wtrac_sn%snowmass(i,i_wt)             = 0.0
      wtrac_sn%snowfall(i,i_wt)             = 0.0
      wtrac_sn%graupfall(i,i_wt)            = 0.0
      wtrac_sn%sice0(i,i_wt)                = 0.0
      wtrac_sn%infiltration(i,i_wt)         = 0.0
      wtrac_sn%infil_rate_con_gbm(i,i_wt)   = 0.0
      wtrac_sn%infil_rate_ls_gbm(i,i_wt)    = 0.0
      wtrac_sn%infil_ground_con_gbm(i,i_wt) = 0.0
      wtrac_sn%infil_ground_ls_gbm(i,i_wt)  = 0.0
    END DO
!$OMP END DO
  END DO
!$OMP END PARALLEL

ELSE
  ALLOCATE(wtrac_sn%snowmass(1,1))
  ALLOCATE(wtrac_sn%snow_can(1,1,1))
  ALLOCATE(wtrac_sn%sice_sl(1,1,1))
  ALLOCATE(wtrac_sn%sliq_sl(1,1,1))
  ALLOCATE(wtrac_sn%snowfall(1,1))
  ALLOCATE(wtrac_sn%graupfall(1,1))
  ALLOCATE(wtrac_sn%sice0(1,1))
  ALLOCATE(wtrac_sn%infiltration(1,1))
  ALLOCATE(wtrac_sn%infil_rate_con_gbm(1,1))
  ALLOCATE(wtrac_sn%infil_rate_ls_gbm(1,1))
  ALLOCATE(wtrac_sn%infil_ground_con_gbm(1,1))
  ALLOCATE(wtrac_sn%infil_ground_ls_gbm(1,1))
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE wtrac_alloc_snow

!-----------------------------------------------------------------------------

! Subroutine Interface:
SUBROUTINE wtrac_dealloc_snow(wtrac_sn)

!
! Description:
!  Deallocate arrays used by water tracers in snow scheme
!

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(wtrac_sn_type), INTENT (IN OUT) :: wtrac_sn     ! Water tracer working
                                                     ! arrays

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_DEALLOC_SNOW'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Deallocate water tracer arrays
DEALLOCATE(wtrac_sn%infil_ground_ls_gbm)
DEALLOCATE(wtrac_sn%infil_ground_con_gbm)
DEALLOCATE(wtrac_sn%infil_rate_ls_gbm)
DEALLOCATE(wtrac_sn%infil_rate_con_gbm)
DEALLOCATE(wtrac_sn%infiltration)
DEALLOCATE(wtrac_sn%sice0)
DEALLOCATE(wtrac_sn%graupfall)
DEALLOCATE(wtrac_sn%snowfall)
DEALLOCATE(wtrac_sn%sliq_sl)
DEALLOCATE(wtrac_sn%sice_sl)
DEALLOCATE(wtrac_sn%snow_can)
DEALLOCATE(wtrac_sn%snowmass)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE wtrac_dealloc_snow

END MODULE wtrac_snow_mod
