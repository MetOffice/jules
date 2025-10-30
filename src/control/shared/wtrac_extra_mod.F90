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

MODULE wtrac_extra_mod

!-----------------------------------------------------------------------------
! Description:
!    Derived type definition for water tracer working arrays used in the
!    surf_couple_extra routine plus related allocation and deallocation
!    routines.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in ??
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

TYPE :: wtrac_ex_type

  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_melt(:,:)
      ! Water tracer snow melt. Passed between snow and hydrology routines.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lake_snow_melt(:,:)
      ! Water tracer snow melt on lake tiles (only set when using FLAKE -
      ! so currently not used with water tracers)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: melt_surft(:,:,:)
      ! Water tracer surface melt per tile.  Passed between snow and hydrology
      ! routines.

END TYPE wtrac_ex_type

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='WTRAC_EXTRA_MOD'

CONTAINS

! Subroutine Interface:
SUBROUTINE wtrac_alloc_extra(land_pts, nsurft, n_wtrac_jls, wtrac_ex)

!
! Description:
!  Allocate arrays used by water tracers in surf_couple_extra
!

USE jules_water_tracers_mod, ONLY: l_wtrac_jls

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts               ! No. of land points
INTEGER, INTENT(IN) :: nsurft                 ! No. of land tiles
INTEGER, INTENT(IN) :: n_wtrac_jls            ! No. of water tracers

TYPE(wtrac_ex_type), INTENT (IN OUT) :: wtrac_ex

INTEGER :: i, n, i_wt                         ! Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_ALLOC_EXTRA'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate water tracer arrays
IF (l_wtrac_jls) THEN
  ALLOCATE(wtrac_ex%snow_melt(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_ex%lake_snow_melt(land_pts,n_wtrac_jls))
  ALLOCATE(wtrac_ex%melt_surft(land_pts,nsurft,n_wtrac_jls))

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(i,n,i_wt)                                                        &
!$OMP SHARED(land_pts,wtrac_ex,n_wtrac_jls,nsurft)
  DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      wtrac_ex%snow_melt(i,i_wt) = 0.0
      wtrac_ex%lake_snow_melt(i,i_wt) = 0.0
      DO n = 1, nsurft
        wtrac_ex%melt_surft(i,n,i_wt) = 0.0
      END DO
    END DO
!$OMP END DO
  END DO
!$OMP END PARALLEL

ELSE
  ALLOCATE(wtrac_ex%snow_melt(1,1))
  ALLOCATE(wtrac_ex%lake_snow_melt(1,1))
  ALLOCATE(wtrac_ex%melt_surft(1,1,1))
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE wtrac_alloc_extra

!-----------------------------------------------------------------------------

! Subroutine Interface:
SUBROUTINE wtrac_dealloc_extra(wtrac_ex)

!
! Description:
!  Deallocate working arrays used by water tracers in surf_couple_extra
!

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(wtrac_ex_type), INTENT (IN OUT) :: wtrac_ex

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='WTRAC_DEALLOC_EXTRA'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Deallocate water tracer arrays
DEALLOCATE(wtrac_ex%melt_surft)
DEALLOCATE(wtrac_ex%lake_snow_melt)
DEALLOCATE(wtrac_ex%snow_melt)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE wtrac_dealloc_extra

END MODULE wtrac_extra_mod
