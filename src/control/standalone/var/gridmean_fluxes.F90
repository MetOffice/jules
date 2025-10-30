#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module containing grid-box mean surface fluxes would be passed
! back to an atmopheric model
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Land
!

MODULE gridmean_fluxes

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module is required only in standalone configuration
!-----------------------------------------------------------------------------
REAL, ALLOCATABLE :: fqw_1_ij(:,:)
!   Moisture flux between layers (kg per square metre per sec)
!   FQW(,1) is total water flux from surface, 'E'
REAL, ALLOCATABLE :: ftl_1_ij(:,:)
!   FTL(,K) contains net turbulent sensible heat flux into layer K from below
!   so FTL(,1) is the surface sensible heat, H.(W/m2)
REAL, ALLOCATABLE :: taux_1_ij(:,:)
!   W'ly component of surface wind stress (N/sq m)
REAL, ALLOCATABLE :: tauy_1_ij(:,:)
!   S'ly component of surface wind stress (N/sq m)
!   On V-grid; comments as per TAUX

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='GRIDMEAN_FLUXES'

CONTAINS

SUBROUTINE gridmean_fluxes_alloc(t_i_length,t_j_length)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: t_i_length,t_j_length

!Local variables
INTEGER :: temp_size, temp_tiles, temp_layers

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='GRIDMEAN_FLUXES_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE( fqw_1_ij(t_i_length,t_j_length))
ALLOCATE( ftl_1_ij(t_i_length,t_j_length))
ALLOCATE( taux_1_ij(t_i_length,t_j_length))
ALLOCATE( tauy_1_ij(t_i_length,t_j_length))

fqw_1_ij(:,:)                = 0.0
ftl_1_ij(:,:)                = 0.0
taux_1_ij(:,:)               = 0.0
tauy_1_ij(:,:)               = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE gridmean_fluxes_alloc

END MODULE gridmean_fluxes

#endif
