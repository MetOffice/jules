! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE leaf_lit_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LEAF_LIT_MOD'

CONTAINS

! Purpose:
! Calculates the leaf turnover rate as a function of temperature and
! soil water availability

! **********************************************************************
SUBROUTINE leaf_lit (land_pts,veg_pts,veg_index,n,fsmc,tstar                   &
,                    g_leaf)

USE pftparm, ONLY: tleaf_of,fsmc_of,g_leaf_0,dgl_dm, dgl_dt

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                            ! IN Total number of land points.
,veg_pts                                                                       &
                            ! IN Number of vegetated points.
,veg_index(land_pts)                                                           &
                            ! IN Index of vegetated points
!                                 !    on the land grid.
,n                          ! IN Plant functional type.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 fsmc(land_pts)                                                                &
                            ! IN Soil moisture availability
!                                 !    factor.
,tstar(land_pts)
                            ! IN Surface temperature (K).
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 g_leaf(land_pts)
                            ! OUT Rate of leaf turnover
!                                 !     (/360days).
REAL(KIND=real_jlslsm) ::                                                      &
 fm,ft                      ! WORK Soil moisture and leaf
!                                        temperature amplifiers of
!                                        leaf turnover.

INTEGER ::                                                                     &
 j,l                        ! Loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LEAF_LIT'

!-----------------------------------------------------------------------
! Calculate the leaf turnover rate
!-----------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,l,ft,fm)                                                       &
!$OMP SHARED(veg_pts,veg_index,tstar,tleaf_of,n,dgl_dt,fsmc,fsmc_of,dgl_dm,    &
!$OMP        g_leaf,g_leaf_0)
DO j = 1,veg_pts
  l = veg_index(j)

  ft = 1.0
  fm = 1.0

  IF (tstar(l)  <   tleaf_of(n)) THEN
    ft = 1.0 + dgl_dt(n) * (tleaf_of(n) - tstar(l))
  ELSE IF (fsmc(l)  <   fsmc_of(n)) THEN
    fm = 1.0 + dgl_dm(n) * (fsmc_of(n) - fsmc(l))
  END IF

  g_leaf(l) = g_leaf_0(n) * ft * fm

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE leaf_lit
END MODULE leaf_lit_mod
