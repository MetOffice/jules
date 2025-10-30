! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE phenol_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PHENOL_MOD'

CONTAINS

!----------------------------------------------------------------------------
! Subroutine PHENOL ---------------------------------------------------------
!
!
! Purpose :  Parametrizes leaf phenological changes and updates the
!            leaf area index and the leaf turnover rate.
!
! ---------------------------------------------------------------------------
SUBROUTINE phenol (land_pts, veg_pts, n, veg_index, dtime_phen, g_leaf, ht,    &
                  lai, g_leaf_phen)

USE jules_vegetation_mod, ONLY: l_nitrogen
USE pftparm, ONLY: a_wl, a_ws, b_wl, eta_sl, g_leaf_0
USE trif, ONLY: g_grow

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  veg_pts,                                                                     &
    ! Number of vegetated points.
  n
    ! Plant functional type.

INTEGER, INTENT(IN) ::                                                         &
  veg_index(land_pts)
    ! Index of vegetated points on the land grid.

REAL(KIND=real_jlslsm) , INTENT(IN) ::                                         &
  dtime_phen
    ! Timestep (years).

REAL(KIND=real_jlslsm) , INTENT(IN) ::                                         &
  g_leaf(land_pts),                                                            &
    ! Rate of leaf turnover (/360days).
  ht(land_pts)
    ! Canopy height (m).

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  lai(land_pts)
    ! Leaf area index.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  g_leaf_phen(land_pts)
    ! Rate of leaf turnover including leaf phenology (/360days).

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  j,l       ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  dphen     ! Increment to phenological state.

REAL(KIND=real_jlslsm) ::                                                      &
  lai_bal(land_pts),                                                           &
    ! Balanced growth LAI.
  phen(land_pts)
    ! Phenological state.

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PHENOL'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Diagnose the phenological state
!-----------------------------------------------------------------------------
DO j = 1,veg_pts
  l = veg_index(j)
  lai_bal(l) = (a_ws(n) * eta_sl(n) * ht(l)                                    &
               /a_wl(n))**(1.0 / (b_wl(n) - 1.0))
  phen(l)    = lai(l) / lai_bal(l)
END DO

!-----------------------------------------------------------------------------
! Update the phenological state and output the leaf turnover rate in
! terms of the balanced growth LAI
!-----------------------------------------------------------------------------
DO j = 1,veg_pts
  l = veg_index(j)

  IF (g_leaf(l) >  2.0 * g_leaf_0(n)) THEN
    dphen          = -dtime_phen * g_grow(n)
    dphen          = MAX(dphen,(0.01 - phen(l)))
    g_leaf_phen(l) = -dphen / dtime_phen

  ELSE
    dphen          = dtime_phen * g_grow(n) * (1.0 - phen(l))
    dphen          = MIN(dphen,(1.0 - phen(l)))
    g_leaf_phen(l) = phen(l) * g_leaf(l)

    ! In the Nitrogen scheme use g_leaf_phen to keep track of net change in
    ! phen.
    IF (l_nitrogen) g_leaf_phen(l) = - dphen / dtime_phen

  END IF

  !---------------------------------------------------------------------------
  ! Update the leaf area index
  !---------------------------------------------------------------------------
  phen(l) = phen(l) + dphen
  lai(l)  = phen(l) * lai_bal(l)

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE phenol

END MODULE phenol_mod
