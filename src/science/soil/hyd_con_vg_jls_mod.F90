! *****************************COPYRIGHT*******************************
! (c) Crown copyright, Met Office, All Rights Reserved.
! Please refer to file $UMDIR/vn$VN/copyright.txt for further details
! *****************************COPYRIGHT*******************************
!    SUBROUTINE HYD_CON_VG----------------------------------------------------

! Description:
!     Calculates the hydraulic conductivity using Van Genuchten curves

! Documentation : UM Documentation Paper 25

MODULE hyd_con_vg_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='HYD_CON_VG_MOD'

CONTAINS

SUBROUTINE hyd_con_vg(npnts, soil_pts, soil_index, b, ks, thetak, k, dk_dthk)

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Points in grid.
  soil_pts,                                                                    &
    ! Number of soil points.
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(npnts),                                                                    &
    ! Exponent in conductivity and soil water suction fits.
  ks(npnts),                                                                   &
    ! The saturated hydraulic conductivity (kg/m2/s)
  thetak(npnts)
    ! Fractional saturation.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  k(npnts),                                                                    &
    ! The hydraulic conductivity (kg/m2/s).
  dk_dthk(npnts)
    ! The rate of change of K with THETAK (kg/m2/s).

!-----------------------------------------------------------------------------
! Local parameters:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  l_wag     = 0.5,                                                             &
    ! Exponent in the Mualem / van Genuchten fit to the hydraulic
    ! conductivity curve.
  theta_min = 0.05,                                                            &
    ! Minimum value of THETAK for K calculation.
  theta_max = 0.95
    ! Maximum value of THETAK for K calculation.

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j
    ! Loop counter.

REAL(KIND=real_jlslsm) ::                                                      &
  bracket(npnts),                                                              &
    ! 1-S^(b+1)
  dbr_dthk(npnts),                                                             &
    ! The rate of change of BRACKET with THETAK.
  kred(npnts),                                                                 &
    ! KSAT*S^L_WAG (kg/m2/s).
  sdum(npnts)
    ! Bounded THETAK value.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='HYD_CON_VG'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!-----------------------------------------------------------------------------

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(soil_pts,soil_index,sdum,thetak,dk_dthk,bracket,b,kred,ks,k,      &
!$OMP        dbr_dthk)                                                         &
!$OMP PRIVATE(j,i)

!CDIR NODEP
DO j = 1,soil_pts
  i = soil_index(j)

  sdum(i) = MAX(thetak(i),theta_min)
  sdum(i) = MIN(sdum(i),theta_max)

  dk_dthk(i) = 0.0
  bracket(i) = 1.0 - sdum(i)**(b(i) + 1.0)
  kred(i)    = ks(i) * sdum(i)**l_wag

  k(i) = kred(i) * (1.0 - bracket(i)**(1.0 / (b(i) + 1.0)))**2

  !----------------------------------------------------------------------
  ! To avoid blow-up of implicit increments approximate by piecewise
  ! linear functions
  ! (a) for THETA>THETA_MAX  (ensuring that K=KS at THETA=1)
  ! (b) for THETA<THETA_MIN  (ensuring that K=0 at THETA=THETA_MIN)
  !----------------------------------------------------------------------
  IF (thetak(i) <  theta_min) THEN
    dk_dthk(i) = k(i) / theta_min
    k(i)       = k(i) + dk_dthk(i) * (MAX(thetak(i),0.0) - theta_min)
  ELSE IF (thetak(i) >  theta_max) THEN
    dk_dthk(i) = (ks(i) - k(i)) / (1.0 - theta_max)
    k(i)       = k(i) + dk_dthk(i) * (MIN(thetak(i),1.0) - theta_max)
  ELSE
    dbr_dthk(i) = -(b(i) + 1.0) * sdum(i)**b(i)
    dk_dthk(i)  = l_wag * k(i) / sdum(i)                                       &
                  - 2.0 * kred(i) / (b(i) + 1.0)                               &
                  * (1.0 - bracket(i)**(1.0 / (b(i) + 1.0)))                   &
                  * (bracket(i)**(-b(i) / (b(i) + 1.0)))                       &
                  * dbr_dthk(i)
  END IF

  IF ((thetak(i) >  1.0) .OR. (thetak(i) <  0.0)) THEN
    dk_dthk(i) = 0.0
    k(i)       = MAX(k(i),0.0)
    k(i)       = MIN(k(i),ks(i))
  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE hyd_con_vg
END MODULE hyd_con_vg_mod
