! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE HYD_CON_CH---------------------------------------------

! Description:
!     Calculates the hydraulic conductivity using Brooks & Corey
!     relationships.

! Documentation : UM Documentation Paper 25

MODULE hyd_con_ch_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='HYD_CON_CH_MOD'

CONTAINS

SUBROUTINE hyd_con_ch (npnts, soil_pts, soil_index, b, ks, thetak, k, dk_dthk)

USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Points in grid.
  soil_pts
    ! Number of soil points.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(npnts),                                                                    &
    ! Exponent in conductivity and soil water suction fits.
  ks(npnts),                                                                   &
    ! The saturated hydraulic conductivity (kg/m2
  thetak(npnts)
    ! Fractional saturation.

!-----------------------------------------------------------------------------
! Arrray arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  k(npnts),                                                                    &
    ! The hydraulic conductivity (kg/m2/s).
  dk_dthk(npnts)
    ! The rate of change of K with THETAK (kg/m2/s).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j
    ! Loop counter.

REAL(KIND=real_jlslsm) ::                                                      &
  small_value

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='HYD_CON_CH'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!-----------------------------------------------------------------------------
small_value = EPSILON(0.0)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(soil_pts,soil_index,dk_dthk,thetak,small_value,k,ks,b)            &
!$OMP PRIVATE(j,i)
!CDIR NODEP
DO j = 1,soil_pts
  i = soil_index(j)

  dk_dthk(i) = 0.0
  IF ( (thetak(i) >= small_value) .AND. (thetak(i) < 1.0) ) THEN
    k(i)       = ks(i) * thetak(i)**(2.0 * b(i) + 3.0)
    dk_dthk(i) = (2.0 * b(i) + 3.0) * ks(i) * (thetak(i)**(2.0 * b(i) + 2.0))
  ELSE IF (thetak(i) < small_value) THEN
    k(i) = 0.0
  ELSE
    k(i) = ks(i)
  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE hyd_con_ch
END MODULE hyd_con_ch_mod
