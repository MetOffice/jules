! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE DARCY_CH-----------------------------------------------

! Description:
!     Calculates the Darcian fluxes between adjacent soil layers.

! Documentation : UM Documentation Paper 25

MODULE darcy_ch_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DARCY_CH_MOD'

CONTAINS

SUBROUTINE darcy_ch (npnts, soil_pts, dz1, dz2, soil_index, b, ks, sathh,      &
                     sthu1, sthu2, wflux,                                      &
                     dwflux_dsthu1, dwflux_dsthu2)

!Use in relevant subroutines
USE hyd_con_ch_mod, ONLY: hyd_con_ch

!Use in relevant variables
USE jules_soil_mod, ONLY: l_dpsids_dsdz

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  soil_pts
    ! Number of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz1,                                                                         &
    ! Thickness of the upper layer (m).
  dz2
    ! Thickness of the lower layer (m).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(npnts,2),                                                                  &
    ! Brooks & Corey exponent.
  ks(npnts),                                                                   &
    ! Saturated hydraulic conductivity (kg/m2/s).
  sathh(npnts,2),                                                              &
    ! Saturated soil water pressure (m).
  sthu1(npnts),                                                                &
    ! Unfrozen soil moisture content of upper layer as a fraction of
    ! saturation.
  sthu2(npnts)
    ! Unfrozen soil moisture content of lower layer as a fraction of
    ! saturation.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  wflux(npnts),                                                                &
    ! The flux of water between layers (kg/m2/s).
  dwflux_dsthu1(npnts),                                                        &
    ! The rate of change of the explicit flux with STHU1 (kg/m2/s).
  dwflux_dsthu2(npnts)
    ! The rate of change of the explicit flux with STHU2 (kg/m2/s).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  dthk_dth1,dthk_dth2,                                                         &
    ! DTHETAK/DTHETA(1:2).
  dk_dth1,dk_dth2,                                                             &
    ! DK/DTHETA(1:2) (kg/m2/s).
  pd,                                                                          &
    ! Hydraulic potential difference (m).
  theta(npnts,2),                                                              &
    ! Fractional saturation of the upper and lower layer respectively.
  thetak(npnts),                                                               &
    ! Fractional saturation at the layer boundary.
  k(npnts),                                                                    &
    ! The hydraulic conductivity between layers (kg/m2/s).
  dk_dthk(npnts),                                                              &
    ! The rate of change of K with THETAK (kg/m2/s).
  psi(npnts,2),                                                                &
    ! The soil water suction of the upper and lower layer respectively (m).
  dpsi_dth(npnts,2),                                                           &
    ! The rate of change of PSI with THETA(1:2) (m).
  bk(npnts),                                                                   &
    ! Brooks & Corey exponent at the layer boundary.
  dpsimdz(npnts),                                                              &
    ! Vertical derivative of soil suction at the boundary between layers
  sathhm
    ! Saturated soil suction at the boundaries

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DARCY_CH'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate the fractional saturation of the layers.
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  theta(i,1) = sthu1(i)
  theta(i,2) = sthu2(i)
END DO

!-----------------------------------------------------------------------------
! Calculate the soil water suction of the layers.
!-----------------------------------------------------------------------------
DO n = 1,2
  !CDIR NODEP
  DO j = 1,soil_pts
    i = soil_index(j)
    IF (theta(i,n) <= 0.01) THEN  ! Prevent blow up for dry soil.
      psi(i,n)      = sathh(i,n) / (0.01**b(i,n))
      dpsi_dth(i,n) = 0.0
    ELSE
      psi(i,n)      = sathh(i,n) / (theta(i,n)**b(i,n))
      dpsi_dth(i,n) = -b(i,n) * psi(i,n) / theta(i,n)
    END IF
  END DO
END DO

!-----------------------------------------------------------------------------
! Estimate the fractional saturation at the layer boundary by
! interpolating the soil moisture.
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  thetak(i) = (dz2 * theta(i,1) + dz1 * theta(i,2)) / (dz2 + dz1)

  ! THIS LINE IS REPLACED WITH THE ONE FOLLOWING
  ! IN ORDER TO OBTAIN BIT-COMPARABILITY
  !        BK(I)=(DZ2*B(I,1)+DZ1*B(I,2))/(DZ2+DZ1)
  bk(i) = b(i,1)
  IF (l_dpsids_dsdz) THEN
    sathhm = (dz2 * sathh(i,1) + dz1 * sathh(i,2)) / (dz2 + dz1)
    ! Extra calculations for calculation of dPsi/dz as dPsi/dS * dS/dz.
    ! Split off very dry soil to avoid numerical overflows.
    IF (thetak(i) < 0.01) THEN
      dpsimdz(i) = -bk(i) * sathhm * 0.01**(-bk(i) - 1) *                      &
                   (theta(i,2) - theta(i,1)) * (2.0 / (dz2 + dz1))
    ELSE
      dpsimdz(i) = -bk(i) * sathhm * thetak(i)**(-bk(i) - 1) *                 &
                   (theta(i,2) - theta(i,1)) * (2.0 / (dz2 + dz1))
    END IF
  END IF
END DO
dthk_dth1 = dz2 / (dz1 + dz2)
dthk_dth2 = dz1 / (dz2 + dz1)

!-----------------------------------------------------------------------------
! Calculate the hydraulic conductivities for transport between layers.
!-----------------------------------------------------------------------------
CALL hyd_con_ch (npnts, soil_pts, soil_index, bk, ks, thetak, k, dk_dthk)

!-----------------------------------------------------------------------------
! Calculate the Darcian flux from the upper to the lower layer.
!-----------------------------------------------------------------------------
DO j = 1,soil_pts
  i = soil_index(j)
  pd       = (2.0 * (psi(i,2) - psi(i,1)) / (dz2 + dz1) + 1)
  wflux(i) = k(i) * pd

  !---------------------------------------------------------------------------
  ! Calculate the rate of change of WFLUX with respect to the STHU1 and STHU2.
  !---------------------------------------------------------------------------
  dk_dth1 = dk_dthk(i) * dthk_dth1
  dk_dth2 = dk_dthk(i) * dthk_dth2
  IF (l_dpsids_dsdz) THEN
    dwflux_dsthu1(i) = dk_dth1 * (dpsimdz(i) + 1.0) -                          &
                       2.0 * k(i) * dpsi_dth(i,1) / (dz1 + dz2)
    dwflux_dsthu2(i) = dk_dth2 * (dpsimdz(i) + 1.0) +                          &
                       2.0 * k(i) * dpsi_dth(i,2) / (dz1 + dz2)
  ELSE
    dwflux_dsthu1(i) = dk_dth1 * pd - 2.0 * k(i) * dpsi_dth(i,1) / (dz1 + dz2)
    dwflux_dsthu2(i) = dk_dth2 * pd + 2.0 * k(i) * dpsi_dth(i,2) / (dz1 + dz2)
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE darcy_ch
END MODULE darcy_ch_mod
