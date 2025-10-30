! *****************************COPYRIGHT*******************************
! (c) Crown copyright, Met Office, All Rights Reserved.
! Please refer to file $UMDIR/vn$VN/copyright.txt for further details
! *****************************COPYRIGHT*******************************
!    SUBROUTINE DARCY_VG-----------------------------------------------

! Description:
!     Calculates the Darcian fluxes between adjacent soil layers
!     using Van Genuchten formulation

! Documentation : UM Documentation Paper 25

MODULE darcy_vg_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DARCY_VG_MOD'

CONTAINS

SUBROUTINE darcy_vg(npnts, soil_pts, dz1, dz2, soil_index,                     &
                    b, ks, sathh,                                              &
                    sthu1, sthu2, wflux,                                       &
                    dwflux_dsthu1, dwflux_dsthu2 )

! Use in relevant subroutines
USE hyd_con_vg_mod, ONLY: hyd_con_vg

! Use in relevant variables
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
    ! Brooks & Corey exponent for PSI.
  ks(npnts),                                                                   &
    ! Saturated hydraulic conductivity (kg/m2/s).
  sathh(npnts,2),                                                              &
    ! Saturated soil water pressure (m).
  sthu1(npnts),                                                                &
    ! Unfrozen soil moisture content of upper layer as a fraction of
    ! saturation.
  sthu2(npnts)
    ! Unfrozen soil moisture content of lower layer as a fraction of#
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
! Local parameters:
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: j_block = 5 ! chunk size for each thread

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  theta_min = 0.05,                                                            &
    ! Minimum value of THETAK for K calculation.
  theta_max = 0.95
    ! Maximum value of THETAK for K calculation.

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n, jj
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  dthk_dth1,dthk_dth2,                                                         &
    ! DTHETAK/DTHETA(1:2).
  dk_dth1,dk_dth2,                                                             &
    ! DK/DTHETA(1:2) (kg/m2/s).
  pd,                                                                          &
    ! Hydraulic potential difference (m).
  sdum(npnts,2),                                                               &
    ! bounded values of THETAK.
  bk(npnts),                                                                   &
    ! Brooks & Corey exponent at the layer boundary.
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
  bracket(npnts,2),                                                            &
    ! 1-S^(-b-1)
  dbr_dth(npnts,2),                                                            &
    ! The rate of change of BRACKET with THETA(1:2) (m).
  dpsimdz(npnts),                                                              &
    ! Vertical derivative of soil suction at the boundary between layers.
  sathhm
    ! Saturated soil suction at the boundaries

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DARCY_VG'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate the fractional saturation of the layers
!-----------------------------------------------------------------------------

!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) SHARED(theta,                &
!$OMP  sdum,  bk, soil_index, sthu1, sthu2, b, soil_pts)                       &
!$OMP  PRIVATE(i, j)
DO j = 1,soil_pts
  i = soil_index(j)
  theta(i,1) = sthu1(i)
  theta(i,2) = sthu2(i)

  ! Assume values at layer boundaries are equal to those at the centre of
  ! the layer above (Thomas Mayr)
  bk(i) = b(i,1)

  sdum(i,1) = MAX(theta(i,1),theta_min)
  sdum(i,1) = MIN(sdum(i,1),theta_max)
  sdum(i,2) = MAX(theta(i,2),theta_min)
  sdum(i,2) = MIN(sdum(i,2),theta_max)
END DO
!$OMP END PARALLEL DO

!-----------------------------------------------------------------------------
! Calculate the soil water suction of the layers.
!-----------------------------------------------------------------------------
! work is blocked over soil_pts to avoid thread parallel
! limitation of n dimension
!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) PRIVATE(i, j, jj, n)         &
!$OMP  SHARED( soil_pts, soil_index, bracket, sdum, b, psi, dpsi_dth,          &
!$OMP          sathh, theta, dbr_dth )
DO jj = 1, soil_pts, j_block
  DO n = 1,2

    !CDIR NODEP
    DO j = jj,MIN(jj + j_block - 1,soil_pts)
      i = soil_index(j)
      bracket(i,n)=-1 + sdum(i,n)**(-b(i,n) - 1)
      psi(i,n) = sathh(i,n) * bracket(i,n)**(b(i,n) / (b(i,n) + 1))

      !----------------------------------------------------------------------
      ! To avoid blow-up of implicit increments approximate by piecewise
      ! linear functions
      ! (a) for THETA>THETA_MAX  (ensuring that PSI=0 at THETA=1)
      ! (b) for THETA<THETA_MIN  (extrapolating using DPSI_DTH at THETA_MIN)
      !----------------------------------------------------------------------
      IF (theta(i,n) >  theta_max) THEN
        dpsi_dth(i,n) = -psi(i,n) / (1.0 - theta_max)
        psi(i,n)      = psi(i,n)                                               &
                        + dpsi_dth(i,n) * (MIN(theta(i,n),1.0) - theta_max)
      ELSE
        dbr_dth(i,n)  = (-b(i,n) - 1.0) * sdum(i,n)**(-b(i,n) - 2.0)
        dpsi_dth(i,n) = sathh(i,n) * b(i,n) / (b(i,n) + 1.0)                   &
                        * bracket(i,n)**(-1.0 / (b(i,n) + 1.0))                &
                        * dbr_dth(i,n)
        IF (theta(i,n) <  theta_min) THEN
          psi(i,n) = psi(i,n)                                                  &
                     + dpsi_dth(i,n) * (MAX(theta(i,n),0.0) - theta_min)
        END IF
      END IF

      IF ((theta(i,n) >  1.0) .OR. (theta(i,n) <  0.0)) THEN
        dpsi_dth(i,n) = 0.0
        psi(i,n)      = MAX(psi(i,n),0.0)
      END IF

    END DO
  END DO

END DO
!$OMP END PARALLEL DO

!-----------------------------------------------------------------------------
! Estimate the fractional saturation at the layer boundary by
! interpolating the soil moisture.
!-----------------------------------------------------------------------------

!$OMP  PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i,j,sathhm)          &
!$OMP  SHARED(soil_pts, thetak, dz2, dz1, theta, soil_index, bk, b,            &
!$OMP  l_dpsids_dsdz, dpsimdz, sdum, sathh)
DO j = 1,soil_pts
  i = soil_index(j)
  thetak(i) = (dz2 * theta(i,1) + dz1 * theta(i,2)) / (dz2 + dz1)
  bk(i)     = (dz2 * b(i,1) + dz1 * b(i,2)) / (dz2 + dz1)
  IF (l_dpsids_dsdz) THEN
    ! Extra calculations for calculation of dPsi/dz as dPsi/dS * dS/dz.
    sathhm  = (dz2 * sathh(i,1) + dz1 * sathh(i,2)) / (dz2 + dz1)
    ! Split off extreme values to avoid numerical problems when bk is large.
    IF (thetak(i) < 0.01) THEN
      dpsimdz(i) = -bk(i) * sathhm                                             &
                   * 0.01**(-bk(i) - 1.0)                                      &
                   * (sdum(i,2) - sdum(i,1)) * (2.0 / (dz2 + dz1))
    ELSE IF (thetak(i) > 0.999) THEN
      ! Truncate S: dPsi/dS will rise as S-->1, but only very close to 1.
      dpsimdz(i) = -bk(i) * sathhm                                             &
                   * (0.999**(-bk(i) - 1.0) - 1.0)**(-1.0 / (bk(i) + 1.0))     &
                   * 0.999**(-bk(i) - 2.0)                                     &
                   * (sdum(i,2) - sdum(i,1)) * (2.0 / (dz2 + dz1))
    ELSE
      dpsimdz(i) = -bk(i) * sathhm                                             &
                   * (thetak(i)**(-bk(i) - 1.0) - 1.0)                         &
                   **(-1.0 / (bk(i) + 1.0))                                    &
                   * thetak(i)**(-bk(i) - 2)                                   &
                   * (sdum(i,2) - sdum(i,1)) * (2.0 / (dz2 + dz1))
    END IF
  END IF
END DO
!$OMP END PARALLEL DO

dthk_dth1 = dz2 / (dz1 + dz2)
dthk_dth2 = dz1 / (dz2 + dz1)

!-----------------------------------------------------------------------------
! Calculate the hydraulic conductivities for transport between layers.
!-----------------------------------------------------------------------------
CALL hyd_con_vg(npnts, soil_pts, soil_index, bk, ks, thetak, k, dk_dthk)

!-----------------------------------------------------------------------------
! Calculate the Darcian flux from the upper to the lower layer.
!-----------------------------------------------------------------------------
!$OMP  PARALLEL DO DEFAULT(NONE) SHARED(soil_index, wflux, dz1,                &
!$OMP  dz2, dwflux_dsthu1, dwflux_dsthu2, psi, k,                              &
!$OMP  dpsi_dth, dk_dthk, soil_pts, dthk_dth1, dthk_dth2,                      &
!$OMP  dpsimdz,l_dpsids_dsdz) PRIVATE(pd, i, j, dk_dth1, dk_dth2)
DO j = 1,soil_pts
  i = soil_index(j)
  IF (l_dpsids_dsdz) THEN
    wflux(i) = k(i) * (dpsimdz(i) + 1.0)
  ELSE
    pd       = (2.0 * (psi(i,2) - psi(i,1)) / (dz2 + dz1) + 1.0)
    wflux(i) = k(i) * pd
  END IF

  !---------------------------------------------------------------------------
  ! Calculate the rate of change of WFLUX with respect to the STHU1 and
  ! STHU2.
  !---------------------------------------------------------------------------
  dk_dth1 = dk_dthk(i) * dthk_dth1
  dk_dth2 = dk_dthk(i) * dthk_dth2
  IF (l_dpsids_dsdz) THEN
    dwflux_dsthu1(i) = dk_dth1 * (dpsimdz(i) + 1.0)                            &
                       - 2.0 * k(i) * dpsi_dth(i,1) / (dz1 + dz2)
    dwflux_dsthu2(i) = dk_dth2 * (dpsimdz(i) + 1.0)                            &
                       + 2.0 * k(i) * dpsi_dth(i,2) / (dz1 + dz2)
  ELSE
    dwflux_dsthu1(i) = dk_dth1 * pd - 2.0 * k(i) * dpsi_dth(i,1) / (dz1 + dz2)
    dwflux_dsthu2(i) = dk_dth2 * pd + 2.0 * k(i) * dpsi_dth(i,2) / (dz1 + dz2)
  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE darcy_vg
END MODULE darcy_vg_mod
