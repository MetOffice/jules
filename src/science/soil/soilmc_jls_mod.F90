! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!    SUBROUTINE SOILMC--------------------------------------------------------

! Description:
!     Diagnoses the soil moisture in a layer at the surface

MODULE soilmc_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOILMC_MOD'

CONTAINS

SUBROUTINE soilmc ( npnts, nshyd, soil_pts, soil_index,                        &
                    dz, sthu, v_sat, v_wilt, smc )

USE water_constants_mod, ONLY: rho_water

USE jules_soil_mod, ONLY:                                                      &
  zsmc ! Depth of layer for soil moisture diagnostic (m).

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts
    ! Number of soil points.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz(nshyd),                                                                   &
    ! Thicknesses of the soil layers (m).
  sthu(npnts,nshyd),                                                           &
    ! Unfrozen soil moisture content of each layer as a frac. of saturation.
  v_sat(npnts,nshyd),                                                          &
    ! Volumetric soil moisture conc. at saturation (m3 H2O/m3 soil).
  v_wilt(npnts,nshyd)
    ! Volumetric soil moisture conc. below which stomata close
    ! (m3 H2O/m3 soil).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  smc(npnts)
    ! Soil moisture (kg/m2).

!-----------------------------------------------------------------------------
! Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n

REAL(KIND=real_jlslsm) ::                                                      &
  z1, z2
    ! Depth of the top and bottom of the soil layers (m).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOILMC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!-----------------------------------------------------------------------------

DO i = 1,npnts
  smc(i) = 0.0
END DO

z2 = 0.0
DO n = 1,nshyd
  z1 = z2
  z2 = z2 + dz(n)
  IF ( z2 <  zsmc ) THEN
    !CDIR NODEP
    DO j = 1,soil_pts
      i = soil_index(j)
      smc(i) = smc(i) + rho_water * dz(n)                                      &
                      * ( sthu(i,n) * v_sat(i,n) - v_wilt(i,n) )
    END DO
  ELSE IF ( z2 >= zsmc .AND. z1 <  zsmc ) THEN
    !CDIR NODEP
    DO j = 1,soil_pts
      i = soil_index(j)
      smc(i) = smc(i) + rho_water * ( zsmc - z1 )                              &
                      * ( sthu(i,n) * v_sat(i,n) - v_wilt(i,n) )
    END DO
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soilmc
END MODULE soilmc_mod
