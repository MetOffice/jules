! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE SOIL_HYD_UPDATE-----------------------------------------------

MODULE soil_hyd_update_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOIL_HYD_UPDATE_MOD'

CONTAINS

SUBROUTINE soil_hyd_update(npnts, nshyd, soil_pts, soil_index,                 &
                           dz, v_sat, zdepth, smclzw, sthzw, smclsat,          &
                           smclsatzw)

USE jules_hydrology_mod, ONLY:                                                 &
  ! imported scalar parameters
  zw_max

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  rho_water  !  density of pure water (kg/m3)

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts
    ! Number of soil points.

INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz(nshyd)
    ! Thicknesses of the soil layers (m).


REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  v_sat(npnts,nshyd),                                                          &
    ! Volumetric soil moisture concentration at saturation (m3 H2O/m3 soil).
  zdepth(0:nshyd),                                                             &
    ! Soil layer depth at lower boundary (m).
  sthzw(npnts)
    ! Soil moist fraction in deep layer.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  smclsat(npnts,nshyd),                                                        &
    ! The saturation moisture content of each layer (kg/m2).
  smclsatzw(npnts),                                                            &
    ! Moisture content in deep layer at saturation (kg/m2).
  smclzw(npnts)
    ! Moisture content in deep layer (kg/m2).

!-----------------------------------------------------------------------------
! Local scalars:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n                ! WORK Loop counters.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_HYD_UPDATE'
!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO n = 1,nshyd
  DO j = 1,soil_pts
    i = soil_index(j)
    smclsat(i,n) = rho_water * dz(n) * v_sat(i,n)
  END DO
END DO

DO j = 1,soil_pts
  i = soil_index(j)
  smclsatzw(i) = rho_water * v_sat(i,nshyd) * (zw_max - zdepth(nshyd))
  smclzw(i)    = sthzw(i) * smclsatzw(i)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_hyd_update
END MODULE soil_hyd_update_mod
