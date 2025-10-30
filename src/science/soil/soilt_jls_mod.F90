! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE SOILT---------------------------------------------------------

! Description:
!     Diagnoses the mean soil temperature from the surface to a
!     defined depth. This mean soil temperature is used in the
!     calculation of wetland CH4 emissions only.

MODULE soilt_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOILT_MOD'

CONTAINS

SUBROUTINE soilt(npnts, nshyd, soil_pts, soil_index, dz, tsoil, tsoil_d )

USE jules_soil_mod, ONLY:                                                      &
  zst  ! Depth of layer for soil moisture diagnostic (m).

USE parkind1,       ONLY: jprb, jpim
USE yomhook,        ONLY: lhook, dr_hook

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
  tsoil(npnts,nshyd)
    ! Soil temperature (K)

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  tsoil_d(npnts)
    ! Mean soil temperature in layer (K)

!-----------------------------------------------------------------------------
!Local variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,j,n

REAL(KIND=real_jlslsm) ::                                                      &
  z1,z2
    ! Depth of the top and bottom of the soil layers (m).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOILT'

!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

z2 = 0.0

DO i = 1,npnts
  tsoil_d(i) = 0.0
END DO

DO n = 1,nshyd
  z1 = z2
  z2 = z2 + dz(n)
  IF ( z2 <  zst ) THEN
    DO j = 1,soil_pts
      i = soil_index(j)
      tsoil_d(i) = tsoil_d(i) + dz(n) * tsoil(i,n)
    END DO
  ELSE IF ( z2 >= zst .AND. z1 <  zst ) THEN
    DO j = 1,soil_pts
      i = soil_index(j)
      tsoil_d(i) = tsoil_d(i) + ( zst - z1 ) * tsoil(i,n)
    END DO
  END IF
END DO

IF (zst > 0.0) THEN
  DO j = 1,soil_pts
    i = soil_index(j)
    tsoil_d(i) = tsoil_d(i) / zst
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soilt
END MODULE soilt_mod
