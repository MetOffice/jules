#if defined(UM_JULES)
!Huw Lewis (MO), Jan 2015
!DEPRECATED CODE
!This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!Future developments will supercede these subroutines, and as such they
!should be considered deprecated. They will be retained in the codebase to
!maintain backward compatibility with functionality prior to
!UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing

MODULE setarea_mod

USE um_types, ONLY: real_jlslsm
USE conversions_mod, ONLY: pi

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SETAREA_MOD'

CONTAINS

SUBROUTINE setarea(nx, ny, area, jmax, offset_ny)

USE arealat1_mod, ONLY: arealat1

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE ereport_mod, ONLY: ereport
USE jules_rivers_mod, ONLY: trip_globe_shape, globe_spherical, globe_ellipsoidal
USE planet_constants_mod, ONLY: planet_radius

IMPLICIT NONE
!
!     set area [m^2] of each grid box
!
INTEGER, INTENT(IN) :: nx, ny, jmax, offset_ny
REAL(KIND=real_jlslsm), INTENT(OUT) :: area(nx, ny)
REAL(KIND=real_jlslsm) :: atmp
INTEGER :: j_offset
INTEGER :: i, j
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

REAL(KIND=real_jlslsm)  :: dlat, dlon
REAL(KIND=real_jlslsm)  :: len_lat
REAL(KIND=real_jlslsm)  :: lat_degrees
REAL(KIND=real_jlslsm)  :: lat_rad
REAL(KIND=real_jlslsm)  :: len_lon
INTEGER                 :: errcode

CHARACTER(LEN=*), PARAMETER :: RoutineName='SETAREA'

j_offset = offset_ny

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

SELECT CASE ( trip_globe_shape )

CASE ( globe_spherical )
  ! UM-TRIP is only capable of handling 1 degree data from south to north
  dlat = pi / 180.0
  dlon = dlat
  len_lat = planet_radius * dlat

  DO j = 1, jmax
    lat_degrees = -90.5 + REAL(j + j_offset)
    lat_rad = lat_degrees * pi / 180.0
    len_lon = planet_radius * COS(lat_rad) * dlon
    atmp = len_lat * len_lon
    DO i = 1, nx
      area(i,j) = atmp
    END DO
  END DO

CASE ( globe_ellipsoidal )
  DO j = 1, jmax
    IF ((j + j_offset) <= 90) THEN
      atmp = arealat1(INT(ABS(91.0 - (j + j_offset)))) * 1.0e06
    ELSE
      atmp = arealat1(INT(ABS((j + j_offset) - 90.0))) * 1.0e06
    END IF
    DO i = 1, nx
      area(i,j) = atmp
    END DO
  END DO

CASE DEFAULT
  errcode = 151
  CALL ereport(RoutineName, errcode,                                           &
     "Unsupported trip_globe_shape")
END SELECT

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE setarea

END MODULE setarea_mod
#endif
