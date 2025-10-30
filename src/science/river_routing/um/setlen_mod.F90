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

MODULE setlen_mod

USE jules_print_mgr, ONLY:                                                     &
    jules_print, jules_message,                                                &
    PrintStatus, PrDiag
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SETLEN_MOD'

CONTAINS

SUBROUTINE setlen (nx, ny, igrcn, inextx, inexty, rlen                         &
     , jmax, rmiss, offset_nx, offset_ny)

USE getlat0_mod,  ONLY: getlat0
USE getlon0_mod,  ONLY: getlon0
USE rivers_utils, ONLY: givelength

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE

!
!     length from (i, j) to the destination in [m]
!
!     river mouth : distance to 1 grid north
!     sea         : 0.0
!
INTEGER :: nx, ny, jmax, offset_nx, offset_ny
INTEGER :: inextx(nx, ny), inexty(nx, ny), igrcn(nx, ny), i, j
REAL(KIND=real_jlslsm) :: rx, ry, rx2, ry2
REAL(KIND=real_jlslsm) :: rlen(nx, ny), rmin, rmax, rmiss
INTEGER :: i_offset, j_offset

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SETLEN'

!
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
rx2 = 0.0
ry2 = 0.0
rmin = 1.0e10
rmax = 0.0

j_offset = offset_ny
i_offset = offset_nx

!
DO j = 1, jmax
  ry = getlat0(j + j_offset, 180)
  DO i = 1, nx
    rx = getlon0(i + i_offset, 360)
    !
    IF ((igrcn(i,j) >= 1) .AND. (igrcn(i,j) <= 8)) THEN
      rx2 = getlon0(inextx(i,j) + i_offset, 360)
      ry2 = getlat0(inexty(i,j) + j_offset, 180)
      rlen(i, j) = givelength(ry, rx, ry2, rx2)
    ELSE IF (igrcn(i,j) == 9) THEN
      ry2 = getlat0((j + j_offset) + 1, 180)
      rlen(i, j) = givelength(ry, rx, ry2, rx)
    ELSE IF (igrcn(i,j) == 10) THEN
      ry2 = getlat0((j + j_offset) + 1, 180)
      rlen(i, j) = givelength(ry, rx, ry2, rx)
    ELSE
      rlen(i,j) = 0.0
    END IF
    IF (igrcn(i,j) /= rmiss) THEN
      IF (rlen(i,j) <  rmin) rmin = rlen(i,j)
      IF (rlen(i,j) >  rmax) rmax = rlen(i,j)
    END IF
    !
  END DO
END DO

IF (PrintStatus >= PrDiag) THEN
  WRITE(jules_message,'(A,F12.2,2X,F10.2)')                                    &
       'setlen: proc local rmax, rmin = ', rmax, rmin
  CALL jules_print('setlen',jules_message)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
!
END SUBROUTINE setlen
END MODULE setlen_mod
#endif
