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
MODULE setnext_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SETNEXT_MOD'

CONTAINS




SUBROUTINE setnext(nx, ny, igrcn, inextx, inexty)


USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE
!
!     set the destination grid point
!
!     (i, j) ===>  (inextx(i,j), inexty(i,j))
!     at river mouth : pointing itself
!     at sea         : 0
!
INTEGER :: nx, ny
INTEGER :: igrcn(nx, ny), inextx(nx, ny), inexty(nx, ny)
INTEGER :: i, j
INTEGER :: dx(10),dy(10)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SETNEXT'

DATA dx / 0, 1, 1, 1, 0, -1, -1, -1, 0, 0 /
DATA dy / 1, 1, 0, -1, -1, -1, 0, 1, 0, 0 /

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)
DO j = 1, ny
  DO i = 1, nx

    IF (igrcn(i,j)  >= 1 .AND. igrcn(i,j)  <= 10) THEN
      inextx(i,j) = i + dx(igrcn(i,j) )
      inexty(i,j) = j + dy(igrcn(i,j) )

    ELSE
      inextx(i,j) = 0
      inexty(i,j) = 0
    END IF

  END DO
END DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN
END SUBROUTINE setnext
END MODULE setnext_mod
#endif
