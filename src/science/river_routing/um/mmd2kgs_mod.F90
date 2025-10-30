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
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
!
!
MODULE mmd2kgs_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='MMD2KGS_MOD'

CONTAINS


SUBROUTINE mmd2kgs(rin, igrcn, din, area, nx, ny, rmiss, jmax)

USE conversions_mod, ONLY: rc => rsec_per_day

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE
!
!     convert rin [mm/day] fo din [kg] using area [m^2]
!     [mm/day] x [m^2] = [10^(-3) m^3/day]
!                       ===> [kg/day] / (3600*24) --> [kg/s]
!
INTEGER :: nx, ny, i, j, igrcn(nx, ny), jmax
REAL(KIND=real_jlslsm) :: rin(nx, ny)
REAL(KIND=real_jlslsm) :: din(nx, ny), area(nx, ny), rmiss

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='MMD2KGS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)

DO j = 1, jmax
  DO i = 1, nx

    IF ((rin(i,j) /= rmiss)) THEN
      din(i,j) = rin(i,j) * area(i,j) / rc
    ELSE
      din(i,j) = 0.0
    END IF
  END DO
END DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN
END SUBROUTINE mmd2kgs
END MODULE mmd2kgs_mod
#endif
