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
MODULE initial0_mod

USE setarea_mod, ONLY: setarea
USE setlen_mod, ONLY: setlen
USE setnext_mod, ONLY: setnext
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INITIAL0_MOD'

CONTAINS

!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing


SUBROUTINE initial0(nx, ny, jmax, rmiss, igrcn, iseq                           &
, nseqmax, inextx, inexty, rlen, area, offset_nx, offset_ny)

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE
!
!     Setting Initial Conditions
!
INTEGER, INTENT(IN) ::                                                         &
 nx                                                                            &
  ! number of columns
, ny                                                                           &
  ! number of rows
, jmax                                                                         &
  ! number of rows ! redundant
, offset_nx                                                                    &
  ! column offset from latitude origin
, offset_ny                                                                    &
  ! row offset from longitude origin
, igrcn(nx, ny)                                                                &
  ! river direction
, iseq(nx, ny)
  ! river sequence field


INTEGER, INTENT(OUT) ::                                                        &
  nseqmax                                                                      &
 ! max sequence in river subdomain
, inextx(nx, ny)                                                               &
 ! next lat downstream points for a river grid point
, inexty(nx, ny)
 ! next long downstream point for a river grid point

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  rlen(nx, ny)
  ! distance between river grid points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  rmiss
  ! flag for missing value

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  area(nx, ny)
  ! river grid point area [m^2]
INTEGER :: i, j
  ! loop counters

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INITIAL0'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)

! get the maximum river sequence in this river subdomain
nseqmax = 0
DO i = 1, nx
  DO j= 1, ny
    IF ( iseq(i,j) >  nseqmax) nseqmax = iseq(i,j)
  END DO
END DO

! set downstream points

CALL setnext(nx, ny, igrcn, inextx, inexty)


! set the distance between grids [m]

CALL setlen (nx, ny, igrcn, inextx, inexty, rlen, jmax                         &
, rmiss, offset_nx, offset_ny)

! set area of each river grid point [m^2]

CALL setarea(nx, ny, area, jmax, offset_ny)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN

END SUBROUTINE initial0
END MODULE initial0_mod
#endif
