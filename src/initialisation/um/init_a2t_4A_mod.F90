#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!Code Owner: Please refer to the UM file CodeOwners.txt
!This file belongs in section: River Routing

MODULE INIT_A2T_4A_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='INIT_A2T_4A_MOD'

CONTAINS
SUBROUTINE init_a2t_4a(a_realhd, xpa, xua, xva, ypa, yua, yva)

!  Routine: INIT_A2T_4A ---------------------------------------------
!
!  Purpose: Initialises the Lat and Long values of ATMOS for
!           regridding to TRIP river routing grid (based on INITA2O)
!           NB this will need to be extended to pick up the values
!           for the river routing grid from the ancil header of one
!           file later so will leave in redundant code for guidance.
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.3 programming standards.
!  -------------------------------------------------------------------

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE nlsizes_namelist_mod, ONLY: a_len_realhd, aocpl_row_length, aocpl_p_rows

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
! Description: Gridline coordinates for interpolation and area-averaging
! between atmosphere and river-routing grids (Part of TYPAOCPL.h)

REAL(KIND=real_jlslsm), INTENT(IN)  :: a_realhd(a_len_realhd)   !real header
REAL(KIND=real_jlslsm), INTENT(OUT) :: xpa(aocpl_row_length+1)
                                              !Atmosphere TP longitude coordina
REAL(KIND=real_jlslsm), INTENT(OUT) :: xua(0:aocpl_row_length)
                                              !Atmosphere U longitude coordinat
REAL(KIND=real_jlslsm), INTENT(OUT) :: xva(aocpl_row_length+1)
                                              !Atmosphere V longitude coordinat
REAL(KIND=real_jlslsm), INTENT(OUT) :: ypa(aocpl_p_rows)
                                              !Atmosphere TP latitude coordinat
REAL(KIND=real_jlslsm), INTENT(OUT) :: yua(aocpl_p_rows)
                                              !Atmosphere U latitude coordinate
REAL(KIND=real_jlslsm), INTENT(OUT) :: yva(0:aocpl_p_rows)
                                              !Atmosphere V latitude coordinate

INTEGER :: i,j

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_A2T_4A'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! This would be the "correct" ENDGame grid, assuming that array
! index 1 refers to
! the first entry (and 0 to the extensions - in ENDGame u&v index 0
! referes to the
! first entry)
!
! Pick up the ATMOS x coords from the dump header
!      XUA(0)=A_REALHD(4)-A_REALHD(1)
!      DO I=1,AOCPL_ROW_LENGTH
!        XPA(I)=A_REALHD(4)+(I-0.5)*A_REALHD(1)
!        XVA(I)=A_REALHD(4)+(I-0.5)*A_REALHD(1)
!        XUA(I)=A_REALHD(4)+(I-1)*A_REALHD(1)
!      END DO
!      XPA(AOCPL_ROW_LENGTH+1)=A_REALHD(4)+(AOCPL_ROW_LENGTH+0.5)*A_REALHD(1)
!      XVA(AOCPL_ROW_LENGTH+1)=A_REALHD(4)+(AOCPL_ROW_LENGTH+0.5)*A_REALHD(1)
!
! Pick up the ATMOS y coords from the dump header
!      DO J=1,AOCPL_P_ROWS
! JCT  S->N now  YTA(J)=A_REALHD(3)-(J-1)*A_REALHD(2)
!        YPA(J)=A_REALHD(3)+(J-0.5)*A_REALHD(2)
!        YUA(J)=A_REALHD(3)+(J-0.5)*A_REALHD(2)
!      END DO
!      DO J=0,AOCPL_P_ROWS
! JCT  S->N now  YUA(J)=A_REALHD(3)-(J-0.5)*A_REALHD(2)
!        YVA(J)=A_REALHD(3)+(J)*A_REALHD(2)
!      END DO

! river routing will assume that a p-point gridbox is defined such that
! the first entry is at XPA(1)=0, YPA(1)=-90 and that the gridbox is defined
!
!    XUA(0),YVA(0)  XUA(1),YVA(0)
!          +--------------+
!          |              |
!          |      x  XPA(1)=0, YPA(1)=-90
!          |              |
!          +--------------+
!    XUA(0),YVA(1)  XUA(1),YVA(1)
!
! for ENDGame that is different. In ENDGame the first p-point is defined at
! XPA(1)=dx/2, YPA(1)=-90+dx/2, XUA(0)=0

xua(0) = a_realhd(4)
DO i = 1,aocpl_row_length
  xpa(i) = a_realhd(4) + (i-0.5) * a_realhd(1)
  xva(i) = a_realhd(4) + (i-0.5) * a_realhd(1)
  xua(i) = a_realhd(4) + i * a_realhd(1)
END DO
xpa(aocpl_row_length+1) = a_realhd(4) + (aocpl_row_length+0.5) * a_realhd(1)
xva(aocpl_row_length+1) = a_realhd(4) + (aocpl_row_length+0.5) * a_realhd(1)

DO j = 1,aocpl_p_rows
  ypa(j) = a_realhd(3) + (j-0.5) * a_realhd(2)
  yua(j) = a_realhd(3) + (j-0.5) * a_realhd(2)
END DO
DO j = 0,aocpl_p_rows
  yva(j) = a_realhd(3) + (j) * a_realhd(2)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
!----------------------------------------------------------------------
END SUBROUTINE init_a2t_4a
END MODULE INIT_A2T_4A_mod
#endif
