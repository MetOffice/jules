! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with setting of
! Critical Richardson number

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE c_surf

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Critical Richardson number, where Z0M_EFF=Z0M.
! Linear interpolation between RIB=0 and RI_CRIT
REAL(KIND=real_jlslsm),PARAMETER :: ri_crit = 0.5

END MODULE c_surf
