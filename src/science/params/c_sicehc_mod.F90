! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with UM setting of
! reciprocal effective areal heat capacity of sea-ice

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.

MODULE c_sicehc

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! reciprocal effective areal heat capacity of sea-ice (1/(J per sq m per K)).
REAL(KIND=real_jlslsm),PARAMETER:: ai  = 4.8e-6

END MODULE c_sicehc
