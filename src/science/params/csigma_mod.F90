! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with UM setting of Stefan-Boltzmann constant (W/m**2/K**4)

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.

MODULE csigma

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Stefan-Boltzmann constant (W/m**2/K**4).
REAL(KIND=real_jlslsm), PARAMETER ::  sbcon = 5.67e-8

END MODULE csigma
