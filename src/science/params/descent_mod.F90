! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with UM setting values of gradient descent parameters
!

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE descent

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Number of TRIFFID iterations for gradient descent to equilibrium.
INTEGER,PARAMETER:: iter_eq = 10

! Minimum value for the denominator of the update equation. Ensures
! that gradient descent does not lead to an unstable solution.
REAL(KIND=real_jlslsm),PARAMETER:: denom_min = 1.0e-6

! Inverse timestep for gradient  descent to equilibrium (/360days).
REAL(KIND=real_jlslsm),PARAMETER:: gamma_eq = 1.0e-1

END MODULE descent
