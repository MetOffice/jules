! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module with UM setting of
! molar universal gas constant (8.314 J K-1 MOL-1)

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 programming standards.


MODULE c_rmol

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

REAL(KIND=real_jlslsm),PARAMETER:: rmol = 8.314
                              ! Molar universal gas constant (J K-1 mol-1)

END MODULE c_rmol
