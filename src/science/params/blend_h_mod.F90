! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!

MODULE blend_h

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Description:
!   This deck sets up the parameter LB

! Declarations:
! Start blend_h
! Description:
!   This file sets the value of the variable LB

REAL(KIND=real_jlslsm),PARAMETER:: lb = 20.0 ! Blending height (m).

END MODULE blend_h
