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
!     returns longitude at (ix) in (nlo)
!
!     NOTICE: this is for GPCP/Xie and Arkin data set
!     which data is located at the center of 2.5 grid box
!
!     ix from west to east.

MODULE getlon0_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CONTAINS

REAL(KIND=real_jlslsm) FUNCTION getlon0(ix, nlo)
IMPLICIT NONE
!
!
!Code Owner: Please refer to the UM file CodeOwners.txt
!This file belongs in section: River Routing
INTEGER :: ix, nlo
!
getlon0 = 360.0 * (REAL(ix) - 0.5) / REAL(nlo)
!

END FUNCTION getlon0
END MODULE getlon0_mod
#endif
