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
!
!  returns latituds at (iy) in (nla)
!
!     NOTICE: this is for GPCP/Xie and Arkin data set
!     which data is located at the center of 2.5 grid box
!
!     iy from SOUTH to NORTH.
!
!     from 23.Feb.1996, by Taikan OKI
!
!
!
!Code Owner: Please refer to the UM file CodeOwners.txt
!This file belongs in section: River Routing

MODULE getlat0_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CONTAINS

REAL(KIND=real_jlslsm) FUNCTION getlat0(iy, nla)
IMPLICIT NONE
!
INTEGER :: iy, nla
!
getlat0 = 180.0 * (REAL(iy) - 0.5) / REAL(nla) - 90.0
!

END FUNCTION getlat0
END MODULE getlat0_mod
#endif
