! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
MODULE c_kappai

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Description:
!   Module used to hold the sea-ice and sea-surface layer thermal
!   conductivities.
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

! Thermal conductivity of sea-ice (W per m per K).
! Note: this default value can be overridden by
!       input in the jules_surface namelist
REAL(KIND=real_jlslsm) :: kappai = 2.09

! Thermal conductivity of snow on zero layer sea-ice (W per m per K).
! Note: this default value can be overridden by
!       input in the jules_surface namelist
REAL(KIND=real_jlslsm) :: kappai_snow = 0.31

! Snow density (Kg per m**3)
REAL(KIND=real_jlslsm),PARAMETER:: rhosnow = 330.0

! Effective thickness of sea-ice surface layer (m).
REAL(KIND=real_jlslsm),PARAMETER:: de = 0.1

! Effective thermal conductivity of sea surface layer (W per m per K).
! Note: this default value can be overridden by
!       input in the jules_sea_seaice namelist
REAL(KIND=real_jlslsm) :: kappa_seasurf = 0.31

! Effective thickness of sea surface layer (m).
REAL(KIND=real_jlslsm),PARAMETER:: dzsea = 1.0

END MODULE c_kappai
