! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module Parameter values for vegetation routines.

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE veg_param

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
SAVE

!-----------------------------------------------------------------------
! Parameters used in calculation of leaf turnover rate.
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
 secs_per_360days = 31104000.0  ! Number of seconds in 360 days

!-----------------------------------------------------------------------
! Parameter used for agriculture option.
LOGICAL, PARAMETER ::                                                          &
 agric = .TRUE.                 ! .T. for TRIFFID to see agriculture
!                                 .F. for natural vegetation.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Normalisation of soil litter inputs for discretised soil carbon
! Should be initialised to the correct value later
REAL(KIND=real_jlslsm) ::                                                      &
 litc_norm = 1.0


END MODULE veg_param
