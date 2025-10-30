#if !defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Global data module for variables concerned with solar incidence.

!----------------------------------------------------------------------
! THIS IS A STRIPPED DOWN VERSION OF THE UM VERSION
!----------------------------------------------------------------------

MODULE solinc_data

IMPLICIT NONE

! Description:
!   Global data necessary for calculating the angle of solar incidence
!   on sloping terrain.
!
! Method:
!   Provides global data.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Radiation Control
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v7.4 programming standards.
!
! Declarations:
!
! Global variables (#include statements etc):

REAL, ALLOCATABLE, DIMENSION(:,:)   :: sky
LOGICAL :: l_skyview = .FALSE.


! sky:          Sky-view correction factor for net surface LW.
!
! l_skyview:    Model switch for skyview scheme.
!

END MODULE solinc_data
#endif
