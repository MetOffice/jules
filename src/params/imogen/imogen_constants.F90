#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE imogen_constants

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Constants required for IMOGEN
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, PARAMETER :: n_olevs = 254
            ! Number of ocean levels in thermal calculation
INTEGER, PARAMETER :: nfarray = 20000
            ! Array size for fa_ocean
INTEGER, PARAMETER :: nyr_max = 700
            ! Maximum array size for emissions data

END MODULE imogen_constants
#endif
