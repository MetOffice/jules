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

MODULE imogen_io_vars

USE imogen_constants, ONLY: nyr_max

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module for the io variables required for JULES-IMOGEN simulations
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------


INTEGER ::                                                                     &
  yr_emiss(nyr_max)
           ! Years in which CO2 emissions are prescribed
           ! (only first NYR_EMISS array components are used)
           ! For this code, years must be sequential

REAL ::                                                                        &
  c_emiss(nyr_max)
           ! Values of CO2 emissions read in (up to NYR_EMISS)

END MODULE imogen_io_vars
#endif
