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

SUBROUTINE radf_co2(co2, co2ref, q2co2, q_co2)

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the radiative forcing due to a given CO2 concentration
!
!    Written by Peter Cox (August 1998)
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

REAL, INTENT (IN) ::                                                           &
  co2,                                                                         &
            ! CO2 concentration (ppmv).
  co2ref,                                                                      &
            ! Reference CO2 concentration (ppmv).
  q2co2
            ! Radiative forcing due to doubling CO2 (W/m2).

REAL, INTENT (OUT) ::                                                          &
  q_co2     ! Radiative forcing due to CO2 (W/m2).

q_co2=(q2co2 / (LOG(2.0))) * LOG(co2 / co2ref)

RETURN
END SUBROUTINE radf_co2
#endif
