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

MODULE diffcarb_land_co2_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE diffcarb_land_co2(land_pts, d_land_atmos_co2, darea, dctot_co2)

USE conversions_mod, ONLY: conv_gtc_to_ppm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the global change in atmospheric CO2 due to
!   grid box changes in total carbon content.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Written by: C. Huntingford (November 1999)
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  land_pts
               ! Number of land points

REAL, INTENT(IN) ::                                                            &
  dctot_co2(land_pts),                                                         &
               ! Change in total surface gridbox co2 content
               ! between a calling period (here years) (kg C/m2)
  darea(land_pts)
               ! Gridbox area (m2)

REAL, INTENT(OUT) ::                                                           &
  d_land_atmos_co2
               ! Change in atmospheric CO2 concentration
               ! as a result of land-atmosphere feedbacks
               ! between calling periods (here years) (ppm/year)

REAL ::                                                                        &
  land_gain
               ! Total gain in carbon by land (kg C)

INTEGER ::                                                                     &
  l            ! Looping parameter



land_gain = 0.0
DO l = 1,land_pts
  land_gain = land_gain + darea(l) * dctot_co2(l)
END DO

d_land_atmos_co2 = -(land_gain / 1.0e12) * conv_gtc_to_ppm

RETURN

END SUBROUTINE diffcarb_land_co2
END MODULE diffcarb_land_co2_mod
#endif
