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

MODULE imogen_drive_vars

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module for the fine-temporal meteorology driving data
!     JULES-IMOGEN simulations
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------
! Fine temperal resolution year of climatology (to be used to
! drive JULES)
!-----------------------------------------------------------------
REAL, ALLOCATABLE ::                                                           &
  t_out(:,:,:,:),                                                              &
              ! Calculated temperature (K)
  conv_rain_out(:,:,:,:),                                                      &
              ! Calculated convective rainfall (mm/day)
  conv_snow_out(:,:,:,:),                                                      &
              ! Calculated convective snowfall (mm/day)
  ls_rain_out(:,:,:,:),                                                        &
              ! Calculated large scale rainfall (mm/day)
  ls_snow_out(:,:,:,:),                                                        &
              ! Calculated large scale snowfall (mm/day)
  qhum_out(:,:,:,:),                                                           &
              ! Calculated humidity
  wind_out(:,:,:,:),                                                           &
              ! Calculated wind  (m/s)
  pstar_out(:,:,:,:),                                                          &
              ! Calculated pressure (Pa)
  sw_out(:,:,:,:),                                                             &
              ! Calculated shortwave radiation
  lw_out(:,:,:,:)
              ! Calculated longwave radiation

END MODULE imogen_drive_vars
#endif
