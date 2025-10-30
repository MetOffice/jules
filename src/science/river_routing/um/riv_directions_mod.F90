#if defined(UM_JULES)
! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

MODULE riv_directions

USE missing_data_mod,   ONLY: rmdi ! UM's real missing data indicator

IMPLICIT NONE

!-----------------------------------------------------------------------------
!
! Module: RIV_DIRECTIONS
!
! Description:
!   Define "special" values of the river directions field used
!   by the TRIP river routine scheme.
!   This prevents the use of "magic numbers" within the main
!   code and make it more transparent.
!
! Current Code Owner: Please refer to the JULES science module leaders
!   This file belongs in module: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

  ! "Directions" used to specify open sea points
  !   Note that this is an array as there are multiple ways of
  !   defining rivermouths, so it is necessary to compare the
  !   ancillary with all elements of this array
  !   In some ancillary files this is specified by zero, but in others
  !   it is INT(rmdi). The latter is a slightly strange definition,
  !   but occurs because the ancillary of directions is defined as
  !   real numbers, where the sea points are rmdi, but before being
  !   used by the river routing, the entire array is converted to integers
INTEGER, PARAMETER, DIMENSION(2) :: open_sea_dirs = [0,INT(rmdi) ]

! "Direction" used to specify rivermouth outflow points
INTEGER, PARAMETER :: river_outflow_dir = 9

! "Direction" used to specify inland basin outflow points
INTEGER, PARAMETER :: inland_outflow_dir = 10

END MODULE riv_directions
#endif
