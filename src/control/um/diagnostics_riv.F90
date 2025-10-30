#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE diagnostics_riv_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DIAGNOSTICS_RIV_MOD'

CONTAINS

SUBROUTINE diagnostics_riv(                                                    &
              row_length, rows,                                                &
              river_row_length, river_rows,                                    &
              riverout, riverout_rgrid,                                        &
              box_outflow, box_inflow,                                         &
              twatstor, inlandout_riv,                                         &
              stashwork)

! Description:
!   Calculates river-related diagnostics (held in STASH section 26).
!
! Method:
!   Each diagnostic is simply copied into the STASHwork array
!   to be passed on to STASH for output processing.
!
!   Diagnostics currently available (in order calculated):
!   Item  Description
!    1    River water storage  (river grid)
!    2    gridbox outflow       (   "     )
!    3    gridbox runoff        (   "     )
!    4    gridbox riverflow     (ATMOS grid)
!    5    coastal outflow       (river grid)
!    6    inland basin outflow       (river grid)


USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE stash_array_mod, ONLY: sf, si, si_last
USE copydiag_mod, ONLY: copydiag

IMPLICIT NONE

! Arguments with Intent IN. ie: Input variables.

INTEGER ::                                                                     &
  row_length                                                                   &
                   ! number of points on a row
, rows                                                                         &
                   ! number of rows in a theta field
, river_row_length                                                             &
                    ! river row length
, river_rows        ! river rows

REAL ::                                                                        &
 riverout(row_length, rows)                                                    &
,riverout_rgrid(river_row_length, river_rows)                                  &
,box_outflow(river_row_length, river_rows)                                     &
,box_inflow(river_row_length, river_rows)                                      &
,twatstor(river_row_length, river_rows)                                        &

! Declare inland basin outflow variable
      ,inlandout_riv(river_row_length, river_rows)

! Local variables
INTEGER, PARAMETER :: Sect = 26  !  Section No for RR diagnostics

CHARACTER(LEN=*) :: RoutineName
PARAMETER ( RoutineName='DIAGNOSTICS_RIV')

INTEGER ::                                                                     &
  im_index        ! internal model index

! Diagnostic variables
REAL ::                                                                        &
 STASHwork( * )    ! STASH workspace

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

! ------------------------------------------------------------------
! Section 1.  Initialisation.
! ------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,                 &
                        zhook_handle)

im_index = 1

! --------------------------------------------------------------------
! River flow (on atmosphere grid)
! --------------------------------------------------------------------
! Item 26 004 riverout
IF (sf(004,sect)) THEN
  CALL copydiag(STASHwork(si(004,sect,im_index):si_last(004,sect,im_index))    &
       ,riverout,row_length,rows)
END IF

! --------------------------------------------------------------------
! Coastal outflow (at sea points on routing grid)
! --------------------------------------------------------------------
! Item 26 005 riverout_rgrid
IF (sf(005,sect)) THEN
  CALL copydiag(STASHwork(si(005,sect,im_index):si_last(005,sect,im_index))    &
       ,riverout_rgrid,river_row_length,river_rows)
END IF

! ----------------------------------------------------------------------
! River water storage
! -------------------------------------------------------------------
! Item 26 001
IF (sf(001,sect)) THEN
  CALL copydiag(STASHwork(si(001,sect,im_index):si_last(001,sect,im_index))    &
       ,twatstor,river_row_length,river_rows)
END IF

! --------------------------------------------------------------------
! River gridbox outflow (all routing grid boxes)
! --------------------------------------------------------------------
! Item 26 002 box_outflow
IF (sf(002,sect)) THEN
  CALL copydiag(STASHwork(si(002,sect,im_index):si_last(002,sect,im_index))    &
       ,box_outflow,river_row_length,river_rows)
END IF
!-------------------------------------------------------------------
! ------------------------------------------------------------------
! River gridbox inflow (all routing grid boxes)
! ------------------------------------------------------------------
! Item 26 003 box_inflow
IF (sf(003,sect)) THEN
  CALL copydiag(STASHwork(si(003,sect,im_index):si_last(003,sect,im_index))    &
       ,box_inflow,river_row_length,river_rows)
END IF
!---------------------------------------------------------------------

! Output inland basin outflow on TRIP grid

! ------------------------------------------------------------------
! Inland basin outflow
! ------------------------------------------------------------------
! Item 26 006 inlandout_riv
IF (sf(006,sect)) THEN
  CALL copydiag(STASHwork(si(006,sect,im_index):si_last(006,sect,im_index)),   &
   inlandout_riv,river_row_length,river_rows)
END IF
!---------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,                &
                        zhook_handle)
RETURN
END SUBROUTINE diagnostics_riv

END MODULE diagnostics_riv_mod
#endif
