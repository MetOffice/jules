!******************************COPYRIGHT****************************************
! (c) British Antarctic Survey, 2023.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE soil_htc_wtrac_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-------------------------------------------------------------------------------
! Description:
!    Update the water tracer unfrozen and frozen soil moisture layer
!    contents for melting and freezing.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Hydrology
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to JULES coding standards v1.
!------------------------------------------------------------------------------

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SOIL_HTC_WTRAC_MOD'

CONTAINS

SUBROUTINE soil_htc_wtrac (npnts, nshyd, soil_pts, i_wt, soil_index,           &
                           sthf, sthf_old, sthu_old, sthf_wtrac, sthu_wtrac)

USE jules_water_tracers_mod, ONLY: wtrac_calc_ratio_fn_jules

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  nshyd,                                                                       &
    ! Number of soil moisture levels.
  soil_pts,                                                                    &
    ! Number of soil points.
  i_wt
    ! Water tracer number

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sthf(npnts,nshyd),                                                           &
    ! Frozen soil moisture content of each layer as a fraction of
    ! saturation AFTER call to soil_htc
  sthf_old(npnts,nshyd),                                                       &
    ! Frozen soil moisture content of each layer as a fraction of
    ! saturation BEFORE call to soil_htc
  sthu_old(npnts,nshyd)
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation BEFORE call to soil_htc

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  sthf_wtrac(npnts,nshyd),                                                     &
    ! Frozen soil water tracer contents of each layer as a fraction of
    ! saturation
  sthu_wtrac(npnts,nshyd)
    ! Unfrozen soil water tracer content of each layer as a fraction of
    ! saturation

!-----------------------------------------------------------------------------
! Local scalars:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i, j, n
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  ratio_wt,                                                                    &
    ! Ratio of the water tracer to water used to scale phase change
  dw
    ! Amount of water changing phase (positive values indicate freezing)

!-----------------------------------------------------------------------------
! Local arrays:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  ratio_u(npnts,nshyd),                                                        &
    ! Ratio of unfrozen soil water tracer to water content
  ratio_f(npnts,nshyd)
    ! Ratio of frozen soil water tracer to water content

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SOIL_HTC_WTRAC'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!----------------------------------------------------------------------------
! Calculate the water tracer to water ratio of the unfrozen and frozen
! soil moisture prior to the call to soil_htc
!----------------------------------------------------------------------------

DO j = 1, soil_pts
  i = soil_index(j)
  DO n = 1,nshyd
    ratio_u(i,n) = wtrac_calc_ratio_fn_jules                                   &
                                  (i_wt,sthu_wtrac(i,n),sthu_old(i,n))
    ratio_f(i,n) = wtrac_calc_ratio_fn_jules                                   &
                                  (i_wt,sthf_wtrac(i,n),sthf_old(i,n))
  END DO
END DO

!----------------------------------------------------------------------------
! Update water tracers for melting/freezing
!----------------------------------------------------------------------------

DO j = 1, soil_pts
  i = soil_index(j)
  DO n = 1,nshyd
    ! Calculate amount of water changing phase
    dw = sthf(i,n) - sthf_old(i,n)

    IF (dw > 0.0) THEN
      ! Freezing - sthu is the source
      ratio_wt = ratio_u(i,n)
    ELSE
      ! Melting - sthf is the source
      ratio_wt = ratio_f(i,n)
    END IF

    ! Move water tracer between phases
    sthu_wtrac(i,n) = sthu_wtrac(i,n) - ratio_wt * dw
    sthf_wtrac(i,n) = sthf_wtrac(i,n) + ratio_wt * dw

  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE soil_htc_wtrac
END MODULE soil_htc_wtrac_mod
