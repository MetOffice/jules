! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE DARCY_IC-----------------------------------------------

! Description:
!     Intermediate control routine to choose between the two
!     versions of the soil dynamics
!      i.e. Brooks & Corey or van Genuchten

! Documentation : UM Documentation Paper 25

MODULE darcy_ic_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DARCY_IC_MOD'

CONTAINS

SUBROUTINE darcy_ic( npnts, soil_pts, dz1, dz2, soil_index, b, ks, sathh,      &
                     sthu1, sthu2, wflux,                                      &
                     dwflux_dsthu1, dwflux_dsthu2)

! Use in relevant subroutines
USE darcy_ch_mod, ONLY: darcy_ch
USE darcy_vg_mod, ONLY: darcy_vg

USE jules_soil_mod, ONLY: l_vg_soil

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Number of gridpoints.
  soil_pts
    ! Number of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  dz1,                                                                         &
    ! Thickness of the upper layer (m).
  dz2
    ! Thickness of the lower layer (m).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(npnts,2),                                                                  &
    ! Brooks & Corey exponent.
  ks(npnts),                                                                   &
    ! Saturated hydraulic conductivity (kg/m2/s).
  sathh(npnts,2),                                                              &
    ! Saturated soil water pressure (m).
  sthu1(npnts),                                                                &
   ! Unfrozen soil moisture content of upper layer as a fraction of
   ! saturation.
  sthu2(npnts)
   ! Unfrozen soil moisture content of lower layer as a fraction of
   ! saturation.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  wflux(npnts),                                                                &
   ! The flux of water between layers (kg/m2/s).
  dwflux_dsthu1(npnts),                                                        &
   ! The rate of change of the explicit flux with STHU1 (kg/m2/s).
  dwflux_dsthu2(npnts)
   ! The rate of change of the explicit flux with STHU2 (kg/m2/s).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DARCY_IC'

!-----------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_vg_soil ) THEN
  ! van Genuchten model
  CALL darcy_vg ( npnts, soil_pts, dz1, dz2, soil_index, b, ks, sathh,         &
                  sthu1, sthu2, wflux,                                         &
                  dwflux_dsthu1, dwflux_dsthu2 )
ELSE
  ! Brooks & Corey model
  CALL darcy_ch ( npnts, soil_pts, dz1, dz2, soil_index, b, ks, sathh,         &
                  sthu1, sthu2, wflux,                                         &
                  dwflux_dsthu1, dwflux_dsthu2 )
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE darcy_ic

END MODULE darcy_ic_mod
