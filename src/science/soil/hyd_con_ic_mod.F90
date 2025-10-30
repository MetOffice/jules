! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!    SUBROUTINE HYD_CON_IC---------------------------------------------

! Description:
!     Intermediate control routine to choose between the two
!     versions of the soil dynamics
!      i.e. Brooks & Corey or van Genuchten

! Documentation : UM Documentation Paper 25

MODULE hyd_con_ic_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='HYD_CON_IC_MOD'

CONTAINS

SUBROUTINE hyd_con_ic( npnts, soil_pts, soil_index, b, ks, thetak,             &
                       k, dk_dthk )

!Use in relevant subroutines
USE hyd_con_vg_mod, ONLY: hyd_con_vg
USE hyd_con_ch_mod, ONLY: hyd_con_ch

!Use in relevant variables
USE jules_soil_mod, ONLY: l_vg_soil

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  npnts,                                                                       &
    ! Points in grid.
  soil_pts
    ! Number of soil points.

INTEGER, INTENT(IN) ::                                                         &
  soil_index(npnts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  b(npnts),                                                                    &
    ! Exponent in conductivity and soil water suction fits.
  ks(npnts),                                                                   &
    ! The saturated hydraulic conductivity (kg/m2/s).
  thetak(npnts)
    ! Fractional saturation.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  k(npnts),                                                                    &
    ! The hydraulic conductivity (kg/m2/s).
  dk_dthk(npnts)
    ! The rate of change of K with THETAK (kg/m2/s).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='HYD_CON_IC'

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_vg_soil ) THEN
  ! van Genuchten model
  CALL hyd_con_vg( npnts, soil_pts, soil_index, b, ks, thetak, k, dk_dthk )
ELSE
  ! Brooks & Corey model
  CALL hyd_con_ch( npnts, soil_pts, soil_index, b, ks, thetak, k, dk_dthk )
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE hyd_con_ic
END MODULE hyd_con_ic_mod
