! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE dpm_rpm_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DPM_RPM_MOD'

CONTAINS

!----------------------------------------------------------------------------
! Subroutine DPM_RPM --------------------------------------------------------
!
! Purpose : Calculates the DPM_RPM ratio of litter input for use
!            in the 4-pool soil carbon sub-model
!
! ---------------------------------------------------------------------------
SUBROUTINE dpm_rpm(land_pts, trif_pts, trif_index, lit_c, dpm_ratio)

USE jules_surface_types_mod, ONLY: npft

USE trif, ONLY: dpm_rpm_ratio

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts
    ! Number of points on which TRIFFID may operate.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  lit_c(land_pts,npft)
    ! PFT carbon litter (kg C/m2/360days).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dpm_ratio(land_pts)   ! Gridbox mean DPM ratio of litter input.

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  n,l,t          ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  total_litter   ! Gridbox total lit_c (kg C/m2/360days).

REAL(KIND=real_jlslsm) ::                                                      &
  dpm(land_pts)  ! Litter input to DPM (kg C/m2/360days).

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='DPM_RPM'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! calculate DPM litter input and hence ratio
DO t = 1,trif_pts
  l = trif_index(t)
  dpm(l)       = 0.0
  total_litter = 0.0

  DO n = 1,npft
    dpm(l)       = dpm(l) + lit_c(l,n) * dpm_rpm_ratio(n)                      &
                            / (1.0 + dpm_rpm_ratio(n))
    total_litter = total_litter + lit_c(l,n)
  END DO

  dpm_ratio(l) = dpm(l) / total_litter
  dpm_ratio(l) = MAX(0.0, dpm_ratio(l))
  dpm_ratio(l) = MIN(1.0, dpm_ratio(l))

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE dpm_rpm

END MODULE dpm_rpm_mod
