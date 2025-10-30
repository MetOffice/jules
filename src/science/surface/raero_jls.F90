! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE raero_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RAERO_MOD'

CONTAINS

! Description:
! Routine to calculate the aerodynamic resistance

! *********************************************************************
SUBROUTINE raero (land_pts,land_index,veg_pts,veg_index                        &
,                 rib,wind,z0h,z0m,z1,ra)

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE planet_constants_mod, ONLY: karman=>vkman
USE jules_surface_mod, ONLY: ah,cz

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                            ! IN Total number of land points.
,land_index(land_pts)                                                          &
                            ! IN Index of land points on the
!                                 !    P-grid.
,veg_pts                                                                       &
                            ! IN Number of vegetated points.
,veg_index(land_pts)
                            ! IN Index of vegetated points
!                                 !    on the land grid.


REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 rib(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                            ! IN Bulk Richardson number.
,wind(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                            ! IN Windspeed (m/s).
,z0h(land_pts)                                                                 &
                            ! IN Roughness length for heat (m).
,z0m(land_pts)                                                                 &
                            ! IN Roughness length for momentum (m)
,z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                            ! IN Reference level (m).
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 ra(land_pts)
                            ! OUT Aerodynamic resistance (s/m).

INTEGER :: i,j,k,l          ! WORK Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
 bh                                                                            &
                            ! WORK Stability coefficient.
,chn                                                                           &
                            ! WORK Neutral drag coefficient.
,fh                                                                            &
                            ! WORK Stability factor.
,zetah,zetam                ! WORK Tempories in calculation of
!                                 !      CHN.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RAERO'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,l,j,i,zetam,zetah,chn,bh,fh)                                   &
!$OMP SHARED(veg_pts,veg_index,land_index,t_i_length,z1,z0m,z0h,rib,ra,wind)
DO k = 1,veg_pts
  l = veg_index(k)
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length

  !-----------------------------------------------------------------------
  ! Calculate the neutral bulk tranfer coefficient.
  !-----------------------------------------------------------------------
  zetam = LOG((z1(i,j) + z0m(l)) / z0m(l))
  zetah = LOG((z1(i,j) + z0m(l)) / z0h(l))
  chn = (karman * karman) / (zetah * zetam)

  !-----------------------------------------------------------------------
  ! Calculate the stability factor.
  !-----------------------------------------------------------------------
  bh = ah * chn * cz * SQRT (z1(i,j) / z0h(l))
  IF (rib(i,j)  >=  0.0) THEN
    fh = 1.0 / (1 + ah * rib(i,j))
  ELSE
    fh = 1 - ah * rib(i,j) / (1 + bh * SQRT(-rib(i,j)))
  END IF

  !-----------------------------------------------------------------------
  ! Calculate the aerodynamic resistance.
  !-----------------------------------------------------------------------
  ra(l) = 1.0 / (fh * chn * wind(i,j))
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE raero
END MODULE raero_mod
