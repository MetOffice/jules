! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE stdev1_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='STDEV1_MOD'

CONTAINS

!  SUBROUTINE STDEV1 ------------------------------------------------
!
!  Purpose: Calculate the standard deviations of layer 1 turbulent
!           fluctuations of temperature and humidity using approximate
!           formulae from first order closure.
!
!  -------------------------------------------------------------------

!  Subroutine interface

SUBROUTINE stdev1 (                                                            &
 points,surft_pts,pts_index,surft_index,fld_sea,                               &
 bq_1,bt_1,fqw_1,ftl_1,rhokm_1,rhostar,vshr,z0m,z1_tq,tile_frac,               &
 q1_sd,t1_sd                                                                   &
 )

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE planet_constants_mod, ONLY: g

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER ::                                                                     &
 points                                                                        &
                       ! IN Total number of points.
,surft_pts                                                                     &
                       ! IN Number of tile points.
,pts_index(points)                                                             &
                       ! IN Index of points.
,surft_index(points)  ! IN Index of tile points.

REAL(KIND=real_jlslsm) ::                                                      &
 fld_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                       ! IN Fraction of land or sea
,bq_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN Buoyancy parameter.
,bt_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN Buoyancy parameter.
,fqw_1(points)                                                                 &
                       ! IN Surface flux of QW.
,ftl_1(points)                                                                 &
                       ! IN Surface flux of TL.
,rhokm_1(points)                                                               &
                       ! IN Surface momentum exchange coefficient.
,rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                         ! IN Surface air density.
,vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! IN Magnitude of surface-to-lowest-level
!                            !    wind shear.
,z0m(points)                                                                   &
                       ! IN Roughness length for momentum.
,z1_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! IN Height of lowest tq level.
,tile_frac(points)   ! IN Tile fraction.

REAL(KIND=real_jlslsm) ::                                                      &
 q1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! INOUT Standard deviation of turbulent
!                            !       fluctuations of surface layer
!                            !       specific humidity (kg/kg).
,t1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                       ! INOUT Standard deviation of turbulent
!                            !       fluctuations of surface layer
!                            !       temperature (K).

!  Workspace --------------------------------------------------------
INTEGER ::                                                                     &
 i,j                                                                           &
                       ! Horizontal field index.
,k                                                                             &
                       ! Tile index.
,l                     ! Points field index.
REAL(KIND=real_jlslsm) ::                                                      &
 vs                                                                            &
                       ! Surface layer friction velocity
,vsf1_cubed                                                                    &
                       ! Cube of surface layer free convective
!                            ! scaling velocity
,ws1                   ! Turbulent velocity scale for surface
!                            ! layer

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='STDEV1'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,j,i,l,vs,vsf1_cubed,ws1)                                       &
!$OMP SHARED(surft_pts,surft_index,pts_index,t_i_length,rhokm_1,rhostar,vshr,  &
!$OMP        z1_tq,z0m,bt_1,ftl_1,bq_1,fqw_1,t1_sd,fld_sea,tile_frac,q1_sd,g)
DO k = 1,surft_pts
  l = surft_index(k)
  j=(pts_index(l) - 1) / t_i_length + 1
  i = pts_index(l) - (j-1) * t_i_length

  vs = SQRT ( rhokm_1(l) / rhostar(i,j) * vshr(i,j) )
  vsf1_cubed = 1.25 * g * (z1_tq(i,j) + z0m(l)) *                              &
             ( bt_1(i,j) * ftl_1(l) + bq_1(i,j) * fqw_1(l) ) /                 &
                 rhostar(i,j)
  IF ( vsf1_cubed  >   0.0 ) THEN
    ws1 = ( vsf1_cubed + vs * vs * vs )** (1.0 / 3.0)
    t1_sd(i,j) = t1_sd(i,j) + MAX ( 0.0 ,                                      &
                 fld_sea(i,j) * tile_frac(l) * 1.93 * ftl_1(l) /               &
                                        (rhostar(i,j) * ws1) )
    q1_sd(i,j) = q1_sd(i,j) + MAX ( 0.0 ,                                      &
                 fld_sea(i,j) * tile_frac(l) * 1.93 * fqw_1(l) /               &
                                        (rhostar(i,j) * ws1) )
  END IF
END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE stdev1
END MODULE stdev1_mod
