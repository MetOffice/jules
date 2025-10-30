! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE sf_orog_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_OROG_MOD'

CONTAINS

!  SUBROUTINE SF_OROG------------------------------------------------
!
!  Purpose: Calculate roughness lengths, blending height and wind
!           profile factor
!
!--------------------------------------------------------------------
SUBROUTINE sf_orog(                                                            &
 land_pts,surft_pts,land_index,surft_index,                                    &
 ho2r2_orog,rib,sil_orog,z0m,z1,                                               &
 wind_profile_factor,z0m_eff                                                   &
 )

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE c_surf, ONLY: ri_crit
USE planet_constants_mod, ONLY: vkman
USE missing_data_mod, ONLY: rmdi
USE jules_surface_mod, ONLY: h_blend_max, fd_stability_dep, on, orog_drag_param
USE stochastic_physics_run_mod, ONLY: l_rp2, i_rp_scheme, i_rp2b, rp_idx,      &
                                      orog_drag_param_rp

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                      ! IN Number of land points.
,surft_pts                                                                     &
                      ! IN Number of tile points.
,land_index(land_pts)                                                          &
                      ! IN Index of land points.
,surft_index(land_pts)
                      ! IN Index of tile points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ho2r2_orog(land_pts)                                                          &
                      !IN Peak to trough height of unresolved
!                           !   orography divided by 2SQRT(2) (m).
,rib(land_pts)                                                                 &
                      ! IN Bulk Richardson number for lowest layer
,sil_orog(land_pts)                                                            &
                      ! IN Silhouette area of unresolved orography
!                           !    per unit horizontal area
,z0m(land_pts)                                                                 &
                      ! IN Roughness length for momentum (m).
,z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                     ! IN Height of lowest atmospheric level (m).

!  Output variables.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 wind_profile_factor(land_pts)                                                 &
!                           ! OUT For transforming effective surface
!                           !     transfer coefficients to those
!                           !     excluding form drag.
,z0m_eff(land_pts)    ! OUT Effective roughness length for
!                                 momentum (m)

!  Work Variables

INTEGER ::                                                                     &
 i,j                                                                           &
              ! Horizontal field index
,k                                                                             &
              ! Tile field index
,l            ! Land field index

REAL(KIND=real_jlslsm) ::                                                      &
 h_blend_orog                                                                  &
              ! Blending height
,rib_fn                                                                        &
              ! Interpolation coefficient for 0 < RIB < RI_CRIT
,zeta1                                                                         &
              ! Work space
,zeta2                                                                         &
              ! More work space
,zeta3        ! Even more work space

REAL(KIND=real_jlslsm), PARAMETER :: root2 = SQRT(2.0)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_OROG'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_rp2 .AND. i_rp_scheme == i_rp2b ) THEN
  orog_drag_param = orog_drag_param_rp(rp_idx)
END IF

! Set blending height, effective roughness length and
! wind profile factor at land points.

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,l,j,i,zeta1,zeta2,zeta3,rib_fn,h_blend_orog)                   &
!$OMP SHARED(surft_pts,surft_index,land_index,t_i_length,sil_orog,z1,z0m,      &
!$OMP        ho2r2_orog,fd_stability_dep,rib,z0m_eff,                          &
!$OMP        wind_profile_factor,orog_drag_param)
DO k = 1,surft_pts
  l = surft_index(k)
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length

  zeta1 = 0.5 * orog_drag_param * sil_orog(l)
  zeta2 = LOG ( 1.0 + z1(i,j) / z0m(l) )
  zeta3 = 1.0 / SQRT ( zeta1 / (vkman * vkman) + 1.0 / (zeta2 * zeta2) )
  zeta2 = 1.0 / EXP(zeta3)
  h_blend_orog = MAX ( z1(i,j) / (1.0 - zeta2) ,                               &
                     ho2r2_orog(l) * root2 )
  h_blend_orog = MIN ( h_blend_max, h_blend_orog )

  ! Apply simple stability correction to form drag if RIB is stable

  IF (sil_orog(l)  ==  rmdi) THEN
    zeta1 = 0.0
  ELSE
    IF (fd_stability_dep == on) THEN
      rib_fn =  ( 1.0 - rib(l) / ri_crit )
      IF (rib_fn >  1.0) rib_fn = 1.0
      IF (rib_fn <  0.0) rib_fn = 0.0
      zeta1 = 0.5 * orog_drag_param * sil_orog(l) * rib_fn
    ELSE
      zeta1 = 0.5 * orog_drag_param * sil_orog(l)
    END IF
  END IF

  z0m_eff(l) = h_blend_orog / EXP ( vkman / SQRT ( zeta1 +                     &
               (vkman / LOG ( h_blend_orog / z0m(l) ) ) *                      &
               (vkman / LOG ( h_blend_orog / z0m(l) ) ) ) )
  IF ( rib(l) >  ri_crit .AND.                                                 &
       fd_stability_dep == on )  z0m_eff(l) = z0m(l)

  wind_profile_factor(l) = LOG( h_blend_orog / z0m_eff(l) ) /                  &
                           LOG( h_blend_orog / z0m(l) )

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_orog
END MODULE sf_orog_mod
