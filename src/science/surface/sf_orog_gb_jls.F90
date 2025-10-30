! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE sf_orog_gb_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_OROG_GB_MOD'

CONTAINS

!  SUBROUTINE SF_OROG_GB --------------------------------------------
!
!  Purpose: Calculate effective roughness length and blending height
!
!--------------------------------------------------------------------
SUBROUTINE sf_orog_gb(                                                         &
 land_pts,land_index,                                                          &
 ho2r2_orog,rib,sil_orog,z0m,z1,                                               &
 h_blend_orog,z0m_eff,sf_diag,z0h,z0h_eff                                      &
)

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE c_surf, ONLY: ri_crit
USE planet_constants_mod, ONLY: vkman
USE missing_data_mod, ONLY: rmdi
USE jules_surface_mod, ONLY: h_blend_max, h_blend_min, fd_stability_dep, on,   &
    orog_drag_param
USE sf_diags_mod, ONLY: strnewsfdiag
USE stochastic_physics_run_mod, ONLY: l_rp2, i_rp_scheme, i_rp2b, rp_idx,      &
                                      orog_drag_param_rp
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                      ! IN Number of land points to be processed.
,land_index(land_pts)
                      ! IN Index of land points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ho2r2_orog(land_pts)                                                          &
                      !IN Peak to trough height of unresolved
!                           !   orography divided by 2SQRT(2) (m).
,rib(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                      ! IN GBM Bulk Richardson number for lowest
!                           !    layer
,sil_orog(land_pts)                                                            &
                      ! IN Silhouette area of unresolved orography
!                           !    per unit horizontal area
,z0m(land_pts)                                                                 &
                      ! IN GBM Roughness length for momentum (m).
,z0h(land_pts)                                                                 &
                      ! IN GBM Roughness length for heat (m)
,z1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                      ! IN Height of lowest atmospheric level (m).

!Diagnostics
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

!  Output variables.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 h_blend_orog(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)             &
!                           ! OUT Blending height
,z0m_eff(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
,z0h_eff(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                           ! OUT Effective roughness lengths for
!                                 momentum, and heat and moisture (m)

!  Work Variables

INTEGER ::                                                                     &
 i,j                                                                           &
              ! Horizontal field index
,l            ! Land field index

REAL(KIND=real_jlslsm) ::                                                      &
 rib_fn                                                                        &
              ! Interpolation coefficient for 0 < RIB < RI_CRIT
,zeta1                                                                         &
              ! Work space
,zeta2                                                                         &
,zeta3                                                                         &
,zeta4
              ! More work space

REAL(KIND=real_jlslsm), PARAMETER :: root2 = SQRT(2.0)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_OROG_GB'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( l_rp2 .AND. i_rp_scheme == i_rp2b ) THEN
  orog_drag_param = orog_drag_param_rp(rp_idx)
END IF

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,j,i,zeta1,zeta2,zeta3,zeta4,rib_fn)                            &
!$OMP SHARED(land_pts,land_index,t_i_length,h_blend_orog,z0m_eff,              &
!$OMP        z0m,orog_drag_param,sil_orog,z1,ho2r2_orog,                       &
!$OMP        fd_stability_dep,rib,sf_diag,z0h_eff,z0h)

! Set blending height, effective roughness length and
! wind profile factor at land points.

!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length

  h_blend_orog(i,j) = h_blend_min
  z0m_eff(i,j) = z0m(l)

  zeta1 = 0.5 * orog_drag_param * sil_orog(l)
  zeta2 = LOG ( 1.0 + z1(i,j) / z0m(l) )
  zeta3 = 1.0 / SQRT ( zeta1 / (vkman * vkman) + 1.0 / (zeta2 * zeta2) )
  zeta2 = 1.0 / EXP(zeta3)
  h_blend_orog(i,j) = MAX ( z1(i,j) / (1.0 - zeta2) ,                          &
                     ho2r2_orog(l) * root2 )
  h_blend_orog(i,j) = MIN ( h_blend_max, h_blend_orog(i,j) )

  ! Apply simple stability correction to form drag if RIB is stable

  IF (sil_orog(l)  ==  rmdi) THEN
    zeta1 = 0.0
  ELSE
    IF (fd_stability_dep == on) THEN
      rib_fn =  ( 1.0 - rib(i,j) / ri_crit )
      IF (rib_fn >  1.0) rib_fn = 1.0
      IF (rib_fn <  0.0) rib_fn = 0.0
      zeta1 = 0.5 * orog_drag_param * sil_orog(l) * rib_fn
    ELSE
      zeta1 = 0.5 * orog_drag_param * sil_orog(l)
    END IF
  END IF

  z0m_eff(i,j) = h_blend_orog(i,j) / EXP ( vkman / SQRT ( zeta1                &
           + (vkman / LOG ( h_blend_orog(i,j) / z0m(l) ) ) *                   &
             (vkman / LOG ( h_blend_orog(i,j) / z0m(l) ) ) ) )
  IF ( rib(i,j) >  ri_crit .AND.                                               &
       fd_stability_dep == on ) z0m_eff(i,j) = z0m(l)
END DO
!$OMP END DO NOWAIT

!     ! Calculate effective Z0H, if required

IF (sf_diag%l_z0h_eff_gb) THEN

!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length

    IF (sil_orog(l)  ==  rmdi) THEN
      zeta1 = 0.0
      zeta4 = 0.0
    ELSE
      IF (fd_stability_dep == on) THEN
        rib_fn =  ( 1.0 - rib(i,j) / ri_crit )
        IF (rib_fn >  1.0) rib_fn = 1.0
        IF (rib_fn <  0.0) rib_fn = 0.0
      ELSE
        rib_fn = 1.0
      END IF
      zeta1 = 0.5 * orog_drag_param * sil_orog(l) * rib_fn
      zeta4 = SQRT(1.0 + zeta1 * LOG( h_blend_orog(i,j) / z0m(l) )             &
                              *LOG( h_blend_orog(i,j) / z0m(l) )               &
                              /(vkman * vkman) )
      !           ! If the Hewer and Wood (1998) modification of scalar flux
      !           ! parametrization were included:
      !             ZETA4 = ZETA4 * ( 1.0 - 2.2*RIB_FN*SIL_OROG(L) )
    END IF

    !         ! Rearranging Eq 148 of UM6.3 documentation gives:
    z0h_eff(i,j) = h_blend_orog(i,j) /                                         &
           EXP ( zeta4 * LOG( h_blend_orog(i,j) / z0h(l) ) )
    IF ( rib(i,j) >  ri_crit .AND.                                             &
         fd_stability_dep == on ) z0h_eff(i,j) = z0h(l)

  END DO
!$OMP END DO NOWAIT
END IF
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_orog_gb
END MODULE sf_orog_gb_mod
