! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sea_rough_int_mod

USE phi_m_h_mod, ONLY: phi_m_h

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SEA_ROUGH_INT_MOD'

CONTAINS
!   SUBROUTINE SEA_ROUGH_INT ------------------------------------------

!  Purpose: Calculate the roughness lengths at the sea surface if
!           this is being done interactively within the iteration
!           for the Obukhov length.


!  Documentation: UM Documentation Paper No 24

!--------------------------------------------------------------------
SUBROUTINE sea_rough_int (                                                     &
 points,surft_pts,surft_index,pts_index,                                       &
 charnock,charnock_w,v_s,recip_l_mo,                                           &
 z0m,z0h                                                                       &
)

USE planet_constants_mod, ONLY:                                                &
                    g, vkman

USE atm_fields_bounds_mod, ONLY: tdims

USE jules_sea_seaice_mod, ONLY:                                                &
                    iseasurfalg,                                               &
                    ip_ss_surf_div_int,                                        &
                    ip_ss_coare_mq,                                            &
                    ip_ss_surf_div_int_coupled,                                &
                    a_chrn_coare,                                              &
                    b_chrn_coare,                                              &
                    u10_min_coare,                                             &
                    u10_max_coare,                                             &
                    l_10m_neut,                                                &
                    z_10m,                                                     &
                    ip_hwdrag_limited,                                         &
                    ip_hwdrag_reduced_v1,                                      &
                    i_high_wind_drag,                                          &
                    cdn_max_sea,                                               &
                    cdn_hw_sea,                                                &
                    u_cdn_max,                                                 &
                    u_cdn_hw

USE theta_field_sizes, ONLY: t_i_length
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
 points                                                                        &
                      ! Number of points.
,surft_pts                                                                     &
                      ! Number of tile points.
,surft_index(points)                                                           &
                      ! Index of tile points.
,pts_index(points)    ! Index of sea points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  charnock                                                                     &
!                    ! Prescribed value of Charnock's coefficient
,charnock_w(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
!                    ! Charnock's coefficient from the wave model
,v_s(points)                                                                   &
                     ! Surface layer scaling velocity
,recip_l_mo(points)
!                    ! Reciprocal of the Obukhov length ! (m^-1).


REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 z0m(points)                                                                   &
               ! Roughness length for momentum transport (m).
,z0h(points)
               ! Roughness length for heat and moisture (m).

!----------------------------------------------------------------------

!  Local variables

INTEGER ::                                                                     &
  k,l ! Loop counter; horizontal field index.
INTEGER ::                                                                     &
  i,j

REAL(KIND=real_jlslsm) ::                                                      &
 charnock_int                                                                  &
              ! Interactive value of Charnock's coefficient
,m10                                                                           &
              ! Wind speed at 10m
,re_rough                                                                      &
              ! Roughness Reynolds number
,z0msea_max                                                                    &
              ! Maximum roughness length to limit the drag
,z0msea_hw                                                                     &
              ! Roughness length at very high wind speeds
,cdn_lim_loc                                                                   &
              ! Local limited neutral drag coefficient based on local neutral
              ! wind speed
,phi_m_10(points)                                                              &
              ! Value of Phi_m at 10m
,phi_h_10(points)
              ! Value of Phi_h at 10m

REAL(KIND=real_jlslsm), PARAMETER :: visc_air = 1.4e-5
              ! Kinematic viscosity of air

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SEA_ROUGH_INT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

SELECT CASE(i_high_wind_drag)
CASE (ip_hwdrag_limited)
  !   Limit the neutral drag coefficient at 10m by calculating the
  !   equivalent roughness length and capping the roughness length.
  !   The equivalent wind speed will depend on the value of Charnock's
  !   coefficient.
  z0msea_max = z_10m / ( EXP(vkman / SQRT(cdn_max_sea)) - 1.0)
CASE (ip_hwdrag_reduced_v1)
  !   Calculate the maximum roughness length and the high-wind limit
  !   to limit the drag coefficient.
  z0msea_max = z_10m / ( EXP(vkman / SQRT(cdn_max_sea)) - 1.0)
  z0msea_hw  = z_10m / ( EXP(vkman / SQRT(cdn_hw_sea)) - 1.0)
END SELECT


SELECT CASE (iseasurfalg)
  !
CASE (ip_ss_surf_div_int)
  !   Constant value of Charnock's coefficient and interactive
  !   calculation of z0h from surface divergence theory.
  CALL phi_m_h ( points,surft_pts,surft_index,pts_index,                       &
                 recip_l_mo,SPREAD(z_10m,1,points),                            &
                 SPREAD(z_10m,1,points),z0m,z0h,                               &
                 phi_m_10,phi_h_10)

!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) SCHEDULE(STATIC)             &
!$OMP PRIVATE(k, l, m10, cdn_lim_loc)                                          &
!$OMP SHARED(surft_pts, surft_index, z0m, z0h, charnock, v_s, phi_m_10, g,     &
!$OMP        i_high_wind_drag, z0msea_max, z0msea_hw, u_cdn_max, u_cdn_hw,     &
!$OMP        cdn_max_sea, cdn_hw_sea)
  DO k = 1,surft_pts
    l = surft_index(k)
    z0m(l) = charnock * v_s(l)**2 / g +                                        &
             1.54e-6 / (v_s(l) + 1.0e-5)

    m10 = (v_s(l) / vkman) * phi_m_10(l)

    SELECT CASE(i_high_wind_drag)
    CASE (ip_hwdrag_limited)
      z0m(l) = MIN(z0m(l), z0msea_max)
    CASE (ip_hwdrag_reduced_v1)
      IF (m10 <= u_cdn_max) THEN
        z0m(l) = MIN(z0m(l), z0msea_max)
      ELSE IF (m10 >= u_cdn_hw) THEN
        z0m(l) = z0msea_hw
      ELSE
        cdn_lim_loc = cdn_max_sea + (cdn_hw_sea - cdn_max_sea) *               &
          (m10 - u_cdn_max) / (u_cdn_hw - u_cdn_max)
        z0m(l)  = z_10m / ( EXP(vkman / SQRT(cdn_lim_loc)) - 1.0)
      END IF
    END SELECT
    z0h(l)   = MAX(7.0e-8, 2.56e-9 / z0m(l))
    IF (v_s(l) < 0.1) THEN
      z0h(l)   = MAX(z0h(l), 2.52e-6 / v_s(l))
    END IF
  END DO
!$OMP END PARALLEL DO
  !
CASE (ip_ss_coare_mq)
  !
  !   Recalculate Charnock's coefficient from the wind-speed at
  !   10 m. Either the true neutral wind may be used (preferred)
  !   or the stability-dependent 10m wind for cases when the
  !   distinction is ignored.
  IF ( .NOT. l_10m_neut) THEN
    CALL phi_m_h ( points,surft_pts,surft_index,pts_index,                     &
                   recip_l_mo,SPREAD(z_10m,1,points),                          &
                   SPREAD(z_10m,1,points),z0m,z0h,                             &
                   phi_m_10,phi_h_10)
  END IF
!$OMP PARALLEL DO IF(surft_pts > 1) DEFAULT(NONE) SCHEDULE(STATIC)             &
!$OMP PRIVATE(k, l, m10, charnock_int, re_rough, cdn_lim_loc)                  &
!$OMP SHARED(surft_pts, surft_index, l_10m_neut, v_s, z0m, phi_m_10, g,        &
!$OMP        a_chrn_coare, b_chrn_coare, u10_max_coare, u10_min_coare, z0h,    &
!$OMP        z0msea_max, z0msea_hw, i_high_wind_drag, u_cdn_max, u_cdn_hw,     &
!$OMP        cdn_max_sea, cdn_hw_sea)
  DO k = 1,surft_pts
    l = surft_index(k)
    IF (l_10m_neut) THEN
      m10 = (v_s(l) / vkman) * LOG(1.0 + z_10m / z0m(l))
    ELSE
      m10 = (v_s(l) / vkman) * phi_m_10(l)
    END IF
    charnock_int = b_chrn_coare + a_chrn_coare *                               &
      MAX(MIN(m10, u10_max_coare), u10_min_coare)
    z0m(l) = charnock_int * v_s(l)**2 / g +                                    &
             0.11 * visc_air / v_s(l)
    !     Impose a lower limit of the molecular mean free path.
    z0m(l) = MAX(z0m(l), 7.0e-8)
    SELECT CASE(i_high_wind_drag)
    CASE (ip_hwdrag_limited)
      z0m(l) = MIN(z0m(l), z0msea_max)
    CASE (ip_hwdrag_reduced_v1)
      IF (m10 <= u_cdn_max) THEN
        z0m(l) = MIN(z0m(l), z0msea_max)
      ELSE IF (m10 >= u_cdn_hw) THEN
        z0m(l) = z0msea_hw
      ELSE
        cdn_lim_loc = cdn_max_sea + (cdn_hw_sea - cdn_max_sea) *               &
          (m10 - u_cdn_max) / (u_cdn_hw - u_cdn_max)
        z0m(l)  = z_10m / ( EXP(vkman / SQRT(cdn_lim_loc)) - 1.0)
      END IF
    CASE DEFAULT
      ! Impose an upper limit of 0.015 m as used with the original
      ! implementation of the COARE algorithm, typically capping
      ! the drag in the range 30 - 40 ms-1. (In the aerodynamically
      ! smooth limit U10 would need to fall below
      ! 0.003 ms-1 to be limited  by this.
      z0m(l) = MIN(z0m(l), 0.015)
    END SELECT
    re_rough = v_s(l) * z0m(l) / visc_air
    !     Roughness lengths for heat and momentum set to the expression
    !     for z0q from COARE3.0 and 3.5, as latent heat should dominate.
    z0h(l)   = MIN(1.15e-4, 5.5e-5 / re_rough**0.6)
    !
    !
  END DO
!$OMP END PARALLEL DO
  !
CASE (ip_ss_surf_div_int_coupled)
  !   Wave value of charnock's cofficient and interactive
  !   calculation of z0h from surface divergence theory.
  DO k = 1,surft_pts
    l = surft_index(k)
    j=(pts_index(l) - 1) / t_i_length + 1
    i = pts_index(l) - (j-1) * t_i_length
    z0m(l) = charnock_w(i,j) * v_s(l) * v_s(l) / g +                           &
             1.54e-6 / (v_s(l) + 1.0e-5)
    z0h(l)   = MAX(7.0e-8, 2.59e-9 / z0m(l))
    IF (v_s(l) < 0.1) z0h(l)   = MAX(z0h(l), 2.52e-6 / v_s(l))
  END DO
  !
END SELECT

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sea_rough_int
END MODULE sea_rough_int_mod
