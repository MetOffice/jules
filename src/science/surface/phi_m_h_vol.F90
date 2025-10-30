! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE phi_m_h_vol_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PHI_M_H_VOL_MOD'

CONTAINS
!   SUBROUTINE PHI_M_H_VOL --------------------------------------------

!  Purpose: Calculate the integrated froms of the Monin-Obukhov
!           stability functions for surface exchanges.

!  Documentation: UM Documentation Paper No 24.

!--- Arguments:---------------------------------------------------------
SUBROUTINE phi_m_h_vol (                                                       &
 points,surft_pts,surft_index,pts_index,                                       &
 recip_l_mo,z_uv,z_tq,z0m,z0h,phi_m,phi_h                                      &
)

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE jules_surface_mod, ONLY: a,b,c,d,c_over_d

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER ::                                                                     &
 points                                                                        &
                      ! IN Number of points.
,surft_pts                                                                     &
                      ! IN Number of tile points.
,surft_index(points)                                                           &
                      ! IN Index of tile points.
,pts_index(points)    ! IN Index of points.

REAL(KIND=real_jlslsm) ::                                                      &
 recip_l_mo(points)                                                            &
!                       ! IN Reciprocal of the Monin-Obukhov length
!                       !     (m^-1).
,z_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
!                       ! IN Height of top of wind level above
!                       !    roughness height (m)
,z_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
!                       ! IN Height of top of temperature,
!                       !     moisture and scalar level above
!                       !     the roughness height (m).
,z0m(points)                                                                   &
                  ! IN Roughness length for momentum (m).
,z0h(points)      ! IN Roughness length for heat/moisture/scalars
!                       !    (m)

REAL(KIND=real_jlslsm) ::                                                      &
 phi_m(points)                                                                 &
                  ! OUT Stability function for momentum.
,phi_h(points)
                  ! OUT Stability function for
!                 !     heat/moisture/scalars.

!    Workspace usage----------------------------------------------------
!    No work areas are required.

!  Define local variables.

INTEGER :: i,j,k,l   ! Loop counter; horizontal field index.

REAL(KIND=real_jlslsm) ::                                                      &
 phi_mn                                                                        &
                ! Neutral value of stability function for momentum
,phi_hn                                                                        &
                ! Neutral value of stability function for scalars.
,zeta_uv                                                                       &
                ! Temporary in calculation of PHI_M.
,zeta_0m                                                                       &
                ! Temporary in calculation of PHI_M.
,zeta_tq                                                                       &
                ! Temporary in calculation of PHI_H.
,zeta_0h                                                                       &
                ! Temporary in calculation of PHI_H.
,x_uv_sq                                                                       &
                ! Temporary in calculation of PHI_M.
,x_0m_sq                                                                       &
                ! Temporary in calculation of PHI_M.
,x_uv                                                                          &
                ! Temporary in calculation of PHI_M.
,x_0m                                                                          &
                ! Temporary in calculation of PHI_M.
,y_tq                                                                          &
                ! Temporary in calculation of PHI_H.
,y_0h                                                                          &
                ! Temporary in calculation of PHI_H.
,phi_h_fz1                                                                     &
                ! Temporary in calculation of PHI_H.
,phi_h_fz0                                                                     &
                ! Temporary in calculation of PHI_H.
,phi_tmp_0                                                                     &
                ! Temporary in calculation of PHI_M/H
,phi_tmp_1
                ! Temporary in calculation of PHI_M/H

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PHI_M_H_VOL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!CDIR NODEP
!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC) IF(surft_pts > 1)             &
!$OMP PRIVATE(i, j, k, l, phi_mn, phi_hn, zeta_uv, zeta_tq, zeta_0m,           &
!$OMP zeta_0h, phi_h_fz1, phi_h_fz0, phi_tmp_0, phi_tmp_1,                     &
!$OMP x_uv_sq, x_0m_sq, x_uv, x_0m, y_tq, y_0h)                                &
!$OMP SHARED(surft_pts, surft_index, pts_index, t_i_length, z_uv, z0m, z_tq,   &
!$OMP        z0h, recip_l_mo, phi_m, phi_h)
DO k = 1,surft_pts
  l = surft_index(k)
  j=(pts_index(l) - 1) / t_i_length + 1
  i = pts_index(l) - (j-1) * t_i_length

  !-----------------------------------------------------------------------
  ! 1. Calculate neutral values of PHI_M and PHI_H.
  !-----------------------------------------------------------------------

  phi_mn = LOG( (z_uv(i,j) + z0m(l)) / z0m(l) )
  phi_hn = LOG( (z_tq(i,j) + z0m(l)) / z0h(l) )

  !-----------------------------------------------------------------------
  ! 2. Calculate stability parameters.
  !-----------------------------------------------------------------------

  zeta_uv = (z_uv(i,j) + z0m(l)) * recip_l_mo(l)
  zeta_tq = (z_tq(i,j) + z0m(l)) * recip_l_mo(l)
  zeta_0m = z0m(l) * recip_l_mo(l)
  zeta_0h = z0h(l) * recip_l_mo(l)

  !-----------------------------------------------------------------------
  ! 3. Calculate PHI_M and PHI_H for neutral and stable conditions.
  !    Formulation of Beljaars and Holtslag (1991).
  !-----------------------------------------------------------------------

  IF (recip_l_mo(l)  >=  0.0) THEN
    phi_m(l) = phi_mn                                                          &
               + a * (zeta_uv - zeta_0m)                                       &
               + b * ( (zeta_uv - c_over_d) * EXP(-d * zeta_uv)                &
                      -(zeta_0m - c_over_d) * EXP(-d * zeta_0m) )
    phi_h_fz1 = SQRT(1.0 + (2.0 / 3.0) * a * zeta_tq)
    phi_h_fz0 = SQRT(1.0 + (2.0 / 3.0) * a * zeta_0h)
    phi_h(l) = phi_hn +                                                        &
                 phi_h_fz1 * phi_h_fz1 * phi_h_fz1                             &
               - phi_h_fz0 * phi_h_fz0 * phi_h_fz0                             &
               + b * ( (zeta_tq - c_over_d) * EXP(-d * zeta_tq)                &
                      -(zeta_0h - c_over_d) * EXP(-d * zeta_0h) )

    !   Adjust to make phi_m a mean across the layer:
    phi_m(l) = (1.0 + z0m(l) / z_uv(i,j)) * phi_m(l)
    phi_h(l) = (1.0 + z0h(l) / z_tq(i,j)) * phi_h(l)
    !
    IF (recip_l_mo(l) > 1.0e-06) THEN
      !     Away from neutrality
      phi_tmp_0 = zeta_0m + 0.5 * a * zeta_0m**2                               &
        + (b/d) * EXP(-d * zeta_0m) * ( d * zeta_0m**2                         &
        + (1.0 - c) * ( zeta_0m + 1.0 / d ) )
      phi_tmp_1 = zeta_uv + 0.5 * a * zeta_uv**2                               &
        + (b/d) * EXP(-d * zeta_uv) * ( d * zeta_uv**2                         &
        + (1.0 - c) * ( zeta_uv + 1.0 / d ) )
      phi_m(l) = phi_m(l)                                                      &
        - ( phi_tmp_1 - phi_tmp_0 ) / (zeta_uv - zeta_0m)
      !
      phi_tmp_0 = zeta_0h - (0.6 / a) * (1.0 - a  * zeta_0h)                   &
        * (SQRT(1.0 + (2.0 / 3.0) * a * zeta_0h))**3                           &
        + (b/d) * EXP(-d * zeta_0h) * ( d * zeta_0h**2                         &
        + (1.0 - c) * ( zeta_0h + 1.0 / d ) )
      phi_tmp_1 = zeta_tq - (0.6 / a) * (1.0 - a  * zeta_tq)                   &
        * (SQRT(1.0 + (2.0 / 3.0) * a * zeta_tq))**3                           &
        + (b/d) * EXP(-d * zeta_tq) * ( d * zeta_tq**2                         &
        + (1.0 - c) * ( zeta_tq + 1.0 / d ) )
      phi_h(l) = phi_h(l)                                                      &
        - ( phi_tmp_1 - phi_tmp_0 ) / (zeta_tq - zeta_0h)
      !
    ELSE

      !     Close to neutrality
      phi_m(l) = phi_m(l) - 1.0
      phi_h(l) = phi_h(l) - 1.0

    END IF

    !-----------------------------------------------------------------------
    ! 4. Calculate PHI_M and PHI_H for unstable conditions.
    !-----------------------------------------------------------------------

  ELSE

    x_uv_sq = SQRT(1.0-16.0 * zeta_uv)
    x_0m_sq = SQRT(1.0-16.0 * zeta_0m)
    x_uv = SQRT(x_uv_sq)
    x_0m = SQRT(x_0m_sq)
    phi_m(l) = phi_mn - 2.0 * LOG( (1.0 + x_uv) / (1.0 + x_0m) )               &
                    - LOG( (1.0 + x_uv_sq) / (1.0 + x_0m_sq) )                 &
                    + 2.0 * ( ATAN(x_uv) - ATAN(x_0m) )

    y_tq = SQRT(1.0-16.0 * zeta_tq)
    y_0h = SQRT(1.0-16.0 * zeta_0h)
    phi_h(l) = phi_hn - 2.0 * LOG( (1.0 + y_tq) / (1.0 + y_0h) )
    !
    !   Adjust to make this a mean value.
    phi_m(l) = (1.0 + z0m(l) / z_uv(i,j)) * phi_m(l)
    phi_h(l) = (1.0 + z0h(l) / z_tq(i,j)) * phi_h(l)
    !
    IF (recip_l_mo(l) < -1.0e-06) THEN
      phi_m(l) = phi_m(l) + (1.0 / 12.0)                                       &
        * (x_uv**3 - x_0m**3) / (zeta_uv - zeta_0m)
      phi_h(l) = phi_h(l) + 0.125 * (y_tq - y_0h)                              &
        / (zeta_tq - zeta_0h)
    ELSE
      !     Close to neutrality
      phi_m(l) = phi_m(l) - 1.0
      phi_h(l) = phi_h(l) - 1.0
      !
    END IF

  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE phi_m_h_vol
END MODULE phi_m_h_vol_mod
