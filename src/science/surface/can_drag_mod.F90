! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE can_drag_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates roughness length and integrated forms of stability functions
!   over the vegetation canopy. This scheme is based on the model developed by
!
!   Harman and Finnigan (2007) Boundary-Layer. Meteorol., 123, 339-363, and
!   Harman and Finnigan (2008) Boundary-Layer. Meteorol., 129, 323-351.
!
! Other elements of the scheme are taken from
!
!   Massman (2007) Boundary-Layer. Meteorol., 83, 407-421, and
!   Massman (2017) Can J. For. Res., 47, 594-603.
!
! Code Owner: Please refer to ModuleLeaders.txt
!   This file belongs in the SURFACE section of JULES.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

PRIVATE

! Module constants
REAL(KIND=real_jlslsm), PARAMETER :: ps_leaf = 1.0
  ! Parameter for sheltering effect (Thom, 1971; QJRMS)
REAL(KIND=real_jlslsm), PARAMETER :: lcrl_max = 2.0
REAL(KIND=real_jlslsm), PARAMETER :: lcrl_min = -2.0
  ! Upper and lower limits of the ratio of adjustment length scale to
  ! MO length.
REAL(KIND=real_jlslsm), PARAMETER :: lai_min = 1.0
  ! Lower limit of plant area index

REAL(KIND=real_jlslsm)                                                         &
    , PARAMETER :: tol_neutrality = SQRT(EPSILON(tol_neutrality))
  ! Tolerance used to check for closeness to neutrality.
  ! Set to balance errors in neglecting the stability correction
  ! very close to neutrality with numerical errors arising in
  ! the full expression.
REAL(KIND=real_jlslsm), PARAMETER :: tol_rsl_sim_converge = 1.0e-07
  ! Tolerance for convergence of the iteration for the RSL
  ! similarity function
REAL(KIND=real_jlslsm), PARAMETER :: tol_rsl_converge = 1.0e-03
  ! General tolerance for convergence of the RSL iteration.

REAL(KIND=real_jlslsm), PARAMETER :: min_den_c2mh = 0.02
  ! Imposed lower bound on the denominator in the equations for
  ! c2m and c2h.

! Module variables for numerical integration
INTEGER, PARAMETER :: nn = 20
  ! A number of partitions for numerical integration
REAL(KIND=real_jlslsm), PARAMETER :: dn = 0.2
  ! An interval for numerical integration

LOGICAL :: l_hyperbolic_done = .FALSE.
  ! Logical set to .TRUE. to indicate when hyperbolic arrays have been
  ! precalculated.

REAL(KIND=real_jlslsm) ::                                                      &
  sinh_ndn(-nn:nn),                                                            &
    ! SINH(dn * REAL(n))
  cosh_ndn(-nn:nn),                                                            &
    ! COSH(dn * REAL(n))
  exp2_sinh_ndn(-nn:nn),                                                       &
    ! EXP( (pi/2) * SINH(dn * REAL(n)) )
  coshp_sinh_ndn(-nn:nn),                                                      &
    ! COSH( (pi/2) * SINH(dn * REAL(n)) )
  tanhp_sinh_ndn(-nn:nn)
    ! TANH( (pi/2) * SINH(dn * REAL(n)) )

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CAN_DRAG_MOD'

PUBLIC :: can_drag_z0, can_drag_phi_m_h, can_drag_phi_m_h_vol

CONTAINS

SUBROUTINE can_drag_z0(points, surft_pts, surft_index,                         &
                       recip_l_mo, canht, lai,                                 &
                       z0m, z0h, zdt)

USE jules_vegetation_mod, ONLY: l_rsl_scalar
USE conversions_mod, ONLY: pi

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates roughness length over the vegetation canopy.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
INTEGER, INTENT(IN) ::                                                         &
  points,                                                                      &
    ! Number of points.
  surft_pts,                                                                   &
    ! Number of tile points.
  surft_index(points)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  recip_l_mo(points),                                                          &
    ! Reciprocal of the Monin-Obukhov length
    ! (m^-1).
  canht(points),                                                               &
    ! Canopy height (m)
  lai(points)
    ! Leaf area index

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  z0m(points),                                                                 &
    ! Roughness length for momentum (m).
  z0h(points)
    ! Roughness length for heat/moisture/scalars (m)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  zdt(points)
    ! Difference between the canopy height and displacement height (m).

! Local variables
INTEGER ::                                                                     &
  k,l,                                                                         &
    ! Loop counter; horizontal field index.
  n
    ! Loop counter; numerical integration

REAL(KIND=real_jlslsm) ::                                                      &
  usuh,                                                                        &
    ! The ratio of friction velocity to wind velocity at the canopy top.
  l_c,                                                                         &
    ! Adjustment length scale of canopy (m)
  l_vor,                                                                       &
    ! Vorticity thickness (m).
  recip_l_mo_limit,                                                            &
    ! Reciprocal of the Monin-Obukhov length
    !  limited to solve non-linear equations satisfactorily
  sc,                                                                          &
    ! Turbulent Schmidt number
  z0h_rsl
    ! RSL corrected heat roughness length (m)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CAN_DRAG_Z0'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( .NOT. l_hyperbolic_done ) THEN
  ! Precalculation of hyperbolic functions using the addition formulae
  ! recursively. Note that this subroutine is called from pft_sparm and
  ! is therefore executed during initialization and before any routines
  ! that require these arrays are executed.
  DO n = -nn, nn
    cosh_ndn(n)        = COSH(dn * REAL(n))
    sinh_ndn(n)        = SINH(dn * REAL(n))
    exp2_sinh_ndn(n)   = EXP( (0.5 * pi) * sinh_ndn(n) )
    coshp_sinh_ndn(n)  = COSH( (0.5 * pi) * sinh_ndn(n) )
    tanhp_sinh_ndn(n)  = TANH( (0.5 * pi) * sinh_ndn(n) )
  END DO
  ! Mark hyperbolic arrays as calculated.
  l_hyperbolic_done = .TRUE.
END IF

!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                               &
!$OMP  PRIVATE(k,l,l_c,recip_l_mo_limit,sc,usuh,l_vor,z0h_rsl)                 &
!$OMP  SHARED(surft_pts,surft_index,                                           &
!$OMP         lai,canht,recip_l_mo,l_rsl_scalar,z0m,z0h,zdt) IF(surft_pts > 1)
DO k = 1,surft_pts
  l = surft_index(k)

  CALL calc_rsl_parameter(canht(l), lai(l), recip_l_mo(l),                     &
                          usuh, l_c, recip_l_mo_limit, sc, zdt(l), l_vor)

  ! Roughness length (including RSL effect and stability variation)
  CALL calc_z0_m_h_rsl(zdt(l), recip_l_mo_limit, l_vor, usuh, sc,              &
                       z0m(l), z0h_rsl)
  IF (l_rsl_scalar) THEN
    z0h(l) = z0h_rsl
  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE can_drag_z0

SUBROUTINE can_drag_phi_m_h(points, surft_pts, surft_index, pts_index,         &
                            recip_l_mo, z_uv, z_tq, canht, lai, z0m, z0h,      &
                            phi_m, phi_h)

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE jules_surface_mod, ONLY: a,b,d,c_over_d
USE jules_vegetation_mod, ONLY: l_rsl_scalar

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates integrated forms of stability functions over the vegetation
!   canopy.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
INTEGER, INTENT(IN) ::                                                         &
  points,                                                                      &
    ! Number of points.
  surft_pts,                                                                   &
    ! Number of tile points.
  surft_index(points),                                                         &
    ! Index of tile points.
  pts_index(points)
    ! Index of points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  recip_l_mo(points),                                                          &
    ! Reciprocal of the Monin-Obukhov length
    ! (m^-1).
  z_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                   &
    ! Height of wind level above the canopy height (m)
  z_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                   &
    ! Height of temperature, moisture and scalar
    ! level above the canopy top (m).
  canht(points),                                                               &
    ! Canopy height (m)
  lai(points),                                                                 &
    ! Leaf area index
  z0m(points),                                                                 &
    ! Roughness length for momentum (m).
  z0h(points)
    ! Roughness length for heat/moisture/scalars (m)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  phi_m(points),                                                               &
    ! Integrated stability function for momentum.
  phi_h(points)
    ! Integrated stability function for heat/moisture/scalars.

  ! Local variables
INTEGER ::                                                                     &
  i,j,k,l
    ! Loop counter; horizontal field index.

REAL(KIND=real_jlslsm) ::                                                      &
  usuh,                                                                        &
    ! The ratio of friction velocity to wind velocity at the canopy top.
  l_c,                                                                         &
    ! Adjustment length scale of canopy (m)
  recip_l_mo_limit,                                                            &
    ! Reciprocal of the Monin-Obukhov length
    !  limited to solve non-linear equations satisfactorily
  sc,                                                                          &
    ! Turbulent Schmidt number
  zdt,                                                                         &
    ! Difference between the canopy height and displacement height (m).
  l_vor,                                                                       &
    ! Vorticity thickness (m).
  psi_m_rsl,                                                                   &
    ! Integrated RSL function for momentum.
  psi_h_rsl,                                                                   &
    ! Integrated RSL function for heat/moisture/scalars.
  phi_mn,                                                                      &
    ! Neutral value of stability function for momentum
  phi_hn,                                                                      &
    ! Neutral value of stability function for scalars.
  zeta_uv,                                                                     &
    ! Temporary in calculation of PHI_M.
  zeta_0m,                                                                     &
    ! Temporary in calculation of PHI_M.
  zeta_tq,                                                                     &
    ! Temporary in calculation of PHI_H.
  zeta_0h,                                                                     &
    ! Temporary in calculation of PHI_H.
  x_uv_sq,                                                                     &
    ! Temporary in calculation of PHI_M.
  x_0m_sq,                                                                     &
    ! Temporary in calculation of PHI_M.
  x_uv,                                                                        &
    ! Temporary in calculation of PHI_M.
  x_0m,                                                                        &
    ! Temporary in calculation of PHI_M.
  y_tq,                                                                        &
    ! Temporary in calculation of PHI_H.
  y_0h,                                                                        &
    ! Temporary in calculation of PHI_H.
  phi_h_fz1,                                                                   &
    ! Temporary in calculation of PHI_H.
  phi_h_fz0
    ! Temporary in calculation of PHI_H.


! Local constants
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CAN_DRAG_PHI_M_H'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                               &
!$OMP  PRIVATE(k,l,j,i,l_c,recip_l_mo_limit,sc,usuh,zdt,l_vor,psi_m_rsl,       &
!$OMP          psi_h_rsl,phi_mn,phi_hn,zeta_uv,zeta_tq,zeta_0m,zeta_0h,        &
!$OMP          phi_h_fz1,phi_h_fz0,x_uv_sq,x_0m_sq,x_uv,x_0m,y_tq,y_0h)        &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,z_uv,z_tq,            &
!$OMP         lai,canht,recip_l_mo,l_rsl_scalar,z0m,z0h,phi_m,phi_h)           &
!$OMP  IF(surft_pts > 1)
DO k = 1,surft_pts
  l = surft_index(k)
  j = (pts_index(l) - 1) / t_i_length + 1
  i = pts_index(l) - (j-1) * t_i_length

  CALL calc_rsl_parameter(canht(l), lai(l), recip_l_mo(l),                     &
                          usuh, l_c, recip_l_mo_limit, sc, zdt, l_vor)

  ! Integrated RSL functions
  CALL calc_psi_m_h_rsl(z_uv(i,j), z_tq(i,j),                                  &
                        zdt, recip_l_mo_limit, l_vor, usuh, sc,                &
                        psi_m_rsl, psi_h_rsl)

  !-----------------------------------------------------------------------
  !! 1. Calculate neutral values of PHI_M and PHI_H.
  !-----------------------------------------------------------------------

  phi_mn = LOG( (z_uv(i,j) + zdt) / z0m(l) )
  phi_hn = LOG( (z_tq(i,j) + zdt) / z0h(l) )

  !-----------------------------------------------------------------------
  !! 2. Calculate stability parameters.
  !-----------------------------------------------------------------------

  zeta_uv = (z_uv(i,j) + zdt) * recip_l_mo(l)
  zeta_tq = (z_tq(i,j) + zdt) * recip_l_mo(l)
  zeta_0m = z0m(l) * recip_l_mo(l)
  zeta_0h = z0h(l) * recip_l_mo(l)

  !-----------------------------------------------------------------------
  !! 3. Calculate PHI_M and PHI_H for neutral and stable conditions.
  !!    Formulation of Beljaars and Holtslag (1991).
  !-----------------------------------------------------------------------

  IF (recip_l_mo(l)  >=  0.0) THEN
    phi_m(l) = phi_mn                                                          &
               + a * (zeta_uv - zeta_0m)                                       &
               + b * ( (zeta_uv - c_over_d) * EXP(-d * zeta_uv)                &
                      -(zeta_0m - c_over_d) * EXP(-d * zeta_0m) )              &
               + psi_m_rsl
    phi_h_fz1 = SQRT(1.0 + (2.0 / 3.0) * a * zeta_tq)
    phi_h_fz0 = SQRT(1.0 + (2.0 / 3.0) * a * zeta_0h)
    phi_h(l) = phi_hn +                                                        &
                 phi_h_fz1 * phi_h_fz1 * phi_h_fz1                             &
               - phi_h_fz0 * phi_h_fz0 * phi_h_fz0                             &
               + b * ( (zeta_tq - c_over_d) * EXP(-d * zeta_tq)                &
                      -(zeta_0h - c_over_d) * EXP(-d * zeta_0h) )
    IF (l_rsl_scalar) THEN
      phi_h(l) = phi_h(l) + psi_h_rsl
    END IF

    !-----------------------------------------------------------------------
    !! 4. Calculate PHI_M and PHI_H for unstable conditions.
    !-----------------------------------------------------------------------

  ELSE

    x_uv_sq = SQRT(1.0-16.0 * zeta_uv)
    x_0m_sq = SQRT(1.0-16.0 * zeta_0m)
    x_uv = SQRT(x_uv_sq)
    x_0m = SQRT(x_0m_sq)
    phi_m(l) = phi_mn - 2.0 * LOG( (1.0 + x_uv) / (1.0 + x_0m) )               &
                      - LOG( (1.0 + x_uv_sq) / (1.0 + x_0m_sq) )               &
                      + 2.0 * ( ATAN(x_uv) - ATAN(x_0m) )                      &
               + psi_m_rsl

    y_tq = SQRT(1.0-16.0 * zeta_tq)
    y_0h = SQRT(1.0-16.0 * zeta_0h)
    phi_h(l) = phi_hn - 2.0 * LOG( (1.0 + y_tq) / (1.0 + y_0h) )
    IF (l_rsl_scalar) THEN
      phi_h(l) = phi_h(l) + psi_h_rsl
    END IF

  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE can_drag_phi_m_h

SUBROUTINE can_drag_phi_m_h_vol(points, surft_pts, surft_index, pts_index,     &
                                recip_l_mo, z_uv, z_tq, canht, lai, z0m, z0h,  &
                                phi_m, phi_h)

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE conversions_mod, ONLY: pi
USE planet_constants_mod, ONLY: vkman
USE jules_surface_mod, ONLY: a,b,c,d,c_over_d
USE jules_vegetation_mod, ONLY: l_rsl_scalar, stanton_leaf

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE jules_print_mgr, ONLY: jules_message, jules_print

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates integrated forms of stability functions over the vegetation
!   canopy.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
INTEGER, INTENT(IN) ::                                                         &
  points,                                                                      &
    ! Number of points.
  surft_pts,                                                                   &
    ! Number of tile points.
  surft_index(points),                                                         &
    ! Index of tile points.
  pts_index(points)
    ! Index of points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  recip_l_mo(points),                                                          &
    ! Reciprocal of the Monin-Obukhov length
    ! (m^-1).
  z_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                   &
    ! Height of wind level above the canopy height (m)
  z_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                   &
    ! Height of temperature, moisture and scalar
    ! level above the canopy height (m).
  canht(points),                                                               &
    ! Canopy height (m)
  lai(points),                                                                 &
    ! Leaf area index
  z0m(points),                                                                 &
    ! Roughness length for momentum (m).
  z0h(points)
    ! Roughness length for heat/moisture/scalars (m)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  phi_m(points),                                                               &
    ! Integrated stability function for momentum.
  phi_h(points)
    ! Integrated stability function for heat/moisture/scalars.

! Local variables
INTEGER ::                                                                     &
  i,j,k,l
    ! Loop counter (horizontal field index)

REAL(KIND=real_jlslsm) ::                                                      &
  usuh,                                                                        &
    ! The ratio of friction velocity to wind velocity at the canopy top.
  l_c,                                                                         &
    ! Adjustment length scale of canopy (m)
  recip_l_mo_limit,                                                            &
    ! Reciprocal of the Monin-Obukhov length
    !  limited to solve non-linear equations satisfactorily
  sc,                                                                          &
    ! Turbulent Schmidt number
  zdt,                                                                         &
    ! Difference between the canopy height and displacement height (m).
  l_vor,                                                                       &
    ! Vorticity thickness (m).
  psi_m_rsl,                                                                   &
    ! Integrated RSL function for momentum.
  psi_h_rsl,                                                                   &
    ! Integrated RSL function for heat/moisture/scalars.
  phi_mn,                                                                      &
    ! Neutral value of stability function for momentum
  phi_hn,                                                                      &
    ! Neutral value of stability function for scalars.
  zeta_uv,                                                                     &
    ! Temporary in calculation of PHI_M.
  zeta_0m,                                                                     &
    ! Temporary in calculation of PHI_M.
  zeta_tq,                                                                     &
    ! Temporary in calculation of PHI_H.
  zeta_0h,                                                                     &
    ! Temporary in calculation of PHI_H.
  x_uv_sq,                                                                     &
    ! Temporary in calculation of PHI_M.
  x_0m_sq,                                                                     &
    ! Temporary in calculation of PHI_M.
  x_dt_sq,                                                                     &
    ! Temporary in calculation of PHI_M.
  x_uv,                                                                        &
    ! Temporary in calculation of PHI_M.
  x_0m,                                                                        &
    ! Temporary in calculation of PHI_M.
  x_dt,                                                                        &
    ! Temporary in calculation of PHI_M.
  y_tq,                                                                        &
    ! Temporary in calculation of PHI_H.
  y_0h,                                                                        &
    ! Temporary in calculation of PHI_H.
  y_dt,                                                                        &
    ! Temporary in calculation of PHI_H.
  phi_h_fz1,                                                                   &
    ! Temporary in calculation of PHI_H.
  phi_h_fz0
    ! Temporary in calculation of PHI_H.

! Local variables for numerical integration
INTEGER ::                                                                     &
  n

LOGICAL :: l_integral_converged
  ! Flag convergence of iteration

REAL(KIND=real_jlslsm) ::                                                      &
  pi_2,                                                                        &
  stbl_func_m0,                                                                &
  stbl_func_h0,                                                                &
  dif_stbl_func_m0,                                                            &
  dif_stbl_func_h0,                                                            &
  stbl_func_mz,                                                                &
  stbl_func_hz,                                                                &
  rsl_func_m,                                                                  &
  rsl_func_h,                                                                  &
  c1m,                                                                         &
  c2m,                                                                         &
  c1h,                                                                         &
  c2h,                                                                         &
  ff,                                                                          &
  zz_uv,                                                                       &
  zz_tq,                                                                       &
  difzz,                                                                       &
  zeta_dt,                                                                     &
  psi_m_rsl_dt,                                                                &
  psi_h_rsl_dt,                                                                &
  psi_m_0m,                                                                    &
  psi_h_0h,                                                                    &
  psi_m_dt,                                                                    &
  psi_h_dt,                                                                    &
  phi_tmp_0,                                                                   &
  phi_tmp_1,                                                                   &
  phi_m_tmp,                                                                   &
  phi_h_tmp,                                                                   &
  phi_rsl_tmp,                                                                 &
  phi_rsl_tmp_old


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CAN_DRAG_PHI_M_H_VOL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

pi_2 = 0.5 * pi

!$OMP PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                               &
!$OMP  PRIVATE(k,l,j,i,l_c,recip_l_mo_limit,sc,usuh,zdt,l_vor,psi_m_rsl,       &
!$OMP          psi_h_rsl,phi_mn,phi_hn,zeta_uv,zeta_tq,zeta_0m,zeta_0h,        &
!$OMP          phi_h_fz1,phi_h_fz0,x_uv_sq,x_0m_sq,x_dt_sq,x_uv,x_0m,x_dt,     &
!$OMP          y_tq,y_0h,y_dt,phi_tmp_0,phi_tmp_1,n,stbl_func_m0,stbl_func_h0, &
!$OMP          dif_stbl_func_m0,dif_stbl_func_h0,stbl_func_mz,stbl_func_hz,    &
!$OMP          rsl_func_m,rsl_func_h,c1m,c2m,c1h,c2h,zeta_dt,psi_m_rsl_dt,     &
!$OMP          psi_h_rsl_dt,psi_m_0m,psi_h_0h,psi_m_dt,psi_h_dt,phi_m_tmp,     &
!$OMP          phi_h_tmp,phi_rsl_tmp,phi_rsl_tmp_old,ff,zz_uv,zz_tq,difzz,     &
!$OMP          l_integral_converged)                                           &
!$OMP  SHARED(surft_pts,surft_index,pts_index,t_i_length,z_uv,z_tq,pi_2,       &
!$OMP         lai,canht,recip_l_mo,l_rsl_scalar,z0m,z0h,phi_m,phi_h,           &
!$OMP         stanton_leaf,cosh_ndn,coshp_sinh_ndn,tanhp_sinh_ndn)             &
!$OMP  IF(surft_pts > 1)
DO k = 1,surft_pts
  l = surft_index(k)
  j = (pts_index(l) - 1) / t_i_length + 1
  i = pts_index(l) - (j-1) * t_i_length

  CALL calc_rsl_parameter(canht(l), lai(l), recip_l_mo(l),                     &
                          usuh, l_c, recip_l_mo_limit, sc,                     &
                          zdt, l_vor)

  ! Integrated RSL functions
  CALL calc_psi_m_h_rsl(z_uv(i,j), z_tq(i,j),                                  &
                        zdt, recip_l_mo_limit, l_vor, usuh, sc,                &
                        psi_m_rsl, psi_h_rsl)

  !-----------------------------------------------------------------------
  !! 1. Calculate neutral values of PHI_M and PHI_H.
  !-----------------------------------------------------------------------

  phi_mn = LOG( (z_uv(i,j) + zdt) / z0m(l) )
  phi_hn = LOG( (z_tq(i,j) + zdt) / z0h(l) )

  !-----------------------------------------------------------------------
  !! 2. Calculate stability parameters.
  !-----------------------------------------------------------------------

  zeta_uv = (z_uv(i,j) + zdt) * recip_l_mo(l)
  zeta_tq = (z_tq(i,j) + zdt) * recip_l_mo(l)
  zeta_0m = z0m(l) * recip_l_mo(l)
  zeta_0h = z0h(l) * recip_l_mo(l)
  zeta_dt = zdt * recip_l_mo(l)

  !-----------------------------------------------------------------------
  !! 3. Calculate PHI_M and PHI_H for neutral and stable conditions.
  !!    Formulation of Beljaars and Holtslag (1991).
  !-----------------------------------------------------------------------

  IF (recip_l_mo(l)  >=  0.0) THEN
    phi_m(l) = phi_mn                                                          &
               + a * (zeta_uv - zeta_0m)                                       &
               + b * (  (zeta_uv - c_over_d) * EXP(-d * zeta_uv)               &
                      - (zeta_0m - c_over_d) * EXP(-d * zeta_0m) )             &
               + psi_m_rsl
    phi_h_fz1 = SQRT(1.0 + (2.0 / 3.0) * a * zeta_tq)
    phi_h_fz0 = SQRT(1.0 + (2.0 / 3.0) * a * zeta_0h)
    phi_h(l) = phi_hn +                                                        &
                 phi_h_fz1 * phi_h_fz1 * phi_h_fz1                             &
               - phi_h_fz0 * phi_h_fz0 * phi_h_fz0                             &
               + b * (  (zeta_tq - c_over_d) * EXP(-d * zeta_tq)               &
                      - (zeta_0h - c_over_d) * EXP(-d * zeta_0h) )
    IF (l_rsl_scalar) THEN
      phi_h(l) = phi_h(l) + psi_h_rsl
    END IF

    ! Adjust to make phi_m a mean across the layer:
    phi_m(l) = (1.0 + zdt / z_uv(i,j)) * phi_m(l)
    phi_h(l) = (1.0 + zdt / z_tq(i,j)) * phi_h(l)

    IF (recip_l_mo(l) > tol_neutrality) THEN
      ! Away from neutrality
      phi_tmp_0 = zeta_dt + 0.5 * a * zeta_dt ** 2                             &
        + (b / d) * EXP(-d * zeta_dt) * ( d * zeta_dt ** 2                     &
        + (1.0 - c) * ( zeta_dt + 1.0 / d ) )
      phi_tmp_1 = zeta_uv + 0.5 * a * zeta_uv ** 2                             &
        + (b / d) * EXP(-d * zeta_uv) * ( d * zeta_uv ** 2                     &
        + (1.0 - c) * ( zeta_uv + 1.0 / d ) )
      phi_m(l) = phi_m(l)                                                      &
        - ( phi_tmp_1 - phi_tmp_0 ) / (zeta_uv - zeta_dt)

      phi_tmp_0 = zeta_dt - (0.6 / a) * (1.0 - a * zeta_dt)                    &
        * (SQRT(1.0 + (2.0 / 3.0) * a * zeta_dt))** 3                          &
        + (b / d) * EXP(-d * zeta_dt) * ( d * zeta_dt ** 2                     &
        + (1.0 - c) * ( zeta_dt + 1.0 / d ) )
      phi_tmp_1 = zeta_tq - (0.6 / a) * (1.0 - a * zeta_tq)                    &
        * (SQRT(1.0 + (2.0 / 3.0) * a * zeta_tq))** 3                          &
        + (b / d) * EXP(-d * zeta_tq) * ( d * zeta_tq ** 2                     &
        + (1.0 - c) * ( zeta_tq + 1.0 / d ) )
      phi_h(l) = phi_h(l)                                                      &
        - ( phi_tmp_1 - phi_tmp_0 ) / (zeta_tq - zeta_dt)
    ELSE
      ! Close to neutrality
      phi_m(l) = phi_m(l) - 1.0
      phi_h(l) = phi_h(l) - 1.0
    END IF

    !-----------------------------------------------------------------------
    !! 4. Calculate PHI_M and PHI_H for unstable conditions.
    !-----------------------------------------------------------------------

  ELSE

    x_uv_sq = SQRT(1.0-16.0 * zeta_uv)
    x_0m_sq = SQRT(1.0-16.0 * zeta_0m)
    x_dt_sq = SQRT(1.0-16.0 * zeta_dt)
    x_uv = SQRT(x_uv_sq)
    x_0m = SQRT(x_0m_sq)
    x_dt = SQRT(x_dt_sq)
    phi_m(l) = phi_mn - 2.0 * LOG( (1.0 + x_uv) / (1.0 + x_0m) )               &
                      - LOG( (1.0 + x_uv_sq) / (1.0 + x_0m_sq) )               &
                      + 2.0 * ( ATAN(x_uv) - ATAN(x_0m) )                      &
               + psi_m_rsl

    y_tq = SQRT(1.0-16.0 * zeta_tq)
    y_0h = SQRT(1.0-16.0 * zeta_0h)
    y_dt = SQRT(1.0-16.0 * zeta_dt)
    phi_h(l) = phi_hn - 2.0 * LOG( (1.0 + y_tq) / (1.0 + y_0h) )
    IF (l_rsl_scalar) THEN
      phi_h(l) = phi_h(l) + psi_h_rsl
    END IF

    ! Adjust to make this a mean value.
    phi_m(l) = (1.0 + zdt / z_uv(i,j)) * phi_m(l)
    phi_h(l) = (1.0 + zdt / z_tq(i,j)) * phi_h(l)
    !
    IF (recip_l_mo(l) < -tol_neutrality) THEN
      ! Away from neutrality
      phi_m(l) = phi_m(l) + (1.0 / 12.0)                                       &
        * (x_uv ** 3 - x_dt ** 3) / (zeta_uv - zeta_dt)
      phi_h(l) = phi_h(l) + 0.125 * (y_tq - y_dt)                              &
        / (zeta_tq - zeta_dt)
    ELSE
      ! Close to neutrality
      phi_m(l) = phi_m(l) - 1.0
      phi_h(l) = phi_h(l) - 1.0
    END IF

  END IF

  !-----------------------------------------------------------------------
  ! Some additional terms for the RSL correction.
  ! The RSL function follows Eq. 24 of HF08,
  ! but does not use the relationship l_vor = 2*zdt:
  ! consult section 2 of the report attached to ticket #754.
  !
  ! Impose a lower limit to avoid unrealistic values
  !-----------------------------------------------------------------------
  CALL calc_stbl_func_m(zdt, recip_l_mo_limit,                                 &
                        stbl_func_m0, dif_stbl_func_m0)

  c2m = vkman *                                                                &
        (1.0 + l_vor / zdt - l_vor / stbl_func_m0 * dif_stbl_func_m0) /        &
        MAX((l_vor * usuh / zdt * stbl_func_m0 - vkman), min_den_c2mh)
  c1m = (1.0 - vkman * zdt / (l_vor * usuh * stbl_func_m0)) *                  &
        EXP(c2m * zdt / l_vor)

  CALL calc_stbl_func_h(zdt, recip_l_mo_limit,                                 &
                        stbl_func_h0, dif_stbl_func_h0)

  ! HF08 equation 11.
  ff = 0.5 * SQRT(1.0+4.0 * stanton_leaf * sc) - 0.5

  c2h = vkman * sc                                                             &
        * (ff + l_vor / zdt - l_vor / stbl_func_h0 * dif_stbl_func_h0)         &
        / MAX((l_vor * usuh / zdt * stbl_func_h0 - vkman * sc), min_den_c2mh)
  c1h = (1.0 - vkman * zdt * sc / (l_vor * usuh * stbl_func_h0))               &
        * EXP(c2h * zdt / l_vor)

  ! Numerical integration
  ! (Double exponential formula + Trapezoidal method)
  l_integral_converged = .FALSE.
  phi_rsl_tmp = 0.0
  DO n = -nn, nn
    phi_rsl_tmp_old = phi_rsl_tmp

    zz_uv = 0.5 * z_uv(i,j) * tanhp_sinh_ndn(n)                                &
          + 0.5 * (z_uv(i,j) + 2.0 * zdt)
    difzz = pi_2 * cosh_ndn(n)                                                 &
            / (coshp_sinh_ndn(n))** 2

    rsl_func_m = 1.0 - c1m * EXP(- c2m * zz_uv / l_vor)
    CALL calc_stbl_func_m(zz_uv, recip_l_mo_limit, stbl_func_mz)

    phi_rsl_tmp = phi_rsl_tmp + stbl_func_mz * (1.0 - rsl_func_m)              &
                                * 0.5 * z_uv(i,j) * difzz

    IF ((phi_rsl_tmp == 0.0 .AND. phi_rsl_tmp_old == 0.0) .OR.                 &
        (ABS((phi_rsl_tmp - phi_rsl_tmp_old) / phi_rsl_tmp) <=                 &
         tol_rsl_sim_converge)) THEN
      l_integral_converged = .TRUE.
      EXIT
    END IF
  END DO

  IF ( .NOT. l_integral_converged ) THEN
    WRITE(jules_message,'(A)') 'Warning: Integral for RSL ' //                 &
      'similarity function has not converged.'
    CALL jules_print('calc_rsl_parameter', jules_message)
  END IF

  phi_rsl_tmp = phi_rsl_tmp * dn / z_uv(i,j)

  CALL calc_psi_m_h(zeta_dt, zeta_dt, recip_l_mo(l), psi_m_dt, psi_h_dt)
  CALL calc_psi_m_h(zeta_0m, zeta_0h, recip_l_mo(l), psi_m_0m, psi_h_0h)

  ! Evaluate the RSL function at the top of the canopy.
  CALL calc_psi_m_h_rsl(0.0, 0.0,                                              &
                        zdt, recip_l_mo_limit, l_vor, usuh, sc,                &
                        psi_m_rsl_dt, psi_h_rsl_dt)

  phi_m_tmp = LOG(z0m(l) / zdt) + psi_m_dt - psi_m_0m - psi_m_rsl_dt

  phi_m(l) = phi_m(l)                                                          &
           + (zdt / z_uv(i,j)) * phi_m_tmp                                     &
           + phi_rsl_tmp

  IF (l_rsl_scalar) THEN
    ! Numerical integration
    ! (Double exponential formula + Trapezoidal method)
    phi_rsl_tmp = 0.0
    DO n = -nn, nn
      phi_rsl_tmp_old = phi_rsl_tmp

      zz_tq = 0.5 * z_tq(i,j) * tanhp_sinh_ndn(n)                              &
            + 0.5 * (z_tq(i,j) + 2.0 * zdt)
      difzz = pi_2 * cosh_ndn(n)                                               &
              / (coshp_sinh_ndn(n))** 2

      rsl_func_h = 1.0 - c1h * EXP(- c2h * zz_tq / l_vor)
      CALL calc_stbl_func_h(zz_tq, recip_l_mo_limit, stbl_func_hz)

      phi_rsl_tmp = phi_rsl_tmp + stbl_func_hz * (1.0 - rsl_func_h)            &
                                  * 0.5 * z_tq(i,j) * difzz

      IF ((phi_rsl_tmp == 0.0 .AND. phi_rsl_tmp_old == 0.0) .OR.               &
          (ABS((phi_rsl_tmp - phi_rsl_tmp_old) / phi_rsl_tmp) <=               &
           tol_rsl_sim_converge)) EXIT
    END DO
    phi_rsl_tmp = phi_rsl_tmp * dn / z_tq(i,j)

    phi_h_tmp = LOG(z0h(l) / zdt) + psi_h_dt - psi_h_0h - psi_h_rsl_dt

    phi_h(l) = phi_h(l)                                                        &
             + (zdt / z_tq(i,j)) * phi_h_tmp                                   &
             + phi_rsl_tmp
  END IF

END DO
!$OMP END PARALLEL DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE can_drag_phi_m_h_vol

!===============================================================================
! PRIVATE SUBROUTINE
!===============================================================================

SUBROUTINE calc_rsl_parameter(canht, lai, recip_l_mo,                          &
                              usuh, l_c, recip_l_mo_limit, sc,                 &
                              zdt, l_vor)

USE jules_vegetation_mod, ONLY: cd_leaf, c1_usuh, c2_usuh, c3_usuh

USE jules_print_mgr, ONLY: jules_message, jules_print

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a private subroutine to Calculate RSL-related parameters.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  canht,                                                                       &
    ! Canopy height (m)
  lai,                                                                         &
    ! Leaf area index
  recip_l_mo
    ! Reciprocal of the Monin-Obukhov length

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  usuh,                                                                        &
    ! The ratio of friction velocity to wind velocity at the canopy top.
  l_c,                                                                         &
    ! Adjustment length scale of canopy (m)
  recip_l_mo_limit,                                                            &
    ! Reciprocal of the Monin-Obukhov length
    !  limited to solve non-linear equations satisfactorily
  sc,                                                                          &
    ! Turbulent Schmidt number
  zdt,                                                                         &
    ! Difference between the canopy height and displacement height (m).
  l_vor
    ! Vorticity thickness (m).

! Local variables
INTEGER ::                                                                     &
  it,                                                                          &
    ! Loop counter for iteration
  itin
    ! Loop counter for inner iteration

LOGICAL :: l_iteration_converged
  ! Flag convergence of iteration
REAL(KIND=real_jlslsm) ::                                                      &
  usuh_neutral,                                                                &
  usuh_old,                                                                    &
  usuh_m1,                                                                     &
  usuh_m2,                                                                     &
  delta_1,                                                                     &
  delta_2,                                                                     &
  pai,                                                                         &
  stbl_func_m

! Local constants
INTEGER, PARAMETER :: n_its = 15

pai = MAX(lai, lai_min)
l_c = 1.0 / (cd_leaf * pai / canht)

! MO length is limited to solve non-linear equations satisfactorily.
! Upper and lower limit values are suggested by Harman and Finnigan (2008).
recip_l_mo_limit = MAX(lcrl_min,                                               &
                       MIN(lcrl_max, l_c * recip_l_mo))                        &
                   / l_c

! u*/U(h) is estimated from empirical equation proposed by
! Massman (1997), BLM, Eq. 6, but the constants may be different.
usuh_neutral = c1_usuh - c2_usuh * EXP(-c3_usuh * (cd_leaf * pai / ps_leaf))

! RSL parameters and usuh
usuh = usuh_neutral
IF (recip_l_mo_limit /= 0.0) THEN
  l_iteration_converged = .FALSE.
  it = 1
  DO
    IF ( it > n_its ) EXIT
    usuh_old = usuh
    DO itin = 1, 3
      CALL calc_zdt(canht, pai, usuh, zdt)
      CALL calc_stbl_func_m(zdt, recip_l_mo_limit, stbl_func_m)
      usuh = usuh_neutral / stbl_func_m
      IF ( itin == 1 ) usuh_m2 = usuh
      IF ( itin == 2 ) usuh_m1 = usuh
    END DO

    !   Refine the estimate invoking geometric convergence.
    delta_1 = usuh - usuh_m1
    delta_2 = usuh - usuh_m2
    IF ( delta_1 ** 2 < ABS( (delta_2-2.0 * delta_1) * (usuh -usuh_old) ) )    &
      usuh = usuh + delta_1 ** 2 / (delta_2-2.0 * delta_1)

    IF (ABS(usuh - usuh_old) < usuh * tol_rsl_converge) THEN
      l_iteration_converged = .TRUE.
      EXIT
    END IF
    it = it + 1
  END DO
  IF ( .NOT. l_iteration_converged ) THEN
    WRITE(jules_message,'(A)') "Iteration for u*/U(h) has not converged."
    CALL jules_print('calc_rsl_parameter', jules_message)
  END IF
END IF

! A simple parameterization is proposed by Harman and Finnigan
! (2008; BLM, Appendix C, Eq. 29)
sc = 0.5 + 0.3 * TANH(2.0 * l_c * recip_l_mo_limit)

! The displacement height is the effective level of mean drag on the canopy
! elements (Shaw and Pereira, 1982; Agric. Meteorol.)
CALL calc_zdt(canht, pai, usuh, zdt)

! Vorticity thickness
l_vor = 2.0 * usuh * usuh * l_c

RETURN
END SUBROUTINE calc_rsl_parameter

SUBROUTINE calc_psi_m_h(zeta_uv, zeta_tq, recip_l_mo,                          &
                        psi_m, psi_h)

USE conversions_mod, ONLY: pi
USE jules_surface_mod, ONLY: a, b, d, c_over_d

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a private subroutine to calculate integrated forms of stability
!   function for momentum and heat.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  zeta_uv,                                                                     &
    ! Stability parameter for momentum
  zeta_tq,                                                                     &
    ! Stability parameter for scalars
  recip_l_mo
    ! Reciprocal of the Monin-Obukhov length

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  psi_m,                                                                       &
    ! Integrated stability function for momentum.
  psi_h
    ! Integrated stability function for scalars.

! Local variables
REAL(KIND=real_jlslsm) ::                                                      &
  psi_h_fz,                                                                    &
  x_uv_sq,                                                                     &
  x_uv,                                                                        &
  y_tq

! Calculate PHI_M and PHI_H for neutral and stable conditions.
! Formulation of Beljaars and Holtslag (1991).

IF (recip_l_mo >= 0.0) THEN
  psi_m = - a * zeta_uv                                                        &
          - b * (zeta_uv - c_over_d) * EXP(- d * zeta_uv)                      &
          - b * c_over_d

  psi_h_fz = SQRT(1.0 + (2.0 / 3.0) * a * zeta_tq)
  psi_h = - psi_h_fz * psi_h_fz * psi_h_fz + 1.0                               &
          - b * (zeta_tq - c_over_d) * EXP(- d * zeta_tq)                      &
          - b * c_over_d
  ! Calculate PHI_M and PHI_H for unstable conditions.

ELSE

  x_uv_sq = SQRT(1.0-16.0 * zeta_uv)
  x_uv = SQRT(x_uv_sq)
  psi_m = 0.5 * pi                                                             &
        - 2.0 * ATAN(x_uv)                                                     &
        + 2.0 * LOG((1.0 + x_uv) * 0.5)                                        &
        + LOG((1.0 + x_uv_sq) * 0.5)

  y_tq = SQRT(1.0-16.0 * zeta_tq)
  psi_h = 2.0 * LOG((1.0 + y_tq) * 0.5)

END IF

RETURN
END SUBROUTINE calc_psi_m_h

SUBROUTINE calc_psi_m_h_rsl(z_uv, z_tq,                                        &
                            zdt, recip_l_mo, l_vor, usuh, sc,                  &
                            psi_m_rsl, psi_h_rsl)
USE conversions_mod, ONLY: pi
USE planet_constants_mod, ONLY: vkman
USE jules_vegetation_mod, ONLY: stanton_leaf

USE jules_print_mgr, ONLY: jules_message, jules_print

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a private subroutine to Calculate integrated roughness sublayer
!   function for momentum and heat.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  z_uv,                                                                        &
    ! Height of wind level above the canopy top (m)
  z_tq,                                                                        &
    ! Height of temperature, moisture and scalar
    ! level above the canopy top (m).
  zdt,                                                                         &
    ! Difference between canopy height and displacement height (m).
  recip_l_mo,                                                                  &
    ! Reciprocal of the Monin-Obukhov length
  l_vor,                                                                       &
    ! Vorticity thickness (m)
  usuh,                                                                        &
    ! The ratio friction velocity to wind velocity at the canopy top.
  sc
    ! Turbulent Schmidt number

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  psi_m_rsl,                                                                   &
    ! Integrated roughness sublayer function for momentum.
  psi_h_rsl
    ! Integrated roughness sublayer function for scalars.

! Local variables
INTEGER :: k
  ! Loop counter
INTEGER ::                                                                     &
  flg_converge_m,                                                              &
  flg_converge_h

REAL(KIND=real_jlslsm) ::                                                      &
  pi_2,                                                                        &
  stbl_func_m0,                                                                &
  stbl_func_h0,                                                                &
  dif_stbl_func_m0,                                                            &
  dif_stbl_func_h0,                                                            &
  stbl_func_mz,                                                                &
  stbl_func_hz,                                                                &
  rsl_func_m,                                                                  &
  rsl_func_h,                                                                  &
  c1m,                                                                         &
  c2m,                                                                         &
  c1h,                                                                         &
  c2h,                                                                         &
  ff,                                                                          &
  ss_m,                                                                        &
  ss_h,                                                                        &
  ss_m_old,                                                                    &
  ss_h_old,                                                                    &
  zz_uv,                                                                       &
  zz_tq,                                                                       &
  difzz

pi_2 = 0.5 * pi

! Parameters for RSL functions
! The RSL function follows Eq. 24 of HF08,
! but does not use the relationship l_vor = 2*zdt:
! consult section 2 of the report attached to ticket #754.

! Impose a lower limit to avoid unrealistic values.

CALL calc_stbl_func_m(zdt, recip_l_mo, stbl_func_m0, dif_stbl_func_m0)
c2m = vkman *                                                                  &
      (1.0 + l_vor / zdt - l_vor / stbl_func_m0 * dif_stbl_func_m0) /          &
      MAX((l_vor * usuh / zdt * stbl_func_m0 - vkman), min_den_c2mh)
c1m = (1.0 - vkman * zdt / (l_vor * usuh * stbl_func_m0)) *                    &
      EXP(c2m * zdt / l_vor)

CALL calc_stbl_func_h(zdt, recip_l_mo, stbl_func_h0, dif_stbl_func_h0)
! HF08 Eq. 11.
ff = 0.5 * SQRT(1.0+4.0 * stanton_leaf * sc) - 0.5
c2h = vkman * sc *                                                             &
      (ff + l_vor / zdt - l_vor / stbl_func_h0 * dif_stbl_func_h0) /           &
      MAX((l_vor * usuh / zdt * stbl_func_h0 - vkman * sc), min_den_c2mh)
c1h = (1.0 - vkman * zdt * sc / (l_vor * usuh * stbl_func_h0)) *               &
      EXP(c2h * zdt / l_vor)

! Numerical integration
! (Double exponential formula + Trapezoidal method)
ss_m = 0.0
ss_h = 0.0
flg_converge_m = 0
flg_converge_h = 0
DO k = -nn, nn
  ss_m_old = ss_m
  ss_h_old = ss_h

  zz_uv = z_uv + zdt + exp2_sinh_ndn(k)
  zz_tq = z_tq + zdt + exp2_sinh_ndn(k)
  difzz = pi_2 * cosh_ndn(k) * exp2_sinh_ndn(k)

  rsl_func_m = 1.0 - c1m * EXP(- c2m * zz_uv / l_vor)
  CALL calc_stbl_func_m(zz_uv, recip_l_mo, stbl_func_mz)

  rsl_func_h = 1.0 - c1h * EXP(- c2h * zz_tq / l_vor)
  CALL calc_stbl_func_h(zz_tq, recip_l_mo, stbl_func_hz)

  ss_m = ss_m + stbl_func_mz * (1.0 - rsl_func_m) / zz_uv * difzz
  ss_h = ss_h + stbl_func_hz * (1.0 - rsl_func_h) / zz_tq * difzz

  IF ((ss_m == 0.0 .AND. ss_m_old == 0.0) .OR.                                 &
      (ABS(ss_m - ss_m_old) <= ABS(ss_m) * tol_rsl_converge) )                 &
      flg_converge_m = 1
  IF ((ss_h == 0.0 .AND. ss_h_old == 0.0) .OR.                                 &
      (ABS(ss_h - ss_h_old) <= ABS(ss_h) * tol_rsl_converge) )                 &
      flg_converge_h = 1
  IF (flg_converge_m == 1 .AND. flg_converge_h == 1) EXIT
END DO

IF ( flg_converge_m /= 1 ) THEN
  WRITE(jules_message,'(A)') 'Integrated roughness sublayer function for ' //  &
                             'momentum has not converged.'
  CALL jules_print('calc_psi_m_h_rsl', jules_message)
END IF

IF ( flg_converge_h /= 1 ) THEN
  WRITE(jules_message,'(A)') 'Integrated roughness sublayer function for ' //  &
                             'scalars has not converged.'
  CALL jules_print('calc_psi_m_h_rsl', jules_message)
END IF

psi_m_rsl = ss_m * dn
psi_h_rsl = ss_h * dn

RETURN
END SUBROUTINE calc_psi_m_h_rsl

SUBROUTINE calc_z0_m_h_rsl(zdt, recip_l_mo, l_vor, usuh, sc,                   &
                           z0m, z0h)

USE planet_constants_mod, ONLY: vkman
USE jules_vegetation_mod, ONLY: stanton_leaf

USE jules_print_mgr, ONLY: jules_message, jules_print

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a private subroutine to calculate roughness length for momentum
!   and scalars.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  zdt,                                                                         &
    ! Difference between canopy height and displacement height (m).
  recip_l_mo,                                                                  &
    ! Reciprocal of the Monin-Obukhov length
  l_vor,                                                                       &
    ! Vorticity thickness (m).
  usuh,                                                                        &
    ! The ratio of friction velocity to wind velocity at the canopy top.
  sc
    ! Turbulent Schmidt number

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  z0m,                                                                         &
    ! Roughness length for momentum.
  z0h
    ! Roughness length for scalars.

! Local variables
INTEGER :: it
  ! Loop counter for iteration
LOGICAL :: l_iteration_converged
  ! Flag convergence of iteration
REAL(KIND=real_jlslsm) ::                                                      &
  ff,                                                                          &
  psi_m,                                                                       &
  psi_h,                                                                       &
  psi_m0,                                                                      &
  psi_h0,                                                                      &
  psi_m_rsl,                                                                   &
  psi_h_rsl,                                                                   &
  z0m_0,                                                                       &
  z0m_old,                                                                     &
  z0h_0,                                                                       &
  z0h_old,                                                                     &
  zeta_uv,                                                                     &
  zeta_tq,                                                                     &
  zeta_uv0,                                                                    &
  zeta_tq0,                                                                    &
  z_uv0,                                                                       &
  z_tq0

! Local constants
INTEGER, PARAMETER :: n_its = 15
  ! Number of iterations to solve for roughness length

ff = 0.5 * SQRT(1.0+4.0 * stanton_leaf * sc) - 0.5

! Calculates integrated stability functions at the canopy top.
zeta_uv = zdt * recip_l_mo
zeta_tq = zdt * recip_l_mo
CALL calc_psi_m_h(zeta_uv, zeta_tq, recip_l_mo, psi_m, psi_h)

! Calculates integrated RSL functions at the canopy top.
z_uv0 = 0.0
z_tq0 = 0.0
CALL calc_psi_m_h_rsl(z_uv0, z_tq0,                                            &
                      zdt, recip_l_mo, l_vor, usuh, sc,                        &
                      psi_m_rsl, psi_h_rsl)

z0m_0 = zdt * EXP(- vkman / usuh                                               &
                  - psi_m                                                      &
                  + psi_m_rsl)

z0h_0 = zdt * EXP(- vkman * sc / (ff * usuh)                                   &
                  - psi_h                                                      &
                  + psi_h_rsl)

z0m = z0m_0
z0h = z0h_0

l_iteration_converged = .FALSE.
it = 1
DO
  IF (it > n_its ) EXIT

  z0m_old = z0m
  z0h_old = z0h
  zeta_uv0 = z0m * recip_l_mo
  zeta_tq0 = z0h * recip_l_mo
  CALL calc_psi_m_h(zeta_uv0, zeta_tq0, recip_l_mo, psi_m0, psi_h0)

  z0m = z0m_0 * EXP(psi_m0)
  z0h = z0h_0 * EXP(psi_h0)
  IF ( (ABS(z0m - z0m_old) < z0m * tol_rsl_converge) .AND.                     &
       (ABS(z0h - z0h_old) < z0h * tol_rsl_converge) ) THEN
    l_iteration_converged = .TRUE.
    EXIT
  END IF

  it = it + 1
END DO

IF ( .NOT. l_iteration_converged ) THEN
  WRITE(jules_message,'(A)') "Iteration for roughness lengths has not converged."
  CALL jules_print('calc_z0_m_h_rsl', jules_message)
END IF


RETURN
END SUBROUTINE calc_z0_m_h_rsl

SUBROUTINE calc_stbl_func_m(zref, recip_l_mo,                                  &
                            stbl_func_m,                                       &
                            dif_stbl_func_m)

USE jules_surface_mod, ONLY: a,b,c,d

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a private subroutine to calculate stability function for
!   momentum.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  zref,                                                                        &
    ! Reference height
  recip_l_mo
    ! Reciprocal of the Monin-Obukhov length

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  stbl_func_m
    ! Stability function for momentum

REAL(KIND=real_jlslsm), INTENT(OUT), OPTIONAL ::                               &
  dif_stbl_func_m
    ! Differential stability function for momentum

! Local variables
REAL(KIND=real_jlslsm) :: xx
REAL(KIND=real_jlslsm) :: zeta

zeta = zref * recip_l_mo
IF (recip_l_mo >= 0.0) THEN
  stbl_func_m = 1.0 + a * zeta                                                 &
              + b * (1.0 + c - d * zeta) * zeta * EXP(-d * zeta)
  IF (PRESENT(dif_stbl_func_m)) THEN
    dif_stbl_func_m = a                                                        &
                    + b * EXP(-d * zeta)                                       &
                      * (1.0 + c - (3.0 + c) * d * zeta                        &
                             + d * d * zeta * zeta)
    dif_stbl_func_m = dif_stbl_func_m * recip_l_mo
  END IF
ELSE
  xx   = 1.0 - 16.0 * zeta
  stbl_func_m = 1.0 / SQRT(SQRT(xx))
  IF (PRESENT(dif_stbl_func_m)) THEN
    dif_stbl_func_m = 4.0 * recip_l_mo / xx * stbl_func_m
  END IF
END IF

RETURN
END SUBROUTINE calc_stbl_func_m

SUBROUTINE calc_stbl_func_h(zref, recip_l_mo,                                  &
                            stbl_func_h,                                       &
                            dif_stbl_func_h)

USE jules_surface_mod, ONLY: a,b,c,d

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a private subroutine to calculate stability function for
!   scalars.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  zref,                                                                        &
    ! Reference height
  recip_l_mo
    ! Reciprocal of the Monin-Obukhov length

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  stbl_func_h
    ! Stability function for scalars

REAL(KIND=real_jlslsm), INTENT(OUT), OPTIONAL ::                               &
  dif_stbl_func_h
    ! Differential stability function for scalars

! Local variables
REAL(KIND=real_jlslsm) :: xx
REAL(KIND=real_jlslsm) :: zeta

zeta = zref * recip_l_mo
IF (recip_l_mo >= 0.0) THEN
  xx = SQRT(1.0+2.0 / 3.0 * zeta)
  stbl_func_h = 1.0 + a * zeta * xx                                            &
              + b * (1.0 + c - d * zeta) * zeta * EXP(-d * zeta)
  IF (PRESENT(dif_stbl_func_h)) THEN
    dif_stbl_func_h = a * xx                                                   &
                    + a * a * zeta / (3.0 * xx)                                &
                    + b * EXP(-d * zeta)                                       &
                      * (1.0 + c - (3.0 + c) * d * zeta                        &
                             + d * d * zeta * zeta)
    dif_stbl_func_h = dif_stbl_func_h * recip_l_mo
  END IF
ELSE
  xx = 1.0 - 16.0 * zeta
  stbl_func_h = 1.0 / SQRT(xx)
  IF (PRESENT(dif_stbl_func_h)) THEN
    dif_stbl_func_h = 8.0 * recip_l_mo / xx * stbl_func_h
  END IF
END IF

RETURN
END SUBROUTINE calc_stbl_func_h

SUBROUTINE calc_zdt(canht, pai, usuh,                                          &
                    zdt)

USE jules_vegetation_mod, ONLY: cd_leaf

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a private subroutine to calculate the difference between canopy
!   height and displacement height.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  canht,                                                                       &
    ! Canopy height (m)
  pai,                                                                         &
    ! Leaf area index
  usuh
    ! The ratio friction velocity to wind velocity at the canopy top.

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  zdt
    ! Difference between the canopy height and displacement height (m).

! Local variables
!
REAL(KIND=real_jlslsm) ::                                                      &
  dai_h,                                                                       &
    ! Drag area index
  att_coef,                                                                    &
    ! Attenuation coefficient for momentum
  e_a,                                                                         &
    ! Negative exponential of att_coeff
  d_h,                                                                         &
    ! d/h
  qs
    ! An empirical factor for att_coef

REAL(KIND=real_jlslsm), PARAMETER :: qb = 2.0 / (1.0 - EXP(-1.0))
    ! Empirical parameter in Massman's (2017) fit to the e-folding scale
REAL(KIND=real_jlslsm), PARAMETER :: qa = 4.02 - qb
    ! Empirical parameter in Massman's (2017) fit to the e-folding scale
    ! for the streamwise stress
REAL(KIND=real_jlslsm), PARAMETER :: qc = 0.6
    ! Empirical parameter in Massman's (2017) fit to the e-folding scale
    ! for the streamwise stress

dai_h = cd_leaf * pai
! This is the in-line equation just below equation (12) in Massman (2017).
qs = qa + qb * EXP(-qc * 0.5 * dai_h / (usuh * usuh))
att_coef = qs * 0.5 * dai_h / (usuh * usuh)

! This equation arises from the algebra, but is rewritten using negative
! exponentials to avoid numerical issues.
! d_h = (1.0-1.0 / COSH(att_coef)) * (1.0 - TANH(att_coef) / (att_coef))
e_a = EXP( -att_coef )
d_h = (1.0-2.0 * e_a / (1.0 + e_a ** 2) ) *                                    &
      (1.0 - (1.0 / att_coef) * (1.0 - e_a ** 2) / (1.0 + e_a ** 2) )


zdt = canht * (1.0 - d_h)

RETURN
END SUBROUTINE calc_zdt

END MODULE can_drag_mod
