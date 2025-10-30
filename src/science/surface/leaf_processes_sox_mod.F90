!******************************COPYRIGHT********************************************
! (c) University of Exeter 2024
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT********************************************

MODULE leaf_processes_sox_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='LEAF_PROCESSES_SOX_MOD'

PRIVATE
PUBLIC leaf_processes_sox

CONTAINS
! *********************************************************************
! Routines to calculate co2p_internal, gs and A values with the semi-analytical
! version of SOX
! Eller et al., (2020) https://doi.org/10.1111/nph.16419
! *********************************************************************

SUBROUTINE leaf_processes_sox(fn_type, land_field, veg_pts, veg_index,         &
                              acr, apar, co2p_canopy, ccp, dq_kgr, k_co2,      &
                              k_o2, o2p_atmos, pstar, vcmax, t_surface,        &
                              h_canopy, swp, resp_d, clos_pts, open_pts,       &
                              clos_index, open_index, co2p_internal, o3mol,    &
                              resist_a, al_phot, flux_o3, fo3, gl_h2o, lwp,    &
                              pft_photo_model)
                              ! fn_type previously ft
                              ! co2p_canopy previously ca
                              ! dq_kgr previously dq
                              ! k_co2 previously kc
                              ! k_o2 previously ko
                              ! o2p_atmos previously oa
                              ! t_surface previously tl
                              ! h_canopy previously ht
                              ! resp_d previously rd
                              ! co2p_internal previously ci
                              ! resist_a previously ra
                              ! al_phot previously al
                              ! gl_h2o previously gl


USE jules_surface_mod, ONLY: beta1, beta2, ratio, ratio_o3, fwe_c3, fwe_c4
USE pftparm, ONLY: dfp_dcuo, fl_o3_ct, glmin, alpha, sox_rp_min, sox_p50,      &
                   sox_a, c3
USE c_rmol, ONLY: rmol
USE planet_constants_mod, ONLY: g
USE water_constants_mod, ONLY: rho_water
USE ccarbon, ONLY: m_air, m_water
USE jules_vegetation_mod, ONLY: l_o3_damage


USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 fn_type                                                                       &
                            ! Plant functional type.
,land_field                                                                    &
                            ! Total number of land points.
,pft_photo_model                                                               &
                            ! Indicates which photosynthesis model to use for
                            ! the current PFT.
,veg_pts                                                                       &
                            ! Number of vegetated points.
,veg_index(land_field)
                            ! IN Index of vegetated points
                            !    on the land grid.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 acr(land_field)                                                               &
                            ! IN Absorbed PAR (mol photons/m2/s).
,apar(land_field)                                                              &
                            ! IN Absorbed PAR (W m-2).
,co2p_canopy(land_field)                                                       &
                            ! IN Canopy CO2 pressure (Pa).
,ccp(land_field)                                                               &
                            ! IN Photorespiratory compensatory point (Pa).
,dq_kgr(land_field)                                                            &
                            ! IN Canopy level specific humidity deficit
                            !    (kg H2O/kg air).
,k_co2(land_field)                                                             &
                            ! IN Michaelis-Menten constant for CO2 (Pa).
,k_o2(land_field)                                                              &
                            ! IN Michaelis-Menten constant for O2 (Pa).
,o2p_atmos(land_field)                                                         &
                            ! IN Atmospheric O2 pressure (Pa).
,pstar(land_field)                                                             &
                            ! IN Atmospheric pressure (Pa).
,vcmax(land_field)                                                             &
                            ! IN Maximum rate of carboxylation of Rubisco
                            !    (mol CO2/m2/s).
,t_surface(land_field)                                                         &
                            ! IN Surface temperature (K).
,h_canopy(land_field)                                                          &
                            ! IN Canopy height (m).
,swp(land_field)                                                               &
                            ! IN Soil water potential in the
                            !    root zone (Pa).
,resist_a(land_field)                                                          &
                            ! IN Total aerodynamic+boundary layer resistance
                            ! between leaf surface and reference level (s/m).
,o3mol(land_field)
                            ! OUT Molar concentration of ozone
                            !     at reference level (nmol/m3).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 resp_d(land_field)
                            ! Dark respiration (mol CO2/m2/s).
                            ! This is modified only if l_o3_damage=.TRUE.
!------------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!------------------------------------------------------------------------------
INTEGER, INTENT(OUT) ::                                                        &
 clos_pts                                                                      &
                            ! Number of land points with closed stomata.
,open_pts                                                                      &
                            ! Number of land points with open stomata.
,clos_index(land_field)                                                        &
                            ! Index of land points with closed stomata.
,open_index(land_field)
                            ! Index of land points with open stomata.


REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 gl_h2o(land_field)                                                            &
                            ! OUT Leaf conductance for H2O (m/s).
,al_phot(land_field)                                                           &
                            ! OUT Net Leaf photosynthesis (mol CO2/m2/s).
,flux_o3(land_field)                                                           &
                            ! OUT Flux of O3 to stomata (nmol O3/m2/s).
,fo3(land_field)                                                               &
                            ! OUT Ozone exposure factor.
,lwp(land_field)                                                               &
                            ! OUT Leaf water potential (MPa)
,co2p_internal(land_field)
                            ! OUT Internal CO2 pressure (Pa).

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  errcode                                                                      &
                            ! Error code to pass to ereport.
 ,j,l                       ! Loop counters.

LOGICAL ::                                                                     &
  l_closed(land_field)      ! Logical to mark closed points to help
                            ! parallel performance.

REAL(KIND=real_jlslsm) ::                                                      &
 b_qdr,c_qdr                                                                   &
                            ! Work variables for ozone flux calculations.
,beta1p2m4, beta2p2m4                                                          &
                            ! beta[12] ** 2 * 4.
,wcarb(land_field)                                                             &
                            ! WORK Carboxylation, ...
,wlite(land_field)                                                             &
                            !      ... Light, and ...
,wexpt(land_field)                                                             &
                            !      ... export limited gross ...
!                           !      ... photosynthetic rates ...
!                           !      ... (mol CO2/m2/s).
,al_ca(land_field)                                                             &
,al_col(land_field)                                                            &
                            ! WORK net photosynthesis at ca and at the
                            !      co2p_internal co-limitation point
                            !      (mol CO2/m2/s).
,b1_coef(land_field)                                                           &
,b2_coef(land_field)                                                           &
,b3_coef(land_field)                                                           &
                            ! WORK Coefficients of the colimitation
                            !      quadratic.
,ci_col(land_field)                                                            &
                            ! WORK internal co2 concentration at the
                            ! photosynthesis co-limitation point (Pa)
,coef_1(land_field)                                                            &
,coef_2(land_field)                                                            &
,coef_3(land_field)                                                            &
,coef_4(land_field)                                                            &
                            ! WORK Coefficients of the A=f(glco2)
                            ! quadratic
,col_a(land_field)                                                             &
,col_b(land_field)                                                             &
,col_c(land_field)                                                             &
,col_d(land_field)                                                             &
,col_x(land_field)                                                             &
                            ! WORK Coefficients of the ci_col quadratic
,dA_dci(land_field)                                                            &
                            ! WORK A partial derivative to compute xi_stomatal
,glco2(land_field)                                                             &
                            ! WORK Stomatal conductance to CO2
,gl_col(land_field)                                                            &
                            ! WORK Stomatal conductance to CO2 at ci_col
,gl_mol(land_field)                                                            &
                            ! WORK Stomatal conductance to CO2 at ci_col
,dq_mol_per_mol(land_field)                                                    &
                            ! WORK dq_kgr in mol/mol
,dK_dLWP_K(land_field)                                                         &
                            ! WORK V partial derivative to compute xi_stomatal
,lwp_pd(land_field)                                                            &
,lwp_m(land_field)                                                             &
                            ! WORK leaf water potentials used to calculate
                            !      dA_dci (MPa)
,K_pd(land_field)                                                              &
                            ! WORK pre-dawn normalised hydraulic
                            !      conductance loss
,K_m(land_field)                                                               &
                            ! WORK normalised hydraulic conductance loss at
                            !      lwp = lwp_pd + p50
,xi_stomatal(land_field)                                                       &
                            ! WORK Parameter representing cost of
                            ! stomatal opening, used to compute glco2
,E_leaf(land_field)
                            ! WORK leaf transpiration (mol H2O/m2/s)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='LEAF_PROCESSES_SOX'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Flag where the stomata are closed
!-----------------------------------------------------------------------------
DO j = 1,veg_pts
  l = veg_index(j)
  IF ( (apar(l) <= 0.0) ) THEN
    l_closed(l) = .TRUE.
  ELSE
    l_closed(l) = .FALSE.
  END IF
END DO

clos_pts = 0
open_pts = 0

DO j = 1,veg_pts
  l = veg_index(j)
  IF ( l_closed(l) ) THEN
    clos_pts = clos_pts + 1
    clos_index(clos_pts) = j
  ELSE
    open_pts = open_pts + 1
    open_index(open_pts) = j
  END IF
END DO

!-----------------------------------------------------------------------------
! Calculate co-limited photosynthesis at co2p_canopy
!-----------------------------------------------------------------------------
CALL w_rates_ci(land_field, veg_pts, veg_index, clos_pts, open_pts,            &
                clos_index, open_index, fn_type, co2p_canopy, o2p_atmos,       &
                pstar, ccp, vcmax, k_o2, k_co2, acr, wcarb, wexpt, wlite)

CALL colim(land_field, veg_pts, veg_index, clos_pts, open_pts,                 &
           clos_index, open_index, wcarb, wexpt, wlite, resp_d, al_ca)

!-----------------------------------------------------------------------------
! Calculate ci at the photosynthetic co-limitation point
! See supplementary material of Eller et al., 2020 for details
! https://doi.org/10.1111/nph.16419
! ----------------------------------------------------------------------------
beta2p2m4 = 4 * beta2 * beta2

IF ( c3(fn_type) == 1 ) THEN
  DO j = 1 ,open_pts
    l = veg_index(open_index(j))

    col_a(l)  = - vcmax(l) * ccp(l)
    col_b(l)  = vcmax(l)
    col_c(l)  = k_co2(l) * ( 1.0 + o2p_atmos(l) / k_o2(l) )
    col_d(l)  = 1.0
    wlite(l)  = alpha(fn_type) * acr(l)
    wlite(l)  = MAX(wlite(l), TINY(1.0e0))
    wexpt(l)  = 0.5 * vcmax(l)

    b1_coef(l)     = beta2
    b2_coef(l)     = - ( wexpt(l) + wlite(l) )
    b3_coef(l)     = wexpt(l) * wlite(l)
    col_x(l)  = -b2_coef(l) / ( 2 * b1_coef(l) )                               &
                - SQRT( b2_coef(l) * b2_coef(l) /                              &
                beta2p2m4 - b3_coef(l) / b1_coef(l) )
    ci_col(l) = ( col_a(l) - col_c(l) *col_x(l) ) /                            &
                ( col_d(l) * col_x(l) - col_b(l) )
  END DO
ELSE
  DO j = 1,open_pts
    l = veg_index(open_index(l))

    wcarb(l)  = vcmax(l)
    wlite(l)  = alpha(fn_type) * acr(l)
    wlite(l)  = MAX(wlite(l), TINY(1.0e0))

    b1_coef(l)     = beta2
    b2_coef(l)     = - ( wcarb(l) + wlite(l) )
    b3_coef(l)     = wcarb(l) * wlite(l)
    col_x(l)  = - b2_coef(l) / ( 2 * b1_coef(l) )                              &
                - SQRT( b2_coef(l) * b2_coef(l) /                              &
                beta2p2m4 - b3_coef(l) / b1_coef(l) )
    ci_col(l) = ( col_x(l) * pstar(l) ) / fwe_c4 * vcmax(l)
  END DO
END IF

!-----------------------------------------------------------------------------
! Calculate co-limited photosynthesis at ci_col
!-----------------------------------------------------------------------------
CALL w_rates_ci(land_field, veg_pts, veg_index, clos_pts, open_pts,            &
                clos_index, open_index, fn_type, ci_col, o2p_atmos,            &
                pstar, ccp, vcmax, k_o2, k_co2, acr, wcarb, wexpt, wlite)

CALL colim(land_field, veg_pts, veg_index, clos_pts, open_pts,                 &
           clos_index, open_index, wcarb, wexpt, wlite, resp_d, al_col)


!-----------------------------------------------------------------------------
! Compute dA_dci
!-----------------------------------------------------------------------------
DO j = 1,open_pts
  l = veg_index(open_index(j))

  dA_dci(l) = ( al_ca(l) - al_col(l) )/                                        &
              ( ( co2p_canopy(l) - ci_col(l) ) / pstar(l) )

  !-----------------------------------------------------------------------------
  ! Calculate dK_dLWP_K
  !-----------------------------------------------------------------------------
  lwp_pd(l)    = ( swp(l) - g * rho_water * h_canopy(l) ) * 1.0e-6
  K_pd(l)      = 1.0 / ( 1.0 + ( lwp_pd(l) / sox_p50(fn_type) )                &
                      ** sox_a(fn_type) )
  lwp_m(l)     = ( lwp_pd(l) + sox_p50(fn_type) )/ 2.0
  K_m(l)       = 1.0 / (1.0 + (lwp_m(l) / sox_p50(fn_type)) ** sox_a(fn_type))
  dK_dLWP_K(l) = ((K_pd(l)-K_m(l))/(lwp_pd(l)-lwp_m(l))) * (1.0/K_pd(l))
  dK_dLWP_K(l) = MAX(dK_dLWP_K(l),TINY(1.0e0))

  !-----------------------------------------------------------------------------
  ! Calculate stomatal conductance
  !-----------------------------------------------------------------------------
   ! Calculate xi_stomatal
  dq_mol_per_mol(l) = dq_kgr(l) / m_water * m_air
  xi_stomatal(l) = 2.0 / ( 1.6 * (sox_rp_min(fn_type)/K_pd(l))                 &
                         * dq_mol_per_mol(l) * dK_dLWP_K(l) )

  ! Calculate glco2
  IF (dA_dci(l) <= 0.0) THEN
        ! If there is no A gradient, glco2 goes to co-limitation point
    glco2(l) = al_col(l) * pstar(l) / (co2p_canopy(l)-ci_col(l))

  ELSE
    glco2(l) = 0.5 * dA_dci(l) *                                               &
               (SQRT(1.0 + 4.0 * xi_stomatal(l)/dA_dci(l)) - 1.0)
  END IF

  ! Diagnose the leaf conductance to water
  gl_mol(l) = ratio * glco2(l)
  gl_h2o(l)     = gl_mol(l) * (8.314472 * t_surface(l)) / pstar(l)
  gl_h2o(l)     = MAX(gl_h2o(l),glmin(fn_type))
  ! Compute LWP
  E_leaf(l) = gl_mol(l) * dq_mol_per_mol(l)
  lwp(l)    = (lwp_pd(l) - E_leaf(l)*(sox_rp_min(fn_type)/K_pd(l)))

END DO

!-----------------------------------------------------------------------------
! Calculate limited A rates in function of stomatal conductance to co2
!-----------------------------------------------------------------------------
IF (c3(fn_type) == 1) THEN

  DO j = 1,open_pts
    l = veg_index(open_index(j))
    ! wcarb
    coef_1(l) = vcmax(l)
    coef_2(l) = k_co2(l) * (1.0 + o2p_atmos(l) / k_o2(l))
    coef_3(l) = (resp_d(l)-coef_1(l)) -                                        &
                    (glco2(l)/pstar(l)) * (co2p_canopy(l) + coef_2(l))
    coef_4(l) = (glco2(l)/pstar(l)) * (coef_1(l)*co2p_canopy(l)                &
                   - coef_1(l)*ccp(l)  - resp_d(l)*co2p_canopy(l)              &
                   - resp_d(l)*coef_2(l))
    wcarb(l)  = (-(coef_3(l)/2.0) - SQRT(((coef_3(l)/2.0) ** 2.0)              &
                   - coef_4(l)) +resp_d(l))
    ! wlite
    coef_1(l) = alpha(fn_type) * acr(l)
    coef_2(l) = 2.0 * ccp(l)
    coef_3(l) = (resp_d(l)-coef_1(l)) - (glco2(l)/pstar(l)) * (co2p_canopy(l)  &
                    + coef_2(l))
    coef_4(l) = (glco2(l)/pstar(l)) * (coef_1(l)*co2p_canopy(l)                &
                  - coef_1(l)*ccp(l) - resp_d(l)*co2p_canopy(l)                &
                  - resp_d(l)*coef_2(l))
    wlite(l) = (-(coef_3(l)/2.0) - SQRT(((coef_3(l)/2.0) ** 2.0)               &
                   - coef_4(l)) +resp_d(l))
    wlite(l) = MAX(wlite(l), TINY(1.0e0))
    ! wexpt
    wexpt(l) = fwe_c3 * vcmax(l)
  END DO

ELSE

  DO j = 1,open_pts
    l = veg_index(open_index(j))

    ! wcarb
    wcarb(l) = vcmax(l)
    ! wlite
    wlite(l) = alpha(fn_type) * acr(l)
    wlite(l) = MAX(wlite(l), TINY(1.0e0))
    ! wexpt
    wexpt(l) = -glco2(l)*(resp_d(l)*pstar(l)-fwe_c4*vcmax(l)*co2p_canopy(l))/  &
               (pstar(l)*(fwe_c4*vcmax(l)+glco2(l)))+resp_d(l)
    wexpt(l) = MAX(wexpt(l), TINY(1.0e0))
  END DO
END IF

!-----------------------------------------------------------------------------
! Calculate final co-limited A for the limited A rates
!-----------------------------------------------------------------------------
CALL colim (land_field,veg_pts,veg_index, clos_pts, open_pts,                  &
            clos_index, open_index, wcarb, wexpt, wlite, resp_d, al_phot)

!----------------------------------------------------------------------
! Diagnose co2p_internal
!----------------------------------------------------------------------
DO j = 1,open_pts
  l = veg_index(open_index(j))

  co2p_internal(l) = co2p_canopy(l) - (al_phot(l) * pstar(l))/glco2(l)

END DO

!-----------------------------------------------------------------------------
! Close stomata at points with negative or zero net photosynthesis
! or where the leaf resistance exceeds its maximum value.
!-----------------------------------------------------------------------------
DO j = 1,open_pts
  l = veg_index(open_index(j))

  IF (gl_h2o(l) <= glmin(fn_type) .OR. al_phot(l) <= 0.0) THEN
    gl_h2o(l)    = glmin(fn_type)
    al_phot(l)    = -resp_d(l)
    lwp(l)   = lwp_pd(l)
    co2p_internal(l)    = ccp(l)
  END IF

END DO

!----------------------------------------------------------------------
! Define fluxes and conductances for points with closed stomata
!----------------------------------------------------------------------
DO j = 1,clos_pts
  l = veg_index(clos_index(j))

  gl_h2o(l)     = glmin(fn_type)
  al_phot(l)     = -resp_d(l)
  lwp_pd(l) = (swp(l) - g * rho_water * h_canopy(l)) * 1.0e-6
  lwp(l)    = lwp_pd(l)
  co2p_internal(l)     = ccp(l)

  ! Indicate no ozone damage at closed points.
  IF (l_o3_damage) fo3(l) = 1.0

END DO

IF ( l_o3_damage ) THEN
  !---------------------------------------------------------------------------
  ! Modify the stomatal conductance and photosynthesis for ozone effects
  ! (Peter Cox, 12/11/04)
  !---------------------------------------------------------------------------
  DO j = 1,open_pts
    l = veg_index(open_index(j))

    !-------------------------------------------------------------------------
    ! Flux of O3 without ozone effects (for use in analytical eqn)
    !-------------------------------------------------------------------------
    flux_o3(l) = o3mol(l) / (resist_a(l) + (ratio_o3 / gl_h2o(l)))

    !-------------------------------------------------------------------------
    ! Analytic solution for the ozone exposure factor
    !-------------------------------------------------------------------------
    ! Use EPSILON to avoid overflow on division
    IF (ABS(resist_a(l)) < EPSILON(1.0)) THEN
      fo3(l) = (1.0 + dfp_dcuo(fn_type) * fl_o3_ct(fn_type))                   &
             / (1.0 + dfp_dcuo(fn_type) * flux_o3(l))
    ELSE
      b_qdr = ratio_o3 / (gl_h2o(l) * resist_a(l))                             &
               + dfp_dcuo(fn_type) * o3mol(l) / resist_a(l)                    &
               - (1.0 + dfp_dcuo(fn_type) * fl_o3_ct(fn_type))
      c_qdr = -ratio_o3 / (gl_h2o(l) * resist_a(l))                            &
               * (1.0 + dfp_dcuo(fn_type) * fl_o3_ct(fn_type))
      fo3(l) = -0.5 * b_qdr + 0.5 * SQRT(b_qdr * b_qdr - 4.0 * c_qdr)
    END IF

    fo3(l) = MIN(MAX(fo3(l),0.0),1.0)

    !-------------------------------------------------------------------------
    ! Update the leaf conductance and photosynthesis
    !-------------------------------------------------------------------------
    gl_h2o(l) = gl_h2o(l) * fo3(l)
    al_phot(l) = al_phot(l) * fo3(l)
  END DO

  !---------------------------------------------------------------------------
  ! Close stomata at points with negative or zero net photosynthesis
  ! or where the leaf resistance exceeds its maximum value.
  !---------------------------------------------------------------------------
  DO j = 1,open_pts
    l = veg_index(open_index(j))

    IF (gl_h2o(l) <= glmin(fn_type) .OR. al_phot(l) <= 0.0) THEN
      gl_h2o(l) = glmin(fn_type)
      al_phot(l) = -resp_d(l) * fo3(l)
    END IF
  END DO

END IF ! o3 damage

IF ( l_o3_damage ) THEN
  !---------------------------------------------------------------------------
  ! Diagnose the ozone deposition flux on all points
  !---------------------------------------------------------------------------
  DO j = 1,veg_pts
    l = veg_index(j)
    flux_o3(l) = o3mol(l) / (resist_a(l) + (ratio_o3 / gl_h2o(l)))
    resp_d(l)      = resp_d(l) * fo3(l)
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE leaf_processes_sox

SUBROUTINE w_rates_ci (land_field, veg_pts, veg_index, clos_pts, open_pts,     &
                       clos_index, open_index, fn_type, co2p_internal,         &
                       o2p_atmos, pstar, ccp, vcmax, k_o2, k_co2, acr, wcarb,  &
                       wexpt, wlite)

USE pftparm, ONLY: alpha, c3
USE jules_surface_mod, ONLY: fwe_c3, fwe_c4

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 fn_type                                                                       &
             ! IN Plant functional type.
,land_field                                                                    &
             ! IN Total number of land points.
,veg_pts                                                                       &
             ! IN Number of vegetated points.
,veg_index(land_field)                                                         &
             ! IN Index of vegetated points
             !    on the land grid.
,clos_index(land_field)                                                        &
             ! IN Index of land points
             !    with closed stomata.
,clos_pts                                                                      &
             ! IN Number of land points
             !    with closed stomata.
,open_index(land_field)                                                        &
             ! IN Index of land points
             !    with open stomata.
,open_pts
             ! IN Number of land points
             !    with open stomata.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 co2p_internal(land_field)                                                     &
             ! IN Internal CO2 pressure (Pa).
,acr(land_field)                                                               &
             ! IN Absorbed PAR
             !      (mol photons/m2/s).
,ccp(land_field)                                                               &
             ! IN Photorespiratory compensatory
             !      point (Pa).
,vcmax(land_field)                                                             &
             ! IN Maximum rate of carboxylation
             !      of Rubisco (mol CO2/m2/s).
,k_co2(land_field)                                                             &
             ! IN Michaelis-Menten constant for CO2 (Pa).
,k_o2(land_field)                                                              &
             ! IN Michaelis-Menten constant for O2 (Pa).
,o2p_atmos(land_field)                                                         &
             ! Atmospheric O2 pressure (Pa).
,pstar(land_field)
             ! Atmospheric pressure (Pa).
!------------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 wcarb(land_field)                                                             &
             ! OUT Carboxylation, ...
,wlite(land_field)                                                             &
             !     ... Light, and ...
,wexpt(land_field)
             !     ... export limited gross ...
             !     ... photosynthetic rates ...
             !     ... (mol CO2/m2/s).
!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 j,l                       ! Loop counters.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='W_RATES_CI'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF (c3(fn_type) == 1) THEN

  DO j = 1,open_pts
    l = veg_index(open_index(j))
    ! The numbers in these equations are from Cox, HCTN 24,
    ! "Description ... Vegetation Model", equations 54 and 55.
    wcarb(l) = vcmax(l) * (co2p_internal(l) - ccp(l)) /                        &
        (co2p_internal(l) + k_co2(l) * (1.0 + o2p_atmos(l) / k_o2(l)))
    wlite(l) = alpha(fn_type) * acr(l) * (co2p_internal(l) - ccp(l)) /         &
        (co2p_internal(l) + 2.0 * ccp(l))
    wlite(l) = MAX(wlite(l), TINY(1.0e0))
    wexpt(l) = fwe_c3 * vcmax(l)
  END DO

ELSE !  C4

  DO j = 1,open_pts
    l = veg_index(open_index(j))
    wcarb(l) = vcmax(l)
    wlite(l) = alpha(fn_type) * acr(l)
    wlite(l) = MAX(wlite(l), TINY(1.0e0))
    wexpt(l) = fwe_c4 * vcmax(l) * co2p_internal(l) / pstar(l)
  END DO

END IF
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE w_rates_ci


SUBROUTINE colim (land_field,veg_pts,veg_index, clos_pts, open_pts,            &
                  clos_index, open_index, wcarb, wexpt, wlite, resp_d,         &
                  al_phot)

USE jules_surface_mod, ONLY: beta1,beta2

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 land_field                                                                    &
             ! IN Total number of land points.
,veg_pts                                                                       &
             ! IN Number of vegetated points.
,veg_index(land_field)                                                         &
             ! IN Index of vegetated points
             !    on the land grid.
,clos_index(land_field)                                                        &
             ! IN Index of land points
             !    with closed stomata.
,clos_pts                                                                      &
             ! IN Number of land points
             !    with closed stomata.
,open_index(land_field)                                                        &
             ! IN Index of land points
             !    with open stomata.
,open_pts
             ! IN Number of land points
             !    with open stomata.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 resp_d(land_field)                                                            &
             ! IN Dark respiration (mol CO2/m2/s).
,wcarb(land_field)                                                             &
             ! IN Carboxylation, ...
,wlite(land_field)                                                             &
             !     ... Light, and ...
,wexpt(land_field)
             !     ... export limited gross ...
             !     ... photosynthetic rates ...
             !     ... (mol CO2/m2/s).

!-----------------------------------------------------------------------------
! Array arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 al_phot(land_field)
                            ! Net Leaf photosynthesis (mol CO2/m2/s).
!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 j,l                       ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
 b1_coef(land_field)                                                           &
,b2_coef(land_field)                                                           &
,b3_coef(land_field)                                                           &
                            ! WORK Coefficients of the quadratic.
,wl_phot(land_field)                                                           &
                            ! WORK Gross leaf phtosynthesis
                            !      (mol CO2/m2/s).
,wp_phot(land_field)
                            ! WORK Smoothed minimum of
                            !      Carboxylation and Light
                            !      limited gross photosynthesis
                            !      (mol CO2/m2/s).



INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='COLIM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
!-----------------------------------------------------------------------------
! Calculate the co-limited rate of gross photosynthesis.
! Note the colimitation parameters (beta1,beta2) are set to 1
! here (they are just replaced in the calculation).
! This is because the semi-analytical version of SOX calculates gs
! and this needs to be converted to co2p_internal.
!-----------------------------------------------------------------------------
DO j = 1,open_pts
  l = veg_index(open_index(j))

  b1_coef(l) = beta1
  b2_coef(l) = - (wcarb(l) + wlite(l))
  b3_coef(l) = wcarb(l) * wlite(l)

  wp_phot(l) = -b2_coef(l) / (2 * b1_coef(l))                                  &
          - SQRT(b2_coef(l) * b2_coef(l) /                                     &
          ( 4.0 * beta1 * beta1 ) - b3_coef(l) / b1_coef(l))

  b1_coef(l) = beta2
  b2_coef(l) = - (wp_phot(l) + wexpt(l))
  b3_coef(l) = wp_phot(l) * wexpt(l)

  wl_phot(l) = -b2_coef(l) / (2 * b1_coef(l))                                  &
          - SQRT(b2_coef(l) * b2_coef(l) /                                     &
          ( 4.0 * beta2 * beta2 ) - b3_coef(l) / b1_coef(l))

END DO

!-----------------------------------------------------------------------------
! Carry out calculations for points with open stomata
!-----------------------------------------------------------------------------
DO j = 1,open_pts
  l = veg_index(open_index(j))
  !---------------------------------------------------------------------------
  ! Calculate the net rate of photosynthesis
  !---------------------------------------------------------------------------
  al_phot(l) = (wl_phot(l) - resp_d(l))

  !----------------------------------------------------------------------
  ! Close stomata at points with negative or zero net photosynthesis
  !----------------------------------------------------------------------
  IF (al_phot(l) <= 0.0) THEN
    al_phot(l)    = -resp_d(l)
  END IF

END DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE colim
END MODULE leaf_processes_sox_mod
