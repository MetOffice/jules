! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in SURFACE

MODULE sf_stom_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_STOM_MOD'

PRIVATE
PUBLIC sf_stom

CONTAINS

! *********************************************************************
! Routines to calculate the bulk stomatal resistance and the canopy
! CO2 fluxes.
!
! References:
!   Bernacchi et al., 2001, Plant Cell and Environment, 24, 253--259,
!      https://doi.org/10.1111/j.1365-3040.2001.00668.x.
!   Medlyn et al., 2002, Plant, Cell and Environment, 25: 1167--1179,
!     https://doi.org/10.1046/j.1365-3040.2002.00891.x.
!   Mercado et al., 2018, New Phytologist, 218: 1462--1477,
!     https://doi.org/10.1111/nph.15100.
!
! *********************************************************************

SUBROUTINE sf_stom  (land_pts,land_index                                       &
,                    veg_pts,veg_index                                         &
,                    ft,co2,co2_3d,co2_dim_len                                 &
,                    co2_dim_row,l_co2_interactive                             &
,                    fsmc,veg_state,ht,ipar,lai                                &
,                    canht,pstar                                               &
,                    q1,ra,tstar,o3,t_home_gb,t_growth_gb                      &
,                    can_rad_mod,ilayers,faparv                                &
,                    psi_root_zone,lwp_c                                       &
,                    gpp,npp,resp_p,resp_l,resp_r,resp_w                       &
,                    growth_sug, f_nsc                                         &
,                    n_leaf,n_root,n_stem,lai_bal,gc                           &
,                    fapar_sun,fapar_shd,fsun                                  &
,                    flux_o3,fo3,fapar_diag,apar_diag                          &
,                    isoprene,terpene,methanol,acetone                         &
,                    open_index,open_pts,                                      &
                    !New arguments replacing USE statements
                    !crop_vars_mod (IN)
                    dvi_cpft,rootc_cpft)

USE leaf_mod, ONLY: leaf
USE leaf_limits_mod, ONLY: leaf_limits
USE leaf_processes_sox_mod, ONLY: leaf_processes_sox
USE bvoc_emissions_mod, ONLY: bvoc_emissions

USE conversions_mod, ONLY: zerodegc
USE theta_field_sizes, ONLY: t_i_length

USE jules_surface_types_mod, ONLY: nnpft, ncpft

USE jules_vegetation_mod, ONLY:                                                &
! imported parameters
    photo_collatz, photo_farquhar, photo_sox_collatz, stomata_medlyn,          &
    stomata_sox, photo_adapt, photo_acclim, photo_adapt_acclim,                &
    photo_act_model, photo_act_pft, photo_act_gb, n_photo_coef,                &
! imported scalars that are not changed
    dsj_coef, dsv_coef, jv25_coef, act_j_coef, act_v_coef,                     &
    l_bvoc_emis, l_fapar_diag, l_trait_phys, l_stem_resp_fix, l_o3_damage,     &
    l_scale_resp_pm, photo_acclim_model, photo_model, stomata_model, l_sugar,  &
    l_red

USE CN_utils_mod, ONLY:                                                        &
! imported procedures
    get_can_ave_fac, nleaf_from_lai

USE pftparm, ONLY:                                                             &
! imported arrays that are not changed
    a_wl, a_ws, act_jmax, act_vcmax, alpha_elec, b_wl, c3, deact_jmax,         &
    deact_vcmax, ds_jmax, ds_vcmax, eta_sl, kpar, nl0, nr_nl, ns_nl, omega,    &
    r_grow, sigl, lma, nmass, kn, knl, tupp, tlow,  q10_leaf, nsw, nr, hw_sw,  &
    jv25_ratio

USE ccarbon, ONLY:                                                             &
! imported scalar parameters
   epco2,epo2

USE c_rmol, ONLY: rmol

USE jules_surface_mod, ONLY:                                                   &
! imported scalar parameters
   iter,o2,cmass

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE crop_utils_mod, ONLY:                                                      &
   stemc_from_prognostics,                                                     &
   lma_from_prognostics
USE cropparm, ONLY: cfrac_l

USE qsat_mod, ONLY: qsat

USE ereport_mod, ONLY: ereport

USE veg3_field_mod, ONLY: veg_state_type

USE sugar_mod, ONLY: sugar

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(in).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 land_pts                                                                      &
                            ! IN Number of land points to be
!                                 !    processed.
,land_index(land_pts)                                                          &
                            ! IN Index of land points on the
!                                 !    P-grid.
,veg_pts                                                                       &
                            ! IN Number of vegetated points.
,veg_index(land_pts)                                                           &
                            ! IN Index of vegetated points
!                                 !    on the land grid.
,co2_dim_len                                                                   &
                            ! IN Length of a CO2 field row.
,co2_dim_row                ! IN Number of CO2 field rows.

INTEGER, INTENT(IN) ::                                                         &
 ft                         ! IN Plant functional type.

LOGICAL, INTENT(IN) :: l_co2_interactive   ! switch for 3D CO2 field

INTEGER, INTENT(IN) ::                                                         &
  can_rad_mod                                                                  &
!                           !Switch for canopy radiation model
 ,ilayers
!                           !No of layers in canopy radiation model

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 co2                                                                           &
                            ! IN Atmospheric CO2 concentration
,co2_3d(co2_dim_len,co2_dim_row)                                               &
!                                 ! IN 3D atmos CO2 concentration
!                                 !    (kg CO2/kg air).
,fsmc(land_pts)                                                                &
                            ! IN Soil water factor.
,ht(land_pts)                                                                  &
                            ! IN Canopy height (m).
,ipar(land_pts)                                                                &
                            ! IN Incident PAR (W/m2).
,lai(land_pts)                                                                 &
                            ! IN Leaf area index.
,canht(land_pts)                                                               &
                            ! IN Canopy Height
,pstar(land_pts)                                                               &
                            ! IN Surface pressure (Pa).
,faparv(land_pts,ilayers)                                                      &
                            ! IN Profile of absorbed PAR.
,fapar_shd(land_pts,ilayers)                                                   &
                            ! IN Profile of absorbed DIFF_PAR.
,fapar_sun(land_pts,ilayers)                                                   &
                            ! IN Profile of absorbed DIR_PAR.
,fsun(land_pts,ilayers)                                                        &
                            ! IN fraction of sunlit leaves
,q1(land_pts)                                                                  &
                            ! IN Specific humidity at level 1
,ra(land_pts)                                                                  &
                            ! IN Aerodynamic resistance (s/m).
,tstar(land_pts)                                                               &
                            ! IN Surface temperature (K).
,o3(land_pts)                                                                  &
                            ! IN Surface ozone concentration (ppb).
,t_home_gb(land_pts)                                                           &
                            ! IN Static (home) temperature for adaptation of
                            ! photosynthesis (K).
,t_growth_gb(land_pts)                                                         &
                            ! IN Running mean (growth) temperature for
                            ! acclimation of photosynthesis (K).
,psi_root_zone(land_pts)
                            ! IN (negative) soil water potential (in Pa)
                            ! in root zone. Only required when SOX is used

TYPE(veg_state_type), INTENT(IN OUT) :: veg_state

!-----------------------------------------------------------------------------
! Arguments with INTENT(out).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 gpp(land_pts)                                                                 &
                            ! OUT Gross Primary Productivity
!                                 !     (kg C/m2/s).
,npp(land_pts)                                                                 &
                            ! OUT Net Primary Productivity
!                                 !     (kg C/m2/s).
,resp_p(land_pts)                                                              &
                            ! OUT Plant respiration rate
!                                 !     (kg C/m2/sec).
,resp_r(land_pts)                                                              &
                            ! OUT Root respiration rate
!                                 !     (kg C/m2/sec).
,resp_l(land_pts)                                                              &
                            ! OUT Leaf maintanence respiration rate
!                                 !     (kg C/m2/sec).
,resp_w(land_pts)                                                              &
                            ! OUT Wood respiration rate
!                                 !     (kg C/m2/sec).
,growth_sug(land_pts)                                                          &
                            ! OUT Net structural C growth rate (SUGAR only)
                                  !     (kg C/m2/sec).
,flux_o3(land_pts)                                                             &
                            ! OUT Flux of O3 to stomata (nmol O3/m2/s).
,fo3(land_pts)                                                                 &
                            ! OUT Ozone exposure factor.
,fapar_diag(land_pts)                                                          &
                            ! OUT FAPAR diagnostic
,apar_diag(land_pts)                                                           &
                            ! OUT APAR diagnostic
,lwp_c(land_pts)
                            ! OUT Canopy leaf water potential (MPa)


REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 gc(land_pts)                                                                  &
                            ! INOUT Canopy resistance to H2O (m/s).
                            ! The input value is only used if can_rad_mod=1.
,f_nsc(land_pts)
                            ! INOUT Non-structural carbohydrate mass fraction
                            !      (kgC/kgC)

! BVOC variables
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 isoprene(land_pts)                                                            &
                   ! OUT Isoprene Emission Flux (kgC/m2/s)
,terpene(land_pts)                                                             &
                   ! OUT (Mono-)Terpene Emission Flux (kgC/m2/s)
,methanol(land_pts)                                                            &
                   ! OUT Methanol Emission Flux (kgC/m2/s)
,acetone(land_pts)
                   ! OUT Acetone Emission Flux (kgC/m2/s)

INTEGER, INTENT(OUT) ::                                                        &
open_index(land_pts)                                                           &
                            ! OUT Index of land points
!                                 !      with open stomata.
,open_pts                   ! OUT Number of land points
!                                 !      with open stomata.

!New arguments replacing USE statements
!crop_vars_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: dvi_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: rootc_cpft(land_pts,ncpft)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  cconu = 12.0e-3,                                                             &
    ! kg C in 1 mol CO2.
  conpar = 2.19e5,                                                             &
    ! Conversion from mol s-1 to W for PAR (J/mol photons).
  t_ref = zerodegc + 25.0,                                                     &
    ! Reference temperature (K).
  tref_rmol = t_ref * rmol
    ! The product of t_ref and rmol (J mol-1).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 i,j,k,l,m,n                                                                   &
                            ! WORK Loop counters.
,clos_pts                                                                      &
                            ! WORK Number of land points
!                                 !      with closed stomata.
,errcode                                                                       &
                            ! Error code to pass to ereport.
,pft_photo_model
                            ! Indicates which photosynthesis model to use for
                            ! the current PFT.

REAL(KIND=real_jlslsm) ::                                                      &
 dq_min                                                                        &
   ! Minimum-allowed specific humidity deficit (kg H20 vapour/kg air).
,expkn                                                                         &
   ! Decay term.
,fstem                                                                         &
   ! Ratio of respiring stem wood to total wood.
,jmax_numerator                                                                &
   ! Numerator term in calculation of Jmax.
,kc_val                                                                        &
   ! Michaelis-Menten constant for CO2 (Pa) - for a single point.
,ko_val                                                                        &
   ! Michaelis-Menten constant for O2 (Pa) - for a single point.
,lma_tmp                                                                       &
   ! Temporary leaf mass per area for crops (kg leaf per m2 leaf area).
,power                                                                         &
   ! Exponent used in Q10 term.
,stem_resp_scaling                                                             &
   ! Scaling factor to reduce stem respiration
,stemc                                                                         &
   ! Stem carbon (kg m-2)
,sun_term                                                                      &
   ! Conversion from PAR to electron flux (mol electrons J-1).
,t_minus_ref                                                                   &
   ! Temperature relative to the reference (K).
,t_term                                                                        &
   ! A temperature-related term (mol J-1).
,tau                                                                           &
   ! Rubisco specificty for CO2 relative to O2.
,tdegc                                                                         &
   ! Temperature (deg C).
,th_degc, tg_degc                                                              &
   ! Temperatures t_home_gb and t_growth_gb in degrees Celsius.
,vcmax_numerator
   ! Numerator term in calculation of Vcmax.

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
 clos_index(land_pts)
                            ! WORK Index of land points
!                                 !      with closed stomata.

REAL(KIND=real_jlslsm) ::                                                      &
 anetc(land_pts)                                                               &
                            ! WORK Net canopy photosynthesis
!                                 !     (mol CO2/m2/s).
,co2c(land_pts)                                                                &
                            ! WORK Canopy level CO2 concentration
!                                 !      (kg CO2/kg air).
,ci(land_pts)                                                                  &
                            ! WORK Internal CO2 pressure (Pa).
,dq(land_pts)                                                                  &
                            ! WORK Specific humidity deficit
!                                 !      (kg H2O/kg air).
,dqc(land_pts)                                                                 &
                            ! WORK Canopy level specific humidity
!                                 !      deficit (kg H2O/kg air).
,fpar(land_pts)                                                                &
                            ! WORK PAR absorption factor.
,i2_shd (land_pts)                                                             &
                            ! Radiation that goes to Photosystem II for
                            ! shaded leaves, expressed as an electron flux
                            ! (mol electrons m-2 s-1).
,i2_sun(land_pts)                                                              &
                            ! Radiation that goes to Photosystem II for
                            ! sunlit leaves, expressed as an electron flux
                            ! (mol electrons m-2 s-1).
,lai_bal(land_pts)                                                             &
                            ! WORK Leaf area index in balanced
!                                 !      growth state.
,nleaf_top(land_pts)                                                           &
                            ! WORK Nitrogen concentration of top leaf.
!                           ! if(l_trait_phys)= g N/m2
                            ! else= kg N/kg C
,n_leaf(land_pts)                                                              &
                            ! WORK Nitrogen contents of the leaf,
,n_root(land_pts)                                                              &
                            !      root,
,n_stem(land_pts)                                                              &
                            !      and stem (kg N/m2).
,leafc(land_pts)                                                               &
                            ! WORK Leaf Carbon (kg C/m2)
,leafc_bal(land_pts)                                                           &
                            ! WORK Carbon of balanced LAI (kg C/m2)
,woodc(land_pts)                                                               &
                            ! WORK Wood Carbon (kg C/m2)
,rootc(land_pts)                                                               &
                            ! WORK Root Carbon (kg C/m2).
,qs(land_pts)                                                                  &
                            ! WORK Saturated specific humidity
!                                 !      (kg H2O/kg air).
,ra_rc(land_pts)                                                               &
                            ! WORK Ratio of aerodynamic resistance
!                                 !      to canopy resistance.
,rdc(land_pts)                                                                 &
                            ! WORK Canopy dark respiration,
!                                 !      without soil water dependence
!                                 !      (mol CO2/m2/s).
,rdmean(land_pts)                                                              &
                            ! WORK Mean dark respiration
!                                 !      per unit leaf area
!                                 !      over canopy
!                                 !      without soil water dependence
!                                 !   (mol CO2/s per m2 leaf area).
,nlmean(land_pts)                                                              &
                            ! WORK Mean nitrogen per unit leaf area
!                                 !      over canopy
!                                 !      without soil water dependence
!                                 !      (kg N per m2 leaf area).
!                                 !      Used in place of n_leaf when LAI
!                                 !      is very small to avoid blowing up
!                                 !      the calculation of respiration.
,resp_p_g(land_pts)                                                            &
                            ! WORK Plant growth respiration rate
!                                 !      (kg C/m2/sec).
,resp_p_m(land_pts)                                                            &
                            ! WORK Plant maintenance respiration
!                                 !      rate (kg C/m2/sec).
,root(land_pts)                                                                &
                            ! WORK Root carbon (kg C/m2).
,faparv_layer(land_pts,ilayers)                                                &
                            ! WORK absorbed par(layers)
,flux_o3_l(land_pts)                                                           &
                            ! WORK Flux of O3 to stomata (nmol O3/m2/s).
,flux_o3_l_sun(land_pts)                                                       &
                            ! WORK Flux of O3 to stomata
                            !      for sunlit leaves
!                             !      (for can_rad_mod=5)
                            !      (nmol O3/m2/s).
,flux_o3_l_shd(land_pts)                                                       &
                            ! WORK Flux of O3 to stomata
                            !      for shaded leaves
                            !      (for can_rad_mod=5)
                            !      (nmol O3/m2/s).
,fo3_l(land_pts)                                                               &
                            ! WORK Ozone exposure factor.
,fo3_l_sun(land_pts)                                                           &
                            ! WORK Ozone exposure factor
                            !      for sunlit leaves
                            !      (for can_rad_mod=5)
,fo3_l_shd(land_pts)                                                           &
                            ! WORK Ozone exposure factor
                            !      for shaded leaves
                            !      (for can_rad_mod=5)
,o3mol(land_pts)                                                               &
                            ! WORK Surface ozone concentration (moles).
,fsmc_scale(land_pts)                                                          &
                            ! WORK Scaling of stem and root plant maintenance
                            ! respiration.
,denom(land_pts)                                                               &
   ! Denominator in temperature-dependency of Vcmax (Collatz model only).
,dsj(land_pts)                                                                 &
   ! Entropy factor for Jmax, including any acclimation (J mol-1 K-1).
,dsv(land_pts)                                                                 &
   ! Entropy factor for Vcmax, including any acclimation (J mol-1 K-1).
,actj(land_pts)                                                                &
   ! Activation energy for Jmax, including any acclimation (J mol-1).
,actv(land_pts)                                                                &
   ! Activation energy for Vcmax, including any acclimation (J mol-1).
,ccp(land_pts)                                                                 &
   ! Photorespiratory compensatory point (Pa). This is zero for C4 plants.
,i2(land_pts)                                                                  &
   ! Radiation that goes to Photosystem II, expressed as an electron flux
   ! (mol electrons m-2 s-1).
,je(land_pts)                                                                  &
   ! Electron transport rate (mol m-2 s-1).
,je_shd(land_pts)                                                              &
   ! Electron transport rate for shaded leaves (mol m-2 s-1).
,je_shd_ratio(land_pts)                                                        &
   ! Ratio je_shd : je.
,je_sun(land_pts)                                                              &
   ! Electron transport rate for sunlit leaves (mol m-2 s-1).
,je_sun_ratio(land_pts)                                                        &
   ! Ratio je_sun : je.
,jmax(land_pts)                                                                &
   ! Maximum rate of electron transport (mol CO2 m-2 s-1).
,jv25(land_pts)                                                                &
   ! Ratio of Jmax to Vcmax at 25 degC, including any acclimation.
,kc(land_pts)                                                                  &
   ! Michaelis-Menten constant for CO2 (Pa).
,km(land_pts)                                                                  &
   ! A combination of Michaelis-Menten and other terms.
,ko(land_pts)                                                                  &
   ! Michaelis-Menten constant for O2 (Pa).
,jmax_temp(land_pts)                                                           &
   ! Factor expressing the effect of temperature on Jmax.
,qtenf_term(land_pts)                                                          &
   ! Q10 temperature term used for Vcmax.
,vcmax_temp(land_pts)                                                          &
   ! Factor expressing the effect of temperature on Vcmax.
,vcmax(land_pts)                                                               &
   ! Maximum rate of carboxylation of Rubisco (mol CO2/m2/s).
,anetl(land_pts)                                                               &
                            ! WORK Net leaf photosynthesis
!                                 !      (mol CO2/m2/s/LAI).
,anetl_sun(land_pts)                                                           &
!                                 ! WORK Net leaf photosynthesis of
!                                 !      sunlit leaves
!                                 !      (mol CO2/m2/s/LAI)
,anetl_shd(land_pts)                                                           &
!                                 ! WORK Net leaf photosynthesis of
!                                 !      shaded leaves
!                                 !      (mol CO2/m2/s/LAI
,apar(land_pts)                                                                &
!                                 ! WORK PAR absorbed by the top leaf
!                                 !      (W/m2).
,acr(land_pts)                                                                 &
                            ! WORK Absorbed PAR
!                                 !      (mol photons/m2/s).
,ca(land_pts)                                                                  &
                            ! WORK Canopy level CO2 pressure
!                                 !      (Pa).
,gl(land_pts)                                                                  &
                            ! WORK Leaf conductance for H2O
!                                 !      (m/s).
,gl_sun(land_pts)                                                              &
                            ! WORK Leaf conductance for H2O of
!                                 !      sunlit leaves (m/s).
,gl_shd(land_pts)                                                              &
                            ! WORK Leaf conductance for H2O of
!                                 !      shaded leaves (m/s).
,icr(land_pts)                                                                 &
                            ! WORK Incident PAR (mol photons/m2/s).
,oa(land_pts)                                                                  &
                            ! WORK Atmospheric O2 pressure
!                                 !      (Pa).
,rd(land_pts)                                                                  &
                            ! WORK Dark respiration, including any effect of
                            !      light inhibition (mol CO2/m2/s).
,rd_dark(land_pts)                                                             &
                            ! WORK Dark respiration, excluding effect of
                            !      light inhibition (mol CO2/m2/s).
,rd_sun(land_pts)                                                              &
                            ! WORK Dark respiration of sunlit leaves
!                                 !      (mol CO2/m2/s).
,rd_shd(land_pts)                                                              &
                            ! WORK Dark respiration of shaded leaves
!                                 !      (mol CO2/m2/s).

,wcarb(land_pts)                                                               &
                            ! WORK Carboxylation, ...
,wlite(land_pts)                                                               &
                            !      ... Light, and ...
,wexpt(land_pts)                                                               &
                            !      ... export limited gross ...
!                                 !      ... photosynthetic rates ...
!                                 !      ... (mol CO2/m2/s).
,wlitev(land_pts)                                                              &
!                                 ! WORK Light limited gross
!                                 !      photosynthetic rates
!                                 !      for each layer
!                                 !      (mol CO2/m2/s).
,wlitev_sun(land_pts)                                                          &
!                                 ! WORK Light limited gross
!                                 !      photosynthetic rates
!                                 !      for sunlit leaves
!                                 !      (mol CO2/m2/s).
,wlitev_shd(land_pts)                                                          &
!                                 ! WORK Light limited gross
!                                 !      photosynthetic rates
!                                 !      for shaded leaves
!                                 !      (mol CO2/m2/s).
,dlai(land_pts)                                                                &
                            ! WORK LAI Increment.
,nleaf_layer(land_pts)                                                         &
                            ! WORK Leaf nitrogen concentration in a layer.
                            ! kgN/kgC if l_trait_phys=F
                            ! gN/m2   if l_trait_phys=T.
,can_averaging_fac(land_pts)
                            ! WORK factor to convert top of canopy
                            ! value to canopy average.

REAL(KIND=real_jlslsm) ::                                                      &
 act_j_tmp(n_photo_coef)                                                       &
   ! Coefficients governing the acclimation of activation energy for Jmax.
,act_v_tmp(n_photo_coef)
   ! Coefficients governing the acclimation of activation energy for Vcmax.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_STOM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisation.
!
! Note that the initialisation is appropriate for the existing code! If an
! existing variable is used differently (e.g. it is made available as a
! diagnostic) it might need to be treated differently - e.g. initialised.
!
! In general we only initialise output variables (diagnostics and/or those
! required elsewhere - so that these have a defined value at all location)
! and any local variables that need to be initialised (e.g. so as to allow
! accumulation over canopy layers). Many other variables are not initialised
! because they are always calculated at all locations for which a value is
! required (e.g. all vegetated points, or all points with open stomata).
!-----------------------------------------------------------------------------

!$OMP PARALLEL DO IF(land_pts > 1)                                             &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(land_pts,l_o3_damage,fo3,flux_o3,isoprene,                        &
!$OMP        terpene,methanol,acetone,fsmc_scale,                              &
!$OMP        stomata_model,lwp_c)                                              &
!$OMP PRIVATE(l)                                                               &
!$OMP SCHEDULE(STATIC)
DO l = 1, land_pts

  ! Ozone variables that are output.
  flux_o3(l)    = 0.0
  IF ( l_o3_damage ) THEN
    ! Initialise to zero to allow accumulation.
    fo3(l)      = 0.0
  ELSE
    ! Initialise to 1 to indicate no effect.
    fo3(l)      = 1.0
  END IF

  ! BVOC fluxes: values used at non-vegetated points or if fluxes are not
  ! calculated (l_bvoc_emis=F).
  isoprene(l)   = 0.0
  terpene(l)    = 0.0
  methanol(l)   = 0.0
  acetone(l)    = 0.0

  fsmc_scale(l) = 1.0  !  Value used if l_scale_resp_pm =.FALSE..
  ! leaf water potential variables for lwp_c diagnostic if SOX is used
  IF ( stomata_model == stomata_sox ) THEN
    ! initialise to zero to allow accumulation
    lwp_c(l) = 0.0
  END IF
END DO
!$OMP END PARALLEL DO

!-----------------------------------------------------------------------------
! Initialise variables that are accumulated over layers.
! Note that some or all of these variables are also used by the big-leaf
! model (can_rad_mod=1) but do not need to be initialised in that case.
!-----------------------------------------------------------------------------
SELECT CASE ( can_rad_mod )
CASE ( 4,5,6 )
!$OMP PARALLEL DO IF(land_pts > 1)                                             &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(land_pts,anetc,gc,rdc,rdmean)                                     &
!$OMP PRIVATE(l)                                                               &
!$OMP SCHEDULE(STATIC)
  DO l = 1, land_pts
    anetc(l)  = 0.0
    gc(l)     = 0.0
    rdc(l)    = 0.0
    rdmean(l) = 0.0
  END DO
!$OMP END PARALLEL DO
END SELECT

!-----------------------------------------------------------------------------
! Decide which photosynthesis model will be used for this PFT.
!-----------------------------------------------------------------------------
SELECT CASE ( photo_model )
CASE ( photo_collatz )
  ! C3 and C4 both use a Collatz model.
  pft_photo_model = photo_collatz
CASE ( photo_farquhar )
  ! C3 uses Farquhar, C4 uses Collatz.
  IF ( c3(ft) == 1 ) THEN
    pft_photo_model = photo_farquhar
  ELSE
    pft_photo_model = photo_collatz
  END IF
CASE ( photo_sox_collatz )
  pft_photo_model = photo_collatz
  ! For the purposes of initialisation these are both photo_collatz
END SELECT

!-----------------------------------------------------------------------------
! Initialise ratios of electron fluxes.
!-----------------------------------------------------------------------------
IF ( pft_photo_model == photo_farquhar ) THEN
  SELECT CASE ( can_rad_mod )
  CASE ( 5, 6 )
!$OMP PARALLEL DO IF(land_pts > 1)                                             &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(land_pts, je_shd_ratio, je_sun_ratio)                             &
!$OMP PRIVATE(l)                                                               &
!$OMP SCHEDULE(STATIC)
    DO l = 1, land_pts
      je_shd_ratio(l)  = 0.0
      je_sun_ratio(l)  = 0.0
    END DO
!$OMP END PARALLEL DO
  END SELECT
END IF

!-----------------------------------------------------------------------------
! Set the CO2 compensation point to zero for C4 plants.
!-----------------------------------------------------------------------------
IF ( c3(ft) == 0 ) THEN
  ccp(:) = 0.0
END IF

!-----------------------------------------------------------------------------
! Calculate humidity at saturation.
!-----------------------------------------------------------------------------
CALL qsat(qs,tstar,pstar,land_pts)

! Set the minimum-allowed humidity deficit.
IF ( ( stomata_model == stomata_medlyn ) .OR. ( stomata_model == stomata_sox ) ) THEN
  ! Avoid dq=0 as this would cause the model to blow up.
  dq_min = 0.0001
ELSE
  dq_min = 0.0
END IF

!-----------------------------------------------------------------------------
! Set the canopy CO2 concentration.
!-----------------------------------------------------------------------------
!$OMP PARALLEL IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(i, j, l, m, lma_tmp)      &
!$OMP SHARED(l_co2_interactive, l_o3_damage, acr, veg_pts, veg_index,          &
!$OMP        land_index, t_i_length, co2c, co2_3d, co2, dq, qs, q1,            &
!$OMP        can_rad_mod, fpar, icr, kpar, lai, ft, apar, omega, ipar,         &
!$OMP        l_trait_phys, nnpft, dvi_cpft, nleaf_top, nmass, nl0,             &
!$OMP        dlai, ilayers, o3mol, o3, pstar, tstar, lma, ca, stomata_model,   &
!$OMP        dq_min, c3, oa)
IF ( l_co2_interactive ) THEN
  !       Use full 3D CO2 field.
!$OMP  DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)
    j = (land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    co2c(l) = co2_3d(i,j)
  END DO
!$OMP END DO NOWAIT
ELSE
  !       Use single CO2_MMR value.
!$OMP DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)
    co2c(l) = co2
  END DO
!$OMP END DO NOWAIT
END IF

!-----------------------------------------------------------------------------
! Calculate the surface to level 1 humidity deficit.
!-----------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO m = 1,veg_pts
  l = veg_index(m)
  dq(l) = MAX(dq_min,(qs(l) - q1(l)))
END DO
!$OMP END DO NOWAIT

!-----------------------------------------------------------------------------
! Calculate the PAR absorption factor
!-----------------------------------------------------------------------------
IF ( can_rad_mod == 1 ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)
    fpar(l) = (1.0 - EXP(-kpar(ft) * lai(l))) / kpar(ft)
  END DO
!$OMP END DO NOWAIT
END IF

!-----------------------------------------------------------------------------
! Calculate the PAR absorbed by the top leaf and set top leaf N value.
!-----------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO m  = 1,veg_pts
  l = veg_index(m)
  apar(l) = (1.0 - omega(ft)) * ipar(l)
  acr(l)  = apar(l) / conpar
  IF ( l_trait_phys ) THEN
    IF ( ft > nnpft ) THEN
      lma_tmp = lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft))
      nleaf_top(l) = nmass(ft) * lma_tmp * 1000.0
        ! gN/gleaf * gleaf/m2 = gN/m2
    ELSE
      nleaf_top(l) = nmass(ft) * lma(ft) * 1000.0
        ! gN/gleaf * gleaf/m2 = gN/m2
    END IF
  ELSE
    nleaf_top(l) = nl0(ft)
  END IF
END DO
!$OMP END DO NOWAIT

!-----------------------------------------------------------------------------
! Calculate incoming PAR (mol photons m-2 s-1).
!-----------------------------------------------------------------------------
SELECT CASE ( can_rad_mod )
CASE ( 5:6 )
!$OMP DO SCHEDULE(STATIC)
  DO m  = 1,veg_pts
    l = veg_index(m)
    icr(l) = ipar(l) / conpar
  END DO
!$OMP END DO NOWAIT
END SELECT

!-----------------------------------------------------------------------------
! Calculate the LAI in each canopy layer.
!-----------------------------------------------------------------------------
SELECT CASE ( can_rad_mod )
CASE ( 4:6 )
!$OMP DO SCHEDULE(STATIC)
  DO m  = 1,veg_pts
    l = veg_index(m)
    dlai(l) = lai(l) / REAL(ilayers)
  END DO
!$OMP END DO NOWAIT
END SELECT

!-----------------------------------------------------------------------------
! Convert O3 concentration from ppb to moles.
!-----------------------------------------------------------------------------
IF ( l_o3_damage ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO m  = 1,veg_pts
    l = veg_index(m)
    o3mol(l) = o3(l) * pstar(l) / (rmol * tstar(l))
  END DO
!$OMP END DO NOWAIT
END IF

!-----------------------------------------------------------------------------
! Calculate partial pressure of oxygen.
!-----------------------------------------------------------------------------
IF ( c3(ft) == 1 ) THEN
  ! Calculate partial pressure of oxygen.
!$OMP DO SCHEDULE(STATIC)
  DO m  = 1,veg_pts
    l = veg_index(m)
    ! Convert O2 from mass fraction to partial pressure.
    oa(l)  = o2 / epo2 * pstar(l)
  END DO
!$OMP END DO
END IF  !  C3

!-----------------------------------------------------------------------------
! Convert CO2 from mass fraction to partial pressure.
! We ignore the (small) difference between the canopy- and
! reference-level CO2 concentrations.
!-----------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO m  = 1,veg_pts
  l = veg_index(m)
  ca(l) = co2c(l) / epco2 * pstar(l)
END DO
!$OMP END DO
!$OMP END PARALLEL

!-----------------------------------------------------------------------------
! Pre-calculate some expensive terms that do not change between iterations
! and layers.
! tstar is for the current pft, so can't be moved up another level.
!-----------------------------------------------------------------------------

SELECT CASE ( pft_photo_model )

CASE ( photo_collatz )
  ! Use the Collatz model (for C3 or C4 plants).
!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l,m,power,tau,tdegc)   &
!$OMP SHARED(c3, veg_pts, veg_index, ccp, denom, ft, kc, ko, oa, tlow,         &
!$OMP        q10_leaf,  qtenf_term, tstar, tupp) SCHEDULE(STATIC)
  DO m  = 1,veg_pts
    l = veg_index(m)
    tdegc         = tstar(l) - zerodegc
    power         = 0.1 * (tdegc- 25.0)
    denom(l)      = (1.0 + EXP (0.3 * (tdegc - tupp(ft)))) *                   &
                    (1.0 + EXP (0.3 * (tlow(ft) - tdegc)))
    qtenf_term(l) = q10_leaf(ft)** power

    IF ( c3(ft) == 1 ) THEN
      ! Calculate terms that are only needed for C3 plants.
      ! Although oa, kc and ko are always used together we keep them separate
      ! to maintain bit comparability.
      tau    = 2600.0  * (0.57 ** power)
      ccp(l) = 0.5 * oa(l) / tau
      kc(l)  = 30.0    * (2.1 ** power)
      ko(l)  = 30000.0 * (1.2 ** power)
    END IF

  END DO
!$OMP END PARALLEL DO

CASE ( photo_farquhar )
  ! Use the Farquhar model (for C3 plants).
  ! Calculate a constant.
  sun_term = alpha_elec(ft) / conpar

  ! Load parameter values, depending on options.
  SELECT CASE ( photo_acclim_model )
  CASE ( 0 )
    ! No acclimation.
    ! Copy the PFT parameters, including fixed J:V.
!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l,m)                   &
!$OMP SHARED(ds_jmax, ds_vcmax, dsj, dsv, ft, jv25, jv25_ratio,                &
!$OMP        actj, act_jmax, actv, act_vcmax, veg_index, veg_pts)              &
!$OMP SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      dsj(l)  = ds_jmax(ft)
      dsv(l)  = ds_vcmax(ft)
      jv25(l) = jv25_ratio(ft)
      actj(l) = act_jmax(ft)
      actv(l) = act_vcmax(ft)
    END DO
!$OMP END PARALLEL DO

  CASE ( photo_adapt, photo_acclim, photo_adapt_acclim )
    ! These use the same forms but t_growth_gb will generally be
    ! different. Although there is no dependency on PFT here (meaning
    ! this could be moved up and out of a PFT loop), we leave it here
    ! so that these parameters are calculated here regardless of the
    ! acclimation model selected.

    ! Decide whether the activation energies are subject to acclimation.  If
    ! they are, then the energies vary by gridbox but not by PFT, otherwise
    ! the energies vary by PFT but not by gridbox.
    SELECT CASE ( photo_act_model )
    CASE ( photo_act_pft )
      act_j_tmp(:) = [act_jmax(ft), 0.0, 0.0]
      act_v_tmp(:) = [act_vcmax(ft), 0.0, 0.0]
    CASE ( photo_act_gb )
      act_j_tmp(:) = act_j_coef(:)
      act_v_tmp(:) = act_v_coef(:)
    CASE DEFAULT
      errcode = 101  !  a hard error
      CALL ereport(RoutineName, errcode,                                       &
                   'photo_act_model should be photo_act_pft or photo_act_gb')
    END SELECT

!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l,m,th_degc,tg_degc)   &
!$OMP SHARED(dsj, dsj_coef, dsv, dsv_coef, jv25, jv25_coef,                    &
!$OMP        actj, act_j_tmp, actv, act_v_tmp,                                 &
!$OMP        t_home_gb, t_growth_gb, veg_index, veg_pts)                       &
!$OMP SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      th_degc = t_home_gb(l) - zerodegc
      tg_degc = t_growth_gb(l) - zerodegc
      dsj(l)  = dsj_coef(1) + dsj_coef(2) * th_degc + dsj_coef(3) * tg_degc
      dsv(l)  = dsv_coef(1) + dsv_coef(2) * th_degc + dsv_coef(3) * tg_degc
      jv25(l) = jv25_coef(1) + jv25_coef(2) * th_degc + jv25_coef(3) * tg_degc
      actj(l) = act_j_tmp(1) + act_j_tmp(2) * th_degc + act_j_tmp(3) * tg_degc
      actv(l) = act_v_tmp(1) + act_v_tmp(2) * th_degc + act_v_tmp(3) * tg_degc
    END DO
!$OMP END PARALLEL DO

  END SELECT  !  photo_acclim_model

!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE)                                &
!$OMP PRIVATE(l, m, jmax_numerator, kc_val, ko_val, t_minus_ref, t_term,       &
!$OMP         vcmax_numerator)                                                 &
!$OMP SHARED(c3, veg_pts, veg_index, acr, actj, actv, alpha_elec,              &
!$OMP        ccp, deact_jmax, deact_vcmax, dsj, dsv, ft, i2, jmax_temp, km,    &
!$OMP        oa, q10_leaf, qtenf_term, tstar, vcmax_temp) SCHEDULE(STATIC)
  DO m = 1,veg_pts

    l = veg_index(m)
    ! Temperature responses of carboxylation, oxygenation,and CO2 compensation
    ! point, from Bernacchi et al. (2001).
    t_minus_ref = tstar(l) - t_ref
    t_term      = t_minus_ref / ( tref_rmol * tstar(l) )
    ccp(l)      = 4.73078 * EXP( 37830.0 * t_term )
    ! For the Farquhar model we combine oa, kc and ko into km.
    kc_val      = 44.8    * EXP( 79430.0 * t_term )
    ko_val      = 30808.2 * EXP( 36380.0 * t_term )
    km(l)       = kc_val * ( 1.0 + oa(l) / ko_val )
    ! Radiation that goes to Photosystem II.
    i2(l)       = alpha_elec(ft) * acr(l)

    ! Calculate the temperature response of Vcmax and Jmax, Eq.17 of
    ! Medlyn et al. (2002).
    vcmax_numerator = EXP( actv(l) * t_minus_ref                               &
                           / ( tref_rmol * tstar(l) ) )                        &
                      * ( 1.0 + EXP( ( t_ref * dsv(l) - deact_vcmax(ft) )      &
                                     / tref_rmol ) )
    vcmax_temp(l)   = vcmax_numerator                                          &
                      / ( 1.0 + EXP( ( tstar(l) * dsv(l) - deact_vcmax(ft) )   &
                                     / ( tstar(l) * rmol ) ) )

    jmax_numerator = EXP( actj(l) * t_minus_ref                                &
                           / ( tref_rmol * tstar(l) ) )                        &
                      * ( 1.0 + EXP( ( t_ref * dsj(l) - deact_jmax(ft) )       &
                                     / tref_rmol ) )
    jmax_temp(l)   = jmax_numerator                                            &
                     / ( 1.0 + EXP( ( tstar(l) * dsj(l) - deact_jmax(ft) )     &
                                    / ( tstar(l) * rmol ) ) )

  END DO
!$OMP END PARALLEL DO

CASE DEFAULT
  errcode = 101  !  a hard error
  CALL ereport(RoutineName, errcode,                                           &
               'pft_photo_model should be photo_collatz or photo_farquhar')

END SELECT  !  pft_photo_model

!-----------------------------------------------------------------------------
! Calculate fluxes.
!-----------------------------------------------------------------------------

SELECT CASE ( can_rad_mod )

CASE ( 4 )

  !---------------------------------------------------------------------------
  ! Varying N model+altered leaf respiration
  ! Multiple canopy layers
  ! N varies through canopy as exponential function of layers.
  !---------------------------------------------------------------------------

  DO n = 1,ilayers
    !-------------------------------------------------------------------------
    ! Initialise GL and calculate the PAR absorbed in this layer.
    !-------------------------------------------------------------------------
    expkn = EXP( REAL(n-1) / REAL(ilayers) * (-kn(ft)) )
!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                  &
!$OMP             SHARED(dlai, expkn, faparv, faparv_layer, gl, nleaf_layer, n,&
!$OMP                    nleaf_top, veg_index, veg_pts)                        &
!$OMP             SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      gl(l)             = 0.0
      nleaf_layer(l)    = nleaf_top(l) * expkn
      faparv_layer(l,n) = faparv(l,n) * dlai(l)
    END DO
!$OMP END PARALLEL DO

    !-------------------------------------------------------------------------
    ! Calculate photosynthetic parameters.
    !-------------------------------------------------------------------------
    CALL calc_photo_parameters( ft, land_pts, pft_photo_model, veg_pts,        &
                                veg_index, denom, jmax_temp, jv25,             &
                                nleaf_layer, qtenf_term, vcmax_temp,           &
                                jmax, rd_dark, vcmax )

    !-------------------------------------------------------------------------
    ! Iterate to ensure that the canopy humidity deficit is consistent with
    ! the H2O flux.
    !-------------------------------------------------------------------------

    DO k = 1,iter
      !-----------------------------------------------------------------------
      ! Diagnose the canopy-level humidity deficit.
      ! Initialise dark respiration with the uninhibited rate.
      !-----------------------------------------------------------------------
!$OMP PARALLEL IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                     &
!$OMP          SHARED(dq, dqc, gl, ra, ra_rc, rd, rd_dark, veg_index, veg_pts)
!$OMP DO SCHEDULE(STATIC)
      DO m = 1,veg_pts
        l = veg_index(m)
        ra_rc(l) = ra(l) * gl(l)
        dqc(l)   = dq(l) / (1.0 + ra_rc(l))
        rd(l)    = rd_dark(l)
      END DO
!$OMP END DO
!$OMP END PARALLEL

      !-----------------------------------------------------------------------
      ! Calculate the limiting factors for leaf photosynthesis
      !-----------------------------------------------------------------------
      CALL leaf_limits (ft, land_pts, pft_photo_model ,veg_pts, veg_index      &
,                       acr, apar, ca, ccp, dqc, fsmc, je, kc, km, ko, oa      &
,                       pstar, vcmax                                           &
,                       clos_pts, open_pts, clos_index, open_index             &
,                       ci, wcarb, wexpt, wlite )

!$OMP PARALLEL DO IF(open_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                 &
!$OMP             SHARED(acr, apar, faparv, faparv_layer, ipar, land_index,    &
!$OMP                    n, open_index, open_pts, rd, t_i_length,              &
!$OMP                    wlite, wlitev, veg_index) SCHEDULE(STATIC)
      DO m = 1,open_pts
        l = veg_index(open_index(m))
        wlitev(l) = wlite(l) / apar(l) * faparv(l,n) * ipar(l)
        ! Calculate light inhibition of dark respiration.
        ! This does not change between iterations (though open_index might).
        IF (acr(l) * 1.0e6 * faparv_layer(l,n) >  10.0) THEN
          rd(l) = ( 0.5 - 0.05 * LOG(acr(l) * faparv_layer(l,n) * 1.0e6) )     &
                  * rd(l)
        END IF
      END DO
!$OMP END PARALLEL DO

      !-----------------------------------------------------------------------
      ! Calculate leaf-level fluxes.
      !-----------------------------------------------------------------------
      CALL leaf (clos_pts, ft, land_pts, open_pts, pft_photo_model, veg_pts    &
,                clos_index, open_index, veg_index                             &
,                ca, ci, fsmc, o3mol, ra, tstar, wcarb, wexpt, wlitev, rd      &
,                anetl, flux_o3_l, fo3_l, gl)

    END DO                 ! K-ITER

    !-------------------------------------------------------------------------
    ! Add to canopy-level values.
    !-------------------------------------------------------------------------
!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                  &
!$OMP             SHARED(anetc, anetl, dlai, flux_o3, flux_o3_l, fo3, fo3_l,   &
!$OMP                    rdmean,ilayers,gc, gl, rd, rdc, veg_index, veg_pts,   &
!$OMP                    LAI,l_o3_damage                                     ) &
!$OMP             SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      anetc(l) = anetc(l) + anetl(l) * dlai(l)
      gc(l)    = gc(l)    + gl(l) * dlai(l)
      rdc(l)   = rdc(l)   + rd(l) * dlai(l)

      rdmean(l) = rdmean(l) + rd(l) / REAL(ilayers)

      IF (l_o3_damage) THEN
        flux_o3(l) = flux_o3(l) + flux_o3_l(l) * dlai(l)
        fo3(l)     = fo3(l) + fo3_l(l) * dlai(l) / lai(l)
      END IF
    END DO
!$OMP END PARALLEL DO

  END DO                   ! N LAYERS


CASE ( 5, 6 )

  !---------------------------------------------------------------------------
  ! Sunlit and shaded leaves treated separately
  ! Multiple canopy layers
  ! N varies through canopy as exponential
  !---------------------------------------------------------------------------

  DO n = 1,ilayers

    !-------------------------------------------------------------------------
    ! Initialise GL for this layer.
    ! We could initialise to gl(n-1) here, but simpler to use zero
    ! and seems to converge pretty quickly anyway.
    !-------------------------------------------------------------------------
!$OMP PARALLEL DO IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                  &
!$OMP             SHARED(ft, gl, ilayers, kn, knl, n, nleaf_top, nleaf_layer,  &
!$OMP   veg_index,dlai,can_rad_mod, veg_pts) SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      gl(l) = 0.0
      IF ( can_rad_mod == 6 ) THEN
        nleaf_layer(l) = nleaf_top(l) * EXP((n-1) * dlai(l) * (-knl(ft)))
      ELSE
        nleaf_layer(l) = nleaf_top(l) * EXP((n-1) / REAL(ilayers) * (-kn(ft)))
      END IF
    END DO
!$OMP END PARALLEL DO

    !-------------------------------------------------------------------------
    ! Calculate photosynthetic parameters.
    !-------------------------------------------------------------------------
    CALL calc_photo_parameters( ft, land_pts, pft_photo_model, veg_pts,        &
                                veg_index, denom, jmax_temp, jv25,             &
                                nleaf_layer, qtenf_term, vcmax_temp,           &
                                jmax, rd_dark, vcmax )

    IF ( pft_photo_model == photo_farquhar ) THEN
      !-----------------------------------------------------------------------
      ! Calculate sunlit and shaded radiation terms.
      !-----------------------------------------------------------------------
!$OMP PARALLEL DO IF(veg_pts > 1)                                              &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(n, veg_pts, veg_index, fapar_sun, fapar_shd, i2_shd, i2_sun,      &
!$OMP        ipar, sun_term)                                                   &
!$OMP PRIVATE(l,m)                                                             &
!$OMP SCHEDULE(STATIC)
      DO m = 1,veg_pts
        l = veg_index(m)
        i2_sun(l) = sun_term * fapar_sun(l,n) * ipar(l)
        i2_shd(l) = sun_term * fapar_shd(l,n) * ipar(l)
      END DO
!$OMP END PARALLEL DO

      !-----------------------------------------------------------------------
      ! Calculate the electron fluxes.
      ! Although we ultimately only need je_sun and je_shd, we also calculate
      ! je because we use that to calculate a single value of wlite, from which
      ! sunlit and shaded terms are calculated using je_sun and je_shd.
      !-----------------------------------------------------------------------
      CALL calc_electron_flux( land_pts, veg_pts, veg_index, i2, jmax, je)
      CALL calc_electron_flux( land_pts, veg_pts, veg_index, i2_sun, jmax,     &
                               je_sun)
      CALL calc_electron_flux( land_pts, veg_pts, veg_index, i2_shd, jmax,     &
                               je_shd)

      ! Calculate ratios of electron flux terms.
      ! These are used to scale the light-limited photosynthesis, which is
      ! linearly related to the electron flux.
!$OMP PARALLEL DO IF(veg_pts > 1)                                              &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(veg_pts, veg_index, je, je_shd, je_shd_ratio, je_sun,             &
!$OMP        je_sun_ratio)                                                     &
!$OMP PRIVATE(l,m)                                                             &
!$OMP SCHEDULE(STATIC)
      DO m = 1,veg_pts
        l = veg_index(m)
        IF ( je(l) > TINY(je(l)) ) THEN
          je_sun_ratio(l) = je_sun(l) / je(l)
          je_shd_ratio(l) = je_shd(l) / je(l)
        END IF
      END DO
!$OMP END PARALLEL DO

    END IF  !  pft_photo_model

    !-------------------------------------------------------------------------
    ! Iterate to ensure that the canopy humidity deficit is consistent with
    ! the H2O flux.
    !-------------------------------------------------------------------------

    DO k = 1,iter

      !-----------------------------------------------------------------------
      ! Diagnose the canopy-level humidity deficit.
      ! Initialise the sunlit and shaded respiration rates with the
      ! uninhibited, sunlit respiration rate.
      !-----------------------------------------------------------------------
!$OMP PARALLEL IF(veg_pts > 1) DEFAULT(NONE) PRIVATE(l, m)                     &
!$OMP          SHARED(dq, dqc, gl, ra, ra_rc, rd_dark, rd_shd, rd_sun,         &
!$OMP                 veg_index, veg_pts)
!$OMP DO SCHEDULE(STATIC)
      DO m = 1,veg_pts
        l = veg_index(m)
        ra_rc(l)  = ra(l) * gl(l)
        dqc(l)    = dq(l) / (1.0 + ra_rc(l))
        rd_sun(l) = rd_dark(l)
        rd_shd(l) = rd_dark(l)
      END DO
!$OMP END DO
!$OMP END PARALLEL

      !-----------------------------------------------------------------------
      ! Calculate the limiting factors for leaf photosynthesis
      !-----------------------------------------------------------------------
      CALL leaf_limits (ft, land_pts, pft_photo_model, veg_pts, veg_index      &
,                       acr, apar, ca, ccp, dqc, fsmc, je, kc, km, ko, oa      &
,                       pstar, vcmax                                           &
,                       clos_pts, open_pts, clos_index, open_index             &
,                       ci, wcarb, wexpt, wlite)

!$OMP PARALLEL IF(veg_pts > 1)                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(m,l)                                                             &
!$OMP SHARED(veg_pts,veg_index,rd_sun,rd_shd,rd,open_pts,pft_photo_model,      &
!$OMP        open_index,wlitev_sun,wlite,apar,fapar_sun,ipar,je_shd_ratio,     &
!$OMP        je_sun_ratio,wlitev_shd,fapar_shd,icr,n,fsun,dlai)

      IF ( pft_photo_model == photo_collatz ) THEN
!$OMP DO SCHEDULE(STATIC)
        DO m = 1,open_pts
          l = veg_index(open_index(m))
          wlitev_sun(l) = wlite(l) / apar(l) * fapar_sun(l,n) * ipar(l)
          wlitev_shd(l) = wlite(l) / apar(l) * fapar_shd(l,n) * ipar(l)
        END DO
!$OMP END DO NOWAIT
      ELSE
!$OMP DO SCHEDULE(STATIC)
        DO m = 1,open_pts
          l = veg_index(open_index(m))
          wlitev_sun(l) = wlite(l) * je_sun_ratio(l)
          wlitev_shd(l) = wlite(l) * je_shd_ratio(l)
        END DO
!$OMP END DO NOWAIT
      END IF  !  photo_model

      !-----------------------------------------------------------------------
      ! Introducing inhibition of leaf respiration in the light for sunlit
      ! and shaded leaves, from papers by Atkin et al. This is an
      ! improvement over the description used for can_rad_mod=4.
      ! This does not change between iterations (though open_index might).
      !-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
      DO m = 1,open_pts
        l = veg_index(open_index(m))
        IF ( fapar_sun(l,n) * icr(l) * fsun(l,n) * dlai(l) *                   &
             1.0e6 >  10.0 ) rd_sun(l) = 0.7 * rd_sun(l)
        IF ( fapar_shd(l,n) * icr(l) * (1.0 - fsun(l,n)) * dlai(l) *           &
             1.0e6 >  10.0 ) rd_shd(l) = 0.7 * rd_shd(l)
      END DO
!$OMP END DO
!$OMP END PARALLEL

      !-----------------------------------------------------------------------
      ! Calculate leaf-level fluxes separately for sunlit and shaded leaves.
      !-----------------------------------------------------------------------
      CALL leaf (clos_pts, ft, land_pts, open_pts, pft_photo_model, veg_pts    &
,                clos_index, open_index, veg_index                             &
,                ca, ci, fsmc, o3mol, ra, tstar                                &
,                wcarb, wexpt, wlitev_sun, rd_sun                              &
,                anetl_sun, flux_o3_l_sun, fo3_l_sun, gl_sun)

      CALL leaf (clos_pts, ft, land_pts, open_pts, pft_photo_model, veg_pts    &
,                clos_index, open_index, veg_index                             &
,                ca, ci, fsmc, o3mol, ra, tstar                                &
,                wcarb, wexpt, wlitev_shd, rd_shd                              &
,                anetl_shd, flux_o3_l_shd, fo3_l_shd, gl_shd)

      !-----------------------------------------------------------------------
      ! Update layer conductance.
      !-----------------------------------------------------------------------
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(m,l)                                                             &
!$OMP SHARED(veg_pts,veg_index,gl,fsun,n,gl_sun,gl_shd,rd,rd_sun,rd_shd)
      DO m = 1,veg_pts
        l = veg_index(m)
        gl(l) = fsun(l,n) * gl_sun(l) + (1.0 - fsun(l,n)) * gl_shd(l)
        rd(l) = fsun(l,n) * rd_sun(l) + (1.0 - fsun(l,n)) * rd_shd(l)
      END DO
!$OMP END PARALLEL DO

    END DO                 ! K-ITER

    !-------------------------------------------------------------------------
    ! Add to canopy-level values.
    !-------------------------------------------------------------------------
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(m,l)                                                             &
!$OMP SHARED(veg_pts,veg_index,anetl,fsun,anetl_sun,anetl_shd,anetc,dlai,gc,   &
!$OMP        gl,rdc,rd,rdmean,ilayers,l_o3_damage,flux_o3_l,flux_o3_l_sun,     &
!$OMP        flux_o3_l_shd,fo3_l_sun,fo3_l_shd,flux_o3,fo3,lai,n,fo3_l)
    DO m = 1,veg_pts
      l = veg_index(m)

      anetl(l)     = fsun(l,n) * anetl_sun(l)                                  &
                     + (1.0 - fsun(l,n)) * anetl_shd(l)
      anetc(l)     = anetc(l) + anetl(l) * dlai(l)

      gc(l)        = gc(l)  + gl(l) * dlai(l)
      rdc(l)       = rdc(l) + rd(l) * dlai(l)

      rdmean(l)    = rdmean(l) + rd(l) / REAL(ilayers)

      IF (l_o3_damage) THEN
        flux_o3_l(l) = fsun(l,n) * flux_o3_l_sun(l)                            &
                       + (1.0 - fsun(l,n)) * flux_o3_l_shd(l)
        fo3_l(l)     = fsun(l,n) * fo3_l_sun(l)                                &
                       + (1.0 - fsun(l,n)) * fo3_l_shd(l)

        flux_o3(l)   = flux_o3(l) + flux_o3_l(l) * dlai(l)
        fo3(l)       = fo3(l)     + fo3_l(l) * dlai(l) / lai(l)
      END IF
    END DO
!$OMP END PARALLEL DO

  END DO                   ! N LAYERS

CASE ( 1 )

  !---------------------------------------------------------------------------
  ! "Big leaf" model.
  ! N varies through canopy according to Beers Law
  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! Calculate photosynthetic parameters.
  ! There is no light limitation of dark respiration in this case.
  !---------------------------------------------------------------------------
  CALL calc_photo_parameters( ft, land_pts, pft_photo_model, veg_pts,          &
                              veg_index, denom, jmax_temp, jv25,               &
                              nleaf_top, qtenf_term, vcmax_temp,               &
                              jmax, rd, vcmax )

  IF ( pft_photo_model == photo_farquhar ) THEN
    ! Calculate the electron flux.
    CALL calc_electron_flux( land_pts, veg_pts, veg_index, i2, jmax, je)
  END IF

  !---------------------------------------------------------------------------
  ! Iterate to ensure that the canopy humidity deficit is consistent with the
  ! H2O flux. The first estimate of the canopy humidity deficit uses the
  ! stomatal conductance from the previous timestep.
  !---------------------------------------------------------------------------
  DO k = 1,iter

    !-------------------------------------------------------------------------
    ! Diagnose the canopy-level humidity deficit.
    !-------------------------------------------------------------------------
    DO m = 1,veg_pts
      l = veg_index(m)
      ra_rc(l) = ra(l) * gc(l)
      dqc(l)   = dq(l) / (1.0 + ra_rc(l))
    END DO

    !-------------------------------------------------------------------------
    ! Calculate leaf fluxes.
    !-------------------------------------------------------------------------
    IF (stomata_model == stomata_sox) THEN
      CALL leaf_processes_sox(ft, land_pts, veg_pts, veg_index                 &
,                             acr, apar, ca, ccp, dqc, kc, ko, oa              &
,                             pstar, vcmax, tstar, ht, psi_root_zone, rd       &
,                             clos_pts, open_pts, clos_index, open_index       &
,                             ci, o3mol, ra, anetl, flux_o3_l                  &
,                             fo3_l, gl, lwp_c, pft_photo_model)

    ELSE
      !-----------------------------------------------------------------------
      ! Calculate the limiting factors for leaf photosynthesis.
      !-----------------------------------------------------------------------
      CALL leaf_limits (ft, land_pts, pft_photo_model, veg_pts, veg_index      &
,                       acr, apar, ca, ccp, dqc, fsmc, je, kc, km, ko, oa      &
,                       pstar, vcmax                                           &
,                       clos_pts, open_pts, clos_index, open_index             &
,                       ci, wcarb, wexpt, wlite)

      !-----------------------------------------------------------------------
      ! Calculate leaf-level fluxes.
      !-----------------------------------------------------------------------
      CALL leaf (clos_pts, ft, land_pts, open_pts, pft_photo_model, veg_pts    &
,                clos_index, open_index, veg_index                             &
,                ca, ci, fsmc, o3mol, ra, tstar, wcarb, wexpt, wlite, rd       &
,                anetl, flux_o3_l, fo3_l, gl)
    END IF ! stomata_model

    !-------------------------------------------------------------------------
    ! Scale to canopy level.
    !-------------------------------------------------------------------------
    DO m = 1,veg_pts
      l = veg_index(m)
      gc(l) = fpar(l) * gl(l)
    END DO

  END DO   ! End of iteration loop

  !---------------------------------------------------------------------------
  ! Calculate canopy-level fluxes.
  !---------------------------------------------------------------------------
  DO m = 1,veg_pts
    l = veg_index(m)

    anetc(l) = anetl(l) * fpar(l)
    rdc(l)   = rd(l) * fpar(l)

    IF ( lai(l) > EPSILON(0.0) ) THEN
      rdmean(l) = rd(l) * fpar(l) / lai(l)
    ELSE
      rdmean(l) = rd(l)
    END IF

    IF (l_o3_damage) THEN
      flux_o3(l) = flux_o3_l(l) * fpar(l)
      fo3(l)     = fo3_l(l)
    END IF

  END DO

CASE DEFAULT
  errcode = 101  !  a hard error
  CALL ereport(RoutineName, errcode,                                           &
               'can_rad_mod should be 1, 4, 5 or 6')

END SELECT  ! can_rad_mod

!-----------------------------------------------------------------------------
! Calculate plant-level respiration, NPP and GPP.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Calculate the conversion from top-leaf to canopy-average nitrogen.
!-----------------------------------------------------------------------------
can_averaging_fac(:) =                                                         &
    get_can_ave_fac( ft, land_pts, veg_pts, veg_index, lai )

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(m,l,stemc,lma_tmp,fstem,stem_resp_scaling)
DO m = 1,veg_pts
  l = veg_index(m)

  !---------------------------------------------------------------------------
  ! Calculate the actual and balanced mean leaf nitrogen concentration
  ! assuming perfect light acclimation, then
  ! calculate the total nitrogen content of the leaf, root and stem
  ! Assume that root biomass is equal to balanced growth leaf biomass
  !---------------------------------------------------------------------------
  IF (l_red) THEN
    lai_bal(l) = veg_state%lai_bal(l,ft)

  ELSE
    lai_bal(l) = (a_ws(ft) * eta_sl(ft) * ht(l) / a_wl(ft))                    &
               **(1.0 / (b_wl(ft) - 1.0))

  END IF

  !---------------------------------------------------------------------------
  ! Calculate the total nitrogen content of the leaf, root and stem
  !---------------------------------------------------------------------------
  IF ( ft > nnpft ) THEN
    !Crop PFTs
    stemc   = stemc_from_prognostics(ft - nnpft, canht(l) )
    root(l) = rootc_cpft(l,ft - nnpft)

    IF ( l_trait_phys ) THEN

      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), dvi_cpft, can_averaging_fac(l) )
      n_root(l) = nr_nl(ft) * nmass(ft) * (1.0 / cfrac_l(ft - nnpft))          &
                  * root(l)
      n_stem(l) = ns_nl(ft) * nmass(ft) * (1.0 / cfrac_l(ft - nnpft)) * stemc
        ! nmass(ft)/cfrac_l(ft-nnpft) is equivalent to nl0(ft)

      lma_tmp   = lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft))
      nlmean(l) = nmass(ft) * lma_tmp * can_averaging_fac(l)

    ELSE

      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), dvi_cpft, can_averaging_fac(l) )
      n_root(l) = nr_nl(ft) * nl0(ft) * root(l)
      n_stem(l) = ns_nl(ft) * nl0(ft) * stemc

      nlmean(l) = nl0(ft) * can_averaging_fac(l) * cfrac_l(ft - nnpft)         &
                  * lma_from_prognostics(ft - nnpft, dvi_cpft(l,ft - nnpft))
    END IF

  ELSE

    !Non-crop PFTs
    IF ( l_trait_phys ) THEN
      root(l) = lma(ft) * lai_bal(l)
      !Note new units of temporary variable root:
      !kg root/m2= gleaf/m2 * kg/g

      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), dvi_cpft, can_averaging_fac(l))
      nlmean(l) = nmass(ft) * lma(ft) * can_averaging_fac(l)

      n_root(l) = nr(ft) * root(l) * cmass

      !Initial calculation of N content in respiring stem wood
      IF (l_red) THEN
        n_stem(l) = veg_state%woodC(l,ft) / a_ws(ft) * nsw(ft)
      ELSE
        n_stem(l) = eta_sl(ft) * ht(l) * lai_bal(l) * nsw(ft)
      END IF

      !Reduce n_stem for consistency with non-trait n_stem for now
      !This must be done to achieve realistic respiration rates.
      fstem            = 1.0 / a_ws(ft)
      stem_resp_scaling = fstem + (1.0 - fstem) * hw_sw(ft)
      n_stem(l)         = n_stem(l) * stem_resp_scaling

    ELSE

      n_leaf(l) = nleaf_from_lai( l, ft, lai(l), dvi_cpft, can_averaging_fac(l))
      root(l)   = sigl(ft) * lai_bal(l)
      n_root(l) = nr_nl(ft) * nl0(ft) * root(l)

      IF ( l_stem_resp_fix ) THEN
        n_stem(l) = ns_nl(ft) * nl0(ft) * eta_sl(ft) * ht(l) * lai_bal(l)
      ELSE
        n_stem(l) = ns_nl(ft) * nl0(ft) * eta_sl(ft) * ht(l) * lai(l)
      END IF

      nlmean(l) = nl0(ft) * can_averaging_fac(l) * sigl(ft)

    END IF

  END IF

  !---------------------------------------------------------------------------
  ! Calculate the Gross Primary Productivity, the plant maintenance
  ! respiration rate, and the wood maintenance respiration rate
  ! in kg C/m2/sec
  !---------------------------------------------------------------------------
  gpp(l) = cconu * (anetc(l) + rdc(l) * fsmc(l))

  IF (l_scale_resp_pm) THEN
    fsmc_scale(l) = fsmc(l)
  END IF

  IF ( l_sugar ) THEN
    !-------------------------------------------------------------------------
    ! Calculate carbon contents of leaf root and wood
    !-------------------------------------------------------------------------
    IF ( l_trait_phys ) THEN

      leafc(l)     = lma(ft) * lai(l) * cmass
      leafc_bal(l) = lma(ft) * lai_bal(l) * cmass

      IF ( l_red ) THEN
        rootc(l) = veg_state%rootC(l,ft)
        woodc(l) = veg_state%woodC(l,ft)
      ELSE
        rootc(l) = lma(ft) * lai_bal(l) * cmass
        woodc(l) = a_ws(ft) * eta_sl(ft) * ht(l) * lai_bal(l)
      END IF ! ( l_red )

    ELSE

      leafc(l)     = sigl(ft) * lai(l)
      leafc_bal(l) = sigl(ft) * lai_bal(l)
      rootc(l) = lma(ft) * lai_bal(l) * cmass
      woodc(l) = a_ws(ft) * eta_sl(ft) * ht(l) * lai_bal(l)

    END IF ! ( l_trait_phys )
    !---------------------------------------------------------------------------
    ! Calculate respration using SUGAR model
    !---------------------------------------------------------------------------
    CALL sugar(ft, leafc(l), leafc_bal(l) , woodc(l), rootc(l), f_nsc(l),      &
               resp_l(l), resp_w(l), resp_r(l), resp_p_m(l), resp_p_g(l),      &
               resp_p(l), growth_sug(l), tstar(l), gpp(l))

  ELSE
    IF ( lai(l) > EPSILON(0.0) ) THEN
      resp_p_m(l) = cconu * rdc(l)                                             &
           * (n_leaf(l) * fsmc(l) + n_stem(l) * fsmc_scale(l) +                &
              n_root(l) * fsmc_scale(l)) / n_leaf(l)
      resp_w(l) = cconu * rdc(l) * n_stem(l) * fsmc_scale(l) / n_leaf(l)
      resp_r(l) = cconu * rdc(l) * n_root(l) * fsmc_scale(l) / n_leaf(l)
      resp_l(l) = cconu * rdc(l) * fsmc(l)
    ELSE
      resp_w(l) = cconu * rdmean(l) * n_stem(l) * fsmc_scale(l) / nlmean(l)
      resp_r(l) = cconu * rdmean(l) * n_root(l) * fsmc_scale(l) / nlmean(l)
      resp_l(l) = cconu * rdc(l) * fsmc(l)
      resp_p_m(l) = resp_w(l) + resp_r(l) + resp_l(l)
    END IF
    !-------------------------------------------------------------------------
    ! Calculate the total plant respiration
    !-------------------------------------------------------------------------
    resp_p_g(l) = r_grow(ft) * (gpp(l) - resp_p_m(l))
    resp_p(l)   = resp_p_m(l) + resp_p_g(l)
  END IF !l_sugar

  !---------------------------------------------------------------------------
  ! Calculate Net Primary Productivity
  !---------------------------------------------------------------------------
  npp(l)      = gpp(l) - resp_p(l)

END DO
!$OMP END PARALLEL DO

!-----------------------------------------------------------------------------
! Calculate BVOC emissions
!-----------------------------------------------------------------------------
IF ( l_bvoc_emis ) THEN
  CALL bvoc_emissions(land_pts,ft,veg_index,                                   &
                      open_pts,open_index,clos_pts,clos_index,                 &
                      lai,ci,gpp,tstar,                                        &
                      isoprene,terpene,methanol,acetone,                       &
                      !New arguments replacing USE statements
                      !crop_vars_mod (IN)
                      dvi_cpft)
END IF

!-----------------------------------------------------------------------------
! If this functional type is a crop that has not emerged in a particular grid
! box, set all the output variables to zero.
!-----------------------------------------------------------------------------
IF ( ft > nnpft ) THEN

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP SHARED(veg_pts,veg_index,dvi_cpft,nnpft,gpp,resp_w,resp_p,npp,gc,fo3,    &
!$OMP         flux_o3,isoprene,terpene,methanol,acetone,ft)                    &
!$OMP PRIVATE(m,l)
  DO m = 1,veg_pts
    l = veg_index(m)

    IF ( dvi_cpft(l,ft - nnpft) < 0.0 ) THEN
      gpp(l)      = 0.0
      resp_w(l)   = 0.0
      resp_p(l)   = 0.0
      npp(l)      = 0.0
      gc(l)       = 0.0
      fo3(l)      = 0.0
      flux_o3(l)  = 0.0
      isoprene(l) = 0.0
      terpene(l)  = 0.0
      methanol(l) = 0.0
      acetone(l)  = 0.0
    END IF
  END DO
!$OMP END PARALLEL DO
END IF

!-----------------------------------------------------------------------------
! Calculate FAPAR diagnostic (fraction of absorbed photosynthetically
! active radiation). N.b. this is not per LAI.
! Calculate APAR diagnostic (absorbed photosynthetically
! active radiation).
!-----------------------------------------------------------------------------

fapar_diag(:) = 0.0
apar_diag(:) = 0.0

IF ( l_fapar_diag ) THEN
  SELECT CASE ( can_rad_mod )
  CASE ( 1 )
    DO m = 1,veg_pts
      l = veg_index(m)
      fapar_diag(l) = (1.0 - omega(ft)) * fpar(l) * kpar(ft)
    END DO
  CASE ( 4 )
    DO m = 1,veg_pts
      l = veg_index(m)
      fapar_diag(l) = SUM(faparv(l,:)) * dlai(l)
    END DO
  CASE ( 5, 6 )
    DO m = 1,veg_pts
      l = veg_index(m)
      fapar_diag(l) = SUM( fsun(l,:) * fapar_sun(l,:) +                        &
                           ( 1.0 - fsun(l,:) ) * fapar_shd(l,:)                &
                         ) * dlai(l)
    END DO
  CASE DEFAULT
    errcode = 101  !  a hard error
    CALL ereport(RoutineName, errcode,                                         &
                 'can_rad_mod should be 1, 4, 5 or 6 (l_fapar_diag)')
  END SELECT
  apar_diag(:) = fapar_diag(:) * ipar(:)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_stom

!#############################################################################
!#############################################################################

SUBROUTINE calc_photo_parameters( ft, land_pts, pft_photo_model, veg_pts,      &
                                  veg_index, denom, jmax_temp, jv25,           &
                                  nleaf, qtenf_term, vcmax_temp,               &
                                  jmax, rd_dark, vcmax )

! Calculate the maximum rates of carboxylation of Rubisco and electron
! transport, and dark respiration without light inhibition.

USE jules_vegetation_mod, ONLY:                                                &
! imported parameters
    jv_ntotal, jv_scale, photo_collatz, photo_farquhar,                        &
! imported scalars that are not changed
    n_alloc_jmax, n_alloc_vcmax, l_trait_phys, photo_jv_model

USE pftparm, ONLY: fd, jv25_ratio, neff, vint, vsl

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER,INTENT(IN) ::                                                          &
  ft,                                                                          &
    ! Index of plant functional type.
  land_pts,                                                                    &
    ! Number of land points.
  pft_photo_model,                                                             &
    ! Indicates which photosynthesis model to use for the current PFT.
  veg_pts,                                                                     &
    ! Number of vegetated points.
  veg_index(land_pts)
    ! Index of vegetated points on the land grid.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  denom(land_pts),                                                             &
    ! Denominator in equation for Vcmax with the Collatz model.
  jmax_temp(land_pts),                                                         &
    ! Factor expressing the effect of temperature on Jmax.
    ! Only used with the Farquhar model.
  jv25(land_pts),                                                              &
    ! Ratio of Jmax to Vcmax at 25 degC, including any acclimation.
    ! Only used with the Farquhar model.
  nleaf(land_pts),                                                             &
    ! Leaf nitrogen concentration.
    ! If l_trait_phys = (kg N m-2),  else = (kgN [kgC]-1).
  qtenf_term(land_pts),                                                        &
   ! Q10 temperature term used for Vcmax with the Collatz model.
  vcmax_temp(land_pts)
    ! Factor expressing the effect of temperature on Vcmax.
    ! Only used with the Farquhar model.

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  jmax(land_pts),                                                              &
    ! Maximum rate of electron transport (mol CO2 m-2 s-1).
    ! Only calculated with the Farquhar model.
  rd_dark(land_pts),                                                           &
    ! Dark respiration before light inhibition (mol CO2/m2/s).
  vcmax(land_pts)
    ! Maximum rate of carboxylation of Rubisco (mol CO2/m2/s).

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l, m
    ! Indices.

REAL(KIND=real_jlslsm) ::                                                      &
  n_total,                                                                     &
    ! Total N allocated to photosynthetic components (kg m-2).
  recip_j,                                                                     &
    ! Reciprocal of n_alloc_jmax (kg m-2 of N [mol CO2 m-2 s-1]).
  recip_v
    ! Reciprocal of n_alloc_vcmax (kg m-2 of N [mol CO2 m-2 s-1]).

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL ::                                                                        &
  vcmax_ref(land_pts)
    ! Maximum rate of carboxylation of Rubisco at the reference temperature,
    ! ignoring the effects of acclimation and N allocation (mol CO2/m2/s).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_PHOTO_PARAMETERS'

!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate some constants.
!-----------------------------------------------------------------------------
IF ( photo_jv_model == jv_ntotal ) THEN
  recip_j  = 1.0 / n_alloc_jmax
  recip_v  = 1.0 / n_alloc_vcmax
END IF

!-----------------------------------------------------------------------------
! Calculate Vcmax at the reference temperature, without any acclimation.
!-----------------------------------------------------------------------------
!$OMP PARALLEL IF(veg_pts > 1)  DEFAULT(NONE)                                  &
!$OMP PRIVATE(l, m, n_total)                                                   &
!$OMP SHARED(ft, pft_photo_model, photo_jv_model, veg_index, veg_pts,          &
!$OMP        denom, fd, jmax, jmax_temp, jv25, jv25_ratio, neff, nleaf,        &
!$OMP        qtenf_term, rd_dark, recip_j, recip_v, vcmax, vcmax_ref,          &
!$OMP        vcmax_temp, vint, vsl, l_trait_phys )

!$OMP DO SCHEDULE(STATIC)
DO m = 1,veg_pts

  l = veg_index(m)

  IF (l_trait_phys) THEN
    vcmax_ref(l) = (vsl(ft) * nleaf(l) + vint(ft)) * 1.0e-6  ! Kattge 2009
  ELSE
    vcmax_ref(l) = neff(ft) * nleaf(l)
  END IF

END DO
!$OMP END DO NOWAIT

!-----------------------------------------------------------------------------
! Calculate Vcmax and Jmax.
!-----------------------------------------------------------------------------
SELECT CASE ( pft_photo_model )
CASE ( photo_collatz )

  !---------------------------------------------------------------------------
  ! Use the Collatz model.
  !---------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)
    ! Using brackets here to recreate existing results.
    vcmax(l) = ( vcmax_ref(l) * qtenf_term(l) ) / denom(l)
  END DO
!$OMP END DO NOWAIT

CASE ( photo_farquhar )

  !---------------------------------------------------------------------------
  ! Use the Farquhar model (for C3 plants).
  !---------------------------------------------------------------------------

  !---------------------------------------------------------------------------
  ! Calculate values at the reference temperature, including any acclimation
  ! of jv25 (but excluding other acclimation terms).
  !---------------------------------------------------------------------------
  SELECT CASE ( photo_jv_model )

  CASE ( jv_scale )
    ! Find J25 by scaling V25.
!$OMP DO SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      vcmax(l) = vcmax_ref(l)
      jmax(l)  = vcmax_ref(l) * jv25(l)
    END DO
!$OMP END DO NOWAIT

  CASE ( jv_ntotal )
    ! Assume the total N allocated to photosynthetic capacity is constant.
!$OMP DO SCHEDULE(STATIC)
    DO m = 1,veg_pts
      l = veg_index(m)
      ! Calculate total N allocated to photosynthetic capacity, using the
      ! prescribed parameters at the reference temperature.
      ! This is Eq.5 of Mercado et al. (2018).
      n_total = vcmax_ref(l) * recip_v                                         &
                + vcmax_ref(l) * jv25_ratio(ft) * recip_j
      ! Calculate Vcmax and Jmax at 25degC, including temperature acclimation
      ! of J:V.
      vcmax(l) = n_total / ( recip_v + jv25(l) * recip_j )
      jmax(l)  = n_total / ( recip_v / jv25(l) + recip_j )
    END DO
!$OMP END DO NOWAIT

  END SELECT  !  photo_jv_model

  !---------------------------------------------------------------------------
  ! Calculate final values, including temperature effect.
  !---------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
  DO m = 1,veg_pts
    l = veg_index(m)

    ! Calculate rates according to acclimated ratio and N allocation to
    ! photosynthesis, and including temperature term.
    ! At present neither acclimation nor N allocation are represented.
    vcmax(l) = vcmax(l) * vcmax_temp(l)
    jmax(l)  = jmax(l)  * jmax_temp(l)

  END DO
!$OMP END DO NOWAIT

END SELECT  !  pft_photo_model

!-----------------------------------------------------------------------------
! Calculate dark respiration. Any effect of light inhibition is added later.
!-----------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO m = 1,veg_pts
  l = veg_index(m)
  rd_dark(l) = fd(ft) * vcmax(l)
END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE calc_photo_parameters

!#############################################################################
!#############################################################################

SUBROUTINE calc_electron_flux( land_pts, veg_pts, veg_index, i2, jmax, je )

! Calculate the electron flux for the Farquhar model.

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER,INTENT(IN) ::                                                          &
  land_pts,                                                                    &
    ! Number of land points.
  veg_pts,                                                                     &
    ! Number of vegetated points.
  veg_index(land_pts)
    ! Index of vegetated points on the land grid.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  i2(land_pts),                                                                &
    ! Radiation that goes to Photosystem II, expressed as an electron flux
    ! (mol m-2 s-1).
  jmax(land_pts)
    ! Maximum rate of electron transport (mol CO2 m-2 s-1).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  je(land_pts)
    ! Electron transport rate (mol m-2 s-1).

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  light_curvature = 0.90
    ! Curvature of the light response function. Used with Farquhar model of
    ! photosynthesis. See Eq.4 of Medlyn et al. (2002).

!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l, m
    ! Indices.

REAL(KIND=real_jlslsm) ::                                                      &
 recip_denom
   ! The reciprocal of the denominator.


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='CALC_ELECTRON_FLUX'

!-----------------------------------------------------------------------------
!end of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Calculate a constant.
!-----------------------------------------------------------------------------
recip_denom = 1.0 / ( 2.0 * light_curvature )

!-----------------------------------------------------------------------------
! Calculate electron flux by finding a root of a quadratic equation.
! This is the solution of Eq.4 of Medlyn et al. (2002).
!-----------------------------------------------------------------------------
DO m = 1,veg_pts
  l = veg_index(m)
  je(l)  = ( i2(l) + jmax(l)                                                   &
                    - SQRT( ( i2(l) + jmax(l) )**2                             &
                            - 4.0 * light_curvature * i2(l) * jmax(l) )        &
           ) * recip_denom
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE calc_electron_flux

END MODULE sf_stom_mod
