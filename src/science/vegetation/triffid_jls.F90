! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE triffid_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TRIFFID_MOD'

CONTAINS

!-----------------------------------------------------------------------------
! Subroutine TRIFFID ---------------------------------------------------------
!
!                     Top-down
!                     Representation of
!                     Interactive
!                     Foliage and
!                     Flora
!                     Including
!                     Dynamics
!
! Purpose : Simulates changes in vegetation structure, areal
!           coverage and the carbon contents of vegetation and soil.
!           can be used to advance these variables dynamically
!           (r_gamma=1/TIMESTEP) or to iterate towards  equilibrium
!           (r_gamma --> 0.0, FORW=1.0).
!
!Note that triffid is not compatible with soil tiling at this time, so all
!_soilt variables have their soil tile index hard-coded to 1
!
! ----------------------------------------------------------------------------
SUBROUTINE triffid (land_pts, trif_pts, trif_index, forw, r_gamma,             &
                    frac_agric, frac_past, frac_biocrop, resp_frac,            &
                    resp_frac_cspool,                                          &
                    g_leaf, npp, resp_s,                                       &
                    resp_w, cs, frac_c_label_pool, c_veg, frac, ht, lai,       &
                    cv, lit_c, lit_c_t,                                        &
                    cnsrv_veg2_correction, cnsrv_soil_resp, burnt_soil,        &
                    nstep_trif,                                                &
                    !New arguments replacing USE statements
                    ! prognostics (IN)
                    wood_prod_fast_gb, wood_prod_med_gb,                       &
                    wood_prod_slow_gb, frac_agr_prev_gb,                       &
                    frac_past_prev_gb, frac_biocrop_prev_gb, n_inorg_gb,       &
                    n_inorg_soilt_lyrs, n_inorg_avail_pft, ns_pool_gb,         &
                    triffid_co2_gb, t_soil_soilt_acc, years_since_harvest,     &
                    !trif_vars_mod (IN)
                    cnsrv_veg_triffid_gb, cnsrv_soil_triffid_gb,               &
                    cnsrv_prod_triffid_gb, cnsrv_carbon_triffid_gb,            &
                    cnsrv_vegN_triffid_gb, cnsrv_soilN_triffid_gb,             &
                    cnsrv_N_inorg_triffid_gb, cnsrv_nitrogen_triffid_gb,       &
                    wp_fast_in_gb, wp_med_in_gb, wp_slow_in_gb,                &
                    wp_fast_out_gb, wp_med_out_gb, wp_slow_out_gb,             &
                    lit_c_orig_pft, lit_c_ag_pft, lit_n_orig_pft,              &
                    lit_n_ag_pft, pc_s_pft, leafc_pft, rootc_pft,              &
                    woodc_pft, droot_pft, dleaf_pft,                           &
                    dwood_pft, root_litc_pft, leaf_litc_pft,                   &
                    wood_litc_pft, root_litn_pft, leaf_litn_pft,               &
                    wood_litn_pft, litterc_pft, lit_n_t_gb, lit_n_pft,         &
                    n_fix_gb, n_fix_add, n_fix_pft, n_gas_gb,                  &
                    n_uptake_pft, n_uptake_extract,                            &
                    n_uptake_gb, n_demand_pft, n_demand_gb, exudates_pft,      &
                    exudates_gb, littern_pft, n_uptake_growth_pft,             &
                    n_demand_growth_pft, n_demand_spread_pft,                  &
                    n_uptake_spread_pft, n_demand_lit_pft, n_veg_gb,           &
                    n_veg_pft, dcveg_pft, dcveg_gb, dnveg_pft,                 &
                    lai_bal_pft, dnveg_gb, n_loss_gb,                          &
                    harvest_pft,  root_abandon_pft,  harvest_gb,               &
                    harvest_n_pft, harvest_n_gb,                               &
                    n_fertiliser_pft, n_fertiliser_gb, n_fertiliser_add,       &
                    root_abandon_n_pft, npp_n_gb, npp_n,                       &
                    lit_c_fire_pft, lit_c_nofire_pft, lit_n_fire_pft,          &
                    lit_n_nofire_pft, veg_c_fire_emission_gb,                  &
                    veg_c_fire_emission_pft,                                   &
                    n_leaf_trif_pft, n_leaf_alloc_trif_pft,                    &
                    n_leaf_labile_trif_pft, n_root_trif_pft,                   &
                    n_stem_trif_pft, lit_n_ag_pft_diag,                        &
                    n_luc, lit_n_pft_diag,                                     &
                    leafC_gbm, woodC_gbm, rootC_gbm,                           &
                    root_abandon_gb, root_abandon_n_gb,                        &
                    minl_n_gb, immob_n_gb, g_burn_pft, g_burn_gb,              &
                    gpp_pft_acc, resp_p_actual_pft, resp_p_actual_gb,          &
                    harvest_biocrop_pft,  harvest_biocrop_gb,                  &
                    harvest_biocrop_n_pft, harvest_biocrop_n_gb,               &
                    harvest_doy,                                               &
                    burnt_carbon_dpm, burnt_carbon_rpm, minl_n_pot_gb,         &
                    immob_n_pot_gb, fn_gb, resp_s_diag_gb,                     &
                    resp_s_pot_diag_gb, dpm_ratio_gb, resp_s_to_atmos_gb,      &
                    deposition_n_gb,  dvi_cpft,                                &
                    !p_s_parms
                    sthu_soilt,                                                &
                    ! soil_ecosse_vars_mod
                    n_soil_pool_soilt, dim_soil_n_pool)


!Use in subroutines
USE soil_biogeochem_utils_mod, ONLY: soil_n_precalc
USE soil_inorg_n_mod, ONLY: soil_n_deposition, soil_n_fertiliser,              &
                            soil_n_fixation, soil_n_uptake
USE CN_utils_mod, ONLY: calc_n_comps_triffid
USE calc_litter_flux_mod, ONLY: calc_litter_flux
USE calc_c_comps_triffid_mod, ONLY: calc_c_comps_triffid
USE lotka_noeq_subset_mod, ONLY: lotka_noeq_subset
USE lotka_eq_mod, ONLY: lotka_eq
USE lotka_noeq_mod, ONLY: lotka_noeq
USE lotka_mod, ONLY: lotka
USE vegcarb_mod, ONLY: vegcarb
USE soilcarb_mod, ONLY: soilcarb
USE woodprod_mod, ONLY: woodprod
USE datetime_utils_mod, ONLY: day_of_year, days_in_year
USE time_info_mod, ONLY: l_360, l_leap, current_model_time

!Use in variables

USE soilcarb_layers_mod, ONLY: soilcarb_layers

USE jules_surface_types_mod, ONLY: nnpft, npft, ntype

USE ancil_info, ONLY: dim_cslayer, nsoilt, dim_cs1

USE jules_soil_mod, ONLY: sm_levels

USE jules_surface_mod, ONLY: cmass

USE pftparm, ONLY: a_wl, a_ws, b_wl, eta_sl, lma, sigl

USE trif, ONLY: crop, harvest_freq, harvest_ht, harvest_type, lai_min

USE jules_soil_biogeochem_mod, ONLY:                                           &
  ! imported scalar parameters
  soil_model_4pool,                                                            &
  ! imported scalar variables
  diff_n_pft, l_layeredC, n_inorg_turnover, soil_bgc_model, tau_resp

USE jules_vegetation_mod, ONLY: l_veg_compete, l_ht_compete,                   &
                                l_trait_phys, l_trif_eq, l_landuse,            &
                                can_rad_mod, l_nitrogen,                       &
                                l_trif_crop, frac_min, triffid_period,         &
                                l_trif_biocrop

USE parkind1, ONLY: jprb, jpim

USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN).
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  trif_pts,                                                                    &
    ! Number of points on which TRIFFID may operate.
  nstep_trif
    ! Number of atmospheric timesteps between calls to TRIFFID.

INTEGER, INTENT(IN) ::                                                         &
  trif_index(land_pts)
    ! Indices of land points on which TRIFFID may operate.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  forw,                                                                        &
    ! Forward timestep weighting.
  r_gamma
    ! Inverse timestep (/360days).

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_agric(land_pts),                                                        &
    ! Fraction of agriculture.
  frac_past(land_pts),                                                         &
    ! Fraction of pasture.
  frac_biocrop(land_pts),                                                      &
    ! Fraction of bioenergy crops.
  resp_frac(land_pts,dim_cslayer),                                             &
    ! The fraction of RESP_S (soil respiration) that forms new soil C.
    ! This is the fraction that is NOT released to the atmosphere.
  dvi_cpft(:,:)
    !  Development index for crop tiles

!-----------------------------------------------------------------------------
! Arguments with INTENT(INOUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  g_leaf(land_pts,npft),                                                       &
    ! Turnover rate for leaf and fine root biomass (/360days).
  npp(land_pts,npft),                                                          &
    ! Net primary productivity (kg C/m2/360days).
  resp_s(land_pts,dim_cslayer,5),                                              &
    ! Soil respiration (kg C/m2/360days).
  resp_w(land_pts,npft),                                                       &
    ! Wood maintenance respiration (kg C/m2/360days).
  cs(land_pts,dim_cslayer,4),                                                  &
    ! Soil carbon (kg C/m2).
  frac_c_label_pool(land_pts,dim_cslayer,4),                                   &
  ! fraction of soil carbon that is labelled and traced
  c_veg(land_pts,npft),                                                        &
    ! Total carbon content of the vegetation (kg C/m2).
  frac(land_pts,ntype),                                                        &
    ! Fractional cover of each Functional Type, with LUC and fire.
  ht(land_pts,npft),                                                           &
    ! Vegetation height (m).
  lai(land_pts,npft),                                                          &
    ! Leaf area index.
  resp_frac_cspool(land_pts,dim_cslayer,dim_cs1)
    ! The fraction of RESP_S (soil respiration) that forms new soil C.
    ! For l_layeredc=true dependent on pool (compare with resp_frac)

INTEGER, INTENT(IN OUT) ::                                                     &
  harvest_doy(land_pts,npft),                                                  &
  years_since_harvest(land_pts,npft)

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  cv(land_pts),                                                                &
    ! Gridbox mean vegetation carbon (kg C/m2).
  lit_c(land_pts,npft),                                                        &
    ! Carbon Litter (kg C/m2/360days).
    ! This is loss of vegetation carbon due to natural vegetaton dynamics.
  lit_c_t(land_pts),                                                           &
    ! Gridbox mean carbon litter(kg C/m2/360days).
    ! Vegetation-to-soil carbon flux due to natural vegetation dynamics and
    ! landuse change.
 cnsrv_veg2_correction(land_pts),                                              &
    ! Corrections to the veg2 carbon conservation diagnostic
    ! accounting for processes not explicitly represented in the veg2
    ! subroutine: Change to wood product pools, crop harvest and fire
    ! emissions (kg m-2).
  cnsrv_soil_resp(land_pts),                                                   &
    ! Carbon flux into atmosphere from soil respiration (kg/m2/360days).
  burnt_soil(land_pts)
    ! Burnt C in RPM and DPM pools (kg m-2 360d-1) for respiration correction
    ! calculation in veg2

!New arguments replacing USE statements
! prognostics
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: wood_prod_fast_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: wood_prod_med_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: wood_prod_slow_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: frac_agr_prev_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: frac_past_prev_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: frac_biocrop_prev_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_inorg_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          n_inorg_soilt_lyrs(land_pts,nsoilt,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          n_inorg_avail_pft(land_pts,npft,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          ns_pool_gb(land_pts,dim_cslayer,dim_cs1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: triffid_co2_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          t_soil_soilt_acc(land_pts,nsoilt,sm_levels)
    ! Sub-surface temperature on layers and soil tiles accumulated over
    ! TRIFFID timestep (K).

!trif_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_veg_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_soil_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_prod_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_carbon_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_vegN_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_soilN_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_N_inorg_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: cnsrv_nitrogen_triffid_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: wp_fast_in_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: wp_med_in_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: wp_slow_in_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: wp_fast_out_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: wp_med_out_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: wp_slow_out_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_c_orig_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_c_ag_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_n_orig_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_n_ag_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: pc_s_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: leafc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: rootc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: woodc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: droot_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: dleaf_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: dwood_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_litc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: leaf_litc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: wood_litc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_litn_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: leaf_litn_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: wood_litn_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: litterc_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_n_t_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_n_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_fix_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_fix_add(land_pts,nsoilt,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_fix_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: n_gas_gb(land_pts,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_uptake_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
        n_uptake_extract(land_pts,nsoilt,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_uptake_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_demand_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_demand_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: exudates_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: exudates_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: littern_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_uptake_growth_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_demand_growth_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_demand_spread_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_uptake_spread_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_demand_lit_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: n_veg_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_veg_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: dcveg_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: dcveg_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: dnveg_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: lai_bal_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: dnveg_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_loss_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_abandon_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_n_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_n_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_biocrop_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_biocrop_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_biocrop_n_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: harvest_biocrop_n_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_fertiliser_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_fertiliser_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
        n_fertiliser_add(land_pts,nsoilt,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_abandon_n_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: npp_n_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: npp_n(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_c_fire_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_c_nofire_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_n_fire_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: lit_n_nofire_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: veg_c_fire_emission_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: veg_c_fire_emission_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_leaf_trif_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: n_leaf_alloc_trif_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: n_leaf_labile_trif_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_root_trif_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_stem_trif_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: lit_n_ag_pft_diag(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: n_luc(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: lit_n_pft_diag(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: leafC_gbm(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: woodC_gbm(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: rootC_gbm(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_abandon_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: root_abandon_n_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          minl_n_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          immob_n_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN) :: g_burn_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: g_burn_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN) :: gpp_pft_acc(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: resp_p_actual_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: resp_p_actual_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: burnt_carbon_dpm(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: burnt_carbon_rpm(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          minl_n_pot_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          immob_n_pot_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: fn_gb(land_pts,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          resp_s_diag_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          resp_s_pot_diag_gb(land_pts,dim_cslayer,dim_cs1+1)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: dpm_ratio_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          resp_s_to_atmos_gb(land_pts,dim_cslayer)
REAL(KIND=real_jlslsm), INTENT(IN) :: deposition_n_gb(land_pts)

!p_s_parms
REAL(KIND=real_jlslsm), INTENT(IN) :: sthu_soilt(land_pts,nsoilt,sm_levels)

! soil_ecosse_vars_mod
INTEGER, INTENT(IN) :: dim_soil_n_pool
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
n_soil_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_soil_n_pool)

!-----------------------------------------------------------------------------
! Local parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER :: veg_types = 4
    ! The number of vegetation types.
    ! PFTs only compete with other PFTs of the same vegetation type.
    ! 1=natural vegetation (crop=0)
    ! 2= nat veg, crops (crop = 0 and 1)
    ! 3= nat veg, crops, pasture (crop = 0, 1 and 2)
    ! 4=nat, crop, pasture, forestry (crop = 0, 1, 2 and 3)
REAL(KIND=real_jlslsm), PARAMETER :: harvest_rate = 0.3
    ! Fraction of litter diverted as harvest.
    ! This fraction ends up in the product pools instead of in the soil.

REAL(KIND=real_jlslsm), PARAMETER :: fire_ratio = 0.13
    ! Fraction of burnt carbon that goes to the atmosphere as CO2 instead
    ! of in the soil. Based on mean whole-plant mortality factor from
    ! Li et al (2012) table 2.

!-----------------------------------------------------------------------------
! Local scalar variables.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  fert_layer,                                                                  &
    ! Deepest soil level to which fertiliser is applied.
  nsub,                                                                        &
    ! Number of PFTs to use in call to lotka_noeq_subset
    ! (if l_trif_crop is true).
  crop_lotka,                                                                  &
    ! Indicates to l_trif_crop if crop or natural PFTs are competing.
    ! 0 = natural vegetation
    ! 1 = crops as in the "crop" variable
    ! 2 = pasture
    ! 3 = bioenergy or forestry
  day,                                                                         &
    ! Current day
  month,                                                                       &
    ! Current month
  year,                                                                        &
    ! Current year
  day_of_yr,                                                                   &
    ! Current day of year
  days_in_yr,                                                                  &
    ! Number of days in the current year
  l,n,t,k,nn
    ! Loop counters

REAL(KIND=real_jlslsm) ::                                                      &
  cratio,                                                                      &
    ! Ratio of above/below veg carbon.
  nratio,                                                                      &
    ! Ratio of above/below veg nitrogen.
  inorg_decay,                                                                 &
    ! Decay of inorganic N turnover with depth.
  disturb_crop
    ! Fraction of crop PFT that can be disturbed by landuse change.

LOGICAL :: have_layers
    !  T if we have multiple soil layers, else F.

!-----------------------------------------------------------------------------
! Local array variables.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  dfrac(land_pts,npft),                                                        &
    ! Change in areal fraction during the timestep.
  dfrac_na(land_pts,npft),                                                     &
    ! As dfrac, but without land use change.
  dfrac_nofire(land_pts,npft),                                                 &
    ! As dfrac, but without fire.
  frac_flux(land_pts,npft),                                                    &
    ! PFT fraction to be used in the calculation of the gridbox mean fluxes.
  frac_flux_nat(land_pts,npft),                                                &
    ! PFT fraction to be used in the calculation of the gridbox mean fluxes.
  frac_flux_nofire(land_pts,npft),                                             &
    ! PFT fraction to be used in the calculation of the gridbox mean fluxes.
  frac_na(land_pts,ntype),                                                     &
    ! Fractional cover of each Functional Type, no LUC, with fire.
  frac_nofire(land_pts,ntype),                                                 &
    ! Fractional cover of each Functional Type, with LUC, no fire.
  ns_gb(land_pts,dim_cslayer),                                                 &
    ! Total Soil N on layers (kg N/m2).
  phen(land_pts,npft),                                                         &
    ! Phenological state.
  frac_prev(land_pts,ntype),                                                   &
    ! Copy of frac for equilibrium TRIFFID.
  deposition(land_pts),                                                        &
    ! N deposition (kgN m-2 360days-1).
  dz(dim_cslayer),                                                             &
    ! Soil carbon layer thicknesses (m).
  n_avail(land_pts,npft),                                                      &
    ! Plant-available inorganic N on tiles (kgN m-2).
  neg_n(land_pts),                                                             &
    ! Negative N required to prevent ns<0 (kg N).
  f_root_pft(npft,dim_cslayer),                                                &
    ! Root fraction in each soil layer per PFT.
  f_root_pft_dz(npft,dim_cslayer),                                             &
    ! Normalised roots in each soil layer per PFT.
  isunfrozen(land_pts,dim_cslayer),                                            &
    ! Matrix to mask out frozen layers (inaccessible to plants).
  ftemp(land_pts,dim_cslayer),                                                 &
    ! Temperature dependence of decay
  subset_space(land_pts),                                                      &
    ! Used if l_trif_crop=.T.
    ! For the natural vegetation type this is the area where vegetation is
    ! prevented from growing because the area has been reserved for
    ! crops/pasture/forestry. For other vegetation types this is the area
    ! reserved for them to grow in, e.g. crops.
  subset_space_prev(land_pts),                                                 &
    ! The value of subset_space at the previous TRIFFID timestep.
  cnsrv_veg_flux(land_pts),                                                    &
    ! Net carbon flux into vegetation (kg m-2).
  cnsrv_soil_flux(land_pts),                                                   &
    ! Net carbon flux into soil (kg m-2).
  cnsrv_prod_flux(land_pts),                                                   &
    ! Net carbon flux into wood products (kg m-2).
  cnsrv_carbon_flux(land_pts),                                                 &
    ! Net carbon flux into land (kg m-2).
  implicit_resp_correction(land_pts),                                          &
    ! Respiration carried to next triffid timestep to account for applying
    ! minimum soil carbon constraint (kg m-2).
  wp_total_out_gb(land_pts),                                                   &
    ! Total flux out of wood products (kg m-2).
  wood_prod_total_gb(land_pts),                                                &
     ! Total of the wood product pools (kg m-2).
  init_agb_c(land_pts,npft),                                                   &
    ! above-ground veg carbon prior to type 2 harvest (kg m-2)
  init_agb_n(land_pts,npft),                                                   &
    ! above-ground veg nitrogen prior to type 2 harvest (kg m-2)
  init_root_c (land_pts,npft),                                                 &
    ! Root carbon prior to type 2 harvest (kg m-2)
  init_root_n(land_pts,npft)
    ! Root nitrogen prior to type 2 harvest (kg m-2)

INTEGER :: harvest_test
    ! Variable for calculating harvest time
LOGICAL :: harvest_switch(land_pts,npft)
    ! Switch to activate type 2 harvest

! Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIFFID'

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------------
! Initialisation.
!-----------------------------------------------------------------------------
DO l = 1,land_pts

  wp_fast_out_gb(l) = 0.0
  wp_med_out_gb(l)  = 0.0
  wp_slow_out_gb(l) = 0.0

  wp_fast_in_gb(l)  = 0.0
  wp_med_in_gb(l)   = 0.0
  wp_slow_in_gb(l)  = 0.0

  burnt_soil(l)     = 0.0

END DO

IF ( .NOT. l_landuse) THEN
  frac_agr_prev_gb(:)  = frac_agric(:)
  frac_past_prev_gb(:) = frac_past(:)
  frac_biocrop_prev_gb(:) = frac_biocrop(:)
END IF

n_veg_pft(:,:)    = 0.0
dnveg_pft(:,:)    = 0.0
dnveg_gb(:)       = 0.0
dcveg_gb(:)       = 0.0
n_uptake_pft(:,:) = 0.0
n_demand_pft(:,:) = 0.0
n_demand_gb(:)    = 0.0
exudates_gb(:)    = 0.0
n_loss_gb(:)      = 0.0
litterC_pft(:,:)  = 0.0
litterN_pft(:,:)  = 0.0
npp_n(:,:)        = 0.0
npp_n_gb(:)       = 0.0
harvest_switch(:,:) = .FALSE.

!-----------------------------------------------------------------------------
! Diagnose the C and N stored in each component of the vegetation, and the
! phenological state.
!-----------------------------------------------------------------------------
DO n = 1,npft
  DO t = 1,trif_pts
    l = trif_index(t)

    !-------------------------------------------------------------------------
    ! Diagnose carbon pools.
    ! Note we do this for every iteration (in equilibrium mode) rather than
    ! rely on values from previous iteration because of slight differences
    ! in how components are calculated here and nearer end of iteration.
    !-------------------------------------------------------------------------
    CALL calc_c_comps_triffid(n, ht(l,n), lai_bal_pft(l,n), leafc_pft(l,n),    &
                              rootc_pft(l,n), woodc_pft(l,n), c_veg(l,n))

    !-------------------------------------------------------------------------
    ! Diagnose the phenological state
    !-------------------------------------------------------------------------
    phen(l,n) = lai(l,n) / lai_bal_pft(l,n)
    phen(l,n) = MIN(1.0, phen(l,n))
    phen(l,n) = MAX(0.0, phen(l,n))

    !-------------------------------------------------------------------------
    ! Diagnose nitrogen pools.
    !-------------------------------------------------------------------------
    CALL calc_n_comps_triffid(l, n, phen(l,n), lai_bal_pft(l,n),               &
                              woodc_pft(l,n), rootc_pft(l,n),                  &
                              n_leaf_trif_pft(l,n), n_root_trif_pft(l,n),      &
                              n_stem_trif_pft(l,n), dvi_cpft)

    n_veg_pft(l,n) = n_leaf_trif_pft(l,n) + n_root_trif_pft(l,n)               &
                     + n_stem_trif_pft(l,n)

    ! Save values at start of timestep.
    dnveg_pft(l,n) = n_veg_pft(l,n)
    dnveg_gb(l)    = dnveg_gb(l) - frac(l,n) * n_veg_pft(l,n)
    dcveg_gb(l)    = dcveg_gb(l) - frac(l,n) * c_veg(l,n)

    !-------------------------------------------------------------------------
    ! Adjust canopy height for biocrop PFTs then recalculate c_veg
    !-------------------------------------------------------------------------

    IF ((l_trif_biocrop) .AND. (harvest_type(n) == 2)) THEN

      ! Check whether to harvest
      ! Get current day of the calendar year and number of days in year
      CALL current_model_time(year, month, day)

      day_of_yr  = day_of_year(year, month, day, l_360, l_leap)
      days_in_yr = days_in_year(year, l_360, l_leap)

      ! First define harvest_test as the number of days since the harvest_doy
      harvest_test = day_of_yr - harvest_doy(l,n)

      ! It's time to harvest (or increment years since harvest) if harvest_test
      ! is between 0 and triffid_period. This ensures harvesting will occur the
      ! first time TRIFFID is called since harvest_test became positive
      IF ((harvest_test >= 0) .AND. (harvest_test < triffid_period)) THEN
        ! Check if the right number of years have passed since the last harvest
        IF ( years_since_harvest(l,n) >= harvest_freq(n) - 1) THEN

          harvest_switch(l,n) = .TRUE.
          ! Reset harvest counter
          years_since_harvest(l,n) = 0

          ! Record initial PFT carbon and nitrogen contents
          init_agb_c(l,n) = woodc_pft(l,n) + leafc_pft(l,n)
          init_agb_n(l,n) = n_stem_trif_pft(l,n) + n_leaf_trif_pft(l,n)
          init_root_c(l,n) = rootc_pft(l,n)
          init_root_n(l,n) = n_root_trif_pft(l,n)

          ! Reduce PFT to harvest height and recalculate biomass
          ! This generates new values for PFT height and biomass.
          ! Moving residual biomass to harvest and soil pools happens below
          ht(l,n) = harvest_ht(n)
          CALL calc_c_comps_triffid(n, ht(l,n), lai_bal_pft(l,n),              &
                                    leafc_pft(l,n), rootc_pft(l,n),            &
                                    woodc_pft(l,n), c_veg(l,n))

          lai(l,n) = lai_min(n)
          phen(l,n) = lai(l,n) / lai_bal_pft(l,n)
          phen(l,n) = MIN(1.0, phen(l,n))
          phen(l,n) = MAX(0.0, phen(l,n))

          CALL calc_n_comps_triffid(l, n, phen(l,n), lai_bal_pft(l,n),         &
                             woodc_pft(l,n), rootc_pft(l,n),                   &
                             n_leaf_trif_pft(l,n), n_root_trif_pft(l,n),       &
                             n_stem_trif_pft(l,n), dvi_cpft)

          n_veg_pft(l,n) = n_leaf_trif_pft(l,n) + n_root_trif_pft(l,n)         &
                    + n_stem_trif_pft(l,n)


        ELSE ! if it's the right harvest_doy but the wrong year, increment
          years_since_harvest(l,n) = years_since_harvest(l,n) + 1
        END IF ! years_since_harvest
      END IF ! harvest_test
    END IF ! l_trif_biocrop


  END DO  !  TRIFFID points
END DO  !  PFTs

!-----------------------------------------------------------------------------
! Check carbon/nitrogen conservation (1/3)
! Calculate C/N stores and net C/N fluxes at start of
! routine. The soil respiration, litter and wood product decay fluxes have not
! yet been calculated and will be added later.
!-----------------------------------------------------------------------------
! If not using 4-pool soil C, set variables related to the soil to zero.
! In that case these are not used further below.
IF ( soil_bgc_model /= soil_model_4pool ) THEN
  cnsrv_soil_triffid_gb(:)     = 0.0
  cnsrv_soilN_triffid_gb(:)    = 0.0
  cnsrv_N_inorg_triffid_gb(:)  = 0.0
  cnsrv_carbon_triffid_gb(:)   = 0.0
  cnsrv_nitrogen_triffid_gb(:) = 0.0
END IF

DO t = 1,trif_pts
  l = trif_index(t)

  DO n = 1,nnpft
    cnsrv_vegN_triffid_gb(l) = cnsrv_vegN_triffid_gb(l)                        &
                               + (frac(l,n) * (n_leaf_trif_pft(l,n)            &
                                            +  n_root_trif_pft(l,n)            &
                                            +  n_stem_trif_pft(l,n)))
  END DO

  cnsrv_veg_triffid_gb(l)     = SUM(frac(l,1:npft) * c_veg(l,1:npft))

  cnsrv_prod_triffid_gb(l)    = wood_prod_fast_gb(l) + wood_prod_med_gb(l) +   &
                                wood_prod_slow_gb(l)

  cnsrv_veg_flux(l)  = SUM(npp(l,1:npft) * frac(l,1:npft)) / r_gamma

  cnsrv_prod_flux(l) = 0.0

  IF ( soil_bgc_model == soil_model_4pool ) THEN
    cnsrv_soil_triffid_gb(l)    = SUM(cs(l,:,:))
    cnsrv_soilN_triffid_gb(l)   = SUM(ns_pool_gb(l,:,:))
    cnsrv_N_inorg_triffid_gb(l) = SUM(N_inorg_soilt_lyrs(l,:,:))

    cnsrv_carbon_triffid_gb(l)  = cnsrv_veg_triffid_gb(l ) +                   &
                                  cnsrv_soil_triffid_gb(l) +                   &
                                  cnsrv_prod_triffid_gb(l)

    cnsrv_nitrogen_triffid_gb(l) = cnsrv_vegN_triffid_gb(l)  +                 &
                                   cnsrv_soilN_triffid_gb(l) +                 &
                                   cnsrv_N_inorg_triffid_gb(l)
    cnsrv_carbon_flux(l) = SUM(npp(l,1:npft) * frac(l,1:npft)) / r_gamma
  END IF  !  4-pool soil C

END DO

!-----------------------------------------------------------------------------
! Preparatory calculations.
!-----------------------------------------------------------------------------
CALL soil_n_precalc( land_pts, trif_pts, nstep_trif, trif_index,               &
                     n_inorg_avail_pft, n_inorg_soilt_lyrs,                    &
                     fert_layer, have_layers, dz, f_root_pft, f_root_pft_dz,   &
                     isunfrozen ,                                              &
                 ! These arguments replace USE statements
                    n_soil_pool_soilt, dim_soil_n_pool,                        &
                    ! prognostics
                    t_soil_soilt_acc)

!-----------------------------------------------------------------------------
! Add nitrogen deposition to the soil.
!-----------------------------------------------------------------------------
CALL soil_n_deposition( land_pts, trif_pts, r_gamma, trif_index,               &
                        n_inorg_avail_pft, n_inorg_soilt_lyrs, deposition,     &
                      !New arguements to replace USE statements
                      !trif_vars_mod
                      deposition_n_gb )

!-----------------------------------------------------------------------------
! Calculate nitrogen fixation.
!-----------------------------------------------------------------------------
CALL soil_n_fixation( land_pts, trif_pts, r_gamma, have_layers,                &
                      trif_index, frac, f_root_pft, isunfrozen, npp,           &
                      n_inorg_avail_pft, n_inorg_soilt_lyrs, n_fix_gb,         &
                      n_fix_add, n_fix_pft)

!-----------------------------------------------------------------------------
! Loop through Functional Types
!-----------------------------------------------------------------------------
DO n = 1,npft

  !---------------------------------------------------------------------------
  ! Calculate the N available to plants.
  ! Assume inorganic N is equally spread amongst PFTs and bare ground.
  !---------------------------------------------------------------------------
  IF ( have_layers ) THEN
    n_avail(:,n) = 0.0
    DO nn = 1,dim_cslayer
      ! Plants can access the plant available nitrogen
      ! (prognostic, depends on root frac) only in unfrozen layers.
      n_avail(:,n) = n_avail(:,n) +                                            &
                     n_inorg_avail_pft(:,n,nn) * isunfrozen(:,nn)
    END DO
  ELSE
    n_avail(:,n) = n_inorg_soilt_lyrs(:,1,1)
  END IF

  !---------------------------------------------------------------------------
  ! Update vegetation carbon contents
  !---------------------------------------------------------------------------
  CALL vegcarb (land_pts, trif_pts, n, trif_index,                             &
                forw, r_gamma, phen(:,n),                                      &
                g_leaf(:,n), n_avail(:,n), npp(:,n), resp_w(:,n),              &
                leafc_pft(:,n), rootc_pft(:,n), woodc_pft(:,n),                &
                dleaf_pft(:,n), droot_pft(:,n), dwood_pft(:,n),                &
                dcveg_pft(:,n), exudates_pft(:,n) ,                            &
                n_demand_growth_pft(:,n), n_demand_lit_pft(:,n),               &
                n_demand_spread_pft(:,n), n_uptake_growth_pft(:,n),            &
                n_uptake_spread_pft(:,n),                                      &
                n_fertiliser_pft(:,n), pc_s_pft(:,n), dvi_cpft,                &
                !New arguments replacing USE statements
                ! trif_vars_mod
                root_litc_pft, leaf_litc_pft, wood_litc_pft,                   &
                root_litn_pft, leaf_litn_pft, wood_litn_pft)

  !---------------------------------------------------------------------------
  ! Diagnose N demand and N uptake
  ! Note N_uptake spreading updated in competition
  ! N uptake could be greater than n_inorg_soilt at this stage due to need
  ! maintain min frac
  !---------------------------------------------------------------------------
  n_uptake_pft(:,n) = n_uptake_growth_pft(:,n) + n_demand_lit_pft(:,n)         &
                      + n_uptake_spread_pft(:,n)
  n_demand_pft(:,n) = n_demand_growth_pft(:,n) + n_demand_lit_pft(:,n)         &
                      + n_demand_spread_pft(:,n)

END DO  !  PFTs

!-----------------------------------------------------------------------------
! Diagnose the new value of Canopy Height, Leaf Area Index and Total
! Vegetation Carbon
!-----------------------------------------------------------------------------

resp_p_actual_pft(:,:) = 0.0
resp_p_actual_gb(:)    = 0.0

DO n = 1,nnpft

  DO t = 1,trif_pts
    l = trif_index(t)

    ht(l,n) = woodc_pft(l,n) / (a_ws(n) * eta_sl(n))                           &
              * (a_wl(n) / woodc_pft(l,n))**(1.0 / b_wl(n))
    IF (l_trait_phys) THEN
      lai_bal_pft(l,n) = leafc_pft(l,n) / (lma(n) * cmass)
    ELSE
      lai_bal_pft(l,n) = leafc_pft(l,n) / sigl(n)
    END IF
    lai(l,n)   = phen(l,n) * lai_bal_pft(l,n)
    c_veg(l,n) = leafc_pft(l,n) + rootc_pft(l,n) + woodc_pft(l,n)

    !-------------------------------------------------------------------------
    ! Diagnose updated nitrogen pools.
    !-------------------------------------------------------------------------
    ! Work out n_leaf for 100% labile pool
    CALL calc_n_comps_triffid(l, n, 0.0, lai_bal_pft(l,n), woodc_pft(l,n),     &
                              rootc_pft(l,n), n_leaf_trif_pft(l,n),            &
                              n_root_trif_pft(l,n), n_stem_trif_pft(l,n),      &
                              dvi_cpft)

    n_leaf_labile_trif_pft(l,n) = n_leaf_trif_pft(l,n) * (1.0 - phen(l,n))

    ! Now for the actual phenological state.
    CALL calc_n_comps_triffid(l, n, phen(l,n), lai_bal_pft(l,n),               &
                              woodc_pft(l,n), rootc_pft(l,n),                  &
                              n_leaf_trif_pft(l,n), n_root_trif_pft(l,n),      &
                              n_stem_trif_pft(l,n), dvi_cpft)

    n_leaf_alloc_trif_pft(l,n) = n_leaf_trif_pft(l,n)                          &
                                 - n_leaf_labile_trif_pft(l,n)

    n_veg_pft(l,n) = n_leaf_trif_pft(l,n) + n_root_trif_pft(l,n)               &
                     + n_stem_trif_pft(l,n)
    dnveg_pft(l,n) = n_veg_pft(l,n) - dnveg_pft(l,n)
    !-------------------------------------------------------------------------
    ! Reduce NPP by exudates. Important for Litterfall calculation
    ! Note this is the diagnostic from TRIFFID
    !-------------------------------------------------------------------------
    npp(l,n)   = npp(l,n) - exudates_pft(l,n)
    npp_n(l,n) = npp(l,n)

    !-------------------------------------------------------------------------
    ! Calculate an updated version of plant respiration for outputting via
    ! diagnostics_veg, from GPP (stored earlier in the timestep from within
    ! PHYSIOL) and NPP, updated immediately above to account for nitrogen
    ! limitation. This is therefore the actual plant respiration, rather
    ! than the potential plant respiration output via STASHcodes 3663 and
    ! 3692.
    !
    ! The accumulated gpp (gpp_pft_acc) is the total kgC/m2 accumulated since
    ! the previous TRIFFID call, so needs scaling by r_gamma
    ! (360/triffid_period) to convert it to the same units as npp, kgC/m2/yr,
    ! which resp_p_actual_pft is also in.
    !-------------------------------------------------------------------------
    resp_p_actual_pft(l,n)   = (gpp_pft_acc(l,n) * r_gamma) - npp(l,n)

  END DO  !  trif_pts
END DO  !  PFTs

!-----------------------------------------------------------------------------
! If l_veg_compete=T, then there is the choice between the old co-competition
! (lotka_jls.F90)
! or the new purely height-based competition (l_ht_compete=T, recommended).
! If the user is not using the original 5 PFTs (BT, NT, C3, C4, SH),
! l_ht_compete must be True.
! If l_ht_compete=T, there are separate subroutines for dynamic (lotka_noeq)
! and equilibrium mode (lotka_eq).
!-----------------------------------------------------------------------------
IF ( l_veg_compete ) THEN

  IF ( l_trif_crop ) THEN
    ! Initialise.
    frac_na(:,:)      = frac(:,:)
    frac_nofire(:,:)  = frac(:,:)
    dfrac(:,:)        = 0.0
    dfrac_na(:,:)     = 0.0
    dfrac_nofire(:,:) = 0.0
    ! Loop over vegetation types: natural, crop, pasture, biocrop
    DO k = 1,veg_types
      crop_lotka = k - 1
      IF ( crop_lotka == 0 ) THEN
        DO l = 1,land_pts
          subset_space(l)      = frac_agric(l) + frac_past(l) + frac_biocrop(l)
          subset_space_prev(l) = frac_agr_prev_gb(l) + frac_past_prev_gb(l)    &
                                 + frac_biocrop_prev_gb(l)
        END DO
      ELSE IF ( crop_lotka == 1 ) THEN
        DO l = 1,land_pts
          subset_space(l)      = frac_agric(l)
          subset_space_prev(l) = frac_agr_prev_gb(l)
        END DO
      ELSE IF ( crop_lotka == 2 ) THEN
        DO l = 1,land_pts
          subset_space(l)      = frac_past(l)
          subset_space_prev(l) = frac_past_prev_gb(l)
        END DO
      ELSE IF ( crop_lotka == 3 ) THEN
        DO l = 1,land_pts
          subset_space(l)      = frac_biocrop(l)
          subset_space_prev(l) = frac_biocrop_prev_gb(l)
        END DO
      END IF
      nsub = 0
      DO n = 1,npft
        IF ( crop(n) == crop_lotka ) THEN
          nsub = nsub + 1
        END IF
      END DO
      IF ( nsub > 0 ) THEN
        CALL lotka_noeq_subset( land_pts, trif_pts, nsub, crop_lotka,          &
                                trif_index, r_gamma,                           &
                                c_veg, subset_space, subset_space_prev,        &
                                ht, pc_s_pft, frac, dfrac, frac_na, dfrac_na,  &
                                frac_nofire, dfrac_nofire, g_burn_pft)

      END IF
    END DO  !  veg types

  ELSE

    ! .NOT. l_trif_crop
    IF ( l_ht_compete ) THEN

      IF ( l_trif_eq ) THEN

        frac_prev(:,:) = frac(:,:)   ! For diagnosing bare soil
        CALL lotka_eq (land_pts, trif_pts, trif_index,                         &
                       c_veg, frac_prev, frac_agric, ht, pc_s_pft,             &
                       frac, frac_nofire, g_burn_pft)

        frac_na(:,:)      = frac(:,:)
        dfrac(:,:)        = 0.0
        dfrac_na(:,:)     = 0.0
        dfrac_nofire(:,:) = 0.0


      ELSE

        CALL lotka_noeq( land_pts, trif_pts, trif_index,                       &
                         r_gamma, c_veg, frac_agric, frac_agr_prev_gb,         &
                         ht, pc_s_pft,                                         &
                         frac, frac_na,                                        &
                         frac_nofire, dfrac, dfrac_na, dfrac_nofire, g_burn_pft)

      END IF

    ELSE
      ! .NOT. l_ht_compete

      CALL lotka (land_pts, trif_pts, trif_index,                              &
                  forw, r_gamma, c_veg, frac_agric, frac_agr_prev_gb,          &
                  lai_bal_pft, pc_s_pft,                                       &
                  frac, frac_na, frac_nofire, dfrac, dfrac_na,                 &
                  dfrac_nofire, g_burn_pft)

    END IF  !  l_ht_compete

  END IF  !  l_trif_crop

ELSE

  ! .NOT. l_veg_compete
  frac_na(:,:)      = frac(:,:)
  frac_nofire(:,:)  = frac(:,:)
  dfrac(:,:)        = 0.0
  dfrac_na(:,:)     = 0.0
  dfrac_nofire(:,:) = 0.0

END IF  !  l_veg_compete

!-----------------------------------------------------------------------------
! Calculate PFT fractions to be used in the calculation of gridbox
! mean fluxes.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  DO n = 1,npft
    frac_flux(l,n)        = frac(l,n)        - (1.0 - forw) * dfrac(l,n)
    frac_flux_nat(l,n)    = frac_na(l,n)     - (1.0 - forw) * dfrac_na(l,n)
    frac_flux_nofire(l,n) = frac_nofire(l,n) - (1.0 - forw) *                  &
                                                             dfrac_nofire(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! N from fertiliser.
!-----------------------------------------------------------------------------
CALL soil_n_fertiliser( land_pts, trif_pts, fert_layer, r_gamma, have_layers,  &
                        trif_index, dz, frac_flux, f_root_pft,                 &
                        f_root_pft_dz, n_fertiliser_pft,                       &
                        n_inorg_avail_pft, n_inorg_soilt_lyrs,                 &
                        n_fertiliser_gb, n_fertiliser_add)

!-----------------------------------------------------------------------------
! Plant uptake of N.
!-----------------------------------------------------------------------------
CALL soil_n_uptake( land_pts, trif_pts, r_gamma, have_layers,                  &
                    trif_index, frac_flux, f_root_pft, isunfrozen,             &
                    n_inorg_avail_pft, n_inorg_soilt_lyrs,                     &
                    n_uptake_pft, n_uptake_gb, n_uptake_extract,               &
               ! These arguments replace USE statements
                   !prognostics
                   n_inorg_gb)

!-----------------------------------------------------------------------------
! Calculate gridbox diagnostics, using fracs prior to competition.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  DO n = 1,npft

    n_demand_gb(l) = n_demand_gb(l) + frac_flux(l,n) * n_demand_pft(l,n)
    exudates_gb(l) = exudates_gb(l) + frac_flux(l,n) * exudates_pft(l,n)
    npp_n_gb(l)    = npp_n_gb(l)    + frac_flux(l,n) * npp_n(l,n)
    resp_p_actual_gb(l) = resp_p_actual_gb(l) +                                &
                          frac_flux(l,n) * resp_p_actual_pft(l,n)

  END DO  !  PFTs
END DO  !  points

!-----------------------------------------------------------------------------
! Diagnose the litterfall from the carbon balance of each vegetation type.
! Including vegetation carbon losses due to crop harvest and landuse change.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  lit_c_t(l)                = 0.0
  lit_N_t_gb(l)             = 0.0
  harvest_gb(l)             = 0.0
  harvest_n_gb(l)           = 0.0
  harvest_biocrop_gb(l)     = 0.0
  harvest_biocrop_n_gb(l)   = 0.0
  veg_c_fire_emission_gb(l) = 0.0
  root_abandon_gb(l)        = 0.0
  root_abandon_n_gb(l)      = 0.0
  n_LUC(l)                  = 0.0
  g_burn_gb(l)              = 0.0

  DO n = 1,nnpft
    lit_c_ag_pft(l,n)       = 0.0
    lit_c_orig_pft(l,n)     = 0.0
    lit_c_fire_pft(l,n)     = 0.0
    lit_c_nofire_pft(l,n)   = 0.0
    lit_n_fire_pft(l,n)     = 0.0
    lit_n_nofire_pft(l,n)   = 0.0
    veg_c_fire_emission_pft(l,n) = 0.0
    harvest_pft(l,n)        = 0.0
    harvest_n_pft(l,n)      = 0.0
    harvest_biocrop_pft(l,n) = 0.0
    harvest_biocrop_n_pft(l,n) = 0.0

    lit_c_orig_pft(l,n) = calc_litter_flux(npp(l,n), c_veg(l,n),               &
                                           dcveg_pft(l,n), frac(l,n),          &
                                           dfrac(l,n), r_gamma,                &
                                           frac_flux(l,n))
    lit_n_orig_pft(l,n) = calc_litter_flux(n_uptake_pft(l,n), n_veg_pft(l,n),  &
                                           dnveg_pft(l,n), frac(l,n),          &
                                           dfrac(l,n), r_gamma,                &
                                           frac_flux(l,n))

    ! ------------------------------------------------------------------------
    ! Diagnose the frac fluxes from 'natural' and frac_agric changes.
    ! If it's a crop type all the change in frac should be considered
    ! 'natural'.
    IF ( (crop(n) == 1) .AND. ( .NOT. l_trif_crop) ) THEN
      ! CASE 1 - CROPS
      lit_c(l,n)        = lit_c_orig_pft(l,n)
      lit_c_ag_pft(l,n) = 0.0
      lit_n_pft(l,n)    = lit_n_orig_pft(l,n)
      lit_n_ag_pft(l,n) = 0.0

    ELSE

      ! DFRAC>0.0 - expansion of frac in presence of Agric
      IF (frac(l,n) <  frac_na(l,n)) THEN
        ! CASE 2 - CONTRACTION DUE TO LAND USE
        lit_c(l,n) = calc_litter_flux(npp(l,n), c_veg(l,n), dcveg_pft(l,n),    &
                                      frac_na(l,n), dfrac_na(l,n), r_gamma,    &
                                      frac_flux_nat(l,n))

        lit_c_ag_pft(l,n) = lit_c_orig_pft(l,n) - lit_c(l,n)

        lit_n_pft(l,n) = calc_litter_flux(n_uptake_pft(l,n), n_veg_pft(l,n),   &
                                          dnveg_pft(l,n), frac_na(l,n),        &
                                          dfrac_na(l,n), r_gamma,              &
                                          frac_flux_nat(l,n))

        lit_n_ag_pft(l,n) = lit_n_orig_pft(l,n) - lit_n_pft(l,n)

        IF (lit_c_ag_pft(l,n) <  0.0) THEN
          lit_c_ag_pft(l,n) = 0.0
          lit_c(l,n)        = lit_c_orig_pft(l,n)
          lit_n_ag_pft(l,n) = 0.0
          lit_n_pft(l,n)    = lit_n_orig_pft(l,n)
        END IF

      ELSE

        ! frac(l,n) >=  frac_na(l,n)
        ! CASE 3 - NON-CROP EXPANSION

        lit_c(l,n)        = lit_c_orig_pft(l,n)
        lit_c_ag_pft(l,n) = 0.0

        lit_n_pft(l,n)    = lit_n_orig_pft(l,n)
        lit_n_ag_pft(l,n) = 0.0

      END IF  !  frac(l,n) <  frac_na(l,n)
    END IF  !  test on crop and l_trif_crop

    !-------------------------------------------------------------------------
    ! Diagnose the frac fluxes and emissions from fire
    !-------------------------------------------------------------------------
    ! LITTER CARBON
    ! Litter carbon with LUC, without fire
    lit_c_nofire_pft(l,n) = calc_litter_flux(npp(l,n),c_veg(l,n),              &
                                             dcveg_pft(l,n),frac_nofire(l,n),  &
                                             dfrac_nofire(l,n),                &
                                             r_gamma,frac_flux_nofire(l,n))

    ! Litter carbon from fire = (total litter with fire) -
    !                           (total litter without fire)
    lit_c_fire_pft(l,n) = lit_c_orig_pft(l,n) - lit_c_nofire_pft(l,n)

    ! LITTER NITROGEN
    ! Litter nitrogen with land use change, without fire
    lit_n_nofire_pft(l,n) = calc_litter_flux(n_uptake_pft(l,n),                &
                                             n_veg_pft(l,n),dnveg_pft(l,n),    &
                                             frac_nofire(l,n),                 &
                                             dfrac_nofire(l,n),r_gamma,        &
                                             frac_flux_nofire(l,n))

    ! Litter nitrogen from fire = (total litter with fire) -
    !                             (total litter without fire)
    lit_n_fire_pft(l,n) = lit_n_orig_pft(l,n) - lit_n_nofire_pft(l,n)

    IF (lit_c_fire_pft(l,n) <  0.0) THEN
      lit_c_fire_pft(l,n) = 0.0
      lit_n_fire_pft(l,n) = 0.0
    END IF

    veg_c_fire_emission_pft(l,n) = lit_c_fire_pft(l,n) * fire_ratio

    veg_c_fire_emission_gb(l) = veg_c_fire_emission_gb(l) +                    &
                                veg_c_fire_emission_pft(l,n)
    lit_c(l,n) = lit_c(l,n) - veg_c_fire_emission_pft(l,n)

    !-------------------------------------------------------------------------
    ! Harvest crop carbon
    !-------------------------------------------------------------------------
    IF (l_trif_biocrop) THEN
      ! Populate harvest variables
      harvest_biocrop_pft(l,n)     = 0.0
      harvest_biocrop_n_pft(l,n)   = 0.0
      harvest_pft(l,n)     = 0.0
      harvest_n_pft(l,n)   = 0.0

      ! Move harvested biomass to harvest pools
      IF ((harvest_type(n) == 2) .AND. (harvest_switch(l,n)) ) THEN
        ! Harvest amount = old aboveground biomass minus new aboveground
        ! biomass, per 360days
        harvest_biocrop_pft(l,n) = (init_agb_c(l,n) - woodc_pft(l,n)           &
                                   - leafc_pft(l,n)) * r_gamma
        harvest_biocrop_gb(l) = harvest_biocrop_gb(l)                          &
                                + (frac(l,n) * harvest_biocrop_pft(l,n))

        harvest_biocrop_n_pft(l,n) = (init_agb_n(l,n) - n_stem_trif_pft(l,n)   &
                                      - n_leaf_trif_pft(l,n)) * r_gamma
        harvest_biocrop_n_gb(l) = harvest_biocrop_n_gb(l)                      &
                                  + (frac(l,n) * harvest_biocrop_n_pft(l,n))

        ! Belowground, (old minus new) biomass is not harvested, but added to
        ! litter flux.
        lit_c(l,n) = lit_c(l,n) + r_gamma * (init_root_c(l,n) - rootc_pft(l,n))
        lit_n_pft(l,n) = lit_n_pft(l,n) + r_gamma * ( init_root_n(l,n)         &
                          - n_root_trif_pft(l,n) )

        ! if harvest_type=1, harvest is taken from litter
      ELSE IF ( (harvest_type(n) == 1) .AND. (lit_c(l,n) > 0.0) .AND.          &
        (lit_n_pft(l,n) > 0.0) ) THEN
        harvest_pft(l,n)   = harvest_rate * lit_c(l,n)
        lit_c(l,n)         = lit_c(l,n) - harvest_pft(l,n)
        harvest_gb(l)      = harvest_gb(l) + harvest_pft(l,n)

        harvest_n_pft(l,n) = harvest_rate * lit_n_pft(l,n)
        lit_n_pft(l,n)     = lit_n_pft(l,n) - harvest_n_pft(l,n)
        harvest_n_gb(l)    = harvest_n_gb(l) + harvest_n_pft(l,n)
      END IF ! harvest_type =2

      ! If l_trif_biocrop=F but l_trif_crop=T, harvest as per harvest_type=1
    ELSE IF ( l_trif_crop ) THEN
      IF ( (crop(n) == 1.0) .AND. (lit_c(l,n) > 0.0) .AND.                     &
           (lit_n_pft(l,n) > 0.0) ) THEN
        harvest_pft(l,n)   = harvest_rate * lit_c(l,n)
        lit_c(l,n)         = lit_c(l,n) - harvest_pft(l,n)
        harvest_gb(l)      = harvest_gb(l) + harvest_pft(l,n)

        harvest_n_pft(l,n) = harvest_rate * lit_n_pft(l,n)
        lit_n_pft(l,n)     = lit_n_pft(l,n) - harvest_n_pft(l,n)
        harvest_n_gb(l)    = harvest_n_gb(l) + harvest_n_pft(l,n)
      END IF
    END IF

    !-------------------------------------------------------------------------
    ! Fate of vegetation carbon removed by landuse change
    !-------------------------------------------------------------------------
    ! If cratio=1.0 all goes into the soil
    ! If cratio=0.0 all goes to wood products
    IF (l_landuse) THEN
      cratio = rootc_pft(l,n) / c_veg(l,n)
      nratio = n_root_trif_pft(l,n) / n_veg_pft(l,n)
    ELSE
      cratio = 1.0
      nratio = 1.0
    END IF

    ! If disturb_crop=1.0 crop PFTs can experience vegetation carbon loss due
    !                     to landuse change
    ! If disturb_crop=0.0 crop PFTs can not lose vegetation carbon loss due to
    !                     landuse change, instead some fraction of the
    !                     existing vegetation carbon is taken to be crop
    !                     carbon not natural grass carbon
    IF ( l_trif_crop ) THEN
      disturb_crop = 1.0
    ELSE
      disturb_crop = 1.0 - REAL(crop(n))
    END IF

    ! Calculate the gridbox mean vegetation to soil flux
    ! Including the root carbon abandoned during landuse change
    lit_c_t(l)    = lit_c_t(l) +                                               &
                    lit_c(l,n) +                                               &
                    lit_c_ag_pft(l,n) * cratio * disturb_crop
    lit_n_t_gb(l) = lit_n_t_gb(l) +                                            &
                    lit_n_pft(l,n) +                                           &
                    lit_n_ag_pft(l,n) * nratio * disturb_crop

    ! Calculate the root_abandon diagnostics
    root_abandon_pft(l,n)   = lit_c_ag_pft(l,n) * cratio * disturb_crop
    root_abandon_n_pft(l,n) = lit_n_ag_pft(l,n) * nratio * disturb_crop
    root_abandon_gb(l)      = root_abandon_gb(l) + root_abandon_pft(l,n)
    root_abandon_n_gb(l)    = root_abandon_n_gb(l) + root_abandon_n_pft(l,n)

    ! Remove the root carbon abandoned during landuse change from the
    ! vegetation to product pool flux
    lit_c_ag_pft(l,n) = lit_c_ag_pft(l,n) * (1.0 - cratio) * disturb_crop
    lit_n_ag_pft(l,n) = lit_n_ag_pft(l,n) * (1.0 - nratio) * disturb_crop

    ! This is total N flux from LUC - in this instance lost from the system.
    ! Equivalent C flux is put through WP pool before being emitted to the
    ! atmosphere.
    n_LUC(l) = n_LUC(l) + lit_n_ag_pft(l,n)

  END DO  !  PFTs
END DO  !  points

DO n = 1,nnpft
  DO l = 1,land_pts
    g_burn_gb(l) = g_burn_gb(l) + frac(l,n) * g_burn_pft(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Check carbon conservation (2/3)
! Add the litter fluxes to the net carbon fluxes.
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  cnsrv_veg_flux(l) = cnsrv_veg_flux(l) -                                      &
                      ( SUM(lit_c_ag_pft(l,:)) + veg_c_fire_emission_gb(l)     &
                        + lit_c_t(l) + harvest_gb(l) ) / r_gamma
  cnsrv_soil_flux(l) = lit_c_t(l) / r_gamma
  cnsrv_prod_flux(l) = cnsrv_prod_flux(l) + SUM(lit_c_ag_pft(l,:)) / r_gamma
  cnsrv_veg2_correction(l) = (veg_c_fire_emission_gb(l) + harvest_gb(l)) /     &
                             r_gamma
END DO

IF ( soil_bgc_model == soil_model_4pool ) THEN
  !---------------------------------------------------------------------------
  ! Update the soil carbon and nitrogen contents.
  !---------------------------------------------------------------------------

  IF ( .NOT. l_layeredC) THEN
    CALL soilcarb (land_pts, trif_pts, trif_index,                             &
                   forw, r_gamma, lit_c, lit_c_t, lit_n_t_gb, resp_frac(:,1),  &
                   resp_s, cs, ns_gb, neg_n,                                   &
                   implicit_resp_correction, burnt_soil,                       &
                  !New arguments replacing USE statements
                  !prognostics
                  ns_pool_gb, n_inorg_soilt_lyrs,                              &
                  !trif_vars_mod
                  burnt_carbon_dpm, g_burn_gb, burnt_carbon_rpm,               &
                  minl_n_gb, minl_n_pot_gb, immob_n_gb, immob_n_pot_gb,        &
                  fn_gb, resp_s_diag_gb, resp_s_pot_diag_gb,                   &
                  dpm_ratio_gb, n_gas_gb, resp_s_to_atmos_gb,                  &
                  !p_s_parms
                  sthu_soilt)
  ELSE
    CALL soilcarb_layers (land_pts, trif_pts, trif_index, forw, r_gamma,       &
                          lit_c, lit_c_t, lit_n_t_gb, resp_frac_cspool,        &
                          resp_s, cs, frac_c_label_pool,                       &
                          ns_gb, neg_n, implicit_resp_correction,              &
                          burnt_soil, isunfrozen,                              &
                         !New arguments replacing USE statements
                         !prognostics
                         ns_pool_gb, n_inorg_soilt_lyrs, n_inorg_avail_pft,    &
                         t_soil_soilt_acc,                                     &
                         !trif_vars_mod
                         burnt_carbon_dpm, g_burn_gb, burnt_carbon_rpm,        &
                         minl_n_gb, minl_n_pot_gb, immob_n_gb, immob_n_pot_gb, &
                         fn_gb, resp_s_diag_gb, resp_s_pot_diag_gb,            &
                         dpm_ratio_gb, n_gas_gb, resp_s_to_atmos_gb,           &
                         !p_s_parms
                         sthu_soilt)
  END IF

  !---------------------------------------------------------------------------
  ! Turn over the inorganic N pool.
  !---------------------------------------------------------------------------
  DO t = 1,trif_pts
    l = trif_index(t)
    IF (l_layeredC) THEN !turnover on soil levels using tau_resp
      DO nn = 1,dim_cslayer
        ftemp(l,nn) = 0.0
        IF (t_soil_soilt_acc(l,1,nn) > 263.15) THEN
          ftemp(l,nn) = 47.9 / (1.0 + EXP(106.0 /                              &
              (t_soil_soilt_acc(l,1,nn) - 254.85)))
        END IF
        inorg_decay = EXP( -tau_resp * (SUM(dz(1:nn)) -                        &
                           0.5 * dz(nn)) ) * ftemp(l,nn)
        n_loss_gb(l) = n_loss_gb(l) +                                          &
                       MAX(0.0, n_inorg_turnover * inorg_decay *               &
                       n_inorg_soilt_lyrs(l,1,nn))
        n_inorg_soilt_lyrs(l,1,nn) = n_inorg_soilt_lyrs(l,1,nn) -              &
                                     MAX( 0.0,                                 &
                                     n_inorg_turnover * inorg_decay *          &
                                     n_inorg_soilt_lyrs(l,1,nn) ) / r_gamma
        DO n = 1,npft
          n_inorg_avail_pft(l,n,nn) = n_inorg_avail_pft(l,n,nn) -              &
                                    MAX(0.0, n_inorg_turnover * inorg_decay *  &
                                    n_inorg_avail_pft(l,n,nn)) / r_gamma
        END DO
        IF (n_inorg_soilt_lyrs(l,1,nn) < 0.0) THEN
          n_loss_gb(l) = n_loss_gb(l) + (n_inorg_soilt_lyrs(l,1,nn) * r_gamma)
          n_inorg_soilt_lyrs(l,1,nn) = 0.0
        END IF
        DO n = 1,npft
          ! Diffuse N between available and unavailable pools.
          ! 0.5 is arbtriary defines threshold to keep n_inorg_avail_pft
          ! stable - see the user guide for more details.
          IF (l_trif_eq .OR. (diff_n_pft > r_gamma * 0.5) ) THEN
            n_inorg_avail_pft(l,n,nn) = f_root_pft_dz(n,nn) *                  &
                                        n_inorg_soilt_lyrs(l,1,nn)
          ELSE
            n_inorg_avail_pft(l,n,nn) = n_inorg_avail_pft(l,n,nn) +            &
                                        (diff_n_pft * ( (f_root_pft_dz(n,nn) * &
                                        n_inorg_soilt_lyrs(l,1,nn)) -          &
                                        n_inorg_avail_pft(l,n,nn) ) / r_gamma)
          END IF
        END DO
      END DO

    ELSE !IF .not. l_layeredC

      n_loss_gb(l) = MAX(0.0, n_inorg_turnover * n_inorg_soilt_lyrs(l,1,1))
      n_inorg_soilt_lyrs(l,1,1) = n_inorg_soilt_lyrs(l,1,1) -                  &
                                  (n_loss_gb(l) / r_gamma)
      !     This condition only occurs if soil pools are at min N content due
      !     to either pool exhaustion or unmet seed N demand.
      IF (n_inorg_soilt_lyrs(l,1,1) < 0.0) THEN
        n_loss_gb(l) = n_loss_gb(l) + (n_inorg_soilt_lyrs(l,1,1) * r_gamma)
        n_inorg_soilt_lyrs(l,1,1) = 0.0
      END IF

    END IF  !  l_layeredC
  END DO  !  points
END IF  !  soil_model_4pool

!-----------------------------------------------------------------------------
! Diagnose the gridbox mean vegetation carbon and nitrogen
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)

  litterC_pft(l,:) = root_litC_pft(l,:) +                                      &
                     leaf_litc_pft(l,:) + wood_litc_pft(l,:)
  litterN_pft(l,:) = root_litn_pft(l,:) +                                      &
                     leaf_litn_pft(l,:) + wood_litn_pft(l,:)

  cv(l)        = 0.0
  leafC_gbm(l) = 0.0
  woodC_gbm(l) = 0.0
  rootC_gbm(l) = 0.0
  n_veg_gb(l)  = 0.0
  DO n = 1,nnpft
    cv(l)        = cv(l)       + frac(l,n) * c_veg(l,n)
    n_veg_gb(l)  = n_veg_gb(l) + frac(l,n) * n_veg_pft(l,n)
    leafC_gbm(l) = leafC_gbm(l) + frac(l,n) * leafC_pft(l,n)
    woodC_gbm(l) = woodC_gbm(l) + frac(l,n) * woodC_pft(l,n)
    rootC_gbm(l) = rootC_gbm(l) + frac(l,n) * rootC_pft(l,n)
  END DO
  dnveg_gb(l) = dnveg_gb(l) + n_veg_gb(l)
  dcveg_gb(l) = dcveg_gb(l) + cv(l)

END DO

DO l = 1,land_pts
  frac_agr_prev_gb(l)  = frac_agric(l)
  frac_past_prev_gb(l) = frac_past(l)
  frac_biocrop_prev_gb(l) = frac_biocrop(l)
END DO

!-----------------------------------------------------------------------------
! Update the Wood Product Pools
!-----------------------------------------------------------------------------

IF (forw == 0.0) THEN
  CALL woodprod ( land_pts, trif_pts, trif_index,                              &
                  r_gamma,                                                     &
                  lit_c_ag_pft,                                                &
                  wood_prod_fast_gb, wood_prod_med_gb,                         &
                  wood_prod_slow_gb,                                           &
                  wp_fast_in_gb, wp_med_in_gb, wp_slow_in_gb,                  &
                  wp_fast_out_gb, wp_med_out_gb, wp_slow_out_gb )
ELSE
  wood_prod_fast_gb(:) = 0.0
  wood_prod_med_gb(:)  = 0.0
  wood_prod_slow_gb(:) = 0.0
END IF

!-----------------------------------------------------------------------------
! Change the units of PFT litter fluxes for output
! Change units to kg/(m2 of PFT)/(360 days)
!            from kg/(m2 of land)/(360 days)
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)
  DO n = 1,nnpft
    IF (frac(l,n) < frac_na(l,n)) THEN
      lit_c(l,n)            = lit_c(l,n)        / frac_flux_nat(l,n)
      lit_n_pft(l,n)        = lit_n_pft(l,n)    / frac_flux_nat(l,n)
      lit_c_ag_pft(l,n)     = lit_c_ag_pft(l,n) / frac_flux(l,n)
      lit_n_ag_pft(l,n)     = lit_n_ag_pft(l,n) / frac_flux(l,n)
    ELSE
      lit_c(l,n)            = lit_c(l,n)        / frac_flux(l,n)
      lit_n_pft(l,n)        = lit_n_pft(l,n)    / frac_flux(l,n)
    END IF

    IF (frac(l,n) < frac_nofire(l,n)) THEN
      lit_c_fire_pft(l,n) = lit_c_fire_pft(l,n) / frac_flux(l,n)
      lit_n_fire_pft(l,n) = lit_n_fire_pft(l,n) / frac_flux(l,n)
      veg_c_fire_emission_pft(l,n) = veg_c_fire_emission_pft(l,n) /            &
                                     frac_flux(l,n)
    END IF

    harvest_pft(l,n)        = harvest_pft(l,n)        / frac_flux(l,n)
    harvest_n_pft(l,n)      = harvest_n_pft(l,n)      / frac_flux(l,n)
    lit_c_orig_pft(l,n)     = lit_c_orig_pft(l,n)     / frac_flux(l,n)
    lit_n_orig_pft(l,n)     = lit_n_orig_pft(l,n)     / frac_flux(l,n)
    root_abandon_pft(l,n)   = root_abandon_pft(l,n)   / frac_flux(l,n)
    root_abandon_n_pft(l,n) = root_abandon_n_pft(l,n) / frac_flux(l,n)
  END DO
END DO

!-----------------------------------------------------------------------------
! Take copies of fluxes to be used for output diagnostics
!-----------------------------------------------------------------------------
! This is the diagnostic of litter per m2 PFT after harvesting but excluding
! roots from LUC
lit_n_pft_diag(:,:) = lit_n_pft(:,:)
! This is the diagnotic of land-use change includes harvest flux but excluding
! roots from LUC which goes to soil
lit_n_ag_pft_diag(:,:) = lit_n_ag_pft(:,:)

!-----------------------------------------------------------------------------
! Check carbon conservation (3/3)
! Add the wood product decay flux to the net carbon fluxes.
! Calculate the error in carbon conservation as:
! error=(final store)-(initial store)-(net flux)
!      ="extra carbon in store not accounted for by fluxes"
!-----------------------------------------------------------------------------
DO t = 1,trif_pts
  l = trif_index(t)

  wp_total_out_gb(l)      = (wp_fast_out_gb(l) + wp_med_out_gb(l)              &
                            + wp_slow_out_gb(l))  / r_gamma

  wood_prod_total_gb(l)   = wood_prod_fast_gb(l) + wood_prod_med_gb(l) +       &
                            wood_prod_slow_gb(l)

  cnsrv_veg2_correction(l) = cnsrv_veg2_correction(l) + wp_total_out_gb(l)     &
                             + wood_prod_total_gb(l)                           &
                             + burnt_soil(l) / r_gamma

  cnsrv_prod_flux(l)      = cnsrv_prod_flux(l) - wp_total_out_gb(l)

  cnsrv_veg_triffid_gb(l) = cv(l) - cnsrv_veg_triffid_gb(l) -                  &
                            cnsrv_veg_flux(l) +                                &
                            exudates_gb(l) / r_gamma

  cnsrv_vegN_triffid_gb(l) = n_veg_gb(l) - cnsrv_vegN_triffid_gb(l)            &
                             - (n_uptake_gb(l) - lit_n_t_gb(l)                 &
                                - harvest_n_gb(l)                              &
                                - n_luc(l))                                    &
                             / r_gamma

  cnsrv_prod_triffid_gb(l) = wood_prod_total_gb(l ) -                          &
                             cnsrv_prod_triffid_gb(l) -                        &
                             cnsrv_prod_flux(l)

  IF ( soil_bgc_model == soil_model_4pool ) THEN
    IF (l_layeredc) THEN
      cnsrv_soil_resp(l) = SUM( (1.0 - resp_frac_cspool(l,:,1:4)) *            &
                                                          resp_s(l,:,1:4) )
    ELSE
      cnsrv_soil_resp(l) = SUM( (1.0 - resp_frac(l,:)) * resp_s(l,:,5) )
    END IF

    cnsrv_soil_flux(l) =  cnsrv_soil_flux(l) - cnsrv_soil_resp(l) / r_gamma

    cnsrv_carbon_flux(l) = cnsrv_carbon_flux(l) - wp_total_out_gb(l)           &
                           - ( veg_c_fire_emission_gb(l) + harvest_gb(l)       &
                               + burnt_soil(l) + cnsrv_soil_resp(l)) / r_gamma

    cnsrv_soil_triffid_gb(l) = SUM(cs(l,:,:)) - cnsrv_soil_triffid_gb(l) -     &
                             cnsrv_soil_flux(l) -                              &
                             implicit_resp_correction(l) +                     &
                             burnt_soil(l) / r_gamma

    cnsrv_soilN_triffid_gb(l) = SUM(ns_gb(l,:)) - cnsrv_soilN_triffid_gb(l)    &
                                - (lit_n_t_gb(l) - SUM(minl_n_gb(l,:,5))       &
                                + SUM(immob_n_gb(l,:,5)) ) / r_gamma

    cnsrv_N_inorg_triffid_gb(l) = SUM(N_inorg_soilt_lyrs(l,:,:))               &
                                  - cnsrv_N_inorg_triffid_gb(l)                &
                                  - (n_fertiliser_gb(l) - n_uptake_gb(l)       &
                                  - n_loss_gb(l) + deposition(l)               &
                                  + n_fix_gb(l) + SUM(minl_n_gb(l,:,5))        &
                                  - SUM(immob_n_gb(l,:,5))                     &
                                  - SUM(n_gas_gb(l,:)))  / r_gamma

    cnsrv_carbon_triffid_gb(l) = (cv(l) + SUM(cs(l,:,:)) +                     &
                                 wood_prod_total_gb(l) ) -                     &
                                 cnsrv_carbon_triffid_gb(l) -                  &
                                 cnsrv_carbon_flux(l) -                        &
                                 implicit_resp_correction(l) +                 &
                                 exudates_gb(l) / r_gamma

    cnsrv_Nitrogen_triffid_gb(l) = (SUM(ns_gb(l,:)) +  n_veg_gb(l)             &
                          + SUM(N_inorg_soilt_lyrs(l,:,:)))                    &
                          - cnsrv_Nitrogen_triffid_gb(l)                       &
                          ! VegN
                          - (n_uptake_gb(l) - lit_n_t_gb(l)                    &
                          - harvest_n_gb(l) -  n_luc(l))                       &
                          / r_gamma                                            &
                          ! SoilN
                          - (lit_n_t_gb(l) - SUM(minl_n_gb(l,:,5))             &
                          + SUM(immob_n_gb(l,:,5)) ) / r_gamma                 &
                          ! N inorg
                          - (n_fertiliser_gb(l) - n_uptake_gb(l)               &
                          - n_loss_gb(l) + deposition(l) + n_fix_gb(l)         &
                          + SUM(minl_n_gb(l,:,5)) - SUM(immob_n_gb(l,:,5))     &
                          - SUM(n_gas_gb(l,:)))  / r_gamma

  END IF  !  soil_bgc_model == soil_model_4pool

END DO

! Variable n_inorg_soilt_lyrs has been updated since it was last copied to
! n_inorg_gb. Since n_inorg_gb is the prognostic variable which gets written
! to start dumps in UM+JULES runs it needs updating here or the latest updates
! made to n_inorg_soilt_lyrs will be lost at the start of the next CRUN.
!
! This should not be necessary in the case l_layeredC=.TRUE. as in that case
! the prognostic variable is n_inorg_soilt_lyrs, and it is that which is
! output via io/model_interface/extract_var.inc . When UM+JULES runs are able
! to run with l_layeredC=.TRUE. it might be necessary to update n_inorg_gb
! with the sum over layers of n_inorg_soilt_lyrs.
IF ( (soil_bgc_model == soil_model_4pool) .AND. .NOT. l_layeredC) THEN
  n_inorg_gb(:) = n_inorg_soilt_lyrs(:,1,1)
END IF
!IF ( (soil_bgc_model == soil_model_4pool) .AND. l_layeredC) THEN
!  DO t = 1,trif_pts
!    l = trif_index(t)
!    n_inorg_gb(l) = SUM(n_inorg_soilt_lyrs(l,1,:))
!  END DO
!END IF

! For interactive CO2 runs, the harvest, exudates, and wood product pool
! fluxes of carbon need to be added to the atmosphere by BL_TRMIX_DD. Since
! TRIFFID is called later in the timestep than BL_TRMIX_DD, the fluxes need
! to be stored in as a prognostic in start dumps so that they're available at
! the start of the next CRUN.
!
! All are combined into one variable, triffid_co2_gb, in units of kgC/m2/yr.
! Scaling to the appropriate units for passing to the atmosphere is done in
! BL_TRMIX_DD.
!
! From comments in TRIF_VARS_MOD and WOODPROD, the individual fluxes are
! already in kgC/m2/yr so can be summed directly.
!
! Include veg_c_fire_emissions_gb and burnt_soil in the calculation of
! triffid_co2_gb, the loss of carbon as CO2 from all land sources. This
! assumes that all carbon lost from land due to fire is lost as CO2, in
! the absence of any explicit stores of burnt carbon. Failing to add it
! results in loss of conservation of carbon in CO2 emissions-driven
! configuration. Both variables have the same units (kg/m2/360 days) as
! exudates_gb, etc, as seen in src/io/model_interface/variable_metadata.F90.

DO t = 1,trif_pts
  l = trif_index(t)
  triffid_co2_gb(l) = harvest_gb(l)     +                                      &
                      exudates_gb(l)    +                                      &
                      wp_fast_out_gb(l) +                                      &
                      wp_med_out_gb(l)  +                                      &
                      wp_slow_out_gb(l) +                                      &
                      veg_c_fire_emission_gb(l) +                              &
                      burnt_soil(l)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE triffid

END MODULE triffid_mod
