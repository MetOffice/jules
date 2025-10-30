! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE trif_vars_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module holding various shared variables for TRIFFID
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Implementation for field variables:
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are generally
! not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines


TYPE :: trif_vars_data_type
  !-----------------------------------------------------------------------------
  ! Diagnostics
  !-----------------------------------------------------------------------------

  REAL(KIND=real_jlslsm), ALLOCATABLE :: wp_fast_in_gb(:)
                        ! C input to fast-turnover wood product pool
                        ! (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wp_med_in_gb(:)
                        ! C input to medium-turnover wood product pool
                        ! (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wp_slow_in_gb(:)
                        ! C input to slow-turnover wood product pool
                        ! (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wp_fast_out_gb(:)
                        ! C output from fast-turnover wood product pool
                        ! (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wp_med_out_gb(:)
                        ! C output from medium-turnover wood product pool
                        ! (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wp_slow_out_gb(:)
                        ! C output from slow-turnover wood product pool
                        ! (kg/m2/360days).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_c_orig_pft(:,:)
                        ! Loss of vegetation carbon due to litter,
                        ! landuse change and fire (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_c_ag_pft(:,:)
                        ! Carbon flux from vegetation to wood product pools
                        ! due to land use change (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_orig_pft(:,:)
                        ! Loss of vegetation nitrogen due to litter,
                        ! landuse change and fire (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_ag_pft(:,:)
                        ! Nitrogen removed from system due to landuse change
                        ! this flux is removed from vegetation and not added
                        ! to any other store (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_c_fire_pft(:,:)
                        ! Loss of vegetation carbon due to fire (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_c_nofire_pft(:,:)
                        ! Loss of vegetation carbon due to litter and
                        ! landuse change (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: burnt_carbon_dpm(:)
                        ! Loss of DPM carbon due fire (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_fire_pft(:,:)
                        ! Loss of vegetation nitrogen due to fire
                        ! (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_nofire_pft(:,:)
                        ! Loss of vegetation nitrogen due to litter and
                        ! landuse change (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: burnt_carbon_rpm(:)
                        ! Loss of RPM carbon due fire (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: veg_c_fire_emission_gb(:)
                        ! Gridbox mean carbon flux to the atmosphere from fire
                        ! (kg/(m2 land)/yr).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: veg_c_fire_emission_pft(:,:)
                        ! Carbon flux to the atmosphere from fire per PFT
                        ! (kg/(m2 PFT)/yr).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_s_to_atmos_gb(:,:)
                        ! Soil-to-atmosphere respiration flux
                        ! [kg m-2 (360 day)-1].
  REAL(KIND=real_jlslsm), ALLOCATABLE :: root_abandon_pft(:,:)
                        ! Root carbon moved to soil carbon during
                        ! landuse change (kg/(m2 PFT)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: root_abandon_gb(:)
                        ! Root carbon moved to soil carbon during
                        ! landuse change (kg/(m2 land)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_pft(:,:)
                        ! Carbon harvested from crops (kg/(m2 PFT)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_gb(:)
                        ! Gridbox mean carbon harvested from crops
                        ! (kg/(m2 land)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: root_abandon_n_pft(:,:)
                        ! Root nitrogen moved to soil nitrogen during
                        ! landuse change (kg/(m2 PFT)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: root_abandon_n_gb(:)
                        ! Root nitrogen moved to soil nitrogen during
                        ! landuse change (kg/(m2 land)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_n_pft(:,:)
                        ! Nitrogen harvested from crops (kg/(m2 PFT)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_n_gb(:)
                        ! Nitrogen harvested from crops: gridbox mean
                        ! (kg/(m2 land)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_fertiliser_pft(:,:)
                        ! Nitrogen available to crop PFTs in addition
                        ! to soil nitrogen (kg/(m2 PFT)/360days)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_fertiliser_gb(:)
                        ! N available to crop PFTs in addition to
                        ! soil N: gridbox mean (kg/(m2 land)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_leaf_pft(:,:)
                        ! Leaf N content scaled by LAI, in sf_stom (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_root_pft(:,:)
                        ! Root N content scaled by LAI_BAL, in sf_stom (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_stem_pft(:,:)
                        ! Stem N content scaled by LAI_BAL, in sf_stom (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_leaf_trif_pft(:,:)
                        ! Total Leaf N content (labile + allocated components)
                        ! scaled by lai_bal, in triffid (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_root_trif_pft(:,:)
                        ! Root N content scaled by LAI_BAL, in triffid (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_stem_trif_pft(:,:)
                        ! Stem N content scaled by LAI_BAL, in triffid (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_leaf_alloc_trif_pft(:,:)
                        ! Leaf N content allocated to leaf via phenology
                        ! (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_leaf_labile_trif_pft(:,:)
                        ! Leaf N content in labile leaf pool
                        ! (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_r_pft(:,:)
                        ! Root maintenance respiration (kg C/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_l_pft(:,:)
                        ! Leaf maintenance respiration (kg C/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lai_bal_pft(:,:)
                        ! Balanced lai from sf_stom.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_past_gb(:)
                        ! Fraction of pasture.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: pc_s_pft(:,:)
                        ! Carbon available for spreading a PFT (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_ag_pft_diag(:,:)
                        ! Nitrogen on tiles lost through landuse
                        ! including harvest (kg/m2/360d).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_luc(:)
                        ! Nitrogen lost through landuse
                        ! including harvest (kg/m2/360d).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_pft_diag(:,:)
                        ! Nitrogen on tiles flux to soil N
                        ! including harvest (kg/m2/360d).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fapar_diag_pft(:,:)
                        ! Fraction of Absorbed Photosynthetically Active
                        ! Radiation diagnostic.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: apar_diag_pft(:,:)
                        ! Absorbed Photosynthetically Active
                        ! Radiation diagnostic in W m-2.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: apar_diag_gb(:)
                        ! Absorbed Photosynthetically Active
                        ! Radiation diagnostic in W m-2.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fao_et0(:)
                        ! FAO Penman-Monteith evapotranspiration for
                        ! reference crop (kg m-2 s-1).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_carbon_veg2_gb(:)
                        ! Diagnostic of error in land carbon
                        ! conservation in the veg2 routine (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_carbon_triffid_gb(:)
                        ! Diagnostic of error in land carbon
                        ! conservation in the triffid routine (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_veg_triffid_gb(:)
                        ! Diagnostic of error in vegetation carbon
                        ! conservation in the triffid routine (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_soil_triffid_gb(:)
                        ! Diagnostic of error in soil carbon
                        ! conservation in the triffid routine (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_prod_triffid_gb(:)
                        ! Diagnostic of error in wood product carbon
                        ! conservation in the triffid routine (kg m-2).

  !-----------------------------------------------------------------------------
  ! Variables added for nitrogen conservation checks
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_nitrogen_triffid_gb(:)
                        ! Diagnostic of error in land nitrogen
                        ! conservation in the triffid routine (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_vegN_triffid_gb(:)
                        ! Diagnostic of error in vegetation nitrogen
                        ! conservation in the triffid routine (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_soilN_triffid_gb(:)
                        ! Diagnostic of error in soil nitrogen
                        ! conservation in the triffid routine (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cnsrv_N_inorg_triffid_gb(:)
                        ! Diagnostic of error in inorganic nitrogen
                        ! conservation in the triffid routine (kg m-2).

  !-----------------------------------------------------------------------------
  ! Variables added for ticket #7,#127 (nitrogen scheme)
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE :: deposition_n_gb(:)
                        ! Nitrogen deposition (kg/m2/s).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: leafC_pft(:,:)
                        ! Leaf carbon on PFTs (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rootC_pft(:,:)
                        ! Root carbon on PFTs (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: stemC_pft(:,:)
                        ! Stem carbon on PFTs (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: woodC_pft(:,:)
                        ! Wood carbon on PFTs (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: leafC_gbm(:)
                        ! Leaf carbon (GBM) (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rootC_gbm(:)
                        ! Root carbon (GBM) (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: woodC_gbm(:)
                        ! Wood carbon (GBM) (kg/m2).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: droot_pft(:,:)
                        ! Increment in leaf carbon on PFTs
                        ! (kg m-2 per TRIFFID timestep).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dleaf_pft(:,:)
                        ! Increment in leaf carbon on PFTs
                        ! (kg m-2 per TRIFFID timestep).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dwood_pft(:,:)
                        ! Increment in wood carbon on PFTs
                        ! (kg m-2 per TRIFFID timestep).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: root_litC_pft(:,:)
                        ! Root litter C turnover on PFTs (kg/m2/360 day).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: leaf_litC_pft(:,:)
                        ! Leaf litter C turnover on PFTs (kg/m2/360 day).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wood_litC_pft(:,:)
                        ! Wood litter C turnover on PFTs (kg/m2/360 day).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: root_litN_pft(:,:)
                        ! Root litter N turnover on PFTs (kg/m2/360 day).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: leaf_litN_pft(:,:)
                        ! Leaf litter N turnover on PFTs (kg/m2/360 day).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wood_litN_pft(:,:)
                        ! Wood litter N turnover on PFTs (kg/m2/360 day).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: litterC_pft(:,:)
                        ! Carbon in local litter production (kg/m2/360 day).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: litterN_pft(:,:)
                        ! Nitrogen in local litter production (kgN/m2/360 day).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_pft(:,:)
                        ! Nitrogen in total litter production (kgN/m2/360 day).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_uptake_growth_pft(:,:)
                        ! Vegetation N uptake for growth on PFTs
                        ! (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_demand_growth_pft(:,:)
                        ! Vegetation N demand for growth on PFTs
                        ! (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_demand_lit_pft(:,:)
                        ! Vegetation N demand for balanced litter
                        ! production on PFTs (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_demand_spread_pft(:,:)
                        ! Vegetation N demand for spreading on
                        ! PFTs(kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_uptake_spread_pft(:,:)
                        ! Vegetation N uptake for spreading in PFTs
                        ! (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_uptake_pft(:,:)
                        ! Vegetation N uptake on PFTs (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_demand_pft(:,:)
                        ! Vegetation N demand on PFTs(kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_uptake_gb(:)
                        ! Vegetation N uptake (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_demand_gb(:)
                        ! Vegetation N demand (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_veg_pft(:,:)
                        ! Veg N on PFTs (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_veg_gb(:)
                        ! Veg N (kg/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_loss_gb(:)
                        ! Other N Gaseous Loss (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_fix_pft(:,:)
                        ! Fixed N on PFTs (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_fix_gb(:)
                        ! Fixed N (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_leach_soilt(:,:)
                        ! Leached N (kg/m2/s).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_gas_gb(:,:)
                        ! Mineralised N Gas Emmissions (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dpm_ratio_gb(:)
                        ! Ratio of Decomposatble Plant Material to Resistant.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dnveg_pft(:,:)
                        ! Increment in veg N on PFTs (kg m-2 per TRIFFID
                        ! timestep).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dcveg_pft(:,:)
                        ! Increment in veg C on PFTs (kg m-2 per TRIFFID
                        ! timestep).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dnveg_gb(:)
                        ! Increment in veg N (kg m-2 per TRIFFID timestep).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: dcveg_gb(:)
                        ! Increment in veg C (kg m-2 per TRIFFID timestep).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: immob_n_gb(:,:,:)
                        ! Immobilised N on soil pools (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: immob_n_pot_gb(:,:,:)
                        ! Unlimited immobilised N on soil pools
                        ! (kg/m2/360 days).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: minl_n_gb(:,:,:)
                        ! Mineralised N on soil pools (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: minl_n_pot_gb(:,:,:)
                        ! Unlimited mineralised N on soil pools
                        ! (kg/m2/360 days).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_s_diag_gb(:,:,:)
                        ! Carbon in diagnosed soil respiration after TRIFFID
                        ! on soil pools (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_s_pot_diag_gb(:,:,:)
                        ! Carbon in diagnosed unlimited soil respiration after
                        ! TRIFFID (kg/m2/360 days).

  REAL(KIND=real_jlslsm), ALLOCATABLE :: fn_gb(:,:)
                        ! Nitrogen decomposition factor.

  REAL(KIND=real_jlslsm), ALLOCATABLE :: lit_n_t_gb(:)
                        ! Total N litter flux (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: exudates_pft(:,:)
                        ! Unallocated C due to lack of N, added to soil
                        ! respiration on PFTs ((kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: exudates_gb(:)
                        ! Unallocated C due to lack of N, added to soil
                        ! respiration ((kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: npp_n(:,:)
                        ! NPP post N limitation on PFTs (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: npp_n_gb(:)
                        ! NPP post N limitation (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp_pft_out(:,:)
                        ! Gross Primary Productivity on PFTs (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp_gb_out(:)
                        ! Gross Primary Productivity (GBM) (kg/m2/360 days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp_pft_acc(:,:)
                        ! Accumulated GPP on PFTs for calculating plant
                        ! respiration after N limitation on TRIFFID timesteps,
                        ! within TRIFFID itself (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gpp_gb_acc(:)
                        ! Accumulated GPP (GBM) for calculating plant
                        ! respiration after N limitation on TRIFFID timesteps,
                        ! within TRIFFID itself (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_p_actual_pft(:,:)
                        ! Carbon in plant respiration on PFTs after NPP
                        ! reduction due to nitrogen limitation (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: resp_p_actual_gb(:)
                        ! Carbon in plant respiration (GBM) after NPP
                        ! reduction due to nitrogen limitation (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_leach_gb_acc(:)
                        ! Accumulated leached nitrogen term for outputting
                        ! on leached N on TRIFFID timesteps via
                        ! diagnostics_veg.F90 (kg/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_burn_pft_acc(:,:)
                        ! Burnt area disturbance accumulation variable
                        ! If this was in the UM it would be considered a
                        ! prognostic (m2/m2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_burn_pft(:,:)
                        ! Burnt area disturbance (m2/m2/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: g_burn_gb(:)
                        ! Burnt area disturbance per gb (m2/m2/360days).

  !------------------------------------------------------------------------------
  ! Variables to pass fluxes to a soil model.
  !------------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_fertiliser_add(:,:,:)
                        ! Nitrogen added to each soil layer by fertiliser
                        ! (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_fix_add(:,:,:)
                        ! Nitrogen added to each soil layer by fixation
                        ! (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_uptake_extract(:,:,:)
                        ! Nitrogen removed from each soil layer by plant
                        ! uptake (kg m-2).

  !------------------------------------------------------------------------------
  ! Variables for biocrop functionality.
  !------------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_biocrop_pft(:,:)
                        ! Carbon harvested from biocrops (kg/(m2 PFT)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_biocrop_gb(:)
                        ! Carbon harvested from biocrops (kg/(m2 land)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_biocrop_n_pft(:,:)
                        ! Nitrogen harvested from biocrops (kg/(m2 PFT)/360days).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: harvest_biocrop_n_gb(:)
                        ! Nitrogen harvested from biocrops: gridbox mean
                        ! (kg/(m2 land)/360days).
  INTEGER, ALLOCATABLE :: harvest_doy(:,:)
                        ! Day of year in which periodic harvest occurs
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_biocrop_gb(:)
                        ! Fraction of biocrop.


END TYPE trif_vars_data_type


TYPE :: trif_vars_type
  !-----------------------------------------------------------------------------
  ! Diagnostics
  !-----------------------------------------------------------------------------

  REAL(KIND=real_jlslsm), POINTER :: wp_fast_in_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: wp_med_in_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: wp_slow_in_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: wp_fast_out_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: wp_med_out_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: wp_slow_out_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: lit_c_orig_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_c_ag_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_orig_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_ag_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_c_fire_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_c_nofire_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: burnt_carbon_dpm(:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_fire_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_nofire_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: burnt_carbon_rpm(:)
  REAL(KIND=real_jlslsm), POINTER :: veg_c_fire_emission_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: veg_c_fire_emission_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_s_to_atmos_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER :: root_abandon_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: root_abandon_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: harvest_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: harvest_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: root_abandon_n_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: root_abandon_n_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: harvest_n_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: harvest_n_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_fertiliser_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_fertiliser_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_leaf_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_root_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_stem_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_leaf_trif_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_root_trif_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_stem_trif_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_leaf_alloc_trif_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_leaf_labile_trif_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_r_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_l_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lai_bal_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: frac_past_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: pc_s_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_ag_pft_diag(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_luc(:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_pft_diag(:,:)
  REAL(KIND=real_jlslsm), POINTER :: fapar_diag_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: apar_diag_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: apar_diag_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: fao_et0(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_carbon_veg2_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_carbon_triffid_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_veg_triffid_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_soil_triffid_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_prod_triffid_gb(:)

  !-----------------------------------------------------------------------------
  ! Variables added for nitrogen conservation checks
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_nitrogen_triffid_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_vegN_triffid_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_soilN_triffid_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cnsrv_N_inorg_triffid_gb(:)

  !-----------------------------------------------------------------------------
  ! Variables added for ticket #7,#127 (nitrogen scheme)
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER :: deposition_n_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: leafC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rootC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: stemC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: woodC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: leafC_gbm(:)
  REAL(KIND=real_jlslsm), POINTER :: rootC_gbm(:)
  REAL(KIND=real_jlslsm), POINTER :: woodC_gbm(:)
  REAL(KIND=real_jlslsm), POINTER :: droot_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dleaf_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dwood_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: root_litC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: leaf_litC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: wood_litC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: root_litN_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: leaf_litN_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: wood_litN_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: litterC_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: litterN_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_uptake_growth_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_demand_growth_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_demand_lit_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_demand_spread_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_uptake_spread_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_uptake_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_demand_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_uptake_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_demand_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_veg_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_veg_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_loss_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_fix_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_fix_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_leach_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_gas_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dpm_ratio_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: dnveg_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dcveg_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: dnveg_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: dcveg_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: immob_n_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: immob_n_pot_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: minl_n_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: minl_n_pot_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_s_diag_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_s_pot_diag_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: fn_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER :: lit_n_t_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: exudates_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: exudates_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: npp_n(:,:)
  REAL(KIND=real_jlslsm), POINTER :: npp_n_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: gpp_pft_out(:,:)
  REAL(KIND=real_jlslsm), POINTER :: gpp_gb_out(:)
  REAL(KIND=real_jlslsm), POINTER :: gpp_pft_acc(:,:)
  REAL(KIND=real_jlslsm), POINTER :: gpp_gb_acc(:)
  REAL(KIND=real_jlslsm), POINTER :: resp_p_actual_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: resp_p_actual_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_leach_gb_acc(:)
  REAL(KIND=real_jlslsm), POINTER :: g_burn_pft_acc(:,:)
  REAL(KIND=real_jlslsm), POINTER :: g_burn_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: g_burn_gb(:)

  !------------------------------------------------------------------------------
  ! Variables to pass fluxes to a soil model.
  !------------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER :: n_fertiliser_add(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_fix_add(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_uptake_extract(:,:,:)

  !-----------------------------------------------------------------------------
  ! Variables added for biocrop functionality
  !-----------------------------------------------------------------------------
  REAL(KIND=real_jlslsm), POINTER :: harvest_biocrop_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: harvest_biocrop_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: harvest_biocrop_n_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: harvest_biocrop_n_gb(:)
  INTEGER, POINTER :: harvest_doy(:,:)
  REAL(KIND=real_jlslsm), POINTER :: frac_biocrop_gb(:)


END TYPE trif_vars_type

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TRIF_VARS_MOD'

CONTAINS

!===============================================================================
SUBROUTINE trif_vars_alloc(land_pts,                                           &
                     npft,dim_cslayer,nsoilt,dim_cs1,                          &
                     l_triffid, l_phenol, trif_vars_data)


!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,                                               &
                       npft,dim_cslayer,nsoilt,dim_cs1

LOGICAL, INTENT(IN) :: l_triffid, l_phenol
TYPE(trif_vars_data_type), INTENT(IN OUT) :: trif_vars_data

!Local variables
INTEGER :: land_pts_dim,npft_dim,dim_cslayer_dim,nsoilt_dim,dim_cs1_dim

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIF_VARS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE(trif_vars_data%resp_l_pft(land_pts,npft))
ALLOCATE(trif_vars_data%resp_r_pft(land_pts,npft))
ALLOCATE(trif_vars_data%n_leaf_pft(land_pts,npft))
ALLOCATE(trif_vars_data%n_root_pft(land_pts,npft))
ALLOCATE(trif_vars_data%n_stem_pft(land_pts,npft))
ALLOCATE(trif_vars_data%lai_bal_pft(land_pts,npft))
ALLOCATE(trif_vars_data%pc_s_pft(land_pts,npft))
ALLOCATE(trif_vars_data%fapar_diag_pft(land_pts,npft))
ALLOCATE(trif_vars_data%apar_diag_pft(land_pts,npft))
ALLOCATE(trif_vars_data%apar_diag_gb(land_pts))
ALLOCATE(trif_vars_data%fao_et0(land_pts))
ALLOCATE(trif_vars_data%frac_past_gb(land_pts))
ALLOCATE(trif_vars_data%frac_biocrop_gb(land_pts))

trif_vars_data%resp_l_pft(:,:)     = 0.0
trif_vars_data%resp_r_pft(:,:)     = 0.0
trif_vars_data%n_leaf_pft(:,:)     = 0.0
trif_vars_data%n_root_pft(:,:)     = 0.0
trif_vars_data%n_stem_pft(:,:)     = 0.0
trif_vars_data%lai_bal_pft(:,:)    = 0.0
trif_vars_data%pc_s_pft(:,:)       = 0.0
trif_vars_data%fapar_diag_pft(:,:) = 0.0
trif_vars_data%apar_diag_pft(:,:)  = 0.0
trif_vars_data%apar_diag_gb(:)     = 0.0
trif_vars_data%fao_et0(:)          = 0.0
trif_vars_data%frac_past_gb(:)     = 0.0
trif_vars_data%frac_biocrop_gb(:)  = 0.0

IF ( l_triffid .OR. l_phenol ) THEN
  land_pts_dim = land_pts
  npft_dim = npft
  dim_cslayer_dim = dim_cslayer
  nsoilt_dim = nsoilt
  dim_cs1_dim = dim_cs1
ELSE
  !Allocate to singletons
  land_pts_dim = 1
  npft_dim = 1
  dim_cslayer_dim = 1
  nsoilt_dim = 1
  dim_cs1_dim = 1
END IF

ALLOCATE(trif_vars_data%wp_fast_in_gb(land_pts_dim))
ALLOCATE(trif_vars_data%wp_med_in_gb(land_pts_dim))
ALLOCATE(trif_vars_data%wp_slow_in_gb(land_pts_dim))
ALLOCATE(trif_vars_data%wp_fast_out_gb(land_pts_dim))
ALLOCATE(trif_vars_data%wp_med_out_gb(land_pts_dim))
ALLOCATE(trif_vars_data%wp_slow_out_gb(land_pts_dim))
ALLOCATE(trif_vars_data%lit_c_orig_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_c_ag_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_c_fire_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_c_nofire_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_n_fire_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_n_nofire_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%veg_c_fire_emission_gb(land_pts_dim))
ALLOCATE(trif_vars_data%veg_c_fire_emission_pft(land_pts_dim,npft_dim))

trif_vars_data%wp_fast_in_gb(:)             = 0.0
trif_vars_data%wp_med_in_gb(:)              = 0.0
trif_vars_data%wp_slow_in_gb(:)             = 0.0
trif_vars_data%wp_fast_out_gb(:)            = 0.0
trif_vars_data%wp_med_out_gb(:)             = 0.0
trif_vars_data%wp_slow_out_gb(:)            = 0.0
trif_vars_data%lit_c_orig_pft(:,:)          = 0.0
trif_vars_data%lit_c_ag_pft(:,:)            = 0.0
trif_vars_data%lit_c_fire_pft(:,:)          = 0.0
trif_vars_data%lit_c_nofire_pft(:,:)        = 0.0
trif_vars_data%lit_n_fire_pft(:,:)          = 0.0
trif_vars_data%lit_n_nofire_pft(:,:)        = 0.0
trif_vars_data%veg_c_fire_emission_gb(:)    = 0.0
trif_vars_data%veg_c_fire_emission_pft(:,:) = 0.0

ALLOCATE(trif_vars_data%cnsrv_carbon_veg2_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_carbon_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_veg_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_soil_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_prod_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%resp_s_to_atmos_gb(land_pts_dim,dim_cslayer_dim))
ALLOCATE(trif_vars_data%root_abandon_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%root_abandon_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_nitrogen_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_vegN_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_soilN_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%cnsrv_N_inorg_triffid_gb(land_pts_dim))
ALLOCATE(trif_vars_data%harvest_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%harvest_gb(land_pts_dim))
ALLOCATE(trif_vars_data%root_abandon_n_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%root_abandon_n_gb(land_pts_dim))
ALLOCATE(trif_vars_data%harvest_n_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%harvest_n_gb(land_pts_dim))
ALLOCATE(trif_vars_data%n_fertiliser_add(land_pts_dim,nsoilt_dim,dim_cslayer_dim))
ALLOCATE(trif_vars_data%n_fertiliser_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_fertiliser_gb(land_pts_dim))
ALLOCATE(trif_vars_data%g_burn_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_leaf_trif_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_leaf_alloc_trif_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_leaf_labile_trif_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_stem_trif_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_root_trif_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_n_ag_pft_diag(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_n_pft_diag(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_luc(land_pts_dim))
ALLOCATE(trif_vars_data%g_burn_pft_acc(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%g_burn_gb(land_pts_dim))
ALLOCATE(trif_vars_data%burnt_carbon_dpm(land_pts_dim))
ALLOCATE(trif_vars_data%burnt_carbon_rpm(land_pts_dim))
ALLOCATE(trif_vars_data%gpp_gb_out(land_pts_dim))
ALLOCATE(trif_vars_data%gpp_pft_out(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%gpp_gb_acc(land_pts_dim))
ALLOCATE(trif_vars_data%gpp_pft_acc(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%resp_p_actual_gb(land_pts_dim))
ALLOCATE(trif_vars_data%resp_p_actual_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_leach_gb_acc(land_pts_dim))

trif_vars_data%cnsrv_carbon_veg2_gb(:)      = 0.0
trif_vars_data%cnsrv_carbon_triffid_gb(:)   = 0.0
trif_vars_data%cnsrv_veg_triffid_gb(:)      = 0.0
trif_vars_data%cnsrv_soil_triffid_gb(:)     = 0.0
trif_vars_data%cnsrv_prod_triffid_gb(:)     = 0.0
trif_vars_data%resp_s_to_atmos_gb(:,:)      = 0.0
trif_vars_data%root_abandon_pft(:,:)        = 0.0
trif_vars_data%root_abandon_gb(:)           = 0.0
trif_vars_data%cnsrv_nitrogen_triffid_gb(:) = 0.0
trif_vars_data%cnsrv_vegN_triffid_gb(:)     = 0.0
trif_vars_data%cnsrv_soilN_triffid_gb(:)    = 0.0
trif_vars_data%cnsrv_N_inorg_triffid_gb(:)  = 0.0
trif_vars_data%harvest_pft(:,:)             = 0.0
trif_vars_data%harvest_gb(:)                = 0.0
trif_vars_data%root_abandon_n_pft(:,:)      = 0.0
trif_vars_data%root_abandon_n_gb(:)         = 0.0
trif_vars_data%harvest_n_pft(:,:)           = 0.0
trif_vars_data%harvest_n_gb(:)              = 0.0
trif_vars_data%n_fertiliser_add(:,:,:)      = 0.0
trif_vars_data%n_fertiliser_pft(:,:)        = 0.0
trif_vars_data%n_fertiliser_gb(:)           = 0.0
trif_vars_data%g_burn_pft(:,:)              = 0.0
trif_vars_data%n_leaf_trif_pft(:,:)         = 0.0
trif_vars_data%n_leaf_alloc_trif_pft(:,:)   = 0.0
trif_vars_data%n_leaf_labile_trif_pft(:,:)  = 0.0
trif_vars_data%n_stem_trif_pft(:,:)         = 0.0
trif_vars_data%n_root_trif_pft(:,:)         = 0.0
trif_vars_data%lit_n_ag_pft_diag(:,:)       = 0.0
trif_vars_data%lit_n_pft_diag(:,:)          = 0.0
trif_vars_data%n_luc(:)                     = 0.0
trif_vars_data%g_burn_pft_acc(:,:)          = 0.0
trif_vars_data%g_burn_gb(:)                 = 0.0
trif_vars_data%burnt_carbon_dpm             = 0.0
trif_vars_data%burnt_carbon_rpm             = 0.0
trif_vars_data%gpp_gb_out(:)                = 0.0
trif_vars_data%gpp_pft_out(:,:)             = 0.0
trif_vars_data%gpp_gb_acc(:)                = 0.0
trif_vars_data%gpp_pft_acc(:,:)             = 0.0
trif_vars_data%resp_p_actual_gb(:)          = 0.0
trif_vars_data%resp_p_actual_pft(:,:)       = 0.0
trif_vars_data%n_leach_gb_acc(:)            = 0.0

! Variables added for #7 (nitrogen scheme)
ALLOCATE(trif_vars_data%resp_s_diag_gb(land_pts_dim,dim_cslayer_dim,dim_cs1_dim+1))
ALLOCATE(trif_vars_data%resp_s_pot_diag_gb(land_pts_dim,dim_cslayer_dim,dim_cs1_dim+1))
ALLOCATE(trif_vars_data%n_veg_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_veg_gb(land_pts_dim))
ALLOCATE(trif_vars_data%dnveg_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%dcveg_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%dnveg_gb(land_pts_dim))
ALLOCATE(trif_vars_data%dcveg_gb(land_pts_dim))
ALLOCATE(trif_vars_data%n_demand_gb(land_pts_dim))
ALLOCATE(trif_vars_data%n_uptake_extract(land_pts_dim,nsoilt_dim,dim_cslayer_dim))
ALLOCATE(trif_vars_data%n_uptake_gb(land_pts_dim))
ALLOCATE(trif_vars_data%deposition_N_gb(land_pts_dim))
ALLOCATE(trif_vars_data%n_uptake_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_demand_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%dleaf_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%droot_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%dwood_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_demand_lit_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_demand_spread_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_demand_growth_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_uptake_growth_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_uptake_spread_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_fix_add(land_pts_dim,nsoilt_dim,dim_cslayer_dim))
ALLOCATE(trif_vars_data%n_fix_gb(land_pts_dim))
ALLOCATE(trif_vars_data%n_fix_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%n_leach_soilt(land_pts_dim,nsoilt_dim))
ALLOCATE(trif_vars_data%n_gas_gb(land_pts_dim,dim_cslayer_dim))
ALLOCATE(trif_vars_data%exudates_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%exudates_gb(land_pts_dim))
ALLOCATE(trif_vars_data%npp_n_gb(land_pts_dim))
ALLOCATE(trif_vars_data%npp_n(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%root_litC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%leaf_litC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%wood_litC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%root_litN_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%leaf_litN_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%wood_litN_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_N_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_n_t_gb(land_pts_dim))
ALLOCATE(trif_vars_data%minl_n_pot_gb(land_pts_dim,dim_cslayer_dim,dim_cs1_dim+1))
ALLOCATE(trif_vars_data%immob_n_gb(land_pts_dim,dim_cslayer_dim,dim_cs1_dim+1))
ALLOCATE(trif_vars_data%immob_n_pot_gb(land_pts_dim,dim_cslayer_dim,dim_cs1_dim+1))
ALLOCATE(trif_vars_data%fn_gb(land_pts_dim,dim_cslayer_dim))
ALLOCATE(trif_vars_data%minl_n_gb(land_pts_dim,dim_cslayer_dim,dim_cs1_dim+1))
ALLOCATE(trif_vars_data%dpm_ratio_gb(land_pts_dim))
ALLOCATE(trif_vars_data%n_loss_gb(land_pts_dim))
ALLOCATE(trif_vars_data%leafC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%rootC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%woodC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%leafC_gbm(land_pts_dim))
ALLOCATE(trif_vars_data%rootC_gbm(land_pts_dim))
ALLOCATE(trif_vars_data%woodC_gbm(land_pts_dim))
ALLOCATE(trif_vars_data%litterC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%stemC_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%litterN_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_n_orig_pft(land_pts_dim,npft_dim))
ALLOCATE(trif_vars_data%lit_n_ag_pft(land_pts_dim,npft_dim))

trif_vars_data%resp_s_diag_gb(:,:,:)        = 0.0
trif_vars_data%resp_s_pot_diag_gb(:,:,:)    = 0.0
trif_vars_data%n_veg_pft(:,:)               = 0.0
trif_vars_data%n_veg_gb(:)                  = 0.0
trif_vars_data%dnveg_pft(:,:)               = 0.0
trif_vars_data%dcveg_pft(:,:)               = 0.0
trif_vars_data%dnveg_gb(:)                  = 0.0
trif_vars_data%dcveg_gb(:)                  = 0.0
trif_vars_data%n_demand_gb(:)               = 0.0
trif_vars_data%n_uptake_extract(:,:,:)      = 0.0
trif_vars_data%n_uptake_gb(:)               = 0.0
trif_vars_data%deposition_n_gb(:)           = 0.0
trif_vars_data%n_uptake_pft(:,:)            = 0.0
trif_vars_data%n_demand_pft(:,:)            = 0.0
trif_vars_data%dleaf_pft(:,:)               = 0.0
trif_vars_data%droot_pft(:,:)               = 0.0
trif_vars_data%dwood_pft(:,:)               = 0.0
trif_vars_data%n_demand_lit_pft(:,:)        = 0.0
trif_vars_data%n_demand_spread_pft(:,:)     = 0.0
trif_vars_data%n_demand_growth_pft(:,:)     = 0.0
trif_vars_data%n_uptake_growth_pft(:,:)     = 0.0
trif_vars_data%n_uptake_spread_pft(:,:)     = 0.0
trif_vars_data%n_fix_add(:,:,:)             = 0.0
trif_vars_data%n_fix_gb(:)                  = 0.0
trif_vars_data%n_fix_pft(:,:)               = 0.0
trif_vars_data%n_leach_soilt(:,:)           = 0.0
trif_vars_data%n_gas_gb(:,:)                = 0.0
trif_vars_data%exudates_pft(:,:)            = 0.0
trif_vars_data%exudates_gb(:)               = 0.0
trif_vars_data%npp_n_gb(:)                  = 0.0
trif_vars_data%npp_n(:,:)                   = 0.0
trif_vars_data%root_litC_pft(:,:)           = 0.0
trif_vars_data%leaf_litC_pft(:,:)           = 0.0
trif_vars_data%wood_litC_pft(:,:)           = 0.0
trif_vars_data%root_litN_pft(:,:)           = 0.0
trif_vars_data%leaf_litN_pft(:,:)           = 0.0
trif_vars_data%wood_litN_pft(:,:)           = 0.0
trif_vars_data%lit_N_pft(:,:)               = 0.0
trif_vars_data%lit_n_t_gb(:)                = 0.0
trif_vars_data%minl_n_pot_gb(:,:,:)         = 0.0
trif_vars_data%immob_n_gb(:,:,:)            = 0.0
trif_vars_data%immob_n_pot_gb(:,:,:)        = 0.0
trif_vars_data%fn_gb(:,:)                   = 0.0
trif_vars_data%minl_n_gb(:,:,:)             = 0.0
trif_vars_data%dpm_ratio_gb(:)              = 0.0
trif_vars_data%n_loss_gb(:)                 = 0.0
trif_vars_data%leafC_pft(:,:)               = 0.0
trif_vars_data%rootC_pft(:,:)               = 0.0
trif_vars_data%woodC_pft(:,:)               = 0.0
trif_vars_data%leafC_gbm(:)                 = 0.0
trif_vars_data%rootC_gbm(:)                 = 0.0
trif_vars_data%woodC_gbm(:)                 = 0.0
trif_vars_data%litterC_pft(:,:)             = 0.0
trif_vars_data%stemC_pft(:,:)               = 0.0
trif_vars_data%litterN_pft(:,:)             = 0.0
trif_vars_data%lit_n_orig_pft(:,:)          = 0.0
trif_vars_data%lit_n_ag_pft(:,:)            = 0.0

! Variables for biocrop functionality
ALLOCATE( trif_vars_data%harvest_biocrop_pft(land_pts,npft))
ALLOCATE( trif_vars_data%harvest_biocrop_gb(land_pts))
ALLOCATE( trif_vars_data%harvest_biocrop_n_pft(land_pts,npft))
ALLOCATE( trif_vars_data%harvest_biocrop_n_gb(land_pts))
ALLOCATE( trif_vars_data%harvest_doy(land_pts,npft))

trif_vars_data%harvest_biocrop_pft(:,:)     = 0.0
trif_vars_data%harvest_biocrop_gb(:)        = 0.0
trif_vars_data%harvest_biocrop_n_pft(:,:)   = 0.0
trif_vars_data%harvest_biocrop_n_gb(:)      = 0.0
trif_vars_data%harvest_doy(:,:)             = 0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trif_vars_alloc


!===============================================================================
SUBROUTINE trif_vars_dealloc(trif_vars_data)


!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(trif_vars_data_type), INTENT(IN OUT) :: trif_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIF_VARS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(trif_vars_data%resp_l_pft)
DEALLOCATE(trif_vars_data%resp_r_pft)
DEALLOCATE(trif_vars_data%n_leaf_pft)
DEALLOCATE(trif_vars_data%n_root_pft)
DEALLOCATE(trif_vars_data%n_stem_pft)
DEALLOCATE(trif_vars_data%lai_bal_pft)
DEALLOCATE(trif_vars_data%pc_s_pft)
DEALLOCATE(trif_vars_data%fapar_diag_pft)
DEALLOCATE(trif_vars_data%apar_diag_pft)
DEALLOCATE(trif_vars_data%apar_diag_gb)
DEALLOCATE(trif_vars_data%fao_et0)
DEALLOCATE(trif_vars_data%frac_past_gb)
DEALLOCATE(trif_vars_data%frac_biocrop_gb)

DEALLOCATE(trif_vars_data%wp_fast_in_gb)
DEALLOCATE(trif_vars_data%wp_med_in_gb)
DEALLOCATE(trif_vars_data%wp_slow_in_gb)
DEALLOCATE(trif_vars_data%wp_fast_out_gb)
DEALLOCATE(trif_vars_data%wp_med_out_gb)
DEALLOCATE(trif_vars_data%wp_slow_out_gb)
DEALLOCATE(trif_vars_data%lit_c_orig_pft)
DEALLOCATE(trif_vars_data%lit_c_ag_pft)
DEALLOCATE(trif_vars_data%lit_c_fire_pft)
DEALLOCATE(trif_vars_data%lit_c_nofire_pft)
DEALLOCATE(trif_vars_data%lit_n_fire_pft)
DEALLOCATE(trif_vars_data%lit_n_nofire_pft)
DEALLOCATE(trif_vars_data%veg_c_fire_emission_gb)
DEALLOCATE(trif_vars_data%veg_c_fire_emission_pft)

DEALLOCATE(trif_vars_data%cnsrv_carbon_veg2_gb)
DEALLOCATE(trif_vars_data%cnsrv_carbon_triffid_gb)
DEALLOCATE(trif_vars_data%cnsrv_veg_triffid_gb)
DEALLOCATE(trif_vars_data%cnsrv_soil_triffid_gb)
DEALLOCATE(trif_vars_data%cnsrv_prod_triffid_gb)
DEALLOCATE(trif_vars_data%resp_s_to_atmos_gb)
DEALLOCATE(trif_vars_data%root_abandon_pft)
DEALLOCATE(trif_vars_data%root_abandon_gb)
DEALLOCATE(trif_vars_data%cnsrv_nitrogen_triffid_gb)
DEALLOCATE(trif_vars_data%cnsrv_vegN_triffid_gb)
DEALLOCATE(trif_vars_data%cnsrv_soilN_triffid_gb)
DEALLOCATE(trif_vars_data%cnsrv_N_inorg_triffid_gb)
DEALLOCATE(trif_vars_data%harvest_pft)
DEALLOCATE(trif_vars_data%harvest_gb)
DEALLOCATE(trif_vars_data%root_abandon_n_pft)
DEALLOCATE(trif_vars_data%root_abandon_n_gb)
DEALLOCATE(trif_vars_data%harvest_n_pft)
DEALLOCATE(trif_vars_data%harvest_n_gb)
DEALLOCATE(trif_vars_data%n_fertiliser_add)
DEALLOCATE(trif_vars_data%n_fertiliser_pft)
DEALLOCATE(trif_vars_data%n_fertiliser_gb)
DEALLOCATE(trif_vars_data%g_burn_pft)
DEALLOCATE(trif_vars_data%n_leaf_trif_pft)
DEALLOCATE(trif_vars_data%n_leaf_alloc_trif_pft)
DEALLOCATE(trif_vars_data%n_leaf_labile_trif_pft)
DEALLOCATE(trif_vars_data%n_stem_trif_pft)
DEALLOCATE(trif_vars_data%n_root_trif_pft)
DEALLOCATE(trif_vars_data%lit_n_ag_pft_diag)
DEALLOCATE(trif_vars_data%lit_n_pft_diag)
DEALLOCATE(trif_vars_data%n_luc)
DEALLOCATE(trif_vars_data%g_burn_pft_acc)
DEALLOCATE(trif_vars_data%g_burn_gb)
DEALLOCATE(trif_vars_data%burnt_carbon_dpm)
DEALLOCATE(trif_vars_data%burnt_carbon_rpm)
DEALLOCATE(trif_vars_data%gpp_gb_out)
DEALLOCATE(trif_vars_data%gpp_pft_out)
DEALLOCATE(trif_vars_data%gpp_gb_acc)
DEALLOCATE(trif_vars_data%gpp_pft_acc)
DEALLOCATE(trif_vars_data%resp_p_actual_gb)
DEALLOCATE(trif_vars_data%resp_p_actual_pft)
DEALLOCATE(trif_vars_data%n_leach_gb_acc)

! Variables added for #7 (nitrogen scheme)
DEALLOCATE(trif_vars_data%resp_s_diag_gb)
DEALLOCATE(trif_vars_data%resp_s_pot_diag_gb)
DEALLOCATE(trif_vars_data%n_veg_pft)
DEALLOCATE(trif_vars_data%n_veg_gb)
DEALLOCATE(trif_vars_data%dnveg_pft)
DEALLOCATE(trif_vars_data%dcveg_pft)
DEALLOCATE(trif_vars_data%dnveg_gb)
DEALLOCATE(trif_vars_data%dcveg_gb)
DEALLOCATE(trif_vars_data%n_demand_gb)
DEALLOCATE(trif_vars_data%n_uptake_extract)
DEALLOCATE(trif_vars_data%n_uptake_gb)
DEALLOCATE(trif_vars_data%deposition_N_gb)
DEALLOCATE(trif_vars_data%n_uptake_pft)
DEALLOCATE(trif_vars_data%n_demand_pft)
DEALLOCATE(trif_vars_data%dleaf_pft)
DEALLOCATE(trif_vars_data%droot_pft)
DEALLOCATE(trif_vars_data%dwood_pft)
DEALLOCATE(trif_vars_data%n_demand_lit_pft)
DEALLOCATE(trif_vars_data%n_demand_spread_pft)
DEALLOCATE(trif_vars_data%n_demand_growth_pft)
DEALLOCATE(trif_vars_data%n_uptake_growth_pft)
DEALLOCATE(trif_vars_data%n_uptake_spread_pft)
DEALLOCATE(trif_vars_data%n_fix_add)
DEALLOCATE(trif_vars_data%n_fix_gb)
DEALLOCATE(trif_vars_data%n_fix_pft)
DEALLOCATE(trif_vars_data%n_leach_soilt)
DEALLOCATE(trif_vars_data%n_gas_gb)
DEALLOCATE(trif_vars_data%exudates_pft)
DEALLOCATE(trif_vars_data%exudates_gb)
DEALLOCATE(trif_vars_data%npp_n_gb)
DEALLOCATE(trif_vars_data%npp_n)
DEALLOCATE(trif_vars_data%root_litC_pft)
DEALLOCATE(trif_vars_data%leaf_litC_pft)
DEALLOCATE(trif_vars_data%wood_litC_pft)
DEALLOCATE(trif_vars_data%root_litN_pft)
DEALLOCATE(trif_vars_data%leaf_litN_pft)
DEALLOCATE(trif_vars_data%wood_litN_pft)
DEALLOCATE(trif_vars_data%lit_N_pft)
DEALLOCATE(trif_vars_data%lit_n_t_gb)
DEALLOCATE(trif_vars_data%minl_n_pot_gb)
DEALLOCATE(trif_vars_data%immob_n_gb)
DEALLOCATE(trif_vars_data%immob_n_pot_gb)
DEALLOCATE(trif_vars_data%fn_gb)
DEALLOCATE(trif_vars_data%minl_n_gb)
DEALLOCATE(trif_vars_data%dpm_ratio_gb)
DEALLOCATE(trif_vars_data%n_loss_gb)
DEALLOCATE(trif_vars_data%leafC_pft)
DEALLOCATE(trif_vars_data%rootC_pft)
DEALLOCATE(trif_vars_data%woodC_pft)
DEALLOCATE(trif_vars_data%leafC_gbm)
DEALLOCATE(trif_vars_data%rootC_gbm)
DEALLOCATE(trif_vars_data%woodC_gbm)
DEALLOCATE(trif_vars_data%litterC_pft)
DEALLOCATE(trif_vars_data%stemC_pft)
DEALLOCATE(trif_vars_data%litterN_pft)
DEALLOCATE(trif_vars_data%lit_n_orig_pft)
DEALLOCATE(trif_vars_data%lit_n_ag_pft)

! Variables for biocrop functionality
DEALLOCATE(trif_vars_data%harvest_biocrop_pft)
DEALLOCATE(trif_vars_data%harvest_biocrop_gb)
DEALLOCATE(trif_vars_data%harvest_biocrop_n_pft)
DEALLOCATE(trif_vars_data%harvest_biocrop_n_gb)
DEALLOCATE(trif_vars_data%harvest_doy)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trif_vars_dealloc

!===============================================================================
SUBROUTINE trif_vars_assoc(trif_vars, trif_vars_data)


!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(trif_vars_data_type), INTENT(IN OUT), TARGET :: trif_vars_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIF_VARS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL trif_vars_nullify(trif_vars)

trif_vars%resp_l_pft => trif_vars_data%resp_l_pft
trif_vars%resp_r_pft => trif_vars_data%resp_r_pft
trif_vars%n_leaf_pft => trif_vars_data%n_leaf_pft
trif_vars%n_root_pft => trif_vars_data%n_root_pft
trif_vars%n_stem_pft => trif_vars_data%n_stem_pft
trif_vars%lai_bal_pft => trif_vars_data%lai_bal_pft
trif_vars%pc_s_pft => trif_vars_data%pc_s_pft
trif_vars%fapar_diag_pft => trif_vars_data%fapar_diag_pft
trif_vars%apar_diag_pft => trif_vars_data%apar_diag_pft
trif_vars%apar_diag_gb => trif_vars_data%apar_diag_gb
trif_vars%fao_et0 => trif_vars_data%fao_et0
trif_vars%frac_past_gb => trif_vars_data%frac_past_gb
trif_vars%frac_biocrop_gb => trif_vars_data%frac_biocrop_gb

trif_vars%wp_fast_in_gb => trif_vars_data%wp_fast_in_gb
trif_vars%wp_med_in_gb => trif_vars_data%wp_med_in_gb
trif_vars%wp_slow_in_gb => trif_vars_data%wp_slow_in_gb
trif_vars%wp_fast_out_gb => trif_vars_data%wp_fast_out_gb
trif_vars%wp_med_out_gb => trif_vars_data%wp_med_out_gb
trif_vars%wp_slow_out_gb => trif_vars_data%wp_slow_out_gb
trif_vars%lit_c_orig_pft => trif_vars_data%lit_c_orig_pft
trif_vars%lit_c_ag_pft => trif_vars_data%lit_c_ag_pft
trif_vars%lit_c_fire_pft => trif_vars_data%lit_c_fire_pft
trif_vars%lit_c_nofire_pft => trif_vars_data%lit_c_nofire_pft
trif_vars%lit_n_fire_pft => trif_vars_data%lit_n_fire_pft
trif_vars%lit_n_nofire_pft => trif_vars_data%lit_n_nofire_pft
trif_vars%veg_c_fire_emission_gb => trif_vars_data%veg_c_fire_emission_gb
trif_vars%veg_c_fire_emission_pft => trif_vars_data%veg_c_fire_emission_pft

trif_vars%cnsrv_carbon_veg2_gb => trif_vars_data%cnsrv_carbon_veg2_gb
trif_vars%cnsrv_carbon_triffid_gb => trif_vars_data%cnsrv_carbon_triffid_gb
trif_vars%cnsrv_veg_triffid_gb => trif_vars_data%cnsrv_veg_triffid_gb
trif_vars%cnsrv_soil_triffid_gb => trif_vars_data%cnsrv_soil_triffid_gb
trif_vars%cnsrv_prod_triffid_gb => trif_vars_data%cnsrv_prod_triffid_gb
trif_vars%resp_s_to_atmos_gb => trif_vars_data%resp_s_to_atmos_gb
trif_vars%root_abandon_pft => trif_vars_data%root_abandon_pft
trif_vars%root_abandon_gb => trif_vars_data%root_abandon_gb
trif_vars%cnsrv_nitrogen_triffid_gb => trif_vars_data%cnsrv_nitrogen_triffid_gb
trif_vars%cnsrv_vegN_triffid_gb => trif_vars_data%cnsrv_vegN_triffid_gb
trif_vars%cnsrv_soilN_triffid_gb => trif_vars_data%cnsrv_soilN_triffid_gb
trif_vars%cnsrv_N_inorg_triffid_gb => trif_vars_data%cnsrv_N_inorg_triffid_gb
trif_vars%harvest_pft => trif_vars_data%harvest_pft
trif_vars%harvest_gb => trif_vars_data%harvest_gb
trif_vars%root_abandon_n_pft => trif_vars_data%root_abandon_n_pft
trif_vars%root_abandon_n_gb => trif_vars_data%root_abandon_n_gb
trif_vars%harvest_n_pft => trif_vars_data%harvest_n_pft
trif_vars%harvest_n_gb => trif_vars_data%harvest_n_gb
trif_vars%n_fertiliser_add => trif_vars_data%n_fertiliser_add
trif_vars%n_fertiliser_pft => trif_vars_data%n_fertiliser_pft
trif_vars%n_fertiliser_gb => trif_vars_data%n_fertiliser_gb
trif_vars%g_burn_pft => trif_vars_data%g_burn_pft
trif_vars%n_leaf_trif_pft => trif_vars_data%n_leaf_trif_pft
trif_vars%n_leaf_alloc_trif_pft => trif_vars_data%n_leaf_alloc_trif_pft
trif_vars%n_leaf_labile_trif_pft => trif_vars_data%n_leaf_labile_trif_pft
trif_vars%n_stem_trif_pft => trif_vars_data%n_stem_trif_pft
trif_vars%n_root_trif_pft => trif_vars_data%n_root_trif_pft
trif_vars%lit_n_ag_pft_diag => trif_vars_data%lit_n_ag_pft_diag
trif_vars%lit_n_pft_diag => trif_vars_data%lit_n_pft_diag
trif_vars%n_luc => trif_vars_data%n_luc
trif_vars%g_burn_pft_acc => trif_vars_data%g_burn_pft_acc
trif_vars%g_burn_gb => trif_vars_data%g_burn_gb
trif_vars%burnt_carbon_dpm => trif_vars_data%burnt_carbon_dpm
trif_vars%burnt_carbon_rpm => trif_vars_data%burnt_carbon_rpm
trif_vars%gpp_gb_out => trif_vars_data%gpp_gb_out
trif_vars%gpp_pft_out => trif_vars_data%gpp_pft_out
trif_vars%gpp_gb_acc => trif_vars_data%gpp_gb_acc
trif_vars%gpp_pft_acc => trif_vars_data%gpp_pft_acc
trif_vars%resp_p_actual_gb => trif_vars_data%resp_p_actual_gb
trif_vars%resp_p_actual_pft => trif_vars_data%resp_p_actual_pft
trif_vars%n_leach_gb_acc => trif_vars_data%n_leach_gb_acc

! Variables added for #7 (nitrogen scheme)
trif_vars%resp_s_diag_gb => trif_vars_data%resp_s_diag_gb
trif_vars%resp_s_pot_diag_gb => trif_vars_data%resp_s_pot_diag_gb
trif_vars%n_veg_pft => trif_vars_data%n_veg_pft
trif_vars%n_veg_gb => trif_vars_data%n_veg_gb
trif_vars%dnveg_pft => trif_vars_data%dnveg_pft
trif_vars%dcveg_pft => trif_vars_data%dcveg_pft
trif_vars%dnveg_gb => trif_vars_data%dnveg_gb
trif_vars%dcveg_gb => trif_vars_data%dcveg_gb
trif_vars%n_demand_gb => trif_vars_data%n_demand_gb
trif_vars%n_uptake_extract => trif_vars_data%n_uptake_extract
trif_vars%n_uptake_gb => trif_vars_data%n_uptake_gb
trif_vars%deposition_N_gb => trif_vars_data%deposition_N_gb
trif_vars%n_uptake_pft => trif_vars_data%n_uptake_pft
trif_vars%n_demand_pft => trif_vars_data%n_demand_pft
trif_vars%dleaf_pft => trif_vars_data%dleaf_pft
trif_vars%droot_pft => trif_vars_data%droot_pft
trif_vars%dwood_pft => trif_vars_data%dwood_pft
trif_vars%n_demand_lit_pft => trif_vars_data%n_demand_lit_pft
trif_vars%n_demand_spread_pft => trif_vars_data%n_demand_spread_pft
trif_vars%n_demand_growth_pft => trif_vars_data%n_demand_growth_pft
trif_vars%n_uptake_growth_pft => trif_vars_data%n_uptake_growth_pft
trif_vars%n_uptake_spread_pft => trif_vars_data%n_uptake_spread_pft
trif_vars%n_fix_add => trif_vars_data%n_fix_add
trif_vars%n_fix_gb => trif_vars_data%n_fix_gb
trif_vars%n_fix_pft => trif_vars_data%n_fix_pft
trif_vars%n_leach_soilt => trif_vars_data%n_leach_soilt
trif_vars%n_gas_gb => trif_vars_data%n_gas_gb
trif_vars%exudates_pft => trif_vars_data%exudates_pft
trif_vars%exudates_gb => trif_vars_data%exudates_gb
trif_vars%npp_n_gb => trif_vars_data%npp_n_gb
trif_vars%npp_n => trif_vars_data%npp_n
trif_vars%root_litC_pft => trif_vars_data%root_litC_pft
trif_vars%leaf_litC_pft => trif_vars_data%leaf_litC_pft
trif_vars%wood_litC_pft => trif_vars_data%wood_litC_pft
trif_vars%root_litN_pft => trif_vars_data%root_litN_pft
trif_vars%leaf_litN_pft => trif_vars_data%leaf_litN_pft
trif_vars%wood_litN_pft => trif_vars_data%wood_litN_pft
trif_vars%lit_N_pft => trif_vars_data%lit_N_pft
trif_vars%lit_n_t_gb => trif_vars_data%lit_n_t_gb
trif_vars%minl_n_pot_gb => trif_vars_data%minl_n_pot_gb
trif_vars%immob_n_gb => trif_vars_data%immob_n_gb
trif_vars%immob_n_pot_gb => trif_vars_data%immob_n_pot_gb
trif_vars%fn_gb => trif_vars_data%fn_gb
trif_vars%minl_n_gb => trif_vars_data%minl_n_gb
trif_vars%dpm_ratio_gb => trif_vars_data%dpm_ratio_gb
trif_vars%n_loss_gb => trif_vars_data%n_loss_gb
trif_vars%leafC_pft => trif_vars_data%leafC_pft
trif_vars%rootC_pft => trif_vars_data%rootC_pft
trif_vars%woodC_pft => trif_vars_data%woodC_pft
trif_vars%leafC_gbm => trif_vars_data%leafC_gbm
trif_vars%rootC_gbm => trif_vars_data%rootC_gbm
trif_vars%woodC_gbm => trif_vars_data%woodC_gbm
trif_vars%litterC_pft => trif_vars_data%litterC_pft
trif_vars%stemC_pft => trif_vars_data%stemC_pft
trif_vars%litterN_pft => trif_vars_data%litterN_pft
trif_vars%lit_n_orig_pft => trif_vars_data%lit_n_orig_pft
trif_vars%lit_n_ag_pft => trif_vars_data%lit_n_ag_pft

!Variables for biocrop functionality
trif_vars%harvest_biocrop_pft => trif_vars_data%harvest_biocrop_pft
trif_vars%harvest_biocrop_gb => trif_vars_data%harvest_biocrop_gb
trif_vars%harvest_biocrop_n_pft => trif_vars_data%harvest_biocrop_n_pft
trif_vars%harvest_biocrop_n_gb => trif_vars_data%harvest_biocrop_n_gb
trif_vars%harvest_doy => trif_vars_data%harvest_doy

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trif_vars_assoc


!===============================================================================
SUBROUTINE trif_vars_nullify(trif_vars)


!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='TRIF_VARS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(trif_vars%resp_l_pft)
NULLIFY(trif_vars%resp_r_pft)
NULLIFY(trif_vars%n_leaf_pft)
NULLIFY(trif_vars%n_root_pft)
NULLIFY(trif_vars%n_stem_pft)
NULLIFY(trif_vars%lai_bal_pft)
NULLIFY(trif_vars%pc_s_pft)
NULLIFY(trif_vars%fapar_diag_pft)
NULLIFY(trif_vars%apar_diag_pft)
NULLIFY(trif_vars%apar_diag_gb)
NULLIFY(trif_vars%fao_et0)
NULLIFY(trif_vars%frac_past_gb)
NULLIFY(trif_vars%frac_biocrop_gb)

NULLIFY(trif_vars%wp_fast_in_gb)
NULLIFY(trif_vars%wp_med_in_gb)
NULLIFY(trif_vars%wp_slow_in_gb)
NULLIFY(trif_vars%wp_fast_out_gb)
NULLIFY(trif_vars%wp_med_out_gb)
NULLIFY(trif_vars%wp_slow_out_gb)
NULLIFY(trif_vars%lit_c_orig_pft)
NULLIFY(trif_vars%lit_c_ag_pft)
NULLIFY(trif_vars%lit_c_fire_pft)
NULLIFY(trif_vars%lit_c_nofire_pft)
NULLIFY(trif_vars%lit_n_fire_pft)
NULLIFY(trif_vars%lit_n_nofire_pft)
NULLIFY(trif_vars%veg_c_fire_emission_gb)
NULLIFY(trif_vars%veg_c_fire_emission_pft)

NULLIFY(trif_vars%cnsrv_carbon_veg2_gb)
NULLIFY(trif_vars%cnsrv_carbon_triffid_gb)
NULLIFY(trif_vars%cnsrv_veg_triffid_gb)
NULLIFY(trif_vars%cnsrv_soil_triffid_gb)
NULLIFY(trif_vars%cnsrv_prod_triffid_gb)
NULLIFY(trif_vars%resp_s_to_atmos_gb)
NULLIFY(trif_vars%root_abandon_pft)
NULLIFY(trif_vars%root_abandon_gb)
NULLIFY(trif_vars%cnsrv_nitrogen_triffid_gb)
NULLIFY(trif_vars%cnsrv_vegN_triffid_gb)
NULLIFY(trif_vars%cnsrv_soilN_triffid_gb)
NULLIFY(trif_vars%cnsrv_N_inorg_triffid_gb)
NULLIFY(trif_vars%harvest_pft)
NULLIFY(trif_vars%harvest_gb)
NULLIFY(trif_vars%root_abandon_n_pft)
NULLIFY(trif_vars%root_abandon_n_gb)
NULLIFY(trif_vars%harvest_n_pft)
NULLIFY(trif_vars%harvest_n_gb)
NULLIFY(trif_vars%n_fertiliser_add)
NULLIFY(trif_vars%n_fertiliser_pft)
NULLIFY(trif_vars%n_fertiliser_gb)
NULLIFY(trif_vars%g_burn_pft)
NULLIFY(trif_vars%n_leaf_trif_pft)
NULLIFY(trif_vars%n_leaf_alloc_trif_pft)
NULLIFY(trif_vars%n_leaf_labile_trif_pft)
NULLIFY(trif_vars%n_stem_trif_pft)
NULLIFY(trif_vars%n_root_trif_pft)
NULLIFY(trif_vars%lit_n_ag_pft_diag)
NULLIFY(trif_vars%lit_n_pft_diag)
NULLIFY(trif_vars%n_luc)
NULLIFY(trif_vars%g_burn_pft_acc)
NULLIFY(trif_vars%g_burn_gb)
NULLIFY(trif_vars%burnt_carbon_dpm)
NULLIFY(trif_vars%burnt_carbon_rpm)
NULLIFY(trif_vars%gpp_gb_out)
NULLIFY(trif_vars%gpp_pft_out)
NULLIFY(trif_vars%gpp_gb_acc)
NULLIFY(trif_vars%gpp_pft_acc)
NULLIFY(trif_vars%resp_p_actual_gb)
NULLIFY(trif_vars%resp_p_actual_pft)
NULLIFY(trif_vars%n_leach_gb_acc)

! Variables added for #7 (nitrogen scheme)
NULLIFY(trif_vars%resp_s_diag_gb)
NULLIFY(trif_vars%resp_s_pot_diag_gb)
NULLIFY(trif_vars%n_veg_pft)
NULLIFY(trif_vars%n_veg_gb)
NULLIFY(trif_vars%dnveg_pft)
NULLIFY(trif_vars%dcveg_pft)
NULLIFY(trif_vars%dnveg_gb)
NULLIFY(trif_vars%dcveg_gb)
NULLIFY(trif_vars%n_demand_gb)
NULLIFY(trif_vars%n_uptake_extract)
NULLIFY(trif_vars%n_uptake_gb)
NULLIFY(trif_vars%deposition_N_gb)
NULLIFY(trif_vars%n_uptake_pft)
NULLIFY(trif_vars%n_demand_pft)
NULLIFY(trif_vars%dleaf_pft)
NULLIFY(trif_vars%droot_pft)
NULLIFY(trif_vars%dwood_pft)
NULLIFY(trif_vars%n_demand_lit_pft)
NULLIFY(trif_vars%n_demand_spread_pft)
NULLIFY(trif_vars%n_demand_growth_pft)
NULLIFY(trif_vars%n_uptake_growth_pft)
NULLIFY(trif_vars%n_uptake_spread_pft)
NULLIFY(trif_vars%n_fix_add)
NULLIFY(trif_vars%n_fix_gb)
NULLIFY(trif_vars%n_fix_pft)
NULLIFY(trif_vars%n_leach_soilt)
NULLIFY(trif_vars%n_gas_gb)
NULLIFY(trif_vars%exudates_pft)
NULLIFY(trif_vars%exudates_gb)
NULLIFY(trif_vars%npp_n_gb)
NULLIFY(trif_vars%npp_n)
NULLIFY(trif_vars%root_litC_pft)
NULLIFY(trif_vars%leaf_litC_pft)
NULLIFY(trif_vars%wood_litC_pft)
NULLIFY(trif_vars%root_litN_pft)
NULLIFY(trif_vars%leaf_litN_pft)
NULLIFY(trif_vars%wood_litN_pft)
NULLIFY(trif_vars%lit_N_pft)
NULLIFY(trif_vars%lit_n_t_gb)
NULLIFY(trif_vars%minl_n_pot_gb)
NULLIFY(trif_vars%immob_n_gb)
NULLIFY(trif_vars%immob_n_pot_gb)
NULLIFY(trif_vars%fn_gb)
NULLIFY(trif_vars%minl_n_gb)
NULLIFY(trif_vars%dpm_ratio_gb)
NULLIFY(trif_vars%n_loss_gb)
NULLIFY(trif_vars%leafC_pft)
NULLIFY(trif_vars%rootC_pft)
NULLIFY(trif_vars%woodC_pft)
NULLIFY(trif_vars%leafC_gbm)
NULLIFY(trif_vars%rootC_gbm)
NULLIFY(trif_vars%woodC_gbm)
NULLIFY(trif_vars%litterC_pft)
NULLIFY(trif_vars%stemC_pft)
NULLIFY(trif_vars%litterN_pft)
NULLIFY(trif_vars%lit_n_orig_pft)
NULLIFY(trif_vars%lit_n_ag_pft)

! Variables added for biocrop functionality

NULLIFY(trif_vars%harvest_biocrop_pft)
NULLIFY(trif_vars%harvest_biocrop_gb)
NULLIFY(trif_vars%harvest_biocrop_n_pft)
NULLIFY(trif_vars%harvest_biocrop_n_gb)
NULLIFY(trif_vars%harvest_doy)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE trif_vars_nullify

END MODULE trif_vars_mod
