! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Description:
! Subroutine to calculate gridbox mean values of surface conductance
! and carbon fluxes. Also returns net primary productivity, leaf
! turnover and wood respiration of each plant functional type for
! driving TRIFFID.

! *********************************************************************
MODULE physiol_mod
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PHYSIOL_MOD'
CONTAINS
SUBROUTINE physiol (                                                           &
  land_pts,land_index,                                                         &
  sm_levels,nsurft,n_wtrac_jls,surft_pts,surft_index,                          &
  dim_cs1,                                                                     &
  co2_mmr,co2_3d,co2_dim_len, co2_dim_row,l_co2_interactive,                   &
  can_model,cs_pool_soilt,veg_state,frac,non_irrig_frac,                       &
  canht_pft,photosynth_act_rad,                                                &
  lai_pft,pstar,qw_1,sthu_soilt,sthf_soilt,t_soil_soilt,tstar_surft,           &
  smvccl_soilt,smvcst_soilt,smvcwt_soilt,vshr,z0_surft,z1_uv_ij,o3,            &
  canhc_surft,vfrac_surft,emis_surft,l_emis_surft_set,emis_soil,flake,         &
  g_leaf,gs,gc_surft,gc_stom_surft,gc_corr,gpp,gpp_pft,npp,npp_pft,            &
  resp_p,resp_p_pft,resp_s_soilt,resp_l_pft,                                   &
  resp_r_pft,resp_w_pft,n_leaf,                                                &
  n_root,n_stem,lai_bal,                                                       &
  smc_soilt,wt_ext_surft,fsmc_pft,                                             &
  albsoil_soilt,cos_zenith_angle,                                              &
  can_rad_mod,ilayers,flux_o3_pft,fo3_pft,sf_diag,asteps_since_triffid,        &
  non_lake_frac,                                                               &
  !SUGAR variables
  f_nsc_pft, growth_sug_gb, growth_sug_pft,                                    &
  ! SOX variables
  psi_root_zone_pft, lwp_c_pft,                                                &
  !New arguments replacing USE statements
  !Fluxes (IN)
  t_home_gb,t_growth_gb,                                                       &
  !urban_param (IN)
  emisr_gb, emisw_gb, hwr_gb,                                                  &
  !jules_mod (IN OUT)
  albobs_scaling_surft,                                                        &
  !bvoc_vars (OUT)
  isoprene_gb, isoprene_pft, terpene_gb , terpene_pft,                         &
  methanol_gb, methanol_pft, acetone_gb, acetone_pft,                          &
  !trif_vars_mod (OUT)
  fapar_diag_pft, apar_diag_pft, apar_diag_gb, gpp_gb_acc, gpp_pft_acc,        &
  !crop_vars_mod (IN)
  rootc_cpft, sthu_irr_soilt, frac_irr_soilt, frac_irr_surft, dvi_cpft,        &
  !crop_vars_mod (OUT)
  gs_irr_surft, smc_irr_soilt, wt_ext_irr_surft, gc_irr_surft,                 &
  gs_nir_surft,                                                                &
  !p_s_parms (IN)
  bexp_soilt, sathh_soilt, v_close_pft, v_open_pft,                            &
  !ancil_info (IN)
  l_soil_point,                                                                &
  !jules_surface_types (IN)
  diff_frac,                                                                   &
  ! water tracers (IN)
  sthu_soilt_wtrac,                                                            &
  ! water tracers (OUT)
  smc_soilt_wtrac)


!Use in relevant subroutines
USE sf_stom_mod,    ONLY: sf_stom
USE albpft_mod,     ONLY: albpft
USE smc_ext_mod,    ONLY: smc_ext
USE urbanemis_mod,  ONLY: urbanemis
USE microbe_mod,    ONLY: microbe
USE root_frac_mod, ONLY: root_frac
USE raero_mod, ONLY: raero
USE soil_evap_mod, ONLY: soil_evap
USE leaf_lit_mod, ONLY: leaf_lit
USE cancap_mod, ONLY: cancap

!Use in relevant variables
USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE water_constants_mod, ONLY: rho_water

USE ancil_info, ONLY: dim_cslayer, nsoilt, rad_nband

USE c_irrigation_mod, ONLY: irrig_tile

USE jules_soil_biogeochem_mod, ONLY:                                           &
! imported scalar parameters
  soil_model_1pool, soil_model_4pool,                                          &
! imported scalar variables
  l_q10, soil_bgc_model

USE jules_surface_types_mod, ONLY:                                             &
   lake, npft, nnpft, ntype, soil, urban_canyon, urban_roof, ncpft

USE nvegparm, ONLY: ch_nvg, emis_nvg, gs_nvg, vf_nvg

USE jules_soil_mod, ONLY: dzsoil
USE jules_surface_mod, ONLY: l_aggregate, l_flake_model

USE jules_vegetation_mod, ONLY:                                                &
  ! imported variables
  l_crop, l_use_pft_psi, l_triffid

USE jules_irrig_mod, ONLY: l_irrig_dmd, l_soil_evap_irrig_expl

USE jules_hydrology_mod, ONLY: l_limit_gsoil

USE pftparm, ONLY: emis_pft, fsmc_p0, rootd_ft, gsoil_f

USE jules_radiation_mod, ONLY: l_spec_albedo, l_albedo_obs,                    &
                               l_spec_alb_bs

USE jules_urban_mod, ONLY:                                                     &
  l_moruses_emissivity, l_moruses_storage, l_moruses_storage_thin

USE urban_param_mod, ONLY: emiss,                                              &
  diffus_road, diffus_wall, diffus_roof, cap_road, cap_wall,                   &
  cap_roof, dz_roof_p, omega_day

USE timestep_mod, ONLY: timestep

USE cropparm, ONLY: rt_dir, cfrac_r

USE jules_print_mgr, ONLY: jules_message, jules_print, PrNorm

USE ereport_mod, ONLY: ereport

USE stochastic_physics_run_mod, ONLY: l_rp2, i_rp_scheme, i_rp2b, lai_mult_rp

USE missing_data_mod, ONLY: imdi

USE set_soil_alb_components_mod, ONLY: set_soil_alb_components

!TYPE definitions
USE sf_diags_mod, ONLY: strnewsfdiag
USE veg3_field_mod, ONLY: veg_state_type

USE jules_water_tracers_mod, ONLY: l_wtrac_jls, wtrac_calc_ratio_fn_jules

!Dr Hook
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    !Number of land points to be processed.
  land_index(land_pts),                                                        &
    !Index of land points on the P-grid.
  co2_dim_len,                                                                 &
    !Length of a CO2 field row.
  co2_dim_row,                                                                 &
    !Number of CO2 field rows.
  sm_levels,                                                                   &
    !Number of soil moisture levels.
  nsurft,                                                                      &
    !Number of surface tiles.
  n_wtrac_jls,                                                                 &
    !Number of water tracers in JULES
  surft_pts(ntype),                                                            &
    !Number of land points which include the nth surface type.
  surft_index(land_pts,ntype),                                                 &
    !Indices of land points which include the nth surface type.
  can_model,                                                                   &
    !Swith for thermal vegetation canopy
  dim_cs1
    !soil carbon dimensions

LOGICAL, INTENT(IN) ::                                                         &
  l_co2_interactive,                                                           &
    !switch for 3D CO2 field
  l_emis_surft_set(nsurft)
    !switch for varying surface emissivity of surface tiles

TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  co2_mmr,                                                                     &
    !Atmospheric CO2 concentration
  co2_3d(co2_dim_len,co2_dim_row),                                             &
    !3D atmos CO2 concentration (kg CO2/kg air).
  cs_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1),                          &
    !Soil carbon (kg C/m2).
  frac(land_pts,ntype),                                                        &
    !Surface type fractions.
  non_irrig_frac(land_pts),                                                    &
    !Fraction of non-irrigated tiles.
  canht_pft(land_pts,npft),                                                    &
    !Canopy height (m).
  photosynth_act_rad(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),     &
    !Incident PAR (W/m2).
  lai_pft(land_pts,npft),                                                      &
    !Leaf area index.
  pstar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                  &
    !Surface pressure (Pa).
  qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                   &
    !Specific humidity at level 1 (kg H2O/kg air).
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    !Soil moisture content in each layer as a fraction of saturation
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    !Frozen Soil moisture content in each layer as a fraction of saturation
  t_soil_soilt(land_pts,nsoilt,sm_levels),                                     &
    !Soil temperature (K).
  tstar_surft(land_pts,nsurft),                                                &
    !Tile surface temperatures (K).
  smvccl_soilt(land_pts,nsoilt,sm_levels),                                     &
    !Volumetric soil moisture concentration at field capacity (m3 H2O/m3 soil).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    !Volumetric soil moisture concentration at saturation (m3 H2O/m3 soil).
  smvcwt_soilt(land_pts,nsoilt,sm_levels),                                     &
    !Volumetric soil moisture concentration below which stomata close
    !(m3 H2O/m3 soil).
  albsoil_soilt(land_pts,nsoilt),                                              &
    !Soil albedo.
  vshr(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                   &
    !Windspeed (m/s).
  z0_surft(land_pts,nsurft),                                                   &
    !Tile roughness lengths (m).
  z1_uv_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),               &
    !Windspeed reference height(m).
  o3(land_pts),                                                                &
    !Surface ozone concentration (ppb).
  cos_zenith_angle(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),       &
    !Cosine of the zenith angle
  non_lake_frac(land_pts)
    ! Sum of fractions of surface tiles linked to soil

TYPE(veg_state_type), INTENT(IN OUT) :: veg_state

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  emis_surft(land_pts,nsurft),                                                 &
    !Emissivity for land tiles
  gs(land_pts)
    !Gridbox mean surface conductance (m/s).

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  canhc_surft(land_pts,nsurft),                                                &
    !Areal heat capacity of canopy for land tiles (J/K/m2).
  flake(land_pts,nsurft),                                                      &
    !Lake fraction.
  g_leaf(land_pts,npft),                                                       &
    !Leaf turnover rate (/360days).
  gc_surft(land_pts,nsurft),                                                   &
    !Surface conductance for land tiles (m/s).
  gpp(land_pts),                                                               &
    !Gridbox mean gross primary productivity (kg C/m2/s).
  gpp_pft(land_pts,npft),                                                      &
    !Gross primary productivity (kg C/m2/s).
  npp(land_pts),                                                               &
  !Gridbox mean net primary productivity (kg C/m2/s).
  npp_pft(land_pts,npft),                                                      &
    !Net primary productivity (kg C/m2/s).
  resp_p(land_pts),                                                            &
    !Gridbox mean plant respiration (kg C/m2/s).
  resp_p_pft(land_pts,npft),                                                   &
    !Plant respiration (kg C/m2/s).
  resp_s_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1),                           &
    !Soil respiration (kg C/m2/s).
  resp_l_pft(land_pts,npft),                                                   &
    !Leaf maintenance respiration (kg C/m2/s).
  resp_r_pft(land_pts,npft),                                                   &
    !Root maintenance respiration (kg C/m2/s).
  resp_w_pft(land_pts,npft),                                                   &
    !Wood maintenance respiration (kg C/m2/s).
  n_leaf(land_pts,npft),                                                       &
    !Leaf N content scaled by LAI (kg N/m2).
  n_root(land_pts,npft),                                                       &
    !Root N content scaled by LAI_bal (kg N/m2).
  n_stem(land_pts,npft),                                                       &
    !Stem N content scaled by LAI_bal (kg N/m2).
  lai_bal(land_pts,npft),                                                      &
    !LAI_bal
  smc_soilt(land_pts,nsoilt),                                                  &
    !Available moisture in the soil profile (mm).
  vfrac_surft(land_pts,nsurft),                                                &
    !Fractional canopy coverage for land tiles.
  emis_soil(land_pts),                                                         &
    !Emissivity of underlying soil
  wt_ext_surft(land_pts,sm_levels,nsurft),                                     &
    !Fraction of evapotranspiration which is extracted from each soil layer
    !by each tile.
  fsmc_pft(land_pts,npft),                                                     &
    !Moisture availability factor.
  flux_o3_pft(land_pts,npft),                                                  &
    !Flux of O3 to stomata (nmol O3/m2/s).
  fo3_pft(land_pts,npft),                                                      &
    !Ozone exposure factor.
  gc_stom_surft(land_pts,nsurft),                                              &
    !Canopy conductance
  gc_corr(land_pts,npft),                                                      &
    !Stomatal conductance without bare soil evaporation
  f_nsc_pft(land_pts,npft),                                                    &
    !Plant non-structural carbohdrate mass fraction (kgC/kgC)
  growth_sug_gb(land_pts),                                                     &
    !Gridbox mean structural C growth rate (kg C/m2/s)
  growth_sug_pft(land_pts,npft),                                               &
    !Plant structural C growth rate (kg C/m2/s)
  psi_root_zone_pft(land_pts,npft),                                            &
    !Water potential in the root zone (Pa)
  lwp_c_pft(land_pts,npft)
    !Canopy leaf water potential (MPa)


!New arguments replacing USE statements
!urban_param
REAL(KIND=real_jlslsm), INTENT(IN) :: emisr_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN) :: emisw_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN) :: hwr_gb(land_pts)

!jules_mod
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
        albobs_scaling_surft(land_pts,ntype,rad_nband)

!p_s_parms (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: bexp_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sathh_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: v_close_pft(land_pts,sm_levels,npft)
REAL(KIND=real_jlslsm), INTENT(IN) :: v_open_pft(land_pts,sm_levels,npft)

!crop_vars_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: rootc_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthu_irr_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: frac_irr_soilt(land_pts,nsoilt)
REAL(KIND=real_jlslsm), INTENT(IN) :: frac_irr_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: dvi_cpft(land_pts,ncpft)

!Fluxes
REAL(KIND=real_jlslsm), INTENT(IN) :: t_home_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN) :: t_growth_gb(land_pts)

!bvoc_vars
REAL(KIND=real_jlslsm), INTENT(OUT) :: isoprene_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: terpene_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: methanol_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: acetone_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: isoprene_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: terpene_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: methanol_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: acetone_pft(land_pts,npft)

!trif_vars_mod
REAL(KIND=real_jlslsm), INTENT(OUT) :: fapar_diag_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: apar_diag_pft(land_pts,npft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: apar_diag_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: gpp_gb_acc(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: gpp_pft_acc(land_pts,npft)

!crop_vars_mod (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: gs_irr_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: smc_irr_soilt(land_pts,nsoilt)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
        wt_ext_irr_surft(land_pts,sm_levels,nsurft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: gc_irr_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: gs_nir_surft(land_pts,nsurft)

!ancil_info (IN)
LOGICAL, INTENT(IN) :: l_soil_point(land_pts)

!JULES surface_types_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: diff_frac(t_i_length * t_j_length)

! Water tracers (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: sthu_soilt_wtrac(land_pts,nsoilt,        &
                                                       sm_levels,n_wtrac_jls)
                                     ! Water tracer soil moisture content in
                                     ! each lyr as a fraction of saturation

! Water tracers (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: smc_soilt_wtrac(land_pts,nsoilt,        &
                                                       n_wtrac_jls)
                                     ! Water tracer in available moisture in
                                     ! the soil profile (kg/m2).

!Local variables
LOGICAL ::                                                                     &
   firstcall = .TRUE.

INTEGER ::                                                                     &
 can_rad_mod                                                                   &
!                                !Switch for canopy radiation model
,ilayers                                                                       &
!                                !No of layers in canopy radiation model
,albpft_call = imdi              ! Flag for albpft, scaling to obs

REAL(KIND=real_jlslsm) ::                                                      &
 alb_type_dummy(land_pts,ntype,4)                                              &
!                                 ! WORK Dummy argument for albedo
!                                 ! subroutine
,fapar_dir(land_pts,npft,ilayers)                                              &
!                                 ! WORK Profile of absorbed PAR -
!                                 ! Direct beam
,fapar_dif(land_pts,npft,ilayers)                                              &
!                                 ! WORK Profile of absorbed PAR -
!                                 ! Diffuse beam
,faparv(land_pts,ilayers)                                                      &
!                                 ! WORK Profile of absorbed PAR.
,fapar_dif2dif(land_pts,npft,ilayers)                                          &
!                                 ! WORK Profile of absorbed PAR
!                                 ! - Diffuse (fraction/LAI)
,fapar_dir2dif(land_pts,npft,ilayers)                                          &
!                                 ! WORK Profile of scattered PAR
!                                 ! -direct to diffuse (fraction/LAI)
,fapar_dir2dir(land_pts,npft,ilayers)                                          &
!                                 ! WORK Profile of absorbed only
!                                 ! direct PAR (fraction/LAI)
,fapar_shd(land_pts,ilayers)                                                   &
!                                 ! WORK fraction of total absorbed PAR
!                                 ! by shaded leaves
,fapar_sun(land_pts,ilayers)                                                   &
!                                 ! WORK fraction of total absorbed PAR
!                                 ! by sunlit leaves
,fsun(land_pts,npft,ilayers)                                                   &
!                                 ! WORK fraction of leaves that are
!                                 ! sunlit
,fsun_tmp(land_pts,ilayers)                                                    &
!                                 ! WORK temporary array for fraction of leaves
!                                 ! that are sunlit
,veg_frac(land_pts)                                                            &
                                  ! WORK vegetated fraction of gridbox
,v_open(land_pts,sm_levels)                                                    &
                                  ! WORK Volumetric soil moisture
                                  ! concentration above which stomatal
                                  ! aperture is not limited by soil water
                                  ! (m3 H2O/m3 soil).
                                  ! When l_use_pft_psi=F, v_open
                                  ! equals smvccl_soilt - fsmc_p0 *
                                  ! (smvccl_soilt - smvcwt_soilt)
                                  ! for that soil tile and pft.
                                  ! When l_use_pft_psi=T, v_open
                                  ! was calculated from psi_open for that
                                  ! soil tile and pft.
,v_close(land_pts,sm_levels)
                                  ! WORK Volumetric soil moisture
                                  ! concentration below which
                                  ! stomata close (m3 H2O/m3 soil).
                                  ! When l_use_pft_psi=F, v_close
                                  ! equals smvcwt_soilt for that soil tile.
                                  ! When l_use_pft_psi=T, v_close
                                  ! was calculated from psi_close for that
                                  ! soil tile and pft.
REAL(KIND=real_jlslsm) ::                                                      &
fsmc_nir(land_pts,npft)                                                        &
!                            ! WORK Soil moisture availability
!                                 !     factor over non-irrigated fraction.
,fsmc_irr(land_pts,npft)                                                       &
!                            ! WORK Soil moisture availability
!                                 !     factor over irrigated fraction.
,sthu_nir_soilt(land_pts,nsoilt,sm_levels)                                     &
                             ! WORK Soil moisture content in each layer
!                                 !     in the non-irrigated soil column
!                                 !     as a fraction of saturation
,sthu_surft(land_pts,nsoilt,sm_levels)                                         &
                             ! WORK Soil moisture content in each layer
!                                 !     as a fraction of saturation
!                                 !     on surface tiles
,wt_ext_nir_soilt(land_pts,nsoilt,sm_levels)                                   &
!                            ! WORK Fraction of transpiration over
!                                 !      non-irrigated fraction extracted
!                                 !      from each soil layer.
,wt_ext_irr_soilt(land_pts,nsoilt,sm_levels)                                   &
!                            ! WORK Fraction of transpiration over
!                                 !      irrigated fraction extracted
!                                 !      from each soil layer.
,wt_ext_nir_type(land_pts,sm_levels,ntype)                                     &
!                            ! WORK Fraction of transpiration over
!                                 !     non-irrigated area extracted from
!                                 !     each soil layer (kg/m2/s).
,wt_ext_irr_type(land_pts,sm_levels,ntype)                                     &
!                            ! WORK Fraction of transpiration over
!                                 !     irrigated area extracted from
!                                 !     each soil layer (kg/m2/s).
,smc_nir_soilt(land_pts,nsoilt)                                                &
,gs_nir_type(land_pts,ntype)                                                   &
!                            ! WORK Conductance for non-irrigated surface types
,gs_irr_type(land_pts,ntype)                                                   &
!                            ! WORK Conductance for irrigated surface types

,gsoil_nir_soilt(land_pts,nsoilt)                                              &
!                                 ! WORK Bare soil conductance over
!                                 !      non-irrigated fraction.
,gsoil_irr_soilt(land_pts,nsoilt)                                              &
!                                 ! WORK Bare soil conductance over
!                                 !      irrigated fraction.
,gsoil_under_canopy(land_pts)                                                  &
!                                 ! WORK Bare soil conductance under
!                                 !      canopy
,gsoil_nir_under_canopy(land_pts)                                              &
!                                 ! WORK Bare soil conductance under
!                                 !      canopy on non-irrigated fraction.
,gsoil_irr_under_canopy(land_pts)
!                                 ! WORK Bare soil conductance under
!                                 !      canopy on irrigated fraction.

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 ratio_wt(:,:,:,:)
                             ! WORK Water tracer to water ratio

INTEGER ::                                                                     &
open_index(land_pts)                                                           &
!                            ! WORK Index of land points
!                                 !      with open stomata.
,open_pts                    ! WORK Number of land points
!                                 !      with open stomata.


REAL(KIND=real_jlslsm) ::                                                      &
 cosz_gb(land_pts)                                                             &
                            !cos zenith angle on land points
,canhc(land_pts)                                                               &
                            ! WORK Canopy heat capacity (J/K/m2).
,ch_type(land_pts,ntype)                                                       &
                            ! WORK CANHC for surface types.
,root_param(land_pts,npft)                                                     &
                            ! WORK Parameter for F_ROOT
,f_root(land_pts,sm_levels)                                                    &
                            ! WORK Fraction of roots in each soil
!                                 !      layer.
,f_root_tmp(sm_levels)                                                         &
                            ! WORK Temp array for fraction of roots
!                                 !      in each soil layer.
,gsoil_soilt(land_pts,nsoilt)                                                  &
                            ! WORK Bare soil conductance.
,gs_type(land_pts,ntype)                                                       &
                            ! WORK Conductance for surface types.
,pstar_land(land_pts)                                                          &
                            ! WORK Surface pressure (Pa).
,ipar_land(land_pts)                                                           &
                            ! WORK Incident PAR (W/m2).
,q1_land(land_pts)                                                             &
                            ! WORK ecific humidity at level 1
,ra(land_pts)                                                                  &
                            ! WORK Aerodynamic resistance (s/m).
,rib(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                            ! WORK Bulk Richardson Number.
,tstar(land_pts)                                                               &
                            ! WORK Surface temperature (K).
,vfrac(land_pts)                                                               &
                            ! WORK Fractional canopy coverage.
,vf_type(land_pts,ntype)                                                       &
                            ! WORK VFRAC for surface types.
,wt_ext_type(land_pts,sm_levels,ntype)                                         &
!                           ! WORK WT_EXT for surface types.
,wt_ext_soilt(land_pts,nsoilt,sm_levels)                                       &
                            ! Gridbox-mean wt_ext_soilt
,fsoil(land_pts,npft)                                                          &
                            ! WORK Fraction of ground below canopy
!                                 !      contributing to evaporation.
,fsoil_irr_tot(land_pts)                                                       &
                            ! WORK Fraction of ground below canopy
!                                 !      contributing to evaporation
!                                 !      over irrigated fraction.
,fsoil_nir_tot(land_pts)                                                       &
                            ! WORK Fraction of ground below canopy
!                                 !      contributing to evaporation
!                                 !      over non-irrigated fraction.
,fsoil_irr_tot_tmp                                                             &
                            ! WORK Fraction of ground below canopy
!                                 !      contributing to evaporation
!                                 !      over irrigated fraction.
,fsoil_nir_tot_tmp                                                             &
                            ! WORK Fraction of ground below canopy
!                                 !      contributing to evaporation
!                                 !      over non-irrigated fraction.
,fsoil_tot(land_pts)                                                           &
                            ! WORK Total fraction of soil
!                                 !      contributing to evaporation
!                                 !      ( = bare soil fraction
!                                 !    + fraction of ground below canopy
,z0(land_pts)                                                                  &
                            ! WORK Roughness length (m).
,emisc(land_pts)                                                               &
!                                 ! WORK Emissivity of canyon
,dz_wall                                                                       &
!                                 ! WORK Damping depth: wall
,dz_road                                                                       &
!                                 ! WORK Damping depth: road
,dz_roof                                                                       &
!                                 ! WORK Damping depth: roof
,dz_roof_c                  ! WORK Damping depth: roof

REAL(KIND=real_jlslsm) :: albudir(land_pts,2,npft)
!                                 ! Direct albedo of underlying
!                                 ! surface
REAL(KIND=real_jlslsm) :: albudif(land_pts,2,npft)
!                                 ! Diffuse albedo of underlying
!                                 ! surface
!

REAL(KIND=real_jlslsm) :: lai_pft_soil_evap(land_pts,npft)
                                   ! Leaf area index for soil_evap routine.
                                   ! If the RP2b scheme is switched on, LAI
                                   ! will have been scaled by a random
                                   ! parameter.  When calculating fsoil
                                   ! in the routine soil_evap, the unscaled
                                   ! LAI needs to be used to prevent bare soil
                                   ! evaporation from taking over.
                                   ! If the RP scheme is switched off or this
                                   ! is a standalone run, then lai_pft_soil_evap
                                   ! equals lai_pft.


INTEGER ::                                                                     &
  i,    & !for ij (gridded variables)
  j,    & !for ij (gridded variables); also l=surft_index(j,n)
  k,    & !index for sm_levels
  l,    & !index for gridbox in gb or tiled variables
  m,    & !index for soil tile
  n,    & !index for surface tile
  il,   & !index for canopy layers
  i_wt, & !index for water tracers
  errorstatus

INTEGER :: asteps_since_triffid

LOGICAL :: l_getprofile     ! Switch IN to albpft

INTEGER, PARAMETER :: omp_cutoff=50   ! Cut off for loop multithreading
                                      ! This variable was introduced to do
                                      ! multithreasding of single level short
                                      ! loops with not much work involved to
                                      ! reduce degradation in their performance
                                      ! with multithreading
                                      ! 50 is a randomly picked up number,
                                      ! omp_cutoff should be O(100) based on
                                      ! using simple test cases
LOGICAL :: l_do_omp                   ! Do OpenMP

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PHYSIOL'

CHARACTER(LEN=80) :: ERRMSG

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Initialisations
!-----------------------------------------------------------------------
veg_frac(:) = 0.0
l_do_omp    = land_pts>omp_cutoff

!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE)                                      &
!$OMP PRIVATE(i, j, k, l, n, m, il, i_wt)                                      &
!$OMP SHARED(dim_cslayer, l_do_omp)                                            &
!$OMP SHARED(npft,land_pts,gpp_pft,npp_pft,resp_p_pft,resp_w_pft,              &
!$OMP resp_l_pft,resp_r_pft,fsmc_pft,apar_diag_pft,isoprene_pft,terpene_pft,   &
!$OMP methanol_pft,acetone_pft, nsoilt,smc_soilt,g_leaf,fsmc_irr,root_param,   &
!$OMP sm_levels,rib,f_root,tdims, ilayers,faparv,nsurft,gc_stom_surft,         &
!$OMP smc_irr_soilt,smc_nir_soilt,ntype,alb_type_dummy,fapar_dir,fapar_dif,    &
!$OMP wt_ext_soilt,wt_ext_irr_soilt,wt_ext_irr_type,wt_ext_type,               &
!$OMP dim_cs1,resp_s_soilt,gpp,npp,resp_p,ra,canhc,vfrac,apar_diag_gb,         &
!$OMP isoprene_gb,terpene_gb,methanol_gb,acetone_gb,fsoil_tot,frac,            &
!$OMP land_index, t_i_length, pstar_land, pstar, ipar_land,                    &
!$OMP photosynth_act_rad, q1_land, qw_1, gs_type, gs,                          &
!$OMP l_irrig_dmd, l_soil_evap_irrig_expl, gs_irr_type,                        &
!$OMP cosz_gb, cos_zenith_angle,                                               &
!$OMP gsoil_irr_soilt, smvccl_soilt, gs_nvg, soil, l_limit_gsoil,              &
!$OMP sthu_irr_soilt, smvcst_soilt, gsoil_soilt, sthu_soilt,                   &
!$OMP gc_corr, n_wtrac_jls, smc_soilt_wtrac, growth_sug_pft, growth_sug_gb,    &
!$OMP fsmc_nir, wt_ext_nir_soilt, wt_ext_nir_type, sthu_nir_soilt,             &
!$OMP frac_irr_soilt, fsoil_irr_tot, fsoil_nir_tot, gs_nir_type,               &
!$OMP gsoil_nir_soilt,lwp_c_pft,l_wtrac_jls,frac_irr_surft)

DO n = 1,npft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    gpp_pft(l,n)      = 0.0
    npp_pft(l,n)      = 0.0
    resp_p_pft(l,n)   = 0.0
    resp_w_pft(l,n)   = 0.0
    resp_l_pft(l,n)   = 0.0
    resp_r_pft(l,n)   = 0.0
    growth_sug_pft(l,n) = 0.0
    apar_diag_pft(l,n)= 0.0
    isoprene_pft(l,n) = 0.0
    terpene_pft(l,n)  = 0.0
    methanol_pft(l,n) = 0.0
    acetone_pft(l,n)  = 0.0
    g_leaf(l,n)       = 0.0
    fsmc_nir(l,n)     = 0.0
    fsmc_irr(l,n)     = 0.0
    root_param(l,n)   = 0.0
    gc_corr(l,n)      = 0.0
    lwp_c_pft(l,n)    = 0.0
    ! Where irrig_option = tile_based_irrigation and irrig_tile = 1 there is unrestricted soil
    ! moisture availability and fsmc_pft = 1.0
    ! For tiles where irrig_tile = 0 and/or irrig_option = no_irrigation
    ! or frac_based_irrigation then fsmc_pft is set by smc_ext.
    fsmc_pft(l,n)     = 1.0
  END DO
!$OMP END DO NOWAIT
END DO

!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  gpp(l)                = 0.0
  npp(l)                = 0.0
  resp_p(l)             = 0.0
  growth_sug_gb(l)      = 0.0
  ra(l)                 = 0.0
  canhc(l)              = 0.0
  vfrac(l)              = 0.0
  apar_diag_gb(l)       = 0.0
  isoprene_gb(l)         = 0.0
  terpene_gb(l)          = 0.0
  methanol_gb(l)         = 0.0
  acetone_gb(l)          = 0.0
END DO
!$OMP END DO NOWAIT

DO m = 1,nsoilt
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    smc_soilt(l,m) = 0.0
    smc_irr_soilt(l,m)     = 0.0
    smc_nir_soilt(l,m)     = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
    DO m = 1,nsoilt
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        smc_soilt_wtrac(l,m,i_wt) = 0.0
      END DO
!$OMP END DO NOWAIT
    END DO
  END DO
END IF

DO k = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    f_root(l,k) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    rib(i,j) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

DO il = 1,ilayers
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    faparv(l,il) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    gc_stom_surft(l,n)     = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

DO k = 1,4
  DO n = 1,ntype
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      alb_type_dummy(l,n,k)     = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END DO

DO il = 1,ilayers
  DO n = 1,npft
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      fapar_dir(l,n,il)     = 0.0
      fapar_dif(l,n,il)     = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END DO

DO k = 1,sm_levels
  DO m = 1,nsoilt
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      wt_ext_soilt(l,m,k)     = 0.0
      wt_ext_irr_soilt(l,m,k)     = 0.0
      wt_ext_nir_soilt(l,m,k)     = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END DO

DO n = 1,ntype
  DO k = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      wt_ext_irr_type(l,k,n)     = 0.0
      wt_ext_nir_type(l,k,n)     = 0.0
      wt_ext_type(l,k,n)     = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END DO

DO n = 1,dim_cs1
  DO k = 1,dim_cslayer
    DO m = 1,nsoilt
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        resp_s_soilt(l,m,k,n) = 0.0
      END DO
!$OMP END DO NOWAIT
    END DO
  END DO
END DO

IF ( l_irrig_dmd ) THEN
  DO k = 1,sm_levels
    DO m = 1,nsoilt
      DO l = 1,land_pts
        sthu_nir_soilt(l,m,k) = sthu_soilt(l,m,k)
        IF ( l_irrig_dmd ) THEN
          sthu_nir_soilt(l,m,k) = sthu_soilt(l,m,k)
          IF ( frac_irr_soilt(l,m) < 1.0 ) THEN
            sthu_nir_soilt(l,m,k) =                                            &
                 (sthu_soilt(l,m,k) - frac_irr_soilt(l,m)                      &
                 * sthu_irr_soilt(l,m,k))                                      &
                 / (1.0 - frac_irr_soilt(l,m))
          ELSE
            sthu_nir_soilt(l,m,k) = sthu_irr_soilt(l,m,k)
          END IF
        END IF
      END DO
    END DO
  END DO
END IF

DO n = 1,ntype
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    gs_type(l,n) = gs(l)
    IF (l_irrig_dmd) THEN
      gs_irr_type(l,n) = gs(l)
      IF ( l_soil_evap_irrig_expl ) THEN
        gs_nir_type(l,n) = gs(l)
      END IF
    END IF
  END DO
!$OMP END DO NOWAIT
END DO

DO m = 1, nsoilt
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    IF (smvccl_soilt(l,m,1) > 0.0 .AND. l_limit_gsoil) THEN
      gsoil_soilt(l,m) = gs_nvg(soil - npft) * MIN(1.0, (sthu_soilt(l,m,1)     &
                          * smvcst_soilt(l,m,1) / smvccl_soilt(l,m,1))**2)
    ELSE IF (smvccl_soilt(l,m,1) > 0.0) THEN
      gsoil_soilt(l,m) = gs_nvg(soil - npft) * (sthu_soilt(l,m,1)              &
                          * smvcst_soilt(l,m,1) / smvccl_soilt(l,m,1))**2
    ELSE
      gsoil_soilt(l,m) = 0.0
    END IF
  END DO
!$OMP END DO NOWAIT

END DO

! repeated for irrigation
IF (l_irrig_dmd) THEN
  DO m = 1, nsoilt
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      gsoil_irr_soilt(l,m) = 0.0
      IF (smvccl_soilt(l,m,1) > 0.0 .AND. l_limit_gsoil) THEN
        gsoil_irr_soilt(l,m) = gs_nvg(soil - npft) *                           &
                               MIN(1.0, (sthu_irr_soilt(l,m,1)                 &
                               * smvcst_soilt(l,m,1) / smvccl_soilt(l,m,1))**2)
      ELSE IF (smvccl_soilt(l,m,1) > 0.0) THEN
        gsoil_irr_soilt(l,m) = gs_nvg(soil - npft) *                           &
                               (sthu_irr_soilt(l,m,1)                          &
                               * smvcst_soilt(l,m,1) / smvccl_soilt(l,m,1))**2
        ! ELSE Do nothing
      END IF
      IF ( l_soil_evap_irrig_expl ) THEN
        gsoil_nir_soilt(l,m) = 0.0
        IF (smvccl_soilt(l,m,1) > 0.0 .AND. l_limit_gsoil) THEN
          gsoil_nir_soilt(l,m) = gs_nvg(soil - npft) *                         &
                              MIN(1.0, (sthu_nir_soilt(l,m,1)                  &
                              * smvcst_soilt(l,m,1) / smvccl_soilt(l,m,1))**2)
        ELSE IF (smvccl_soilt(l,m,1) > 0.0) THEN
          gsoil_nir_soilt(l,m) = gs_nvg(soil - npft) *                         &
                              (sthu_nir_soilt(l,m,1)                           &
                              * smvcst_soilt(l,m,1) / smvccl_soilt(l,m,1))**2
          ! ELSE Do nothing
        END IF
      END IF
    END DO
!$OMP END DO NOWAIT
  END DO
END IF

!Compress some variables to land points only for st_stom

!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j             = (land_index(l) - 1) / t_i_length + 1
  i             = land_index(l) - (j-1) * t_i_length
  pstar_land(l) = pstar(i,j)
  ipar_land(l)  = photosynth_act_rad(i,j)
  q1_land(l)    = qw_1(i,j)
  cosz_gb(l)    = cos_zenith_angle(i,j)
  fsoil_tot(l) = frac(l,soil)
  IF ( l_soil_evap_irrig_expl ) THEN
    fsoil_irr_tot(l) = frac(l,soil)*frac_irr_surft(l,soil)
    fsoil_nir_tot(l) = frac(l,soil)*(1.0-frac_irr_surft(l,soil))
  END IF
  gs(l)        = 0.0
END DO
!$OMP END DO NOWAIT

!$OMP END PARALLEL


!==============================================================================
! *NOTICE REGARDING SOIL TILING**
!
!The following section facilitates the use of soil tiling. As implemented,
!there are two soil tiling options:
!
!nsoilt == 1
!Operate as with a single soil tile, functionally identical to JULES upto
! at least vn4.7 (Oct 2016)
! This means that a soilt variable being passed 'up' to the surface is
! broadcast to the surft variable (with weighting by frac if requred)
!
!nsoilt > 1
!Operate with nsoilt = nsurft, with a direct mapping between them
! This means that a soilt variable being passed 'up' to the surface is simply
! copied into the surft variable
!
! This will need to be refactored for other tiling approaches. This note
! will be replicated elsewhere in the code as required
!
!These comments apply until **END NOTICE REGARDING SOIL TILING**
!==============================================================================

!-----------------------------------------------------------------------
! Calculate light absorption by the plant canopy
!-----------------------------------------------------------------------
IF ( can_rad_mod /= 1 ) THEN
  ! The logical argument (getProfile in albpft) is TRUE to indicate
  ! that profiles through the canopy should be calculated.
  IF (l_albedo_obs .AND. l_spec_albedo .AND.                                   &
        ( .NOT. l_spec_alb_bs ) ) THEN
    ! scale the input PFT scattering and reflectivity parameters to match
    ! albedo obs, using pre-corrected scalings:
    albpft_call = 2
  ELSE
    ! do not scale to match albedo obs:
    albpft_call = 0
  END IF
  !
  IF ( albpft_call > 0 ) THEN
    ! Set all underlying albedos to that of bare soil.
    ! The necessary logical conditions for the assignment will have been
    ! checked when the albedo was calculated.
    DO n = 1,npft

      !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        m = 1
      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        m = n
      END IF !nsoilt

      !     Note that albobs scaling is not soil tile-aware, so it may behave
      !     unxepectedly when used with soil tiling.
      CALL set_soil_alb_components(land_pts, albsoil_soilt(:,m), cosz_gb,      &
        albudir(:,:,n), albudif(:,:,n),                                        &
        albobs_scaling_surft(:,soil,1:2))

    END DO
  ELSE
    DO n = 1,npft

      !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        m = 1
      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        m = n
      END IF !nsoilt

      CALL set_soil_alb_components(land_pts, albsoil_soilt(:,m), cosz_gb,      &
        albudir(:,:,n), albudif(:,:,n))

    END DO
  END IF

  l_getprofile = .TRUE.

  CALL albpft     (                                                            &
    !INTENT(IN)
    l_getprofile, land_pts, ilayers, albpft_call,                              &
    surft_pts, surft_index,                                                    &
    cosz_gb, lai_pft, albudir, albudif,                                        &
  !INTENT(INOUT)
    alb_type_dummy,                                                            &
  !INTENT(OUT)
    !Real
    fapar_dir, fapar_dif, fapar_dir2dif,                                       &
    fapar_dif2dif, fapar_dir2dir, fsun,                                        &
    !New arguments replacing USE statements
    !jules_mod (IN OUT)
    albobs_scaling_surft)
END IF

!-----------------------------------------------------------------------
! Loop over Plant Functional Types to calculate the available moisture
! and the values of canopy conductance, the carbon fluxes and the leaf
! turnover rate
!-----------------------------------------------------------------------

DO n = 1,npft

  !Set the current soil tile (see notice above)
  IF (nsoilt == 1) THEN
    !There is only 1 soil tile
    m = 1
  ELSE ! nsoilt == nsurft
    !Soil tiles map directly on to surface tiles
    m = n
  END IF !nsoilt

  IF ( l_aggregate ) THEN
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(land_pts,       &
!$OMP   l_do_omp,  tstar, tstar_surft, z0, z0_surft) SCHEDULE(STATIC)
    DO l = 1,land_pts
      tstar(l) = tstar_surft(l,1)
      z0(l) = z0_surft(l,1)
    END DO
!$OMP END PARALLEL DO
  ELSE
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(land_pts,       &
!$OMP   l_do_omp,  n, tstar, tstar_surft, z0, z0_surft) SCHEDULE(STATIC)
    DO l = 1,land_pts
      tstar(l) = tstar_surft(l,n)
      z0(l) = z0_surft(l,n)
    END DO
!$OMP END PARALLEL DO
  END IF

  root_param(:,n) = rootd_ft(n)

  IF ( l_crop .AND. n > nnpft ) THEN
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(i, j)             &
!$OMP             SHARED(cfrac_r, n, nnpft, rootc_cpft, root_param, rootd_ft,  &
!$OMP                    rt_dir, surft_index, surft_pts) SCHEDULE(STATIC)
    DO j = 1,surft_pts(n)
      i = surft_index(j,n)

      root_param(i,n) = rootd_ft(n) *                                          &
         ( rootc_cpft(i,n - nnpft) / cfrac_r(n - nnpft) )** rt_dir(n - nnpft)
    END DO
!$OMP END PARALLEL DO
  END IF

!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(i, j, f_root_tmp) &
!$OMP SHARED(surft_pts, n, sm_levels, dzsoil, root_param, f_root, surft_index) &
!$OMP SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    i = surft_index(j,n)
    CALL root_frac(n,sm_levels,dzsoil,root_param(i,n),f_root_tmp)
    f_root(i,:) = f_root_tmp(:)
  END DO
!$OMP END PARALLEL DO

  ! Soil moisture *in this tile* is a combination of soil moisture
  ! in irrigated and non-irrigated fraction of grid box, dependent
  ! on irrigated fraction *in this tile*
  ! However, in the irrigated fraction *in this tile*, soil moisture
  ! is the same as in the overall gridbox irrigated fraction
!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE) PRIVATE(l, k)                        &
!$OMP             SHARED(frac_irr_soilt, frac_irr_surft, land_pts, v_open,     &
!$OMP             l_irrig_dmd, l_soil_evap_irrig_expl,                         &
!$OMP           n, m, l_use_pft_psi, v_close, v_close_pft,                     &
!$OMP             sm_levels, sthu_soilt, sthu_irr_soilt, sthu_nir_soilt,       &
!$OMP l_do_omp,    sthu_surft, v_open_pft, smvcwt_soilt, smvccl_soilt, fsmc_p0)
  DO k = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      sthu_surft(l,m,k) = sthu_soilt(l,m,k)
    END DO
!$OMP END DO

    IF ( l_irrig_dmd ) THEN
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        sthu_surft(l,m,k) = frac_irr_surft(l,n) * sthu_irr_soilt(l,m,k)        &
               + (1.0 - frac_irr_surft(l,n)) * sthu_nir_soilt(l,m,k)
      END DO
!$OMP END DO
    END IF
  END DO

  IF (l_use_pft_psi ) THEN
    DO k = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        v_close(l,k) = v_close_pft(l,k,n)
        v_open(l,k)  = v_open_pft(l,k,n)
      END DO
!$OMP END DO
    END DO
  ELSE
    DO k = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        v_close(l,k) = smvcwt_soilt(l,m,k)
        v_open(l,k)  = smvccl_soilt(l,m,k) -                                   &
                     fsmc_p0(n) * (smvccl_soilt(l,m,k) - smvcwt_soilt(l,m,k))
      END DO
!$OMP END DO
    END DO
  END IF
!$OMP END PARALLEL

  ! Tile-based irrigated surface types do not extract water so routine is not called
  IF ( irrig_tile(n) /= 1 ) THEN
    CALL smc_ext (land_pts,sm_levels,surft_pts(n),surft_index(:,n), n, f_root, &
                  sthu_surft(:,m,:),                                           &
                  v_open,smvcst_soilt(:,m,:),                                  &
                  v_close,                                                     &
                  bexp_soilt(:,m,:), sathh_soilt(:,m,:),                       &
                  wt_ext_type(:,:,n),fsmc_pft(:,n),                            &
                  psi_root_zone_pft(:,n))
  END IF

  IF (l_irrig_dmd) THEN
    CALL smc_ext (land_pts,sm_levels,surft_pts(n),surft_index(:,n), n, f_root, &
                  sthu_irr_soilt(:,m,:),                                       &
                  v_open,smvcst_soilt(:,m,:),                                  &
                  v_close,                                                     &
                  bexp_soilt(:,m,:), sathh_soilt(:,m,:),                       &
                  wt_ext_irr_type(:,:,n),fsmc_irr(:,n),                        &
                  psi_root_zone_pft(:,n))

    IF ( l_soil_evap_irrig_expl ) THEN
      CALL smc_ext (land_pts,sm_levels,surft_pts(n),surft_index(:,n), n,       &
                    f_root,sthu_nir_soilt(:,m,:),                              &
                    v_open,smvcst_soilt(:,m,:),                                &
                    v_close,                                                   &
                    bexp_soilt(:,m,:), sathh_soilt(:,m,:),                     &
                    wt_ext_nir_type(:,:,n),fsmc_nir(:,n),                      &
                    psi_root_zone_pft(:,n))

      DO l = 1,land_pts
        fsmc_pft(l,n) = fsmc_nir(l,n)*(1.0-frac_irr_surft(l,n))                &
             + fsmc_irr(l,n)*frac_irr_surft(l,n)
        DO k = 1,sm_levels
          wt_ext_type(l,k,n) =                                                 &
               wt_ext_nir_type(l,k,n)*(1.0-frac_irr_surft(l,n))                &
               + wt_ext_irr_type(l,k,n)*frac_irr_surft(l,n)
        END DO
      END DO
    END IF
  END IF

  CALL raero (land_pts,land_index,surft_pts(n),surft_index(:,n)                &
,             rib,vshr,z0,z0,z1_uv_ij,ra)
  !-----------------------------------------------------------------------
  ! Calculate light absorption by the plant canopy
  !-----------------------------------------------------------------------

  IF (can_rad_mod /= 1) THEN

!$OMP PARALLEL DO IF(ilayers > 1) DEFAULT(NONE) PRIVATE(i, k, l, il)           &
!$OMP SHARED(ilayers, surft_pts, surft_index, land_index, faparv, diff_frac,   &
!$OMP        fapar_dir, fapar_dif, n)                                          &
!$OMP SCHEDULE(STATIC)
    DO il = 1,ilayers
      DO k = 1,surft_pts(n)
        l = surft_index(k,n)
        i = land_index(l)

        faparv(l,il) = (1.0 - diff_frac(i)) * fapar_dir(l,n,il)                &
                       + diff_frac(i) * fapar_dif(l,n,il)
      END DO !  points
    END DO  !  layer
!$OMP END PARALLEL DO

    IF ( can_rad_mod == 5 .OR. can_rad_mod == 6 ) THEN
!$OMP PARALLEL DO IF(ilayers > 1) DEFAULT(NONE) PRIVATE(i, k, l, il)           &
!$OMP SHARED(ilayers, surft_pts, surft_index, land_index, fapar_shd,           &
!$OMP        diff_frac, fapar_dif2dif, fapar_dir2dir, fsun, fapar_sun,         &
!$OMP        fapar_dir2dif, n)                                                 &
!$OMP SCHEDULE(STATIC)
      DO il = 1,ilayers
        DO k = 1,surft_pts(n)
          l = surft_index(k,n)
          i = land_index(l)

          fapar_shd(l,il) = diff_frac(i) * fapar_dif2dif(l,n,il)               &
                            + ( 1.0 - diff_frac(i) ) * fapar_dir2dif(l,n,il)

          IF ( fsun(l,n,il) > EPSILON(fsun) ) THEN
            fapar_sun(l,il) = fapar_shd(l,il)                                  &
                              + (1.0 - diff_frac(i)) * fapar_dir2dir(l,n,il)   &
                              / fsun(l,n,il)
          ELSE
            fapar_sun(l,il) = 0.0
          END IF

        END DO !  points
      END DO  !  layer
!$OMP END PARALLEL DO
    END IF  !  can_rad_mod=5/6

  END IF  !  can_rad_mod

  fsun_tmp(:,:) = fsun(:,n,:)

  CALL sf_stom (land_pts,land_index                                            &
,               surft_pts(n),surft_index(:,n),n                                &
,               co2_mmr,co2_3d,co2_dim_len                                     &
,               co2_dim_row,l_co2_interactive                                  &
,               fsmc_pft(:,n),veg_state,canht_pft(:,n)                         &
,               ipar_land,lai_pft(:,n)                                         &
,               canht_pft(:,n),pstar_land                                      &
,               q1_land,ra,tstar,o3,t_home_gb,t_growth_gb                      &
,               can_rad_mod,ilayers,faparv                                     &
,               psi_root_zone_pft(:,n),lwp_c_pft(:,n)                          &
,               gpp_pft(:,n),npp_pft(:,n),resp_p_pft(:,n)                      &
,               resp_l_pft(:,n),resp_r_pft(:,n),resp_w_pft(:,n)                &
,               growth_sug_pft(:,n),f_nsc_pft(:,n)                             &
,               n_leaf(:,n),n_root(:,n),n_stem(:,n)                            &
,               lai_bal(:,n)                                                   &
,               gs_type(:,n)                                                   &
,               fapar_sun,fapar_shd,fsun_tmp                                   &
,               flux_o3_pft(:,n),fo3_pft(:,n)                                  &
,               fapar_diag_pft(:,n),apar_diag_pft(:,n)                         &
,               isoprene_pft(:,n),terpene_pft(:,n)                             &
,               methanol_pft(:,n),acetone_pft(:,n)                             &
,               open_index,open_pts,                                           &
                    !New arguments replacing USE statements
                    !crop_vars_mod (IN)
                    dvi_cpft,rootc_cpft)

  ! Store conductance before adjustment for soil evaporation
  gc_corr(:,n) = gs_type(:,n)

  IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
    IF (l_aggregate) THEN
      DO l = 1,land_pts
        gc_stom_surft(l,1) = gs_type(l,1)
      END DO
    ELSE
      DO l = 1,land_pts
        gc_stom_surft(l,n) = gs_type(l,n)
      END DO
    END IF
  END IF

  IF (l_irrig_dmd) THEN
    ! adjust conductance for irrigated fraction
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(k, l)             &
!$OMP             SHARED(gs_irr_type, gs_nir_type, gs_type, n, surft_index,    &
!$OMP             surft_pts)                                                   &
!$OMP             SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      gs_nir_type(l,n) = gs_type(l,n)
      gs_irr_type(l,n) = gs_type(l,n)
    END DO
!$OMP END PARALLEL DO

    DO j = 1,open_pts
      l = surft_index(open_index(j),n)

      IF ( frac_irr_surft(l,n) > 0.0 ) THEN
        IF ( fsmc_pft(l,n) > 0.0 ) THEN
          gs_irr_type(l,n) = gs_type(l,n) * fsmc_irr(l,n) / fsmc_pft(l,n)
          IF ( l_soil_evap_irrig_expl ) THEN
            gs_nir_type(l,n) = gs_type(l,n) * fsmc_nir(l,n) / fsmc_pft(l,n)
          END IF

        ELSE
          ! hadrd - this should only happen if sthu is 0.0, see smc_ext
          WRITE (ERRMSG,*) 'tile:', n, 'point:',l, 'fsmc:', fsmc_pft(l,n),     &
              'sthu', sthu_soilt(l,m,n)
          errorstatus = 101
          CALL ereport("physiol", errorstatus,                                 &
              "***warning fsmc <= 0 in physiol.F90***" //                      &
               ERRMSG )
        END IF
      END IF
    END DO
  END IF

  ! Un-do scaling of lai by RP scheme to preserve the value of fsoil
  ! (calculated in the routine soil_evap).  This is to prevent
  ! bare-soil evaporation from taking over.
  IF (l_rp2 .AND. i_rp_scheme == i_rp2b) THEN
    ! lai_mult_rp is always non-zero
    lai_pft_soil_evap(:,n) = lai_pft(:,n) / lai_mult_rp(n)
  ELSE
    lai_pft_soil_evap(:,n) = lai_pft(:,n)
  END IF

  IF ( ABS(gsoil_f(n) - 1.0) <= EPSILON(0.0) ) THEN
    ! only needed for bit compatibility
    ! with code before gsoil_f parameter was added
    gsoil_under_canopy(:) = gsoil_soilt(:,m)
    gsoil_irr_under_canopy(:) = gsoil_irr_soilt(:,m)
    IF ( l_soil_evap_irrig_expl ) THEN
      gsoil_nir_under_canopy(:) = gsoil_nir_soilt(:,m)
    END IF
  ELSE
    gsoil_under_canopy(:) = gsoil_soilt(:,m) * gsoil_f(n)
    gsoil_irr_under_canopy(:) = gsoil_irr_soilt(:,m) * gsoil_f(n)
    IF ( l_soil_evap_irrig_expl ) THEN
      gsoil_nir_under_canopy(:) = gsoil_nir_soilt(:,m) * gsoil_f(n)
    END IF
  END IF

  CALL soil_evap (land_pts,sm_levels,surft_pts(n),surft_index(:,n),            &
                  irrig_tile(n),gsoil_under_canopy(:),lai_pft_soil_evap(:,n),  &
                  gs_type(:,n), wt_ext_type(:,:,n),fsoil(:,n),                 &
                  frac_irr_surft(:,n),gsoil_nir_under_canopy(:),               &
                  gs_nir_type(:,n),wt_ext_nir_type(:,:,n),                     &
                  gsoil_irr_under_canopy(:),                                   &
                  gs_irr_type(:,n),wt_ext_irr_type(:,:,n))

  CALL leaf_lit (land_pts,surft_pts(n),surft_index(:,n)                        &
,                n,fsmc_pft(:,n),tstar,g_leaf(:,n))

  CALL cancap (land_pts,surft_pts(n),surft_index(:,n),can_model,n              &
,              veg_state                                                       &
,              canht_pft(:,n),lai_pft(:,n),ch_type(:,n),vf_type(:,n),          &
                    !New arguments replacing USE statements
                    !crop_vars_mod (IN)
                    dvi_cpft)

!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(frac,           &
!$OMP   l_do_omp, fsoil, fsoil_tot, fsoil_irr_tot, fsoil_nir_tot,              &
!$OMP   l_soil_evap_irrig_expl, frac_irr_surft, land_pts, n) SCHEDULE(STATIC)
  DO l = 1,land_pts
    fsoil_tot(l) = fsoil_tot(l) + frac(l,n) * fsoil(l,n)
    IF ( l_soil_evap_irrig_expl ) THEN
      fsoil_irr_tot(l) = fsoil_irr_tot(l) + frac(l,n) * fsoil(l,n) *           &
           frac_irr_surft(l,n)
      fsoil_nir_tot(l) = fsoil_nir_tot(l) + frac(l,n) * fsoil(l,n) *           &
           (1.0-frac_irr_surft(l,n))

    END IF
  END DO
!$OMP END PARALLEL DO

END DO

!==============================================================================
! *END NOTICE REGARDING SOIL TILING**
!==============================================================================

!----------------------------------------------------------------------
! Non-vegetated surface types
!----------------------------------------------------------------------
DO n = npft+1,ntype
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP             SHARED(gs_irr_type, gs_nir_type, gs_nvg, gs_type,            &
!$OMP                    l_irrig_dmd, l_soil_evap_irrig_expl,                  &
!$OMP                    n, npft, surft_index, surft_pts)                      &
!$OMP             SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    gs_type(l,n) = gs_nvg(n - npft)
    IF (l_irrig_dmd) THEN
      gs_irr_type(l,n) = gs_nvg(n - npft) ! irrigation
      IF (l_soil_evap_irrig_expl ) THEN
        gs_nir_type(l,n) = gs_nvg(n - npft) ! non-irrigation
      END IF
    END IF
  END DO
!$OMP END PARALLEL DO
END DO

!==============================================================================
! *NOTICE REGARDING SOIL TILING**
!
!The following section facilitates the use of soil tiling. As implemented,
!there are two soil tiling options:
!
!nsoilt == 1
!Operate as with a single soil tile, functionally identical to JULES upto
! at least vn4.7 (Oct 2016)
! This means that a soilt variable being passed 'up' to the surface is
! broadcast to the surft variable (with weighting by frac if requred)
!
!nsoilt > 1
!Operate with nsoilt = nsurft, with a direct mapping between them
! This means that a soilt variable being passed 'up' to the surface is simply
! copied into the surft variable
!
! This will need to be refactored for other tiling approaches. This note
! will be replicated elsewhere in the code as required
!
!These comments apply until **END NOTICE REGARDING SOIL TILING**
!==============================================================================

!Set the current soil tile (see notice above)
IF (nsoilt == 1) THEN
  !There is only 1 soil tile
  m = 1
ELSE ! nsoilt == nsurft
  !Soil tiles map directly on to surface tiles
  m = soil
END IF !nsoilt

! Copy soil conductance and add bare soil fraction to extraction from
! surface layer
n = soil
!$OMP PARALLEL DO IF (surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)            &
!$OMP             SHARED(gs_irr_type, gs_type, gsoil_soilt, gsoil_irr_soilt,   &
!$OMP                    l_irrig_dmd, l_soil_evap_irrig_expl, frac_irr_surft,  &
!$OMP                    gsoil_nir_soilt, irrig_tile,                          &
!$OMP                    n, m, surft_index, surft_pts, wt_ext_type,            &
!$OMP                    wt_ext_irr_type, gs_nir_type) SCHEDULE(STATIC)
DO j = 1,surft_pts(n)
  l = surft_index(j,n)
  gs_type(l,n) = gsoil_soilt(l,m)
  IF (irrig_tile(n) /= 1 ) THEN
    wt_ext_type(l,1,n) = 1.0
  END IF
  IF (l_irrig_dmd) THEN
    IF ( l_soil_evap_irrig_expl ) THEN
      gs_type(l,n) = frac_irr_surft(l,n)*gsoil_irr_soilt(l,m)                  &
           +(1.0-frac_irr_surft(l,n))*gsoil_nir_soilt(l,m)
      gs_nir_type(l,n) = gsoil_nir_soilt(l,m) ! non-irrigation
    END IF
    gs_irr_type(l,n) = gsoil_irr_soilt(l,m) ! irrigation
    wt_ext_irr_type(l,1,n) = 1.0 ! irrigation
  END IF
END DO
!$OMP END PARALLEL DO

!==============================================================================
! *END NOTICE REGARDING SOIL TILING**
!==============================================================================

!----------------------------------------------------------------------
! Canopy heat capacity and coverage for non-vegetated surfaces
!----------------------------------------------------------------------
DO n = npft+1,ntype
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP             SHARED(ch_nvg, ch_type, n, npft, surft_index, surft_pts,     &
!$OMP                    vf_nvg, vf_type) SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    ch_type(l,n) = ch_nvg(n - npft)
    vf_type(l,n) = vf_nvg(n - npft)
  END DO
!$OMP END PARALLEL DO
END DO

! MORUSES - Update ch_type (ch_nvg) with calculated values for urban fabric.
! Roof is done at the same time as canyon; cannot have one without the other.
! Differs from original coding for efficiency reasons; taken out of previous
! loop.
IF ( l_moruses_storage ) THEN

  dz_wall   = ( 2.0 * diffus_wall / omega_day )**( 1.0 / 2.0 )
  dz_road   = ( 2.0 * diffus_road / omega_day )**( 1.0 / 2.0 )
  dz_roof_c = ( 2.0 * diffus_roof / omega_day )**( 1.0 / 2.0 )
  ! dz_roof is very thin to represent insulation
  IF ( l_moruses_storage_thin ) THEN
    dz_roof = MIN( dz_roof_c, dz_roof_p )
  ELSE
    dz_roof = dz_roof_c
  END IF

  n = urban_canyon
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP             SHARED(ch_type, dz_road, dz_wall, dz_roof, hwr_gb,           &
!$OMP                    l_aggregate,                                          &
!$OMP                    n, surft_index, surft_pts, urban_roof, vf_type)       &
!$OMP             SCHEDULE(STATIC)
  DO j = 1, surft_pts(n)
    l = surft_index(j,n)

    ! Canyon
    ch_type(l,n) = cap_road * dz_road +                                        &
       ( 2.0 * hwr_gb(l) * cap_wall * dz_wall )

    ! Where there's a roof there's a canyon
    ch_type(l,urban_roof) = cap_roof * dz_roof

  END DO
!$OMP END PARALLEL DO

END IF

!----------------------------------------------------------------------
! Surface emissivity
!----------------------------------------------------------------------

! URBAN-2T & MORUSES: Set canyon emissivity

IF ( l_moruses_emissivity ) THEN
  n = urban_canyon
  IF (.NOT. l_emis_surft_set(n)) THEN
    IF ( firstcall) THEN
      CALL jules_print('physiol_jls',                                          &
        'MORUSES canyon emissivity calculated',level = PrNorm)
    END IF
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP           SHARED(surft_pts, n, surft_index, hwr_gb, emisr_gb, emisw_gb, emisc) &
!$OMP           SCHEDULE(STATIC)
    DO j = 1, surft_pts(n)
      l = surft_index(j,n)
      CALL urbanemis(hwr_gb(l), emisr_gb(l), emiss, emisw_gb(l),               &
       emisc(l))
    END DO
!$OMP END PARALLEL DO
  END IF
END IF

! Calculate EMIS_surft

IF ( l_aggregate ) THEN

  DO n = 1,nsurft
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l)                        &
!$OMP              SHARED(land_pts, n, emis_surft, l_do_omp)                   &
!$OMP              SCHEDULE(STATIC)
    DO l = 1,land_pts
      emis_surft(l,n) = 0.0
    END DO
!$OMP END PARALLEL DO
  END DO

  DO n = 1,npft
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP           SHARED(emis_pft, emis_surft, frac, n, surft_index, surft_pts)  &
!$OMP           SCHEDULE(STATIC)
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      emis_surft(l,1) = emis_surft(l,1) + frac(l,n) * emis_pft(n)
    END DO
!$OMP END PARALLEL DO
  END DO

  ! MORUSES: Use emisc instead of EMIS_NVG if canyon tile is updated by MORUSES

  ! Implementation inelegant. If emis_nvg becomes dependent on land_pts then
  ! could overwrite emis_nvg with SUBROUTINE urbanemis and do away with the
  ! if statements. Alternatively could create a 2d work array e.g.
  ! emis_nvg_wk(land_pts,ntype) or re-write to have just
  ! emis_surft(land_pts,ntype) then copy aggregated value to
  ! emis_surft(land_pts,1). Similar to albedo in tile_albedo.

  IF ( l_moruses_emissivity ) THEN

    ! MORUSES

    DO n = npft+1,ntype
      IF ( n == urban_canyon ) THEN
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          IF ( firstcall ) THEN
            WRITE(jules_message,*) 'EMIS_surft: Emissivity of canyon used',    &
                n,j,l
            CALL jules_print('physiol_jls',jules_message,level = PrNorm)
          END IF
          emis_surft(l,1) = emis_surft(l,1)                                    &
             + frac(l,n) * emisc(l)
        END DO
      ELSE
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP              SHARED(emis_nvg, emis_surft, frac, n, npft, surft_index,    &
!$OMP                     surft_pts) SCHEDULE(STATIC)
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          emis_surft(l,1) = emis_surft(l,1)                                    &
             + frac(l,n) * emis_nvg(n - npft)
        END DO
!$OMP END PARALLEL DO
      END IF
    END DO

    firstcall = .FALSE.

  ELSE ! .NOT. l_moruses_emissivity
    DO n = npft+1,ntype
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP              SHARED(emis_nvg, emis_surft, frac, n, npft, surft_index,    &
!$OMP                     surft_pts) SCHEDULE(STATIC)
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        emis_surft(l,1) = emis_surft(l,1)                                      &
           + frac(l,n) * emis_nvg(n - npft)
      END DO
!$OMP END PARALLEL DO
    END DO
  END IF

ELSE ! .NOT. L_AGGREGATE

  DO n = 1,npft
    IF (surft_pts(n) > 0) THEN
      IF (.NOT. l_emis_surft_set(n)) THEN
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP              SHARED(emis_pft, emis_surft, n, surft_index, surft_pts)     &
!$OMP              SCHEDULE(STATIC)
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          emis_surft(l,n) = emis_pft(n)
        END DO
!$OMP END PARALLEL DO
      END IF
    END IF
  END DO

  DO n = npft+1,ntype
    IF (surft_pts(n) > 0) THEN
      IF (.NOT. l_emis_surft_set(n)) THEN
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP              SHARED(emis_nvg, emis_surft, n, npft, surft_index,          &
!$OMP                     surft_pts) SCHEDULE(STATIC)
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          emis_surft(l,n) = emis_nvg(n - npft)
        END DO
!$OMP END PARALLEL DO
      END IF
    END IF
  END DO

  ! MORUSES: Overwrite EMIS_surft for urban canyon
  IF ( l_moruses_emissivity ) THEN
    n = urban_canyon
    IF (.NOT. l_emis_surft_set(n)) THEN
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP             SHARED(emisc, emis_surft, n, surft_index,                    &
!$OMP                    surft_pts) SCHEDULE(STATIC)
      DO j = 1, surft_pts(n)
        l = surft_index(j,n)
        emis_surft(l,n) = emisc(l)
      END DO
!$OMP END PARALLEL DO
      firstcall = .FALSE.
    END IF
  END IF

END IF ! L_AGGREGATE

DO l = 1,land_pts
  emis_soil(l) = emis_nvg(soil - npft)
END DO

!----------------------------------------------------------------------
! Calculate the rate of soil respiration
!----------------------------------------------------------------------
SELECT CASE ( soil_bgc_model )
CASE ( soil_model_1pool, soil_model_4pool )

  !   Set VEG_FRAC according to whether it is full or dummy field
  IF ( soil_bgc_model == soil_model_4pool ) THEN
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(frac,           &
!$OMP             land_pts, npft, veg_frac,l_do_omp) SCHEDULE(STATIC)
    DO l = 1,land_pts
      veg_frac(l) = SUM(frac(l,1:npft))
    END DO
!$OMP END PARALLEL DO
  END IF  !  4-pool soil C

  DO m = 1, nsoilt
    CALL microbe (land_pts,dim_cs1,l_q10,cs_pool_soilt(:,m,:,:),               &
                  sthu_soilt(:,m,:),sthf_soilt(:,m,:),smvcst_soilt(:,m,:),     &
                  smvccl_soilt(:,m,:),smvcwt_soilt(:,m,:),t_soil_soilt(:,m,:), &
                  resp_s_soilt(:,m,:,:),veg_frac,sf_diag,l_soil_point)
  END DO
END SELECT

!----------------------------------------------------------------------
! Form gridbox mean values
!----------------------------------------------------------------------

DO n = 1,ntype
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP             SHARED(frac, gs, gs_type, n, surft_index, surft_pts)         &
!$OMP             SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)
    gs(l) = gs(l) + frac(l,n) * gs_type(l,n)
  END DO
!$OMP END PARALLEL DO
END DO

!==============================================================================
! *NOTICE REGARDING SOIL TILING**
!
!The following section facilitates the use of soil tiling. As implemented,
!there are two soil tiling options:
!
!nsoilt == 1
!Operate as with a single soil tile, functionally identical to JULES upto
! at least vn4.7 (Oct 2016)
! This means that a soilt variable being passed 'up' to the surface is
! broadcast to the surft variable (with weighting by frac if requred)
!
!nsoilt > 1
!Operate with nsoilt = nsurft, with a direct mapping between them
! This means that a soilt variable being passed 'up' to the surface is simply
! copied into the surft variable
!
! This will need to be refactored for other tiling approaches. This note
! will be replicated elsewhere in the code as required
!
!These comments apply until **END NOTICE REGARDING SOIL TILING**
!==============================================================================

IF ( l_aggregate ) THEN
  !If we're using aggregation, there can only be 1 soil tile
  m = 1
  DO n = 1,ntype
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP             SHARED(canhc, ch_type, frac, n, sm_levels,                   &
!$OMP                    l_irrig_dmd, l_soil_evap_irrig_expl,                  &
!$OMP                    surft_index, surft_pts, vfrac, vf_type, wt_ext_soilt, &
!$OMP                    frac_irr_surft, frac_irr_soilt,                       &
!$OMP                    wt_ext_nir_soilt, wt_ext_nir_type,                    &
!$OMP                    wt_ext_irr_soilt, wt_ext_type, wt_ext_irr_type, m)    &
!$OMP             SCHEDULE(STATIC)
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      canhc(l) = canhc(l) + frac(l,n) * ch_type(l,n)
      vfrac(l) = vfrac(l) + frac(l,n) * vf_type(l,n)
      DO k = 1,sm_levels
        wt_ext_soilt(l,m,k) = wt_ext_soilt(l,m,k) + frac(l,n)                  &
                              * wt_ext_type(l,k,n)
        IF (l_irrig_dmd) THEN
          IF (frac_irr_soilt(l,m) > EPSILON(1.0)) THEN
            wt_ext_irr_soilt(l,m,k) = wt_ext_irr_soilt(l,m,k)                  &
                                    + frac_irr_surft(l,n)                      &
                                    / frac_irr_soilt(l,m)                      &
                                    * frac(l,n) * wt_ext_irr_type(l,k,n)
          END IF
          IF ( l_soil_evap_irrig_expl) THEN
            IF ((1.0-frac_irr_soilt(l,m)) > EPSILON(1.0)) THEN
              wt_ext_nir_soilt(l,m,k) = wt_ext_nir_soilt(l,m,k)                &
                                      + (1.0-frac_irr_surft(l,n))              &
                                      / (1.0-frac_irr_soilt(l,m))              &
                                      * frac(l,n) * wt_ext_nir_type(l,k,n)
            END IF
            wt_ext_soilt(l,m,k) = frac_irr_soilt(l,m)*wt_ext_irr_soilt(l,m,k)  &
                                + (1.0-frac_irr_soilt(l,m))                    &
                                * wt_ext_nir_soilt(l,m,k)

          END IF
        END IF
      END DO
    END DO
!$OMP END PARALLEL DO
  END DO
!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE) PRIVATE(k, l)                        &
!$OMP             SHARED(canhc, canhc_surft, flake, frac, gs, gc_surft, lake,  &
!$OMP                    land_pts, sm_levels, vfrac, vfrac_surft, l_do_omp,    &
!$OMP                    wt_ext_soilt, m, wt_ext_surft, non_irrig_frac)
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    IF (non_irrig_frac(l)  >   0.0) THEN
      DO k = 1,sm_levels
        wt_ext_soilt(l,m,k) = wt_ext_soilt(l,m,k) / non_irrig_frac(l)
      END DO
    END IF
    IF ( lake > 0 ) THEN
      flake(l,1) = frac(l,lake)
    ELSE
      flake(l,1) = 0.0
    END IF
    gc_surft(l,1) = 0.0
    IF (flake(l,1) <  1.0)                                                     &
      gc_surft(l,1) = gs(l) / (1.0 - flake(l,1))
    canhc_surft(l,1) = canhc(l)
    vfrac_surft(l,1) = vfrac(l)
  END DO
!$OMP END DO
  DO k = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      wt_ext_surft(l,k,1) = wt_ext_soilt(l,m,k)
    END DO
!$OMP END DO
  END DO
!$OMP END PARALLEL
ELSE
!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE) PRIVATE(k, l, n)                     &
!$OMP             SHARED(nsurft, land_pts, gc_surft, canhc_surft, vfrac_surft, &
!$OMP                    l_do_omp, l_irrig_dmd, gs_irr_surft)
  DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
    DO i =1, land_pts
      gc_surft(i,n)=0.0
      canhc_surft(i,n)=0.0
      vfrac_surft(i,n)=0.0
    END DO
!$OMP END DO NOWAIT
  END DO
  IF (l_irrig_dmd) THEN
    DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
      DO i =1, land_pts
        gs_irr_surft(i,n)=0.0 ! irrigation
      END DO
!$OMP END DO NOWAIT
    END DO
  END IF
!$OMP END PARALLEL

  DO n = 1,ntype

    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = n
    END IF !nsoilt

!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(k, l, j)          &
!$OMP SHARED(surft_pts, surft_index, flake, gc_surft, gs_type, l_irrig_dmd,    &
!$OMP        l_soil_evap_irrig_expl, gs_irr_surft, gs_irr_type,                &
!$OMP        canhc_surft, ch_type, vfrac_surft,                                &
!$OMP        vf_type, sm_levels, wt_ext_soilt, frac, wt_ext_type,              &
!$OMP        wt_ext_surft, frac_irr_surft, frac_irr_soilt,                     &
!$OMP        wt_ext_irr_soilt, wt_ext_irr_type, wt_ext_irr_surft,              &
!$OMP        wt_ext_nir_soilt, wt_ext_nir_type, gs_nir_surft, gc_irr_surft,    &
!$OMP        gs_nir_type, n, m, lake,                                          &
!$OMP        l_flake_model, non_lake_frac)                                     &
!$OMP             SCHEDULE(STATIC)
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      flake(l,n) = 0.0
      gc_surft(l,n) = gs_type(l,n)
      IF (l_irrig_dmd) THEN
        gs_irr_surft(l,n) = gs_irr_type(l,n) ! irrigation
        IF (l_soil_evap_irrig_expl) THEN
          gs_nir_surft(l,n) = gs_nir_type(l,n) ! non irrigation
          gc_irr_surft(l,n) = gs_irr_surft(l,n)
        END IF
      END IF
      canhc_surft(l,n) = ch_type(l,n)
      vfrac_surft(l,n) = vf_type(l,n)

      DO k = 1,sm_levels
        IF ( .NOT. ((l_flake_model) .AND. (n == lake)) ) THEN
          wt_ext_soilt(l,m,k) = wt_ext_soilt(l,m,k) + frac(l,n)                &
                                * wt_ext_type(l,k,n)
        END IF
        wt_ext_surft(l,k,n) = wt_ext_type(l,k,n)
        IF (l_irrig_dmd) THEN
          IF (frac_irr_soilt(l,m) > EPSILON(1.0)) THEN
            wt_ext_irr_soilt(l,m,k) = wt_ext_irr_soilt(l,m,k)                  &
                                      + frac_irr_surft(l,n)                    &
                                      / frac_irr_soilt(l,m)                    &
                                      * frac(l,n) * wt_ext_irr_type(l,k,n)
          END IF
          wt_ext_irr_surft(l,k,n) = wt_ext_irr_type(l,k,n)
          IF (l_soil_evap_irrig_expl) THEN
            IF ((1.0-frac_irr_soilt(l,m)) > EPSILON(1.0)) THEN
              wt_ext_nir_soilt(l,m,k) = wt_ext_nir_soilt(l,m,k)                &
                                      + (1.0-frac_irr_surft(l,n))              &
                                      / (1.0-frac_irr_soilt(l,m))              &
                                      * frac(l,n) * wt_ext_nir_type(l,k,n)
            END IF
            wt_ext_soilt(l,m,k) = frac_irr_soilt(l,m)*wt_ext_irr_soilt(l,m,k)  &
                                + (1.0-frac_irr_soilt(l,m))                    &
                                * wt_ext_nir_soilt(l,m,k)
          END IF
        END IF
      END DO !sm_levels
    END DO !surf_pts
!$OMP END PARALLEL DO
  END DO !ntype

  ! If using a shared soil, normalise wt_ext_soilt for non-irrigated surface
  ! tile fractions
  DO l = 1,land_pts
    IF ((non_irrig_frac(l) > EPSILON(0.0)) .AND. (nsoilt == 1)) THEN
      DO k = 1,sm_levels
        wt_ext_soilt(l,1,k) = wt_ext_soilt(l,1,k) / non_irrig_frac(l)
      END DO
    END IF
  END DO

  IF (l_flake_model) THEN
    ! Normalise wt_ext_soilt, excluding the lake tile fraction.
    ! Not required if tiled soils are being used.
    DO l = 1,land_pts
      IF ((non_lake_frac(l) > EPSILON(0.0)) .AND. (nsoilt == 1)) THEN
        DO k = 1,sm_levels
          wt_ext_soilt(l,1,k) = wt_ext_soilt(l,1,k) / non_lake_frac(l)
        END DO
      END IF
    END DO
  END IF

  IF ( lake > 0 ) THEN
    n = lake    ! Lake tile
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l, j)             &
!$OMP             SHARED(flake, n, surft_index, surft_pts) SCHEDULE(STATIC)
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      flake(l,n) = 1.0
    END DO
!$OMP END PARALLEL DO
  END IF
END IF

!==============================================================================
! *END NOTICE REGARDING SOIL TILING**
!==============================================================================

! If asteps since triffid is 1 - i.e. TRIFFID has just been called - zero
! gpp_pft_acc. The associated diagnostic is output via section 19, sampled on
! TRIFFID timesteps, so if gpp_pft_acc is not zeroed every time TRIFFID is
! called the gpp accumulated before TRIFFID calls earlier in the present
! run are counted multiple times.

IF (l_triffid) THEN
  IF (asteps_since_triffid == 1) THEN
    gpp_pft_acc(:,:) = 0.0
  END IF
END IF

DO n = 1,npft
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l,j)              &
!$OMP SHARED(surft_pts, surft_index, n, gpp, frac, gpp_pft, npp, npp_pft,      &
!$OMP        resp_p, resp_p_pft, apar_diag_gb, apar_diag_pft,                  &
!$OMP        growth_sug_gb, growth_sug_pft,                                    &
!$OMP        isoprene_gb, isoprene_pft, terpene_gb,                            &
!$OMP        terpene_pft, methanol_gb, methanol_pft, acetone_gb, acetone_pft,  &
!$OMP        gpp_pft_acc, timestep, l_triffid)                                 &
!$OMP             SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    l = surft_index(j,n)

    gpp(l)       = gpp(l) + frac(l,n) * gpp_pft(l,n)
    npp(l)       = npp(l) + frac(l,n) * npp_pft(l,n)
    resp_p(l)    = resp_p(l) + frac(l,n) * resp_p_pft(l,n)
    growth_sug_gb(l) = growth_sug_gb(l) + frac(l,n) * growth_sug_pft(l,n)

    apar_diag_gb(l) = apar_diag_gb(l) + frac(l,n) * apar_diag_pft(l,n)

    isoprene_gb(l) = isoprene_gb(l) + frac(l,n) * isoprene_pft(l,n)
    terpene_gb(l)  = terpene_gb(l)  + frac(l,n) * terpene_pft(l,n)
    methanol_gb(l) = methanol_gb(l) + frac(l,n) * methanol_pft(l,n)
    acetone_gb(l)  = acetone_gb(l)  + frac(l,n) * acetone_pft(l,n)

    ! Need to accumulate the gpp, calculated every JULES timestep, so that it can
    ! be used in conjunction with the N limited version of NPP to calculate the
    ! plant respiration after N limitation.

    IF (l_triffid) THEN
      gpp_pft_acc(l,n) = gpp_pft_acc(l,n) +  gpp_pft(l,n) * timestep
    END IF

  END DO
!$OMP END PARALLEL DO
END DO

! Calculate the GBM version of the accumulated GPP calculated above

IF (l_triffid) THEN
  gpp_gb_acc(:) = 0.0

  DO n = 1,npft
!$OMP PARALLEL DO IF(surft_pts(n) > 1) DEFAULT(NONE) PRIVATE(l,m)              &
!$OMP SHARED(surft_pts, surft_index, n, frac, gpp_gb_acc, gpp_pft_acc)         &
!$OMP             SCHEDULE(STATIC)
    DO m = 1,surft_pts(n)
      l = surft_index(m,n)
      gpp_gb_acc(l)    = gpp_gb_acc(l)    + frac(l,n) * gpp_pft_acc(l,n)
    END DO
!$OMP END PARALLEL DO
  END DO

END IF

! Water tracers - calculate ratio of water tracer to water in the soil
! ahead of diagnosing available water tracer in the soil profile
IF (l_wtrac_jls) THEN

  ALLOCATE(ratio_wt(land_pts,nsoilt,sm_levels,n_wtrac_jls))

!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE) PRIVATE(l,i_wt,m,k)                  &
!$OMP SHARED(ratio_wt, land_pts, n_wtrac_jls, nsoilt, sm_levels,               &
!$OMP        sthu_soilt, sthu_soilt_wtrac, l_do_omp)
  DO i_wt = 1, n_wtrac_jls
    DO m = 1, nsoilt
      DO k = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
        DO l = 1,land_pts
          ratio_wt(l,m,k,i_wt) = wtrac_calc_ratio_fn_jules(i_wt,               &
                                      sthu_soilt_wtrac(l,m,k,i_wt),            &
                                      sthu_soilt(l,m,k))
        END DO ! l
!$OMP END DO NOWAIT
      END DO ! k
    END DO ! m
  END DO ! i_wt
!$OMP END PARALLEL
END IF ! l_wtrac_jls

!----------------------------------------------------------------------
! Diagnose the available moisture in the soil profile
!----------------------------------------------------------------------
!AJM START
! Available water for plant transpiration
IF (l_use_pft_psi ) THEN
  DO n = 1,npft

    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = n
    END IF !nsoilt

    DO k = 1,sm_levels
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(dzsoil,         &
!$OMP             land_pts, k, n, m, smc_soilt, sthu_surft, smvcst_soilt,      &
!$OMP             v_close_pft, wt_ext_type, frac, l_do_omp)                    &
!$OMP             SCHEDULE(STATIC)
      DO l = 1,land_pts
        smc_soilt(l,m) = smc_soilt(l,m)                                        &
                        + MAX(0.0,                                             &
                              frac(l,n) * wt_ext_type(l,k,n)                   &
                              * rho_water * dzsoil(k)                          &
                              * (sthu_surft(l,m,k) * smvcst_soilt(l,m,k)       &
                                  - v_close_pft(l,k,n)))
      END DO
!$OMP END PARALLEL DO
    END DO
  END DO

  DO l = 1,land_pts
    IF ((non_irrig_frac(l) > EPSILON(0.0)) .AND. (nsoilt == 1)) THEN
      smc_soilt(l,m) = smc_soilt(l,m) / non_irrig_frac(l)
    END IF
  END DO

ELSE
  DO k = 1,sm_levels
    DO m = 1, nsoilt
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(dzsoil,         &
!$OMP             land_pts, k, n, m, smc_soilt, sthu_soilt, smvcst_soilt,      &
!$OMP             smvcwt_soilt, wt_ext_soilt, l_do_omp)                        &
!$OMP             SCHEDULE(STATIC)
      DO l = 1,land_pts
        smc_soilt(l,m) = smc_soilt(l,m)                                        &
                        + MAX(0.0,                                             &
                              wt_ext_soilt(l,m,k) * rho_water * dzsoil(k)      &
                              * (sthu_soilt(l,m,k) * smvcst_soilt(l,m,k)       &
                                  - smvcwt_soilt(l,m,k)))
      END DO
!$OMP END PARALLEL DO
    END DO
  END DO

  ! Water tracers - calculate water tracer content of water available for
  ! plant transpiration
  IF (l_wtrac_jls) THEN
!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE) PRIVATE(l,i_wt,m,k)                  &
!$OMP SHARED(dzsoil, land_pts, n_wtrac_jls, nsoilt, sm_levels, sthu_soilt,     &
!$OMP        smvcst_soilt, smvcwt_soilt, wt_ext_soilt, smc_soilt_wtrac,        &
!$OMP        ratio_wt, l_do_omp)
    DO i_wt = 1, n_wtrac_jls
      DO m = 1, nsoilt
        DO k = 1, sm_levels
!$OMP DO SCHEDULE(STATIC)
          DO l = 1,land_pts
            smc_soilt_wtrac(l,m,i_wt) = smc_soilt_wtrac(l,m,i_wt)              &
                 + ratio_wt(l,m,k,i_wt) * MAX(0.0,                             &
                                  wt_ext_soilt(l,m,k) * rho_water * dzsoil(k)  &
                                  * ( sthu_soilt(l,m,k) * smvcst_soilt(l,m,k)  &
                                     - smvcwt_soilt(l,m,k) ))
          END DO  ! l
!$OMP END DO NOWAIT
        END DO  ! k
      END DO ! m
    END DO ! i_wt
!$OMP END PARALLEL

  END IF ! l_wtrac_jls

END IF ! l_use_pft_psi


IF (sf_diag%l_wt_ext) THEN
  sf_diag%wt_ext(:,:) = wt_ext_soilt(:,1,:)
END IF

DO m = 1, nsoilt
  ! Add available water for evaporation from bare soil
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(dzsoil,         &
!$OMP  l_do_omp,  fsoil_tot, land_pts, smc_soilt, sthu_soilt, smvcst_soilt, m) &
!$OMP             SCHEDULE(STATIC)
  DO l = 1,land_pts
    smc_soilt(l,m) = (1.0 - fsoil_tot(l)) * smc_soilt(l,m) +                   &
                     fsoil_tot(l) * rho_water * dzsoil(1) *                    &
                     MAX(0.0,sthu_soilt(l,m,1)) * smvcst_soilt(l,m,1)
  END DO
!$OMP END PARALLEL DO
END DO

! Water tracers - add available water tracer for evaporation from bare soil

IF (l_wtrac_jls) THEN
!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE) PRIVATE(l,i_wt,m)                    &
!$OMP SHARED(dzsoil, fsoil_tot, land_pts, sthu_soilt, smvcst_soilt, nsoilt,    &
!$OMP        n_wtrac_jls, smc_soilt_wtrac, ratio_wt, l_do_omp)

  DO i_wt = 1, n_wtrac_jls
    DO m = 1, nsoilt
!$OMP DO SCHEDULE(STATIC)
      DO l = 1,land_pts
        smc_soilt_wtrac(l,m,i_wt) =                                            &
                     ! From plants
                     (1.0 - fsoil_tot(l)) * smc_soilt_wtrac(l,m,i_wt)          &
                     ! From bare soil
                     + ratio_wt(l,m,1,i_wt) *                                  &
                     fsoil_tot(l) * rho_water * dzsoil(1) *                    &
                     MAX(0.0,sthu_soilt(l,m,1)) * smvcst_soilt(l,m,1)
      END DO ! l
!$OMP END DO NOWAIT
    END DO ! m
  END DO ! i_wt
!$OMP END PARALLEL

  ! Deallocate water tracer working array
  DEALLOCATE(ratio_wt)

END IF ! l_wtrac_jls

IF (l_irrig_dmd) THEN
  IF (l_use_pft_psi ) THEN
    DO n = 1,npft

      !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        m = 1
      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        m = n
      END IF !nsoilt

      DO k = 1,sm_levels
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(dzsoil,         &
!$OMP             land_pts, k, n, m, smc_irr_soilt, sthu_irr_soilt,            &
!$OMP             smc_nir_soilt, sthu_nir_soilt, frac, frac_irr_surft,         &
!$OMP             frac_irr_soilt, l_do_omp,smvcst_soilt, v_close_pft,          &
!$OMP             l_soil_evap_irrig_expl, wt_ext_irr_type, wt_ext_nir_type)    &
!$OMP             SCHEDULE(STATIC)
        DO l = 1,land_pts
          IF ( frac_irr_soilt(l,m) > EPSILON(1.0) ) THEN
            smc_irr_soilt(l,m) = smc_irr_soilt(l,m)                            &
                           + MAX(0.0,                                          &
                                 frac_irr_surft(l,n) / frac_irr_soilt(l,m)     &
                                 * frac(l,n) * wt_ext_irr_type(l,k,n)          &
                                 * rho_water * dzsoil(k)                       &
                                 * (sthu_irr_soilt(l,m,k)                      &
                                 * smvcst_soilt(l,m,k)                         &
                                 - v_close_pft(l,k,n)))
          END IF
          IF (l_soil_evap_irrig_expl) THEN
            IF (1.0 - frac_irr_soilt(l,m) > EPSILON(1.0) ) THEN
              smc_nir_soilt(l,m) = smc_nir_soilt(l,m)                          &
                             + MAX(0.0,(1.0-frac_irr_surft(l,n))               &
                                   / (1.0-frac_irr_soilt(l,m))                 &
                                   * frac(l,n) * wt_ext_nir_type(l,k,n)        &
                                   * rho_water * dzsoil(k)                     &
                                   * (sthu_nir_soilt(l,m,k)                    &
                                   * smvcst_soilt(l,m,k)                       &
                                   - v_close_pft(l,k,n)))
            END IF
          END IF
        END DO
!$OMP END PARALLEL DO
      END DO
    END DO
  ELSE
    ! Available water for plant transpiration in irrigated fraction.
    DO k = 1,sm_levels
      DO m = 1,nsoilt
!$OMP PARALLEL DO IF(l_do_omp) DEFAULT(NONE) PRIVATE(l) SHARED(dzsoil,         &
!$OMP             land_pts, k, n, smc_irr_soilt, sthu_irr_soilt,               &
!$OMP             l_soil_evap_irrig_expl,                                      &
!$OMP             smc_nir_soilt, sthu_nir_soilt,smvcst_soilt,                  &
!$OMP             smvcwt_soilt,wt_ext_irr_soilt,wt_ext_nir_soilt,m,l_do_omp)   &
!$OMP             SCHEDULE(STATIC)
        DO l = 1,land_pts
          smc_irr_soilt(l,m) = smc_irr_soilt(l,m)                              &
                              + MAX(0.0 ,                                      &
                                    wt_ext_irr_soilt(l,m,k) * rho_water        &
                                    * dzsoil(k)                                &
                                    * (sthu_irr_soilt(l,m,k)                   &
                                        * smvcst_soilt(l,m,k)                  &
                                        - smvcwt_soilt(l,m,k)))
          IF (l_soil_evap_irrig_expl) THEN
            smc_nir_soilt(l,m) = smc_nir_soilt(l,m)                            &
                               + MAX(0.0 ,                                     &
                                      wt_ext_nir_soilt(l,m,k) * rho_water      &
                                      * dzsoil(k)                              &
                                      * (sthu_nir_soilt(l,m,k)                 &
                                          * smvcst_soilt(l,m,k)                &
                                          - smvcwt_soilt(l,m,k)))
          END IF
        END DO
!$OMP END PARALLEL DO
      END DO
    END DO
  END IF

  ! Add available water for evaporation from bare soil in irrig frac.
!$OMP PARALLEL IF(l_do_omp) DEFAULT(NONE) PRIVATE(l,m,n) SHARED(dzsoil,        &
!$OMP             fsoil_tot, land_pts, smc_irr_soilt, sthu_irr_soilt, nsoilt,  &
!$OMP             smc_nir_soilt, smc_soilt, sthu_nir_soilt,                    &
!$OMP             smvcst_soilt, gs_irr_surft, gc_irr_surft, nsurft, l_do_omp,  &
!$OMP             fsoil_irr_tot, fsoil_nir_tot, frac_irr_soilt,                &
!$OMP             fsoil_irr_tot_tmp, fsoil_nir_tot_tmp, frac_irr_surft,        &
!$OMP             l_soil_evap_irrig_expl)
  DO m = 1,nsoilt
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      IF (l_soil_evap_irrig_expl) THEN
        IF (frac_irr_soilt(l,m) > 0.0) THEN
          fsoil_irr_tot_tmp = fsoil_irr_tot(l)/frac_irr_soilt(l,m)
          smc_irr_soilt(l,m) = (1.0 - fsoil_irr_tot_tmp ) *                    &
             smc_irr_soilt(l,m) + fsoil_irr_tot_tmp *                          &
             rho_water * dzsoil(1) *                                           &
             MAX(0.0,sthu_irr_soilt(l,m,1)) * smvcst_soilt(l,m,1)
        ELSE
          smc_irr_soilt(l,m) = 0.0
        END IF
        IF (1.0 - frac_irr_soilt(l,m) > 0.0) THEN
          fsoil_nir_tot_tmp = fsoil_nir_tot(l)/(1.0-frac_irr_soilt(l,m))
          smc_nir_soilt(l,m) = (1.0 - fsoil_nir_tot_tmp) *                     &
               smc_nir_soilt(l,m) + fsoil_nir_tot_tmp *                        &
               rho_water * dzsoil(1) *                                         &
              MAX(0.0,sthu_nir_soilt(l,m,1)) * smvcst_soilt(l,m,1)
        ELSE
          smc_nir_soilt(l,m) = 0.0
        END IF

        smc_soilt(l,m) = frac_irr_soilt(l,m) * smc_irr_soilt(l,m) +            &
            (1.0 - frac_irr_soilt(l,m)) * smc_nir_soilt(l,m)
      ELSE
        smc_irr_soilt(l,m) = (1.0 - fsoil_tot(l)) * smc_irr_soilt(l,m) +       &
                          fsoil_tot(l) * rho_water * dzsoil(1) *               &
                          MAX(0.0,sthu_irr_soilt(l,m,1)) * smvcst_soilt(l,m,1)
      END IF
    END DO
!$OMP END DO
  END DO

  DO n =1, nsurft
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      gc_irr_surft(l,n)=gs_irr_surft(l,n)
    END DO
!$OMP END DO NOWAIT
  END DO
!$OMP END PARALLEL

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE physiol
END MODULE physiol_mod
