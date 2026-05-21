MODULE surf_couple_extra_mod
! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE

PUBLIC :: surf_couple_extra

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SURF_COUPLE_EXTRA_MOD'

CONTAINS

!===============================================================================
!Note that the commented intents may not be correct. The declarations section
!will be more correct

!Routine Layout **PLEASE READ**

!-Header (please follow ordering guidance)
!-Type correction for timestep
!-SELECT for LSM
!--JULES
!---Redimensioning of arrays (Note 1)
!---Compression to land points (Note 2)
!---Science calls (Note 3)
!---Expansion to full grid
!---UM Diagnostics calls
!--CABLE
!---Details to follow
!-END

!Note 1- This is for any variable that might have differnt numbers of
!        dimensions in standalone vs UM mode. These generally arise due to the
!        differing science payloads.

!Note 2- This is where all gridded variables (with _ij suffix) are compressed
!        onto land points. This allows efficient use of OpenMP

!Note 3- New additions here should ideally consist ONLY of calls to other
!        routines. Please encapsulate longer code within a subroutine.

!===============================================================================
SUBROUTINE surf_couple_extra(                                                  &
   ! Arguments used by JULES-standalone
   u_1_ij, v_1_ij,                                                             &
   !Misc INTENT(IN)
   a_step, smlt, tile_frac, hcons_soilt, rhostar,                              &
   !Arguments for the UM-----------------------------------------
   !IN
   land_pts, row_length, rows, river_row_length, river_rows,                   &
   ls_graup_ij,                                                                &
   cca_2d, nsurft, surft_pts,                                                  &
   lice_pts, soil_pts,                                                         &
   stf_sub_surf_roff,                                                          &
   fexp_soilt, gamtot_soilt, ti_mean_soilt, ti_sig_soilt, ti_local_soilt,      &
   flash_rate_ancil, pop_den_ancil, wealth_index_ancil,                        &
   a_fsat_soilt, c_fsat_soilt, a_fwet_soilt, c_fwet_soilt,                     &
   ntype,                                                                      &
   delta_lambda, delta_phi, xx_cos_theta_latitude,                             &
   aocpl_row_length, aocpl_p_rows, xpa, xua, xva, ypa, yua, yva,               &
   g_p_field, g_r_field, n_proc, global_row_length, global_rows,               &
   global_river_row_length, global_river_rows, flandg,                         &
   trivdir, trivseq, r_area, slope, flowobs1, r_inext, r_jnext, r_land,        &
   frac_agr_gb, soil_clay_ij,npp_gb,                                           &
   u_s_std_surft,                                                              &
   !INOUT
   a_steps_since_riv,                                                          &
   fsat_soilt, fwetl_soilt,                                                    &
   zw_soilt, sthzw_soilt,                                                      &
   ls_rainfrac_gb,                                                             &
   substore, surfstore, flowin, bflowin,                                       &
   tot_surf_runoff_gb, tot_sub_runoff_gb, acc_lake_evap_gb, twatstor,          &
   asteps_since_triffid,                                                       &
   inlandout_atm_gb,                                                           &
   !OUT
   dhf_surf_minus_soil,                                                        &
   land_sea_mask,                                                              &
   !TYPES containing field data (IN OUT)
   crop_vars,psparms,toppdm,fire_vars,ainfo,trif_vars,soilecosse,urban_param,  &
   progs,trifctltype,coast,jules_vars,                                         &
   fluxes,                                                                     &
   lake_vars,                                                                  &
   forcing,                                                                    &
   rivers,                                                                     &
   chemvars, water_resources,                                                  &
   wtrac_jls,                                                                  &
   work_cbl                                                                    &
  )

!Module imports

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type
USE p_s_parms, ONLY: psparms_type
USE top_pdm, ONLY: top_pdm_type
USE fire_vars_mod, ONLY: fire_vars_type
USE ancil_info,    ONLY: ainfo_type
USE trif_vars_mod, ONLY: trif_vars_type
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type
USE urban_param_mod, ONLY:urban_param_type
USE prognostics, ONLY: progs_type
USE coastal, ONLY: coastal_type
USE trifctl, ONLY: trifctl_type
USE jules_vars_mod, ONLY: jules_vars_type
USE fluxes_mod, ONLY: fluxes_type
USE lake_mod, ONLY: lake_type
USE jules_forcing_mod, ONLY: forcing_type
USE jules_rivers_mod, ONLY: rivers_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
USE jules_chemvars_mod, ONLY: chemvars_type
USE water_resources_vars_mod, ONLY: water_resources_type
USE jules_wtrac_type_mod, ONLY: jls_wtrac_type

! In general CABLE utilizes a required subset of tbe JULES types, however;
USE work_vars_mod_cbl,  ONLY: work_vars_type      ! and some kept thru timestep

!Import interfaces to subroutines called
USE hydrol_mod,               ONLY: hydrol
USE snow_mod,                 ONLY: snow
USE jules_rivers_mod,         ONLY: l_rivers, l_inland, rivers_call

! Code which isn't currently suitable for building into LFRic
#if !defined(LFRIC)
USE surf_couple_rivers_mod,   ONLY: surf_couple_rivers
USE inferno_mod,              ONLY: calc_soil_carbon_pools
USE inferno_io_mod,           ONLY: inferno_io
USE irrigation_mod,           ONLY: irrigation_control
USE veg_control_mod,          ONLY: veg_control
USE next_gen_biogeochem_mod,  ONLY: next_gen_biogeochem
USE sparm_mod,                ONLY: sparm
USE infiltration_rate_mod,    ONLY: infiltration_rate
USE flake_interface_mod,      ONLY: flake_interface

!ifdef required to manage the different science payloads available for
!coupled and standalone use
#if defined(UM_JULES)
USE diagnostics_riv_mod,      ONLY: diagnostics_riv
USE diagnostics_hyd_mod,      ONLY: diagnostics_hyd
USE diagnostics_veg_mod,      ONLY: diagnostics_veg
#else
USE crop_mod,                 ONLY: crop
USE fire_timestep_mod,        ONLY: fire_timestep
USE metstats_timestep_mod,    ONLY: metstats_timestep
USE gridbox_mean_mod,         ONLY: soiltiles_to_gbm, surftiles_to_gbm
USE fao_evapotranspiration,   ONLY: fao_ref_evapotranspiration
USE water_resources_control_mod, ONLY: water_resources_control
USE zenith_mod,               ONLY: photoperiod
! End of standalone Jules code
#endif

! End of code excluded from LFRic builds
#else
! Code only used for LFRic
USE missing_data_mod, ONLY: rmdi
#endif

!Variables- The rules here are:
!Each module is only USED once, using an ifdef to control the variables if req.
!Whole modules are grouped into a single ifdef/else
!Alphabetical order

!Modules common to JULES and UM

USE ancil_info,               ONLY:                                            &
#if !defined(UM_JULES)
                                    dim_cs1,                                   &
#endif
                                    dim_cslayer, nsoilt, nmasst,               &
                                  ! Replaces USE statement
                                    dim_soil_n_pool

USE atm_fields_bounds_mod,    ONLY: tdims, tdims_s, pdims_s

USE conversions_mod,          ONLY: rsec_per_day

! Used for atmospheric deposition
USE jules_deposition_mod,     ONLY: l_deposition, l_deposition_from_ukca

USE deposition_from_surf_couple_extra_mod,                                     &
                              ONLY: deposition_from_surf_couple_extra

USE jules_hydrology_mod,      ONLY: l_hydrology, l_pdm, l_top, l_var_rainfrac

USE lake_mod, ONLY:    h_snow_sw_att                                           &
                     , trap_frozen                                             &
                     , trap_unfrozen

USE jules_snow_mod,      ONLY: nsmax

USE jules_soil_biogeochem_mod, ONLY:                                           &
  soil_model_4pool, soil_bgc_model, dim_ch4subgrid

USE jules_soil_mod,           ONLY: sm_levels, l_soil_sat_down, confrac

USE jules_surface_mod,        ONLY: l_aggregate, l_flake_model

USE jules_surface_types_mod,  ONLY: npft, ncpft, nnpft, lake

USE jules_vegetation_mod,     ONLY:                                            &
#if !defined(UM_JULES)
  l_crop, l_fao_ref_evapotranspiration,                                        &
#endif
  l_triffid, l_trif_eq, l_phenol, phenol_period, triffid_period,               &
  l_inferno, ignition_method,                                                  &
  ignition_vary_natural, ignition_vary_natural_human,                          &
  l_acclim, n_day_photo_acclim, l_red

USE jules_irrig_mod, ONLY: l_irrig_dmd

USE jules_water_tracers_mod,  ONLY: n_wtrac_jls

USE jules_water_resources_mod, ONLY:                                           &
  l_water_resources, sw_river_source

USE jules_water_tracers_mod,  ONLY: l_wtrac_jls
USE wtrac_extra_mod,          ONLY: wtrac_alloc_extra, wtrac_dealloc_extra,    &
                                    wtrac_ex_type
USE wtrac_checks_jls_mod,     ONLY: wtrac_checks_sno, wtrac_checks_hyd,        &
                                    wtrac_checks_grid
USE wtrac_correct_jls_mod,    ONLY: wtrac_correct_sno, wtrac_correct_hyd,      &
                                    wtrac_correct_grid

USE sf_diags_mod,             ONLY: sf_diag

USE timestep_mod,             ONLY: timestep

USE veg3_parm_mod,            ONLY: veg3_ctrl,litter_parms,red_parms

USE veg3_field_mod,           ONLY: veg_state,red_state

USE water_constants_mod,      ONLY: rho_water

USE ereport_mod,              ONLY: ereport

!Modules specific to the UM and not needed in LFRic
#if defined(UM_JULES) && !defined(LFRIC)
USE atm_fields_mod,      ONLY: disturb_veg_prev

USE atm_step_local,           ONLY:  STASHwork19, STASHwork8, STASHwork26

USE model_domain_mod,         ONLY: model_type, mt_single_column

USE stash_array_mod,          ONLY: sf
#endif
! Modules used by both UM and LFRic
#if defined(UM_JULES)
USE atm_step_local,           ONLY: dim_cs1
#else
!Modules specific to JULES
USE fire_mod,                 ONLY: fire_prog, fire_diag, l_fire

USE metstats_mod,             ONLY: metstats_prog, l_metstats

USE model_grid_mod,           ONLY: grid_area_ij

USE model_time_mod,           ONLY: current_time
#endif

#if !defined(UM_JULES)
USE theta_field_sizes,        ONLY: t_i_length, t_j_length
#endif

!Technical modules
USE parkind1,                    ONLY: jprb, jpim
USE yomhook,                     ONLY: lhook, dr_hook
USE ereport_mod,                 ONLY: ereport
USE jules_print_mgr,             ONLY: jules_message, jules_print
USE jules_model_environment_mod, ONLY: lsm_id, jules, cable


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Private subroutine for accessing the JULES science routines that are called
!   after the implicit code
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!Subroutine Arguments, ordered by intent and type

INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
  row_length,                                                                  &
  rows,                                                                        &
  river_row_length,                                                            &
  river_rows,                                                                  &
  nsurft,                                                                      &
  a_step,                                                                      &
  surft_pts(nsurft),                                                           &
  lice_pts,                                                                    &
  soil_pts,                                                                    &
  ntype,                                                                       &
  aocpl_row_length,                                                            &
  aocpl_p_rows,                                                                &
  g_p_field,                                                                   &
  g_r_field,                                                                   &
  n_proc,                                                                      &
  global_row_length,                                                           &
  global_rows,                                                                 &
  global_river_row_length,                                                     &
  global_river_rows

LOGICAL, INTENT(IN) ::                                                         &
  smlt,                                                                        &
  stf_sub_surf_roff,                                                           &
  land_sea_mask(row_length, rows)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  ls_graup_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),            &
  u_1_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                 &
  v_1_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                 &
  tile_frac(land_pts,nsurft),                                                  &
  rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                &
       !Surface air density
  cca_2d(row_length,rows),                                                     &
  fexp_soilt(land_pts,nsoilt),                                                 &
  gamtot_soilt(land_pts,nsoilt),                                               &
  ti_mean_soilt(land_pts,nsoilt),                                              &
  ti_sig_soilt(land_pts,nsoilt),                                               &
  ti_local_soilt(land_pts,nsoilt,dim_ch4subgrid),                              &
  npp_gb(land_pts),                                                            &
  a_fsat_soilt(land_pts,nsoilt),                                               &
  c_fsat_soilt(land_pts,nsoilt),                                               &
  a_fwet_soilt(land_pts,nsoilt),                                               &
  c_fwet_soilt(land_pts,nsoilt),                                               &
  frac_agr_gb(land_pts),                                                       &
  soil_clay_ij(row_length,rows),                                               &
  flash_rate_ancil(row_length,rows),                                           &
  pop_den_ancil(row_length,rows),                                              &
  wealth_index_ancil(row_length,rows),                                         &
  u_s_std_surft(land_pts, nsurft),                                             &

  !River routing
  delta_lambda,                                                                &
  delta_phi,                                                                   &
  xx_cos_theta_latitude(tdims_s%i_start:tdims_s%i_end,                         &
                        tdims_s%j_start:tdims_s%j_end),                        &
  xpa(aocpl_row_length+1),                                                     &
  xua(0:aocpl_row_length),                                                     &
  xva(aocpl_row_length+1),                                                     &
  ypa(aocpl_p_rows),                                                           &
  yua(aocpl_p_rows),                                                           &
  yva(0:aocpl_p_rows),                                                         &
  flandg(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end),         &
  trivdir(river_row_length, river_rows),                                       &
  trivseq(river_row_length, river_rows),                                       &
  r_area(row_length, rows),                                                    &
  slope(row_length, rows),                                                     &
  flowobs1(row_length, rows),                                                  &
  r_inext(row_length, rows),                                                   &
  r_jnext(row_length, rows),                                                   &
  r_land(row_length, rows)

INTEGER, INTENT(IN OUT)  ::                                                    &
  a_steps_since_riv,                                                           &
  asteps_since_triffid

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  hcons_soilt(land_pts,nsoilt),                                                &
  dhf_surf_minus_soil(land_pts),                                               &
        ! Heat flux difference across the FLake snowpack (W/m2)
  ls_rainfrac_gb(land_pts),                                                    &
  fsat_soilt(land_pts,nsoilt),                                                 &
  fwetl_soilt(land_pts,nsoilt),                                                &
  zw_soilt(land_pts,nsoilt),                                                   &
  sthzw_soilt(land_pts,nsoilt),                                                &
  substore(row_length, rows),                                                  &
  surfstore(row_length, rows),                                                 &
  flowin(row_length, rows),                                                    &
  bflowin(row_length, rows),                                                   &
  acc_lake_evap_gb(row_length,rows),                                           &
  tot_surf_runoff_gb(land_pts),                                                &
  tot_sub_runoff_gb(land_pts),                                                 &
  twatstor(river_row_length, river_rows),                                      &
  inlandout_atm_gb(land_pts)

!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(top_pdm_type), INTENT(IN OUT) :: toppdm
TYPE(fire_vars_type), INTENT(IN OUT) :: fire_vars
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype
TYPE(coastal_type), INTENT(IN OUT) :: coast
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
TYPE(lake_type), INTENT(IN OUT) :: lake_vars
TYPE(forcing_type), INTENT(IN OUT) :: forcing
TYPE(rivers_type), INTENT(IN OUT) :: rivers
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field
TYPE(chemvars_type), INTENT(IN OUT) :: chemvars
TYPE(water_resources_type), INTENT(IN OUT) :: water_resources
TYPE(jls_wtrac_type), INTENT(IN OUT) :: wtrac_jls

!CABLE TYPES containing field data (IN OUT)
TYPE(work_vars_type), INTENT(OUT) :: work_cbl

!==============================================================================
!Local variables

INTEGER ::                                                                     &
   i,j,k,l,n,m,i_wt,                                                           &
        !Various counters
  errcode
        ! error code to pass to ereport.

#if !defined(UM_JULES)
INTEGER ::  crop_call
        !indicates whether crop model is to be called
#endif

INTEGER, PARAMETER :: crop_period = 1
        ! Crop code hard wired to run daily : crop_period = 1

REAL(KIND=real_jlslsm) ::                                                      &
  !Gridbox versions of forcing data (ls_rainfrac_gb is an argument)
  con_rainfrac_gb(land_pts),                                                   &
  ls_rain_gb(land_pts),                                                        &
  con_rain_gb(land_pts),                                                       &
  ls_snow_gb(land_pts),                                                        &
  ls_graup_gb(land_pts),                                                       &
  con_snow_gb(land_pts),                                                       &
  pstar_gb(land_pts),                                                          &
  tl_1_gb(land_pts),                                                           &
  qw_1_gb(land_pts),                                                           &
  u_1_gb(land_pts),                                                            &
  v_1_gb(land_pts),                                                            &

  !Passed between different science schemes (various)
  qbase_l_soilt(land_pts,nsoilt,sm_levels+1),                                  &
        ! Base flow from each soil layer (kg m-2 s-1).
  w_flux_soilt(land_pts,nsoilt,0:sm_levels),                                   &
        ! Fluxes of water between layers (kg m-2 s-1).
  surf_ht_flux_ld(land_pts),                                                   &
  dwsw_sub_snow,                                                               &
        !remaining SW flux under the snowpack (W/m2), for FLake with ML-snow.

  !Passed between fire & INFERNO routines only
  c_soil_dpm_gb(land_pts),                                                     &
         ! Gridbox soil C in the Decomposable Plant Material pool (kg m-2).
  c_soil_rpm_gb(land_pts),                                                     &
         ! Gridbox soil C in the Resistant Plant Material pool (kg m-2).
#if !defined(UM_JULES)
  smc_gb(land_pts),                                                            &
         !To allow GBM soil moisture to be passed down to fire.

  !Passed between evapotranspiration routines only
  trad(land_pts),                                                              &
         ! gridbox effective radiative temperature (assuming emissivity=1)
#endif

  !Passed from snow or hydrol to diagnostics_hyd only
  snow_mass_gb(land_pts),                                                      &

  !Passed between river routing and diagnostics_riv only
  riverout(row_length, rows),                                                  &
  riverout_rgrid(river_row_length, river_rows),                                &
  box_outflow(river_row_length, river_rows),                                   &
  box_inflow(river_row_length, river_rows),                                    &
  inlandout_riv(river_row_length,river_rows)

! Smoothing factor of exponential filter used with temperature acclimation.
REAL(KIND=real_jlslsm) :: alpha_acclim

! Water tracer local arrays
TYPE(wtrac_ex_type) :: wtrac_ex

! Compressed water tracer rain and snow fields on land points only
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  con_snow_wtrac(:,:),                                                         &
  con_rain_wtrac(:,:),                                                         &
  ls_rain_wtrac(:,:),                                                          &
  ls_snow_wtrac(:,:),                                                          &
  ls_graup_wtrac(:,:)

INTEGER                      :: errorstatus
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'SURF_COUPLE_EXTRA'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!===============================================================================
!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!CABLE_LSM: implement switching based on lsm_id
SELECT CASE( lsm_id )

  !=============================================================================
CASE ( jules )

  !=============================================================================
  ! Redimensioning of arrays

#if defined(UM_JULES)
  !Dimensionality of variables differ betweeen UM and standalone, so copy across


  ! Change 2d to 1d soil clay content for soil respiration- dimensionalities
  ! differ between UM and standalone.
  ! Soil tiling not currently in the UM, so broadcast ij value to all tiles.
  ! Multi-layer clay not currently in UM so set all layers to same value.
  IF ( soil_bgc_model == soil_model_4pool ) THEN
    m = 1
    DO l = 1, land_pts
      j = (ainfo%land_index(l) - 1) / row_length + 1
      i = ainfo%land_index(l) - (j-1) * row_length
      DO n = 1, dim_cslayer
        psparms%clay_soilt(l,m,n) = soil_clay_ij(i,j)
      END DO
    END DO
  END IF

  ! Compress pop_den and flash rate fields to land points if required.
  ! In the UM the ancils are 2D fields.
  IF (l_inferno) THEN
    IF (ignition_method == ignition_vary_natural) THEN
      DO l = 1, land_pts
        j = (ainfo%land_index(l) - 1) / row_length + 1
        i = ainfo%land_index(l) - (j-1) * row_length
        fire_vars%flash_rate(l) = flash_rate_ancil(i,j)
      END DO
    END IF
    IF (ignition_method == ignition_vary_natural_human) THEN
      DO l = 1, land_pts
        j = (ainfo%land_index(l) - 1) / row_length + 1
        i = ainfo%land_index(l) - (j-1) * row_length
        fire_vars%flash_rate(l) = flash_rate_ancil(i,j)
        fire_vars%pop_den(l)    = pop_den_ancil(i,j)
        fire_vars%wealth_index(l) = wealth_index_ancil(i,j)
      END DO
    END IF
  END IF
#endif

  ! Set up water tracer working arrays
  CALL wtrac_alloc_extra(land_pts, nsurft, n_wtrac_jls, wtrac_ex)

  !Encompassing IF statement for land_pts > 0. We exit/re-enter this IF about
  !half way down to allow for river routing

  IF (l_hydrology .AND. land_pts > 0 ) THEN

    !===========================================================================
    ! Compression to land points

    DO l = 1, land_pts
      j = (ainfo%land_index(l) - 1) / row_length + 1
      i = ainfo%land_index(l) - (j-1) * row_length
      ls_rain_gb(l)    = forcing%ls_rain_ij(i,j)
      con_rain_gb(l)   = forcing%con_rain_ij(i,j)
      con_snow_gb(l)   = forcing%con_snow_ij(i,j)
      ls_snow_gb(l)    = forcing%ls_snow_ij(i,j)
      ls_graup_gb(l)   = ls_graup_ij(i,j)
      pstar_gb(l)      = forcing%pstar_ij(i,j)
      tl_1_gb(l)       = forcing%tl_1_ij(i,j)
      qw_1_gb(l)       = forcing%qw_1_ij(i,j)
      u_1_gb(l)        = u_1_ij(i,j)
      v_1_gb(l)        = v_1_ij(i,j)
    END DO

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      ALLOCATE(con_snow_wtrac(land_pts,n_wtrac_jls))
      ALLOCATE(con_rain_wtrac(land_pts,n_wtrac_jls))
      ALLOCATE(ls_snow_wtrac(land_pts,n_wtrac_jls))
      ALLOCATE(ls_rain_wtrac(land_pts,n_wtrac_jls))
      ALLOCATE(ls_graup_wtrac(land_pts,n_wtrac_jls))
      DO i_wt = 1, n_wtrac_jls
        DO l = 1, land_pts
          j = (ainfo%land_index(l) - 1) / row_length + 1
          i = ainfo%land_index(l) - (j-1) * row_length
          con_snow_wtrac(l,i_wt) = wtrac_jls%con_snow_ij(i,j,i_wt)
          con_rain_wtrac(l,i_wt) = wtrac_jls%con_rain_ij(i,j,i_wt)
          ls_rain_wtrac(l,i_wt)  = wtrac_jls%ls_rain_ij(i,j,i_wt)
          ls_snow_wtrac(l,i_wt)  = wtrac_jls%ls_snow_ij(i,j,i_wt)
          ls_graup_wtrac(l,i_wt) = 0.0
        END DO
      END DO
    ELSE
      ALLOCATE(con_snow_wtrac(1,1))
      ALLOCATE(con_rain_wtrac(1,1))
      ALLOCATE(ls_snow_wtrac(1,1))
      ALLOCATE(ls_rain_wtrac(1,1))
      ALLOCATE(ls_graup_wtrac(1,1))
    END IF

    !Pass jules the modelled rain fractions
    !In standalone mode, both rainfracs are passed in as zeroed arrays
    IF (l_var_rainfrac) THEN
      DO l = 1, land_pts
        j = (ainfo%land_index(l) - 1) / row_length + 1
        i = ainfo%land_index(l) - (j-1) * row_length

        con_rainfrac_gb(l) = MIN(cca_2d(i,j),0.5) !As in diagnostics_conv

        !provide some safety checking for convective rain with no CCA
        IF (con_rain_gb(l) > 0.0 ) THEN
          IF (con_rainfrac_gb(l) == 0.0) THEN
            con_rainfrac_gb(l) = confrac
            !and for very small CCA amounts
          ELSE IF (con_rainfrac_gb(l) < 0.01) THEN
            con_rainfrac_gb(l) = 0.01
          END IF
        END IF

        !provide some safety checking for ls rain with no rainfrac
        IF (ls_rain_gb(l) > 0.0) THEN
          IF (ls_rainfrac_gb(l) == 0.0) THEN
            ls_rainfrac_gb(l) = 0.5
            !and for very small rainfrac amounts
          ELSE IF (ls_rainfrac_gb(l) < 0.01) THEN
            ls_rainfrac_gb(l) = 0.01
          END IF
        END IF
      END DO

    ELSE !use original default values
      DO l = 1, land_pts
        con_rainfrac_gb(l) = confrac
        ls_rainfrac_gb(l)  = 1.0
      END DO
    END IF !l_var_rainfrac

    !===========================================================================
    ! Science calls

    !---------------------------------------------------------------------------
    ! Calculation of atmospheric deposition parameters and fluxes
    !---------------------------------------------------------------------------

    ! For JULES standalone applications, the JULES-based atmospheric deposition
    ! routines are called here, by setting the switches l_deposition to true
    ! and l_deposition_from_ukca to false.

    ! For coupled UM_JULES applications, the JULES-based atmospheric deposition
    ! routines can only be called from the UKCA routine ukca_chemistry_ctl
    ! (or its equivalents, ukca_chemistry_ctl_BE and ukca_chemistry_ctl_col),
    ! setting the switch l_deposition_from_ukca to true. This is to demonstrate
    ! that the JULES-based routines give outputs that bit compare with
    ! UKCA known good outputs.

    ! The longer-term plan is for the UKCA to provide surface atmospheric
    ! concentrations of the deposited species with JULES returning deposition
    ! fluxes, with the call routed through here.

    IF (l_deposition .AND. (.NOT. l_deposition_from_ukca)) THEN

      CALL deposition_from_surf_couple_extra(                                  &
           a_step, timestep,                                                   &
             ! passed to calling routine, defined in module ainfo
           row_length, rows, land_pts, ainfo%land_index,                       &
             ! From TYPE ainfo, coast
           surft_pts, ainfo%surft_index, ainfo%ice_fract_ij, coast%fland,      &
             ! From TYPE forcing: pressure and surface water mixing ratio
           forcing%pstar_ij, forcing%qw_1_ij,                                  &
             ! From TYPE fluxes: temperature, SW radiation & surface roughness
           fluxes%tstar_ij, fluxes%sw_surft, fluxes%z0h_surft,                 &
             ! From TYPE progs
           progs%gc_surft, progs%lai_pft, progs%canht_pft, progs%tstar_surft,  &
           progs%canopy_surft, progs%smc_soilt,                                &
             ! From TYPES jules_vars
           jules_vars%zh, jules_vars%dzl,                                      &
             ! passed From calling routine
           tile_frac,                                                          &
             ! From TYPE chemvars: atmospheric variables & diagnostics
           chemvars,                                                           &
             ! Parameters for Paulot et al H2 deposition scheme
           nsurft, nsoilt, sm_levels, dim_cslayer, dim_cs1,                    &
           psparms%smvcst_soilt, progs%snowdepth_surft, progs%cs_pool_soilt,   &
           progs%t_soil_soilt )

    END IF

    !Snow (standalone and UM)
    CALL snow ( land_pts,timestep,smlt,nsurft,n_wtrac_jls,surft_pts,           &
                ainfo%surft_index,psparms%catch_snow_surft,con_snow_gb,        &
                con_rain_gb, tile_frac,ls_snow_gb,ls_graup_gb,ls_rain_gb,      &
                fluxes%ei_surft,psparms%hcap_soilt(:,:,1),hcons_soilt,         &
                fluxes%melt_surft,fluxes%snowinc_surft,                        &
                progs%smcl_soilt(:,:,1),psparms%sthf_soilt(:,:,1),             &
                fluxes%surf_htf_surft, progs%t_soil_soilt(:,:,1),              &
                progs%tsurf_elev_surft, progs%tstar_surft,                     &
                psparms%smvcst_soilt(:,:,1), con_snow_wtrac,                   &
                wtrac_jls%ei_surft, progs%rgrain_surft,                        &
                progs%rgrainl_surft, progs%rho_snow_grnd_surft,                &
                progs%sice_surft,progs%sliq_surft,progs%snow_grnd_surft,       &
                progs%snow_surft, progs%snowdepth_surft, progs%tsnow_surft,    &
                progs%nsnow_surft, con_rain_wtrac, ls_rain_wtrac,              &
                ls_snow_wtrac, ls_graup_wtrac, wtrac_ex%melt_surft,            &
                wtrac_jls%snow_grnd_surft, wtrac_jls%snow_surft,               &
                wtrac_jls%sice_surft, wtrac_jls%sliq_surft, progs%ds_surft,    &
                fluxes%hf_snow_melt_gb,snow_mass_gb,progs%rho_snow_surft,      &
                fluxes%snomlt_sub_htf_gb, fluxes%snow_melt_gb,                 &
                fluxes%snow_soil_htf,surf_ht_flux_ld,                          &
                sf_diag, dhf_surf_minus_soil, wtrac_ex%snow_melt,              &
                wtrac_ex%lake_snow_melt,                                       &
                ! New Arguments to replace USE statements
                ! jules_internal
                jules_vars%unload_backgrnd_pft, npft, l_flake_model,           &
                !Ancil info (IN)
                ainfo%l_lice_point, ainfo%l_lice_surft,                        &
                ! Types Variables
                lake_vars%lake_h_ice_gb, lake_vars%hcon_lake,                  &
                lake_vars%ts1_lake_gb, lake_vars%lake_snow_melt,               &
                lake_vars%non_lake_frac, lake_vars%lake_h_mxl_gb,              &
                lake_vars%lake_depth_gb)

    IF (l_wtrac_jls) THEN

      ! Check that the snow normal water tracer stores are not significantly
      ! diverging from the water fields
      CALL wtrac_checks_sno(land_pts, nsurft, nsmax, surft_pts,                &
                            ainfo%surft_index,                                 &
                            progs%snow_surft, progs%snow_grnd_surft,           &
                            progs%sice_surft,progs%sliq_surft,                 &
                            wtrac_jls%snow_surft(:,:,1),                       &
                            wtrac_jls%snow_grnd_surft(:,:,1),                  &
                            wtrac_jls%sice_surft(:,:,:,1),                     &
                            wtrac_jls%sliq_surft(:,:,:,1))

      ! Correct snow water tracer stores to prevent build up of numerical error
      CALL wtrac_correct_sno(land_pts, nsurft, nsmax, n_wtrac_jls,             &
                            surft_pts, ainfo%surft_index,                      &
                            progs%snow_surft, progs%snow_grnd_surft,           &
                            progs%sice_surft,progs%sliq_surft,                 &
                            wtrac_jls%snow_surft, wtrac_jls%snow_grnd_surft,   &
                            wtrac_jls%sice_surft, wtrac_jls%sliq_surft)

    END IF  ! l_wtrac_jls

    !Hydrology (standalone and UM)
    CALL hydrol (                                                              &
      land_pts, soil_pts, lice_pts, sm_levels, npft, nsurft, dim_cs1,          &
      asteps_since_triffid, n_wtrac_jls, timestep, ainfo%l_lice_surft,         &
      l_inland, l_pdm, l_soil_sat_down, l_top, stf_sub_surf_roff,              &
      ainfo%soil_index, ainfo%lice_index,                                      &
      surft_pts, ainfo%surft_index, progs%nsnow_surft,                         &
      psparms%bexp_soilt, psparms%hcap_soilt, psparms%hcon_soilt,              &
      psparms%satcon_soilt, psparms%sathh_soilt,                               &
      psparms%smvcst_soilt, psparms%smvcwt_soilt,                              &
      psparms%catch_surft, crop_vars%frac_irr_soilt, psparms%infil_surft,      &
      lake_vars%non_lake_frac, toppdm%slope_gb, tile_frac,                     &
      con_rainfrac_gb, con_rain_gb, ls_rainfrac_gb, ls_rain_gb,                &
      con_rain_wtrac, ls_rain_wtrac,                                           &
      surf_ht_flux_ld,                                                         &
      fluxes%ecan_surft, crop_vars%ext_irr_soilt, fluxes%ext_soilt,            &
      wtrac_jls%ecan_surft, wtrac_jls%ext_soilt,                               &
      progs%snowdepth_surft, fluxes%melt_surft, fluxes%snow_melt_gb,           &
      wtrac_ex%melt_surft, wtrac_ex%snow_melt, fluxes%snow_soil_htf,           &
      a_fsat_soilt,c_fsat_soilt,a_fwet_soilt, c_fwet_soilt,                    &
      fexp_soilt, ti_mean_soilt, ti_local_soilt,                               &
      toppdm%zw_local_soilt, toppdm%fch4_local_soilt,                          &
      npp_gb, inlandout_atm_gb, wtrac_jls%inlandout_atm_gb,                    &
      progs%canopy_surft, wtrac_jls%canopy_surft,                              &
      progs%smcl_soilt, psparms%sthf_soilt,                                    &
      psparms%sthu_soilt, crop_vars%sthu_irr_soilt, progs%tsoil_deep_gb,       &
      progs%t_soil_soilt, progs%t_soil_soilt_acc, progs%alt_currentyear_soilt, &
      progs%alt_lastyear_soilt, progs%timewet_lyr, progs%tsurf_elev_surft,     &
      wtrac_jls%smcl_soilt, wtrac_jls%sthf_soilt, wtrac_jls%sthu_soilt,        &
      fsat_soilt, fwetl_soilt, sthzw_soilt, zw_soilt, wtrac_jls%sthzw_soilt,   &
      progs%cs_pool_soilt, trifctltype%resp_s_soilt,                           &
      toppdm%cs_ch4_soilt, toppdm%fch4_wetl_acc_soilt,                         &
      progs%substr_ch4, progs%mic_ch4, progs%mic_act_ch4, progs%acclim_ch4,    &
      progs%n_inorg_avail_pft, progs%n_inorg_soilt_lyrs,                       &
      trif_vars%n_leach_gb_acc,                                                &
      progs%canopy_gb, progs%smc_soilt,                                        &
      toppdm%drain_soilt, toppdm%dun_roff_soilt, fluxes%sub_surf_roff_gb,      &
      fluxes%surf_roff_gb, fluxes%tot_tfall_gb, fluxes%tot_tfall_surft,        &
      w_flux_soilt, wtrac_jls%surf_roff_gb,  wtrac_jls%sub_surf_roff_gb,       &
      toppdm%qbase_soilt, qbase_l_soilt, toppdm%qbase_zw_soilt,                &
      toppdm%fch4_wetl_soilt, toppdm%fch4_wetl_cs_soilt,                       &
      toppdm%fch4_wetl_npp_soilt, toppdm%fch4_wetl_resps_soilt,                &
      trif_vars%n_leach_soilt, psparms%dtsd_acc_soilt)


    DEALLOCATE(ls_graup_wtrac)
    DEALLOCATE(ls_rain_wtrac)
    DEALLOCATE(ls_snow_wtrac)
    DEALLOCATE(con_rain_wtrac)
    DEALLOCATE(con_snow_wtrac)

    IF (l_wtrac_jls) THEN

      ! Check that the hydrological normal water tracer stores are not
      ! significantly diverging from the water fields
      CALL wtrac_checks_hyd(land_pts, nsurft, nsoilt, sm_levels, soil_pts,     &
                            ainfo%soil_index, surft_pts, ainfo%surft_index,    &
                            progs%canopy_surft, psparms%sthu_soilt,            &
                            psparms%sthf_soilt, progs%smcl_soilt, sthzw_soilt, &
                            wtrac_jls%canopy_surft(:,:,1),                     &
                            wtrac_jls%sthu_soilt(:,:,:,1),                     &
                            wtrac_jls%sthf_soilt(:,:,:,1),                     &
                            wtrac_jls%smcl_soilt(:,:,:,1),                     &
                            wtrac_jls%sthzw_soilt(:,:,1))

      ! Correct hydrological water tracer stores to prevent build up of
      ! numerical errors
      CALL wtrac_correct_hyd(land_pts, nsurft, nsoilt, sm_levels, n_wtrac_jls, &
                            soil_pts, ainfo%soil_index,                        &
                            surft_pts, ainfo%surft_index,                      &
                            progs%canopy_surft, psparms%sthu_soilt,            &
                            psparms%sthf_soilt, progs%smcl_soilt, sthzw_soilt, &
                            wtrac_jls%canopy_surft, wtrac_jls%sthu_soilt,      &
                            wtrac_jls%sthf_soilt,   wtrac_jls%smcl_soilt,      &
                            wtrac_jls%sthzw_soilt)

    END IF ! l_wtrac_jls

  END IF ! ( l_hydrology .AND. land_pts /= 0 )

  ! Code not yet ported to LFRic
#if !defined(LFRIC)

  !Here we need to exit the land_pts IF to allow river routing to be called
  !on all MPI ranks. This is because it is possible that a rank with
  !land_pts = 0 still has a river routing point.

#if !defined(UM_JULES)
    ! Water resources (standalone; not yet allowed in UM).
  IF ( l_water_resources ) THEN
    CALL water_resources_control(                                              &
           rivers%global_land_index, ainfo%land_index,                         &
           rivers%map_river_to_land_points, rivers%rivers_index_rp,            &
           forcing%con_rain_ij, forcing%con_snow_ij,                           &
           water_resources%conv_loss_frac,                                     &
           water_resources%demand_rate_domestic,                               &
           water_resources%demand_rate_industry,                               &
           water_resources%demand_rate_livestock,                              &
           water_resources%demand_rate_transfers, crop_vars%dvi_cpft,          &
           flandg, crop_vars%frac_irr_soilt, ainfo%frac_soilt,                 &
           ainfo%frac_surft, grid_area_ij,                                     &
           forcing%ls_rain_ij, forcing%ls_snow_ij, forcing%lw_down_ij,         &
           psparms%smvccl_soilt,                                               &
           psparms%smvcst_soilt, psparms%smvcwt_soilt, psparms%sthf_soilt,     &
           fluxes%sw_surft, forcing%tl_1_ij, progs%tstar_surft,                &
           crop_vars%icntmax_gb, crop_vars%plant_n_gb,                         &
           water_resources%demand_accum,                                       &
           crop_vars%prec_1_day_av_gb, crop_vars%prec_1_day_av_use_gb,         &
           rivers%rfm_surfstore_rp, rivers%rivers_sto_rp,                      &
           crop_vars%rn_1_day_av_gb,                                           &
           crop_vars%rn_1_day_av_use_gb, water_resources%sfc_water_frac,       &
           progs%smcl_soilt,                                                   &
           crop_vars%sthu_irr_soilt, psparms%sthu_soilt,                       &
           sthzw_soilt, fluxes%sub_surf_roff_gb, crop_vars%tl_1_day_av_gb,     &
           crop_vars%tl_1_day_av_use_gb, water_resources%priority_order,       &
           water_resources%demand_unmet, water_resources%gw_abstracted,        &
           water_resources%gw_avail, water_resources%gw_nr_abstracted,         &
           crop_vars%irrig_water_gb, water_resources%net_abstracted_river,     &
           water_resources%sw_abstracted, water_resources%sw_avail_total,      &
           water_resources%water_removed )
  END IF
#endif

  IF ( l_rivers ) THEN
    CALL surf_couple_rivers(                                                   &
      !INTEGER, INTENT(IN)
      land_pts, n_wtrac_jls,                                                   &
      !REAL, INTENT(IN)
      water_resources%net_abstracted_river,                                    &
      fluxes%sub_surf_roff_gb, fluxes%surf_roff_gb,                            &
      wtrac_jls%sub_surf_roff_gb,  wtrac_jls%surf_roff_gb,                     &
      !INTEGER, INTENT(INOUT)
      a_steps_since_riv,                                                       &
      !REAL, INTENT (INOUT)
      tot_surf_runoff_gb, tot_sub_runoff_gb, acc_lake_evap_gb,                 &
      wtrac_jls%tot_surf_runoff_gb, wtrac_jls%tot_sub_runoff_gb,               &
      wtrac_jls%acc_lake_evap_gb,                                              &
      !REAL, INTENT (OUT)
      rivers%rivers_sto_per_m2_on_landpts, fluxes%rflow_gb, fluxes%rrun_gb,    &
      !Arguments for the UM-----------------------------------------
      !INTEGER, INTENT(IN)
      n_proc, row_length, rows, river_row_length, river_rows,                  &
      ainfo%land_index, aocpl_row_length, aocpl_p_rows, g_p_field,             &
      g_r_field, global_row_length, global_rows, global_river_row_length,      &
       global_river_rows,                                                      &
      !REAL, INTENT(IN)
      fluxes%lake_evap, delta_lambda, delta_phi, xx_cos_theta_latitude,        &
      xpa, xua, xva, ypa, yua, yva, flandg, trivdir,                           &
      trivseq, r_area, slope, flowobs1, r_inext, r_jnext, r_land,              &
      psparms%smvcst_soilt, psparms%smvcwt_soilt, ainfo%frac_surft,            &
      wtrac_jls%lake_evap,                                                     &
      !REAL, INTENT(INOUT)
      substore, surfstore, flowin, bflowin, twatstor,                          &
      progs%smcl_soilt, psparms%sthu_soilt,                                    &
      wtrac_jls%twatstor, wtrac_jls%smcl_soilt, wtrac_jls%sthu_soilt,          &
      !REAL, INTENT(OUT)
      inlandout_atm_gb, inlandout_riv, riverout, box_outflow, box_inflow,      &
      riverout_rgrid, wtrac_jls%inlandout_atm_gb,                              &
      !  imported rivers arrays
      rivers)
  END IF ! l_rivers (ATMOS)

  IF (l_rivers .AND. l_wtrac_jls) THEN

    ! Check that the river normal water tracer store is not diverging
    ! signifantly from the water field.
    CALL wtrac_checks_grid(river_row_length, river_rows,                       &
                           twatstor, wtrac_jls%twatstor(:,:,1),'twatstor')

    ! Correct river water tracer store to prevent build up of numerical errors
    CALL wtrac_correct_grid(river_row_length, river_rows, n_wtrac_jls,         &
                           twatstor, wtrac_jls%twatstor)

    ! Check that the hydrological normal water tracer stores are not
    ! significantly diverging from the water fields
    CALL wtrac_checks_hyd(land_pts, nsurft, nsoilt, sm_levels, soil_pts,       &
                          ainfo%soil_index, surft_pts, ainfo%surft_index,      &
                          progs%canopy_surft, psparms%sthu_soilt,              &
                          psparms%sthf_soilt, progs%smcl_soilt, sthzw_soilt,   &
                          wtrac_jls%canopy_surft(:,:,1),                       &
                          wtrac_jls%sthu_soilt(:,:,:,1),                       &
                          wtrac_jls%sthf_soilt(:,:,:,1),                       &
                          wtrac_jls%smcl_soilt(:,:,:,1),                       &
                          wtrac_jls%sthzw_soilt(:,:,1))

  END IF ! (l_rivers .AND. l_wtrac_jls)

  !Irrigation (standalone and UM for some options).
  IF ( l_irrig_dmd .AND. .NOT. l_water_resources ) THEN
    CALL irrigation_control( a_step, land_pts,                                 &
                       ainfo%land_index,                                       &
                       forcing%con_rain_ij, forcing%con_snow_ij,               &
                       crop_vars%dvi_cpft,                                     &
                       crop_vars%frac_irr_soilt, ainfo%frac_surft,             &
                       forcing%ls_rain_ij, forcing%ls_snow_ij,                 &
                       forcing%lw_down_ij,                                     &
                       psparms%smvccl_soilt, psparms%smvcst_soilt,             &
                       psparms%smvcwt_soilt, psparms%sthf_soilt,               &
                       fluxes%sw_surft, forcing%tl_1_ij, progs%tstar_surft,    &
                       crop_vars%icntmax_gb, crop_vars%plant_n_gb,             &
                       crop_vars%irrDaysDiag_gb, crop_vars%prec_1_day_av_gb,   &
                       crop_vars%prec_1_day_av_use_gb,                         &
                       crop_vars%rn_1_day_av_gb, crop_vars%rn_1_day_av_use_gb, &
                       crop_vars%tl_1_day_av_gb, crop_vars%tl_1_day_av_use_gb, &
                       progs%smcl_soilt,  crop_vars%sthu_irr_soilt,            &
                       psparms%sthu_soilt, sthzw_soilt,                        &
                       crop_vars%irrig_water_gb,                               &
                       !New arguments replacing USE statements
                       !jules_rivers_mod
                       rivers%rivers_sto_per_m2_on_landpts,                    &
                       rivers%rivers_adj_on_landpts,                           &
                       rivers )
  ELSE IF ( .NOT. l_water_resources ) THEN
    ! Set sthu_irr_soilt to 0.0 in case it is still reported
    ! It would be better to not allocate this array when it is not being used.
    ! This led to a memory leak in the D1 array (UM).
    crop_vars%sthu_irr_soilt(:,:,:) = 0.0
  END IF ! l_irrig_dmd

  !Restart the IF for land_pts > 0 now that river routing is done with
  IF (land_pts > 0) THEN

    !Crops (standalone only)
#if !defined(UM_JULES)
    IF ( l_crop ) THEN
      crop_call = MOD ( REAL(a_step),                                          &
                        REAL(crop_period) * rsec_per_day / timestep )

      DO n = 1,ncpft
        DO l = 1,land_pts
          trifctltype%npp_acc_pft(l,nnpft + n) =                               &
                        trifctltype%npp_acc_pft(l,nnpft + n)                   &
                        + (trifctltype%npp_pft(l,nnpft + n) * timestep)
        END DO
      END DO

      CALL photoperiod(t_i_length * t_j_length, crop_vars%phot,                &
                       crop_vars%dphotdt)

      CALL crop(t_i_length * t_j_length, land_pts, ainfo%land_index, a_step,   &
                crop_call, sm_levels, ainfo%frac_surft, crop_vars%phot,        &
                crop_vars%dphotdt,                                             &
                sf_diag%t1p5m_surft, progs%t_soil_soilt, psparms%sthu_soilt,   &
                psparms%smvccl_soilt,                                          &
                psparms%smvcst_soilt, trifctltype%npp_acc_pft,                 &
                progs%canht_pft, progs%lai_pft, crop_vars%dvi_cpft,            &
                crop_vars%rootc_cpft,                                          &
                crop_vars%harvc_cpft,                                          &
                crop_vars%reservec_cpft, crop_vars%croplai_cpft,               &
                crop_vars%cropcanht_cpft,                                      &
                psparms%catch_surft, psparms%z0_surft,                         &
                !New arguments replacing USE statements
                !crop_vars_mod
                crop_vars%sow_date_cpft, crop_vars%tt_veg_cpft,                &
                crop_vars%tt_rep_cpft,                                         &
                crop_vars%latestharv_date_cpft,                                &
                crop_vars%yield_diag_cpft, crop_vars%stemc_diag_cpft,          &
                crop_vars%leafc_diag_cpft, crop_vars%nonyield_diag_cpft,       &
                crop_vars%harvest_trigger_cpft, crop_vars%harvest_counter_cpft,&
                !ancil_info (IN)
                ainfo%l_lice_point, ainfo%l_lice_surft)

    END IF  ! l_crop

    !Metstats (standalone only)
        !Beware- does not currently account for graupel
    IF ( l_metstats ) THEN
      CALL metstats_timestep(tl_1_gb, qw_1_gb, u_1_gb, v_1_gb, ls_rain_gb,     &
                            con_rain_gb, ls_snow_gb, con_snow_gb, pstar_gb,    &
                            metstats_prog,                                     &
                            !Vars that should be USED but can't due to
                            !UM/standalone differences
                            current_time%TIME, timestep,land_pts)
    END IF

    !Fire (standalone only)
    IF ( l_fire ) THEN
      !Calculate the gridbox mean soil moisture
      smc_gb = soiltiles_to_gbm(progs%smc_soilt, ainfo)
      CALL fire_timestep(metstats_prog, smc_gb, fire_prog, fire_diag,          &
                        !Vars that should be USED but can't due to
                        !UM/standalone differences
                        current_time%TIME, current_time%month, timestep,       &
                        land_pts)
    END IF
#endif

    !INFERNO (standalone and UM)
    IF ( l_inferno ) THEN
      CALL calc_soil_carbon_pools(land_pts, soil_pts, ainfo%soil_index,        &
                                  dim_cs1, progs%cs_pool_soilt,                &
                                  c_soil_dpm_gb, c_soil_rpm_gb)

      CALL inferno_io( sf_diag%t1p5m_surft, sf_diag%q1p5m_surft, pstar_gb,     &
                      psparms%sthu_soilt, sm_levels,                           &
                      ainfo%frac_surft, c_soil_dpm_gb, c_soil_rpm_gb,          &
                      progs%canht_pft,                                         &
                      ls_rain_gb, con_rain_gb,                                 &
                      fire_vars,                                               &
                      land_pts, ignition_method,                               &
                      nsurft, asteps_since_triffid,                            &
                    ! New Arguments to replace USE statements
                    ! TRIF_VARS_MOD
                      trif_vars%g_burn_pft_acc)
    END IF  !  l_inferno

    IF (l_phenol .OR. l_triffid) THEN
      IF (l_red) THEN

        !Update veg3 structures
        !Structures can then be passed straight through
        ! Copy prognostics and forcings in and then out
        ! Same calling when coulpled to atmos
        DO n = 1, nnpft
          DO k = 1, land_pts
            l = ainfo%land_index(k)
            veg_state%npp_acc(l,n) = trifctltype%npp_acc_pft(l,n)
          END DO
        END DO

        CALL next_gen_biogeochem(                                              &
          !IN control vars
            asteps_since_triffid,land_pts,nnpft,nmasst,veg3_ctrl,              &
            ainfo,                                                             &
          !IN parms
            litter_parms,red_parms,                                            &
          !INOUT data structures
            veg_state,red_state                                                &
          !OUT diagnostics
          )

        ! Update the physical state of the land
        ! Outside of main call as
        ! - Uses non-veg tilepts
        ! - Accesses fields via USE statements

        ! Note use lai_bal for now as we don't yet have phenology
        CALL sparm (land_pts,nsurft,surft_pts,ainfo%surft_index,               &
                    veg_state%frac,veg_state%canht,                            &
                    veg_state%lai_bal,psparms%z0m_soil_gb,                     &
                    psparms%catch_snow_surft,psparms%catch_surft,              &
                    psparms%z0_surft,psparms%z0h_bare_surft,urban_param%ztm_gb)

        CALL infiltration_rate(land_pts,nsurft,surft_pts,ainfo%surft_index,    &
                               psparms%satcon_soilt,veg_state%frac,            &
                               psparms%infil_surft)

        DO n = 1, nnpft
          DO k = 1, land_pts
            l = ainfo%land_index(k)
            trifctltype%npp_acc_pft(l,n) = veg_state%npp_acc(l,n)
          END DO
        END DO

      ELSE ! Triffid based model

        !Vegetation (standalone and UM- differing functionality)

        CALL veg_control(                                                      &
          land_pts, nsurft, dim_cs1,                                           &
          a_step, asteps_since_triffid,                                        &
          phenol_period, triffid_period,                                       &
          l_phenol, l_triffid, l_trif_eq,                                      &
          timestep, frac_agr_gb, trif_vars%frac_past_gb,                       &
          trif_vars%frac_biocrop_gb, psparms%satcon_soilt,                     &
          trifctltype%g_leaf_acc_pft, trifctltype%g_leaf_phen_acc_pft,         &
          trifctltype%npp_acc_pft,                                             &
          trifctltype%resp_w_acc_pft,                                          &
          ainfo%frac_surft, progs%lai_pft,                                     &
          psparms%clay_soilt, psparms%z0m_soil_gb,                             &
          progs%canht_pft,                                                     &
          psparms%catch_snow_surft, psparms%catch_surft,                       &
          psparms%infil_surft, psparms%z0_surft, psparms%z0h_bare_surft,       &
          trifctltype%c_veg_pft, trifctltype%cv_gb, trifctltype%lit_c_pft,     &
          trifctltype%lit_c_mn_gb, trifctltype%g_leaf_day_pft,                 &
          trifctltype%g_leaf_phen_pft,trifctltype%lai_phen_pft,                &
          trifctltype%g_leaf_dr_out_pft, trifctltype%npp_dr_out_pft,           &
          trifctltype%resp_w_dr_out_pft, qbase_l_soilt,                        &
          psparms%sthf_soilt, psparms%sthu_soilt,                              &
          w_flux_soilt, progs%t_soil_soilt, progs%alt_lastyear_soilt,          &
          progs%cs_pool_soilt,                                                 &
          progs%frac_c_label_pool_soilt,                                       &
          !New arguments replacing USE statements
          !trif_vars_mod (IN OUT)
          trif_vars,                                                           &
          !crop_vars_mod (IN)
          crop_vars%rootc_cpft, crop_vars%harvc_cpft, crop_vars%reservec_cpft, &
          crop_vars%stemc_diag_cpft,                                           &
          crop_vars%leafc_diag_cpft, crop_vars%dvi_cpft,                       &
          ! prognostics (IN)
          progs%wood_prod_fast_gb, progs%wood_prod_med_gb,                     &
          progs%wood_prod_slow_gb, progs%frac_agr_prev_gb,                     &
          progs%frac_past_prev_gb, progs%frac_biocrop_prev_gb,                 &
          progs%n_inorg_gb, progs%n_inorg_soilt_lyrs,                          &
          progs%n_inorg_avail_pft, progs%ns_pool_gb,                           &
          progs%triffid_co2_gb, progs%t_soil_soilt_acc,                        &
          progs%years_since_harvest,                                           &
          ! p_s_parms (IN)
          psparms%bexp_soilt, psparms%sathh_soilt,                             &
          psparms%smvcst_soilt, psparms%smvcwt_soilt,                          &
          psparms%soil_ph_soilt,                                               &
          ! soil_ecosse_vars_mod (OUT)
          soilecosse%n_soil_pool_soilt, dim_soil_n_pool,                       &
          soilecosse%co2_soil_gb, soilecosse%n2o_soil_gb,                      &
          soilecosse%n2o_denitrif_gb, soilecosse%n2o_nitrif_gb,                &
          soilecosse%n2o_partial_nitrif_gb, soilecosse%n2_denitrif_gb,         &
          soilecosse%n_denitrification_gb, soilecosse%n_leach_amm_gb,          &
          soilecosse%n_leach_nit_gb, soilecosse%n_nitrification_gb,            &
          soilecosse%no_soil_gb, soilecosse%soil_c_add, soilecosse%soil_n_add, &
          ! soil_ecosse_vars_mod (IN)
          soilecosse%qbase_l_driver, soilecosse%sthf_driver,                   &
          soilecosse%sthu_driver, soilecosse%tsoil_driver,                     &
          soilecosse%wflux_driver,                                             &
          !ancil_info (IN)
          ainfo%l_lice_point, ainfo%l_lice_surft, ainfo%soil_index,            &
          !TYPES
          soilecosse, urban_param,trifctltype)

      END IF
    END IF

    IF ( l_acclim ) THEN
      ! Update the growth temperature for the thermal acclimation of
      ! photosynthesis.
      alpha_acclim = 1.0 - EXP( -1.0 * timestep                                &
                              / ( n_day_photo_acclim * rsec_per_day ) )
      progs%t_growth_gb(:) = progs%t_growth_gb(:)                              &
                     + alpha_acclim * ( tl_1_gb(:) - progs%t_growth_gb(:) )
    END IF

#if !defined(UM_JULES)
    !Reference evapotranspiration (standalone only)
    IF (l_fao_ref_evapotranspiration) THEN
      trad = ( surftiles_to_gbm(progs%tstar_surft**4, ainfo) )**0.25
      CALL fao_ref_evapotranspiration(soil_pts, ainfo%soil_index,              &
        land_pts, ainfo%land_index, sf_diag%t1p5m,                             &
        forcing%sw_down_ij, forcing%lw_down_ij, fluxes%surf_ht_flux_ij,        &
        sf_diag%u10m, sf_diag%v10m, sf_diag%q1p5m, forcing%pstar_ij,           &
        trad, trif_vars%fao_et0)
    END IF
    ! End of standalone Jules code
#endif

    !-----------------------------------------------------------------------
    !     call to the FLake interface
    !-----------------------------------------------------------------------
    IF (       l_flake_model                                                   &
         .AND. ( .NOT. l_aggregate)                                            &
         .AND. (land_pts > 0 ) ) THEN

      DO k = 1,surft_pts(lake)
        l = ainfo%surft_index(k,lake)
        j=(ainfo%land_index(l) - 1) / row_length + 1
        i = ainfo%land_index(l) - (j-1) * row_length

        !       U* of lake obtained by matching surface stress
        lake_vars%u_s_lake_gb(l) =   u_s_std_surft(l, lake)                    &
                      * SQRT( rhostar(i,j) / rho_water )

        !       Downwelling SW on lake tile
        lake_vars%sw_down_gb(l) = fluxes%sw_surft(l,lake) /                    &
                                    (1.0 - lake_vars%lake_albedo_gb(l))

        !       Take the net SW flux out of the surface heat flux
        !       since this is done separately within FLake.
        lake_vars%surf_ht_flux_lk_gb(l) = lake_vars%surf_ht_flux_lake_ij(i,j) -&
                                            fluxes%sw_surft(l,lake)

        IF ( (nsmax > 0) .AND.                                                 &
             (lake_vars%lake_h_snow_gb(l) > EPSILON(1.0)) ) THEN
          ! For the new snow scheme, FLake is forced with zero snow
          ! and the forcing fluxes are those at the bottom of the snowpack.
          ! The order of the following calculations is important.

          ! attenuated DWSW below snow
          dwsw_sub_snow = lake_vars%sw_down_gb(l) *                            &
                    EXP( -lake_vars%lake_h_snow_gb(l) / h_snow_sw_att )

          ! following last calculation, set snow depth to zero
          lake_vars%lake_h_snow_gb(l) = 0.0

          ! heat flux into the lake becomes
          ! the sub-snow value minus remaining DWSW

          lake_vars%surf_ht_flux_lk_gb(l) = lake_vars%surf_ht_flux_lake_ij(i,j)&
                                  - dhf_surf_minus_soil(l)                     &
                                  - dwsw_sub_snow

          ! now overwrite the DWSW with the remaining sub-snow amount
          lake_vars%sw_down_gb(l) = dwsw_sub_snow
        END IF

      END DO

      trap_frozen   = 0
      trap_unfrozen = 0

      CALL flake_interface( land_pts                                           &
                           ,surft_pts(lake)                                    &
                           ,ainfo%surft_index(:,lake)                          &
                           ,lake_vars%u_s_lake_gb                              &
                           ,lake_vars%surf_ht_flux_lk_gb                       &
                           ,lake_vars%sw_down_gb                               &
                           ,lake_vars%lake_depth_gb                            &
                           ,lake_vars%coriolis_param_gb                        &
                           ,timestep                                           &
                           ,lake_vars%lake_albedo_gb                           &
                           ,lake_vars%lake_t_snow_gb                           &
                           ,lake_vars%lake_t_ice_gb                            &
                           ,lake_vars%lake_t_mean_gb                           &
                           ,lake_vars%lake_t_mxl_gb                            &
                           ,lake_vars%lake_shape_factor_gb                     &
                           ,lake_vars%lake_h_snow_gb                           &
                           ,lake_vars%lake_h_ice_gb                            &
                           ,lake_vars%lake_h_mxl_gb                            &
                           ,lake_vars%lake_t_sfc_gb                            &
                           ,lake_vars%ts1_lake_gb                              &
                           ,lake_vars%g_dt_gb                                  &
                           ,trap_frozen                                        &
                           ,trap_unfrozen )

      IF ( trap_frozen > 0 ) THEN
        errcode = -1
        CALL ereport('control', errcode,                                       &
          'surf_couple_extra-FLake: # zero-divide (frozen) avoided =')
      END IF
      IF ( trap_unfrozen > 0 ) THEN
        errcode = -1
        CALL ereport('control', errcode,                                       &
          'surf_couple_extra-FLake: # zero-divide (unfrozen) avoided =')
      END IF

    END IF ! Flake

    ! End of code excluded from LFRic builds
#else
    ! These variables are intent out and so must be initialised for LFRic to
    ! compile. Set to rmdi in the hope this will show errors if they are used!
  fluxes%rrun_gb = rmdi
  fluxes%rflow_gb = rmdi
  psparms%z0_surft = rmdi
  psparms%z0h_bare_surft = rmdi

  IF (land_pts > 0) THEN

    ! End of bespoke LFRic code
#endif

    !End of science calls
    !===========================================================================
    !Expansion to full grid

    DO l = 1, land_pts
      j=(ainfo%land_index(l) - 1) / row_length + 1
      i = ainfo%land_index(l) - (j-1) * row_length
      progs%snow_mass_ij(i,j) = snow_mass_gb(l)
    END DO

  END IF ! land_pts > 0

  !=============================================================================
  !UM Diagnostics calls

    !For the UM, soil tiling has not been implemented, ie nsoilt = 1, so we can
    !hard-code _soilt variables with index 1 using m = 1
#if defined(UM_JULES) && !defined(LFRIC)
  IF (model_type /= mt_single_column) THEN
    m = 1
    IF (l_hydrology .AND. sf(0,8) ) THEN
      CALL diagnostics_hyd(                                                    &
        row_length, rows,                                                      &
        land_pts, sm_levels,                                                   &
        ainfo%land_index,inlandout_atm_gb,                                     &
        progs%smc_soilt(:,m), fluxes%surf_roff_gb, fluxes%sub_surf_roff_gb,    &
        snow_mass_gb, fluxes%snow_melt_gb,                                     &
        progs%t_soil_soilt(:,m,:),                                             &
        fluxes%snow_soil_htf,                                                  &
        progs%smcl_soilt(:,m,:),                                               &
        nsurft, fluxes%hf_snow_melt_gb, psparms%sthu_soilt(:,m,:),             &
        psparms%sthf_soilt(:,m,:),                                             &
        fluxes%tot_tfall_gb, fluxes%melt_surft,                                &
        land_sea_mask,                                                         &
        toppdm%dun_roff_soilt(:,m), toppdm%drain_soilt(:,m),                   &
        toppdm%qbase_soilt(:,m),                                               &
        toppdm%qbase_zw_soilt(:,m), toppdm%fch4_wetl_soilt(:,m),               &
        toppdm%fch4_wetl_cs_soilt(:,m),                                        &
        toppdm%fch4_wetl_npp_soilt(:,m),toppdm%fch4_wetl_resps_soilt(:,m),     &
        fexp_soilt(:,m),gamtot_soilt(:,m),ti_mean_soilt(:,m),                  &
        ti_sig_soilt(:,m),                                                     &
        fsat_soilt(:,m),fwetl_soilt(:,m),zw_soilt(:,m),sthzw_soilt(:,m),       &
        timestep,                                                              &
        STASHwork8,                                                            &
        sf_diag,                                                               &
        !JULES TYPEs
        fire_vars, progs)
    END IF

    IF ( l_rivers .AND. rivers_call .AND. sf(0,26) ) THEN
      CALL diagnostics_riv(                                                    &
        row_length, rows,                                                      &
        river_row_length, river_rows,                                          &
        riverout,                                                              &
        riverout_rgrid,                                                        &
        box_outflow, box_inflow,                                               &
        twatstor,inlandout_riv,                                                &
        STASHwork26                                                            &
        )
    END IF

    IF (sf(0,19)) THEN
      CALL diagnostics_veg(                                                    &
        row_length, rows,                                                      &
        land_pts, sm_levels,                                                   &
        ainfo%land_index,                                                      &
        ntype,npft,                                                            &
        trifctltype%c_veg_pft,trifctltype%cv_gb,trifctltype%g_leaf_phen_pft,   &
        trifctltype%lit_c_pft,trifctltype%lit_c_mn_gb,                         &
        trifctltype%g_leaf_day_pft, trifctltype%lai_phen_pft,                  &
        trifctltype%g_leaf_dr_out_pft,trifctltype%npp_dr_out_pft,              &
        trifctltype%resp_w_dr_out_pft,frac_agr_gb,                             &
        disturb_veg_prev,                                                      &
        ainfo%frac_surft,                                                      &
        STASHwork19,                                                           &
        !JULES TYPEs
        trif_vars, progs, trifctltype                                          &
        )
    END IF

  END IF
#endif

  ! Deallocate water tracer working arrays
  CALL wtrac_dealloc_extra(wtrac_ex)

  !=============================================================================
CASE ( cable )
  ! for testing LSM switch
  WRITE(jules_message,'(A)') "CABLE not yet implemented"
  CALL jules_print(RoutineName, jules_message)

  ! initialise all INTENT(OUT) for now until CABLE is implemented
  fluxes%melt_surft(:,:) = 0.0
  fluxes%snowinc_surft(:,:) = 0.0
  fluxes%hf_snow_melt_gb(:) = 0.0
  fluxes%snomlt_sub_htf_gb(:) = 0.0
  fluxes%sub_surf_roff_gb(:) = 0.0
  fluxes%surf_roff_gb(:) = 0.0
  fluxes%tot_tfall_gb(:) = 0.0
  fluxes%snow_melt_gb(:) = 0.0
  fluxes%rrun_gb(:) = 0.0
  fluxes%rflow_gb(:) = 0.0
  fluxes%snow_soil_htf(:,:) = 0.0

  work_cbl%snow_tile(:,:) = 0.0
  work_cbl%lying_snow(:) = 0.0
  work_cbl%surf_roff(:) = 0.0
  work_cbl%sub_surf_roff(:) = 0.0
  work_cbl%tot_tfall(:) = 0.0

CASE DEFAULT
  errorstatus = 101
  WRITE(jules_message,'(A,I0)') 'Unrecognised surface scheme. lsm_id = ',      &
     lsm_id
  CALL ereport(RoutineName, errorstatus, jules_message)

END SELECT

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_extra
END MODULE surf_couple_extra_mod
