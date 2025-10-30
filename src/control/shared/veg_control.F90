! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  Top-level control routine for vegetation section
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

MODULE veg_control_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='VEG_CONTROL_MOD'

CONTAINS
SUBROUTINE veg_control(                                                        &
  land_pts, nsurft, dim_cs1,                                                   &
  a_step, asteps_since_triffid,                                                &
  phenol_period, triffid_period,                                               &
  l_phenol, l_triffid, l_trif_eq,                                              &
  timestep_real, frac_agr_gb, frac_past_gb, frac_biocrop_gb, satcon_soilt,     &
  g_leaf_acc_pft, g_leaf_phen_acc_pft, npp_acc_pft,                            &
  resp_w_acc_pft,                                                              &
  frac_surft, lai_pft, clay_soilt, z0m_soil_gb, canht_pft,                     &
  catch_snow_surft, catch_surft, infil_surft, z0_surft, z0h_bare_surft,        &
  c_veg_pft, cv_gb, lit_c_pft, lit_c_mn_gb, g_leaf_day_pft, g_leaf_phen_pft,   &
  lai_phen_pft, g_leaf_dr_out_pft, npp_dr_out_pft, resp_w_dr_out_pft,          &
  qbase_l_soilt, sthf_soilt, sthu_soilt, w_flux_soilt,                         &
  t_soil_soilt, cs_pool_soilt, frac_c_label_pool_soilt,                        &
  !New arguments replacing USE statements
  !trif_vars_mod (IN OUT)
  trif_vars,                                                                   &
  !crop_vars_mod (IN)
  rootc_cpft, harvc_cpft, reservec_cpft, stemc_diag_cpft,                      &
  leafc_diag_cpft, dvi_cpft,                                                   &
  ! prognostics (IN)
  wood_prod_fast_gb, wood_prod_med_gb,                                         &
  wood_prod_slow_gb, frac_agr_prev_gb,                                         &
  frac_past_prev_gb, frac_biocrop_prev_gb, n_inorg_gb, n_inorg_soilt_lyrs,     &
  n_inorg_avail_pft, ns_pool_gb,                                               &
  triffid_co2_gb, t_soil_soilt_acc, years_since_harvest,                       &
  ! p_s_parms (IN)
  bexp_soilt, sathh_soilt, smvcst_soilt, smvcwt_soilt,                         &
  soil_ph_soilt,                                                               &
  ! soil_ecosse_vars_mod (OUT)
  n_soil_pool_soilt, dim_soil_n_pool, co2_soil_gb, n2o_soil_gb,                &
  n2o_denitrif_gb, n2o_nitrif_gb, n2o_partial_nitrif_gb,                       &
  n2_denitrif_gb, n_denitrification_gb, n_leach_amm_gb,                        &
  n_leach_nit_gb, n_nitrification_gb, no_soil_gb,                              &
  soil_c_add, soil_n_add,                                                      &
  ! soil_ecosse_vars_mod (IN)
  qbase_l_driver, sthf_driver, sthu_driver, tsoil_driver, wflux_driver,        &
  !ancil_info (IN)
  l_lice_point, l_lice_surft, soil_index,                                      &
  !TYPES containing field data (IN OUT)
  soilecosse, urban_param,trifctltype)

!Module imports

!TYPE definitions
USE trif_vars_mod, ONLY: trif_vars_type
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type
USE urban_param_mod, ONLY: urban_param_type
USE trifctl, ONLY: trifctl_type

!Import relevant subroutines
USE veg1_mod,                     ONLY: veg1
USE veg2_mod,                     ONLY: veg2
USE veg_soil_index_mod,           ONLY: get_veg_soil_index

#if ! defined(UM_JULES)
USE soil_biogeochem_control_mod,  ONLY: soil_biogeochem_control
#endif

!Import variables. Modules in alphabetical order
USE ancil_info,                   ONLY: dim_cslayer, nsoilt

USE conversions_mod,              ONLY: rsec_per_day, isec_per_day

! Use in ifdef
USE jules_soil_biogeochem_mod,    ONLY: soil_model_ecosse, soil_bgc_model

USE jules_soil_mod,               ONLY: sm_levels

USE jules_surface_types_mod,      ONLY: npft, ntype, ncpft

USE jules_vegetation_mod,         ONLY:                                        &
  i_veg_vn, i_veg_vn_1b, i_veg_vn_2b

#if ! defined(UM_JULES)
USE model_time_mod,               ONLY: timestep_len
#endif

USE timestep_mod,                 ONLY: timestep

!Dr Hook etc
USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE errormessagelength_mod, ONLY: errormessagelength
USE ereport_mod, ONLY: ereport

USE um_types, ONLY: real_jlslsm


IMPLICIT NONE

!Arguments

INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
  nsurft,                                                                      &
  dim_cs1,                                                                     &
  a_step,                                                                      &
  phenol_period,                                                               &
  triffid_period

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep_real,                                                               &
  frac_agr_gb(land_pts),                                                       &
  satcon_soilt(land_pts,nsoilt,0:sm_levels),                                   &
    ! Saturated hydraulic conductivity of the soil surface (kg/m2/s).
  z0m_soil_gb(land_pts),                                                       &
  qbase_l_soilt(land_pts,nsoilt,sm_levels+1),                                  &
        ! Base flow from each soil layer (kg m-2 s-1).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
  w_flux_soilt(land_pts,nsoilt,0:sm_levels),                                   &
        ! Fluxes of water between layers (kg m-2 s-1).
  t_soil_soilt(land_pts,nsoilt,sm_levels)

LOGICAL, INTENT(IN) ::                                                         &
  l_phenol, l_triffid, l_trif_eq

INTEGER, INTENT(IN OUT) ::                                                     &
  asteps_since_triffid

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  g_leaf_acc_pft(land_pts,npft),                                               &
  g_leaf_phen_acc_pft(land_pts,npft),                                          &
  npp_acc_pft(land_pts,npft),                                                  &
  resp_w_acc_pft(land_pts,npft),                                               &
  frac_surft(land_pts,ntype),                                                  &
  lai_pft(land_pts,npft),                                                      &
  frac_past_gb(land_pts),                                                      &
  frac_biocrop_gb(land_pts),                                                   &
  canht_pft(land_pts,npft),                                                    &
  clay_soilt(land_pts,nsoilt,dim_cslayer),                                     &
  frac_c_label_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1),                &
  cs_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  catch_snow_surft(land_pts,nsurft),                                           &
  catch_surft(land_pts,nsurft),                                                &
  infil_surft(land_pts,nsurft),                                                &
  z0_surft(land_pts,nsurft),                                                   &
  z0h_bare_surft(land_pts,nsurft),                                             &
  c_veg_pft(land_pts,npft),                                                    &
  cv_gb(land_pts),                                                             &
  lit_c_pft(land_pts,npft),                                                    &
  lit_c_mn_gb(land_pts),                                                       &
  g_leaf_day_pft(land_pts,npft),                                               &
  g_leaf_phen_pft(land_pts,npft),                                              &
  lai_phen_pft(land_pts,npft),                                                 &
  g_leaf_dr_out_pft(land_pts,npft),                                            &
  npp_dr_out_pft(land_pts,npft),                                               &
  resp_w_dr_out_pft(land_pts,npft)

!TYPES containing field data (IN OUT)
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param

!New arguments replacing USE statements
!crop_vars_mod
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: rootc_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: harvc_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: reservec_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: stemc_diag_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: leafc_diag_cpft(land_pts,ncpft)
REAL(KIND=real_jlslsm), INTENT(IN) :: dvi_cpft(land_pts,ncpft)

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
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
          triffid_co2_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
        t_soil_soilt_acc(land_pts,nsoilt,sm_levels)
        ! Sub-surface temperature on layers and soil tiles accumulated over
        ! TRIFFID timestep (K).
INTEGER, INTENT(IN OUT) :: years_since_harvest(land_pts,npft)

! p_s_params
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: bexp_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: sathh_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smvcst_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: smvcwt_soilt(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN) :: soil_ph_soilt(land_pts,nsoilt,dim_cslayer)

! soil_ecosse_vars_mod
INTEGER, INTENT(IN) :: dim_soil_n_pool
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  n_soil_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_soil_n_pool)
    ! N in soil pools (kg m-2).
REAL(KIND=real_jlslsm), INTENT(OUT) :: co2_soil_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_soil_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_denitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_nitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2o_partial_nitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n2_denitrif_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_denitrification_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_leach_amm_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_leach_nit_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: n_nitrification_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: no_soil_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN)  :: qbase_l_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN)  :: sthf_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN)  :: sthu_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN)  :: tsoil_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(IN)  :: wflux_driver(land_pts,nsoilt,sm_levels)
REAL(KIND=real_jlslsm), INTENT(OUT) :: soil_c_add(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: soil_n_add(land_pts)

!ancil_info (IN)
LOGICAL, INTENT(IN) :: l_lice_point(land_pts)
LOGICAL, INTENT(IN OUT) :: l_lice_surft(ntype)
INTEGER, INTENT(IN) ::  soil_index(land_pts)
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype


!Local variables

INTEGER ::                                                                     &
  l,    trif_pts, phenol_call, triffid_call, nstep_trif,                       &
#if !defined(UM_JULES)
  m,                                                                           &
#endif
  trif_index(land_pts)

REAL(KIND=real_jlslsm) ::                                                      &
#if !defined(UM_JULES)
  frac_surft_start(land_pts,ntype),                                            &
         ! Fractions of surface types at the start of the timestep.
#endif
  frac_vs(land_pts)
        ! Fraction of gridbox covered by veg or soil.

CHARACTER(LEN=errormessagelength) :: cmessage
INTEGER                           :: errorstatus

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='VEG_CONTROL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! initialize carbon conservation diagnostics
! otherwise they can be non-zero on non-triffid timesteps
IF ( l_triffid ) THEN
  DO l = 1, land_pts
    trif_vars%cnsrv_carbon_veg2_gb(l)      = 0.0
    trif_vars%cnsrv_carbon_triffid_gb(l)   = 0.0
    trif_vars%cnsrv_veg_triffid_gb(l)      = 0.0
    trif_vars%cnsrv_soil_triffid_gb(l)     = 0.0
    trif_vars%cnsrv_prod_triffid_gb(l)     = 0.0
    trif_vars%cnsrv_nitrogen_triffid_gb(l) = 0.0
    trif_vars%cnsrv_vegN_triffid_gb(l)     = 0.0
    trif_vars%cnsrv_soilN_triffid_gb(l)    = 0.0
    trif_vars%cnsrv_N_inorg_triffid_gb(l)  = 0.0
  END DO
END IF

!Determine triggering for the two veg schemes.
!There is a subtle difference between standalone and UM behaviour
phenol_call  = 1
triffid_call = 1

IF (l_phenol) THEN
#if defined(UM_JULES)
  phenol_call = MOD(REAL(a_step),                                              &
                    (REAL(phenol_period) * (rsec_per_day / timestep)))
#else
  phenol_call = MOD (a_step,                                                   &
                          phenol_period * isec_per_day / timestep_len)
#endif
END IF

IF (l_triffid) THEN
#if defined(UM_JULES)
  nstep_trif = INT(rsec_per_day *       triffid_period / timestep)
#else
  nstep_trif = INT(rsec_per_day * REAL(triffid_period) / timestep_real )
#endif
  IF ( asteps_since_triffid == nstep_trif ) THEN
    triffid_call = 0
  END IF
END IF


#if defined(UM_JULES)

!Call to veg 1 or 2 as appropriate
IF ((phenol_call == 0) .OR. (triffid_call == 0)) THEN
  SELECT CASE ( i_veg_vn )
  CASE ( i_veg_vn_2b )
    !     ! Find total fraction of gridbox covered by vegetation and soil, and
    !     !  use this to set indices of land points on which TRIFFID may operate.

    CALL get_veg_soil_index( land_pts, frac_surft, trif_pts,                   &
                            trif_index, frac_vs )

    ! Running with nsoilt > 1 is not compatible with dynamic vegetation, so we
    ! can hard-code the arguments appropriately.
    CALL veg2( land_pts, nsurft, a_step,                                       &
               phenol_period, triffid_period,                                  &
               trif_pts, trif_index, timestep_real,                            &
               frac_agr_gb, frac_past_gb, frac_biocrop_gb, frac_vs,            &
               satcon_soilt(:,1,0), clay_soilt(:,1,:), z0m_soil_gb,            &
               l_phenol, l_triffid, l_trif_eq,                                 &
               asteps_since_triffid,                                           &
               g_leaf_acc_pft, g_leaf_phen_acc_pft, npp_acc_pft,               &
               trifctltype%resp_s_acc_soilt(:,1,:,:), resp_w_acc_pft,          &
               cs_pool_soilt(:,1,:,:), frac_c_label_pool_soilt(:,1,:,:),       &
               frac_surft, lai_pft, canht_pft,                                 &
               catch_snow_surft, catch_surft, infil_surft,                     &
               z0_surft, z0h_bare_surft, c_veg_pft, cv_gb,                     &
               g_leaf_day_pft, g_leaf_phen_pft, g_leaf_dr_out_pft,             &
               lai_phen_pft, lit_c_pft, lit_c_mn_gb, npp_dr_out_pft,           &
               resp_w_dr_out_pft, trifctltype%resp_s_dr_out_gb,                &
               !New arguments replacing USE statements
               !crop_vars_mod (IN)
               rootc_cpft, harvc_cpft, reservec_cpft, stemc_diag_cpft,         &
               leafc_diag_cpft, dvi_cpft,                                      &
               ! prognostics (IN)
               wood_prod_fast_gb, wood_prod_med_gb,                            &
               wood_prod_slow_gb, frac_agr_prev_gb,                            &
               frac_past_prev_gb, frac_biocrop_prev_gb,                        &
               n_inorg_gb, n_inorg_soilt_lyrs,                                 &
               n_inorg_avail_pft, ns_pool_gb,                                  &
               triffid_co2_gb, t_soil_soilt_acc, years_since_harvest,          &
               !p_s_parms
               sthu_soilt,                                                     &
               ! soil_ecosse_vars_mod
               n_soil_pool_soilt, dim_soil_n_pool,                             &
               !ancil_info (IN)
               l_lice_point, l_lice_surft,                                     &
               !urban_param
               urban_param%ztm_gb,                                             &
               ! TYPES containing field data
               trif_vars)


  CASE ( i_veg_vn_1b )
    CALL veg1( land_pts, nsurft, a_step, phenol_period, timestep_real,         &
               satcon_soilt(:,1,0), z0m_soil_gb, l_phenol,                     &
               g_leaf_acc_pft, g_leaf_phen_acc_pft, frac_surft, lai_pft,       &
               canht_pft,                                                      &
               catch_snow_surft, catch_surft, infil_surft,                     &
               g_leaf_day_pft, g_leaf_phen_pft,                                &
               lai_phen_pft, z0_surft, z0h_bare_surft, l_lice_point,           &
               l_lice_surft, urban_param%ztm_gb)

  CASE DEFAULT ! i_veg_vn
    errorstatus = 10
    WRITE (cmessage,'(A,A,I6)') 'Vegetation scheme version value',             &
           'i_veg_vn = ',i_veg_vn
    CALL Ereport ('VEG_CTL', errorstatus, cmessage)

  END SELECT ! i_veg_vn
END IF

#else

IF ( l_triffid ) THEN

  IF ( triffid_call == 0 .OR. soil_bgc_model == soil_model_ecosse ) THEN

    ! Find total fraction of gridbox covered by vegetation and soil, and use
    ! this to set indices of land points on which TRIFFID may operate.
    ! We also do this if ECOSSE is used so the veg and soil models operate
    ! on the same set of points.
    ! Note: This code is essentially a repeat of code in veg_control. That
    ! subroutine is expected to eventually also be used for standalone
    ! JULES, but in the meanwhile we also calculate trif_pts here.
    CALL get_veg_soil_index( land_pts, frac_surft, trif_pts,                   &
                            trif_index, frac_vs )
  END IF  !  triffid_call OR ecosse
END IF  !  l_triffid

! Save frac at start of timestep (for ECOSSE).
frac_surft_start(:,:) = frac_surft(:,:)

IF ( triffid_call == 0 ) THEN
  !     Run includes dynamic vegetation
  !
  ! Running with nsoilt > 1 is not compatible with dynamic vegetation, so we
  ! can hard-code the arguments appropriately by setting m = 1

  m = 1
  CALL veg2( land_pts, nsurft, a_step                                          &
            ,phenol_period, triffid_period                                     &
            ,trif_pts, trif_index, timestep_real                               &
            ,frac_agr_gb, frac_past_gb, frac_biocrop_gb, frac_vs               &
            ,satcon_soilt(:,:,0), clay_soilt(:,m,:), z0m_soil_gb               &
            ,l_phenol, l_triffid, l_trif_eq                                    &
            ,asteps_since_triffid                                              &
            ,g_leaf_acc_pft, g_leaf_phen_acc_pft, npp_acc_pft                  &
            ,trifctltype%resp_s_acc_soilt(:,m,:,:), resp_w_acc_pft             &
            ,cs_pool_soilt(:,m,:,:), frac_c_label_pool_soilt(:,m,:,:)          &
            ,frac_surft, lai_pft, canht_pft                                    &
            ,catch_snow_surft, catch_surft, infil_surft                        &
            ,z0_surft, z0h_bare_surft, c_veg_pft, cv_gb                        &
            ,g_leaf_day_pft, g_leaf_phen_pft, g_leaf_dr_out_pft                &
            ,lai_phen_pft, lit_c_pft, lit_c_mn_gb, npp_dr_out_pft              &
            ,resp_w_dr_out_pft, trifctltype%resp_s_dr_out_gb,                  &
            !New arguments replacing USE statements
            !crop_vars_mod (IN)
            rootc_cpft, harvc_cpft, reservec_cpft, stemc_diag_cpft,            &
            leafc_diag_cpft, dvi_cpft,                                         &
            ! prognostics (IN)
            wood_prod_fast_gb, wood_prod_med_gb,                               &
            wood_prod_slow_gb, frac_agr_prev_gb,                               &
            frac_past_prev_gb, frac_biocrop_prev_gb,                           &
            n_inorg_gb, n_inorg_soilt_lyrs,                                    &
            n_inorg_avail_pft, ns_pool_gb,                                     &
            triffid_co2_gb, t_soil_soilt_acc, years_since_harvest,             &
            !p_s_parms
            sthu_soilt,                                                        &
            ! soil_ecosse_vars_mod
            n_soil_pool_soilt, dim_soil_n_pool,                                &
            !ancil_info (IN)
            l_lice_point, l_lice_surft,                                        &
            !urban_params
            urban_param%ztm_gb,                                                &
            ! TYPES containing field data
            trif_vars)
ELSE

  IF ( phenol_call == 0 ) THEN
    ! Run includes phenology,  but not dynamic vegetation
    ! therefore call veg1 rather than veg2
    CALL veg1( land_pts, nsurft, a_step, phenol_period, timestep_real          &
              ,satcon_soilt(:,:,0), z0m_soil_gb, l_phenol                      &
              ,g_leaf_acc_pft, g_leaf_phen_acc_pft, frac_surft, lai_pft        &
              ,canht_pft, catch_snow_surft, catch_surft, infil_surft           &
              ,g_leaf_day_pft, g_leaf_phen_pft                                 &
              ,lai_phen_pft, z0_surft, z0h_bare_surft, l_lice_point            &
              ,l_lice_surft,urban_param%ztm_gb )
  END IF

END IF  !  triffid_call
#endif


#if !defined(UM_JULES)
!Soil biogeochemistry (Standalone only)
! At present this only deals with the ECOSSE model of soil C and N.
IF ( soil_bgc_model == soil_model_ecosse ) THEN
  CALL soil_biogeochem_control( land_pts, triffid_call, trif_pts,              &
        trif_index, trif_vars%deposition_n_gb, frac_surft_start,               &
        qbase_l_soilt, sthf_soilt, sthu_soilt, w_flux_soilt, t_soil_soilt,     &
    ! These arguments replace USE statements
       ! p_s_params
       bexp_soilt, clay_soilt, sathh_soilt, smvcst_soilt,                      &
       smvcwt_soilt, soil_ph_soilt,                                            &
        ! p_s_params_alloc
       dim_cslayer,                                                            &
       ! soil_ecosse_vars_mod
       co2_soil_gb, n2o_soil_gb, n2o_denitrif_gb,                              &
       n2o_nitrif_gb, n2o_partial_nitrif_gb,                                   &
       n2_denitrif_gb, n_denitrification_gb,                                   &
       n_leach_amm_gb, n_leach_nit_gb,                                         &
       n_nitrification_gb, n_soil_pool_soilt,                                  &
       no_soil_gb,                                                             &
       soil_c_add, soil_n_add,                                                 &
       !trif_vars_mod
       trif_vars%lit_c_orig_pft, trif_vars%lit_n_orig_pft,                     &
       trif_vars%minl_n_gb, trif_vars%minl_n_pot_gb,                           &
       trif_vars%immob_n_gb, trif_vars%immob_n_pot_gb,                         &
       trif_vars%resp_s_diag_gb, trif_vars%resp_s_pot_diag_gb ,                &
       trif_vars%fn_gb, trif_vars%n_fertiliser_add, trif_vars%n_fix_add,       &
       trif_vars%n_uptake_extract,                                             &
       ! prognostics
       cs_pool_soilt,                                                          &
       !ancil_info
       soil_index,                                                             &
       ! TYPES
       soilecosse)
END IF
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE veg_control
END MODULE veg_control_mod
