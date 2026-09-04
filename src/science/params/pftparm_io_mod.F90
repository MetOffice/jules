! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! This module contains variables used for reading in pftparm data
! and initialisations

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE pftparm_io

USE max_dimensions, ONLY:                                                      &
  npft_max
USE missing_data_mod, ONLY: imdi, rmdi
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!---------------------------------------------------------------------
! Set up variables to use in IO (a fixed size version of each array
! in pftparm that we want to initialise).
!---------------------------------------------------------------------
#if !defined(UM_JULES)
INTEGER ::                                                                     &
  fsmc_mod_io(npft_max) = imdi

REAL(KIND=real_jlslsm) ::                                                      &
  canht_ft_io(npft_max) = rmdi,                                                &
  lai_io(npft_max) = rmdi,                                                     &
  psi_close_io(npft_max) = rmdi,                                               &
  psi_open_io(npft_max) = rmdi
#endif

INTEGER ::                                                                     &
  c3_io(npft_max) = imdi,                                                      &
  irrig_pft_io(npft_max) = imdi,                                               &
  orient_io(npft_max) = imdi

REAL(KIND=real_jlslsm) ::                                                      &
  a_wl_io(npft_max) = rmdi,                                                    &
  a_ws_io(npft_max) = rmdi,                                                    &
  act_jmax_io(npft_max) = rmdi,                                                &
  act_vcmax_io(npft_max) = rmdi,                                               &
  aef_io(npft_max) = rmdi,                                                     &
  albsnc_max_io(npft_max) = rmdi,                                              &
  albsnc_min_io(npft_max) = rmdi,                                              &
  albsnf_max_io(npft_max) = rmdi,                                              &
  albsnf_maxl_io(npft_max) = rmdi,                                             &
  albsnf_maxu_io(npft_max) = rmdi,                                             &
  alpha_io(npft_max) = rmdi,                                                   &
  alpha_elec_io(npft_max) = rmdi,                                              &
  alnir_io(npft_max) = rmdi,                                                   &
  alnirl_io(npft_max) = rmdi,                                                  &
  alniru_io(npft_max) = rmdi,                                                  &
  alpar_io(npft_max) = rmdi,                                                   &
  alparl_io(npft_max) = rmdi,                                                  &
  alparu_io(npft_max) = rmdi,                                                  &
  avg_ba_io(npft_max) = rmdi,                                                  &
  b_wl_io(npft_max) = rmdi,                                                    &
  can_struct_a_io(npft_max) = rmdi,                                            &
  catch0_io(npft_max) = rmdi,                                                  &
  ccleaf_min_io(npft_max) = rmdi,                                              &
  ccleaf_max_io(npft_max) = rmdi,                                              &
  ccwood_min_io(npft_max) = rmdi,                                              &
  ccwood_max_io(npft_max) = rmdi,                                              &
  ci_st_io(npft_max) = rmdi,                                                   &
  dcatch_dlai_io(npft_max) = rmdi,                                             &
  deact_jmax_io(npft_max) = rmdi,                                              &
  deact_vcmax_io(npft_max) = rmdi,                                             &
  dfp_dcuo_io(npft_max) = rmdi,                                                &
  dgl_dm_io(npft_max) = rmdi,                                                  &
  dgl_dt_io(npft_max) = rmdi,                                                  &
  dqcrit_io(npft_max) = rmdi,                                                  &
  ds_jmax_io(npft_max) = rmdi,                                                 &
  ds_vcmax_io(npft_max) = rmdi,                                                &
  dust_veg_scj_io(npft_max) = rmdi,                                            &
  dz0v_dh_io(npft_max) = rmdi,                                                 &
  emis_pft_io(npft_max) = rmdi,                                                &
  eta_sl_io(npft_max) = rmdi,                                                  &
  f0_io(npft_max) = rmdi,                                                      &
  fef_bc_io(npft_max) = rmdi,                                                  &
  fef_c2h4_io(npft_max) = rmdi,                                                &
  fef_c2h6_io(npft_max) = rmdi,                                                &
  fef_c3h8_io(npft_max) = rmdi,                                                &
  fef_ch4_io(npft_max) = rmdi,                                                 &
  fef_co_io(npft_max) = rmdi,                                                  &
  fef_co2_io(npft_max) = rmdi,                                                 &
  fef_dms_io(npft_max) = rmdi,                                                 &
  fef_hcho_io(npft_max) = rmdi,                                                &
  fef_mecho_io(npft_max) = rmdi,                                               &
  fef_nh3_io(npft_max) = rmdi,                                                 &
  fef_nox_io(npft_max) = rmdi,                                                 &
  fef_oc_io(npft_max) = rmdi,                                                  &
  fef_so2_io(npft_max) = rmdi,                                                 &
  fd_io(npft_max) = rmdi,                                                      &
  fire_mort_io(npft_max) = rmdi,                                               &
  fireveg_c_to_atmos_io(npft_max) = rmdi,                                      &
  fl_o3_ct_io(npft_max) = rmdi,                                                &
  fsmc_of_io(npft_max) = rmdi,                                                 &
  fsmc_p0_io(npft_max) = rmdi,                                                 &
  sug_g0_io(npft_max) = rmdi,                                                  &
  g1_stomata_io(npft_max) = rmdi,                                              &
  g_leaf_0_io(npft_max) = rmdi,                                                &
  glmin_io(npft_max) = rmdi,                                                   &
  gpp_st_io(npft_max) = rmdi,                                                  &
  sug_grec_io(npft_max) = rmdi,                                                &
  gsoil_f_io(npft_max) = rmdi,                                                 &
  hw_sw_io(npft_max) = rmdi,                                                   &
  ief_io(npft_max) = rmdi,                                                     &
  infil_f_io(npft_max) = rmdi,                                                 &
  jv25_ratio_io(npft_max) = rmdi,                                              &
  kext_io(npft_max) = rmdi,                                                    &
  kn_io(npft_max) = rmdi,                                                      &
  knl_io(npft_max) = rmdi,                                                     &
  kpar_io(npft_max) = rmdi,                                                    &
  lai_alb_lim_io(npft_max) = rmdi,                                             &
  lma_io(npft_max) = rmdi,                                                     &
  mef_io(npft_max) = rmdi,                                                     &
  neff_io(npft_max) = rmdi,                                                    &
  nl0_io(npft_max) = rmdi,                                                     &
  nmass_io(npft_max) = rmdi,                                                   &
  nr_nl_io(npft_max) = rmdi,                                                   &
  ns_nl_io(npft_max) = rmdi,                                                   &
  nsw_io(npft_max) = rmdi,                                                     &
  nr_io(npft_max) = rmdi,                                                      &
  omega_io(npft_max) = rmdi,                                                   &
  omegal_io(npft_max) = rmdi,                                                  &
  omegau_io(npft_max) = rmdi,                                                  &
  omnir_io(npft_max) = rmdi,                                                   &
  omnirl_io(npft_max) = rmdi,                                                  &
  omniru_io(npft_max) = rmdi,                                                  &
  q10_leaf_io(npft_max) = rmdi,                                                &
  r_grow_io(npft_max) = rmdi,                                                  &
  rootd_ft_io(npft_max) = rmdi,                                                &
  sigl_io(npft_max) = rmdi,                                                    &
  tef_io(npft_max) = rmdi,                                                     &
  tleaf_of_io(npft_max) = rmdi,                                                &
  tlow_io(npft_max) = rmdi,                                                    &
  tupp_io(npft_max) = rmdi,                                                    &
  vint_io(npft_max) = rmdi,                                                    &
  vsl_io(npft_max) = rmdi,                                                     &
  sug_yg_io(npft_max) = rmdi,                                                  &
  z0hm_pft_io(npft_max) = rmdi,                                                &
  z0hm_classic_pft_io(npft_max) = rmdi,                                        &
  z0v_io(npft_max) = rmdi,                                                     &
  sox_a_io(npft_max) = rmdi,                                                   &
  sox_p50_io(npft_max) = rmdi,                                                 &
  sox_rp_min_io(npft_max) = rmdi
!---------------------------------------------------------------------
! Set up a namelist for reading and writing these arrays
!---------------------------------------------------------------------
NAMELIST  / jules_pftparm/                                                     &
#if !defined(UM_JULES)
  canht_ft_io,     lai_io,                                                     &
  fsmc_mod_io,     psi_close_io,     psi_open_io,                              &
#endif
  a_wl_io,         a_ws_io,          aef_io,                                   &
  act_jmax_io,     act_vcmax_io,     albsnc_max_io,                            &
  albsnc_min_io,   albsnf_max_io,    albsnf_maxl_io,                           &
  albsnf_maxu_io,  alpha_io,         alpha_elec_io,                            &
  alnir_io,        alnirl_io,        alniru_io,                                &
  alpar_io,        alparl_io,        alparu_io,                                &
  avg_ba_io,       b_wl_io,          c3_io,                                    &
  can_struct_a_io, catch0_io,        ccleaf_max_io,                            &
  ccleaf_min_io,   ccwood_max_io,    ccwood_min_io,                            &
  ci_st_io,        dcatch_dlai_io,   deact_jmax_io,                            &
  deact_vcmax_io,  dfp_dcuo_io,      dgl_dm_io,                                &
  dgl_dt_io,       dqcrit_io,        ds_jmax_io,                               &
  ds_vcmax_io,     dust_veg_scj_io,  dz0v_dh_io,                               &
  emis_pft_io,     eta_sl_io,        f0_io,                                    &
  fef_bc_io,       fef_ch4_io,       fef_co_io,                                &
  fef_co2_io,      fef_nox_io,       fef_oc_io,                                &
  fef_c2h4_io,     fef_c2h6_io,      fef_c3h8_io,                              &
  fef_hcho_io,     fef_mecho_io,                                               &
  fef_nh3_io,      fef_dms_io,                                                 &
  fef_so2_io,      fd_io,            fire_mort_io,                             &
  fl_o3_ct_io,     fsmc_of_io,       fsmc_p0_io,                               &
  sug_g0_io,       g1_stomata_io,    g_leaf_0_io,                              &
  glmin_io,        gpp_st_io,        sug_grec_io,                              &
  gsoil_f_io,      hw_sw_io,         ief_io,                                   &
  infil_f_io,      irrig_pft_io,     jv25_ratio_io,                            &
  kext_io,         kn_io,            knl_io,                                   &
  kpar_io,         lai_alb_lim_io,   lma_io,                                   &
  mef_io,          neff_io,          nl0_io,                                   &
  nmass_io,        nr_io,            nr_nl_io,                                 &
  ns_nl_io,        nsw_io,           omega_io,                                 &
  omegal_io,       omegau_io,        omnir_io,                                 &
  omnirl_io,       omniru_io,        orient_io,                                &
  q10_leaf_io,     r_grow_io,        rootd_ft_io,                              &
  sigl_io,         tef_io,           tleaf_of_io,                              &
  tlow_io,         tupp_io,          vint_io,                                  &
  vsl_io,          sug_yg_io,        z0hm_pft_io,                              &
  z0hm_classic_pft_io, z0v_io,       sox_a_io,                                 &
  sox_p50_io,      sox_rp_min_io,    fireveg_c_to_atmos_io

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PFTPARM_IO'

CONTAINS

#if defined(UM_JULES)
SUBROUTINE read_nml_jules_pftparm (unitnumber)

! Description:
!  Read the JULES_PFTPARM namelist

USE setup_namelist, ONLY: setup_nml_type
USE check_iostat_mod, ONLY:  check_iostat
USE UM_parcore,       ONLY:  mype
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE errormessagelength_mod, ONLY: errormessagelength
IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_PFTPARM'
INTEGER(KIND=jpim), PARAMETER          :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER          :: zhook_out = 1
CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_int = 3 * npft_max
INTEGER, PARAMETER :: n_real = 109 * npft_max

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: c3_io(npft_max)
  INTEGER :: irrig_pft_io(npft_max)
  INTEGER :: orient_io(npft_max)
  REAL(KIND=real_jlslsm) :: a_wl_io(npft_max)
  REAL(KIND=real_jlslsm) :: a_ws_io(npft_max)
  REAL(KIND=real_jlslsm) :: act_jmax_io(npft_max)
  REAL(KIND=real_jlslsm) :: act_vcmax_io(npft_max)
  REAL(KIND=real_jlslsm) :: aef_io(npft_max)
  REAL(KIND=real_jlslsm) :: albsnc_max_io(npft_max)
  REAL(KIND=real_jlslsm) :: albsnc_min_io(npft_max)
  REAL(KIND=real_jlslsm) :: albsnf_max_io(npft_max)
  REAL(KIND=real_jlslsm) :: albsnf_maxl_io(npft_max)
  REAL(KIND=real_jlslsm) :: albsnf_maxu_io(npft_max)
  REAL(KIND=real_jlslsm) :: alpha_io(npft_max)
  REAL(KIND=real_jlslsm) :: alpha_elec_io(npft_max)
  REAL(KIND=real_jlslsm) :: alnir_io(npft_max)
  REAL(KIND=real_jlslsm) :: alnirl_io(npft_max)
  REAL(KIND=real_jlslsm) :: alniru_io(npft_max)
  REAL(KIND=real_jlslsm) :: alpar_io(npft_max)
  REAL(KIND=real_jlslsm) :: alparl_io(npft_max)
  REAL(KIND=real_jlslsm) :: alparu_io(npft_max)
  REAL(KIND=real_jlslsm) :: avg_ba_io(npft_max)
  REAL(KIND=real_jlslsm) :: b_wl_io(npft_max)
  REAL(KIND=real_jlslsm) :: can_struct_a_io(npft_max)
  REAL(KIND=real_jlslsm) :: catch0_io(npft_max)
  REAL(KIND=real_jlslsm) :: ccleaf_min_io(npft_max)
  REAL(KIND=real_jlslsm) :: ccleaf_max_io(npft_max)
  REAL(KIND=real_jlslsm) :: ccwood_min_io(npft_max)
  REAL(KIND=real_jlslsm) :: ccwood_max_io(npft_max)
  REAL(KIND=real_jlslsm) :: ci_st_io(npft_max)
  REAL(KIND=real_jlslsm) :: dcatch_dlai_io(npft_max)
  REAL(KIND=real_jlslsm) :: deact_jmax_io(npft_max)
  REAL(KIND=real_jlslsm) :: deact_vcmax_io(npft_max)
  REAL(KIND=real_jlslsm) :: dfp_dcuo_io(npft_max)
  REAL(KIND=real_jlslsm) :: dgl_dm_io(npft_max)
  REAL(KIND=real_jlslsm) :: dgl_dt_io(npft_max)
  REAL(KIND=real_jlslsm) :: dqcrit_io(npft_max)
  REAL(KIND=real_jlslsm) :: ds_jmax_io(npft_max)
  REAL(KIND=real_jlslsm) :: ds_vcmax_io(npft_max)
  REAL(KIND=real_jlslsm) :: dust_veg_scj_io(npft_max)
  REAL(KIND=real_jlslsm) :: dz0v_dh_io(npft_max)
  REAL(KIND=real_jlslsm) :: emis_pft_io(npft_max)
  REAL(KIND=real_jlslsm) :: eta_sl_io(npft_max)
  REAL(KIND=real_jlslsm) :: f0_io(npft_max)
  REAL(KIND=real_jlslsm) :: fd_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_bc_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_ch4_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_co_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_co2_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_nox_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_oc_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_so2_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_c2h4_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_c2h6_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_c3h8_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_hcho_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_mecho_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_nh3_io(npft_max)
  REAL(KIND=real_jlslsm) :: fef_dms_io(npft_max)
  REAL(KIND=real_jlslsm) :: fire_mort_io(npft_max)
  REAL(KIND=real_jlslsm) :: fireveg_c_to_atmos_io(npft_max)
  REAL(KIND=real_jlslsm) :: fl_o3_ct_io(npft_max)
  REAL(KIND=real_jlslsm) :: fsmc_of_io(npft_max)
  REAL(KIND=real_jlslsm) :: fsmc_p0_io(npft_max)
  REAL(KIND=real_jlslsm) :: sug_g0_io(npft_max)
  REAL(KIND=real_jlslsm) :: g1_stomata_io(npft_max)
  REAL(KIND=real_jlslsm) :: g_leaf_0_io(npft_max)
  REAL(KIND=real_jlslsm) :: glmin_io(npft_max)
  REAL(KIND=real_jlslsm) :: gpp_st_io(npft_max)
  REAL(KIND=real_jlslsm) :: sug_grec_io(npft_max)
  REAL(KIND=real_jlslsm) :: gsoil_f_io(npft_max)
  REAL(KIND=real_jlslsm) :: hw_sw_io(npft_max)
  REAL(KIND=real_jlslsm) :: ief_io(npft_max)
  REAL(KIND=real_jlslsm) :: infil_f_io(npft_max)
  REAL(KIND=real_jlslsm) :: jv25_ratio_io(npft_max)
  REAL(KIND=real_jlslsm) :: kext_io(npft_max)
  REAL(KIND=real_jlslsm) :: kn_io(npft_max)
  REAL(KIND=real_jlslsm) :: knl_io(npft_max)
  REAL(KIND=real_jlslsm) :: kpar_io(npft_max)
  REAL(KIND=real_jlslsm) :: lai_alb_lim_io(npft_max)
  REAL(KIND=real_jlslsm) :: lma_io(npft_max)
  REAL(KIND=real_jlslsm) :: mef_io(npft_max)
  REAL(KIND=real_jlslsm) :: neff_io(npft_max)
  REAL(KIND=real_jlslsm) :: nl0_io(npft_max)
  REAL(KIND=real_jlslsm) :: nmass_io(npft_max)
  REAL(KIND=real_jlslsm) :: nr_io(npft_max)
  REAL(KIND=real_jlslsm) :: nr_nl_io(npft_max)
  REAL(KIND=real_jlslsm) :: ns_nl_io(npft_max)
  REAL(KIND=real_jlslsm) :: nsw_io(npft_max)
  REAL(KIND=real_jlslsm) :: omega_io(npft_max)
  REAL(KIND=real_jlslsm) :: omegal_io(npft_max)
  REAL(KIND=real_jlslsm) :: omegau_io(npft_max)
  REAL(KIND=real_jlslsm) :: omnir_io(npft_max)
  REAL(KIND=real_jlslsm) :: omnirl_io(npft_max)
  REAL(KIND=real_jlslsm) :: omniru_io(npft_max)
  REAL(KIND=real_jlslsm) :: q10_leaf_io(npft_max)
  REAL(KIND=real_jlslsm) :: r_grow_io(npft_max)
  REAL(KIND=real_jlslsm) :: rootd_ft_io(npft_max)
  REAL(KIND=real_jlslsm) :: sigl_io(npft_max)
  REAL(KIND=real_jlslsm) :: tef_io(npft_max)
  REAL(KIND=real_jlslsm) :: tleaf_of_io(npft_max)
  REAL(KIND=real_jlslsm) :: tlow_io(npft_max)
  REAL(KIND=real_jlslsm) :: tupp_io(npft_max)
  REAL(KIND=real_jlslsm) :: vint_io(npft_max)
  REAL(KIND=real_jlslsm) :: vsl_io(npft_max)
  REAL(KIND=real_jlslsm) :: sug_yg_io(npft_max)
  REAL(KIND=real_jlslsm) :: z0hm_pft_io(npft_max)
  REAL(KIND=real_jlslsm) :: z0hm_classic_pft_io(npft_max)
  REAL(KIND=real_jlslsm) :: z0v_io(npft_max)
  REAL(KIND=real_jlslsm) :: sox_a_io(npft_max)
  REAL(KIND=real_jlslsm) :: sox_p50_io(npft_max)
  REAL(KIND=real_jlslsm) :: sox_rp_min_io(npft_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_pftparm, IOSTAT = errorstatus,          &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_pftparm",                     &
           iomessage)

  my_nml % a_wl_io        = a_wl_io
  my_nml % a_ws_io        = a_ws_io
  my_nml % act_jmax_io    = act_jmax_io
  my_nml % act_vcmax_io   = act_vcmax_io
  my_nml % aef_io         = aef_io
  my_nml % albsnc_max_io  = albsnc_max_io
  my_nml % albsnc_min_io  = albsnc_min_io
  my_nml % albsnf_max_io  = albsnf_max_io
  my_nml % albsnf_maxl_io = albsnf_maxl_io
  my_nml % albsnf_maxu_io = albsnf_maxu_io
  my_nml % alpha_io       = alpha_io
  my_nml % alpha_elec_io  = alpha_elec_io
  my_nml % alnir_io       = alnir_io
  my_nml % alnirl_io      = alnirl_io
  my_nml % alniru_io      = alniru_io
  my_nml % alpar_io       = alpar_io
  my_nml % alparl_io      = alparl_io
  my_nml % alparu_io      = alparu_io
  my_nml % avg_ba_io      = avg_ba_io
  my_nml % b_wl_io        = b_wl_io
  my_nml % c3_io          = c3_io
  my_nml % can_struct_a_io = can_struct_a_io
  my_nml % catch0_io      = catch0_io
  my_nml % ccleaf_min_io  = ccleaf_min_io
  my_nml % ccleaf_max_io  = ccleaf_max_io
  my_nml % ccwood_min_io  = ccwood_min_io
  my_nml % ccwood_max_io  = ccwood_max_io
  my_nml % ci_st_io       = ci_st_io
  my_nml % dcatch_dlai_io = dcatch_dlai_io
  my_nml % deact_jmax_io  = deact_jmax_io
  my_nml % deact_vcmax_io = deact_vcmax_io
  my_nml % dfp_dcuo_io    = dfp_dcuo_io
  my_nml % dgl_dm_io      = dgl_dm_io
  my_nml % dgl_dt_io      = dgl_dt_io
  my_nml % dqcrit_io      = dqcrit_io
  my_nml % ds_jmax_io     = ds_jmax_io
  my_nml % ds_vcmax_io    = ds_vcmax_io
  my_nml % dust_veg_scj_io = dust_veg_scj_io
  my_nml % dz0v_dh_io     = dz0v_dh_io
  my_nml % emis_pft_io    = emis_pft_io
  my_nml % eta_sl_io      = eta_sl_io
  my_nml % f0_io          = f0_io
  my_nml % fd_io          = fd_io
  my_nml % fef_bc_io      = fef_bc_io
  my_nml % fef_ch4_io     = fef_ch4_io
  my_nml % fef_co_io      = fef_co_io
  my_nml % fef_co2_io     = fef_co2_io
  my_nml % fef_nox_io     = fef_nox_io
  my_nml % fef_oc_io      = fef_oc_io
  my_nml % fef_so2_io     = fef_so2_io
  my_nml % fef_c2h4_io    = fef_c2h4_io
  my_nml % fef_c2h6_io    = fef_c2h6_io
  my_nml % fef_c3h8_io    = fef_c3h8_io
  my_nml % fef_hcho_io    = fef_hcho_io
  my_nml % fef_mecho_io   = fef_mecho_io
  my_nml % fef_nh3_io     = fef_nh3_io
  my_nml % fef_dms_io     = fef_dms_io
  my_nml % fire_mort_io   = fire_mort_io
  my_nml % fireveg_c_to_atmos_io = fireveg_c_to_atmos_io
  my_nml % fl_o3_ct_io    = fl_o3_ct_io
  my_nml % fsmc_of_io     = fsmc_of_io
  my_nml % fsmc_p0_io     = fsmc_p0_io
  my_nml % sug_g0_io      = sug_g0_io
  my_nml % g1_stomata_io  = g1_stomata_io
  my_nml % g_leaf_0_io    = g_leaf_0_io
  my_nml % glmin_io       = glmin_io
  my_nml % gpp_st_io      = gpp_st_io
  my_nml % sug_grec_io    = sug_grec_io
  my_nml % gsoil_f_io     = gsoil_f_io
  my_nml % hw_sw_io       = hw_sw_io
  my_nml % ief_io         = ief_io
  my_nml % infil_f_io     = infil_f_io
  my_nml % irrig_pft_io   = irrig_pft_io
  my_nml % jv25_ratio_io  = jv25_ratio_io
  my_nml % kext_io        = kext_io
  my_nml % kn_io          = kn_io
  my_nml % knl_io         = knl_io
  my_nml % kpar_io        = kpar_io
  my_nml % lai_alb_lim_io = lai_alb_lim_io
  my_nml % lma_io         = lma_io
  my_nml % mef_io         = mef_io
  my_nml % neff_io        = neff_io
  my_nml % nl0_io         = nl0_io
  my_nml % nmass_io       = nmass_io
  my_nml % nr_io          = nr_io
  my_nml % nr_nl_io       = nr_nl_io
  my_nml % ns_nl_io       = ns_nl_io
  my_nml % nsw_io         = nsw_io
  my_nml % omega_io       = omega_io
  my_nml % omegal_io      = omegal_io
  my_nml % omegau_io      = omegau_io
  my_nml % omnir_io       = omnir_io
  my_nml % omnirl_io      = omnirl_io
  my_nml % omniru_io      = omniru_io
  my_nml % orient_io      = orient_io
  my_nml % q10_leaf_io    = q10_leaf_io
  my_nml % r_grow_io      = r_grow_io
  my_nml % rootd_ft_io    = rootd_ft_io
  my_nml % sigl_io        = sigl_io
  my_nml % tef_io         = tef_io
  my_nml % tleaf_of_io    = tleaf_of_io
  my_nml % tlow_io        = tlow_io
  my_nml % tupp_io        = tupp_io
  my_nml % vint_io        = vint_io
  my_nml % vsl_io         = vsl_io
  my_nml % sug_yg_io      = sug_yg_io
  my_nml % z0hm_pft_io    = z0hm_pft_io
  my_nml % z0hm_classic_pft_io = z0hm_classic_pft_io
  my_nml % z0v_io         = z0v_io
  my_nml % sox_a_io       = sox_a_io
  my_nml % sox_p50_io     = sox_p50_io
  my_nml % sox_rp_min_io  = sox_rp_min_io
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  a_wl_io         = my_nml % a_wl_io
  a_ws_io         = my_nml % a_ws_io
  act_jmax_io     = my_nml % act_jmax_io
  act_vcmax_io    = my_nml % act_vcmax_io
  aef_io          = my_nml % aef_io
  albsnc_max_io   = my_nml % albsnc_max_io
  albsnc_min_io   = my_nml % albsnc_min_io
  albsnf_max_io   = my_nml % albsnf_max_io
  albsnf_maxl_io  = my_nml % albsnf_maxl_io
  albsnf_maxu_io  = my_nml % albsnf_maxu_io
  alpha_io        = my_nml % alpha_io
  alpha_elec_io   = my_nml % alpha_elec_io
  alnir_io        = my_nml % alnir_io
  alnirl_io       = my_nml % alnirl_io
  alniru_io       = my_nml % alniru_io
  alpar_io        = my_nml % alpar_io
  alparl_io       = my_nml % alparl_io
  alparu_io       = my_nml % alparu_io
  avg_ba_io       = my_nml % avg_ba_io
  b_wl_io         = my_nml % b_wl_io
  c3_io           = my_nml % c3_io
  can_struct_a_io = my_nml % can_struct_a_io
  catch0_io       = my_nml % catch0_io
  ccleaf_min_io   = my_nml % ccleaf_min_io
  ccleaf_max_io   = my_nml % ccleaf_max_io
  ccwood_min_io   = my_nml % ccwood_min_io
  ccwood_max_io   = my_nml % ccwood_max_io
  ci_st_io        = my_nml % ci_st_io
  dcatch_dlai_io  = my_nml % dcatch_dlai_io
  deact_jmax_io   = my_nml % deact_jmax_io
  deact_vcmax_io  = my_nml % deact_vcmax_io
  dfp_dcuo_io     = my_nml % dfp_dcuo_io
  dgl_dm_io       = my_nml % dgl_dm_io
  dgl_dt_io       = my_nml % dgl_dt_io
  dqcrit_io       = my_nml % dqcrit_io
  ds_jmax_io      = my_nml % ds_jmax_io
  ds_vcmax_io     = my_nml % ds_vcmax_io
  dust_veg_scj_io = my_nml % dust_veg_scj_io
  dz0v_dh_io      = my_nml % dz0v_dh_io
  emis_pft_io     = my_nml % emis_pft_io
  eta_sl_io       = my_nml % eta_sl_io
  f0_io           = my_nml % f0_io
  fd_io           = my_nml % fd_io
  fef_bc_io       = my_nml % fef_bc_io
  fef_ch4_io      = my_nml % fef_ch4_io
  fef_co_io       = my_nml % fef_co_io
  fef_co2_io      = my_nml % fef_co2_io
  fef_nox_io      = my_nml % fef_nox_io
  fef_oc_io       = my_nml % fef_oc_io
  fef_so2_io      = my_nml % fef_so2_io
  fef_c2h4_io     = my_nml % fef_c2h4_io
  fef_c2h6_io     = my_nml % fef_c2h6_io
  fef_c3h8_io     = my_nml % fef_c3h8_io
  fef_hcho_io     = my_nml % fef_hcho_io
  fef_mecho_io    = my_nml % fef_mecho_io
  fef_nh3_io      = my_nml % fef_nh3_io
  fef_dms_io      = my_nml % fef_dms_io
  fire_mort_io    = my_nml % fire_mort_io
  fireveg_c_to_atmos_io = my_nml % fireveg_c_to_atmos_io
  fl_o3_ct_io     = my_nml % fl_o3_ct_io
  fsmc_of_io      = my_nml % fsmc_of_io
  fsmc_p0_io      = my_nml % fsmc_p0_io
  g1_stomata_io   = my_nml % g1_stomata_io
  sug_g0_io       = my_nml % sug_g0_io
  g_leaf_0_io     = my_nml % g_leaf_0_io
  glmin_io        = my_nml % glmin_io
  gpp_st_io       = my_nml % gpp_st_io
  sug_grec_io     = my_nml % sug_grec_io
  gsoil_f_io      = my_nml % gsoil_f_io
  hw_sw_io        = my_nml % hw_sw_io
  ief_io          = my_nml % ief_io
  infil_f_io      = my_nml % infil_f_io
  irrig_pft_io    = my_nml % irrig_pft_io
  jv25_ratio_io   = my_nml % jv25_ratio_io
  kext_io         = my_nml % kext_io
  kn_io           = my_nml % kn_io
  knl_io          = my_nml % knl_io
  kpar_io         = my_nml % kpar_io
  lai_alb_lim_io  = my_nml % lai_alb_lim_io
  lma_io          = my_nml % lma_io
  mef_io          = my_nml % mef_io
  neff_io         = my_nml % neff_io
  nl0_io          = my_nml % nl0_io
  nmass_io        = my_nml % nmass_io
  nr_io           = my_nml % nr_io
  nr_nl_io        = my_nml % nr_nl_io
  ns_nl_io        = my_nml % ns_nl_io
  nsw_io          = my_nml % nsw_io
  omega_io        = my_nml % omega_io
  omegal_io       = my_nml % omegal_io
  omegau_io       = my_nml % omegau_io
  omnir_io        = my_nml % omnir_io
  omnirl_io       = my_nml % omnirl_io
  omniru_io       = my_nml % omniru_io
  orient_io       = my_nml % orient_io
  q10_leaf_io     = my_nml % q10_leaf_io
  r_grow_io       = my_nml % r_grow_io
  rootd_ft_io     = my_nml % rootd_ft_io
  sigl_io         = my_nml % sigl_io
  tef_io          = my_nml % tef_io
  tleaf_of_io     = my_nml % tleaf_of_io
  tlow_io         = my_nml % tlow_io
  tupp_io         = my_nml % tupp_io
  vint_io         = my_nml % vint_io
  vsl_io          = my_nml % vsl_io
  sug_yg_io       = my_nml % sug_yg_io
  z0hm_pft_io     = my_nml % z0hm_pft_io
  z0hm_classic_pft_io = my_nml % z0hm_classic_pft_io
  z0v_io          = my_nml % z0v_io
  sox_a_io        = my_nml % sox_a_io
  sox_p50_io      = my_nml % sox_p50_io
  sox_rp_min_io   = my_nml % sox_rp_min_io
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_pftparm
#endif


SUBROUTINE init_pftparm_allocated()

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

USE pftparm, ONLY:                                                             &
! namelist variables:
#if !defined(UM_JULES)
  fsmc_mod,        psi_close,        psi_open,                                 &
#endif
  a_wl,            a_ws,             aef,                                      &
  act_jmax,        act_vcmax,        albsnc_max,                               &
  albsnc_min,      albsnf_max,       albsnf_maxl,                              &
  albsnf_maxu,     alpha,            alpha_elec,                               &
  alnir,           alnirl,           alniru,                                   &
  alpar,           alparl,           alparu,                                   &
  avg_ba,          b_wl,             c3,                                       &
  can_struct_a,    catch0,           ccleaf_max,                               &
  ccleaf_min,      ccwood_max,       ccwood_min,                               &
  ci_st,           dcatch_dlai,      deact_jmax,                               &
  deact_vcmax,     dfp_dcuo,         dgl_dm,                                   &
  dgl_dt,          dqcrit,           ds_jmax,                                  &
  ds_vcmax,        dust_veg_scj,     dz0v_dh,                                  &
  emis_pft,        eta_sl,           f0,                                       &
  fd,              fef_bc,           fef_ch4,                                  &
  fef_co,          fef_co2,          fef_nox,                                  &
  fef_oc,          fef_so2,          fef_c2h4,                                 &
  fef_c2h6,        fef_c3h8,         fef_hcho,                                 &
  fef_mecho,       fef_nh3,                                                    &
  fef_dms,         fire_mort,        fireveg_c_to_atmos,                       &
  fl_o3_ct,        fsmc_of,          fsmc_p0,                                  &
  sug_g0,          g1_stomata,       g_leaf_0,                                 &
  glmin,           gpp_st,           sug_grec,                                 &
  gsoil_f,         hw_sw,            ief,                                      &
  infil_f,         jv25_ratio,       kext,                                     &
  kn,              knl,              kpar,                                     &
  lai_alb_lim,     lma,              mef,                                      &
  neff,            nl0,              nmass,                                    &
  nr,              nr_nl,            ns_nl,                                    &
  nsw,             omega,            omegal,                                   &
  omegau,          omnir,            omnirl,                                   &
  omniru,          orient,           q10_leaf,                                 &
  r_grow,          rootd_ft,         sigl,                                     &
  tef,             tleaf_of,         tlow,                                     &
  tupp,            vint,             vsl,                                      &
  sug_yg,          z0v,              sox_a,                                    &
  sox_p50,         sox_rp_min

USE c_irrigation_mod, ONLY: irrig_tile
USE c_z0h_z0m,    ONLY: z0h_z0m,  z0h_z0m_classic

USE jules_surface_types_mod, ONLY: npft

IMPLICIT NONE

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_PFTPARM_ALLOCATED'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Radiation and albedo parameters.
albsnc_max(:)   = albsnc_max_io(1:npft)
albsnc_min(:)   = albsnc_min_io(1:npft)
albsnf_max(:)   = albsnf_max_io(1:npft)
albsnf_maxl(:)  = albsnf_maxl_io(1:npft)
albsnf_maxu(:)  = albsnf_maxu_io(1:npft)
alnir(:)        = alnir_io(1:npft)
alnirl(:)       = alnirl_io(1:npft)
alniru(:)       = alniru_io(1:npft)
alpar(:)        = alpar_io(1:npft)
alparl(:)       = alparl_io(1:npft)
alparu(:)       = alparu_io(1:npft)
kext(:)         = kext_io(1:npft)
kpar(:)         = kpar_io(1:npft)
lai_alb_lim(:)  = lai_alb_lim_io(1:npft)
omega(:)        = omega_io(1:npft)
omegal(:)       = omegal_io(1:npft)
omegau(:)       = omegau_io(1:npft)
omnir(:)        = omnir_io(1:npft)
omnirl(:)       = omnirl_io(1:npft)
omniru(:)       = omniru_io(1:npft)
orient(:)       = orient_io(1:npft)

! Photosynthesis and respiration parameters.
act_jmax(:)   = act_jmax_io(1:npft)
act_vcmax(:)  = act_vcmax_io(1:npft)
alpha(:)        = alpha_io(1:npft)
alpha_elec(:) = alpha_elec_io(1:npft)
c3(:)           = c3_io(1:npft)
can_struct_a(:) = can_struct_a_io(1:npft)
deact_jmax(:) = deact_jmax_io(1:npft)
deact_vcmax(:)= deact_vcmax_io(1:npft)
dqcrit(:)       = dqcrit_io(1:npft)
ds_jmax(:)    = ds_jmax_io(1:npft)
ds_vcmax(:)   = ds_vcmax_io(1:npft)
f0(:)           = f0_io(1:npft)
fd(:)           = fd_io(1:npft)
g1_stomata(:) = g1_stomata_io(1:npft)
jv25_ratio(:) = jv25_ratio_io(1:npft)
kn(:)           = kn_io(1:npft)
knl(:)          = knl_io(1:npft)
neff(:)         = neff_io(1:npft)
nl0(:)          = nl0_io(1:npft)
nr_nl(:)        = nr_nl_io(1:npft)
ns_nl(:)        = ns_nl_io(1:npft)
r_grow(:)       = r_grow_io(1:npft)
tlow(:)         = tlow_io(1:npft)
tupp(:)         = tupp_io(1:npft)

! Trait physiology parameters
hw_sw(:)        = hw_sw_io(1:npft)
lma(:)          = lma_io(1:npft)
nmass(:)        = nmass_io(1:npft)
nr(:)           = nr_io(1:npft)
nsw(:)          = nsw_io(1:npft)
q10_leaf(:)     = q10_leaf_io(1:npft)
vint(:)         = vint_io(1:npft)
vsl(:)          = vsl_io(1:npft)

! Allometric and other parameters.
a_wl(:)         = a_wl_io(1:npft)
a_ws(:)         = a_ws_io(1:npft)
b_wl(:)         = b_wl_io(1:npft)
eta_sl(:)       = eta_sl_io(1:npft)
sigl(:)         = sigl_io(1:npft)

! Phenology parameters.
dgl_dm(:)       = dgl_dm_io(1:npft)
dgl_dt(:)       = dgl_dt_io(1:npft)
fsmc_of(:)      = fsmc_of_io(1:npft)
g_leaf_0(:)     = g_leaf_0_io(1:npft)
tleaf_of(:)     = tleaf_of_io(1:npft)

! Hydrological, thermal and other "physical" characteristics.
#if !defined(UM_JULES)
fsmc_mod(:)     = fsmc_mod_io(1:npft)
psi_close(:)    = psi_close_io(1:npft)
psi_open(:)     = psi_open_io(1:npft)
#endif
catch0(:)       = catch0_io(1:npft)
dcatch_dlai(:)  = dcatch_dlai_io(1:npft)
dust_veg_scj(:) = dust_veg_scj_io(1:npft)
dz0v_dh(:)      = dz0v_dh_io(1:npft)
emis_pft(:)     = emis_pft_io(1:npft)
fsmc_p0(:)      = fsmc_p0_io(1:npft)
glmin(:)        = glmin_io(1:npft)
gsoil_f(:)      = gsoil_f_io(1:npft)
infil_f(:)      = infil_f_io(1:npft)
irrig_tile(1:npft)      = irrig_pft_io(1:npft)
rootd_ft(:)     = rootd_ft_io(1:npft)
z0v(:)          = z0v_io(1:npft)
z0h_z0m(1:npft) = z0hm_pft_io(1:npft)
z0h_z0m_classic(1:npft) = z0hm_classic_pft_io(1:npft)


! Ozone damage parameters.
dfp_dcuo(:)     = dfp_dcuo_io(1:npft)
fl_o3_ct(:)     = fl_o3_ct_io(1:npft)

! BVOC emission parameters.
aef(:)          = aef_io(1:npft)
ci_st(:)        = ci_st_io(1:npft)
gpp_st(:)       = gpp_st_io(1:npft)
ief(:)          = ief_io(1:npft)
mef(:)          = mef_io(1:npft)
tef(:)          = tef_io(1:npft)

! INFERNO combustion parameters
avg_ba(:)       = avg_ba_io(1:npft)
ccleaf_max(:)   = ccleaf_max_io(1:npft)
ccleaf_min(:)   = ccleaf_min_io(1:npft)
ccwood_max(:)   = ccwood_max_io(1:npft)
ccwood_min(:)   = ccwood_min_io(1:npft)
fire_mort(:)  = fire_mort_io(1:npft)
fireveg_c_to_atmos(:) = fireveg_c_to_atmos_io(1:npft)

! INFERNO emission parameters
fef_bc(:)       = fef_bc_io(1:npft)
fef_ch4(:)      = fef_ch4_io(1:npft)
fef_co(:)       = fef_co_io(1:npft)
fef_co2(:)      = fef_co2_io(1:npft)
fef_nox(:)      = fef_nox_io(1:npft)
fef_oc(:)       = fef_oc_io(1:npft)
fef_so2(:)      = fef_so2_io(1:npft)
fef_c2h4(:)     = fef_c2h4_io(1:npft)
fef_c2h6(:)     = fef_c2h6_io(1:npft)
fef_c3h8(:)     = fef_c3h8_io(1:npft)
fef_hcho(:)     = fef_hcho_io(1:npft)
fef_mecho(:)    = fef_mecho_io(1:npft)
fef_nh3(:)      = fef_nh3_io(1:npft)
fef_dms(:)      = fef_dms_io(1:npft)

! SUGAR parameters
sug_g0(:)       = sug_g0_io(1:npft)
sug_grec(:)     = sug_grec_io(1:npft)
sug_yg(:)       = sug_yg_io(1:npft)

! SOX parameters
sox_a(:)        = sox_a_io(1:npft)
sox_p50(:)      = sox_p50_io(1:npft)
sox_rp_min(:)   = sox_rp_min_io(1:npft)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_pftparm_allocated

END MODULE pftparm_io
