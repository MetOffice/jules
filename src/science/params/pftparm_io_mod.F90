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

USE missing_data_mod, ONLY: imdi, rmdi
USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!---------------------------------------------------------------------
! Set up variables to use in IO (a fixed size version of each array
! in pftparm that we want to initialise).
!---------------------------------------------------------------------
#if !defined(UM_JULES)
INTEGER ::                                                                     &
  fsmc_mod_io = imdi

REAL(KIND=real_jlslsm) ::                                                      &
  canht_ft_io = rmdi,                                                &
  lai_io = rmdi,                                                     &
  psi_close_io = rmdi,                                               &
  psi_open_io = rmdi
#endif

INTEGER ::                                                                     &
  c3_io = imdi,                                                      &
  orient_io = imdi

REAL(KIND=real_jlslsm) ::                                                      &
  a_wl_io = rmdi,                                                    &
  a_ws_io = rmdi,                                                    &
  act_jmax_io = rmdi,                                                &
  act_vcmax_io = rmdi,                                               &
  aef_io = rmdi,                                                     &
  albsnc_max_io = rmdi,                                              &
  albsnc_min_io = rmdi,                                              &
  albsnf_max_io = rmdi,                                              &
  albsnf_maxl_io = rmdi,                                             &
  albsnf_maxu_io = rmdi,                                             &
  alpha_io = rmdi,                                                   &
  alpha_elec_io = rmdi,                                              &
  alnir_io = rmdi,                                                   &
  alnirl_io = rmdi,                                                  &
  alniru_io = rmdi,                                                  &
  alpar_io = rmdi,                                                   &
  alparl_io = rmdi,                                                  &
  alparu_io = rmdi,                                                  &
  avg_ba_io = rmdi,                                                  &
  b_wl_io = rmdi,                                                    &
  can_struct_a_io = rmdi,                                            &
  catch0_io = rmdi,                                                  &
  ccleaf_min_io = rmdi,                                              &
  ccleaf_max_io = rmdi,                                              &
  ccwood_min_io = rmdi,                                              &
  ccwood_max_io = rmdi,                                              &
  ci_st_io = rmdi,                                                   &
  dcatch_dlai_io = rmdi,                                             &
  deact_jmax_io = rmdi,                                              &
  deact_vcmax_io = rmdi,                                             &
  dfp_dcuo_io = rmdi,                                                &
  dgl_dm_io = rmdi,                                                  &
  dgl_dt_io = rmdi,                                                  &
  dqcrit_io = rmdi,                                                  &
  ds_jmax_io = rmdi,                                                 &
  ds_vcmax_io = rmdi,                                                &
  dust_veg_scj_io = rmdi,                                            &
  dz0v_dh_io = rmdi,                                                 &
  emis_pft_io = rmdi,                                                &
  eta_sl_io = rmdi,                                                  &
  f0_io = rmdi,                                                      &
  fef_bc_io = rmdi,                                                  &
  fef_c2h4_io = rmdi,                                                &
  fef_c2h6_io = rmdi,                                                &
  fef_c3h8_io = rmdi,                                                &
  fef_ch4_io = rmdi,                                                 &
  fef_co_io = rmdi,                                                  &
  fef_co2_io = rmdi,                                                 &
  fef_dms_io = rmdi,                                                 &
  fef_hcho_io = rmdi,                                                &
  fef_mecho_io = rmdi,                                               &
  fef_nh3_io = rmdi,                                                 &
  fef_nox_io = rmdi,                                                 &
  fef_oc_io = rmdi,                                                  &
  fef_so2_io = rmdi,                                                 &
  fd_io = rmdi,                                                      &
  fire_mort_io = rmdi,                                               &
  fl_o3_ct_io = rmdi,                                                &
  fsmc_of_io = rmdi,                                                 &
  fsmc_p0_io = rmdi,                                                 &
  sug_g0_io = rmdi,                                                  &
  g1_stomata_io = rmdi,                                              &
  g_leaf_0_io = rmdi,                                                &
  glmin_io = rmdi,                                                   &
  gpp_st_io = rmdi,                                                  &
  sug_grec_io = rmdi,                                                &
  gsoil_f_io = rmdi,                                                 &
  hw_sw_io = rmdi,                                                   &
  ief_io = rmdi,                                                     &
  infil_f_io = rmdi,                                                 &
  jv25_ratio_io = rmdi,                                              &
  kext_io = rmdi,                                                    &
  kn_io = rmdi,                                                      &
  knl_io = rmdi,                                                     &
  kpar_io = rmdi,                                                    &
  lai_alb_lim_io = rmdi,                                             &
  lma_io = rmdi,                                                     &
  mef_io = rmdi,                                                     &
  neff_io = rmdi,                                                    &
  nl0_io = rmdi,                                                     &
  nmass_io = rmdi,                                                   &
  nr_nl_io = rmdi,                                                   &
  ns_nl_io = rmdi,                                                   &
  nsw_io = rmdi,                                                     &
  nr_io = rmdi,                                                      &
  omega_io = rmdi,                                                   &
  omegal_io = rmdi,                                                  &
  omegau_io = rmdi,                                                  &
  omnir_io = rmdi,                                                   &
  omnirl_io = rmdi,                                                  &
  omniru_io = rmdi,                                                  &
  q10_leaf_io = rmdi,                                                &
  r_grow_io = rmdi,                                                  &
  rootd_ft_io = rmdi,                                                &
  sigl_io = rmdi,                                                    &
  tef_io = rmdi,                                                     &
  tleaf_of_io = rmdi,                                                &
  tlow_io = rmdi,                                                    &
  tupp_io = rmdi,                                                    &
  vint_io = rmdi,                                                    &
  vsl_io = rmdi,                                                     &
  sug_yg_io = rmdi,                                                  &
  z0hm_pft_io = rmdi,                                                &
  z0hm_classic_pft_io = rmdi,                                        &
  z0v_io = rmdi,                                                     &
  sox_a_io = rmdi,                                                   &
  sox_p50_io = rmdi,                                                 &
  sox_rp_min_io = rmdi

CHARACTER(LEN=30) :: pft_name_io

!---------------------------------------------------------------------
! Set up a namelist for reading and writing these arrays
!---------------------------------------------------------------------
NAMELIST  / jules_pftparm/                                                     &
#if !defined(UM_JULES)
  canht_ft_io,     lai_io,                                                     &
  fsmc_mod_io,     psi_close_io,           psi_open_io,                        &
#endif
  a_wl_io,         a_ws_io,                act_jmax_io, &
  act_vcmax_io,    aef_io,                 albsnc_max_io, &
  albsnc_min_io,   albsnf_max_io,          albsnf_maxl_io, &
  albsnf_maxu_io,  alnir_io,               alnirl_io, &
  alniru_io,       alpar_io,               alparl_io, &
  alparu_io,       alpha_elec_io,          alpha_io, &
  avg_ba_io,       b_wl_io,                c3_io, &
  can_struct_a_io, catch0_io,              ccleaf_max_io, &
  ccleaf_min_io,   ccwood_max_io,          ccwood_min_io, &
  ci_st_io,        dcatch_dlai_io,         deact_jmax_io, &
  deact_vcmax_io,  dfp_dcuo_io,            dgl_dm_io, &
  dgl_dt_io,       dqcrit_io,              ds_jmax_io, &
  ds_vcmax_io,     dust_veg_scj_io,        dz0v_dh_io, &
  emis_pft_io,     eta_sl_io,              f0_io, &
  fd_io,           fef_bc_io,              fef_c2h4_io, &
  fef_c2h6_io,     fef_c3h8_io,            fef_ch4_io, &
  fef_co2_io,      fef_co_io,              fef_dms_io, &
  fef_hcho_io,     fef_mecho_io,           fef_nh3_io, &
  fef_nox_io,      fef_oc_io,              fef_so2_io, &
  fire_mort_io,    fl_o3_ct_io,            fsmc_of_io, &
  fsmc_p0_io,      g1_stomata_io,          g_leaf_0_io, &
  glmin_io,        gpp_st_io,              gsoil_f_io, &
  hw_sw_io,        ief_io,                 infil_f_io, &
  jv25_ratio_io,   kext_io,                kn_io, &
  knl_io,          kpar_io,                lai_alb_lim_io, &
  lai_io,          lma_io,                 mef_io, &
  neff_io,         nl0_io,                 nmass_io, &
  nr_io,           nr_nl_io,               ns_nl_io, &
  nsw_io,          omega_io,               omegal_io, &
  omegau_io,       omnir_io,               omnirl_io, &
  omniru_io,       orient_io,              pft_name_io, &
  q10_leaf_io,     r_grow_io,              rootd_ft_io, &
  sigl_io,         sox_a_io,               sox_p50_io, &
  sox_rp_min_io,   sug_g0_io,              sug_grec_io, &
  sug_yg_io,       tef_io,                 tleaf_of_io, &
  tlow_io,         tupp_io,                vint_io, &
  vsl_io,          z0hm_classic_pft_io,    z0hm_pft_io, &
  z0v_io

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PFTPARM_IO'

CONTAINS

#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_pftparm (nml_dir)

USE jules_surface_types_mod, ONLY: npft

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists
! Work variables
INTEGER :: i, ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_PFTPARM'

!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
CALL log_info(routinename, "Reading JULES_PFTPARM namelist...")

! Open the pft parameters namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'pft_params.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error opening namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

DO i = 1, npft
  pft_name_io = ''
  READ(namelist_unit, NML = jules_pftparm, IOSTAT = ERROR, IOMSG = iomessage)
  IF ( ERROR /= 0 ) THEN
    CALL log_fatal(routinename,                                                &
                   "Error reading namelist JULES_PFTPARM " //                  &
                   "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //        &
                   TRIM(iomessage) // ")")
  END IF
  CALL init_pftparm_allocated()
END DO

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_pftparm
#endif

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
INTEGER, PARAMETER :: n_int = 2
INTEGER, PARAMETER :: n_real = 108

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: c3_io
  INTEGER :: orient_io
  REAL(KIND=real_jlslsm) :: a_wl_io
  REAL(KIND=real_jlslsm) :: a_ws_io
  REAL(KIND=real_jlslsm) :: act_jmax_io
  REAL(KIND=real_jlslsm) :: act_vcmax_io
  REAL(KIND=real_jlslsm) :: aef_io
  REAL(KIND=real_jlslsm) :: albsnc_max_io
  REAL(KIND=real_jlslsm) :: albsnc_min_io
  REAL(KIND=real_jlslsm) :: albsnf_max_io
  REAL(KIND=real_jlslsm) :: albsnf_maxl_io
  REAL(KIND=real_jlslsm) :: albsnf_maxu_io
  REAL(KIND=real_jlslsm) :: alpha_io
  REAL(KIND=real_jlslsm) :: alpha_elec_io
  REAL(KIND=real_jlslsm) :: alnir_io
  REAL(KIND=real_jlslsm) :: alnirl_io
  REAL(KIND=real_jlslsm) :: alniru_io
  REAL(KIND=real_jlslsm) :: alpar_io
  REAL(KIND=real_jlslsm) :: alparl_io
  REAL(KIND=real_jlslsm) :: alparu_io
  REAL(KIND=real_jlslsm) :: avg_ba_io
  REAL(KIND=real_jlslsm) :: b_wl_io
  REAL(KIND=real_jlslsm) :: can_struct_a_io
  REAL(KIND=real_jlslsm) :: catch0_io
  REAL(KIND=real_jlslsm) :: ccleaf_min_io
  REAL(KIND=real_jlslsm) :: ccleaf_max_io
  REAL(KIND=real_jlslsm) :: ccwood_min_io
  REAL(KIND=real_jlslsm) :: ccwood_max_io
  REAL(KIND=real_jlslsm) :: ci_st_io
  REAL(KIND=real_jlslsm) :: dcatch_dlai_io
  REAL(KIND=real_jlslsm) :: deact_jmax_io
  REAL(KIND=real_jlslsm) :: deact_vcmax_io
  REAL(KIND=real_jlslsm) :: dfp_dcuo_io
  REAL(KIND=real_jlslsm) :: dgl_dm_io
  REAL(KIND=real_jlslsm) :: dgl_dt_io
  REAL(KIND=real_jlslsm) :: dqcrit_io
  REAL(KIND=real_jlslsm) :: ds_jmax_io
  REAL(KIND=real_jlslsm) :: ds_vcmax_io
  REAL(KIND=real_jlslsm) :: dust_veg_scj_io
  REAL(KIND=real_jlslsm) :: dz0v_dh_io
  REAL(KIND=real_jlslsm) :: emis_pft_io
  REAL(KIND=real_jlslsm) :: eta_sl_io
  REAL(KIND=real_jlslsm) :: f0_io
  REAL(KIND=real_jlslsm) :: fd_io
  REAL(KIND=real_jlslsm) :: fef_bc_io
  REAL(KIND=real_jlslsm) :: fef_ch4_io
  REAL(KIND=real_jlslsm) :: fef_co_io
  REAL(KIND=real_jlslsm) :: fef_co2_io
  REAL(KIND=real_jlslsm) :: fef_nox_io
  REAL(KIND=real_jlslsm) :: fef_oc_io
  REAL(KIND=real_jlslsm) :: fef_so2_io
  REAL(KIND=real_jlslsm) :: fef_c2h4_io
  REAL(KIND=real_jlslsm) :: fef_c2h6_io
  REAL(KIND=real_jlslsm) :: fef_c3h8_io
  REAL(KIND=real_jlslsm) :: fef_hcho_io
  REAL(KIND=real_jlslsm) :: fef_mecho_io
  REAL(KIND=real_jlslsm) :: fef_nh3_io
  REAL(KIND=real_jlslsm) :: fef_dms_io
  REAL(KIND=real_jlslsm) :: fire_mort_io
  REAL(KIND=real_jlslsm) :: fl_o3_ct_io
  REAL(KIND=real_jlslsm) :: fsmc_of_io
  REAL(KIND=real_jlslsm) :: fsmc_p0_io
  REAL(KIND=real_jlslsm) :: sug_g0_io
  REAL(KIND=real_jlslsm) :: g1_stomata_io
  REAL(KIND=real_jlslsm) :: g_leaf_0_io
  REAL(KIND=real_jlslsm) :: glmin_io
  REAL(KIND=real_jlslsm) :: gpp_st_io
  REAL(KIND=real_jlslsm) :: sug_grec_io
  REAL(KIND=real_jlslsm) :: gsoil_f_io
  REAL(KIND=real_jlslsm) :: hw_sw_io
  REAL(KIND=real_jlslsm) :: ief_io
  REAL(KIND=real_jlslsm) :: infil_f_io
  REAL(KIND=real_jlslsm) :: jv25_ratio_io
  REAL(KIND=real_jlslsm) :: kext_io
  REAL(KIND=real_jlslsm) :: kn_io
  REAL(KIND=real_jlslsm) :: knl_io
  REAL(KIND=real_jlslsm) :: kpar_io
  REAL(KIND=real_jlslsm) :: lai_alb_lim_io
  REAL(KIND=real_jlslsm) :: lma_io
  REAL(KIND=real_jlslsm) :: mef_io
  REAL(KIND=real_jlslsm) :: neff_io
  REAL(KIND=real_jlslsm) :: nl0_io
  REAL(KIND=real_jlslsm) :: nmass_io
  REAL(KIND=real_jlslsm) :: nr_io
  REAL(KIND=real_jlslsm) :: nr_nl_io
  REAL(KIND=real_jlslsm) :: ns_nl_io
  REAL(KIND=real_jlslsm) :: nsw_io
  REAL(KIND=real_jlslsm) :: omega_io
  REAL(KIND=real_jlslsm) :: omegal_io
  REAL(KIND=real_jlslsm) :: omegau_io
  REAL(KIND=real_jlslsm) :: omnir_io
  REAL(KIND=real_jlslsm) :: omnirl_io
  REAL(KIND=real_jlslsm) :: omniru_io
  REAL(KIND=real_jlslsm) :: q10_leaf_io
  REAL(KIND=real_jlslsm) :: r_grow_io
  REAL(KIND=real_jlslsm) :: rootd_ft_io
  REAL(KIND=real_jlslsm) :: sigl_io
  REAL(KIND=real_jlslsm) :: tef_io
  REAL(KIND=real_jlslsm) :: tleaf_of_io
  REAL(KIND=real_jlslsm) :: tlow_io
  REAL(KIND=real_jlslsm) :: tupp_io
  REAL(KIND=real_jlslsm) :: vint_io
  REAL(KIND=real_jlslsm) :: vsl_io
  REAL(KIND=real_jlslsm) :: sug_yg_io
  REAL(KIND=real_jlslsm) :: z0hm_pft_io
  REAL(KIND=real_jlslsm) :: z0hm_classic_pft_io
  REAL(KIND=real_jlslsm) :: z0v_io
  REAL(KIND=real_jlslsm) :: sox_a_io
  REAL(KIND=real_jlslsm) :: sox_p50_io
  REAL(KIND=real_jlslsm) :: sox_rp_min_io
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
  canht_pft,       lai_pft,                                                    &
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
  fef_dms,         fire_mort,                                                  &
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

USE c_z0h_z0m, ONLY: z0h_z0m,  z0h_z0m_classic

use jules_surface_types_mod,  only: npft, brd_leaf, ndl_leaf, c3_grass,    &
   c4_grass, shrub

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_print, jules_message

IMPLICIT NONE

INTEGER :: i, errorstatus

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='INIT_PFTPARM_ALLOCATED'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Probably need to find a better way to map these; for now test with the
! original 5 PFTs.
select case ( trim( pft_name_io ) )
case ( 'brd_leaf' )
  i = brd_leaf
case ( 'ndl_leaf' )
  i = ndl_leaf
case ( 'c3_grass' )
  i = c3_grass
case ( 'c4_grass' )
  i = c4_grass
case ( 'shrub' )
  i = shrub
case DEFAULT
  errorstatus = 101
  write(jules_message,'(A)')                                         &
     'PFT name not recognised: ' // trim( pft_name_io )
  CALL ereport(RoutineName, errorstatus, jules_message)
end select

! Range of specified types (1:npft) checked by check_jules_surface_types
if ( i < 1 ) then
  errorstatus = 101
  write(jules_message,'(A)')                                         &
     'jules_pftparm and jules_surface_types inputs are inconsistent; '// &
     trim(pft_name_io) //                                &
     ' is not specified in jules_surface_types'
  CALL ereport(RoutineName, errorstatus, jules_message)
end if

! Radiation and albedo parameters.
albsnc_max(i)   = albsnc_max_io
albsnc_min(i)   = albsnc_min_io
albsnf_max(i)   = albsnf_max_io
albsnf_maxl(i)  = albsnf_maxl_io
albsnf_maxu(i)  = albsnf_maxu_io
alnir(i)        = alnir_io
alnirl(i)       = alnirl_io
alniru(i)       = alniru_io
alpar(i)        = alpar_io
alparl(i)       = alparl_io
alparu(i)       = alparu_io
kext(i)         = kext_io
kpar(i)         = kpar_io
lai_alb_lim(i)  = lai_alb_lim_io
omega(i)        = omega_io
omegal(i)       = omegal_io
omegau(i)       = omegau_io
omnir(i)        = omnir_io
omnirl(i)       = omnirl_io
omniru(i)       = omniru_io
orient(i)       = orient_io

! Photosynthesis and respiration parameters.
act_jmax(i)     = act_jmax_io
act_vcmax(i)    = act_vcmax_io
alpha(i)        = alpha_io
alpha_elec(i)   = alpha_elec_io
c3(i)           = c3_io
can_struct_a(i) = can_struct_a_io
deact_jmax(i)   = deact_jmax_io
deact_vcmax(i)  = deact_vcmax_io
dqcrit(i)       = dqcrit_io
ds_jmax(i)      = ds_jmax_io
ds_vcmax(i)     = ds_vcmax_io
f0(i)           = f0_io
fd(i)           = fd_io
g1_stomata(i)   = g1_stomata_io
jv25_ratio(i)   = jv25_ratio_io
kn(i)           = kn_io
knl(i)          = knl_io
neff(i)         = neff_io
nl0(i)          = nl0_io
nr_nl(i)        = nr_nl_io
ns_nl(i)        = ns_nl_io
r_grow(i)       = r_grow_io
tlow(i)         = tlow_io
tupp(i)         = tupp_io

! Trait physiology parameters
hw_sw(i)        = hw_sw_io
lma(i)          = lma_io
nmass(i)        = nmass_io
nr(i)           = nr_io
nsw(i)          = nsw_io
q10_leaf(i)     = q10_leaf_io
vint(i)         = vint_io
vsl(i)          = vsl_io

! Allometric and other parameters.
a_wl(i)         = a_wl_io
a_ws(i)         = a_ws_io
b_wl(i)         = b_wl_io
eta_sl(i)       = eta_sl_io
sigl(i)         = sigl_io

! Phenology parameters.
dgl_dm(i)       = dgl_dm_io
dgl_dt(i)       = dgl_dt_io
fsmc_of(i)      = fsmc_of_io
g_leaf_0(i)     = g_leaf_0_io
tleaf_of(i)     = tleaf_of_io

! Hydrological, thermal and other "physical" characteristics.
#if !defined(UM_JULES)
! *** Just adding these here for now ***
canht_pft(i)    = canht_ft_io
lai_pft(i)      = lai_io
! *** Just adding these here for now ***
fsmc_mod(i)     = fsmc_mod_io
psi_close(i)    = psi_close_io
psi_open(i)     = psi_open_io
#endif
catch0(i)       = catch0_io
dcatch_dlai(i)  = dcatch_dlai_io
dust_veg_scj(i) = dust_veg_scj_io
dz0v_dh(i)      = dz0v_dh_io
emis_pft(i)     = emis_pft_io
fsmc_p0(i)      = fsmc_p0_io
glmin(i)        = glmin_io
gsoil_f(i)      = gsoil_f_io
infil_f(i)      = infil_f_io
rootd_ft(i)     = rootd_ft_io
z0v(i)          = z0v_io
z0h_z0m(i)      = z0hm_pft_io
z0h_z0m_classic(i) = z0hm_classic_pft_io

! Ozone damage parameters.
dfp_dcuo(i)     = dfp_dcuo_io
fl_o3_ct(i)     = fl_o3_ct_io

! BVOC emission parameters.
aef(i)          = aef_io
ci_st(i)        = ci_st_io
gpp_st(i)       = gpp_st_io
ief(i)          = ief_io
mef(i)          = mef_io
tef(i)          = tef_io

! INFERNO combustion parameters
avg_ba(i)       = avg_ba_io
ccleaf_max(i)   = ccleaf_max_io
ccleaf_min(i)   = ccleaf_min_io
ccwood_max(i)   = ccwood_max_io
ccwood_min(i)   = ccwood_min_io
fire_mort(i)    = fire_mort_io

! INFERNO emission parameters
fef_bc(i)       = fef_bc_io
fef_ch4(i)      = fef_ch4_io
fef_co(i)       = fef_co_io
fef_co2(i)      = fef_co2_io
fef_nox(i)      = fef_nox_io
fef_oc(i)       = fef_oc_io
fef_so2(i)      = fef_so2_io
fef_c2h4(i)     = fef_c2h4_io
fef_c2h6(i)     = fef_c2h6_io
fef_c3h8(i)     = fef_c3h8_io
fef_hcho(i)     = fef_hcho_io
fef_mecho(i)    = fef_mecho_io
fef_nh3(i)      = fef_nh3_io
fef_dms(i)      = fef_dms_io

! SUGAR parameters
sug_g0(i)       = sug_g0_io
sug_grec(i)     = sug_grec_io
sug_yg(i)       = sug_yg_io

! SOX parameters
sox_a(i)        = sox_a_io
sox_p50(i)      = sox_p50_io
sox_rp_min(i)   = sox_rp_min_io


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE init_pftparm_allocated

END MODULE pftparm_io
