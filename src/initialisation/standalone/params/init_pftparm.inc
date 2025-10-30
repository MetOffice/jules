#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE init_pftparm_jules(nml_dir,progs)

USE missing_data_mod, ONLY:                                                    &
!  imported scalar parameters
     rmdi

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_soil_biogeochem_mod, ONLY: l_layeredC, soil_bgc_model,               &
                                      soil_model_4pool

USE jules_surface_types_mod, ONLY: npft, nnpft

USE ancil_info, ONLY: land_pts

USE pftparm_io, ONLY: jules_pftparm, init_pftparm_allocated,                   &
                      canht_ft_io, lai_io

USE pftparm, ONLY:                                                             &
  fsmc_mod,     psi_close,     psi_open,                                       &
  a_wl,         a_ws,          aef,                                            &
  act_jmax,     act_vcmax,     albsnc_max,                                     &
  albsnc_min,   albsnf_max,    albsnf_maxl,                                    &
  albsnf_maxu,  alpha,         alpha_elec,                                     &
  alnir,        alnirl,        alniru,                                         &
  alpar,        alparl,        alparu,                                         &
  avg_ba,       b_wl,          c3,                                             &
  can_struct_a, catch0,        ccleaf_max,                                     &
  ccleaf_min,   ccwood_max,    ccwood_min,                                     &
  ci_st,        dcatch_dlai,   deact_jmax,                                     &
  deact_vcmax,  dfp_dcuo,      dgl_dm,                                         &
  dgl_dt,       dqcrit,        ds_jmax,                                        &
  ds_vcmax,     dust_veg_scj,  dz0v_dh,                                        &
  emis_pft,     eta_sl,        f0,                                             &
  fd,           fef_bc,        fef_ch4,                                        &
  fef_co,       fef_co2,       fef_nox,                                        &
  fef_oc,       fef_so2,       fef_c2h4,                                       &
  fef_c2h6,     fef_c3h8,      fef_hcho,                                       &
  fef_mecho,    fef_nh3,                                                       &
  fef_dms,      fire_mort,     fl_o3_ct,                                       &
  fsmc_of,      fsmc_p0,       sug_g0,                                         &
  g1_stomata,   g_leaf_0,      glmin,                                          &
  gpp_st,       sug_grec,      gsoil_f,                                        &
  hw_sw,        ief,           infil_f,                                        &
  jv25_ratio,   kext,          kn,                                             &
  knl,          kpar,          lai_alb_lim,                                    &
  lma,          mef,           neff,                                           &
  nl0,          nmass,         nr,                                             &
  nr_nl,        ns_nl,         nsw,                                            &
  omega,        omegal,        omegau,                                         &
  omnir,        omnirl,        omniru,                                         &
  orient,       q10_leaf,      r_grow,                                         &
  rootd_ft,     sigl,          tef,                                            &
  tleaf_of,     tlow,          tupp,                                           &
  vint,         vsl,           sug_yg,                                         &
  z0v,          sox_a,         sox_p50,                                        &
  sox_rp_min

USE c_z0h_z0m, ONLY: z0h_z0m, z0h_z0m_classic

USE jules_vegetation_mod, ONLY: can_rad_mod, l_crop, l_trait_phys,             &
                                 l_use_pft_psi, l_bvoc_emis, l_inferno,        &
                                 l_o3_damage, l_trif_fire, photo_acclim_model, &
                                 photo_act_model, photo_act_pft,               &
                                 photo_farquhar, photo_model,                  &
                                 stomata_jacobs, stomata_medlyn, stomata_sox,  &
                                 stomata_model, l_spec_veg_z0, l_sugar,        &
                                 l_scale_resp_pm

USE jules_radiation_mod, ONLY: l_spec_albedo, l_albedo_obs, l_snow_albedo

USE errormessagelength_mod, ONLY: errormessagelength

!TYPE definitions
USE prognostics, ONLY: progs_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises the PFT parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs

! Work variables
INTEGER :: ERROR  ! Error indicator
INTEGER :: i      ! Loop counter.
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=*), PARAMETER :: routinename='INIT_PFTPARM_JULES'

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

READ(namelist_unit, NML = jules_pftparm, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error reading namelist JULES_PFTPARM " //                    &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Error closing namelist file pft_params.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
CALL init_pftparm_allocated()
progs%canht_pft(:,:)   = SPREAD(canht_ft_io(1:npft), 1, land_pts)
progs%lai_pft(:,:)     = SPREAD(lai_io(1:npft), 1, land_pts)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters but in some cases these are
! still tested below.
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY( orient(:) < 0 ) ) THEN  ! orient was initialised to < 0
  ERROR = 1
  CALL log_error(routinename, "No value for orient")
END IF
IF ( ANY( fsmc_mod(:) < 0 ) ) THEN  ! fsmc_mod was initialised to < 0
  ERROR = 1
  CALL log_error(routinename, "No value for fsmc_mod")
END IF
IF ( ANY( ABS( kext(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for kext")
END IF
IF ( ANY( ABS( kpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for kpar")
END IF
IF ( ANY( ABS( lai_alb_lim(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for lai_alb_lim")
END IF
IF ( ANY( ABS( can_struct_a(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for can_struct_a")
END IF
IF ( ANY( ABS( gsoil_f(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for gsoil_f")
END IF
IF ( ANY( c3(:) < 0 ) ) THEN  ! c3 was initialised to < 0
  ERROR = 1
  CALL log_error(routinename, "No value for c3")
END IF
IF ( ANY( ABS( alpha(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for alpha")
END IF
IF ( ANY( ABS( fd(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for fd")
END IF
IF ( ANY( ABS( nr_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for nr_nl")
END IF
IF ( ANY( ABS( ns_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for ns_nl")
END IF
IF ( ANY( ABS( r_grow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for r_grow")
END IF
! Note that tlow and tupp are always required, for some PFTs at least.
! If using the Farquhar model for C3 plants, we still need tlow and
! tupp for C4 plants.
IF ( ANY( ABS( tlow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tlow")
END IF
IF ( ANY( ABS( tupp(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tupp")
END IF

SELECT CASE ( photo_model )
CASE ( photo_farquhar )
  !---------------------------------------------------------------------------
  ! First check parameters that are always required with this model.
  !---------------------------------------------------------------------------
  ! Note that these parameter values are not used for C4 plants, but
  ! here we're still checking that they have been provided.
  IF ( ANY( ABS( alpha_elec(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for alpha_elec")
  END IF
  IF ( ANY( ABS( deact_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for deact_jmax")
  END IF
  IF ( ANY( ABS( deact_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for deact_vcmax")
  END IF
  !---------------------------------------------------------------------------
  ! Check parameters that depend on any chosen acclimation model.
  !---------------------------------------------------------------------------
  IF ( photo_acclim_model == 0 ) THEN
    ! No acclimation.
    IF ( ANY( ABS( ds_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for ds_jmax")
    END IF
    IF ( ANY( ABS( ds_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for ds_vcmax")
    END IF
    IF ( ANY( ABS( jv25_ratio(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for jv25_ratio")
    END IF
  END IF  !  photo_acclim_model == 0

  IF ( photo_acclim_model == 0 .OR.                                            &
      (photo_acclim_model /= 0 .AND. photo_act_model == photo_act_pft) ) THEN
    IF ( ANY( ABS( act_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for act_jmax")
    END IF
    IF ( ANY( ABS( act_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error("init_pftparm", "No value for act_vcmax")
    END IF
  END IF  !  photo_acclim_model

END SELECT  !  photo_model

SELECT CASE ( stomata_model )
CASE ( stomata_jacobs )
  IF ( ANY( ABS( dqcrit(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for dqcrit")
  END IF
  IF ( ANY( ABS( f0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for f0")
  END IF
CASE ( stomata_medlyn )
  IF ( ANY( ABS( g1_stomata(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for g1_stomata")
  END IF
CASE ( stomata_sox )
  IF ( ANY( ABS( sox_p50(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for sox_p50")
  END IF
  IF ( ANY( ABS( sox_rp_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for sox_rp_min")
  END IF
  IF ( ANY( ABS( sox_a(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for sox_a")
  END IF
END SELECT

IF ( .NOT. l_spec_albedo .AND. can_rad_mod == 1 ) THEN
  ! These don't need to be set
ELSE
  IF ( ANY( ABS( alnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alnir")
  END IF
  IF ( ANY( ABS( alpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alpar")
  END IF
  IF ( ANY( ABS( omega(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omega")
  END IF
  IF ( ANY( ABS( omnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omnir")
  END IF
END IF

IF ( l_albedo_obs ) THEN
  IF ( ANY( ABS( alnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alnirl")
  END IF
  IF ( ANY( ABS( alniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alniru")
  END IF
  IF ( ANY( ABS( alparl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alparl")
  END IF
  IF ( ANY( ABS( alparu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for alparu")
  END IF
  IF ( ANY( ABS( omegal(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omegal")
  END IF
  IF ( ANY( ABS( omegau(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omegau")
  END IF
  IF ( ANY( ABS( omnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omnirl")
  END IF
  IF ( ANY( ABS( omniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for omniru")
  END IF
END IF

IF ( .NOT. l_spec_albedo ) THEN
  IF ( ANY( ABS( albsnf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for albsnf_max")
  END IF
  IF ( l_albedo_obs ) THEN
    IF ( ANY( ABS( albsnf_maxl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error(routinename, "No value for albsnf_maxl")
    END IF
    IF ( ANY( ABS( albsnf_maxu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL log_error(routinename, "No value for albsnf_maxu")
    END IF
  END IF
END IF

IF ( .NOT. l_snow_albedo ) THEN
  IF ( ANY( ABS( albsnc_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for albsnc_max")
  END IF
  IF ( ANY( ABS( albsnc_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for albsnc_min")
  END IF
END IF

IF (l_trait_phys) THEN
  IF ( ANY( ABS( lma(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for lma")
  END IF
  IF ( ANY( ABS( nmass(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for nmass")
  END IF
  IF ( ANY( ABS( vsl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for vsl")
  END IF
  IF ( ANY( ABS( vint(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for vint")
  END IF
  IF ( ANY( ABS( nr(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for nr")
  END IF
  IF ( ANY( ABS( nsw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for nsw")
  END IF
  IF ( ANY( ABS( hw_sw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for hw_sw")
  END IF
ELSE
  IF ( ANY( ABS( neff(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for neff")
  END IF
  IF ( ANY( ABS( nl0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for nl0")
  END IF
  IF ( ANY( ABS( sigl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for sigl")
  END IF
END IF !l_trait_phys

IF ( ANY( ABS( kn(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for kn")
END IF
IF ( can_rad_mod == 6 .AND. ANY( ABS( knl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for knl")
END IF
IF ( ANY( ABS( q10_leaf(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for q10_leaf")
END IF
IF ( ANY( ABS( a_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for a_wl")
END IF
IF ( ANY( ABS( a_ws(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for a_ws")
END IF
IF ( ANY( ABS( b_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for b_wl")
END IF
IF ( ANY( ABS( eta_sl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for eta_sl")
END IF
IF ( ANY( ABS( g_leaf_0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for g_leaf_0")
END IF
IF ( ANY( ABS( dgl_dm(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for dgl_dm")
END IF
IF ( ANY( ABS( fsmc_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for fsmc_of")
END IF
IF ( ANY( ABS( dgl_dt(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for dgl_dt")
END IF
IF ( ANY( ABS( tleaf_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for tleaf_of")
END IF
IF ( ANY( ABS( catch0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for catch0")
END IF
IF ( ANY( ABS( dcatch_dlai(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for dcatch_dlai")
END IF
IF ( ANY( ABS( infil_f(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for infil_f")
END IF
IF ( ANY( ABS( glmin(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for glmin")
END IF
IF ( .NOT. l_spec_veg_z0) THEN
  IF ( ANY( ABS( dz0v_dh(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for dz0v_dh")
  END IF
ELSE
  IF ( ANY( ABS( z0v(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error("init_pftparm", "No value for z0v")
  END IF
END IF
IF ( ANY( ABS( rootd_ft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for rootd_ft")
END IF

IF ( l_use_pft_psi ) THEN
  IF ( ANY( ABS( psi_close(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for psi_close")
  END IF
  IF ( ANY( ABS( psi_open(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for psi_open")
  END IF
ELSE
  IF ( ANY( ABS( fsmc_p0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fsmc_p0")
  END IF
END IF !l_use_pft_psi

IF ( l_bvoc_emis ) THEN
  IF ( ANY( ABS( ci_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ci_st")
  END IF
  IF ( ANY( ABS( gpp_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for gpp_st")
  END IF
  IF ( ANY( ABS( ief(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ief")
  END IF
  IF ( ANY( ABS( tef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for tef")
  END IF
  IF ( ANY( ABS( mef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for mef")
  END IF
  IF ( ANY( ABS( aef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for aef")
  END IF
END IF

IF ( l_inferno ) THEN
  IF ( ANY( ABS( fef_co2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_co2")
  END IF
  IF ( ANY( ABS( fef_co(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_co")
  END IF
  IF ( ANY( ABS( fef_ch4(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_ch4")
  END IF
  IF ( ANY( ABS( fef_nox(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_nox")
  END IF
  IF ( ANY( ABS( fef_so2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_so2")
  END IF
  IF ( ANY( ABS( fef_c2h4(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_c2h4")
  END IF
  IF ( ANY( ABS( fef_c2h6(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_c2h6")
  END IF
  IF ( ANY( ABS( fef_c3h8(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_c3h8")
  END IF
  IF ( ANY( ABS( fef_hcho(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_hcho")
  END IF
  IF ( ANY( ABS( fef_mecho(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_mecho")
  END IF
  IF ( ANY( ABS( fef_nh3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_nh3")
  END IF
  IF ( ANY( ABS( fef_dms(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fef_dms")
  END IF
  IF ( ANY( ABS( ccleaf_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccleaf_min")
  END IF
  IF ( ANY( ABS( ccleaf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccleaf_max")
  END IF
  IF ( ANY( ABS( ccwood_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccwood_min")
  END IF
  IF ( ANY( ABS( ccwood_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for ccwood_max")
  END IF
  IF ( ANY( ABS( avg_ba(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for avg_ba")
  END IF
END IF

IF ( l_trif_fire ) THEN
  IF ( ANY( ABS( fire_mort(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fire_mort")
  END IF
END IF

IF ( l_o3_damage ) THEN
  IF ( ANY( ABS( fl_o3_ct(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for fl_o3_ct")
  END IF
  IF ( ANY( ABS( dfp_dcuo(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for dfp_dcuo")
  END IF
END IF

IF ( l_sugar ) THEN
  IF ( ANY( ABS( sug_g0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for sug_g0")
  END IF
  IF ( ANY( ABS( sug_grec(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for sug_grec")
  END IF
  IF ( ANY( ABS( sug_yg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL log_error(routinename, "No value for sug_yg")
  END IF
END IF

IF ( ANY( ABS( emis_pft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for emis_pft")
END IF
IF ( ANY( ABS( z0h_z0m(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for z0hm_pft")
END IF
IF ( ANY( ABS( z0h_z0m_classic(1:npft) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for z0hm_classic_pft")
END IF
IF ( ANY( ABS( progs%canht_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for canht_ft")
END IF
IF ( ANY( ABS( progs%lai_pft(:,:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL log_error(routinename, "No value for lai")
END IF

IF ( ERROR /= 0 )                                                              &
  CALL log_fatal(routinename,                                                  &
                 "Variable(s) missing from namelist - see earlier " //         &
                 "error message(s)")

!-----------------------------------------------------------------------------
! Check that glmin is >0.
! This ensures that wt_ext in subroutine soil_evap cannot become a NaN (which
! it would if gs=glmin and gsoil=0), or blow up, and might well be required
! elsewhere too.
!-----------------------------------------------------------------------------
IF ( ANY(glmin < 1.0e-10) )                                                    &
  CALL log_warn(routinename,                                                   &
                "Increasing one or more values of glmin - very small " //      &
                "values can cause model to blow up or NaNs")

WHERE ( glmin < 1.0e-10 )
  glmin = 1.0e-10
END WHERE

IF ( l_crop ) THEN
  IF ( ANY( ABS( a_ws(nnpft+1: npft) - 1.0 ) > EPSILON(1.0) ) ) THEN
    CALL log_fatal(routinename, "crop tiles should have a_ws=1.0")
  END IF
END IF

IF ( l_use_pft_psi ) THEN
  IF ( ANY( psi_close(1: npft) > EPSILON(1.0) ) ) THEN
    CALL log_fatal(routinename, "psi_close should be negative")
  END IF
  IF ( ANY( psi_open(1: npft) > EPSILON(1.0) ) ) THEN
    CALL log_fatal(routinename, "psi_open should be negative")
  END IF
END IF

!-----------------------------------------------------------------------------
! fsmc_mod=1 should not be allowed with a layered 4-pool C model until this has
! been properly evaluated. (With fsmc_mod=1, subroutine root_frac does not
! return the exponential root profile that users might expect.)
!-----------------------------------------------------------------------------
IF ( l_layeredC .AND. ( soil_bgc_model == soil_model_4pool ) .AND.             &
     ANY( fsmc_mod(:) == 1 ) ) THEN
  CALL log_error(routinename,                                                  &
                 "fsmc_mod=1 is not allowed with l_layeredC and 4-pool C model")
END IF

!-----------------------------------------------------------------------------
! stomata_model = stomata_sox must be used with fsmc_mod = 1
! Cannot be run with l_scale_resp_pm
! Must be run with can_rad_mod = 1 (implementation for can_rad_mod = 6 ongoing)
!-----------------------------------------------------------------------------
IF ( stomata_model == stomata_sox ) THEN ! SOX
  IF ( l_scale_resp_pm ) THEN
    CALL log_fatal(routinename,                                                &
    'l_scale_resp_pm=T is incompatible with SOX (stomata_model=3)')
  END IF

  IF ( .NOT. ANY ( fsmc_mod(:) == 1 ) ) THEN
    CALL log_fatal(routinename,                                                &
    'SOX (stomata_model=3) must be used with fsmc_mod = 1')
  END IF

  IF ( .NOT. ( can_rad_mod == 1 ) ) THEN
    CALL log_fatal(routinename,                                                &
    'SOX (stomata_model=3) must be used with can_rad_mod = 1')
  END IF

  IF ( .NOT. ( photo_model == 3 ) ) THEN
    CALL log_fatal(routinename,                                                &
    'SOX (stomata_model=3) uses the SOX derivation of Collatz (photo_model=3)')
  END IF
END IF

RETURN

END SUBROUTINE init_pftparm_jules

#endif
