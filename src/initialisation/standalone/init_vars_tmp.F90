#if !defined(UM_JULES)

SUBROUTINE init_vars_tmp(crop_vars,psparms,toppdm,ainfo,trif_vars, aerotype,   &
                         progs, trifctltype, coast, jules_vars,                &
                          !fluxes, &
                          !lake, &
                          forcing                                              &
                          !rivers, &
                          !veg3_parm, &
                          !veg3_field, &
                          !chemvars, &
                          )

USE Ancil_info, ONLY: lice_pts
USE jules_deposition_mod, ONLY: dzl_const, l_deposition
USE jules_vegetation_mod, ONLY: l_triffid, l_use_pft_psi, fsmc_shape,          &
                                l_phenol

USE trif, ONLY: lai_min

USE pftparm, ONLY: fsmc_mod, psi_close, psi_open
USE ancil_info, ONLY: land_pts, nsoilt
USE jules_sea_seaice_mod, ONLY: z0hsea, alpham, alphac, alphab, dtice
USE C_kappai, ONLY: kappai, de

USE hyd_psi_mod, ONLY: sthu_from_psi

USE jules_sea_seaice_mod, ONLY: l_ssice_albedo

USE calc_c_comps_triffid_mod, ONLY: calc_c_comps_triffid

USE CN_utils_mod, ONLY: calc_n_comps_triffid

USE update_mod, ONLY: bl_height, l_imogen

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE jules_surface_types_mod, ONLY: npft, nnpft

USE jules_soil_mod, ONLY: sm_levels, soil_props_const_z

USE dump_mod, ONLY: ancil_dump_read

USE um_types, ONLY: real_jlslsm

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type
USE p_s_parms,     ONLY: psparms_type
USE top_pdm,       ONLY: top_pdm_type
USE ancil_info,    ONLY: ainfo_type
USE trif_vars_mod, ONLY: trif_vars_type
USE aero,          ONLY: aero_type
USE prognostics,   ONLY: progs_type
USE trifctl,       ONLY: trifctl_type
USE coastal,       ONLY: coastal_type
USE jules_vars_mod,ONLY: jules_vars_type
! USE fluxes, ONLY: fluxes_type
! USE lake_mod, ONLY: lake_type
USE jules_forcing_mod, ONLY: forcing_type
! USE jules_rivers_mod, ONLY: rivers_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
! USE jules_chemvars_mod, ONLY: chemvars_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Initialises various variables that may change their initialisation in
!   future versions
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(aero_type), INTENT(IN OUT) :: aerotype
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(coastal_type), INTENT(IN OUT) :: coast
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
!TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
!TYPE(lake_type), INTENT(IN OUT) :: lake
TYPE(forcing_type), INTENT(IN OUT) :: forcing
!TYPE(rivers_type), INTENT(IN OUT) :: rivers
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field
!TYPE(chemvars_type), INTENT(IN OUT) :: chemvars

! Work variables
REAL(KIND=real_jlslsm) :: phen ! Phenological state (=LAI/lai_bal)

INTEGER :: i,l,n,ft,m  ! Loop counters

!-----------------------------------------------------------------------
! Type Definitions
!-----------------------------------------------------------------------
TYPE(top_pdm_type), INTENT(IN OUT) :: toppdm
TYPE(trifctl_type), INTENT(IN OUT)  :: trifctltype

!-----------------------------------------------------------------------
! Initialise accumulated fluxes for TRIFFID and phenology.
! This is not necessary if these are read from a restart file - but at
! present they're not.
!-----------------------------------------------------------------------
trifctltype%g_leaf_acc_pft(:,:)       = 0.0
trifctltype%g_leaf_phen_acc_pft(:,:)  = 0.0
trifctltype%npp_acc_pft(:,:)          = 0.0
trifctltype%resp_s_acc_soilt(:,:,:,:) = 0.0
trifctltype%resp_w_acc_pft(:,:)       = 0.0

!-----------------------------------------------------------------------
! Set saturated hydraulic conductivity to zero at land ice points.
!-----------------------------------------------------------------------
IF ( lice_pts > 0 ) THEN
  CALL log_info("init_vars_tmp",                                               &
                "Setting satcon to zero at land ice points")
  DO i = 1,lice_pts
    l = ainfo%lice_index(i)
    psparms%satcon_soilt(l,:,:) = 0.0
  END DO
END IF

!-----------------------------------------------------------------------
! Set surface velocity to be zero
!-----------------------------------------------------------------------
forcing%u_0_ij(:,:) = 0.0
forcing%v_0_ij(:,:) = 0.0

!-----------------------------------------------------------------------
! Set boundary layer height and layer separation (if needed).
! These might later be updated via prescribed data.
!-----------------------------------------------------------------------
jules_vars%zh(:,:) = bl_height
IF ( l_deposition ) jules_vars%dzl(:,:,:) = dzl_const

!-----------------------------------------------------------------------
! Set CO2 variables
!-----------------------------------------------------------------------
aerotype%co2_3d_ij(:,:) = 0.0

!-----------------------------------------------------------------------
! Set coastal tiling variables
!-----------------------------------------------------------------------
coast%tstar_sea_ij(:,:)       = 280.0
coast%tstar_sice_sicat(:,:,:) = 270.0

!-----------------------------------------------------------------------
! Set orographic roughness variables
!-----------------------------------------------------------------------
jules_vars%h_blend_orog_ij(:,:) = 0.0
jules_vars%sil_orog_land_gb(:)  = 0.0
jules_vars%ho2r2_orog_gb(:)     = 0.0

!-----------------------------------------------------------------------
! Set up prognostics which are not currently in dump
!-----------------------------------------------------------------------
ainfo%ti_cat_sicat(:,:,:)         = 270.0
progs%z0msea_ij(:,:)              = z0hsea
progs%snow_mass_ij(:,:)           = 0.0
ainfo%ice_fract_ncat_sicat(:,:,:) = 0.0
progs%di_ncat_sicat(:,:,:)        = 0.0
progs%snow_mass_sea_sicat(:,:,:)  = 0.0
progs%k_sice_sicat(:,:,:)         = 2.0 * kappai / de

!-----------------------------------------------------------------------
! Set up sea-ice parameter variables
!-----------------------------------------------------------------------
IF (l_ssice_albedo) THEN
  alpham = 0.65
  alphac = 0.80
  alphab = 0.57
  dtice = 2.00
ELSE
  alpham = 0.50
  alphac = 0.80
  alphab=-1.00
  dtice = 10.00
END IF

progs%soot_ij(:,:) = 0.0

!-----------------------------------------------------------------------------
! Unless IMOGEN is on, cv will not have been initialised
! Unless TRIFFID is on, it will also not be used...
!-----------------------------------------------------------------------------
IF ( .NOT. l_imogen ) trifctltype%cv_gb(:) = 0.0

! Initialise c_veg to 0 as not doing so can cause issues with IMOGEN outputs
trifctltype%c_veg_pft(:,:) = 0.0

!-----------------------------------------------------------------------------
! Initialising TRIFFID Diagnostic Veg Pools and fluxes prior to first TRIFFID call
! This is necessary to ensure time average diagnostics are produced correctly.
!-----------------------------------------------------------------------------

IF (l_triffid) THEN
  trif_vars%rootC_pft       = 0.0
  trif_vars%woodC_pft       = 0.0
  trif_vars%leafC_pft       = 0.0
  trifctltype%c_veg_pft       = 0.0
  trif_vars%n_leaf_pft      = 0.0
  trif_vars%n_root_pft      = 0.0
  trif_vars%n_stem_pft      = 0.0
  trif_vars%lai_bal_pft     = 0.0
  trifctltype%cv_gb           = 0.0
  trif_vars%n_veg_gb        = 0.0
  trif_vars%wp_fast_out_gb  = 0.0
  trif_vars%wp_med_out_gb   = 0.0
  trif_vars%wp_slow_out_gb  = 0.0
  trif_vars%wp_fast_in_gb   = 0.0
  trif_vars%wp_med_in_gb    = 0.0
  trif_vars%wp_slow_in_gb   = 0.0

  IF ( .NOT. l_phenol) THEN
    DO n = 1,nnpft
      progs%lai_pft(:,n) = lai_min(n)
    END DO
  END IF

  DO l = 1,land_pts
    DO n = 1,nnpft
      IF (ainfo%frac_surft(l,n) > 0.0) THEN

        CALL calc_c_comps_triffid(n, progs%canht_pft(l,n),                     &
                                  trif_vars%lai_bal_pft(l,n),                  &
                                  trif_vars%leafC_pft(l,n),                    &
                                  trif_vars%rootC_pft(l,n),                    &
                                  trif_vars%woodC_pft(l,n),                    &
                                  trifctltype%c_veg_pft(l,n))

        IF (l_phenol) THEN
          phen = progs%lai_pft(l,n) / trif_vars%lai_bal_pft(l,n)
          IF ( phen > 1.0 + TINY(1.0e0) ) THEN
            CALL log_warn("init_vars_tmp",                                     &
                          "lai_pft should be <= trif_vars%lai_bal_pft")
          END IF
        ELSE
          phen = 1.0
          progs%lai_pft(l,n) = trif_vars%lai_bal_pft(l,n)
        END IF

        CALL calc_n_comps_triffid(l ,n, phen, trif_vars%lai_bal_pft(l,n),      &
                                  trif_vars%woodC_pft(l,n),                    &
                                  trif_vars%rootC_pft(l,n),                    &
                                  trif_vars%n_leaf_pft(l,n),                   &
                                  trif_vars%n_root_pft(l,n),                   &
                                  trif_vars%n_stem_pft(l,n),                   &
                                  crop_vars%dvi_cpft)

        trif_vars%n_veg_pft(l,n) = trif_vars%n_leaf_pft(l,n) +                 &
                      trif_vars%n_root_pft(l,n) + trif_vars%n_stem_pft(l,n)

        trifctltype%cv_gb(l) = trifctltype%cv_gb(l) +                          &
                      ainfo%frac_surft(l,n) * trifctltype%c_veg_pft(l,n)

        trif_vars%n_veg_gb(l) = trif_vars%n_veg_gb(l) +                        &
                      ainfo%frac_surft(l,n) * trif_vars%n_veg_pft(l,n)
      END IF
    END DO
  END DO
END IF

!------------------------------------------------------------------
! Temporary initialisation of variables added during UM integration
!------------------------------------------------------------------

toppdm%inlandout_atm_gb(:) = 0.0

!------------------------------------------------------------------
! Initialisation of v_close_soilt_pft and v_open_soilt_pft
!------------------------------------------------------------------

IF ( l_use_pft_psi ) THEN
  ! now set v_close_soilt_pft and v_open_soilt_pft
  DO ft = 1,npft

    !Set the current soil tile (see *NOTICE REGARDING SOIL TILING* in physiol)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = ft
    END IF !nsoilt

    DO n = 1,sm_levels
      DO i = 1,land_pts
        psparms%v_close_pft(i,n,ft) = sthu_from_psi(psi_close(ft),             &
                                      psparms%sathh_soilt(i,m,n),              &
                                      psparms%bexp_soilt(i,m,n),               &
                                      0.01) * psparms%smvcst_soilt(i,m,n)
        psparms%v_open_pft(i,n,ft) = sthu_from_psi(psi_open(ft),               &
                                     psparms%sathh_soilt(i,m,n),               &
                                     psparms%bexp_soilt(i,m,n),                &
                                     0.01) * psparms%smvcst_soilt(i,m,n)
      END DO
    END DO
  END DO
END IF

IF ((fsmc_shape == 1) .AND. ANY(fsmc_mod == 1)) THEN
  IF ( ancil_dump_read%soil_props ) THEN
    CALL log_fatal("init_vars_tmp",                                            &
                   "fsmc_shape=fsmc_mod=1 can not currently be used " //       &
                   "when JULES_SOIL_PROPS has read_from_dump=T.")
    ! FIXME: could allow this if the values in the soil ancils are checked
  END IF
  IF ( .NOT. soil_props_const_z ) THEN
    CALL log_fatal("init_vars_tmp",                                            &
                   "fsmc_shape=fsmc_mod=1 requires " //                        &
                   "const_z=T in JULES_SOIL_PROPS.")
  END IF
END IF

RETURN

END SUBROUTINE init_vars_tmp
#endif
