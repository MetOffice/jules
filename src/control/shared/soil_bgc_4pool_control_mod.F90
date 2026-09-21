! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Soil Biogeochemistry
! *****************************COPYRIGHT****************************************
!
! Some of the content of this file has been produced with the assistance of
! Met Office Github Copilot Enterprise.

MODULE soil_bgc_4pool_control_mod

! Description:
!   Control routine coupling litter carbon produced by a dynamic vegetation
!   model into the 4-pool (DPM/RPM/BIO/HUM) soil carbon model.
!
!   This is deliberately kept separate from next_gen_biogeochem_mod.F90 (the
!   veg3/RED vegetation dynamics control), and is called directly from
!   surf_couple_extra_mod.F90, so that soil biogeochemistry remains modular
!   and independent of the choice of vegetation dynamics model driving it.

IMPLICIT NONE

PRIVATE
!Make routines available
PUBLIC :: soil_bgc_4pool_control

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='SOIL_BGC_4POOL_CONTROL_MOD'

CONTAINS

!------------------------------------------------------------------------------
SUBROUTINE soil_bgc_4pool_control(                                      &
                !IN Control vars
                veg_index_pts,veg_index,land_pts,nnpft,veg3_ctrl,soil_parms,   &
                !IN vegetation-derived fluxes (plain arrays rather than the
                ! veg_state derived type, so this routine has no dependency
                ! on the choice of vegetation dynamics model)
                frac,litCpft,litC,npp_n_gb,                                   &
                !INOUT state
                soil_state,                                                    &
                !OUT diagnostics
                nbp_gb                                                         &
                )

! Couples litter carbon to the soil carbon model, mirroring the soil carbon
! section of TRIFFID (see triffid_jls.F90 and
! soilcarb_layers_jls_mod.F90/soilcarb_jls.F90). Requires the 4-pool soil
! carbon model (soil_bgc_model=soil_model_4pool), either layered
! (l_layeredC=.TRUE.) or single-layer; nitrogen is not yet coupled to
! veg3/RED.

!Only get the data structures - the data comes through the calling tree
USE veg3_parm_mod,                    ONLY: veg3_ctrl_type, soil_parm_type
USE soil_bgc_4pool_field_mod,  ONLY: soil_state_type

!Access subroutines
USE soilcarb_layers_mod, ONLY: soilcarb_layers
USE soilcarb_mod,        ONLY: soilcarb
#if !defined(UM_JULES)
USE soilcarb_mix_mod, ONLY: soilcarb_mix
#endif

!Access some parameters direct from module
USE conversions_mod, ONLY: rsec_per_day

IMPLICIT NONE

!----------------------------------------------------------------------------
! Integers with INTENT IN
!----------------------------------------------------------------------------
INTEGER, INTENT(IN)    :: land_pts,nnpft,veg_index_pts,veg_index(land_pts)

!-----------------------------------------------------------------------------
! Objects with INTENT IN
!-----------------------------------------------------------------------------
TYPE(veg3_ctrl_type),INTENT(IN)   :: veg3_ctrl
TYPE(soil_parm_type),INTENT(IN)   :: soil_parms

!-----------------------------------------------------------------------------
! Reals with INTENT IN
!-----------------------------------------------------------------------------
REAL, INTENT(IN) ::                                                            &
frac(land_pts,nnpft),                                                          &
    ! PFT fractional coverage of the gridbox (-).
litCpft(land_pts,nnpft),                                                       &
    ! Litter carbon flux normalised per unit PFT canopy area.
    ! (kg C m-2 (360d)-1)
litC(land_pts),                                                                &
    ! Gridbox mean total litter carbon flux. (kg C m-2 (360d)-1)
npp_n_gb(land_pts)
    ! Gridbox mean NPP after nitrogen limitation. (kg C m-2 (360d)-1)

!-----------------------------------------------------------------------------
! Objects with INTENT INOUT
!-----------------------------------------------------------------------------
TYPE(soil_state_type),INTENT(IN OUT)  :: soil_state

!-----------------------------------------------------------------------------
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT) ::                                                           &
nbp_gb(land_pts)
    ! Gridbox mean net biosphere productivity (NPP minus all carbon fluxes
    ! out of land). Only soil respiration is currently coupled to
    ! vegetation dynamics, so this is npp_n_gb minus the soil-to-atmosphere
    ! respiration flux. (kg C m-2 (360d)-1)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
INTEGER :: l,n,k
    ! Loop counters.

REAL ::                                                                        &
inv_timestep,                                                                  &
    ! Inverse soil carbon coupling timestep ((360d)-1).
resp_frac(land_pts,soil_parms%dim_cslayer),                                    &
    ! The fraction of soil respiration that forms new soil C (i.e. is NOT
    ! released to the atmosphere).
resp_frac_cspool(land_pts,soil_parms%dim_cslayer,soil_parms%dim_cs1),          &
    ! As resp_frac, but broken down by soil C pool. Only used for
    ! l_layeredC.
resp_s_dr(land_pts,soil_parms%dim_cslayer,5),                                  &
    ! Mean soil respiration for driving the soil carbon update
    ! (kg C/m2/360days). NB 5=dim_cs1+1; the 5th element is workspace.
lit_c_pft_gb(land_pts,nnpft),                                                   &
    ! Litter carbon flux from each PFT normalised to gridbox area (i.e.
    ! litCpft weighted by frac), as required by soilcarb/soilcarb_layers.
    ! Held as a local copy so the litCpft argument is never modified.
    ! (kg C/m2/360days)
lit_n_t_gb(land_pts),                                                          &
    ! Total nitrogen litter (kg N/m2/360days). The nitrogen cycle is not
    ! yet coupled to veg3/RED, so this is always zero.
cs_tot(land_pts,soil_parms%dim_cslayer),                                       &
    ! Soil carbon content (kg C/m2).
ns_gb(land_pts,soil_parms%dim_cslayer),                                        &
    ! Total soil N on layers (kg N/m2). Always zero (see lit_n_t_gb).
neg_n(land_pts),                                                               &
    ! Negative N required to prevent ns<0 (kg N). Unused (l_nitrogen=F).
implicit_resp_correction(land_pts),                                            &
    ! Respiration carried to next coupling period to account for applying
    ! the minimum soil carbon constraint (kg m-2).
isunfrozen(land_pts,soil_parms%dim_cslayer),                                   &
    ! Matrix to mask out frozen layers (inaccessible to plants). Assumed
    ! unfrozen throughout, consistent with the nitrogen cycle being off.
burnt_soil(land_pts),                                                          &
    ! Burnt C in RPM and DPM pools (kg m-2 360d-1). Fire is not yet coupled
    ! to veg3/RED, so this is always zero.
lit_frac(soil_parms%dim_cslayer),                                              &
    ! Litter fraction into each soil layer.
dcs(land_pts,soil_parms%dim_cslayer),                                          &
    ! Change in soil carbon over the coupling period (kg C/m2).
denom_resp,                                                                    &
    ! Denominator for calculating resp_s_acc_soilt.
#if !defined(UM_JULES)
dcs_pools(land_pts,soil_parms%dim_cslayer,4),                                  &
    ! Soil carbon by pool at the start of the coupling period, used to
    ! calculate the layer mixing term (kg C/m2).
mix_s(land_pts,soil_parms%dim_cslayer-1,4),                                    &
    ! Diffusion coefficient for soil C between soil layers (m2 360d-1).
    ! Equation 15 of Burke et al. (2017),
    ! https://www.geosci-model-dev.net/10/959/2017/gmd-10-959-2017.pdf
#endif
mix_term(land_pts,soil_parms%dim_cslayer,4),                                   &
    ! Mixing term for calculating the respiration correction
    ! (kg C/m2/360days).
lit_resp
    ! Net litter carbon reaching the soil after allowing for the change in
    ! soil carbon and respiration (kg C/m2/360days).

!End of headers

! Nitrogen and fire are not yet coupled to veg3/RED soil carbon - keep the
! associated inputs at zero/unfrozen so that the shared soilcarb/
! soilcarb_layers routines behave as a pure carbon-only 4-pool model.
lit_n_t_gb(:)   = 0.0
ns_gb(:,:)      = 0.0
burnt_soil(:)   = 0.0
isunfrozen(:,:) = 1.0

! Inverse soil carbon coupling timestep (/360days). Note this must be based
! on the vegetation dynamics/soil carbon coupling period (dt_red), not the
! raw physics timestep - resp_s_acc_soilt is accumulated over a full
! coupling period between calls to this routine, mirroring r_gamma/gam_trif
! in TRIFFID (veg-veg2a_jls_mod.F90).
inv_timestep = 1.0 / (veg3_ctrl%dt_red / rsec_per_day / 360.0)

resp_s_dr(:,:,:) = 0.0

DO k = 1,veg_index_pts
  l = veg_index(k)
  DO n = 1,soil_parms%dim_cslayer
    resp_s_dr(l,n,1) = soil_state%resp_s_acc_soilt(l,1,n,1) * inv_timestep
    resp_s_dr(l,n,2) = soil_state%resp_s_acc_soilt(l,1,n,2) * inv_timestep
    resp_s_dr(l,n,3) = soil_state%resp_s_acc_soilt(l,1,n,3) * inv_timestep
    resp_s_dr(l,n,4) = soil_state%resp_s_acc_soilt(l,1,n,4) * inv_timestep

    soil_state%resp_s_dr_out_gb(l,n,1) = resp_s_dr(l,n,1)
    soil_state%resp_s_dr_out_gb(l,n,2) = resp_s_dr(l,n,2)
    soil_state%resp_s_dr_out_gb(l,n,3) = resp_s_dr(l,n,3)
    soil_state%resp_s_dr_out_gb(l,n,4) = resp_s_dr(l,n,4)
    soil_state%resp_s_dr_out_gb(l,n,5) =                                       &
      SUM(soil_state%resp_s_dr_out_gb(l,n,1:4))

    ! Save the soil carbon at the start of the coupling period, used below
    ! to diagnose the change in soil carbon.
    dcs(l,n) = SUM(soil_state%cs_pool_soilt(l,1,n,1:4))

#if !defined(UM_JULES)
    dcs_pools(l,n,:) = soil_state%cs_pool_soilt(l,1,n,1:4)
#endif

    ! Fraction of soil respiration that forms new soil C (i.e. is NOT
    ! released to the atmosphere), calculated from the clay content.
    resp_frac(l,n) = 1.0 / (soil_parms%resp_frac_a + soil_parms%resp_frac_b *  &
                     EXP(soil_parms%resp_frac_c * 100.0 *                      &
                     soil_state%clay_soilt(l,1,n)))
    resp_frac_cspool(l,n,:) = resp_frac(l,n)

  END DO

  ! Convert the litter carbon from per-PFT-area to gridbox area units, as
  ! required by soilcarb/soilcarb_layers. Held in a local array so the
  ! litCpft argument itself is never modified.
  DO n = 1,nnpft
    lit_c_pft_gb(l,n) = litCpft(l,n) * frac(l,n)
  END DO

END DO

! Layered vs single-layer 4-pool soil carbon, as per triffid_jls.F90.
IF (soil_parms%l_layeredc) THEN
  CALL soilcarb_layers(land_pts, veg_index_pts, veg_index, 0.0,                &
                       inv_timestep, lit_c_pft_gb, litC,                       &
                       lit_n_t_gb, resp_frac_cspool, resp_s_dr,                &
                       soil_state%cs_pool_soilt(:,1,:,:),                      &
                       soil_state%frac_c_label_pool_soilt(:,1,:,:),            &
                       ns_gb, neg_n, implicit_resp_correction, burnt_soil,     &
                       isunfrozen, soil_state%ns_pool_gb,                      &
                       soil_state%n_inorg_soilt_lyrs,                          &
                       soil_state%n_inorg_avail_pft,                           &
                       soil_state%t_soil_soilt_acc,                            &
                       soil_state%burnt_carbon_dpm, soil_state%g_burn_gb,      &
                       soil_state%burnt_carbon_rpm, soil_state%minl_n_gb,      &
                       soil_state%minl_n_pot_gb, soil_state%immob_n_gb,        &
                       soil_state%immob_n_pot_gb, soil_state%fn_gb,            &
                       soil_state%resp_s_diag_gb,                              &
                       soil_state%resp_s_pot_diag_gb,                          &
                       soil_state%dpm_ratio_gb, soil_state%n_gas_gb,           &
                       soil_state%resp_s_to_atmos_gb, soil_state%sthu_soilt)
ELSE
  CALL soilcarb(land_pts, veg_index_pts, veg_index, 0.0,                       &
                inv_timestep, lit_c_pft_gb, litC,                              &
                lit_n_t_gb, resp_frac(:,1), resp_s_dr,                         &
                soil_state%cs_pool_soilt(:,1,:,:),                             &
                ns_gb, neg_n, implicit_resp_correction, burnt_soil,            &
                soil_state%ns_pool_gb, soil_state%n_inorg_soilt_lyrs,          &
                soil_state%burnt_carbon_dpm, soil_state%g_burn_gb,             &
                soil_state%burnt_carbon_rpm, soil_state%minl_n_gb,             &
                soil_state%minl_n_pot_gb, soil_state%immob_n_gb,               &
                soil_state%immob_n_pot_gb, soil_state%fn_gb,                   &
                soil_state%resp_s_diag_gb, soil_state%resp_s_pot_diag_gb,      &
                soil_state%dpm_ratio_gb, soil_state%n_gas_gb,                  &
                soil_state%resp_s_to_atmos_gb, soil_state%sthu_soilt)
END IF

!-----------------------------------------------------------------------------
! Diagnose the mean soil respiration to drive the soil carbon update over
! the next coupling period, correcting for any minimum soil carbon
! constraint applied above.
!-----------------------------------------------------------------------------
mix_term(:,:,:) = 0.0
lit_frac(:)     = 1.0

#if !defined(UM_JULES)
! Layer profile/mixing only apply with more than one soil C layer; for a
! single layer all litter reaches the one layer (lit_frac=1.0) and there is
! nothing to mix.
IF (soil_parms%l_layeredc) THEN
  ! Calculate vertical profile of litter inputs.
  lit_frac(1) = soil_parms%dzsoil(1) *                                         &
                EXP( -soil_parms%tau_lit * 0.5 * soil_parms%dzsoil(1) ) /      &
                soil_parms%litc_norm
  DO n = 2,soil_parms%dim_cslayer
    lit_frac(n) = soil_parms%dzsoil(n) * EXP( -soil_parms%tau_lit *            &
                  (SUM(soil_parms%dzsoil(1:n-1)) + 0.5 *                       &
                  soil_parms%dzsoil(n)) ) / soil_parms%litc_norm
  END DO

  ! Calculate the mixing term between soil layers.
  CALL soilcarb_mix(land_pts, veg_index_pts, veg_index, dcs_pools,             &
                     soil_state%t_soil_soilt_acc, mix_term, mix_s)
END IF
#endif

DO k = 1,veg_index_pts
  l = veg_index(k)
  DO n = 1,soil_parms%dim_cslayer
    cs_tot(l,n) = MAX(1.0e-10, SUM(soil_state%cs_pool_soilt(l,1,n,1:4)))
    denom_resp  = 1.0 / (cs_tot(l,n) * inv_timestep)
    dcs(l,n)    = cs_tot(l,n) - dcs(l,n)

    resp_s_dr(l,n,1) = SUM((1.0 - resp_frac_cspool(l,n,1:4)) *                 &
                           resp_s_dr(l,n,1:4))

    lit_resp = litC(l) * lit_frac(n) - (inv_timestep * dcs(l,n))               &
               - resp_s_dr(l,n,1) - burnt_soil(l) + SUM(mix_term(l,n,:))

    soil_state%resp_s_acc_soilt(l,1,n,1) = lit_resp *                          &
      soil_state%cs_pool_soilt(l,1,n,1) * denom_resp
    soil_state%resp_s_acc_soilt(l,1,n,2) = lit_resp *                          &
      soil_state%cs_pool_soilt(l,1,n,2) * denom_resp
    soil_state%resp_s_acc_soilt(l,1,n,3) = lit_resp *                          &
      soil_state%cs_pool_soilt(l,1,n,3) * denom_resp
    soil_state%resp_s_acc_soilt(l,1,n,4) = lit_resp *                          &
      soil_state%cs_pool_soilt(l,1,n,4) * denom_resp
  END DO
END DO

! Diagnose the net biosphere productivity (NPP minus soil respiration to
! atmosphere) for each land point.
DO k = 1,veg_index_pts
  l = veg_index(k)
  nbp_gb(l) = npp_n_gb(l) - SUM(soil_state%resp_s_to_atmos_gb(l,:))
END DO

END SUBROUTINE soil_bgc_4pool_control

END MODULE soil_bgc_4pool_control_mod
