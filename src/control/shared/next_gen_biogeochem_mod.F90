! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in Veg3 Ecosystem Demography
! *****************************COPYRIGHT****************************************

MODULE next_gen_biogeochem_mod

IMPLICIT NONE

PRIVATE
!Make routines available
PUBLIC :: next_gen_biogeochem

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='NEXT_GEN_BIOGEOCHEM_MOD'

CONTAINS

SUBROUTINE next_gen_biogeochem(                                                &
        !IN control vars
          asteps_since_triffid,a_step,land_pts,nnpft,nmasst,veg3_ctrl,         &
          ainfo,                                                               &
        !IN parms
          litter_parms,red_parms,soil_parms,                                   &
        !INOUT data structures
          veg_state,red_state,soil_state                                      &
        !OUT diagnostics
        )

!Only get the data structures - the data comes through the calling tree

USE veg3_parm_mod,           ONLY:                                             &
  veg3_ctrl_type,litter_parm_type,red_parm_type,soil_parm_type

USE veg3_field_mod,          ONLY:                                             &
  veg_state_type,red_state_type,soil_state_type

USE ancil_info,              ONLY: ainfo_type

IMPLICIT NONE

!----------------------------------------------------------------------------
! Objects with INTENT in
!----------------------------------------------------------------------------
TYPE(veg3_ctrl_type), INTENT(IN)     :: veg3_ctrl
TYPE(litter_parm_type), INTENT(IN)   :: litter_parms
TYPE(red_parm_type), INTENT(IN)      :: red_parms
TYPE(soil_parm_type), INTENT(IN)     :: soil_parms

!----------------------------------------------------------------------------
! Objects with INTENT inout
!----------------------------------------------------------------------------
TYPE(veg_state_type), INTENT(IN OUT)  :: veg_state
TYPE(red_state_type), INTENT(IN OUT)  :: red_state
TYPE(soil_state_type), INTENT(IN OUT) :: soil_state
TYPE(ainfo_type), INTENT(IN OUT)      :: ainfo

!----------------------------------------------------------------------------
! INTEGERS with INTENT in
!----------------------------------------------------------------------------
INTEGER, INTENT(IN)    :: land_pts, nnpft, nmasst
INTEGER, INTENT(IN)    :: a_step
                      ! Current atmospheric timestep number, used to
                      ! determine when the phenology timestep falls due.

!----------------------------------------------------------------------------
! Variables with INTENT inout
!----------------------------------------------------------------------------

INTEGER, INTENT(IN OUT) ::                                                     &
asteps_since_triffid
                      ! IN Number of atmospheric timesteps since last call
                      !    to TRIFFID.

!-----------------------------------------------------------------------------
! Local Objects
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Local Variables
!-----------------------------------------------------------------------------

INTEGER ::                                                                     &
  veg_index(land_pts),                                                         &
  veg_index_pts,                                                               &
  l,n
        ! Counters

REAL ::                                                                        &
  frac_vs(land_pts)
        ! Veg/Soil Fractional coverage

LOGICAL ::                                                                     &
  l_veg_step    
  ! Flag to indicate whether this is a vegetation dynamics timestep.

! End of header

!---------------------------------------------------------------------
! Find total fraction of gridbox covered by vegetation and soil, and
! use this to set indices of land points on which veg3 may operate.
!---------------------------------------------------------------------
veg_index_pts = 0
DO l = 1,land_pts
  frac_vs(l) = 0.0
  DO n = 1,nnpft
    frac_vs(l) = frac_vs(l) + veg_state%frac(l,n)
  END DO
  frac_vs(l) = frac_vs(l) + veg_state%frac(l,veg3_ctrl%soil)
  IF ( frac_vs(l) >= REAL(nnpft) *  veg3_ctrl%frac_min ) THEN
    veg_index_pts = veg_index_pts + 1
    veg_index(veg_index_pts) = l
  END IF
END DO


l_veg_step = (asteps_since_triffid == veg3_ctrl%nstep_trif)

! Call the Vegetation Biogeochemistry model
IF (veg_index_pts > 0) CALL veg3_run_ctrl(                                     &
              !IN Control vars
              asteps_since_triffid,a_step,land_pts,nnpft,nmasst,veg_index_pts, &
              veg_index,veg3_ctrl,ainfo,                                       &
              !IN parms
              litter_parms,red_parms,                                          &
              !IN state
              veg_state,red_state                                              &
              !OUT Diagnostics
              )

! Call the soil Biogeochemistry model
IF (veg_index_pts > 0 .AND. l_veg_step) CALL veg3_soil_couple(                &
              !IN Control vars
              veg_index_pts,veg_index,land_pts,nnpft,veg3_ctrl,soil_parms,     &
              !INOUT state
              veg_state,soil_state                                             &
              )

END SUBROUTINE next_gen_biogeochem

!------------------------------------------------------------------------------
SUBROUTINE veg3_run_ctrl(                                                      &
                !IN Control vars
                asteps_since_triffid,a_step,land_pts,nnpft,nmasst,             &
                veg_index_pts,veg_index,veg3_ctrl,ainfo,                       &
                !IN parms
                litter_parms,red_parms,                                        &
                !IN state
                veg_state,red_state                                            &
                !OUT Diagnostics
                )

!Only get the data structures - the data comes through the calling tree
USE veg3_parm_mod,   ONLY:  veg3_ctrl_type,litter_parm_type,red_parm_type
USE veg3_field_mod,  ONLY:  veg_state_type,red_state_type,red_veg3_couple
USE veg3_litter_mod, ONLY:  veg3_litter
USE ancil_info,      ONLY: ainfo_type

!Access subroutines
USE veg3_red_dynamic_mod, ONLY:  veg3_red_dynamic

!Access some parameters direct from module
USE conversions_mod, ONLY: rsec_per_day

IMPLICIT NONE

!----------------------------------------------------------------------------
! Integers with INTENT IN
!----------------------------------------------------------------------------
INTEGER, INTENT(IN OUT) ::                                                     &
asteps_since_triffid
                      ! IN Number of atmospheric timesteps since last call
                      !    to TRIFFID.

INTEGER, INTENT(IN)    :: a_step
                      ! IN Current atmospheric timestep number, used to
                      !    determine when the phenology timestep falls due.

!-----------------------------------------------------------------------------
! Objects with INTENT IN
!-----------------------------------------------------------------------------
TYPE(veg3_ctrl_type),INTENT(IN)   :: veg3_ctrl
TYPE(litter_parm_type),INTENT(IN) :: litter_parms
TYPE(red_parm_type),INTENT(IN)    :: red_parms

!-----------------------------------------------------------------------------
! Objects with INTENT INOUT
!-----------------------------------------------------------------------------
TYPE(veg_state_type),INTENT(IN OUT)   :: veg_state
TYPE(red_state_type),INTENT(IN OUT)   :: red_state
TYPE(ainfo_type), INTENT(IN OUT)      :: ainfo

!----------------------------------------------------------------------------
! INTEGERS with INTENT in
!----------------------------------------------------------------------------
INTEGER, INTENT(IN)    :: land_pts,nnpft,nmasst,veg_index_pts,veg_index(land_pts)

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------

REAL::                                                                         &
npp_dr(land_pts,nnpft),                                                        &
    ! Mean NPP for driving vegetation (kg C/m2/s).
g_leaf_dr(land_pts,nnpft),                                                     &
    ! Mean phenology-driven leaf turnover rate for driving litterfall and
    ! vegetation dynamics (s-1).
local_litter(land_pts,nnpft),                                                  &
    ! Litter production (kg C/m2/s).
growth(land_pts,nnpft),                                                        &
    ! growth (kg C/m2/s).
mort_add(land_pts,nnpft,nmasst)
    ! mortality above baseline (/m2)

!End of headers

!Initialise Arrays
npp_dr(:,:)          = 0.0
g_leaf_dr(:,:)       = 0.0
mort_add(:,:,:)      = 0.0

!-----------------------------------------------------------------------------
! Work out the phenology at its own timestep, appending to the accumulated
! leaf turnover rates, and (on the vegetation dynamics timestep) diagnose
! the mean phenology-driven leaf turnover rate for driving litterfall and
! vegetation dynamics. This mirrors the phenology/TRIFFID timestep split in
! veg-veg2a_jls_mod.
!-----------------------------------------------------------------------------
CALL veg3_phenol_couple(veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,      &
                        a_step,asteps_since_triffid,veg_state,g_leaf_dr)

! Now call vegetation model
IF (asteps_since_triffid == veg3_ctrl%nstep_trif) THEN

  !Call Litter
  CALL veg3_Litter(                                                            &
                !IN Control vars
                veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,              &
                !IN parms
                litter_parms,                                                  &
                !IN fields
                g_leaf_dr,                                                     &
                !IN state
                veg_state,                                                     &
                ! OUT Fields
                local_litter                                                   &
                !OUT Diagnostics
                )

  ! Record driving g_leaf_dr and convert to s-1 -> (360 days)-1
  veg_state%g_leaf_dr_out(:,:) = g_leaf_dr * rsec_per_day * 360.0

  !CALL Allocation/Nitrogen/NSC


  !Work out growth

  ! Use the accumulated npp from sf_expl. Copy to new variable for driving
  ! veg dynamics

  npp_dr = veg_state%npp_acc / veg3_ctrl%dt_red

  ! Record driving npp_dr and convert to s-1 -> (360 days)-1
  veg_state%npp_dr_out(:,:) = npp_dr * rsec_per_day * 360

  ! Reset accumulation to zero - note this can be used to pass a negative flux
  ! back to JULES.
  veg_state%npp_acc(:,:)=0.0
  growth = npp_dr - local_litter

  !Call Allocation/Nitrogen/NSC

  !Now on Mass classes
  !Veg_dynamics_mass

  !Call Mortality+Disturbance - total disturbance term on mass classes
  ! mort_add should be calculated here

  !This could get complicated - what is the disturbance term to maintain a
  !managed gridbox fraction? Maybe need multiple calls but then the order matters.

  !Call Veg Dynamics
  !Call Veg Dynamics - in this case RED
  CALL veg3_red_dynamic(                                                       &
                  !IN control vars
                  veg3_ctrl%dt_red,veg_index_pts,veg_index,veg3_ctrl,land_pts, &
                  nnpft,nmasst,                                                &
                  !IN RED_parms
                  red_parms,                                                   &
                  !IN fields
                  growth,mort_add,                                             &
                  !IN state
                  veg_state,red_state                                          &
                  !OUT Diagnostics
                  )

  !Update Vegetation State
  CALL red_veg3_couple(ainfo)

  !Partition Density Dependent Litter

  !Call Harvest

  !Call Wood Products

  !Pass fVegLitterC and N out to soil bgc


END IF

IF ( asteps_since_triffid == veg3_ctrl%nstep_trif ) asteps_since_triffid = 0

END SUBROUTINE veg3_run_ctrl

!------------------------------------------------------------------------------
SUBROUTINE veg3_phenol_couple(                                                 &
                !IN Control vars
                veg_index_pts,veg_index,veg3_ctrl,land_pts,nnpft,              &
                a_step,asteps_since_triffid,                                   &
                !IN state
                veg_state,                                                     &
                !OUT Diagnostics
                g_leaf_dr                                                      &
                )

! Diagnoses leaf phenology and the mean phenology-driven leaf turnover rate
! that drives litterfall/vegetation dynamics, in veg1/veg2.

!Only get the data structures - the data comes through the calling tree
USE jules_vegetation_mod, ONLY: l_phenol
USE veg3_parm_mod,        ONLY: veg3_ctrl_type
USE veg3_field_mod,       ONLY: veg_state_type
USE phenol_mod,           ONLY: phenol

!Access some parameters direct from module
USE conversions_mod, ONLY: rsec_per_day

IMPLICIT NONE

!----------------------------------------------------------------------------
! Integers with INTENT IN
!----------------------------------------------------------------------------
INTEGER, INTENT(IN)    :: land_pts,nnpft,veg_index_pts,veg_index(land_pts)

INTEGER, INTENT(IN)    :: a_step
                      ! Current atmospheric timestep number, used to
                      ! determine when the phenology timestep falls due.

INTEGER, INTENT(IN)    :: asteps_since_triffid
                      ! Number of atmospheric timesteps since last call to
                      ! vegetation dynamics.

!-----------------------------------------------------------------------------
! Objects with INTENT IN
!-----------------------------------------------------------------------------
TYPE(veg3_ctrl_type),INTENT(IN)   :: veg3_ctrl

!-----------------------------------------------------------------------------
! Objects with INTENT INOUT
!-----------------------------------------------------------------------------
TYPE(veg_state_type),INTENT(IN OUT)   :: veg_state

!-----------------------------------------------------------------------------
! Reals with INTENT OUT
!-----------------------------------------------------------------------------
REAL, INTENT(OUT) :: g_leaf_dr(land_pts,nnpft)
              ! Mean phenology-driven leaf turnover rate for driving
              ! litterfall and vegetation dynamics (s-1).

!-----------------------------------------------------------------------------
!Local Vars
!-----------------------------------------------------------------------------
REAL ::                                                                        &
gam_trif,                                                                      &
              ! Inverse vegetation dynamics coupling timestep ((360d)-1).
lai_bal_dummy(land_pts)
              ! Dummy lai to pass into phenol routine, gets around bug where
              ! FORTRAN does not accept veg_state%lai_bal as optional
              ! argument.

INTEGER :: l,n,k
    ! Loop counters.

!End of headers

g_leaf_dr(:,:) = 0.0

!-----------------------------------------------------------------------------
! Work out the phenology at its own timestep, appending to the accumulated
! leaf turnover rates. This is called independently of the vegetation
! dynamics timestep, mirroring the phenology/TRIFFID timestep split in
! veg-veg2a_jls_mod.
!-----------------------------------------------------------------------------
IF (l_phenol .AND. MOD(a_step,veg3_ctrl%nstep_phen) == 0) THEN

  veg_state%phen(:,:) = 1.0
  lai_bal_dummy(:) = 0.0

  DO n = 1,nnpft
    lai_bal_dummy(:) = veg_state%lai_bal(:,n)

    ! Diagnose the mean leaf turnover rate driving phenology over the
    ! elapsed phenology period, mirroring g_leaf_day in veg-veg2a_jls_mod.
    DO k = 1,veg_index_pts
      l = veg_index(k)
      veg_state%g_leaf_day(l,n) = veg_state%g_leaf_acc(l,n) /                  &
                                  veg3_ctrl%dt_phen_360d
    END DO

    CALL phenol(land_pts,veg_index_pts,n,veg_index,veg3_ctrl%dt_phen_360d,     &
                veg_state%g_leaf_day(:,n),veg_state%canht(:,n),                &
                veg_state%lai(:,n),veg_state%g_leaf_phen(:,n),lai_bal_dummy)

    DO k = 1,veg_index_pts
      l = veg_index(k)

      ! Save the diagnosed LAI immediately following the phenology update,
      veg_state%lai_phen(l,n) = veg_state%lai(l,n)

      ! Accumulate the mean phenological leaf turnover rate for driving
      ! vegetation dynamics.
      veg_state%g_leaf_phen_acc(l,n) = veg_state%g_leaf_phen_acc(l,n)          &
                                       + veg_state%g_leaf_phen(l,n) *          &
                                         veg3_ctrl%dt_phen_360d

      ! Reset the accumulated physiological leaf turnover ready for the
      ! next phenology period.
      veg_state%g_leaf_acc(l,n) = 0.0

      IF (veg_state%lai_bal(l,n) > 0) veg_state%phen(l,n) =                    &
      veg_state%lai(l,n)/veg_state%lai_bal(l,n)

    END DO

  END DO

END IF

!-----------------------------------------------------------------------------
! On the vegetation dynamics timestep, diagnose the mean phenology-driven
! leaf turnover rate that will drive litterfall and vegetation dynamics.
!-----------------------------------------------------------------------------
IF (asteps_since_triffid == veg3_ctrl%nstep_trif) THEN

  ! Calculate the inverse vegetation dynamics coupling timestep.
  gam_trif = 360.0 / REAL(veg3_ctrl%triffid_period)

  DO n = 1,nnpft
    DO k = 1,veg_index_pts
      l = veg_index(k)

      IF (l_phenol) THEN
        ! Diagnose the mean phenological leaf turnover rate over the
        ! coupling period, in JULES-standard per-second units.
        g_leaf_dr(l,n) = veg_state%g_leaf_phen_acc(l,n) *                      &
                         gam_trif / (rsec_per_day * 360.0)

        ! Reset the accumulated phenological turnover ready for the next
        ! coupling period.
        veg_state%g_leaf_phen_acc(l,n) = 0.0
      ELSE
        ! No phenology - fall back to the raw accumulated physiological
        ! leaf turnover rate, as in veg-veg2a_jls_mod.
        g_leaf_dr(l,n) = veg_state%g_leaf_acc(l,n) * gam_trif /                &
                         (rsec_per_day * 360.0)

        veg_state%g_leaf_acc(l,n) = 0.0
      END IF

      ! Ensure the turnover will not remove more leaf than is present over
      ! the vegetation dynamics timestep. If it does, reduce the rate so
      ! that the turnover does not exceed the current LAI.
      IF (veg_state%lai(l,n) > 0.0) THEN
        IF (g_leaf_dr(l,n) * veg3_ctrl%dt_red > 1.0) THEN
          g_leaf_dr(l,n) = 1.0 / veg3_ctrl%dt_red

        END IF
      ELSE
        g_leaf_dr(l,n) = 0.0

      END IF

    END DO
  END DO

END IF

END SUBROUTINE veg3_phenol_couple

!------------------------------------------------------------------------------
SUBROUTINE veg3_soil_couple(                                                   &
                !IN Control vars
                veg_index_pts,veg_index,land_pts,nnpft,veg3_ctrl,soil_parms,   &
                !INOUT state
                veg_state,soil_state                                          &
                )

! Couples RED litter carbon to the soil carbon model, mirroring the soil
! carbon section of TRIFFID (see triffid_jls.F90 and
! soilcarb_layers_jls_mod.F90/soilcarb_jls.F90). Requires the 4-pool soil
! carbon model (soil_bgc_model=soil_model_4pool), either layered
! (l_layeredC=.TRUE.) or single-layer; nitrogen is not yet coupled to
! veg3/RED.

!Only get the data structures - the data comes through the calling tree
USE veg3_parm_mod,   ONLY: veg3_ctrl_type, soil_parm_type
USE veg3_field_mod,  ONLY: veg_state_type, soil_state_type

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
! Objects with INTENT INOUT
!-----------------------------------------------------------------------------
TYPE(veg_state_type),INTENT(IN OUT)   :: veg_state
TYPE(soil_state_type),INTENT(IN OUT)  :: soil_state

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
lit_n_t_gb(land_pts),                                                          &
    ! Total nitrogen litter (kg N/m2/360days). The nitrogen cycle is not
    ! yet coupled to veg3/RED, so this is always zero.
cs_tot(land_pts,soil_parms%dim_cslayer),                                       &
    ! Soil carbon content (kg C/m2).
ns_gb(land_pts,soil_parms%dim_cslayer),                                        &
    ! Total soil N on layers (kg N/m2). Always zero (see lit_n_t_gb).
neg_n(land_pts),                                                               &
    ! Negative N required to prevent ns<0 (kg N). Unused (l_nitrogen=F).
implicit_resp_correction(land_pts),                                           &
    ! Respiration carried to next coupling period to account for applying
    ! the minimum soil carbon constraint (kg m-2).
isunfrozen(land_pts,soil_parms%dim_cslayer),                                   &
    ! Matrix to mask out frozen layers (inaccessible to plants). Assumed
    ! unfrozen throughout, consistent with the nitrogen cycle being off.
burnt_soil(land_pts),                                                          &
    ! Burnt C in RPM and DPM pools (kg m-2 360d-1). Fire is not yet coupled
    ! to veg3/RED, so this is always zero.
lit_frac(soil_parms%dim_cslayer),                                             &
    ! Litter fraction into each soil layer.
dcs(land_pts,soil_parms%dim_cslayer),                                         &
    ! Change in soil carbon over the coupling period (kg C/m2).
denom_resp,                                                                   &
    ! Denominator for calculating resp_s_acc_soilt.
#if !defined(UM_JULES)
dcs_pools(land_pts,soil_parms%dim_cslayer,4),                                 &
    ! Soil carbon by pool at the start of the coupling period, used to
    ! calculate the layer mixing term (kg C/m2).
mix_s(land_pts,soil_parms%dim_cslayer-1,4),                                   &
    ! Diffusion coefficient for soil C between soil layers (m^2/360days).
    ! Equation 15 of Burke et al. (2017),
    ! https://www.geosci-model-dev.net/10/959/2017/gmd-10-959-2017.pdf
#endif
mix_term(land_pts,soil_parms%dim_cslayer,4),                                  &
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

  ! Convert the litter carbon from per-PFT-area to gridbox mean units, as
  ! required by soilcarb/soilcarb_layers.
  DO n = 1,nnpft
    veg_state%litCpft(l,n) = veg_state%litCpft(l,n) * veg_state%frac(l,n)
  END DO

END DO

! Layered vs single-layer 4-pool soil carbon, as per triffid_jls.F90.
IF (soil_parms%l_layeredc) THEN
  CALL soilcarb_layers(land_pts, veg_index_pts, veg_index, 0.0,                &
                       inv_timestep, veg_state%litCpft, veg_state%litC,        &
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
                inv_timestep, veg_state%litCpft, veg_state%litC,               &
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

! Convert the litter carbon back to per-PFT-area units.
DO k = 1,veg_index_pts
  l = veg_index(k)
  DO n = 1,nnpft
    IF (veg_state%frac(l,n) > 0.0) THEN
      veg_state%litCpft(l,n) = veg_state%litCpft(l,n) / veg_state%frac(l,n)
    ELSE
      veg_state%litCpft(l,n) = 0.0
    END IF
  END DO
END DO

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
  lit_frac(1) = soil_parms%dzsoil(1) *                                        &
                EXP( -soil_parms%tau_lit * 0.5 * soil_parms%dzsoil(1) ) /     &
                soil_parms%litc_norm
  DO n = 2,soil_parms%dim_cslayer
    lit_frac(n) = soil_parms%dzsoil(n) * EXP( -soil_parms%tau_lit *            &
                  (SUM(soil_parms%dzsoil(1:n-1)) + 0.5 *                      &
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

    lit_resp = veg_state%litC(l) * lit_frac(n) - (inv_timestep * dcs(l,n))     &
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

END SUBROUTINE veg3_soil_couple

END MODULE next_gen_biogeochem_mod
