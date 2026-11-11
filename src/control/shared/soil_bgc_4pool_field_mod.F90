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

MODULE soil_bgc_4pool_field_mod

! Description:
!   Holds the state variables/fields for the 4-pool (DPM/RPM/BIO/HUM) soil
!   carbon model when coupled to a dynamic vegetation model, and provides
!   routines to allocate, deallocate and associate them.
!
!   This is deliberately kept separate from next_gen_biogeochem_mod.F90 and
!   veg3_field_mod.F90 (the veg3/RED vegetation dynamics) so that the soil
!   biogeochemistry fields stay modular and independent of the choice of
!   vegetation dynamics model driving them.

IMPLICIT NONE

! Structure to keep the soil state variables and fields used in coupling a
! dynamic vegetation model (currently veg3/RED) to the soil carbon (and, in
! future, nitrogen) model.
TYPE :: soil_state_type
  REAL, POINTER ::                                                             &
      cs_pool_soilt(:,:,:,:),                                                  &
              ! Soil carbon pools (DPM,RPM,bio,humus), by soil tile and
              ! layer. (kg C m-2)
      frac_c_label_pool_soilt(:,:,:,:),                                        &
              ! Fraction of each soil C pool from labelled carbon.
      ns_pool_gb(:,:,:),                                                       &
              ! Soil nitrogen pools, by layer. (kg N m-2)
      n_inorg_soilt_lyrs(:,:,:),                                               &
              ! Gridbox inorganic N pool on soil levels. (kg N m-2)
      n_inorg_avail_pft(:,:,:),                                                &
              ! Available inorganic N for PFTs. (kg N m-2)
      t_soil_soilt_acc(:,:,:),                                                 &
              ! Accumulated soil temperature, used for soil C mixing. (K)
      clay_soilt(:,:,:),                                                       &
              ! Soil clay fraction, by layer.
      sthu_soilt(:,:,:),                                                       &
              ! Unfrozen soil moisture content as a fraction of saturation.
      resp_s_acc_soilt(:,:,:,:),                                               &
              ! Accumulated soil respiration since the last coupling call.
              ! (kg C m-2)
      resp_s_dr_out_gb(:,:,:),                                                 &
              ! Mean soil respiration driving the soil C update. (kg C m-2
              ! (360d)-1)
      burnt_carbon_dpm(:),                                                     &
              ! Burnt DPM carbon. (kg C m-2 (360d)-1)
      burnt_carbon_rpm(:),                                                     &
              ! Burnt RPM carbon. (kg C m-2 (360d)-1)
      g_burn_gb(:),                                                            &
              ! Gridbox mean fire disturbance rate. ((360d)-1)
      minl_n_gb(:,:,:),                                                        &
              ! Gross mineralisation of N. (kg N m-2 (360d)-1)
      minl_n_pot_gb(:,:,:),                                                    &
              ! Potential gross mineralisation of N. (kg N m-2 (360d)-1)
      immob_n_gb(:,:,:),                                                       &
              ! Immobilisation of N. (kg N m-2 (360d)-1)
      immob_n_pot_gb(:,:,:),                                                   &
              ! Potential immobilisation of N. (kg N m-2 (360d)-1)
      fn_gb(:,:),                                                              &
              ! Nitrogen decomposition rate modifier.
      resp_s_diag_gb(:,:,:),                                                   &
              ! Diagnosed soil respiration by pool. (kg C m-2 (360d)-1)
      resp_s_pot_diag_gb(:,:,:),                                               &
              ! Diagnosed potential soil respiration by pool.
              ! (kg C m-2 (360d)-1)
      dpm_ratio_gb(:),                                                         &
              ! Ratio of DPM carbon to total litter carbon.
      n_gas_gb(:,:),                                                           &
              ! Gaseous N loss. (kg N m-2 (360d)-1)
      resp_s_to_atmos_gb(:,:)
              ! Soil-to-atmosphere respiration flux. (kg C m-2 (360d)-1)
END TYPE soil_state_type

TYPE(soil_state_type)  :: soil_state

!Private by default
PRIVATE

!Expose routines
PUBLIC :: soil_bgc_4pool_allocate, soil_bgc_4pool_deallocate,                  &
          soil_bgc_4pool_assoc

!Expose data
PUBLIC :: soil_state

!Expose data structures
PUBLIC :: soil_state_type

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='SOIL_BGC_4POOL_FIELD_MOD'

CONTAINS
!-------------------------------------------------------------------------------

SUBROUTINE soil_bgc_4pool_allocate(land_pts,nnpft)

! Allocates and initialises the soil_state fields.

USE ancil_info,     ONLY: dim_cslayer, dim_cs1, nsoilt
USE jules_soil_mod, ONLY: sm_levels

IMPLICIT NONE
INTEGER, INTENT(IN) :: land_pts, nnpft

!End of header

ALLOCATE(soil_state%cs_pool_soilt          (land_pts,nsoilt,dim_cslayer,dim_cs1))
ALLOCATE(soil_state%frac_c_label_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1))
ALLOCATE(soil_state%ns_pool_gb             (land_pts,dim_cslayer,dim_cs1))
ALLOCATE(soil_state%n_inorg_soilt_lyrs     (land_pts,nsoilt,dim_cslayer))
ALLOCATE(soil_state%n_inorg_avail_pft      (land_pts,nnpft,dim_cslayer))
ALLOCATE(soil_state%t_soil_soilt_acc       (land_pts,nsoilt,sm_levels))
ALLOCATE(soil_state%clay_soilt             (land_pts,nsoilt,dim_cslayer))
ALLOCATE(soil_state%sthu_soilt             (land_pts,nsoilt,sm_levels))
ALLOCATE(soil_state%resp_s_acc_soilt       (land_pts,nsoilt,dim_cslayer,dim_cs1))
ALLOCATE(soil_state%resp_s_dr_out_gb       (land_pts,dim_cslayer,dim_cs1+1))
ALLOCATE(soil_state%burnt_carbon_dpm       (land_pts))
ALLOCATE(soil_state%burnt_carbon_rpm       (land_pts))
ALLOCATE(soil_state%g_burn_gb              (land_pts))
ALLOCATE(soil_state%minl_n_gb              (land_pts,dim_cslayer,dim_cs1+1))
ALLOCATE(soil_state%minl_n_pot_gb          (land_pts,dim_cslayer,dim_cs1+1))
ALLOCATE(soil_state%immob_n_gb             (land_pts,dim_cslayer,dim_cs1+1))
ALLOCATE(soil_state%immob_n_pot_gb         (land_pts,dim_cslayer,dim_cs1+1))
ALLOCATE(soil_state%fn_gb                  (land_pts,dim_cslayer))
ALLOCATE(soil_state%resp_s_diag_gb         (land_pts,dim_cslayer,dim_cs1+1))
ALLOCATE(soil_state%resp_s_pot_diag_gb     (land_pts,dim_cslayer,dim_cs1+1))
ALLOCATE(soil_state%dpm_ratio_gb           (land_pts))
ALLOCATE(soil_state%n_gas_gb               (land_pts,dim_cslayer))
ALLOCATE(soil_state%resp_s_to_atmos_gb     (land_pts,dim_cslayer))

!Initialise
soil_state%cs_pool_soilt(:,:,:,:)           = 0.0
soil_state%frac_c_label_pool_soilt(:,:,:,:) = 0.0
soil_state%ns_pool_gb(:,:,:)                = 0.0
soil_state%n_inorg_soilt_lyrs(:,:,:)        = 0.0
soil_state%n_inorg_avail_pft(:,:,:)         = 0.0
soil_state%t_soil_soilt_acc(:,:,:)          = 0.0
soil_state%clay_soilt(:,:,:)                = 0.0
soil_state%sthu_soilt(:,:,:)                = 0.0
soil_state%resp_s_acc_soilt(:,:,:,:)        = 0.0
soil_state%resp_s_dr_out_gb(:,:,:)          = 0.0
soil_state%burnt_carbon_dpm(:)              = 0.0
soil_state%burnt_carbon_rpm(:)              = 0.0
soil_state%g_burn_gb(:)                     = 0.0
soil_state%minl_n_gb(:,:,:)                 = 0.0
soil_state%minl_n_pot_gb(:,:,:)             = 0.0
soil_state%immob_n_gb(:,:,:)                = 0.0
soil_state%immob_n_pot_gb(:,:,:)            = 0.0
soil_state%fn_gb(:,:)                       = 0.0
soil_state%resp_s_diag_gb(:,:,:)            = 0.0
soil_state%resp_s_pot_diag_gb(:,:,:)        = 0.0
soil_state%dpm_ratio_gb(:)                  = 0.0
soil_state%n_gas_gb(:,:)                    = 0.0
soil_state%resp_s_to_atmos_gb(:,:)          = 0.0

RETURN
END SUBROUTINE soil_bgc_4pool_allocate

!-------------------------------------------------------------------------------

SUBROUTINE soil_bgc_4pool_deallocate()

! Deallocates the soil_state POINTER components that were allocated locally
! by soil_bgc_4pool_allocate but which soil_bgc_4pool_assoc
! later re-associates onto external targets (progs/psparms/trifctl_data/
! trif_vars_data fields). This must be called before that re-association is
! done, otherwise the original local allocations become orphaned (leaked).

IMPLICIT NONE

!End of header

DEALLOCATE(soil_state%cs_pool_soilt)
DEALLOCATE(soil_state%frac_c_label_pool_soilt)
DEALLOCATE(soil_state%ns_pool_gb)
DEALLOCATE(soil_state%n_inorg_soilt_lyrs)
DEALLOCATE(soil_state%n_inorg_avail_pft)
DEALLOCATE(soil_state%t_soil_soilt_acc)
DEALLOCATE(soil_state%clay_soilt)
DEALLOCATE(soil_state%sthu_soilt)
DEALLOCATE(soil_state%resp_s_acc_soilt)
DEALLOCATE(soil_state%resp_s_dr_out_gb)
DEALLOCATE(soil_state%burnt_carbon_dpm)
DEALLOCATE(soil_state%burnt_carbon_rpm)
DEALLOCATE(soil_state%g_burn_gb)
DEALLOCATE(soil_state%minl_n_gb)
DEALLOCATE(soil_state%minl_n_pot_gb)
DEALLOCATE(soil_state%immob_n_gb)
DEALLOCATE(soil_state%immob_n_pot_gb)
DEALLOCATE(soil_state%fn_gb)
DEALLOCATE(soil_state%resp_s_diag_gb)
DEALLOCATE(soil_state%resp_s_pot_diag_gb)
DEALLOCATE(soil_state%dpm_ratio_gb)
DEALLOCATE(soil_state%n_gas_gb)
DEALLOCATE(soil_state%resp_s_to_atmos_gb)

RETURN
END SUBROUTINE soil_bgc_4pool_deallocate

!-------------------------------------------------------------------------------
SUBROUTINE soil_bgc_4pool_assoc(progs, psparms, trifctl_data,                  &
                                        trif_vars_data)

! Associates the soil_state fields to the rest of JULES.

! Note: trifctl_data and trif_vars_data are passed in as arguments (rather
! than USE-associated from jules_fields_mod) because this module lives in
! src/control/shared and must also build for the UM, where jules_fields_mod
! (a standalone-only module) is not available.

USE prognostics,   ONLY: progs_type
USE p_s_parms,     ONLY: psparms_type
USE trifctl,       ONLY: trifctl_data_type
USE trif_vars_mod, ONLY: trif_vars_data_type

IMPLICIT NONE

TYPE(progs_type), INTENT(IN) :: progs
TYPE(psparms_type), INTENT(IN) :: psparms
TYPE(trifctl_data_type), INTENT(IN), TARGET :: trifctl_data
TYPE(trif_vars_data_type), INTENT(IN), TARGET :: trif_vars_data
! End of header
!-------------------------------------------------------------------------------

! Deallocate the local pointer targets set up in
! soil_bgc_4pool_allocate before they are re-associated onto external
! targets below, to avoid leaking the original allocations (see
! soil_bgc_4pool_deallocate for details).
CALL soil_bgc_4pool_deallocate()

! Firstly progs
soil_state%cs_pool_soilt => progs%cs_pool_soilt
soil_state%frac_c_label_pool_soilt => progs%frac_c_label_pool_soilt
soil_state%ns_pool_gb => progs%ns_pool_gb
soil_state%n_inorg_soilt_lyrs => progs%n_inorg_soilt_lyrs
soil_state%n_inorg_avail_pft => progs%n_inorg_avail_pft
soil_state%t_soil_soilt_acc => progs%t_soil_soilt_acc

! Next psparms
soil_state%clay_soilt => psparms%clay_soilt
soil_state%sthu_soilt => psparms%sthu_soilt

! Next trifctl_data
soil_state%resp_s_acc_soilt => trifctl_data%resp_s_acc_soilt
soil_state%resp_s_dr_out_gb => trifctl_data%resp_s_dr_out_gb

! Finally trif_vars_data
soil_state%burnt_carbon_dpm => trif_vars_data%burnt_carbon_dpm
soil_state%burnt_carbon_rpm => trif_vars_data%burnt_carbon_rpm
soil_state%g_burn_gb => trif_vars_data%g_burn_gb
soil_state%minl_n_gb => trif_vars_data%minl_n_gb
soil_state%minl_n_pot_gb => trif_vars_data%minl_n_pot_gb
soil_state%immob_n_gb => trif_vars_data%immob_n_gb
soil_state%immob_n_pot_gb => trif_vars_data%immob_n_pot_gb
soil_state%fn_gb => trif_vars_data%fn_gb
soil_state%resp_s_diag_gb => trif_vars_data%resp_s_diag_gb
soil_state%resp_s_pot_diag_gb => trif_vars_data%resp_s_pot_diag_gb
soil_state%dpm_ratio_gb => trif_vars_data%dpm_ratio_gb
soil_state%n_gas_gb => trif_vars_data%n_gas_gb
soil_state%resp_s_to_atmos_gb => trif_vars_data%resp_s_to_atmos_gb

RETURN

END SUBROUTINE soil_bgc_4pool_assoc

END MODULE soil_bgc_4pool_field_mod
