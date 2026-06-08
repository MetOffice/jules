MODULE control_mod
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

PUBLIC :: control

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CONTROL_MOD'

CONTAINS

SUBROUTINE control (                                                           &
    !TYPES containing field data (IN OUT)
    psparms, ainfo, progs, fluxes, river, wtrac_jls )


!-------------------------------------------------------------------------------
! Control level routine, to call the main parts of the model.
!-------------------------------------------------------------------------------

!TYPE definitions
USE p_s_parms,            ONLY: psparms_type
USE ancil_info,           ONLY: ainfo_type
USE prognostics,          ONLY: progs_type
USE fluxes_mod,           ONLY: fluxes_type
USE jules_wtrac_type_mod, ONLY: jls_wtrac_type

!Import the main subroutines we will use
USE surf_couple_rivers_mod,    ONLY: surf_couple_rivers

!Variables- modules in alphabetical order

USE ancil_info,               ONLY:                                            &
  land_pts, nsurft, row_length, rows

USE atm_fields_bounds_mod,    ONLY: tdims_s

USE coastal,                  ONLY: flandg

USE jules_rivers_mod,         ONLY: rivers_type

USE jules_model_environment_mod, ONLY: lsm_id, rivers

USE jules_print_mgr,             ONLY: jules_message, jules_print

USE jules_water_tracers_mod,     ONLY: n_wtrac_jls

!-------------------------------------------------------------------------------

IMPLICIT NONE

!-------------------------------------------------------------------------------
! Arguments with intent(in out)
!-------------------------------------------------------------------------------
TYPE(psparms_type),   INTENT(IN OUT) :: psparms
TYPE(ainfo_type),     INTENT(IN OUT) :: ainfo
TYPE(progs_type),     INTENT(IN OUT) :: progs
TYPE(fluxes_type),    INTENT(IN OUT) :: fluxes
TYPE(rivers_type),    INTENT(IN OUT) :: river
TYPE(jls_wtrac_type), INTENT(IN OUT) :: wtrac_jls

!-------------------------------------------------------------------------------
!Parameters
!-------------------------------------------------------------------------------

INTEGER, PARAMETER ::                                                          &
  n_proc = 2,                                                                  &
  river_row_length = 1,                                                        &
  river_rows = 1,                                                              &
  aocpl_row_length = 1,                                                        &
  aocpl_p_rows = 1

!-------------------------------------------------------------------------------
! Local variables
! In the vast majority of cases, these are arguments only required when
! coupled to the UM.
!-------------------------------------------------------------------------------

INTEGER ::                                                                     &
  a_steps_since_riv = 0,                                                       &
  g_p_field,                                                                   &
  g_r_field,                                                                   &
  global_row_length,                                                           &
  global_rows,                                                                 &
  global_river_row_length,                                                     &
  global_river_rows

REAL ::                                                                        &
  inlandout_atm_gb(land_pts),                                                  &
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
  trivdir(river_row_length, river_rows),                                       &
  trivseq(river_row_length, river_rows),                                       &
  r_area(row_length, rows),                                                    &
  slope(row_length, rows),                                                     &
  flowobs1(row_length, rows),                                                  &
  r_inext(row_length, rows),                                                   &
  r_jnext(row_length, rows),                                                   &
  r_land(row_length, rows),                                                    &
  substore(row_length, rows),                                                  &
  surfstore(row_length, rows),                                                 &
  flowin(row_length, rows),                                                    &
  bflowin(row_length, rows),                                                   &
  twatstor(river_row_length, river_rows),                                      &
!cxyz Sizes not done yet.
  abstracted_minor_res_global(land_pts),                                       &
    ! Water abstracted from minor reservoirs (kg).
  net_abstracted_river_global(land_pts),                                       &
  ! Net abstraction from rivers (kg m-2)
  tot_abstracted_minor_res_global(land_pts),                                   &
    ! Water abstracted from minor reservoirs over river timestep (kg).
  tot_net_abstracted_river_global(land_pts)
    ! Water abstracted from rivers over river timestep (kg m-2).

REAL(KIND=real_jlslsm) ::                                                      &
  !Passed between river routing and UM diagnostics_riv only
  riverout(row_length, rows),                                                  &
  riverout_rgrid(river_row_length, river_rows),                                &
  box_outflow(river_row_length, river_rows),                                   &
  box_inflow(river_row_length, river_rows),                                    &
  inlandout_riv(river_row_length,river_rows)

CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'CONTROL'

!------------------------------------------------------------------------------
!End of header

! Abstractions of water are zero in rivers-only configuration.
abstracted_minor_res_global(:)     = 0.0
net_abstracted_river_global(:)     = 0.0
tot_abstracted_minor_res_global(:) = 0.0
tot_net_abstracted_river_global(:) = 0.0

SELECT CASE ( lsm_id )
CASE ( rivers )
  CALL surf_couple_rivers(                                                     &
     !INTEGER, INTENT(IN)
     land_pts, n_wtrac_jls,                                                    &
     !REAL, INTENT(IN)
     abstracted_minor_res_global, net_abstracted_river_global,                 &
     fluxes%sub_surf_roff_gb, fluxes%surf_roff_gb,                             &
     wtrac_jls%sub_surf_roff_gb,  wtrac_jls%surf_roff_gb,                      &
     !INTEGER, INTENT(INOUT)
     a_steps_since_riv,                                                        &
     !REAL, INTENT (INOUT)
     river%tot_surf_runoff_gb, river%tot_sub_runoff_gb,                        &
     tot_abstracted_minor_res_global,                                          &
     tot_net_abstracted_river_global, river%acc_lake_evap_gb,                  &
     wtrac_jls%tot_surf_runoff_gb, wtrac_jls%tot_sub_runoff_gb,                &
     wtrac_jls%acc_lake_evap_gb,                                               &
     !REAL, INTENT (OUT)
     river%rivers_sto_per_m2_on_landpts, fluxes%rflow_gb, fluxes%rrun_gb,      &
     !Arguments for the UM-----------------------------------------
     !INTEGER, INTENT(IN)
     n_proc, row_length, rows, river_row_length, river_rows,                   &
     ainfo%land_index, aocpl_row_length, aocpl_p_rows, g_p_field,              &
     g_r_field, global_row_length, global_rows, global_river_row_length,       &
     global_river_rows,                                                        &
     !REAL, INTENT(IN)
     fluxes%lake_evap, delta_lambda, delta_phi, xx_cos_theta_latitude,         &
     xpa, xua, xva, ypa, yua, yva, flandg, trivdir,                            &
     trivseq, r_area, slope, flowobs1, r_inext, r_jnext, r_land,               &
     psparms%smvcst_soilt, psparms%smvcwt_soilt, ainfo%frac_surft,             &
     wtrac_jls%lake_evap,                                                      &
     !REAL, INTENT(INOUT)
     substore, surfstore, flowin, bflowin, twatstor,                           &
     progs%smcl_soilt, psparms%sthu_soilt,                                     &
     wtrac_jls%twatstor, wtrac_jls%smcl_soilt, wtrac_jls%sthu_soilt,           &
     !REAL, INTENT(OUT)
     inlandout_atm_gb, inlandout_riv, riverout, box_outflow, box_inflow,       &
     riverout_rgrid, wtrac_jls%inlandout_atm_gb,                               &
     ! TYPES
     river)
CASE DEFAULT
  WRITE(jules_message,'(A,I0)')                                                &
     "Land surface mode not allowed in Standalone Rivers",                     &
     lsm_id
  CALL jules_print(RoutineName, jules_message)
END SELECT

END SUBROUTINE control

END MODULE control_mod
