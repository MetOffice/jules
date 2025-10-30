#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
MODULE update_derived_variables_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE update_derived_variables(crop_vars,psparms,ainfo,urban_param,progs, &
                                    jules_vars,                                &
                                  !fluxes, &
                                  !lake, &
                                  forcing,                                     &
                                  imgn_drive                                   &
                                  !rivers, &
                                  !veg3_parm, &
                                  !veg3_field, &
                                  !chemvars, &
                                  )

!Use in relevant subroutines
USE sparm_mod,                   ONLY: sparm
USE assign_irrig_fraction_mod,   ONLY: assign_irrig_fraction
USE update_irrig_variables_mod,  ONLY: update_irrig_variables
USE update_precip_variables_mod, ONLY: update_precip_variables
USE impose_diurnal_cycle_mod,    ONLY: impose_diurnal_cycle
USE fill_disaggregated_precip_arrays_mod, ONLY: fill_disaggregated_precip_arrays

!Use in relevant variables
USE datetime_mod,         ONLY: secs_in_day
USE model_time_mod,       ONLY: current_time, timestep_len
USE jules_forcing_mod,    ONLY: u_1_ij, v_1_ij
USE jules_vegetation_mod, ONLY: l_croprotate, frac_min
USE jules_irrig_mod,      ONLY: l_irrig_dmd
USE ancil_info,           ONLY: land_pts, nsurft, nsoilt, soil_pts
USE theta_field_sizes,    ONLY: t_i_length
USE update_mod,           ONLY: l_imogen, l_daily_disagg, have_prescribed_veg, &
                                have_prescribed_sthuf,                         &
                                prescribed_sthuf_levels, io_wind_speed,        &
                                l_perturb_driving,                             &
                                temperature_abs_perturbation,                  &
                                precip_rel_perturbation

USE string_utils_mod, ONLY: to_string
USE jules_surface_types_mod, ONLY: ncpft, nnpft
USE jules_soil_mod,       ONLY: dzsoil, sm_levels

USE water_constants_mod,  ONLY: rho_water

USE freeze_soil_mod,      ONLY: freeze_soil

USE logging_mod, ONLY: log_warn, log_fatal

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type
USE p_s_parms, ONLY: psparms_type
USE ancil_info,    ONLY: ainfo_type
USE urban_param_mod, ONLY: urban_param_type
USE prognostics, ONLY: progs_type
USE jules_vars_mod, ONLY: jules_vars_type
! USE fluxes, ONLY: fluxes_type
! USE lake_mod, ONLY: lake_type
USE jules_forcing_mod, ONLY: forcing_type
USE imgn_drive_mod, ONLY: imgn_drive_type
! USE jules_rivers_mod, ONLY: rivers_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
! USE jules_chemvars_mod, ONLY: chemvars_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Updates variables that are derived from those given in time-varying files
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
!Arguments
!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
!TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
!TYPE(lake_type), INTENT(IN OUT) :: lake
TYPE(forcing_type), INTENT(IN OUT) :: forcing
!TYPE(rivers_type), INTENT(IN OUT) :: rivers
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field
!TYPE(chemvars_type), INTENT(IN OUT) :: chemvars

! Work variables
INTEGER :: insd  ! Timestep in day - used to index IMOGEN arrays

INTEGER :: i,j,l,m,n  ! Index variables
LOGICAL :: reset_done  ! Indicates if a reset of frac to frac_min was
                         ! performed
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------


IF ( l_imogen ) THEN
  !-------------------------------------------------------------------------------
  ! If IMOGEN is enabled, copy the correct timestep of the current climatology
  ! into the driving variables
  !-------------------------------------------------------------------------------
  !Get the timestep in the day that we are on
  insd = (current_time%TIME / timestep_len) + 1

  DO l = 1,land_pts
    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length

    forcing%pstar_ij(i,j) =                                                    &
       imgn_drive%pstar_ij_drive(i,j,current_time%month,current_time%day,insd)
    u_1_ij(i,j) =                                                              &
       imgn_drive%wind_ij_drive(i,j,current_time%month,current_time%day,insd)
    v_1_ij(i,j) = 0.0
    forcing%u_0_ij(i,j) = 0.0
    forcing%v_0_ij(i,j) = 0.0
    forcing%con_rain_ij(i,j) =                                                 &
       imgn_drive%conv_rain_ij_drive                                           &
                                (i,j,current_time%month,current_time%day,insd)
    forcing%con_snow_ij(i,j) =                                                 &
       imgn_drive%conv_snow_ij_drive                                           &
                                (i,j,current_time%month,current_time%day,insd)
    forcing%ls_rain_ij(i,j) =                                                  &
       imgn_drive%ls_rain_ij_drive                                             &
                                (i,j,current_time%month,current_time%day,insd)
    forcing%ls_snow_ij(i,j) =                                                  &
       imgn_drive%ls_snow_ij_drive                                             &
                                (i,j,current_time%month,current_time%day,insd)
    forcing%sw_down_ij(i,j) =                                                  &
       imgn_drive%swdown_ij_drive(i,j,current_time%month,current_time%day,insd)
    forcing%lw_down_ij(i,j) =                                                  &
       imgn_drive%lwdown_ij_drive(i,j,current_time%month,current_time%day,insd)
    forcing%qw_1_ij(i,j) =                                                     &
       imgn_drive%ql1_ij_drive(i,j,current_time%month,current_time%day,insd)
    forcing%tl_1_ij(i,j) =                                                     &
       imgn_drive%tl1_ij_drive(i,j,current_time%month,current_time%day,insd)
  END DO

ELSE
  !-----------------------------------------------------------------------------
  ! Otherwise, update the main driving variables based on what was given in the
  ! input file(s) - the update of those variables has already taken place
  !-----------------------------------------------------------------------------
  ! Initialise wind based on io_wind_speed
  ! This is just a case of initialising variables that are not set via file
  ! u_1 is always set from file, v_1 is only set from file if using both
  ! wind components
  IF ( io_wind_speed ) THEN
    v_1_ij(:,:) = 0.0
  END IF
  forcing%u_0_ij(:,:) = 0.0
  forcing%v_0_ij(:,:) = 0.0

  !------------------------------------------------------------------------------
  ! Apply any perturbations
  IF ( l_perturb_driving ) THEN
    forcing%tl_1_ij(:,:) = forcing%tl_1_ij(:,:) + temperature_abs_perturbation
    forcing%con_rain_ij(:,:) = precip_rel_perturbation * forcing%con_rain_ij(:,:)
    forcing%con_snow_ij(:,:) = precip_rel_perturbation * forcing%con_snow_ij(:,:)
    forcing%ls_rain_ij(:,:)  = precip_rel_perturbation * forcing%ls_rain_ij(:,:)
    forcing%ls_snow_ij(:,:)  = precip_rel_perturbation * forcing%ls_snow_ij(:,:)
  END IF

  IF (l_daily_disagg) THEN
    !-----------------------------------------------------------------------------
    ! If we are using the disaggregator, we need to disaggregate the precip at
    ! the start of every day, then impose a diurnal cycle
    !-----------------------------------------------------------------------------
    IF ( current_time%TIME == 0 ) THEN
      CALL update_precip_variables(forcing)
      CALL fill_disaggregated_precip_arrays(progs, forcing)
    END IF

    CALL impose_diurnal_cycle(forcing)
  ELSE
    !-----------------------------------------------------------------------------
    ! Otherwise, just update the precip variables based on the io_precip_type
    !-----------------------------------------------------------------------------
    CALL update_precip_variables(forcing)
  END IF

  !-----------------------------------------------------------------------------
  ! Radiation variables are updated in CONTROL, since the new albedos need to be
  ! known before the update takes place
  !-----------------------------------------------------------------------------
END IF

!-------------------------------------------------------------------------------
! Copy information to U, V and T grids (assume that att grids are the same)
!-------------------------------------------------------------------------------
jules_vars%u_0_p_ij(:,:) = forcing%u_0_ij(:,:)
jules_vars%v_0_p_ij(:,:) = forcing%v_0_ij(:,:)
jules_vars%u_1_p_ij(:,:) = u_1_ij(:,:)
jules_vars%v_1_p_ij(:,:) = v_1_ij(:,:)


!-----------------------------------------------------------------------------
! Update any variables dependent on variables being prescribed that are not
! driving variables
!-----------------------------------------------------------------------------
IF ( have_prescribed_veg ) THEN
  WHERE ( progs%lai_pft(:,:) <= 0.01 )
    progs%lai_pft = 0.01
  END WHERE

  CALL sparm(land_pts, nsurft, ainfo%surft_pts,                                &
             ainfo%surft_index, ainfo%frac_surft,                              &
             progs%canht_pft, progs%lai_pft, psparms%z0m_soil_gb,              &
             psparms%catch_snow_surft, psparms%catch_surft, psparms%z0_surft,  &
             psparms%z0h_bare_surft, urban_param%ztm_gb)

      !infiltration_rate does not need to be called because frac has not been
      !changed. This should be reviewed if a new parametrisation is added.

END IF

IF ( have_prescribed_sthuf ) THEN

  !-----------------------------------------------------------------------------
  ! Calculate soil moisture content from wetness
  !-----------------------------------------------------------------------------
  DO i = 1,sm_levels
    IF ( ANY(prescribed_sthuf_levels == i) ) THEN
      ! Use the sthuf_soilt that was just read in from the prescribed data file
      progs%smcl_soilt(:,:,i) = rho_water * dzsoil(i) *                        &
                          jules_vars%sthuf_soilt(:,:,i) *                      &
                          psparms%smvcst_soilt(:,:,i)
    ELSE
      ! Use the sthu_soilt and sthu_soilt values calculated on the previous
      ! timestep
      progs%smcl_soilt(:,:,i) = rho_water * dzsoil(i) *                        &
                    (psparms%sthu_soilt(:,:,i) + psparms%sthf_soilt(:,:,i)) *  &
                    psparms%smvcst_soilt(:,:,i)
    END IF
  END DO

  !-----------------------------------------------------------------------------
  ! Calculate frozen and unfrozen fractions of soil moisture.
  ! freeze_soil assumes bexp_soilt, sathh_soilt, smvcst_soilt are
  ! constant in soil column, so loop around soil layers to allow
  ! depth varying soil properties and to maintain compatibility with the UM
  !-----------------------------------------------------------------------------
  DO i = 1,sm_levels
    DO m = 1,nsoilt
      CALL freeze_soil (land_pts,1,psparms%bexp_soilt(:,m,i),dzsoil(i:i),      &
                        psparms%sathh_soilt(:,m,i),progs%smcl_soilt(:,m,i),    &
                        progs%t_soil_soilt(:,m,i),psparms%smvcst_soilt(:,m,i), &
                        psparms%sthu_soilt(:,m,i),psparms%sthf_soilt(:,m,i))
    END DO
  END DO
END IF
!-----------------------------------------------------------------------------
!   If using l_croprotate ensure that fractions of
!   crop PFTs are not below minimum. Only do this over land points.
!   This ensures that each crop fraction is initialised properly so it can be
!   used later. If a fraction is zero at the start it cannot be used later.
!-----------------------------------------------------------------------------

IF (l_croprotate) THEN
  ! Set up a flag to see if any points were reset to frac_min
  reset_done = .FALSE.

  DO j = 1,soil_pts
    i = ainfo%soil_index(j)

    IF ( ANY( ainfo%frac_surft(i,nnpft+1:nnpft + ncpft) < frac_min ) ) THEN
      ! Reset all small values. Renormalisation is done later,
      ! but will fail if frac_min is sufficiently large.
      ! We only reset crop PFT tiles
      DO n = 1,ncpft
        WHERE ( ainfo%frac_surft(:,n + nnpft) < frac_min )
          ainfo%frac_surft(:,n + nnpft) = frac_min
        END WHERE
      END DO
      reset_done = .TRUE.
    END IF
  END DO

  IF ( reset_done )                                                            &
    CALL log_warn("update_derived_variables",                                  &
                  "frac < frac_min at one or more points - reset to " //       &
                  "frac_min at those points for crop tiles")

  !-----------------------------------------------------------------------------
  ! Check that ainfo%frac_surft sums to 1.0 (with a bit of leeway).
  !-----------------------------------------------------------------------------
  DO i = 1,land_pts
    IF ( ABS( SUM(ainfo%frac_surft(i,:)) - 1.0 ) >= 1.0e-2 )                   &
      ! If the discrepancy is big enough, bail
      CALL log_fatal("update_derived_variables",                               &
                     "frac does not sum to 1 at point " //                     &
                     TRIM(to_string(i)))
      ! Ignore any discrepancy below the threshold completely
  END DO
END IF
!-----------------------------------------------------------------------------
! Update irrigation fractions
!-----------------------------------------------------------------------------
IF ( l_irrig_dmd ) THEN
  CALL assign_irrig_fraction(crop_vars,ainfo)
  CALL update_irrig_variables(crop_vars,psparms,ainfo)
END IF

RETURN

END SUBROUTINE update_derived_variables
END MODULE update_derived_variables_mod
#endif
