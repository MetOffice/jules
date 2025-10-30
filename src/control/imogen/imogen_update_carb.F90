#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

SUBROUTINE imogen_update_carb(progs, imgn_vars)

USE model_grid_mod, ONLY: grid_area_ij
USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE model_time_mod, ONLY: current_time, main_run_end

USE ancil_info, ONLY: dim_cs1, dim_cslayer, land_pts, nsoilt

USE parallel_mod, ONLY: is_master_task, gather_land_field,                     &
      scatter_land_field

USE model_grid_mod, ONLY: global_land_pts

USE jules_hydrology_mod, ONLY: l_top

USE imogen_run, ONLY:                                                          &
  include_co2,c_emissions,co2_init_ppmv,land_feed_co2,                         &
  nyr_emiss, ocean_feed, initial_co2_ch4_year, land_feed_ch4,                  &
  change_metdata_method, l_change_metdata

USE aero, ONLY: co2_mmr

USE imogen_constants, ONLY: nfarray

USE conversions_mod, ONLY: conv_gtc_to_ppm, ocean_area, ppm_to_mmr

USE imogen_anlg_vals, ONLY:                                                    &
  t_ocean_init

USE imogen_io_vars, ONLY:                                                      &
  yr_emiss,c_emiss

USE logging_mod, ONLY: log_fatal

!TYPE definitions
USE jules_fields_mod, ONLY: toppdm, trifctltype
USE imgn_vars_mod, ONLY: imgn_vars_type
USE prognostics, ONLY: progs_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Updates the ocean and atmosphere carbon stores based on the accumulated
!   Carbon uptake of the land and prescribed emissions/concentrations.
!   Can include the natural methane feedback when land_feed_ch4 is TRUE
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

! Arguments

! TYPES containing the data
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(imgn_vars_type), INTENT(IN OUT) :: imgn_vars

INTEGER ::                                                                     &
  emiss_tally ! Checks that datafile of emissions includes
              ! year of interest

INTEGER :: l, n, nn, m, i, j ! loop counters

REAL ::                                                                        &
  d_land_atmos_field(land_pts),                                                &
              ! d_land_atmos scattered on to each processor
  darea(land_pts)
              ! area of each grid cell on each processor

REAL, ALLOCATABLE ::                                                           &
  global_data_dctot(:),                                                        &
              ! imgn_vars%dctot_co2 on full grid
  global_data_darea(:),                                                        &
              ! area of each grid cell on full grid
  global_data_land_atmos(:)
              ! d_land_atmos on full grid same value for each cell
!-----------------------------------------------------------------


!-----------------------------------------------------------------
! Calculate land CO2 atmosphere carbon exchange based in the difference
! between the current land carbon and the land carbon at the start of the
! year (this is rather confusingly stored as dctot_co2).
!-----------------------------------------------------------------
IF (land_feed_co2) THEN
  DO l = 1,land_pts
    imgn_vars%dctot_co2(l) = trifctltype%cv_gb(l) -                            &
                                           imgn_vars%dctot_co2(l)
  END DO

  DO l = 1,land_pts
    DO m = 1,nsoilt
      DO n = 1,dim_cs1
        DO nn = 1,dim_cslayer
          imgn_vars%dctot_co2(l) = imgn_vars%dctot_co2(l) +                    &
                                    progs%cs_pool_soilt(l,m,nn,n)
        END DO
      END DO
    END DO
  END DO

  !need to gather dctot_co2 and darea to global grid
  IF ( is_master_task() ) ALLOCATE(global_data_dctot(global_land_pts))
  IF ( is_master_task() ) ALLOCATE(global_data_darea(global_land_pts))
  IF ( is_master_task() ) ALLOCATE(global_data_land_atmos(global_land_pts))
  CALL gather_land_field(imgn_vars%dctot_co2, global_data_dctot)
  darea = RESHAPE(grid_area_ij(:,:), [ t_i_length * t_j_length ])
  CALL gather_land_field(darea, global_data_darea)

  IF ( is_master_task() ) THEN
    CALL diffcarb_land_co2(global_land_pts, imgn_vars%d_land_atmos_co2(1),     &
              global_data_darea, global_data_dctot)
    global_data_land_atmos(:) = imgn_vars%d_land_atmos_co2(1)
  END IF

  ! need to scatter imgn_vars%d_land_atmos_co2 to each processor
  CALL scatter_land_field(global_data_land_atmos, d_land_atmos_field)
  imgn_vars%d_land_atmos_co2 = d_land_atmos_field

  IF ( ALLOCATED(global_data_dctot) )      DEALLOCATE(global_data_dctot)
  IF ( ALLOCATED(global_data_darea) )      DEALLOCATE(global_data_darea)
  IF ( ALLOCATED(global_data_land_atmos) ) DEALLOCATE(global_data_land_atmos)

END IF  ! end co2 land feedbacks

!-----------------------------------------------------------------
! Now do the carbon cycling update.
!-----------------------------------------------------------------
IF (include_co2 .AND. c_emissions .AND. l_change_metdata                       &
                         .AND. change_metdata_method == 1 ) THEN  ! anlg_model

  ! Include anthropogenic carbon emissions (c_emissions=true)
  emiss_tally = 0
  DO n = 1,nyr_emiss
    IF (yr_emiss(n) == current_time%year) THEN
      imgn_vars%c_emiss_data(1) = c_emiss(n)
      imgn_vars%co2_ppmv(1) = imgn_vars%co2_ppmv(1) + conv_gtc_to_ppm *        &
                               imgn_vars%c_emiss_data(1)
      emiss_tally = emiss_tally + 1
      ! We have found the right year so we can exit the loop
      EXIT
    END IF
  END DO

  IF (emiss_tally /= 1)                                                        &
    CALL log_fatal("imogen_update_carb",                                       &
                   'IMOGEN: Emission dataset does not match run')

  IF (land_feed_co2) THEN
    ! Update with land feedbacks if required
    imgn_vars%co2_ppmv(1) = imgn_vars%co2_ppmv(1) + imgn_vars%d_land_atmos_co2(1)
  END IF

  IF (ocean_feed) THEN
    ! Update with ocean feedbacks if required
    CALL ocean_co2(                                                            &
      current_time%year - initial_co2_ch4_year+1,                              &
      1,imgn_vars%co2_ppmv(1),co2_init_ppmv,imgn_vars%dtemp_o(1),              &
      imgn_vars%fa_ocean,ocean_area,imgn_vars%co2_change_ppmv(1),              &
      main_run_end%year - initial_co2_ch4_year,                                &
      t_ocean_init,nfarray,imgn_vars%d_ocean_atmos(1)                          &
    )
    imgn_vars%co2_ppmv(1) = imgn_vars%co2_ppmv(1) + imgn_vars%d_ocean_atmos(1)
  END IF
END IF

IF (include_co2) THEN
  ! if co2 concentration is allowed to change
  imgn_vars%co2_change_ppmv(1) = imgn_vars%co2_ppmv(1) -                       &
                        imgn_vars%co2_start_ppmv(1)
END IF

!Unit conversion ppm to mmr (g/g): mol mass co2 / mol mass dry air * 1e-6
co2_mmr = imgn_vars%co2_ppmv(1) * 44.0 / 28.97 * 1.0e-6
!ejb deleted co2_mmr = imgn_vars%co2_ppmv(1) * ppm_to_mmr

!-----------------------------------------------------------------
! Calculate ch4 feedbacks
!-----------------------------------------------------------------
IF (land_feed_ch4) THEN
  ! This following is an attempt to future proof the format of the ch4
  ! emissions. At some point fire CH4 emissions could also be included however
  ! this will require a better treatment of the fch4_ref  and it may also be
  ! advisable to use a different logical than l_top to identify wetland
  ! emissions
  IF (l_top) THEN
    DO l = 1,land_pts
      ! Sum flux of accumulated ch4 over land in kg C / year
      imgn_vars%dctot_ch4(l) = SUM(toppdm%fch4_wetl_acc_soilt(l,:))
      toppdm%fch4_wetl_acc_soilt(l,:) = 0.0 ! reset
    END DO
  ELSE
    CALL log_fatal("imogen_update_carb",                                       &
                   "top model is switched off (l_top) " //                     &
                   " therefore no CH4 feedbacks are included" )
  END IF

  ! Gather dctot_ch4 and darea to global grid
  IF ( is_master_task() ) ALLOCATE(global_data_dctot(global_land_pts))
  IF ( is_master_task() ) ALLOCATE(global_data_darea(global_land_pts))
  IF ( is_master_task() ) ALLOCATE(global_data_land_atmos(global_land_pts))
  CALL gather_land_field(imgn_vars%dctot_ch4, global_data_dctot)
  darea = RESHAPE(grid_area_ij(:,:), [ t_i_length * t_j_length ])
  CALL gather_land_field(darea, global_data_darea)

  ! Calculate the additional global flux of ch4 from land to atmosphere
  ! compared to a reference flux which is applicable to the atmospheric
  ! baseline ch4 concentration.
  IF ( is_master_task() ) THEN
    CALL diffcarb_land_ch4(global_land_pts, imgn_vars%d_land_atmos_ch4(1),     &
                global_data_darea, global_data_dctot)
    global_data_land_atmos(:) = imgn_vars%d_land_atmos_ch4(1)
  END IF

  ! Need to scatter d_land_atmos_ch4 to each processor
  CALL scatter_land_field(global_data_land_atmos, d_land_atmos_field)
  imgn_vars%d_land_atmos_ch4(:) = d_land_atmos_field(1)

  ! Update atmospheric methane at end of year. This accounts for the change in
  ! methane global flux from natural source from a reference amount, and the
  ! change in decay rate due to a changed atmospheric concentration
  CALL diff_atmos_ch4(imgn_vars%d_land_atmos_ch4(1), imgn_vars%ch4_ppbv(1))

  IF ( ALLOCATED(global_data_dctot) )      DEALLOCATE(global_data_dctot)
  IF ( ALLOCATED(global_data_land_atmos) ) DEALLOCATE(global_data_land_atmos)
  IF ( ALLOCATED(global_data_darea) )      DEALLOCATE(global_data_darea)

END IF

RETURN

END SUBROUTINE imogen_update_carb
#endif
