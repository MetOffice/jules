#if !defined(RIVERS_ONLY)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE write_dump_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE write_dump()

USE jules_fields_mod, ONLY: crop_vars, psparms, ainfo, trif_vars, progs,       &
                            fluxes, lake_vars, rivers, water_resources

!Science variables
! TYPE Definitions
USE jules_fields_mod, ONLY: toppdm, soilecosse, trifctltype
USE imgn_vars_mod, ONLY: imgn_vars

USE model_grid_mod, ONLY:                                                      &
  global_land_pts, grid_area_ij, latitude, longitude, l_coord_latlon,          &
  projection_x_coord, projection_y_coord

USE ancil_info, ONLY:                                                          &
  dim_cs1, dim_soil_n_pool, nsurft, nsoilt, dim_cslayer, nmasst

USE jules_surface_types_mod, ONLY:                                             &
  npft, ntype, ncpft

USE jules_irrig_mod, ONLY: irrtiles, frac_irrig_all_tiles, nirrtile,           &
  set_irrfrac_on_irrtiles

USE jules_snow_mod, ONLY:                                                      &
  nsmax

USE jules_soil_mod, ONLY:                                                      &
  sm_levels, ns_deep

USE jules_soil_biogeochem_mod, ONLY:                                           &
  dim_ch4layer, dim_ch4subgrid

USE fire_mod, ONLY:                                                            &
  fire_prog

USE metstats_mod, ONLY:                                                        &
  metstats_prog

USE aero, ONLY:                                                                &
  co2_mmr

USE jules_rivers_mod, ONLY:                                                    &
  l_rivers

!Others
USE parallel_mod, ONLY:                                                        &
  is_master_task, gather_land_field

USE model_interface_mod, ONLY:                                                 &
  identifier_len

USE file_mod, ONLY:                                                            &
  file_handle, file_write_var, file_close

USE dump_mod,                            ONLY: max_var_dump
USE logging_mod,                         ONLY: log_fatal, log_info
USE required_vars_for_configuration_mod, ONLY: required_vars_for_configuration
USE create_dump_file_mod,                ONLY: create_dump_file
USE write_dump_var_rivers_mod,           ONLY: write_dump_var_rivers
USE add_dump_coords_for_rivers_mod,      ONLY: add_dump_coords_for_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Writes a dump file for the current timestep
!   Note that the writing of the dump is done by the master task with the
!   values gathered from other tasks. This means that dumps written with
!   different amounts of tasks should be interchangable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
CHARACTER(LEN=identifier_len) :: identifiers(max_var_dump)
                                    ! The model identifiers for the variables
                                    ! to put in the dump
CHARACTER(LEN=identifier_len) :: vars_from_ancil(max_var_dump)
                             ! The variable identifiers of the ancil
                             ! variables (not used in this subroutine)

TYPE(file_handle) :: FILE  ! The dump file

INTEGER :: nvars  ! The number of variables we are processing
INTEGER :: nvars_from_ancil

INTEGER :: var_ids(max_var_dump)
                      ! The ids of the variables in the dump file

INTEGER :: i, m, n, s  ! Loop counters

REAL :: frac_irrig_all_tiles_real
REAL :: set_irrfrac_irrtiles_real
REAL :: irrtiles_real(npft)
REAL :: nirrtile_real

! Arrays to hold global land points version of data gathered in master task
! before writing
REAL, ALLOCATABLE :: global_data_1d(:)  ! For data with no vertical levels
REAL, ALLOCATABLE :: global_data_2d(:,:)   ! With one vertical level
REAL, ALLOCATABLE :: global_data_3d(:,:,:) ! With two "vertical" levels
                                        ! I.E. snow variables or soil C/N
REAL, ALLOCATABLE :: global_data_4d(:,:,:,:)

LOGICAL, PARAMETER :: l_output_mode = .TRUE.

CHARACTER(LEN=*), PARAMETER :: RoutineName='WRITE_DUMP'

!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Get the list of identifiers that we are going to output
!-----------------------------------------------------------------------------
nvars            = 0
nvars_from_ancil = 0
CALL required_vars_for_configuration(nvars, identifiers,                       &
                                     nvars_from_ancil, vars_from_ancil,        &
                                     l_output_mode)

! Add latitude and longitude, and possibly projection coordinates, to the list
! to help offline inspection of the dump file, ie they are diagnostics, not
! prognostics or ancillaries. Projection coordinates are only included if
! they are not latitude and longitude - primarily to preserve existing results.
! Lat & lon (and projection coords) will not be read in from the dump to
! prevent confusion with the model grid namelists.

nvars = nvars + 1
identifiers(nvars) = 'latitude'
nvars = nvars + 1
identifiers(nvars) = 'longitude'
IF ( .NOT. l_coord_latlon ) THEN
  nvars = nvars + 1
  identifiers(nvars) = 'projection_x_coord'
  nvars = nvars + 1
  identifiers(nvars) = 'projection_y_coord'
END IF

! Similarly, add latitude and longitude of river points, if needed.
IF ( l_rivers ) THEN
  CALL add_dump_coords_for_rivers(nvars, identifiers)
END IF

CALL create_dump_file(nvars, identifiers, FILE, var_ids)

!-----------------------------------------------------------------------------
! Gather data from other tasks and write it to file
!-----------------------------------------------------------------------------
! Allocate the global data arrays
IF ( is_master_task() ) THEN
  ALLOCATE(global_data_1d(global_land_pts))
  ALLOCATE(global_data_2d(global_land_pts, MAX(npft, sm_levels,                &
                                               nsurft, ntype, ns_deep,         &
                                               nsoilt, dim_ch4layer)))
  ALLOCATE(global_data_3d(global_land_pts,                                     &
                    MAX(nsurft, dim_cslayer, nsoilt, npft, dim_ch4subgrid),    &
                    MAX(nsmax, dim_cs1, dim_soil_n_pool, sm_levels,            &
                              nmasst, dim_ch4layer)))
  ALLOCATE(global_data_4d(global_land_pts, nsoilt,                             &
                          MAX(nsurft, dim_cslayer),                            &
                          MAX(nsmax, dim_cs1, dim_soil_n_pool, sm_levels)))
ELSE
  ALLOCATE(global_data_1d(1))
  ALLOCATE(global_data_2d(1,1))
  ALLOCATE(global_data_3d(1,1,1))
END IF

DO i = 1,nvars
  ! Gather the variables into a global array to write to file
  ! Note that gather_land_field can only gather one land_pts array at a time,
  ! so to gather variables with multiple levels we must loop

  CALL log_info(RoutineName, identifiers(i))

  SELECT CASE ( identifiers(i) )
  CASE ( 'gs' )
    CALL gather_land_field(progs%gs_gb, global_data_1d)

  CASE ( 't_growth_gb' )
    CALL gather_land_field(progs%t_growth_gb, global_data_1d)

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'sthzw' )
    CALL gather_land_field(toppdm%sthzw_soilt(:,1), global_data_1d)

  CASE ( 'toppdm%sthzw_soilt' )
    DO m = 1,nsoilt
      CALL gather_land_field(toppdm%sthzw_soilt(:,m), global_data_2d(:,m))
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'zw' )
    CALL gather_land_field(toppdm%zw_soilt(:,1), global_data_1d)

  CASE ( 'toppdm%zw_soilt' )
    DO m = 1,nsoilt
      CALL gather_land_field(toppdm%zw_soilt(:,m), global_data_2d(:,m))
    END DO

  CASE ( 'cv' )
    CALL gather_land_field(trifctltype%cv_gb, global_data_1d)

  CASE ( 'frac_agr_prev' )
    CALL gather_land_field(progs%frac_agr_prev_gb, global_data_1d)

  CASE ( 'frac_past_prev' )
    CALL gather_land_field(progs%frac_past_prev_gb, global_data_1d)

  CASE ( 'frac_biocrop_prev' )
    CALL gather_land_field(progs%frac_biocrop_prev_gb, global_data_1d)

  CASE ( 'wood_prod_fast' )
    CALL gather_land_field(progs%wood_prod_fast_gb, global_data_1d)

  CASE ( 'wood_prod_med' )
    CALL gather_land_field(progs%wood_prod_med_gb, global_data_1d)

  CASE ( 'wood_prod_slow' )
    CALL gather_land_field(progs%wood_prod_slow_gb, global_data_1d)

  CASE ( 'lake_fetch_gb' )
    CALL gather_land_field(lake_vars%lake_fetch_gb, global_data_1d)

  CASE ( 'lake_t_mean_gb' )
    CALL gather_land_field(lake_vars%lake_t_mean_gb, global_data_1d)

  CASE ( 'lake_t_mxl_gb' )
    CALL gather_land_field(lake_vars%lake_t_mxl_gb, global_data_1d)

  CASE ( 'lake_h_mxl_gb' )
    CALL gather_land_field(lake_vars%lake_h_mxl_gb, global_data_1d)

  CASE ( 'lake_t_ice_gb' )
    CALL gather_land_field(lake_vars%lake_t_ice_gb, global_data_1d)

  CASE ( 'lake_h_ice_gb' )
    CALL gather_land_field(lake_vars%lake_h_ice_gb, global_data_1d)

  CASE ( 'lake_shape_factor_gb' )
    CALL gather_land_field(lake_vars%lake_shape_factor_gb, global_data_1d)

  CASE ( 'latitude' )
    CALL gather_land_field(latitude, global_data_1d)

  CASE ( 'longitude' )
    CALL gather_land_field(longitude, global_data_1d)

  CASE ( 'projection_x_coord' )
    CALL gather_land_field(projection_y_coord, global_data_1d)

  CASE ( 'projection_y_coord' )
    CALL gather_land_field(projection_x_coord, global_data_1d)

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'n_inorg' )
    DO n = 1,dim_cslayer
      CALL gather_land_field(progs%n_inorg_soilt_lyrs(:,1,n),                  &
                             global_data_2d(:,n))
    END DO

  CASE ( 'n_inorg_soilt' )
    DO m = 1,nsoilt
      DO n = 1,dim_cslayer
        CALL gather_land_field(progs%n_inorg_soilt_lyrs(:,m,n),                &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'substr_ch4' )
    DO m = 1,dim_ch4subgrid
      DO n = 1,dim_ch4layer
        CALL gather_land_field(progs%substr_ch4(:,m,n),                        &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'mic_ch4' )
    DO m = 1,dim_ch4subgrid
      DO n = 1,dim_ch4layer
        CALL gather_land_field(progs%mic_ch4(:,m,n),                           &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'timewet_lyr' )
    DO m = 1,dim_ch4subgrid
      DO n = 1,dim_ch4layer
        CALL gather_land_field(progs%timewet_lyr(:,m,n),                       &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'mic_act_ch4' )
    DO m = 1,dim_ch4subgrid
      DO n = 1,dim_ch4layer
        CALL gather_land_field(progs%mic_act_ch4(:,m,n),                       &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'acclim_ch4' )
    DO n = 1,dim_ch4layer
      CALL gather_land_field(progs%acclim_ch4(:,n),                            &
                             global_data_2d(:,n))
    END DO

  CASE ( 'canht' )
    DO n = 1,npft
      CALL gather_land_field(progs%canht_pft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'lai' )
    DO n = 1,npft
      CALL gather_land_field(progs%lai_pft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'years_since_harvest' )
    DO n = 1,npft
      CALL gather_land_field(REAL(progs%years_since_harvest(:,n)),             &
                             global_data_2d(:,n))
    END DO

  CASE ( 'harvest_doy' )
    DO n = 1,npft
      CALL gather_land_field(REAL(trif_vars%harvest_doy(:,n)),                 &
                             global_data_2d(:,n))
    END DO

  CASE ( 'f_nsc' )
    DO n = 1,npft
      CALL gather_land_field(progs%f_nsc_pft(:,n), global_data_2d(:,n))
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'cs' )
    DO n = 1,dim_cs1
      DO m = 1,dim_cslayer
        CALL gather_land_field(progs%cs_pool_soilt(:,1,m,n),                   &
                               global_data_3d(:,m,n))
      END DO
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'frac_c_label_pool' )
    DO n = 1,dim_cs1
      DO m = 1,dim_cslayer
        CALL gather_land_field(progs%frac_c_label_pool_soilt(:,1,m,n),         &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'plantNumDensity' )
    DO n = 1,npft
      DO m = 1,nmasst
        CALL gather_land_field(progs%plantNumDensity(:,n,m),                   &
                               global_data_3d(:,n,m))
      END DO
    END DO


  CASE ( 'cs_soilt' )
    DO s = 1,nsoilt
      DO n = 1,dim_cs1
        DO m = 1,dim_cslayer
          CALL gather_land_field(progs%cs_pool_soilt(:,s,m,n),                 &
                                 global_data_4d(:,s,m,n))
        END DO
      END DO
    END DO

  CASE ( 'frac_c_label_pool_soilt' )
    DO s = 1,nsoilt
      DO n = 1,dim_cs1
        DO m = 1,dim_cslayer
          CALL gather_land_field(progs%frac_c_label_pool_soilt(:,s,m,n),       &
                                 global_data_4d(:,s,m,n))
        END DO
      END DO
    END DO

  CASE ( 'cropdvi' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%dvi_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'croprootc' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%rootc_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'cropharvc' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%harvc_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'cropreservec' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%reservec_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'croplai' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%croplai_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'cropcanht' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%cropcanht_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'ns' )
    DO n = 1,dim_cs1
      DO m = 1,dim_cslayer
        CALL gather_land_field(progs%ns_pool_gb(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'sthuf' )
    ! sthuf is held in sthu until it is processed
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%sthu_soilt(:,1,n) +                       &
                             psparms%sthf_soilt(:,1,n),                        &
                             global_data_2d(:,n))
    END DO

  CASE ( 'sthuf_soilt' )
    ! sthuf is held in sthu until it is processed
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%sthu_soilt(:,m,n) +                     &
                               psparms%sthf_soilt(:,m,n),                      &
                               global_data_3d(:,m,n))
      END DO
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 't_soil' )
    DO n = 1,sm_levels
      CALL gather_land_field(progs%t_soil_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 't_soil_soilt' )
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(progs%t_soil_soilt(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'tsoil_deep' )
    DO n = 1,ns_deep
      CALL gather_land_field(progs%tsoil_deep_gb(:,n), global_data_2d(:,n))
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'sthu_irr' )
    DO n = 1,sm_levels
      CALL gather_land_field(crop_vars%sthu_irr_soilt(:,1,n),                  &
                             global_data_2d(:,n))
    END DO

  CASE ( 'sthu_irr_soilt' )
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(crop_vars%sthu_irr_soilt(:,m,n),                &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'canopy' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%canopy_surft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'nsnow' )
    DO n = 1,nsurft
      CALL gather_land_field(REAL(progs%nsnow_surft(:,n)), global_data_2d(:,n))
    END DO

  CASE ( 'rgrain' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%rgrain_surft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'rho_snow' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%rho_snow_grnd_surft(:,n),                   &
                             global_data_2d(:,n))
    END DO

  CASE ( 'snow_tile' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%snow_surft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'snow_depth' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%snowdepth_surft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'snow_grnd' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%snow_grnd_surft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'tstar_tile' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%tstar_surft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'tsurf_elev_surft' )
    DO n = 1,nsurft
      CALL gather_land_field(progs%tsurf_elev_surft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'rgrainl' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL gather_land_field(progs%rgrainl_surft(:,m,n),                     &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'snow_ds' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL gather_land_field(progs%ds_surft(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'snow_ice' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL gather_land_field(progs%sice_surft(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO


  CASE ( 'snow_liq' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL gather_land_field(progs%sliq_surft(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO


  CASE ( 'tsnow' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL gather_land_field(progs%tsnow_surft(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO

    ! Cases for metstats variables
  CASE ('temp_max_00h_r')
    CALL gather_land_field(metstats_prog(:)%temp_max_00h%run,                  &
                           global_data_1d)

  CASE ('temp_ave_00h_r')
    CALL gather_land_field(metstats_prog(:)%temp_ave_00h%run,                  &
                           global_data_1d)

  CASE ('prec_tot_00h_r')
    CALL gather_land_field(metstats_prog(:)%prec_tot_00h%run,                  &
                           global_data_1d)

  CASE ('prec_tot_12h_r')
    CALL gather_land_field(metstats_prog(:)%prec_tot_12h%run,                  &
                           global_data_1d)

  CASE ('rhum_min_00h_r')
    CALL gather_land_field(metstats_prog(:)%rhum_min_00h%run,                  &
                           global_data_1d)

  CASE ('dewp_ave_00h_r')
    CALL gather_land_field(metstats_prog(:)%dewp_ave_00h%run,                  &
                           global_data_1d)

  CASE ('wind_ave_00h_r')
    CALL gather_land_field(metstats_prog(:)%wind_ave_00h%run,                  &
                           global_data_1d)

  CASE ('temp_max_00h')
    CALL gather_land_field(metstats_prog(:)%temp_max_00h%fin,                  &
                           global_data_1d)

  CASE ('temp_ave_00h')
    CALL gather_land_field(metstats_prog(:)%temp_ave_00h%fin,                  &
                           global_data_1d)

  CASE ('temp_ave_nday')
    CALL gather_land_field(metstats_prog(:)%temp_ave_nday%fin,                 &
                           global_data_1d)

  CASE ('temp_pnt_12h')
    CALL gather_land_field(metstats_prog(:)%temp_pnt_12h%fin,                  &
                           global_data_1d)

  CASE ('prec_tot_00h')
    CALL gather_land_field(metstats_prog(:)%prec_tot_00h%fin,                  &
                           global_data_1d)

  CASE ('prec_tot_12h')
    CALL gather_land_field(metstats_prog(:)%prec_tot_12h%fin,                  &
                           global_data_1d)

  CASE ('rhum_min_00h')
    CALL gather_land_field(metstats_prog(:)%rhum_min_00h%fin,                  &
                           global_data_1d)

  CASE ('rhum_pnt_12h')
    CALL gather_land_field(metstats_prog(:)%rhum_pnt_12h%fin,                  &
                           global_data_1d)

  CASE ('dewp_ave_00h')
    CALL gather_land_field(metstats_prog(:)%dewp_ave_00h%fin,                  &
                           global_data_1d)

  CASE ('wind_ave_00h')
    CALL gather_land_field(metstats_prog(:)%wind_ave_00h%fin,                  &
                           global_data_1d)

  CASE ('wind_pnt_12h')
    CALL gather_land_field(metstats_prog(:)%wind_pnt_12h%fin,                  &
                           global_data_1d)

    ! Cases for Fire variables
  CASE ( 'fire_mcarthur_r_dr' )
    CALL gather_land_field(fire_prog(:)%mcarthur%r_dr, global_data_1d)

  CASE ( 'fire_mcarthur_n_dr' )
    CALL gather_land_field(fire_prog(:)%mcarthur%n_dr, global_data_1d)

  CASE ( 'fire_canadian_ffmc' )
    CALL gather_land_field(fire_prog(:)%canadian%ffmc, global_data_1d)

  CASE ( 'fire_canadian_ffmc_mois' )
    CALL gather_land_field(fire_prog(:)%canadian%ffmc_mois,                    &
                           global_data_1d)

  CASE ( 'fire_canadian_dmc' )
    CALL gather_land_field(fire_prog(:)%canadian%dmc, global_data_1d)

  CASE ( 'fire_canadian_dc' )
    CALL gather_land_field(fire_prog(:)%canadian%dc, global_data_1d)

  CASE ( 'fire_nesterov' )
    CALL gather_land_field(fire_prog(:)%nesterov%findex, global_data_1d)

    ! ECOSSE variables
    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'n_soil' )
    DO n = 1,dim_soil_n_pool
      DO m = 1,dim_cslayer
        CALL gather_land_field(soilecosse%n_soil_pool_soilt(:,1,m,n),          &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'n_soil_soilt' )
    DO s = 1,nsoilt
      DO n = 1,dim_soil_n_pool
        DO m = 1,dim_cslayer
          CALL gather_land_field(progs%cs_pool_soilt(:,s,m,n),                 &
                                 global_data_4d(:,s,m,n))
        END DO
      END DO
    END DO

    ! Since each task runs its own version of IMOGEN, we just use the values
    ! from the master task.
  CASE ( 'co2_ppmv', 'co2_change_ppmv', 'dtemp_o', 'fa_ocean',                 &
         'seed_rain', 'ch4_ppbv' )
    ! Nothing to do

  CASE ( 'rivers_lat_rp', 'rivers_lon_rp', 'rivers_sto_rp',                    &
         'rivers_x_coord_rp', 'rivers_y_coord_rp',                             &
         'rfm_surfstore_rp', 'rfm_substore_rp',                                &
         'rfm_flowin_rp', 'rfm_bflowin_rp', 'rivers_outflow_rp',               &
         'flood_flow', 'flood_flow_prev', 'flood_storage',                     &
         'flood_storage_prev', 'river_channel_flow', 'river_channel_storage',  &
         'river_depth_prev', 'river_flow_prev' )
    ! Rivers are run on a single task, hence nothing to do here.

    !-------------------------------------------------------------------------
    ! Ancillary variables
    !-------------------------------------------------------------------------

    ! latlon ancil namelist
  CASE ( 'grid_area' )
    CALL gather_land_field(grid_area_ij, global_data_1d)

    !Frac ancil namelist
  CASE ( 'frac' )
    DO n = 1,ntype
      CALL gather_land_field(ainfo%frac_surft(:,n), global_data_2d(:,n))
    END DO

    ! Vegetation properties ancil namelist
  CASE ( 't_home_gb' )
    CALL gather_land_field(progs%t_home_gb, global_data_1d)

    !Soil properties ancil namelist
    !Cases if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'b      ')
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%bexp_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 'b_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%bexp_soilt(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'sathh  ')
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%sathh_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 'sathh_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%sathh_soilt(:,m,n),                     &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'satcon ')
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%satcon_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 'satcon_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%satcon_soilt(:,m,n),                    &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'sm_sat ')
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%smvcst_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 'sm_sat_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%smvcst_soilt(:,m,n),                    &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'sm_crit')
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%smvccl_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 'sm_crit_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%smvccl_soilt(:,m,n),                    &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'sm_wilt')
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%smvcwt_soilt(:,1,n),                      &
                             global_data_2d(:,n))
    END DO

  CASE ( 'sm_wilt_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%smvcwt_soilt(:,m,n),                    &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'hcap   ')
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%hcap_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 'hcap_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%hcap_soilt(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'hcon   ' )
    DO n = 1,sm_levels
      CALL gather_land_field(psparms%hcon_soilt(:,1,n), global_data_2d(:,n))
    END DO

  CASE ( 'hcon_soilt')
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL gather_land_field(psparms%hcon_soilt(:,m,n), global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'albsoil' )
    CALL gather_land_field(psparms%albsoil_soilt(:,1), global_data_1d)

  CASE ( 'albsoil_soilt' )
    DO m = 1,nsoilt
      CALL gather_land_field(psparms%albsoil_soilt(:,m), global_data_2d(:,m))
    END DO

  CASE ( 'clay' )
    DO n = 1,dim_cslayer
      CALL gather_land_field(psparms%clay_soilt(:,1,n),global_data_2d(:,n))
    END DO

  CASE ( 'clay_soilt' )
    DO m = 1,nsoilt
      DO n = 1,dim_cslayer
        CALL gather_land_field(psparms%clay_soilt(:,m,n),                      &
                               global_data_3d(:,m,n))
      END DO
    END DO

  CASE ( 'soil_ph' )
    DO n = 1,dim_cslayer
      CALL gather_land_field(psparms%soil_ph_soilt(:,1,n),global_data_2d(:,n))
    END DO

  CASE ( 'soil_ph_soilt' )
    DO m = 1,nsoilt
      DO n = 1,dim_cslayer
        CALL gather_land_field(psparms%soil_ph_soilt(:,m,n),                   &
                               global_data_3d(:,m,n))
      END DO
    END DO

    !Topmodel ancillaries namelist
    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'fexp   ' )
    CALL gather_land_field(toppdm%fexp_soilt(:,1), global_data_1d)

  CASE ( 'toppdm%fexp_soilt' )
    DO m = 1,nsoilt
      CALL gather_land_field(toppdm%fexp_soilt(:,m), global_data_2d(:,m))
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'ti_mean' )
    CALL gather_land_field(toppdm%ti_mean_soilt(:,1), global_data_1d)

  CASE ( 'toppdm%ti_mean_soilt' )
    DO m = 1,nsoilt
      CALL gather_land_field(toppdm%ti_mean_soilt(:,m), global_data_2d(:,m))
    END DO

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'ti_sig ' )
    CALL gather_land_field(toppdm%ti_sig_soilt(:,1), global_data_1d)

  CASE ( 'toppdm%ti_sig_soilt' )
    DO m = 1,nsoilt
      CALL gather_land_field(toppdm%ti_sig_soilt(:,m), global_data_2d(:,m))
    END DO

    !Agric ancillaries namelist
  CASE ( 'frac_agr' )
    CALL gather_land_field(trifctltype%frac_agr_gb, global_data_1d)

  CASE ( 'frac_past' )
    CALL gather_land_field(trif_vars%frac_past_gb, global_data_1d)

  CASE ( 'frac_biocrop' )
    CALL gather_land_field(trif_vars%frac_biocrop_gb, global_data_1d)


    !Crop props ancillaries namelist
  CASE ( 'cropsowdate       ' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%sow_date_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'croplatestharvdate' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%latestharv_date_cpft(:,n),              &
                             global_data_2d(:,n))
    END DO

  CASE ( 'cropttveg         ' )
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%tt_veg_cpft(:,n), global_data_2d(:,n))
    END DO

  CASE ( 'cropttrep         ')
    DO n = 1,ncpft
      CALL gather_land_field(crop_vars%tt_rep_cpft(:,n), global_data_2d(:,n))
    END DO

    !Irrigation ancillaries namelist
  CASE ( 'frac_irrig' )
    CALL gather_land_field(crop_vars%frac_irr_all(:,1), global_data_1d)

  CASE ( 'irrfrac_irrtiles' )
    CALL gather_land_field(crop_vars%irrfrac_irrtiles(:,1), global_data_1d)

  CASE ( 'frac_irr_all_tiles' )
    ! Convert to a real
    IF ( frac_irrig_all_tiles ) THEN
      frac_irrig_all_tiles_real = 1.0
    ELSE
      frac_irrig_all_tiles_real = 0.0
    END IF

  CASE ( 'irrtiles' )
    irrtiles_real = REAL(irrtiles(1:npft))

  CASE ( 'nirrtile' )
    nirrtile_real = REAL(nirrtile)

  CASE ( 'set_irrfrac_on_irrtiles' )
    ! Convert to a real
    IF ( set_irrfrac_on_irrtiles ) THEN
      set_irrfrac_irrtiles_real = 1.0
    ELSE
      set_irrfrac_irrtiles_real = 0.0
    END IF

    ! Water resources properties ancil namelist
  CASE ( 'conv_loss_frac' )
    CALL gather_land_field(water_resources%conv_loss_frac, global_data_1d)

  CASE ( 'sfc_water_frac' )
    CALL gather_land_field(water_resources%sfc_water_frac, global_data_1d)

    !CO2 ancil namelist
  CASE ( 'co2_mmr' )
    !Nothing to do

    !FLake ancil namelist
  CASE ( 'lake_depth_gb' )
    CALL gather_land_field(lake_vars%lake_depth_gb, global_data_1d)

  CASE DEFAULT
    CALL log_fatal(RoutineName,                                                &
                   "No code to gather variable for dump - " //                 &
                   TRIM(identifiers(i)))
  END SELECT

  !---------------------------------------------------------------------------
  ! In the master task, write the global data to file
  !---------------------------------------------------------------------------
  IF ( is_master_task() ) THEN

    SELECT CASE ( identifiers(i) )
      ! If it is a land_pts array with no levels associated,
      ! write the global_data_1d array
    CASE ( 'gs', 't_growth_gb', 'sthzw', 'zw', 'cv',                           &
           'frac_agr_prev', 'frac_past_prev',                                  &
           'frac_biocrop_prev',                                                &
           'wood_prod_fast', 'wood_prod_med', 'wood_prod_slow',                &
           'temp_max_00h_r', 'temp_ave_00h_r', 'prec_tot_00h_r',               &
           'prec_tot_12h_r', 'rhum_min_00h_r', 'dewp_ave_00h_r',               &
           'wind_ave_00h_r', 'temp_max_00h',   'temp_ave_00h',                 &
           'temp_ave_nday',                                                    &
           'temp_pnt_12h',   'prec_tot_00h',   'prec_tot_12h',                 &
           'rhum_min_00h',   'rhum_pnt_12h',   'dewp_ave_00h',                 &
           'wind_ave_00h',   'wind_pnt_12h',                                   &
           'fire_mcarthur_r_dr', 'fire_mcarthur_n_dr',                         &
           'fire_canadian_ffmc', 'fire_canadian_ffmc_mois',                    &
           'fire_canadian_dmc',  'fire_canadian_dc',                           &
           'fire_nesterov', 'lake_fetch_gb', 'lake_t_mean_gb',                 &
           'lake_t_mxl_gb', 'lake_h_mxl_gb', 'lake_t_ice_gb',                  &
           'lake_h_ice_gb', 'lake_shape_factor_gb', 'latitude', 'longitude',   &
           'projection_x_coord', 'projection_y_coord' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'toppdm%sthzw_soilt', 'toppdm%zw_soilt' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:nsoilt))

      ! If it is a variable with one levels dimension, write the appropriate
      ! number of levels to global_data_2d.
    CASE ( 'canht', 'lai', 'years_since_harvest', 'harvest_doy', 'f_nsc')
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:npft))

    CASE ( 'cropdvi', 'croprootc', 'cropharvc', 'cropreservec',                &
           'croplai', 'cropcanht' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:ncpft))

    CASE ( 'sthuf', 't_soil', 'sthu_irr' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_2d(:,1:sm_levels))

    CASE ( 'sthuf_soilt', 't_soil_soilt', 'sthu_irr_soilt' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:nsoilt,1:sm_levels))

    CASE ( 'n_inorg')
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_2d(:,1:dim_cslayer))

    CASE ( 'n_inorg_soilt')
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:nsoilt,1:dim_cslayer))

    CASE ( 'acclim_ch4' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_2d(:,1:dim_ch4layer))

    CASE ( 'substr_ch4','mic_ch4','mic_act_ch4','timewet_lyr' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:dim_ch4subgrid,1:dim_ch4layer))

    CASE ( 'tsoil_deep' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:ns_deep))

    CASE ( 'canopy', 'nsnow', 'rgrain', 'rho_snow', 'snow_tile',               &
           'snow_depth', 'snow_grnd', 'tstar_tile', 'tsurf_elev_surft' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:nsurft))

      ! Snow and soil C/N variables with 2 levels dimensions.
    CASE ( 'rgrainl', 'snow_ds', 'snow_ice', 'snow_liq', 'tsnow' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:nsurft,1:nsmax))
    CASE ( 'cs','ns', 'frac_c_label_pool' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:dim_cslayer,1:dim_cs1))

    CASE ( 'cs_soilt', 'frac_c_label_pool_soilt' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_4d(:,1:nsoilt,1:dim_cslayer,             &
                                         1:dim_cs1))
    CASE ( 'n_soil' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:dim_cslayer,1:dim_soil_n_pool))

    CASE ( 'n_soil_soilt' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_4d(:,1:nsoilt,1:dim_cslayer,             &
                                         1:dim_soil_n_pool))

    CASE ( 'plantNumDensity' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:npft,1:nmasst))

      ! Cases for IMOGEN variables
      ! Each task runs its own version of IMOGEN - we just write the master
      ! task's versions.
    CASE ( 'co2_ppmv' )
      CALL file_write_var(FILE, var_ids(i), imgn_vars%co2_ppmv)

    CASE ( 'co2_change_ppmv' )
      CALL file_write_var(FILE, var_ids(i), imgn_vars%co2_change_ppmv)

    CASE ( 'dtemp_o' )
      CALL file_write_var(FILE, var_ids(i), imgn_vars%dtemp_o)

    CASE ( 'fa_ocean' )
      CALL file_write_var(FILE, var_ids(i), imgn_vars%fa_ocean)

    CASE ( 'seed_rain' )
      CALL file_write_var(FILE, var_ids(i), REAL(progs%seed_rain))

    CASE ( 'ch4_ppbv' )
      CALL file_write_var(FILE, var_ids(i), imgn_vars%ch4_ppbv)

      !-----------------------------------------------------------------------
      ! Ancillary variables
      !-----------------------------------------------------------------------

      ! latlon ancil namelist
    CASE ( 'grid_area' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

      !Frac ancil namelist
    CASE ( 'frac' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:ntype))

      ! Vegetation properties ancil namelist
    CASE ( 't_home_gb' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

      !Soil properties ancil namelist
    CASE ( 'b      ', 'sathh  ', 'satcon ', 'sm_sat ', 'sm_crit',              &
           'sm_wilt', 'hcap   ', 'hcon   ' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:sm_levels))

    CASE ( 'b_soilt', 'sathh_soilt', 'satcon_soilt', 'sm_sat_soilt',           &
           'sm_crit_soilt', 'sm_wilt_soilt', 'hcap_soilt', 'hcon_soilt' )
      CALL file_write_var(FILE, var_ids(i),                                    &
                          global_data_3d(:,1:nsoilt,1:sm_levels))

    CASE ( 'albsoil' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'albsoil_soilt' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:nsoilt))

    CASE ( 'clay', 'soil_ph' )
      CALL file_write_var(FILE, var_ids(i),                                    &
           global_data_2d(:,1:dim_cslayer))

    CASE ( 'clay_soilt', 'soil_ph_soilt' )
      CALL file_write_var(FILE, var_ids(i),                                    &
           global_data_3d(:,1:nsoilt,1:dim_cslayer))

      !Topmodel ancil namelist
    CASE ( 'fexp   ', 'ti_mean', 'ti_sig ' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'toppdm%fexp_soilt', 'toppdm%ti_mean_soilt', 'toppdm%ti_sig_soilt' )
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:nsoilt))

      !Agric ancil namelist
    CASE ( 'frac_agr' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'frac_past' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'frac_biocrop' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

      !Crop props ancillaries namelist
    CASE ( 'cropsowdate', 'cropttveg  ', 'cropttrep  ','croplatestharvdate')
      CALL file_write_var(FILE, var_ids(i), global_data_2d(:,1:ncpft))

      !Irrigation ancillaries namelist
    CASE ( 'frac_irrig' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'irrfrac_irrtiles' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'frac_irr_all_tiles' )
      CALL file_write_var(FILE, var_ids(i), frac_irrig_all_tiles_real)

    CASE ( 'set_irrfrac_on_irrtiles' )
      CALL file_write_var(FILE, var_ids(i), set_irrfrac_irrtiles_real)

    CASE ( 'irrtiles' )
      CALL file_write_var(FILE, var_ids(i), irrtiles_real)

    CASE ( 'nirrtile' )
      CALL file_write_var(FILE, var_ids(i), nirrtile_real)

      ! Water resources properties ancil namelist
    CASE ( 'conv_loss_frac' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE ( 'sfc_water_frac' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

      !CO2 ancil namelist
    CASE ( 'co2_mmr' )
      CALL file_write_var(FILE, var_ids(i), co2_mmr)

    CASE ( 'lake_depth_gb' )
      CALL file_write_var(FILE, var_ids(i), global_data_1d)

    CASE DEFAULT
      ! Cases for river routing variables plus log fatal for unknown identifier
      CALL write_dump_var_rivers( identifiers(i), FILE, var_ids(i) )

    END SELECT
  END IF  ! MASTER TASK
END DO

! We are done with the file and dictionaries
IF ( is_master_task() ) CALL file_close(FILE)

IF ( ALLOCATED(global_data_1d) ) DEALLOCATE(global_data_1d)
IF ( ALLOCATED(global_data_2d) ) DEALLOCATE(global_data_2d)
IF ( ALLOCATED(global_data_3d) ) DEALLOCATE(global_data_3d)
IF ( ALLOCATED(global_data_4d) ) DEALLOCATE(global_data_4d)

RETURN

END SUBROUTINE write_dump
END MODULE write_dump_mod
#endif
