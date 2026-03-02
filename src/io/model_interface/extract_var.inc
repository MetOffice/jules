#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

FUNCTION extract_var(var_id) RESULT(cube)

USE jules_fields_mod, ONLY: crop_vars, psparms, toppdm, fire_vars, ainfo,      &
                            trif_vars, soilecosse, urban_param, progs,         &
                            trifctltype, jules_vars, fluxes, forcing,          &
                            lake_vars, chemvars, rivers, water_resources

  !Use subroutines
! TYPE Definitions
USE imgn_drive_mod, ONLY: imgn_drive
USE imgn_vars_mod, ONLY: imgn_vars

USE gridbox_mean_mod, ONLY:                                                    &
  surftiles_to_gbm, soiltiles_to_gbm

USE timestep_mod, ONLY: timestep

!Use variables
!Science variables
USE water_constants_mod, ONLY:                                                 &
  rho_water, tm

USE jules_surface_types_mod, ONLY:                                             &
  npft, lake, ncpft

USE theta_field_sizes, ONLY:                                                   &
  t_i_length, t_j_length

USE jules_surface_mod, ONLY:                                                   &
  l_aggregate, i_aggregate_opt

USE jules_water_resources_mod, ONLY:                                           &
  use_domestic, use_environment, use_industry, use_irrigation,                 &
  use_livestock, use_transfers, water_res_count

USE jules_radiation_mod, ONLY:                                                 &
  wght_alb

USE jules_vegetation_mod, ONLY:                                                &
  l_vegdrag_surft

USE c_z0h_z0m, ONLY:                                                           &
  z0h_z0m

USE model_grid_mod, ONLY:                                                      &
  grid_area_ij, latitude, longitude, projection_x_coord, projection_y_coord

USE ancil_info, ONLY:                                                          &
  dim_cs1, dim_cslayer, land_pts, nsurft, soil_pts, nsoilt

USE soil_ecosse_vars_mod, ONLY:                                                &
  i_amm, i_nit

USE gridmean_fluxes, ONLY:                                                     &
  fqw_1_ij,ftl_1_ij,taux_1_ij,tauy_1_ij

USE veg_param, ONLY:                                                           &
  secs_per_360days

USE jules_soil_biogeochem_mod, ONLY: soil_bgc_model, soil_model_ecosse,        &
  soil_model_4pool

USE jules_soil_ecosse_mod, ONLY: dt_soilc

USE jules_soil_mod, ONLY:                                                      &
  dzsoil, zsmc, sm_levels, ns_deep

USE sf_diags_mod, ONLY:                                                        &
  sf_diag

USE jules_snow_mod, ONLY:                                                      &
  canSnowTile

USE overbank_inundation_mod, ONLY:                                             &
  frac_fplain_lp, frac_fplain_rp

USE aero, ONLY:                                                                &
  co2_mmr

USE fire_mod,  ONLY:                                                           &
  fire_prog, fire_diag

USE jules_forcing_mod, ONLY: u_1_ij, v_1_ij

USE jules_rivers_mod, ONLY: nstep_rivers, np_rivers, n_rivers,                 &
   calc_outflow_per_river

USE coastal, ONLY: flandg

!Others
USE io_constants, ONLY:                                                        &
  mdi
USE logging_mod, ONLY:                                                         &
  log_fatal
USE data_cube_mod, ONLY:                                                       &
  data_cube, cube_from_array,                                                  &
  cube_free, OPERATOR ( * ), OPERATOR ( - )

USE parallel_mod, ONLY: is_master_task

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given an identifer for a model variable, returns the data currently
!   associated with that variable as a cube
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
INTEGER, INTENT(IN) :: var_id
                     ! Identifies the variable to extract data from


! Return
TYPE(data_cube) :: cube  ! The extracted data


! Work variables
TYPE(data_cube) :: cube_land  ! Workspace cube for land data
                              ! This is required so that it can be deallocated
                              ! to avoid memory leaks

TYPE(data_cube) :: lw_up_cube, lw_down_cube
                              ! Work cubes for use in calculation of lw_net
                              ! Required to avoid memory leaks

REAL :: workspace_land(land_pts)  ! Used as a space for calculations
                                  ! before creating a cube

REAL :: workspace_surft(land_pts, nsurft)
                                ! Used in calculation of tile variables

REAL :: workspace_levs(land_pts, sm_levels)
                                ! Used in calculation of gridbox mean (GBM)
                                ! on soil levels

REAL :: workspace_pft(land_pts, npft)
                                ! Used in calculation of PFT variables

REAL :: workspace_cpft(land_pts, ncpft)
                                ! Used in calculation of crop tile variables

REAL :: workspace_cs(land_pts, dim_cslayer)
                                ! Used in calculation of soil C layer
                                ! variables.

REAL :: workspace_rivers(np_rivers) ! Used in calculation of river
                                    ! grid variables (if rivers are off then
                                    ! this has size 0)

REAL :: ones(land_pts, sm_levels)  ! An array full of ones to aid with
                                   ! calculation of first frozen/unfrozen
                                   ! layer
INTEGER :: layer(land_pts)  ! Used in calculation of first frozen/unfrozen
                            ! soil layer

REAL :: sum_frac(land_pts)  ! Used in calculation of fsmc_gb - the sum
                            ! of frac_surft over all pft tiles

REAL :: dz  ! Used in calculation of available soil moisture
            ! Size of current soil layer
REAL :: ztop  ! Used in calculation of available soil moisture
              ! Depth to top of current soil layer

INTEGER :: i,j,k,l,n,m  ! Index variables

REAL :: onedvar_tmp(land_pts) ! Used to out put the 1-d variable

!-----------------------------------------------------------------------------
! Initialise data to missing data value
workspace_land(:)     = mdi
workspace_surft(:,:)  = mdi
workspace_cpft(:,:)   = mdi
workspace_levs(:,:)   = mdi
workspace_cs(:,:)     = mdi
workspace_rivers(:)   = mdi
ones(:,:)             = 1.0

! We use the string identifier to search for CASEs in the SELECT, rather than
! being sensitive to the implementation of integer variable ids
SELECT CASE ( get_string_identifier(var_id) )
CASE ( 'latitude' )
  cube = cube_from_array(latitude)

CASE ( 'longitude' )
  cube = cube_from_array(longitude)

CASE ( 'projection_x_coord' )
  cube = cube_from_array(projection_x_coord)

CASE ( 'projection_y_coord' )
  cube = cube_from_array(projection_y_coord)

CASE ( 'rivers_lat_rp' )
  cube = cube_from_array(rivers%rivers_lat_rp)

CASE ( 'rivers_lon_rp' )
  cube = cube_from_array(rivers%rivers_lon_rp)

CASE ( 'rivers_x_coord_rp' )
  cube = cube_from_array(rivers%rivers_x_coord_rp)

CASE ( 'rivers_y_coord_rp' )
  cube = cube_from_array(rivers%rivers_y_coord_rp)

CASE ( 'land_fraction' )
  cube = cube_from_array(flandg)

CASE ( 'frac' )
  cube_land = cube_from_array(ainfo%frac_surft)
  cube = map_from_land(cube_land)

CASE ( 'b' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%bexp_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for b with nsoilt>1: " //      &
                   "calculation is unphysical. Use b_soilt instead.")
  END IF

CASE ( 'b_soilt' )
  cube_land = cube_from_array(psparms%bexp_soilt)
  cube = map_from_land(cube_land)

CASE ( 'clay' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, dim_cslayer
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%clay_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:dim_cslayer))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for clay with nsoilt>1: " //   &
                   "calculation is unphysical. Use psparms%clay_soilt instead.")
  END IF

CASE ( 'clay_soilt' )
  cube_land = cube_from_array(psparms%clay_soilt)
  cube = map_from_land(cube_land)

CASE ( 'sathh' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%sathh_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sathh with nsoilt>1: " //  &
                  "calculation is unphysical. Use psparms%sathh_soilt instead.")
  END IF

CASE ( 'sathh_soilt' )
  cube_land = cube_from_array(psparms%sathh_soilt)
  cube = map_from_land(cube_land)

CASE ( 'satcon' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%satcon_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for satcon with nsoilt>1: " // &
                 "calculation is unphysical. Use psparms%satcon_soilt instead.")
  END IF

CASE ( 'satcon_soilt' )
  cube_land = cube_from_array(psparms%satcon_soilt)
  cube = map_from_land(cube_land)

CASE ( 'sm_sat' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%smvcst_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sm_sat with nsoilt>1: " // &
                   "calculation is unphysical. Use sm_sat_soilt instead.")
  END IF

CASE ( 'sm_sat_soilt' )
  cube_land = cube_from_array(psparms%smvcst_soilt)
  cube = map_from_land(cube_land)

CASE ( 'sm_crit' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%smvccl_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sm_crit with nsoilt>1:" // &
                   " calculation is unphysical. Use sm_crit_soilt instead.")
  END IF

CASE ( 'sm_crit_soilt' )
  cube_land = cube_from_array(psparms%smvccl_soilt)
  cube = map_from_land(cube_land)

CASE ( 'sm_wilt' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%smvcwt_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sm_wilt with nsoilt>1:" // &
                   " calculation is unphysical. Use sm_wilt_soilt instead.")
  END IF

CASE ( 'sm_wilt_soilt' )
  cube_land = cube_from_array(psparms%smvcwt_soilt)
  cube = map_from_land(cube_land)

CASE ( 'hcap' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%hcap_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for hcap with nsoilt>1: " //   &
                   "calculation is unphysical. Use psparms%hcap_soilt instead.")
  END IF

CASE ( 'hcap_soilt' )
  cube_land = cube_from_array(psparms%hcap_soilt)
  cube = map_from_land(cube_land)

CASE ( 'hcon' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%hcon_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for hcon with nsoilt>1: " //   &
                   "calculation is unphysical. Use psparms%hcon_soilt instead.")
  END IF

CASE ( 'hcon_soilt' )
  cube_land = cube_from_array(psparms%hcon_soilt)
  cube = map_from_land(cube_land)

  !-----------------------------------------------------------------------------
  ! Soil ancillaries on soil carbon layers.
  !-----------------------------------------------------------------------------
CASE ( 'soil_ph' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, dim_cslayer
      workspace_cs(:,k) = soiltiles_to_gbm(psparms%soil_ph_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_cs(:,1:dim_cslayer))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal( "extract_var",                                             &
                    "Cannot output GB mean value for soil_ph "           //    &
                    "with nsoilt>1: calculation is unphysical. "         //    &
                    "Use psparms%soil_ph_soilt instead." )
  END IF

CASE ( 'soil_ph_soilt' )
  cube_land = cube_from_array(psparms%soil_ph_soilt)
  cube = map_from_land(cube_land)

CASE ( 'albedo_land' )
  ! Calculate the albedo as used in subroutine control when calculating the net
  ! shortwave on tiles
  ! Here we take the average of diffuse albedos in VIS and NIR
  cube_land = cube_from_array(                                                 &
    surftiles_to_gbm( (wght_alb(1) * fluxes%alb_surft(:,:,1) +                 &
                   wght_alb(2) * fluxes%alb_surft(:,:,2) +                     &
                   wght_alb(3) * fluxes%alb_surft(:,:,3) +                     &
                   wght_alb(4) * fluxes%alb_surft(:,:,4)), ainfo )             &
  )
  cube = map_from_land(cube_land)

CASE ( 'canopy_gb' )
  !     Don't use the canopy_gb variable, as this is calculated part-way through
  !     a timestep.
  cube_land = cube_from_array(surftiles_to_gbm( progs%canopy_surft, ainfo ))
  cube = map_from_land(cube_land)

CASE ( 'cs_gb' )
  IF ( nsoilt == 1) THEN
    !Case for a single soil tile
    cube_land = cube_from_array(SUM(SUM(progs%cs_pool_soilt(:,1,:,:), 3), 2))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output cs_gb for soil tiled runs")
  END IF

CASE ( 'cs_label_gb' )
  IF ( nsoilt == 1) THEN
    !Case for a single soil tile
    cube_land = cube_from_array(SUM(SUM(progs%cs_pool_soilt(:,1,:,:) *         &
                                progs%frac_c_label_pool_soilt(:,1,:,:), 3), 2))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output cs_label_gb for soil tiled runs")
  END IF

CASE ( 'cs_soilt' )
  cube_land = cube_from_array(progs%cs_pool_soilt)
  cube = map_from_land(cube_land)

CASE ( 'cv' )
  cube_land = cube_from_array(trifctltype%cv_gb)
  cube = map_from_land(cube_land)

CASE ( 'depth_frozen' )
  IF ( nsoilt == 1) THEN
    ! Get frozen depth from surface
    ! Start by assuming that there are no frozen layers anywhere,
    !and so frozen depth is 0
    workspace_land(:) = 0.0

    ! Get the first unfrozen layer for every land point using MINLOC
    ! We give an array of ones as the array to take mins from,
    ! so that we get the minimum index at which the mask is true
    layer(:) = MINLOC(ones, 2, progs%t_soil_soilt(:,1,:) >= tm)

    ! If the layer found above is 0 for any point, that means
    !no unfrozen layers were found and hence the whole column is frozen
    WHERE ( layer(:) == 0 )
      workspace_land(:) = SUM(dzsoil(:))
    END WHERE

    ! At points where the layer found above is 1, that means
    ! no frozen layers so we can ignore those points
    ! Otherwise, interpolate to estimate depth of zero degC isotherm
    ! Do all points with the same unfrozen layer at once
    DO i = 2,sm_levels
      WHERE ( layer(:) == i )
        workspace_land(:) = SUM(dzSoil(1:i-1)) + dzSoil(i) *                   &
                          (tm - progs%t_soil_soilt(:,1,i-1)) /                 &
                          (progs%t_soil_soilt(:,1,i) -                         &
                           progs%t_soil_soilt(:,1,i-1))
      END WHERE
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for depth_frozen: " //         &
                   "soil tiled diagnostic not yet implemented.")
  END IF

CASE ( 'depth_frozen_sthf' )
  IF ( nsoilt == 1) THEN
    ! Get frozen depth from surface in terms on frozen/unfrozen soil moisture
    ! Method similar in construction to depth_frozen
    workspace_land(:) = 0.0
    layer(:) = MINLOC(ones, 2, psparms%sthf_soilt(:,1,:) == 0.0)
    WHERE ( layer(:) == 0 )
      workspace_land(:) = SUM(dzsoil(:)) - dzsoil(sm_levels) *                 &
            (1 - psparms%sthf_soilt(:,1,sm_levels) /                           &
            (psparms%sthf_soilt(:,1,sm_levels) +                               &
            psparms%sthu_soilt(:,1,sm_levels) -                                &
            psparms%sthu_min_soilt(:,1,sm_levels)))
      ! The bottom layer can be below freezing temp but only partially frozen.
    END WHERE
    DO i = 2,sm_levels
      WHERE ( layer(:) == i )
        workspace_land(:) = SUM(dzSoil(1:i-1)) - dzsoil(i-1) *                 &
                (1 - psparms%sthf_soilt(:,1,i-1) /                             &
                (psparms%sthf_soilt(:,1,i-1) +                                 &
                 psparms%sthu_soilt(:,1,i-1) - psparms%sthu_min_soilt(:,1,i-1)))
      END WHERE
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for depth_frozen_sthf: " //    &
                   "soil tiled diagnostic not yet implemented.")
  END IF

CASE ( 'depth_unfrozen' )
  IF ( nsoilt == 1) THEN
    ! Get unfrozen depth from surface
    ! See depth_frozen for method description, but swapping frozen for unfrozen
    ! and vica-versa
    workspace_land(:) = 0.0
    layer(:) = MINLOC(ones, 2, progs%t_soil_soilt(:,1,:) < tm)
    WHERE ( layer(:) == 0 )
      workspace_land(:) = SUM(dzsoil(:))
    END WHERE
    DO i = 2,sm_levels
      WHERE ( layer(:) == i )
        workspace_land(:) = SUM(dzSoil(1:i-1)) + dzSoil(i) *                   &
                          (tm - progs%t_soil_soilt(:,1,i-1)) /                 &
                          (progs%t_soil_soilt(:,1,i) -                         &
                           progs%t_soil_soilt(:,1,i-1))
      END WHERE
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for depth_unfrozen: "  //      &
                   "soil tiled diagnostic not yet implemented.")
  END IF

CASE ( 'depth_unfrozen_sthf' )
  IF ( nsoilt == 1) THEN
    ! Get unfrozen depth from surface in terms of frozen/unfrozen
    ! soil moisture
    ! See depth_frozen for method, but swapping frozen for unfrozen
    ! & vica-versa
    workspace_land(:) = 0.0
    layer(:) = MINLOC(ones, 2, psparms%sthf_soilt(:,1,:) > 0.0)
    WHERE ( layer(:) == 0 )
      workspace_land(:) = SUM(dzsoil(:))
    END WHERE
    DO i = 1,sm_levels           ! HERE WE CAN NO LONGER IGNORE i=1.
      WHERE ( layer(:) == i )
        workspace_land(:) = SUM(dzSoil(1:i)) - dzSoil(i) *                     &
                            psparms%sthf_soilt(:,1,i) /                        &
                            (psparms%sthf_soilt(:,1,i) +                       &
                             psparms%sthu_soilt(:,1,i) -                       &
                             psparms%sthu_min_soilt(:,1,i))
      END WHERE
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for depth_unfrozen_sthf: " //  &
                   "soil tiled diagnostic not yet implemented.")
  END IF

CASE ( 'drain' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%drain_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for drain with nsoilt>1: " //  &
                   "calculation is unphysical. Use toppdm%drain_soilt instead.")
  END IF

CASE ( 'drain_soilt' )
  cube_land = cube_from_array(toppdm%drain_soilt)
  cube = map_from_land(cube_land)

CASE ( 'elake' )
  workspace_land(:) = 0.0
  IF ( .NOT. l_aggregate .AND. lake > 0 )                                      &
    workspace_land(:) = fluxes%fqw_surft(:,lake) * ainfo%frac_surft(:,lake)
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)

CASE ( 'emis_gb' )
  cube_land = cube_from_array(surftiles_to_gbm(fluxes%emis_surft, ainfo))
  cube = map_from_land(cube_land)

CASE ( 'fch4_wetl' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%fch4_wetl_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for fch4_wetl with "  //       &
                   "nsoilt>1. Use toppdm%fch4_wetl_soilt instead.")
  END IF

CASE ( 'fch4_wetl_soilt' )
  cube_land = cube_from_array(toppdm%fch4_wetl_soilt)
  cube = map_from_land(cube_land)

CASE ( 'fch4_wetl_cs' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%fch4_wetl_cs_soilt,    &
                                                 ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for fch4_wetl_cs with " //     &
                   " nsoilt>1. Use toppdm%fch4_wetl_cs_soilt instead.")
  END IF

CASE ( 'fch4_wetl_cs_soilt' )
  cube_land = cube_from_array(toppdm%fch4_wetl_cs_soilt)
  cube = map_from_land(cube_land)

CASE ( 'fch4_wetl_npp' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%fch4_wetl_npp_soilt,   &
                                                 ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for fch4_wetl_npp with " //    &
                   "nsoilt>1. Use toppdm%fch4_wetl_npp_soilt instead.")
  END IF

CASE ( 'fch4_wetl_npp_soilt' )
  cube_land = cube_from_array(toppdm%fch4_wetl_npp_soilt)
  cube = map_from_land(cube_land)

CASE ( 'fch4_wetl_resps' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%fch4_wetl_resps_soilt, &
                                                 ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for fch4_wetl_resps with " //  &
                   "nsoilt>1. Use toppdm%fch4_wetl_resps_soilt instead.")
  END IF

CASE ( 'fch4_wetl_resps_soilt' )
  cube_land = cube_from_array(toppdm%fch4_wetl_resps_soilt)
  cube = map_from_land(cube_land)

CASE ( 'fsat' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%fsat_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for fsat with nsoilt>1. " //   &
                   "Use toppdm%fsat_soilt instead.")
  END IF

CASE ( 'fsat_soilt' )
  cube_land = cube_from_array(toppdm%fsat_soilt)
  cube = map_from_land(cube_land)

CASE ( 'fsmc_gb' )
  ! Calculate gridbox mean over PFTs.
  ! Calculate the weighted sum over pfts
  workspace_land(:) = SUM(fluxes%fsmc_pft * ainfo%frac_surft(:,1:npft), 2)
  sum_frac(:) = SUM(ainfo%frac_surft(:,1:npft), 2)
  ! Normalise to the vegetation fraction
  WHERE ( sum_frac > EPSILON(1.0) )
    workspace_land(:) = workspace_land(:) / sum_frac(:)
  ELSE WHERE
    ! Where there is no veg, set an impossible value
    workspace_land(:) = mdi
  END WHERE
  ! Copy the result onto the full grid
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)

CASE ( 'fwetl' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%fwetl_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for fwetl with nsoilt>1. " //  &
                   "Use toppdm%fwetl_soilt instead.")
  END IF

CASE ( 'fwetl_soilt' )
  cube_land = cube_from_array(toppdm%fwetl_soilt)
  cube = map_from_land(cube_land)

CASE ( 'gpp_gb' )
  cube_land = cube_from_array(trifctltype%gpp_gb)
  cube = map_from_land(cube_land)

CASE ( 'lai_gb' )
  ! n.b. lai_gb is calculated here to make sure it is
  ! consistent with lai_pft after lai_pft is updated
  workspace_land(:) = SUM(progs%lai_pft * ainfo%frac_surft(:,1:npft), 2)
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)

CASE ( 'et_stom_gb' )
  cube = cube_from_array(sf_diag%et_stom_ij)

CASE ( 'et_stom' )
  cube_land = cube_from_array(sf_diag%et_stom_surft)
  cube = map_from_land(cube_land)

CASE ( 'plantNumDensity' )
  cube_land = cube_from_array(progs%plantNumDensity)
  cube = map_from_land(cube_land)

CASE ( 'fprf' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(sf_diag%fprf)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output fprf with tiled soil." )
  END IF

CASE ( 'fsth' )
  IF ( nsoilt == 1) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = sf_diag%fsth(:,k)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output fsth with tiled soil." )
  END IF

CASE ( 'ftemp' )
  IF ( nsoilt == 1) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = sf_diag%ftemp(:,k)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output ftemp with tiled soil."  )
  END IF

CASE ( 'gs' )
  cube_land = cube_from_array(progs%gs_gb)
  cube = map_from_land(cube_land)

CASE ( 'hf_snow_melt' )
  cube_land = cube_from_array(fluxes%hf_snow_melt_gb)
  cube = map_from_land(cube_land)

CASE ( 'land_index' )
  cube_land = cube_from_array(REAL(ainfo%land_index))
  cube = map_from_land(cube_land)

CASE ( 'lice_index' )
  cube_land = cube_from_array(REAL(ainfo%lice_index))
  cube = map_from_land(cube_land)

CASE ( 'lit_c_mean' )
  cube_land = cube_from_array(trifctltype%lit_c_mn_gb)
  cube = map_from_land(cube_land)

CASE ( 'lw_net' )
  ! Upward flux.
  cube_land = cube_from_array(surftiles_to_gbm(sf_diag%lw_up_surft, ainfo))
  lw_up_cube = map_from_land(cube_land)
  ! Downward flux.
  lw_down_cube = cube_from_array(forcing%lw_down_ij)
  ! Now calculate the net flux
  cube = lw_down_cube - lw_up_cube

  ! Free work cubes
  CALL cube_free(lw_up_cube)
  CALL cube_free(lw_down_cube)

CASE ( 'lw_up' )
  cube_land = cube_from_array(surftiles_to_gbm(sf_diag%lw_up_surft, ainfo))
  cube = map_from_land(cube_land)

CASE ( 'npp_gb' )
  cube_land = cube_from_array(trifctltype%npp_gb)
  cube = map_from_land(cube_land)

CASE ( 'qbase' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%qbase_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for qbase with nsoilt>1. " //  &
                   "Use toppdm%qbase_soilt instead.")
  END IF

CASE ( 'qbase_soilt' )
  cube_land = cube_from_array(toppdm%qbase_soilt)
  cube = map_from_land(cube_land)

CASE ( 'qbase_zw' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%qbase_zw_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for qbase_zw with " //         &
                   "nsoilt>1. Use toppdm%qbase_zw_soilt instead.")
  END IF

CASE ( 'qbase_zw_soilt' )
  cube_land = cube_from_array(toppdm%qbase_zw_soilt)
  cube = map_from_land(cube_land)

CASE ( 'rad_net' )
  cube_land = cube_from_array(surftiles_to_gbm(fluxes%radnet_surft, ainfo))
  cube = map_from_land(cube_land)

CASE ( 'resp_p_gb' )
  cube_land = cube_from_array(trifctltype%resp_p_gb)
  cube = map_from_land(cube_land)

CASE ( 'resp_s_gb' )
  IF ( nsoilt == 1) THEN
    !Case for a single soil tile
    cube_land = cube_from_array(SUM(                                           &
                                SUM(trifctltype%resp_s_soilt(:,1,:,:), 3), 2))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output resp_s_gb for soil tiled runs.")
  END IF

CASE ( 'resp_label_cs_gb' )
  IF ( nsoilt == 1) THEN
    !Case for a single soil tile
    cube_land = cube_from_array(SUM(                                           &
                            SUM(trifctltype%resp_s_soilt(:,1,:,1:4) *          &
                            progs%frac_c_label_pool_soilt(:,1,:,1:4), 3), 2))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output resp_label_cs_gb for soil tiled runs")
  END IF

CASE ( 'resp_label_cs' )
  IF ( nsoilt == 1) THEN
    !Case for a single soil tile
    cube_land = cube_from_array(trifctltype%resp_s_soilt(:,1,:,1:4) *          &
                                  progs%frac_c_label_pool_soilt(:,1,:,1:4))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output resp_label_cs for soil tiled runs")
  END IF

CASE ( 'resp_s_dr_out' )
  ! HACK: We only output the total respiration for now
  cube_land = cube_from_array(SUM(trifctltype%resp_s_dr_out_gb(:,:,5), 2))
  cube = map_from_land(cube_land)

CASE ( 'resp_s_diag' )
  cube_land = cube_from_array(trif_vars%resp_s_diag_gb(:,:,1:4))
  cube = map_from_land(cube_land)

CASE ( 'resp_s_pot_diag' )
  cube_land = cube_from_array(trif_vars%resp_s_pot_diag_gb(:,:,1:4))
  cube = map_from_land(cube_land)

CASE ( 'immob_n' )
  cube_land = cube_from_array(trif_vars%immob_n_gb(:,:,1:4))
  cube = map_from_land(cube_land)

CASE ( 'immob_n_pot' )
  cube_land = cube_from_array(trif_vars%immob_n_pot_gb(:,:,1:4))
  cube = map_from_land(cube_land)

CASE ( 'minl_n' )
  cube_land = cube_from_array(trif_vars%minl_n_gb(:,:,1:4))
  cube = map_from_land(cube_land)

CASE ( 'minl_n_pot' )
  cube_land = cube_from_array(trif_vars%minl_n_pot_gb(:,:,1:4))
  cube = map_from_land(cube_land)

CASE ( 'ns' )
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_4pool )
    cube_land = cube_from_array(progs%ns_pool_gb(:,:,:))
  CASE ( soil_model_ecosse )
    IF (nsoilt == 1) THEN
      ! Take the organic pools.
      cube_land = cube_from_array(soilecosse%n_soil_pool_soilt(:,1,:,1:dim_cs1))
    ELSE
      CALL log_fatal("extract_var",                                            &
                     "Cannot output ns when nsoilt > 1")
    END IF
  END SELECT
  cube = map_from_land(cube_land)

CASE ( 'fN' )
  cube_land = cube_from_array(trif_vars%fn_gb(:,:))
  cube = map_from_land(cube_land)

CASE ( 'soil_CN' )
  IF ( nsoilt == 1) THEN
    !Case for a single soil tile
    cube_land = cube_from_array(progs%cs_pool_soilt(:,1,:,:) /                 &
                                progs%ns_pool_gb(:,:,:))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output soil_CN for soil tiled runs.")
  END IF

CASE ( 'resp_s_diag_gb' )
  cube_land = cube_from_array(SUM(trif_vars%resp_s_diag_gb(:,:,5), 2))
  cube = map_from_land(cube_land)

CASE ( 'resp_s_pot_diag_gb' )
  cube_land = cube_from_array(SUM(trif_vars%resp_s_pot_diag_gb(:,:,5), 2))
  cube = map_from_land(cube_land)

CASE ( 'immob_n_gb' )
  cube_land = cube_from_array(SUM(trif_vars%immob_n_gb(:,:,5), 2))
  cube = map_from_land(cube_land)

CASE ( 'immob_n_pot_gb' )
  cube_land = cube_from_array(SUM(trif_vars%immob_n_pot_gb(:,:,5), 2))
  cube = map_from_land(cube_land)

CASE ( 'minl_n_gb' )
  cube_land = cube_from_array(SUM(trif_vars%minl_n_gb(:,:,5), 2))
  cube = map_from_land(cube_land)

CASE ( 'minl_n_pot_gb' )
  cube_land = cube_from_array(SUM(trif_vars%minl_n_pot_gb(:,:,5), 2))
  cube = map_from_land(cube_land)

CASE ( 'ns_gb' )
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_4pool )
    cube_land = cube_from_array(SUM(SUM(progs%ns_pool_gb(:,:,1:4),DIM = 3), 2))
  CASE ( soil_model_ecosse )
    IF (nsoilt == 1) THEN
      ! Sum over levels and pools.
      cube_land = cube_from_array(SUM(                                         &
                                SUM(soilecosse%n_soil_pool_soilt(:,1,:,:),3),2))
    ELSE
      CALL log_fatal("extract_var",                                            &
                     "Cannot output ns_gb when nsoilt > 1")
    END IF
  END SELECT
  cube = map_from_land(cube_land)

CASE ( 'soil_CN_gb' )
  IF ( nsoilt == 1) THEN
    cube_land =                                                                &
      cube_from_array(SUM(SUM(progs%cs_pool_soilt(:,1,:,1:4),DIM = 3), 2) /    &
      SUM(SUM(progs%ns_pool_gb(:,:,1:4),DIM = 3), 2))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output soil_CN_gb for soil tiled runs.")
  END IF

CASE ( 'runoff' )
  cube_land = cube_from_array(fluxes%sub_surf_roff_gb(:) + fluxes%surf_roff_gb(:))
  cube = map_from_land(cube_land)

CASE ( 'sat_excess_roff' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%dun_roff_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sat_excess_roff with " //  &
                   "nsoilt>1. Use sat_excess_roff_soilt instead.")
  END IF

CASE ( 'sat_excess_roff_soilt' )
  cube_land = cube_from_array(toppdm%dun_roff_soilt)
  cube = map_from_land(cube_land)

CASE ( 'smc_avail_top' )
  IF ( nsoilt == 1) THEN
    workspace_land(:) = 0.0
    ! We maintain the depth of the top of the current layer as we go down
    ! through the soil column
    ztop = 0.0
    DO k = 1,sm_levels
      ! If the top of this layer is below where we want to calculate to,
      !we are done
      IF ( ztop >= zsmc ) EXIT

      ! Calculate the amount of this layer that we want to take into account
      dz = dzsoil(k)
      ! If the layer contains the calculation limit, update dz
      IF ( ztop + dz > zsmc ) dz = zsmc - ztop

      ! Add on the contribution for this layer
      DO j = 1,soil_pts
        i = ainfo%soil_index(j)
        workspace_land(i) = workspace_land(i) + rho_water * dz *               &
                            MAX(0.0,                                           &
                            psparms%sthu_soilt(i,1,k)                          &
                            * psparms%smvcst_soilt(i,1,k)                      &
                            - psparms%smvcwt_soilt(i,1,k))
      END DO
      ztop = ztop + dzsoil(k)
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for"                   //      &
                   "smc_avail_top with nsoilt>1: "                     //      &
                   "soil-tiled diagnostic not yet implemented.")
  END IF

CASE ( 'smc_avail_tot' )
  IF ( nsoilt == 1) THEN
    ! This is the same as smc_avail_top, but for whole column
    workspace_land(:) = 0.0
    DO k = 1,sm_levels
      DO j = 1,soil_pts
        i = ainfo%soil_index(j)
        workspace_land(i) = workspace_land(i) + rho_water * dzsoil(k) *        &
                            MAX(0.0,                                           &
                            psparms%sthu_soilt(i,1,k) *                        &
                            psparms%smvcst_soilt(i,1,k) -                      &
                            psparms%smvcwt_soilt(i,1,k))
      END DO
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for"                   //      &
                   "smc_avail_tot with nsoilt>1: "                     //      &
                   "soil-tiled diagnostic not yet implemented.")
  END IF

CASE ( 'smc_tot' )
  workspace_land(:) = 0.0
  IF ( nsoilt == 1) THEN
    DO k = 1,sm_levels
      DO j = 1,soil_pts
        i = ainfo%soil_index(j)
        workspace_land(i) = workspace_land(i) + rho_water * dzsoil(k) *        &
                            MAX(0.0,                                           &
                            (psparms%sthu_soilt(i,1,k) +                       &
                            psparms%sthf_soilt(i,1,k)) *                       &
                            psparms%smvcst_soilt(i,1,k))
      END DO
    END DO
  ELSE
    !We can't take a gbm of psparms%smvcst_soilt so add up manually
    DO k = 1,sm_levels
      DO m = 1,nsoilt
        DO j = 1,soil_pts
          i = ainfo%soil_index(j)
          workspace_land(i) = workspace_land(i) + (ainfo%frac_soilt(i,m) *     &
                              (rho_water * dzsoil(k) *                         &
                              MAX(0.0,                                         &
                              (psparms%sthu_soilt(i,m,k) +                     &
                              psparms%sthf_soilt(i,m,k)) *                     &
                              psparms%smvcst_soilt(i,m,k))))
        END DO !j
      END DO !m
    END DO !k
  END IF
  ! Map the result onto the full grid
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)

CASE ( 'snomlt_sub_htf' )
  cube_land = cube_from_array(fluxes%snomlt_sub_htf_gb)
  cube = map_from_land(cube_land)

CASE ( 'snow_can_gb' )
  ! Only include tiles where canopy snow model is used
  cube_land = cube_from_array(surftiles_to_gbm(progs%snow_surft, ainfo, canSnowTile))
  cube = map_from_land(cube_land)

CASE ( 'snow_depth_gb' )
  cube_land = cube_from_array(surftiles_to_gbm(progs%snowdepth_surft, ainfo))
  cube = map_from_land(cube_land)

CASE ( 'snow_frac' )
  ! Sum frac over tiles with snow.
  workspace_land(:) = 0.0
  IF ( l_aggregate ) THEN
    WHERE ( progs%snow_surft(:,1) + progs%snow_grnd_surft(:,1) > EPSILON(1.0) )
      workspace_land(:) = 1.0
    END WHERE
  ELSE
    workspace_land(:) = SUM(ainfo%frac_surft, 2, progs%snow_surft +            &
                            progs%snow_grnd_surft > EPSILON(1.0))
  END IF
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)

CASE ( 'snow_grnd_gb' )
  ! Only include tiles where canopy snow model is used
  cube_land = cube_from_array(surftiles_to_gbm(progs%snow_grnd_surft,          &
  ainfo,canSnowTile))
  cube = map_from_land(cube_land)

CASE ( 'snow_ice_gb' )
  ! Calculate sum of sice along the snow layers dimension
  DO n = 1,nsurft
    DO j = 1,ainfo%surft_pts(n)
      i = ainfo%surft_index(j,n)
      workspace_surft(i,n) = SUM(progs%sice_surft(i,n,1:progs%nsnow_surft(i,n)))
    END DO
  END DO
  cube_land = cube_from_array(surftiles_to_gbm(workspace_surft, ainfo))
  cube = map_from_land(cube_land)

CASE ( 'snow_liq_gb' )
  ! Calculate sum of sliq along the snow layers dimension
  DO n = 1,nsurft
    DO j = 1,ainfo%surft_pts(n)
      i = ainfo%surft_index(j,n)
      workspace_surft(i,n) = SUM(progs%sliq_surft(i,n,1:progs%nsnow_surft(i,n)))
    END DO
  END DO
  cube_land = cube_from_array(surftiles_to_gbm(workspace_surft, ainfo))
  cube = map_from_land(cube_land)

CASE ( 'snow_melt_gb' )
  cube_land = cube_from_array(fluxes%snow_melt_gb)
  cube = map_from_land(cube_land)

CASE ( 'soil_index' )
  DO l = 1,soil_pts
    workspace_land(ainfo%soil_index(l)) = REAL(ainfo%soil_index(l))
  END DO
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)

CASE ( 'sthzw' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%sthzw_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sthzw with nsoilt>1: " //  &
                   "calculation is unphysical. Use toppdm%sthzw_soilt instead.")
  END IF

CASE ( 'sthzw_soilt' )
  cube_land = cube_from_array(toppdm%sthzw_soilt)
  cube = map_from_land(cube_land)

CASE ( 'sub_surf_roff' )
  cube_land = cube_from_array(fluxes%sub_surf_roff_gb)
  cube = map_from_land(cube_land)

CASE ( 'surf_roff' )
  cube_land = cube_from_array(fluxes%surf_roff_gb)
  cube = map_from_land(cube_land)

CASE ( 'swet_liq_tot' )
  IF ( nsoilt == 1) THEN
    ! Divide column unfrozen moisture content by saturated moisture content
    workspace_land(:) = 0.0
    DO j = 1,soil_pts
      i = ainfo%soil_index(j)
      workspace_land(i) = SUM(dzsoil(:) * MAX(0.0,                             &
                        psparms%sthu_soilt(i,1,:) *                            &
                        psparms%smvcst_soilt(i,1,:)))                          &
                        / SUM(dzsoil(:) * MAX(0.0, psparms%smvcst_soilt(i,1,:)))
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for"                   //      &
                   "swet_liq_tot with nsoilt>1: "                      //      &
                   "soil tiled diagnostic not yet implemented.")
  END IF

CASE ( 'swet_tot' )
  IF ( nsoilt == 1) THEN
    ! Divide column total moisture content by saturated moisture content
    workspace_land(:) = 0.0
    DO j = 1,soil_pts
      i = ainfo%soil_index(j)
      workspace_land(i) = SUM(dzsoil(:) *                                      &
                            MAX(0.0,                                           &
                            (psparms%sthu_soilt(i,1,:) +                       &
                            psparms%sthf_soilt(i,1,:)) *                       &
                            psparms%smvcst_soilt(i,1,:)))                      &
                            /SUM(dzsoil(:) * MAX(0.0,                          &
                            psparms%smvcst_soilt(i,1,:)))
    END DO
    ! Map the result onto the full grid
    cube_land = cube_from_array(workspace_land)
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for "                   //     &
                   "swet_tot wth nsoilt>1: "                            //     &
                   "soil tiled diagnostic not yet implemented.")
  END IF

CASE ( 'sw_net' )
  ! Calculate the albedo as used in subroutine control when calculating
  ! the net shortwave on tiles
  ! Here we take the average of diffuse albedos in VIS and NIR.
  DO l = 1,land_pts
    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length
    workspace_land(l) = ( 1.0 - (                                              &
                          wght_alb(1) * fluxes%land_albedo_ij(i,j,1) +         &
                          wght_alb(2) * fluxes%land_albedo_ij(i,j,2) +         &
                          wght_alb(3) * fluxes%land_albedo_ij(i,j,3) +         &
                          wght_alb(4) * fluxes%land_albedo_ij(i,j,4)           &
                ) ) * forcing%sw_down_ij(i,j)

  END DO
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)

CASE ( 'NDVI_land' )
  ! Calculate NDVI on tiles
  ! Using the average of direct & diffuse albedos in VIS and NIR.
  DO l = 1,land_pts
    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length
    workspace_land(l) =                                                        &
      (((fluxes%land_albedo_ij(i,j,3) + fluxes%land_albedo_ij(i,j,4)) -        &
        (fluxes%land_albedo_ij(i,j,1) + fluxes%land_albedo_ij(i,j,2))) /       &
      ((fluxes%land_albedo_ij(i,j,3) + fluxes%land_albedo_ij(i,j,4)) +         &
        (fluxes%land_albedo_ij(i,j,1) + fluxes%land_albedo_ij(i,j,2))))

  END DO
  cube_land = cube_from_array(workspace_land)
  cube = map_from_land(cube_land)


CASE ( 'tfall' )
  cube_land = cube_from_array(fluxes%tot_tfall_gb)
  cube = map_from_land(cube_land)

CASE ( 'trad' )
  ! Assuming emissivity=1.
  cube_land = cube_from_array((                                                &
                        surftiles_to_gbm(progs%tstar_surft**4, ainfo) )**0.25)
  cube = map_from_land(cube_land)

CASE ( 'zw' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soiltiles_to_gbm(toppdm%zw_soilt,ainfo))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for zw with nsoilt>1. " //     &
                   "Use toppdm%zw_soilt instead.")
  END IF

CASE ( 'zw_soilt' )
  cube_land = cube_from_array(toppdm%zw_soilt)
  cube = map_from_land(cube_land)

CASE ( 'c_veg' )
  cube_land = cube_from_array(trifctltype%c_veg_pft)
  cube = map_from_land(cube_land)

CASE ( 'fapar' )
  cube_land = cube_from_array(trif_vars%fapar_diag_pft)
  cube = map_from_land(cube_land)

CASE ( 'apar' )
  cube_land = cube_from_array(trif_vars%apar_diag_pft)
  cube = map_from_land(cube_land)

CASE ( 'apar_gb' )
  cube_land = cube_from_array(trif_vars%apar_diag_gb)
  cube = map_from_land(cube_land)

CASE ( 'fao_et0' )
  cube_land = cube_from_array(trif_vars%fao_et0)
  cube = map_from_land(cube_land)

CASE ( 'canht' )
  cube_land = cube_from_array(progs%canht_pft)
  cube = map_from_land(cube_land)

CASE ( 'flux_o3_stom' )
  cube_land = cube_from_array(chemvars%flux_o3_pft)
  cube = map_from_land(cube_land)

CASE ( 'fsmc' )
  cube_land = cube_from_array(fluxes%fsmc_pft)
  cube = map_from_land(cube_land)

CASE ( 'g_leaf' )
  cube_land = cube_from_array(trifctltype%g_leaf_pft)
  cube = map_from_land(cube_land)

CASE ( 'g_leaf_day' )
  cube_land = cube_from_array(trifctltype%g_leaf_day_pft)
  cube = map_from_land(cube_land)

CASE ( 'g_leaf_dr_out' )
  cube_land = cube_from_array(trifctltype%g_leaf_dr_out_pft)
  cube = map_from_land(cube_land)

CASE ( 'g_leaf_phen' )
  cube_land = cube_from_array(trifctltype%g_leaf_phen_pft)
  cube = map_from_land(cube_land)

CASE ( 'gpp' )
  cube_land = cube_from_array(trifctltype%gpp_pft)
  cube = map_from_land(cube_land)

CASE ( 'lai' )
  cube_land = cube_from_array(progs%lai_pft)
  cube = map_from_land(cube_land)

CASE ( 'lai_phen' )
  cube_land = cube_from_array(trifctltype%lai_phen_pft)
  cube = map_from_land(cube_land)

CASE ( 'lit_c' )
  cube_land = cube_from_array(trifctltype%lit_c_pft)
  cube = map_from_land(cube_land)

CASE ( 'lit_c_ag' )
  cube_land = cube_from_array(trif_vars%lit_c_ag_pft)
  cube = map_from_land(cube_land)

CASE ( 'lit_c_orig' )
  cube_land = cube_from_array(trif_vars%lit_c_orig_pft)
  cube = map_from_land(cube_land)

CASE ( 'harvest' )
  cube_land = cube_from_array(trif_vars%harvest_pft)
  cube = map_from_land(cube_land)

CASE ( 'harvest_gb' )
  cube_land = cube_from_array(trif_vars%harvest_gb)
  cube = map_from_land(cube_land)

CASE ( 'harvest_n' )
  cube_land = cube_from_array(trif_vars%harvest_n_pft)
  cube = map_from_land(cube_land)

CASE ( 'harvest_n_gb' )
  cube_land = cube_from_array(trif_vars%harvest_n_gb)
  cube = map_from_land(cube_land)

CASE ( 'harvest_biocrop' )
  cube_land = cube_from_array(trif_vars%harvest_biocrop_pft)
  cube = map_from_land(cube_land)

CASE ( 'harvest_biocrop_gb' )
  cube_land = cube_from_array(trif_vars%harvest_biocrop_gb)
  cube = map_from_land(cube_land)

CASE ( 'harvest_biocrop_n' )
  cube_land = cube_from_array(trif_vars%harvest_biocrop_n_pft)
  cube = map_from_land(cube_land)

CASE ( 'harvest_biocrop_n_gb' )
  cube_land = cube_from_array(trif_vars%harvest_biocrop_n_gb)
  cube = map_from_land(cube_land)

CASE ( 'frac_biocrop' )
  cube_land = cube_from_array(trif_vars%frac_biocrop_gb)
  cube = map_from_land(cube_land)

CASE ( 'harvest_doy' )
  workspace_pft(:,:) = REAL(trif_vars%harvest_doy(:,:))
  cube_land = cube_from_array(workspace_pft)
  cube = map_from_land(cube_land)

CASE ( 'years_since_harvest' )
  workspace_pft(:,:) = REAL(progs%years_since_harvest(:,:))
  cube_land = cube_from_array(workspace_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_fertiliser' )
  cube_land = cube_from_array(trif_vars%n_fertiliser_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_fertiliser_gb' )
  cube_land = cube_from_array(trif_vars%n_fertiliser_gb)
  cube = map_from_land(cube_land)

CASE ( 'root_abandon' )
  cube_land = cube_from_array(trif_vars%root_abandon_pft)
  cube = map_from_land(cube_land)

CASE ( 'root_abandon_gb' )
  cube_land = cube_from_array(trif_vars%root_abandon_gb)
  cube = map_from_land(cube_land)

CASE ( 'root_abandon_n' )
  cube_land = cube_from_array(trif_vars%root_abandon_n_pft)
  cube = map_from_land(cube_land)

CASE ( 'root_abandon_n_gb' )
  cube_land = cube_from_array(trif_vars%root_abandon_n_gb)
  cube = map_from_land(cube_land)

CASE ( 'lit_n_ag' )
  cube_land = cube_from_array(trif_vars%lit_n_ag_pft)
  cube = map_from_land(cube_land)

CASE ( 'lit_n_orig' )
  cube_land = cube_from_array(trif_vars%lit_n_orig_pft)
  cube = map_from_land(cube_land)

CASE ( 'npp_dr_out' )
  cube_land = cube_from_array(trifctltype%npp_dr_out_pft)
  cube = map_from_land(cube_land)

CASE ( 'npp' )
  cube_land = cube_from_array(trifctltype%npp_pft)
  cube = map_from_land(cube_land)

CASE ( 'o3_exp_fac' )
  cube_land = cube_from_array(chemvars%fo3_pft)
  cube = map_from_land(cube_land)

CASE ( 'co2_mmr' )
  onedvar_tmp(:)=co2_mmr
  cube_land = cube_from_array(onedvar_tmp)
  cube = map_from_land(cube_land)

CASE ( 'frac_agr' )
  cube_land = cube_from_array(trifctltype%frac_agr_gb)
  cube = map_from_land(cube_land)

CASE ( 'frac_past' )
  cube_land = cube_from_array(trif_vars%frac_past_gb)
  cube = map_from_land(cube_land)

CASE ( 'pc_s' )
  cube_land = cube_from_array(trif_vars%pc_s_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_leaf' )
  cube_land = cube_from_array(trif_vars%n_leaf_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_root' )
  cube_land = cube_from_array(trif_vars%n_root_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_stem' )
  cube_land = cube_from_array(trif_vars%n_stem_pft)
  cube = map_from_land(cube_land)

CASE ( 'lai_bal' )
  cube_land = cube_from_array(trif_vars%lai_bal_pft)
  cube = map_from_land(cube_land)

CASE ( 'resp_p' )
  cube_land = cube_from_array(trifctltype%resp_p_pft)
  cube = map_from_land(cube_land)

CASE ( 'growth_sug_gb' )
  cube_land = cube_from_array(trifctltype%growth_sug_gb)
  cube = map_from_land(cube_land)

CASE ( 'growth_sug' )
  cube_land = cube_from_array(trifctltype%growth_sug_pft)
  cube = map_from_land(cube_land)

CASE ( 'f_nsc' )
  cube_land = cube_from_array(progs%f_nsc_pft)
  cube = map_from_land(cube_land)

CASE ( 'resp_r' )
  cube_land = cube_from_array(trif_vars%resp_r_pft)
  cube = map_from_land(cube_land)

CASE ( 'resp_l' )
  cube_land = cube_from_array(trif_vars%resp_l_pft)
  cube = map_from_land(cube_land)

CASE ( 'resp_w_dr_out' )
  cube_land = cube_from_array(trifctltype%resp_w_dr_out_pft)
  cube = map_from_land(cube_land)

CASE ( 'resp_w' )
  cube_land = cube_from_array(trifctltype%resp_w_pft)
  cube = map_from_land(cube_land)

CASE ( 'lwp_c' )
  cube_land = cube_from_array(trifctltype%lwp_c_pft)
  cube = map_from_land(cube_land)

CASE ( 'cs' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(progs%cs_pool_soilt(:,1,:,:))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for cs when nsoilt > 1.")
  END IF

CASE ( 'cs_label' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(progs%cs_pool_soilt(:,1,:,:) *                 &
                                       progs%frac_c_label_pool_soilt(:,1,:,:))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for cs when nsoilt > 1.")
  END IF

CASE ( 'resp_s_to_atmos_gb' )
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_4pool )
    cube_land = cube_from_array( SUM(trif_vars%resp_s_to_atmos_gb(:,:),2) )
  CASE ( soil_model_ecosse )
    ! Convert units from kg m-2 s-1 to kg m-2 (360 days)-1.
    cube_land = cube_from_array( soilecosse%co2_soil_gb(:) * secs_per_360days  &
                                 / dt_soilc )
  CASE DEFAULT
    CALL log_fatal("extract_var",                                              &
                   "Cannot output resps_to_atmos_gb - no code for " //         &
                   "this soil model.")
  END SELECT
  cube = map_from_land(cube_land)

CASE ( 'resp_s' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(trifctltype%resp_s_soilt(:,1,:,:))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for resp_s when nsoilt > 1.")
  END IF

CASE ( 'wood_prod_fast' )
  cube_land = cube_from_array(progs%wood_prod_fast_gb)
  cube = map_from_land(cube_land)

CASE ( 'wood_prod_med' )
  cube_land = cube_from_array(progs%wood_prod_med_gb)
  cube = map_from_land(cube_land)

CASE ( 'wood_prod_slow' )
  cube_land = cube_from_array(progs%wood_prod_slow_gb)
  cube = map_from_land(cube_land)

CASE ( 'WP_fast_in' )
  cube_land = cube_from_array(trif_vars%WP_fast_in_gb)
  cube = map_from_land(cube_land)

CASE ( 'WP_med_in' )
  cube_land = cube_from_array(trif_vars%WP_med_in_gb)
  cube = map_from_land(cube_land)

CASE ( 'WP_slow_in' )
  cube_land = cube_from_array(trif_vars%WP_slow_in_gb)
  cube = map_from_land(cube_land)

CASE ( 'WP_fast_out' )
  cube_land = cube_from_array(trif_vars%WP_fast_out_gb)
  cube = map_from_land(cube_land)

CASE ( 'WP_med_out' )
  cube_land = cube_from_array(trif_vars%WP_med_out_gb)
  cube = map_from_land(cube_land)

CASE ( 'WP_slow_out' )
  cube_land = cube_from_array(trif_vars%WP_slow_out_gb)
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_carbon_veg2' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_carbon_veg2_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_carbon_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_carbon_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_veg_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_veg_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_soil_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_soil_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_prod_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_prod_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_nitrogen_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_nitrogen_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_vegN_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_vegN_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_soilN_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_soilN_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'cnsrv_n_inorg_triffid' )
  cube_land = cube_from_array(REAL(trif_vars%cnsrv_n_inorg_triffid_gb))
  cube = map_from_land(cube_land)

CASE ( 'lit_c_fire' )
  cube_land = cube_from_array(REAL(trif_vars%lit_c_fire_pft))
  cube = map_from_land(cube_land)

CASE ( 'burnt_carbon_dpm' )
  cube_land = cube_from_array(REAL(trif_vars%burnt_carbon_dpm))
  cube = map_from_land(cube_land)

CASE ( 'lit_n_fire' )
  cube_land = cube_from_array(REAL(trif_vars%lit_n_fire_pft))
  cube = map_from_land(cube_land)

CASE ( 'burnt_carbon_rpm' )
  cube_land = cube_from_array(REAL(trif_vars%burnt_carbon_rpm))
  cube = map_from_land(cube_land)

CASE ( 'veg_c_fire_emission_gb' )
  cube_land = cube_from_array(REAL(trif_vars%veg_c_fire_emission_gb))
  cube = map_from_land(cube_land)

CASE ( 'veg_c_fire_emission_pft' )
  cube_land = cube_from_array(REAL(trif_vars%veg_c_fire_emission_pft))
  cube = map_from_land(cube_land)

CASE ( 'n_inorg' )
  cube_land = cube_from_array(progs%n_inorg_soilt_lyrs(:,1,:))
  cube = map_from_land(cube_land)

CASE ( 'n_inorg_avail_pft' )
  cube_land = cube_from_array(progs%n_inorg_avail_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_inorg_gb' )
  IF ( nsoilt == 1 ) THEN
    IF ( soil_bgc_model == soil_model_4pool ) THEN
      cube_land = cube_from_array( SUM( progs%n_inorg_soilt_lyrs(:,1,:), 2 ) )
    ELSE IF (  soil_bgc_model == soil_model_ecosse ) THEN
      ! Sum over levels.
      cube_land = cube_from_array( SUM(                                        &
                                soilecosse%n_soil_pool_soilt(:,1,:,i_amm) +    &
                                soilecosse%n_soil_pool_soilt(:,1,:,i_nit), 2 ) )
    END IF
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for n_inorg_gb with " //       &
                   "nsoilt>1.")
  END IF

CASE ( 'deposition_n' )
  cube_land = cube_from_array(trif_vars%deposition_n_gb)
  cube = map_from_land(cube_land)

CASE ( 'substr_ch4' )
  cube_land = cube_from_array(progs%substr_ch4)
  cube = map_from_land(cube_land)

CASE ( 'mic_ch4' )
  cube_land = cube_from_array(progs%mic_ch4)
  cube = map_from_land(cube_land)

CASE ( 'mic_act_ch4' )
  cube_land = cube_from_array(progs%mic_act_ch4)
  cube = map_from_land(cube_land)

CASE ( 'acclim_ch4' )
  cube_land = cube_from_array(progs%acclim_ch4)
  cube = map_from_land(cube_land)

CASE ( 'g_burn_pft' )
  cube_land = cube_from_array(REAL(trif_vars%g_burn_pft))
  cube = map_from_land(cube_land)

CASE ( 'g_burn_gb' )
  cube_land = cube_from_array(REAL(trif_vars%g_burn_gb))
  cube = map_from_land(cube_land)

CASE ( 'leafC' )
  cube_land = cube_from_array(trif_vars%leafc_pft)
  cube = map_from_land(cube_land)

CASE ( 'rootC' )
  cube_land = cube_from_array(trif_vars%rootc_pft)
  cube = map_from_land(cube_land)

CASE ( 'stemC' )
  cube_land = cube_from_array(trif_vars%stemc_pft)
  cube = map_from_land(cube_land)

CASE ( 'woodC' )
  cube_land = cube_from_array(trif_vars%woodc_pft)
  cube = map_from_land(cube_land)

CASE ( 'dleaf' )
  cube_land = cube_from_array(trif_vars%dleaf_pft)
  cube = map_from_land(cube_land)

CASE ( 'droot' )
  cube_land = cube_from_array(trif_vars%droot_pft)
  cube = map_from_land(cube_land)

CASE ( 'dwood' )
  cube_land = cube_from_array(trif_vars%dwood_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_uptake' )
  cube_land = cube_from_array(trif_vars%n_uptake_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_uptake_gb' )
  cube_land = cube_from_array(trif_vars%n_uptake_gb)
  cube = map_from_land(cube_land)

CASE ( 'leaf_litC' )
  cube_land = cube_from_array(trif_vars%leaf_litc_pft)
  cube = map_from_land(cube_land)

CASE ( 'root_litC' )
  cube_land = cube_from_array(trif_vars%root_litc_pft)
  cube = map_from_land(cube_land)

CASE ( 'wood_litC' )
  cube_land = cube_from_array(trif_vars%wood_litc_pft)
  cube = map_from_land(cube_land)

CASE ( 'leaf_litN' )
  cube_land = cube_from_array(trif_vars%leaf_litn_pft)
  cube = map_from_land(cube_land)

CASE ( 'root_litN' )
  cube_land = cube_from_array(trif_vars%root_litn_pft)
  cube = map_from_land(cube_land)

CASE ( 'wood_litN' )
  cube_land = cube_from_array(trif_vars%wood_litn_pft)
  cube = map_from_land(cube_land)

CASE ( 'litterC' )
  cube_land = cube_from_array(trif_vars%litterc_pft)
  cube = map_from_land(cube_land)

CASE ( 'litterN' )
  cube_land = cube_from_array(trif_vars%littern_pft)
  cube = map_from_land(cube_land)

CASE ( 'exudates' )
  cube_land = cube_from_array(trif_vars%exudates_pft)
  cube = map_from_land(cube_land)

CASE ( 'exudates_gb' )
  cube_land = cube_from_array(trif_vars%exudates_gb)
  cube = map_from_land(cube_land)

CASE ( 'npp_n' )
  cube_land = cube_from_array(trif_vars%npp_n)
  cube = map_from_land(cube_land)

CASE ( 'npp_n_gb' )
  cube_land = cube_from_array(trif_vars%npp_n_gb)
  cube = map_from_land(cube_land)

CASE ( 'nbp_gb' )
  cube_land = cube_from_array(trif_vars%npp_n_gb                       -       &
                              SUM(trif_vars%resp_s_to_atmos_gb(:,:),2) -       &
                              trif_vars%wp_fast_out_gb                 -       &
                              trif_vars%wp_med_out_gb                  -       &
                              trif_vars%wp_slow_out_gb                 -       &
                              trif_vars%harvest_gb                     -       &
                              trif_vars%veg_c_fire_emission_gb         -       &
                              trif_vars%burnt_carbon_dpm               -       &
                              trif_vars%burnt_carbon_rpm )
  cube = map_from_land(cube_land)

CASE ( 'n_demand' )
  cube_land = cube_from_array(trif_vars%n_demand_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_fix' )
  cube_land = cube_from_array(trif_vars%n_fix_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_fix_gb' )
  cube_land = cube_from_array(trif_vars%n_fix_gb)
  cube = map_from_land(cube_land)

CASE ( 'n_gas' )
  cube_land = cube_from_array(trif_vars%n_gas_gb(:,:))
  cube = map_from_land(cube_land)

CASE ( 'n_gas_gb' )
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_4pool )
    cube_land = cube_from_array(SUM(trif_vars%n_gas_gb(:,:),2))
  CASE ( soil_model_ecosse )
    cube_land = cube_from_array( soilecosse%n2_denitrif_gb                     &
                                                + soilecosse%no_soil_gb        &
                                                + soilecosse%n2o_soil_gb )
  END SELECT
  cube = map_from_land(cube_land)

CASE ( 'n_leach' )
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_4pool )
    IF (nsoilt == 1) THEN
      cube_land = cube_from_array(trif_vars%n_leach_soilt(:,1))
    ELSE
      CALL log_fatal("extract_var",                                            &
                     "Cannot output GB mean value for n_leach when " //        &
                     "nsoilt > 1.")
    END IF
  CASE ( soil_model_ecosse )
    cube_land = cube_from_array( soilecosse%n_leach_amm_gb(:) +                &
                                 soilecosse%n_leach_nit_gb(:) )
  END SELECT
  cube = map_from_land(cube_land)

CASE ( 'n_demand_gb' )
  cube_land = cube_from_array(trif_vars%n_demand_gb)
  cube = map_from_land(cube_land)

CASE ( 'n_demand_growth' )
  cube_land = cube_from_array(trif_vars%n_demand_growth_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_uptake_growth' )
  cube_land = cube_from_array(trif_vars%n_uptake_growth_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_demand_lit' )
  cube_land = cube_from_array(trif_vars%n_demand_lit_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_demand_spread' )
  cube_land = cube_from_array(trif_vars%n_demand_spread_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_veg' )
  cube_land = cube_from_array(trif_vars%n_veg_pft)
  cube = map_from_land(cube_land)

CASE ( 'n_veg_gb' )
  cube_land = cube_from_array(trif_vars%n_veg_gb)
  cube = map_from_land(cube_land)

CASE ( 'n_loss' )
  cube_land = cube_from_array(trif_vars%n_loss_gb)
  cube = map_from_land(cube_land)

CASE ( 'dpm_ratio' )
  cube_land = cube_from_array(trif_vars%dpm_ratio_gb)
  cube = map_from_land(cube_land)

CASE ( 'dnveg' )
  cube_land = cube_from_array(trif_vars%dnveg_pft)
  cube = map_from_land(cube_land)

CASE ( 'dnveg_gb' )
  cube_land = cube_from_array(trif_vars%dnveg_gb)
  cube = map_from_land(cube_land)

CASE ( 'dcveg' )
  cube_land = cube_from_array(trif_vars%dcveg_pft)
  cube = map_from_land(cube_land)

CASE ( 'dcveg_gb' )
  cube_land = cube_from_array(trif_vars%dcveg_gb)
  cube = map_from_land(cube_land)

CASE ( 'lit_N' )
  cube_land = cube_from_array(trif_vars%lit_n_pft)
  cube = map_from_land(cube_land)

CASE ( 'lit_N_t' )
  cube_land = cube_from_array(trif_vars%lit_n_t_gb)
  cube = map_from_land(cube_land)

CASE ( 'lit_n' )
  cube_land = cube_from_array(trif_vars%lit_n_pft)
  cube = map_from_land(cube_land)

CASE ( 'lit_n_t' )
  cube_land = cube_from_array(trif_vars%lit_n_t_gb)
  cube = map_from_land(cube_land)

CASE ( 'con_rain' )
  cube = cube_from_array(forcing%con_rain_ij)

CASE ( 'con_snow' )
  cube = cube_from_array(forcing%con_snow_ij)

CASE ( 'cosz' )
  cube = cube_from_array(psparms%cosz_ij)

CASE ( 'diff_frac' )
  cube = cube_from_array(RESHAPE(jules_vars%diff_frac, [ t_i_length, t_j_length ]))

CASE ( 'ecan_gb' )
  cube = cube_from_array(fluxes%ecan_ij)

CASE ( 'ei_gb' )
  cube = cube_from_array(fluxes%ei_ij)

CASE ( 'esoil_gb' )
  IF (nsoilt == 1) THEN
    cube = cube_from_array(fluxes%esoil_ij_soilt(:,:,1))
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for esoil_gb when nsoilt>1.")
  END IF

CASE ( 'fqw_gb' )
  cube = cube_from_array(fqw_1_ij)

CASE ( 'ftl_gb' )
  cube = cube_from_array(ftl_1_ij)

CASE ( 'tau_gb' )
  cube = cube_from_array(sf_diag%tau_1)

CASE ( 'land_albedo_1' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,1))

CASE ( 'land_albedo_2' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,2))

CASE ( 'land_albedo_3' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,3))

CASE ( 'land_albedo_4' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,4))

CASE ( 'upward_sw_1' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,1) * forcing%sw_down_ij(:,:) * wght_alb(1))

CASE ( 'upward_sw_2' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,2) * forcing%sw_down_ij(:,:) * wght_alb(2))

CASE ( 'upward_sw_3' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,3) * forcing%sw_down_ij(:,:) * wght_alb(3))

CASE ( 'upward_sw_4' )
  cube = cube_from_array(fluxes%land_albedo_ij(:,:,4) * forcing%sw_down_ij(:,:) * wght_alb(4))

CASE ( 'latent_heat' )
  cube = cube_from_array(sf_diag%latent_heat)

CASE ( 'ls_rain' )
  cube = cube_from_array(forcing%ls_rain_ij)

CASE ( 'ls_snow' )
  cube = cube_from_array(forcing%ls_snow_ij)

CASE ( 'lw_down' )
  cube = cube_from_array(forcing%lw_down_ij)

CASE ( 'precip' )
  cube = cube_from_array(forcing%ls_rain_ij + forcing%con_rain_ij +            &
                         forcing%ls_snow_ij + forcing%con_snow_ij)

CASE ( 'pstar' )
  cube = cube_from_array(forcing%pstar_ij)

CASE ( 'q1p5m_gb' )
  cube = cube_from_array(sf_diag%q1p5m)

CASE ( 'qw1' )
  cube = cube_from_array(forcing%qw_1_ij)

CASE ( 'rainfall' )
  cube = cube_from_array(forcing%ls_rain_ij + forcing%con_rain_ij)

CASE ( 'snomlt_surf_htf' )
  cube = cube_from_array(sf_diag%snomlt_surf_htf)

CASE ( 'snowfall' )
  cube = cube_from_array(forcing%ls_snow_ij + forcing%con_snow_ij)

CASE ( 'snow_mass_gb' )
  !     Don't use the snow_mass_ij variable as that is calculated under control.
  cube_land = cube_from_array(surftiles_to_gbm(progs%snow_grnd_surft +         &
                                               progs%snow_surft, ainfo))
  cube = map_from_land(cube_land)

CASE ( 'surf_ht_flux_gb' )
  cube = cube_from_array(fluxes%surf_ht_flux_ij)

CASE ( 'sw_down' )
  cube = cube_from_array(forcing%sw_down_ij)

CASE ( 't1p5m_gb' )
  cube = cube_from_array(sf_diag%t1p5m)

CASE ( 'taux1' )
  cube = cube_from_array(taux_1_ij)

CASE ( 'tauy1' )
  cube = cube_from_array(tauy_1_ij)

CASE ( 'tl1' )
  cube = cube_from_array(forcing%tl_1_ij)

CASE ( 'tstar_gb' )
  !     Don't use tstar as that is calculated part-way through a timestep.
  cube_land = cube_from_array(surftiles_to_gbm( progs%tstar_surft, ainfo ))
  cube = map_from_land(cube_land)

  !-----------------------------------------------------------------------------
  ! imogen driving variables
  !-----------------------------------------------------------------------------
CASE ( 'tl1_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%tl1_ij_clim)

CASE ( 'ql1_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%ql1_ij_clim)

CASE ( 'wind_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%wind_ij_clim)

CASE ( 'lwdown_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%lwdown_ij_clim)

CASE ( 'swdown_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%swdown_ij_clim)

CASE ( 'diurnal_tl1_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%diurnal_tl1_ij_clim)

CASE ( 'precip_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%precip_ij_clim)

CASE ( 'pstar_ij_clim_imgn' )
  cube = cube_from_array(imgn_drive%pstar_ij_clim)

CASE ( 'tl1_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%tl1_ij_patt)

CASE ( 'ql1_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%ql1_ij_patt)

CASE ( 'wind_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%wind_ij_patt)

CASE ( 'lwdown_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%lwdown_ij_patt)

CASE ( 'swdown_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%swdown_ij_patt)

CASE ( 'diurnal_tl1_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%diurnal_tl1_ij_patt)

CASE ( 'precip_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%precip_ij_patt)

CASE ( 'pstar_ij_patt_imgn' )
  cube = cube_from_array(imgn_drive%pstar_ij_patt)

CASE ( 'tl1_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%tl1_ij_anom)

CASE ( 'ql1_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%ql1_ij_anom)

CASE ( 'wind_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%wind_ij_anom)

CASE ( 'lwdown_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%lwdown_ij_anom)

CASE ( 'swdown_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%swdown_ij_anom)

CASE ( 'diurnal_tl1_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%diurnal_tl1_ij_anom)

CASE ( 'precip_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%precip_ij_anom)

CASE ( 'pstar_ij_anom_imgn' )
  cube = cube_from_array(imgn_drive%pstar_ij_anom)

CASE ( 'u1' )
  cube = cube_from_array(u_1_ij)

CASE ( 'u10m' )
  cube = cube_from_array(sf_diag%u10m)

CASE ( 'v1' )
  cube = cube_from_array(v_1_ij)

CASE ( 'v10m' )
  cube = cube_from_array(sf_diag%v10m)

CASE ( 'wind' )
  cube = cube_from_array(SQRT(u_1_ij**2 + v_1_ij**2))

CASE ( 'dt_range' )
  cube = cube_from_array(forcing%diurnal_temperature_range_ij)

CASE ( 'ext' )
  IF ( nsoilt == 1 ) THEN
    cube_land = cube_from_array(fluxes%ext_soilt(:,1,:))
    cube      = map_from_land(cube_land)
  ELSE
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(fluxes%ext_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube      = map_from_land(cube_land)
  END IF

CASE ( 'ext_soilt' )
  cube_land = cube_from_array(fluxes%ext_soilt)
  cube = map_from_land(cube_land)

CASE ( 'smcl' )
  IF ( nsoilt == 1 ) THEN
    cube_land = cube_from_array(progs%smcl_soilt(:,1,:))
    cube      = map_from_land(cube_land)
  ELSE
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(progs%smcl_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube      = map_from_land(cube_land)
  END IF

CASE ( 'smcl_soilt' )
  cube_land = cube_from_array(progs%smcl_soilt)
  cube = map_from_land(cube_land)

CASE ( 'soil_wet' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%sthu_soilt(:,:,k),ainfo)  &
                          + soiltiles_to_gbm(psparms%sthf_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for soil_wet with " //         &
                   "with nsoilt>1: diagnostic not yet implemented.")
  END IF

CASE ( 'sthf' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%sthf_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sthf with nsoilt>1: " //   &
                   "calculation is unphysical. Use psparms%sthf_soilt instead.")
  END IF

CASE ( 'sthf_soilt' )
  cube_land = cube_from_array(psparms%sthf_soilt)
  cube = map_from_land(cube_land)

CASE ( 'sthu' )
  IF ( nsoilt == 1 ) THEN
    DO k = 1, sm_levels
      workspace_levs(:,k) = soiltiles_to_gbm(psparms%sthu_soilt(:,:,k),ainfo)
    END DO
    cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output GB mean value for sthu with nsoilt>1: " //   &
                   "calculation is unphysical. Use psparms%sthu_soilt instead.")
  END IF

CASE ( 'sthu_soilt' )
  cube_land = cube_from_array(psparms%sthu_soilt)
  cube = map_from_land(cube_land)

CASE ( 't_soil' )
  DO k = 1, sm_levels
    workspace_levs(:,k) = soiltiles_to_gbm(progs%t_soil_soilt(:,:,k),ainfo)
  END DO
  cube_land = cube_from_array(workspace_levs(:,1:sm_levels))
  cube = map_from_land(cube_land)

CASE ( 't_soil_soilt' )
  cube_land = cube_from_array(progs%t_soil_soilt)
  cube = map_from_land(cube_land)

CASE ( 'tsoil_deep' )
  cube_land = cube_from_array(progs%tsoil_deep_gb(:,1:ns_deep))
  cube = map_from_land(cube_land)

CASE ( 'alb_tile_1' )
  cube_land = cube_from_array(fluxes%alb_surft(:,:,1))
  cube = map_from_land(cube_land)

CASE ( 'alb_tile_2' )
  cube_land = cube_from_array(fluxes%alb_surft(:,:,2))
  cube = map_from_land(cube_land)

CASE ( 'alb_tile_3' )
  cube_land = cube_from_array(fluxes%alb_surft(:,:,3))
  cube = map_from_land(cube_land)

CASE ( 'alb_tile_4' )
  cube_land = cube_from_array(fluxes%alb_surft(:,:,4))
  cube = map_from_land(cube_land)

CASE ( 'anthrop_heat' )
  cube_land = cube_from_array(fluxes%anthrop_heat_surft)
  cube = map_from_land(cube_land)

CASE ( 'canopy' )
  cube_land = cube_from_array(progs%canopy_surft)
  cube = map_from_land(cube_land)

CASE ( 'catch' )
  cube_land = cube_from_array(psparms%catch_surft)
  cube = map_from_land(cube_land)

CASE ( 'ecan' )
  cube_land = cube_from_array(fluxes%ecan_surft)
  cube = map_from_land(cube_land)

CASE ( 'ei' )
  cube_land = cube_from_array(fluxes%ei_surft)
  cube = map_from_land(cube_land)

CASE ( 'emis' )
  cube_land = cube_from_array(fluxes%emis_surft)
  cube = map_from_land(cube_land)

CASE ( 'esoil' )
  cube_land = cube_from_array(fluxes%esoil_surft)
  cube = map_from_land(cube_land)

CASE ( 'fqw' )
  ! Note that fqw_surft does not (always) do this job!
  DO n = 1,nsurft
    DO j = 1,ainfo%surft_pts(n)
      l = ainfo%surft_index(j,n)
      workspace_surft(l,n) = fluxes%ecan_surft(l,n) + fluxes%ei_surft(l,n) +   &
                             fluxes%esoil_surft(l,n)
      ! Add lake evaporation
      IF ( n == lake )                                                         &
        workspace_surft(l,n) = workspace_surft(l,n) + fluxes%fqw_surft(l,n)
    END DO
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'ftl' )
  cube_land = cube_from_array(fluxes%ftl_surft)
  cube = map_from_land(cube_land)

CASE ( 'tau' )
  cube_land = cube_from_array(sf_diag%tau_surft)
  cube = map_from_land(cube_land)

CASE ( 'gc' )
  cube_land = cube_from_array(progs%gc_surft)
  cube = map_from_land(cube_land)

CASE ( 'le' )
  cube_land = cube_from_array(fluxes%le_surft)
  cube = map_from_land(cube_land)

CASE ( 'nsnow' )
  cube_land = cube_from_array(REAL(progs%nsnow_surft))
  cube = map_from_land(cube_land)

CASE ( 'q1p5m' )
  cube_land = cube_from_array(sf_diag%q1p5m_surft)
  cube = map_from_land(cube_land)

CASE ( 'rad_net_tile' )
  cube_land = cube_from_array(fluxes%radnet_surft)
  cube = map_from_land(cube_land)

CASE ( 'sw_surft' )
  cube_land = cube_from_array(fluxes%sw_surft)
  cube = map_from_land(cube_land)

CASE ( 'lw_up_surft' )
  cube_land = cube_from_array(sf_diag%lw_up_surft)
  cube = map_from_land(cube_land)

CASE ( 'lw_down_surft' )
  cube_land = cube_from_array(sf_diag%lw_down_surft)
  cube = map_from_land(cube_land)

CASE ( 'rgrain' )
  cube_land = cube_from_array(progs%rgrain_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_can_melt' )
  ! Only include tiles where canopy snow model is used
  DO i = 1,nsurft
    IF ( canSnowTile(i) ) workspace_surft(:,i) = fluxes%melt_surft(:,i)
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_can' )
  ! Only include tiles where canopy snow model is used
  DO i = 1,nsurft
    IF ( canSnowTile(i) ) workspace_surft(:,i) = progs%snow_surft(:,i)
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_depth' )
  cube_land = cube_from_array(progs%snowdepth_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_grnd_rho' )
  cube_land = cube_from_array(progs%rho_snow_grnd_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_grnd' )
  ! Only include tiles where canopy snow model is used
  DO i = 1,nsurft
    IF ( canSnowTile(i) ) workspace_surft(:,i) = progs%snow_grnd_surft(:,i)
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_ground' )
  DO i = 1,nsurft
    IF ( canSnowTile(i) ) THEN
      workspace_surft(:,i) = progs%snow_grnd_surft(:,i)
    ELSE
      workspace_surft(:,i) = progs%snow_surft(:,i)
    END IF
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_ice_tile' )
  DO n = 1,nsurft
    DO j = 1,ainfo%surft_pts(n)
      i = ainfo%surft_index(j,n)
      workspace_surft(i,n) = SUM(progs%sice_surft(i,n,1:progs%nsnow_surft(i,n)))
    END DO
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_liq_tile' )
  DO n = 1,nsurft
    DO j = 1,ainfo%surft_pts(n)
      i = ainfo%surft_index(j,n)
      workspace_surft(i,n) = SUM(progs%sliq_surft(i,n,1:progs%nsnow_surft(i,n)))
    END DO
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_mass' )
  workspace_surft(:,:) = progs%snow_surft(:,:)
  ! Add snow below canopy
  DO n = 1,nsurft
    IF ( canSnowTile(n) )                                                      &
      workspace_surft(:,n) = workspace_surft(:,n) + progs%snow_grnd_surft(:,n)
  END DO
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_melt' )
  workspace_surft(:,:) = fluxes%melt_surft(:,:)
  ! Add melting of snow below canopy.
  !     IF ( can_model == 4 )
  !workspace_surft(:,:) = workspace_surft(:,:) + snowGMeltDiag(:,:)
  cube_land = cube_from_array(workspace_surft)
  cube = map_from_land(cube_land)

CASE ( 'surf_ht_flux' )
  cube_land = cube_from_array(fluxes%surf_htf_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_soil_htf' )
  cube_land = cube_from_array(fluxes%snow_soil_htf)
  cube = map_from_land(cube_land)

CASE ( 'snice_smb_surft' )
  cube_land = cube_from_array(sf_diag%snice_smb_surft)
  cube = map_from_land(cube_land)

CASE ( 'snice_m_surft' )
  cube_land = cube_from_array(sf_diag%snice_m_surft)
  cube = map_from_land(cube_land)

CASE ( 'snice_freez_surft' )
  cube_land = cube_from_array(sf_diag%snice_freez_surft)
  cube = map_from_land(cube_land)

CASE ( 'snice_sicerate_surft' )
  cube_land = cube_from_array(sf_diag%snice_sicerate_surft)
  cube = map_from_land(cube_land)

CASE ( 'snice_sliqrate_surft' )
  cube_land = cube_from_array(sf_diag%snice_sliqrate_surft)
  cube = map_from_land(cube_land)

CASE ( 'snice_runoff_surft' )
  cube_land = cube_from_array(sf_diag%snice_runoff_surft)
  cube = map_from_land(cube_land)

CASE ( 'surf_ht_store' )
  cube_land = cube_from_array(fluxes%surf_ht_store_surft)
  cube = map_from_land(cube_land)

CASE ( 't1p5m' )
  cube_land = cube_from_array(sf_diag%t1p5m_surft)
  cube = map_from_land(cube_land)

CASE ( 'tstar' )
  cube_land = cube_from_array(progs%tstar_surft)
  cube = map_from_land(cube_land)

CASE ( 'tsurf_elev_surft' )
  cube_land = cube_from_array(progs%tsurf_elev_surft)
  cube = map_from_land(cube_land)

CASE ( 'z0' )
  IF ( ANY(l_vegdrag_surft) ) THEN
    cube_land = cube_from_array(fluxes%z0m_surft)
  ELSE
    cube_land = cube_from_array(psparms%z0_surft)
  END IF
  cube = map_from_land(cube_land)

CASE ( 'z0h' )
  !     This diagnostic is set from psparms%z0_surft unless separately
  !     aggregated.
  IF ( l_aggregate .AND. (i_aggregate_opt == 1) ) THEN
    cube_land = cube_from_array(psparms%z0h_bare_surft)
  ELSE
    IF ( ANY(l_vegdrag_surft) ) THEN
      cube_land = cube_from_array(fluxes%z0h_surft)
    ELSE
      DO n = 1,nsurft
        workspace_surft(:,n) =  z0h_z0m(n) * psparms%z0_surft(:,n)
      END DO
      cube_land = cube_from_array(workspace_surft)
    END IF
  END IF
  cube = map_from_land(cube_land)

CASE ( 'tile_index' )
  cube_land = cube_from_array(REAL(ainfo%surft_index))
  cube = map_from_land(cube_land)

CASE ( 'isoprene_gb' )
  cube_land = cube_from_array(chemvars%isoprene_gb)
  cube = map_from_land(cube_land)

CASE ( 'isoprene' )
  cube_land = cube_from_array(chemvars%isoprene_pft)
  cube = map_from_land(cube_land)

CASE ( 'terpene_gb' )
  cube_land = cube_from_array(chemvars%terpene_gb)
  cube = map_from_land(cube_land)

CASE ( 'terpene' )
  cube_land = cube_from_array(chemvars%terpene_pft)
  cube = map_from_land(cube_land)

CASE ( 'methanol_gb' )
  cube_land = cube_from_array(chemvars%methanol_gb)
  cube = map_from_land(cube_land)

CASE ( 'methanol' )
  cube_land = cube_from_array(chemvars%methanol_pft)
  cube = map_from_land(cube_land)

CASE ( 'acetone_gb' )
  cube_land = cube_from_array(chemvars%acetone_gb)
  cube = map_from_land(cube_land)

CASE ( 'acetone' )
  cube_land = cube_from_array(chemvars%acetone_pft)
  cube = map_from_land(cube_land)

CASE ( 'croprootc' )
  cube_land = cube_from_array(crop_vars%rootc_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropharvc' )
  cube_land = cube_from_array(crop_vars%harvc_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropreservec' )
  cube_land = cube_from_array(crop_vars%reservec_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropdvi' )
  cube_land = cube_from_array(crop_vars%dvi_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropyield' )
  cube_land = cube_from_array(crop_vars%yield_diag_cpft)
  cube = map_from_land(cube_land)

CASE ( 'harvest_trigger' )
  workspace_cpft(:,:) = REAL(crop_vars%harvest_trigger_cpft(:,:))
  cube_land = cube_from_array(workspace_cpft)
  cube = map_from_land(cube_land)

CASE ( 'harvest_counter' )
  workspace_cpft(:,:) = REAL(crop_vars%harvest_counter_cpft(:,:))
  cube_land = cube_from_array(workspace_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropstemc' )
  cube_land = cube_from_array(crop_vars%stemc_diag_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropleafc' )
  cube_land = cube_from_array(crop_vars%leafc_diag_cpft)
  cube = map_from_land(cube_land)

CASE ( 'croplai' )
  cube_land = cube_from_array(crop_vars%croplai_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropcanht' )
  cube_land = cube_from_array(crop_vars%cropcanht_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropsowdate' )
  cube_land = cube_from_array(crop_vars%sow_date_cpft)
  cube = map_from_land(cube_land)

CASE ( 'croplatestharvdate' )
  cube_land = cube_from_array(crop_vars%latestharv_date_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropttveg' )
  cube_land = cube_from_array(crop_vars%tt_veg_cpft)
  cube = map_from_land(cube_land)

CASE ( 'cropttrep' )
  cube_land = cube_from_array(crop_vars%tt_rep_cpft)
  cube = map_from_land(cube_land)

CASE ( 'frac_irrig' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(crop_vars%frac_irr_soilt(:,1))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output frac_irrig when nsoilt > 1. " //             &
                   "Use frac_irrig_soilt instead.")
  END IF

CASE ( 'frac_irrig_soilt' )
  cube_land = cube_from_array(crop_vars%frac_irr_soilt)
  cube = map_from_land(cube_land)

CASE ( 'irrfrac_irrtiles' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(crop_vars%irrfrac_irrtiles(:,1))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
         "Cannot output irrfrac_irrtiles when nsoilt > 1.")
  END IF

CASE ( 'sthu_irr' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(crop_vars%sthu_irr_soilt(:,1,:))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output sthu_irr when nsoilt > 1. " //               &
                   "Use sthu_irr_soilt instead")
  END IF

CASE ( 'sthu_irr_soilt' )
  cube_land = cube_from_array(crop_vars%sthu_irr_soilt)
  cube = map_from_land(cube_land)

CASE ( 'irrDaysDiag' )
  cube_land = cube_from_array(crop_vars%irrDaysDiag_gb)
  cube = map_from_land(cube_land)

CASE ( 'irrig_water' )
  cube_land = cube_from_array(crop_vars%irrig_water_gb)
  cube = map_from_land(cube_land)

CASE ( 'rgrainl' )
  cube_land = cube_from_array(progs%rgrainl_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_ds' )
  cube_land = cube_from_array(progs%ds_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_ice' )
  cube_land = cube_from_array(progs%sice_surft)
  cube = map_from_land(cube_land)

CASE ( 'snow_liq' )
  cube_land = cube_from_array(progs%sliq_surft)
  cube = map_from_land(cube_land)

CASE ( 'tsnow' )
  cube_land = cube_from_array(progs%tsnow_surft)
  cube = map_from_land(cube_land)

CASE ( 'rflow' )
  cube_land = cube_from_array(fluxes%rflow_gb)
  cube = map_from_land(cube_land)

CASE ( 'rrun' )
  cube_land = cube_from_array(fluxes%rrun_gb)
  cube = map_from_land(cube_land)

CASE ( 'rflow_rp' )
  cube = cube_from_array(rivers%rflow_rp)

CASE ( 'rrun_rp' )
  cube = cube_from_array(rivers%rrun_rp)

CASE ( 'rrun_surf_rp' )
  cube = cube_from_array(rivers%rrun_surf_rp)

CASE ( 'rrun_sub_surf_rp' )
  cube = cube_from_array(rivers%rrun_sub_surf_rp)

CASE ( 'grid_area_rp' )
  cube = cube_from_array(rivers%rivers_boxareas_rp)

CASE ( 'rivers_sto_rp' )
  cube = cube_from_array(rivers%rivers_sto_rp)

CASE ( 'rfm_bflowin_rp' )
  ! Internal units are m<3/timestep, convert to m3/s
  workspace_rivers = rivers%rfm_bflowin_rp * nstep_rivers * timestep
  cube = cube_from_array(workspace_rivers)

CASE ( 'rfm_flowin_rp' )
  ! Internal units are m3/timestep, convert to m3/s
  workspace_rivers = rivers%rfm_flowin_rp * nstep_rivers * timestep
  cube = cube_from_array(workspace_rivers)

CASE ( 'rfm_substore_rp' )
  cube = cube_from_array(rivers%rfm_substore_rp)

CASE ( 'rfm_surfstore_rp' )
  cube = cube_from_array(rivers%rfm_surfstore_rp)

CASE ( 'rivers_outflow_rp' )
  cube = cube_from_array(rivers%rivers_outflow_rp)

CASE ( 'river_elevation' )
  cube = cube_from_array(rivers%river_elevation)

CASE ( 'channel_bed_elevation' )
  ! Calculate bed elevation.
  workspace_rivers = rivers%river_elevation - rivers%channel_depth
  cube = cube_from_array(workspace_rivers)

CASE ( 'channel_depth' )
  cube = cube_from_array(rivers%channel_depth)

CASE ( 'channel_width' )
  cube = cube_from_array(rivers%channel_width)

CASE ( 'flood_flow' )
  cube = cube_from_array(rivers%flood_flow)

CASE ( 'flood_storage' )
  cube = cube_from_array(rivers%flood_storage)

CASE ( 'river_channel_flow' )
  cube = cube_from_array(rivers%river_channel_flow)

CASE ( 'river_channel_storage' )
  cube = cube_from_array(rivers%river_channel_storage)

CASE ( 'river_depth' )
  cube = cube_from_array(rivers%river_depth)

CASE ( 'outflow_per_river' )
  ! Using workspace rivers (initialised to mdi) although it is of size
  ! n_rivers < np_rivers. This allows it to be written to the same output file.
  IF ( is_master_task() ) THEN
    workspace_rivers(1:n_rivers) = calc_outflow_per_river(rivers)
  END IF
  cube = cube_from_array(workspace_rivers)

CASE ( 'frac_fplain_rp' )
  cube = cube_from_array(frac_fplain_rp)

CASE ( 'frac_fplain_lp' )
  cube_land = cube_from_array(frac_fplain_lp)
  cube = map_from_land(cube_land)

CASE ( 'minor_res_capacity' )
  cube = cube_from_array(rivers%minor_res_capacity)

CASE ( 'minor_res_frac' )
  cube = cube_from_array(rivers%minor_res_frac)

CASE ( 'minor_res_storage' )
  cube = cube_from_array(rivers%minor_res_storage)

CASE ( 'lake_depth' )
  cube_land = cube_from_array(lake_vars%lake_depth_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_fetch_gb' )
  cube_land = cube_from_array(lake_vars%lake_fetch_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_t_mean_gb' )
  cube_land = cube_from_array(lake_vars%lake_t_mean_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_t_mxl_gb' )
  cube_land = cube_from_array(lake_vars%lake_t_mxl_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_t_ice_gb' )
  cube_land = cube_from_array(lake_vars%lake_t_ice_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_h_mxl_gb' )
  cube_land = cube_from_array(lake_vars%lake_h_mxl_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_h_ice_gb' )
  cube_land = cube_from_array(lake_vars%lake_h_ice_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_shape_factor_gb' )
  cube_land = cube_from_array(lake_vars%lake_shape_factor_gb)
  cube = map_from_land(cube_land)

CASE ( 'g_dt_gb' )
  cube_land = cube_from_array(lake_vars%g_dt_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_t_sfc_gb' )
  cube_land = cube_from_array(lake_vars%lake_t_sfc_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_t_snow_gb' )
  cube_land = cube_from_array(lake_vars%lake_t_snow_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_h_snow_gb' )
  cube_land = cube_from_array(lake_vars%lake_h_snow_gb)
  cube = map_from_land(cube_land)

CASE ( 'lake_albedo_gb' )
  cube_land = cube_from_array(lake_vars%lake_albedo_gb)
  cube = map_from_land(cube_land)

  !-----------------------------------------------------------------------------
  ! Equivalent neutral winds
  !-----------------------------------------------------------------------------
CASE ( 'u10m_n' )
  cube = cube_from_array(sf_diag%u10m_n)

CASE ( 'v10m_n' )
  cube = cube_from_array(sf_diag%v10m_n)

CASE ( 'mu10m_n' )
  cube = cube_from_array(sf_diag%mu10m_n)

CASE ( 'mv10m_n' )
  cube = cube_from_array(sf_diag%mv10m_n)

  ! Fire module variables
CASE ('fire_mcarthur')
  cube_land = cube_from_array(fire_diag(:)%mcarthur%ffdi)
  cube      = map_from_land(cube_land)

CASE ('fire_canadian_ffmc')
  cube_land = cube_from_array(fire_prog(:)%canadian%ffmc)
  cube      = map_from_land(cube_land)

CASE ('fire_canadian_dmc')
  cube_land = cube_from_array(fire_prog(:)%canadian%dmc)
  cube      = map_from_land(cube_land)

CASE ('fire_canadian_dc')
  cube_land = cube_from_array(fire_prog(:)%canadian%dc)
  cube      = map_from_land(cube_land)

CASE ('fire_canadian_isi')
  cube_land = cube_from_array(fire_diag(:)%canadian%isi)
  cube      = map_from_land(cube_land)

CASE ('fire_canadian_bui')
  cube_land = cube_from_array(fire_diag(:)%canadian%bui)
  cube      = map_from_land(cube_land)

CASE ('fire_canadian')
  cube_land = cube_from_array(fire_diag(:)%canadian%fwi)
  cube      = map_from_land(cube_land)

CASE ('fire_nesterov')
  cube_land = cube_from_array(fire_prog(:)%nesterov%findex)
  cube      = map_from_land(cube_land)

  ! INFERNO variables
CASE ( 'flammability' )
  cube_land = cube_from_array(fire_vars%flammability_ft)
  cube = map_from_land(cube_land)

CASE ( 'burnt_area_gb' )
  cube_land = cube_from_array(fire_vars%burnt_area)
  cube = map_from_land(cube_land)

CASE ( 'burnt_area' )
  cube_land = cube_from_array(fire_vars%burnt_area_ft)
  cube = map_from_land(cube_land)

CASE ( 'emitted_carbon_gb' )
  cube_land = cube_from_array(fire_vars%emitted_carbon)
  cube = map_from_land(cube_land)
CASE ( 'emitted_carbon' )
  cube_land = cube_from_array(fire_vars%emitted_carbon_ft)
  cube = map_from_land(cube_land)
CASE ( 'emitted_carbon_DPM' )
  cube_land = cube_from_array(fire_vars%emitted_carbon_DPM)
  cube = map_from_land(cube_land)
CASE ( 'emitted_carbon_RPM' )
  cube_land = cube_from_array(fire_vars%emitted_carbon_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_CO2_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_CO2)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CO2' )
  cube_land = cube_from_array(fire_vars%fire_em_CO2_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CO2_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_CO2_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CO2_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_CO2_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_CO_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_CO)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CO' )
  cube_land = cube_from_array(fire_vars%fire_em_CO_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CO_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_CO_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CO_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_CO_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_CH4_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_CH4)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CH4' )
  cube_land = cube_from_array(fire_vars%fire_em_CH4_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CH4_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_CH4_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_CH4_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_CH4_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_NOx_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_NOx)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_NOx' )
  cube_land = cube_from_array(fire_vars%fire_em_NOx_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_NOx_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_NOx_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_NOx_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_NOx_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_SO2_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_SO2)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_SO2' )
  cube_land = cube_from_array(fire_vars%fire_em_SO2_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_SO2_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_SO2_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_SO2_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_SO2_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_OC_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_OC)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_OC' )
  cube_land = cube_from_array(fire_vars%fire_em_OC_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_OC_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_OC_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_OC_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_OC_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_BC_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_BC)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_BC' )
  cube_land = cube_from_array(fire_vars%fire_em_BC_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_BC_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_BC_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_BC_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_BC_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_C2H4_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H4)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C2H4' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H4_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C2H4_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H4_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C2H4_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H4_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_C2H6_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H6)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C2H6' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H6_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C2H6_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H6_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C2H6_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_C2H6_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_C3H8_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_C3H8)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C3H8' )
  cube_land = cube_from_array(fire_vars%fire_em_C3H8_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C3H8_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_C3H8_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_C3H8_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_C3H8_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_HCHO_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_HCHO)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_HCHO' )
  cube_land = cube_from_array(fire_vars%fire_em_HCHO_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_HCHO_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_HCHO_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_HCHO_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_HCHO_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_MeCHO_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_MeCHO)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_MeCHO' )
  cube_land = cube_from_array(fire_vars%fire_em_MeCHO_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_MeCHO_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_MeCHO_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_MeCHO_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_MeCHO_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_NH3_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_NH3)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_NH3' )
  cube_land = cube_from_array(fire_vars%fire_em_NH3_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_NH3_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_NH3_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_NH3_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_NH3_RPM)
  cube = map_from_land(cube_land)

CASE ( 'fire_em_DMS_gb' )
  cube_land = cube_from_array(fire_vars%fire_em_DMS)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_DMS' )
  cube_land = cube_from_array(fire_vars%fire_em_DMS_ft)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_DMS_DPM' )
  cube_land = cube_from_array(fire_vars%fire_em_DMS_DPM)
  cube = map_from_land(cube_land)
CASE ( 'fire_em_DMS_RPM' )
  cube_land = cube_from_array(fire_vars%fire_em_DMS_RPM)
  cube = map_from_land(cube_land)

  !-----------------------------------------------------------------------------
  ! Variables available with any multi-pool soil C model (e.g. 4-pool, ECOSSE).
  !-----------------------------------------------------------------------------
  ! Soil prognostic variables and single pools from multi-pool prognostic
  ! variables. These are all coded for a single soil tile only.
  !-----------------------------------------------------------------------------
CASE ( 'c_bio' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(progs%cs_pool_soilt(:,1,:,3))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_bio when nsoilt > 1.")
  END IF

CASE ( 'c_dpm' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(progs%cs_pool_soilt(:,1,:,1))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_dpm when nsoilt > 1.")
  END IF

CASE ( 'c_hum' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(progs%cs_pool_soilt(:,1,:,4))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_hum when nsoilt > 1.")
  END IF

CASE ( 'c_rpm' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(progs%cs_pool_soilt(:,1,:,2))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_rpm when nsoilt > 1.")
  END IF

CASE ( 'n_bio' )
  cube_land = cube_from_array(progs%ns_pool_gb(:,:,3))
  cube = map_from_land(cube_land)

CASE ( 'n_dpm' )
  cube_land = cube_from_array(progs%ns_pool_gb(:,:,1))
  cube = map_from_land(cube_land)

CASE ( 'n_hum' )
  cube_land = cube_from_array(progs%ns_pool_gb(:,:,4))
  cube = map_from_land(cube_land)

CASE ( 'n_rpm' )
  cube_land = cube_from_array(progs%ns_pool_gb(:,:,2))
  cube = map_from_land(cube_land)

  ! Gridbox totals of multi-pool soil prognostic variables.
  ! All are coded for a single soil tile only.
CASE ( 'c_bio_gb' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array( SUM(progs%cs_pool_soilt(:,1,:,3),2) )
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_bio_gb when nsoilt > 1.")
  END IF

CASE ( 'c_dpm_gb' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array( SUM(progs%cs_pool_soilt(:,1,:,1),2) )
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_dpm_gb when nsoilt > 1.")
  END IF

CASE ( 'c_hum_gb' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array( SUM(progs%cs_pool_soilt(:,1,:,4),2) )
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_hum_gb when nsoilt > 1.")
  END IF

CASE ( 'c_rpm_gb' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array( SUM(progs%cs_pool_soilt(:,1,:,2),2) )
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output c_rpm_gb when nsoilt > 1.")
  END IF

CASE ( 'n_bio_gb' )
  cube_land = cube_from_array( SUM(progs%ns_pool_gb(:,:,3),2) )
  cube = map_from_land(cube_land)

CASE ( 'n_dpm_gb' )
  cube_land = cube_from_array( SUM(progs%ns_pool_gb(:,:,1),2) )
  cube = map_from_land(cube_land)

CASE ( 'n_hum_gb' )
  cube_land = cube_from_array( SUM(progs%ns_pool_gb(:,:,4),2) )
  cube = map_from_land(cube_land)

CASE ( 'n_rpm_gb' )
  cube_land = cube_from_array( SUM(progs%ns_pool_gb(:,:,2),2) )
  cube = map_from_land(cube_land)

  ! Gridbox totals of derived variables for multi-pool soil models.
CASE ( 'n_soil_gb' )
  ! Coded for a single soil tile only.
  IF (nsoilt == 1) THEN
    IF ( soil_bgc_model == soil_model_4pool ) THEN
      ! Sum organic pools.
      workspace_land(:) = SUM( SUM(progs%ns_pool_gb(:,:,:),3), 2 )
      ! Add inorganic pools.
      workspace_land(:) = workspace_land(:) +                                  &
                          SUM( progs%n_inorg_soilt_lyrs(:,1,:), 2 )
    ELSE IF ( soil_bgc_model == soil_model_ecosse ) THEN
      ! Sum over all pools and levels.
      workspace_land(:) = SUM( SUM(                                            &
                        soilecosse%n_soil_pool_soilt(:,1,:,:), 3), 2 )
    END IF
    ! Map the result onto the full grid.
    cube_land = cube_from_array(workspace_land)
    cube      = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output n_soil_gb when nsoilt > 1.")
  END IF

  ! ECOSSE prognostic variables (and components).
  ! All are coded for a single soil tile only.
CASE ( 'n_soil' )
  IF ( nsoilt == 1) THEN
    cube_land = cube_from_array(soilecosse%n_soil_pool_soilt(:,1,:,:))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output n_soil when nsoilt > 1")
  END IF

CASE ( 'n_amm' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(soilecosse%n_soil_pool_soilt(:,1,:,i_amm))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output n_amm when nsoilt > 1.")
  END IF

CASE ( 'n_nit' )
  IF (nsoilt == 1) THEN
    cube_land = cube_from_array(soilecosse%n_soil_pool_soilt(:,1,:,i_nit))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output n_nit when nsoilt > 1.")
  END IF

  ! Gridbox totals of components of ECOSSE prognostic variables.
CASE ( 'n_amm_gb' )
  IF (nsoilt == 1) THEN
    ! Sum over levels.
    cube_land = cube_from_array( SUM(                                          &
                                soilecosse%n_soil_pool_soilt(:,1,:,i_amm), 2 ))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output n_amm_gb when nsoilt > 1.")
  END IF

CASE ( 'n_nit_gb' )
  IF (nsoilt == 1) THEN
    ! Sum over levels.
    cube_land = cube_from_array( SUM(                                          &
                                soilecosse%n_soil_pool_soilt(:,1,:,i_nit), 2 ))
    cube = map_from_land(cube_land)
  ELSE
    CALL log_fatal("extract_var",                                              &
                   "Cannot output n_nit_gb when nsoilt > 1.")
  END IF

  !   ECOSSE fluxes.
CASE ( 'plant_input_c_gb' )
  cube_land = cube_from_array( soilecosse%plant_input_c_gb(:) )
  cube = map_from_land(cube_land)

CASE ( 'plant_input_n_gb' )
  cube_land = cube_from_array( soilecosse%plant_input_n_gb(:) )
  cube = map_from_land(cube_land)

  ! Imogen prognostics
CASE ( 'd_land_atmos_co2' )
  cube_land = cube_from_array(imgn_vars%d_land_atmos_co2)
  cube = map_from_land(cube_land)

CASE ( 'd_ocean_atmos' )
  cube_land = cube_from_array(imgn_vars%d_ocean_atmos)
  cube = map_from_land(cube_land)

CASE ( 'imogen_radf' )
  cube_land = cube_from_array(imgn_vars%imogen_radf)
  cube = map_from_land(cube_land)

CASE ( 'dtemp_g' )
  cube_land = cube_from_array(imgn_vars%dtemp_g)
  cube = map_from_land(cube_land)

CASE ( 'c_emiss_data' )
  cube_land = cube_from_array(imgn_vars%c_emiss_data)
  cube = map_from_land(cube_land)

CASE ( 'dctot_co2' )
  cube_land = cube_from_array(imgn_vars%dctot_co2)
  cube = map_from_land(cube_land)

CASE ( 'dctot_ch4' )
  cube_land = cube_from_array(imgn_vars%dctot_ch4)
  cube = map_from_land(cube_land)
  ! Imogen end

CASE ( 'n_denitrif_gb' )
  cube_land = cube_from_array( soilecosse%n_denitrification_gb )
  cube = map_from_land(cube_land)

CASE ( 'n_nitrif_gb' )
  cube_land = cube_from_array( soilecosse%n_nitrification_gb )
  cube = map_from_land(cube_land)

CASE ( 'no_soil_gb' )
  cube_land = cube_from_array( soilecosse%no_soil_gb )
  cube = map_from_land(cube_land)

CASE ( 'n2_denitrif_gb' )
  cube_land = cube_from_array( soilecosse%n2_denitrif_gb )
  cube = map_from_land(cube_land)

CASE ( 'n2o_denitrif_gb' )
  cube_land = cube_from_array( soilecosse%n2o_denitrif_gb )
  cube = map_from_land(cube_land)

CASE ( 'n2o_nitrif_gb' )
  cube_land = cube_from_array( soilecosse%n2o_nitrif_gb )
  cube = map_from_land(cube_land)

CASE ( 'n2o_part_nitrif_gb' )
  cube_land = cube_from_array( soilecosse%n2o_partial_nitrif_gb )
  cube = map_from_land(cube_land)

CASE ( 'n2o_soil_gb' )
  cube_land = cube_from_array( soilecosse%n2o_soil_gb )
  cube = map_from_land(cube_land)

  ! ECOSSE leaching diagnostics.
CASE ( 'n_leach_amm_gb' )
  cube_land = cube_from_array( soilecosse%n_leach_amm_gb )
  cube = map_from_land(cube_land)

CASE ( 'n_leach_nit_gb' )
  cube_land = cube_from_array( soilecosse%n_leach_nit_gb )
  cube = map_from_land(cube_land)

CASE ( 't_home_gb' )
  cube_land = cube_from_array( progs%t_home_gb )
  cube = map_from_land(cube_land)

CASE ( 't_growth_gb' )
  cube_land = cube_from_array( progs%t_growth_gb )
  cube = map_from_land(cube_land)

  ! Water resource variables.
CASE ( 'conv_loss_frac' )
  cube_land = cube_from_array( water_resources%conv_loss_frac )
  cube = map_from_land(cube_land)

CASE ( 'demand_rate_domestic' )
  cube_land = cube_from_array(water_resources%demand_rate_domestic)
  cube = map_from_land(cube_land)

CASE ( 'demand_rate_industry' )
  cube_land = cube_from_array(water_resources%demand_rate_industry)
  cube = map_from_land(cube_land)

CASE ( 'demand_rate_livestock' )
  cube_land = cube_from_array(water_resources%demand_rate_livestock)
  cube = map_from_land(cube_land)

CASE ( 'demand_rate_transfers' )
  cube_land = cube_from_array(water_resources%demand_rate_transfers)
  cube = map_from_land(cube_land)

CASE ( 'demand_domestic' )
  ! Note for use of demand_accum: as this is accumulated over the water
  ! resource timestep generally it is not appropriate to use it on
  ! intermediate timesteps - hence we return zero for those.
  IF ( water_res_count == 0 ) THEN
    ! Convert kg to kg s-1.
    cube_land = cube_from_array(                                               &
                  water_resources%demand_accum(:,use_domestic) / timestep )
  ELSE
    workspace_land(:) = 0.0
    cube_land = cube_from_array( workspace_land )
  END IF
  cube = map_from_land( cube_land )

CASE ( 'demand_environment' )
  IF ( water_res_count == 0 ) THEN
    ! Convert kg to kg s-1.
    cube_land = cube_from_array(                                               &
                  water_resources%demand_accum(:,use_environment) / timestep )
  ELSE
    workspace_land(:) = 0.0
    cube_land = cube_from_array( workspace_land )
  END IF
  cube = map_from_land(cube_land)

CASE ( 'demand_industry' )
  IF ( water_res_count == 0 ) THEN
    ! Convert kg to kg s-1.
    cube_land = cube_from_array( water_resources%demand_accum(:,use_industry)  &
                                 / timestep )
  ELSE
    workspace_land(:) = 0.0
    cube_land = cube_from_array( workspace_land )
  END IF
  cube = map_from_land(cube_land)

CASE ( 'demand_irrigation' )
  IF ( water_res_count == 0 ) THEN
    ! Convert kg to kg s-1.
    cube_land = cube_from_array( water_resources%demand_accum(:,use_irrigation)&
                                 / timestep )
  ELSE
    workspace_land(:) = 0.0
    cube_land = cube_from_array( workspace_land )
  END IF
  cube = map_from_land(cube_land)

CASE ( 'demand_livestock' )
  IF ( water_res_count == 0 ) THEN
    ! Convert kg to kg s-1.
    cube_land = cube_from_array( water_resources%demand_accum(:,use_livestock) &
                                 / timestep )
  ELSE
    workspace_land(:) = 0.0
    cube_land = cube_from_array( workspace_land )
  END IF
  cube = map_from_land(cube_land)

CASE ( 'demand_transfers' )
  IF ( water_res_count == 0 ) THEN
    ! Convert kg to kg s-1.
    cube_land = cube_from_array( water_resources%demand_accum(:,use_transfers) &
                                 / timestep )
  ELSE
    workspace_land(:) = 0.0
    cube_land = cube_from_array( workspace_land )
  END IF
  cube = map_from_land(cube_land)

CASE ( 'grid_area' )
  cube = cube_from_array( grid_area_ij )

CASE ( 'gw_abstracted' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%gw_abstracted / timestep )
  cube = map_from_land(cube_land)

CASE ( 'gw_avail' )
  cube_land = cube_from_array( water_resources%gw_avail )
  cube = map_from_land(cube_land)

CASE ( 'gw_nr_abstracted' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%gw_nr_abstracted / timestep )
  cube = map_from_land(cube_land)

CASE ( 'minor_res_abstracted' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%abstracted_minor_res / timestep )
  cube = map_from_land(cube_land)

CASE ( 'sfc_water_frac' )
  cube_land = cube_from_array( water_resources%sfc_water_frac )
  cube = map_from_land(cube_land)

CASE ( 'sw_abstracted' )
  ! Sum over sources and convert kg to kg s-1.
  cube_land = cube_from_array( SUM(water_resources%sw_abstracted,2)            &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'sw_avail' )
  cube_land = cube_from_array( water_resources%sw_avail_total )
  cube = map_from_land(cube_land)

CASE ( 'unmet_domestic' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%demand_unmet(:,use_domestic)    &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'unmet_environment' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%demand_unmet(:,use_environment) &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'unmet_industry' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%demand_unmet(:,use_industry)    &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'unmet_irrigation' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%demand_unmet(:,use_irrigation)  &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'unmet_livestock' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%demand_unmet(:,use_livestock)   &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'unmet_transfers' )
  cube_land = cube_from_array( water_resources%demand_unmet(:,use_transfers)   &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'water_demand' )
  IF ( water_res_count == 0 ) THEN
    ! Sum over sectors and convert kg to kg s-1.
    cube_land = cube_from_array( SUM(water_resources%demand_accum,2)           &
                                 / timestep )
  ELSE
    workspace_land(:) = 0.0
    cube_land = cube_from_array( workspace_land )
  END IF
  cube = map_from_land(cube_land)

CASE ( 'water_demand_unmet' )
  ! Sum over sectors and convert kg to kg s-1.
  cube_land = cube_from_array( SUM(water_resources%demand_unmet,2)             &
                               / timestep )
  cube = map_from_land(cube_land)

CASE ( 'water_removed' )
  ! Convert kg to kg s-1.
  cube_land = cube_from_array( water_resources%water_removed / timestep )
  cube = map_from_land(cube_land)

  !---------------------------------------------------------------------------
  ! Urban morphology on land points
  ! These are calculated when l_urban_empirical otherwise they will be the
  ! same as the input value.
  ! wrr   Repeating width ratio (or canyon fraction, W/R)
  ! hwr   Height-to-width ratio (H/W)
  ! hgt   Building height (H)
  !---------------------------------------------------------------------------
CASE ( 'wrr' )
  cube_land = cube_from_array(urban_param%wrr_gb)
  cube = map_from_land(cube_land)

CASE ( 'hwr' )
  cube_land = cube_from_array(urban_param%hwr_gb)
  cube = map_from_land(cube_land)

CASE ( 'hgt' )
  cube_land = cube_from_array(urban_param%hgt_gb)
  cube = map_from_land(cube_land)

  ! Atmospheric deposition variables.
  ! Surface atmospheric concentrations of deposited species
CASE ( 'deposition_conc' )
  cube = cube_from_array( chemvars%dep_surfconc_ij )

  ! Aerodynamic resistance, Ra
CASE ( 'deposition_ra' )
  cube = cube_from_array( chemvars%dep_ra_ij )

  ! Quasi-laminar resistance, Rb
CASE ( 'deposition_rb' )
  cube = cube_from_array( chemvars%dep_rb_ij )

  ! Surface resistance, Rc
CASE ( 'deposition_rc' )
  cube = cube_from_array( chemvars%dep_rc_ij )

  ! Surface resistance, Rc - stomatal component
CASE ( 'deposition_rc_stom' )
  cube = cube_from_array( chemvars%dep_rc_stom_ij )

  ! Surface resistance, Rc - non-stomatal component
CASE ( 'deposition_rc_nonstom' )
  cube = cube_from_array( chemvars%dep_rc_nonstom_ij )

  ! Deposition velocity, vd
CASE ( 'deposition_vd' )
  cube = cube_from_array( chemvars%dep_vd_ij )

  ! Deposition velocity, vd - grid-box mean
CASE ( 'deposition_vd_gb' )
  cube = cube_from_array( chemvars%dep_vd_gb )

  ! Deposition loss rate
CASE ( 'deposition_lossrate' )
  cube = cube_from_array( chemvars%dep_loss_rate_ij )

  ! Deposition flux
CASE ( 'deposition_flux' )
  cube = cube_from_array( chemvars%dep_flux_ij )

  ! Boundary height
CASE ( 'bl_height' )
  cube = cube_from_array( jules_vars%zh )

  ! Atmospheric layer separation
CASE ( 'level_separation' )
  cube = cube_from_array( jules_vars%dzl )


CASE DEFAULT
  CALL log_fatal("extract_var",                                                &
                 "Unrecognised variable for output - '" //                     &
                 TRIM(get_string_identifier(var_id)) // "'. " //               &
                 "See docs for available variables")
END SELECT

!-----------------------------------------------------------------------------
! Free the land cube
!
! Note that is safe to call this routine even if the cube has not been
! allocated - it just won't do anything
!-----------------------------------------------------------------------------
CALL cube_free(cube_land)


RETURN

END FUNCTION extract_var
#endif
