#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE populate_var(var_id, cube, const_val)

USE jules_fields_mod, ONLY: crop_vars, psparms, toppdm, fire_vars, ainfo,      &
                            trif_vars, soilecosse, urban_param, progs,         &
                            trifctltype, jules_vars, fluxes, forcing,          &
                            lake_vars, chemvars, rivers, water_resources

  !Science variables
! TYPE Definitions
USE jules_fields_mod, ONLY: toppdm
USE imgn_drive_mod, ONLY: imgn_drive
USE imgn_vars_mod, ONLY: imgn_vars

USE ancil_info, ONLY:                                                          &
  dim_cslayer, land_pts, nsurft, nsoilt

USE model_grid_mod, ONLY:                                                      &
  grid_area_ij, latitude, longitude, projection_x_coord, projection_y_coord

USE coastal, ONLY:                                                             &
  flandg

USE prognostics, ONLY:                                                         &
  l_broadcast_soilt

USE jules_rivers_mod, ONLY:                                                    &
! 2D ancillary fields used during initialisation.
  channel_depth_grid, channel_width_grid, mean_sea_level_grid,                 &
  river_distance_grid, river_elevation_grid, river_length_grid,                &
  river_manning_grid, river_nextx_grid, river_nexty_grid

USE jules_soil_mod, ONLY:                                                      &
  sm_levels, l_tile_soil, l_broadcast_ancils

USE overbank_inundation_mod, ONLY:                                             &
  hypsometric_quantiles_grid, logn_mean, logn_stdev

USE aero, ONLY:                                                                &
  co2_mmr

USE fire_mod, ONLY:                                                            &
  fire_prog

USE jules_forcing_mod, ONLY: u_1_ij, v_1_ij

USE metstats_mod, ONLY:                                                        &
  metstats_prog

!Others
USE data_cube_mod, ONLY:                                                       &
  data_cube, cube_get_data, cube_free, cube_create

USE cube_interp_climatology_data_mod, ONLY:                                    &
  cube_interp_climatology_data

USE jules_surface_types_mod, ONLY: ncpft, nnpft, npft

USE cable_fields_mod, ONLY: progs_cbl_vars

IMPLICIT NONE

! Argument types
INTEGER, INTENT(IN) :: var_id  ! Identifies the variable to fill
TYPE(data_cube), INTENT(IN), OPTIONAL :: cube
                     ! The data to put in to the variable as a cube
REAL, INTENT(IN), OPTIONAL :: const_val
                     ! A constant value to fill all elements of the variable
                     ! with


! Work variables
TYPE(data_cube) :: cube_land  ! Workspace cube for land and river data
                              ! This is required so that it can be deallocated
                              ! to avoid memory leaks

REAL :: nsnow_real(land_pts, nsurft)  ! Real version of nsnow

REAL :: harvest_doy_real(land_pts, npft) ! Real version of harvest_doy

REAL :: years_since_harvest_real(land_pts, npft)
        ! Real version of years_since_harvest

REAL :: co2_tmp(land_pts)

REAL, ALLOCATABLE :: frac_cpft(:, :)

!-----------------------------------------------------------------------------


IF ( .NOT. PRESENT(cube) .AND. .NOT. PRESENT(const_val) )                      &
  CALL log_fatal("populate_var",                                               &
                 "Neither data cube nor const_val have been provided for " //  &
                 "variable '" // TRIM(get_string_identifier(var_id)) // "'")

IF ( PRESENT(cube) .AND. PRESENT(const_val) )                                  &
  CALL log_warn("populate_var",                                                &
                "data cube and const_val both provided for variable '" //      &
                TRIM(get_string_identifier(var_id)) //                         &
                "' - using data in preference")

! We use the string identifier to select a CASE from the SELECT statement to
! avoid being dependent on how integer variable ids are implemented
SELECT CASE ( get_string_identifier(var_id) )

CASE ( 'latitude' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, latitude)
  ELSE
    latitude(:,:) = const_val
  END IF

CASE ( 'longitude' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, longitude)
  ELSE
    longitude(:,:) = const_val
  END IF

CASE ( 'projection_x_coord' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, projection_x_coord)
  ELSE
    projection_x_coord(:,:) = const_val
  END IF

CASE ( 'projection_y_coord' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, projection_y_coord)
  ELSE
    projection_y_coord(:,:) = const_val
  END IF

CASE ( 'land_fraction' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, flandg)
  ELSE
    flandg(:,:) = const_val
  END IF

CASE ( 'surf_hgt_surft' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, jules_vars%surf_hgt_surft)
  ELSE
    jules_vars%surf_hgt_surft(:,:) = const_val
  END IF

CASE ( 'z_land_land' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, jules_vars%z_land_land)
  ELSE
    jules_vars%z_land_land(:) = const_val
  END IF

CASE ( 'frac' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, ainfo%frac_surft)
  ELSE
    ainfo%frac_surft(:,:) = const_val
  END IF

CASE ( 'cropfrac' )
  ALLOCATE(frac_cpft(land_pts, ncpft))
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, frac_cpft)
  ELSE
    frac_cpft(:,:) = const_val
  END IF
  ainfo%frac_surft(:,1 + nnpft:ncpft + nnpft) = frac_cpft(:,:)
  DEALLOCATE(frac_cpft)

  !-----------------------------------------------------------------------------
  ! Soil properties have a corresponding 0-level version for input of constant
  ! z values. albsoil_gb, psparms%albobs_sw_gb, etc have no levels anyway
  !
  ! Each variable can be read in with _soilt if the soil tile dimension is
  ! present. Where appropriate, this can be combined with _const_z as well.
  ! There are a few possible outcomes
  !
  ! No suffix:
  !  l_tile_soil = T and l_broadcast_ancils = T
  !  --Spread around soilt dim
  !  l_tile_soil = F
  !  --Do nothing
  !
  ! _const_z suffix:
  !  l_tile_soil = T and l_broadcast_ancils = T (and l_const_z = T)
  !  --Spread around soilt and layer dims
  !  l_tile_soil = F
  !  --Spread around layer dim
  !
  ! _soilt suffix:
  ! l_tile_soil = T and l_broadcast_ancils = F
  ! --Do nothing
  !
  ! _const_z_soilt:
  ! l_tile_soil = T and l_broadcast_ancils = F (and l_const_z = T)
  ! --Spread around layer dim
  !
  !-----------------------------------------------------------------------------

      !=== Start cases for albsoil ===
CASE ( 'albsoil' )  !no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%albsoil_soilt(:,1))
      psparms%albsoil_soilt(:,:) = SPREAD(psparms%albsoil_soilt(:,1), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%albsoil_soilt(:,1))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate albsoil- failure in logic")
    END IF
  ELSE
    psparms%albsoil_soilt(:,:) = const_val
  END IF

CASE ( 'albsoil_soilt' ) !with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%albsoil_soilt(:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate psparms%albsoil_soilt- failure in logic")
    END IF
  ELSE
    psparms%albsoil_soilt(:,:) = const_val
  END IF
  !=== End cases for albsoil ===

  !=== Start cases for clay ===
CASE ( 'clay' )  ! With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt dim.
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%clay_soilt(:,1,1:dim_cslayer))
      psparms%clay_soilt(:,:,:) = SPREAD(psparms%clay_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Do nothing special.
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%clay_soilt(:,1,1:dim_cslayer))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate clay - failure in logic")
    END IF
  ELSE
    psparms%clay_soilt(:,:,:) = const_val
  END IF

CASE ( 'clay_const_z' )  ! No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%clay_soilt(:,1,1))
      psparms%clay_soilt(:,1,1:dim_cslayer) =                                  &
                        SPREAD(psparms%clay_soilt(:,1,1), 2, dim_cslayer)
      psparms%clay_soilt(:,:,:) = SPREAD(psparms%clay_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%clay_soilt(:,1,1))
      psparms%clay_soilt(:,1,1:dim_cslayer) =                                  &
                        SPREAD(psparms%clay_soilt(:,1,1), 2, dim_cslayer)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate clay_const_z - failure in logic")
    END IF
  ELSE
    psparms%clay_soilt(:,:,:) = const_val
  END IF

CASE ( 'clay_soilt' )  ! With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       ! Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%clay_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate psparms%clay_soilt- failure in logic")
    END IF
  ELSE
    psparms%clay_soilt(:,:,:) = const_val
  END IF

CASE ( 'clay_const_z_soilt' ) ! No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%clay_soilt(:,1:nsoilt,1))
      psparms%clay_soilt(:,:,:) =                                              &
                        SPREAD(psparms%clay_soilt(:,:,1), 3, dim_cslayer)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate clay_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%clay_soilt(:,:,:) = const_val
  END IF
  !=== End cases for clay ===

  !=== Start cases for b ===
CASE ( 'b' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%bexp_soilt(:,1,1:sm_levels))
      psparms%bexp_soilt(:,:,:) = SPREAD(psparms%bexp_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%bexp_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate b- failure in logic")
    END IF
  ELSE
    psparms%bexp_soilt(:,:,:) = const_val
  END IF

CASE ( 'b_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%bexp_soilt(:,1,1))
      psparms%bexp_soilt(:,1,1:sm_levels) =                                    &
                            SPREAD(psparms%bexp_soilt(:,1,1), 2, sm_levels)
      psparms%bexp_soilt(:,:,:)           =                                    &
                            SPREAD(psparms%bexp_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%bexp_soilt(:,1,1))
      psparms%bexp_soilt(:,1,1:sm_levels) =                                    &
                            SPREAD(psparms%bexp_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate b_const_z- failure in logic")
    END IF
  ELSE
    psparms%bexp_soilt(:,:,:) = const_val
  END IF

CASE ( 'b_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%bexp_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate b_soilt- failure in logic")
    END IF
  ELSE
    psparms%bexp_soilt(:,:,:) = const_val
  END IF

CASE ( 'b_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%bexp_soilt(:,1:nsoilt,1))
      psparms%bexp_soilt(:,:,:) =                                              &
                        SPREAD(psparms%bexp_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate b_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%bexp_soilt(:,:,:) = const_val
  END IF
  !=== End cases for b ===

  !=== Start cases for sathh ===
CASE ( 'sathh' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%sathh_soilt(:,1,1:sm_levels))
      psparms%sathh_soilt(:,:,:) = SPREAD(psparms%sathh_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%sathh_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sathh- failure in logic")
    END IF
  ELSE
    psparms%sathh_soilt(:,:,:) = const_val
  END IF

CASE ( 'sathh_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%sathh_soilt(:,1,1))
      psparms%sathh_soilt(:,1,1:sm_levels) =                                   &
                            SPREAD(psparms%sathh_soilt(:,1,1), 2, sm_levels)
      psparms%sathh_soilt(:,:,:)           =                                   &
                            SPREAD(psparms%sathh_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%sathh_soilt(:,1,1))
      psparms%sathh_soilt(:,1,1:sm_levels) =                                   &
                            SPREAD(psparms%sathh_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sathh_const_z- failure in logic")
    END IF
  ELSE
    psparms%sathh_soilt(:,:,:) = const_val
  END IF

CASE ( 'sathh_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%sathh_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate psparms%sathh_soilt- failure in logic")
    END IF
  ELSE
    psparms%sathh_soilt(:,:,:) = const_val
  END IF

CASE ( 'sathh_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%sathh_soilt(:,1:nsoilt,1))
      psparms%sathh_soilt(:,:,:) =                                             &
                        SPREAD(psparms%sathh_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sathh_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%sathh_soilt(:,:,:) = const_val
  END IF
  !=== End cases for sathh ===

  !=== Start cases for sathh ===
CASE ( 'satcon' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%satcon_soilt(:,1,1:sm_levels))
      psparms%satcon_soilt(:,:,:) =                                            &
                        SPREAD(psparms%satcon_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%satcon_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate satcon- failure in logic")
    END IF
  ELSE
    psparms%satcon_soilt(:,:,:) = const_val
  END IF

CASE ( 'satcon_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%satcon_soilt(:,1,1))
      psparms%satcon_soilt(:,1,1:sm_levels) =                                  &
                        SPREAD(psparms%satcon_soilt(:,1,1), 2, sm_levels)
      psparms%satcon_soilt(:,:,:)           =                                  &
                        SPREAD(psparms%satcon_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%satcon_soilt(:,1,1))
      psparms%satcon_soilt(:,1,1:sm_levels) =                                  &
                        SPREAD(psparms%satcon_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate satcon_const_z- failure in logic")
    END IF
  ELSE
    psparms%satcon_soilt(:,:,:) = const_val
  END IF

CASE ( 'satcon_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%satcon_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate psparms%satcon_soilt- failure in logic")
    END IF
  ELSE
    psparms%satcon_soilt(:,:,:) = const_val
  END IF

CASE ( 'satcon_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%satcon_soilt(:,1:nsoilt,1))
      psparms%satcon_soilt(:,:,:) =                                            &
                        SPREAD(psparms%satcon_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate satcon_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%satcon_soilt(:,:,:) = const_val
  END IF
  !=== End cases for satcon ===

  !=== Start cases for sm_sat ===
CASE ( 'sm_sat' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcst_soilt(:,1,1:sm_levels))
      psparms%smvcst_soilt(:,:,:) =                                            &
                        SPREAD(psparms%smvcst_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcst_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_sat- failure in logic")
    END IF
  ELSE
    psparms%smvcst_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_sat_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcst_soilt(:,1,1))
      psparms%smvcst_soilt(:,1,1:sm_levels) =                                  &
                        SPREAD(psparms%smvcst_soilt(:,1,1), 2, sm_levels)
      psparms%smvcst_soilt(:,:,:)           =                                  &
                        SPREAD(psparms%smvcst_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcst_soilt(:,1,1))
      psparms%smvcst_soilt(:,1,1:sm_levels) =                                  &
                        SPREAD(psparms%smvcst_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_sat_const_z- failure in logic")
    END IF
  ELSE
    psparms%smvcst_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_sat_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcst_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_sat_soilt- failure in logic")
    END IF
  ELSE
    psparms%smvcst_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_sat_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcst_soilt(:,1:nsoilt,1))
      psparms%smvcst_soilt(:,:,:) =                                            &
                            SPREAD(psparms%smvcst_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_sat_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%smvcst_soilt(:,:,:) = const_val
  END IF
  !=== End cases for sm_sat ===

  !=== Start cases for sm_crit ===
CASE ( 'sm_crit' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvccl_soilt(:,1,1:sm_levels))
      psparms%smvccl_soilt(:,:,:) = SPREAD(psparms%smvccl_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvccl_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_crit- failure in logic")
    END IF
  ELSE
    psparms%smvccl_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_crit_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvccl_soilt(:,1,1))
      psparms%smvccl_soilt(:,1,1:sm_levels) =                                  &
                            SPREAD(psparms%smvccl_soilt(:,1,1), 2, sm_levels)
      psparms%smvccl_soilt(:,:,:)           =                                  &
                            SPREAD(psparms%smvccl_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvccl_soilt(:,1,1))
      psparms%smvccl_soilt(:,1,1:sm_levels) =                                  &
                            SPREAD(psparms%smvccl_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_crit_const_z- failure in logic")
    END IF
  ELSE
    psparms%smvccl_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_crit_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvccl_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_crit_soilt- failure in logic")
    END IF
  ELSE
    psparms%smvccl_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_crit_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvccl_soilt(:,1:nsoilt,1))
      psparms%smvccl_soilt(:,:,:) =                                            &
                            SPREAD(psparms%smvccl_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_crit_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%smvccl_soilt(:,:,:) = const_val
  END IF
  !=== End cases for sm_crit ===

  !=== Start cases for sm_wilt ===
CASE ( 'sm_wilt' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcwt_soilt(:,1,1:sm_levels))
      psparms%smvcwt_soilt(:,:,:) =                                            &
                            SPREAD(psparms%smvcwt_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcwt_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_wilt- failure in logic")
    END IF
  ELSE
    psparms%smvcwt_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_wilt_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcwt_soilt(:,1,1))
      psparms%smvcwt_soilt(:,1,1:sm_levels) =                                  &
                            SPREAD(psparms%smvcwt_soilt(:,1,1), 2, sm_levels)
      psparms%smvcwt_soilt(:,:,:)           =                                  &
                            SPREAD(psparms%smvcwt_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcwt_soilt(:,1,1))
      psparms%smvcwt_soilt(:,1,1:sm_levels) =                                  &
                            SPREAD(psparms%smvcwt_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_wilt_const_z- failure in logic")
    END IF
  ELSE
    psparms%smvcwt_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_wilt_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcwt_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_wilt_soilt- failure in logic")
    END IF
  ELSE
    psparms%smvcwt_soilt(:,:,:) = const_val
  END IF

CASE ( 'sm_wilt_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%smvcwt_soilt(:,1:nsoilt,1))
      psparms%smvcwt_soilt(:,:,:) =                                            &
                            SPREAD(psparms%smvcwt_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sm_wilt_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%smvcwt_soilt(:,:,:) = const_val
  END IF
  !=== End cases for sm_wilt ===

  !=== Start cases for hcap ===
CASE ( 'hcap' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcap_soilt(:,1,1:sm_levels))
      psparms%hcap_soilt(:,:,:) = SPREAD(psparms%hcap_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcap_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate hcap- failure in logic")
    END IF
  ELSE
    psparms%hcap_soilt(:,:,:) = const_val
  END IF

CASE ( 'hcap_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcap_soilt(:,1,1))
      psparms%hcap_soilt(:,1,1:sm_levels) =                                    &
                            SPREAD(psparms%hcap_soilt(:,1,1), 2, sm_levels)
      psparms%hcap_soilt(:,:,:)           =                                    &
                            SPREAD(psparms%hcap_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcap_soilt(:,1,1))
      psparms%hcap_soilt(:,1,1:sm_levels) =                                    &
                            SPREAD(psparms%hcap_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate hcap_const_z- failure in logic")
    END IF
  ELSE
    psparms%hcap_soilt(:,:,:) = const_val
  END IF

CASE ( 'hcap_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcap_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate psparms%hcap_soilt- failure in logic")
    END IF
  ELSE
    psparms%hcap_soilt(:,:,:) = const_val
  END IF

CASE ( 'hcap_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcap_soilt(:,1:nsoilt,1))
      psparms%hcap_soilt(:,:,:) =                                              &
                          SPREAD(psparms%hcap_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate hcap_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%hcap_soilt(:,:,:) = const_val
  END IF
  !=== End cases for hcap ===

  !=== Start cases for hcon ===
CASE ( 'hcon' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcon_soilt(:,1,1:sm_levels))
      psparms%hcon_soilt(:,:,:) = SPREAD(psparms%hcon_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcon_soilt(:,1,1:sm_levels))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate hcon- failure in logic")
    END IF
  ELSE
    psparms%hcon_soilt(:,:,:) = const_val
  END IF

CASE ( 'hcon_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcon_soilt(:,1,1))
      psparms%hcon_soilt(:,1,1:sm_levels) =                                    &
                        SPREAD(psparms%hcon_soilt(:,1,1), 2, sm_levels)
      psparms%hcon_soilt(:,:,:)           =                                    &
                        SPREAD(psparms%hcon_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcon_soilt(:,1,1))
      psparms%hcon_soilt(:,1,1:sm_levels) =                                    &
                        SPREAD(psparms%hcon_soilt(:,1,1), 2, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate hcon_const_z- failure in logic")
    END IF
  ELSE
    psparms%hcon_soilt(:,:,:) = const_val
  END IF

CASE ( 'hcon_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcon_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate psparms%hcon_soilt- failure in logic")
    END IF
  ELSE
    psparms%hcon_soilt(:,:,:) = const_val
  END IF

CASE ( 'hcon_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%hcon_soilt(:,1:nsoilt,1))
      psparms%hcon_soilt(:,:,:) =                                              &
                        SPREAD(psparms%hcon_soilt(:,:,1), 3, sm_levels)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate hcon_const_z_soilt- failure in logic")
    END IF
  ELSE
    psparms%hcon_soilt(:,:,:) = const_val
  END IF
  !=== End cases for hcon ===

  !-----------------------------------------------------------------------------
  ! Soil ancillaries on soil carbon layers.
  !-----------------------------------------------------------------------------

      !=== Start cases for soil_ph ===
CASE ( 'soil_ph' )  !With levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%soil_ph_soilt(:,1,1:dim_cslayer))
      psparms%soil_ph_soilt(:,:,:) =                                           &
                        SPREAD(psparms%soil_ph_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%soil_ph_soilt(:,1,1:dim_cslayer))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate soil_ph- failure in logic")
    END IF
  ELSE
    psparms%soil_ph_soilt(:,:,:) = const_val
  END IF

CASE ( 'soil_ph_const_z' ) !No levels, no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      ! Spread around soilt and layer dims
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%soil_ph_soilt(:,1,1))
      psparms%soil_ph_soilt(:,1,1:dim_cslayer) =                               &
                        SPREAD(psparms%soil_ph_soilt(:,1,1), 2, dim_cslayer)
      psparms%soil_ph_soilt(:,:,:)             =                               &
                        SPREAD(psparms%soil_ph_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      ! Copy the values from the first vertical level to all others
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%soil_ph_soilt(:,1,1))
      psparms%soil_ph_soilt(:,1,1:dim_cslayer) =                               &
                        SPREAD(psparms%soil_ph_soilt(:,1,1), 2, dim_cslayer)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate soil_ph_const_z- failure in logic")
    END IF
  ELSE
    psparms%soil_ph_soilt(:,:,:) = const_val
  END IF

CASE ( 'soil_ph_soilt' ) !With levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%soil_ph_soilt(:,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate psparms%soil_ph_soilt- failure in logic")
    END IF
  ELSE
    psparms%soil_ph_soilt(:,:,:) = const_val
  END IF

CASE ( 'soil_ph_const_z_soilt' ) !No levels, with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Spread around layer dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, psparms%soil_ph_soilt(:,1:nsoilt,1))
      psparms%soil_ph_soilt(:,:,:) = SPREAD(psparms%soil_ph_soilt(:,:,1), 3,   &
                                      dim_cslayer)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate soil_ph_const_z_soilt- " //              &
                     "failure in logic")
    END IF
  ELSE
    psparms%soil_ph_soilt(:,:,:) = const_val
  END IF
  !=== End cases for soil_ph ===

  !-----------------------------------------------------------------------------
  ! TOPMODEL variables are just on land points
  !
  ! Each variable can be read in with _soilt if the soil tile dimension is
  ! present.
  ! There are a few possible outcomes:
  !
  ! No suffix:
  !  l_tile_soil = T and l_broadcast_ancils = T
  !  --Spread around soilt dim
  !  l_tile_soil = F
  !  --Do nothing
  !
  ! _soilt suffix:
  ! l_tile_soil = T and l_broadcast_ancils = F
  ! --Do nothing
  !-----------------------------------------------------------------------------
      !=== Start cases for fexp ===
CASE ( 'fexp' )  !no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%fexp_soilt(:,1))
      toppdm%fexp_soilt(:,:) = SPREAD(toppdm%fexp_soilt(:,1), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%fexp_soilt(:,1))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate fexp- failure in logic")
    END IF
  ELSE
    toppdm%fexp_soilt(:,:) = const_val
  END IF

CASE ( 'toppdm%fexp_soilt' ) !with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%fexp_soilt(:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate toppdm%fexp_soilt- failure in logic")
    END IF
  ELSE
    toppdm%fexp_soilt(:,:) = const_val
  END IF
  !=== End cases for fexp ===

  !=== Start cases for ti_mean ===
CASE ( 'ti_mean' )  !no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%ti_mean_soilt(:,1))
      toppdm%ti_mean_soilt(:,:) = SPREAD(toppdm%ti_mean_soilt(:,1), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%ti_mean_soilt(:,1))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate ti_mean- failure in logic")
    END IF
  ELSE
    toppdm%ti_mean_soilt(:,:) = const_val
  END IF

CASE ( 'toppdm%ti_mean_soilt' ) !with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%ti_mean_soilt(:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate toppdm%ti_mean_soilt- failure in logic")
    END IF
  ELSE
    toppdm%ti_mean_soilt(:,:) = const_val
  END IF
  !=== End cases for ti_mean ===

  !=== Start cases for ti_sig ===
CASE ( 'ti_sig' )  !no tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_ancils) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%ti_sig_soilt(:,1))
      toppdm%ti_sig_soilt(:,:) = SPREAD(toppdm%ti_sig_soilt(:,1), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%ti_sig_soilt(:,1))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate ti_sig- failure in logic")
    END IF
  ELSE
    toppdm%ti_sig_soilt(:,:) = const_val
  END IF

CASE ( 'toppdm%ti_sig_soilt' ) !with tiles
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. .NOT. l_broadcast_ancils) THEN
       !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%ti_sig_soilt(:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate toppdm%ti_sig_soilt- failure in logic")
    END IF
  ELSE
    toppdm%ti_sig_soilt(:,:) = const_val
  END IF
  !=== End cases for ti_sig ===

  !-----------------------------------------------------------------------------
  ! Agricultural fraction is on land points only
  !-----------------------------------------------------------------------------
CASE ( 'frac_agr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, trifctltype%frac_agr_gb)
  ELSE
    trifctltype%frac_agr_gb(:) = const_val
  END IF

CASE ( 'frac_past' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, trif_vars%frac_past_gb)
  ELSE
    trif_vars%frac_past_gb(:) = const_val
  END IF

CASE ( 'frac_biocrop' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, trif_vars%frac_biocrop_gb)
  ELSE
    trif_vars%frac_biocrop_gb(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Irrigation fraction is on land points only
  !-----------------------------------------------------------------------------
CASE ( 'frac_irrig' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%frac_irr_all(:,1))
  ELSE
    crop_vars%frac_irr_all(:,1) = const_val
  END IF

CASE ( 'irrfrac_irrtiles' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%irrfrac_irrtiles(:,1))
  ELSE
    crop_vars%irrfrac_irrtiles(:,1) = const_val
  END IF

CASE ( 'frac_agr_prev' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%frac_agr_prev_gb)
  ELSE
    progs%frac_agr_prev_gb(:) = const_val
  END IF

CASE ( 'frac_past_prev' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%frac_past_prev_gb)
  ELSE
    progs%frac_past_prev_gb(:) = const_val
  END IF

CASE ( 'frac_biocrop_prev' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%frac_biocrop_prev_gb)
  ELSE
    progs%frac_biocrop_prev_gb(:) = const_val
  END IF

CASE ( 'harvest_doy' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    harvest_doy_real = REAL(trif_vars%harvest_doy)
    CALL cube_get_data(cube_land, harvest_doy_real)
    trif_vars%harvest_doy(:,:) = NINT(harvest_doy_real)
  ELSE
    trif_vars%harvest_doy(:,:) = NINT(const_val)
  END IF

CASE ( 'years_since_harvest' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    years_since_harvest_real = REAL(progs%years_since_harvest)
    CALL cube_get_data(cube_land, years_since_harvest_real)
    progs%years_since_harvest(:,:) = NINT(years_since_harvest_real)
  ELSE
    progs%years_since_harvest(:,:) = NINT(const_val)
  END IF

CASE ( 'wood_prod_fast' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%wood_prod_fast_gb)
  ELSE
    progs%wood_prod_fast_gb(:) = const_val
  END IF

CASE ( 'wood_prod_med' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%wood_prod_med_gb)
  ELSE
    progs%wood_prod_med_gb(:) = const_val
  END IF

CASE ( 'wood_prod_slow' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%wood_prod_slow_gb)
  ELSE
    progs%wood_prod_slow_gb(:) = const_val
  END IF

CASE ( 'n_inorg_avail_pft' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%n_inorg_avail_pft)
  ELSE
    progs%n_inorg_avail_pft(:,:,:) = const_val
  END IF

CASE ( 'n_inorg' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%n_inorg_soilt_lyrs(:,1,:))
      progs%n_inorg_soilt_lyrs(:,:,:) =                                        &
        SPREAD(progs%n_inorg_soilt_lyrs(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%n_inorg_soilt_lyrs(:,1,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate n_inorg- failure in logic")
    END IF
  ELSE
    progs%n_inorg_soilt_lyrs(:,:,:) = const_val
  END IF

CASE ( 'substr_ch4' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%substr_ch4)
  ELSE
    progs%substr_ch4(:,:,:) = const_val
  END IF

CASE ( 'mic_ch4' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%mic_ch4)
  ELSE
    progs%mic_ch4(:,:,:) = const_val
  END IF

CASE ( 'timewet_lyr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%timewet_lyr)
  ELSE
    progs%timewet_lyr(:,:,:) = const_val
  END IF

CASE ( 'mic_act_ch4' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%mic_act_ch4)
  ELSE
    progs%mic_act_ch4(:,:,:) = const_val
  END IF

CASE ( 'acclim_ch4' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%acclim_ch4)
  ELSE
    progs%acclim_ch4(:,:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Urban variables are on land points only
  !-----------------------------------------------------------------------------
CASE ( 'wrr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%wrr_gb)
  ELSE
    urban_param%wrr_gb(:) = const_val
  END IF

CASE ( 'hwr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%hwr_gb)
  ELSE
    urban_param%hwr_gb(:) = const_val
  END IF

CASE ( 'hgt' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%hgt_gb)
  ELSE
    urban_param%hgt_gb(:) = const_val
  END IF

CASE ( 'ztm' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%ztm_gb)
  ELSE
    urban_param%ztm_gb(:) = const_val
  END IF

CASE ( 'disp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%disp_gb)
  ELSE
    urban_param%disp_gb(:) = const_val
  END IF

CASE ( 'albwl' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%albwl_gb)
  ELSE
    urban_param%albwl_gb(:) = const_val
  END IF

CASE ( 'albrd' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%albrd_gb)
  ELSE
    urban_param%albrd_gb(:) = const_val
  END IF

CASE ( 'emisw' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%emisw_gb)
  ELSE
    urban_param%emisw_gb(:) = const_val
  END IF

CASE ( 'emisr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, urban_param%emisr_gb)
  ELSE
    urban_param%emisr_gb(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! FLake variables are on land points only
  !-----------------------------------------------------------------------------
CASE ( 'lake_depth' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_depth_gb(:))
  ELSE
    lake_vars%lake_depth_gb(:) = const_val
  END IF

CASE ( 'lake_fetch_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_fetch_gb(:))
  ELSE
    lake_vars%lake_fetch_gb(:) = const_val
  END IF

CASE ( 'lake_h_mxl_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_h_mxl_gb(:))
  ELSE
    lake_vars%lake_h_mxl_gb(:) = const_val
  END IF

CASE ( 'lake_shape_factor_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_shape_factor_gb(:))
  ELSE
    lake_vars%lake_shape_factor_gb(:) = const_val
  END IF

CASE ( 'lake_t_mean_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_t_mean_gb(:))
  ELSE
    lake_vars%lake_t_mean_gb(:) = const_val
  END IF

CASE ( 'lake_t_mxl_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_t_mxl_gb(:))
  ELSE
    lake_vars%lake_t_mxl_gb(:) = const_val
  END IF

CASE ( 'lake_t_ice_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_t_ice_gb(:))
  ELSE
    lake_vars%lake_t_ice_gb(:) = const_val
  END IF

CASE ( 'lake_h_ice_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_h_ice_gb(:))
  ELSE
    lake_vars%lake_h_ice_gb(:) = const_val
  END IF

CASE ( 'lake_t_sfc_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, lake_vars%lake_t_sfc_gb(:))
  ELSE
    lake_vars%lake_t_sfc_gb(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Variables that are set as initial conditions
  ! These are all land points only, but with varying numbers of levels

  ! Some variables can be read in with _soilt if the soil tile dimension is
  ! present. There are a few possible outcomes
  !
  ! No suffix:
  !  l_tile_soil = T and l_broadcast_soilt = T
  !  --Spread around soilt dim
  !  l_tile_soil = F
  !  --Do nothing
  !
  ! _soilt suffix:
  ! l_tile_soil = T and l_broadcast_soilt = F
  ! --Do nothing
  !-----------------------------------------------------------------------------

CASE ( 'canopy' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%canopy_surft)
  ELSE
    progs%canopy_surft(:,:) = const_val
  END IF

CASE ( 'cs' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%cs_pool_soilt(:,1,:,:))
      progs%cs_pool_soilt(:,:,:,:) =                                           &
                        SPREAD(progs%cs_pool_soilt(:,1,:,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%cs_pool_soilt(:,1,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate cs- failure in logic")
    END IF
  ELSE
    progs%cs_pool_soilt(:,:,:,:) = const_val
  END IF

CASE ( 'cs_soilt' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%cs_pool_soilt(:,:,:,:))
  ELSE
    progs%cs_pool_soilt(:,:,:,:) = const_val
  END IF

CASE ( 'frac_c_label_pool' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%frac_c_label_pool_soilt(:,1,:,:))
      progs%frac_c_label_pool_soilt(:,:,:,:) =                                 &
                        SPREAD(progs%cs_pool_soilt(:,1,:,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%frac_c_label_pool_soilt(:,1,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate frac_c_label_pool - failure in logic")
    END IF
  ELSE
    progs%frac_c_label_pool_soilt(:,:,:,:) = const_val
  END IF

CASE ( 'frac_c_label_pool_soilt' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%frac_c_label_pool_soilt(:,:,:,:))
  ELSE
    progs%frac_c_label_pool_soilt(:,:,:,:) = const_val
  END IF

CASE ( 'plantNumDensity' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%plantNumDensity(:,:,:))
  ELSE
    progs%plantNumDensity(:,:,:) = const_val
  END IF

CASE ( 'ns' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%ns_pool_gb)
  ELSE
    progs%ns_pool_gb(:,:,:) = const_val
  END IF

CASE ( 'deposition_n' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, trif_vars%deposition_n_gb)
  ELSE
    trif_vars%deposition_n_gb(:) = const_val
  END IF

CASE ( 'trif_vars%g_burn_pft' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, trif_vars%g_burn_pft)
  ELSE
    trif_vars%g_burn_pft(:,:) = const_val
  END IF

CASE ( 'gs' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%gs_gb)
  ELSE
    progs%gs_gb(:) = const_val
  END IF

CASE ( 'snow_tile' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%snow_surft)
  ELSE
    progs%snow_surft(:,:) = const_val
  END IF

CASE ( 'sthuf' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, jules_vars%sthuf_soilt(:,1,:))
      jules_vars%sthuf_soilt(:,:,:) = SPREAD(jules_vars%sthuf_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, jules_vars%sthuf_soilt(:,1,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sthuf- failure in logic")
    END IF
  ELSE
    jules_vars%sthuf_soilt(:,:,:) = const_val
  END IF

CASE ( 'sthuf_soilt' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, jules_vars%sthuf_soilt)
  ELSE
    jules_vars%sthuf_soilt(:,:,:) = const_val
  END IF

CASE ( 't_soil' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%t_soil_soilt(:,1,:))
      progs%t_soil_soilt(:,:,:) = SPREAD(progs%t_soil_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, progs%t_soil_soilt(:,1,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate t_soil- failure in logic")
    END IF
  ELSE
    progs%t_soil_soilt(:,:,:) = const_val
  END IF

CASE ( 't_soil_soilt' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%t_soil_soilt)
  ELSE
    progs%t_soil_soilt(:,:,:) = const_val
  END IF

CASE ( 'tsoil_deep' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%tsoil_deep_gb)
  ELSE
    progs%tsoil_deep_gb(:,:) = const_val
  END IF

CASE ( 'sthu_irr' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, crop_vars%sthu_irr_soilt(:,1,:))
      crop_vars%sthu_irr_soilt(:,:,:) =                                        &
        SPREAD(crop_vars%sthu_irr_soilt(:,1,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, crop_vars%sthu_irr_soilt(:,1,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sthu_irr- failure in logic")
    END IF
  ELSE
    crop_vars%sthu_irr_soilt(:,:,:) = const_val
  END IF

CASE ( 'tstar_tile' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%tstar_surft)
  ELSE
    progs%tstar_surft(:,:) = const_val
  END IF

CASE ( 'tsurf_elev_surft' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%tsurf_elev_surft)
  ELSE
    progs%tsurf_elev_surft(:,:) = const_val
  END IF

CASE ( 'cropdvi' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%dvi_cpft)
  ELSE
    crop_vars%dvi_cpft(:,:) = const_val
  END IF

CASE ( 'croprootc' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%rootc_cpft)
  ELSE
    crop_vars%rootc_cpft(:,:) = const_val
  END IF

CASE ( 'cropharvc' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%harvc_cpft)
  ELSE
    crop_vars%harvc_cpft(:,:) = const_val
  END IF

CASE ( 'cropreservec' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%reservec_cpft)
  ELSE
    crop_vars%reservec_cpft(:,:) = const_val
  END IF

CASE ( 'croplai' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%croplai_cpft)
  ELSE
    crop_vars%croplai_cpft(:,:) = const_val
  END IF

CASE ( 'cropcanht' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%cropcanht_cpft)
  ELSE
    crop_vars%cropcanht_cpft(:,:) = const_val
  END IF

CASE ( 'cropsowdate' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%sow_date_cpft)
  ELSE
    crop_vars%sow_date_cpft(:,:) = const_val
  END IF

CASE ( 'croplatestharvdate' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%latestharv_date_cpft)
  ELSE
    crop_vars%latestharv_date_cpft(:,:) = const_val
  END IF

CASE ( 'cropttveg' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%tt_veg_cpft)
  ELSE
    crop_vars%tt_veg_cpft(:,:) = const_val
  END IF

CASE ( 'cropttrep' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, crop_vars%tt_rep_cpft)
  ELSE
    crop_vars%tt_rep_cpft(:,:) = const_val
  END IF

CASE ( 'lai' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%lai_pft)
  ELSE
    progs%lai_pft(:,:) = const_val
  END IF

CASE ( 'canht' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%canht_pft)
  ELSE
    progs%canht_pft(:,:) = const_val
  END IF

CASE ( 'sthzw' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%sthzw_soilt(:,1))
      toppdm%sthzw_soilt(:,:) = SPREAD(toppdm%sthzw_soilt(:,1), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%sthzw_soilt(:,1))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate sthzw- failure in logic")
    END IF
  ELSE
    toppdm%sthzw_soilt(:,:) = const_val
  END IF

CASE ( 'toppdm%sthzw_soilt' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, toppdm%sthzw_soilt)
  ELSE
    toppdm%sthzw_soilt(:,:) = const_val
  END IF

CASE ( 'sub_surf_roff' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fluxes%sub_surf_roff_gb)
  ELSE
    fluxes%sub_surf_roff_gb(:) = const_val
  END IF

CASE ( 'surf_roff' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fluxes%surf_roff_gb)
  ELSE
    fluxes%surf_roff_gb(:) = const_val
  END IF

CASE ( 'zw' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%zw_soilt(:,1))
      toppdm%zw_soilt(:,:) = SPREAD(toppdm%zw_soilt(:,1), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, toppdm%zw_soilt(:,1))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate zw- failure in logic")
    END IF
  ELSE
    toppdm%zw_soilt(:,:) = const_val
  END IF

CASE ( 'toppdm%zw_soilt' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, toppdm%zw_soilt)
  ELSE
    toppdm%zw_soilt(:,:) = const_val
  END IF

CASE ( 'slope' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, toppdm%slope_gb)
  ELSE
    toppdm%slope_gb(:) = const_val
  END IF

CASE ( 'rgrain' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%rgrain_surft)
  ELSE
    progs%rgrain_surft(:,:) = const_val
  END IF

CASE ( 'cv' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, trifctltype%cv_gb)
  ELSE
    trifctltype%cv_gb(:) = const_val
  END IF

CASE ( 'rho_snow' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%rho_snow_grnd_surft)
  ELSE
    progs%rho_snow_grnd_surft(:,:) = const_val
  END IF

CASE ( 'snow_depth' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%snowdepth_surft)
  ELSE
    progs%snowdepth_surft(:,:) = const_val
  END IF

CASE ( 'snow_grnd' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%snow_grnd_surft)
  ELSE
    progs%snow_grnd_surft(:,:) = const_val
  END IF

CASE ( 'nsnow' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, nsnow_real)
    progs%nsnow_surft(:,:) = NINT(nsnow_real)
  ELSE
    progs%nsnow_surft(:,:) = NINT(const_val)
  END IF

CASE ( 'snow_ds' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%ds_surft)
  ELSE
    progs%ds_surft(:,:,:) = const_val
  END IF

CASE ( 'snow_ice' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%sice_surft)
  ELSE
    progs%sice_surft(:,:,:) = const_val
  END IF

CASE ( 'snow_liq' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%sliq_surft)
  ELSE
    progs%sliq_surft(:,:,:) = const_val
  END IF

CASE ( 'tsnow' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%tsnow_surft)
  ELSE
    progs%tsnow_surft(:,:,:) = const_val
  END IF

CASE ( 'rgrainl' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%rgrainl_surft)
  ELSE
    progs%rgrainl_surft(:,:,:) = const_val
  END IF

CASE ( 'f_nsc' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%f_nsc_pft(:,:))
  ELSE
    progs%f_nsc_pft(:,:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Forcing variables
  !-----------------------------------------------------------------------------
CASE ( 'pstar' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%pstar_ij)
  ELSE
    forcing%pstar_ij(:,:) = const_val
  END IF

CASE ( 'q' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%qw_1_ij)
  ELSE
    forcing%qw_1_ij(:,:) = const_val
  END IF
  forcing%qw_1_ij(:,:) = MAX(forcing%qw_1_ij, 0.0)

CASE ( 't' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube,forcing%tl_1_ij)
  ELSE
    forcing%tl_1_ij(:,:) = const_val
  END IF

CASE ( 'rad_net' )
  ! Net downward radiation is stored in lw_down until it is processed
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%lw_down_ij)
  ELSE
    forcing%lw_down_ij(:,:) = const_val
  END IF

CASE ( 'lw_net' )
  ! Net LW downward radiation is stored in lw_down until it is processed
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%lw_down_ij)
  ELSE
    forcing%lw_down_ij(:,:) = const_val
  END IF

CASE ( 'sw_net' )
  ! Net SW downward radiation is stored in sw_down until it is processed
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%sw_down_ij)
  ELSE
    forcing%sw_down_ij(:,:) = const_val
  END IF

CASE ( 'lw_down' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%lw_down_ij)
  ELSE
    forcing%lw_down_ij(:,:) = const_val
  END IF

CASE ( 'sw_down' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%sw_down_ij)
  ELSE
    forcing%sw_down_ij(:,:) = const_val
  END IF

CASE ( 'diff_rad' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%diff_rad_ij)
  ELSE
    forcing%diff_rad_ij(:,:) = const_val
  END IF

CASE ( 'dt_range' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%diurnal_temperature_range_ij)
  ELSE
    forcing%diurnal_temperature_range_ij(:,:) = const_val
  END IF

CASE ( 'precip' )
  ! Store total precip as large-scale rainfall until it is partitioned
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube,forcing%ls_rain_ij)
  ELSE
    forcing%ls_rain_ij(:,:) = const_val
  END IF

CASE ( 'tot_rain' )
  ! Store total rainfall as large-scale until it is partitioned
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%ls_rain_ij)
  ELSE
    forcing%ls_rain_ij(:,:) = const_val
  END IF

CASE ( 'tot_snow' )
  ! If given total snow, we assume it is all large-scale
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%ls_snow_ij)
  ELSE
    forcing%ls_snow_ij(:,:) = const_val
  END IF

CASE ( 'con_rain' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%con_rain_ij)
  ELSE
    forcing%con_rain_ij(:,:) = const_val
  END IF

CASE ( 'ls_rain' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%ls_rain_ij)
  ELSE
    forcing%ls_rain_ij(:,:) = const_val
  END IF

CASE ( 'con_snow' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%con_snow_ij)
  ELSE
    forcing%con_snow_ij(:,:) = const_val
  END IF

CASE ( 'ls_snow' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, forcing%ls_snow_ij)
  ELSE
    forcing%ls_snow_ij(:,:) = const_val
  END IF

CASE ( 'wind' )
  ! Wind speed just goes directly into u component
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, u_1_ij)
  ELSE
    u_1_ij(:,:) = const_val
  END IF

CASE ( 'u' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, u_1_ij)
  ELSE
    u_1_ij(:,:) = const_val
  END IF

CASE ( 'v' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, v_1_ij)
  ELSE
    v_1_ij(:,:) = const_val
  END IF

CASE ( 'z1_tq_in' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, ainfo%z1_tq_ij)
  ELSE
    ainfo%z1_tq_ij(:,:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! imogen driving variables
  !-----------------------------------------------------------------------------
CASE ( 'tl1_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%tl1_ij_anom)
  ELSE
    imgn_drive%tl1_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'wind_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%wind_ij_anom)
  ELSE
    imgn_drive%wind_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'lwdown_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%lwdown_ij_anom)
  ELSE
    imgn_drive%lwdown_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'swdown_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%swdown_ij_anom)
  ELSE
    imgn_drive%swdown_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'diurnal_tl1_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%diurnal_tl1_ij_anom)
  ELSE
    imgn_drive%diurnal_tl1_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'precip_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%precip_ij_anom)
  ELSE
    imgn_drive%precip_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'ql1_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%ql1_ij_anom)
  ELSE
    imgn_drive%ql1_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'pstar_ij_anom_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%pstar_ij_anom)
  ELSE
    imgn_drive%pstar_ij_anom(:,:,:) = const_val
  END IF

CASE ( 'tl1_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%tl1_ij_clim)
  ELSE
    imgn_drive%tl1_ij_clim(:,:,:) = const_val
  END IF

CASE ( 'tl1_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%tl1_ij_patt)
  ELSE
    imgn_drive%tl1_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'wind_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%wind_ij_patt)
  ELSE
    imgn_drive%wind_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'lwdown_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%lwdown_ij_patt)
  ELSE
    imgn_drive%lwdown_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'swdown_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%swdown_ij_patt)
  ELSE
    imgn_drive%swdown_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'diurnal_tl1_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%diurnal_tl1_ij_patt)
  ELSE
    imgn_drive%diurnal_tl1_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'precip_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%precip_ij_patt)
  ELSE
    imgn_drive%precip_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'ql1_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%ql1_ij_patt)
  ELSE
    imgn_drive%ql1_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'pstar_ij_patt_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%pstar_ij_patt)
  ELSE
    imgn_drive%pstar_ij_patt(:,:,:) = const_val
  END IF

CASE ( 'wind_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%wind_ij_clim)
  ELSE
    imgn_drive%wind_ij_clim(:,:,:) = const_val
  END IF

CASE ( 'lwdown_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%lwdown_ij_clim)
  ELSE
    imgn_drive%lwdown_ij_clim(:,:,:) = const_val
  END IF

CASE ( 'swdown_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%swdown_ij_clim)
  ELSE
    imgn_drive%swdown_ij_clim(:,:,:) = const_val
  END IF

CASE ( 'diurnal_tl1_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%diurnal_tl1_ij_clim)
  ELSE
    imgn_drive%diurnal_tl1_ij_clim(:,:,:) = const_val
  END IF

CASE ( 'precip_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%precip_ij_clim)
  ELSE
    imgn_drive%precip_ij_clim(:,:,:) = const_val
  END IF

CASE ( 'ql1_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%ql1_ij_clim)
  ELSE
    imgn_drive%ql1_ij_clim(:,:,:) = const_val
  END IF

CASE ( 'pstar_ij_clim_imgn' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, imgn_drive%pstar_ij_clim)
  ELSE
    imgn_drive%pstar_ij_clim(:,:,:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! imogen variables
  !-----------------------------------------------------------------------------

CASE ( 'dtemp_g' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, imgn_vars%dtemp_g)
  ELSE
    imgn_vars%dtemp_g(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! River routing variables
  !-----------------------------------------------------------------------------

CASE ( 'latitude_2d' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, rivers%rivers_lat2d)
  ELSE
    rivers%rivers_lat2d(:,:) = const_val
  END IF

CASE ( 'longitude_2d' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, rivers%rivers_lon2d)
  ELSE
    rivers%rivers_lon2d(:,:) = const_val
  END IF

CASE ( 'direction' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, rivers%rivers_dir)
  ELSE
    rivers%rivers_dir(:,:) = const_val
  END IF

CASE ( 'sequence' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, rivers%rivers_seq)
  ELSE
    rivers%rivers_seq(:,:) = const_val
  END IF

CASE ( 'area' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, rivers%rivers_dra)
  ELSE
    rivers%rivers_dra(:,:) = const_val
  END IF

CASE ( 'channel_depth_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, channel_depth_grid)
  ELSE
    channel_depth_grid(:,:) = const_val
  END IF

CASE ( 'channel_width_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, channel_width_grid)
  ELSE
    channel_width_grid(:,:) = const_val
  END IF

CASE ( 'mean_sea_level_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, mean_sea_level_grid)
  ELSE
    mean_sea_level_grid(:,:) = const_val
  END IF

CASE ( 'river_distance_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, river_distance_grid)
  ELSE
    river_distance_grid(:,:) = const_val
  END IF

CASE ( 'river_elevation_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, river_elevation_grid)
  ELSE
    river_elevation_grid(:,:) = const_val
  END IF

CASE ( 'river_length_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, river_length_grid)
  ELSE
    river_length_grid(:,:) = const_val
  END IF

CASE ( 'river_manning_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, river_manning_grid)
  ELSE
    river_manning_grid(:,:) = const_val
  END IF

CASE ( 'river_nextx_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, river_nextx_grid)
  ELSE
    river_nextx_grid(:,:) = const_val
  END IF

CASE ( 'river_nexty_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, river_nexty_grid)
  ELSE
    river_nexty_grid(:,:) = const_val
  END IF

CASE ( 'rivers_ygrid' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    cube_land%values(:) = RESHAPE(cube%values, [ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rivers_ygrid)
  ELSE
    rivers%rivers_ygrid(:) = const_val
  END IF

CASE ( 'rivers_xgrid' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    cube_land%values(:) = RESHAPE(cube%values, [ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rivers_xgrid)
  ELSE
    rivers%rivers_xgrid(:) = const_val
  END IF

CASE ( 'rivers_storage' )
  IF ( PRESENT(cube) ) THEN
    IF ( SIZE(cube%SHAPE) == 2 ) THEN
      CALL cube_get_data(cube, rivers%rivers_storage)
    ELSE IF ( SIZE(cube%SHAPE) == 3 ) THEN
      ! Climatology ancil requires time interpolation
      CALL cube_interp_climatology_data(cube, rivers%rivers_storage)
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Unexpected rank of cube for rivers_storage")
    END IF
  ELSE
    rivers%rivers_storage(:,:) = const_val
  END IF

CASE ( 'rivers_sto_rp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rivers_sto_rp)
  ELSE
    rivers%rivers_sto_rp(:) = const_val
  END IF

CASE ( 'rivers_outflow_rp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rivers_outflow_rp)
  ELSE
    rivers%rivers_outflow_rp(:) = const_val
  END IF

CASE ( 'rfm_surfstore_rp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rfm_surfstore_rp)
  ELSE
    rivers%rfm_surfstore_rp(:) = const_val
  END IF

CASE ( 'rfm_substore_rp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rfm_substore_rp)
  ELSE
    rivers%rfm_substore_rp(:) = const_val
  END IF

CASE ( 'rfm_flowin_rp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rfm_flowin_rp)
  ELSE
    rivers%rfm_flowin_rp(:) = const_val
  END IF

CASE ( 'rfm_bflowin_rp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = cube_create([ SIZE(cube%values) ])
    CALL cube_get_data(cube_land, rivers%rfm_bflowin_rp)
  ELSE
    rivers%rfm_bflowin_rp(:) = const_val
  END IF

CASE ( 'sea_level_lp' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, rivers%sea_level_lp)
  ELSE
    rivers%sea_level_lp(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Overbank inundation variables
  !-----------------------------------------------------------------------------

CASE ( 'hypso_quantiles_grid' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, hypsometric_quantiles_grid)
  ELSE
    hypsometric_quantiles_grid(:,:,:) = const_val
  END IF

CASE ( 'logn_mean' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, logn_mean)
  ELSE
    logn_mean(:,:) = const_val
  END IF

CASE ( 'logn_stdev' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, logn_stdev)
  ELSE
    logn_stdev(:,:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Fire and metstats variables- land points only
  !-----------------------------------------------------------------------------
  ! metstats module cases- land points only
CASE ( 'temp_max_00h_r' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%temp_max_00h%run)
  ELSE
    metstats_prog(:)%temp_max_00h%run = const_val
  END IF

CASE ( 'temp_ave_00h_r' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%temp_ave_00h%run)
  ELSE
    metstats_prog(:)%temp_ave_00h%run = const_val
  END IF

CASE ( 'prec_tot_00h_r' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%prec_tot_00h%run)
  ELSE
    metstats_prog(:)%prec_tot_00h%run = const_val
  END IF

CASE ( 'prec_tot_12h_r' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%prec_tot_12h%run)
  ELSE
    metstats_prog(:)%prec_tot_12h%run = const_val
  END IF

CASE ( 'rhum_min_00h_r' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%rhum_min_00h%run)
  ELSE
    metstats_prog(:)%rhum_min_00h%run = const_val
  END IF

CASE ( 'dewp_ave_00h_r' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%dewp_ave_00h%run)
  ELSE
    metstats_prog(:)%dewp_ave_00h%run = const_val
  END IF

CASE ( 'wind_ave_00h_r' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%wind_ave_00h%run)
  ELSE
    metstats_prog(:)%wind_ave_00h%run = const_val
  END IF

CASE ( 'temp_max_00h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%temp_max_00h%fin)
  ELSE
    metstats_prog(:)%temp_max_00h%fin = const_val
  END IF

CASE ( 'temp_ave_00h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%temp_ave_00h%fin)
  ELSE
    metstats_prog(:)%temp_ave_00h%fin = const_val
  END IF

CASE ( 'temp_ave_nday' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%temp_ave_nday%fin)
  ELSE
    metstats_prog(:)%temp_ave_nday%fin = const_val
  END IF

CASE ( 'temp_pnt_12h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%temp_pnt_12h%fin)
  ELSE
    metstats_prog(:)%temp_pnt_12h%fin = const_val
  END IF

CASE ( 'prec_tot_00h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%prec_tot_00h%fin)
  ELSE
    metstats_prog(:)%prec_tot_00h%fin = const_val
  END IF

CASE ( 'prec_tot_12h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%prec_tot_12h%fin)
  ELSE
    metstats_prog(:)%prec_tot_12h%fin = const_val
  END IF

CASE ( 'rhum_min_00h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%rhum_min_00h%fin)
  ELSE
    metstats_prog(:)%rhum_min_00h%fin = const_val
  END IF

CASE ( 'rhum_pnt_12h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%rhum_pnt_12h%fin)
  ELSE
    metstats_prog(:)%rhum_pnt_12h%fin = const_val
  END IF

CASE ( 'dewp_ave_00h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%dewp_ave_00h%fin)
  ELSE
    metstats_prog(:)%dewp_ave_00h%fin = const_val
  END IF

CASE ( 'wind_ave_00h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%wind_ave_00h%fin)
  ELSE
    metstats_prog(:)%wind_ave_00h%fin = const_val
  END IF

CASE ( 'wind_pnt_12h' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, metstats_prog(:)%wind_pnt_12h%fin)
  ELSE
    metstats_prog(:)%wind_pnt_12h%fin = const_val
  END IF

  ! Fire module variables- land points only
CASE ( 'fire_mcarthur_r_dr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_prog(:)%mcarthur%r_dr)
  ELSE
    fire_prog(:)%mcarthur%r_dr = const_val
  END IF

CASE ( 'fire_mcarthur_n_dr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_prog(:)%mcarthur%n_dr)
  ELSE
    fire_prog(:)%mcarthur%n_dr = const_val
  END IF

CASE ( 'fire_canadian_ffmc' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_prog(:)%canadian%ffmc)
  ELSE
    fire_prog(:)%canadian%ffmc = const_val
  END IF

CASE ( 'fire_canadian_ffmc_mois' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_prog(:)%canadian%ffmc_mois)
  ELSE
    fire_prog(:)%canadian%ffmc_mois = const_val
  END IF

CASE ( 'fire_canadian_dmc' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_prog(:)%canadian%dmc)
  ELSE
    fire_prog(:)%canadian%dmc = const_val
  END IF

CASE ( 'fire_canadian_dc' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_prog(:)%canadian%dc)
  ELSE
    fire_prog(:)%canadian%dc = const_val
  END IF

CASE ( 'fire_nesterov' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_prog(:)%nesterov%findex)
  ELSE
    fire_prog(:)%nesterov%findex = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Other variables that might be prescribed
  !-----------------------------------------------------------------------------
CASE ( 'bl_height' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, jules_vars%zh)
  ELSE
    jules_vars%zh(:,:) = const_val
  END IF

CASE ( 'level_separation' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, jules_vars%dzl)
  ELSE
    jules_vars%dzl(:,:,:) = const_val
  END IF

CASE ( 'ozone' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, chemvars%o3_gb)
  ELSE
    chemvars%o3_gb(:) = const_val
  END IF

CASE ( 'deposition_conc' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, chemvars%dep_surfconc_ij)
  ELSE
    chemvars%dep_surfconc_ij(:,:,:) = const_val
  END IF

CASE ( 'albobs_sw' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, psparms%albobs_sw_gb)
  ELSE
    psparms%albobs_sw_gb(:) = const_val
  END IF

CASE ( 'albobs_vis' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, psparms%albobs_vis_gb)
  ELSE
    psparms%albobs_vis_gb(:) = const_val
  END IF

CASE ( 'albobs_nir' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, psparms%albobs_nir_gb)
  ELSE
    psparms%albobs_nir_gb(:) = const_val
  END IF

CASE ( 'co2_mmr' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, co2_tmp)

    IF ((MAXVAL(co2_tmp) /= co2_tmp(1)) .OR.                                   &
        (MINVAL(co2_tmp) /= co2_tmp(1))) THEN
      CALL log_fatal("populate_var",                                           &
                     "All land points must have same CO2 concetration")
    END IF
    co2_mmr = co2_tmp(1)
  ELSE
    co2_mmr = const_val
  END IF

CASE ( 'pop_den' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_vars%pop_den)
  ELSE
    fire_vars%pop_den(:) = const_val
  END IF

CASE ( 'flash_rate' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_vars%flash_rate)
  ELSE
    fire_vars%flash_rate(:) = const_val
  END IF

CASE ( 'wealth_index' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, fire_vars%wealth_index)
  ELSE
    fire_vars%wealth_index(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Variables for thermal acclimation of photosynthesis.
  !-----------------------------------------------------------------------------
CASE ( 't_home_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%t_home_gb)
  ELSE
    progs%t_home_gb(:) = const_val
  END IF

CASE ( 't_growth_gb' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs%t_growth_gb)
  ELSE
    progs%t_growth_gb(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  !   ECOSSE variables.
  !-----------------------------------------------------------------------------
  !   ECOSSE prognostic variables.

CASE ( 'n_soil' )
  IF ( PRESENT(cube) ) THEN
    IF ( l_tile_soil .AND. l_broadcast_soilt) THEN
      !Spread around soilt dim
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, soilecosse%n_soil_pool_soilt(:,1,:,:))
      soilecosse%n_soil_pool_soilt(:,:,:,:) =                                  &
                    SPREAD(soilecosse%n_soil_pool_soilt(:,1,:,:), 2, nsoilt)
    ELSE IF ( .NOT. l_tile_soil ) THEN
      !Do nothing special
      cube_land = map_to_land(cube)
      CALL cube_get_data(cube_land, soilecosse%n_soil_pool_soilt(:,1,:,:))
    ELSE
      CALL log_fatal("populate_var",                                           &
                     "Cannot populate n_soil- failure in logic")
    END IF
  ELSE
    soilecosse%n_soil_pool_soilt(:,:,:,:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Variables for water resource management.
  !-----------------------------------------------------------------------------
CASE ( 'conv_loss_frac' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, water_resources%conv_loss_frac)
  ELSE
    water_resources%conv_loss_frac(:) = const_val
  END IF

CASE ( 'demand_rate_domestic' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, water_resources%demand_rate_domestic)
  ELSE
    water_resources%demand_rate_domestic(:) = const_val
  END IF

CASE ( 'demand_rate_industry' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, water_resources%demand_rate_industry)
  ELSE
    water_resources%demand_rate_industry(:) = const_val
  END IF

CASE ( 'demand_rate_livestock' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, water_resources%demand_rate_livestock)
  ELSE
    water_resources%demand_rate_livestock(:) = const_val
  END IF

CASE ( 'demand_rate_transfers' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, water_resources%demand_rate_transfers)
  ELSE
    water_resources%demand_rate_transfers(:) = const_val
  END IF

CASE ( 'grid_area' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, grid_area_ij)
  ELSE
    grid_area_ij(:,:) = const_val
  END IF

CASE ( 'sfc_water_frac' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, water_resources%sfc_water_frac)
  ELSE
    water_resources%sfc_water_frac(:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  !   CABLE prognostic variables.
  !-----------------------------------------------------------------------------

CASE ( 'SoilTemp_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%SoilTemp_CABLE )
  ELSE
    progs_cbl_vars%SoilTemp_CABLE(:,:,:) = const_val
  END IF

CASE ( 'FrozenSoilFrac_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land,progs_cbl_vars%FrozenSoilFrac_CABLE )
  ELSE
    progs_cbl_vars%FrozenSoilFrac_CABLE(:,:,:) = const_val
  END IF

CASE ( 'SoilMoisture_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%SoilMoisture_CABLE )
  ELSE
    progs_cbl_vars%SoilMoisture_CABLE(:,:,:) = const_val
  END IF

CASE ( 'SnowDepth_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%SnowDepth_CABLE )
  ELSE
    progs_cbl_vars%SnowDepth_CABLE(:,:,:) = const_val
  END IF

CASE ( 'SnowAge_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%SnowAge_CABLE )
  ELSE
    progs_cbl_vars%SnowAge_CABLE(:,:) = const_val
  END IF

CASE ( 'SnowMass_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%SnowMass_CABLE )
  ELSE
    progs_cbl_vars%SnowMass_CABLE(:,:,:) = const_val
  END IF

CASE ( 'ThreeLayerSnowFlag_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%ThreeLayerSnowFlag_CABLE)
  ELSE
    progs_cbl_vars%ThreeLayerSnowFlag_CABLE(:,:) = const_val
  END IF

CASE ( 'SnowTemp_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%SnowTemp_CABLE )
  ELSE
    progs_cbl_vars%SnowTemp_CABLE(:,:,:) = const_val
  END IF

CASE ( 'SnowDensity_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%SnowDensity_CABLE)
  ELSE
    progs_cbl_vars%SnowDensity_CABLE(:,:,:) = const_val
  END IF

CASE ( 'OneLyrSnowDensity_CABLE' )
  IF ( PRESENT(cube) ) THEN
    cube_land = map_to_land(cube)
    CALL cube_get_data(cube_land, progs_cbl_vars%OneLyrSnowDensity_CABLE)
  ELSE
    progs_cbl_vars%OneLyrSnowDensity_CABLE(:,:) = const_val
  END IF

  !-----------------------------------------------------------------------------
  ! Variables for OASIS river coupling.
  !-----------------------------------------------------------------------------
CASE ( 'rivers_outflow_number' )
  IF ( PRESENT(cube) ) THEN
    CALL cube_get_data(cube, rivers%rivers_outflow_number)
  ELSE
    ! The functionality is not available for const_val and does not make
    ! scientific sense
    CALL log_fatal("populate_var",                                             &
                   "cube not present for rivers_outflow_number")
  END IF

CASE DEFAULT
  CALL log_fatal("populate_var",                                               &
                 "Unrecognised variable for input - '" //                      &
                 TRIM(get_string_identifier(var_id)) // "'. " //               &
                 "See docs for available variables")
END SELECT


! Free the land cube
CALL cube_free(cube_land)

RETURN

END SUBROUTINE populate_var
#endif
