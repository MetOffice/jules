#if !defined(RIVERS_ONLY)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE read_dump_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE read_dump(file_name, identifiers)

USE jules_fields_mod, ONLY: crop_vars, psparms, toppdm, ainfo, trif_vars,      &
                            soilecosse, progs, progs_data, trifctltype,        &
                            jules_vars, fluxes, rivers, water_resources
USE imgn_vars_mod, ONLY: imgn_vars


USE model_grid_mod, ONLY:                                                      &
  global_land_pts, grid_area_ij

USE ancil_info, ONLY:                                                          &
  land_pts, dim_cs1, dim_soil_n_pool, nsurft, nsoilt, dim_cslayer, nsoilt,     &
  nmasst

USE jules_surface_types_mod, ONLY:                                             &
  npft, ntype, ncpft

USE imogen_constants, ONLY:                                                    &
  n_olevs, nfarray

USE prognostics, ONLY:                                                         &
  l_broadcast_soilt

USE jules_irrig_mod, ONLY: frac_irrig_all_tiles, set_irrfrac_on_irrtiles

USE jules_snow_mod, ONLY:                                                      &
  nsmax

USE jules_soil_mod, ONLY:                                                      &
  sm_levels, ns_deep, l_tile_soil

USE jules_soil_biogeochem_mod, ONLY:                                           &
  dim_ch4layer

USE fire_mod, ONLY:                                                            &
  fire_prog

USE metstats_mod, ONLY:                                                        &
  metstats_prog

USE aero, ONLY:                                                                &
  co2_mmr

!Others
USE mpi, ONLY: mpi_real, mpi_logical, mpi_integer
USE jules_vars_mod, ONLY: mpi_local_comm

USE parallel_mod, ONLY:                                                        &
  master_task_id, is_master_task, scatter_land_field

USE file_mod, ONLY:                                                            &
  file_handle, file_read_var, file_close

USE dump_mod,         ONLY: ancil_dump_read
USE logging_mod,      ONLY: log_fatal

USE read_dump_shared_mod,     ONLY: read_dump_shared
USE read_dump_var_rivers_mod, ONLY: read_dump_var_rivers

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Check that the given file is a JULES dump compatible with the current
!   run, and read the given identifiers from it.
!   Note that the reading of the dump is done by the master task and the
!   results scattered to other tasks. This means that dumps written with
!   different amounts of tasks should be interchangable.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
CHARACTER(LEN=*) :: file_name  ! The dump file
CHARACTER(LEN=*) :: identifiers(:)  ! The model identifiers for the
                                    ! variables to define

! Work variables
TYPE(file_handle) :: FILE
  ! The opened file

INTEGER :: nvars
  ! The number of variables we are processing

INTEGER :: var_ids(SIZE(identifiers))
  ! The ids of the variables in the dump file

REAL :: frac_irrig_all_tiles_real
REAL :: set_irrfrac_irrtiles_real
REAL :: irrtiles_real(npft)
REAL :: nirrtile_real

! Real versions of integer valued variables
REAL :: nsnow_real(land_pts, nsurft)
REAL :: harvest_doy_real(land_pts, npft)
REAL :: years_since_harvest_real(land_pts, npft)
REAL, ALLOCATABLE :: seed_rain_real(:)
! The number of elements of the "seed_rain" array
INTEGER :: seed_rain_len

INTEGER :: i, j, m, n, s  ! Loop counters

INTEGER :: ERROR  ! Variable to collect MPI errors - most MPI
                  ! implementations bail on error, so this is not checked.


! Arrays to hold global land points version of data read in master task
! before scattering
REAL, ALLOCATABLE :: global_data_1d(:)     ! Data with no vertical levels
REAL, ALLOCATABLE :: global_data_2d(:,:)   ! With one vertical level
REAL, ALLOCATABLE :: global_data_3d(:,:,:) ! With two "vertical" levels
                                             ! e.g. snow variables
REAL, ALLOCATABLE :: global_data_4d(:,:,:,:) ! With 3 "vertical" levels

!-----------------------------------------------------------------------------

IF (ALLOCATED(progs_data%seed_rain)) THEN
  seed_rain_len = SIZE(progs_data%seed_rain)
ELSE
  seed_rain_len = 0
END IF

! Check that the given file is a dump compatible with the current
! run, and read the given identifiers from it.
CALL read_dump_shared(file_name, identifiers, nvars, FILE, var_ids)

!-----------------------------------------------------------------------------
! Set the requested variables from the file
!
! This is done by reading the value of the variable on global land points
! in the master task, then scattering it to the other tasks
!
! We assume that if the file passed all the checks on dimensions above, then
! it will be fine to fill variables here (i.e. we don't check the dimensions
! associated with the variables)
!-----------------------------------------------------------------------------
! Allocate the global data arrays
IF ( is_master_task() ) THEN
  ALLOCATE(global_data_1d(global_land_pts))
  ALLOCATE(global_data_2d(global_land_pts, MAX(npft, dim_cs1, sm_levels,       &
                                               nsurft, ntype, ns_deep,         &
                                               nsoilt, dim_ch4layer)))
  ALLOCATE(global_data_3d(global_land_pts,                                     &
                          MAX(nsurft, dim_cslayer, nsoilt, npft),              &
                          MAX(nsmax, dim_cs1, sm_levels, nmasst)))
  ALLOCATE(global_data_4d(global_land_pts, nsoilt,                             &
                          MAX(nsurft, dim_cslayer),                            &
                          MAX(nsmax, dim_cs1)))
ELSE
  ALLOCATE(global_data_1d(1))
  ALLOCATE(global_data_2d(1,1))
  ALLOCATE(global_data_3d(1,1,1))
  ALLOCATE(global_data_4d(1,1,1,1))
END IF

DO i = 1,nvars

  !---------------------------------------------------------------------------
  ! In the master task, read the global data
  !---------------------------------------------------------------------------
  IF ( is_master_task() ) THEN
    SELECT CASE ( identifiers(i) )

      !----------------------------------------------------------------------
      ! If it is a land_pts array with no levels associated, read into the
      ! global_data_1d array.
      !----------------------------------------------------------------------
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
           'fire_nesterov')
      CALL file_read_var(FILE, var_ids(i), global_data_1d)

      !----------------------------------------------------------------------
      ! If it is a variable with one or more levels,
      ! read the appropriate number of levels into global_data_2d/3d/4d.
      !----------------------------------------------------------------------
    CASE ( 'toppdm%sthzw_soilt', 'toppdm%zw_soilt' )
      CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:nsoilt))

    CASE ( 'canht', 'lai', 'years_since_harvest' , 'f_nsc' )
      CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:npft))

    CASE ( 'plantNumDensity' )
      CALL file_read_var(FILE, var_ids(i), global_data_3d(:,1:npft,1:nmasst))

    CASE ( 'cropdvi', 'croprootc', 'cropharvc', 'cropreservec',                &
           'croplai', 'cropcanht' )
      CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:ncpft))

    CASE ( 'sthuf', 't_soil', 'sthu_irr' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_2d(:,1:sm_levels))

    CASE ( 'sthuf_soilt', 't_soil_soilt', 'sthu_irr_soilt')
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_3d(:,1:nsoilt,1:sm_levels))

    CASE ( 'n_inorg' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_2d(:,1:dim_cslayer))

    CASE ( 'n_inorg_soilt' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_3d(:,1:nsoilt,1:dim_cslayer))

    CASE ( 'substr_ch4','mic_ch4','mic_act_ch4','acclim_ch4' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_2d(:,1:dim_ch4layer))

    CASE ( 'tsoil_deep' )
      CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:ns_deep))

    CASE ( 'canopy', 'nsnow', 'rgrain', 'rho_snow', 'snow_tile',               &
           'snow_depth', 'snow_grnd', 'tstar_tile', 'tsurf_elev_surft' )
      CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:nsurft))

    CASE ( 'rgrainl', 'snow_ds', 'snow_ice', 'snow_liq', 'tsnow' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_3d(:,1:nsurft,1:nsmax))

    CASE ( 'cs','ns', 'frac_c_label_pool' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_3d(:,1:dim_cslayer,1:dim_cs1))

    CASE ( 'cs_soilt', 'ns_soilt', 'frac_c_label_pool_soilt' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_4d(:,1:nsoilt,1:dim_cslayer,1:dim_cs1))

    CASE ( 'n_soil' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_3d(:,1:dim_cslayer,1:dim_soil_n_pool))

    CASE ( 'n_soil_soilt' )
      CALL file_read_var(FILE, var_ids(i),                                     &
                         global_data_4d(:,1:nsoilt,1:dim_cslayer,              &
                                        1:dim_soil_n_pool))

      ! Cases for IMOGEN variables
      ! Each task runs its own version of IMOGEN - these variables are
      ! broadcast to all tasks below.
    CASE ( 'co2_ppmv' )
      CALL file_read_var(FILE, var_ids(i), imgn_vars%co2_ppmv)

    CASE ( 'co2_change_ppmv' )
      CALL file_read_var(FILE, var_ids(i), imgn_vars%co2_change_ppmv)

    CASE ( 'dtemp_o' )
      CALL file_read_var(FILE, var_ids(i), imgn_vars%dtemp_o)

    CASE ( 'fa_ocean' )
      CALL file_read_var(FILE, var_ids(i), imgn_vars%fa_ocean)

    CASE ( 'seed_rain' )
      IF (ALLOCATED(progs_data%seed_rain)) THEN
        ALLOCATE(seed_rain_real(seed_rain_len))
        CALL file_read_var(FILE, var_ids(i), seed_rain_real)
      ELSE
        CALL log_fatal("read_dump",                                            &
                       "seed_rain needs to be allocated before use")
      END IF


    CASE ( 'ch4_ppbv' )
      CALL file_read_var(FILE, var_ids(i), imgn_vars%ch4_ppbv)

      !----------------------------------------------------------------------
      ! Ancillary variables
      !----------------------------------------------------------------------

      ! latlon ancil namelist
    CASE ( 'grid_area' )
      IF ( ancil_dump_read%latlon ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_1d)
      END IF

      !Frac ancil namelist
    CASE ( 'frac' )
      IF ( ancil_dump_read%frac ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:ntype))
      END IF

      ! Vegetation properties ancil namelist
    CASE ( 't_home_gb' )
      IF ( ancil_dump_read%vegetation_props ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_1d)
      END IF

      !Soil properties ancil namelist
    CASE ( 'b      ', 'sathh  ', 'satcon ', 'sm_sat ', 'sm_crit',              &
           'sm_wilt', 'hcap   ', 'hcon   ' )
      IF ( ancil_dump_read%soil_props ) THEN
        CALL file_read_var(FILE, var_ids(i),                                   &
                           global_data_2d(:,1:sm_levels))
      END IF

    CASE ( 'b_soilt', 'sathh_soilt', 'satcon_soilt', 'sm_sat_soilt',           &
           'sm_crit_soilt', 'sm_wilt_soilt', 'hcap_soilt', 'hcon_soilt' )
      IF ( ancil_dump_read%soil_props ) THEN
        CALL file_read_var(FILE, var_ids(i),                                   &
                           global_data_3d(:,1:nsoilt,1:sm_levels))
      END IF

    CASE ( 'albsoil' )
      IF ( ancil_dump_read%soil_props ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_1d)
      END IF

    CASE ( 'albsoil_soilt' )
      IF ( ancil_dump_read%soil_props ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:nsoilt))
      END IF

    CASE ( 'clay', 'soil_ph' )
      IF ( ancil_dump_read%soil_props ) THEN
        CALL file_read_var(FILE, var_ids(i),                                   &
                           global_data_2d(:,1:dim_cslayer))
      END IF

    CASE ( 'clay_soilt', 'soil_ph_soilt' )
      IF ( ancil_dump_read%soil_props ) THEN
        CALL file_read_var(FILE, var_ids(i),                                   &
                           global_data_3d(:,1:nsoilt,1:dim_cslayer))
      END IF

      !Topmodel ancil namelist
    CASE ( 'fexp   ', 'ti_mean', 'ti_sig ' )
      IF ( ancil_dump_read%top ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_1d)
      END IF

    CASE ( 'toppdm%fexp_soilt', 'toppdm%ti_mean_soilt', 'toppdm%ti_sig_soilt' )
      IF ( ancil_dump_read%top ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:nsoilt))
      END IF

      !Agric ancil namelist
    CASE ( 'frac_agr', 'frac_past', 'frac_biocrop' )
      IF ( ancil_dump_read%agric ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_1d)
      END IF

      !Crop props ancillaries namelist
    CASE ( 'cropsowdate', 'cropttveg  ', 'cropttrep  ','croplatestharvdate')
      IF ( ancil_dump_read%crop_props ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:ncpft))
      END IF

    CASE ( 'harvest_doy' )
      IF ( ancil_dump_read%biocrop ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_2d(:,1:npft))
      END IF

      !Irrigation ancillaries namelist
    CASE ( 'frac_irrig', 'irrfrac_irrtiles' )
      IF ( ancil_dump_read%irrig ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_1d)
      END IF

    CASE ( 'frac_irr_all_tiles' )
      IF ( ancil_dump_read%irrig ) THEN
        CALL file_read_var(FILE, var_ids(i), frac_irrig_all_tiles_real)
      END IF

    CASE ( 'irrtiles' )
      IF ( ancil_dump_read%irrig ) THEN
        CALL file_read_var(FILE, var_ids(i), irrtiles_real)
      END IF

    CASE ( 'nirrtile' )
      IF ( ancil_dump_read%irrig ) THEN
        CALL file_read_var(FILE, var_ids(i), nirrtile_real)
      END IF

    CASE ( 'set_irrfrac_on_irrtiles' )
      IF ( ancil_dump_read%irrig ) THEN
        CALL file_read_var(FILE, var_ids(i), set_irrfrac_irrtiles_real)
      END IF

      ! Water resources properties ancil namelist
    CASE ( 'conv_loss_frac', 'sfc_water_frac' )
      IF ( ancil_dump_read%water_resources_props ) THEN
        CALL file_read_var(FILE, var_ids(i), global_data_1d)
      END IF

      !CO2 ancil namelist
    CASE ( 'co2_mmr' )
      IF ( ancil_dump_read%co2 ) THEN
        CALL file_read_var(FILE, var_ids(i), co2_mmr)
      END IF

    CASE DEFAULT
      ! Cases for river routing variables plus log fatal for unknown identifier
      CALL read_dump_var_rivers( identifiers(i), FILE, var_ids(i) )
    END SELECT
  END IF  ! MASTER TASK

  !---------------------------------------------------------------------------
  ! Now scatter the variables into their final destinations.
  ! Note that scatter_land_field can only scatter one land_pts array at a time
  ! so to scatter variables with multiple levels we must loop
  !---------------------------------------------------------------------------
  SELECT CASE ( identifiers(i) )
  CASE ( 'gs' )
    CALL scatter_land_field(global_data_1d, progs%gs_gb)

  CASE ( 't_growth_gb' )
    CALL scatter_land_field(global_data_1d, progs%t_growth_gb)

  CASE ( 'sthzw' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO m = 1, nsoilt
        CALL scatter_land_field(global_data_1d, toppdm%sthzw_soilt(:,m))
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      CALL scatter_land_field(global_data_1d, toppdm%sthzw_soilt(:,1))
    END IF

  CASE ( 'toppdm%sthzw_soilt' )
    DO m = 1, nsoilt
      CALL scatter_land_field(global_data_2d(:,m), toppdm%sthzw_soilt(:,m))
    END DO

  CASE ( 'zw' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO m = 1, nsoilt
        CALL scatter_land_field(global_data_1d, toppdm%zw_soilt(:,m))
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      CALL scatter_land_field(global_data_1d, toppdm%zw_soilt(:,1))
    END IF

  CASE ( 'toppdm%zw_soilt' )
    DO m = 1,nsoilt
      CALL scatter_land_field(global_data_2d(:,m), toppdm%zw_soilt(:,m))
    END DO

  CASE ( 'cv' )
    CALL scatter_land_field(global_data_1d, trifctltype%cv_gb)

  CASE ( 'frac_agr_prev' )
    CALL scatter_land_field(global_data_1d, progs%frac_agr_prev_gb)

  CASE ( 'frac_past_prev' )
    CALL scatter_land_field(global_data_1d, progs%frac_past_prev_gb)

  CASE ( 'frac_biocrop_prev' )
    CALL scatter_land_field(global_data_1d, progs%frac_biocrop_prev_gb)

  CASE ( 'wood_prod_fast' )
    CALL scatter_land_field(global_data_1d, progs%wood_prod_fast_gb)

  CASE ( 'wood_prod_med' )
    CALL scatter_land_field(global_data_1d, progs%wood_prod_med_gb)

  CASE ( 'wood_prod_slow' )
    CALL scatter_land_field(global_data_1d, progs%wood_prod_slow_gb)

  CASE ( 'n_inorg' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO m = 1, nsoilt
        DO n = 1,dim_cslayer
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  progs%n_inorg_soilt_lyrs(:,m,n))
        END DO
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      DO n = 1,dim_cslayer
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                progs%n_inorg_soilt_lyrs(:,1,n))
      END DO
    END IF

  CASE ( 'n_inorg_soilt' )
    DO m = 1, nsoilt
      DO n = 1,dim_cslayer
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                progs%n_inorg_soilt_lyrs(:,m,n))
      END DO
    END DO

  CASE ( 'substr_ch4' )
    DO n = 1,dim_ch4layer
      CALL scatter_land_field(global_data_2d(:,n), progs%substr_ch4(:,n))
    END DO

  CASE ( 'mic_ch4' )
    DO n = 1,dim_ch4layer
      CALL scatter_land_field(global_data_2d(:,n), progs%mic_ch4(:,n))
    END DO

  CASE ( 'mic_act_ch4' )
    DO n = 1,dim_ch4layer
      CALL scatter_land_field(global_data_2d(:,n), progs%mic_act_ch4(:,n))
    END DO

  CASE ( 'acclim_ch4' )
    DO n = 1,dim_ch4layer
      CALL scatter_land_field(global_data_2d(:,n), progs%acclim_ch4(:,n))
    END DO

  CASE ( 'canht' )
    DO n = 1,npft
      CALL scatter_land_field(global_data_2d(:,n), progs%canht_pft(:,n))
    END DO

  CASE ( 'lai' )
    DO n = 1,npft
      CALL scatter_land_field(global_data_2d(:,n), progs%lai_pft(:,n))
    END DO

  CASE ( 'f_nsc' )
    DO n = 1,npft
      CALL scatter_land_field(global_data_2d(:,n), progs%f_nsc_pft(:,n))
    END DO

  CASE ( 'plantNumDensity' )
    DO n = 1,npft
      DO m = 1,nmasst
        CALL scatter_land_field(global_data_3d(:,n,m),                         &
                                progs%plantNumDensity(:,n,m))
      END DO
    END DO

  CASE ( 'years_since_harvest' )
    DO n = 1,npft
      CALL scatter_land_field(global_data_2d(:,n),                             &
                              years_since_harvest_real(:,n))
    END DO
    progs%years_since_harvest(:,:) = NINT(years_since_harvest_real(:,:))

  CASE ( 'cropdvi' )
    DO n = 1,ncpft
      CALL scatter_land_field(global_data_2d(:,n), crop_vars%dvi_cpft(:,n))
    END DO

  CASE ( 'croprootc' )
    DO n = 1,ncpft
      CALL scatter_land_field(global_data_2d(:,n), crop_vars%rootc_cpft(:,n))
    END DO

  CASE ( 'cropharvc' )
    DO n = 1,ncpft
      CALL scatter_land_field(global_data_2d(:,n), crop_vars%harvc_cpft(:,n))
    END DO

  CASE ( 'cropreservec' )
    DO n = 1,ncpft
      CALL scatter_land_field(global_data_2d(:,n), crop_vars%reservec_cpft(:,n))
    END DO

  CASE ( 'croplai' )
    DO n = 1,ncpft
      CALL scatter_land_field(global_data_2d(:,n), crop_vars%croplai_cpft(:,n))
    END DO

  CASE ( 'cropcanht' )
    DO n = 1,ncpft
      CALL scatter_land_field(global_data_2d(:,n), crop_vars%cropcanht_cpft(:,n))
    END DO

  CASE ( 'cs' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO s = 1,nsoilt !using s rather than m as usual
        DO n = 1,dim_cs1
          DO m = 1,dim_cslayer
            CALL scatter_land_field(global_data_3d(:,m,n),                     &
                                    progs%cs_pool_soilt(:,s,m,n))
          END DO
        END DO
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      DO n = 1,dim_cs1
        DO m = 1,dim_cslayer
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  progs%cs_pool_soilt(:,1,m,n))
        END DO
      END DO
    END IF

  CASE ( 'frac_c_label_pool' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO s = 1,nsoilt !using s rather than m as usual
        DO n = 1,dim_cs1
          DO m = 1,dim_cslayer
            CALL scatter_land_field(global_data_3d(:,m,n),                     &
                                    progs%frac_c_label_pool_soilt(:,s,m,n))
          END DO
        END DO
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      DO n = 1,dim_cs1
        DO m = 1,dim_cslayer
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  progs%frac_c_label_pool_soilt(:,1,m,n))
        END DO
      END DO
    END IF

  CASE ( 'cs_soilt' )
    DO s = 1,nsoilt !using s rather than m as usual
      DO n = 1,dim_cs1
        DO m = 1,dim_cslayer
          CALL scatter_land_field(global_data_4d(:,s,m,n),                     &
                                  progs%cs_pool_soilt(:,s,m,n))
        END DO
      END DO
    END DO

  CASE ( 'frac_c_label_pool_soilt' )
    DO s = 1,nsoilt !using s rather than m as usual
      DO n = 1,dim_cs1
        DO m = 1,dim_cslayer
          CALL scatter_land_field(global_data_4d(:,s,m,n),                     &
                                  progs%frac_c_label_pool_soilt(:,s,m,n))
        END DO
      END DO
    END DO

  CASE ( 'ns' )
    DO n = 1,dim_cs1
      DO m = 1,dim_cslayer
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                progs%ns_pool_gb(:,m,n))
      END DO
    END DO

  CASE ( 'sthuf' )
    ! sthuf is held in sthu until it is processed
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  jules_vars%sthuf_soilt(:,m,n))
        END DO
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      DO n = 1,sm_levels
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                jules_vars%sthuf_soilt(:,1,n))
      END DO
    END IF

  CASE ( 'sthuf_soilt' )
    ! sthuf is held in sthu until it is processed
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                jules_vars%sthuf_soilt(:,m,n))
      END DO
    END DO

  CASE ( 't_soil' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  progs%t_soil_soilt(:,m,n))
        END DO
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      DO n = 1,sm_levels
        CALL scatter_land_field(global_data_2d(:,n), progs%t_soil_soilt(:,1,n))
      END DO
    END IF

  CASE ( 't_soil_soilt' )
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                progs%t_soil_soilt(:,m,n))
      END DO
    END DO

  CASE ( 'tsoil_deep' )
    DO n = 1,ns_deep
      CALL scatter_land_field(global_data_2d(:,n), progs%tsoil_deep_gb(:,n))
    END DO

  CASE ( 'sthu_irr' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  crop_vars%sthu_irr_soilt(:,m,n))
        END DO
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      DO n = 1,sm_levels
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                crop_vars%sthu_irr_soilt(:,1,n))
      END DO
    END IF

  CASE ( 'sthu_irr_soilt' )
    DO m = 1,nsoilt
      DO n = 1,sm_levels
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                crop_vars%sthu_irr_soilt(:,m,n))
      END DO
    END DO

  CASE ( 'canopy' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n), progs%canopy_surft(:,n))
    END DO

  CASE ( 'nsnow' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n), nsnow_real(:,n))
    END DO
    progs%nsnow_surft(:,:) = NINT(nsnow_real(:,:))

  CASE ( 'rgrain' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n), progs%rgrain_surft(:,n))
    END DO

  CASE ( 'rho_snow' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n),                             &
                              progs%rho_snow_grnd_surft(:,n))
    END DO

  CASE ( 'snow_tile' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n), progs%snow_surft(:,n))
    END DO

  CASE ( 'snow_depth' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n),                             &
                              progs%snowdepth_surft(:,n))
    END DO

  CASE ( 'snow_grnd' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n), progs%snow_grnd_surft(:,n))
    END DO

  CASE ( 'tstar_tile' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n), progs%tstar_surft(:,n))
    END DO

  CASE ( 'tsurf_elev_surft' )
    DO n = 1,nsurft
      CALL scatter_land_field(global_data_2d(:,n),                             &
                              progs%tsurf_elev_surft(:,n))
    END DO

  CASE ( 'rgrainl' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                progs%rgrainl_surft(:,m,n))
      END DO
    END DO

  CASE ( 'snow_ds' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL scatter_land_field(global_data_3d(:,m,n), progs%ds_surft(:,m,n))
      END DO
    END DO

  CASE ( 'snow_ice' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                progs%sice_surft(:,m,n))
      END DO
    END DO


  CASE ( 'snow_liq' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                progs%sliq_surft(:,m,n))
      END DO
    END DO


  CASE ( 'tsnow' )
    DO n = 1,nsmax
      DO m = 1,nsurft
        CALL scatter_land_field(global_data_3d(:,m,n),                         &
                                progs%tsnow_surft(:,m,n))
      END DO
    END DO

    ! Fire and metstats
  CASE ( 'temp_max_00h_r' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%temp_max_00h%run)

  CASE ( 'temp_ave_00h_r' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%temp_ave_00h%run)

  CASE ( 'prec_tot_00h_r' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%prec_tot_00h%run)

  CASE ( 'prec_tot_12h_r' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%prec_tot_12h%run)

  CASE ( 'rhum_min_00h_r' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%rhum_min_00h%run)

  CASE ( 'dewp_ave_00h_r' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%dewp_ave_00h%run)

  CASE ( 'wind_ave_00h_r' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%wind_ave_00h%run)

  CASE ( 'temp_max_00h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%temp_max_00h%fin)

  CASE ( 'temp_ave_00h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%temp_ave_00h%fin)

  CASE ( 'temp_ave_nday' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%temp_ave_nday%fin)

  CASE ( 'temp_pnt_12h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%temp_pnt_12h%fin)

  CASE ( 'prec_tot_00h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%prec_tot_00h%fin)

  CASE ( 'prec_tot_12h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%prec_tot_12h%fin)

  CASE ( 'rhum_min_00h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%rhum_min_00h%fin)

  CASE ( 'rhum_pnt_12h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%rhum_pnt_12h%fin)

  CASE ( 'dewp_ave_00h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%dewp_ave_00h%fin)

  CASE ( 'wind_ave_00h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%wind_ave_00h%fin)

  CASE ( 'wind_pnt_12h' )
    CALL scatter_land_field(global_data_1d,                                    &
                            metstats_prog(:)%wind_pnt_12h%fin)

    ! Fire module variables- land points only
  CASE ( 'fire_mcarthur_r_dr' )
    CALL scatter_land_field(global_data_1d, fire_prog(:)%mcarthur%r_dr)

  CASE ( 'fire_mcarthur_n_dr' )
    CALL scatter_land_field(global_data_1d, fire_prog(:)%mcarthur%n_dr)

  CASE ( 'fire_canadian_ffmc' )
    CALL scatter_land_field(global_data_1d, fire_prog(:)%canadian%ffmc)

  CASE ( 'fire_canadian_ffmc_mois' )
    CALL scatter_land_field(global_data_1d,                                    &
                            fire_prog(:)%canadian%ffmc_mois)

  CASE ( 'fire_canadian_dmc' )
    CALL scatter_land_field(global_data_1d, fire_prog(:)%canadian%dmc)

  CASE ( 'fire_canadian_dc' )
    CALL scatter_land_field(global_data_1d, fire_prog(:)%canadian%dc)

  CASE ( 'fire_nesterov' )
    CALL scatter_land_field(global_data_1d, fire_prog(:)%nesterov%findex)

    ! ECOSSE variables.
  CASE ( 'n_soil' )
    IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
      DO s = 1,nsoilt !using s rather than m as usual
        DO n = 1,dim_soil_n_pool
          DO m = 1,dim_cslayer
            CALL scatter_land_field(global_data_3d(:,m,n),                     &
                                    soilecosse%n_soil_pool_soilt(:,s,m,n))
          END DO
        END DO
      END DO
    ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
      DO n = 1,dim_soil_n_pool
        DO m = 1,dim_cslayer
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  soilecosse%n_soil_pool_soilt(:,1,m,n))
        END DO
      END DO
    END IF

  CASE ( 'n_soil_soilt' )
    DO s = 1,nsoilt !using s rather than m as usual
      DO n = 1,dim_soil_n_pool
        DO m = 1,dim_cslayer
          CALL scatter_land_field(global_data_4d(:,s,m,n),                     &
                                  soilecosse%n_soil_pool_soilt(:,s,m,n))
        END DO
      END DO
    END DO

    ! IMOGEN variables are just broadcast to all tasks
  CASE ( 'co2_ppmv' )
    CALL mpi_bcast(imgn_vars%co2_ppmv, 1, mpi_real,                            &
                   master_task_id, mpi_local_comm, ERROR)

  CASE ( 'co2_change_ppmv' )
    CALL mpi_bcast(imgn_vars%co2_change_ppmv, 1, mpi_real,                     &
                   master_task_id, mpi_local_comm, ERROR)

  CASE ( 'dtemp_o' )
    CALL mpi_bcast(imgn_vars%dtemp_o, n_olevs, mpi_real,                       &
                   master_task_id, mpi_local_comm, ERROR)

  CASE ( 'fa_ocean' )
    CALL mpi_bcast(imgn_vars%fa_ocean, nfarray, mpi_real,                      &
                   master_task_id, mpi_local_comm, ERROR)

  CASE ( 'seed_rain' )
    IF (ALLOCATED(progs_data%seed_rain)) THEN
      ! The _real version should be allocated on the "master" task but not
      ! the other tasks.
      IF ( .NOT. ALLOCATED(seed_rain_real)) THEN
        ALLOCATE(seed_rain_real(seed_rain_len))
      END IF
      ! The "master" task has read the field into seed_rain_real
      CALL mpi_bcast(seed_rain_real, seed_rain_len, mpi_real,                  &
                     master_task_id, mpi_local_comm, ERROR)
      progs_data%seed_rain(:) = NINT(seed_rain_real(:))
      DEALLOCATE(seed_rain_real)
    ELSE
      CALL log_fatal("read_dump",                                              &
          "Tried to read seed_rain but internal array was not allocated")
    END IF
  CASE ( 'ch4_ppbv' )
    CALL mpi_bcast(imgn_vars%ch4_ppbv, 1, mpi_real,                            &
                   master_task_id, mpi_local_comm, ERROR)

    ! River routing variables
  CASE ( 'flood_flow', 'flood_flow_prev', 'flood_storage',                     &
         'flood_storage_prev', 'minor_res_storage',                            &
         'rfm_bflowin_rp', 'rfm_flowin_rp',                                    &
         'rfm_substore_rp', 'rfm_surfstore_rp', 'river_channel_storage',       &
         'river_channel_flow', 'river_depth_prev', 'river_flow_prev',          &
         'rivers_outflow_rp', 'rivers_sto_rp', 'inland_outflow_rp' )
    ! nothing to do

    !-------------------------------------------------------------------------
    ! Ancillary variables
    !-------------------------------------------------------------------------

    ! latlon ancil namelist
  CASE ( 'grid_area' )
    IF ( ancil_dump_read%latlon ) THEN
      CALL scatter_land_field(global_data_1d, grid_area_ij)
    END IF

    !Frac ancil namelist
  CASE ( 'frac' )
    IF ( ancil_dump_read%frac ) THEN
      DO n = 1,ntype
        CALL scatter_land_field(global_data_2d(:,n), ainfo%frac_surft(:,n))
      END DO
    END IF

    ! Vegetation properties ancil namelist
  CASE ( 't_home_gb' )
    IF ( ancil_dump_read%vegetation_props ) THEN
      CALL scatter_land_field(global_data_1d, progs%t_home_gb)
    END IF

    !Soil properties ancil namelist
    !Cases if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1

  CASE ( 'b      ' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%bexp_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%bexp_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'b_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%bexp_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'sathh  ' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%sathh_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%sathh_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'sathh_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%sathh_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'satcon ' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%satcon_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%satcon_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'satcon_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%satcon_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'sm_sat ' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%smvcst_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%smvcst_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'sm_sat_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%smvcst_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'sm_crit' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%smvccl_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%smvccl_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'sm_crit_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%smvccl_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'sm_wilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%smvcwt_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%smvcwt_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'sm_wilt_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%smvcwt_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'hcap   ' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%hcap_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%hcap_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'hcap_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%hcap_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'hcon   ' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          DO n = 1,sm_levels
            CALL scatter_land_field(global_data_2d(:,n),                       &
                                    psparms%hcon_soilt(:,m,n))
          END DO
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_2d(:,n),                         &
                                  psparms%hcon_soilt(:,1,n))
        END DO
      END IF
    END IF

  CASE ( 'hcon_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,sm_levels
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%hcon_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'albsoil' )
    IF ( ancil_dump_read%soil_props ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          CALL scatter_land_field(global_data_1d, psparms%albsoil_soilt(:,m))
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        CALL scatter_land_field(global_data_1d, psparms%albsoil_soilt(:,1))
      END IF
    END IF

  CASE ( 'albsoil_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        CALL scatter_land_field(global_data_2d(:,m), psparms%albsoil_soilt(:,m))
      END DO
    END IF

  CASE ( 'clay' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO n = 1,dim_cslayer
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                psparms%clay_soilt(:,1,n))
      END DO
    END IF

  CASE ( 'clay_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,dim_cslayer
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%clay_soilt(:,m,n))
        END DO
      END DO
    END IF

  CASE ( 'soil_ph' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO n = 1,dim_cslayer
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                psparms%soil_ph_soilt(:,1,n))
      END DO
    END IF

  CASE ( 'soil_ph_soilt' )
    IF ( ancil_dump_read%soil_props ) THEN
      DO m = 1,nsoilt
        DO n = 1,dim_cslayer
          CALL scatter_land_field(global_data_3d(:,m,n),                       &
                                  psparms%soil_ph_soilt(:,m,n))
        END DO
      END DO
    END IF

    !Topmodel ancil namelist
  CASE ( 'fexp   ' )
    IF ( ancil_dump_read%top ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          CALL scatter_land_field(global_data_1d, toppdm%fexp_soilt(:,m))
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        CALL scatter_land_field(global_data_1d, toppdm%fexp_soilt(:,1))
      END IF
    END IF

  CASE ( 'toppdm%fexp_soilt' )
    IF ( ancil_dump_read%top ) THEN
      DO m = 1,nsoilt
        CALL scatter_land_field(global_data_2d(:,m), toppdm%fexp_soilt(:,m))
      END DO
    END IF

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'ti_mean' )
    IF ( ancil_dump_read%top ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          CALL scatter_land_field(global_data_1d, toppdm%ti_mean_soilt(:,m))
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        CALL scatter_land_field(global_data_1d, toppdm%ti_mean_soilt(:,1))
      END IF
    END IF

  CASE ( 'toppdm%ti_mean_soilt' )
    IF ( ancil_dump_read%top ) THEN
      DO m = 1,nsoilt
        CALL scatter_land_field(global_data_2d(:,m), toppdm%ti_mean_soilt(:,m))
      END DO
    END IF

    !Case if nsoilt == 1, so it is OK to hardwire the 2nd dimension to 1
  CASE ( 'ti_sig ' )
    IF ( ancil_dump_read%top ) THEN
      IF ( l_tile_soil .AND. l_broadcast_soilt ) THEN
        DO m = 1,nsoilt
          CALL scatter_land_field(global_data_1d, toppdm%ti_sig_soilt(:,m))
        END DO
      ELSE !Case if nsoilt == 1, so OK to hardwire the 2nd dimension to 1
        CALL scatter_land_field(global_data_1d, toppdm%ti_sig_soilt(:,1))
      END IF
    END IF

  CASE ( 'toppdm%ti_sig_soilt' )
    IF ( ancil_dump_read%top ) THEN
      DO m = 1,nsoilt
        CALL scatter_land_field(global_data_2d(:,m), toppdm%ti_sig_soilt(:,m))
      END DO
    END IF

    !Agric ancil namelist
  CASE ( 'frac_agr' )
    IF ( ancil_dump_read%agric ) THEN
      CALL scatter_land_field(global_data_1d, trifctltype%frac_agr_gb)
    END IF

  CASE ( 'frac_past' )
    IF ( ancil_dump_read%agric ) THEN
      CALL scatter_land_field(global_data_1d, trif_vars%frac_past_gb)
    END IF

  CASE ( 'frac_biocrop' )
    IF ( ancil_dump_read%agric ) THEN
      CALL scatter_land_field(global_data_1d, trif_vars%frac_biocrop_gb)
    END IF

    !Biocrop ancillaries namelist
  CASE ( 'harvest_doy' )
    IF ( ancil_dump_read%biocrop ) THEN
      DO n = 1,npft
        CALL scatter_land_field(global_data_2d(:,n), harvest_doy_real(:,n))
      END DO
      trif_vars%harvest_doy(:,:) = NINT(harvest_doy_real(:,:))
    END IF

    !Crop props ancillaries namelist
  CASE ( 'cropsowdate       ' )
    IF ( ancil_dump_read%crop_props ) THEN
      DO n = 1,ncpft
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                crop_vars%sow_date_cpft(:,n))
      END DO
    END IF

  CASE ( 'croplatestharvdate' )
    IF ( ancil_dump_read%crop_props ) THEN
      DO n = 1,ncpft
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                crop_vars%latestharv_date_cpft(:,n))
      END DO
    END IF

  CASE ( 'cropttveg         ' )
    IF ( ancil_dump_read%crop_props ) THEN
      DO n = 1,ncpft
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                crop_vars%tt_veg_cpft(:,n))
      END DO
    END IF

  CASE ( 'cropttrep         ' )
    IF ( ancil_dump_read%crop_props ) THEN
      DO n = 1,ncpft
        CALL scatter_land_field(global_data_2d(:,n),                           &
                                crop_vars%tt_rep_cpft(:,n))
      END DO
    END IF

    !Irrigation ancillaries namelist
  CASE ( 'frac_irrig' )
    IF ( ancil_dump_read%irrig ) THEN
      CALL scatter_land_field(global_data_1d,                                  &
                              crop_vars%frac_irr_all(:,1))
    END IF

  CASE ( 'frac_irr_all_tiles' )
    IF ( ancil_dump_read%irrig ) THEN
      IF ( frac_irrig_all_tiles_real > 0.5 ) THEN
        frac_irrig_all_tiles = .TRUE.
      ELSE
        frac_irrig_all_tiles = .FALSE.
      END IF
      CALL mpi_bcast(frac_irrig_all_tiles, 1, mpi_logical,                     &
           master_task_id, mpi_local_comm, ERROR)
    END IF

  CASE ( 'irrfrac_irrtiles' )
    IF ( ancil_dump_read%irrig ) THEN
      CALL scatter_land_field(global_data_1d, crop_vars%irrfrac_irrtiles(:,1))
    END IF

  CASE ( 'set_irrfrac_on_irrtiles' )
    IF ( ancil_dump_read%irrig ) THEN
      IF ( set_irrfrac_irrtiles_real > 0.5 ) THEN
        set_irrfrac_on_irrtiles = .TRUE.
      ELSE
        set_irrfrac_on_irrtiles = .FALSE.
      END IF
      CALL mpi_bcast(set_irrfrac_on_irrtiles, 1, mpi_logical,                  &
           master_task_id, mpi_local_comm, ERROR)
    END IF

  CASE ( 'irrtiles' )
    IF ( ancil_dump_read%irrig ) THEN
      CALL mpi_bcast(NINT(irrtiles_real), npft, mpi_integer,                   &
           master_task_id, mpi_local_comm, ERROR)
    END IF

  CASE ( 'nirrtile' )
    IF ( ancil_dump_read%irrig ) THEN
      CALL mpi_bcast(NINT(nirrtile_real), 1, mpi_integer,                      &
           master_task_id, mpi_local_comm, ERROR)
    END IF

    ! Water resources properties ancil namelist
  CASE ( 'conv_loss_frac' )
    IF ( ancil_dump_read%water_resources_props ) THEN
      CALL scatter_land_field(global_data_1d, water_resources%conv_loss_frac)
    END IF

  CASE ( 'sfc_water_frac' )
    IF ( ancil_dump_read%water_resources_props ) THEN
      CALL scatter_land_field(global_data_1d, water_resources%sfc_water_frac)
    END IF

    !CO2 ancil namelist
  CASE ( 'co2_mmr' )
    IF ( ancil_dump_read%co2 ) THEN
      CALL mpi_bcast(co2_mmr, 1, mpi_real,                                     &
           master_task_id, mpi_local_comm, ERROR)
    END IF

  CASE DEFAULT
    CALL log_fatal("read_dump",                                                &
                   "Unexpected variable in dump - " //                         &
                   TRIM(identifiers(i)))
  END SELECT

END DO

! We are done with the file
IF ( is_master_task() ) CALL file_close(FILE)

IF ( ALLOCATED(global_data_1d) ) DEALLOCATE(global_data_1d)
IF ( ALLOCATED(global_data_2d) ) DEALLOCATE(global_data_2d)
IF ( ALLOCATED(global_data_3d) ) DEALLOCATE(global_data_3d)
IF ( ALLOCATED(global_data_4d) ) DEALLOCATE(global_data_4d)

RETURN

END SUBROUTINE read_dump
END MODULE read_dump_mod
#endif
