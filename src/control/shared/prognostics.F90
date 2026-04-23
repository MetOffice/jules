! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module containing all of the prognostic variables

MODULE prognostics

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Description
! Module containing all of the prognostic variables,
! i.e.those required to be kept from one timestep
! to another. Variables all appear in a model dump - NOT AT PRESENT!
! And some of these are not prognostics (eg smc)....

! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

! Implementation:
! Each variable is declared in both the 'data' TYPE and the 'pointer' type.
! Instances of these types are declared at at high level as required
! This is to facilitate advanced memory management features, which are generally
! not visible in the science code.
! Checklist for adding a new variable:
! -add to data_type
! -add to pointer_type
! -add to the allocate routine, passing in any new dimension sizes required
!  by argument (not via USE statement)
! -add to the deallocate routine
! -add to the assoc and nullify routines

TYPE :: progs_data_type
  INTEGER, ALLOCATABLE :: nsnow_surft(:,:)
    ! Number of snow layers on ground on tiles
  INTEGER, ALLOCATABLE :: seed_rain(:)
    ! Seeding number for subdaily rainfall for use
    ! in IMOGEN or when l_daily_disagg = T
  INTEGER, ALLOCATABLE :: years_since_harvest(:,:)
    ! Number of years since the previous harvest occurred
    ! for each land point and each PFT
    ! only used if l_trif_biocrop=T and harvest_type_io=2

  REAL(KIND=real_jlslsm), ALLOCATABLE :: tsoil_deep_gb(:,:)
    ! Deep soil temperatures (K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice_surft(:,:,:)
    ! Snow layer ice mass on tiles (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sliq_surft(:,:,:)
    ! Snow layer liquid mass on tiles (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snowdepth_surft(:,:)
    ! Snow depth on ground on tiles (m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tsnow_surft(:,:,:)
    ! Snow layer temperature (K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rgrainl_surft(:,:,:)
    ! Snow layer grain size on tiles (microns)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rho_snow_grnd_surft(:,:)
    ! Snowpack bulk density (kg/m3)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rho_snow_surft(:,:,:)
    ! Snow layer densities (m)
    ! The status of this as a prognostic needs to be reviewed. It is in the UM
    ! dump but not the JULES-standalone dump
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ds_surft(:,:,:)
    ! Snow layer thickness (m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wood_prod_fast_gb(:)
    ! Fast-turnover wood product C pool (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wood_prod_med_gb(:)
    ! Medium-turnover wood product C pool (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: wood_prod_slow_gb(:)
    ! Slow-turnover wood product C pool (kg m-2).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_agr_prev_gb(:)
    ! Agricultural fraction from previous TRIFFID call
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_past_prev_gb(:)
    ! Pasture fraction from previous TRIFFID call
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_biocrop_prev_gb(:)
    ! Biocrop fraction from previous TRIFFID call
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_inorg_soilt_lyrs(:,:,:)
    ! Gridbox Inorganic N pool on soil levels (kg N/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_inorg_gb(:)
    ! Gridbox Inorganic N pool (kg N/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: n_inorg_avail_pft(:,:,:)
    ! Available inorganic N for PFTs (depends on roots) (kg N/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ns_pool_gb(:,:,:)
    ! Soil Organic Nitrogen (kg N/m2)
    ! If dim_cs1=1, there is a single soil C pool.
    ! If dim_cs1=4, the pools are:
    ! 1  decomposable plant material
    ! 2  resistant plant material
    ! 3  biomass
    ! 4  humus
  REAL(KIND=real_jlslsm), ALLOCATABLE :: substr_ch4(:,:,:)
    ! Dissolved substrate that methanogens consume (kg C/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: mic_ch4(:,:,:)
    ! Methanogenic biomass (kg C/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: mic_act_ch4(:,:,:)
    ! Activity level of methanogenic biomass (fraction)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: timewet_lyr(:,:,:)
    ! Time that each methane-producing layer has been saturated (s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: acclim_ch4(:,:)
    ! Acclimation factor for microbial trait adaptation
  REAL(KIND=real_jlslsm), ALLOCATABLE :: triffid_co2_gb(:)
    ! Atmospheric CO2 fluxes from TRIFFID (kgC/m2/yr)
    ! exudates + wood product pool flux + harvest for adding to the
    ! atmosphere in interactive CO2 runs
  REAL(KIND=real_jlslsm), ALLOCATABLE :: canht_pft(:,:)
    ! Canopy height (m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: canopy_surft(:,:)
    ! Surface/canopy water for snow-free land tiles (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: canopy_gb(:)
    ! Gridbox canopy water content (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: cs_pool_soilt(:,:,:,:)
    ! Soil carbon (kg C/m2)
    ! If dim_cs1=1, there is a single soil C pool per layer.
    ! If dim_cs1=4, the pools are:
    ! 1  decomposable plant material
    ! 2  resistant plant material
    ! 3  biomass
    ! 4  humus
  REAL(KIND=real_jlslsm), ALLOCATABLE :: frac_c_label_pool_soilt(:,:,:,:)
    ! fraction of soil carbon that is labelled and traced
    ! pools as for cs_pool_soilt
  REAL(KIND=real_jlslsm), ALLOCATABLE :: di_ncat_sicat(:,:,:)
    !  "Equivalent thickness" of sea-ice catagories (m)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: k_sice_sicat(:,:,:)
    ! Sea ice effective conductivity (2*kappai/de)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gc_surft(:,:)
    ! Stomatal" conductance to evaporation for land tiles(m/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: gs_gb(:)
    ! "Stomatal" conductance to evaporation (m/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lai_pft(:,:)
    ! LAI of plant functional types
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rgrain_surft(:,:)
    ! Snow surface grain size on tiles (microns)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: smc_soilt(:,:)
    ! Soil moisture in a layer at the surface (kg/m2).
    ! Note that SMC is used twice:
    ! 1) SF_EXPL and SF_IMPL2 use it to return the available water in the total
    !    soil column (as calculated in PHYSIOL)
    ! 2) HYDROL uses it to return the available water in a layer of a given
    !    depth (as calculated in SOILMC)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: smcl_soilt(:,:,:)
    ! Soil moisture content of layers on soil tiles (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_surft(:,:)
    ! Lying snow on tiles (kg/m2)
    ! If can_model=4,
    !   snow_surft is the snow on the canopy
    !   snow_grnd is the snow on the ground beneath canopy
    ! If can_model/=4, snow_surft is the total snow.

    ! Warning! snow_tile misbehaves in the UM radiation scheme so is passed
    ! as a simple array rather than via the prog TYPE

  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_grnd_surft(:,:)
    ! Snow on the ground (kg/m2)
    ! This is the snow beneath the canopy and is only
    ! used if can_model=4.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_mass_ij(:,:)
    ! Gridbox snowmass (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_mass_sea_sicat(:,:,:)
    ! Snow on category sea-ice (kg/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: soot_ij(:,:)
    ! Snow soot content (kg/kg)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t_soil_soilt(:,:,:)
    ! Sub-surface temperature on layers and soil tiles (K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t_soil_soilt_acc(:,:,:)
    ! Sub-surface temperature on layers and soil tiles
    ! accumulated over TRIFFID timestep (K).
    ! Gives mean temperature over TRIFFID timestep.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ti_sicat(:,:,:)
    ! Sea-ice surface layer
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tstar_surft(:,:)
    ! Tile surface temperatures (K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tsurf_elev_surft(:,:)
    ! Tiled land-ice bedrock subsurface temperatures (K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0msea_ij(:,:)
    ! Sea-surface roughness length for momentum (m).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0m_sice_fmd(:,:)
    ! Prognostic sea ice form drag roughness length.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0m_sice_skin(:,:)
    ! Prognostic sea ice skin roughness length.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: plantNumDensity(:,:,:)
    ! Prognostic cohort contribution towards the total PFT assimilate (/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t_home_gb(:)
  ! Average temperature (home temperature) for thermal adaptation of
  ! photosynthetic capacity (K).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: t_growth_gb(:)
  ! Average temperature (growth temperature) for thermal acclimation of
  ! photosynthetic capacity (K).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: f_nsc_pft(:,:)
    ! Non-structural carbohydrate mass fraction (kgC/kgC)

END TYPE

!================================

TYPE :: progs_type
  INTEGER, POINTER :: nsnow_surft(:,:)
  INTEGER, POINTER :: seed_rain(:)
  INTEGER, POINTER :: years_since_harvest(:,:)

  REAL(KIND=real_jlslsm), POINTER :: tsoil_deep_gb(:,:)
  REAL(KIND=real_jlslsm), POINTER :: sice_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER ::   sliq_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: snowdepth_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: tsnow_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: rgrainl_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: rho_snow_grnd_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rho_snow_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ds_surft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: wood_prod_fast_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: wood_prod_med_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: wood_prod_slow_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: frac_agr_prev_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: frac_past_prev_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: frac_biocrop_prev_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_inorg_soilt_lyrs(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: n_inorg_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: n_inorg_avail_pft(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ns_pool_gb(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: substr_ch4(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: mic_ch4(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: mic_act_ch4(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: timewet_lyr(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: acclim_ch4(:,:)
  REAL(KIND=real_jlslsm), POINTER :: triffid_co2_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: canht_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: canopy_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: canopy_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: cs_pool_soilt(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: frac_c_label_pool_soilt(:,:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: di_ncat_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: k_sice_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: gc_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: gs_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: lai_pft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: rgrain_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: smc_soilt(:,:)
  REAL(KIND=real_jlslsm), POINTER :: smcl_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: snow_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: snow_grnd_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: snow_mass_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: snow_mass_sea_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: soot_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: t_soil_soilt(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: t_soil_soilt_acc(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: ti_sicat(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: tstar_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: tsurf_elev_surft(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z0msea_ij(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z0m_sice_fmd(:,:)
  REAL(KIND=real_jlslsm), POINTER :: z0m_sice_skin(:,:)
  REAL(KIND=real_jlslsm), POINTER :: plantNumDensity(:,:,:)
  REAL(KIND=real_jlslsm), POINTER :: t_home_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: t_growth_gb(:)
  REAL(KIND=real_jlslsm), POINTER :: f_nsc_pft(:,:)
END TYPE

LOGICAL :: l_broadcast_soilt = .FALSE.
                      !Switch to broadcast model state around all soil tiles.
                      !Only has an effect if l_tile_soil is true.
                      !Does not do anything with ancils- this is done by
                      !l_broadcast_ancils



CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PROGNOSTICS'

CONTAINS

!===============================================================================
SUBROUTINE prognostics_alloc(land_pts, t_i_length, t_j_length,                 &
                       nsurft, npft, nsoilt, sm_levels, ns_deep, nsmax,        &
                       dim_cslayer, dim_cs1, dim_ch4layer, dim_ch4subgrid,     &
                       nice, nice_use, soil_bgc_model, soil_model_ecosse,      &
                       l_layeredc, l_triffid, l_phenol, l_bedrock, l_red,      &
                       nmasst, nnpft, l_acclim, l_sugar, progs_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts, t_i_length, t_j_length,                       &
                       nsurft, npft, nsoilt, sm_levels, ns_deep, nsmax,        &
                       dim_cslayer, dim_cs1, dim_ch4layer, dim_ch4subgrid,     &
                       nice, nice_use, soil_bgc_model, soil_model_ecosse,      &
                       nmasst, nnpft

LOGICAL, INTENT(IN) :: l_layeredc, l_triffid, l_phenol, l_bedrock, l_red,      &
                       l_acclim, l_sugar

TYPE(progs_data_type), INTENT(IN OUT) :: progs_data
  !Instance of the data type we need to allocate

!Local variables
INTEGER :: temp_size, temp_tiles, temp_layers

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PROGNOSTICS_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!seed_rain is allocated elsewhere

!  ====prognostics module common====
ALLOCATE(progs_data%nsnow_surft(land_pts,nsurft))
ALLOCATE(progs_data%rho_snow_grnd_surft(land_pts,nsurft))
ALLOCATE(progs_data%snowdepth_surft(land_pts,nsurft))
ALLOCATE(progs_data%cs_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1))
progs_data%nsnow_surft(:,:)         = 0
progs_data%rho_snow_grnd_surft(:,:) = 0.0
progs_data%snowdepth_surft(:,:)     = 0.0
progs_data%cs_pool_soilt(:,:,:,:)   = 0.0

! Additional soil carbon variables
IF (l_layeredc .OR. soil_bgc_model == soil_model_ecosse) THEN
  ALLOCATE(progs_data%t_soil_soilt_acc(land_pts,nsoilt,sm_levels))
  progs_data%t_soil_soilt_acc(:,:,:)  = 0.0
ELSE
  ALLOCATE(progs_data%t_soil_soilt_acc(1,1,1))
END IF
ALLOCATE(progs_data%frac_c_label_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1))
progs_data%frac_c_label_pool_soilt(:,:,:,:)  = 0.0

! Nitrogen scheme variables
ALLOCATE(progs_data%n_inorg_gb(land_pts))
ALLOCATE(progs_data%ns_pool_gb(land_pts,dim_cslayer,dim_cs1))
ALLOCATE(progs_data%n_inorg_soilt_lyrs(land_pts,nsoilt,dim_cslayer))
ALLOCATE(progs_data%n_inorg_avail_pft(land_pts,npft,dim_cslayer))

progs_data%ns_pool_gb(:,:,:)         = 0.001
progs_data%n_inorg_soilt_lyrs(:,:,:) = 0.0
progs_data%n_inorg_gb(:)             = 0.0
progs_data%n_inorg_avail_pft(:,:,:)  = 0.0

!For multilayer snow variables, only allocate to full size if the scheme is
!being used, ie nsmax > 0
IF (nsmax > 0) THEN
  temp_size   = land_pts
  temp_tiles  = nsurft
  temp_layers = nsmax
ELSE
  temp_size   = 1
  temp_tiles  = 1
  temp_layers = 1
END IF

ALLOCATE(progs_data%ds_surft(temp_size,temp_tiles,temp_layers))
ALLOCATE(progs_data%rgrainl_surft(temp_size,temp_tiles,temp_layers))
ALLOCATE(progs_data%sice_surft(temp_size,temp_tiles,temp_layers))
ALLOCATE(progs_data%sliq_surft(temp_size,temp_tiles,temp_layers))
ALLOCATE(progs_data%tsnow_surft(temp_size,temp_tiles,temp_layers))

progs_data%ds_surft(:,:,:)      = 0.0
progs_data%rgrainl_surft(:,:,:) = 0.0
progs_data%sice_surft(:,:,:)    = 0.0
progs_data%sliq_surft(:,:,:)    = 0.0
progs_data%tsnow_surft(:,:,:)   = 0.0

!See comment in prognostics module
ALLOCATE(progs_data%rho_snow_surft(land_pts,nsurft,nsmax))
progs_data%rho_snow_surft(:,:,:) = 0.0

! Allocate WP Pools
IF ( l_triffid .OR. l_phenol ) THEN
  ALLOCATE(progs_data%wood_prod_fast_gb(land_pts))
  ALLOCATE(progs_data%wood_prod_med_gb(land_pts))
  ALLOCATE(progs_data%wood_prod_slow_gb(land_pts))
  ALLOCATE(progs_data%frac_agr_prev_gb(land_pts))
  ALLOCATE(progs_data%frac_past_prev_gb(land_pts))
  ALLOCATE(progs_data%frac_biocrop_prev_gb(land_pts))

  progs_data%wood_prod_fast_gb(:) = 0.0
  progs_data%wood_prod_med_gb(:)  = 0.0
  progs_data%wood_prod_slow_gb(:) = 0.0
  progs_data%frac_agr_prev_gb(:)  = 0.0
  progs_data%frac_past_prev_gb(:) = 0.0
  progs_data%frac_biocrop_prev_gb(:) = 0.0
ELSE
  ALLOCATE(progs_data%wood_prod_fast_gb(1))
  ALLOCATE(progs_data%wood_prod_med_gb(1))
  ALLOCATE(progs_data%wood_prod_slow_gb(1))
  ALLOCATE(progs_data%frac_agr_prev_gb(1))
  ALLOCATE(progs_data%frac_past_prev_gb(1))
  ALLOCATE(progs_data%frac_biocrop_prev_gb(1))
END IF

! If TRIFFID is switched on, the TRIFFID-derived CO2 flux variable,
! which is passed to the atmosphere in UM simulations with interactive CO2,
! needs to be allocated. Not testing for L_CO2_INTERACTIVE so that the
! diagnostic of triffid_co2_gb can be output with or without interactive CO2.
IF ( l_triffid ) THEN
  ALLOCATE(progs_data%triffid_co2_gb(land_pts))
  ALLOCATE(progs_data%years_since_harvest(land_pts,npft))
  progs_data%triffid_co2_gb(:)    = 0.0
  progs_data%years_since_harvest(:,:) = 0
ELSE
  ALLOCATE(progs_data%triffid_co2_gb(1))
  ALLOCATE(progs_data%years_since_harvest(1,1))
END IF

! Only allocate the bedrock tsoil_deep_gb if bedrock is being used
IF ( l_bedrock ) THEN
  ALLOCATE(progs_data%tsoil_deep_gb(land_pts,ns_deep))
  progs_data%tsoil_deep_gb(:,:) = 0.0
ELSE
  ALLOCATE(progs_data%tsoil_deep_gb(1,1))
END IF

! Prognostics for microbial methane scheme
ALLOCATE(progs_data%substr_ch4(land_pts,dim_ch4subgrid,dim_ch4layer))
ALLOCATE(progs_data%mic_ch4(land_pts,dim_ch4subgrid,dim_ch4layer))
ALLOCATE(progs_data%mic_act_ch4(land_pts,dim_ch4subgrid,dim_ch4layer))
ALLOCATE(progs_data%timewet_lyr(land_pts,dim_ch4subgrid,dim_ch4layer))
ALLOCATE(progs_data%acclim_ch4(land_pts,dim_ch4layer))
progs_data%substr_ch4(:,:,:) = 0.0
progs_data%mic_ch4(:,:,:) = 0.0
progs_data%mic_act_ch4(:,:,:) = 0.0
progs_data%timewet_lyr(:,:,:) = 0.0
progs_data%acclim_ch4(:,:) = 0.0

! Prognostics for RED scheme
IF ( l_red ) THEN
  ALLOCATE(progs_data%plantNumDensity(land_pts,nnpft,nmasst))
  progs_data%plantNumDensity(:,:,:) = 0.0
ELSE
  ALLOCATE(progs_data%plantNumDensity(1,1,1))
END IF

IF ( l_acclim ) THEN
  ALLOCATE(progs_data%t_home_gb(land_pts))
  ALLOCATE(progs_data%t_growth_gb(land_pts))
  progs_data%t_home_gb(:)           = 0.0
  progs_data%t_growth_gb(:)         = 0.0
ELSE
  ALLOCATE(progs_data%t_home_gb(1))
  ALLOCATE(progs_data%t_growth_gb(1))
  progs_data%t_home_gb(1)           = 0.0
  progs_data%t_growth_gb(1)         = 0.0
END IF

! Prognostics for SUGAR module
ALLOCATE(progs_data%f_nsc_pft(land_pts,npft))
progs_data%f_nsc_pft(:,:) = 0.0


!  ====prognostics module JULES-standalone only====
ALLOCATE(progs_data%lai_pft(land_pts,npft))
ALLOCATE(progs_data%canht_pft(land_pts,npft))
ALLOCATE(progs_data%smcl_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(progs_data%t_soil_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(progs_data%tsurf_elev_surft(land_pts,nsurft))
ALLOCATE(progs_data%rgrain_surft(land_pts,nsurft))
ALLOCATE(progs_data%snow_surft(land_pts,nsurft))
ALLOCATE(progs_data%soot_ij(t_i_length,t_j_length))
ALLOCATE(progs_data%tstar_surft(land_pts,nsurft))
ALLOCATE(progs_data%canopy_surft(land_pts,nsurft))
ALLOCATE(progs_data%canopy_gb(land_pts))
ALLOCATE(progs_data%ti_sicat(t_i_length,t_j_length,nice))
ALLOCATE(progs_data%z0msea_ij(t_i_length,t_j_length))
ALLOCATE(progs_data%z0m_sice_fmd(t_i_length,t_j_length))
ALLOCATE(progs_data%z0m_sice_skin(t_i_length,t_j_length))
ALLOCATE(progs_data%gs_gb(land_pts))
ALLOCATE(progs_data%gc_surft(land_pts,nsurft))
ALLOCATE(progs_data%smc_soilt(land_pts,nsoilt))
ALLOCATE(progs_data%di_ncat_sicat(t_i_length,t_j_length,nice))
ALLOCATE(progs_data%k_sice_sicat(t_i_length,t_j_length,nice))
ALLOCATE(progs_data%snow_grnd_surft(land_pts,nsurft))
ALLOCATE(progs_data%snow_mass_ij(t_i_length,t_j_length))
ALLOCATE(progs_data%snow_mass_sea_sicat(t_i_length,t_j_length,nice_use))

progs_data%lai_pft(:,:)               = 0.0
progs_data%canht_pft(:,:)             = 0.0
progs_data%smcl_soilt(:,:,:)          = 0.0
progs_data%t_soil_soilt(:,:,:)        = 0.0
progs_data%tsurf_elev_surft(:,:)      = 0.0
progs_data%rgrain_surft(:,:)          = 0.0
progs_data%snow_surft(:,:)            = 0.0
progs_data%soot_ij(:,:)               = 0.0
progs_data%tstar_surft(:,:)           = 0.0
progs_data%canopy_surft(:,:)          = 0.0
progs_data%canopy_gb(:)               = 0.0
progs_data%ti_sicat(:,:,:)            = 0.0
progs_data%z0msea_ij(:,:)             = 0.0
progs_data%z0m_sice_fmd(:,:)          = 0.0
progs_data%z0m_sice_skin(:,:)         = 0.0
progs_data%gs_gb(:)                   = 0.0
progs_data%gc_surft(:,:)              = 0.0
progs_data%smc_soilt(:,:)             = 0.0
progs_data%di_ncat_sicat(:,:,:)       = 0.0
progs_data%k_sice_sicat(:,:,:)        = 0.0
progs_data%snow_grnd_surft(:,:)       = 0.0
progs_data%snow_mass_ij(:,:)          = 0.0
progs_data%snow_mass_sea_sicat(:,:,:) = 0.0

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE prognostics_alloc

!==============================================================================
SUBROUTINE prognostics_dealloc(progs_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(progs_data_type), INTENT(IN OUT) :: progs_data
  !Instance of the data type we need to deallocate

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PROGNOSTICS_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!  ====prognostics module common====
DEALLOCATE(progs_data%nsnow_surft)
DEALLOCATE(progs_data%rho_snow_grnd_surft)
DEALLOCATE(progs_data%snowdepth_surft)
DEALLOCATE(progs_data%cs_pool_soilt)
DEALLOCATE(progs_data%t_soil_soilt_acc)
DEALLOCATE(progs_data%ns_pool_gb)
DEALLOCATE(progs_data%n_inorg_gb)
DEALLOCATE(progs_data%n_inorg_soilt_lyrs)
DEALLOCATE(progs_data%n_inorg_avail_pft)
DEALLOCATE(progs_data%ds_surft)
DEALLOCATE(progs_data%rgrainl_surft)
DEALLOCATE(progs_data%sice_surft)
DEALLOCATE(progs_data%sliq_surft)
DEALLOCATE(progs_data%tsnow_surft)
DEALLOCATE(progs_data%rho_snow_surft)
DEALLOCATE(progs_data%substr_ch4)
DEALLOCATE(progs_data%mic_ch4)
DEALLOCATE(progs_data%mic_act_ch4)
DEALLOCATE(progs_data%timewet_lyr)
DEALLOCATE(progs_data%acclim_ch4)
DEALLOCATE(progs_data%frac_c_label_pool_soilt)

!Can just test on one of these being allocated
IF ( ALLOCATED(progs_data%wood_prod_fast_gb) ) THEN
  DEALLOCATE(progs_data%wood_prod_fast_gb)
  DEALLOCATE(progs_data%wood_prod_med_gb)
  DEALLOCATE(progs_data%wood_prod_slow_gb)
  DEALLOCATE(progs_data%frac_agr_prev_gb)
  DEALLOCATE(progs_data%frac_past_prev_gb)
  DEALLOCATE(progs_data%frac_biocrop_prev_gb)
END IF

IF ( ALLOCATED(progs_data%triffid_co2_gb) ) THEN
  DEALLOCATE(progs_data%triffid_co2_gb)
END IF

IF ( ALLOCATED(progs_data%years_since_harvest) ) THEN
  DEALLOCATE(progs_data%years_since_harvest)
END IF

DEALLOCATE(progs_data%tsoil_deep_gb)

IF ( ALLOCATED(progs_data%t_home_gb) ) THEN
  DEALLOCATE(progs_data%t_home_gb)
  DEALLOCATE(progs_data%t_growth_gb)
END IF

IF ( ALLOCATED(progs_data%f_nsc_pft) ) THEN
  DEALLOCATE(progs_data%f_nsc_pft)
END IF

!  ====prognostics module JULES-standalone only====
DEALLOCATE(progs_data%lai_pft)
DEALLOCATE(progs_data%canht_pft)
DEALLOCATE(progs_data%smcl_soilt)
DEALLOCATE(progs_data%t_soil_soilt)
DEALLOCATE(progs_data%tsurf_elev_surft)
DEALLOCATE(progs_data%rgrain_surft)
DEALLOCATE(progs_data%snow_surft)
DEALLOCATE(progs_data%soot_ij)
DEALLOCATE(progs_data%tstar_surft)
DEALLOCATE(progs_data%canopy_surft)
DEALLOCATE(progs_data%canopy_gb)
DEALLOCATE(progs_data%ti_sicat)
DEALLOCATE(progs_data%z0msea_ij)
DEALLOCATE(progs_data%z0m_sice_fmd)
DEALLOCATE(progs_data%z0m_sice_skin)
DEALLOCATE(progs_data%gs_gb)
DEALLOCATE(progs_data%gc_surft)
DEALLOCATE(progs_data%smc_soilt)
DEALLOCATE(progs_data%di_ncat_sicat)
DEALLOCATE(progs_data%k_sice_sicat)
DEALLOCATE(progs_data%snow_grnd_surft)
DEALLOCATE(progs_data%snow_mass_ij)
DEALLOCATE(progs_data%snow_mass_sea_sicat)
DEALLOCATE(progs_data%plantNumDensity)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE prognostics_dealloc

!==============================================================================
SUBROUTINE prognostics_assoc(progs,progs_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(progs_type), INTENT(IN OUT) :: progs
  !Instance of the pointer type we are associating

TYPE(progs_data_type), INTENT(IN OUT), TARGET :: progs_data
  !Instance of the data type we are associtating to

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PROGNOSTICS_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL prognostics_nullify(progs)

progs%nsnow_surft => progs_data%nsnow_surft
progs%rho_snow_grnd_surft => progs_data%rho_snow_grnd_surft
progs%snowdepth_surft => progs_data%snowdepth_surft
progs%cs_pool_soilt => progs_data%cs_pool_soilt
progs%frac_c_label_pool_soilt => progs_data%frac_c_label_pool_soilt
progs%t_soil_soilt_acc => progs_data%t_soil_soilt_acc
progs%ns_pool_gb => progs_data%ns_pool_gb
progs%n_inorg_gb => progs_data%n_inorg_gb
progs%n_inorg_soilt_lyrs => progs_data%n_inorg_soilt_lyrs
progs%n_inorg_avail_pft => progs_data%n_inorg_avail_pft
progs%ds_surft => progs_data%ds_surft
progs%rgrainl_surft => progs_data%rgrainl_surft
progs%sice_surft => progs_data%sice_surft
progs%sliq_surft => progs_data%sliq_surft
progs%tsnow_surft => progs_data%tsnow_surft
progs%rho_snow_surft => progs_data%rho_snow_surft
progs%substr_ch4 => progs_data%substr_ch4
progs%mic_ch4 => progs_data%mic_ch4
progs%mic_act_ch4 => progs_data%mic_act_ch4
progs%timewet_lyr => progs_data%timewet_lyr
progs%acclim_ch4 => progs_data%acclim_ch4
progs%wood_prod_fast_gb => progs_data%wood_prod_fast_gb
progs%wood_prod_med_gb => progs_data%wood_prod_med_gb
progs%wood_prod_slow_gb => progs_data%wood_prod_slow_gb
progs%frac_agr_prev_gb => progs_data%frac_agr_prev_gb
progs%frac_past_prev_gb => progs_data%frac_past_prev_gb
progs%frac_biocrop_prev_gb => progs_data%frac_biocrop_prev_gb
progs%triffid_co2_gb => progs_data%triffid_co2_gb
progs%tsoil_deep_gb => progs_data%tsoil_deep_gb
progs%lai_pft => progs_data%lai_pft
progs%canht_pft => progs_data%canht_pft
progs%smcl_soilt => progs_data%smcl_soilt
progs%t_soil_soilt => progs_data%t_soil_soilt
progs%tsurf_elev_surft => progs_data%tsurf_elev_surft
progs%rgrain_surft => progs_data%rgrain_surft
progs%snow_surft => progs_data%snow_surft
progs%soot_ij => progs_data%soot_ij
progs%tstar_surft => progs_data%tstar_surft
progs%canopy_surft => progs_data%canopy_surft
progs%canopy_gb => progs_data%canopy_gb
progs%ti_sicat => progs_data%ti_sicat
progs%z0msea_ij => progs_data%z0msea_ij
progs%z0m_sice_fmd => progs_data%z0m_sice_fmd
progs%z0m_sice_skin => progs_data%z0m_sice_skin
progs%gs_gb => progs_data%gs_gb
progs%gc_surft => progs_data%gc_surft
progs%smc_soilt => progs_data%smc_soilt
progs%di_ncat_sicat => progs_data%di_ncat_sicat
progs%k_sice_sicat => progs_data%k_sice_sicat
progs%snow_grnd_surft => progs_data%snow_grnd_surft
progs%snow_mass_ij => progs_data%snow_mass_ij
progs%snow_mass_sea_sicat => progs_data%snow_mass_sea_sicat
progs%plantNumDensity => progs_data%plantNumDensity
progs%years_since_harvest => progs_data%years_since_harvest
progs%t_home_gb => progs_data%t_home_gb
progs%t_growth_gb => progs_data%t_growth_gb
progs%f_nsc_pft => progs_data%f_nsc_pft

IF (ALLOCATED(progs_data%seed_rain)) THEN
  progs%seed_rain => progs_data%seed_rain
END IF


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE prognostics_assoc

!==============================================================================

SUBROUTINE prognostics_nullify(progs)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(progs_type), INTENT(IN OUT) :: progs
  !Instance of the pointer type we are nullifying

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PROGNOSTICS_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(progs%nsnow_surft)
NULLIFY(progs%rho_snow_grnd_surft)
NULLIFY(progs%snowdepth_surft)
NULLIFY(progs%cs_pool_soilt)
NULLIFY(progs%frac_c_label_pool_soilt)
NULLIFY(progs%t_soil_soilt_acc)
NULLIFY(progs%ns_pool_gb)
NULLIFY(progs%n_inorg_gb)
NULLIFY(progs%n_inorg_soilt_lyrs)
NULLIFY(progs%n_inorg_avail_pft)
NULLIFY(progs%ds_surft)
NULLIFY(progs%rgrainl_surft)
NULLIFY(progs%sice_surft)
NULLIFY(progs%sliq_surft)
NULLIFY(progs%tsnow_surft)
NULLIFY(progs%rho_snow_surft)
NULLIFY(progs%substr_ch4)
NULLIFY(progs%mic_ch4)
NULLIFY(progs%mic_act_ch4)
NULLIFY(progs%timewet_lyr)
NULLIFY(progs%acclim_ch4)
NULLIFY(progs%wood_prod_fast_gb)
NULLIFY(progs%wood_prod_med_gb)
NULLIFY(progs%wood_prod_slow_gb)
NULLIFY(progs%frac_agr_prev_gb)
NULLIFY(progs%frac_past_prev_gb)
NULLIFY(progs%frac_biocrop_prev_gb)
NULLIFY(progs%triffid_co2_gb)
NULLIFY(progs%tsoil_deep_gb)
NULLIFY(progs%lai_pft)
NULLIFY(progs%canht_pft)
NULLIFY(progs%smcl_soilt)
NULLIFY(progs%t_soil_soilt)
NULLIFY(progs%tsurf_elev_surft)
NULLIFY(progs%rgrain_surft)
NULLIFY(progs%snow_surft)
NULLIFY(progs%soot_ij)
NULLIFY(progs%tstar_surft)
NULLIFY(progs%canopy_surft)
NULLIFY(progs%canopy_gb)
NULLIFY(progs%ti_sicat)
NULLIFY(progs%z0msea_ij)
NULLIFY(progs%z0m_sice_fmd)
NULLIFY(progs%z0m_sice_skin)
NULLIFY(progs%gs_gb)
NULLIFY(progs%gc_surft)
NULLIFY(progs%smc_soilt)
NULLIFY(progs%di_ncat_sicat)
NULLIFY(progs%k_sice_sicat)
NULLIFY(progs%snow_grnd_surft)
NULLIFY(progs%snow_mass_ij)
NULLIFY(progs%snow_mass_sea_sicat)
NULLIFY(progs%seed_rain)
NULLIFY(progs%plantNumDensity)
NULLIFY(progs%years_since_harvest)
NULLIFY(progs%t_home_gb)
NULLIFY(progs%t_growth_gb)
NULLIFY(progs%f_nsc_pft)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE prognostics_nullify

END MODULE prognostics
