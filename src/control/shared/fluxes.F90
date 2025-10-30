! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module containing surface fluxes.
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Land
!
! Changes to variable names to enable soil tiling
! Anything named _tile is now ambiguous, so the following convention is
! adopted:
! _gb for variables on land points
! _ij for variables with i and j indices
! Surface heterogeneity...
! _surft for surface tiled variables (generally size n_surft)
! _pft for plant functional type surface tiled variables (generally sized n_pft)
! _sicat for sea ice catergories (generally size nice_use)
! Sub-surface heterogeneity...
! _soilt for soil tiled variables

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

MODULE fluxes_mod

USE um_types, ONLY: real_jlslsm
USE missing_data_mod, ONLY: rmdi

IMPLICIT NONE

TYPE :: fluxes_data_type

  ! T if varying grey emissivity is used for surface tile type
  LOGICAL, ALLOCATABLE :: l_emis_surft_set(:)
  ! anthrop_heat is required by both the UM and standalone configurations

  REAL(KIND=real_jlslsm), ALLOCATABLE :: anthrop_heat_surft(:,:)
  ! Additional heat source on surface tiles used for anthropgenic urban heat
  ! source (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_ht_store_surft(:,:)
  ! Diagnostic to store values of C*(dT/dt) during calculation of energy
  ! balance
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sw_sicat(:,:)
  ! Net SW on sea ice categories
  REAL(KIND=real_jlslsm), ALLOCATABLE :: alb_sicat(:,:,:)
  ! Albedo of sea ice categories (ij point, sicat, band- see below)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: penabs_rad_frac(:,:,:)
  ! Fraction of downward solar that penetrates the sea ice and is absorbed
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sw_sea(:)
  ! Net SW on open sea
  REAL(KIND=real_jlslsm), ALLOCATABLE :: alb_surft(:,:,:)
  !   Albedo for surface tiles
  !     (:,:,1) direct beam visible
  !     (:,:,2) diffuse visible
  !     (:,:,3) direct beam near-IR
  !     (:,:,4) diffuse near-IR
  REAL(KIND=real_jlslsm), ALLOCATABLE :: e_sea_ij(:,:)
  !   Evaporation from sea times leads fraction. Zero over land
  !                                (kg per square metre per sec)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ecan_ij(:,:)
  !   Gridbox mean evaporation from canopy/surface store (kg/m2/s)
  !     Zero over sea
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ecan_surft(:,:)
  !   Canopy evaporation from for snow-free land tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ei_ij(:,:)
  !   Sublimation from lying snow or sea-ice (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ei_surft(:,:)
  !   EI for land tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: esoil_ij_soilt(:,:,:)
  !   Surface evapotranspiration from soil moisture store (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: esoil_surft(:,:)
  !   ESOIL for snow-free land tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ext_soilt(:,:,:)
  !   Extraction of water from each soil layer (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fqw_surft(:,:)
  !   Surface FQW for land tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fqw_sicat(:,:,:)
  !   Surface FQW for sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: fsmc_pft(:,:)
  !   Moisture availability factor.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ftl_sicat(:,:,:)
  !   Surface FTL for sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ftl_surft(:,:)
  !   Surface FTL for land tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: h_sea_ij(:,:)
  !   Surface sensible heat flux over sea times leads fraction (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: hf_snow_melt_gb(:)
  !   Gridbox snowmelt heat flux (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: land_albedo_ij(:,:,:)
  !   GBM albedo
  !     (:,:,1) direct beam visible
  !     (:,:,2) diffuse visible
  !     (:,:,3) direct beam near-IR
  !     (:,:,4) diffuse near-IR
  REAL(KIND=real_jlslsm), ALLOCATABLE :: le_surft(:,:)
  !   Surface latent heat flux for land tiles
  REAL(KIND=real_jlslsm), ALLOCATABLE :: melt_surft(:,:)
  !   Snowmelt on land tiles (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snowinc_surft(:,:)
  !   Increment to snow from sublimation and melting on
  !   surface tiles (kg m-2 TS-1)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tot_tfall_surft(:,:)
  !   Canopy througfall rate on land tiles (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sice_melt(:,:,:)
  !   Sea ice top melt on sea ice categories (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: ei_sice(:,:,:)
  !   Sea ice sublimation on sea ice categories (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_ht_flux_sice(:,:,:)
  !   Heat flux through sea-ice (W/m2, positive downwards)
  !   used in models coupled to ocean/sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sea_ice_htf_sicat(:,:,:)
  !   Heat flux through sea-ice (W/m2, positive downwards)
  !   used in models NOT coupled to ocean/sea-ice
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snomlt_sub_htf_gb(:)
  !   Sub-canopy snowmelt heat flux (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: snow_melt_gb(:)
  !   snowmelt on land points (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sub_surf_roff_gb(:)
  !   Sub-surface runoff (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_ht_flux_ij(:,:)
  !   Net downward heat flux at surface over land and sea-ice fraction of
  !gridbox (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE ::  snow_soil_htf(:,:)
  !   Heat flux under snow to subsurface on tiles (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_htf_surft(:,:)
  !   Surface heat flux on land tiles (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: surf_roff_gb(:)
  !   Surface runoff (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: radnet_surft(:,:)
  !   Surface net radiation on tiles ( W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tot_tfall_gb(:)
  !   Total throughfall (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: tstar_ij(:,:)
  !   GBM surface temperature (K)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: emis_surft(:,:)
  !   Tile emissivity
  REAL(KIND=real_jlslsm), ALLOCATABLE :: sw_surft(:,:)
  !   Surface net shortwave on tiles (W/m2)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rflow_gb(:)
  !   River outflow on model grid (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: rrun_gb(:)
  !   Runoff after river routing on model grid (kg/m2/s)
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0m_surft(:,:)
  !   Tile roughness lengths for momentum.
  REAL(KIND=real_jlslsm), ALLOCATABLE :: z0h_surft(:,:)
  !   Tile roughness lengths for heat and moisture (m).
  REAL(KIND=real_jlslsm), ALLOCATABLE :: lake_evap(:)
  !   Evaporation from lakes (kg m-2 s-1).
END TYPE

TYPE :: fluxes_type

  ! T if varying grey emissivity is used for surface tile type
  LOGICAL, POINTER :: l_emis_surft_set(:)

  ! anthrop_heat is required by both the UM and standalone configurations
  REAL(KIND=real_jlslsm), POINTER :: anthrop_heat_surft(:,:)
  ! Additional heat source on surface tiles used for anthropgenic urban heat
  ! source (W/m2)
  REAL(KIND=real_jlslsm), POINTER :: surf_ht_store_surft(:,:)
  ! Diagnostic to store values of C*(dT/dt) during calculation of energy
  ! balance
  REAL(KIND=real_jlslsm), POINTER :: sw_sicat(:,:)
  ! Net SW on sea ice categories
  REAL(KIND=real_jlslsm), POINTER :: alb_sicat(:,:,:)
  ! Albedo of sea ice categories (ij point, sicat, band- see below)
  REAL(KIND=real_jlslsm), POINTER :: penabs_rad_frac(:,:,:)
  ! Fraction of downward solar that penetrates the sea ice and is absorbed
  REAL(KIND=real_jlslsm), POINTER :: sw_sea(:)
  ! Net SW on open sea
  REAL(KIND=real_jlslsm), POINTER :: alb_surft(:,:,:)
  !   Albedo for surface tiles
  !     (:,:,1) direct beam visible
  !     (:,:,2) diffuse visible
  !     (:,:,3) direct beam near-IR
  !     (:,:,4) diffuse near-IR
  REAL(KIND=real_jlslsm), POINTER :: e_sea_ij(:,:)
  !   Evaporation from sea times leads fraction. Zero over land
  !                                (kg per square metre per sec)
  REAL(KIND=real_jlslsm), POINTER :: ecan_ij(:,:)
  !   Gridbox mean evaporation from canopy/surface store (kg/m2/s)
  !     Zero over sea
  REAL(KIND=real_jlslsm), POINTER :: ecan_surft(:,:)
  !   Canopy evaporation from for snow-free land tiles
  REAL(KIND=real_jlslsm), POINTER :: ei_ij(:,:)
  !   Sublimation from lying snow or sea-ice (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: ei_surft(:,:)
  !   EI for land tiles
  REAL(KIND=real_jlslsm), POINTER :: esoil_ij_soilt(:,:,:)
  !   Surface evapotranspiration from soil moisture store (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: esoil_surft(:,:)
  !   ESOIL for snow-free land tiles
  REAL(KIND=real_jlslsm), POINTER :: ext_soilt(:,:,:)
  !   Extraction of water from each soil layer (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: fqw_surft(:,:)
  !   Surface FQW for land tiles
  REAL(KIND=real_jlslsm), POINTER :: fqw_sicat(:,:,:)
  !   Surface FQW for sea-ice
  REAL(KIND=real_jlslsm), POINTER :: fsmc_pft(:,:)
  !   Moisture availability factor.
  REAL(KIND=real_jlslsm), POINTER :: ftl_sicat(:,:,:)
  !   Surface FTL for sea-ice
  REAL(KIND=real_jlslsm), POINTER :: ftl_surft(:,:)
  !   Surface FTL for land tiles
  REAL(KIND=real_jlslsm), POINTER :: h_sea_ij(:,:)
  !   Surface sensible heat flux over sea times leads fraction (W/m2)
  REAL(KIND=real_jlslsm), POINTER :: hf_snow_melt_gb(:)
  !   Gridbox snowmelt heat flux (W/m2)
  REAL(KIND=real_jlslsm), POINTER :: land_albedo_ij(:,:,:)
  !   GBM albedo
  !     (:,:,1) direct beam visible
  !     (:,:,2) diffuse visible
  !     (:,:,3) direct beam near-IR
  !     (:,:,4) diffuse near-IR
  REAL(KIND=real_jlslsm), POINTER :: le_surft(:,:)
  !   Surface latent heat flux for land tiles
  REAL(KIND=real_jlslsm), POINTER :: melt_surft(:,:)
  !   Snowmelt on land tiles (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: snowinc_surft(:,:)
  !   Increment to snow from sublimation and melting on
  !   surface tiles (kg m-2 TS-1)
  REAL(KIND=real_jlslsm), POINTER :: tot_tfall_surft(:,:)
  !   Canopy througfall rate on land tiles (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: sice_melt(:,:,:)
  !   Sea ice top melt on sea ice categories (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: ei_sice(:,:,:)
  !   Sea ice sublimation on sea ice categories (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: surf_ht_flux_sice(:,:,:)
  !   Heat flux through sea-ice (W/m2, positive downwards)
  !   used in models coupled to ocean/sea-ice
  REAL(KIND=real_jlslsm), POINTER :: sea_ice_htf_sicat(:,:,:)
  !   Heat flux through sea-ice (W/m2, positive downwards)
  !   used in models NOT coupled to ocean/sea-ice
  REAL(KIND=real_jlslsm), POINTER :: snomlt_sub_htf_gb(:)
  !   Sub-canopy snowmelt heat flux (W/m2)
  REAL(KIND=real_jlslsm), POINTER :: snow_melt_gb(:)
  !   snowmelt on land points (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: sub_surf_roff_gb(:)
  !   Sub-surface runoff (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: surf_ht_flux_ij(:,:)
  !   Net downward heat flux at surface over land and sea-ice fraction of
  !gridbox (W/m2)
  REAL(KIND=real_jlslsm), POINTER ::  snow_soil_htf(:,:)
  !   Heat flux under snow to subsurface on tiles (W/m2)
  REAL(KIND=real_jlslsm), POINTER :: surf_htf_surft(:,:)
  !   Surface heat flux on land tiles (W/m2)
  REAL(KIND=real_jlslsm), POINTER :: surf_roff_gb(:)
  !   Surface runoff (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: radnet_surft(:,:)
  !   Surface net radiation on tiles ( W/m2)
  REAL(KIND=real_jlslsm), POINTER :: tot_tfall_gb(:)
  !   Total throughfall (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: tstar_ij(:,:)
  !   GBM surface temperature (K)
  REAL(KIND=real_jlslsm), POINTER :: emis_surft(:,:)
  !   Tile emissivity
  REAL(KIND=real_jlslsm), POINTER :: sw_surft(:,:)
  !   Surface net shortwave on tiles (W/m2)
  REAL(KIND=real_jlslsm), POINTER :: rflow_gb(:)
  !   River outflow on model grid (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: rrun_gb(:)
  !   Runoff after river routing on model grid (kg/m2/s)
  REAL(KIND=real_jlslsm), POINTER :: z0m_surft(:,:)
  !   Tile roughness lengths for momentum.
  REAL(KIND=real_jlslsm), POINTER :: z0h_surft(:,:)
  !   Tile roughness lengths for heat and moisture (m)
  REAL(KIND=real_jlslsm), POINTER :: lake_evap(:)
  !   Evaporation from lakes (kg m-2 s-1).
END TYPE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='FLUXES_MOD'

CONTAINS

SUBROUTINE fluxes_alloc(land_pts, t_i_length, t_j_length,                      &
                  nsurft, npft, nsoilt, sm_levels,                             &
                  nice, nice_use,                                              &
                  fluxes_data)

!No USE statements other than error reporting and Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts, t_i_length, t_j_length,                       &
                       nsurft, npft, nsoilt, sm_levels,                        &
                       nice, nice_use

TYPE(fluxes_data_type), INTENT(IN OUT) :: fluxes_data

!Local variables

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FLUXES_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

ALLOCATE(fluxes_data%l_emis_surft_set(nsurft))

fluxes_data%l_emis_surft_set(:) = .FALSE.

ALLOCATE(fluxes_data%surf_ht_store_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%anthrop_heat_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%sw_sicat(t_i_length * t_j_length, nice_use))
ALLOCATE(fluxes_data%alb_sicat(t_i_length * t_j_length, nice_use, 4))
ALLOCATE(fluxes_data%penabs_rad_frac(t_i_length * t_j_length, nice_use, 4))
ALLOCATE(fluxes_data%sw_sea(t_i_length * t_j_length))

fluxes_data%surf_ht_store_surft(:,:) = 0.0
fluxes_data%anthrop_heat_surft(:,:)  = 0.0
fluxes_data%sw_sicat(:,:)            = 0.0
fluxes_data%alb_sicat(:,:,:)         = 0.0
fluxes_data%penabs_rad_frac(:,:,:)   = 0.0
fluxes_data%sw_sea(:)                = 0.0

ALLOCATE(fluxes_data%sub_surf_roff_gb(land_pts))
ALLOCATE(fluxes_data%surf_roff_gb(land_pts))
ALLOCATE(fluxes_data%alb_surft(land_pts,nsurft,4) )
ALLOCATE(fluxes_data%tstar_ij(t_i_length,t_j_length))
ALLOCATE(fluxes_data%e_sea_ij(t_i_length,t_j_length))
ALLOCATE(fluxes_data%fsmc_pft(land_pts,npft))
ALLOCATE(fluxes_data%ftl_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%le_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%h_sea_ij(t_i_length,t_j_length))
ALLOCATE(fluxes_data%fqw_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%fqw_sicat(t_i_length,t_j_length,nice_use))
ALLOCATE(fluxes_data%ftl_sicat(t_i_length,t_j_length,nice_use))
ALLOCATE(fluxes_data%ecan_ij(t_i_length,t_j_length))
ALLOCATE(fluxes_data%esoil_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%surf_ht_flux_sice(t_i_length,t_j_length,nice_use))
ALLOCATE(fluxes_data%sea_ice_htf_sicat(t_i_length,t_j_length,nice))
ALLOCATE(fluxes_data%surf_ht_flux_ij(t_i_length,t_j_length))
ALLOCATE(fluxes_data%surf_htf_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%snow_soil_htf(land_pts,nsurft))
ALLOCATE(fluxes_data%land_albedo_ij(t_i_length,t_j_length,4) )
ALLOCATE(fluxes_data%ei_ij(t_i_length,t_j_length))
ALLOCATE(fluxes_data%ei_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%ecan_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%esoil_ij_soilt(t_i_length,t_j_length,nsoilt))
ALLOCATE(fluxes_data%ext_soilt(land_pts,nsoilt,sm_levels))
ALLOCATE(fluxes_data%hf_snow_melt_gb(land_pts))
ALLOCATE(fluxes_data%radnet_surft(land_pts,nsurft) )
ALLOCATE(fluxes_data%sw_surft(land_pts,nsurft) )
ALLOCATE(fluxes_data%emis_surft(land_pts,nsurft) )
ALLOCATE(fluxes_data%snow_melt_gb(land_pts))
ALLOCATE(fluxes_data%snomlt_sub_htf_gb(land_pts))
ALLOCATE(fluxes_data%tot_tfall_gb(land_pts))
ALLOCATE(fluxes_data%melt_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%snowinc_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%tot_tfall_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%sice_melt(t_i_length,t_j_length,nice_use))
ALLOCATE(fluxes_data%ei_sice(t_i_length,t_j_length,nice_use))
ALLOCATE(fluxes_data%z0m_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%z0h_surft(land_pts,nsurft))
ALLOCATE(fluxes_data%rflow_gb(land_pts))
ALLOCATE(fluxes_data%rrun_gb(land_pts))
ALLOCATE(fluxes_data%lake_evap(land_pts))

fluxes_data%sub_surf_roff_gb(:)          = 0.0
fluxes_data%surf_roff_gb(:)              = 0.0
fluxes_data%alb_surft(:,:,:)             = 0.0
fluxes_data%tstar_ij(:,:)                = 0.0
fluxes_data%e_sea_ij(:,:)                = 0.0
fluxes_data%fsmc_pft(:,:)                = 0.0
fluxes_data%ftl_surft(:,:)               = 0.0
fluxes_data%le_surft(:,:)                = 0.0
fluxes_data%h_sea_ij(:,:)                = 0.0
fluxes_data%fqw_surft(:,:)               = 0.0
fluxes_data%fqw_sicat(:,:,:)             = 0.0
fluxes_data%ftl_sicat(:,:,:)             = 0.0
fluxes_data%ecan_ij(:,:)                 = 0.0
fluxes_data%esoil_surft(:,:)             = 273.15
fluxes_data%surf_ht_flux_sice(:,:,:)     = 0.0
fluxes_data%sea_ice_htf_sicat(:,:,:)     = 0.0
fluxes_data%surf_ht_flux_ij(:,:)         = 0.0
fluxes_data%surf_htf_surft(:,:)          = 0.0
fluxes_data%snow_soil_htf(:,:)           = 0.0
fluxes_data%land_albedo_ij(:,:,:)        = 0.0
fluxes_data%ei_ij(:,:)                   = 0.0
fluxes_data%ei_surft(:,:)                = 0.0
fluxes_data%ecan_surft(:,:)              = 0.0
fluxes_data%esoil_ij_soilt(:,:,:)        = 0.0
fluxes_data%ext_soilt(:,:,:)             = 0.0
fluxes_data%hf_snow_melt_gb(:)           = 0.0
fluxes_data%radnet_surft(:,:)            = 0.0
fluxes_data%sw_surft(:,:)                = 0.0
fluxes_data%emis_surft(:,:)              = 0.0
fluxes_data%snow_melt_gb(:)              = 0.0
fluxes_data%snomlt_sub_htf_gb(:)         = 0.0
fluxes_data%tot_tfall_gb(:)              = 0.0
fluxes_data%melt_surft(:,:)              = 0.0
fluxes_data%snowinc_surft(:,:)           = 0.0
fluxes_data%tot_tfall_surft(:,:)         = 0.0
fluxes_data%sice_melt(:,:,:)             = 0.0
fluxes_data%ei_sice(:,:,:)               = 0.0
fluxes_data%z0m_surft(:,:)               = 0.0
fluxes_data%z0h_surft(:,:)               = 0.0
fluxes_data%rflow_gb(:)                  = 0.0
fluxes_data%rrun_gb(:)                   = 0.0
fluxes_data%lake_evap(:)                 = rmdi

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fluxes_alloc

!===============================================================================
SUBROUTINE fluxes_dealloc(fluxes_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
TYPE(fluxes_data_type), INTENT(IN OUT) :: fluxes_data

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FLUXES_DEALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DEALLOCATE(fluxes_data%l_emis_surft_set)
DEALLOCATE(fluxes_data%surf_ht_store_surft)
DEALLOCATE(fluxes_data%anthrop_heat_surft)
DEALLOCATE(fluxes_data%sw_sicat)
DEALLOCATE(fluxes_data%alb_sicat)
DEALLOCATE(fluxes_data%penabs_rad_frac)
DEALLOCATE(fluxes_data%sw_sea)
DEALLOCATE(fluxes_data%sub_surf_roff_gb)
DEALLOCATE(fluxes_data%surf_roff_gb)
DEALLOCATE(fluxes_data%alb_surft)
DEALLOCATE(fluxes_data%tstar_ij)
DEALLOCATE(fluxes_data%e_sea_ij)
DEALLOCATE(fluxes_data%fsmc_pft)
DEALLOCATE(fluxes_data%ftl_surft)
DEALLOCATE(fluxes_data%le_surft)
DEALLOCATE(fluxes_data%h_sea_ij)
DEALLOCATE(fluxes_data%fqw_surft)
DEALLOCATE(fluxes_data%fqw_sicat)
DEALLOCATE(fluxes_data%ftl_sicat)
DEALLOCATE(fluxes_data%ecan_ij)
DEALLOCATE(fluxes_data%esoil_surft)
DEALLOCATE(fluxes_data%surf_ht_flux_sice)
DEALLOCATE(fluxes_data%sea_ice_htf_sicat)
DEALLOCATE(fluxes_data%surf_ht_flux_ij)
DEALLOCATE(fluxes_data%surf_htf_surft)
DEALLOCATE(fluxes_data%snow_soil_htf)
DEALLOCATE(fluxes_data%land_albedo_ij)
DEALLOCATE(fluxes_data%ei_ij)
DEALLOCATE(fluxes_data%ei_surft)
DEALLOCATE(fluxes_data%ecan_surft)
DEALLOCATE(fluxes_data%esoil_ij_soilt)
DEALLOCATE(fluxes_data%ext_soilt)
DEALLOCATE(fluxes_data%hf_snow_melt_gb)
DEALLOCATE(fluxes_data%radnet_surft)
DEALLOCATE(fluxes_data%sw_surft)
DEALLOCATE(fluxes_data%emis_surft)
DEALLOCATE(fluxes_data%snow_melt_gb)
DEALLOCATE(fluxes_data%snomlt_sub_htf_gb)
DEALLOCATE(fluxes_data%tot_tfall_gb)
DEALLOCATE(fluxes_data%melt_surft)
DEALLOCATE(fluxes_data%snowinc_surft)
DEALLOCATE(fluxes_data%tot_tfall_surft)
DEALLOCATE(fluxes_data%sice_melt)
DEALLOCATE(fluxes_data%ei_sice)
DEALLOCATE(fluxes_data%z0m_surft)
DEALLOCATE(fluxes_data%z0h_surft)
DEALLOCATE(fluxes_data%rflow_gb)
DEALLOCATE(fluxes_data%rrun_gb)
DEALLOCATE(fluxes_data%lake_evap)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fluxes_dealloc

!==============================================================================
SUBROUTINE fluxes_assoc(fluxes,fluxes_data)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
  !Instance of the pointer type we are associating

TYPE(fluxes_data_type), INTENT(IN OUT), TARGET :: fluxes_data
  !Instance of the data type we are associating to

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FLUXES_ASSOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL fluxes_nullify(fluxes)

fluxes%l_emis_surft_set => fluxes_data%l_emis_surft_set
fluxes%surf_ht_store_surft => fluxes_data%surf_ht_store_surft
fluxes%anthrop_heat_surft => fluxes_data%anthrop_heat_surft
fluxes%sw_sicat => fluxes_data%sw_sicat
fluxes%alb_sicat => fluxes_data%alb_sicat
fluxes%penabs_rad_frac => fluxes_data%penabs_rad_frac
fluxes%sw_sea => fluxes_data%sw_sea
fluxes%sub_surf_roff_gb => fluxes_data%sub_surf_roff_gb
fluxes%surf_roff_gb => fluxes_data%surf_roff_gb
fluxes%alb_surft => fluxes_data%alb_surft
fluxes%tstar_ij => fluxes_data%tstar_ij
fluxes%e_sea_ij => fluxes_data%e_sea_ij
fluxes%fsmc_pft => fluxes_data%fsmc_pft
fluxes%ftl_surft => fluxes_data%ftl_surft
fluxes%le_surft => fluxes_data%le_surft
fluxes%h_sea_ij => fluxes_data%h_sea_ij
fluxes%fqw_surft => fluxes_data%fqw_surft
fluxes%fqw_sicat => fluxes_data%fqw_sicat
fluxes%ftl_sicat => fluxes_data%ftl_sicat
fluxes%ecan_ij => fluxes_data%ecan_ij
fluxes%esoil_surft => fluxes_data%esoil_surft
fluxes%surf_ht_flux_sice => fluxes_data%surf_ht_flux_sice
fluxes%sea_ice_htf_sicat => fluxes_data%sea_ice_htf_sicat
fluxes%surf_ht_flux_ij => fluxes_data%surf_ht_flux_ij
fluxes%surf_htf_surft => fluxes_data%surf_htf_surft
fluxes%snow_soil_htf => fluxes_data%snow_soil_htf
fluxes%land_albedo_ij => fluxes_data%land_albedo_ij
fluxes%ei_ij => fluxes_data%ei_ij
fluxes%ei_surft => fluxes_data%ei_surft
fluxes%ecan_surft => fluxes_data%ecan_surft
fluxes%esoil_ij_soilt => fluxes_data%esoil_ij_soilt
fluxes%ext_soilt => fluxes_data%ext_soilt
fluxes%hf_snow_melt_gb => fluxes_data%hf_snow_melt_gb
fluxes%radnet_surft => fluxes_data%radnet_surft
fluxes%sw_surft => fluxes_data%sw_surft
fluxes%emis_surft => fluxes_data%emis_surft
fluxes%snow_melt_gb => fluxes_data%snow_melt_gb
fluxes%snomlt_sub_htf_gb => fluxes_data%snomlt_sub_htf_gb
fluxes%tot_tfall_gb => fluxes_data%tot_tfall_gb
fluxes%melt_surft => fluxes_data%melt_surft
fluxes%snowinc_surft => fluxes_data%snowinc_surft
fluxes%tot_tfall_surft => fluxes_data%tot_tfall_surft
fluxes%sice_melt => fluxes_data%sice_melt
fluxes%ei_sice => fluxes_data%ei_sice
fluxes%z0m_surft => fluxes_data%z0m_surft
fluxes%z0h_surft => fluxes_data%z0h_surft
fluxes%rflow_gb => fluxes_data%rflow_gb
fluxes%rrun_gb => fluxes_data%rrun_gb
fluxes%lake_evap => fluxes_data%lake_evap

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fluxes_assoc

!==============================================================================

SUBROUTINE fluxes_nullify(fluxes)

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
  !Instance of the pointer type we are nullifying

!Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='FLUXES_NULLIFY'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

NULLIFY(fluxes%l_emis_surft_set)
NULLIFY(fluxes%surf_ht_store_surft)
NULLIFY(fluxes%anthrop_heat_surft)
NULLIFY(fluxes%sw_sicat)
NULLIFY(fluxes%alb_sicat)
NULLIFY(fluxes%penabs_rad_frac)
NULLIFY(fluxes%sw_sea)
NULLIFY(fluxes%sub_surf_roff_gb)
NULLIFY(fluxes%surf_roff_gb)
NULLIFY(fluxes%alb_surft)
NULLIFY(fluxes%tstar_ij)
NULLIFY(fluxes%e_sea_ij)
NULLIFY(fluxes%fsmc_pft)
NULLIFY(fluxes%ftl_surft)
NULLIFY(fluxes%le_surft)
NULLIFY(fluxes%h_sea_ij)
NULLIFY(fluxes%fqw_surft)
NULLIFY(fluxes%fqw_sicat)
NULLIFY(fluxes%ftl_sicat)
NULLIFY(fluxes%ecan_ij)
NULLIFY(fluxes%esoil_surft)
NULLIFY(fluxes%surf_ht_flux_sice)
NULLIFY(fluxes%sea_ice_htf_sicat)
NULLIFY(fluxes%surf_ht_flux_ij)
NULLIFY(fluxes%surf_htf_surft)
NULLIFY(fluxes%snow_soil_htf)
NULLIFY(fluxes%land_albedo_ij)
NULLIFY(fluxes%ei_ij)
NULLIFY(fluxes%ei_surft)
NULLIFY(fluxes%ecan_surft)
NULLIFY(fluxes%esoil_ij_soilt)
NULLIFY(fluxes%ext_soilt)
NULLIFY(fluxes%hf_snow_melt_gb)
NULLIFY(fluxes%radnet_surft)
NULLIFY(fluxes%sw_surft)
NULLIFY(fluxes%emis_surft)
NULLIFY(fluxes%snow_melt_gb)
NULLIFY(fluxes%snomlt_sub_htf_gb)
NULLIFY(fluxes%tot_tfall_gb)
NULLIFY(fluxes%melt_surft)
NULLIFY(fluxes%snowinc_surft)
NULLIFY(fluxes%tot_tfall_surft)
NULLIFY(fluxes%sice_melt)
NULLIFY(fluxes%ei_sice)
NULLIFY(fluxes%z0m_surft)
NULLIFY(fluxes%z0h_surft)
NULLIFY(fluxes%rflow_gb)
NULLIFY(fluxes%rrun_gb)
NULLIFY(fluxes%lake_evap)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE fluxes_nullify

END MODULE fluxes_mod
