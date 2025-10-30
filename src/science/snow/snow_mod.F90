! *****************************COPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE SNOW-----------------------------------------------------

! Description:
!     Calling routine for snow module

! Subroutine Interface:
MODULE snow_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SNOW_MOD'

CONTAINS

SUBROUTINE snow ( land_pts, timestep, stf_hf_snow_melt, nsurft, n_wtrac_jls,   &
                  surft_pts, surft_index, catch_snow, con_snow,                &
                  con_rain, tile_frac, ls_snow, ls_graup, ls_rain, ei_surft,   &
                  hcaps1_soilt, hcons, melt_surft, snowinc_surft,              &
                  smcl1_soilt, sthf1_soilt, surf_htf_surft,                    &
                  t_soil1_soilt, tsurf_elev_surft, tstar_surft,                &
                  smvcst1_soilt, con_snow_wtrac, ei_surft_wtrac,               &
                  rgrain, rgrainl, rho_snow_grnd, sice,                        &
                  sliq, snow_grnd, snow_surft, snowdepth,                      &
                  tsnow, nsnow, con_rain_wtrac, ls_rain_wtrac,                 &
                  ls_snow_wtrac, ls_graup_wtrac, melt_surft_wtrac,             &
                  snow_grnd_wtrac, snow_surft_wtrac, sice_wtrac,               &
                  sliq_wtrac,                                                  &
                  ds, hf_snow_melt, lying_snow,                                &
                  rho_snow, snomlt_sub_htf, snow_melt,                         &
                  snow_soil_htf,  surf_ht_flux_ld,  sf_diag,                   &
                  dhf_surf_minus_soil, snow_melt_wtrac,                        &
                  lake_snow_melt_wtrac,                                        &
                  ! New Arguments to replace USE statements
                  ! jules_internal
                  unload_backgrnd_pft, npft, l_flake_model,                    &
                  !Ancil info (IN)
                  l_lice_point, l_lice_surft,                                  &
                  ! Types Variables
                  lake_h_ice_gb, hcon_lake, ts1_lake_gb, lake_snow_melt,       &
                  non_lake_frac, lake_h_mxl_gb, lake_depth_gb                  &
                  )

USE canopysnow_mod,  ONLY: canopysnow
USE compactsnow_mod, ONLY: compactsnow
USE layersnow_mod,   ONLY: layersnow

USE relayersnow_mod, ONLY: relayersnow
USE snowgrain_mod,   ONLY: snowgrain
USE snowpack_mod,    ONLY: snowpack
USE snowtherm_mod,   ONLY: snowtherm

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
   lf
     ! Latent heat of fusion of water at 0degc (J kg-1).

USE jules_snow_mod, ONLY:                                                      &
  nsmax,                                                                       &
    ! Maximum possible number of snow layers.
  l_snow_infilt,                                                               &
    ! Include infiltration of rain into the snowpack.
  graupel_options,                                                             &
    ! Switch for treatment of graupel in the snow scheme.
  graupel_as_snow,                                                             &
    ! Include graupel in the surface snowfall.
  ignore_graupel,                                                              &
    ! Ignore graupel in the surface snowfall.
  r0,                                                                          &
    ! Grain size for fresh snow (microns).
  cansnowtile
    ! Switch for canopy snow model.

USE jules_surface_types_mod, ONLY: lake, ntype

USE jules_radiation_mod, ONLY: l_snow_albedo, l_embedded_snow

USE sf_diags_mod, ONLY: strnewsfdiag

USE ancil_info, ONLY: nsoilt

USE jules_water_tracers_mod, ONLY: l_wtrac_jls, wtrac_calc_ratio_fn_jules
USE wtrac_snow_mod,          ONLY: wtrac_sn_type, wtrac_alloc_snow,            &
                                   wtrac_dealloc_snow

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts              ! Total number of land points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep              ! Timestep length (s).

LOGICAL, INTENT(IN) ::                                                         &
  stf_hf_snow_melt,                                                            &
                        ! Stash flag for snowmelt heat flux.
  l_flake_model         ! Switch for using the FLake lake model

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nsurft,                                                                      &
    ! Number of land tiles.
  n_wtrac_jls,                                                                 &
    ! Number of water tracers
  surft_pts(nsurft),                                                           &
    ! Number of tile points.
  surft_index(land_pts,nsurft)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  catch_snow(land_pts,nsurft),                                                 &
    ! Canopy snow capacity (kg/m2).
  con_snow(land_pts),                                                          &
    ! Convective snowfall rate (kg/m2/s).
  tile_frac(land_pts,nsurft),                                                  &
    ! Tile fractions.
  ei_surft(land_pts,nsurft),                                                   &
    ! Sublimation of snow (kg/m2/s).
  snowinc_surft(land_pts,nsurft),                                              &
    ! Total increment to snow from the surface scheme, including sublimation
    ! and melting (kg m-2 TS-1).
    ! Using the increment, rather than the rate times the timestep allows
    ! the mass of snow to be reduced to 0, whereas using the rate can
    ! result in very small presisting amounts of snow because of rounding
    ! errors.
  hcaps1_soilt(land_pts,nsoilt),                                               &
    ! Soil heat capacity of top layer(J/K/m3).
  hcons(land_pts),                                                             &
    ! Thermal conductivity of top soil layer,
    ! including water and ice (W/m/K).
  hcon_lake(land_pts),                                                         &
    ! Thermal conductivity of surface layer of the lake (W/m/K).
  non_lake_frac(land_pts),                                                     &
    ! Sum of non-FLake tile fractions used for weighted average of
    ! surf_ht_flux_ld and snow_melt.
  smcl1_soilt(land_pts,nsoilt),                                                &
    ! Moisture content of surface soil layer (kg/m2).
  sthf1_soilt(land_pts,nsoilt),                                                &
    ! Frozen soil moisture content of surface layer as a fraction
    ! of saturation.
  surf_htf_surft(land_pts,nsurft),                                             &
    ! Surface heat flux (W/m2).
  tstar_surft(land_pts,nsurft),                                                &
    ! Tile surface temperature (K).
  smvcst1_soilt(land_pts,nsoilt),                                              &
    ! Surface soil layer volumetric moisture concentration at saturation.
  con_snow_wtrac(land_pts,n_wtrac_jls),                                        &
    ! Water tracer convective snowfall rate (kg/m2/s).
  ei_surft_wtrac(land_pts,nsurft,n_wtrac_jls)
    ! Water tracer sublimation of snow (kg/m2/s).

!-----------------------------------------------------------------------------
! Array arguments with intent(inout)
!-----------------------------------------------------------------------------
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

INTEGER, INTENT(IN OUT) ::                                                     &
  nsnow(land_pts,nsurft)   ! Number of snow layers.

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  con_rain(land_pts),                                                          &
    ! Convective rainfall rate (kg/m2/s).
  ls_snow(land_pts),                                                           &
    ! Large-scale frozen precip fall rate (kg/m2/s).
  ls_graup(land_pts),                                                          &
    ! Large-scale graupel fall rate (kg/m2/s).
  ls_rain(land_pts),                                                           &
    ! Large-scale rainfall rate (kg/m2/s).
  melt_surft(land_pts,nsurft),                                                 &
    ! Surface or canopy snowmelt rate (kg/m2/s).
    ! On output, this is the total melt rate for the tile
    ! (i.e. sum of  melt on canopy and ground).
  t_soil1_soilt(land_pts,nsoilt),                                              &
    ! Soil surface layer temperature (K).
  tsurf_elev_surft(land_pts,nsurft),                                           &
    ! Temperature of elevated subsurface tiles (K).
  ts1_lake_gb(land_pts),                                                       &
    ! Temperature of the surface layer of the lake (K).
  rgrain(land_pts,nsurft),                                                     &
    ! Snow surface grain size (microns).
  rgrainl(land_pts,nsurft,nsmax),                                              &
    ! Snow layer grain size (microns).
  rho_snow_grnd(land_pts,nsurft),                                              &
    ! Snowpack bulk density (kg/m3).
  sice(land_pts,nsurft,nsmax),                                                 &
    ! Ice content of snow layers (kg/m2).
  sliq(land_pts,nsurft,nsmax),                                                 &
    ! Liquid content of snow layers (kg/m2).
  snow_grnd(land_pts,nsurft),                                                  &
    ! Snow beneath canopy (kg/m2).
  snow_surft(land_pts,nsurft),                                                 &
    ! Snow mass (kg/m2).
  snowdepth(land_pts,nsurft),                                                  &
    ! Snow depth (m).
  tsnow(land_pts,nsurft,nsmax),                                                &
    ! Snow layer temperatures (K).
  lake_h_ice_gb(land_pts),                                                     &
    ! lake ice thickness (m)
  lake_h_mxl_gb(land_pts),                                                     &
    ! lake mixed layer thickness (m)
  lake_depth_gb(land_pts),                                                     &
    ! lake depth (m)
  con_rain_wtrac(land_pts,n_wtrac_jls),                                        &
    ! Water tracer convective rainfall rate (kg/m2/s).
  ls_snow_wtrac(land_pts,n_wtrac_jls),                                         &
    ! Water tracer large-scale frozen precip fall rate (kg/m2/s).
  ls_graup_wtrac(land_pts,n_wtrac_jls),                                        &
    ! Water tracer large-scale graupel fall rate (kg/m2/s).
  ls_rain_wtrac(land_pts,n_wtrac_jls),                                         &
    ! Water tracer large-scale rainfall rate (kg/m2/s).
  melt_surft_wtrac(land_pts,nsurft,n_wtrac_jls),                               &
    ! Surface or canopy water tracer snowmelt rate (kg/m2/s).
    ! On output, this is the total melt rate for the tile
    ! (i.e. sum of  melt on canopy and ground).
  snow_grnd_wtrac(land_pts,nsurft,n_wtrac_jls),                                &
    ! Water tracer snow content beneath canopy (kg/m2).
  snow_surft_wtrac(land_pts,nsurft,n_wtrac_jls),                               &
    ! Water tracer snow mass content(kg/m2).
  sice_wtrac(land_pts,nsurft,nsmax,n_wtrac_jls),                               &
    ! Water tracer ice content of snow layers (kg/m2).
  sliq_wtrac(land_pts,nsurft,nsmax,n_wtrac_jls)
    ! Water tracer liquid content of snow layers (kg/m2).


!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  ds(land_pts,nsurft,nsmax),                                                   &
    ! Snow layer thicknesses (m).
  hf_snow_melt(land_pts),                                                      &
    ! Gridbox snowmelt heat flux (W/m2).
  lake_snow_melt(land_pts),                                                    &
    ! snowmelt on the lake tile when using FLake (kg/m2/s).
  lying_snow(land_pts),                                                        &
    ! Gridbox snow mass (kg m-2).
  rho_snow(land_pts,nsurft,nsmax),                                             &
    ! Snow layer densities (kg/m3).
  snomlt_sub_htf(land_pts),                                                    &
    ! Sub-canopy snowmelt heat flux (W/m2).
  snow_melt(land_pts),                                                         &
    ! Gridbox snowmelt (kg/m2/s).
  surf_ht_flux_ld(land_pts),                                                   &
    ! Surface heat flux on land (W/m2).
  snow_soil_htf(land_pts,nsurft),                                              &
    ! Heat flux into the uppermost subsurface layer (W/m2).
    ! i.e. snow to ground, or into snow/soil composite layer.
  dhf_surf_minus_soil(land_pts),                                               &
    ! Heat flux difference across the FLake snowpack (W/m2).
  snow_melt_wtrac(land_pts,n_wtrac_jls),                                       &
    ! GBM water tracer snowmelt (kg/m2/s).
  lake_snow_melt_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer snowmelt on the lake tile when using FLake (kg/m2/s).

!-----------------------------------------------------------------------------
! New arguments to replace USE statements
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: npft
REAL(KIND=real_jlslsm), INTENT(IN) :: unload_backgrnd_pft(land_pts,npft)

!ancil_info (IN)
LOGICAL, INTENT(IN) :: l_lice_point(land_pts)
LOGICAL, INTENT(IN) :: l_lice_surft(ntype)

!-----------------------------------------------------------------------------
! Local scalars
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,                                                                           &
    ! Land point index and loop counter.
  j,                                                                           &
    ! Tile pts loop counter.
  k,                                                                           &
    ! Tile number.
  n,                                                                           &
    ! Tile loop counter.
  m,                                                                           &
    ! Soil tile index.
  ns,                                                                          &
    ! Snow layer index
  n_can,                                                                       &
    ! Actual or dummy pointer to array defined only on PFTs.
  i_wt,                                                                        &
    ! Water tracer loop counter
  n_wt
    ! Actual or dummy pointer to array defined only when running with water
    ! tracers

!-----------------------------------------------------------------------------
! Local arrays
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  csnow(land_pts,nsmax),                                                       &
    ! Areal heat capacity of layers (J/K/m2).
  ksnow(land_pts,nsmax),                                                       &
    ! Thermal conductivity of layers (W/m/K).
  rho0(land_pts),                                                              &
    ! Density of fresh snow (kg/m3).
    ! Where nsnow=0, rho0 is the density of the snowpack.
  snowfall(land_pts),                                                          &
    ! Total frozen precip reaching the ground in timestep
    ! (kg/m2) - includes any canopy unloading.
  graupfall(land_pts),                                                         &
    ! Graupel reaching the ground in timestep(kg/m2).
  infiltration(land_pts),                                                      &
    ! Infiltration of rainfall into snow pack
    ! on the current tile (kg/m2).
  infil_rate_con_gbm(land_pts),                                                &
    ! Grid-box mean rate of infiltration of convective rainfall into
    ! snowpack (kg/m2/s).
  infil_rate_ls_gbm(land_pts),                                                 &
    ! Grid-box mean rate of infiltration of large-scale rainfall into
    ! snowpack (kg/m2/s).
  infil_ground_con_gbm(land_pts),                                              &
    ! Grid-box mean rate of infiltration of convective rainfall into
    ! ground (kg/m2/s)
  infil_ground_ls_gbm(land_pts),                                               &
    ! Grid-box mean rate of infiltration of large-scale rainfall into
    ! ground (kg/m2/s).
  snowmass(land_pts),                                                          &
    ! Snow mass on the ground (kg/m2).
  rgrain0(land_pts),                                                           &
    ! Fresh snow grain size (microns).
  sice0(land_pts),                                                             &
   ! Ice content of fresh snow (kg/m2).
   ! Where nsnow=0, sice0 is the mass of the snowpack.
  snow_can(land_pts,nsurft),                                                   &
    ! Canopy snow load (kg/m2).
  tsnow0(land_pts)
    ! Temperature of fresh snow (K).

! Snow quantities on a single surft, i.e. over snow layers (sl)
REAL(KIND=real_jlslsm) ::                                                      &
  ds_sl(land_pts,nsmax),                                                       &
    ! Snow layer thicknesses (m).
  sice_sl(land_pts,nsmax),                                                     &
    ! Ice content of snow layers (kg/m2).
  sliq_sl(land_pts,nsmax),                                                     &
    ! Liquid content of snow layers (kg/m2).
  tsnow_sl(land_pts,nsmax),                                                    &
    ! Temperature of fresh snow (K).
  rho_snow_sl(land_pts,nsmax),                                                 &
    ! Snow layer densities (kg/m3).
  rgrainl_sl(land_pts,nsmax)
    ! Snow layer grain size (microns).

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  snow_surft_old(:,:),                                                         &
  snow_grnd_old(:,:),                                                          &
  nsnow_old(:,:),                                                              &
  sice_old(:,:),                                                               &
  sliq_old(:,:)
    ! Temporary copies of things that may be needed for diagnostics.

REAL(KIND=real_jlslsm) :: snow_ratio    ! Ratio of water tracer to water

TYPE(wtrac_sn_type) :: wtrac_sn         ! Water tracer working arrays


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SNOW'

!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate water tracer arrays and initialise GBM fields
CALL wtrac_alloc_snow(land_pts, nsurft, nsmax, n_wtrac_jls, wtrac_sn)

!-----------------------------------------------------------------------------
! Initialise gridbox variables.
!-----------------------------------------------------------------------------

IF (sf_diag%l_snice) THEN
  ALLOCATE( snow_surft_old(land_pts,nsurft) )
  ALLOCATE( snow_grnd_old(land_pts,nsurft)  )
  ALLOCATE( nsnow_old(land_pts,nsurft)      )
  ALLOCATE( sice_old(land_pts,nsurft)       )
  ALLOCATE( sliq_old(land_pts,nsurft)       )

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(n,k,i)                                                           &
!$OMP SHARED(nsurft,surft_pts,land_pts,sf_diag,snow_surft_old,snow_surft,      &
!$OMP        snow_grnd_old,snow_grnd,sice_old,sliq_old,nsmax,nsnow,            &
!$OMP        sice,sliq,surft_index)

  ! Zeroing diagnostics
  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      sf_diag%snice_m_surft(i,n)        = 0.0
      sf_diag%snice_freez_surft(i,n)    = 0.0
      sf_diag%snice_sicerate_surft(i,n) = 0.0
      sf_diag%snice_sliqrate_surft(i,n) = 0.0
      sf_diag%snice_runoff_surft(i,n)   = 0.0
      sf_diag%snice_smb_surft(i,n)      = 0.0
    END DO
!$OMP END DO NOWAIT

!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      snow_surft_old(i,n)               = snow_surft(i,n)
      snow_grnd_old(i,n)                = snow_grnd(i,n)
      sice_old(i,n)                     = 0.0
      sliq_old(i,n)                     = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO

  IF (nsmax > 0) THEN
    DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        i = surft_index(k,n)
        IF (nsnow(i,n) > 0) THEN
          sice_old(i,n) = SUM(sice(i,n,1:nsnow(i,n)))
          sliq_old(i,n) = SUM(sliq(i,n,1:nsnow(i,n)))
        END IF
      END DO
!$OMP END DO
    END DO
  END IF

!$OMP END PARALLEL

END IF

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(i,i_wt)                                                          &
!$OMP SHARED(land_pts,lying_snow,snomlt_sub_htf,snow_melt,infil_rate_con_gbm,  &
!$OMP        infil_rate_ls_gbm,dhf_surf_minus_soil,graupel_options,            &
!$OMP        ls_graup,ls_snow,lake_snow_melt,infil_ground_con_gbm,             &
!$OMP        infil_ground_ls_gbm,l_wtrac_jls,ls_graup_wtrac,ls_snow_wtrac,     &
!$OMP        lake_snow_melt_wtrac,snow_melt_wtrac,n_wtrac_jls)
!$OMP DO SCHEDULE(STATIC)
DO i = 1, land_pts
  lying_snow(i)           = 0.0
  snomlt_sub_htf(i)       = 0.0
  snow_melt(i)            = 0.0
  lake_snow_melt(i)       = 0.0
  infil_rate_con_gbm(i)   = 0.0
  infil_rate_ls_gbm(i)    = 0.0
  infil_ground_con_gbm(i) = 0.0
  infil_ground_ls_gbm(i)  = 0.0

  ! initialise FLake tile flux divergence
  dhf_surf_minus_soil(i) = 0.0
END DO
!$OMP END DO

IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      snow_melt_wtrac(i,i_wt)      = 0.0
      lake_snow_melt_wtrac(i,i_wt) = 0.0
    END DO
!$OMP END DO
  END DO
END IF !l_wtrac_jls

! Set required snow fall
IF (graupel_options == graupel_as_snow) THEN
  ! leave graupel to be treated as snow (ls_snow is total frozen precip)
!$OMP DO SCHEDULE(STATIC)
  DO i = 1, land_pts
    ls_graup(i) = 0.0
  END DO
!$OMP END DO
  ! Repeat for water tracers
  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        ls_graup_wtrac(i,i_wt) = 0.0
      END DO
!$OMP END DO
    END DO
  END IF

ELSE IF (graupel_options == ignore_graupel) THEN
  ! get rid of graupel from ls_snow and throw it away completely
!$OMP DO SCHEDULE(STATIC)
  DO i = 1, land_pts
    ls_snow(i)  = MAX(0.0, ls_snow(i) - ls_graup(i) )
    ls_graup(i) = 0.0
  END DO
!$OMP END DO
  ! Repeat for water tracers
  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        ls_snow_wtrac(i,i_wt)  = MAX(0.0,                                      &
                              ls_snow_wtrac(i,i_wt) - ls_graup_wtrac(i,i_wt) )
        ls_graup_wtrac(i,i_wt) = 0.0
      END DO
!$OMP END DO
    END DO
  END IF
END IF

!$OMP END PARALLEL

! Calculate surface melt for water tracers
IF (l_wtrac_jls) THEN
!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(k,i,n,i_wt,snow_ratio)                                           &
!$OMP SHARED(surft_pts,surft_index,snow_surft_wtrac,snow_surft,melt_surft,     &
!$OMP        melt_surft_wtrac,n_wtrac_jls,nsurft)
  DO i_wt = 1, n_wtrac_jls
    DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        i = surft_index(k,n)
        snow_ratio = wtrac_calc_ratio_fn_jules(i_wt,                           &
                           snow_surft_wtrac(i,n,i_wt),snow_surft(i,n))
        melt_surft_wtrac(i,n,i_wt) = melt_surft(i,n) * snow_ratio
      END DO
!$OMP END DO
    END DO
  END DO
!$OMP END PARALLEL
END IF ! l_wtrac_jls

DO n = 1,nsurft
  !---------------------------------------------------------------------------
  ! Set snow mass variables
  !---------------------------------------------------------------------------

  IF ( cansnowtile(n) ) THEN

    !-------------------------------------------------------------------------
    ! With the canopy snow model, snow_surft is held on the canopy,
    ! while the mass of the snowpack (on the ground) is snow_grnd.
    !-------------------------------------------------------------------------
!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(k,i,i_wt)                                                        &
!$OMP SHARED(surft_pts,surft_index,snow_can,snow_surft,snowmass,snow_grnd,     &
!$OMP        timestep,snowinc_surft,n,l_wtrac_jls,wtrac_sn,snow_surft_wtrac,   &
!$OMP        snow_grnd_wtrac,ei_surft_wtrac,n_wtrac_jls,melt_surft_wtrac)
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      snow_can(i,n) = snow_surft(i,n)
      snowmass(i)   = snow_grnd(i,n)

      !-----------------------------------------------------------------------
      ! Subtract sublimation and melt of canopy snow.
      !-----------------------------------------------------------------------
      snow_can(i,n) = snow_can(i,n) + snowinc_surft(i,n)

    END DO
!$OMP END DO NOWAIT

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          i = surft_index(k,n)
          wtrac_sn%snow_can(i,n,i_wt) = snow_surft_wtrac(i,n,i_wt)
          wtrac_sn%snowmass(i,i_wt)   = snow_grnd_wtrac(i,n,i_wt)

          !---------------------------------------------------------------------
          ! Subtract sublimation and melt of canopy snow.
          !---------------------------------------------------------------------
          wtrac_sn%snow_can(i,n,i_wt) = wtrac_sn%snow_can(i,n,i_wt) -          &
               ( ei_surft_wtrac(i,n,i_wt) + melt_surft_wtrac(i,n,i_wt) )       &
                * timestep

        END DO
!$OMP END DO
      END DO
    END IF  ! l_wtrac_jls
!$OMP END PARALLEL

  ELSE

    !-------------------------------------------------------------------------
    ! Without the snow canopy model, all the snow is in the snowpack
    !-------------------------------------------------------------------------
!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(k,i,i_wt)                                                        &
!$OMP SHARED(surft_pts,surft_index,snowmass,snow_surft,n,l_wtrac_jls,wtrac_sn, &
!$OMP        snow_surft_wtrac,n_wtrac_jls)
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      snowmass(i) = snow_surft(i,n)
    END DO
!$OMP END DO NOWAIT

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          i = surft_index(k,n)
          wtrac_sn%snowmass(i,i_wt) = snow_surft_wtrac(i,n,i_wt)
        END DO
!$OMP END DO
      END DO
    END IF ! l_wtrac_jls
!$OMP END PARALLEL

  END IF

  ! Copy data for this surft to the snow layer (sl) arrays
!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(ns,i,i_wt)                                                       &
!$OMP SHARED(nsmax,land_pts,sice,sice_sl,sliq,sliq_sl,tsnow,tsnow_sl,          &
!$OMP        l_snow_albedo,l_embedded_snow,rgrainl,rgrainl_sl,n,               &
!$OMP        l_wtrac_jls,n_wtrac_jls,wtrac_sn,sice_wtrac,sliq_wtrac)
  DO ns = 1, nsmax
!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      sice_sl(i,ns) = sice(i,n,ns)
      sliq_sl(i,ns) = sliq(i,n,ns)
      tsnow_sl(i,ns) = tsnow(i,n,ns)
    END DO
!$OMP END DO NOWAIT
  END DO

  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
    DO i_wt = 1, n_wtrac_jls
      DO ns = 1, nsmax
!$OMP DO SCHEDULE(STATIC)
        DO i = 1, land_pts
          wtrac_sn%sice_sl(i,ns,i_wt) = sice_wtrac(i,n,ns,i_wt)
          wtrac_sn%sliq_sl(i,ns,i_wt) = sliq_wtrac(i,n,ns,i_wt)
        END DO
!$OMP END DO NOWAIT
      END DO
    END DO

  END IF

  ! Only need rgrainl when certain science options are turned on
  IF (l_snow_albedo .OR. l_embedded_snow) THEN
    DO ns = 1, nsmax
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        rgrainl_sl(i,ns) = rgrainl(i,n,ns)
      END DO
!$OMP END DO NOWAIT
    END DO
  END IF

!$OMP END PARALLEL

  !---------------------------------------------------------------------------
  ! Canopy interception, throughfall and unloading of snow
  !---------------------------------------------------------------------------
  ! The canopy model applies only on PFTs, so set a dummy pointer
  ! in the array on unused tiles.
  IF (cansnowtile(n)) THEN
    n_can = n
  ELSE
    n_can = 1
  END IF

  ! Set tile pointer for water tracers
  IF (l_wtrac_jls) THEN
    n_wt = n
  ELSE
    n_wt = 1    ! Dummy pointer
  END IF

  CALL canopysnow ( land_pts, surft_pts(n), n_wtrac_jls,                       &
                    timestep, cansnowtile(n),                                  &
                    surft_index(:,n), catch_snow(:,n), con_snow,               &
                    ls_snow, ls_graup, unload_backgrnd_pft(:,n_can),           &
                    melt_surft(:,n), con_snow_wtrac,                           &
                    ls_snow_wtrac, ls_graup_wtrac,                             &
                    snow_can(:,n), wtrac_sn%snow_can(:,n_wt,:),                &
                    snowfall, graupfall, wtrac_sn%snowfall,                    &
                    wtrac_sn%graupfall)


  !---------------------------------------------------------------------------
  ! Divide snow pack into layers
  !---------------------------------------------------------------------------

  CALL layersnow ( land_pts, surft_pts(n), surft_index(:, n),                  &
                   snowdepth(:,n), nsnow(:,n), ds_sl )

  !---------------------------------------------------------------------------
  ! Thermal properties of snow layers
  !---------------------------------------------------------------------------
  IF ( nsmax > 0 ) THEN
    CALL snowtherm ( land_pts, surft_pts(n), nsnow(:,n),                       &
                     surft_index(:,n), ds_sl, sice_sl,                         &
                     sliq_sl, csnow, ksnow )
  END IF

  !---------------------------------------------------------------------------
  ! Snow thermodynamics and hydrology
  !---------------------------------------------------------------------------
  IF (l_snow_infilt) THEN
!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(k,i,i_wt)                                                        &
!$OMP SHARED(surft_pts,surft_index,nsnow,infiltration,ls_rain,con_rain,        &
!$OMP        timestep,infil_rate_con_gbm,tile_frac,infil_rate_ls_gbm,n,        &
!$OMP        infil_ground_con_gbm,infil_ground_ls_gbm,                         &
!$OMP        l_wtrac_jls,ls_rain_wtrac,con_rain_wtrac,wtrac_sn,n_wtrac_jls)
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)

      ! Where there is snow on the ground direct rainfall into infiltration.
      IF (nsnow(i,n) > 0) THEN
        infiltration(i)       = ( ls_rain(i) + con_rain(i) ) * timestep
        infil_rate_con_gbm(i) = infil_rate_con_gbm(i) +                        &
                                tile_frac(i,n) * con_rain(i)
        infil_rate_ls_gbm(i)  = infil_rate_ls_gbm(i) +                         &
                                tile_frac(i,n) * ls_rain(i)
      ELSE
        infiltration(i) = 0.0
        infil_ground_con_gbm(i) = infil_ground_con_gbm(i) +                    &
                                  tile_frac(i,n) * con_rain(i)
        infil_ground_ls_gbm(i)  = infil_ground_ls_gbm(i) +                     &
                                tile_frac(i,n) * ls_rain(i)
      END IF
    END DO
!$OMP END DO NOWAIT

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          i = surft_index(k,n)

          ! Where there is snow on the ground direct rainfall into
          !infiltration.
          IF (nsnow(i,n) > 0) THEN
            wtrac_sn%infiltration(i,i_wt)  = ( ls_rain_wtrac(i,i_wt) +         &
                                   con_rain_wtrac(i,i_wt) ) * timestep
            wtrac_sn%infil_rate_con_gbm(i,i_wt) =                              &
                                   wtrac_sn%infil_rate_con_gbm(i,i_wt) +       &
                                   tile_frac(i,n) * con_rain_wtrac(i,i_wt)
            wtrac_sn%infil_rate_ls_gbm(i,i_wt)  =                              &
                                   wtrac_sn%infil_rate_ls_gbm(i,i_wt) +        &
                                   tile_frac(i,n) * ls_rain_wtrac(i,i_wt)
          ELSE
            wtrac_sn%infiltration(i,i_wt)         = 0.0
            wtrac_sn%infil_ground_con_gbm(i,i_wt) =                            &
                                   wtrac_sn%infil_ground_con_gbm(i,i_wt) +     &
                                   tile_frac(i,n) * con_rain_wtrac(i,i_wt)
            wtrac_sn%infil_ground_ls_gbm(i,i_wt)  =                            &
                                   wtrac_sn%infil_ground_ls_gbm(i,i_wt) +      &
                                   tile_frac(i,n) * ls_rain_wtrac(i,i_wt)
          END IF
        END DO
!$OMP END DO NOWAIT
      END DO
    END IF  ! l_wtrac_jls
!$OMP END PARALLEL

  ELSE
!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(k,i,i_wt)                                                        &
!$OMP SHARED(surft_pts,surft_index,infiltration,n,l_wtrac_jls,wtrac_sn,        &
!$OMP        n_wtrac_jls)
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      infiltration(i) = 0.0
    END DO
!$OMP END DO NOWAIT

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          i = surft_index(k,n)
          wtrac_sn%infiltration(i,i_wt) = 0.0
        END DO
!$OMP END DO NOWAIT
      END DO
    END IF ! l_wtrac_jls
!$OMP END PARALLEL
  END IF

  !==========================================================================
  ! *NOTICE REGARDING SOIL TILING**
  !
  !The following section facilitates the use of soil tiling. As implemented,
  !there are two soil tiling options:
  !
  !nsoilt == 1
  !Operate as with a single soil tile, functionally identical to JULES upto
  ! at least vn4.7 (Oct 2016)
  ! This means that a soilt variable being passed 'up' to the surface is
  ! broadcast to the surft variable (with weighting by frac if requred)
  !
  !nsoilt > 1
  !Operate with nsoilt = nsurft, with a direct mapping between them
  ! This means that a soilt variable being passed 'up' to the surface is
  ! simply copied into the surft variable
  !
  ! This will need to be refactored for other tiling approaches. This note
  ! will be replicated elsewhere in the code as required
  !
  !These comments apply until **END NOTICE REGARDING SOIL TILING**
  !===========================================================================

  !Set the current soil tile (see notice above).
  IF (nsoilt == 1) THEN
    !There is only 1 soil tile
    m = 1
  ELSE ! nsoilt == nsurft
    !Soil tiles map directly on to surface tiles
    m = n
  END IF !nsoilt

  IF ((l_flake_model) .AND. (n == lake)) THEN
    CALL snowpack ( n, land_pts, surft_pts(n), n_wtrac_jls, timestep,          &
                  cansnowtile(n), nsnow(:,n), surft_index(:,n), nsurft, csnow, &
                  ei_surft(:,n), hcaps1_soilt(:,m), hcon_lake, infiltration,   &
                  ksnow, rho_snow_grnd(:,n), smcl1_soilt(:,m),                 &
                  snowfall, sthf1_soilt(:,m), surf_htf_surft(:,n),             &
                  tile_frac, smvcst1_soilt(:,m), ei_surft_wtrac,               &
                  wtrac_sn%infiltration, ds_sl,                                &
                  melt_surft(:,n), snowinc_surft(:,n), sice_sl, sliq_sl,       &
                  snomlt_sub_htf, snowdepth(:,n), snowmass, tsnow_sl,          &
                  ts1_lake_gb, tsurf_elev_surft(:,n),                          &
                  melt_surft_wtrac, wtrac_sn%sice_sl, wtrac_sn%sliq_sl,        &
                  wtrac_sn%snowfall, wtrac_sn%snowmass,                        &
                  snow_soil_htf(:,n), rho_snow_sl, rho0, sice0, tsnow0,        &
                  wtrac_sn%sice0, sf_diag, non_lake_frac,                      &
                  !Ancil info (IN)
                  l_lice_point, l_lice_surft,                                  &
                  ! Types Variables
                  lake_h_ice_gb, lake_h_mxl_gb, lake_depth_gb)
  ELSE
    CALL snowpack ( n, land_pts, surft_pts(n), n_wtrac_jls, timestep,          &
                  cansnowtile(n), nsnow(:,n), surft_index(:,n), nsurft, csnow, &
                  ei_surft(:,n), hcaps1_soilt(:,m), hcons, infiltration,       &
                  ksnow, rho_snow_grnd(:,n), smcl1_soilt(:,m),                 &
                  snowfall, sthf1_soilt(:,m), surf_htf_surft(:,n),             &
                  tile_frac, smvcst1_soilt(:,m), ei_surft_wtrac,               &
                  wtrac_sn%infiltration, ds_sl,                                &
                  melt_surft(:,n), snowinc_surft(:,n), sice_sl, sliq_sl,       &
                  snomlt_sub_htf, snowdepth(:,n), snowmass, tsnow_sl,          &
                  t_soil1_soilt(:,m), tsurf_elev_surft(:,n),                   &
                  melt_surft_wtrac, wtrac_sn%sice_sl, wtrac_sn%sliq_sl,        &
                  wtrac_sn%snowfall, wtrac_sn%snowmass,                        &
                  snow_soil_htf(:,n), rho_snow_sl, rho0, sice0, tsnow0,        &
                  wtrac_sn%sice0, sf_diag, non_lake_frac,                      &
                  !Ancil info (IN)
                  l_lice_point, l_lice_surft,                                  &
                  ! Types Variables
                  lake_h_ice_gb, lake_h_mxl_gb, lake_depth_gb)

  END IF

  !===========================================================================
  ! *END NOTICE REGARDING SOIL TILING**
  !===========================================================================

  !---------------------------------------------------------------------------
  ! Growth of snow grains
  !---------------------------------------------------------------------------
  IF ( l_snow_albedo .OR. l_embedded_snow ) THEN
    CALL snowgrain ( land_pts, surft_pts(n), timestep, nsnow(:,n),             &
                     surft_index(:,n), sice_sl, snowfall,                      &
                     snowmass, tsnow_sl, tstar_surft(:,n),                     &
                     rgrain(:,n), rgrainl_sl, rgrain0 )
  ELSE
    ! Default initialization required for bit-comparison in the UM.
    rgrain0(:) = r0
  END IF

  IF ( nsmax > 0 ) THEN
    !-------------------------------------------------------------------------
    ! Mechanical compaction of snow
    !-------------------------------------------------------------------------
    CALL compactsnow ( land_pts, surft_pts(n), timestep, nsnow(:,n),           &
                       surft_index(:,n), sice_sl, sliq_sl,                     &
                       tsnow_sl, rho_snow_sl, ds_sl )

    !-------------------------------------------------------------------------
    ! Redivide snowpack after changes in depth, conserving mass and energy
    !-------------------------------------------------------------------------
    CALL relayersnow ( land_pts, surft_pts(n), n_wtrac_jls, surft_index(:,n),  &
                       rgrain0, rho0, sice0, snowfall,                         &
                       snowmass, tsnow0, wtrac_sn%sice0, nsnow(:,n), ds_sl,    &
                       rgrain(:,n), rgrainl_sl, sice_sl,                       &
                       rho_snow_grnd(:,n), sliq_sl,                            &
                       tsnow_sl, wtrac_sn%sice_sl, wtrac_sn%sliq_sl,           &
                       rho_snow_sl, snowdepth(:,n) )

  END IF  !  NSMAX>0

  !---------------------------------------------------------------------------
  ! Copy into final snow mass variables
  !---------------------------------------------------------------------------

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,i,ns,i_wt)                                                     &
!$OMP SHARED(n,cansnowtile,surft_pts,surft_index,snow_grnd,snowmass,           &
!$OMP        snow_surft,snow_can,lying_snow,tile_frac,snow_melt,melt_surft,    &
!$OMP        nsmax,land_pts,ds,ds_sl,sice,sice_sl,sliq,sliq_sl,tsnow,          &
!$OMP        tsnow_sl,rho_snow,rho_snow_sl,l_snow_albedo,                      &
!$OMP        l_embedded_snow,rgrainl,rgrainl_sl, lake_snow_melt,               &
!$OMP        l_flake_model,lake,l_wtrac_jls,snow_grnd_wtrac,snow_surft_wtrac,  &
!$OMP        sice_wtrac,sliq_wtrac,lake_snow_melt_wtrac,snow_melt_wtrac,       &
!$OMP        wtrac_sn,melt_surft_wtrac,n_wtrac_jls)

  IF ( cansnowtile(n) ) THEN
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      snow_grnd(i,n)  = snowmass(i)
      snow_surft(i,n) = snow_can(i,n)
    END DO
!$OMP END DO
    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          i = surft_index(k,n)
          snow_grnd_wtrac(i,n,i_wt)  = wtrac_sn%snowmass(i,i_wt)
          snow_surft_wtrac(i,n,i_wt) = wtrac_sn%snow_can(i,n,i_wt)
        END DO
!$OMP END DO
      END DO
    END IF ! l_wtrac_jls
  ELSE
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      snow_surft(i,n) = snowmass(i)
    END DO
!$OMP END DO
    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          i = surft_index(k,n)
          snow_surft_wtrac(i,n,i_wt) = wtrac_sn%snowmass(i,i_wt)
        END DO
!$OMP END DO
      END DO
    END IF ! l_wtrac_jls
  END IF

  ! Copy data for this surft from the snow layer (sl) arrays
  DO ns = 1, nsmax
!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      ds(i,n,ns) = ds_sl(i,ns)
      sice(i,n,ns) = sice_sl(i,ns)
      sliq(i,n,ns) = sliq_sl(i,ns)
      tsnow(i,n,ns) = tsnow_sl(i,ns)
      rho_snow(i,n,ns) = rho_snow_sl(i,ns)
    END DO
!$OMP END DO NOWAIT
  END DO

  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
    DO i_wt = 1, n_wtrac_jls
      DO ns = 1, nsmax
!$OMP DO SCHEDULE(STATIC)
        DO i = 1, land_pts
          sice_wtrac(i,n,ns,i_wt) = wtrac_sn%sice_sl(i,ns,i_wt)
          sliq_wtrac(i,n,ns,i_wt) = wtrac_sn%sliq_sl(i,ns,i_wt)
        END DO
!$OMP END DO NOWAIT
      END DO
    END DO
  END IF ! l_wtrac_jls

  IF (l_snow_albedo .OR. l_embedded_snow) THEN
    DO ns = 1, nsmax
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        rgrainl(i,n,ns) = rgrainl_sl(i,ns)
      END DO
!$OMP END DO NOWAIT
    END DO
  END IF

  !---------------------------------------------------------------------------
  ! Increment gridbox lying snow and snow melt.
  !---------------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    i = surft_index(k,n)
    lying_snow(i) = lying_snow(i) +                                            &
                    tile_frac(i,n) * snow_surft(i,n)
    ! Add snow beneath canopy.
    IF ( cansnowtile(n) )                                                      &
      lying_snow(i) = lying_snow(i) +                                          &
                      tile_frac(i,n) * snow_grnd(i,n)

    IF ((l_flake_model) .AND. (n == lake)) THEN
      ! Exclude snowmelt on the lake from gridbox snowmelt so that it
      ! doesn't affect soil calculations.
      lake_snow_melt(i) = tile_frac(i,n) * melt_surft(i,n)
    ELSE
      ! Snow melt.
      snow_melt(i) = snow_melt(i) + tile_frac(i,n) * melt_surft(i,n)
    END IF
  END DO
!$OMP END DO

  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
    ! (lying_snow equivalent isn't calculated here as diagnostic only)
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        i = surft_index(k,n)
        IF ((l_flake_model) .AND. (n == lake)) THEN
          ! Exclude snowmelt on the lake from gridbox snowmelt so that it
          ! doesn't affect soil calculations.
          lake_snow_melt_wtrac(i,i_wt) =                                       &
                            tile_frac(i,n) * melt_surft_wtrac(i,n,i_wt)
        ELSE
          ! Snow melt.
          snow_melt_wtrac(i,i_wt) = snow_melt_wtrac(i,i_wt)                    &
                            + tile_frac(i,n) * melt_surft_wtrac(i,n,i_wt)
        END IF
      END DO
!$OMP END DO
    END DO
  END IF ! l_wtrac_jls
!$OMP END PARALLEL

END DO  !  tiles

IF (l_flake_model) THEN
!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(i,i_wt)                                                          &
!$OMP SHARED(land_pts,non_lake_frac,snow_melt,l_wtrac_jls,snow_melt_wtrac,     &
!$OMP        n_wtrac_jls)
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,land_pts
    IF (non_lake_frac(i) > EPSILON(0.0)) THEN
      snow_melt(i) = snow_melt(i) / non_lake_frac(i)
    END IF
  END DO
!$OMP END DO

  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO i = 1,land_pts
        IF (non_lake_frac(i) > EPSILON(0.0)) THEN
          snow_melt_wtrac(i,i_wt) = snow_melt_wtrac(i,i_wt) / non_lake_frac(i)
        END IF
      END DO
!$OMP END DO
    END DO
  END IF ! l_wtrac_jls
!$OMP END PARALLEL
END IF

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,i,n,j)                                                         &
!$OMP SHARED(stf_hf_snow_melt,land_pts,hf_snow_melt,snow_melt,surf_ht_flux_ld, &
!$OMP        nsurft,surft_pts,surft_index,tile_frac,snow_soil_htf,lake,        &
!$OMP        dhf_surf_minus_soil,surf_htf_surft, l_flake_model)

!-----------------------------------------------------------------------------
! Calculate the total snowmelt heat flux.
!-----------------------------------------------------------------------------
IF ( stf_hf_snow_melt ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,land_pts
    hf_snow_melt(i) = lf * snow_melt(i)
  END DO
!$OMP END DO
END IF

!-----------------------------------------------------------------------------
! Calculate gridbox surface heat flux over land.
!-----------------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
DO i = 1,land_pts
  surf_ht_flux_ld(i) = 0.0
END DO
!$OMP END DO

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO j = 1,surft_pts(n)
    i = surft_index(j,n)
    IF ((l_flake_model) .AND. (n == lake)) THEN
      ! The snow-to-soil heat flux for the lake tile gets passed
      ! into the lake via dhf_surf_minus_soil rather than the soil
      dhf_surf_minus_soil(i) = surf_htf_surft(i,n) - snow_soil_htf(i,n)
    ELSE
      surf_ht_flux_ld(i) = surf_ht_flux_ld(i) +                                &
                           tile_frac(i,n) * snow_soil_htf(i,n)
    END IF
  END DO
!$OMP END DO
END DO

!$OMP END PARALLEL

IF (l_flake_model) THEN
  DO i = 1,land_pts
    IF (non_lake_frac(i) > EPSILON(0.0)) THEN
      surf_ht_flux_ld(i) = surf_ht_flux_ld(i) / non_lake_frac(i)
    END IF
  END DO
END IF

!-----------------------------------------------------------------------------
! Calculate surface mass balance diagnostics
! Large packs can have a high mass (kg) with small rates of change (/s)
! - beware floating-point precision rounding errors
!-----------------------------------------------------------------------------

IF (sf_diag%l_snice) THEN

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(n,k,i)

  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      sf_diag%snice_smb_surft(i,n) = (snow_surft(i,n) - snow_surft_old(i,n))   &
                                     / timestep
    END DO
!$OMP END DO
  END DO

  DO n = 1,nsurft
    IF (cansnowtile(n)) THEN
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        i = surft_index(k,n)
        sf_diag%snice_smb_surft(i,n) = sf_diag%snice_smb_surft(i,n)            &
                                   + (snow_grnd(i,n) - snow_grnd_old(i,n))     &
                                   / timestep
      END DO
!$OMP END DO
    END IF
  END DO

  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      i = surft_index(k,n)
      IF ( snow_surft(i,n)     > EPSILON(snow_surft) .OR.                      &
           snow_surft_old(i,n) > EPSILON(snow_surft) ) THEN
        IF (nsnow(i,n) > 0) THEN
          sf_diag%snice_sicerate_surft(i,n) = ( SUM(sice(i,n,1:nsnow(i,n))) -  &
                                              sice_old(i,n) ) / timestep
          sf_diag%snice_sliqrate_surft(i,n) = ( SUM(sliq(i,n,1:nsnow(i,n))) -  &
                                              sliq_old(i,n) ) / timestep
          sf_diag%snice_runoff_surft(i,n) = con_rain(i) + ls_rain(i)           &
                                            + sf_diag%snice_m_surft(i,n)       &
                                            - sf_diag%snice_freez_surft(i,n)   &
                                            - sf_diag%snice_sliqrate_surft(i,n)
        ELSE
          sf_diag%snice_runoff_surft(i,n)   = con_rain(i) + ls_rain(i)         &
                                              + melt_surft(i,n)
          sf_diag%snice_sicerate_surft(i,n) = sf_diag%snice_smb_surft(i,n)
          sf_diag%snice_sliqrate_surft(i,n) = (0.0 - sliq_old(i,n)) / timestep

        END IF
      END IF
    END DO
!$OMP END DO
  END DO
!$OMP END PARALLEL

  DEALLOCATE(sliq_old)
  DEALLOCATE(sice_old)
  DEALLOCATE(nsnow_old)
  DEALLOCATE(snow_grnd_old)
  DEALLOCATE(snow_surft_old)

END IF

!-----------------------------------------------------------------------------
! Remove the rainfall redirected into precipitation.
!-----------------------------------------------------------------------------
IF (l_snow_infilt) THEN
!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(i,i_wt)                                                          &
!$OMP SHARED(land_pts,con_rain,infil_ground_con_gbm,ls_rain,                   &
!$OMP infil_ground_ls_gbm,l_wtrac_jls,n_wtrac_jls,con_rain_wtrac,              &
!$OMP ls_rain_wtrac,wtrac_sn)
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,land_pts
    con_rain(i) = infil_ground_con_gbm(i)
    ls_rain(i) = infil_ground_ls_gbm(i)
  END DO
!$OMP END DO

  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
      DO i = 1,land_pts
        con_rain_wtrac(i,i_wt) = wtrac_sn%infil_ground_con_gbm(i,i_wt)
        ls_rain_wtrac(i,i_wt)  = wtrac_sn%infil_ground_ls_gbm(i,i_wt)
      END DO
!$OMP END DO
    END DO
  END IF

!$OMP END PARALLEL
END IF

! Deallocate water tracer working arrays
CALL wtrac_dealloc_snow(wtrac_sn)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE snow
END MODULE snow_mod
