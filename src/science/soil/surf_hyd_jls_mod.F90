! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  SUBROUTINE SURF_HYD-----------------------------------------------

!  PURPOSE : TO CARRY OUT CANOPY AND SURFACE HYDROLOGY CALCULATIONS

!            CANOPY WATER CONTENT IS DEPRECIATED BY EVAPORATION

!            SNOWMELT IS RUNOFF THE SURFACE WITHOUT INTERACTING
!            WITH THE CANOPY

!            THE CANOPY INTERCEPTION AND SURFACE RUNOFF OF
!            LARGE-SCALE RAIN IS CALCULATED

!            THE CANOPY INTERCEPTION AND SURFACE RUNOFF OF
!            CONVECTIVE RAIN IS CALCULATED


!  SUITABLE FOR SINGLE COLUMN MODEL USE

!  DOCUMENTATION : UNIFIED MODEL DOCUMENTATION PAPER NO 25

!-----------------------------------------------------------------------------

MODULE surf_hyd_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SURF_HYD_MOD'

CONTAINS

SUBROUTINE surf_hyd (                                                          &
            land_pts, nsurft, sm_levels, soil_pts, n_wtrac_jls, timestep,      &
            l_top, l_pdm,                                                      &
            surft_pts, surft_index, soil_index,                                &
            catch_surft, ecan_surft, tile_frac, non_lake_frac, infil_surft,    &
            con_rain_land, ls_rain_land, con_rainfrac_land, ls_rainfrac_land,  &
            melt_surft, slope_gb, snow_melt, fsat_soilt, smvcst_soilt,         &
            sthf_soilt, sthu_soilt, ecan_surft_wtrac, melt_surft_wtrac,        &
            snow_melt_wtrac, con_rain_land_wtrac, ls_rain_land_wtrac,          &
            canopy_surft, canopy_surft_wtrac, canopy_gb, dsmc_dt_soilt,        &
            surf_roff_gb, tot_tfall_gb, tot_tfall_surft,                       &
            dun_roff_soilt, surf_roff_soilt, canopy_gb_wtrac,                  &
            dsmc_dt_soilt_wtrac, surf_roff_gb_wtrac, surf_roff_soilt_wtrac)

! Use in relevant subroutines
USE frunoff_mod,  ONLY: frunoff
USE sieve_mod,   ONLY: sieve
USE pdm_mod,     ONLY: pdm

! Use in relevant variables
USE jules_surface_mod, ONLY: l_point_data, l_flake_model
! Switch for using point rainfall data, switch for using FLake model.

USE jules_surface_types_mod, ONLY: lake

USE jules_water_tracers_mod, ONLY: l_wtrac_jls

USE ancil_info,        ONLY: nsoilt

USE parkind1, ONLY: jprb, jpim
USE yomhook,  ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Total number of land points.
  nsurft,                                                                      &
    ! Number of tiles.
  sm_levels,                                                                   &
    ! Number of soil moisture levels.
  soil_pts,                                                                    &
    ! Number of soil points.
  n_wtrac_jls

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep
    ! Timestep (s).

LOGICAL, INTENT(IN) ::                                                         &
  l_top,                                                                       &
    ! TOPMODEL-based hydrology logical.
  l_pdm
    ! PDM logical.

!-----------------------------------------------------------------------------
! Array arguments with INTENT(IN):
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  surft_pts(nsurft),                                                           &
    ! Number of tile points.
  surft_index(land_pts,nsurft),                                                &
    ! Index of tile points.
  soil_index(land_pts)
    ! Array of soil points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  catch_surft(land_pts,nsurft),                                                &
    ! Canopy capacity for land tiles (kg/m2).
  ecan_surft(land_pts,nsurft),                                                 &
    ! Canopy evaporation (kg/m2/s).
  tile_frac(land_pts,nsurft),                                                  &
    ! Tile fractions.
  non_lake_frac(land_pts),                                                     &
    ! Sum of non-FLake tile fractions used for weighted average of surf_roff_gb.
  infil_surft(land_pts,nsurft),                                                &
    ! Tile infiltration rate (kg/m2/s).
  con_rain_land(land_pts),                                                     &
    ! Convective rain (kg/m2/s).
  ls_rain_land(land_pts),                                                      &
    ! Large-scale rain (kg/m2/s).
  con_rainfrac_land(land_pts),                                                 &
    ! Convective rain fraction
  ls_rainfrac_land(land_pts),                                                  &
    ! large scale rain fraction
  melt_surft(land_pts,nsurft),                                                 &
    ! Snow melt on tiles (kg/m2/s).
  slope_gb(land_pts),                                                          &
    ! Terrain slope.
  snow_melt(land_pts),                                                         &
    ! GBM snow melt (kg/m2/s).
  fsat_soilt(land_pts,nsoilt),                                                 &
    ! Surface saturation fraction.
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric soil moisture concentration at saturation (m3 H2O/m3 soil).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.
  ecan_surft_wtrac(land_pts,nsurft,n_wtrac_jls),                               &
    ! Water tracer canopy evaporation (kg/m2/s).
  melt_surft_wtrac(land_pts,nsurft,n_wtrac_jls),                               &
    ! Water tracer snow melt on tiles (kg/m2/s).
  snow_melt_wtrac(land_pts,n_wtrac_jls),                                       &
    ! Water tracer GBM snow melt (kg/m2/s).
  con_rain_land_wtrac(land_pts,n_wtrac_jls),                                   &
    ! Water tracer convective rain (kg/m2/s).
  ls_rain_land_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer large-scale rain (kg/m2/s).

!-----------------------------------------------------------------------------
! Arguments with INTENT(IN OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  canopy_surft(land_pts,nsurft),                                               &
    ! Tile canopy water contents (kg/m2).
  canopy_surft_wtrac(land_pts,nsurft,n_wtrac_jls)
    ! Tile canopy water tracer contents (kg/m2).

!-----------------------------------------------------------------------------
! Arguments with INTENT(OUT):
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  canopy_gb(land_pts),                                                         &
    ! Gridbox canopy water content (kg/m2).
  dsmc_dt_soilt(land_pts,nsoilt),                                              &
    ! Rate of change of soil moisture content (kg/m2/s).
  surf_roff_gb(land_pts),                                                      &
    ! Cumulative surface runoff (kg/m2/s).
  tot_tfall_gb(land_pts),                                                      &
    ! Cumulative canopy throughfall (kg/m2/s).
  tot_tfall_surft(land_pts,nsurft),                                            &
    ! Surface-tiled contributions to throughfall.
  dun_roff_soilt(land_pts,nsoilt),                                             &
    ! Cumulative Dunne sfc runoff (kg/m2/s).
  surf_roff_soilt(land_pts,nsoilt),                                            &
    ! Soil-tiled contributions to surface runoff (kg/m2/s).
  canopy_gb_wtrac(land_pts,n_wtrac_jls),                                       &
    ! Gridbox canopy water tracer content (kg/m2).
  dsmc_dt_soilt_wtrac(land_pts,nsoilt,n_wtrac_jls),                            &
    ! Rate of change of water tracer soil moisture content (kg/m2/s).
  surf_roff_gb_wtrac(land_pts,n_wtrac_jls),                                    &
    ! Cumulative water tracer surface runoff (kg/m2/s).
  surf_roff_soilt_wtrac(land_pts,nsoilt,n_wtrac_jls)
    ! Soil-tiled contributions to water tracer surface runoff (kg/m2/s).

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  i,j,n,m,i_wt
    ! Loop counters:
    ! i for land point
    ! j for tile point
    ! n for surface tile
    ! m for soil tile
    ! i_wt for water tracer

REAL(KIND=real_jlslsm) ::                                                      &
  r,                                                                           &
    ! Total downward water flux (i.e. rain + condensation + snowmelt)
    ! (kg/m2/s).
  tfall,                                                                       &
    ! Cumulative canopy throughfall on a tile (kg/m2/s).
  s_roff,                                                                      &
    ! Cumulative surface runoff on a tile (kg/m2/s).
  p_in_wtrac
    ! Water tracer flux reaching the soil surface (kg/m2/s).

!-----------------------------------------------------------------------------
! Local array variables:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  can_cond(land_pts),                                                          &
    ! Canopy condensation (kg/m2/s).
  frac_cov(land_pts),                                                          &
    ! Fraction of gridbox with snowmelt or evap.
  surf_roff_surft(land_pts,nsurft),                                            &
    ! Surface-tiled contributions to surface runoff.
  tot_tfall_soilt(land_pts,nsoilt)

! Allocatable water tracer arrays:
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  surf_roff_surft_wtrac(:,:,:),                                                &
    ! Surface-tiled contributions to water tracer surface runoff.
  can_cond_wtrac(:,:),                                                         &
    ! Water tracer canopy condensation (kg/m2/s).
  tot_tfall_gb_wtrac(:,:),                                                     &
    ! Cumulative water tracer canopy throughfall (kg/m2/s).
    ! (Currently not output from this routine as just diagnostic)
  tot_tfall_surft_wtrac(:,:,:),                                                &
    ! Surface-tiled contributions to water tracer throughfall.
    ! (Currently not output from this routine as just diagnostic)
  dun_roff_soilt_wtrac(:,:,:),                                                 &
    ! Cumulative water tracer Dunne sfc runoff (kg/m2/s).
    ! (Currently not output from this routine as just diagnostic)
  tot_tfall_soilt_wtrac(:,:,:),                                                &
  melt_surft_wtrac_tile(:,:),                                                  &
    ! Temporary copy of the melt_surft_wtrac field for a particular tile.
  p_in_store(:,:)
    ! Water flux reaching the soil surface which is stored for water tracer
    ! calculations if l_pdm=T or l_top=T

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SURF_HYD'

!-----------------------------------------------------------------------------
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate water tracer fields
IF (l_wtrac_jls) THEN
  ALLOCATE(surf_roff_surft_wtrac(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(can_cond_wtrac(land_pts,n_wtrac_jls))
  ALLOCATE(tot_tfall_gb_wtrac(land_pts,n_wtrac_jls))
  ALLOCATE(tot_tfall_surft_wtrac(land_pts,nsurft,n_wtrac_jls))
  ALLOCATE(dun_roff_soilt_wtrac(land_pts,nsoilt,n_wtrac_jls))
  ALLOCATE(tot_tfall_soilt_wtrac(land_pts,nsoilt,n_wtrac_jls))
ELSE
  ALLOCATE(surf_roff_surft_wtrac(1,1,1))
  ALLOCATE(can_cond_wtrac(1,1))
  ALLOCATE(tot_tfall_gb_wtrac(1,1))
  ALLOCATE(tot_tfall_surft_wtrac(1,1,1))
  ALLOCATE(dun_roff_soilt_wtrac(1,1,1))
  ALLOCATE(tot_tfall_soilt_wtrac(1,1,1))
END IF

! Assume snowmelt and evaporation cover 100% of the gridbox
! Zero cumulative stores
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, i_wt)                               &
!$OMP SHARED(land_pts, nsoilt, nsurft, frac_cov, tot_tfall_gb, surf_roff_gb,   &
!$OMP dsmc_dt_soilt, dun_roff_soilt, surf_roff_surft, tot_tfall_surft,         &
!$OMP l_wtrac_jls, surf_roff_gb_wtrac, surf_roff_surft_wtrac, n_wtrac_jls,     &
!$OMP tot_tfall_surft_wtrac, tot_tfall_gb_wtrac)
!$OMP DO SCHEDULE(STATIC)
DO i = 1, land_pts
  frac_cov(i)         = 1.0
  tot_tfall_gb(i)     = 0.0
  surf_roff_gb(i)     = 0.0
END DO
!$OMP END DO
DO j = 1, nsoilt
!$OMP DO SCHEDULE(STATIC)
  DO i = 1, land_pts
    dsmc_dt_soilt(i,j)  = 0.0
    dun_roff_soilt(i,j) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO
DO j = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
  DO i = 1, land_pts
    surf_roff_surft(i,j) = 0.0
    tot_tfall_surft(i,j) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO
! Initialiase water tracer equivalent fields
IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls

!$OMP DO SCHEDULE(STATIC)
    DO i = 1, land_pts
      surf_roff_gb_wtrac(i,i_wt) = 0.0
      tot_tfall_gb_wtrac(i,i_wt) = 0.0
    END DO
!$OMP END DO

    DO j = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, land_pts
        surf_roff_surft_wtrac(i,j,i_wt) = 0.0
        tot_tfall_surft_wtrac(i,j,i_wt) = 0.0
      END DO
!$OMP END DO NOWAIT
    END DO

  END DO ! i_wt
END IF ! l_wtrac_jls
!$OMP END PARALLEL

! Reduce canopy water content by evaporation
DO n = 1,nsurft
  DO j = 1,surft_pts(n)
    i = surft_index(j,n)
    IF (ecan_surft(i,n) > 0.0) THEN
      canopy_surft(i,n) =  MAX(canopy_surft(i,n) - ecan_surft(i,n) * timestep, &
                               0.0 )
    END IF
  END DO
END DO

IF (l_wtrac_jls) THEN
  ! Repeat for water tracers
  DO i_wt = 1, n_wtrac_jls
    DO n = 1,nsurft
      DO j = 1,surft_pts(n)
        i = surft_index(j,n)
        IF (ecan_surft_wtrac(i,n,i_wt) > 0.0) THEN
          canopy_surft_wtrac(i,n,i_wt) = MAX(canopy_surft_wtrac(i,n,i_wt)      &
                                      - ecan_surft_wtrac(i,n,i_wt) * timestep, &
                                       0.0 )
        END IF
      END DO
    END DO
  END DO
END IF

IF (l_point_data) THEN
  !-----------------------------------------------------------------------
  ! Using point precipitation data. (Note, water tracers not coded for this
  ! option.)
  !-----------------------------------------------------------------------
  DO n = 1,nsurft
    DO j = 1,surft_pts(n)
      i = surft_index(j,n)
      ! Calculate total downward flux of liquid water.
      r = ls_rain_land(i) + con_rain_land(i)
      ! Add any condensation
      IF ( ecan_surft(i,n) < 0.0 ) r = r - ecan_surft(i,n)
      ! Calculate throughfall.
      IF ( r <= catch_surft(i,n) / timestep .AND.                              &
           catch_surft(i,n) > 0 ) THEN
        tfall = r * canopy_surft(i,n) / catch_surft(i,n)
      ELSE
        tfall = r - ( catch_surft(i,n) - canopy_surft(i,n) ) / timestep
      END IF
      ! Update canopy water content.
      canopy_surft(i,n) = canopy_surft(i,n) + ( r - tfall ) * timestep
      ! Add melt to throughfall before calculation of runoff.
      tfall = tfall + melt_surft(i,n)
      ! Calculate surface runoff.
      IF ( tfall > infil_surft(i,n) ) THEN
        s_roff = tfall - infil_surft(i,n)
      ELSE
        s_roff = 0.0
      END IF

      IF ( .NOT. ((l_flake_model) .AND. (n == lake)) ) THEN
        ! Add to gridbox accumulations, exclude accumulations on lakes when
        ! using FLake.
        ! Don't include melt in throughfall.
        tot_tfall_gb(i) = tot_tfall_gb(i)                                      &
                          + ( tfall - melt_surft(i,n) ) * tile_frac(i,n)
        surf_roff_gb(i) = surf_roff_gb(i) + s_roff * tile_frac(i,n)
      END IF

      ! Tiled runoff and throughfall.
      tot_tfall_surft(i,n) = tfall - melt_surft(i,n)
      surf_roff_surft(i,n) = s_roff

    END DO
  END DO

ELSE

  !-----------------------------------------------------------------------
  ! Using area-average precipitation data. Assume spatial distribution.
  !
  ! Note, water tracers are only coded for this option.
  !-----------------------------------------------------------------------

  DO n = 1,nsurft

    ! Create a temporary array to hold melt_surft_wtrac for the current tile
    IF (l_wtrac_jls) THEN
      ALLOCATE(melt_surft_wtrac_tile(land_pts,n_wtrac_jls))

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i,i_wt)               &
!$OMP SHARED(n, land_pts, n_wtrac_jls, melt_surft_wtrac_tile, melt_surft_wtrac)
      DO i = 1, land_pts
        DO i_wt = 1, n_wtrac_jls
          melt_surft_wtrac_tile(i,i_wt) = melt_surft_wtrac(i,n,i_wt)
        END DO
      END DO
!$OMP END PARALLEL DO

    ELSE
      ALLOCATE(melt_surft_wtrac_tile(1,1))
    END IF

    ! Surface runoff of snowmelt, assumed to cover 100% of tile
    CALL frunoff (land_pts, surft_pts(n), n, nsurft, n_wtrac_jls, timestep,    &
                  surft_index(:,n),                                            &
                  frac_cov, catch_surft(:,n), catch_surft(:,n),                &
                  infil_surft(:,n), melt_surft(:,n),                           &
                  melt_surft_wtrac_tile, tile_frac(:,n),                       &
                  surf_roff_gb, surf_roff_surft(:,n),surf_roff_gb_wtrac,       &
                  surf_roff_surft_wtrac)

    DEALLOCATE(melt_surft_wtrac_tile)

    ! Define canopy condensation when evaporation is negative
    DO j = 1,surft_pts(n)
      i = surft_index(j,n)
      IF ( ecan_surft(i,n)  <  0.0 ) THEN
        can_cond(i) = - ecan_surft(i,n)
      ELSE
        can_cond(i) = 0.0
      END IF
    END DO


    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1, n_wtrac_jls
        DO j = 1,surft_pts(n)
          i = surft_index(j,n)
          IF ( ecan_surft_wtrac(i,n,i_wt)  <  0.0 ) THEN
            can_cond_wtrac(i,i_wt) = - ecan_surft_wtrac(i,n,i_wt)
          ELSE
            can_cond_wtrac(i,i_wt) = 0.0
          END IF
        END DO
      END DO
    END IF

    ! Canopy interception, throughfall and surface runoff for condensation,
    ! assumed to cover 100% of gridbox
    CALL sieve (land_pts, surft_pts(n), n, nsurft, n_wtrac_jls, timestep,      &
                surft_index(:,n), frac_cov, catch_surft(:,n), can_cond,        &
                can_cond_wtrac, tile_frac(:,n),                                &
                canopy_surft(:,n), tot_tfall_gb, tot_tfall_surft(:,n),         &
                canopy_surft_wtrac, tot_tfall_gb_wtrac,                        &
                tot_tfall_surft_wtrac)

    CALL frunoff (land_pts, surft_pts(n), n, nsurft, n_wtrac_jls, timestep,    &
                  surft_index(:,n),                                            &
                  frac_cov, catch_surft(:,n), canopy_surft(:,n),               &
                  infil_surft(:,n), can_cond, can_cond_wtrac, tile_frac(:,n),  &
                  surf_roff_gb, surf_roff_surft(:,n),surf_roff_gb_wtrac,       &
                  surf_roff_surft_wtrac)

    ! Canopy interception, throughfall and surface runoff for large-scale
    ! rain, assumed to cover "ls_rainfrac_land" of gridbox

    CALL sieve (land_pts, surft_pts(n), n, nsurft, n_wtrac_jls, timestep,      &
                surft_index(:,n), ls_rainfrac_land, catch_surft(:,n),          &
                ls_rain_land, ls_rain_land_wtrac, tile_frac(:,n),              &
                canopy_surft(:,n), tot_tfall_gb, tot_tfall_surft(:,n),         &
                canopy_surft_wtrac, tot_tfall_gb_wtrac,                        &
                tot_tfall_surft_wtrac)

    CALL frunoff (land_pts, surft_pts(n), n, nsurft, n_wtrac_jls, timestep,    &
                  surft_index(:,n),                                            &
                  ls_rainfrac_land, catch_surft(:,n), canopy_surft(:,n),       &
                  infil_surft(:,n), ls_rain_land, ls_rain_land_wtrac,          &
                  tile_frac(:,n),                                              &
                  surf_roff_gb, surf_roff_surft(:,n),surf_roff_gb_wtrac,       &
                  surf_roff_surft_wtrac)

    ! Canopy interception, throughfall and surface runoff for convective
    ! rain, assumed to cover fraction "con_rainfrac_land" of gridbox

    CALL sieve (land_pts, surft_pts(n), n, nsurft, n_wtrac_jls, timestep,      &
                surft_index(:,n), con_rainfrac_land, catch_surft(:,n),         &
                con_rain_land, con_rain_land_wtrac, tile_frac(:,n),            &
                canopy_surft(:,n), tot_tfall_gb, tot_tfall_surft(:,n),         &
                canopy_surft_wtrac, tot_tfall_gb_wtrac,                        &
                tot_tfall_surft_wtrac)

    CALL frunoff (land_pts, surft_pts(n), n, nsurft, n_wtrac_jls, timestep,    &
                  surft_index(:,n),                                            &
                  con_rainfrac_land, catch_surft(:,n), canopy_surft(:,n),      &
                  infil_surft(:,n), con_rain_land,  con_rain_land_wtrac,       &
                  tile_frac(:,n),                                              &
                  surf_roff_gb, surf_roff_surft(:,n),surf_roff_gb_wtrac,       &
                  surf_roff_surft_wtrac)

  END DO
END IF   !   l_point_data

IF (l_flake_model) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) IF(land_pts>1) DEFAULT(NONE) PRIVATE(i)     &
!$OMP SHARED(land_pts, non_lake_frac, tot_tfall_gb, surf_roff_gb)
  DO i = 1,land_pts
    IF (non_lake_frac(i) > EPSILON(0.0)) THEN
      tot_tfall_gb(i) = tot_tfall_gb(i) / non_lake_frac(i)
      surf_roff_gb(i) = surf_roff_gb(i) / non_lake_frac(i)
    END IF
  END DO
!$OMP END PARALLEL DO


  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
!$OMP PARALLEL DO SCHEDULE(STATIC) IF(land_pts>1) DEFAULT(NONE) PRIVATE(i,i_wt)&
!$OMP SHARED(land_pts, non_lake_frac, tot_tfall_gb_wtrac, surf_roff_gb_wtrac,  &
!$OMP         n_wtrac_jls)
    DO i = 1,land_pts
      IF (non_lake_frac(i) > EPSILON(0.0)) THEN
        DO i_wt = 1, n_wtrac_jls
          tot_tfall_gb_wtrac(i,i_wt) = tot_tfall_gb_wtrac(i,i_wt)              &
                                         / non_lake_frac(i)
          surf_roff_gb_wtrac(i,i_wt) = surf_roff_gb_wtrac(i,i_wt)              &
                                         / non_lake_frac(i)
        END DO
      END IF
    END DO
!$OMP END PARALLEL DO
  END IF  ! l_wtrac_jls

END IF

!=============================================================================
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
! This means that a soilt variable being passed 'up' to the surface is simply
! copied into the surft variable
!
! This will need to be refactored for other tiling approaches. This note
! will be replicated elsewhere in the code as required
!
!These comments apply until **END NOTICE REGARDING SOIL TILING**
!=============================================================================

IF ( nsoilt == 1) THEN
  ! There is only one soil tile
  m = 1

  ! Store water flux reaching the soil surface for later use by water tracers
  IF (l_wtrac_jls .AND. (l_pdm .OR. l_top)) THEN
    ALLOCATE(p_in_store(land_pts,nsoilt))
    DO i = 1,land_pts
      p_in_store(i,m) = tot_tfall_gb(i) + snow_melt(i) - surf_roff_gb(i)
    END DO
  END IF

  ! To maintain bit-comparability, we need to run pdm with the _gb versions.
  !---------------------------------------------------------------------------
  ! Calculate Saturation excess runoff through PDM:
  !---------------------------------------------------------------------------
  IF (soil_pts > 0 .AND. l_pdm) THEN
    CALL pdm(                                                                  &
            land_pts, sm_levels, soil_pts, timestep, soil_index,               &
            slope_gb, tot_tfall_gb, snow_melt,                                 &
            surf_roff_gb,                                                      &
            smvcst_soilt(:,m,:), sthu_soilt(:,m,:), sthf_soilt(:,m,:),         &
            dun_roff_soilt(:,m))
  END IF

  IF (l_top) THEN
    DO i = 1,land_pts
      dun_roff_soilt(i,m) = fsat_soilt(i,m) * (tot_tfall_gb(i)                 &
                            + snow_melt(i) - surf_roff_gb(i))
    END DO
  END IF

  DO i = 1,land_pts
    IF (l_top .OR. l_pdm) THEN
      surf_roff_gb(i) = surf_roff_gb(i) + dun_roff_soilt(i,m)
    END IF
    dsmc_dt_soilt(i,m) = tot_tfall_gb(i) + snow_melt(i) - surf_roff_gb(i)
    ! Copy the final answers across to the _soilt version so they can be used
    ! elsewhere.
    surf_roff_soilt(i,m) = surf_roff_gb(i)
  END DO

  ! Repeat for water tracers
  IF (l_wtrac_jls) THEN
    IF (l_top .OR. l_pdm) THEN
      DO i_wt = 1, n_wtrac_jls
        DO i = 1, land_pts
          p_in_wtrac = tot_tfall_gb_wtrac(i,i_wt)                              &
                        + snow_melt_wtrac(i,i_wt) - surf_roff_gb_wtrac(i,i_wt)

          ! Calculate saturation excess runoff for water tracers
          IF (ABS(p_in_store(i,m)) > TINY(p_in_store)) THEN
            dun_roff_soilt_wtrac(i,m,i_wt) = p_in_wtrac                        &
                                     * (dun_roff_soilt(i,m)/p_in_store(i,m))
          ELSE
            dun_roff_soilt_wtrac(i,m,i_wt) = 0.0
          END IF
        END DO
      END DO
      DEALLOCATE(p_in_store)
    END IF

    DO i_wt = 1, n_wtrac_jls
      DO i = 1,land_pts
        IF (l_top .OR. l_pdm) THEN
          surf_roff_gb_wtrac(i,i_wt) = surf_roff_gb_wtrac(i,i_wt)              &
                                      + dun_roff_soilt_wtrac(i,m,i_wt)
        END IF
        dsmc_dt_soilt_wtrac(i,m,i_wt) = tot_tfall_gb_wtrac(i,i_wt)             &
                                       + snow_melt_wtrac(i,i_wt)               &
                                       - surf_roff_gb_wtrac(i,i_wt)
        ! Copy the final answers across to the _soilt version so they can be
        ! used elsewhere.
        surf_roff_soilt_wtrac(i,m,i_wt) = surf_roff_gb_wtrac(i,i_wt)
      END DO
    END DO
  END IF  ! l_wtrac_jls

ELSE
  ! nsoilt = nsurft
  DO j = 1,nsurft
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i)                    &
!$OMP SHARED(j, land_pts,surf_roff_soilt,tot_tfall_soilt,surf_roff_surft,      &
!$OMP        tot_tfall_surft)
    DO i = 1,land_pts
      surf_roff_soilt(i,j)   = surf_roff_surft(i,j)
      tot_tfall_soilt(i,j)   = tot_tfall_surft(i,j)
    END DO
!$OMP END PARALLEL DO
  END DO

  IF (l_wtrac_jls) THEN
    ! Repeat for water tracers
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,i_wt)                                 &
!$OMP SHARED(n_wtrac_jls,nsurft,land_pts,surf_roff_soilt_wtrac,                &
!$OMP        tot_tfall_soilt_wtrac,surf_roff_surft_wtrac,tot_tfall_surft_wtrac)
    DO i_wt = 1, n_wtrac_jls
      DO j = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
        DO i = 1,land_pts
          surf_roff_soilt_wtrac(i,j,i_wt) = surf_roff_surft_wtrac(i,j,i_wt)
          tot_tfall_soilt_wtrac(i,j,i_wt) = tot_tfall_surft_wtrac(i,j,i_wt)
        END DO
!$OMP END DO
      END DO
    END DO
!$OMP END PARALLEL
  END IF ! l_wtrac_jls

  ! Store water flux reaching the soil surface for later use by water tracers
  IF (l_wtrac_jls .AND. (l_pdm .OR. l_top)) THEN
    ALLOCATE(p_in_store(land_pts,nsoilt))
    DO m = 1, nsoilt
      n = m
      DO i = 1,land_pts
        p_in_store(i,m) = tot_tfall_soilt(i,m) + melt_surft(i,n)               &
                            - surf_roff_soilt(i,m)
      END DO
    END DO
  END IF

  !-----------------------------------------------------------------------
  ! Calculate saturation excess runoff through PDM:
  !-----------------------------------------------------------------------
  IF (soil_pts > 0 .AND. l_pdm) THEN
    DO m = 1, nsoilt
      n = m
      !Note we use the surface-tiled melt, rather than the GBM
      CALL pdm(                                                                &
            land_pts, sm_levels, soil_pts, timestep, soil_index,               &
            slope_gb, tot_tfall_soilt(:,m), melt_surft(:,n),                   &
            surf_roff_soilt(:,m),                                              &
            smvcst_soilt(:,m,:), sthu_soilt(:,m,:), sthf_soilt(:,m,:),         &
            dun_roff_soilt(:,m))
    END DO
  END IF

  IF (l_top) THEN
    DO m = 1, nsoilt
      n = m
      DO i = 1,land_pts
        dun_roff_soilt(i,m) = fsat_soilt(i,m) * (tot_tfall_soilt(i,m)          &
                              + melt_surft(i,n) - surf_roff_soilt(i,m))
      END DO
    END DO
  END IF

  DO m = 1, nsoilt
    n = m
    DO i = 1,land_pts
      IF (l_top .OR. l_pdm) THEN
        surf_roff_soilt(i,m) = surf_roff_soilt(i,m) + dun_roff_soilt(i,m)
        surf_roff_gb(i) = surf_roff_gb(i)                                      &
                          + tile_frac(i,n) * dun_roff_soilt(i,m)
      END IF
      dsmc_dt_soilt(i,m) = tot_tfall_soilt(i,m) + melt_surft(i,n)              &
                           - surf_roff_soilt(i,m)
    END DO
  END DO

  ! Repeat for water tracers
  IF (l_wtrac_jls) THEN
    IF (l_top .OR. l_pdm) THEN
      DO i_wt = 1, n_wtrac_jls
        DO m = 1, nsoilt
          n = m
          DO i = 1,land_pts
            ! Calculate water tracer flux reaching soil surface
            p_in_wtrac = tot_tfall_soilt_wtrac(i,m,i_wt)                       &
                          + melt_surft_wtrac(i,n,i_wt)                         &
                          - surf_roff_soilt_wtrac(i,m,i_wt)

            ! Calculate saturation excess runoff for water tracers
            IF (ABS(p_in_store(i,m)) > TINY(p_in_store)) THEN
              dun_roff_soilt_wtrac(i,m,i_wt) =  p_in_wtrac                     &
                                    * (dun_roff_soilt(i,m) / p_in_store(i,m))
            ELSE
              dun_roff_soilt_wtrac(i,m,i_wt) = 0.0
            END IF
          END DO
        END DO
      END DO
      DEALLOCATE(p_in_store)
    END IF

    DO i_wt = 1, n_wtrac_jls
      DO m = 1, nsoilt
        n = m
        DO i = 1,land_pts
          IF (l_top .OR. l_pdm) THEN
            surf_roff_soilt_wtrac(i,m,i_wt) = surf_roff_soilt_wtrac(i,m,i_wt)  &
                                            + dun_roff_soilt_wtrac(i,m,i_wt)
            surf_roff_gb_wtrac(i,i_wt) = surf_roff_gb_wtrac(i,i_wt)            &
                            + tile_frac(i,n) * dun_roff_soilt_wtrac(i,m,i_wt)
          END IF
          dsmc_dt_soilt_wtrac(i,m,i_wt) = tot_tfall_soilt_wtrac(i,m,i_wt)      &
                                         + melt_surft_wtrac(i,n,i_wt)          &
                                         - surf_roff_soilt_wtrac(i,m,i_wt)
        END DO
      END DO
    END DO
  END IF  ! l_wtrac_jls

END IF  !  nsoilt

!=============================================================================
! *END NOTICE REGARDING SOIL TILING**
!=============================================================================

!Calculate GBM canopy water content
DO i = 1, land_pts
  canopy_gb(i) = 0.0
END DO
DO n = 1, nsurft
  DO j = 1,surft_pts(n)
    i = surft_index(j,n)
    canopy_gb(i) = canopy_gb(i) + (tile_frac(i,n) * canopy_surft(i,n))
  END DO
END DO

! Repeat for water tracers
IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
    DO i = 1, land_pts
      canopy_gb_wtrac(i,i_wt) = 0.0
    END DO

    DO n = 1, nsurft
      DO j = 1,surft_pts(n)
        i = surft_index(j,n)
        canopy_gb_wtrac(i,i_wt) = canopy_gb_wtrac(i,i_wt)                      &
                             + (tile_frac(i,n) * canopy_surft_wtrac(i,n,i_wt))
      END DO
    END DO
  END DO

END IF  ! l_wtrac_jls

! Deallocate water tracer fields
DEALLOCATE(tot_tfall_soilt_wtrac)
DEALLOCATE(dun_roff_soilt_wtrac)
DEALLOCATE(tot_tfall_surft_wtrac)
DEALLOCATE(tot_tfall_gb_wtrac)
DEALLOCATE(can_cond_wtrac)
DEALLOCATE(surf_roff_surft_wtrac)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_hyd
END MODULE surf_hyd_mod
