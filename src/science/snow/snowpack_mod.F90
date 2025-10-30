! *****************************COPYRIGHT*******************************

! (c) [University of Edinburgh]. All rights reserved.
! This routine has been licensed to the Met Office for use and
! distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
! [Met Office Ref SC237]

! *****************************COPYRIGHT*******************************
!  SUBROUTINE SNOWPACK-------------------------------------------------

! Description:
!     Snow thermodynamics and hydrology
!
! Note that _soilt suffix is for reference only in this subroutine so does not
! need the soilt dimension. The appropriate soil tile is passed down from snow.
!
! Subroutine Interface:
MODULE snowpack_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SNOWPACK_MOD'

CONTAINS

SUBROUTINE snowpack ( surft_n, land_pts, surft_pts, n_wtrac_jls, timestep,     &
                      cansnowtile, nsnow, surft_index, nsurft, csnow,          &
                      ei_surft, hcaps1_soilt, hcons, infiltration, ksnow,      &
                      rho_snow_grnd, smcl1_soilt, snowfall, sthf1_soilt,       &
                      surf_htf_surft, tile_frac, smvcst1_soilt,                &
                      ei_surft_wtrac, infiltration_wtrac, ds,                  &
                      melt_surft, snowinc_surft, sice, sliq, snomlt_sub_htf,   &
                      snowdepth, snowmass, tsnow,                              &
                      t_soil1_soilt, tsurf_elev_surft,                         &
                      melt_surft_wtrac, sice_wtrac, sliq_wtrac,                &
                      snowfall_wtrac, snowmass_wtrac,                          &
                      snow_soil_htf, rho_snow, rho0, sice0, tsnow0,            &
                      sice0_wtrac, sf_diag, non_lake_frac,                     &
                      !Ancil info (IN)
                      l_lice_point, l_lice_surft,                              &
                      ! Types Variables
                      lake_h_ice_gb, lake_h_mxl_gb, lake_depth_gb)

USE tridag_mod, ONLY: tridag

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  hcapi,                                                                       &
    ! Specific heat capacity of ice (J/kg/K).
  hcapw,                                                                       &
    ! Specific heat capacity of water (J/kg/K).
  lf,                                                                          &
    ! Latent heat of fusion at 0degC (J kg-1).
  rho_ice,                                                                     &
    ! Specific density of solid ice (kg/m3).
  rho_water,                                                                   &
    ! Density of pure water (kg/m3).
  tm
    ! Temperature at which fresh water freezes and ice melts (K).

USE jules_soil_mod, ONLY: dzsoil, dzsoil_elev, hcondeep

USE jules_surface_mod, ONLY: l_elev_land_ice, l_flake_model

USE jules_surface_types_mod, ONLY: lake, ntype

USE jules_snow_mod, ONLY:                                                      &
  nsmax,                                                                       &
    ! Maximum possible number of snow layers.
  l_snow_infilt,                                                               &
    ! Include infiltration of rain into snow.
  i_basal_melting_opt,                                                         &
    ! Selector of option for basal melting
  ip_basal_melting_diag_0,                                                     &
    ! Option to melt snow below the surface of the
    ! snow pack in the zero layer scheme
  rho_snow_fresh,                                                              &
    ! Density of fresh snow (kg per m**3).
  snowliqcap,                                                                  &
    ! Liquid water holding capacity of lying snow
    ! as a fraction of snow mass.
  snow_hcon,                                                                   &
    ! Conductivity of snow.
  rho_firn_pore_restrict,                                                      &
    ! Density at which ability of snowpack to hold/percolate
    ! water starts to be restricted.
  rho_firn_pore_closure
    ! Density at which snowpack pores close off entirely and
    ! no additional melt can be held/percolated through.

USE jules_water_tracers_mod, ONLY: l_wtrac_jls, wtrac_calc_ratio_fn_jules

USE sf_diags_mod, ONLY: strnewsfdiag

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
                 ! Total number of land points.
  nsurft,                                                                      &
                 ! Number of surface tiles.
  surft_n,                                                                     &
                 ! Tile number this loop.
  surft_pts,                                                                   &
                 ! Number of tile points.
  n_wtrac_jls
                 ! Number of water tracers.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep       ! Timestep (s).

LOGICAL, INTENT(IN) ::                                                         &
  cansnowtile    ! Switch for canopy snow model.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  nsnow(land_pts),                                                             &
    ! Number of snow layers.
  surft_index(land_pts)
    ! Index of tile points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  csnow(land_pts,nsmax),                                                       &
    ! Areal heat capacity of layers (J/K/m2).
  ei_surft(land_pts),                                                          &
    ! Sublimation of snow (kg/m2/s).
  snowinc_surft(land_pts),                                                     &
    ! Total snow increment over the time step (kg m-2 TS-1)
  hcaps1_soilt(land_pts),                                                      &
    ! Heat capacity of soil surface layer (J/K/m3).
  hcons(land_pts),                                                             &
    ! Thermal conductivity of top soil layer,
    ! including water and ice (W/m/K).
  ksnow(land_pts,nsmax),                                                       &
    ! Thermal conductivity of layers (W/m/K).
  rho_snow_grnd(land_pts),                                                     &
    ! Snowpack bulk density (kg/m3).
  smcl1_soilt(land_pts),                                                       &
    ! Moisture content of surface soil layer (kg/m2).
  infiltration(land_pts),                                                      &
    ! Rainfall infiltrating into snowpack (kg/m2).
  sthf1_soilt(land_pts),                                                       &
    ! Frozen soil moisture content of surface layer
    ! as a fraction of saturation.
  surf_htf_surft(land_pts),                                                    &
    ! Snow surface heat flux (W/m2).
  tile_frac(land_pts,nsurft),                                                  &
    ! Tile fractions
  non_lake_frac(land_pts),                                                     &
    ! Fraction of the gridbox not taken up by the lake tile.
  smvcst1_soilt(land_pts),                                                     &
    ! Surface soil layer volumetric
    ! moisture concentration at saturation.
  lake_h_ice_gb(land_pts),                                                     &
    ! lake ice thickness (m)
  lake_h_mxl_gb(land_pts),                                                     &
    ! lake mixed layer thickness (m)
  lake_depth_gb(land_pts),                                                     &
    ! lake depth (m)
  ei_surft_wtrac(land_pts,nsurft,n_wtrac_jls),                                 &
    ! Water tracer sublimation of snow for all tiles (kg/m2/s).
    ! (Note, unlike the water equivalent, this is the full field for all tiles.)
  infiltration_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer content in rainfall infiltrating into snowpack (kg/m2).

! Array arguments with intent(inout)
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  ds(land_pts,nsmax),                                                          &
    ! Snow layer depths (m).
  melt_surft(land_pts),                                                        &
    ! Surface snowmelt rate (kg/m2/s).
  sice(land_pts,nsmax),                                                        &
    ! Ice content of snow layers (kg/m2).
  sliq(land_pts,nsmax),                                                        &
    ! Liquid content of snow layers (kg/m2).
  snomlt_sub_htf(land_pts),                                                    &
    ! Sub-canopy snowmelt heat flux (W/m2).
  snowdepth(land_pts),                                                         &
    ! Snow depth (m).
  snowfall(land_pts),                                                          &
    ! Frozen precip reaching the ground (kg/m2).
  snowmass(land_pts),                                                          &
    ! Snow mass on the ground (kg/m2).
  tsnow(land_pts,nsmax),                                                       &
    ! Snow layer temperatures (K).
  t_soil1_soilt(land_pts),                                                     &
    ! Soil or lake surface layer temperature(K).
  tsurf_elev_surft(land_pts),                                                  &
    ! Temperature of elevated subsurface tiles (K).
  melt_surft_wtrac(land_pts,nsurft,n_wtrac_jls),                               &
    ! Water tracer surface snowmelt rate for all tiles (kg/m2/s).
    ! (Note, unlike the water equivalent, this is the full field for all tiles)
  sice_wtrac(land_pts,nsmax,n_wtrac_jls),                                      &
    ! Water tracer ice content of snow layers (kg/m2).
  sliq_wtrac(land_pts,nsmax,n_wtrac_jls),                                      &
    ! Water tracer liquid content of snow layers (kg/m2).
  snowfall_wtrac(land_pts,n_wtrac_jls),                                        &
    ! Water tracer content of frozen precip reaching the ground (kg/m2).
  snowmass_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer content in snow mass on the ground (kg/m2).

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  snow_soil_htf(land_pts),                                                     &
    ! Heat flux into the uppermost subsurface layer (W/m2)
    ! i.e. snow to ground, or into snow/soil composite layer.
  rho0(land_pts),                                                              &
    ! Density of fresh snow (kg/m3).
    ! Where nsnow=0, rho0 is the density of the snowpack.
  sice0(land_pts),                                                             &
    ! Ice content of fresh snow (kg/m2).
    ! Where nsnow=0, sice0 is the mass of the snowpack.
  tsnow0(land_pts),                                                            &
    ! Temperature of fresh snow (K).
  rho_snow(land_pts,nsmax),                                                    &
    ! Density of snow layers (kg/m3).
  sice0_wtrac(land_pts,n_wtrac_jls)
    ! Water tracer ice content of fresh snow (kg/m2).
    ! Where nsnow=0, sice0 is the mass of the snowpack.

!ancil_info (IN)
LOGICAL, INTENT(IN) :: l_lice_point(land_pts)
LOGICAL, INTENT(IN) :: l_lice_surft(ntype)

!-----------------------------------------------------------------------------
! Scalar parameters
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER :: r_gamma = 0.5
    ! Implicit timestep weighting.

!-----------------------------------------------------------------------------
! Local scalars
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  flake_tile,                                                                  &
    ! true when the FLake model is active on the current surface tile
  flake_not_lake
    ! true when FLake model is being used but this is not a lake tile

INTEGER ::                                                                     &
  i,                                                                           &
    ! land point index.
  k,                                                                           &
    ! Tile point index.
  n,                                                                           &
    ! Snow layer index.
  i_wt
    ! Water tracer index.

REAL(KIND=real_jlslsm) ::                                                      &
  asoil,                                                                       &
    ! 1 / (dz*hcap) for surface soil layer.
  can_melt,                                                                    &
    ! Melt of snow on the canopy (kg/m2/s).
  coldsnow,                                                                    &
    ! layer cold content (J/m2).
  dsice,                                                                       &
    ! Change in layer ice content (kg/m2).
  g_snow_surf,                                                                 &
    ! Heat flux just above the snow surface, ie.
    ! without any contribution from surface melting (W/m2)
  rho_temp,                                                                    &
    ! Temporary variable for layer density.
  sliqmax,                                                                     &
    ! Maximum liquid content for layer (kg/m2).
  submelt,                                                                     &
    ! Melt of snow beneath canopy (kg/m2/s).
  smclf,                                                                       &
    ! Frozen soil moisture content of surface layer (kg/m2).
  tile_frac_lake_adj,                                                          &
    ! Weighted share of the lake tile when it's divided up between tiles that
    ! have a soil column.
  win,                                                                         &
    ! Water entering layer (kg/m2).
  tsoilw,                                                                      &
  hconsw,                                                                      &
  dzsoilw,                                                                     &
    ! Working copies of tsoil, hcon and dzsoil for the loop.
  ratio_snowmass,                                                              &
  ratio_sice,                                                                  &
  ratio_sliq,                                                                  &
    ! Water tracer to water ratios
  submelt_wtrac
    ! Amount of water tracer snow that is melted or liquid that is frozen

!-----------------------------------------------------------------------------
! Local arrays
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  asnow(nsmax),                                                                &
    ! Effective thermal conductivity (W/m2/K).
  a(nsmax),                                                                    &
    ! Below-diagonal matrix elements.
  b(nsmax),                                                                    &
    ! Diagonal matrix elements.
  c(nsmax),                                                                    &
    ! Above-diagonal matrix elements.
  dt(nsmax),                                                                   &
    ! Temperature increments (K).
  r(nsmax),                                                                    &
    ! Matrix equation rhs.
  dsice_wtrac(n_wtrac_jls),                                                    &
    ! Melt of water tracer snow beneath canopy (kg/m2/s)
  can_melt_wtrac(n_wtrac_jls),                                                 &
    ! Melt of water tracer snow on the canopy (kg/m2/s).
  win_wtrac(n_wtrac_jls)
    ! Water tracer entering layer (kg/m2).

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SNOWPACK'

!-----------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set logicals:
flake_tile = (( l_flake_model ) .AND. (surft_n == lake ))
flake_not_lake = (( l_flake_model ) .AND. .NOT. (surft_n == lake ))

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(i,k,n,i_wt,asoil,can_melt,coldsnow,dsice,g_snow_surf,rho_temp,   &
!$OMP         sliqmax,submelt,smclf,win,tsoilw,hconsw,dzsoilw,asnow,a,b,c,dt,  &
!$OMP         r, tile_frac_lake_adj,ratio_snowmass,ratio_sice,ratio_sliq,      &
!$OMP         submelt_wtrac,win_wtrac,dsice_wtrac,can_melt_wtrac)

! Required for bit comparison in the UM to ensure all tile points are set to
! zero regardless of fraction present

DO n = 1,nsmax
!$OMP DO SCHEDULE(STATIC)
  DO i = 1, land_pts
    rho_snow(i,n) = 0.0
  END DO
!$OMP END DO
END DO

!$OMP DO SCHEDULE(STATIC)
DO i = 1, land_pts
  snow_soil_htf(i) = 0.0
END DO
!$OMP END DO

!$OMP DO SCHEDULE(STATIC)
DO k = 1,surft_pts
  i = surft_index(k)

  IF (l_elev_land_ice .AND. l_lice_point(i)) THEN
    tsoilw  = tsurf_elev_surft(i)
    dzsoilw = dzsoil_elev
    IF (l_lice_surft(surft_n)) THEN
      hconsw = snow_hcon
    ELSE
      hconsw = hcondeep
    END IF
  ELSE
    tsoilw  = t_soil1_soilt(i)
    hconsw  = hcons(i)
    dzsoilw = dzsoil(1)
  END IF

  g_snow_surf = surf_htf_surft(i)

  !---------------------------------------------------------------------------
  ! Add melt to snow surface heat flux, unless using the snow canopy model
  !---------------------------------------------------------------------------
  IF ( .NOT. cansnowtile ) g_snow_surf = g_snow_surf + lf * melt_surft(i)

  IF ( nsnow(i) == 0 ) THEN

    IF (flake_tile) THEN
      IF (lake_h_ice_gb(i) < EPSILON(0.0) ) THEN
        ! Remove snow that falls on unfrozen lake_vars.
        snowfall(i) = 0.0
        IF (l_wtrac_jls) THEN
          ! Repeat for water tracers
          DO i_wt = 1, n_wtrac_jls
            snowfall_wtrac(i,i_wt) = 0.0
          END DO
        END IF
      END IF
    END IF

    ! Add snowfall (including canopy unloading) to ground snowpack.
    snowmass(i) = snowmass(i) + snowfall(i)

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1,n_wtrac_jls
        snowmass_wtrac(i,i_wt) = snowmass_wtrac(i,i_wt)+snowfall_wtrac(i,i_wt)
      END DO
    END IF

    IF ( .NOT. cansnowtile ) THEN
      !-----------------------------------------------------------------------
      ! Remove sublimation and melt from snowpack.
      !-----------------------------------------------------------------------
      snowmass(i) = snowmass(i) + snowinc_surft(i)

      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          snowmass_wtrac(i,i_wt) = snowmass_wtrac(i,i_wt) -                    &
                                  ( ei_surft_wtrac(i,surft_n,i_wt)             &
                                    + melt_surft_wtrac(i,surft_n,i_wt) )       &
                                  * timestep
        END DO
      END IF

      IF (sf_diag%l_snice) sf_diag%snice_m_surft(i,surft_n) = melt_surft(i)

    ELSE
      !     Melting of snow on the canopy is not accounted for within the
      !     surface budget.
      IF (sf_diag%l_snice) sf_diag%snice_m_surft(i,surft_n) = 0.0
    END IF
    !
        !-------------------------------------------------------------------------
        ! Melt snow on the ground if appropriate. This is always considered if
        ! a canopy model is used, but otherwise it must be selected by an
        ! appropriate option.
        !-------------------------------------------------------------------------
    IF ( ( .NOT. l_lice_point(i)) .AND. (cansnowtile .OR.                      &
         (i_basal_melting_opt == ip_basal_melting_diag_0) ) ) THEN
      IF ( tsoilw > tm ) THEN

        smclf = rho_water * dzsoilw * smvcst1_soilt(i) * sthf1_soilt(i)

        tile_frac_lake_adj = 0.0

        IF (flake_tile .AND. (lake_h_ice_gb(i) > EPSILON(0.0)) ) THEN
          ! Basal melting of snow on lake ice.
          smclf = rho_ice * lake_h_ice_gb(i) * 1.0
          asoil = 1.0 / (hcapi * smclf)
          submelt = MIN( snowmass(i) / timestep,                               &
                             (tsoilw - tm) / (lf * asoil * timestep) )

        ELSE IF (flake_tile .AND. (lake_h_mxl_gb(i) > EPSILON(0.0)) ) THEN
          ! No ice present, melt snow into the lake
          smclf = rho_water * lake_h_mxl_gb(i) * 1.0
          asoil = 1.0 / (hcapw * smclf)
          submelt = MIN( snowmass(i) / timestep,                               &
                             (tsoilw - tm) / (lf * asoil * timestep) )

        ELSE IF (flake_tile .AND. (lake_depth_gb(i) > EPSILON(0.0)) ) THEN
          ! No ice present, melt snow into the lake
          smclf = rho_water * lake_depth_gb(i) * 1.0
          asoil = 1.0 / (hcapw * smclf)
          submelt = MIN( snowmass(i) / timestep,                               &
                             (tsoilw - tm) / (lf * asoil * timestep) )

        ELSE IF (flake_not_lake .AND. (non_lake_frac(i) > EPSILON(0.0))) THEN
          ! Not the lake tile, melt snow into the soil.
          asoil = 1.0 /                                                        &
                (dzsoilw * hcaps1_soilt(i)                                     &
                 + hcapw * (smcl1_soilt(i) - smclf)                            &
                 + hcapi * smclf)

          submelt       = MIN( snowmass(i) / timestep,                         &
                             (tsoilw - tm) / (lf * asoil * timestep) )

          ! To make sure soil temperature values are scaled appropriately
          ! for the whole gridbox (i.e. fractions used to calculate
          ! t_soil1_soilt add up to 1.0), share the lake fraction between
          ! the other surface tiles, weighting the fractions.
          tile_frac_lake_adj = tile_frac(i,lake)                               &
                                * (tile_frac(i,surft_n) / non_lake_frac(i))

        ELSE
          ! Not using FLake, melt snow into the soil.
          asoil = 1.0 /                                                        &
                (dzsoilw * hcaps1_soilt(i)                                     &
                 + hcapw * (smcl1_soilt(i) - smclf)                            &
                 + hcapi * smclf)

          submelt       = MIN( snowmass(i) / timestep,                         &
                             (tsoilw - tm) / (lf * asoil * timestep) )

        END IF

        ! Update water tracers first, so ratio calculation can use snowmass
        ! before is it updated
        IF (l_wtrac_jls) THEN
          DO i_wt = 1,n_wtrac_jls
            ratio_snowmass           = wtrac_calc_ratio_fn_jules(i_wt,         &
                                        snowmass_wtrac(i,i_wt), snowmass(i))
            submelt_wtrac            = MIN(ratio_snowmass * submelt,           &
                                           snowmass_wtrac(i,i_wt) / timestep)
            snowmass_wtrac(i,i_wt)   = snowmass_wtrac(i,i_wt)                  &
                                      - submelt_wtrac * timestep
            melt_surft_wtrac(i,surft_n,i_wt) =                                 &
                                       melt_surft_wtrac(i,surft_n,i_wt)        &
                                      + submelt_wtrac
          END DO
        END IF      ! l_wtrac_jls

        snowmass(i)   = snowmass(i) - submelt * timestep
        tsoilw        = tsoilw - (tile_frac(i,surft_n) + tile_frac_lake_adj)   &
                         * asoil * timestep * lf * submelt
        melt_surft(i) = melt_surft(i) + submelt
        !       Add any melting of snow on the ground into the water budget.
        IF (sf_diag%l_snice) sf_diag%snice_m_surft(i,surft_n) =                &
          sf_diag%snice_m_surft(i,surft_n) + submelt
        snomlt_sub_htf(i) = snomlt_sub_htf(i) +                                &
                            tile_frac(i,surft_n) * lf * submelt

      END IF
    END IF

    ! Set flux into uppermost snow/soil layer (after melting).
    snow_soil_htf(i) = surf_htf_surft(i)

    ! Diagnose snow depth.
    snowdepth(i) = snowmass(i) / rho_snow_grnd(i)

    ! Set values for the surface layer. These are only needed for nsmax>0.
    IF ( nsmax > 0 ) THEN
      rho0(i)   = rho_snow_grnd(i)
      sice0(i)  = snowmass(i)
      tsnow0(i) = MIN( tsoilw, tm )
      IF (l_wtrac_jls) THEN
        DO i_wt = 1,n_wtrac_jls
          sice0_wtrac(i,i_wt)  = snowmass_wtrac(i,i_wt)
        END DO
      END IF   ! l_wtrac_jls
    END IF

    ! Note with nsmax>0 the density of a shallow pack (nsnow=0) does not
    ! evolve with time (until it is exhausted). We could consider updating
    ! the density using a mass-weighted mean of the pack density and that
    ! of fresh snow, so that a growing pack would reach nsnow=1 more quickly.
    ! This would only affect a pack that grows from a non-zero state, and
    ! is not an issue if the pack grows from zero, because in that case the
    ! the density was previously set to the fresh snow value.

  ELSE

    !-------------------------------------------------------------------------
    ! There is at least one snow layer. Calculate heat conduction between
    !   layers and temperature increments.
    !-------------------------------------------------------------------------

    ! Save rate of melting of snow on the canopy.
    IF ( cansnowtile ) THEN
      can_melt = melt_surft(i)
      IF (l_wtrac_jls) THEN
        ! Repeat for water tracersw
        DO i_wt = 1,n_wtrac_jls
          can_melt_wtrac(i_wt) = melt_surft_wtrac(i,surft_n,i_wt)
        END DO
      END IF    ! l_wtrac_jls
    ELSE
      can_melt = 0.0
      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          can_melt_wtrac(i_wt) = 0.0
        END DO
      END IF    ! l_wtrac_jls
    END IF

    IF ( nsnow(i) == 1 ) THEN

      ! Single layer of snow.
      asnow(1)        = 2.0 / ( snowdepth(i) / ksnow(i,1) + dzsoilw / hconsw )
      snow_soil_htf(i) = asnow(1) * ( tsnow(i,1) - tsoilw )
      dt(1)            = ( g_snow_surf - snow_soil_htf(i) ) * timestep /       &
                         ( csnow(i,1) + r_gamma * asnow(1) * timestep )
      snow_soil_htf(i) = asnow(1) * ( tsnow(i,1) + r_gamma * dt(1) - tsoilw )
      tsnow(i,1)       = tsnow(i,1) + dt(1)

    ELSE

      ! Multiple snow layers.
      DO n = 1,nsnow(i) - 1
        asnow(n) = 2.0 / ( ds(i,n) / ksnow(i,n) + ds(i,n+1) / ksnow(i,n+1) )
      END DO
      n = nsnow(i)
      asnow(n) = 2.0 / ( ds(i,n) / ksnow(i,n) + dzsoilw / hconsw )
      a(1)     = 0.0
      b(1)     = csnow(i,1) + r_gamma * asnow(1) * timestep
      c(1)     = -r_gamma * asnow(1) * timestep
      r(1)     = ( g_snow_surf - asnow(1) * (tsnow(i,1) - tsnow(i,2)) )        &
                 * timestep
      DO n = 2,nsnow(i) - 1
        a(n) = -r_gamma * asnow(n-1) * timestep
        b(n) = csnow(i,n) + r_gamma * ( asnow(n-1) + asnow(n) )                &
                                                       * timestep
        c(n) = -r_gamma * asnow(n) * timestep
        r(n) = asnow(n-1) * (tsnow(i,n-1) - tsnow(i,n) ) * timestep            &
               +  asnow(n) * (tsnow(i,n+1) - tsnow(i,n)) * timestep
      END DO
      n = nsnow(i)
      a(n) = -r_gamma * asnow(n-1) * timestep
      b(n) = csnow(i,n) + r_gamma * (asnow(n-1) + asnow(n)) * timestep
      c(n) = 0.0
      r(n) = asnow(n-1) * ( tsnow(i,n-1) - tsnow(i,n) ) * timestep             &
             + asnow(n) * ( tsoilw - tsnow(i,n) ) * timestep

      !-----------------------------------------------------------------------
      ! Call the tridiagonal solver.
      !-----------------------------------------------------------------------
      CALL tridag( nsnow(i),nsmax,a,b,c,r,dt )

      n = nsnow(i)
      snow_soil_htf(i) = asnow(n) * ( tsnow(i,n) + r_gamma * dt(n) - tsoilw )
      DO n = 1,nsnow(i)
        tsnow(i,n) = tsnow(i,n) + dt(n)
      END DO

    END IF  !  NSNOW

    !-------------------------------------------------------------------------
    ! Melt snow in layers with temperature exceeding melting point
    !-------------------------------------------------------------------------
    DO n = 1,nsnow(i)
      coldsnow = csnow(i,n) * (tm - tsnow(i,n))
      IF ( coldsnow < 0.0 ) THEN
        tsnow(i,n) = tm
        dsice = -coldsnow / lf
        IF ( dsice > sice(i,n) ) dsice = sice(i,n)
        ds(i,n)   = ( 1.0 - dsice / sice(i,n) ) * ds(i,n)

        ! Update water tracers first, so ratio calculation can use sice before
        ! it is updated
        IF (l_wtrac_jls) THEN
          DO i_wt = 1,n_wtrac_jls
            ratio_sice = wtrac_calc_ratio_fn_jules(i_wt, sice_wtrac(i,n,i_wt), &
                                                   sice(i,n))
            dsice_wtrac(i_wt)    = MIN(dsice * ratio_sice, sice_wtrac(i,n,i_wt))
            sice_wtrac(i,n,i_wt) = sice_wtrac(i,n,i_wt) - dsice_wtrac(i_wt)
            sliq_wtrac(i,n,i_wt) = sliq_wtrac(i,n,i_wt) + dsice_wtrac(i_wt)
          END DO
        END IF  ! l_wtrac_jls

        sice(i,n) = sice(i,n) - dsice
        sliq(i,n) = sliq(i,n) + dsice
        IF (sf_diag%l_snice) THEN
          sf_diag%snice_m_surft(i,surft_n) = sf_diag%snice_m_surft(i,surft_n)  &
                                             + dsice / timestep
        END IF
      END IF
    END DO
    ! Melt still > 0? - no snow left

    !-------------------------------------------------------------------------
    ! Remove snow by sublimation unless snow is beneath canopy
    !-------------------------------------------------------------------------
    IF ( .NOT. cansnowtile ) THEN

      dsice = MAX( ei_surft(i), 0.0 ) * timestep
      IF (l_wtrac_jls) THEN
        DO i_wt = 1,n_wtrac_jls
          dsice_wtrac(i_wt) = MAX( ei_surft_wtrac(i,surft_n,i_wt), 0.0 )       &
                                * timestep
        END DO
      END IF   ! l_wtrac_jls

      IF ( dsice > 0.0 ) THEN
        DO n = 1,nsnow(i)
          IF ( dsice > sice(i,n) ) THEN
            ! Layer sublimates completely
            dsice     = dsice - sice(i,n)
            sice(i,n) = 0.0
            ds(i,n)   = 0.0
            IF (l_wtrac_jls) THEN
              ! Need to remove all water tracer in layer
              DO i_wt = 1, n_wtrac_jls
                dsice_wtrac(i_wt)    = dsice_wtrac(i_wt) - sice_wtrac(i,n,i_wt)
                sice_wtrac(i,n,i_wt) = 0.0
              END DO
            END IF
          ELSE
            ! Layer sublimates partially
            ds(i,n)   = (1.0 - dsice / sice(i,n)) * ds(i,n)
            sice(i,n) = sice(i,n) - dsice
            IF (l_wtrac_jls) THEN
              ! Remove all water tracer leftover sublimation here
              DO i_wt = 1, n_wtrac_jls
                sice_wtrac(i,n,i_wt) = sice_wtrac(i,n,i_wt) - dsice_wtrac(i_wt)
              END DO
            END IF

            EXIT    !   sublimation exhausted
          END IF

        END DO
      END IF  !  DSICE>0
    END IF  !  CANSNOWTILE

    !-------------------------------------------------------------------------
    ! Move liquid water in excess of holding capacity downwards or refreeze.
    !-------------------------------------------------------------------------
    ! Optionally include infiltration of rainwater and melting from the
    ! canopy into the snowpack.
    IF (l_snow_infilt) THEN
      win = infiltration(i) + can_melt * timestep
      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          win_wtrac(i_wt) = infiltration_wtrac(i,i_wt) + can_melt_wtrac(i_wt)  &
                            * timestep
        END DO
      END IF   ! l_wtrac_jls

    ELSE
      win = 0.0
      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          win_wtrac(i_wt) = 0.0
        END DO
      END IF   ! l_wtrac_jls

    END IF ! l_snow_infilt

    DO n = 1,nsnow(i)

      sliq(i,n) = sliq(i,n) + win
      win       = 0.0
      sliqmax   = snowliqcap * rho_water * ds(i,n)

      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          sliq_wtrac(i,n,i_wt) = sliq_wtrac(i,n,i_wt) + win_wtrac(i_wt)
          win_wtrac(i_wt) = 0.0
        END DO
      END IF   ! l_wtrac_jls

      ! Construct a temporary rho here. If layer has gone (ds=0), just feed
      ! everything down to the next layer.
      IF ( ds(i,n) > EPSILON(0.0) ) THEN
        rho_temp = MIN(rho_ice,(sice(i,n) + sliq(i,n)) / ds(i,n))
      ELSE
        rho_temp = rho_ice
      END IF

      ! Reduce pore density of pack as we approach solid ice.
      IF (l_elev_land_ice .AND. l_lice_point(i)) THEN
        IF (rho_temp >= rho_firn_pore_closure) THEN
          sliqmax = 0.0
        ELSE IF ((rho_temp >= rho_firn_pore_restrict) .AND.                    &
                 (rho_temp < rho_firn_pore_closure)) THEN
          sliqmax = sliqmax * (rho_firn_pore_closure - rho_temp) /             &
                              (rho_firn_pore_closure - rho_firn_pore_restrict)
        END IF
      END IF

      IF (sliq(i,n) > sliqmax) THEN
        ! Liquid capacity exceeded

        ! Update water tracers first so that sliq can be used in ratio
        ! calculation before it is updated
        IF (l_wtrac_jls) THEN
          DO i_wt = 1, n_wtrac_jls
            ratio_sliq           = wtrac_calc_ratio_fn_jules(i_wt,             &
                                     sliq_wtrac(i,n,i_wt), sliq(i,n))

            win_wtrac(i_wt)      = (sliq(i,n)-sliqmax) * ratio_sliq
            sliq_wtrac(i,n,i_wt) = sliq_wtrac(i,n,i_wt) - win_wtrac(i_wt)
          END DO
        END IF

        win       = sliq(i,n) - sliqmax
        sliq(i,n) = sliqmax
      END IF

      coldsnow = csnow(i,n) * (tm - tsnow(i,n))
      IF (coldsnow > 0.0) THEN
        ! Liquid can freeze
        dsice      = MIN(sliq(i,n), coldsnow / lf)

        ! Update water tracers first so the ratio calculation can use sliq
        ! before it is updated
        IF (l_wtrac_jls) THEN
          DO i_wt = 1,n_wtrac_jls
            IF (dsice == sliq(i,n)) THEN
              ! Simply move all water tracer from liquid to ice
              dsice_wtrac(i_wt) = sliq_wtrac(i,n,i_wt)
            ELSE
              ! Partial freezing
              ratio_sliq = wtrac_calc_ratio_fn_jules(i_wt,                     &
                                     sliq_wtrac(i,n,i_wt), sliq(i,n))
              dsice_wtrac(i_wt) = ratio_sliq * dsice
            END IF
            sliq_wtrac(i,n,i_wt)  = sliq_wtrac(i,n,i_wt) - dsice_wtrac(i_wt)
            sice_wtrac(i,n,i_wt)  = sice_wtrac(i,n,i_wt) + dsice_wtrac(i_wt)
          END DO
        END IF    ! l_wtrac_jls

        sliq(i,n)  = sliq(i,n) - dsice
        sice(i,n)  = sice(i,n) + dsice
        tsnow(i,n) = tsnow(i,n) + lf * dsice / csnow(i,n)
        IF (sf_diag%l_snice) THEN
          sf_diag%snice_freez_surft(i,surft_n) =                               &
                       sf_diag%snice_freez_surft(i,surft_n) + dsice / timestep
        END IF
      END IF  !  coldsnow

    END DO  !  layers

    !-------------------------------------------------------------------------
    ! The remaining liquid water flux is melt.
    ! Include any separate canopy melt in this diagnostic.
    !-------------------------------------------------------------------------
    IF ( l_snow_infilt ) THEN
      ! Canopy melting has already been added to infiltration.
      melt_surft(i) = ( win / timestep )
      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          melt_surft_wtrac(i,surft_n,i_wt) = ( win_wtrac(i_wt) / timestep )
        END DO
      END IF
    ELSE
      melt_surft(i) = ( win / timestep ) + can_melt
      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          melt_surft_wtrac(i,surft_n,i_wt) = ( win_wtrac(i_wt) / timestep )    &
                                             + can_melt_wtrac(i_wt)
        END DO
      END IF
    END IF

    !-------------------------------------------------------------------------
    ! Diagnose layer densities
    !-------------------------------------------------------------------------
    DO n = 1,nsnow(i)
      IF ( ds(i,n) > EPSILON(ds) ) THEN
        rho_snow(i,n) = (sice(i,n) + sliq(i,n)) / ds(i,n)
      END IF
    END DO

    !-------------------------------------------------------------------------
    ! Add snowfall and frost as layer 0.
    !-------------------------------------------------------------------------
    sice0(i) = snowfall(i)
    IF (l_wtrac_jls) THEN
      DO i_wt = 1,n_wtrac_jls
        ! Repeat for water tracers
        sice0_wtrac(i,i_wt) = snowfall_wtrac(i,i_wt)
      END DO
    END IF     !l_wtrac_jls

    IF ( .NOT. cansnowtile ) THEN
      sice0(i) = snowfall(i) - MIN(ei_surft(i), 0.0) * timestep
      IF (l_wtrac_jls) THEN
        ! Repeat for water tracers
        DO i_wt = 1,n_wtrac_jls
          sice0_wtrac(i,i_wt) = snowfall_wtrac(i,i_wt)                         &
                               - MIN(ei_surft_wtrac(i,surft_n,i_wt), 0.0)      &
                                 * timestep
        END DO
      END IF    !l_wtrac_jls
    END IF

    tsnow0(i) = tsnow(i,1)
    rho0(i)   = rho_snow_fresh

    !-------------------------------------------------------------------------
    ! Diagnose total snow depth and mass
    !-------------------------------------------------------------------------
    snowdepth(i) = sice0(i) / rho0(i)
    snowmass(i)  = sice0(i)
    DO n = 1,nsnow(i)
      snowdepth(i) = snowdepth(i) + ds(i,n)
      snowmass(i)  = snowmass(i) + sice(i,n) + sliq(i,n)
    END DO

    IF (l_wtrac_jls) THEN
      ! Repeat for water tracers
      DO i_wt = 1,n_wtrac_jls
        snowmass_wtrac(i,i_wt)  = sice0_wtrac(i,i_wt)
      END DO
      DO n = 1,nsnow(i)
        DO i_wt = 1,n_wtrac_jls
          snowmass_wtrac(i,i_wt) = snowmass_wtrac(i,i_wt)                      &
                               + sice_wtrac(i,n,i_wt) + sliq_wtrac(i,n,i_wt)
        END DO
      END DO
    END IF  ! l_wtrac_jls

  END IF    !  nsnow

  ! tsoil only modified for canopy snow tiles (and not land ice ones,
  ! although they shouldn't have this switch on anyway)
  IF ( .NOT. l_elev_land_ice) THEN
    t_soil1_soilt(i) = tsoilw
  ELSE IF ( .NOT. l_lice_point(i)) THEN
    t_soil1_soilt(i) = tsoilw
  END IF

END DO  !  k (points)
!$OMP END DO
!$OMP END PARALLEL
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE snowpack
END MODULE snowpack_mod
