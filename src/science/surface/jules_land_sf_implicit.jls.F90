! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE jules_land_sf_implicit_mod

USE ereport_mod, ONLY: ereport
USE sf_melt_mod, ONLY: sf_melt
USE screen_tq_mod, ONLY: screen_tq
USE sf_evap_mod, ONLY: sf_evap
USE im_sf_pt2_mod, ONLY: im_sf_pt2
USE sice_htf_mod, ONLY: sice_htf

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='JULES_LAND_SF_IMPLICIT_MOD'

CONTAINS
!  SUBROUTINE JULES_LAND_SF_IMPLICIT --------------------------------
!
!  Purpose: Calculate implicit correction for land point to surface
!           fluxes of heat,moisture and momentum, to be used by
!           the unconditionally stable and non-oscillatory BL
!           numerical solver.
!
!--------------------------------------------------------------------
!    Arguments :-
SUBROUTINE jules_land_sf_implicit (                                            &
! IN values defining field dimensions and subset to be processed :
 land_pts,land_index,nsurft,surft_index,surft_pts,sm_levels,                   &
 canhc_surft,canopy,flake,smc_soilt,tile_frac,wt_ext_surft,fland,flandg,       &
! IN values defining water tracer field dimensions
 n_wtrac_jls, n_evap_srce,                                                     &
! IN everything not covered so far :
 lw_down,sw_surft,sky,t_soil_soilt,r_gamma,alpha1,ashtf_prime_surft,           &
 dtrdz_charney_grid_1,fracaero_t,fracaero_s,resfs,resft,rhokh_surft,           &
 emis_surft,snow_surft,dtstar_surft,                                           &
! INOUT data :
 tstar_surft,fqw_surft,fqw_1,ftl_1,ftl_surft,sf_diag,                          &
! OUT Diagnostic not requiring STASH flags :
 ecan,ei_surft,esoil_surft,surf_ht_flux_land,ei_land,surf_htf_surft,           &
! OUT data required elsewhere in UM system :
 tstar_land,le_surft,radnet_surft,ecan_surft,esoil_soilt,                      &
 ext_soilt,melt_surft,snowinc_surft,tstar_surft_old,ERROR,                     &
 !New arguments replacing USE statements
 ! lake_mod (IN)
 lake_h_ice_gb,                                                                &
 ! lake_mod (OUT)
 surf_ht_flux_lake_ij, non_lake_frac,                                          &
 ! fluxes (IN)
 anthrop_heat_surft,                                                           &
 ! fluxes (OUT)
 surf_ht_store_surft,                                                          &
 lake_evap,                                                                    &
 ! c_elevate (IN)
 lw_down_elevcorr_surft,                                                       &
 ! prognostics (IN)
 nsnow_surft,                                                                  &
 ! jules_mod (IN)
 snowdep_surft,                                                                &
 ! JULES Types containing field data (IN OUT)
 crop_vars,                                                                    &
 ! Water tracers (IN)
 snow_surft_wtrac, smc_soilt_wtrac, canopy_wtrac, fqw_evapsrce_wtrac,          &
 ! Water tracers (INOUT)
 fqw_surft_wtrac,                                                              &
 ! Water tracers (OUT)
 ei_surft_wtrac, ei_wtrac, esoil_surft_wtrac, esoil_soilt_wtrac,               &
 ext_soilt_wtrac, ecan_surft_wtrac, ecan_wtrac, lake_evap_wtrac,               &
 fqw_1_wtrac, dfqw_wtrac                                                       &
)

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type

USE csigma,                   ONLY: sbcon

USE planet_constants_mod,     ONLY: cp

USE atm_fields_bounds_mod,    ONLY: tdims, pdims

USE theta_field_sizes,        ONLY: t_i_length, t_j_length

USE jules_surface_mod,        ONLY: l_aggregate, l_flake_model, ls

USE jules_snow_mod,           ONLY:                                            &
  nsmax, rho_snow_const, cansnowtile, l_snow_nocan_hc

USE jules_surface_types_mod,  ONLY: lake

USE sf_diags_mod,             ONLY: strnewsfdiag

USE timestep_mod,             ONLY: timestep

USE ancil_info,               ONLY: nsoilt

USE jules_surface_mod,        ONLY: l_neg_tstar
USE jules_rivers_mod,         ONLY: l_rivers, lake_water_conserve_method,      &
                                    use_fqw_surft, use_elake_surft,            &
                                    i_river_vn, rivers_um_trip

USE water_constants_mod,      ONLY: lc, lf, rho_ice

USE solinc_data,              ONLY: l_skyview

USE jules_water_tracers_mod,  ONLY: l_wtrac_imp_jls, standard_ratio_wtrac
USE sf_land_imp_wtrac_mod,    ONLY: sf_land_imp_wtrac

USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

USE jules_print_mgr,          ONLY: jules_message, jules_print


IMPLICIT NONE
!--------------------------------------------------------------------
!  Inputs :-
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
!--------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
 land_pts    ! IN No of land points

! (c) Soil/vegetation/land surface parameters (mostly constant).
INTEGER, INTENT(IN) ::                                                         &
 land_index(land_pts)        ! IN LAND_INDEX(I)=J => the Jth
                             !    point in ROW_LENGTH,ROWS is the
                             !    Ith land point.

INTEGER, INTENT(IN) ::                                                         &
 sm_levels                                                                     &
                             ! IN No. of soil moisture levels
,nsurft                                                                        &
                             ! IN No. of land tiles
,surft_index(land_pts,nsurft)                                                  &
                             ! IN Index of tile points
,surft_pts(nsurft)
                             ! IN Number of tile points

INTEGER, INTENT(IN) ::                                                         &
 n_wtrac_jls,                                                                  &
                             ! IN No. of water tracers in JULES
 n_evap_srce
                             ! IN No. of evaporative water sources

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 canhc_surft(land_pts,nsurft)                                                  &
                             ! IN Areal heat capacity of canopy
                             !    for land tiles (J/K/m2).
,canopy(land_pts,nsurft)                                                       &
                             ! IN Surface/canopy water for
                             !    snow-free land tiles (kg/m2)
,flake(land_pts,nsurft)                                                        &
                             ! IN Lake fraction.
,smc_soilt(land_pts,nsoilt)                                                    &
                             ! IN Available soil moisture (kg/m2).
,tile_frac(land_pts,nsurft)                                                    &
                             ! IN Tile fractions including
                             !    snow cover in the ice tile.
,wt_ext_surft(land_pts,sm_levels,nsurft)                                       &
                             ! IN Fraction of evapotranspiration
                             !    extracted from each soil layer
                             !    by each tile.
,fland(land_pts)                                                               &
                             ! IN Land fraction on land pts.
,flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Land fraction on all pts.
,emis_surft(land_pts,nsurft)                                                   &
                             ! IN Emissivity for land tiles
,snow_surft(land_pts,nsurft)                                                   &
                             ! IN Lying snow on tiles (kg/m2)
,dtstar_surft(land_pts,nsurft)
                             ! IN Change in TSTAR over timestep
                             !    for land tiles

! (f) Atmospheric + any other data not covered so far, incl control.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 lw_down(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! IN Surface downward LW radiation
                                  !    (W/m2).
,sky(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                             ! IN skyview correction for surface LW
,sw_surft(land_pts,nsurft)                                                     &
                             ! IN Surface net SW radiation on
                                  !    land tiles (W/m2).
,t_soil_soilt(land_pts,nsoilt,sm_levels)
                             ! IN Soil temperatures (K).

REAL(KIND=real_jlslsm), INTENT(IN) :: r_gamma
                             ! IN implicit weight in level 1

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 alpha1(land_pts,nsurft)                                                       &
                             ! IN Mean gradient of saturated
                             !    specific humidity with respect
                             !    to temperature between the
                             !    bottom model layer and tile
                             !    surfaces
,ashtf_prime_surft(land_pts,nsurft)                                            &
                             ! IN Adjusted SEB coefficient for
                             !    land tiles.
,dtrdz_charney_grid_1(pdims%i_start:pdims%i_end,                               &
                      pdims%j_start:pdims%j_end)                               &
                             ! IN -g.dt/dp for model layers.
,fracaero_t(land_pts,nsurft)                                                   &
                             ! IN Total fraction of surface moisture
                             !    flux with only aerodynamic
                             !    resistance
,fracaero_s(land_pts,nsurft)                                                   &
                             ! IN Fraction of surface moisture
                             !    flux with only aerodynamic
                             !    resistance over the frozen part of the
                             !    surface
,resfs(land_pts,nsurft)                                                        &
                             ! IN Combined soil, stomatal
                             !    and aerodynamic resistance
                             !    factor for fraction (1-fracaero_t) of
                             !    snow-free land tiles.
,resft(land_pts,nsurft)                                                        &
                             ! IN Total resistance factor.
                             !    fracaero+(1-fracaero)*resfs for
                             !    snow-free land, 1 for snow.
,rhokh_surft(land_pts,nsurft)
                             ! IN Surface exchange coefficients
                             !    for land tiles

! Water tracers (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: snow_surft_wtrac(land_pts,nsurft,        &
                                                       n_wtrac_jls)
                             ! Water tracer lying snow on tiles (kg/m2)
REAL(KIND=real_jlslsm), INTENT(IN) :: smc_soilt_wtrac(land_pts,nsoilt,         &
                                                       n_wtrac_jls)
                             ! Water tracer in available soil moisture
                             !    (kg/m2).
REAL(KIND=real_jlslsm), INTENT(IN) :: canopy_wtrac(land_pts,nsurft,n_wtrac_jls)
                             ! Water tracer surface/canopy water for
                             !    snow-free land tiles (kg/m2)
REAL(KIND=real_jlslsm), INTENT(IN) :: fqw_evapsrce_wtrac(land_pts,             &
                                               n_evap_srce,nsurft,n_wtrac_jls)
                             ! Water tracer surface FQW for each
                             !     evap source for land tiles (kg/m2/s)


!--------------------------------------------------------------------
!  In/outs :-
!--------------------------------------------------------------------
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 tstar_surft(land_pts,nsurft)                                                  &
                             ! INOUT Surface tile temperatures
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT Moisture flux between layers
                             !       (kg per square metre per sec)
                             !       FQW(,1) is total water flux
                             !       from surface, 'E'.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT FTL(,K) contains net
                             !       turbulent sensible heat flux
                             !       into layer K from below; so
                             !       FTL(,1) is the surface
                             !       sensible heat, H.(W/m2)
,ftl_surft(land_pts,nsurft)                                                    &
                             ! INOUT Surface FTL for land tiles
,fqw_surft(land_pts,nsurft)
                             ! INOUT Surface FQW for land tiles


! Water tracers (INOUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: fqw_surft_wtrac(land_pts,nsurft,     &
                                                         n_wtrac_jls)
                             ! Water tracer surface FQW for land tiles
                             !    (kg/m2/s)

!--------------------------------------------------------------------
!  Outputs :-
!-1 Diagnostic (or effectively so - includes coupled model requisites):-

!  (a) Calculated anyway (use STASH space from higher level) :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 ecan(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! OUT Gridbox mean evaporation from
                             !     canopy/surface store (kg/m2/s).
                             !     Zero over sea.
,esoil_surft(land_pts,nsurft)                                                  &
                             ! OUT ESOIL for snow-free land tiles
,surf_ht_flux_land(tdims%i_start:tdims%i_end,                                  &
                   tdims%j_start:tdims%j_end)                                  &
                             ! OUT Net downward heat flux at
                             !     surface over land
                             !     fraction of gridbox (W/m2).
,ei_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! OUT Sublimation from lying snow
                             !     (kg/m2/s).
,surf_htf_surft(land_pts,nsurft)
                             ! OUT Net downward surface heat flux
                             !     on tiles (W/m2)

!-2 Genuinely output, needed by other atmospheric routines :-

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 tstar_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! OUT   Land mean sfc temperature (K)
,le_surft(land_pts,nsurft)                                                     &
                             ! OUT Surface latent heat flux for
                             !     land tiles
,radnet_surft(land_pts,nsurft)                                                 &
                             ! OUT Surface net radiation on
                             !     land tiles (W/m2)
,ei_surft(land_pts,nsurft)                                                     &
                             ! OUT EI for land tiles.
,ecan_surft(land_pts,nsurft)                                                   &
                             ! OUT ECAN for snow-free land tiles
,esoil_soilt(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nsoilt)       &
                             ! OUT Surface evapotranspiration
                             !     from soil moisture store
                             !     (kg/m2/s).
,ext_soilt(land_pts,nsoilt,sm_levels)                                          &
                             ! OUT Extraction of water from each
                             !     soil layer (kg/m2/s).
,melt_surft(land_pts,nsurft)                                                   &
                             ! OUT Snowmelt on land tiles (kg/m2/s
,snowinc_surft(land_pts,nsurft)                                                &
                             ! OUT Total increment to snow on land tiles
                             !     (kg m-2 TS-1). This is used in preference
                             !     to the rate to ensure the precise removal
                             !     of snow, rather than allowing rounding
                             !     errors to generate very small amounts.
,tstar_surft_old(land_pts,nsurft)                                              &
                             ! OUT Tile surface temperatures at
                             !     beginning of timestep.
,non_lake_frac(land_pts)
                             ! OUT total tile fraction for surface types
                             ! other than inland water

INTEGER, INTENT(OUT) ::                                                        &
 ERROR                       ! OUT 0 - AOK;
                             !     1 to 7  - bad grid definition detected;


! Water tracers (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ei_surft_wtrac(land_pts,nsurft,         &
                                                        n_wtrac_jls)
                             ! Water tracer sublimation for land tiles
                             !     (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ei_wtrac(tdims%i_start:tdims%i_end,     &
                                        tdims%j_start:tdims%j_end,n_wtrac_jls)
                             ! Water tracer sublimation from lying snow
                             !     (kg/m2/s).
REAL(KIND=real_jlslsm), INTENT(OUT) :: esoil_surft_wtrac(land_pts,nsurft,      &
                                                         n_wtrac_jls)
                             ! Water tracer sfc evapotranspiration for
                             !                 snow-free land tiles (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: esoil_soilt_wtrac(                      &
                                            tdims%i_start:tdims%i_end,         &
                                            tdims%j_start:tdims%j_end,nsoilt,  &
                                            n_wtrac_jls)
                             ! Water tracer surface evapotranspiration
                             !     from soil moisture store per soil tile
                             !     (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ext_soilt_wtrac(land_pts,nsoilt,        &
                                                       sm_levels,n_wtrac_jls)
                             ! Water tracer extraction of water from each
                             !     soil layer (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ecan_surft_wtrac(land_pts,nsurft,       &
                                                        n_wtrac_jls)
                             ! Water tracer canopy evaporation for snow-free
                             !     land tiles (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ecan_wtrac(tdims%i_start:tdims%i_end,   &
                                         tdims%j_start:tdims%j_end,n_wtrac_jls)
                             ! Water tracer GBM evaporation from
                             !     canopy/surface store (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: lake_evap_wtrac(land_pts,n_wtrac_jls)
                             ! Water tracer lake evaporation
REAL(KIND=real_jlslsm), INTENT(OUT) :: fqw_1_wtrac(                            &
                                          tdims%i_start:tdims%i_end,           &
                                          tdims%j_start:tdims%j_end,n_wtrac_jls)
                             ! Water tracer surface flux (kg/m2/s)
REAL(KIND=real_jlslsm), INTENT(OUT) :: dfqw_wtrac(tdims%i_start:tdims%i_end,   &
                                        tdims%j_start:tdims%j_end,n_wtrac_jls)
                             ! Increment in water tracer GBM moisture flux
                             !      (kg/m2/s)

!New arguments replacing USE statements
! lake_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: lake_h_ice_gb(land_pts)
! lake_mod (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: surf_ht_flux_lake_ij(t_i_length,t_j_length)
! fluxes (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: anthrop_heat_surft(land_pts,nsurft)
! fluxes (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: surf_ht_store_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(OUT) :: lake_evap(land_pts)
! c_elevate (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: lw_down_elevcorr_surft(land_pts,nsurft)
! prognostics (IN)
INTEGER, INTENT(IN) :: nsnow_surft(land_pts,nsurft)
! jules_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: snowdep_surft(land_pts,nsurft)

!TYPES containing field data (IN OUT)
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars

!--------------------------------------------------------------------
!  Workspace :-
!--------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
 elake_surft(land_pts,nsurft)                                                  &
                             ! Lake evaporation.
,melt_ice_surft(land_pts,nsurft)                                               &
                             ! Ice melt on FLake lake tile (kg/m2/s)
,lake_ice_mass(land_pts)                                                       &
                             ! areal density equivalent to
                             ! lake ice of a given depth (kg/m2)
,snowmelt(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                 &
                             ! Snowmelt (kg/m2/s).
,snowinc_flake(land_pts,nsurft)
                             ! Increment to ice for FLake

REAL(KIND=real_jlslsm) ::                                                      &
 elake_surft_wtrac(land_pts,nsurft,n_wtrac_jls)
                             ! Lake evaporation on land tiles
                             ! (When using tiles, this will only be non-zero
                             !  on the 'lake' tile.)

REAL(KIND=real_jlslsm) ::                                                      &
 canhc_surf(land_pts)
                             ! Areal heat capacity of canopy
                             ! for land tiles (J/K/m2).

!  Local scalars :-

INTEGER ::                                                                     &
 i,j                                                                           &
            ! LOCAL Loop counter (horizontal field index).
,k                                                                             &
            ! LOCAL Tile pointer
,l                                                                             &
            ! LOCAL Land pointer
,n                                                                             &
            ! LOCAL Loop counter (tile index).
,m                                                                             &
            ! Loop counter for soil tiles
,i_wt
            ! Loop counter for water tracers

#if defined(LFRIC)
LOGICAL, PARAMETER :: using_lfric = .TRUE.
#else
LOGICAL, PARAMETER :: using_lfric = .FALSE.
#endif

INTEGER :: errorstatus

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_LAND_SF_IMPLICIT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------

! Calculate surface scalar fluxes, temperatures only at the 1st call
! of the subroutine (first stage of the new BL solver) using standard
! MOSES2 physics and equations. These are the final values for this
! timestep and there is no need to repeat the calculation.
!-----------------------------------------------------------------------

ERROR = 0


!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,n,j,i,k)                                                       &
!$OMP SHARED(tdims,nsurft,surft_pts,surft_index,                               &
!$OMP        ftl_surft,nsoilt,land_pts,t_soil_soilt,                           &
!$OMP        tstar_surft_old,tstar_surft,dtstar_surft,cp,error,tile_frac,      &
!$OMP        non_lake_frac,lake,l_flake_model,l_aggregate)

!-----------------------------------------------------------------------
! 6.1 Convert FTL to sensible heat flux in Watts per square metre.
!-----------------------------------------------------------------------

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    ftl_surft(l,n) = cp * ftl_surft(l,n)
  END DO
!$OMP END DO NOWAIT
END DO

!-----------------------------------------------------------------------
! Land surface calculations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Optional error check : test for negative top soil layer temperature
!-----------------------------------------------------------------------
IF (l_neg_tstar) THEN
  DO m = 1,nsoilt
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      IF (t_soil_soilt(l,m,1) < 0) THEN
        ERROR = 1
        WRITE(jules_message,*)                                                 &
              '*** ERROR DETECTED BY ROUTINE JULES_LAND_SF_IMPLICIT ***'
        CALL jules_print('jules_land_sf_implicit_jls',jules_message)
        WRITE(jules_message,*) 'NEGATIVE TEMPERATURE IN TOP SOIL LAYER AT '
        CALL jules_print('jules_land_sf_implicit_jls',jules_message)
        WRITE(jules_message,*) 'LAND POINT ',l
        CALL jules_print('jules_land_sf_implicit_jls',jules_message)
      END IF
    END DO
!$OMP END DO NOWAIT
  END DO
END IF

!-----------------------------------------------------------------------
!   Diagnose the land surface temperature
!-----------------------------------------------------------------------

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    tstar_surft_old(l,n) = tstar_surft(l,n)
    tstar_surft(l,n) = tstar_surft_old(l,n) + dtstar_surft(l,n)
  END DO
!$OMP END DO NOWAIT
END DO

!-----------------------------------------------------------------------
!   Calculate non_lake_frac
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  ! initialise the non-lake fraction to one, not zero,
  ! in case there should ever be more than one lake tile, see below
  non_lake_frac(l) = 1.0
END DO
!$OMP END DO NOWAIT

IF ( ( l_flake_model ) .AND. ( .NOT. l_aggregate) ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    ! Remove FLake tile fraction.
    non_lake_frac(l) = non_lake_frac(l) - tile_frac(l,lake)
  END DO
!$OMP END DO NOWAIT
END IF

!$OMP END PARALLEL

!-----------------------------------------------------------------------
! 7.  Surface evaporation components and updating of surface
!     temperature (P245, routine SF_EVAP).
!-----------------------------------------------------------------------
CALL sf_evap (                                                                 &
  land_pts,nsurft,                                                             &
  land_index,surft_index,surft_pts,sm_levels,fland,                            &
  ashtf_prime_surft,canopy,dtrdz_charney_grid_1,flake,fracaero_t,fracaero_s,   &
  snow_surft,resfs,resft,rhokh_surft,tile_frac,smc_soilt,wt_ext_surft,         &
  timestep,r_gamma,fqw_1,fqw_surft,ftl_1,ftl_surft,tstar_surft,                &
  ecan,ecan_surft,elake_surft,esoil_soilt,esoil_surft,ei_surft,ext_soilt,      &
  sf_diag, non_lake_frac,                                                      &
  ! crop_vars_mod (IN)
  crop_vars%frac_irr_soilt, crop_vars%frac_irr_surft,                          &
  crop_vars%wt_ext_irr_surft, crop_vars%resfs_irr_surft,                       &
  ! crop_vars_mod (IN OUT)
  crop_vars%smc_irr_soilt,                                                     &
  ! crop_vars_mod (OUT)
  crop_vars%ext_irr_soilt)

!-----------------------------------------------------------------------
!     Surface melting of sea-ice and snow on land tiles.
!-----------------------------------------------------------------------

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l,n,j,i)                                                         &
!$OMP SHARED(tdims,ei_land,snowmelt,nsurft,land_pts,melt_ice_surft,            &
!$OMP snowinc_flake)

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    ei_land(i,j)  = 0.0
    snowmelt(i,j) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

! Lake initialisation
DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    melt_ice_surft(l,n) = 0.0
    snowinc_flake(l,n) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

!$OMP END PARALLEL

DO n = 1,nsurft
  CALL sf_melt (                                                               &
    land_pts,land_index,                                                       &
    surft_index(:,n),surft_pts(n),flandg,                                      &
    alpha1(:,n),ashtf_prime_surft(:,n),dtrdz_charney_grid_1,                   &
    fracaero_s(:,n),resft(:,n),rhokh_surft(:,n),tile_frac(:,n),                &
    timestep,r_gamma, ei_surft(:,n),fqw_1,ftl_1,fqw_surft(:,n),ftl_surft(:,n), &
    tstar_surft(:,n),snow_surft(:,n),snowdep_surft(:,n),                       &
    melt_surft(:,n),snowinc_surft(:,n)                                         &
    )

  !-----------------------------------------------------------------------
  ! thermodynamic, flux contribution of melting ice on the FLake lake tile
  !-----------------------------------------------------------------------
  IF (     (l_flake_model   )                                                  &
    .AND. ( .NOT. l_aggregate)                                                 &
    .AND. (n == lake       ) ) THEN

    ! lake_h_ice_gb is only initialised if FLake is on.

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(l)                                                               &
!$OMP SHARED(land_pts,lake_ice_mass,lake_h_ice_gb)
    DO l = 1, land_pts
      lake_ice_mass(l) = lake_h_ice_gb(l) * rho_ice
    END DO
!$OMP END PARALLEL DO

    CALL sf_melt (                                                             &
      land_pts,land_index,                                                     &
      surft_index(:,n),surft_pts(n),flandg,                                    &
      alpha1(:,n),ashtf_prime_surft(:,n),dtrdz_charney_grid_1,                 &
      fracaero_s(:,n),resft(:,n),rhokh_surft(:,n),tile_frac(:,n),              &
      timestep,r_gamma,                                                        &
      ei_surft(:,n),fqw_1,ftl_1,fqw_surft(:,n),ftl_surft(:,n),                 &
      tstar_surft(:,n),lake_ice_mass,lake_ice_mass / rho_snow_const,           &
      melt_ice_surft(:,n),snowinc_flake(:,n)                                   &
        )
  END IF

  !-----------------------------------------------------------------------
  !  Increment snow by sublimation and melt
  !-----------------------------------------------------------------------

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(k,l,j,i)                                                         &
!$OMP SHARED(surft_pts,surft_index,land_index,t_i_length,ei_land,tile_frac,    &
!$OMP        ei_surft,snowmelt,melt_surft,n)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    ei_land(i,j) = ei_land(i,j) + tile_frac(l,n) * ei_surft(l,n)
    snowmelt(i,j) = snowmelt(i,j) +                                            &
                    tile_frac(l,n) * melt_surft(l,n)
  END DO
!$OMP END PARALLEL DO

END DO

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(l,n,j,i,k)

IF (sf_diag%smlt) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%snomlt_surf_htf(i,j) = lf * snowmelt(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    surf_ht_flux_land(i,j) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT

IF (     (l_flake_model   )                                                    &
    .AND. ( .NOT. l_aggregate) ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      surf_ht_flux_lake_ij(i,j) = 0.0
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length
  tstar_land(i,j) = 0.0
END DO
!$OMP END DO NOWAIT

! initialise diagnostics to 0 to avoid packing problems
DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_pts
    radnet_surft(l,n) = 0.0
    le_surft(l,n) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

IF (sf_diag%l_lw_surft) THEN
  DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      sf_diag%lw_up_surft(l,n) = 0.0
      sf_diag%lw_down_surft(l,n) = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END IF

!$OMP BARRIER

IF (l_skyview) THEN
  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      j=(land_index(l) - 1) / tdims%i_end + 1
      i = land_index(l) - (j-1) * tdims%i_end
      radnet_surft(l,n) = sw_surft(l,n) +   emis_surft(l,n) *                  &
        sky(i,j) * ( lw_down(i,j) + lw_down_elevcorr_surft(l,n)                &
                                - sbcon * tstar_surft(l,n)**4 )
    END DO
!$OMP END DO
  END DO
  IF (sf_diag%l_lw_surft) THEN
    DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        l = surft_index(k,n)
        j=(land_index(l) - 1) / tdims%i_end + 1
        i = land_index(l) - (j-1) * tdims%i_end
        sf_diag%lw_up_surft(l,n)   = emis_surft(l,n) * sky(i,j) *              &
                                     sbcon * tstar_surft(l,n)**4               &
                                   + (1.0 - emis_surft(l,n)) *                 &
                                     sky(i,j) * (lw_down(i,j) +                &
                                     lw_down_elevcorr_surft(l,n))
        sf_diag%lw_down_surft(l,n) = sky(i,j) * (lw_down(i,j) +                &
                                     lw_down_elevcorr_surft(l,n))
      END DO
!$OMP END DO
    END DO
  END IF
ELSE
  DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,surft_pts(n)
      l = surft_index(k,n)
      j=(land_index(l) - 1) / tdims%i_end + 1
      i = land_index(l) - (j-1) * tdims%i_end
      radnet_surft(l,n) = sw_surft(l,n) +   emis_surft(l,n) *                  &
                 ( lw_down(i,j) + lw_down_elevcorr_surft(l,n)                  &
                                - sbcon * tstar_surft(l,n)**4 )
    END DO
!$OMP END DO
  END DO
  IF (sf_diag%l_lw_surft) THEN
    DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        l = surft_index(k,n)
        j=(land_index(l) - 1) / tdims%i_end + 1
        i = land_index(l) - (j-1) * tdims%i_end
        sf_diag%lw_up_surft(l,n)   = emis_surft(l,n) * sbcon *                 &
                                     tstar_surft(l,n)**4                       &
                                   + (1.0 - emis_surft(l,n)) *                 &
                                     (lw_down(i,j) +                           &
                                     lw_down_elevcorr_surft(l,n))
        sf_diag%lw_down_surft(l,n) = lw_down(i,j) +                            &
                                     lw_down_elevcorr_surft(l,n)
      END DO
!$OMP END DO
    END DO
  END IF
END IF

! Get lake evaporation based on method of conserving water in lakes
IF ( (l_rivers .AND. i_river_vn == rivers_um_trip) .OR. using_lfric ) THEN
  SELECT CASE (lake_water_conserve_method)
    !
  CASE ( use_fqw_surft )
    ! This method gets lake evaporation from fqw_surft which already has
    ! sublimation removed from it in sf_evap. This needs to be done
    ! before fqw_surft gets reset from its component parts in the next section
    ! of code.

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      lake_evap(l) = fqw_surft(l,lake)
    END DO
!$OMP END DO

  CASE ( use_elake_surft )
    ! This method gets lake evaporation from elake_surft. This is more
    ! representative of evaporation from the lakes that the atmosphere sees.

!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      lake_evap(l) = elake_surft(l,lake)
    END DO
!$OMP END DO

  CASE DEFAULT
    errorstatus = 101
    CALL ereport(RoutineName, errorstatus, 'Invalid value for lake_water_conserve_method')
  END SELECT
END IF

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    canhc_surf(l) = canhc_surft(l,n)
    IF ( ( .NOT. cansnowtile(n)) .AND. l_snow_nocan_hc .AND.                   &
         (nsmax > 0) .AND. (nsnow_surft(l,n) > 0) ) canhc_surf(l) = 0.0
    fqw_surft(l,n) = ecan_surft(l,n) + esoil_surft(l,n) +                      &
                     elake_surft(l,n) + ei_surft(l,n)
    le_surft(l,n) = lc * ecan_surft(l,n) + lc * esoil_surft(l,n) +             &
                   lc * elake_surft(l,n) + ls * ei_surft(l,n)
    surf_ht_store_surft(l,n) = (canhc_surf(l) / timestep) *                    &
                         (tstar_surft(l,n) - tstar_surft_old(l,n))
    surf_htf_surft(l,n) = radnet_surft(l,n) + anthrop_heat_surft(l,n) -        &
                        ftl_surft(l,n) -                                       &
                        le_surft(l,n) -                                        &
                        lf * (melt_surft(l,n) + melt_ice_surft(l,n)) -         &
                        surf_ht_store_surft(l,n)
    ! separate out the lake heat flux for FLake
    ! and replace the snow-melt (NSMAX=0 only) and ice-melt heat fluxes
    ! so Flake can do its melting
    IF (     (l_flake_model   )                                                &
        .AND. ( .NOT. l_aggregate)                                             &
        .AND. (n == lake       ) ) THEN
      IF (nsmax == 0) THEN
        surf_ht_flux_lake_ij(i,j) = surf_htf_surft(l,n)                        &
                      + lf * (melt_surft(l,n) + melt_ice_surft(l,n))
      ELSE
        surf_ht_flux_lake_ij(i,j) = surf_htf_surft(l,n)                        &
                      + lf * melt_ice_surft(l,n)
      END IF
    ELSE
      surf_ht_flux_land(i,j) = surf_ht_flux_land(i,j)                          &
                        + tile_frac(l,n) * surf_htf_surft(l,n)
    END IF
    tstar_land(i,j) = tstar_land(i,j)                                          &
               + tile_frac(l,n) * tstar_surft(l,n)
  END DO
!$OMP END DO
END DO

  ! normalise the non-lake surface heat flux
IF ( l_flake_model .AND. ( .NOT. l_aggregate) ) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    ! be careful about gridboxes that are all lake
    IF (non_lake_frac(l) > EPSILON(0.0)) THEN
      surf_ht_flux_land(i,j) = surf_ht_flux_land(i,j) / non_lake_frac(l)
    END IF
  END DO
!$OMP END DO
END IF

IF (sf_diag%l_lh_land) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    sf_diag%lh_land(l) = SUM((tile_frac(l,:) * le_surft(l,:)))
  END DO
!$OMP END DO
END IF

!-----------------------------------------------------------------------
! Optional error check : test for negative surface temperature
!-----------------------------------------------------------------------
IF (l_neg_tstar) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    IF (tstar_land(i,j) < 0) THEN
      ERROR = 1
      WRITE(jules_message,*)                                                   &
           '*** ERROR DETECTED BY ROUTINE JULES_LAND_SF_IMPLICIT ***'
      CALL jules_print('jules_land_sf_implicit_jls',jules_message)
      WRITE(jules_message,*) 'NEGATIVE SURFACE TEMPERATURE AT LAND POINT ',l
      CALL jules_print('jules_land_sf_implicit_jls',jules_message)
    END IF
  END DO
!$OMP END DO
END IF

!$OMP END PARALLEL

!--------------------------------------------------------------------------
! Update water tracers for implicit calculation
!--------------------------------------------------------------------------

IF (l_wtrac_imp_jls) THEN
  DO i_wt = 1, n_wtrac_jls
    CALL sf_land_imp_wtrac(                                                    &
      land_pts, nsurft, sm_levels, n_evap_srce,                                &
      surft_pts, land_index, surft_index, timestep,                            &
      standard_ratio_wtrac(i_wt), flandg, tile_frac, non_lake_frac,            &
      snow_surft_wtrac(:,:,i_wt), smc_soilt_wtrac(:,:,i_wt),                   &
      canopy_wtrac(:,:,i_wt), wt_ext_surft,                                    &
      ei_surft, esoil_surft, ecan_surft, elake_surft,                          &
      fqw_evapsrce_wtrac(:,:,:,i_wt), fqw_evapsrce_wtrac(:,:,:,1),             &
      fqw_surft_wtrac(:,:,i_wt), ei_wtrac(:,:,i_wt),                           &
      ei_surft_wtrac(:,:,i_wt), esoil_soilt_wtrac(:,:,:,i_wt),                 &
      esoil_surft_wtrac(:,:,i_wt), ext_soilt_wtrac(:,:,:,i_wt),                &
      ecan_wtrac(:,:,i_wt), ecan_surft_wtrac(:,:,i_wt),                        &
      elake_surft_wtrac(:,:,i_wt), fqw_1_wtrac(:,:,i_wt),                      &
      dfqw_wtrac(:,:,i_wt))

    ! Set lake evaporation which is used in river_routing for conservation
    ! purposes.
    ! Water tracers always used lake_water_conserve_method=use_elake_surft

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP PRIVATE(l) SHARED(i_wt,land_pts,lake_evap_wtrac,lake,elake_surft_wtrac)
    DO l = 1, land_pts
      lake_evap_wtrac(l,i_wt) = elake_surft_wtrac(l,lake,i_wt)
    END DO
!$OMP END PARALLEL DO

  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE jules_land_sf_implicit
END MODULE jules_land_sf_implicit_mod
