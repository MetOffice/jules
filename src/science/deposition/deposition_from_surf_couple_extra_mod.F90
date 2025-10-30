!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE deposition_from_surf_couple_extra_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName=                            &
  'DEPOSITION_FROM_SURF_COUPLE_EXTRA_MOD'

CONTAINS

SUBROUTINE deposition_from_surf_couple_extra(                                  &
  a_step, secs_per_step, row_length, rows,                                     &
  land_pts, land_index, surft_pts, surft_index, ice_fract_ij, fland,           &
  pstar_ij, qw_1_ij, tstar_ij, sw_surft, z0h_surft,                            &
  gc_surft, lai_pft, canht_pft, tstar_surft, canopy_surft, smc_soilt,          &
  zh, dzl, frac_types,                                                         &
  chemvars, nsurft, nsoilt, sm_levels, dim_cslayer, dim_cs1,                   &
  vol_smc_sat, snow_depth, soil_carbon, deep_soil_temp )

USE nlsizes_namelist_mod,    ONLY: bl_levels

USE deposition_jules_ddepctl_mod, ONLY: deposition_jules_ddepctl

USE deposition_species_mod,  ONLY: dep_species_name

USE deposition_ukca_var_mod, ONLY: deposition_ukca_var_alloc,                  &
                                   jpspec_jules, ndepd_jules,                  &
                                   nldepd_jules, speci_jules

USE jules_chemvars_mod,      ONLY: chemvars_type

USE jules_deposition_mod,    ONLY: l_deposition, l_deposition_gc_corr,         &
                                   ndry_dep_species, dep_h2_soil_scheme,       &
                                   dep_h2_scheme_conrad, dep_h2_scheme_paulot

USE jules_surface_types_mod, ONLY: ntype, npft

! Used for relative humidity calculation
USE qsat_mod,                ONLY: qsat_wat_mix

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

! Dimensions
INTEGER, INTENT(IN) ::                                                         &
  a_step,                                                                      &
    ! Atmospheric timestep number.
  land_pts,                                                                    &
    ! Number of land points
  nsurft,                                                                      &
    ! Number of surface tiles
  sm_levels,                                                                   &
    ! Number of soil layers
  nsoilt,                                                                      &
    ! Number of soil tiles
  dim_cs1,                                                                     &
    ! Number of pools for soil carbon (cs)
  dim_cslayer,                                                                 &
    ! Number of depth dimensions for soil carbon (cs)
  row_length,                                                                  &
    ! Number of points on a row
  rows
    ! Number of rows

! Input arguments on land points
INTEGER, INTENT(IN) ::                                                         &
  land_index(land_pts),                                                        &
    ! index of land points
  surft_pts(ntype),                                                            &
    ! Number of land points which include the nth surface type
  surft_index(land_pts, ntype)
    ! indices of land points which include the nth surface type

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  secs_per_step,                                                               &
   ! time step (in s)
  ice_fract_ij(row_length,rows),                                               &
   ! grid-cell sea ice fraction
  pstar_ij(row_length, rows),                                                  &
   ! surface pressure (Pa)
  qw_1_ij(row_length, rows),                                                   &
   ! surface water vapour mixing ratio (kg kg-1)
  tstar_ij(row_length, rows),                                                  &
   ! surface temperature (oC)
  dzl(row_length, rows, bl_levels),                                            &
   ! separation of boundary-layer levels (m)
  zh(row_length,rows),                                                         &
   ! boundary-layer height (m)
  fland(land_pts),                                                             &
   ! grid-cell land fraction (-)
  sw_surft(land_pts, ntype),                                                   &
    ! Net SW downward radiation (W m-2)
  z0h_surft(land_pts,ntype),                                                   &
   ! roughness length for heat and moisture (m) by surface type
  gc_surft(land_pts,npft),                                                     &
   ! stomatal conductance (s m-1)
  lai_pft(land_pts,npft),                                                      &
   ! leaf area index (m2 m-2)
  canht_pft(land_pts,npft),                                                    &
   ! canopy height (m)
  tstar_surft(land_pts,ntype),                                                 &
   ! surface temperature (K) by surface type
  canopy_surft(land_pts, npft),                                                &
    ! Canopy water content
  smc_soilt(land_pts),                                                         &
    ! soil moisture
  vol_smc_sat(land_pts,nsoilt,sm_levels),                                      &
    ! volumetric saturation point (m3 water / m3 of soil)
    ! for Paulot H2 deposition scheme
    ! equivalent to psparms%smvcst_soilt
  snow_depth(land_pts,nsurft),                                                 &
    ! snow depth on ground on tiles (m)
    ! for Paulot H2 deposition scheme
    ! equivalent to progs%snowdepth_surft
  soil_carbon(land_pts,nsoilt,dim_cslayer,dim_cs1),                            &
    ! soil moisture
    ! for Paulot H2 deposition scheme
    ! equivalent to progs%cs_pool_soilt
  deep_soil_temp(land_pts,nsoilt,sm_levels),                                   &
    ! soil moisture
    ! for Paulot H2 deposition scheme
    ! equivalent to progs%t_soil_soilt
  frac_types(land_pts,ntype)
   ! surface tile fractions (-)

TYPE(chemvars_type), INTENT(IN OUT) :: chemvars

! Local variables
INTEGER  :: i, j, l, m, n

REAL(KIND=real_jlslsm) ::                                                      &
  stom_con_pft(land_pts, npft),                                                &
    ! Stomatal conductance (m s-1)
  qsat_ij(row_length, rows),                                                   &
   ! Surface saturated water vapour mixing ratio (kg kg-1)
  rh_ij(row_length, rows),                                                     &
   ! Relative humidity
  surf_wetness_ij(row_length, rows),                                           &
   ! surface wetness
  vol_smc_sat_dep(land_pts),                                                   &
    ! volumetric saturation point (m3 water / m3 of soil)
    ! for Paulot H2 deposition scheme
    ! Reduce # of dimensions: nsoilt = 1 and using top soil layer
  soil_carbon_dep(land_pts,dim_cs1),                                           &
    ! soil carbon for Paulot H2 deposition scheme
    ! Reduce # of dimensions: nsoilt = 1 and using top soil layer
  deep_soil_temp_dep(land_pts),                                                &
    ! deep soil temperature for Paulot H2 deposition scheme
    ! Reduce # of dimensions: nsoilt = 1 and using top soil layer
  soil_sand(land_pts)
   ! fraction of soil that is sand - fixed at 0.3

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER   :: RoutineName=                                  &
  'DEPOSITION_FROM_SURF_COUPLE_EXTRA'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( .NOT. ALLOCATED(speci_jules) .AND. .NOT. ALLOCATED(nldepd_jules) ) THEN

  ! For dry_dep_model = dry_dep_model_ukca, the species-dependent deposition
  ! parameters are hardwired in the deposition routines. Only the species name
  ! is used, primarily for JULES standalone application.

  CALL deposition_ukca_var_alloc(ndry_dep_species)
  jpspec_jules = ndry_dep_species
  ndepd_jules  = ndry_dep_species
  speci_jules(:)  = dep_species_name(:)

  DO i = 1,ndry_dep_species
    nldepd_jules(i) = i
  END DO

  ! For UM-coupled JULES applications with l_deposition_from_ukca = TRUE,
  ! we need to check that the order of species in the jules_deposition_species
  ! namelists matches the order of deposited species in the UKCA.
  ! The UKCA array nldepd holds the indices of the chemical species
  ! in the UKCA chemical mechanism that are deposited.
  ! NOTE: nldepd is not set until the first UKCA timestep

  ! This check is not needed for JULES standalone

END IF

! Initialise variables to match call to JULES-based deposition routines
! from the UKCA routines: ukca_chemistry_ctl, ukca_chemistry_ctl_BE, or
! ukca_chemistry_ctl_col
surf_wetness_ij(:,:) = 1.0

! Initialise variables used in the Paulot et al H2 deposition scheme
SELECT CASE( dep_h2_soil_scheme )
CASE ( dep_h2_scheme_paulot )
  vol_smc_sat_dep(1:land_pts) = vol_smc_sat(1:land_pts,1,1)
  soil_carbon_dep(1:land_pts,dim_cs1) = soil_carbon(1:land_pts,1,1,dim_cs1)
  deep_soil_temp_dep(1:land_pts) = deep_soil_temp(1:land_pts,1,1)
  ! Fixed value for sand fraction of soil as soil composition not available
  ! in standalone JULES
  soil_sand(:) = 0.3
CASE DEFAULT
  vol_smc_sat_dep(1:land_pts) = 0.0
  soil_carbon_dep(1:land_pts,dim_cs1) = 0.0
  deep_soil_temp_dep(1:land_pts) = 0.0
  soil_sand(:) = 0.0
END SELECT

! Initialise deposition diagnostics
chemvars%dep_flux_ij(:,:,:) = 0.0
chemvars%dep_loss_rate_ij(:,:,:) = 0.0
chemvars%dep_ra_ij(:,:,:) = 0.0
chemvars%dep_rb_ij(:,:,:) = 0.0
chemvars%dep_rc_ij(:,:,:,:) = 0.0
chemvars%dep_rc_stom_ij(:,:,:,:) = 0.0
chemvars%dep_rc_nonstom_ij(:,:,:,:) = 0.0
chemvars%dep_vd_ij(:,:,:,:) = 0.0
chemvars%dep_vd_gb(:,:,:)   = 0.0
chemvars%nlev_with_ddep = 0

! Federico Centoni identified that 'standard' stomatal conductance
! term includes bare soil evaporation component

IF (l_deposition_gc_corr) THEN
  ! Corrected for bare soil evaporation
  DO m = 1, npft
    DO l = 1, land_pts
      stom_con_pft(l,m) = chemvars%gc_corr(l,m)
    END DO
  END DO
ELSE
  ! As used in UKCA
  DO m = 1, npft
    DO l = 1, land_pts
      stom_con_pft(l,m) = gc_surft(l,m)
    END DO
  END DO
END IF

! Calculate RH from ratio of qw to qsat
DO j = 1, rows
  CALL qsat_wat_mix(qsat_ij(:,j), tstar_ij(:,j), pstar_ij(:,j), row_length)
  rh_ij(:,j) = qw_1_ij(:,j)/qsat_ij(:,j)
END DO

DO j = 1, rows
  DO i = 1, row_length
    ! remove negatives
    IF (rh_ij(i,j) < 0.0) THEN
      rh_ij(i,j) = 0.0
    END IF
    ! remove values >= 1.0
    IF (rh_ij(i,j) > 1.0) THEN
      rh_ij(i,j) = 0.999
    END IF
  END DO
END DO

CALL deposition_jules_ddepctl(a_step, secs_per_step,                           &
  bl_levels, row_length, rows, land_pts, land_index, surft_pts, surft_index,   &
  ice_fract_ij, fland, chemvars%dep_sinlat_ij,                                 &
  pstar_ij, rh_ij, tstar_ij, chemvars%dep_ftl_1_ij,                            &
  z0h_surft, sw_surft, surf_wetness_ij, stom_con_pft,                          &
  lai_pft, canht_pft, tstar_surft, canopy_surft, smc_soilt,                    &
  zh, dzl, frac_types, chemvars%dep_ustar_ij, chemvars%dep_surfconc_ij,        &
    ! Parameters for Paulot et al H2 deposition scheme
  nsurft, dim_cs1,                                                             &
  vol_smc_sat_dep, snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand, &
    ! Output deposition parameters
  chemvars%dep_ra_ij, chemvars%dep_rb_ij, chemvars%dep_rc_ij,                  &
  chemvars%dep_rc_stom_ij, chemvars%dep_rc_nonstom_ij,                         &
  chemvars%dep_vd_ij, chemvars%dep_loss_rate_ij, chemvars%dep_flux_ij,         &
  chemvars%nlev_with_ddep, chemvars%dep_vd_gb                                  &
  )

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE deposition_from_surf_couple_extra

END MODULE deposition_from_surf_couple_extra_mod
