#if defined(UM_JULES)
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

MODULE deposition_from_ukca_chemistry_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName=                            &
  'DEPOSITION_FROM_UKCA_CHEMISTRY_MOD'

CONTAINS

SUBROUTINE deposition_from_ukca_chemistry(                                     &
             secs_per_step, bl_levels, row_length, rows, ntype, npft,          &
             jpspec_ukca, ndepd_ukca, nldepd_ukca, speci_ukca,                 &
             land_points, land_index, tile_pts, tile_index,                    &
             seaice_frac, fland, sinlat,                                       &
             p_surf, rh, t_surf, surf_hf, surf_wetness,                        &
             z0tile_lp, stcon, laift_lp, canhtft_lp, t0tile_lp,                &
             soilmc_lp, zbl, dzl, frac_types, u_s,                             &
             dep_loss_rate_ij, nlev_with_ddep, len_stashwork50, stashwork50)

USE ancil_info, ONLY: nsurft, dim_cs1

USE deposition_check_species_mod, ONLY: deposition_check_species

USE deposition_jules_ddepctl_mod, ONLY: deposition_jules_ddepctl

USE deposition_ukca_var_mod, ONLY: deposition_ukca_var_alloc,                  &
                                   jpspec_jules, ndepd_jules,                  &
                                   nldepd_jules, speci_jules

! Module diagnostics_dep has STASH calls specific to the UM
! Do not include in LFRic code as not relevant nor workable
#if !defined (LFRIC)
USE diagnostics_dep_mod, ONLY: diagnostics_dep
#endif

USE jules_deposition_mod, ONLY: ndry_dep_species

USE um_types, ONLY: real_jlslsm
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

! Dimensions
INTEGER, INTENT(IN) ::                                                         &
  row_length,                                                                  &
   ! size of UKCA x dimension
  rows,                                                                        &
   ! size of UKCA y dimension
  bl_levels,                                                                   &
   ! no. of atmospheric boundary layer levels
  ntype,                                                                       &
   ! no. of surface types
  npft,                                                                        &
   ! no. of plant functional types
  jpspec_ukca,                                                                 &
   ! no. of chemical species in the UKCA mechanism
  ndepd_ukca,                                                                  &
   ! no. of chemical species that are deposited
   ! Equal to and used interchangeably with jpdd
  land_points

INTEGER, INTENT(IN) ::                                                         &
  land_index(land_points),                                                     &
  tile_pts(ntype),                                                             &
  tile_index(land_points,ntype),                                               &
  nldepd_ukca(jpspec_ukca)
    ! Holds array elements of speci, identifying those chemical species
    ! in the chemical mechanism that are deposited

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  secs_per_step,                                                               &
   ! time step (in s)
  sinlat(row_length, rows),                                                    &
   ! sin(latitude)
  p_surf(row_length, rows),                                                    &
   ! surface pressure
  dzl(row_length, rows, bl_levels),                                            &
   ! separation of boundary-layer levels
  u_s(row_length, rows),                                                       &
   ! surface friction velocity (m s-1)
  t_surf(row_length, rows),                                                    &
   ! surface temperature
  rh(row_length, rows),                                                        &
   ! relative humidity (-)
  surf_wetness(row_length, rows),                                              &
   ! surface wetness
  frac_types(land_points,ntype),                                               &
   ! surface tile fractions (-)
  zbl(row_length,rows),                                                        &
   ! boundary-layer height (m)
  surf_hf(row_length,rows),                                                    &
   ! sensible heat flux (W m-2)
  seaice_frac(row_length,rows),                                                &
   ! grid-cell sea ice fraction
  stcon(row_length,rows,npft),                                                 &
   ! stomatal conductance (s m-1)
  soilmc_lp(land_points),                                                      &
   ! soil moisture
  fland(land_points),                                                          &
   ! grid-cell land fraction
  laift_lp(land_points,npft),                                                  &
   ! leaf area index (m2 m-2)
  canhtft_lp(land_points,npft),                                                &
   ! canopy height (m)
  z0tile_lp(land_points,ntype),                                                &
   ! roughness length for heat and moisture (m) by surface type
  t0tile_lp(land_points,ntype)
   ! surface temperature (K) by surface type

CHARACTER(LEN=10), INTENT(IN) :: speci_ukca(jpspec_ukca)
                                   ! Names of all the chemical species
                                   ! in the UKCA chemical mechanism

INTEGER, INTENT(IN)  :: len_stashwork50
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  stashwork50 (len_stashwork50)
    ! Diagnostics array (UKCA stashwork50)

! Output variables
INTEGER, INTENT(OUT) ::                                                        &
  nlev_with_ddep(row_length, rows)
    ! Number of levs with deposition in boundary layer

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dep_loss_rate_ij(row_length, rows, ndepd_ukca)
    ! dry deposition loss rate (s-1)

! Local variables
INTEGER  :: i, j, l, m, n

! JULES and Atmospheric Deposition
! Dummy arrays for compatibility with JULES standalone version
INTEGER, SAVE          ::                                                      &
  a_step = 0
    ! Atmospheric timestep number

REAL(KIND=real_jlslsm) ::                                                      &
  stcon_lp(land_points, npft),                                                 &
   ! stomatal conductance (s m-1) on land vector
  sw_surft(row_length, rows),                                                  &
    ! downward short wave radiation (W m-2)
  canopy_surft(row_length, rows),                                              &
    ! canopy water content
  dep_surfconc_ij(row_length, rows, ndry_dep_species),                         &
    ! surface concentration of deposited species
  dep_flux_ij(row_length, rows, ndry_dep_species),                             &
    ! deposition flux (kg m-3 s-1) for trace gases
  dep_ra_ij(row_length, rows, ntype),                                          &
    ! aerodynamic resistance, Ra (s m-1)
  dep_rb_ij(row_length, rows, ndry_dep_species),                               &
    ! quasi-laminar resistance, Rb (s m-1)
  dep_rc_ij(row_length, rows, ntype, ndry_dep_species),                        &
    ! surface resistance, Rc (s m-1)
  dep_rc_stom_ij(row_length, rows, ntype, ndry_dep_species),                   &
    ! surface resistance, Rc (s m-1)
    ! - stomatal component
  dep_rc_nonstom_ij(row_length, rows, ntype, ndry_dep_species),                &
    ! surface resistance, Rc (s m-1)
    ! - non-stomatal component
  dep_vd_ij(row_length, rows, ntype, ndry_dep_species),                        &
    ! surface deposition velocity term (m s-1)
  dep_vd_gb(row_length, rows, ndry_dep_species)
    ! surface deposition velocity term (m s-1) on gridbox

! For H2 deposition: Paulot et al. scheme
REAL(KIND=real_jlslsm) ::                                                      &
  vol_smc_sat_dep(land_points),                                                &
    ! volumetric saturation point (m3 water / m3 of soil)
    ! for Paulot H2 deposition scheme
  soil_carbon_dep(land_points,dim_cs1),                                        &
    ! soil carbon for Paulot H2 deposition scheme
  deep_soil_temp_dep(land_points),                                             &
    ! deep soil temperature for Paulot H2 deposition scheme
  snow_depth(land_points,nsurft),                                              &
    ! snow depth on ground on tiles (m)
    ! for Paulot H2 deposition scheme
  soil_sand(land_points)
    ! fraction of soil that is sand

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER   :: RoutineName=                                  &
  'DEPOSITION_FROM_UKCA_CHEMISTRY'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF ( .NOT. ALLOCATED(speci_jules) .AND. .NOT. ALLOCATED(nldepd_jules) ) THEN

  ! For dry_dep_model = dry_dep_model_ukca, the species-dependent deposition
  ! parameters are hardwired in the deposition routines. Only the species name
  ! is used, primarily for JULES standalone application.

  ! Create JULES-based equivalents of the UKCA parameters & variables:
  ! jpspec(_ukca), ndepd(_ukca), nldepd(_ukca) and speci(_ukca)

  CALL deposition_ukca_var_alloc(jpspec_ukca)
  jpspec_jules = jpspec_ukca
  ndepd_jules  = ndepd_ukca
  nldepd_jules(:) = nldepd_ukca(:)
  speci_jules(:)  = speci_ukca(:)

  ! Check that the order of species in the deposition_species namelists matches
  ! the order of deposited species in the UKCA. The array nldepd holds the
  ! indices of the chemical species in the UKCA chemical mechanism that are
  ! deposited.
  ! NOTE: nldepd is not set until the first UKCA timestep

  CALL deposition_check_species(jpspec_jules, ndepd_jules, nldepd_jules,       &
    speci_jules)

END IF

! Initialise variables not passed through subroutine call
a_step = a_step+1
sw_surft(:,:) = 0.0
canopy_surft(:,:) = 0.0
dep_surfconc_ij(:,:,:) = 0.0
dep_flux_ij(:,:,:) = 0.0
dep_loss_rate_ij(:,:,:) = 0.0
dep_ra_ij(:,:,:) = 0.0
dep_rb_ij(:,:,:) = 0.0
dep_rc_ij(:,:,:,:) = 0.0
dep_rc_stom_ij(:,:,:,:) = 0.0
dep_rc_nonstom_ij(:,:,:,:) = 0.0
dep_vd_ij(:,:,:,:) = 0.0
dep_vd_gb(:,:,:) = 0.0
! For H2 deposition scheme
! Placeholder: will eventually be passed through subroutine interface
soil_sand(:) = 0.0
vol_smc_sat_dep(:) = 0.0
soil_carbon_dep(:,:) = 0.0
deep_soil_temp_dep(:) = 0.0
snow_depth(:,:) = 0.0

! Convert stomatal conductance from grid to land vector
DO m = 1, npft
  DO n = 1, tile_pts(m)
    l             = tile_index(n,m)
    j             = (land_index(l)-1)/row_length + 1
    i             = land_index(l) - (j-1)*row_length
    stcon_lp(l,m) = stcon(i,j,m)
  END DO
END DO

CALL deposition_jules_ddepctl(a_step, secs_per_step,                           &
  bl_levels, row_length, rows,                                                 &
  land_points, land_index, tile_pts, tile_index,                               &
  seaice_frac, fland, sinlat,                                                  &
  p_surf, rh, t_surf, surf_hf,                                                 &
  z0tile_lp, sw_surft, surf_wetness, stcon_lp, laift_lp, canhtft_lp, t0tile_lp,&
  canopy_surft, soilmc_lp, zbl, dzl, frac_types, u_s,                          &
  dep_surfconc_ij,                                                             &
    ! Parameters for Paulot et al H2 deposition scheme
  nsurft, dim_cs1,                                                             &
  vol_smc_sat_dep, snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand, &
    ! Output deposition parameters
  dep_ra_ij, dep_rb_ij, dep_rc_ij, dep_rc_stom_ij, dep_rc_nonstom_ij,          &
  dep_vd_ij, dep_loss_rate_ij, dep_flux_ij, nlev_with_ddep, dep_vd_gb          &
  )

! Calculate STASH deposition diagnostics
! Do not include in LFRic code as not relevant nor workable
#if !defined (LFRIC)
CALL diagnostics_dep( row_length, rows, bl_levels,                             &
  dep_vd_ij, dep_vd_gb, dep_loss_rate_ij, stashwork50 )
#endif

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE deposition_from_ukca_chemistry

END MODULE deposition_from_ukca_chemistry_mod
#endif
