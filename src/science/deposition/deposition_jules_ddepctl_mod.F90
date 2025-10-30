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

MODULE deposition_jules_ddepctl_mod

!-----------------------------------------------------------------------------
! Description:
!   Deposition control module
!   Based on UKCA routine
!     ukca/src/science/chemistry/deposition/ukca_ddepctl.F90
!   Returns deposition parameters, loss rates & fluxes
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod,        ONLY: rmdi, imdi
USE ereport_mod,             ONLY: ereport
USE jules_print_mgr,         ONLY: jules_print
USE um_types,                ONLY: real_jlslsm
USE parkind1,                ONLY: jprb, jpim
USE yomhook,                 ONLY: lhook, dr_hook

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = "DEPOSITION_JULES_DDEPCTL_MOD"

CONTAINS

SUBROUTINE deposition_jules_ddepctl(                                           &
       a_step, timestep,                                                       &
         ! from nlsizes_namelist_mod
       bl_levels,                                                              &
         ! from ancil_info
       row_length, rows, land_pts, land_index, surft_pts, surft_index,         &
       ice_fract_ij,                                                           &
         ! from coastal
       fland,                                                                  &
         ! sin of latitude
       sinlat_ij,                                                              &
         ! Driving meteorological parameters passed via subrountine call
       pstar_ij, rh_ij,                                                        &
         ! from fluxes
       tstar_ij, surf_ht_flux_ij, z0h_surft, sw_surft, surf_wetness_ij,        &
         ! from prognostics
       stom_con_pft, lai_pft, canht_pft, tstar_surft, canopy_surft, smc_soilt, &
         ! from boundary_layer_var_mod
       zh, dzl,                                                                &
         ! local
       tile_frac, ustar_ij,                                                    &
         ! Corrected stomatal conductance
       dep_surfconc_ij,                                                        &
         ! Parameters for Paulot et al H2 deposition scheme
       nsurft, dim_cs1, vol_smc_sat_dep,                                       &
       snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand,             &
         ! Output deposition parameters
       dep_ra_ij, dep_rb_ij, dep_rc_ij, dep_rc_stom_ij, dep_rc_nonstom_ij,     &
       dep_vd_ij, dep_loss_rate_ij, dep_flux_ij, nlev_with_ddep, dep_vd_gb     &
     )

!-------------------------------------------------------------------------------
! Variable renamed to the equivalent used in JULES
! Variables with _ij are gridded
!
! UKCA variable                     JULES equivalent
! -------------                     ----------------
! row_length                        row_length                subroutine call
! rows                              rows                      subroutine call
! bl_levels                         bl_levels                 subroutine call
! land_points                       land_pts                  subroutine call
! land_index                        land_index                subroutine call
! tile_pts                          surft_pts                 subroutine call
! tile_index                        surft_index               subroutine call
! timestep (sec_per_step)           timestep                  subroutine call
! sinlat (sinlat_pos)               as latitude               subroutine call
! tile_frac                         tile_frac                 subroutine call
! t_surf                            tstar_ij                  subroutine call
! p_surf                            pstar_ij                  subroutine call
! dzl                               dzl                       subroutine call
! zbl                               zh                        subroutine call
! surf_hf                           surf_ht_flux_ij           subroutine call
! u_s                               ustar_ij                  subroutine call
! rh (rel_humid_frac)               rh_ij                     use qsat_wat
! seaice_frac                       ice_fract_ij              subroutine call
! stcon                             gc_surft/gc_corr          subroutine call
! soilmc_lp (soil_moisture_layer1)  smc_soilt                 subroutine call
! fland                             fland                     subroutine call
! laift_lp                          lai_pft                   subroutine call
! canhtft_lp                        canht_pft                 subroutine call
! z0tile_lp                         z0h_surft                 subroutine call
! t0tile_lp                         tstar_surft               subroutine call
! canwctile_lp                      canopy_surft              subroutine call
! nlev_with_ddep                    nlev_with_ddep            subroutine call
! zdryrt                            dep_loss_rate             subroutine call
!-------------------------------------------------------------------------------

USE jules_surface_types_mod, ONLY: ntype, npft, nnvg, lake, ice, elev_ice

USE jules_soil_mod,          ONLY: dzsoil

USE jules_deposition_mod,    ONLY: ndry_dep_species, dry_dep_model,            &
                                   dry_dep_model_ukca, dry_dep_model_jules,    &
                                   l_deposition_flux, tundra_s_limit

USE deposition_output_arrays_mod, ONLY:                                        &
                                   deposition_output_array_int,                &
                                   deposition_output_array_real

USE deposition_species_mod, ONLY: dep_species_name
USE deposition_jules_aerod_mod, ONLY: deposition_jules_aerod
USE deposition_jules_ddcalc_mod, ONLY: deposition_jules_ddcalc
USE deposition_jules_surfddr_mod, ONLY: deposition_jules_surfddr
USE deposition_ukca_surfddr_mod, ONLY: deposition_ukca_surfddr

! The following UKCA modules have been replaced:
! (1) Replace asad_mod with deposition_ukca_var_mod
! (2) ukca_config_specification_mod for ukca_config
!     Use jules_deposition_mod for UKCA deposition switches

USE water_constants_mod,     ONLY: tfs, rho_water

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  a_step,                                                                      &
    ! Atmospheric timestep number.
  land_pts,                                                                    &
    ! Number of land points
  nsurft,                                                                      &
    ! Number of surface tiles
  dim_cs1,                                                                     &
    ! Number of pools for soil carbon (cs)
  row_length,                                                                  &
    ! Number of points on a row
  rows,                                                                        &
    ! Number of rows
  bl_levels
    ! Number of atmospheric levels in boundary layer

! Input arguments on land points
INTEGER, INTENT(IN) ::                                                         &
  land_index(land_pts),                                                        &
    ! index of land points
  surft_pts(ntype),                                                            &
    ! Number of land points which include the nth surface type
  surft_index(land_pts, ntype)
    ! indices of land points which include the nth surface type

! Input arguments on land points
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  timestep,                                                                    &
    ! timestep length (s)
  tile_frac(land_pts, nsurft),                                                 &
    ! Tile fractions including snow cover in the ice tile.
  fland(land_pts),                                                             &
    ! Land fraction on land tiles
  lai_pft(land_pts, npft),                                                     &
    ! leaf area index (m2 m-2)
  canht_pft(land_pts, npft),                                                   &
    ! Canopy height (m)
  canopy_surft(land_pts, npft),                                                &
    ! Canopy water content
  smc_soilt(land_pts),                                                         &
    ! Soil moisture
  vol_smc_sat_dep(land_pts),                                                   &
    ! volumetric saturation point (m3 water / m3 of soil)
    ! for Paulot H2 deposition scheme
    ! equivalent to psparms%smvcst_soilt
  snow_depth(land_pts,nsurft),                                                 &
    ! snow depth on ground on tiles (m)
    ! for Paulot H2 deposition scheme
    ! equivalent to progs%snowdepth_surft
  soil_carbon_dep(land_pts,dim_cs1),                                           &
    ! soil moisture
    ! for Paulot H2 deposition scheme
    ! equivalent to progs%cs_pool_soilt
  deep_soil_temp_dep(land_pts),                                                &
    ! sub-surface temperature for surface layer (K)
    ! for Paulot H2 deposition scheme
    ! equivalent to progs%t_soil_soilt
  stom_con_pft(land_pts, npft),                                                &
    ! Stomatal conductance (m s-1)
  tstar_surft(land_pts, nsurft),                                               &
    ! Surface temperature by tile
  z0h_surft(land_pts, nsurft),                                                 &
    ! Tile roughness lengths for heat and moisture (m)
  sw_surft(land_pts, nsurft),                                                  &
    ! Net SW downward radiation (W m-2)
  soil_sand(land_pts)
   ! fraction of soil that is sand

! Input arguments on lat, lon grid
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sinlat_ij(row_length, rows),                                                 &
    ! latitudes of grid cells
  ice_fract_ij(row_length, rows),                                              &
    ! Tile ice fraction (-)
  tstar_ij(row_length, rows),                                                  &
    ! Surface temperature (K)
  pstar_ij(row_length, rows),                                                  &
    ! Surface pressure (Pa)
  rh_ij(row_length, rows),                                                     &
    ! Relative humidity (-)
  surf_wetness_ij(row_length, rows),                                           &
    ! Surface wetness index (-)
  surf_ht_flux_ij(row_length, rows),                                           &
    ! Surface heat flux pressure (W m-2)
  ustar_ij(row_length, rows),                                                  &
    ! Surface friction velocity (m s-1)
  zh(row_length, rows),                                                        &
    ! Height of planetary boundary layer (in m)
  dzl(row_length, rows, bl_levels),                                            &
    ! Separation of atmospheric layers (in m)
  dep_surfconc_ij(row_length, rows, ndry_dep_species)
    ! Concentration of deposited chemical species in the atmosphere,
    ! for the calculation of deposition, as mass mixing ratio (kg kg-1).

! Output arguments
INTEGER, INTENT(OUT) :: nlev_with_ddep(row_length,rows)
    ! Number of layers in boundary layer

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dep_ra_ij(row_length, rows, ntype),                                          &
    ! Aerodynamic resistance, Ra (s m-1)
  dep_rb_ij(row_length, rows, ndry_dep_species),                               &
    ! Quasi-laminar resistance, Rb (s m-1)
  dep_rc_ij(row_length, rows, ntype, ndry_dep_species),                        &
    ! Surface resistance, Rc (s m-1)
  dep_rc_stom_ij(row_length, rows, ntype, ndry_dep_species),                   &
    ! Surface resistance, Rc (s m-1): stomatal component
  dep_rc_nonstom_ij(row_length, rows, ntype, ndry_dep_species),                &
    ! Surface resistance, Rc (s m-1): non-stomatal component
  dep_vd_ij(row_length, rows, ntype, ndry_dep_species),                        &
    ! Surface deposition velocity term (m s-1)
  dep_vd_gb(row_length, rows, ndry_dep_species),                               &
    ! Surface deposition velocity term (m s-1) on gridboxes (for diagnostics)
  dep_loss_rate_ij(row_length, rows, ndry_dep_species),                        &
    ! First-order loss rate (s-1) for trace gases
  dep_flux_ij(row_length, rows, ndry_dep_species)
    ! Depostion flux (kg m-3 s-1) for trace gases

!-------------------------------------------------------------------------------
! Local variables

INTEGER :: i, j, k, l, m, n
INTEGER :: errcode
INTEGER :: ErrorStatus

INTEGER ::                                                                     &
  land_index_ij(row_length, rows)
    ! Identifies land points on grid and their index

! Variables used to add sea ice fraction to an elevated ice tile.
INTEGER :: i_elev_ice, n_elev_ice

REAL(KIND=real_jlslsm) ::                                                      &
  seafrac,                                                                     &
    ! Fraction of sea in grid square
  lftotal,                                                                     &
    ! Sum of land tile fractions
  lflocal
    ! Land fraction in grid square

REAL(KIND=real_jlslsm) ::                                                      &
  smc_land(land_pts),                                                          &
    ! Soil moisture content (Fraction by volume)
  gsf_ij(row_length, rows, ntype),                                             &
    ! Global surface fractions
  t0tile_ij(row_length, rows, ntype),                                          &
    ! Surface temperature on tiles (K)
  z0tile_ij(row_length, rows, ntype),                                          &
    ! Roughness length on tiles (m)
  o3_stom_frac_ij(row_length, rows),                                           &
    ! Fraction of O3 deposition through stomata
  dep_vd_so4_ij(row_length, rows)
    ! Surface deposition velocity term for sulphate particles

LOGICAL ::                                                                     &
  tundra_mask_ij(row_length, rows)
    ! identifies high-latitude grid cells as tundra

CHARACTER(LEN=*), PARAMETER    :: RoutineName='DEPOSITION_JULES_DDEPCTL'
CHARACTER(LEN=50000)           :: lineBuffer

INTEGER(KIND=jpim), PARAMETER  :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER  :: zhook_out = 1
REAL(KIND=jprb)                :: zhook_handle

!-------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise variables on grid to cover deposition to ocean and cryosphere
z0tile_ij(:,:,:)       =  0.0
t0tile_ij(:,:,:)       =  0.0
gsf_ij(:,:,:)          =  0.0
dep_vd_gb(:,:,:)       =  0.0

! Regrid, with type dimension
DO m = 1, ntype
  DO n = 1, surft_pts(m)
    l                        = surft_index(n,m)
    j                        = (land_index(l)-1)/row_length + 1
    i                        = land_index(l) - (j-1)*row_length
    ! Tile roughness lengths for heat and moisture (m)
    z0tile_ij(i,j,m)         = z0h_surft(l,m)
    ! surface temperature by type
    t0tile_ij(i,j,m)         = tstar_surft(l,m)
    ! tile fraction
    gsf_ij(i,j,m)            = tile_frac(l,m)
  END DO
END DO

! Calculate soil mositure content (fraction by volume)
! Only land points
DO l = 1, land_pts
  smc_land(l) = smc_soilt(l)/(dzsoil(1)*rho_water)
END DO

! Create a gridded land index variable (land_index_ij), which is used in
! deposition_jules_surfddr and deposition_ukca_surfddr routines to identify
! the land index on the 2D grid for land-based variables on land vector,
! such as stomatal conductance, lai, ..
! Missing data will be used to identify non-land points
! NOTE: land_index and surft_index do not have the required indices.

! Set all elements to missing data
DO j = 1, rows
  DO i = 1, row_length
    land_index_ij(i,j) = imdi
  END DO
END DO

! Assign land index for land points
DO l = 1, land_pts
  j           = (land_index(l)-1)/row_length + 1
  i           = land_index(l) - (j-1)*row_length
  land_index_ij(i,j) = l
END DO

! Need to create single gridded array of surface type fractions, including ocean/sea
! and sea ice as deposition also to ocean and cryosphere.
! Use JULES lake/water for sea and JULES ice for sea ice.

! The following block is taken from the UKCA routine:
! ukca/src/science/core/chemistry/deposition/ukca_ddepctl.F90, lines 185-211
! Used here but noting the non-optimal DO loop order
DO j = 1, rows
  DO i = 1, row_length

    l = land_index_ij(i,j)
    IF (l == imdi) THEN
      ! Not a land point - ocean/sea
      lflocal = 0.0
    ELSE
      ! Land point
      lflocal = fland(l)
    END IF

    ! Set land fraction (lflocal) to zero and sea fraction (seafrac) to one
    ! if grid cell is entirely open water
    ! Note: lake is surface type for water
    IF (gsf_ij(i,j,lake) == 1.0) THEN
      lflocal = 0.0
    END IF
    seafrac = 1.0 - lflocal

    ! Check for each 'land' grid square that the total land fraction
    ! still sums to one and adjust if needed.
    IF (lflocal < 1.0 .AND. gsf_ij(i,j,lake) < 1.0) THEN
      gsf_ij(i,j,lake)  = seafrac
      IF (lflocal > 0.0) THEN
        lftotal = 0.0
        ! Sum surface type fractions for the grid cell, ignoring water
        DO n = 1, ntype
          IF (n /= lake) THEN
            lftotal = lftotal + gsf_ij(i,j,n)
          END IF
        END DO

        ! Ensure total surface fraction is still one
        DO n = 1, ntype
          IF (n /= lake) THEN
            gsf_ij(i,j,n) = gsf_ij(i,j,n) * lflocal / lftotal
          END IF
        END DO
      END IF
    END IF

    ! Adjust open water fraction for sea ice
    IF (ice_fract_ij(i,j) > 0.0) THEN
      gsf_ij(i,j,lake)  = (1.0 - ice_fract_ij(i,j)) * seafrac

      ! Check that ice tile has some surface fraction before adding the
      ! sea ice fraction and potentially making a tile active which
      ! should be redundant.
      IF (gsf_ij(i,j,ice) > 0.0) THEN
        gsf_ij(i,j,ice) = gsf_ij(i,j,ice) + ice_fract_ij(i,j) * seafrac
      ELSE
        ! It may still be possible to add the sea ice fraction to an
        ! elevated ice tile.
        n_elev_ice = COUNT(elev_ice(1:nnvg) > 0)
        IF (n_elev_ice > 0) THEN
          i_elev_ice = 1
          n = elev_ice(1)
          DO WHILE ( (gsf_ij(i,j,n) == 0.0) .AND. (i_elev_ice <= n_elev_ice) )
            i_elev_ice = i_elev_ice + 1
            n = elev_ice(i_elev_ice)
          END DO
          ! Only add the fraction of sea ice if an ice tile with some surface
          ! fraction is found.
          IF (i_elev_ice <= n_elev_ice) THEN
            gsf_ij(i,j,n) = gsf_ij(i,j,n) + ice_fract_ij(i,j) * seafrac
          END IF
        END IF
      END IF
    END IF

  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! Have surface temperatures by surface type on land vector
! Need to create a gridded version as need surface temperatures over
! the ocean and cryosphere
! Set all tile temperatures to grid box surface temperature where not set.
DO n = 1, ntype
  DO j = 1, rows
    DO i = 1, row_length
      IF (t0tile_ij(i,j,n) < 100.0) THEN
        t0tile_ij(i,j,n) = tstar_ij(i,j)
      END IF
    END DO      ! End of DO loop over row_length
  END DO        ! End of DO loop over rows
END DO          ! End of DO loop over surface types

! Set up tile temperatures where a mixture of sea and sea ice is
! present. Set ice to freezing temperature (tfs)
DO j = 1, rows
  DO i = 1, row_length
    IF (ice_fract_ij(i,j) > 0.0) THEN
      t0tile_ij(i,j,lake) = tfs
    END IF
  END DO
END DO

! Identify grid cells as tundra if latitude > tundra_s_limit
tundra_mask_ij(:,:) = (sinlat_ij(:,:) > tundra_s_limit)

! Derive resistance terms, deposition velocities and loss rates

! Calculate aerodynamic and quasi-laminar resistances, Ra and Rb
CALL deposition_jules_aerod(row_length, rows, land_pts, land_index, lake,      &
  tstar_ij, pstar_ij, surf_ht_flux_ij, ustar_ij, gsf_ij, zh, z0tile_ij,        &
  canht_pft, dep_ra_ij, dep_rb_ij, dep_vd_so4_ij)

! Calculate surface resistance term Rc
! Depends on dry_dep_model
errcode = dry_dep_model
SELECT CASE ( dry_dep_model )
CASE ( dry_dep_model_ukca )
  ! 1. UKCA routines as used in UM-UKCA (in JULES code base)
  CALL deposition_ukca_surfddr(                                                &
    row_length, rows, land_pts, ntype, npft, land_index_ij, tundra_mask_ij,    &
    tstar_ij, pstar_ij, rh_ij, surf_wetness_ij, ustar_ij, gsf_ij, t0tile_ij,   &
    smc_land, stom_con_pft, lai_pft, nsurft, dim_cs1, vol_smc_sat_dep,         &
    snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand,                &
    dep_vd_so4_ij, dep_rc_ij, dep_rc_stom_ij, dep_rc_nonstom_ij,               &
    o3_stom_frac_ij)

CASE ( dry_dep_model_jules )
  ! 2. UKCA routines for use in JULES with flexible tile configuration
  CALL deposition_jules_surfddr(                                               &
    row_length, rows, land_pts, lake, land_index_ij, tundra_mask_ij,           &
    tstar_ij, pstar_ij, rh_ij, surf_wetness_ij, ustar_ij, gsf_ij, t0tile_ij,   &
    smc_land, stom_con_pft, lai_pft, nsurft, dim_cs1, vol_smc_sat_dep,         &
    snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand,                &
    dep_vd_so4_ij, dep_rc_ij, dep_rc_stom_ij, dep_rc_nonstom_ij,               &
    o3_stom_frac_ij)

CASE DEFAULT
  CALL ereport ("Invalid value for dry_dep_model. ", errcode,                  &
                 "Please check: deposition_jules_ddepctl")

END SELECT

! Combine resistance terms to give deposition velocity and loss rate
CALL deposition_jules_ddcalc(row_length, rows, bl_levels,                      &
  timestep, zh, dzl, gsf_ij, dep_ra_ij, dep_rb_ij, dep_rc_ij,                  &
  dep_vd_ij, dep_loss_rate_ij, nlev_with_ddep                                  &
  )

! If l_deposition_flux true, derive deposition fluxes of deposited chemical
! species. For JULES Standalone, these could be either observed (with missing
! measurements) or modelled concentrations. For UM-coupled JULES,
! concentrations as used in UM (i.e., mass mixing ratio)
IF (l_deposition_flux) THEN
  ! Derive deposition fluxes if dep_surfconc_ij > missing data
  DO n = 1, ndry_dep_species
    DO j = 1, rows
      DO i = 1, row_length
        ! Initialise to missing data
        dep_flux_ij(i,j,n) = rmdi
        IF (ABS( dep_surfconc_ij(i,j,n)-rmdi ) < EPSILON(1.0) ) THEN
          dep_flux_ij(i,j,n) = dep_loss_rate_ij(i,j,n)*dep_surfconc_ij(i,j,n)
        END IF
      END DO
    END DO
  END DO        ! End of DO loop over deposited atmospheric species
END IF

DO j = 1, rows
  DO i = 1, row_length
    DO m = 1, ndry_dep_species
      DO n = 1, ntype
        dep_vd_gb(i,j,m) = dep_vd_gb(i,j,m) + gsf_ij(i,j,n)*dep_vd_ij(i,j,n,m)
      END DO ! ntype
    END DO   ! ndry_dep
  END DO     ! row_length
END DO       ! rows

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE deposition_jules_ddepctl

END MODULE deposition_jules_ddepctl_mod
