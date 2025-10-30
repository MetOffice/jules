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

MODULE deposition_jules_surfddr_mod

! ------------------------------------------------------------------------------
! Description:
!   Calculates surface resistances.
!   Based on UKCA routine
!     ukca/src/science/chemistry/deposition/ukca_surfddr.F90
!   Returns Rc
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE um_types,                ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName = 'DEPOSITION_JULES_SURFDDR_MOD'

CONTAINS

! ------------------------------------------------------------------------------
SUBROUTINE deposition_jules_surfddr(                                           &
  row_length, rows, land_pts, water, land_index_ij, tundra_mask_ij,            &
  tstar_ij, pstar_ij, rh_ij, surf_wetness_ij, ustar_ij, gsf_ij, t0tile_ij,     &
  smc_land, stom_con_pft, lai_pft, nsurft, dim_cs1,                            &
  vol_smc_sat_dep, snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand, &
  dep_vd_so4_ij, dep_rc_ij, dep_rc_stom_ij, dep_rc_nonstom_ij,                 &
  o3_stom_frac_ij)
! ------------------------------------------------------------------------------

USE jules_surface_types_mod, ONLY: ntype, npft, nnvg, elev_tile_max,           &
                                   brd_leaf, brd_leaf_dec,                     &
                                   brd_leaf_eg_trop, brd_leaf_eg_temp,         &
                                   ndl_leaf, ndl_leaf_dec, ndl_leaf_eg,        &
                                   c3_grass, c3_crop, c3_pasture,              &
                                   c4_grass, c4_crop, c4_pasture,              &
                                   shrub, shrub_dec, shrub_eg,                 &
                                   urban, urban_canyon, urban_roof,            &
                                   lake, soil, ice, elev_ice

USE jules_deposition_mod,    ONLY: ndry_dep_species, glmin,                    &
                                   l_ukca_ddepo3_ocean, l_ukca_dry_dep_so2wet, &
                                   l_ukca_emsdrvn_ch4, dep_rnull,              &
                                   dep_rzero, dep_rten, dep_h2_soil_scheme,    &
                                   dep_h2_scheme_conrad, dep_h2_scheme_paulot

USE jules_science_fixes_mod, ONLY: l_fix_improve_drydep, l_fix_ukca_h2dd_x,    &
                                   l_fix_drydep_so2_water

USE deposition_initialisation_aerod_mod, ONLY:                                 &
                                   zero, one

USE deposition_initialisation_surfddr_mod,                                     &
                                   ! Common to both deposition_jules_surfddr
                                   ! and deposition_ukca_surfddr
                             ONLY: rh_thresh, minus1degc, minus5degc,          &
                                   r_wet_so2, r_dry_so2, r_cut_so2_5degc,      &
                                   r_cut_so2_5to_1,                            &
                                   so2con1, so2con2, so2con3, so2con4,         &
                                   smfrac1, smfrac2, smfrac3, smfrac4,         &
                                   tol, min_tile_frac, soil_moist_thresh,      &
                                   microb_tmin, microb_rhmin, tmin_hno3,       &
                                   h2dd_sf, h2dd_dep_rmax, h2dd_sm_min,        &
                                   secs_in_hour, ten, zero_int

USE deposition_species_mod,  ONLY: dep_species_name, diffusion_corr,           &
                                   rsurf_std, r_wet_soil_o3, cuticle_o3,       &
                                   r_tundra, dd_ice_coeff, ch4dd_tundra,       &
                                   ch4_mml, ch4_scaling, ch4_up_flux,          &
                                   h2dd_c, h2dd_m, h2dd_q

USE deposition_ukca_ddepo3_ocean_mod, ONLY: deposition_ukca_ddepo3_ocean

USE deposition_ukca_h2dd_soil_mod, ONLY: deposition_ukca_h2dd_soil

USE jules_print_mgr,         ONLY: jules_print, jules_message
USE missing_data_mod,        ONLY: imdi
USE c_rmol,                  ONLY: rmol       ! Universal gas constant
USE parkind1,                ONLY: jprb, jpim
USE yomhook,                 ONLY: lhook, dr_hook
USE ereport_mod,             ONLY: ereport

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  nsurft,                                                                      &
    ! Number of surface tiles
  dim_cs1,                                                                     &
    ! Number of pools for soil carbon (cs)
  row_length,                                                                  &
    ! Number of points on a row
  rows,                                                                        &
    ! Number of rows
  land_pts,                                                                    &
    ! Number of land points
  water
    ! pseudo tile ID for open water surface type

! Input arguments on lat, long grid
INTEGER, INTENT(IN) ::                                                         &
  land_index_ij(row_length, rows)

! Input arguments on lat, lon grid
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  tstar_ij(row_length, rows),                                                  &
    ! Surface temperature (K)
  pstar_ij(row_length, rows),                                                  &
    ! Surface pressure (Pa)
  ustar_ij(row_length, rows),                                                  &
    ! Surface friction velocity (m s-1)
  rh_ij(row_length, rows),                                                     &
    ! Relative humidity (-)
  surf_wetness_ij(row_length, rows),                                           &
    ! Surface wetness index (-)
  gsf_ij(row_length, rows, ntype),                                             &
    ! Global surface fractions
  t0tile_ij(row_length, rows, ntype),                                          &
    ! Surface temperature on tiles (K)
  dep_vd_so4_ij(row_length, rows)
    ! Surface deposition velocity term for sulphate particles

! Input arguments on land vector
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  smc_land(land_pts),                                                          &
    ! Soil moisture content (fraction by volume)
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
  soil_sand(land_pts),                                                         &
    ! fraction of soil that is sand
  lai_pft(land_pts, npft),                                                     &
    ! Leaf area index (m2 leaf m-2)
  stom_con_pft(land_pts, npft)
    ! Stomatal conductance (m s-1)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  dep_rc_ij(row_length, rows, ntype, ndry_dep_species),                        &
    ! Surface resistance, Rc (s m-1)
  dep_rc_stom_ij(row_length, rows, ntype, ndry_dep_species),                   &
    ! Surface resistance, Rc (s m-1): stomatal component
  dep_rc_nonstom_ij(row_length, rows, ntype, ndry_dep_species),                &
    ! Surface resistance, Rc (s m-1): non-stomatal component
  o3_stom_frac_ij(row_length, rows)
    ! Fraction of O3 deposition through stomata

LOGICAL, INTENT(IN) ::                                                         &
  tundra_mask_ij(row_length, rows)
    ! identifies high-latitude grid cells as tundra

! ------------------------------------------------------------------------------
! Local variables

INTEGER              :: i, j, k, l, m, n
  ! Loop parameters

! Surface type indices (local copies)
INTEGER, PARAMETER :: first_term  = 1
INTEGER, PARAMETER :: second_term = 2
INTEGER, PARAMETER :: third_term  = 3
INTEGER, PARAMETER :: fourth_term = 4

! Number of species for which surface resistance values are not set
INTEGER            :: n_nosurf

REAL(KIND=real_jlslsm) ::                                                      &
  sm,                                                                          &
   ! Soil moisture content of gridbox
  rr,                                                                          &
   ! General temporary store for resistances.
  ts,                                                                          &
   ! Temperature of a particular tile.
  f_ch4_uptk,                                                                  &
    ! Factor to modify CH4 uptake fluxes
  t_surf1,                                                                     &
    ! surface temperature
  p_surf1,                                                                     &
    ! surface pressure
  ustar,                                                                       &
    ! Friction velocity
  sst,                                                                         &
    ! Sea surface temperature (K)
  rc_ocean,                                                                    &
    ! ocean surface resistance term (s/m)
  r_cut_o3,                                                                    &
    ! Cuticular Resistance for O3 (s/m)
  r_cut_so2
    ! Cuticular resistance for SO2 (s/m)

REAL(KIND=real_jlslsm) ::                                                      &
  r_stom(row_length,rows,npft,ndry_dep_species)
    ! Stomatal resistance

! For H2 deposition scheme ------------------
REAL(KIND=real_jlslsm) ::                                                      &
  dep_rc_h2(row_length,rows,ntype)
    ! Dry deposition for H2 uptake (rc)
! ---------------------------------------------

LOGICAL :: todo(row_length,rows,ntype)
  ! True if tile fraction > min_tile_frac
LOGICAL :: microb(row_length,rows)
  ! True if T > 5 C and RH > 40%
  ! (i.e. microbes in soil are active).

INTEGER :: errcode
  ! error code

INTEGER(KIND=jpim), PARAMETER     :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER     :: zhook_out = 1
REAL(KIND=jprb)                   :: zhook_handle

CHARACTER(LEN=*), PARAMETER       :: RoutineName='DEPOSITION_JULES_SURFDDR'

! ------------------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set logical todo for surface types
DO n = 1, ntype
  DO j = 1, rows
    DO i = 1, row_length
      todo(i,j,n) = (gsf_ij(i,j,n) > min_tile_frac)
    END DO      ! End of DO loop over row_length
  END DO        ! End of DO loop over rows
END DO          ! End of DO loop over surface types

! Set logical microb for microbial activity.
! Used in deposition of CH4, CO and H2
DO j = 1, rows
  DO i = 1, row_length
    microb(i,j) = (rh_ij(i,j) > microb_rhmin .AND. tstar_ij(i,j) > microb_tmin)
  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! The array rsurf_std has the standard resistance terms for vegetation, soil,
! rock, water etc. In the UKCA, rsurf_std is set-up on a IF first call in the
! routine, ukca/src/science/chemistry/deposition/ukca_surfddr.F90.
! This initialisation is not needed as rsurf_std is input through the set of
! jules_deposition_species namelists

! Set all surface types to standard values. These values will be modified
! below as necessary. Extra terms for vegetated surfaces (stomatal, cuticular)
! will be added as required.
! Loop over all parts of array rc to ensure all of it is assigned a value.

DO m = 1, ndry_dep_species
  DO n = 1, ntype
    DO j = 1, rows
      DO i = 1, row_length
        dep_rc_stom_ij(i,j,n,m) = dep_rnull
        IF (todo(i,j,n)) THEN
          dep_rc_ij(i,j,n,m) = rsurf_std(n,m)
          dep_rc_nonstom_ij(i,j,n,m) = rsurf_std(n,m)
        ELSE
          dep_rc_ij(i,j,n,m) = dep_rnull
          dep_rc_nonstom_ij(i,j,n,m) = dep_rnull
        END IF
      END DO
    END DO
  END DO         ! End of DO loop over surface types

  ! Call ozone dry deposition scheme for ocean surface for both O3 and O3S
  ! Parameterisation of Ashok et al.
  IF ( ((dep_species_name(m) == 'O3        ')  .OR.                            &
        (dep_species_name(m) == 'O3S       ')) .AND.                           &
         l_ukca_ddepo3_ocean ) THEN

    DO n = 1, ntype
      DO j = 1, rows
        DO i = 1, row_length
          IF (( gsf_ij(i,j,n) > 0.75) .AND. ( n == water) ) THEN
            ! gsf > 0.75 is to make sure that the bulk of the grid is
            ! water to invoke the new scheme

            ! Setup fields for deposition to the ocean
            p_surf1 = pstar_ij(i,j)
            t_surf1 = tstar_ij(i,j)
            ustar = ustar_ij(i,j)
            sst = tstar_ij(i,j) ! assume t0 is representative of SST (K)

            CALL deposition_ukca_ddepo3_ocean(p_surf1, t_surf1, sst, ustar,    &
              rc_ocean)
            dep_rc_ij(i,j,n,m) = rc_ocean
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over surface types
  END IF         ! End of IF species = O3 or O3S

END DO           ! End of DO loop over deposited species

! Set stomatal resistance terms to no deposition
DO m = 1, ndry_dep_species
  DO n = 1, npft
    DO j = 1, rows
      DO i = 1, row_length
        r_stom(i,j,n,m)            = dep_rnull
      END DO    ! End of DO loop over row_length
    END DO      ! End of DO loop over rows
  END DO        ! End of DO loop over pfts
END DO          ! End of DO loop over deposited atmospheric species

! Set fraction of ozone deposition that occurs through the stomata to zero
DO j = 1, rows
  DO i = 1, row_length
    o3_stom_frac_ij(i,j) = zero
  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! Calculate stomatal resistances.
! Only relevant to specific deposited chemical species
! A positive value of diffusion_corr is used to identify the species
DO m = 1, ndry_dep_species
  IF (diffusion_corr(m) > 0) THEN
    DO n = 1, npft
      DO j = 1, rows
        DO i = 1, row_length
          l = land_index_ij(i,j)
          IF (l /= imdi) THEN
            ! Land point
            IF (todo(i,j,n) .AND. stom_con_pft(l,n) > glmin) THEN
              IF (dep_species_name(m) == 'NO2       ') THEN
                ! To maintain bit comparability with UKCA-based routines
                r_stom(i,j,n,m) = 1.5 * diffusion_corr(m) / stom_con_pft(l,n)
              ELSE
                r_stom(i,j,n,m) = diffusion_corr(m) / stom_con_pft(l,n)
              END IF
            END IF
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over pfts
  END IF
END DO           ! End of DO loop over deposited species

! Now begin assigning specific surface resistances.
DO m = 1, ndry_dep_species

  SELECT CASE( dep_species_name(m) )
  CASE ( 'O3        ', 'O3S       ' )
    ! Deposition/Uptake of O3
    ! Recoded as UKCA routines previously based on 5pft surface type ordering

    ! Resistance terms dependent on pft
    DO n = 1, npft

      ! Change land deposition values if surface is wet;
      ! Soil moisture value of 0.3 fairly arbitrary.
      DO j = 1, rows
        DO i = 1, row_length
          l = land_index_ij(i,j)
          IF (l /= imdi) THEN
            IF ((smc_land(l) > soil_moist_thresh) .AND. todo(i,j,n)) THEN
              dep_rc_ij(i,j,n,m) = r_wet_soil_o3
            END IF
          END IF
        END DO
      END DO

      ! Change values for shrubs and soil in tundra regions
      IF (n == shrub .OR. n == shrub_dec .OR. n == shrub_eg) THEN
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask_ij(i,j) .AND. todo(i,j,n)) THEN
              dep_rc_ij(i,j,n,m) = r_tundra(m)
            END IF
          END DO
        END DO
      END IF

      DO j = 1, rows
        DO i = 1, row_length
          l = land_index_ij(i,j)
          ! Cuticular resistance for ozone, only on land points.
          IF (l == imdi) THEN
            ! Not a land point
            r_cut_o3 = dep_rnull
          ELSE
            ! Land point
            IF (todo(i,j,n) .AND. lai_pft(l,n) > zero) THEN
              r_cut_o3 = cuticle_o3 / lai_pft(l,n)
            ELSE
              r_cut_o3 = dep_rnull
            END IF
          END IF

          ! Calculate plant deposition terms.
          IF (todo(i,j,n)) THEN
            rr = (one/r_stom(i,j,n,m)) + (one/r_cut_o3) +                      &
                 (one/dep_rc_ij(i,j,n,m))
            dep_rc_stom_ij(i,j,n,m) = r_stom(i,j,n,m)
            dep_rc_nonstom_ij(i,j,n,m) = one / ( (one/r_cut_o3) +              &
              (one/dep_rc_ij(i,j,n,m)) )
            dep_rc_ij(i,j,n,m) = one / rr
            o3_stom_frac_ij(i,j) = o3_stom_frac_ij(i,j) +                      &
              gsf_ij(i,j,n) / (rr * r_stom(i,j,n,m))
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over pft

    ! Deposition to bare soil
    DO j = 1, rows
      DO i = 1, row_length
        ! Change land deposition values if surface is wet;
        ! Soil moisture value of 0.3 fairly arbitrary.
        l = land_index_ij(i,j)
        IF (l /= imdi) THEN
          IF ((smc_land(l) > soil_moist_thresh) .AND. todo(i,j,soil)) THEN
            dep_rc_ij(i,j,soil,m) = r_wet_soil_o3
            dep_rc_nonstom_ij(i,j,soil,m) = r_wet_soil_o3
          END IF
        END IF

        ! Change values for soil in tundra regions
        IF (tundra_mask_ij(i,j) .AND. todo(i,j,soil)) THEN
          dep_rc_ij(i,j,soil,m) = r_tundra(m)
          dep_rc_nonstom_ij(i,j,soil,m) = r_tundra(m)
        END IF
      END DO
    END DO
    ! End of Deposition/Uptake of O3

  CASE ( 'SO2       ' )
    ! Deposition/Uptake of SO2

    ! The parametrization below is based on `Parametrization of surface
    ! resistance for the quantification of atmospheric deposition of
    ! acidifying pollutants and ozone' by Erisman, Pul and Wyers (1993)
    ! (hereafter EPW1994) and `The Elspeetsche Veld experiment on surface
    ! exchange of trace gases: Summary of results' by Erisman et al (1994).
    ! The surface resistance for SO2 deposition is modified depending
    ! on if the surface is wet or dry or it is raining. If dry and no rain,
    ! make rc a function of surface relative humidity. Also calculate the
    ! cuticular resistance term for SO2 and include both r_cut_so2 and
    ! r_stom in final calculated value for SO2.
    ! Snow on veg/soil does not appear to be considered for any
    ! deposition term, so it is not included in the parameterization below.
    ! It is partly accounted for in the r_cut_so2 term via the use of
    ! t0tile_ij.

    IF (l_ukca_dry_dep_so2wet) THEN

      ! Change to dry deposition for soil tile
      IF (soil > zero_int) THEN
        DO j = 1, rows
          DO i = 1, row_length
            l = land_index_ij(i,j)
            IF (l /= imdi) THEN
              IF ( smc_land(l) > soil_moist_thresh .OR.                        &
                   surf_wetness_ij(i,j) > tol ) THEN
                IF ( t0tile_ij(i,j,soil) > minus1degc .AND.                    &
                     todo(i,j,soil) ) THEN
                  ! Surface is wet
                  dep_rc_ij(i,j,soil,m) = r_wet_so2
                END IF
              END IF
            END IF
          END DO
        END DO
      END IF

      ! Change to dry deposition for vegetation tiles
      DO n = 1, npft
        DO j = 1, rows
          DO i = 1, row_length
            IF (todo(i,j,n)) THEN
              l = land_index_ij(i,j)
              IF (l /= imdi) THEN
                ! Change land deposition values if wet
                IF ( ((smc_land(l) > soil_moist_thresh) .OR.                   &
                      (surf_wetness_ij(i,j) > tol))     .AND.                  &
                     (t0tile_ij(i,j,n) > minus1degc) ) THEN
                  ! Surface is wet
                  dep_rc_ij(i,j,n,m) = r_wet_so2
                END IF

                ! Cuticular resistance for SO2
                ! A land point
                IF (lai_pft(l,n) > tol) THEN
                  ! Determine the temperature range
                  IF (t0tile_ij(i,j,n) >= minus1degc) THEN
                    ! Temperature is greater than -1 degrees C
                    IF (smc_land(l) > soil_moist_thresh .OR.                   &
                        surf_wetness_ij(i,j) > tol) THEN
                      ! Surface is wet
                      r_cut_so2 = r_wet_so2
                    ELSE
                      ! Equation 9 from EPW1994 is not applied to crops
                      IF ( (n == c3_crop) .OR. (n == c4_crop) ) THEN
                        r_cut_so2 = dep_rc_ij(i,j,n,m)
                      ELSE
                        IF (rh_ij(i,j) < rh_thresh) THEN
                          r_cut_so2 = so2con1 * EXP(so2con3*rh_ij(i,j))
                        ELSE
                          r_cut_so2 = so2con2 * EXP(so2con4*rh_ij(i,j))
                        END IF
                      END IF

                      ! Ensure that r_cut_so2 stays within reasonable bounds
                      ! i.e. r_wet_so2 <= r_cut_so2 <= r_dry_so2
                      r_cut_so2 = MAX( r_wet_so2, MIN(r_dry_so2, r_cut_so2) )
                    END IF
                  ELSE IF (t0tile_ij(i,j,n) >= minus5degc) THEN
                    ! Temperature is between -5 degrees C and -1 degrees C
                    r_cut_so2 = r_cut_so2_5to_1
                  ELSE
                    ! Temperature is < -5 degrees C
                    r_cut_so2 = r_cut_so2_5degc
                  END IF
                ELSE
                  r_cut_so2 = dep_rnull
                END IF
              END IF

              ! Include plant deposition and cuticular term in rc calculation
              rr = (one/r_stom(i,j,n,m)) + (one/r_cut_so2) +                   &
                   (one/dep_rc_ij(i,j,n,m))
              dep_rc_stom_ij(i,j,n,m) = r_stom(i,j,n,m)
              dep_rc_nonstom_ij(i,j,n,m) = one / ( (one/r_cut_so2) +           &
                (one/dep_rc_ij(i,j,n,m)) )
              dep_rc_ij(i,j,n,m) = one / rr

            END IF ! End of IF on todo
          END DO
        END DO
      END DO       ! End of DO loop over pft

    END IF
    ! End of Deposition/Uptake of SO2

  CASE ( 'NO2       ' )
    ! Deposition/Uptake of NO2
    ! Recoded as UKCA routines previously based on 5pft surface type ordering

    ! Resistance terms dependent on pft
    DO n = 1, npft

      ! Change values for shrubs and soil in tundra regions
      IF (n == shrub .OR. n == shrub_dec .OR. n == shrub_eg) THEN
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask_ij(i,j) .AND. todo(i,j,n)) THEN
              dep_rc_ij(i,j,n,m) = r_tundra(m)
            END IF
          END DO
        END DO
      END IF

      ! Calculate plant deposition terms.
      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n)) THEN
            rr = (one/r_stom(i,j,n,m)) + (one/dep_rc_ij(i,j,n,m))
            dep_rc_stom_ij(i,j,n,m)    = r_stom(i,j,n,m)
            dep_rc_nonstom_ij(i,j,n,m) = dep_rc_ij(i,j,n,m)
            dep_rc_ij(i,j,n,m) = one / rr
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over pft

    ! Deposition to bare soil
    DO j = 1, rows
      DO i = 1, row_length
        IF (tundra_mask_ij(i,j) .AND. todo(i,j,soil)) THEN
          ! Change values for shrubs and soil in tundra regions
          dep_rc_ij(i,j,soil,m) = r_tundra(m)
          dep_rc_nonstom_ij(i,j,soil,m) = r_tundra(m)
        END IF
      END DO
    END DO
    ! End of Deposition/Uptake of NO2

  CASE ( 'PAN       ', 'PPAN      ', 'MPAN      ', 'ONITU     ' )
    ! Deposition/Uptake of PAN, PPAN, MPAN, ONITU
    ! Recoded as UKCA routines previously based on 5pft surface type ordering

    ! Resistance terms dependent on pft
    DO n = 1, npft

      ! Change values for shrubs and soil in tundra regions
      IF (n == shrub .OR. n == shrub_dec .OR. n == shrub_eg) THEN
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask_ij(i,j) .AND. todo(i,j,n)) THEN
              dep_rc_ij(i,j,n,m) = r_tundra(m)
            END IF
          END DO
        END DO
      END IF

      ! Calculate plant deposition terms.
      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n)) THEN
            rr = (one/r_stom(i,j,n,m)) + (one/dep_rc_ij(i,j,n,m))
            dep_rc_stom_ij(i,j,n,m)    = r_stom(i,j,n,m)
            dep_rc_nonstom_ij(i,j,n,m) = dep_rc_ij(i,j,n,m)
            dep_rc_ij(i,j,n,m) = one / rr
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over pft

    ! Deposition to bare soil
    DO j = 1, rows
      DO i = 1, row_length
        IF (tundra_mask_ij(i,j) .AND. todo(i,j,soil)) THEN
          ! Change values for shrubs and soil in tundra regions
          dep_rc_ij(i,j,soil,m) = r_tundra(m)
          dep_rc_nonstom_ij(i,j,soil,m) = r_tundra(m)
        END IF
      END DO
    END DO
    ! End of Deposition/Uptake of PAN, PPAN, MPAN, ONITU

  CASE ( 'H2        ' )
    ! Deposition/Uptake of H2

    SELECT CASE( dep_h2_soil_scheme )
    CASE ( dep_h2_scheme_conrad )
      ! Sanderson et al. (2003), based on Conrad & Seiler. Uses land-use type

      DO n = 1, npft

        IF (n == c4_grass .OR. n == c4_pasture .OR. n == c4_crop) THEN
          ! C4 grass
          ! H2 dry dep has quadratic-log dependence on soil moisture
          ! Sanderson et al., JACS, 2003
          DO j = 1, rows
            DO i = 1, row_length
              IF (todo(i,j,n) .AND. microb(i,j)) THEN
                l = land_index_ij(i,j)
                IF (l == imdi) THEN
                  sm = LOG(h2dd_sm_min)
                ELSE
                  sm = LOG(MAX(smc_land(l),h2dd_sm_min))
                END IF
                IF (l_fix_ukca_h2dd_x) THEN
                  rr = (h2dd_c(n) + sm*(h2dd_m(n) + sm*h2dd_q(n))) * h2dd_sf
                ELSE
                  rr = (h2dd_c(n) + sm*(h2dd_m(n) + sm*h2dd_q(n)))
                END IF
                IF (rr > h2dd_dep_rmax) THEN
                  rr = h2dd_dep_rmax       ! Conrad/Seiler Max value
                END IF
                dep_rc_ij(i,j,n,m) = one / rr
              END IF
            END DO
          END DO

        ELSE IF (n == shrub .OR. n == shrub_dec .OR. n == shrub_eg) THEN
          ! Shrub and bare soil
          ! H2 dry dep velocity has no dependence on soil moisture
          DO j = 1, rows
            DO i = 1, row_length
              IF (todo(i,j,n) .AND. microb(i,j)) THEN
                IF (tundra_mask_ij(i,j)) THEN
                  IF ( l_fix_ukca_h2dd_x ) THEN
                    dep_rc_ij(i,j,n,m)    = one / (h2dd_c(n) * h2dd_sf)
                  ELSE
                    dep_rc_ij(i,j,n,m)    = one / (h2dd_c(n))
                  END IF
                ELSE
                  ! Effectively C3 grass
                  l = land_index_ij(i,j)
                  IF (l == imdi) THEN
                    sm = h2dd_sm_min
                  ELSE
                    sm = MAX(smc_land(l),h2dd_sm_min)
                  END IF
                  IF (c3_crop > zero_int) THEN
                    IF ( l_fix_ukca_h2dd_x ) THEN
                      dep_rc_ij(i,j,n,m) =                                     &
                        one/( (h2dd_m(c3_crop)*sm+h2dd_c(c3_crop)) *h2dd_sf )
                    ELSE
                      dep_rc_ij(i,j,n,m) =                                     &
                        one/( h2dd_m(c3_crop)*sm+h2dd_c(c3_crop) )
                    END IF
                  END IF
                  IF (c3_pasture > zero_int) THEN
                    IF ( l_fix_ukca_h2dd_x ) THEN
                      dep_rc_ij(i,j,n,m) =                                     &
                        one/( (h2dd_m(c3_pasture)*sm+h2dd_c(c3_pasture))       &
                        *h2dd_sf )
                    ELSE
                      dep_rc_ij(i,j,n,m) =                                     &
                        one/( h2dd_m(c3_pasture)*sm+h2dd_c(c3_pasture) )
                    END IF
                  END IF
                  IF (c3_grass > zero_int) THEN
                    IF ( l_fix_ukca_h2dd_x ) THEN
                      dep_rc_ij(i,j,n,m) =                                     &
                        one/( (h2dd_m(c3_grass)*sm+h2dd_c(c3_grass)) *h2dd_sf )
                    ELSE
                      dep_rc_ij(i,j,n,m) =                                     &
                        one/( h2dd_m(c3_grass)*sm+h2dd_c(c3_grass) )
                    END IF
                  END IF
                END IF
              END IF
            END DO
          END DO

        ELSE
          ! All other pfts
          ! H2 dry deposition velocity has linear dependence on soil moisture
          ! Limit soil moisture to avoid excessively high deposition velocities

          DO j = 1, rows
            DO i = 1, row_length
              IF (todo(i,j,n) .AND. microb(i,j)) THEN
                l = land_index_ij(i,j)
                IF (l == imdi) THEN
                  sm = h2dd_sm_min
                ELSE
                  sm = MAX(smc_land(l),h2dd_sm_min)
                END IF
                IF ( l_fix_ukca_h2dd_x ) THEN
                  dep_rc_ij(i,j,n,m) =                                         &
                    one/((h2dd_m(n)*sm + h2dd_c(n))*h2dd_sf)
                ELSE
                  dep_rc_ij(i,j,n,m) =                                         &
                    one/(h2dd_m(n)*sm + h2dd_c(n))
                END IF
              END IF
            END DO
          END DO

        END IF

      END DO ! End loop over pfts

      ! Uptake by bare soil
      IF ( l_fix_ukca_h2dd_x ) THEN
        n =soil
      ELSE
        ! Treat as shrub
        IF (shrub_dec > zero_int) THEN
          n = shrub_dec
        END IF
        IF (shrub_eg  > zero_int) THEN
          n = shrub_eg
        END IF
        IF (shrub     > zero_int) THEN
          n = shrub
        END IF
      END IF

      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n) .AND. microb(i,j)) THEN
            IF (tundra_mask_ij(i,j)) THEN
              ! treat soil as shrub above tundra limit
              IF (shrub_dec > zero_int) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rr = one / (h2dd_c(shrub_dec)*h2dd_sf)
                ELSE
                  rr = one / h2dd_c(shrub_dec)
                END IF
              END IF
              IF (shrub_eg > zero_int) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rr = one / (h2dd_c(shrub_eg)*h2dd_sf)
                ELSE
                  rr = one / h2dd_c(shrub_eg)
                END IF
              END IF
              IF (shrub > zero_int) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rr = one / (h2dd_c(shrub)*h2dd_sf)
                ELSE
                  rr = one / h2dd_c(shrub)
                END IF
              END IF
              dep_rc_ij(i,j,soil,m) = one / rr
            ELSE
              ! treat soil as c3_grass below tundra limit
              l = land_index_ij(i,j)
              IF (l == imdi) THEN
                sm = h2dd_sm_min
              ELSE
                sm = MAX(smc_land(l),h2dd_sm_min)
              END IF
              IF (c3_crop > zero_int) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  dep_rc_ij(i,j,soil,m) =                                      &
                    one/( (h2dd_m(c3_crop)*sm+h2dd_c(c3_crop)) *h2dd_sf )
                ELSE
                  dep_rc_ij(i,j,soil,m) =                                      &
                    one/( h2dd_m(c3_crop)*sm+h2dd_c(c3_crop) )
                END IF
              END IF
              IF (c3_pasture > zero_int) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  dep_rc_ij(i,j,soil,m) =                                      &
                    one/( (h2dd_m(c3_pasture)*sm+h2dd_c(c3_pasture))           &
                    *h2dd_sf )
                ELSE
                  dep_rc_ij(i,j,soil,m) =                                      &
                    one/( h2dd_m(c3_pasture)*sm+h2dd_c(c3_pasture) )
                END IF
              END IF
              IF (c3_grass > zero_int) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  dep_rc_ij(i,j,soil,m) =                                      &
                    one/( (h2dd_m(c3_grass)*sm+h2dd_c(c3_grass)) *h2dd_sf )
                ELSE
                  dep_rc_ij(i,j,soil,m) =                                      &
                    one/( h2dd_m(c3_grass)*sm+h2dd_c(c3_grass) )
                END IF
              END IF
            END IF
          END IF

        END DO   ! End loop over row_length
      END DO     ! End loop over rows

    CASE ( dep_h2_scheme_paulot )
      ! Using H2 dry deposition from Paulot et al. (2021) Uses soil properties
      CALL deposition_ukca_h2dd_soil(row_length, rows, land_pts, ntype,        &
        nsurft, dim_cs1, land_index_ij, gsf_ij, pstar_ij, smc_land,            &
        vol_smc_sat_dep, snow_depth, soil_carbon_dep,deep_soil_temp_dep,       &
        soil_sand, dep_rc_h2 )

      dep_rc_ij(:,:,:,m) = dep_rc_h2(:,:,:)

    CASE DEFAULT
      CALL ereport ("Invalid value for dep_H2_soil_scheme. ", errcode,         &
                    "Please check: deposition_jules_surfddr")

    END SELECT   ! dep_H2_soil_scheme

    dep_rc_nonstom_ij(:,:,:,m) = dep_rc_ij(:,:,:,m)
    ! End of Deposition/Uptake of H2

  CASE ( 'CH4       ' )
    ! Deposition/Uptake of CH4

    ! Calculate an uptake flux initially. The uptake flux depends
    ! on soil moisture, based on results of Reay et al. (2001).

    IF (l_ukca_emsdrvn_ch4) THEN
      ! initialise to minimum uptake flux
      DO n = 1, ntype
        DO j = 1, rows
          DO i = 1, row_length
            dep_rc_ij(i,j,n,m) = one/dep_rnull
            ! uptake rate, NOT a resistance!
          END DO
        END DO
      END DO
    END IF

    DO n = 1, npft
      DO j = 1, rows
        DO i = 1, row_length
          IF (microb(i,j)) THEN

            l = land_index_ij(i,j)
            IF (l /= imdi) THEN
              sm = smc_land(l)
            ELSE
              sm = zero
            END IF

            IF (sm < smfrac1) THEN
              f_ch4_uptk = sm / smfrac1
            ELSE IF (sm > smfrac2) THEN
              f_ch4_uptk = (smfrac3 - sm) / smfrac4
            ELSE
              f_ch4_uptk = one
            END IF

            IF (l_ukca_emsdrvn_ch4) THEN
              f_ch4_uptk = MIN(MAX(f_ch4_uptk,zero),one)
            ELSE
              f_ch4_uptk = MAX(f_ch4_uptk,zero)
            END IF

            ! Do the PFTs Broadleaf, Needleleaf, C3 and C4 grasses first.
            IF (n /= shrub .AND. n /= shrub_dec .AND. n /= shrub_eg) THEN
              IF (todo(i,j,n)) THEN
                dep_rc_ij(i,j,n,m) = ch4_up_flux(n) * f_ch4_uptk
              END IF
            ELSE
              ! Now do shrubs. Assumed to be tundra if latitude > 60N
              ! Otherwise, as for other pfts

              IF (tundra_mask_ij(i,j)) THEN
                ts = t0tile_ij(i,j,n)
                rr =         ch4dd_tundra(fourth_term) +                       &
                     (ts * ( ch4dd_tundra(third_term)  +                       &
                     (ts * ( ch4dd_tundra(second_term) +                       &
                     (ts *   ch4dd_tundra(first_term)) )) ))
                rr = rr * secs_in_hour ! Convert from s-1 to h-1
                IF (todo(i,j,n)) THEN
                  dep_rc_ij(i,j,n,m) = MAX(rr,zero)
                END IF
              ELSE
                IF (todo(i,j,n)) THEN
                  dep_rc_ij(i,j,n,m) = ch4_up_flux(n)*f_ch4_uptk
                END IF
              END IF
            END IF        ! End of IF pft
          END IF          ! End of IF microb
        END DO            ! End of loop over row_length
      END DO              ! End of loop over rows
    END DO                ! End of loop over pft

    ! Now do soil. Assumed to be tundra if latitude > 60N
    IF ( l_fix_ukca_h2dd_x ) THEN
      n = soil
      DO j = 1, rows
        DO i = 1, row_length
          IF (microb(i,j)) THEN
            IF (tundra_mask_ij(i,j)) THEN
              ts = t0tile_ij(i,j,soil)
              rr =         ch4dd_tundra(fourth_term) +                         &
                   (ts * ( ch4dd_tundra(third_term)  +                         &
                   (ts * ( ch4dd_tundra(second_term) +                         &
                   (ts *   ch4dd_tundra(first_term)) )) ))
              rr = rr * secs_in_hour ! Convert from s-1 to h-1
              IF (todo(i,j,soil)) THEN
                dep_rc_ij(i,j,soil,m) = MAX(rr, zero)
              END IF
            ELSE
              l = land_index_ij(i,j)
              IF (l /= imdi) THEN
                sm = smc_land(l)
              ELSE
                sm = zero
              END IF
              IF (sm < smfrac1) THEN
                f_ch4_uptk = sm / smfrac1
              ELSE IF (sm > smfrac2) THEN
                f_ch4_uptk = (smfrac3 - sm) / smfrac4
              ELSE
                f_ch4_uptk = one
              END IF
              IF (l_ukca_emsdrvn_ch4) THEN
                f_ch4_uptk = MIN(MAX(f_ch4_uptk, zero), one)
              ELSE
                f_ch4_uptk = MAX(f_ch4_uptk, zero)
              END IF
              IF (todo(i,j,soil)) THEN
                dep_rc_ij(i,j,soil,m) = ch4_up_flux(soil) * f_ch4_uptk
              END IF
            END IF
          ELSE
            IF (l_ukca_emsdrvn_ch4) THEN
              dep_rc_ij(i,j,n,m) = one/dep_rnull
                ! uptake rate, NOT a resistance!
            ELSE
              dep_rc_ij(i,j,n,m) = dep_rnull
            END IF
          END IF          ! End of IF microb
        END DO            ! End of loop over row_length
      END DO              ! End of loop over rows
    ELSE
      n = npft ! ( shrub not soil )
      DO j = 1, rows
        DO i = 1, row_length
          IF (microb(i,j)) THEN
            IF (tundra_mask_ij(i,j)) THEN
              ts = t0tile_ij(i,j,npft) ! ( shrub not soil )
              rr =         ch4dd_tundra(fourth_term) +                         &
                   (ts * ( ch4dd_tundra(third_term)  +                         &
                   (ts * ( ch4dd_tundra(second_term) +                         &
                   (ts *   ch4dd_tundra(first_term)) )) ))
              rr = rr * secs_in_hour ! Convert from s-1 to h-1
              IF (todo(i,j,soil)) THEN
                dep_rc_ij(i,j,soil,m) = MAX(rr, zero)
              END IF
            ELSE
              l = land_index_ij(i,j)
              IF (l /= imdi) THEN
                sm = smc_land(l)
              ELSE
                sm = zero
              END IF
              IF (sm < smfrac1) THEN
                f_ch4_uptk = sm / smfrac1
              ELSE IF (sm > smfrac2) THEN
                f_ch4_uptk = (smfrac3 - sm) / smfrac4
              ELSE
                f_ch4_uptk = one
              END IF
              IF (l_ukca_emsdrvn_ch4) THEN
                f_ch4_uptk = MIN(MAX(f_ch4_uptk, zero), one)
              ELSE
                f_ch4_uptk = MAX(f_ch4_uptk, zero)
              END IF
              IF (todo(i,j,soil)) THEN
                dep_rc_ij(i,j,soil,m) = ch4_up_flux(soil) * f_ch4_uptk
              END IF
            END IF
          ELSE
            IF (l_ukca_emsdrvn_ch4) THEN
              dep_rc_ij(i,j,n,m) = one/dep_rnull
                ! uptake rate, NOT a resistance!
            ELSE
              dep_rc_ij(i,j,n,m) = dep_rnull
            END IF
          END IF          ! End of IF microb
        END DO            ! End of loop over row_length
      END DO              ! End of loop over rows
    END IF

    ! Convert CH4 uptake fluxes (ug m-2 h-1) to resistance (s m-1).
    IF (l_ukca_emsdrvn_ch4) THEN
      DO n = 1, ntype
        DO j = 1, rows
          DO i = 1, row_length
            IF (todo(i,j,n)) THEN ! global sfc fraction >zero for sfc type n
              IF (microb(i,j)) THEN ! there is microbial activity
                rr = dep_rc_ij(i,j,n,m)
                IF (rr > zero) THEN ! the default uptake flux is non-zero
                  IF ( n <= npft ) THEN
                    ! Vegetated surfaces
                    dep_rc_ij(i,j,n,m) = pstar_ij(i,j)*ch4_mml /               &
                      (rmol*t0tile_ij(i,j,n)*rr)
                  ELSE IF (n == soil) THEN
                    ! Apply scaling factor for soil tiles
                    dep_rc_ij(i,j,n,m) = pstar_ij(i,j)*ch4_mml /               &
                      (rmol*t0tile_ij(i,j,n)*rr*ch4_scaling)
                  ELSE
                    dep_rc_ij(i,j,n,m) = dep_rnull
                  END IF
                ELSE ! default deposition resistance
                  dep_rc_ij(i,j,n,m) = dep_rnull
                END IF
              ELSE ! no microbial activity in sfc fraction
                dep_rc_ij(i,j,n,m) = dep_rnull
              END IF
              ! process non-PFT and non-soil tiles
              IF (n > npft .AND. n /= soil) THEN
                dep_rc_ij(i,j,n,m) = dep_rnull
              END IF
            ELSE ! tile fraction below minimum for sfc type n
              dep_rc_ij(i,j,n,m) = dep_rnull
            END IF
          END DO          ! End of loop over row_length
        END DO            ! End of loop over rows
      END DO              ! End of loop over surface type
    ELSE
      DO n = 1, npft
        DO j = 1, rows
          DO i = 1, row_length
            IF (todo(i,j,n) .AND. microb(i,j)) THEN
              rr = dep_rc_ij(i,j,n,m)
              IF (rr > zero) THEN
                dep_rc_ij(i,j,n,m) = pstar_ij(i,j)*ch4_mml /                   &
                  (rmol*t0tile_ij(i,j,n)* rr)
              ELSE
                dep_rc_ij(i,j,n,m) = dep_rnull
              END IF
            END IF
          END DO          ! End of loop over row_length
        END DO            ! End of loop over rows
      END DO              ! End of loop over pft

      n = soil
      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n) .AND. microb(i,j)) THEN
            rr = dep_rc_ij(i,j,n,m)
            IF (rr > zero) THEN
              dep_rc_ij(i,j,n,m) = pstar_ij(i,j)*ch4_mml /                     &
                (rmol*t0tile_ij(i,j,n)*rr*ch4_scaling)
            ELSE
              dep_rc_ij(i,j,n,m) = dep_rnull
            END IF
          END IF
        END DO
      END DO

    END IF      ! l_ukca_emsdrvn_ch4

    dep_rc_nonstom_ij(:,:,:,m) = dep_rc_ij(:,:,:,m)

    ! End of Deposition/Uptake of CH4

  CASE ( 'CO        ' )
    ! Deposition/Uptake of CO

    ! Recoded as UKCA routines previously based on 5pft surface type ordering
    ! Change standard resistance for shrubs in tundra regions
    ! Only assign values for CO if microbes are active.
    DO n = 1, npft
      IF (n == shrub .OR. n == shrub_dec .OR. n == shrub_eg) THEN
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask_ij(i,j) .AND. microb(i,j) .AND. todo(i,j,n)) THEN
              dep_rc_ij(i,j,n,m) = r_tundra(m)   ! CO
            END IF
          END DO
        END DO
      END IF
    END DO       ! End of DO loop over pft

    ! Change values soil in tundra regions
    DO j = 1, rows
      DO i = 1, row_length
        IF (tundra_mask_ij(i,j) .AND. microb(i,j) .AND. todo(i,j,soil)) THEN
          dep_rc_ij(i,j,soil,m) = r_tundra(m)    ! CO
        END IF
      END DO
    END DO

    dep_rc_nonstom_ij(:,:,:,m) = dep_rc_ij(:,:,:,m)
    ! End of Deposition/Uptake of CO

  CASE ( 'HNO3      ', 'HONO2     ', 'ISON      ',                             &
        'HCl       ', 'HOCl      ', 'HBr       ', 'HOBr      ' )
    ! Deposition/Uptake of HONO2, ISON, HX, HOX (X=Cl or Br)

    ! Only consider HX and HOX if l_fix_improve_drydep is true
    IF ( (dep_species_name(m) == 'HCl       '  .OR.                            &
          dep_species_name(m) == 'HOCl      '  .OR.                            &
          dep_species_name(m) == 'HBr       '  .OR.                            &
          dep_species_name(m) == 'HOBr      ') .AND.                           &
          .NOT. l_fix_improve_drydep ) THEN

      ! No modification
      dep_rc_nonstom_ij(:,:,:,m) = dep_rc_ij(:,:,:,m)

    ELSE
      ! Calculate resistances for deposition to ice, which depend on temperature
      ! Ensure resistance does not fall below 10 s m-1.
      n = ice
      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n)) THEN
            ! Limit temperature to a minimum of 252K. Curve used
            ! only used data between 255K and 273K.
            ts = MAX(t0tile_ij(i,j,n), tmin_hno3)
            rr =         dd_ice_coeff(m,third_term)  +                         &
                 (ts * ( dd_ice_coeff(m,second_term) +                         &
                 (ts *   dd_ice_coeff(m,first_term)) ))
            dep_rc_ij(i,j,n,m) = MAX(rr,ten)
          END IF
        END DO
      END DO

      ! Elevated ice tiles
      DO l = 1, SIZE(elev_ice)
        IF (elev_ice(l) > zero_int) THEN
          n = elev_ice(l)
          DO j = 1, rows
            DO i = 1, row_length
              IF (todo(i,j,n)) THEN
                ! Limit temperature to a minimum of 252K.
                ! Curve used only data between 255K and 273K.
                ts = MAX(t0tile_ij(i,j,n), tmin_hno3)
                rr =         dd_ice_coeff(m,third_term)  +                     &
                     (ts * ( dd_ice_coeff(m,second_term) +                     &
                     (ts *   dd_ice_coeff(m,first_term)) ))
                dep_rc_ij(i,j,n,m) = MAX(rr,ten)
              END IF
            END DO
          END DO
        END IF
      END DO
      dep_rc_nonstom_ij(:,:,:,m) = dep_rc_ij(:,:,:,m)
    END IF
    ! End of Deposition/Uptake of HONO2, ISON, HX, HOX

  CASE ( 'ORGNIT    ', 'BSOA      ', 'ASOA      ', 'ISOSOA    ',               &
         'Sec_Org   ', 'SEC_ORG_I ' )
    ! Deposition/Uptake of secondary (in)organic aerosol components

    ! Only consider the Secondary Organic (Sec_Org) species
    ! if l_fix_improve_drydep is true
    IF ( (dep_species_name(m) == 'Sec_Org   '  .OR.                            &
          dep_species_name(m) == 'SEC_ORG_I ') .AND.                           &
        .NOT. l_fix_improve_drydep ) THEN
      ! No modification
      dep_rc_nonstom_ij(:,:,:,m) = dep_rc_ij(:,:,:,m)
    ELSE
      ! Deposition/Uptake of ORGNIT
      ! ORGNIT is treated as an aerosol.
      ! Assume vd is valid for all land types and aerosol types.

      DO n = 1, ntype
        DO j = 1, rows
          DO i = 1, row_length
            IF (dep_vd_so4_ij(i,j) > zero .AND. gsf_ij(i,j,n) > zero) THEN
              ! for all plant functional plant types, soil and urban surfaces
              IF (n <= npft .OR. n == soil .OR. n == urban .OR.                &
                  n == urban_roof .OR. n == urban_canyon) THEN
                rr          = one / dep_vd_so4_ij(i,j)
                dep_rc_ij(i,j,n,m) = rr
              END IF
            END IF
          END DO
        END DO
      END DO
      dep_rc_nonstom_ij(:,:,:,m) = dep_rc_ij(:,:,:,m)
    END IF

    ! End of Deposition/Uptake of secondary (in)organic aerosol components

  END SELECT ! End of SELECT for name of deposited species

END DO  ! End of DO loop over ndry_dep_species

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE deposition_jules_surfddr
END MODULE deposition_jules_surfddr_mod
