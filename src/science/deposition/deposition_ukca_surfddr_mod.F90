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

MODULE deposition_ukca_surfddr_mod

! ------------------------------------------------------------------------------
! Description:
!   Calculates surface resistances.
!   Based on UKCA routine
!     ukca/src/science/chemistry/deposition/ukca_surfddr.F90
!   Key changes are:
!   (1) the IF first block is moved to the JULES-based module
!       deposition_initialisation_surfddr_mod
!   (2) land variables used on land vector
!   (3) DO loops reordered for optimal performance and
!       loop indexing consistent with JULES practice
!
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
  ModuleName = 'DEPOSITION_UKCA_SURFDDR_MOD'

CONTAINS

SUBROUTINE deposition_ukca_surfddr(                                            &
  row_length, rows, land_pts, ntype, npft, land_index_ij, tundra_mask,         &
  t0, p0, rh, surf_wetness, u_s, gsf, t0tile, smr_lp, stcon_lp, lai_lp,        &
  nsurft, dim_cs1, vol_smc_sat_dep, snow_depth, soil_carbon_dep,               &
  deep_soil_temp_dep, soil_sand, so4_vd, rc, rc_stom, rc_nonstom,              &
  o3_stom_frac )

! The following UKCA modules have been replaced:
! (1) asad_mod for ndepd, speci, nldepd, jpdd
!     Replace asad_mod with deposition_ukca_var_mod
! (2) ukca_config_specification_mod for ukca_config
!     Use jules_deposition_mod for UKCA deposition switches

USE deposition_ukca_var_mod, ONLY: ndepd_jules, speci_jules, nldepd_jules

USE jules_deposition_mod,    ONLY: l_ukca_ddepo3_ocean,                        &
                                   l_ukca_dry_dep_so2wet,                      &
                                   l_ukca_emsdrvn_ch4,                         &
                                   dep_rnull,                                  &
                                   dep_h2_soil_scheme,                         &
                                   dep_h2_scheme_conrad,                       &
                                   dep_h2_scheme_paulot


USE jules_science_fixes_mod, ONLY: l_fix_improve_drydep,                       &
                                   l_fix_ukca_h2dd_x,                          &
                                   l_fix_drydep_so2_water

USE deposition_initialisation_aerod_mod, ONLY:                                 &
                                   zero, one

USE deposition_initialisation_surfddr_mod,                                     &
                                     ! Only used by deposition_ukca_surfddr
                             ONLY: r_stom_spec, r_stom_no2, r_stom_o3,         &
                                   r_stom_pan, r_stom_so2, r_stom_nh3,         &
                                   r_tundra_spec, r_tundra_no2, r_tundra_co,   &
                                   r_tundra_o3, r_tundra_h2, r_tundra_pan,     &
                                   r_tundra, glmin,                            &
                                   r_wet_o3, cuticle_o3, mml, TAR_scaling,     &
                                   ch4dd_tun, ch4_up_flux, hno3dd_ice, dif,    &
                                   h2dd_c, h2dd_m, h2dd_q, rsurf,              &
                                   rooh, aerosol, tenpointzero,                &
                                     ! Common to both deposition_jules_surfddr
                                     ! and deposition_ukca_surfddr
                                   rh_thresh, minus1degc, minus5degc,          &
                                   r_wet_so2, r_dry_so2, r_cut_so2_5degc,      &
                                   r_cut_so2_5to_1,                            &
                                   so2con1, so2con2, so2con3, so2con4,         &
                                   smfrac1, smfrac2, smfrac3, smfrac4,         &
                                   tol, min_tile_frac, soil_moist_thresh,      &
                                   microb_tmin, microb_rhmin, tmin_hno3,       &
                                   h2dd_sf, h2dd_dep_rmax, h2dd_sm_min,        &
                                   secs_in_hour, ten, zero_int

USE jules_print_mgr,         ONLY: jules_print, jules_message

USE ereport_mod,             ONLY:                                             &
    ereport

USE deposition_output_arrays_mod, ONLY: deposition_output_array_real

USE deposition_ukca_ddepo3_ocean_mod, ONLY: deposition_ukca_ddepo3_ocean

USE deposition_ukca_h2dd_soil_mod, ONLY: deposition_ukca_h2dd_soil

USE jules_surface_types_mod, ONLY:                                             &
    brd_leaf,                                                                  &
    brd_leaf_dec,                                                              &
    brd_leaf_eg_trop,                                                          &
    brd_leaf_eg_temp,                                                          &
    ndl_leaf,                                                                  &
    ndl_leaf_dec,                                                              &
    ndl_leaf_eg,                                                               &
    c3_grass,                                                                  &
    c3_crop,                                                                   &
    c3_pasture,                                                                &
    c4_grass,                                                                  &
    c4_crop,                                                                   &
    c4_pasture,                                                                &
    shrub,                                                                     &
    shrub_dec,                                                                 &
    shrub_eg,                                                                  &
    urban,                                                                     &
    urban_canyon,                                                              &
    urban_roof,                                                                &
    lake,                                                                      &
    soil,                                                                      &
    ice,                                                                       &
    elev_ice

USE c_rmol,                  ONLY: rmol       ! Universal gas constant
USE missing_data_mod,        ONLY: imdi
USE parkind1,                ONLY: jprb, jpim
USE yomhook,                 ONLY: lhook, dr_hook

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
  row_length,                                                                  &
    ! Number of points on a row
  rows,                                                                        &
    ! Number of rows
  land_pts,                                                                    &
    ! Number of land points
  ntype,                                                                       &
    ! Number of surface types
  nsurft,                                                                      &
    ! Number of surface tiles
  dim_cs1,                                                                     &
    ! Number of pools for soil carbon (cs)
  npft
    ! Number of plant functional types

! Input arguments on lat, lon grid
INTEGER, INTENT(IN) ::                                                         &
  land_index_ij(row_length, rows)

! Input arguments on lat, lon grid
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  t0(row_length,rows),                                                         &
    ! Surface temperature (K)
  p0(row_length,rows),                                                         &
    ! Surface pressure (Pa)
  rh(row_length,rows),                                                         &
    ! Relative humidity (fraction)
  surf_wetness(row_length,rows),                                               &
    ! Surface wetness index (fraction)
  u_s(row_length,rows),                                                        &
    ! Surface friction velocity (m s-1)
  gsf(row_length,rows,ntype),                                                  &
    ! Global surface fractions
  t0tile(row_length,rows,ntype),                                               &
    ! Surface temperature on tiles (K)
  so4_vd(row_length,rows)
    ! Aerosol deposition velocity (m s-1)
    !(assumed to be the same for SO4 and other aerosols)

! Input arguments on land vector
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  smr_lp(land_pts),                                                            &
    ! Soil moisture content (Fraction by volume)
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
  lai_lp(land_pts, npft),                                                      &
    ! Leaf area index (m2 leaf m-2)
  stcon_lp(land_pts, npft)
    ! Stomatal conductance (m s-1)

! Surface resistance on tiles (s m-1).
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  rc(row_length,rows,ntype,ndepd_jules),                                       &
    ! Surface resistance, Rc (s m-1)
  rc_stom(row_length,rows,ntype,ndepd_jules),                                  &
    ! Surface resistance, Rc (s m-1): stomatal component
  rc_nonstom(row_length,rows,ntype,ndepd_jules),                               &
    ! Surface resistance, Rc (s m-1): non-stomatal component
  o3_stom_frac(row_length,rows)
    ! Fraction of O3 deposition through stomata

LOGICAL, INTENT(IN) ::                                                         &
  tundra_mask(row_length,rows)
    ! identifies high-latitude grid cells as tundra

! ------------------------------------------------------------------------------

! Local variables
INTEGER :: errcode
  ! error code

INTEGER :: i         ! index for longitudes
INTEGER :: j         ! index for latitudes
INTEGER :: k         ! index for atmospheric levels
INTEGER :: l         ! index for land points
INTEGER :: n         ! index for pfts or tiles
INTEGER :: m         ! index for species that deposit

! Surface type indices (local copies)
INTEGER, PARAMETER :: n_elev_ice = 10

INTEGER, PARAMETER :: first_term  = 1
INTEGER, PARAMETER :: second_term = 2
INTEGER, PARAMETER :: third_term  = 3
INTEGER, PARAMETER :: fourth_term = 4

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
    ! Cuticular Resistance for O3
  r_cut_so2
    ! Cuticular resistance for SO2

REAL(KIND=real_jlslsm) ::                                                      &
  r_stom(row_length,rows,npft,r_stom_spec)
    ! Stomatal resistance

! For H2 deposition scheme ------------------
REAL(KIND=real_jlslsm) ::                                                      &
  rc_h2(row_length,rows,ntype)
    ! Dry deposition for H2 uptake (rc)
! ---------------------------------------------

LOGICAL :: todo(row_length,rows,ntype)
  ! True if tile fraction > min_tile_frac
LOGICAL :: microb(row_length,rows)
  ! True if T > 5 C and RH > 40%
  ! (i.e. microbes in soil are active).

INTEGER  :: n_nosurf
  ! Number of species for which surface resistance values are not set

! Temporary logicals (local copies)
! The block defining the following deposition logicals has been deleted as
! they are defined in the JULES-based module jules_deposition above:
! l_fix_improve_drydep, l_fix_ukca_h2dd_x, l_fix_drydep_so2_water,
! l_ukca_dry_dep_so2wet, l_ukca_ddepo3_ocean, l_ukca_emsdrvn_ch4
! This is to avoid the use of UKCA-based modules in JULES.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER   :: RoutineName='DEPOSITION_UKCA_SURFDDR'


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Set logical for surface types
DO n = 1, ntype
  DO j = 1, rows
    DO i = 1, row_length
      todo(i,j,n) = (gsf(i,j,n) > min_tile_frac)
    END DO
  END DO
END DO

! Set logical microb for microbial activity.
! Used in deposition of CH4, CO and H2
DO j = 1, rows
  DO i = 1, row_length
    microb(i,j) = (rh(i,j) > microb_rhmin .AND. t0(i,j) > microb_tmin)
  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! The array rsurf has the standard resistance terms for vegetation, soil,
! rock, water etc. In the UKCA, rsurf is set-up on a IF first call in the
! routine, ukca/src/science/chemistry/deposition/ukca_surfddr.F90.
! This initialisation, which is hardwired, is now in the JULES-based module
! jules/src/science/deposition/deposition_initialisation_surfddr_mod.F90

! Set all surface types to standard values. These values will be modified
! below as necessary. Extra terms for vegetated surfaces (stomatal, cuticular)
! will be added as required.
! Loop over all parts of array rc to ensure all of it is assigned a value.

DO m = 1, ndepd_jules

  DO n = 1, ntype
    DO j = 1, rows
      DO i = 1, row_length
        rc_stom(i,j,n,m) = dep_rnull
        IF (todo(i,j,n)) THEN
          rc(i,j,n,m) = rsurf(n,m)
          rc_nonstom(i,j,n,m) = rsurf(n,m)
        ELSE
          rc(i,j,n,m) = dep_rnull
          rc_nonstom(i,j,n,m) = dep_rnull
        END IF
      END DO
    END DO
  END DO       ! End of DO loop over surface types

  IF (((speci_jules(nldepd_jules(m)) == 'O3        ')  .OR.                    &
       (speci_jules(nldepd_jules(m)) == 'O3S       ')) .AND.                   &
         l_ukca_ddepo3_ocean ) THEN

    ! Call ozone dry deposition scheme for ocean surface for both O3 and O3S
    ! Parameterisation of Ashok et al.
    DO n = 1, ntype
      DO j = 1, rows
        DO i = 1, row_length
          IF (( gsf(i,j,n) > 0.75) .AND. ( n == lake) ) THEN
            ! gsf > 0.75 is to make sure that the bulk of the grid is
            ! water to invoke the new scheme

            ! Setup fields for deposition to the ocean
            p_surf1 = p0(i,j)
            t_surf1 = t0(i,j)
            ustar = u_s(i,j)
            sst   = t0(i,j) ! assume t0 is representative of SST (K)

            CALL deposition_ukca_ddepo3_ocean(p_surf1, t_surf1, sst, ustar,    &
                                              rc_ocean)
            rc(i,j,n,m) = rc_ocean
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over surface types
  END IF         ! End of IF species = O3 or O3S

END DO           ! End of DO loop over deposited species

! Set fraction of ozone deposition that occurs through the stomata to zero
DO j = 1, rows
  DO i = 1, row_length
    o3_stom_frac(i,j) = zero
  END DO        ! End of DO loop over row_length
END DO          ! End of DO loop over rows

! Set stomatal resistance terms to no deposition
DO m = 1, r_stom_spec
  DO n = 1, npft
    DO j = 1, rows
      DO i = 1, row_length
        r_stom(i,j,n,m)            = dep_rnull
      END DO    ! End of DO loop over row_length
    END DO      ! End of DO loop over rows
  END DO        ! End of DO loop over pfts
END DO          ! End of DO loop over deposited atmosphe! Initialise and calculate stomatal resistances

! Calculate stomatal resistances.
! Only relevant to specific deposited chemical species
DO n = 1, npft
  DO j = 1, rows
    DO i = 1, row_length
      ! Set stomatal resistance if a land point
      l = land_index_ij(i,j)
      IF (l /= imdi) THEN
        ! A land point
        IF (todo(i,j,n) .AND. stcon_lp(l,n) > glmin) THEN
          r_stom(i,j,n,r_stom_no2) = 1.5*dif(r_stom_no2) / stcon_lp(l,n) ! NO2
          r_stom(i,j,n,r_stom_o3)  =     dif(r_stom_o3)  / stcon_lp(l,n) ! O3
          r_stom(i,j,n,r_stom_pan) =     dif(r_stom_pan) / stcon_lp(l,n) ! PAN
          r_stom(i,j,n,r_stom_so2) =     dif(r_stom_so2) / stcon_lp(l,n) ! SO2
          r_stom(i,j,n,r_stom_nh3) =     dif(r_stom_nh3) / stcon_lp(l,n) ! NH3
        END IF
      END IF
    END DO
  END DO
END DO           ! End of DO loop over pfts


! Now begin assigning specific surface resistances.
DO m = 1, ndepd_jules

  SELECT CASE( speci_jules(nldepd_jules(m)) )
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
            IF ((smr_lp(l) > soil_moist_thresh) .AND. todo(i,j,n)) THEN
              rc(i,j,n,m) = r_wet_o3
            END IF
          END IF
        END DO
      END DO

      ! Change values for shrubs and soil in tundra regions if latitude > 60N
      ! When l_fix_ukca_h2dd_x is retired, this could become a CASE statement
      IF ( ( .NOT. l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == npft) .OR.    &
           ( l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == shrub)      .OR.    &
           ( n == shrub_dec .OR. n == shrub_eg ) ) THEN
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask(i,j)) THEN
              IF (todo(i,j,n)) THEN
                rc(i,j,n,m) = r_tundra(r_tundra_o3)
              END IF
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
            IF (todo(i,j,n) .AND. lai_lp(l,n) > zero) THEN
              r_cut_o3 = cuticle_o3 / lai_lp(l,n)
            ELSE
              r_cut_o3 = dep_rnull
            END IF
          END IF

          ! Calculate plant deposition terms.
          IF (todo(i,j,n)) THEN
            rr = (one/r_stom(i,j,n,r_stom_o3)) + (one/r_cut_o3) +              &
                 (one/rc(i,j,n,m))
            rc_stom(i,j,n,m) = r_stom(i,j,n,r_stom_o3)
            rc_nonstom(i,j,n,m) = one / ( (one/r_cut_o3) +                     &
              (one/rc(i,j,n,m)) )
            rc(i,j,n,m) = one / rr
            o3_stom_frac(i,j) = o3_stom_frac(i,j) +                            &
              gsf(i,j,n) / (rr * r_stom(i,j,n,r_stom_o3))
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over pft

    ! Deposition to bare soil
    DO j = 1, rows
      DO i = 1, row_length
        l = land_index_ij(i,j)

        ! Change land deposition values if surface is wet;
        ! Soil moisture value of 0.3 fairly arbitrary.
        IF (l /= imdi) THEN
          IF ((smr_lp(l) > soil_moist_thresh) .AND. todo(i,j,soil)) THEN
            rc(i,j,soil,m) = r_wet_o3
            rc_nonstom(i,j,soil,m) = r_wet_o3
          END IF
        END IF

        ! Change values for soil in tundra regions
        IF (tundra_mask(i,j) .AND. todo(i,j,soil)) THEN
          rc(i,j,soil,m)  = r_tundra(r_tundra_o3)
          rc_nonstom(i,j,soil,m)  = r_tundra(r_tundra_o3)
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
    ! The surface resistance for S02 deposition is modified depending
    ! on if the surface is wet or dry or it is raining. If dry and no rain,
    ! make rc a function of surface relative humidity. Also calculate the
    ! cuticular resistance term for SO2 and include both r_cut_so2 and
    ! r_stom in final calculated value for SO2.
    ! Snow on veg/soil does not appear to be considered for any
    ! deposition term, so it is not included in the parameterization below.
    ! It is partly accounted for in the r_cut_so2 term via the use of t0tile.

    IF (l_ukca_dry_dep_so2wet) THEN

      ! Change to dry deposition for soil tile
      IF (soil > 0) THEN
        DO j = 1, rows
          DO i = 1, row_length
            l = land_index_ij(i,j)
            IF (l /= imdi) THEN
              IF ( smr_lp(l) > soil_moist_thresh .OR.                          &
                   surf_wetness(i,j) > tol ) THEN
                IF ( t0tile(i,j,soil) > minus1degc .AND.                       &
                     todo(i,j,soil) ) THEN
                  ! Surface is wet
                  rc(i,j,soil,m) = r_wet_so2
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
                ! Change land deposition values
                IF ( ((smr_lp(l) > soil_moist_thresh) .OR.                     &
                      (surf_wetness(i,j) > tol))      .AND.                    &
                     (t0tile(i,j,n) > minus1degc) ) THEN
                  ! Surface is wet
                  rc(i,j,n,m) = r_wet_so2
                END IF

                ! Cuticular resistance for SO2
                ! A land point
                IF (lai_lp(l,n) > tol) THEN
                  ! Determine the temperature range
                  IF (t0tile(i,j,n) >= minus1degc) THEN
                    ! Temperature is greater than -1 degrees C
                    IF (smr_lp(l) > soil_moist_thresh .OR.                     &
                        surf_wetness(i,j) > tol) THEN
                      ! Surface is wet
                      r_cut_so2 = r_wet_so2
                    ELSE
                      ! Equation 9 from EPW1994 is not applied to crops
                      IF ( (n == c3_crop) .OR. (n == c4_crop) ) THEN
                        r_cut_so2 = rc(i,j,n,m)
                      ELSE
                        IF (rh(i,j) < rh_thresh) THEN
                          r_cut_so2 = so2con1 * EXP(so2con3*rh(i,j))
                        ELSE
                          r_cut_so2 = so2con2 * EXP(so2con4*rh(i,j))
                        END IF
                      END IF

                      ! Ensure that r_cut_so2 stays within reasonable bounds
                      ! i.e. r_wet_so2 <= r_cut_so2 <= r_dry_so2
                      r_cut_so2 = MAX( r_wet_so2, MIN(r_dry_so2, r_cut_so2) )
                    END IF
                  ELSE IF (t0tile(i,j,n) >= minus5degc) THEN
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
              rr = (one/r_stom(i,j,n,r_stom_so2)) + (one/r_cut_so2) +          &
                   (one/rc(i,j,n,m))
              rc_stom(i,j,n,m) = r_stom(i,j,n,r_stom_so2)
              rc_nonstom(i,j,n,m) = one / ( (one/r_cut_so2) +                  &
                (one/rc(i,j,n,m)) )
              rc(i,j,n,m) = one / rr

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

      ! Change values for shrubs and soil in tundra regions if latitude > 60N
      IF ( ( .NOT. l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == npft) .OR.    &
           ( l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == shrub)      .OR.    &
           ( n == shrub_dec .OR. n == shrub_eg ) ) THEN
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask(i,j) .AND. todo(i,j,n)) THEN
              rc(i,j,n,m) = r_tundra(r_tundra_no2)
            END IF
          END DO
        END DO
      END IF

      ! Calculate plant deposition terms.
      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n)) THEN
            rr = (one/r_stom(i,j,n,r_stom_no2)) + (one/rc(i,j,n,m))
            rc_stom(i,j,n,m) = r_stom(i,j,n,r_stom_no2)
            rc_nonstom(i,j,n,m) = rc(i,j,n,m)
            rc(i,j,n,m) = one / rr
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over pft

    ! Deposition to bare soil
    DO j = 1, rows
      DO i = 1, row_length
        IF (tundra_mask(i,j) .AND. todo(i,j,soil)) THEN
          rc(i,j,soil,m) = r_tundra(r_tundra_no2)
          rc_nonstom(i,j,soil,m) = r_tundra(r_tundra_no2)
        END IF
      END DO
    END DO
    ! End of Deposition/Uptake of NO2

  CASE ( 'PAN       ', 'PPAN      ', 'MPAN      ', 'ONITU     ' )
    ! Deposition/Uptake of PAN, PPAN, MPAN, ONITU
    ! Recoded as UKCA routines previously based on 5pft surface type ordering

    ! Resistance terms dependent on pft
    DO n = 1, npft

      ! Change values for shrubs and soil in tundra regions if latitude > 60N
      IF ( ( .NOT. l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == npft) .OR.    &
           ( l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == shrub)      .OR.    &
           ( n == shrub_dec .OR. n == shrub_eg ) ) THEN
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask(i,j) .AND. todo(i,j,n)) THEN
              rc(i,j,n,m) = r_tundra(r_tundra_pan)
            END IF
          END DO
        END DO
      END IF

      ! Calculate plant deposition terms.
      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n)) THEN
            rr = (one/r_stom(i,j,n,r_stom_pan)) + (one/rc(i,j,n,m))
            rc_stom(i,j,n,m) = r_stom(i,j,n,r_stom_pan)
            rc_nonstom(i,j,n,m) = rc(i,j,n,m)
            rc(i,j,n,m) = one / rr
          END IF
        END DO
      END DO
    END DO       ! End of DO loop over pft

    ! Deposition to bare soil
    DO j = 1, rows
      DO i = 1, row_length
        IF (tundra_mask(i,j) .AND. todo(i,j,soil)) THEN
          rc(i,j,soil,m) = r_tundra(r_tundra_pan)
          rc_nonstom(i,j,soil,m) = r_tundra(r_tundra_pan)
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
                  sm = LOG(MAX(smr_lp(l),h2dd_sm_min))
                END IF
                IF (l_fix_ukca_h2dd_x) THEN
                  rr = (h2dd_c(n) + sm*(h2dd_m(n) + sm*h2dd_q(n))) * h2dd_sf
                ELSE
                  rr = (h2dd_c(n) + sm*(h2dd_m(n) + sm*h2dd_q(n)))
                END IF
                IF (rr > h2dd_dep_rmax) THEN
                  rr = h2dd_dep_rmax       ! Conrad/Seiler Max value
                END IF
                rc(i,j,n,m) = one / rr
              END IF
            END DO
          END DO

        ELSE IF (n == shrub .OR. n == shrub_dec .OR. n == shrub_eg) THEN
          ! Shrub and bare soil
          ! H2 dry dep velocity has no dependence on soil moisture

          DO j = 1, rows
            DO i = 1, row_length
              IF (todo(i,j,n) .AND. microb(i,j)) THEN
                IF (tundra_mask(i,j)) THEN

                  ! treat shrub as shrub above tundra limit
                  IF ( l_fix_ukca_h2dd_x ) THEN
                    rc(i,j,n,m) = one / (h2dd_c(n) * h2dd_sf)
                  ELSE
                    rc(i,j,n,m) = one / (h2dd_c(n))
                  END IF

                ELSE

                  ! Effectively C3 grass
                  l = land_index_ij(i,j)
                  IF (l == imdi) THEN
                    sm = h2dd_sm_min
                  ELSE
                    sm = MAX(smr_lp(l),h2dd_sm_min)
                  END IF
                  IF (c3_crop > 0) THEN
                    IF ( l_fix_ukca_h2dd_x ) THEN
                      rc(i,j,n,m) =  one /                                     &
                        ((h2dd_m(c3_crop)*sm + h2dd_c(c3_crop)) *h2dd_sf)
                    ELSE
                      rc(i,j,n,m) =  one /                                     &
                        (h2dd_m(c3_crop)*sm + h2dd_c(c3_crop))
                    END IF
                  END IF

                  IF (c3_pasture > 0) THEN
                    IF ( l_fix_ukca_h2dd_x ) THEN
                      rc(i,j,n,m) = one /                                      &
                        ((h2dd_m(c3_pasture)*sm + h2dd_c(c3_pasture)) *h2dd_sf)
                    ELSE
                      rc(i,j,n,m) = one /                                      &
                        (h2dd_m(c3_pasture)*sm + h2dd_c(c3_pasture))
                    END IF
                  END IF

                  IF (c3_grass > 0) THEN
                    IF ( l_fix_ukca_h2dd_x ) THEN
                      rc(i,j,n,m) = one /                                      &
                        ((h2dd_m(c3_grass)*sm + h2dd_c(c3_grass)) *h2dd_sf)
                    ELSE
                      rc(i,j,n,m) = one /                                      &
                        (h2dd_m(c3_grass)*sm + h2dd_c(c3_grass))
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
                  sm = MAX(smr_lp(l),h2dd_sm_min)
                END IF
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rc(i,j,n,m) = one /                                          &
                    ((h2dd_m(n) * sm + h2dd_c(n) ) * h2dd_sf)
                ELSE
                  rc(i,j,n,m) = one /                                          &
                    (h2dd_m(n) * sm + h2dd_c(n) )
                END IF
              END IF
            END DO
          END DO

        END IF

      END DO ! End loop over pfts

      ! Uptake by bare soil
      IF ( l_fix_ukca_h2dd_x ) THEN
        n = soil
      ELSE
        ! Treat as shrub
        IF (shrub_dec > 0) THEN
          n = shrub_dec
        END IF
        IF (shrub_eg  > 0) THEN
          n = shrub_eg
        END IF
        IF (ntype == 9) THEN
          IF ( l_fix_ukca_h2dd_x ) THEN
            n = npft
          ELSE
            n = shrub
          END IF
        END IF
      END IF

      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n) .AND. microb(i,j)) THEN
            IF (tundra_mask(i,j)) THEN
              ! treat soil as shrub above tundra limit
              IF (shrub_dec > 0) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rr = one / (h2dd_c(shrub_dec)*h2dd_sf)
                ELSE
                  rr = one / h2dd_c(shrub_dec)
                END IF
              END IF
              IF (shrub_eg > 0) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rr = one / (h2dd_c(shrub_eg)*h2dd_sf)
                ELSE
                  rr = one / h2dd_c(shrub_eg)
                END IF
              END IF
              IF (ntype == 9) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rr = one / (h2dd_c(shrub)*h2dd_sf)
                ELSE
                  rr = one / h2dd_c(npft)
                END IF
              END IF
              rc(i,j,soil,m) = one / rr
            ELSE
              ! treat soil as c3_grass below tundra limit
              l = land_index_ij(i,j)
              IF (l == imdi) THEN
                sm = h2dd_sm_min
              ELSE
                sm = MAX(smr_lp(l),h2dd_sm_min)
              END IF
              IF (c3_crop > 0) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rc(i,j,soil,m) = one /                                       &
                    ( (h2dd_m(c3_crop)*sm+h2dd_c(c3_crop)) *h2dd_sf )
                ELSE
                  rc(i,j,soil,m) = one /                                       &
                    ( h2dd_m(c3_crop)*sm+h2dd_c(c3_crop) )
                END IF
              END IF
              IF (c3_pasture > 0) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rc(i,j,soil,m) = one /                                       &
                    ( (h2dd_m(c3_pasture)*sm+h2dd_c(c3_pasture))               &
                      *h2dd_sf )
                ELSE
                  rc(i,j,soil,m) = one /                                       &
                    ( h2dd_m(c3_pasture)*sm+h2dd_c(c3_pasture) )
                END IF
              END IF
              IF (c3_grass > 0) THEN
                IF ( l_fix_ukca_h2dd_x ) THEN
                  rc(i,j,soil,m) = one /                                       &
                    ( (h2dd_m(c3_grass)*sm+h2dd_c(c3_grass)) *h2dd_sf )
                ELSE
                  rc(i,j,soil,m) = one /                                       &
                    ( h2dd_m(c3_grass)*sm+h2dd_c(c3_grass) )
                END IF
              END IF
            END IF
          END IF

        END DO   ! End loop over row_length
      END DO     ! End loop over rows

    CASE ( dep_h2_scheme_paulot )
      ! Using H2 dry deposition from Paulot et al. (2021) Uses soil properties
      CALL deposition_ukca_h2dd_soil(row_length, rows, land_pts, ntype,        &
        nsurft, dim_cs1, land_index_ij, gsf, p0, smr_lp, vol_smc_sat_dep,      &
        snow_depth, soil_carbon_dep, deep_soil_temp_dep, soil_sand, rc_h2 )

      rc(:,:,:,m) = rc_h2(:,:,:)

    CASE DEFAULT
      CALL ereport ("Invalid value for dep_H2_soil_scheme. ", errcode,         &
                    "Please check: deposition_ukca_surfddr")

    END SELECT   ! dep_H2_soil_scheme

    rc_nonstom(:,:,:,m) = rc(:,:,:,m)
    ! End of Deposition/Uptake of H2

  CASE ( 'CH4       ' )
    ! Deposition/Uptake of CH4

    ! Calculate an uptake flux initially. The uptake flux depends
    ! on soil moisture, based on results of Reay et al. (2001).

    IF (l_ukca_emsdrvn_ch4) THEN
      ! initialise to minimum uptake flux
      DO n = 1,ntype
        DO j = 1, rows
          DO i = 1, row_length
            rc(i,j,n,m) = one/dep_rnull
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
              sm = smr_lp(l)
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
                rc(i,j,n,m) = ch4_up_flux(n) * f_ch4_uptk
              END IF
            ELSE
              ! Now do shrubs. Assumed to be tundra if latitude > 60N
              ! Otherwise, as for other pfts

              IF (tundra_mask(i,j)) THEN
                ts = t0tile(i,j,n)
                rr =         ch4dd_tun(fourth_term) +                          &
                     (ts * ( ch4dd_tun(third_term)  +                          &
                     (ts * ( ch4dd_tun(second_term) +                          &
                     (ts *   ch4dd_tun(first_term)) )) ))
                rr = rr * secs_in_hour ! Convert from s-1 to h-1
                IF (todo(i,j,n)) THEN
                  rc(i,j,n,m) = MAX(rr, zero)
                END IF
              ELSE
                IF (todo(i,j,n)) THEN
                  rc(i,j,n,m) = ch4_up_flux(n) * f_ch4_uptk
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
            IF (tundra_mask(i,j)) THEN
              ts = t0tile(i,j,soil)
              rr =         ch4dd_tun(fourth_term) +                            &
                   (ts * ( ch4dd_tun(third_term)  +                            &
                   (ts * ( ch4dd_tun(second_term) +                            &
                   (ts *   ch4dd_tun(first_term)) )) ))
              rr = rr * secs_in_hour ! Convert from s-1 to h-1
              IF (todo(i,j,soil)) THEN
                rc(i,j,soil,m) = MAX(rr, zero)
              END IF
            ELSE
              l = land_index_ij(i,j)
              IF (l /= imdi) THEN
                sm = smr_lp(l)
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
                rc(i,j,soil,m) = ch4_up_flux(soil) * f_ch4_uptk
              END IF
            END IF
          ELSE
            IF (l_ukca_emsdrvn_ch4) THEN
              rc(i,j,n,m) = one/dep_rnull ! uptake rate, NOT a resistance!
            ELSE
              rc(i,j,n,m) = dep_rnull
            END IF
          END IF
        END DO
      END DO
    ELSE
      n = npft ! ( shrub not soil )
      DO j = 1, rows
        DO i = 1, row_length
          IF (microb(i,j)) THEN
            IF (tundra_mask(i,j)) THEN
              ts = t0tile(i,j,npft) ! ( shrub not soil )
              rr =         ch4dd_tun(fourth_term) +                            &
                   (ts * ( ch4dd_tun(third_term)  +                            &
                   (ts * ( ch4dd_tun(second_term) +                            &
                   (ts *   ch4dd_tun(first_term)) )) ))
              rr = rr * secs_in_hour ! Convert from s-1 to h-1
              IF (todo(i,j,soil)) THEN
                rc(i,j,soil,m) = MAX(rr, zero)
              END IF
            ELSE
              l = land_index_ij(i,j)
              IF (l /= imdi) THEN
                sm = smr_lp(l)
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
                rc(i,j,soil,m) = ch4_up_flux(soil) * f_ch4_uptk
              END IF
            END IF
          ELSE
            IF (l_ukca_emsdrvn_ch4) THEN
              rc(i,j,n,m) = one/dep_rnull ! uptake rate, NOT a resistance!
            ELSE
              rc(i,j,n,m) = dep_rnull
            END IF
          END IF
        END DO
      END DO
    END IF

    ! Convert CH4 uptake fluxes (ug m-2 h-1) to resistance (s m-1).
    IF (l_ukca_emsdrvn_ch4) THEN
      DO n = 1, ntype
        DO j = 1, rows
          DO i = 1, row_length
            IF (todo(i,j,n)) THEN ! global sfc fraction >zero for sfc type n
              IF (microb(i,j)) THEN ! there is microbial activity
                rr = rc(i,j,n,m)
                IF (rr > zero) THEN ! the default uptake flux is non-zero
                  IF (n == soil) THEN ! apply scaling factor for soil tiles
                    rc(i,j,n,m) = (p0(i,j)*mml)/(rmol*t0tile(i,j,n)*           &
                                   rr*TAR_scaling)
                  ELSE IF ((n == urban) .OR. (n == lake) .OR.                  &
                           (n >= ice)) THEN ! not in cities, lakes or on ice
                    rc(i,j,n,m) = dep_rnull
                  ELSE
                    rc(i,j,n,m) = (p0(i,j)*mml)/(rmol*t0tile(i,j,n)*rr)
                  END IF
                ELSE ! default deposition resistance
                  rc(i,j,n,m) = dep_rnull
                END IF
              ELSE ! no microbial activity in sfc fraction
                rc(i,j,n,m) = dep_rnull
              END IF
              ! process non-PFT and non-soil tiles
              IF (n > npft .AND. n /= soil) THEN
                rc(i,j,n,m) = dep_rnull
              END IF
            ELSE ! tile fraction below minimum for sfc type n
              rc(i,j,n,m) = dep_rnull
            END IF
          END DO          ! End of loop over row_length
        END DO            ! End of loop over rows
      END DO              ! End of loop over surface type
    ELSE
      DO n = 1, npft
        DO j = 1, rows
          DO i = 1, row_length
            IF (todo(i,j,n) .AND. microb(i,j)) THEN
              rr = rc(i,j,n,m)
              IF (rr > zero) THEN
                rc(i,j,n,m) = p0(i,j) * mml / (rmol * t0tile(i,j,n) * rr)
              ELSE
                rc(i,j,n,m) = dep_rnull
              END IF
            END IF
          END DO          ! End of loop over row_length
        END DO            ! End of loop over rows
      END DO              ! End of loop over pft

      n = soil
      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n) .AND. microb(i,j)) THEN
            rr = rc(i,j,n,m)
            IF (rr > zero) THEN
              rc(i,j,n,m) = p0(i,j)*mml / (rmol*t0tile(i,j,n)*rr*TAR_scaling)
            ELSE
              rc(i,j,n,m) = dep_rnull
            END IF
          END IF
        END DO          ! End of loop over row_length
      END DO            ! End of loop over rows

    END IF      ! l_ukca_emsdrvn_ch4

    rc_nonstom(:,:,:,m) = rc(:,:,:,m)

    ! End of Deposition/Uptake of CH4

  CASE ( 'CO        ' )
    ! Deposition/Uptake of CO

    ! Recoded as UKCA routines previously based on 5pft surface type ordering
    ! Only assign values for CO if microbes are active.
    DO n = 1, npft
      IF ( ( .NOT. l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == npft) .OR.    &
           ( l_fix_ukca_h2dd_x .AND. ntype == 9 .AND. n == shrub)      .OR.    &
           ( n == shrub_dec .OR. n == shrub_eg ) ) THEN
        ! Change values for shrubs in tundra regions
        DO j = 1, rows
          DO i = 1, row_length
            IF (tundra_mask(i,j) .AND. microb(i,j) .AND. todo(i,j,n)) THEN
              rc(i,j,n,m) = r_tundra(r_tundra_co) ! CO
            END IF
          END DO
        END DO
      END IF
    END DO       ! End of DO loop over pft

    ! Change values soil in tundra regions
    DO j = 1, rows
      DO i = 1, row_length
        IF (tundra_mask(i,j) .AND. microb(i,j) .AND. todo(i,j,soil)) THEN
          rc(i,j,soil,m) = r_tundra(r_tundra_co)  ! CO
        END IF
      END DO
    END DO

    rc_nonstom(:,:,:,m) = rc(:,:,:,m)
    ! End of Deposition/Uptake of CO

  CASE ( 'HNO3      ', 'HONO2     ', 'ISON      ',                             &
        'HCl       ', 'HOCl      ', 'HBr       ', 'HOBr      ' )
    ! Deposition/Uptake of HONO2, ISON, HX, HOX (X=Cl or Br)

    ! Only consider HX and HOX if l_fix_improve_drydep is true
    IF ( (speci_jules(nldepd_jules(m)) == 'HCl       '  .OR.                   &
          speci_jules(nldepd_jules(m)) == 'HOCl      '  .OR.                   &
          speci_jules(nldepd_jules(m)) == 'HBr       '  .OR.                   &
          speci_jules(nldepd_jules(m)) == 'HOBr      ') .AND.                  &
          .NOT. l_fix_improve_drydep ) THEN

      ! No modification
      rc_nonstom(:,:,:,m) = rc(:,:,:,m)

    ELSE
      ! Calculate resistances for HONO2 deposition to ice, which
      ! depend on temperature. Ensure resistance for HONO2 does not fall
      ! below 10 s m-1.
      n = ice
      IF ( ( .NOT. l_fix_ukca_h2dd_x )                              .OR.       &
           ( (ntype == 9) .OR. (ntype == 13) .OR. (ntype == 17) ) ) THEN
        n = ntype
      END IF

      DO j = 1, rows
        DO i = 1, row_length
          IF (todo(i,j,n)) THEN
            ! Limit temperature to a minimum of 252K. Curve used
            ! only used data between 255K and 273K.

            ts = MAX(t0tile(i,j,n), tmin_hno3)
            rr =         hno3dd_ice(third_term)  +                             &
                 (ts * ( hno3dd_ice(second_term) +                             &
                 (ts *   hno3dd_ice(first_term)) ))
            rc(i,j,n,m) = MAX(rr, ten)
          END IF
        END DO
      END DO

      ! Elevated ice tiles
      DO l = 1, SIZE(elev_ice)
        IF (elev_ice(l) > 0) THEN
          n = elev_ice(l)
          DO j = 1, rows
            DO i = 1, row_length
              IF (todo(i,j,n)) THEN
                ! Limit temperature to a minimum of 252K.
                ! Curve used only data between 255K and 273K.
                ts = MAX(t0tile(i,j,n), tmin_hno3)
                rr =         hno3dd_ice(third_term)  +                         &
                     (ts * ( hno3dd_ice(second_term) +                         &
                     (ts *   hno3dd_ice(first_term)) ))
                rc(i,j,n,m) = MAX(rr, ten)
              END IF
            END DO
          END DO
        END IF
      END DO
      rc_nonstom(:,:,:,m) = rc(:,:,:,m)
    END IF

    ! End of Deposition/Uptake of HONO2, ISON, HX, HOX

  CASE ( 'ORGNIT    ', 'BSOA      ', 'ASOA      ', 'ISOSOA    ',               &
         'Sec_Org   ', 'SEC_ORG_I ' )
    ! Deposition/Uptake of secondary (in)organic aerosol components

    ! Only consider the Secondary Organic (Sec_Org) species
    ! if l_fix_improve_drydep is true
    IF ( (speci_jules(nldepd_jules(m)) == 'Sec_Org   '  .OR.                   &
          speci_jules(nldepd_jules(m)) == 'SEC_ORG_I ') .AND.                  &
         .NOT. l_fix_improve_drydep ) THEN
      ! No modification
      rc_nonstom(:,:,:,m) = rc(:,:,:,m)
    ELSE
      ! Deposition/Uptake of ORGNIT
      ! ORGNIT is treated as an aerosol.
      ! Assume vd is valid for all land types and aerosol types.

      DO n = 1, ntype
        DO j = 1, rows
          DO i = 1, row_length
            IF (so4_vd(i,j) > zero .AND. gsf(i,j,n) > zero) THEN
              ! for all plant functional plant types, soil and urban surfaces
              IF (n <= npft .OR. n == soil .OR. n == urban .OR.                &
                  n == urban_roof .OR. n == urban_canyon) THEN
                rr          = one / so4_vd(i,j)
                rc(i,j,n,m) = rr
              END IF
            END IF
          END DO
        END DO
      END DO
      rc_nonstom(:,:,:,m) = rc(:,:,:,m)
    END IF

    ! End of Deposition/Uptake of secondary (in)organic aerosol components

  END SELECT ! End of SELECT for name of deposited species

END DO  ! End of DO m = 1, ndepd_jules

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE deposition_ukca_surfddr

END MODULE deposition_ukca_surfddr_mod
