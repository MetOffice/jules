! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sf_evap_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SF_EVAP_MOD'

CONTAINS
!  SUBROUTINE SF_EVAP------------------------------------------------

!  Purpose: Calculate surface evaporation and sublimation amounts
!           (without applying them to the surface stores).

!  Suitable for single column usage.

!  Documentation: UMDP 24

!--------------------------------------------------------------------
SUBROUTINE sf_evap (                                                           &
 land_pts,nsurft,                                                              &
 land_index,surft_index,surft_pts,sm_levels,fland,                             &
 ashtf_prime_surft,canopy,dtrdz_1,flake,fracaero_t,fracaero_s,snow_surft,resfs,&
 resft,rhokh_1,tile_frac,smc_soilt,wt_ext_surft,timestep,r_gamma,              &
 fqw_1,fqw_surft,ftl_1,ftl_surft,tstar_surft,                                  &
 ecan,ecan_surft,elake_surft,esoil_soilt,esoil_surft,ei_surft,ext_soilt,       &
 sf_diag, non_lake_frac,                                                       &
 !New arguments replacing USE statements
 ! crop_vars_mod (IN)
 frac_irr_soilt, frac_irr_surft, wt_ext_irr_surft, resfs_irr_surft,            &
 ! crop_vars_mod (IN OUT)
 smc_irr_soilt,                                                                &
 ! crop_vars_mod (OUT)
 ext_irr_soilt)

USE atm_fields_bounds_mod, ONLY: tdims
USE theta_field_sizes, ONLY: t_i_length

USE planet_constants_mod, ONLY: cp
USE water_constants_mod, ONLY: lc, lf, tm

USE ancil_info, ONLY: nsoilt

USE jules_irrig_mod, ONLY: l_irrig_dmd

USE jules_surface_mod, ONLY: l_flake_model
USE jules_surface_types_mod, ONLY: lake

USE jules_science_fixes_mod, ONLY: l_fix_neg_snow

USE sf_diags_mod, ONLY: strnewsfdiag

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE

INTEGER ::                                                                     &
 land_pts                                                                      &
                       ! IN Number of land points to be processed.
,nsurft                                                                        &
                       ! IN Number of tiles per land point.
,land_index(land_pts)                                                          &
                       ! IN Index of land points.
,surft_index(land_pts,nsurft)                                                  &
!                            ! IN Index of tile points.
,surft_pts(nsurft)                                                             &
                       ! IN Number of tile points.
,sm_levels                 ! IN Number of soil moisture levels.


REAL(KIND=real_jlslsm) ::                                                      &
 fland(land_pts)                                                               &
                       ! IN Fraction of gridbox which is land.
,ashtf_prime_surft(land_pts,nsurft)                                            &
!                            ! IN Adjusted SEB coefficient
,canopy(land_pts,nsurft)                                                       &
!                            ! IN Surface/canopy water on land
!                            !    tiles (kg/m2).
,dtrdz_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
!                            ! IN -g.dt/dp for surface layer
,flake(land_pts,nsurft)                                                        &
                       ! IN Lake fraction.
,fracaero_t(land_pts,nsurft)                                                   &
                       ! IN Total fractions of surface moisture flux
!                            !    with only aerodynamic resistance
!                            !    for land tiles
,fracaero_s(land_pts,nsurft)                                                   &
                       ! IN Fractions of surface moisture flux
!                            !    with only aerodynamic resistance
!                            !    for land tiles from the frozen part of the
!                            !    surface alone
,snow_surft(land_pts,nsurft)                                                   &
!                            ! IN Lying snow amount on tiles (kg/m2).
,resfs(land_pts,nsurft)                                                        &
                       ! IN Combined soil, stomatal and aerodynam.
!                            !    resistance factor for fraction 1-fracaero_t
!                            !    of land tiles.
,resft(land_pts,nsurft)                                                        &
                       ! IN Total resistance factor
!                            !    fracaero_t+(1-fracaero_t)*resfs.
,rhokh_1(land_pts,nsurft)                                                      &
!                            ! IN Surface exchange coefficients.
,tile_frac(land_pts,nsurft)                                                    &
!                            ! IN Tile fractions.
,smc_soilt(land_pts,nsoilt)                                                    &
                       ! IN Available soil moisture (kg/m2).
,wt_ext_surft(land_pts,sm_levels,nsurft)                                       &
!                            ! IN Fraction of transpiration
!                            !    extracted from each soil layer
!                            !    by each tile.
,timestep                                                                      &
                       ! IN Timestep in seconds.
,r_gamma                                                                       &
!                      ! IN implicit weight in level 1
,non_lake_frac(land_pts)
!                      ! IN Sum of fractions of non-FLake surface tiles

REAL(KIND=real_jlslsm) ::                                                      &
 fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! INOUT Surface moisture flux (kg/m2/s).
,fqw_surft(land_pts,nsurft)                                                    &
!                            ! INOUT Local FQW_1 for tiles.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                       ! INOUT Surface sensible heat flux (W/m2).
,ftl_surft(land_pts,nsurft)                                                    &
!                            ! INOUT Local FTL_1 for tiles.
,tstar_surft(land_pts,nsurft)
!                            ! INOUT Tile surface temperatures (K).

TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

REAL(KIND=real_jlslsm) ::                                                      &
 ecan(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                       ! OUT Gridbox mean evaporation from canopy/
!                            !     surface store (kg per sq m per s).
!                            !     Zero over sea and sea-ice.
,ecan_surft(land_pts,nsurft)                                                   &
!                            ! OUT ECAN for land tiles.
,elake_surft(land_pts,nsurft)                                                  &
!                            ! OUT Lake evaporation.
,esoil_soilt(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nsoilt)       &
                       ! OUT Gridbox mean evapotranspiration from
!                            !     soil moisture (kg per sq m per s).
!                            !     Zero over sea and sea-ice.
,esoil_surft(land_pts,nsurft)                                                  &
!                            ! OUT ESOIL for land tiles.
,ei_surft(land_pts,nsurft)                                                     &
!                            ! OUT Sublimation from snow or land-ice
!                            !     (kg per sq m per s).
,ext_soilt(land_pts,nsoilt,sm_levels)
                             ! OUT Extraction of water from each
                             !     soil layer (kg/m2/s).
                             !     of land tiles.


!New arguments replacing USE statements
! crop_vars_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: frac_irr_soilt(land_pts,nsoilt)
REAL(KIND=real_jlslsm), INTENT(IN) :: frac_irr_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: wt_ext_irr_surft(land_pts,sm_levels,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: resfs_irr_surft(land_pts,nsurft)

! crop_vars_mod (IN OUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: smc_irr_soilt(land_pts,nsoilt)
! crop_vars_mod (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ext_irr_soilt(land_pts,nsoilt,sm_levels)

REAL(KIND=real_jlslsm) ::                                                      &
 esoil_irr_soilt(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nsoilt)   &
!                            ! WORK Gridbox mean evapotranspiration from
!                            !     soil moisture (kg per sq m per s).
,esoil_nir_soilt(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nsoilt)   &
!                            ! WORK Gridbox mean evapotranspiration from
!                            !     soil moisture (kg per sq m per s).
,esoil_irr_surft(land_pts,nsurft)                                              &
!                            ! WORK Evapotranspiration from soil
!                            !     moisture through irrigated
!                            !     fraction of land tiles (kg/m2/s).
,esoil_nir_surft(land_pts,nsurft)                                              &
!                            ! WORK Evapotranspiration from soil
!                            !     moisture through non-irrigated
!                            !     fraction of land tiles (kg/m2/s).
,smc_nir_soilt(land_pts,nsoilt)
!                            ! WORK Available soil moisture (kg/m2).
!                            !     fraction (kg/m2/s).

REAL(KIND=real_jlslsm) ::                                                      &
 dfqw(land_pts)                                                                &
                       ! Increment in GBM moisture flux.
,dftl(land_pts)                                                                &
                       ! Increment in GBM sensible heat flux.
,e_surft_old(land_pts,nsurft)                                                  &
!                            ! Surface moisture flux before adjustment.
,le_surft_old(land_pts,nsurft)
!                            ! Surf latent heat flux before adjustment.

REAL(KIND=real_jlslsm) ::                                                      &
 diff_lat_htf                                                                  &
                       ! Increment in local latent heat flux.
,diff_sens_htf                                                                 &
                       ! Increment in local sensible heat flux.
,dtstar                                                                        &
                       ! Increment in local surface temperature.
,edt                                                                           &
                       ! Moisture flux x timestep
,rhokh1_prime          ! Modified forward time-weighted
                       ! transfer coefficient.

INTEGER ::                                                                     &
 i,j                                                                           &
             ! Loop counter (horizontal field index).
,k                                                                             &
             ! Loop counter (land, snow or land-ice field index).
,m                                                                             &
             ! Loop counter (soil level index).
,l                                                                             &
             ! Loop counter (land point field index).
,n                                                                             &
              ! Loop counter (tile index).
,mm
              ! Index for soil tile

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SF_EVAP'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!$OMP PARALLEL                                                                 &
!$OMP DEFAULT(SHARED)                                                          &
!$OMP PRIVATE(i,j,k,m,l,n,mm,edt,rhokh1_prime,diff_lat_htf,dtstar,             &
!$OMP diff_sens_htf)

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    ecan_surft(l,n) = 0.0
    esoil_surft(l,n) = 0.0
    IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
      sf_diag%et_stom_surft(l,n) = 0.0
    END IF
    IF (l_irrig_dmd) THEN
      esoil_nir_surft(l,n) = 0.0
      esoil_irr_surft(l,n) = 0.0
    END IF
    elake_surft(l,n) = 0.0
    ei_surft(l,n) = 0.0
  END DO
!$OMP END DO
END DO

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    e_surft_old(l,n) = fqw_surft(l,n)
    IF (l_fix_neg_snow) THEN
      ! Calculate the sublimation or deposition on the snow fraction
      ! consistently with the assumed resistance network. The canopy
      ! evaporation must be set here to allow it to be modified to
      ! account for exhaustion of the snow store.
      IF (resft(l,n) > 0.0) ecan_surft(l,n) =                                  &
                          (1.0 - flake(l,n)) *                                 &
                          (fracaero_t(l,n) - fracaero_s(l,n)) *                &
                          fqw_surft(l,n) / resft(l,n)
      le_surft_old(l,n) = lc * fqw_surft(l,n)
      IF (snow_surft(l,n)  >   0.0) THEN
        ei_surft(l,n) = (1.0 - flake(l,n)) * fracaero_s(l,n) *                 &
                        fqw_surft(l,n) / resft(l,n)
        le_surft_old(l,n) = le_surft_old(l,n) + lf * ei_surft(l,n)

      END IF
    ELSE
      IF (snow_surft(l,n)  >   0.0) THEN
        le_surft_old(l,n) = (lc + lf) * fqw_surft(l,n)
      ELSE
        le_surft_old(l,n) = lc * fqw_surft(l,n)
      END IF
    END IF
  END DO
!$OMP END DO
END DO

!-----------------------------------------------------------------------
! Sublimation from snow-covered land tiles
!-----------------------------------------------------------------------
DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    IF (snow_surft(l,n)  >   0.0) THEN
      IF (l_fix_neg_snow) THEN
        IF (fqw_surft(l,n) < 0.0) THEN
          IF (tstar_surft(l,n) < tm) THEN
            ! Deposit all moisture on the snow.
            ei_surft(l,n)   = fqw_surft(l,n)
            ecan_surft(l,n) = 0.0
          ELSE
            ! Partition the downward flux between the lake and canopy
            ! fractions. Actual fluxes are used directly rather than
            ! fluxes deduced from the potential evaporation to follow
            ! previous functionality for this case.
            ecan_surft(l,n)  = (1.0 - flake(l,n)) * fqw_surft(l,n)
            elake_surft(l,n) = flake(l,n) * fqw_surft(l,n)
            ei_surft(l,n)    = 0.0
          END IF
        ELSE
          edt = ei_surft(l,n) * timestep
          IF ( edt  >   snow_surft(l,n) ) THEN
            ecan_surft(l,n) = ecan_surft(l,n) + ei_surft(l,n) -                &
                              snow_surft(l,n) / timestep
            ei_surft(l,n) = snow_surft(l,n) / timestep
          END IF
        END IF
      ELSE
        ei_surft(l,n) =  fqw_surft(l,n)
        edt = ei_surft(l,n) * timestep
        IF ( edt  >   snow_surft(l,n) )                                        &
          ei_surft(l,n) = snow_surft(l,n) / timestep
        fqw_surft(l,n) = fqw_surft(l,n) -  ei_surft(l,n)
      END IF
    END IF
  END DO
!$OMP END DO
END DO

!-----------------------------------------------------------------------
! Surface evaporation from and condensation onto snow-free land
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    ecan(i,j) = 0.0
    esoil_soilt(i,j,:) = 0.0
    IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
      sf_diag%et_stom_ij(i,j) = 0.0
    END IF
    IF (l_irrig_dmd) THEN
      esoil_nir_soilt(i,j,:) = 0.0
      esoil_irr_soilt(i,j,:) = 0.0
    END IF
  END DO
END DO
!$OMP END DO

DO n = 1,nsurft

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

    !Set the current soil tile (see notice above)
  IF (nsoilt == 1) THEN
    !There is only 1 soil tile
    mm = 1
  ELSE ! nsoilt == nsurft
    !Soil tiles map directly on to surface tiles
    mm = n
  END IF !nsoilt

  !=============================================================================
  ! *END NOTICE REGARDING SOIL TILING**
  !=============================================================================
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    IF ( fqw_surft(l,n)  >   0.0 ) THEN
      esoil_surft(l,n) = (1.0 - flake(l,n)) *                                  &
                         (1.0 - fracaero_t(l,n)) * resfs(l,n) * fqw_surft(l,n) &
                                                           / resft(l,n)
      IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
        sf_diag%et_stom_surft(l,n) = (1.0 - flake(l,n)) *                      &
                                     (1.0 - fracaero_t(l,n))                   &
                                     *sf_diag%resfs_stom(l,n)                  &
                                     *fqw_surft(l,n) / resft(l,n)
      END IF
      elake_surft(l,n) = flake(l,n) * fqw_surft(l,n) / resft(l,n)
      ! With the fix, this has already been calculated and adjusted for
      ! exhaustion of the snow store.
      IF ( .NOT. l_fix_neg_snow)                                               &
        ecan_surft(l,n) = (1.0 - flake(l,n)) *                                 &
                          fracaero_t(l,n) * fqw_surft(l,n) / resft(l,n)
      edt = ecan_surft(l,n) * timestep
      IF ( edt  >   canopy(l,n) ) THEN
        IF (l_fix_neg_snow) THEN
          esoil_surft(l,n) = esoil_surft(l,n) + resfs(l,n) *                   &
                             (ecan_surft(l,n) - canopy(l,n) / timestep)
          IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
            !           Check that we can use ecan_surft here.
            sf_diag%et_stom_surft(l,n) = sf_diag%et_stom_surft(l,n) +          &
                                         sf_diag%resfs_stom(l,n) *             &
                                         (ecan_surft(l,n) -                    &
                                          canopy(l,n) / edt)
          END IF
        ELSE
          esoil_surft(l,n) = (1.0 - flake(l,n)) *                              &
                             (1.0 - fracaero_t(l,n) * canopy(l,n) / edt) *     &
                                 resfs(l,n) * fqw_surft(l,n) / resft(l,n)
          IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
            sf_diag%et_stom_surft(l,n) = (1.0 - flake(l,n)) *                  &
                                         (1.0 - fracaero_t(l,n) *              &
                                          canopy(l,n) / edt) *                 &
                                          sf_diag%resfs_stom(l,n) *            &
                                          fqw_surft(l,n) / resft(l,n)
          END IF
        END IF
        ecan_surft(l,n) = canopy(l,n) / timestep
      END IF
    ELSE IF (snow_surft(l,n) <= 0.0) THEN
      IF (tstar_surft(l,n) >= tm) THEN
        ecan_surft(l,n) = (1.0 - flake(l,n)) * fqw_surft(l,n)
        elake_surft(l,n) = flake(l,n) * fqw_surft(l,n)
      ELSE
        ei_surft(l,n) =  fqw_surft(l,n)
        ! With the fix, ecan_surft will have been initialized and must be
        ! zeroed.
        IF (l_fix_neg_snow)                                                    &
          ecan_surft(l,n) = 0.0
      END IF
    END IF
    ecan(i,j) = ecan(i,j) + tile_frac(l,n) * ecan_surft(l,n)
    IF ( .NOT. ((l_flake_model) .AND. (n == lake) .AND. (nsoilt == 1)) ) THEN
      esoil_soilt(i,j,mm) = esoil_soilt(i,j,mm) +                              &
                          tile_frac(l,n) * esoil_surft(l,n)
    END IF
    IF (sf_diag%l_et_stom .OR. sf_diag%l_et_stom_surft) THEN
      sf_diag%et_stom_ij(i,j) = sf_diag%et_stom_ij(i,j) + tile_frac(l,n)       &
                                *sf_diag%et_stom_surft(l,n)
    END IF

    IF ( l_irrig_dmd ) THEN

      ! split esoil_surft into irrigated and non-irrigated fraction
      ! for irrigated fraction, esoil_surft scaled according to
      ! resfs_irr_surft/resfs
      IF ( resfs(l,n) > 0.0 ) THEN
        esoil_irr_surft(l,n) = esoil_surft(l,n) *                              &
                              resfs_irr_surft(l,n) / resfs(l,n)
      ELSE
        esoil_irr_surft(l,n) = esoil_surft(l,n)
      END IF

      ! for non-irrigated fraction, assume total esoil_surft is
      ! linear combination of irrigated and non-irrigated fraction
      IF ( frac_irr_surft(l,n) <= EPSILON(1.0) ) THEN
        ! but when irrigated fraction is (close to) 0
        esoil_nir_surft(l,n) = esoil_surft(l,n)
      ELSE IF ( frac_irr_surft(l,n) < 1.0 ) THEN
        esoil_nir_surft(l,n) = ( esoil_surft(l,n) -                            &
                                frac_irr_surft(l,n) * esoil_irr_surft(l,n) ) / &
                              (1.0 - frac_irr_surft(l,n) )
      ELSE
        esoil_nir_surft(l,n) = 0.0
      END IF

      ! hadrd - because ESOIL_IRR_surft is scaled according to
      ! resfs_irr_surft/resfs, the contribution of the irrigated fraction
      ! *in this tile* may (in some cases) exceed the total tile ESOIL_surft,
      ! and ESOIL_NIR_surft may become negative.
      ! Including the line below prevents this, and gives an ESOIL_NIR_surft
      ! that (at the 1-2 grid boxes where this has been checked) is closer to
      ! ESOIL_surft when irrigation is switched off. However, it may be worth
      ! pointing out that ESOIL_NIR_surft can still turn out to be less than
      ! ESOIL_surft without irrigation.
      esoil_nir_surft(l,n) = MAX( esoil_nir_surft(l,n), 0.0 )

      ! hadrd - (TILE_FRAC*FRAC_IRR_surft) gives value for total grid box, not
      ! the irrigated fraction only to convert to irrigated fraction of total
      ! gridbox, multiply by (TILE_FRAC*FRAC_IRR_surft)/FRAC_IRR
      IF ( frac_irr_soilt(l,mm) > EPSILON(1.0) ) THEN
        esoil_irr_soilt(i,j,mm) = esoil_irr_soilt(i,j,mm) +                    &
                                  tile_frac(l,n) *                             &
                                  frac_irr_surft(l,n) *                        &
                                  esoil_irr_surft(l,n) / frac_irr_soilt(l,mm)
      ELSE
        esoil_irr_soilt(i,j,mm) = 0.0
      END IF

      IF ( frac_irr_soilt(l,mm) > EPSILON(1.0) ) THEN

        ! hadrd - same for non-irrigated fraction
        IF ( 1.0 - frac_irr_soilt(l,mm) > EPSILON(1.0) ) THEN
          esoil_nir_soilt(i,j,mm) = esoil_nir_soilt(i,j,mm) +                  &
                                    tile_frac(l,n) * (1.0 -                    &
                                    frac_irr_surft(l,n)) *                     &
                                    esoil_nir_surft(l,n)                       &
                                    / (1.0 - frac_irr_soilt(l,mm))
        ELSE
          esoil_nir_soilt(i,j,mm) = 0.0
        END IF
      ELSE
        esoil_nir_soilt(i,j,mm) = esoil_soilt(i,j,mm)
      END IF

      ! total smc_soilt is linear combination of smc_soilt in irrig and
      ! non-irrig fraction
      ! hadrd - this could/should be done outside loop over tiles
      IF ( smc_irr_soilt(l,mm) < 0.0 ) smc_irr_soilt(l,mm) = 0.0
      smc_nir_soilt(l,mm) = smc_irr_soilt(l,mm)
      IF ( frac_irr_soilt(l,mm) < 1.0 ) THEN
        smc_nir_soilt(l,mm) = (smc_soilt(l,mm) - frac_irr_soilt(l,mm) *        &
                               smc_irr_soilt(l,mm))                            &
                              /(1.0 - frac_irr_soilt(l,mm))
        ! hadrd - added case where total grid box frac_irr_soilt is 1.0
      ELSE
        smc_nir_soilt(l,mm) = 0.0
      END IF

      IF ( frac_irr_soilt(l,mm) <= EPSILON(1.0) ) THEN
        smc_nir_soilt(l,mm) = smc_soilt(l,mm)
      END IF

      IF ( smc_nir_soilt(l,mm) < 0.0 ) smc_nir_soilt(l,mm) = 0.0
        ! this may not be necessary?

    END IF ! l_irrig_dmd

  END DO !surft_pts
!$OMP END DO
END DO !nsurft

IF ((nsoilt == 1) .AND. (l_flake_model)) THEN
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    IF (non_lake_frac(l) > EPSILON(0.0)) THEN
      j=(land_index(l) - 1) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length
      esoil_soilt(i,j,1) = esoil_soilt(i,j,1) / non_lake_frac(l)
    END IF
  END DO
!$OMP END DO
END IF

!-----------------------------------------------------------------------
! Soil evapotranspiration
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length

  IF ( l_irrig_dmd ) THEN
    IF (nsoilt == 1) THEN
      mm = 1

      ! non-irrigated fraction
      edt = esoil_nir_soilt(i,j,mm) * timestep
      IF ( edt > smc_nir_soilt(l,mm) ) THEN
        DO n = 1,nsurft
          esoil_nir_surft(l,n) = smc_nir_soilt(l,mm) * esoil_nir_surft(l,n)    &
                                 / edt
        END DO
        esoil_nir_soilt(i,j,mm) = smc_nir_soilt(l,mm) / timestep
      END IF

      ! irrigated fraction
      edt = esoil_irr_soilt(i,j,mm) * timestep
      IF ( edt > smc_irr_soilt(l,mm) ) THEN
        DO n = 1,nsurft
          esoil_irr_surft(l,n) = smc_irr_soilt(l,mm) * esoil_irr_surft(l,n)    &
                                  / edt
        END DO
        esoil_irr_soilt(i,j,mm) = smc_irr_soilt(l,mm) / timestep
      END IF

      ! combine irrigated and non-irrigated fractions
      DO n = 1,nsurft
        esoil_surft(l,n) = esoil_nir_surft(l,n)
        IF ( frac_irr_surft(l,n) > EPSILON(1.0) ) THEN
          esoil_surft(l,n) = frac_irr_surft(l,n) * esoil_irr_surft(l,n) +      &
                            (1.0 - frac_irr_surft(l,n) ) * esoil_nir_surft(l,n)
        END IF

        esoil_soilt(i,j,mm) = esoil_nir_soilt(i,j,mm)
        IF ( frac_irr_soilt(l,mm) > EPSILON(1.0) ) THEN
          esoil_soilt(i,j,mm) = frac_irr_soilt(l,mm) * esoil_irr_soilt(i,j,mm) &
                                + (1.0 - frac_irr_soilt(l,mm) ) *              &
                                esoil_nir_soilt(i,j,mm)
        END IF

      END DO
    ELSE

      DO n = 1, nsurft
        mm = n

        ! non-irrigated fraction
        edt = esoil_nir_soilt(i,j,mm) * timestep
        IF ( edt > smc_nir_soilt(l,mm) ) THEN
          esoil_nir_surft(l,n) = smc_nir_soilt(l,mm) * esoil_nir_surft(l,n)    &
                                  / edt
          esoil_nir_soilt(i,j,mm) = smc_nir_soilt(l,mm) / timestep
        END IF

        ! irrigated fraction
        edt = esoil_irr_soilt(i,j,mm) * timestep
        IF ( edt > smc_irr_soilt(l,mm) ) THEN
          esoil_irr_surft(l,n) = smc_irr_soilt(l,mm) * esoil_irr_surft(l,n)    &
                                  / edt
          esoil_irr_soilt(i,j,mm) = smc_irr_soilt(l,mm) / timestep
        END IF

        ! combine irrigated and non-irrigated fractions
        esoil_surft(l,n) = esoil_nir_surft(l,n)
        IF ( frac_irr_surft(l,n) > EPSILON(1.0) ) THEN
          esoil_surft(l,n) = frac_irr_surft(l,n) * esoil_irr_surft(l,n) +      &
                            (1.0 - frac_irr_surft(l,n) ) * esoil_nir_surft(l,n)
        END IF

        esoil_soilt(i,j,mm) = esoil_nir_soilt(i,j,mm)
        IF ( frac_irr_soilt(l,mm) > EPSILON(1.0) ) THEN
          esoil_soilt(i,j,mm) = frac_irr_soilt(l,mm) * esoil_irr_soilt(i,j,mm) &
                                + (1.0 - frac_irr_soilt(l,mm) )                &
                                * esoil_nir_soilt(i,j,mm)
        END IF
      END DO !nsurft

    END IF !nsoilt == 1

  ELSE ! l_irrig_dmd is F

    ! revert to original code

    IF (nsoilt == 1) THEN
      mm = 1

      edt = esoil_soilt(i,j,mm) * timestep
      IF ( edt > smc_soilt(l,mm) ) THEN
        DO n = 1,nsurft
          esoil_surft(l,n) = smc_soilt(l,mm) * esoil_surft(l,n) / edt
        END DO
        esoil_soilt(i,j,mm) = smc_soilt(l,mm) / timestep
      END IF

    ELSE

      DO n = 1, nsurft
        mm = n
        edt = esoil_soilt(i,j,mm) * timestep
        IF ( edt > smc_soilt(l,mm) ) THEN
          esoil_surft(l,n) = smc_soilt(l,mm) * esoil_surft(l,n) / edt
          esoil_soilt(i,j,mm) = smc_soilt(l,mm) / timestep
        END IF
      END DO !nsurft
    END IF !nsoilt == 1

  END IF ! l_irrig_dmd
END DO
!$OMP END DO NOWAIT

!-----------------------------------------------------------------------
! Extraction of water from each layer
!-----------------------------------------------------------------------

DO m = 1,sm_levels
!$OMP DO SCHEDULE(STATIC)
  DO l = 1,land_pts
    ext_soilt(l,:,m) = 0.0
    ext_irr_soilt(l,:,m) = 0.0
  END DO
!$OMP END DO
END DO

! Here we need to do slightly different calculations of ext_soilt depending
! on whether soil tiling is being used. This is to prevent double-counting
! of the tile fraction.

DO m = 1,sm_levels
  DO n = 1,nsurft
    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      mm = 1
      IF ( .NOT. ((l_flake_model) .AND. (n == lake)) ) THEN
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          l = surft_index(k,n)
          ext_soilt(l,mm,m) = ext_soilt(l,mm,m)                                &
                            + tile_frac(l,n) * wt_ext_surft(l,m,n)             &
                            * esoil_surft(l,n)
        END DO
!$OMP END DO
      END IF
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      mm = n
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,surft_pts(n)
        l = surft_index(k,n)
        ext_soilt(l,mm,m) = ext_soilt(l,mm,m)                                  &
                            + wt_ext_surft(l,m,n)                              &
                            * esoil_surft(l,n)
      END DO
!$OMP END DO
    END IF !nsoilt
  END DO !nsurft
  IF ((nsoilt == 1) .AND. (l_flake_model)) THEN
    ! Normalise ext_soilt, excluding the lake tile fraction.
!$OMP DO SCHEDULE(STATIC)
    DO l = 1,land_pts
      IF (non_lake_frac(l) > EPSILON(0.0)) THEN
        ext_soilt(l,1,m) = ext_soilt(l,1,m) / non_lake_frac(l)
      END IF
    END DO
!$OMP END DO
  END IF
END DO !sm_levels

! Do similar for irrigation, if required
IF (l_irrig_dmd) THEN
  DO m = 1,sm_levels
    DO n = 1,nsurft

      !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        mm = 1
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          l = surft_index(k,n)
          IF ( frac_irr_soilt(l,mm) > EPSILON(1.0) ) THEN
            ext_irr_soilt(l,mm,m) = ext_irr_soilt(l,mm,m)                      &
                                    + tile_frac(l,n)                           &
                                    * wt_ext_irr_surft(l,m,n)                  &
                                    * esoil_irr_surft(l,n)                     &
                                    * frac_irr_surft(l,n)                      &
                                    / frac_irr_soilt(l,mm)
            !with original code, FRAC_IRR_surft(L,N)/FRAC_IRR(L)
            !is always 1.0 since frac_irr_soilt is the same in all tiles
          END IF
        END DO !surft_pts
!$OMP END DO

      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        mm = n
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,surft_pts(n)
          l = surft_index(k,n)
          IF ( frac_irr_soilt(l,mm) > EPSILON(1.0) ) THEN
            ext_irr_soilt(l,mm,m) = ext_irr_soilt(l,mm,m)                      &
                                    + wt_ext_irr_surft(l,m,n)                  &
                                    * esoil_irr_surft(l,n)                     &
                                    * frac_irr_surft(l,n)                      &
                                    / frac_irr_soilt(l,mm)
            !with original code, FRAC_IRR_surft(L,N)/FRAC_IRR(L)
            !is always 1.0 since frac_irr_soilt is the same in all tiles
          END IF
        END DO !surft_pts
!$OMP END DO
      END IF !nsoilt
    END DO !nsurft
  END DO !sm_levels
END IF !l_irrig_dmd

!-----------------------------------------------------------------------
! Calculate increments to surface heat fluxes, moisture fluxes and
! temperatures
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  dftl(l) = 0.0
  dfqw(l) = 0.0
END DO
!$OMP END DO

DO n = 1,nsurft
!$OMP DO SCHEDULE(STATIC)
  DO k = 1,surft_pts(n)
    l = surft_index(k,n)
    j=(land_index(l) - 1) / t_i_length + 1
    i = land_index(l) - (j-1) * t_i_length
    rhokh1_prime = 1.0 / ( 1.0 / rhokh_1(l,n)                                  &
                       + r_gamma * dtrdz_1(i,j) )
    diff_lat_htf = (lc + lf) * ei_surft(l,n) + lc * ecan_surft(l,n)            &
                    + lc * esoil_surft(l,n) + lc * elake_surft(l,n)            &
                    - le_surft_old(l,n)
    dtstar = - diff_lat_htf /                                                  &
                ( cp * rhokh1_prime + ashtf_prime_surft(l,n) )
    diff_sens_htf = cp * rhokh1_prime * dtstar
    ftl_surft(l,n) = ftl_surft(l,n) + diff_sens_htf
    tstar_surft(l,n) = tstar_surft(l,n) + dtstar
    dftl(l) = dftl(l) + tile_frac(l,n) * diff_sens_htf
    dfqw(l) = dfqw(l) + tile_frac(l,n) * ( ecan_surft(l,n) +                   &
                  esoil_surft(l,n) + ei_surft(l,n) + elake_surft(l,n)          &
                  - e_surft_old(l,n) )
  END DO
!$OMP END DO
END DO

!-----------------------------------------------------------------------
! Update level 1 temperature and humidity and GBM heat and moisture
! fluxes due to limited moisture availability
!-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC)
DO l = 1,land_pts
  j=(land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length
  ftl_1(i,j) = ftl_1(i,j) + fland(l) * dftl(l)
  fqw_1(i,j) = fqw_1(i,j) + fland(l) * dfqw(l)
END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sf_evap
END MODULE sf_evap_mod
