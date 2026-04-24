! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Routine to calculate albedos of land-surface tiles and gridbox-mean
! albedo for JULES.

MODULE jules_land_albedo_mod

USE calc_direct_albsoil_mod, ONLY: calc_direct_albsoil

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_LAND_ALBEDO_MOD'

CONTAINS

SUBROUTINE jules_land_albedo(                                                  &
        pfield,                                                                &
        !INTENT(IN)
        land_pts, nsurft,                                                      &
        land_index, surft_pts, surft_index,                                    &
        albsoil_soilt, albobs_sw_gb, albobs_vis_gb, albobs_nir_gb,             &
        cosz_gb, soot_gb, ho2r2_orog_gb,                                       &
        lai_pft, canht_pft,                                                    &
        rgrain_surft, snow_surft, tstar_surft, z0_surft, frac_surft,           &
        !INTENT(OUT)
        alb_surft,albobs_sc_ij,fsnow,land_albedo_ij,                           &
        !New arguments replacing USE statements
        !jules_mod (IN OUT)
        albobs_scaling_surft,                                                  &
        !jules_mod (OUT)
        snowdep_surft,                                                         &
        !urban_param (IN)
        albwl_gb, albrd_gb, hwr_gb,                                            &
        !lake_mod (IN)
        lake_h_ice_gb,                                                         &
        !ancil_info (IN)
        l_lice_point, l_lice_surft,                                            &
        !prognostics (IN)
        snowdepth_surft, rho_snow_grnd_surft, nsnow_surft, sice_surft,         &
        sliq_surft, ds_surft)

!Use in subroutines
USE albpft_mod,               ONLY: albpft
USE albsnow_mod,              ONLY: albsnow
USE albsnow_ts_mod,           ONLY: albsnow_ts
USE canyonalb_mod,            ONLY: canyonalb

!Use in science variables
USE nvegparm,                 ONLY:                                            &
  albsnf_nvg, albsnc_nvg, albsnf_nvgl, albsnf_nvgu

USE pftparm,                  ONLY:                                            &
  lai_alb_lim, kext, albsnc_max, albsnc_min, albsnf_max, albsnf_maxl,          &
  albsnf_maxu

USE jules_snow_mod,           ONLY:                                            &
  kland, maskd, tcland, rho_snow_const, rho_snow_fresh, cansnowtile,           &
  l_snowdep_surf, can_clump, lai_alb_lim_sn, n_lai_exposed, amax, aicemax,     &
  i_snow_tile, rho_firn_albedo, nsmax

USE jules_surface_types_mod,  ONLY:                                            &
  npft, ntype, ice, lake, soil, urban_canyon

USE water_constants_mod,      ONLY:                                            &
  rho_ice, tm

USE jules_surface_mod,        ONLY:                                            &
  l_point_data, l_flake_model, l_aggregate, l_elev_land_ice

USE jules_radiation_mod,      ONLY:                                            &
  l_spec_albedo, l_spec_alb_bs, l_albedo_obs, l_embedded_snow,                 &
  l_mask_snow_orog, l_snow_albedo,                                             &
  l_partition_albsoil, ratio_albsoil, swdn_frac_albsoil, l_hapke_soil

USE jules_science_fixes_mod, ONLY: l_fix_snow_frac

USE jules_vegetation_mod,     ONLY:                                            &
  can_model

USE jules_urban_mod,          ONLY:                                            &
  l_moruses_albedo_surft

USE lake_mod,                 ONLY:                                            &
  albedo_whiteice_ref, albedo_blueice_ref, c_albice_MR

USE ancil_info,               ONLY:                                            &
  nsoilt, rad_nband

USE set_soil_alb_components_mod, ONLY: set_soil_alb_components

!Non-science modules
USE jules_print_mgr,          ONLY: jules_message, jules_print
USE errormessagelength_mod,   ONLY: errormessagelength
USE ereport_mod,              ONLY: ereport
USE missing_data_mod,         ONLY: rmdi, imdi

USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!Subroutine arguments
!Scalar arguments with intent(in):
INTEGER, INTENT(IN) ::                                                         &
  pfield,                                                                      &
                                      !No of ixj points
  land_pts,                                                                    &
                                      !No. of land points.
  nsurft
                                      !Number of surface tiles.

!  Array arguments with intent(in):
INTEGER, INTENT(IN) ::                                                         &
  land_index(land_pts),                                                        &
                                      !Index of land points.
  surft_pts(ntype),                                                            &
                                      !Number of land points which
                                      !include the nth surface type.
  surft_index(land_pts,ntype)
                                      !Indices of land points which include
                                      !the nth surface type.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  albsoil_soilt(land_pts,nsoilt),                                              &
                                      !Soil albedo.
  albobs_sw_gb(land_pts),                                                      &
                                      !Observed snow-free sw albedo.
  albobs_vis_gb(land_pts),                                                     &
                                      !Observed snow-free vis albedo.
  albobs_nir_gb(land_pts),                                                     &
                                      !Observed snow-free nir albedo.
  cosz_gb(land_pts),                                                           &
                                      !Cosine of the zenith angle.
  soot_gb(land_pts),                                                           &
                                      !Snow soot content (kg/kg).
  ho2r2_orog_gb(land_pts),                                                     &
                                      !Standard deviation of surface orography
  lai_pft(land_pts,npft),                                                      &
                                      !Leaf area index.
  canht_pft(land_pts,npft),                                                    &
                                      !Canopy height
  rgrain_surft(land_pts,nsurft),                                               &
                                      !Snow grain size on tiles (microns).
  snow_surft(land_pts,nsurft),                                                 &
                                      !Canopy snow on tiles (kg/m2)
  tstar_surft(land_pts,nsurft),                                                &
                                      !Tile surface temperatures (K).
  z0_surft(land_pts,nsurft),                                                   &
                                      !Surface roughness on tiles (m).
  frac_surft(land_pts,ntype)
                                      !Fractional cover of each surface type.


!Array arguments with intent(out):
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  alb_surft(land_pts,nsurft,4),                                                &
                                      !Albedos for surface tiles.
                                      !  (*,*,1) - Direct beam visible
                                      !  (*,*,2) - Diffuse visible
                                      !  (*,*,3) - Direct beam near-IR
                                      !  (*,*,4) - Diffuse near-IR
  albobs_sc_ij(pfield,nsurft,2),                                               &
                              ! albedo scaling to obs in VIS and NIR
                              ! for diagnostics output by the UM
  fsnow(land_pts,nsurft),                                                      &
                              ! Snow cover fraction on tiles
  land_albedo_ij(pfield,4)
                              ! GBM albedos.

!New arguments replacing USE statements
!jules_mod (IN OUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: albobs_scaling_surft(land_pts,ntype,rad_nband)
!jules_mod (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: snowdep_surft(land_pts,nsurft)
!urban_param (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: albwl_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN) :: albrd_gb(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN) :: hwr_gb(land_pts)
!lake_mod (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: lake_h_ice_gb(land_pts)
!ancil_info (IN)
LOGICAL, INTENT(IN) :: l_lice_point(land_pts)
LOGICAL, INTENT(IN) :: l_lice_surft(ntype)
!prognostics (IN)
INTEGER, INTENT(IN) :: nsnow_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: snowdepth_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: rho_snow_grnd_surft(land_pts,nsurft)
REAL(KIND=real_jlslsm), INTENT(IN) :: sice_surft(land_pts,nsurft,nsmax)
REAL(KIND=real_jlslsm), INTENT(IN) :: sliq_surft(land_pts,nsurft,nsmax)
REAL(KIND=real_jlslsm), INTENT(IN) :: ds_surft(land_pts,nsurft,nsmax)

!Local variables:
INTEGER, PARAMETER ::       ilayers_dummy = 1

INTEGER ::                                                                     &
  n_surft_pft,                                                                 &
                                      !Index in a tiled array indicating
                                      !which tile contains the PFT being
                                      !considered at that time.
  n_surft_nvg,                                                                 &
                                      !Index in a tiled array indicating
                                      !which tile contains the unvegetated
                                      !surface being considered at that time.
  albpft_call = imdi,                                                          &
                                      !Option for call to albpft, matching
                                      !the internal one
  band,i,j,k,l,n,m
                                      !Loop counters

!Local scalars:
REAL(KIND=real_jlslsm) ::                                                      &
  dsa,                                                                         &
                                      !Deep-snow albedo.
  flit,                                                                        &
                                      !Weighting factor for albedo.
  mask_orog,                                                                   &
                                      !Orographic masking factor
  snowdepth_eff,                                                               &
                                      !Effective snow depth
  rho_snow_surf,                                                               &
  snow_alb_vis_as,                                                             &
  snow_alb_nir_as,                                                             &
  ssum

LOGICAL ::                                                                     &
  l_getprofile,                                                                &
                                      !Switch IN to albpft
  l_infer_direct
                                      !Switch to apply calculation of direct
                                      !albedo

INTEGER ::                                                                     &
  snow_pts(ntype),                                                             &
                                      !Number of points with snow cover
  snow_index(land_pts,ntype)
                                      !List of points with snow cover

REAL(KIND=real_jlslsm) ::                                                      &
  albsnc(land_pts,ntype),                                                      &
                                      !Snow-covered albedo of surf types.
  albsnf(land_pts,ntype),                                                      &
                                      !Snow-free albedo of surf types.
  albsfsc(land_pts,2),                                                         &
                                      !local scaling factor to match obs
  albobs_surft(land_pts,ntype),                                                &
                                      !Albedo of the tiles (full veg) after
                                      !being scaled to obs
  alb_type(land_pts,ntype,4),                                                  &
                                      !Albedos of surface types.
  snow_mass_alb(land_pts),                                                     &
                                      !Mass of snow in calculation of albedo
  alb_snow(land_pts,ntype,4),                                                  &
                                      !Snow albedos.
  alb_snow_surft(land_pts,4),                                                  &
                                      !Tiled snow albedo: used with embedded
                                      !snow
  lai(land_pts,npft),                                                          &
                                      !Adjusted leaf area index.
  snowd(land_pts),                                                             &
                                      !Work variable (snow depth).
  tstar(land_pts),                                                             &
                                      !Copy of TSTAR_SURFT.
  z0(land_pts),                                                                &
                                      !Copy of Z0_SURFT.
  albsfm_sw(land_pts),                                                         &
                                      !Model grid-box mean sf sw albedo
  albsfm_vis(land_pts),                                                        &
                                      !Model grid-box mean sf vis albedo
  albsfm_nir(land_pts),                                                        &
                                      !Model grid-box mean sf nir albedo
  fapar_dir_dummy(land_pts,npft,ilayers_dummy),                                &
                                      !DUMMY
  fapar_dif_dummy(land_pts,npft,ilayers_dummy),                                &
                                      !DUMMY
  fapar_dir2dif_dummy(land_pts,npft,ilayers_dummy),                            &
                                      !DUMMY
  fapar_dif2dif_dummy(land_pts,npft,ilayers_dummy),                            &
                                      !DUMMY
  fapar_dir2dir_dummy(land_pts,npft,ilayers_dummy),                            &
                                      !DUMMY
  fsun_dummy(land_pts,npft,ilayers_dummy),                                     &
                                      !DUMMY
  albudir(land_pts,2,ntype),                                                   &
                                      !Direct albedo of underlying surface
  albudif(land_pts,2,ntype)
                                      !Diffuse albedo of underlying surface

REAL(KIND=real_jlslsm) :: disaggregate_albsoil(2)
                                      !Disaggregation coefficients for soil
                                      !albedo: these are for real bands
                                      !(Beware of alternative use of band
                                      !to encode the split between direct
                                      !and diffuse too.)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_LAND_ALBEDO'

INTEGER ::              errcode                 !Error code
CHARACTER(LEN=errormessagelength) :: ERRMSG     !Error message text

!End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Set appropriate value of l_getprofile for all calls to albpft
l_getprofile = .FALSE.

! Set up partitioning of the soil albedo
IF (l_partition_albsoil) THEN
  disaggregate_albsoil(1) = 1.0 /                                              &
    (1.0 + swdn_frac_albsoil * (ratio_albsoil-1.0))
  disaggregate_albsoil(2) = ratio_albsoil /                                    &
    (1.0 + swdn_frac_albsoil * (ratio_albsoil-1.0))
ELSE
  disaggregate_albsoil(:) = 1.0
END IF

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(j, l, n, n_surft_pft, band, i, k)         &
!$OMP SHARED(ntype, land_pts, albsnc, nsurft, alb_surft, alb_type, alb_snow,   &
!$OMP        snowdep_surft, snowdepth_surft, can_model, cansnowtile,           &
!$OMP        l_snowdep_surf, snow_surft, rho_snow_const, surft_pts, npft,      &
!$OMP        surft_index, lai, lai_pft, lai_alb_lim, lai_alb_lim_sn,           &
!$OMP        l_aggregate, rad_nband, albobs_scaling_surft, l_albedo_obs,       &
!$OMP        pfield, albobs_sc_ij)
DO n = 1, ntype
!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_pts
    !Initialise to zero to allow for moruses
    albsnc(l,n) = 0.0
  END DO
!$OMP END DO NOWAIT
END DO
DO band = 1, 4
  DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      alb_surft(l,n,band) = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END DO
DO band = 1, 4
  DO n = 1, ntype
!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      alb_type(l,n,band) = 0.0
      alb_snow(l,n,band) = 0.0
    END DO
!$OMP END DO NOWAIT
  END DO
END DO

! Equivalent snowdepth_surft for surface calculations.
DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
  DO l = 1, land_pts
    snowdep_surft(l,n) = snowdepth_surft(l,n)
  END DO
!$OMP END DO
END DO
DO n = 1, nsurft
  IF ( (can_model == 4) .AND. cansnowtile(n) .AND. l_snowdep_surf ) THEN
!$OMP DO SCHEDULE(STATIC)
    DO l = 1, land_pts
      snowdep_surft(l,n) = snow_surft(l,n) / rho_snow_const
    END DO
!$OMP END DO
  END IF
END DO

! Impose minimum LAI for bare vegetation
DO n = 1,npft
  ! Although this loop is over PFTs, with aggregation we
  ! have only one tile and snow loading, so point to the
  ! tile associated with the PFT.
  IF ( .NOT. l_aggregate) THEN
    n_surft_pft = n
  ELSE
    n_surft_pft = 1
  END IF
!$OMP DO SCHEDULE(STATIC)
  DO j = 1, surft_pts(n)
    l = surft_index(j,n)
    IF (snowdep_surft(l,n_surft_pft) > EPSILON(0.0)) THEN
      lai(l,n) = MAX( lai_pft(l,n), lai_alb_lim_sn(n) )
    ELSE
      lai(l,n) = MAX( lai_pft(l,n), lai_alb_lim(n) )
    END IF
  END DO
!$OMP END DO NOWAIT
END DO

! scaling factor to get the model albedo to agree with the obs
! (initialise it as it goes into the module for use elsewhere)
IF (l_albedo_obs) THEN
  DO k = 1, rad_nband
    DO j = 1, ntype
!$OMP DO SCHEDULE(STATIC)
      DO l = 1, land_pts
        albobs_scaling_surft(l,j,k) = 1.0
      END DO
!$OMP END DO NOWAIT
    END DO
  END DO
END IF
DO k = 1, 2
  DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
    DO i = 1, pfield
      albobs_sc_ij(i,n,k) = rmdi
    END DO
!$OMP END DO NOWAIT
  END DO
END DO
!$OMP END PARALLEL

IF (l_spec_albedo) THEN
  !----------------------------------------------------------------------
  ! Spectral albedo scheme, can have prognostic or diagnostic snow albedo
  !----------------------------------------------------------------------

  ! Set albedos of vegetated surface types
  ! The logical argument (getProfile in albpft) is FALSE to indicate
  ! that profiles through the canopy should not be calculated.
  ! The 0 indicates that no scaling to obs is required
  ! Set the underlying albedo to the values for bare soil.

  !============================================================================
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
  !============================================================================

  DO n = 1,npft

    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = n
    END IF !nsoilt

    CALL set_soil_alb_components(land_pts,                                     &
      albsoil_soilt(:,m), cosz_gb,                                             &
      albudir(:,:,n), albudif(:,:,n))

  END DO


  albpft_call = 0
  CALL albpft(                                                                 &
    !INTENT(IN)
    l_getprofile,                                                              &
    land_pts, ilayers_dummy, albpft_call,                                      &
    surft_pts, surft_index,                                                    &
    cosz_gb, lai, albudir, albudif,                                            &
    !INTENT(INOUT)
    alb_type,                                                                  &
    !INTENT(OUT)
    fapar_dir_dummy, fapar_dif_dummy, fapar_dir2dif_dummy,                     &
    fapar_dif2dif_dummy, fapar_dir2dir_dummy, fsun_dummy,                      &
    !New arguments replacing USE statements
    !jules_mod (IN OUT)
    albobs_scaling_surft)

  ! Set albedos of non-vegetated surface types
  DO band = 1,4
    l_infer_direct = (l_hapke_soil) .AND. (MOD(band, 2) > 0)
    DO n = npft+1,ntype
      IF ( .NOT. l_moruses_albedo_surft(n) ) THEN
        !Set the current soil tile (see notice above)
        IF (nsoilt == 1) THEN
          !There is only 1 soil tile
          m = 1
        ELSE ! nsoilt == nsurft
          !Soil tiles map directly on to surface tiles
          m = n
        END IF !nsoilt

        ! Disaggregate the soil albedo between the VIS and NIR and apply
        ! the zenith-angle dependence to the direct albedo.
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP PRIVATE(j,l)                                                             &
!$OMP SHARED(surft_pts,n,m,band,surft_index,alb_type,albsnf_nvg,npft,          &
!$OMP        albsoil_soilt,disaggregate_albsoil,l_infer_direct,cosz_gb)
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          alb_type(l,n,band) = albsnf_nvg(n - npft)
          IF ( albsnf_nvg(n - npft) <  0.0 ) THEN
            ! Soil tile
            ! In this loop band also denotes the split between direct and
            ! diffuse
            IF (albsoil_soilt(l,m) < 0.6) THEN
              alb_type(l,n,band) = albsoil_soilt(l,m) *                        &
                disaggregate_albsoil((band+1) / 2)
            ELSE
              alb_type(l,n,band) = albsoil_soilt(l,m)
            END IF
            IF ( l_infer_direct ) THEN
              IF ( cosz_gb(l) > EPSILON(cosz_gb) ) THEN
                alb_type(l,n,band) = calc_direct_albsoil(alb_type(l,n,band),   &
                                                         cosz_gb(l))
              END IF
            END IF
          END IF
        END DO
!$OMP END PARALLEL DO
      END IF ! .NOT. l_moruses_albedo; alb_type(urban_canyon) set by canyonalb
    END DO
  END DO

  ! MORUSES: Set alb_type with values from canyonalb. No snow is currently
  ! involved at the moment so all bands have the same albedo
  IF ( ANY( l_moruses_albedo_surft(:) ) ) THEN
    n = urban_canyon
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(j,l)                  &
!$OMP SHARED(surft_pts,n,surft_index,cosz_gb,hwr_gb,albwl_gb,albrd_gb,alb_type)
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      ! MORUSES does not have spectral albedos, therefore canyonalb is only
      ! called for the first spectral band then copied to the others. If snow
      ! is added to MORUSES, this will need to change.
      CALL canyonalb(cosz_gb(l),hwr_gb(l),albwl_gb(l),albrd_gb(l),             &
         alb_type(l,n,1))
      alb_type(l,n,2:4) = alb_type(l,n,1)
    END DO
!$OMP END PARALLEL DO
  END IF

  IF ( l_albedo_obs ) THEN
    ! Average the model snow-free diffuse albedos (2 for VIS and 4 for NIR)
    ! over the grid box, by the tile fraction:
!$OMP PARALLEL DO SCHEDULE(STATIC) IF(land_pts>1) DEFAULT(NONE) PRIVATE(l)     &
!$OMP SHARED(land_pts, albsfm_vis, albsfm_nir)
    DO l = 1, land_pts
      albsfm_vis(l) = 0.0
      albsfm_nir(l) = 0.0
    END DO
!$OMP END PARALLEL DO
    DO n = 1,ntype
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        albsfm_vis(l) = albsfm_vis(l) + ( alb_type(l,n,2) * frac_surft(l,n) )
        albsfm_nir(l) = albsfm_nir(l) + ( alb_type(l,n,4) * frac_surft(l,n) )
      END DO
    END DO

    ! Work out the scaling factor that needs to be applied to make the model
    ! snow-free albedo agree with the obs:
!$OMP PARALLEL DO SCHEDULE(STATIC) IF(land_pts>1) DEFAULT(NONE) PRIVATE(l)     &
!$OMP SHARED(land_pts, albsfm_vis, albsfm_nir, albobs_vis_gb, albobs_nir_gb,   &
!$OMP        albsfsc, l_lice_point)
    DO l = 1,land_pts
      albsfsc(l,1) = albobs_vis_gb(l) / albsfm_vis(l)
      albsfsc(l,2) = albobs_nir_gb(l) / albsfm_nir(l)
      ! If point is land ice then do not do anything:
      IF ( l_lice_point(l) ) albsfsc(l,:) = 1.0
    END DO
!$OMP END PARALLEL DO

    ! Recalculate the above albedos, but with scaled input albedos, within
    ! limits and store the scaling:
    !
    ! starting with the non-veg (only need to do calculations twice, as the
    ! diffuse and direct albedos are the same):
    DO band = 1,2
      DO n = npft+1,ntype
        IF ( .NOT. l_moruses_albedo_surft(n) ) THEN
          !Set the current soil tile (see notice above)
          IF (nsoilt == 1) THEN
            !There is only 1 soil tile
            m = 1
          ELSE ! nsoilt == nsurft
            !Soil tiles map directly on to surface tiles
            m = n
          END IF !nsoilt

          !       Set the diffuse albedo first and then the direct. (This
          !       is the opposite of the order in the original code, but is
          !       more natural when applying a zenith-angle dependence.)

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(j, l)                 &
!$OMP SHARED(n, surft_pts, surft_index, soil, band, alb_type, albsoil_soilt,   &
!$OMP        m, disaggregate_albsoil, albsfsc, albsnf_nvgl, albsnf_nvgu, npft, &
!$OMP        albobs_scaling_surft, albsnf_nvg)
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            IF ( n == soil ) THEN
              !           band denotes the real band here, ie. the spectral
              !           region, and not the diffuse or direct components
              !           as well.
              !
              !           Separate soil and land ice using the original
              !           broad-band albedo.
              IF (albsoil_soilt(l,m) < 0.6) THEN
                alb_type(l,n,2 * band) = MIN(MAX(albsoil_soilt(l,m) *          &
                                             disaggregate_albsoil(band) *      &
                                             albsfsc(l,band),                  &
                                             albsnf_nvgl(n - npft)),           &
                                             albsnf_nvgu(n - npft))

                albobs_scaling_surft(l,n,band) = alb_type(l,n,2 * band) /      &
                           (disaggregate_albsoil(band) * albsoil_soilt(l,m))
              ELSE
                alb_type(l,n,2 * band) = MIN(MAX(albsoil_soilt(l,m) *          &
                                             albsfsc(l,band),                  &
                                             albsnf_nvgl(n - npft)),           &
                                             albsnf_nvgu(n - npft))
                albobs_scaling_surft(l,n,band) = alb_type(l,n,2 * band) /      &
                                                 albsoil_soilt(l,m)
              END IF
            ELSE ! not soil
              alb_type(l,n,2 * band) =                                         &
                 MIN(MAX(albsnf_nvg(n - npft) * albsfsc(l,band),               &
                 albsnf_nvgl(n - npft)), albsnf_nvgu(n - npft))
              albobs_scaling_surft(l,n,band) =                                 &
                 alb_type(l,n,2 * band) / albsnf_nvg(n - npft)
            END IF
          END DO
!$OMP END PARALLEL DO
        END IF ! .NOT. l_moruses_albedo; alb_type(urban_canyon) set by canyonalb
      END DO
    END DO
    ! now fill in the direct albedo from the diffuse value:
    DO band = 1,2
      DO n = npft+1,ntype
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(j, l)                 &
!$OMP SHARED(n, surft_pts, surft_index, alb_type, band, l_hapke_soil, cosz_gb)
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          alb_type(l,n,2 * band-1) = alb_type(l,n,2 * band)
          IF ( l_hapke_soil .AND. (cosz_gb(l) > EPSILON(cosz_gb)) ) THEN
            alb_type(l,n,2 * band-1) =                                         &
              calc_direct_albsoil(alb_type(l,n,2 * band), cosz_gb(l))
          END IF
        END DO
!$OMP END PARALLEL DO
      END DO
    END DO

    !   now do the veg tiles, by calling albpft again and telling it to
    !   scale the vegetation scattering and reflectivity parameters:
    !
    !   Put the intended scaling into albobs_scaling_surft for each PFT
    !   and then albpft will correct it for the non-linearity
    !   (which is what the 1 indicates):
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(l, n, band)                               &
!$OMP SHARED(npft, land_pts, albobs_scaling_surft, albsfsc)
    DO band = 1,2
      DO n = 1,npft
!$OMP DO SCHEDULE(STATIC)
        DO l = 1,land_pts
          albobs_scaling_surft(l,n,band) = albsfsc(l,band)
        END DO
!$OMP END DO NOWAIT
      END DO
    END DO
!$OMP END PARALLEL
    !
    !   Scale the soil albedo using the provisional scaling
    DO n = 1,npft

      !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        m = 1
      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        m = n
      END IF !nsoilt

      CALL set_soil_alb_components(land_pts,                                   &
        albsoil_soilt(:,m), cosz_gb,                                           &
        albudir(:,:,n), albudif(:,:,n),                                        &
        albobs_scaling_surft(:,soil,1:2))

    END DO

    albpft_call = 1
    CALL albpft(                                                               &
      !INTENT(IN)
        l_getprofile, land_pts, ilayers_dummy, albpft_call,                    &
        surft_pts, surft_index,                                                &
        cosz_gb, lai, albudir, albudif,                                        &
      !INTENT(INOUT)
        alb_type,                                                              &
      !INTENT(OUT)
        fapar_dir_dummy, fapar_dif_dummy, fapar_dir2dif_dummy,                 &
        fapar_dif2dif_dummy, fapar_dir2dir_dummy, fsun_dummy,                  &
        !New arguments replacing USE statements
        !jules_mod (IN OUT)
        albobs_scaling_surft)

  END IF ! End of test on l_albedo_obs
  !
  ! ----------------------------------------------------------------
  ! If snow is embedded in the canopy calculate the albedo for the
  ! snow on the ground, recalculate for the exposed LAI and for
  ! any snow retained on the canopy. This is not compatible with
  ! the aggregation of tiled properties.
  ! ----------------------------------------------------------------
  IF (l_embedded_snow) THEN
    !
    DO n = 1, npft
      !
      !     Point to the tile associated with the PFT.
      IF ( .NOT. l_aggregate) THEN
        n_surft_pft = n
      ELSE
        n_surft_pft = 1
      END IF
      !
      !     Gather snow-covered points.
      snow_pts(n) = 0
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        !       Allow a numerical tolerance for checking snow depth.
        IF (snowdepth_surft(l,n_surft_pft) >                                   &
            128 * EPSILON(snowdepth_surft)) THEN
          snow_pts(n) = snow_pts(n) + 1
          snow_index(snow_pts(n),n) = l
        END IF
      END DO
      !
      !     Calculate the albedos for the snow on the ground.
      IF ( (can_model == 4) .AND. cansnowtile(n) ) THEN
        DO j = 1,snow_pts(n)
          l = snow_index(j,n)
          !         Provisional check on the density. In a future upgrade
          !         reconfiguration will ensure that the density of snow on
          !         the ground is properly initialized.
          snow_mass_alb(l) = MIN(rho_ice,                                      &
            MAX(rho_snow_grnd_surft(l,n_surft_pft),rho_snow_fresh) ) *         &
            snowdepth_surft(l,n_surft_pft)
        END DO
      ELSE
        DO j = 1,snow_pts(n)
          l = snow_index(j,n)
          snow_mass_alb(l) = snow_surft(l,n_surft_pft)
        END DO
      END IF

      !     The albedo of the underlying surface is now the scaled value for
      !     bare soil.
      CALL albsnow_ts(land_pts, snow_pts(n), snow_index(:,n),cosz_gb,          &
                      albudir(:,:,n),albudif(:,:,n),                           &
                      rgrain_surft(:,n_surft_pft),                             &
                      snow_mass_alb,soot_gb,alb_snow_surft)

      !     Prepare to calculate the albedo for the exposed canopy. Set
      !     the appropriate exposed LAI and set the snow albedo on
      !     the underlying surface.
      DO j = 1,snow_pts(n)
        l = snow_index(j,n)
        snowdepth_eff = MAX(0.0, snowdepth_surft(l,n_surft_pft))
        !
        IF (l_mask_snow_orog) THEN
          !         Include orographic masking, based on Roesch et al. (2006),
          !         J. Clim., vol. 19, p.3828. Note that they record the snow
          !         loading using SWE in m. Here we take a typical snow
          !         density as 200 kgm-3.
          snowdepth_eff = snowdepth_eff *                                      &
                ( snowdepth_eff /                                              &
                ( snowdepth_eff + EPSILON(1.0) +                               &
                                  0.00075 * ho2r2_orog_gb(l)) )
        END IF
        !
        IF (snowdepth_eff < canht_pft(l,n)) THEN
          lai(l,n) = lai(l, n) *                                               &
            (1.0 - snowdepth_eff / canht_pft(l,n) )** n_lai_exposed(n)
        ELSE
          lai(l,n) = 0.0
        END IF
        !
        albudir(l,1,n) = alb_snow_surft(l,1)
        albudif(l,1,n) = alb_snow_surft(l,2)
        albudir(l,2,n) = alb_snow_surft(l,3)
        albudif(l,2,n) = alb_snow_surft(l,4)
      END DO
    END DO
    !
    ! ----------------------------------------------------------------
    !   Calculate the albedo of the canopy using the exposed LAI.
    !   embeded snow is assumed).
    ! ----------------------------------------------------------------
    IF (l_albedo_obs) THEN
      !     We have already calculated the scaling without snow. Iterating
      !     again would scale the transmission and reflection coefficients
      !     by a snow albedo and would be wrong.
      albpft_call = 2
    ELSE
      !     Just calculate the albedo directly with the exposed LAI.
      albpft_call = 0
    END IF

    !   This call is made only at snow points, with the underlying albedo
    !   set for snow.
    CALL albpft(                                                               &
      !INTENT(IN)
        l_getprofile, land_pts, ilayers_dummy, albpft_call,                    &
        surft_pts, surft_index,                                                &
        cosz_gb, lai, albudir, albudif,                                        &
      !INTENT(INOUT)
        alb_type,                                                              &
      !INTENT(OUT)
        fapar_dir_dummy, fapar_dif_dummy, fapar_dir2dif_dummy,                 &
        fapar_dif2dif_dummy, fapar_dir2dir_dummy, fsun_dummy,                  &
        !New arguments replacing USE statements
        !jules_mod (IN OUT)
        albobs_scaling_surft)

    ! ----------------------------------------------------------------
    ! If embedded snow is assumed with a canopy model, allow for
    ! snow on the canopy. Then, assign final albedos.
    ! ----------------------------------------------------------------
    !
    DO n = 1, npft
      !
      !     Point to the tile associated with the PFT.
      IF ( .NOT. l_aggregate) THEN
        n_surft_pft = n
      ELSE
        n_surft_pft = 1
      END IF
      !
      !     We need only reset alb_type if there is a canopy model --
      !     at this point alb_type holds the albedo up to the top of
      !     the canopy, but excluding any snow thereon. Snow is clumped
      !     on to a fraction 1/can_clump of the canopy and the albedo
      !     is weighted accordingly.
      IF ( (can_model == 4) .AND. cansnowtile(n) ) THEN
        DO j = 1,snow_pts(n)
          l = snow_index(j,n)
          albudir(l,1,n) = alb_type(l,n,1)
          albudif(l,1,n) = alb_type(l,n,2)
          albudir(l,2,n) = alb_type(l,n,3)
          albudif(l,2,n) = alb_type(l,n,4)
          snow_mass_alb(l) = snow_surft(l,n_surft_pft) * can_clump(n)
        END DO

        CALL albsnow_ts(land_pts,snow_pts(n),snow_index(:,n),cosz_gb,          &
                        albudir(:,:,n),albudif(:,:,n),                         &
                        rgrain_surft(:,n_surft_pft),                           &
                        snow_mass_alb,soot_gb,alb_snow_surft)
        !
        !       Pass the final albedos back into alb_type.
        DO band = 1,4
          DO j = 1,snow_pts(n)
            l = snow_index(j,n)
            alb_type(l,n,band) = ( alb_snow_surft(l,band) +                    &
              (can_clump(n) - 1.0) * alb_type(l,n,band) ) /                    &
              can_clump(n)
          END DO
        END DO
        !
      END IF
      !
    END DO
    !
    !   Finish off with the non-vegetated tiles.
    DO n = npft+1, ntype
      !
      !     Point to the tile associated with the surface type.
      IF ( .NOT. l_aggregate) THEN
        n_surft_nvg = n
      ELSE
        n_surft_nvg = 1
      END IF
      !
      !     Gather snow-covered points.
      snow_pts(n) = 0
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        !       Allow a numerical tolerance for checking snow depth.
        IF (snowdepth_surft(l,n_surft_nvg) >                                   &
            128 * EPSILON(snowdepth_surft)) THEN
          snow_pts(n) = snow_pts(n) + 1
          snow_index(snow_pts(n),n) = l
        END IF
      END DO
      !
      !     The (adjusted) snow-free albedo will have been set on
      !     unvegetated tiles
      DO j = 1,snow_pts(n)
        l = snow_index(j,n)
        albudir(l,1,n) = alb_type(l,n,1)
        albudif(l,1,n) = alb_type(l,n,2)
        albudir(l,2,n) = alb_type(l,n,3)
        albudif(l,2,n) = alb_type(l,n,4)
      END DO

      CALL albsnow_ts(land_pts,snow_pts(n),snow_index(:,n),cosz_gb,            &
                      albudir(:,:,n),albudif(:,:,n),                           &
                      rgrain_surft(:,n_surft_nvg),                             &
                      snow_surft(:,n_surft_nvg),soot_gb,alb_snow_surft)

      !------------------------------------------------------------------------
      ! For land ice surfaces where deep, dense snow may be emulating firn/bare
      ! ice, scattering physics as in albedo_ts less valid. As in MAR, scale
      ! albedo above threshold with surface density (Gruell and Konzellmann '94)
      ! using ~ top 10cm
      !------------------------------------------------------------------------
      IF (l_elev_land_ice .AND. l_lice_surft(n)) THEN
        DO j = 1,snow_pts(n)
          l = snow_index(j,n)
          IF (l_lice_point(l) .AND. nsnow_surft(l,n) > 0) THEN

            ssum = 0.0
            DO k = 1,nsnow_surft(l,n)
              ssum = ssum + ds_surft(l,n,k)
              IF (ssum > 0.1) EXIT
            END DO
            k = MIN(k,nsnow_surft(l,n))

            IF (SUM(ds_surft(l,n,1:k)) > 1.0e-3) THEN
              rho_snow_surf = (SUM(sice_surft(l,n,1:k)) +                      &
                               SUM(sliq_surft(l,n,1:k))) /                     &
                              SUM(ds_surft(l,n,1:k))
            ELSE
              rho_snow_surf = rho_snow_const
            END IF

            IF (rho_snow_surf > rho_firn_albedo) THEN
              snow_alb_vis_as = aicemax(1) + (rho_snow_surf - rho_ice) *       &
                                ((amax(1) - aicemax(1)) /                      &
                                 (rho_snow_const - rho_ice))

              snow_alb_nir_as = aicemax(2) + (rho_snow_surf - rho_ice) *       &
                                ((amax(2) - aicemax(2)) /                      &
                                 (rho_snow_const - rho_ice))

              alb_snow_surft(l,1) = MIN(alb_snow_surft(l,1),snow_alb_vis_as)
              alb_snow_surft(l,2) = MIN(alb_snow_surft(l,2),snow_alb_vis_as)
              alb_snow_surft(l,3) = MIN(alb_snow_surft(l,3),snow_alb_nir_as)
              alb_snow_surft(l,4) = MIN(alb_snow_surft(l,4),snow_alb_nir_as)
            ELSE
              alb_snow_surft(l,1) = MIN(alb_snow_surft(l,1),amax(1))
              alb_snow_surft(l,2) = MIN(alb_snow_surft(l,2),amax(1))
              alb_snow_surft(l,3) = MIN(alb_snow_surft(l,3),amax(2))
              alb_snow_surft(l,4) = MIN(alb_snow_surft(l,4),amax(2))
            END IF
          END IF !on an elevated ice tile with deep snow
        END DO !loop points
      END IF !elevated ice tiles exist somewhere for this n

      IF (l_mask_snow_orog) THEN
        !
        !       Include orographic masking, based on Roesch et al. (2006),
        !       J. Clim., vol. 19, p.3828, where we take a typical snow
        !       density as 200 kgm-3. Note that they measure SWE in m.
        !       Note also that we are relying on the previous initialization
        !       of alb_type to the background value, which will not be
        !       adjusted if snow is present.
        DO j = 1, snow_pts(n)
          l = snow_index(j,n)
          snowdepth_eff = MAX(0.0, snowdepth_surft(l,n_surft_nvg))
          mask_orog = snowdepth_eff /                                          &
                ( snowdepth_eff + EPSILON(1.0) +                               &
                                0.00075 * ho2r2_orog_gb(l) )
          DO band = 1,4
            alb_type(l,n,band) = alb_type(l,n,band) + mask_orog *              &
              (alb_snow_surft(l,band) - alb_type(l,n,band))
          END DO
        END DO
        !
      ELSE
        !
        DO band = 1,4
          DO j = 1, snow_pts(n)
            l = snow_index(j,n)
            alb_type(l,n,band) = alb_snow_surft(l,band)
          END DO
        END DO
        !
      END IF
      !
    END DO
    !
  END IF ! l_embedded_snow

  IF (l_albedo_obs) THEN
    ! ! Check on 0 > albedo > 1
    DO band = 1,4
      DO n = 1,ntype

        !Set the current soil tile (see notice above)
        IF (nsoilt == 1) THEN
          !There is only 1 soil tile
          m = 1
        ELSE ! nsoilt == nsurft
          !Soil tiles map directly on to surface tiles
          m = n
        END IF !nsoilt

        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          IF ( (alb_type(l,n,band) < 0.0) .OR. (alb_type(l,n,band) > 1.0) ) THEN
            WRITE(jules_message,'(a,i5,F24.12)')                               &
                'ERROR: Albedo < 0.0 or > 1.0: ',l,alb_type(l,n,band)
            CALL jules_print(RoutineName,jules_message)
            WRITE(jules_message,'(a,I3,a,I3)') 'Tile number', n, ', Band', band
            CALL jules_print(RoutineName,jules_message)
            WRITE(jules_message,'(a,F24.12)') 'Scaling', albsfsc(l,band)
            CALL jules_print(RoutineName,jules_message)
            IF (band <= 2) THEN
              WRITE(jules_message,'(a,F24.12,F24.12,F24.12)')                  &
                  'obs, model, soil:',albobs_vis_gb(l), albsfm_vis(l),         &
                  albsoil_soilt(l,m) * disaggregate_albsoil((band+1) / 2)
              CALL jules_print(RoutineName,jules_message)
            ELSE
              WRITE(jules_message,'(a,F24.12,F24.12,F24.12)')                  &
                  'obs, model, soil:',albobs_nir_gb(l), albsfm_nir(l),         &
                  albsoil_soilt(l,m) * disaggregate_albsoil((band+1) / 2)
              CALL jules_print(RoutineName,jules_message)
            END IF
            errcode = 1
            ERRMSG  = 'Unphysical albedos being created'
            CALL ereport ('jules_land_albedo',errcode,ERRMSG)
          END IF
        END DO
      END DO
    END DO

  END IF ! ends l_albedo_obs for the spectral albedo scheme

  IF ( l_spec_alb_bs ) THEN
    ! If the spectral albedos are being used with a single ('blue' sky) value
    ! for the direct ('black' sky) and diffuse ('white' sky) beams, then change
    ! the direct albedos to the diffuse values:
    DO band = 1,2
      ! at the moment only the veg tiles have a different direct/diffuse value:
      DO n = 1,npft
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          alb_type(l,n,2 * band-1) = alb_type(l,n,2 * band)
        END DO
      END DO
    END DO
  END IF

  ! Re-set albedos of frozen lakes if FLake is used
  ! using the algorithm from the flake interface program
  ! (reference Mironov & Ritter 2004).
  IF ( l_flake_model .AND. ( .NOT. l_aggregate) ) THEN
    n = lake
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      IF ((lake_h_ice_gb(l) > 0.0) .AND. (tstar_surft(l,n) <= tm)) THEN
        alb_type(l,n,:) =   albedo_whiteice_ref                                &
                          + EXP( -c_albice_MR * (tm - tstar_surft(l,n)) / tm ) &
                          * (albedo_blueice_ref - albedo_whiteice_ref)
      END IF
    END DO
  END IF

  ! ---------------------------------------------------------------------
  ! Assign snow albedos using the default scheme, essentially assuming
  ! that the snow is on the canopy.
  ! ---------------------------------------------------------------------
  IF ( .NOT. l_embedded_snow) THEN
    IF (l_snow_albedo) THEN
      !----------------------------------------------------------------------
      ! Spectral albedo scheme with prognostic snow albedo
      !----------------------------------------------------------------------
      ! Calculate snow albedos
      CALL albsnow(land_pts,nsurft,surft_index,surft_pts,                      &
                   cosz_gb,rgrain_surft,snowdep_surft,soot_gb,alb_snow)

      !------------------------------------------------------------------------
      ! For land ice surfaces where deep, dense snow may be emulating firn/bare
      ! ice, scattering physics as in albedo_ts less valid. As in MAR, scale
      ! albedo above threshold with surface density (Gruell and Konzellmann '94)
      ! using ~ top 10cm
      !------------------------------------------------------------------------
      IF (l_elev_land_ice) THEN
        DO n = 1,ntype
          IF (l_lice_surft(n)) THEN
            DO j = 1,surft_pts(n)
              l = surft_index(j,n)
              IF (l_lice_point(l) .AND. nsnow_surft(l,n) > 0) THEN
                ssum = 0.0
                DO k = 1,nsnow_surft(l,n)
                  ssum = ssum + ds_surft(l,n,k)
                  IF (ssum > 0.1) EXIT
                END DO
                k = MIN(k,nsnow_surft(l,n))

                IF (SUM(ds_surft(l,n,1:k)) > 1.0e-3) THEN
                  rho_snow_surf = (SUM(sice_surft(l,n,1:k)) +                  &
                                   SUM(sliq_surft(l,n,1:k))) /                 &
                                  SUM(ds_surft(l,n,1:k))
                ELSE
                  rho_snow_surf = rho_snow_const
                END IF

                IF (rho_snow_surf > rho_firn_albedo) THEN
                  snow_alb_vis_as = aicemax(1) + (rho_snow_surf - rho_ice) *   &
                                    ((amax(1) - aicemax(1)) /                  &
                                     (rho_snow_const - rho_ice))

                  snow_alb_nir_as = aicemax(2) + (rho_snow_surf - rho_ice) *   &
                                    ((amax(2) - aicemax(2)) /                  &
                                     (rho_snow_const - rho_ice))

                  alb_snow(l,n,1) = MIN(alb_snow(l,n,1),snow_alb_vis_as)
                  alb_snow(l,n,2) = MIN(alb_snow(l,n,2),snow_alb_vis_as)
                  alb_snow(l,n,3) = MIN(alb_snow(l,n,3),snow_alb_nir_as)
                  alb_snow(l,n,4) = MIN(alb_snow(l,n,4),snow_alb_nir_as)
                ELSE
                  alb_snow(l,n,1) = MIN(alb_snow(l,n,1),amax(1))
                  alb_snow(l,n,2) = MIN(alb_snow(l,n,2),amax(1))
                  alb_snow(l,n,3) = MIN(alb_snow(l,n,3),amax(2))
                  alb_snow(l,n,4) = MIN(alb_snow(l,n,4),amax(2))
                END IF
              END IF ! on an ice tile with deep snow
            END DO !loop points
          END IF !ice tiles exist somewhere for this n
        END DO !loop types
      END IF !elevated surfaces exist

      ! Adjust surface type albedos for snow cover
      IF ( l_aggregate ) THEN
        DO l = 1,land_pts
          snowd(l) = snowdep_surft(l,1)
          z0(l) = z0_surft(l,1)
        END DO
      END IF
      DO n = 1,ntype
        IF ( .NOT. l_aggregate) THEN
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            snowd(l) = snowdep_surft(l,n)
            z0(l) = z0_surft(l,n)
          END DO
        END IF
        ! Calculate snow albedo weighting factor.
        fsnow(:,n) = 0.0
        IF ( l_point_data .AND. .NOT. cansnowtile(n)) THEN
          IF (l_fix_snow_frac) THEN
            DO j = 1,surft_pts(n)
              l = surft_index(j,n)
              snowd(l) = MAX(0.0, snowd(l))
              ! Use linear expansion of exponential if non-linear term
              ! is of order EPSILON (i.e., x^2.0/2.0 ~ EPSILON)
              IF ( snowd(l) > SQRT(2.0*EPSILON(snowd))/maskd) THEN
                fsnow(l,n) = 1.0 - EXP( -maskd * snowd(l) )
              ELSE
                fsnow(l,n) = maskd * snowd(l)
              END IF
            END DO
          ELSE
            DO j = 1,surft_pts(n)
              l = surft_index(j,n)
              IF (snowd(l) > 0.0) fsnow(l,n) = 1.0 - EXP( -maskd * snowd(l) )
            END DO
          END IF
        ELSE
          IF (i_snow_tile(n) == 1) z0(:) = z0_surft(:,ice)
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            IF (snowd(l) > 0.0) fsnow(l,n) = snowd(l) / (snowd(l) + 10 * z0(l))
          END DO
        END IF
        ! Calculate weighted tile albedo.
        IF (i_snow_tile(n) == 1) THEN
          ! Ice tile is used as a separate snow tile
          ! Preserve snow-free albedo on snow-free fractions of selected tiles
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            fsnow(l,ice) = 1.0
            DO band = 1,4
              alb_type(l,ice,band) = alb_snow(l,n,band)
            END DO
          END DO
        ELSE
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            DO band = 1,4
              alb_type(l,n,band) = fsnow(l,n) * alb_snow(l,n,band)             &
                                   + (1.0 - fsnow(l,n)) * alb_type(l,n,band)
            END DO
          END DO
        END IF
      END DO   !  ntype

    ELSE ! l_snow_albedo
      !----------------------------------------------------------------------
      ! spectral albedo scheme with diagnosed snow albedo
      !----------------------------------------------------------------------
      ! Adjust surface type albedos for snow cover
      DO l = 1,land_pts
        tstar(l) = tstar_surft(l,1)
        snowd(l) = snowdep_surft(l,1)
      END DO
      ! Set albedos of snow covered vegetated surface types:
      DO n = 1,npft
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          flit = 1.0 - EXP(-kext(n) * lai(l,n))
          albsnc(l,n) = albsnc_min(n) * (1 - flit) + albsnc_max(n) * flit
        END DO
      END DO
      ! and the snow covered non-veg types:
      DO n = npft+1,ntype
        DO j = 1,surft_pts(n)
          l = surft_index(j,n)
          albsnc(l,n) = albsnc_nvg(n - npft)
        END DO
      END DO
      ! now apply the snow covered albedos to the snow free ones in alb_type
      DO band = 1,4
        DO n = 1,ntype
          IF ( .NOT. l_aggregate) THEN
            DO j = 1,surft_pts(n)
              l = surft_index(j,n)
              tstar(l) = tstar_surft(l,n)
              snowd(l) = snowdep_surft(l,n)
            END DO
          END IF
          fsnow(:,n) = 0.0
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            IF ( tstar(l)  <   tcland ) THEN
              dsa = albsnc(l,n)
            ELSE IF ( tstar(l)  <   tm ) THEN
              dsa = albsnc(l,n) + kland * (alb_type(l,n,band) - albsnc(l,n)) * &
                                     (tstar(l) - tcland)
            ELSE
              dsa = albsnc(l,n) + kland * (alb_type(l,n,band) - albsnc(l,n)) * &
                                     (tm - tcland)
            END IF
            IF (l_fix_snow_frac) THEN
              snowd(l) = MAX(0.0, snowd(l))
              ! Use linear expansion of exponential if non-linear term
              ! is of order EPSILON (i.e., x^2.0/2.0 ~ EPSILON)
              IF (snowd(l)  >   SQRT(2.0*EPSILON(snowd))/maskd) THEN
                fsnow(l,n) = 1.0 - EXP(-maskd * snowd(l))
              ELSE
                fsnow(l,n) = maskd * snowd(l)
              END IF
            ELSE
              fsnow(l,n) = 1.0 - EXP(-maskd * snowd(l))
            END IF
            alb_type(l,n,band) = alb_type(l,n,band) +                          &
                                   (dsa - alb_type(l,n,band)) * fsnow(l,n)
          END DO
        END DO
      END DO

    END IF ! l_snow_albedo ends test on snow scheme for spectral albedo
    !
  END IF ! .NOT. l_embedded_snow ends test on embedded snow.

ELSE ! l_spec_albedo
  !----------------------------------------------------------------------
  ! Non-spectral albedo scheme with diagnosed snow albedo
  !----------------------------------------------------------------------
  IF (l_snow_albedo) THEN
    errcode = 1
    ERRMSG  = 'l_snow_albedo is dependent on l_spec_albedo'
    CALL ereport ('jules_land_albedo',errcode,ERRMSG)
  END IF

  ! Set albedos of vegetated surface types
  DO n = 1,npft

    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = n
    END IF !nsoilt

    !   This scheme is non-spectral, so we cannot disaggregate the soil albedo.
    !   As there is no split between direct and diffuse, we do not apply
    !   Hapke's model.
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      flit = 1.0 - EXP(-kext(n) * lai(l,n))
      albsnc(l,n) = albsnc_min(n) * (1 - flit) + albsnc_max(n) * flit
      albsnf(l,n) = albsoil_soilt(l,m) * (1 - flit) + albsnf_max(n) * flit
    END DO
  END DO

  ! Set albedos of non-vegetated surface types
  DO n = npft+1,ntype
    IF ( .NOT. l_moruses_albedo_surft(n) ) THEN
      !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        m = 1
      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        m = n
      END IF !nsoilt

      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        albsnc(l,n) = albsnc_nvg(n - npft)
        albsnf(l,n) = albsnf_nvg(n - npft)
        IF ( albsnf_nvg(n - npft) <  0.0 ) albsnf(l,n) = albsoil_soilt(l,m)
      END DO
    END IF ! .NOT. l_moruses_albedo; alb_type(urban_canyon) set by canyonalb
  END DO

  ! MORUSES: Set canyon albedos
  IF ( ANY( l_moruses_albedo_surft(:) ) ) THEN
    n = urban_canyon
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(j,l)                  &
!$OMP SHARED(surft_pts,n,surft_index,cosz_gb,hwr_gb,albwl_gb,albrd_gb,albsnf)
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      CALL canyonalb( cosz_gb(l), hwr_gb(l), albwl_gb(l), albrd_gb(l),         &
         albsnf(l,n) )
    END DO
!$OMP END PARALLEL DO
  END IF

  IF ( l_albedo_obs ) THEN
    ! Average the model snow-free albedo over the grid box, by the tile
    ! fraction:
    albsfm_sw(:) = 0.0
    DO n = 1,ntype
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        albsfm_sw(l) = albsfm_sw(l) + ( albsnf(l,n) * frac_surft(l,n) )
      END DO
    END DO

    ! Work out the scaling factor that needs to be applied to make the model
    ! albedo agree with the obs:
    DO l = 1,land_pts
      albsfsc(l,1) = albobs_sw_gb(l) / albsfm_sw(l)
      ! If point is land ice then do not do anything:
      IF ( l_lice_point(l) ) albsfsc(l,1) = 1.0
    END DO

    !Set the current soil tile (see notice above)
    IF (nsoilt == 1) THEN
      !There is only 1 soil tile
      m = 1
    ELSE ! nsoilt == nsurft
      !Soil tiles map directly on to surface tiles
      m = soil
    END IF !nsoilt

    ! Recalculate the above albedos, but with scaled input albedos, within
    ! limits:
    ! scale the bare soil albedo first:
    DO l = 1,land_pts

      !Note that this is not soil tile-aware, so may behave
      !unxepectedly when used with soil tiling.
      albobs_surft(l,soil) = albsoil_soilt(l,m) * albsfsc(l,1)
    END DO
    ! apply the limits:
    DO l = 1,land_pts
      albobs_surft(l,soil)  = MIN(                                             &
                                MAX(albobs_surft(l,soil),                      &
                                    albsnf_nvgl(soil - npft)),                 &
                                albsnf_nvgu(soil - npft))
    END DO
    ! and make a note of what the scaling ended up being, after the limits:
    DO l = 1,land_pts

      !Note that this is not soil tile-aware, so may behave
      !unxepectedly when used with soil tiling.
      albobs_scaling_surft(l,soil,1) = albobs_surft(l,soil) / albsoil_soilt(l,m)
    END DO

    ! now do the veg tiles
    DO n = 1,npft
       !DO l=1,land_pts
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        ! scale the fully veg albedo, and apply limits:
        albobs_surft(l,n) = MIN(                                               &
                                MAX(albsnf_max(n) * albsfsc(l,1),              &
                                    albsnf_maxl(n)),                           &
                                albsnf_maxu(n))
        ! work out the albedo of the tile, with bare soil under the actual
        ! partial veg:
        flit = 1.0 - EXP(-kext(n) * lai(l,n))
        albsnf(l,n) = (1 - flit) * albobs_surft(l,soil) + flit *               &
           albobs_surft(l,n)
        ! and store the scaling:
        albobs_scaling_surft(l,n,1) =  albobs_surft(l,n) / albsnf_max(n)
      END DO
    END DO

    ! non-vegetated surface types:
    DO n = npft+1,ntype
      IF ( .NOT. l_moruses_albedo_surft(n) ) THEN
        IF ( n == soil ) THEN
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            albsnf(l,n) = albobs_surft(l,soil)
          END DO
        ELSE
          DO j = 1,surft_pts(n)
            l = surft_index(j,n)
            ! apply scaling and limits:
            albsnf(l,n) = MIN( MAX(albsnf_nvg(n - npft) * albsfsc(l,1),        &
                         albsnf_nvgl(n - npft)), albsnf_nvgu(n - npft))
            albobs_surft(l,n)= albsnf(l,n)
            ! and store the scaling
            albobs_scaling_surft(l,n,1) =  albobs_surft(l,n) /                 &
               albsnf_nvg(n - npft)
          END DO
        END IF
      END IF ! .NOT. l_moruses_albedo; alb_type(urban_canyon) set by canyonalb
    END DO

    ! ! Check on 0 > albedo > 1
    DO n = 1,ntype

      !Set the current soil tile (see notice above)
      IF (nsoilt == 1) THEN
        !There is only 1 soil tile
        m = 1
      ELSE ! nsoilt == nsurft
        !Soil tiles map directly on to surface tiles
        m = n
      END IF !nsoilt

      !DO l=1,land_pts
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        IF ( (albsnf(l,n) < 0.0) .OR. (albsnf(l,n) > 1.0) ) THEN
          WRITE(jules_message,'(a,F24.12)')                                    &
             'ERROR: Albedo < 0.0 or > 1.0: ', albsnf(l,n)
          CALL jules_print(RoutineName,jules_message)
          WRITE(jules_message,'(a,I3)') 'Tile number', n
          CALL jules_print(RoutineName,jules_message)
          WRITE(jules_message,'(a,F24.12)') 'Scaling', albsfsc(l,1)
          CALL jules_print(RoutineName,jules_message)
          WRITE(jules_message,'(a,F24.12,F24.12,F24.12)')                      &
             'obs, model, soil:', albobs_sw_gb(l), albsfm_sw(l),               &
             albsoil_soilt(l,m)
          CALL jules_print(RoutineName,jules_message)
          errcode = 1
          ERRMSG  = 'Unphysical albedos being created'
          CALL ereport ('jules_land_albedo',errcode,ERRMSG)
        END IF
      END DO
    END DO

  END IF ! ends l_albedo_obs for the non-spectral albedo scheme

  ! Re-set albedos of frozen lakes if FLake is used
  ! using the algorithm from the flake interface program
  ! (reference Mironov & Ritter 2004).
  IF ( l_flake_model .AND. ( .NOT. l_aggregate) ) THEN
    n = lake
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      IF ((lake_h_ice_gb(l) > 0.0) .AND. (tstar_surft(l,n) <= tm)) THEN
        albsnf(l,n) =   albedo_whiteice_ref                                    &
                      + EXP( -c_albice_MR * (tm - tstar_surft(l,n)) / tm )     &
                      * (albedo_blueice_ref - albedo_whiteice_ref)
      END IF
    END DO
  END IF

  ! Adjust surface type albedos for snow cover
  IF ( l_aggregate ) THEN
    DO l = 1,land_pts
      tstar(l) = tstar_surft(l,1)
      snowd(l) = snowdep_surft(l,1)
    END DO
  END IF
  DO n = 1,ntype
    IF ( .NOT. l_aggregate) THEN
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        tstar(l) = tstar_surft(l,n)
        snowd(l) = snowdep_surft(l,n)
      END DO
    END IF
    DO j = 1,surft_pts(n)
      l = surft_index(j,n)
      IF ( tstar(l)  <   tcland ) THEN
        dsa = albsnc(l,n)
      ELSE IF ( tstar(l)  <   tm ) THEN
        dsa = albsnc(l,n) + kland * (albsnf(l,n) - albsnc(l,n))                &
                                 *(tstar(l) - tcland)
      ELSE
        dsa = albsnc(l,n) + kland * (albsnf(l,n) - albsnc(l,n))                &
                                 *(tm - tcland)
      END IF
      IF (l_fix_snow_frac) THEN
        snowd(l) = MAX(0.0, snowd(l))
        ! Use linear expansion of exponential if non-linear term
        ! is of order EPSILON (i.e., x^2.0/2.0 ~ EPSILON)
        IF (snowd(l)  >   SQRT(2.0*EPSILON(snowd))/maskd) THEN
          fsnow(l,n) = 1.0 - EXP(-maskd * snowd(l))
        ELSE
          fsnow(l,n) = maskd * snowd(l)
        END IF
      ELSE
        fsnow(l,n) =  1.0 - EXP(-maskd * snowd(l))
      END IF
      alb_type(l,n,1) = albsnf(l,n) + (dsa - albsnf(l,n)) * fsnow(l,n)
    END DO
  END DO

  ! Copy albedo to all bands
  DO band = 2,4
    DO n = 1,ntype
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        alb_type(l,n,band) = alb_type(l,n,1)
      END DO
    END DO
  END DO

END IF ! l_spec_albedo Spectral or non-spectral albedo schemes

!----------------------------------------------------------------------
! Calculate GBM surface albedo
!----------------------------------------------------------------------
DO band = 1,4
  DO i = 1,pfield
    land_albedo_ij(i,band) = 0.0
  END DO
  IF (ANY(i_snow_tile == 1)) THEN
    ! using a separate snow tile
    DO n = 1,ntype
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        i = land_index(l)
        IF (i_snow_tile(n) == 1) THEN
          land_albedo_ij(i,band) = land_albedo_ij(i,band) +                    &
                      (1 - fsnow(l,n)) * frac_surft(l,n) * alb_type(l,n,band)  &
                        + fsnow(l,n) * frac_surft(l,n) * alb_type(l,ice,band)
        ELSE
          land_albedo_ij(i,band) = land_albedo_ij(i,band) +                    &
                                   frac_surft(l,n) * alb_type(l,n,band)
        END IF
      END DO
    END DO
  ELSE
    ! not using a separate snow tile
    DO n = 1,ntype
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        i = land_index(l)
        land_albedo_ij(i,band) = land_albedo_ij(i,band) +                      &
                                 frac_surft(l,n) * alb_type(l,n,band)
      END DO
    END DO
  END IF
END DO

!----------------------------------------------------------------------
! Copy albedos as required for aggregate or distinct tiles
!----------------------------------------------------------------------
IF (l_aggregate) THEN
  DO band = 1,4
    DO l = 1,land_pts
      i = land_index(l)
      alb_surft(l,1,band) = land_albedo_ij(i,band)
    END DO
  END DO
ELSE
  DO band = 1,4
    DO n = 1,ntype
      DO j = 1,surft_pts(n)
        l = surft_index(j,n)
        alb_surft(l,n,band) = alb_type(l,n,band)
      END DO
    END DO
  END DO
END IF

!----------------------------------------------------------------------
! Copy the albedo scaling to obs for output as diagnostic.
!----------------------------------------------------------------------
IF (l_albedo_obs) THEN
  IF (l_aggregate) THEN
    ! Output the unmodified scaling factor albsfsc
    IF (l_spec_albedo) THEN
      ! There are 2 scalings, one for VIS and NIR
      DO band = 1,2
        DO l = 1,land_pts
          i = land_index(l)
          albobs_sc_ij(i,1,band) = albsfsc(l,band)
        END DO
      END DO
    ELSE
      ! Both diagnostics will see the same scaling
      DO l = 1,land_pts
        i = land_index(l)
        albobs_sc_ij(i,1,1) = albsfsc(l,1)
        albobs_sc_ij(i,1,2) = albsfsc(l,1)
      END DO
    END IF
  ELSE
    ! Output the modified scaling factor on tiles, albobs_scaling_surft
    IF (l_spec_albedo) THEN
      ! There are 2 scalings, one for VIS and NIR
      DO band = 1,2
        DO n = 1,nsurft
          DO l = 1,land_pts
            i = land_index(l)
            albobs_sc_ij(i,n,band) = albobs_scaling_surft(l,n,band)
          END DO
        END DO
      END DO
    ELSE
      ! Both diagnostics will see the same scaling
      DO n = 1,nsurft
        DO l = 1,land_pts
          i = land_index(l)
          albobs_sc_ij(i,n,1) = albobs_scaling_surft(l,n,1)
          albobs_sc_ij(i,n,2) = albobs_scaling_surft(l,n,1)
        END DO
      END DO
    END IF
  END IF
ELSE
  ! no obs scaling used, so the scaling is 1.0
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, k, n)                                  &
!$OMP SHARED(nsurft, pfield, albobs_sc_ij)
  DO k = 1, 2
    DO n = 1, nsurft
!$OMP DO SCHEDULE(STATIC)
      DO i = 1, pfield
        albobs_sc_ij(i,n,k) = 1.0
      END DO
!$OMP END DO NOWAIT
    END DO
  END DO
!$OMP END PARALLEL
END IF

!==============================================================================
! *END NOTICE REGARDING SOIL TILING**
!==============================================================================

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_land_albedo
END MODULE jules_land_albedo_mod
