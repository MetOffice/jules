! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!    Subroutine JULES_SSI_ALBEDO --------------------------------------
!
!    It calculates (true) surface albedos for sea and sea ice
!    Release 2.8 of the UM allows for separate surface
!    albedos for direct and diffuse light over sea.
!
!    Suitable for single column model use.
!
!    Author: William Ingram
!
!    Offline documentation is in UMDP 23, sections "True surface albedo
!    specification" and "Modifications to the radiation scheme to
!    accommodate the leads model"
!
!    There are two options for the sea ice albedo scheme:
!    1) the default albedo parameterisation of the Los Alamos sea ice
!    model CICE vn4.1 (Hunke and Lipscomb, 2010) which was used in CCSM3.
!    This calculates the albedo for 4 bands, visible/near-IR
!    diffuse/direct.
!    2) the original UM scheme which simply uses 1 band.
!
MODULE jules_ssi_albedo_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SSI_ALBEDO_MOD'

CONTAINS

SUBROUTINE jules_ssi_albedo (                                                  &
  !INTENT(IN)
  flandg, aice, tstar, tstar_sice_cat, cosz, ws_10m, chloro,                   &
  s_sea_cat, di_cat, pond_frac_cat, pond_depth_cat,                            &
  alpham,alphac,alphab,dtice,                                                  &
  dt_bare,dalb_bare_wet,pen_rad_frac,beta,                                     &
  albicev_cice, albicei_cice, albsnowv_cice, albsnowi_cice,                    &
  albpondv_cice, albpondi_cice,                                                &
  ahmax, dalb_mlt_cice, dalb_mlts_v_cice, dalb_mlts_i_cice,                    &
  dt_bare_cice, dt_snow_cice,                                                  &
  pen_rad_frac_cice, beta_cice, snowpatch,                                     &
  n_points, n_band_dim, n_bands, nice, nice_use, spec_band_bot, spec_band_top, &
  !INTENT(OUT)
  sa_sice, saos,                                                               &
  !ancil_info (IN)
  sea_index, ssi_index, sice_index_ncat, sice_frac_ncat, sice_pts_ncat,        &
  sea_pts,                                                                     &
  !Fluxes (OUT)
  alb_sicat, penabs_rad_frac)

USE jules_radiation_mod, ONLY: i_sea_alb_method, l_spec_sea_alb,               &
                                l_sea_alb_var_chl, fixed_sea_albedo

USE jules_sea_seaice_mod, ONLY: l_ssice_albedo, l_sice_meltponds,              &
                                 l_sice_meltponds_cice,                        &
                                 l_sice_scattering, l_sice_swpen,              &
                                 l_sice_multilayers, l_cice_alb

USE jules_science_fixes_mod, ONLY: l_fix_alb_ice_thick, l_fix_snow_frac

USE theta_field_sizes,        ONLY: t_i_length, t_j_length
USE water_constants_mod, ONLY: tm, tfs
USE c_kappai, ONLY: kappai,kappai_snow,rhosnow
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE Jin11_osa_mod, ONLY: Jin11_osa
USE errormessagelength_mod, ONLY: errormessagelength
USE ereport_mod, ONLY: ereport

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                         &
     n_points,                                                                 &
     ! Full field dimension
     n_band_dim,                                                               &
     ! Dimension of the sea albedo in bands (to fit all SW calls)
     n_bands,                                                                  &
     ! Number of SW radiation bands in this call
     nice,                                                                     &
     ! Total number of sea ice categories
     nice_use,                                                                 &
     ! Number of sea ice categories used in radiation scheme
     sea_pts

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
     flandg(n_points),                                                         &
                                    ! Land fraction
     aice(n_points),                                                           &
                                    ! Sea-ice fraction
     tstar(n_points),                                                          &
                                    ! Surface temperature
     tstar_sice_cat(n_points,nice_use),                                        &
                                    ! Category seaice surface temperature
     cosz(n_points),                                                           &
                                    ! cos(solar zenith angle)
     ws_10m(n_points),                                                         &
                                    ! 10m wind speed
     chloro(n_points),                                                         &
                                    ! nr surface chlorophyll content
     s_sea_cat(n_points,nice_use),                                             &
                     ! Category snow amount on sea ice (mass/area of ice)
     di_cat(n_points, nice_use),                                               &
                     ! Effective thickness of sea ice categories
     pond_frac_cat(n_points, nice),                                            &
                     ! Meltpond fraction on sea ice categories
     pond_depth_cat(n_points, nice),                                           &
                     ! Meltpond depth on sea ice categories (m)
     spec_band_bot(n_bands), spec_band_top(n_bands)
                     ! boundaries of the spectral bands


REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
!
! Constants used to determine the albedo of sea-ice:
! Albedo of sea-ice at melting point (TM) if .not.l_ssice_albedo, or
! Albedo of snow on sea-ice at melting point (TM) if l_ssice_albedo
!
     alpham                                                                    &
                       ! "M" for "melting"
! Albedo of sea-ice at and below TM-DTICE if .not.l_ssice_albedo, or
! Albedo of snow on sea-ice at and below TM-DTICE if l_ssice_albedo
!
    ,alphac                                                                    &
                       ! "C" for "cold"
! Albedo of snow-free sea-ice if l_ssice_albedo
    ,alphab                                                                    &
                       ! "B" for "bare"
!
! Temperature range in which albedo of sea-ice, if .not.l_ssice_albedo,
! or of snow on sea-ice, if l_ssice_albedo, varies between its limits
    ,dtice                                                                     &
! Temperature range below TM over which meltponds form if
! l_sice_meltponds and l_ssice_albedo
    ,dt_bare                                                                   &
! Increment to albedo for each degree temperature rises above
! TM-DT_BARE. Only used if l_sice_meltponds and l_ssice_albedo
    ,dalb_bare_wet                                                             &
! Fraction of SW radiation that penetrates seaice and scatters
! back causing an addition to the albedo. Only active if l_ssice_albedo
! and l_sice_scattering
    ,pen_rad_frac                                                              &
! attenuation parameter for SW in seaice which controls the
! additional albedo due to internal scattering
   ,beta

! Parameters for the CICE multi-band albedo scheme:
REAL(KIND=real_jlslsm), INTENT(IN) :: albicev_cice
      ! Visible ice albedo for h > ahmax and T < -1C
REAL(KIND=real_jlslsm), INTENT(IN) :: albicei_cice
      ! Near-ir ice albedo for h > ahmax and T < -1C
REAL(KIND=real_jlslsm), INTENT(IN) :: albsnowv_cice
      ! Visible snow albedo for cold snow (T < -1C)
REAL(KIND=real_jlslsm), INTENT(IN) :: albsnowi_cice
      ! Near-ir snow albedo for cold snow (T < -1C)
REAL(KIND=real_jlslsm), INTENT(IN) :: albpondv_cice
      ! Visible melt pond albedo
REAL(KIND=real_jlslsm), INTENT(IN) :: albpondi_cice
      ! Near-ir melt pond albedo
REAL(KIND=real_jlslsm), INTENT(IN) :: ahmax
      ! Albedo is constant above this thickness (metres)
REAL(KIND=real_jlslsm), INTENT(IN) :: dalb_mlt_cice
      ! Ice albedo change per 1 degree change in temp for ice (for -1C to 0C)
REAL(KIND=real_jlslsm), INTENT(IN) :: dalb_mlts_v_cice
      ! Snow albedo change per 1 degree change in temp (for -1C to 0C)
      ! (visible radiation)
REAL(KIND=real_jlslsm), INTENT(IN) :: dalb_mlts_i_cice
      ! Snow albedo change per 1 degree change in temp (for -1C to 0C)
      ! (near-ir radiation)
REAL(KIND=real_jlslsm), INTENT(IN) :: dt_bare_cice
      ! Temperature range below TM over which meltponds form (in CICE
      ! albedo scheme when CICE meltpond scheme is turned off)
REAL(KIND=real_jlslsm), INTENT(IN) :: dt_snow_cice
      ! Temperature range in which temperature of snow on sea-ice varies
      ! between its limits (in CICE albedo scheme)
REAL(KIND=real_jlslsm), INTENT(IN) :: snowpatch
      ! Length scale for parameterizing non uniform snow coverage (m)
REAL(KIND=real_jlslsm), INTENT(IN) :: beta_cice
      ! Attenuation parameter for SW in seaice which controls the
      ! additional albedo due to internal scattering in the zero-layer
      ! case for the CICE albedo scheme.
REAL(KIND=real_jlslsm), INTENT(IN) :: pen_rad_frac_cice
      ! Fraction of SW radiation that penetrates seaice and scatters
      ! back causing an addition to the albedo in the zero-layer
      ! case for the CICE albedo scheme.
      ! Note that this value is double the value used in the old HadGEM
      ! sea ice albedo scheme.  This is because the old scheme is
      ! not wavelength-dependent, i.e. the Semtner correction is
      ! applied equally to all wavelengths.  The CICE scheme is
      ! band-dependent, and the Semtner correction is applied only
      ! to visible radiation, not infra-red.  Visible comprises
     ! around 50% of the total radiation.  Hence the value of the
      ! penetrating radiation fraction is set to twice the value
      ! used in the HadGEM scheme to achieve approximately the same
      ! effect.


REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
     sa_sice(n_points, 4),                                                     &
      ! Surface Albedo for sea ice
      !       (*,1) - direct beam visible
      !       (*,2) - diffuse visible
      !       (*,3) - direct beam near-ir
      !       (*,4) - diffuse near-ir
     saos(n_points, 2, n_band_dim)
      ! Surface albedo for Open Sea (direct and diffuse components, for each
      ! band, with zeros for safety where no value applies)


!ancil_info (IN)
INTEGER, INTENT(IN) ::                                                         &
  sea_index(t_i_length * t_j_length),                                          &
  ssi_index(t_i_length * t_j_length),                                          &
  sice_index_ncat(t_i_length * t_j_length,nice),                               &
  sice_pts_ncat(nice)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  sice_frac_ncat(t_i_length * t_j_length,nice)

!Fluxes (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
alb_sicat(t_i_length * t_j_length, nice_use, 4),                               &
penabs_rad_frac(t_i_length * t_j_length, nice_use, 4)

! Local variables
! reduce some inputs on n_points, onto sea-points:

REAL(KIND=real_jlslsm) :: pond_frac_cat_use
                             ! Meltpond fraction on sea ice categories
                             ! to be used in calculation.
                             ! (set to zero if pond depth < 0.004 m, or
                             ! to pond_frac_cat otherwise).

REAL(KIND=real_jlslsm), ALLOCATABLE :: cosz_s(:)
REAL(KIND=real_jlslsm), ALLOCATABLE :: ws_10m_s(:)
REAL(KIND=real_jlslsm), ALLOCATABLE :: chloro_s(:)
REAL(KIND=real_jlslsm), ALLOCATABLE :: saos_s(:,:,:)

INTEGER :: band, n, j, l, ll, i, k      ! Loops over points
REAL(KIND=real_jlslsm) :: bx                                                   &
                                    ! 1 - COSZ
! Temperature at which (snow on) sea-ice reaches its "cold" value
    ,tcice                                                                     &
! Slope and intercept of temperature dependence of the albedo of
! (snow on) sea-ice
    ,ice1,ice2

! Local parameters

REAL(KIND=real_jlslsm), PARAMETER :: adifc = 0.06
      ! Surface albedo of ice-free sea for the diffuse beam
REAL(KIND=real_jlslsm), PARAMETER :: chloro_constant = 5.0e-7
      ! Chlorophyll content of the sea near-surface, if constant (0.5mg m-3)

REAL(KIND=real_jlslsm), PARAMETER :: maskd = 0.2
      ! Masking depth (S in 3.6.1)

REAL(KIND=real_jlslsm) ::                                                      &
        fh, albo, albice(4), albsnow(4), fT, albs(4), area_snow,               &
        dalb_mlts_cice(4), albpond(4), albp(4)

REAL(KIND=real_jlslsm) :: fhtan

REAL(KIND=real_jlslsm) :: snow_albedo ! Snow albedo
REAL(KIND=real_jlslsm) :: ice_alb     ! Sea ice albedo

REAL(KIND=real_jlslsm) :: hice(n_points,nice_use)
      ! Ice thickness

REAL(KIND=real_jlslsm) :: saos_bb(n_points,2)
      ! Broadband open sea albedo, for methods that need it.
REAL(KIND=real_jlslsm) :: spectralsea(n_bands)
      ! Spectral albedo scaling factor, for methods that need it.
REAL(KIND=real_jlslsm), PARAMETER :: spec_band_bb(1) = -1.0
      ! Array to use as sop and bottom of spectral bands, for Jin11 broadband

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_SSI_ALBEDO'

INTEGER ::              errcode                ! Error code
CHARACTER(LEN=errormessagelength) :: ERRMSG    ! Error message text

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

fhtan = ATAN(ahmax * 4.0)

! Open Sea albedo
IF (i_sea_alb_method == 1) THEN
  ! 1 - Briegleb and Ramanathan, 1982, J. Appl. Met.
  ! (doi:10.1175/1520-0450(1982)021<1160:SADVIC>2.0.CO;2)
  DO j = 1, 2
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i)                    &
!$OMP SHARED(n_points, saos_bb, j)
    DO i = 1, n_points
      saos_bb(i,j) = 0.0
    END DO
!$OMP END PARALLEL DO
  END DO
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i)                    &
!$OMP SHARED(n_points, saos_bb, flandg, cosz)
  DO i = 1, n_points
    IF (flandg(i) <  1.0) THEN
      saos_bb(i, 1) = 0.026 / (cosz(i)**1.7 + 0.065)                           &
          +0.15 * (cosz(i) - 0.1) * (cosz(i) - 0.5) * (cosz(i) - 1.0)
      saos_bb(i,2) = adifc
    END IF
  END DO
!$OMP END PARALLEL DO

  DO k = 1, n_band_dim
    DO j = 1, 2
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i)                    &
!$OMP SHARED(n_points, j, k, saos)
      DO i = 1, n_points
        saos(i,j,k) = 0.0
      END DO
!$OMP END PARALLEL DO
    END DO
  END DO
  ! this method can only apply spectral scaling if there are 6 bands
  ! as per HadGEM2, as the scaling is hardcoded. This is kept here for
  ! consistency with the UM radiation code.
  IF (l_spec_sea_alb .AND. n_bands == 6) THEN
    ! copy the bb albedo onto bands, with a scaling factor
    spectralsea = [ 0.000000, 1.444205, 1.799420,                              &
                         0.5470291, 0.000000, 0.000000 ]
    ! copy the broadband albedo onto all bands:
    DO band = 1, n_bands
      DO j = 1, n_points
        saos(j, 1, band) = saos_bb(j, 1) * spectralsea(band)
        saos(j, 2, band) = saos_bb(j, 2) * spectralsea(band)
      END DO
    END DO
  ELSE
    ! copy the broadband albedo onto all bands:
    DO band = 1, n_bands
      DO j = 1, n_points
        saos(j, 1, band) = saos_bb(j, 1)
        saos(j, 2, band) = saos_bb(j, 2)
      END DO
    END DO
  END IF

ELSE IF (i_sea_alb_method == 2) THEN
  ! 2 - Modified Barker and Li, 1995, J. Climate,
  ! (doi:10.1175/1520-0442(1995)008<2213:ISOCSS>2.0.CO;2)
  ! modified to agree with Met Office aircraft obs for
  ! water clarity around the UK
  DO j = 1, 2
    DO i = 1, n_points
      saos_bb(i,j) = 0.0
    END DO
  END DO
  DO j = 1, n_points
    IF (flandg(j) <  1.0) THEN
      bx = 1.0 - cosz(j)
      saos_bb(j, 1) = 0.0315 + 0.0421 * bx**2                                  &
             + 0.128 * bx**3 - 0.04 * bx**4                                    &
             + (3.12 / (5.68) + (0.074 * bx / (1.0))) * bx**5
      saos_bb(j,2) = adifc
    END IF
  END DO

  DO k = 1, n_band_dim
    DO j = 1, 2
      DO i = 1,n_points
        saos(i,j,k) = 0.0
      END DO
    END DO
  END DO
  ! this method can only apply spectral scaling if there are 6 bands
  ! as per HadGEM2, as the scaling is hardcoded. This is kept here for
  ! consistency with the UM radiaiton code.
  IF (l_spec_sea_alb .AND. n_bands == 6) THEN
    ! copy the bb albedo onto bands, with a scaling factor
    spectralsea = [ 0.000000, 1.444205, 1.799420,                              &
                         0.5470291, 0.000000, 0.000000 ]
    ! copy the broadband albedo onto all bands:
    DO band = 1, n_bands
      DO j = 1, n_points
        saos(j, 1, band) = saos_bb(j, 1) * spectralsea(band)
        saos(j, 2, band) = saos_bb(j, 2) * spectralsea(band)
      END DO
    END DO
  ELSE
    ! copy the broadband albedo onto all bands:
    DO band = 1, n_bands
      DO j = 1, n_points
        saos(j, 1, band) = saos_bb(j, 1)
        saos(j, 2, band) = saos_bb(j, 2)
      END DO
    END DO
  END IF

ELSE IF (i_sea_alb_method == 3) THEN
  ! 3 - Jin et al. 2011, Optics Express
  ! (doi:10.1364/OE.19.026429)

  ! the Jin11 albedo parameterisation is a bit more complex/expensive
  ! than the others, so it is worth compressing the input fields onto
  ! sea points only:
  ! (note sea_pts and sea_index are not all land, and not all sea-ice)
  ALLOCATE(cosz_s(sea_pts))
  ALLOCATE(ws_10m_s(sea_pts))
  ALLOCATE(chloro_s(sea_pts))
  ALLOCATE(saos_s(sea_pts, 2, n_bands))

  DO j = 1, sea_pts
    cosz_s(j) = cosz(sea_index(j))
    ws_10m_s(j) = ws_10m(sea_index(j))
    IF (l_sea_alb_var_chl) THEN
      chloro_s(j) = chloro(sea_index(j))
    END IF
  END DO
  IF ( .NOT. l_sea_alb_var_chl) THEN
    ! set the chlorophyll content to the constant value:
    chloro_s = chloro_constant
  END IF

  IF (l_spec_sea_alb) THEN
    ! call the Jin 2011 albedo parameterisation, with spectral bands:
    CALL Jin11_osa(n_bands, spec_band_bot, spec_band_top,                      &
                   sea_pts, cosz_s, ws_10m_s, chloro_s, saos_s)

    ! push the output albedo on sea points back onto all points:
    DO k = 1, n_band_dim
      DO j = 1, 2
        DO i = 1,n_points
          saos(i,j,k) = 0.0
        END DO
      END DO
    END DO
    DO band = 1, n_bands
      DO j = 1, sea_pts
        saos(sea_index(j),1,band) = saos_s(j,1,band)
        saos(sea_index(j),2,band) = saos_s(j,2,band)
      END DO
    END DO
  ELSE
    ! call the Jin 2011 albedo, for broadband:
    CALL Jin11_osa(1, spec_band_bb, spec_band_bb,                              &
                   sea_pts, cosz_s, ws_10m_s, chloro_s, saos_s)

    ! and push the output albedo on sea points back onto all points:
    DO k = 1, n_band_dim
      DO j = 1, 2
        DO i = 1,n_points
          saos(i,j,k) = 0.0
        END DO
      END DO
    END DO
    DO band = 1, n_bands
      DO j = 1, sea_pts
        saos(sea_index(j),1,band) = saos_s(j,1,1)
        saos(sea_index(j),2,band) = saos_s(j,2,1)
      END DO
    END DO
  END IF

  ! and finally deallocate arrays:
  DEALLOCATE(saos_s)
  DEALLOCATE(chloro_s)
  DEALLOCATE(ws_10m_s)
  DEALLOCATE(cosz_s)

ELSE IF (i_sea_alb_method == 4) THEN
  ! Simple fixed albedo as set in jules_radiation namelist
  DO k = 1, n_band_dim
    DO j = 1, 2
      DO i = 1,n_points
        saos(i,j,k) = fixed_sea_albedo
      END DO
    END DO
  END DO

ELSE IF (i_sea_alb_method == 5) THEN
  ! Fixed sea albedo with simple sea-ice parametrization below the
  ! temperature where sea-water starts to freeze (tfs)

  ! First set the sea-surface to the namelist parameter
  DO k = 1, n_band_dim
    DO j = 1, 2
      DO i = 1,n_points
        saos(i,j,k) = fixed_sea_albedo
      END DO
    END DO
  END DO

  ! Then adjust for the sea-ice.
  ! Parametrization is from Liu et al (2007, Int J Clim, 27, 81)
  ! (the HIRHAM parametrization)
  ! modified to be spectrally dependent for use around M-dwarfs.
  ! The values for bands and albedo are loosely based on
  ! Joshi & Haberle (2012, Astrobio, 12, 3) and
  ! Turbet et al (2016, A&A, 596, A112), tuned to give sensible values
  ! for the mean albedo when using spectral files for Earth and Proxima B
  DO band = 1, n_bands
    IF (spec_band_top(band) < 1.1e-6) THEN
      ! At short wavelengths, the albedo is high
      DO j = 1, sea_pts
        IF (tstar(sea_index(j)) < tfs) THEN
          saos(sea_index(j),:,band) = 0.8 -                                    &
               EXP(-(tfs - tstar(sea_index(j))) / 2.0) *                       &
               (0.8 - fixed_sea_albedo)
        END IF
      END DO
    ELSE IF (spec_band_bot(band) > 1.1e-6) THEN
      ! At long wavelengths, the albedo is low
      DO j = 1, sea_pts
        IF (tstar(sea_index(j)) < tfs) THEN
          saos(sea_index(j),:,band) = 0.05 -                                   &
               EXP(-(tfs - tstar(sea_index(j))) / 2.0) *                       &
               (0.05 - fixed_sea_albedo)
        END IF
      END DO
    ELSE
      ! Transition between the two extremes in the overlap band based on a
      ! linear weighting of the wavelength
      DO j = 1, sea_pts
        IF (tstar(sea_index(j)) < tfs) THEN
          ice_alb = 0.8 * (1.1e-6-spec_band_bot(band)) /                       &
                          (spec_band_top(band) - spec_band_bot(band))          &
                  + 0.05 * (spec_band_top(band) - 1.1e-6) /                    &
                           (spec_band_top(band) - spec_band_bot(band))
          saos(sea_index(j),:,band) = ice_alb -                                &
               EXP(-(tfs - tstar(sea_index(j))) / 2.0) *                       &
               (ice_alb - fixed_sea_albedo)
        END IF
      END DO
    END IF
  END DO

ELSE
  errcode = 1
  ERRMSG  = 'i_sea_alb_method does not have a valid setting'
  CALL ereport ('jules_ssi_albedo',errcode,ERRMSG)
END IF


! Sea ice albedo

DO j = 1, 4
  DO i = 1, n_points
    sa_sice (i,j)   = 0.0
  END DO
END DO
DO k = 1, 4
  DO j = 1, nice_use
    DO i = 1, t_i_length * t_j_length
      alb_sicat(i,j,k) = 0.0
      penabs_rad_frac(i,j,k) = 0.0
    END DO
  END DO
END DO

IF (l_cice_alb) THEN

  ! Parametrisation from Los Alamos sea ice model (CICE vn4.1),
  !         their 'default' option
  albice(1) = albicev_cice  ! Direct=diffuse
  albice(2) = albicev_cice
  albice(3) = albicei_cice
  albice(4) = albicei_cice
  albsnow(1) = albsnowv_cice
  albsnow(2) = albsnowv_cice
  albsnow(3) = albsnowi_cice
  albsnow(4) = albsnowi_cice
  albpond(1) = albpondv_cice
  albpond(2) = albpondv_cice
  albpond(3) = albpondi_cice
  albpond(4) = albpondi_cice
  dalb_mlts_cice(1) = dalb_mlts_v_cice
  dalb_mlts_cice(2) = dalb_mlts_v_cice
  dalb_mlts_cice(3) = dalb_mlts_i_cice
  dalb_mlts_cice(4) = dalb_mlts_i_cice

  DO band = 1, 4
    DO n = 1, nice_use
      DO l = 1, sice_pts_ncat(n)
        ll = sice_index_ncat(l,n)
        j = ssi_index(ll)

        IF (l_sice_multilayers .AND. l_fix_alb_ice_thick) THEN
                 ! True ice thickness (hice) is equal to di_cat:
          hice(j,n) = di_cat(j,n)
        ELSE
          ! Convert effective ice thickness (di_cat) to true ice
          ! thickness (hice):
          IF (l_fix_snow_frac) THEN
            hice(j,n) = di_cat(j,n)                                            &
                          - (kappai / kappai_snow)                             &
                          * (MAX(0.0, s_sea_cat(j,n)) / rhosnow)
          ELSE
            hice(j,n) = di_cat(j,n)                                            &
                        - (kappai / kappai_snow) * (s_sea_cat(j,n) / rhosnow)
          END IF
        END IF

        ! Bare ice, thickness dependence
        fh = MIN(ATAN(hice(j,n) * 4.0) / fhtan, 1.0)
        albo = albice(band) * fh + adifc * (1.0 - fh)

        IF ( l_sice_meltponds .AND. ( .NOT. l_sice_meltponds_cice) ) THEN
          ! Bare ice, simple meltpond scheme (temperature dependence)
          fT = MIN(tm - tstar_sice_cat(j,n) - dt_bare_cice, 0.0)
          alb_sicat(ll,n,band) = MAX(albo - dalb_mlt_cice * fT, adifc)
        ELSE
          ! No dependence of bare ice albedo on temperature in case where
          ! more sophisticated meltpond scheme is used.
          alb_sicat(ll,n,band) = MAX(albo, adifc)
        END IF

        ! Bare ice - Semtner scattering approximation
        ! and penetrating-absorbed (penabs) radiation
        IF (l_sice_scattering) THEN


          ! A fraction of penetrating solar radiation is absorbed (penabs)
          ! where pen_rad_frac_cice is the fraction of net solar radiation
          ! that penetrates and 1-beta_cice is the fraction of that that is
          ! absorbed. penabs_rad_frac then becomes the fraction of surface
          ! downward solar that is absorbed after penetrating into the ice.
          IF (l_sice_swpen .AND. ( (band == 1) .OR. (band == 2) ) ) THEN
            penabs_rad_frac(ll,n,band) = (1.0 - alb_sicat(ll,n,band))          &
                      * pen_rad_frac_cice * (1.0 - beta_cice)
          END IF

          ! Semtner modification dry albedo for internal
          ! scattering (Semtner, 1976).
          ! Only applied to visible radiation.
          IF ((band == 1) .OR. (band == 2)) THEN
            alb_sicat(ll,n,band) = alb_sicat(ll,n,band) + beta_cice *          &
                (1.0 - alb_sicat(ll,n,band)) * pen_rad_frac_cice
          END IF

        END IF       ! l_sice_scattering

              ! Snow, temperature dependence
        IF (s_sea_cat(j,n) > 0.0) THEN
          albs(band) = albsnow(band)
          fT = MIN(tm - tstar_sice_cat(j,n) - dt_snow_cice, 0.0)
          albs(band) = albs(band) - dalb_mlts_cice(band) * fT
          ! Note rhosnow in next line is required to convert s_sea_cat
          !        from kg/m2 to m
          area_snow = s_sea_cat(j,n)                                           &
               / (s_sea_cat(j,n) + snowpatch * rhosnow)
        ELSE
          area_snow = 0.0
        END IF

        ! Dependence of pond albedo on pond depth (for Flocco et al. scheme)
        IF (l_sice_meltponds_cice) THEN
          IF (pond_depth_cat(j,n) < 0.004) THEN  ! < 4mm bare ice albedo
            albp(band) = alb_sicat(ll,n,band)
          ELSE IF (pond_depth_cat(j,n) > 0.2) THEN  ! > 20cm pond albedo
            albp(band) =  albpond(band)
          ELSE ! linear relationship approx. to Delta-Eddington
            albp(band) = (pond_depth_cat(j,n) / 0.2) * albpond(band) +         &
                    (1.0 - (pond_depth_cat(j,n) / 0.2)) * alb_sicat(ll,n,band)
          END IF
        END IF

        ! Combine snow and ice albedo and penetrating-absorbed radiation
        ! dependence on snow cover
        IF (s_sea_cat(j,n) > 0.0) THEN
          alb_sicat(ll,n,band) =                                               &
                  alb_sicat(ll,n,band) * (1.0 - area_snow)  +                  &
                  albs(band) * area_snow
          penabs_rad_frac(ll,n,band) = penabs_rad_frac(ll,n,band)              &
                    * (1.0 - area_snow)
        END IF

        ! Combine snow and ice albedo with pond albedo
        IF (l_sice_meltponds_cice) THEN
          IF (nice_use == nice) THEN
            IF (pond_depth_cat(j,n) < 0.004) THEN
              pond_frac_cat_use = 0.0
            ELSE
              pond_frac_cat_use = pond_frac_cat(j,n)
            END IF
            alb_sicat(ll,n,band) =                                             &
                alb_sicat(ll,n,band) * (1.0 - pond_frac_cat_use)  +            &
                albp(band) * pond_frac_cat_use
          ELSE  ! Fail if nice_use is not equal to nice.
            errcode = 556
            ERRMSG = "CICE meltpond fields cannot be used" //                  &
                     "in JULES albedo without CICE multicategories."
            CALL ereport('jules_ssi_albedo', errcode, ERRMSG)
          END IF
        END IF

              ! Mean sea ice albedo
        sa_sice(j, band) = sa_sice(j, band) +                                  &
          alb_sicat(ll,n,band) * sice_frac_ncat(ll,n) / aice(j)
      END DO
    END DO
  END DO

ELSE

  ! Use old UM schemes
  ! Derive 3 constants from the basic quantities (supplied in the
  ! UM namelist RUN_Radiation) for sea-ice albedo calculations:
  tcice = tm - dtice
  ice1 = (alpham - alphac) / dtice
  ice2 = alpham - tm * ice1

  DO n = 1, nice_use
    DO l = 1, sice_pts_ncat(n)
      ll = sice_index_ncat(l,n)
      j = ssi_index(ll)

      IF (l_ssice_albedo) THEN   ! Which albedo scheme?
        ! Effect of snow on sea-ice albedo

        IF (s_sea_cat(j,n) >  0.0) THEN   ! snow on sea ice
                                          ! Cox et al., Clim Dyn,1999
          IF (tstar_sice_cat(j,n) >  tcice) THEN
            snow_albedo = ice2 + ice1 * tstar_sice_cat(j,n)
          ELSE
            snow_albedo = alphac
          END IF

          IF (l_fix_snow_frac .AND.                                            &
               s_sea_cat(j,n)  <=   SQRT(2.0*EPSILON(s_sea_cat))/maskd) THEN
            ! Use linear expansion of exponential if non-linear term
            ! is of order EPSILON (i.e., x^2.0/2.0 ~ EPSILON)
            alb_sicat(ll,n,1) = alphab                                         &
            +(snow_albedo - alphab) * maskd * s_sea_cat(j,n)
          ELSE
            alb_sicat(ll,n,1) = alphab                                         &
            +(snow_albedo - alphab) * (1.0 - EXP(-maskd * s_sea_cat(j,n)))
          END IF
        ELSE           ! no snow so bare ice only

          IF (l_sice_meltponds) THEN

            ! bare ice, temperature dep. (Ebert &Curry,1993)
            IF (tstar_sice_cat(j,n) > (tm - dt_bare)) THEN
              alb_sicat(ll,n,1) = alphab                                       &
                                 + (tstar_sice_cat(j,n) - tm + dt_bare)        &
                                 * dalb_bare_wet
            ELSE      ! surface is dry
              alb_sicat(ll,n,1) = alphab
            END IF     ! end melt ponds

          ELSE         ! just use bare ice albedo

            alb_sicat(ll,n,1) = alphab

          END IF       ! l_sice_meltponds

          IF (l_sice_scattering) THEN
            ! Semtner modification dry albedo for internal
            ! scattering (Semtner, 1976)
            alb_sicat(ll,n,1) = alb_sicat(ll,n,1) +                            &
                                beta * (1.0 - alb_sicat(ll,n,1))               &
                                *pen_rad_frac

          END IF       ! l_sice_scattering

        END IF         !  any snow on ice

      ELSE             ! default to operational NWP scheme 3.5.1:

        IF ( tstar_sice_cat(j,n)  <   tcice ) THEN
          alb_sicat(ll,n,1) = alphac
        ELSE
          alb_sicat(ll,n,1) = ice1 * tstar_sice_cat(j,n) + ice2
        END IF

      END IF          ! Which albedo scheme?

      IF (alb_sicat(ll,n,1) <  0.0) THEN
        alb_sicat(ll,n,1) = alpham
      END IF

      !     For this scheme all band albedos are the same:
      DO band = 2, 4
        alb_sicat(ll,n,band) = alb_sicat(ll,n,1)
      END DO

    END DO  ! l
  END DO    ! n

  ! Calculate sea ice mean albedo
  DO band = 1, 4
    DO n = 1, nice_use
      DO l = 1, sice_pts_ncat(n)
        ll = sice_index_ncat(l,n)
        j = ssi_index(ll)
        sa_sice(j,band) = sa_sice(j,band) +                                    &
               alb_sicat(ll,n,band) * sice_frac_ncat(ll,n) / aice(j)
      END DO
    END DO
  END DO

END IF


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE jules_ssi_albedo
END MODULE jules_ssi_albedo_mod
