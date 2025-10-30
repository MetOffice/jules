! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE Jin11_osa_mod

USE errormessagelength_mod, ONLY: errormessagelength
USE ereport_mod, ONLY: ereport
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JIN11_OSA_MOD'
INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_out = 1

CONTAINS

!---------------------------------------------------------------------
SUBROUTINE Jin11_osa(nd_bands, band_bot, band_top,                             &
                     nd_points, cosz_in, ws_10m_in, chloro_in, osa)
! Purpose:
!     Calculates the open sea albedo using the parameterisation of
!     Jin et al. 2011, Optics Express (doi:10.1364/OE.19.026429)
!
! Inputs:
!   nd_bands: number of spectral bands for this calculation
!
!   band_bot(nd_bands): bottom of the spectral bands (in m)
!
!   band_top(nd_bands): top of the spectral bands (in m)
!
!   nd_points: number of spatial points for calculation
!
!   cosz_in(nd_points): cosine of the solar zenith angle on points
!
!   ws_10m_in(nd_points): 10m wind speed on points (in ms-1)
!
!   chloro_in(nd_points): chlorophyll content on points (km m-3)
!
! Outputs:
!   osa(nd_points, 2, nd_bands): open sea albedos, on points and bands
!                                osa(:,1,:) is direct
!                                osa(:,2,:) is diffuse
!


IMPLICIT NONE

! number of spectral bands
INTEGER, INTENT(IN) :: nd_bands
! number of points, spatially
INTEGER, INTENT(IN) :: nd_points

! bottom and top wavelengths of the spectral bands (in m)
REAL(KIND=real_jlslsm), INTENT(IN) :: band_bot(nd_bands)
REAL(KIND=real_jlslsm), INTENT(IN) :: band_top(nd_bands)

! cosine of the solar zenith angle
REAL(KIND=real_jlslsm), INTENT(IN) :: cosz_in(nd_points)
! windspeed at 10m (ms-1)
REAL(KIND=real_jlslsm), INTENT(IN) :: ws_10m_in(nd_points)
! Nr sea surface chlorophyll content (kg m-3)
REAL(KIND=real_jlslsm), INTENT(IN) :: chloro_in(nd_points)

! output open sea albedo, direct and diffuse, for each band
REAL(KIND=real_jlslsm), INTENT(OUT) :: osa(nd_points, 2, nd_bands)

! LOCAL VARIABLES:
! copies of the inputs are capped to sensible values:
REAL(KIND=real_jlslsm) :: cosz(nd_points)
REAL(KIND=real_jlslsm) :: ws_10m(nd_points)
REAL(KIND=real_jlslsm) :: chloro(nd_points)

! the parameterisation splits the albedo into four component albedos:
! surface direct albedo:
REAL(KIND=real_jlslsm) :: alb_s_dr(nd_points)
! ocean/water/sub-surface direct albedo
REAL(KIND=real_jlslsm) :: alb_w_dr(nd_points)
!  surface diffuse albedo:
REAL(KIND=real_jlslsm) :: alb_s_df(nd_points)
! ocean/water/sub-surface diffuse albedo
REAL(KIND=real_jlslsm) :: alb_w_df(nd_points)
! which get aggregated to direct/diffuse albedo on subsampled wavelengths:
REAL(KIND=real_jlslsm), ALLOCATABLE :: alb_dr_wl(:,:)
REAL(KIND=real_jlslsm), ALLOCATABLE :: alb_df_wl(:,:)
! and this gets used along the way for that aggregation
REAL(KIND=real_jlslsm) :: alb_dr_tmp(nd_points)
REAL(KIND=real_jlslsm) :: alb_df_tmp(nd_points)

! local parameters, from Jin 2011:
! the relative refractive index of water and air, for broadband VIS:
REAL(KIND=real_jlslsm), PARAMETER :: bb_refm = 1.34
! the albedo of whitecaps (foam on top of large waves):
REAL(KIND=real_jlslsm), PARAMETER :: albedo_whitecap = 0.55
! Equivalent incident cos(solar zenith angle), used for diffuse sub-surface
! reflection. Morel and Gentili, 1991 (doi:10.1364/AO.30.004427).
REAL(KIND=real_jlslsm)                                                         &
#if !defined(GNU_FORTRAN)
,PARAMETER                                                                     &
#endif
:: cosz_e(1) = [0.676 ]
! fir coefficients for surface diffuse albedo (eg 5a in Jin 2011):
REAL(KIND=real_jlslsm)                                                         &
    , PARAMETER :: s_df_fit(1:4) = [-0.1482,-0.012,0.1609,-0.0244 ]

! and local variables:
! width of a spectral band:
REAL(KIND=real_jlslsm) :: bandwidth
! wavelength increment for subsampling within the band
REAL(KIND=real_jlslsm) :: wl_incr

! wind speed parameter from Cox and Munk, 1954 (doi:10.1364/JOSA.44.000838)
REAL(KIND=real_jlslsm) :: sigma(nd_points)
! Regression function values, Jin'11 equation 4, (actual solar zenith angle)
REAL(KIND=real_jlslsm) :: zmod_cosz(nd_points)
! Regression function values, Jin'11 equation 4, (effective cosz, cosz_e)
REAL(KIND=real_jlslsm) :: zmod_cosz_e(nd_points)

! wavelengths where the given bands have been subsampled:
REAL(KIND=real_jlslsm), ALLOCATABLE :: wl(:)
! relative refractive indices of air and water, at different wavelenghts
REAL(KIND=real_jlslsm), ALLOCATABLE :: refm(:)
! angle and refractive index term in calculation of fresnel reflectance
REAL(KIND=real_jlslsm) :: xx2(nd_points)
! repeated, for a broadband value of refm (bb_refm)
REAL(KIND=real_jlslsm) :: xx2_bb(nd_points)
! and repeated for a single representative angle (cosz_e)
REAL(KIND=real_jlslsm) :: xx2_cosz_e(1)
! and repeated for a single representative angle (cosz_e) and broadband radn
REAL(KIND=real_jlslsm) :: xx2_bb_cosz_e(1)
! fresnel refelectance, at current solar zenith angle and wavelength
REAL(KIND=real_jlslsm) :: rr0(nd_points)
! repeated for a single representative angle (cosz_e)
REAL(KIND=real_jlslsm) :: rr0_cosz_e(1)
! fresnel reflection, at current solar zenith angle, for broadband radn
REAL(KIND=real_jlslsm) :: rrr(nd_points)
! repeated for a single representative angle (cosz_e) and broadband radn
REAL(KIND=real_jlslsm) :: rrr_cosz_e(1)
! water to water reflectance
REAL(KIND=real_jlslsm) :: r22(nd_points)
! irradiance reflectance, just below the surface
REAL(KIND=real_jlslsm) :: r00(nd_points)
! irradiance reflectance, just below the surface, for a reference cosz:
REAL(KIND=real_jlslsm) :: r00_cosz_e(nd_points)
! surface direct albedo, at representative angle (for sub-surface diffuse)
REAL(KIND=real_jlslsm) :: r11_cosz_e(nd_points)
! foam water fraction  (Koepke, 1984, doi:10.1364/AO.23.001816)
REAL(KIND=real_jlslsm) :: foam_frac(nd_points)

! number of wavelenght subsamples to use (n_band_subsample or 1 for broadband)
INTEGER :: nd_wl
! indices of the subsampled wavelenghts that are in each band:
INTEGER, ALLOCATABLE :: ind_band_wls(:,:)

! Is this a broadband calcualtion?
LOGICAL :: l_broadband

! local variables, for this implementation of Jin 2011:
! the min and max wavelengths to calculate (out of range wl will be capped)
REAL(KIND=real_jlslsm), PARAMETER :: wl_min = 2.0e-8 ! 200nm
REAL(KIND=real_jlslsm), PARAMETER :: wl_max = 5.0e-6 ! 5000nm
! the number of times to subsample within the spectral bands:
! The number of wavelengths to subsample the band depends on how many bands
! we have for the whole spectrum (need to subsample more for wide bands):
! See JULES ticket #100 details for this:
INTEGER, PARAMETER :: n_band_subsample = 50
! method to aggregate the subsampled wavelengths back into bands:
! 1 - simple linear mean
! 2 - weighted average, mean surface solar flux
INTEGER, PARAMETER :: i_aggregate_method = 1

! loop indices: point, band, wavelength submampling the waveband
INTEGER :: ipt, ibd, iwl

! Error code and message:
INTEGER ::              errcode
CHARACTER(LEN=errormessagelength) :: ERRMSG
REAL(KIND=jprb)   :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='JIN11_OSA'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! quick subroutine that checks the values of the inputs:
CALL Jin11_check_inputs(nd_points, cosz_in, ws_10m_in, chloro_in,              &
                        cosz, ws_10m, chloro)

! calculate the sigma wind speed parameter, from
! Cox and Munk, 1954 (doi:10.1364/JOSA.44.000838)
DO ipt = 1, nd_points
  sigma(ipt) = SQRT(0.003+0.00512 * ws_10m(ipt))
END DO

! with sigma set, can find the fit to eq 4 for the actual Solar Zenith Angle
CALL Jin11_regr_eq4(nd_points, nd_points, cosz, sigma, zmod_cosz)
! and on the effective incidence angle for subsurface reflection:
CALL Jin11_regr_eq4(1, nd_points, cosz_e, sigma, zmod_cosz_e)

! Test to see if this calculation is for broadband albedos,
! and setup subsamples of the spectral bands:
IF (nd_bands == 1 .AND. band_bot(1) < 0.0 .AND. band_top(1) < 0.0) THEN
  ! onyl need to do one calculation:
  l_broadband = .TRUE.
  nd_wl = 1
ELSE
  ! calculating multiple wavelengths, per band:
  l_broadband = .FALSE.
  ! total number of wavelengths - it is tempting to reduce this to
  !       nd_bands * (n_band_subsample-1) + 1
  ! but we cannot assume that the band_top of one band is the same as the
  ! band_bot of the next one.
  nd_wl = nd_bands * n_band_subsample
END IF


! Allocate and set up the arrays that define the subsampling wavelenghts:
ALLOCATE(wl(nd_wl))
ALLOCATE(refm(nd_wl))
ALLOCATE(ind_band_wls(nd_bands, n_band_subsample))
! and the combined direct/diffuse albedo results, subsampled:
ALLOCATE(alb_dr_wl(nd_points, nd_wl))
ALLOCATE(alb_df_wl(nd_points, nd_wl))

IF (l_broadband) THEN
  wl(1) = -1.0
  ind_band_wls(1,1) = 1

  ! the direct and diffuse sub-surface albedos are constant:
  alb_w_dr(:) = 0.006
  alb_w_df(:) = 0.006

ELSE ! not broadband
  ! initialise for safety. Will be properly populated later.
  alb_w_dr(:) = 0.0
  alb_w_df(:) = 0.0

  DO ibd = 1, nd_bands
    ! check the band is not tryig to be broadband (top or bottom < 0)
    IF (band_top(ibd) < 0 .OR. band_bot(ibd) < 0) THEN
      errcode = 1
      ERRMSG  = 'Attempting to mix broadband and spectral bands'
      CALL ereport ('Jin11_osa',errcode,ERRMSG)
    END IF

    bandwidth =  band_top(ibd) - band_bot(ibd)
    wl_incr = bandwidth / (n_band_subsample-1)
    DO iwl = 1, n_band_subsample
      ! this is an index of the wavelengths in each band:
      ind_band_wls(ibd,iwl) = iwl + ((ibd-1) *  n_band_subsample)
      ! and the wavelengths themselves:
      wl(ind_band_wls(ibd,iwl)) = band_bot(ibd) + wl_incr * (iwl-1)
      ! Test to see if this wavelength is out of range:
      IF (wl(ind_band_wls(ibd,iwl)) < wl_min) THEN
        wl(ind_band_wls(ibd,iwl)) = wl_min
      ELSE IF (wl(ind_band_wls(ibd,iwl)) > wl_max) THEN
        wl(ind_band_wls(ibd,iwl)) = wl_max
      END IF ! ends test on wl out of range
    END DO ! ends loop over wavelengths in the band

  END DO ! ends loop over bands to set up subsample wavelengths
END IF ! ends the else condition on l_broadband

!---------------------------------------------------------------------
! Section 1: parameters that can be done outside the wavelength loop:

IF (l_broadband) THEN
  ! broadband calculations just used the bb refractive index of water and air
  refm(1) = bb_refm
ELSE
  ! calculate the relative refractive index of water and air for these
  ! subsample wavelengths:
  CALL Jin11_get_refm(nd_wl, wl, refm)

  ! water to water reflectance (eq 7 in Jin 2011), no wl dependence:
  DO ipt = 1, nd_points
    r22(ipt) = 0.48168549 - (0.014894708 * sigma(ipt)) -                       &
                           ( 0.20703885 * (sigma(ipt)**2) )
  END DO

  xx2_bb_cosz_e(1) = SQRT( 1.0 - (1.0 - cosz_e(1)**2) / bb_refm**2 )

  rrr_cosz_e(1) = 0.5 * ( ( ( xx2_bb_cosz_e(1) - (bb_refm * cosz_e(1)) ) /     &
                          ( xx2_bb_cosz_e(1) + (bb_refm * cosz_e(1)) ) )**2 +  &
                        ( ( cosz_e(1) - (bb_refm * xx2_bb_cosz_e(1)) ) /       &
                          ( cosz_e(1) + (bb_refm * xx2_bb_cosz_e(1)) ) )**2 )
END IF

!$OMP PARALLEL DEFAULT(NONE)                                                   &
!$OMP PRIVATE(iwl, ipt, xx2, rr0, alb_s_dr, alb_s_df, r00,                     &
!$OMP         xx2_cosz_e, rr0_cosz_e, r11_cosz_e, r00_cosz_e,                  &
!$OMP         alb_dr_tmp, alb_df_tmp)                                          &
!$OMP FIRSTPRIVATE(alb_w_dr, alb_w_df)                                         &
!$OMP SHARED(nd_wl, nd_points, cosz, rrr, refm, zmod_cosz,                     &
#if defined(GNU_FORTRAN)
!$OMP        cosz_e,                                                           &
#endif
!$OMP        sigma, l_broadband, wl, chloro, r22, zmod_cosz_e,                 &
!$OMP        rrr_cosz_e, foam_frac, alb_dr_wl, alb_df_wl,                      &
!$OMP        xx2_bb, ws_10m, osa, nd_bands, ind_band_wls)

! angle and refm term in fresnel reflectance, as below, but with a
! broadband refm
!$OMP DO SCHEDULE(STATIC)
DO ipt = 1, nd_points
  xx2_bb(ipt) = SQRT(1.0 - (1.0 - cosz(ipt)**2) / bb_refm**2)

  ! rrr, the fresnel reflectance for at each point, for the broadband refm:
  rrr(ipt) = 0.5 * ( ( ( xx2_bb(ipt) - (bb_refm * cosz(ipt)) ) /               &
                       ( xx2_bb(ipt) + (bb_refm * cosz(ipt)) ) )**2 +          &
                     ( ( cosz(ipt) - (bb_refm * xx2_bb(ipt)) ) /               &
                       ( cosz(ipt) + (bb_refm * xx2_bb(ipt)) ) )**2 )

  ! the foam/whitecap fraction. (Eq 16 of Jin 2011, from Koepke 1984,
  ! doi:10.1364/AO.23.001816)
  foam_frac(ipt) = MIN(1.0, (2.95e-6) * ws_10m(ipt)**3.52 )
END DO
!$OMP END DO

!---------------------------------------------------------------------
! Section 2: main wavelength loop:
!$OMP DO SCHEDULE(STATIC)
DO iwl = 1, nd_wl
  ! angle and refm term in fresnel reflectance:
  DO ipt = 1, nd_points
    xx2(ipt) = SQRT(1.0 - (1.0 - cosz(ipt)**2) / refm(iwl)**2)

    ! fresnel reflection, at current solar zenith angle and wavelength:
    rr0(ipt) = 0.5 * ( ( ( xx2(ipt) - (refm(iwl) * cosz(ipt)) ) /              &
                         ( xx2(ipt) + (refm(iwl) * cosz(ipt)) ) )**2 +         &
                       ( ( cosz(ipt) - (refm(iwl) * xx2(ipt)) ) /              &
                         ( cosz(ipt) + (refm(iwl) * xx2(ipt)) ) )**2 )

    ! This gives the direct surface albedo (eq 1 in Jin 2011):
    alb_s_dr(ipt) = rr0(ipt) - ( (rr0(ipt) / rrr(ipt)) * zmod_cosz(ipt) )

    ! and this is the surface diffuse (Eq 5a in Jin 2011):
    alb_s_df(ipt) = s_df_fit(1) + s_df_fit(2) * sigma(ipt) +                   &
                    s_df_fit(3) * refm(iwl) + s_df_fit(4) * sigma(ipt) *       &
                    refm(iwl)
  END DO

  ! subsurface albedos, complicated for spectral case,
  ! (broadband values set above)
  IF ( .NOT. l_broadband) THEN
    ! the direct sub-surface albedo uses the actual solar zenith angle:
    !
    ! irradiance reflectance, just below the surface (eq 8),
    ! with varying solar zenith angle:
    CALL Jin11_get_r0(nd_points, nd_points, wl(iwl), cosz, chloro, r00)

    ! direct water-leaving albedo (eq 6 of Jin 2011):
    DO ipt = 1, nd_points
      alb_w_dr(ipt) = r00(ipt) * (1.0 - r22(ipt)) * (1.0 - alb_s_dr(ipt)) /    &
                                                 (1.0 - r00(ipt) * r22(ipt))
    END DO

    ! for the diffuse sub-surface/water leaving albedo, the same calculation
    ! is repeated, but assuming a representative angle (cosz_e = 0.676),
    ! Morel and Gentili, 1991 (doi:10.1364/AO.30.004427).
    !
    ! calculate the xx2 for the cosz_e, at this wavelength:
    xx2_cosz_e(1) = SQRT( 1.0 - (1.0 - cosz_e(1)**2) / refm(iwl)**2 )

    rr0_cosz_e(1) = 0.5 * ( ( ( xx2_cosz_e(1) - (refm(iwl) * cosz_e(1)) ) /    &
                            ( xx2_cosz_e(1) + (refm(iwl) * cosz_e(1)) ) )**2 + &
                          ( ( cosz_e(1) - (refm(iwl) * xx2_cosz_e(1)) ) /      &
                            ( cosz_e(1) + (refm(iwl) * xx2_cosz_e(1)) ) )**2 )

    CALL Jin11_get_r0(1, nd_points, wl(iwl), cosz_e, chloro, r00_cosz_e)

    DO ipt = 1, nd_points
      r11_cosz_e(ipt) = rr0_cosz_e(1) - zmod_cosz_e(ipt) * rr0_cosz_e(1) /     &
                                                   rrr_cosz_e(1)

      ! now combine to give the diffuse sub-surface/water leaving albedo:
      alb_w_df(ipt) = r00_cosz_e(ipt) * (1.0 - r22(ipt)) *                     &
                     (1.0 - r11_cosz_e(ipt)) / (1.0 - r00_cosz_e(ipt) *        &
                      r22(ipt))
    END DO

  END IF ! ends test on .NOT.l_broadband

  ! these are all of the albedo terms:
  !write (6,*) 'alb_s_dr: ', alb_s_dr
  !write (6,*) 'alb_s_df: ', alb_s_df
  !write (6,*) 'alb_w_dr: ', alb_w_dr
  !write (6,*) 'alb_w_df: ', alb_w_df

  ! now combine the surface and sub-surface/water albedos, together with the
  ! foam fraction, to give a direct/diffuse albedo for this wavelength
  ! as per eq 17 of Jin 2011, direct first:
  DO ipt = 1, nd_points
    alb_dr_wl(ipt,iwl) = (1.0 - foam_frac(ipt)) * (alb_s_dr(ipt) + alb_w_dr(ipt)) + &
                          foam_frac(ipt) * albedo_whitecap
    ! then diffuse:
    alb_df_wl(ipt,iwl) = (1.0 - foam_frac(ipt)) * (alb_s_df(ipt) + alb_w_df(ipt)) + &
                          foam_frac(ipt) * albedo_whitecap
  END DO

END DO ! ends main wavelength loop:
!$OMP END DO NOWAIT

!---------------------------------------------------------------------
! Section 3: combine the subsampled wavelengths back onto bands

! combine the component albedos into a single array to be output:
! reminder: osa(points, 1 ,bands)  is DIRECT albedo
!           osa(points, 2 ,bands)  is DIFFUSE albedo

!$OMP DO SCHEDULE(STATIC)
DO ibd = 1, nd_bands
  DO ipt = 1, nd_points
    osa(ipt,:,ibd) = 0.0
  END DO
END DO
!$OMP END DO


IF (i_aggregate_method == 1) THEN
  IF (l_broadband) THEN
!$OMP DO SCHEDULE(STATIC)
    DO ipt = 1, nd_points
      osa(ipt, 1, 1) = alb_dr_wl(ipt,1)
      osa(ipt, 2, 1) = alb_df_wl(ipt,1)
    END DO
!$OMP END DO
  ELSE
    ! this method uses a simple linear mean to combine the albedos:
!$OMP DO SCHEDULE(STATIC)
    DO ibd = 1, nd_bands
      ! reset the tmp arrays:
      DO ipt = 1, nd_points
        alb_dr_tmp(ipt) = 0.0
        alb_df_tmp(ipt) = 0.0
      END DO

      DO iwl = 1, n_band_subsample
        DO ipt = 1, nd_points
          alb_dr_tmp(ipt) = alb_dr_tmp(ipt) +                                  &
                            alb_dr_wl(ipt,ind_band_wls(ibd,iwl))
          ! add the values up:
          alb_df_tmp(ipt) = alb_df_tmp(ipt) +                                  &
                            alb_df_wl(ipt,ind_band_wls(ibd,iwl))
        END DO
      END DO
      ! and now divide by n_band_subsample to give the final values:
      DO ipt = 1, nd_points
        osa(ipt, 1, ibd) = alb_dr_tmp(ipt) / n_band_subsample
        osa(ipt, 2, ibd) = alb_df_tmp(ipt) / n_band_subsample
      END DO
    END DO ! ends loop over bands to aggregate albedos from wavelengths:
!$OMP END DO
  END IF ! ends else test on l_broadband
  !  ELSE IF (i_aggregate_method == 2) THEN
  !    ! TODO: write some code here, with guidance from James Manners:
  !
  !    WRITE (6,*) 'need to write some code here!'
  !
END IF

!$OMP END PARALLEL
! Deallocate arrays:
DEALLOCATE(alb_df_wl)
DEALLOCATE(alb_dr_wl)
DEALLOCATE(ind_band_wls)
DEALLOCATE(refm)
DEALLOCATE(wl)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE Jin11_osa
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
SUBROUTINE Jin11_check_inputs(nd_points, cosz_in, ws_10m_in, chloro_in,        &
                              cosz, ws_10m, chloro)
! Purpose: copies the input arrays to a new array, and caps their values
!          to sensible numbers:

IMPLICIT NONE

! number of points, spatially
INTEGER, INTENT(IN) :: nd_points
! cosine of the solar zenith angle
REAL(KIND=real_jlslsm), INTENT(IN) :: cosz_in(nd_points)
! windspeed at 10m (ms-1)
REAL(KIND=real_jlslsm), INTENT(IN) :: ws_10m_in(nd_points)
! Nr sea surface chlorophyll content (kg m-3)
REAL(KIND=real_jlslsm), INTENT(IN) :: chloro_in(nd_points)

! output copies of the inputs, capped at sensible values:
REAL(KIND=real_jlslsm), INTENT(OUT) :: cosz(nd_points)
REAL(KIND=real_jlslsm), INTENT(OUT) :: ws_10m(nd_points)
REAL(KIND=real_jlslsm), INTENT(OUT) :: chloro(nd_points)

! swithces, if a variable has been capped:
LOGICAL :: l_cosz_capped, l_ws_10m_capped, l_chloro_capped

! loop variables
INTEGER :: ipt

! Error code and message:
INTEGER ::              errcode
CHARACTER(LEN=errormessagelength) :: ERRMSG

! check the cosz, should be between 0.0 and 1.0:
l_cosz_capped = .FALSE.
DO ipt = 1, nd_points
  IF (cosz_in(ipt) < 0.0) THEN
    l_cosz_capped = .TRUE.
    cosz(ipt) = 0.0
  ELSE IF (cosz_in(ipt) > 1.0) THEN
    l_cosz_capped = .TRUE.
    cosz(ipt) = 1.0
  ELSE
    cosz(ipt) = cosz_in(ipt)
  END IF
END DO
IF (l_cosz_capped) THEN
  errcode = 1
  ERRMSG  = 'Cos(sza) out of range 0-1'
  CALL ereport ('Jin11_check_inputs',errcode,ERRMSG)
END IF

! check the wind speed, should be between 0.0, and not too large:
l_ws_10m_capped = .FALSE.
DO ipt = 1, nd_points
  IF (ws_10m_in(ipt) < 0.0) THEN
    l_ws_10m_capped = .TRUE.
    ws_10m(ipt) = 0.0
  ELSE IF (ws_10m_in(ipt) > 100.0) THEN
    l_ws_10m_capped = .TRUE.
    ws_10m(ipt) = 100.0
  ELSE
    ws_10m(ipt) = ws_10m_in(ipt)
  END IF
END DO
IF (l_ws_10m_capped) THEN
  ! Use ereport with a -ve status to issue warnings
  errcode = -1
  CALL ereport('Jin11_check_inputs', errcode,                                  &
               '10m wind speed out of range 0.0 - 100.0 ms-1')
END IF

! check the chlorophyll content, should be between 0.0, and 100 gm-3:
l_chloro_capped = .FALSE.
DO ipt = 1, nd_points
  IF (chloro_in(ipt) < 0.0) THEN
    l_chloro_capped = .TRUE.
    chloro(ipt) = 0.0
  ELSE IF (chloro_in(ipt) > 100.0e-3) THEN
    l_chloro_capped = .TRUE.
    chloro(ipt) = 100.0e-3
  ELSE
    chloro(ipt) = chloro_in(ipt)
  END IF
END DO
IF (l_chloro_capped) THEN
  ! Use ereport with a -ve status to issue warnings
  errcode = -2
  CALL ereport('Jin11_check_inputs', errcode,                                  &
               'chlorophyll content out of range 0.0 - 100.0 gm-3')
END IF

END SUBROUTINE Jin11_check_inputs
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
SUBROUTINE Jin11_lin_interp_real(nd_in, nd_out, x_in, y_in, x_out, y_out)
! Purpose: a simple linear interpolation, with no extrapolation.
! x_in must be monotonically increasing, or it will not work correctly.

IMPLICIT NONE

! number of input x and y values
INTEGER, INTENT(IN) :: nd_in
! number of output x and y values
INTEGER, INTENT(IN) :: nd_out
! x of input
REAL(KIND=real_jlslsm), INTENT(IN) :: x_in(nd_in)
! y of input
REAL(KIND=real_jlslsm), INTENT(IN) :: y_in(nd_in)
! x of output
REAL(KIND=real_jlslsm), INTENT(IN) :: x_out(nd_out)

! output values of y, at x_out
REAL(KIND=real_jlslsm), INTENT(OUT) :: y_out(nd_out)

! local variables:
! differences in x and y:
REAL(KIND=real_jlslsm) :: d_x_in(nd_in-1)
REAL(KIND=real_jlslsm) :: d_y_in(nd_in-1)
! start point:
INTEGER :: st

! loop variables
INTEGER :: ii, jj

! Error code and message:
INTEGER ::              errcode
CHARACTER(LEN=errormessagelength) :: ERRMSG
REAL(KIND=jprb)   :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='JIN11_LIN_INTERP_REAL'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! differences of x and y input:
DO jj = 1, nd_in - 1
  d_x_in(jj) = x_in(jj+1) - x_in(jj)
  d_y_in(jj) = y_in(jj+1) - y_in(jj)
END DO ! ends loop over x_in


! now linearly interpolate to get y_out at x_out:
DO ii = 1, nd_out
  st = -1
  ! now look to see if we can find a start point:
  DO jj = 1, nd_in -1
    IF (x_in(jj+1) >= x_out(ii)) THEN
      st = jj
      EXIT ! break out of this loop on jj
    END IF
  END DO

  IF (st < 0) THEN
    ! no start point was found, trying to extrapolate out of range of data
    errcode = 1
    ERRMSG  = 'Attempting to interpolate out of range'
    CALL ereport ('Jin11_lin_interp_real',errcode,ERRMSG)
  ELSE
    y_out(ii) = y_in(st) + ( (x_out(ii) - x_in(st)) / d_x_in(st) ) * d_y_in(st)
  END IF

END DO ! ends loop over x_out

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE Jin11_lin_interp_real
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
SUBROUTINE Jin11_regr_eq4(nd_cosz, nd_points, cosz, sigma, zmod)
! Purpose: calculates equation 4 in Jin 2011, which is derived from
!          multiple regression of COART data.
!

IMPLICIT NONE

! number of angles to calculate reflection:
INTEGER, INTENT(IN) :: nd_cosz
! number of points, spatially
INTEGER, INTENT(IN) :: nd_points
! cosine of the solar zenith angle:
REAL(KIND=real_jlslsm), INTENT(IN) :: cosz(nd_cosz)
! sigma (wind speed param from Cox and Munk)
REAL(KIND=real_jlslsm), INTENT(IN) :: sigma(nd_points)

! output:
REAL(KIND=real_jlslsm), INTENT(OUT) :: zmod(nd_points)

! local vars:
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
                   p(1:11) = [0.0152, -1.7873, 6.8972, -8.5778, 4.071,         &
               -7.6446, 0.1643, -7.8409, -3.5639, -2.3588, 10.0538 ]

! loop vars:
INTEGER :: ipt

! Error code and message:
INTEGER ::              errcode
CHARACTER(LEN=errormessagelength) :: ERRMSG
REAL(KIND=jprb)   :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='JIN11_REGR_EQ4'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF (nd_cosz == 1) THEN
  ! calculating the regression function with a variable wind speeed
  ! but a single solar zenith angle representative of sub-surface reflection:
  DO ipt = 1, nd_points
    zmod(ipt) = (p(1) + p(2) * cosz(1) + p(3) * cosz(1)**2 +                   &
                 p(4) * cosz(1)**3 + p(5) * sigma(ipt) +                       &
                 p(6) * cosz(1) * sigma(ipt) ) *                               &
                EXP( p(7) + p(8) * cosz(1) + p(9) * cosz(1)**2 +               &
                p(10) * sigma(ipt) + p(11) * cosz(1) * sigma(ipt) )
  END DO
ELSE
  ! calculating the regression function with both the wind speeed and
  ! solar zenith angle varying
  IF (nd_cosz /= nd_points) THEN
    errcode = 1
    ERRMSG  = 'Array length mismatch between wind and solar zenith angle'
    CALL ereport ('Jin11_regr_eq4',errcode,ERRMSG)
  END IF

  DO ipt = 1, nd_points
    zmod(ipt) = (p(1) + p(2) * cosz(ipt) + p(3) * cosz(ipt)**2 +               &
                 p(4) * cosz(ipt)**3 + p(5) * sigma(ipt) +                     &
                 p(6) * cosz(ipt) * sigma(ipt) ) *                             &
                EXP( p(7) + p(8) * cosz(ipt) + p(9) * cosz(ipt)**2 +           &
                p(10) * sigma(ipt) + p(11) * cosz(ipt) * sigma(ipt) )
  END DO
END IF
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE Jin11_regr_eq4
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
SUBROUTINE Jin11_get_refm(nd_wl, wl, refm)
! Purpose: interpolates the relative refractive index of water and air
!          (n in the Jin 2011 paper), for a given set of wavelengths, using
!          linear interpolation against values from a lookup table.
!
!          Values from the supplementary information of the Jin 2011 paper
!          (with an extraplolated value at 5001nm added)

IMPLICIT NONE

! number of wavelengths
INTEGER, INTENT(IN) :: nd_wl
! wavelengths at which to calculate refm
REAL(KIND=real_jlslsm), INTENT(IN) :: wl(nd_wl)

! output:
REAL(KIND=real_jlslsm), INTENT(OUT) :: refm(nd_wl)

! local vars:
! the relative refractive index of water and air, for broadband VIS:
REAL(KIND=real_jlslsm), PARAMETER :: bb_refm = 1.34

! apply a limit to refm, so it stays below bb_refm.
! The supplementary material incluces a cap on refm, which isnt in the paper.
! After checking with the author on this, this cap can be ignored.
! Note: it does not make a large difference, although it is not
! negligible either.
LOGICAL, PARAMETER :: l_limit_refm = .FALSE.

! vars for the lookup table:
INTEGER, PARAMETER :: nd_lu = 401
REAL(KIND=real_jlslsm) :: wl_lu(nd_lu)
REAL(KIND=real_jlslsm) :: refm_lu(nd_lu)

! loop vars:
INTEGER :: iwl

REAL(KIND=jprb)   :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='JIN11_GET_REFM'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! ! wavelengths of the lookup table:
wl_lu(1:nd_lu)=[200.0e-9, 210.0e-9, 220.0e-9, 230.0e-9, 240.0e-9,              &
                 250.0e-9, 260.0e-9, 270.0e-9, 280.0e-9, 290.0e-9,             &
                 300.0e-9, 310.0e-9, 320.0e-9, 330.0e-9, 340.0e-9,             &
                 350.0e-9, 360.0e-9, 370.0e-9, 380.0e-9, 390.0e-9,             &
                 400.0e-9, 410.0e-9, 420.0e-9, 430.0e-9, 440.0e-9,             &
                 450.0e-9, 460.0e-9, 470.0e-9, 480.0e-9, 490.0e-9,             &
                 500.0e-9, 510.0e-9, 520.0e-9, 530.0e-9, 540.0e-9,             &
                 550.0e-9, 560.0e-9, 570.0e-9, 580.0e-9, 590.0e-9,             &
                 600.0e-9, 610.0e-9, 620.0e-9, 630.0e-9, 640.0e-9,             &
                 650.0e-9, 660.0e-9, 670.0e-9, 680.0e-9, 690.0e-9,             &
                 700.0e-9, 710.0e-9, 720.0e-9, 730.0e-9, 740.0e-9,             &
                 750.0e-9, 760.0e-9, 770.0e-9, 780.0e-9, 790.0e-9,             &
                 800.0e-9, 810.0e-9, 820.0e-9, 830.0e-9, 840.0e-9,             &
                 850.0e-9, 860.0e-9, 870.0e-9, 880.0e-9, 890.0e-9,             &
                 900.0e-9, 910.0e-9, 920.0e-9, 930.0e-9, 940.0e-9,             &
                 950.0e-9, 960.0e-9, 970.0e-9, 980.0e-9, 990.0e-9,             &
                1000.0e-9,1010.0e-9,1020.0e-9,1030.0e-9,1040.0e-9,             &
                1050.0e-9,1060.0e-9,1070.0e-9,1080.0e-9,1090.0e-9,             &
                1100.0e-9,1110.0e-9,1120.0e-9,1130.0e-9,1140.0e-9,             &
                1150.0e-9,1160.0e-9,1170.0e-9,1180.0e-9,1190.0e-9,             &
                1200.0e-9,1210.0e-9,1220.0e-9,1230.0e-9,1240.0e-9,             &
                1250.0e-9,1260.0e-9,1270.0e-9,1280.0e-9,1290.0e-9,             &
                1300.0e-9,1310.0e-9,1320.0e-9,1330.0e-9,1340.0e-9,             &
                1350.0e-9,1360.0e-9,1370.0e-9,1380.0e-9,1390.0e-9,             &
                1400.0e-9,1410.0e-9,1420.0e-9,1430.0e-9,1440.0e-9,             &
                1450.0e-9,1460.0e-9,1470.0e-9,1480.0e-9,1490.0e-9,             &
                1500.0e-9,1510.0e-9,1520.0e-9,1530.0e-9,1540.0e-9,             &
                1550.0e-9,1560.0e-9,1570.0e-9,1580.0e-9,1590.0e-9,             &
                1600.0e-9,1610.0e-9,1620.0e-9,1630.0e-9,1640.0e-9,             &
                1650.0e-9,1660.0e-9,1670.0e-9,1680.0e-9,1690.0e-9,             &
                1700.0e-9,1710.0e-9,1720.0e-9,1730.0e-9,1740.0e-9,             &
                1750.0e-9,1760.0e-9,1770.0e-9,1780.0e-9,1790.0e-9,             &
                1800.0e-9,1810.0e-9,1820.0e-9,1830.0e-9,1840.0e-9,             &
                1850.0e-9,1860.0e-9,1870.0e-9,1880.0e-9,1890.0e-9,             &
                1900.0e-9,1910.0e-9,1920.0e-9,1930.0e-9,1940.0e-9,             &
                1950.0e-9,1960.0e-9,1970.0e-9,1980.0e-9,1990.0e-9,             &
                2000.0e-9,2010.0e-9,2020.0e-9,2030.0e-9,2040.0e-9,             &
                2050.0e-9,2060.0e-9,2070.0e-9,2080.0e-9,2090.0e-9,             &
                2100.0e-9,2110.0e-9,2120.0e-9,2130.0e-9,2140.0e-9,             &
                2150.0e-9,2160.0e-9,2170.0e-9,2180.0e-9,2190.0e-9,             &
                2200.0e-9,2210.0e-9,2220.0e-9,2230.0e-9,2240.0e-9,             &
                2250.0e-9,2260.0e-9,2270.0e-9,2280.0e-9,2290.0e-9,             &
                2300.0e-9,2310.0e-9,2320.0e-9,2330.0e-9,2340.0e-9,             &
                2350.0e-9,2360.0e-9,2370.0e-9,2380.0e-9,2390.0e-9,             &
                2400.0e-9,2410.0e-9,2420.0e-9,2430.0e-9,2440.0e-9,             &
                2450.0e-9,2460.0e-9,2470.0e-9,2480.0e-9,2490.0e-9,             &
                2500.0e-9,2510.0e-9,2520.0e-9,2530.0e-9,2540.0e-9,             &
                2550.0e-9,2560.0e-9,2570.0e-9,2580.0e-9,2590.0e-9,             &
                2600.0e-9,2610.0e-9,2620.0e-9,2630.0e-9,2640.0e-9,             &
                2650.0e-9,2660.0e-9,2670.0e-9,2680.0e-9,2690.0e-9,             &
                2700.0e-9,2710.0e-9,2720.0e-9,2730.0e-9,2740.0e-9,             &
                2750.0e-9,2760.0e-9,2770.0e-9,2780.0e-9,2790.0e-9,             &
                2800.0e-9,2810.0e-9,2820.0e-9,2830.0e-9,2840.0e-9,             &
                2850.0e-9,2860.0e-9,2870.0e-9,2880.0e-9,2890.0e-9,             &
                2900.0e-9,2910.0e-9,2920.0e-9,2930.0e-9,2940.0e-9,             &
                2950.0e-9,2960.0e-9,2970.0e-9,2980.0e-9,2990.0e-9,             &
                3000.0e-9,3010.0e-9,3020.0e-9,3030.0e-9,3040.0e-9,             &
                3050.0e-9,3060.0e-9,3070.0e-9,3080.0e-9,3090.0e-9,             &
                3100.0e-9,3110.0e-9,3120.0e-9,3130.0e-9,3140.0e-9,             &
                3150.0e-9,3160.0e-9,3170.0e-9,3180.0e-9,3190.0e-9,             &
                3200.0e-9,3210.0e-9,3220.0e-9,3230.0e-9,3240.0e-9,             &
                3250.0e-9,3260.0e-9,3270.0e-9,3280.0e-9,3290.0e-9,             &
                3300.0e-9,3310.0e-9,3320.0e-9,3330.0e-9,3340.0e-9,             &
                3350.0e-9,3360.0e-9,3370.0e-9,3380.0e-9,3390.0e-9,             &
                3400.0e-9,3410.0e-9,3420.0e-9,3430.0e-9,3440.0e-9,             &
                3450.0e-9,3460.0e-9,3470.0e-9,3480.0e-9,3490.0e-9,             &
                3500.0e-9,3510.0e-9,3520.0e-9,3530.0e-9,3540.0e-9,             &
                3550.0e-9,3560.0e-9,3570.0e-9,3580.0e-9,3590.0e-9,             &
                3600.0e-9,3610.0e-9,3620.0e-9,3630.0e-9,3640.0e-9,             &
                3650.0e-9,3660.0e-9,3670.0e-9,3680.0e-9,3690.0e-9,             &
                3700.0e-9,3710.0e-9,3720.0e-9,3730.0e-9,3740.0e-9,             &
                3750.0e-9,3760.0e-9,3770.0e-9,3780.0e-9,3790.0e-9,             &
                3800.0e-9,3810.0e-9,3820.0e-9,3830.0e-9,3840.0e-9,             &
                3850.0e-9,3860.0e-9,3870.0e-9,3880.0e-9,3890.0e-9,             &
                3900.0e-9,3910.0e-9,3920.0e-9,3930.0e-9,3940.0e-9,             &
                3950.0e-9,3960.0e-9,3970.0e-9,3980.0e-9,3990.0e-9,             &
                4000.0e-9,4010.0e-9,4020.0e-9,4030.0e-9,4040.0e-9,             &
                4050.0e-9,4060.0e-9,4070.0e-9,4080.0e-9,4090.0e-9,             &
                4100.0e-9,4110.0e-9,4120.0e-9,4130.0e-9,4140.0e-9,             &
                4150.0e-9,4160.0e-9,4170.0e-9,4180.0e-9,4190.0e-9,             &
                5001.0e-9 ]

refm_lu(1:nd_lu) = [1.4517, 1.4345, 1.4214, 1.4108, 1.4022,                    &
                     1.3950, 1.3889, 1.3836, 1.3790, 1.3750,                   &
                     1.3714, 1.3682, 1.3653, 1.3627, 1.3604,                   &
                     1.3582, 1.3562, 1.3544, 1.3528, 1.3512,                   &
                     1.3498, 1.3484, 1.3472, 1.3460, 1.3449,                   &
                     1.3439, 1.3429, 1.3420, 1.3411, 1.3402,                   &
                     1.3394, 1.3387, 1.3379, 1.3372, 1.3366,                   &
                     1.3359, 1.3353, 1.3347, 1.3342, 1.3336,                   &
                     1.3331, 1.3326, 1.3321, 1.3316, 1.3311,                   &
                     1.3307, 1.3303, 1.3299, 1.3295, 1.3291,                   &
                     1.3288, 1.3284, 1.3281, 1.3278, 1.3275,                   &
                     1.3272, 1.3269, 1.3266, 1.3264, 1.3261,                   &
                     1.3259, 1.3256, 1.3254, 1.3252, 1.3249,                   &
                     1.3247, 1.3245, 1.3243, 1.3241, 1.3238,                   &
                     1.3236, 1.3234, 1.3232, 1.3230, 1.3228,                   &
                     1.3226, 1.3225, 1.3223, 1.3221, 1.3219,                   &
                     1.3217, 1.3215, 1.3213, 1.3211, 1.3210,                   &
                     1.3208, 1.3206, 1.3204, 1.3202, 1.3200,                   &
                     1.3198, 1.3197, 1.3195, 1.3193, 1.3191,                   &
                     1.3189, 1.3187, 1.3186, 1.3184, 1.3182,                   &
                     1.3180, 1.3178, 1.3176, 1.3174, 1.3172,                   &
                     1.3170, 1.3169, 1.3167, 1.3165, 1.3163,                   &
                     1.3161, 1.3159, 1.3156, 1.3154, 1.3152,                   &
                     1.3150, 1.3148, 1.3146, 1.3143, 1.3141,                   &
                     1.3139, 1.3137, 1.3135, 1.3134, 1.3132,                   &
                     1.3130, 1.3129, 1.3127, 1.3125, 1.3123,                   &
                     1.3121, 1.3119, 1.3116, 1.3114, 1.3111,                   &
                     1.3109, 1.3106, 1.3104, 1.3101, 1.3099,                   &
                     1.3096, 1.3094, 1.3091, 1.3088, 1.3086,                   &
                     1.3083, 1.3080, 1.3077, 1.3074, 1.3071,                   &
                     1.3068, 1.3065, 1.3061, 1.3058, 1.3055,                   &
                     1.3051, 1.3048, 1.3044, 1.3041, 1.3037,                   &
                     1.3034, 1.3030, 1.3026, 1.3022, 1.3017,                   &
                     1.3013, 1.3007, 1.3002, 1.2998, 1.2995,                   &
                     1.2990, 1.2988, 1.2988, 1.2987, 1.2986,                   &
                     1.2985, 1.2983, 1.2980, 1.2977, 1.2973,                   &
                     1.2969, 1.2965, 1.2960, 1.2955, 1.2950,                   &
                     1.2945, 1.2940, 1.2935, 1.2929, 1.2924,                   &
                     1.2918, 1.2913, 1.2907, 1.2901, 1.2895,                   &
                     1.2889, 1.2883, 1.2876, 1.2870, 1.2863,                   &
                     1.2856, 1.2849, 1.2842, 1.2835, 1.2828,                   &
                     1.2820, 1.2812, 1.2804, 1.2796, 1.2788,                   &
                     1.2779, 1.2770, 1.2761, 1.2751, 1.2741,                   &
                     1.2731, 1.2721, 1.2711, 1.2699, 1.2688,                   &
                     1.2676, 1.2664, 1.2652, 1.2639, 1.2625,                   &
                     1.2612, 1.2597, 1.2582, 1.2567, 1.2552,                   &
                     1.2535, 1.2518, 1.2499, 1.2480, 1.2459,                   &
                     1.2436, 1.2413, 1.2388, 1.2361, 1.2331,                   &
                     1.2299, 1.2263, 1.2223, 1.2182, 1.2140,                   &
                     1.2091, 1.2034, 1.1973, 1.1910, 1.1850,                   &
                     1.1780, 1.1691, 1.1564, 1.1489, 1.1430,                   &
                     1.1351, 1.1318, 1.1323, 1.1308, 1.1277,                   &
                     1.1294, 1.1286, 1.1255, 1.1271, 1.1348,                   &
                     1.1417, 1.1480, 1.1612, 1.1736, 1.1847,                   &
                     1.2008, 1.2177, 1.2336, 1.2505, 1.2678,                   &
                     1.2840, 1.3007, 1.3149, 1.3281, 1.3407,                   &
                     1.3539, 1.3661, 1.3782, 1.3903, 1.4024,                   &
                     1.4136, 1.4217, 1.4299, 1.4380, 1.4461,                   &
                     1.4528, 1.4557, 1.4587, 1.4616, 1.4645,                   &
                     1.4665, 1.4655, 1.4645, 1.4634, 1.4624,                   &
                     1.4613, 1.4589, 1.4566, 1.4543, 1.4519,                   &
                     1.4496, 1.4465, 1.4433, 1.4401, 1.4369,                   &
                     1.4338, 1.4304, 1.4271, 1.4237, 1.4203,                   &
                     1.4170, 1.4144, 1.4117, 1.4091, 1.4065,                   &
                     1.4041, 1.4020, 1.3999, 1.3977, 1.3956,                   &
                     1.3936, 1.3916, 1.3897, 1.3879, 1.3860,                   &
                     1.3841, 1.3824, 1.3808, 1.3791, 1.3774,                   &
                     1.3758, 1.3743, 1.3729, 1.3714, 1.3700,                   &
                     1.3685, 1.3673, 1.3660, 1.3647, 1.3635,                   &
                     1.3623, 1.3612, 1.3601, 1.3589, 1.3578,                   &
                     1.3568, 1.3558, 1.3548, 1.3538, 1.3528,                   &
                     1.3519, 1.3510, 1.3501, 1.3493, 1.3484,                   &
                     1.3476, 1.3468, 1.3460, 1.3452, 1.3444,                   &
                     1.3437, 1.3430, 1.3422, 1.3415, 1.3408,                   &
                     1.3401, 1.3395, 1.3388, 1.3382, 1.3375,                   &
                     1.3369, 1.3363, 1.3357, 1.3351, 1.3345,                   &
                     1.3339, 1.3333, 1.3328, 1.3323, 1.3317,                   &
                     1.3312, 1.3307, 1.3301, 1.3296, 1.3291,                   &
                     1.3286, 1.3281, 1.3276, 1.3271, 1.3266,                   &
                     1.3262, 1.3257, 1.3252, 1.3248, 1.3243,                   &
                     1.28375 ]

! now linearly interpolate refm, to the desired wavelengths:
CALL Jin11_lin_interp_real(nd_lu, nd_wl, wl_lu, refm_lu, wl, refm)

! limit refm, if required:
IF (l_limit_refm) THEN
  DO iwl = 1, nd_wl
    IF (refm(iwl) > bb_refm) THEN
      refm(iwl) = bb_refm
    END IF
  END DO
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE Jin11_get_refm
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
SUBROUTINE Jin11_get_r0(nd_cosz, nd_points, wl, cosz, chloro, r0)
! Purpose: calculates equation 8 in Jin 2011, irradiance reflectance,
!          just below the surface. This is a complcicated function of solar
!          zenith anngle, wavelength and chloropyll.
!          See Morel and Maritorena, 2001, doi:10.1029/2000JC000319

USE jules_science_fixes_mod, ONLY: l_fix_osa_chloro

IMPLICIT NONE

! number of angles to consider (can either be spatially varting or 1
! representative angle for deep water):
INTEGER, INTENT(IN) :: nd_cosz
! number of points, spatially
INTEGER, INTENT(IN) :: nd_points
! wavelength
REAL(KIND=real_jlslsm), INTENT(IN) :: wl(1)
! cosine of the solar zenith angle:
REAL(KIND=real_jlslsm), INTENT(IN) :: cosz(nd_cosz)
! chlorophyll content
REAL(KIND=real_jlslsm), INTENT(IN) :: chloro(nd_points)

! output:
REAL(KIND=real_jlslsm), INTENT(OUT) :: r0(nd_points)

! local vars:
REAL(KIND=real_jlslsm), PARAMETER :: aw440 = 0.00635
REAL(KIND=real_jlslsm) :: chlabs(1)
REAL(KIND=real_jlslsm) :: aw(1)
REAL(KIND=real_jlslsm) :: bw(1)
REAL(KIND=real_jlslsm) :: ylmd
REAL(KIND=real_jlslsm) :: ap(nd_points)
REAL(KIND=real_jlslsm) :: bp550(nd_points)
REAL(KIND=real_jlslsm) :: chl_v(nd_points)
REAL(KIND=real_jlslsm) :: bbp(nd_points)
REAL(KIND=real_jlslsm) :: hb(nd_points)
REAL(KIND=real_jlslsm) :: f(nd_points)
! parameters in this param are for wavelengths in nm:
REAL(KIND=real_jlslsm) :: wl_nm(1)
! and chlorophyll in mg m-3:
REAL(KIND=real_jlslsm) :: chloro_mgm3(nd_points)

! lookup table for chlabs (achl and wl_achl):
INTEGER, PARAMETER :: n_chlabs_lu = 38
REAL(KIND=real_jlslsm) :: chlabs_lu(n_chlabs_lu)
REAL(KIND=real_jlslsm) :: wl_chlabs(n_chlabs_lu)
! lookup table for aw (aw_lu and wl_aw):
INTEGER, PARAMETER :: n_aw_lu = 128
REAL(KIND=real_jlslsm) :: aw_lu(n_aw_lu)
REAL(KIND=real_jlslsm) :: wl_aw(n_aw_lu)
! lookup table for bw (bw_lu and wl_bw):
INTEGER, PARAMETER :: n_bw_lu = 62
REAL(KIND=real_jlslsm) :: bw_lu(n_bw_lu)
REAL(KIND=real_jlslsm) :: wl_bw(n_bw_lu)

! loop vars:
INTEGER :: ipt

! Error code and message:
INTEGER :: errcode
CHARACTER(LEN=errormessagelength) :: ERRMSG

REAL(KIND=jprb)   :: zhook_handle
CHARACTER(LEN=*), PARAMETER :: RoutineName='JIN11_GET_R0'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Consistency check
! This routine has been written with the assumption that nd_cosz == 1 or
! nd_cosz == nd_points.
IF ( .NOT. (nd_cosz == 1 .OR. nd_cosz == nd_points)) THEN
  errcode = 7
  WRITE(ERRMSG,'(A,I0,A,I0,A)')  'nd_cosz = ', nd_cosz,                        &
      ' instead of 1 or nd_points (',nd_points,')'
  CALL ereport(RoutineName, errcode, ERRMSG)
END IF

! the wavelengths here are defined in nm:
wl_nm = wl * 1.0e9
! and chlorophyll content in g m-3:
IF (l_fix_osa_chloro) THEN
  DO ipt = 1, nd_points
    chloro_mgm3(ipt) = chloro(ipt) * 1.0e6
  END DO
ELSE
  DO ipt = 1, nd_points
    chloro_mgm3(ipt) = chloro(ipt) * 1.0e3
  END DO
END IF

! interpolate a set of coefficients to the current wavelength:
chlabs_lu = [ 0.410, 0.560, 0.570, 0.590, 0.610, 0.660, 0.687, 0.828,          &
               0.913, 0.973, 1.000, 0.944, 0.917, 0.870, 0.798, 0.750,         &
               0.668, 0.618, 0.528, 0.474, 0.416, 0.357, 0.294, 0.276,         &
               0.291, 0.282, 0.236, 0.252, 0.276, 0.317, 0.334, 0.356,         &
               0.441, 0.595, 0.502, 0.329, 0.215, 0.215 ]

wl_chlabs = [ 200.0, 350.0, 360.0, 370.0, 380.0, 390.0, 400.0, 410.0,          &
               420.0, 430.0, 440.0, 450.0, 460.0, 470.0, 480.0, 490.0,         &
               500.0, 510.0, 520.0, 530.0, 540.0, 550.0, 560.0, 570.0,         &
               580.0, 590.0, 600.0, 610.0, 620.0, 630.0, 640.0, 650.0,         &
               660.0, 670.0, 680.0, 690.0,700.0, 5000.0 ]
CALL Jin11_lin_interp_real(n_chlabs_lu,1,wl_chlabs,chlabs_lu,wl_nm,chlabs)

aw_lu = [  3.07000,  1.99000,  1.31000,  0.92700,  0.72000,  0.55900,          &
            0.45700,  0.37300,  0.28800,  0.21500,  0.14100,  0.10500,         &
            0.08440,  0.06780,  0.05610,  0.04630,  0.03790,  0.03000,         &
            0.02200,  0.01910,  0.01710,  0.01620,  0.01530,  0.01440,         &
            0.01450,  0.01450,  0.01560,  0.01560,  0.01760,  0.01960,         &
            0.02570,  0.03570,  0.03720,  0.03960,  0.03990,  0.04090,         &
            0.04160,  0.04170,  0.04280,  0.04340,  0.04470,  0.04520,         &
            0.04660,  0.04740,  0.04890,  0.05110,  0.05370,  0.05650,         &
            0.05930,  0.05960,  0.06060,  0.06190,  0.06400,  0.06420,         &
            0.06720,  0.06950,  0.07330,  0.07720,  0.08360,  0.08960,         &
            0.09890,  0.11000,  0.12200,  0.13510,  0.15160,  0.16720,         &
            0.19250,  0.22240,  0.24700,  0.25770,  0.26290,  0.26440,         &
            0.26650,  0.26780,  0.27070,  0.27550,  0.28100,  0.28340,         &
            0.29040,  0.29160,  0.29950,  0.30120,  0.30770,  0.31080,         &
            0.32200,  0.32500,  0.33500,  0.34000,  0.35800,  0.37100,         &
            0.39300,  0.41000,  0.42400,  0.42900,  0.43600,  0.43900,         &
            0.44800,  0.44800,  0.46100,  0.46500,  0.47800,  0.48600,         &
            0.50200,  0.51600,  0.53800,  0.55900,  0.59200,  0.62400,         &
            0.66300,  0.70400,  0.75600,  0.82700,  0.91400,  1.00700,         &
            1.11900,  1.23100,  1.35600,  1.48900,  1.67800,  1.79900,         &
            2.38000,  2.47000,  2.55000,  2.51000,  2.36000,  2.16000,         &
            2.07000,  2.07000 ]
wl_aw = [ 200.0, 210.0, 220.0, 230.0, 240.0, 250.0,                            &
           260.0, 270.0, 280.0, 290.0, 300.0, 310.0,                           &
           320.0, 330.0, 340.0, 350.0, 360.0, 370.0,                           &
           380.0, 390.0, 400.0, 410.0, 420.0, 430.0,                           &
           440.0, 450.0, 460.0, 470.0, 480.0, 490.0,                           &
           500.0, 510.0, 512.5, 515.0, 517.5, 520.0,                           &
           522.5, 525.0, 527.5, 530.0, 532.5, 535.0,                           &
           537.5, 540.0, 542.5, 545.0, 547.5, 550.0,                           &
           552.5, 555.0, 557.5, 560.0, 562.5, 565.0,                           &
           567.5, 570.0, 572.5, 575.0, 577.5, 580.0,                           &
           582.5, 585.0, 587.5, 590.0, 592.5, 595.0,                           &
           597.5, 600.0, 602.5, 605.0, 607.5, 610.0,                           &
           612.5, 615.0, 617.5, 620.0, 622.5, 625.0,                           &
           627.5, 630.0, 632.5, 635.0, 637.5, 640.0,                           &
           642.5, 645.0, 647.5, 650.0, 652.5, 655.0,                           &
           657.5, 660.0, 662.5, 665.0, 667.5, 670.0,                           &
           672.5, 675.0, 677.5, 680.0, 682.5, 685.0,                           &
           687.5, 690.0, 692.5, 695.0, 697.5, 700.0,                           &
           702.5, 705.0, 707.5, 710.0, 712.5, 715.0,                           &
           717.5, 720.0, 722.5, 725.0, 727.5, 730.0,                           &
           740.0, 750.0, 760.0, 770.0, 780.0, 790.0,                           &
           800.0, 5000.0 ]
CALL Jin11_lin_interp_real(n_aw_lu, 1, wl_aw, aw_lu, wl_nm, aw)

bw_lu = [ 0.1510, 0.1190, 0.0995, 0.0820, 0.0685, 0.0575, 0.0485, 0.0415,      &
           0.0353, 0.0305, 0.0262, 0.0229, 0.0200, 0.0175, 0.0153, 0.0134,     &
           0.0120, 0.0106, 0.0094, 0.0084, 0.0076, 0.0068, 0.0061, 0.0055,     &
           0.0049, 0.0045, 0.0041, 0.0037, 0.0034, 0.0031, 0.0029, 0.0026,     &
           0.0024, 0.0022, 0.0021, 0.0019, 0.0018, 0.0017, 0.0016, 0.0015,     &
           0.0014, 0.0013, 0.0012, 0.0011, 0.0010, 0.0010, 0.0008, 0.0008,     &
           0.0007, 0.0007, 0.0007, 0.0007, 0.0006, 0.0006, 0.0006, 0.0005,     &
           0.0005, 0.0005, 0.0004, 0.0004, 0.0004, 0.0004 ]
wl_bw = [ 200.0, 210.0, 220.0, 230.0, 240.0, 250.0, 260.0, 270.0,              &
           280.0, 290.0, 300.0, 310.0, 320.0, 330.0, 340.0, 350.0,             &
           360.0, 370.0, 380.0, 390.0, 400.0, 410.0, 420.0, 430.0,             &
           440.0, 450.0, 460.0, 470.0, 480.0, 490.0, 500.0, 510.0,             &
           520.0, 530.0, 540.0, 550.0, 560.0, 570.0, 580.0, 590.0,             &
           600.0, 610.0, 620.0, 630.0, 640.0, 650.0, 660.0, 670.0,             &
           680.0, 690.0, 700.0, 710.0, 720.0, 730.0, 740.0, 750.0,             &
           760.0, 770.0, 780.0, 790.0, 00.0, 5000.0 ]
CALL Jin11_lin_interp_real(n_bw_lu, 1, wl_bw, bw_lu, wl_nm, bw)

! now calculate r0 from the this:
ylmd = EXP( 0.014 * (440.0 - wl_nm(1) ) )
DO ipt = 1, nd_points
  ap(ipt) = (0.06 * chlabs(1) * (chloro_mgm3(ipt)**0.65)) +                    &
              0.2 * (aw440+0.06 * (chloro_mgm3(ipt)**0.65)) * ylmd

  bp550(ipt) =  0.416 * chloro_mgm3(ipt)**0.766
END DO

! code for Eq.13-14 from paper at
! https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1029/2000JC000319
DO ipt = 1, nd_points
  IF (chloro_mgm3(ipt) > 630.9) THEN
    errcode = 8
    ERRMSG  = 'High chlorophyll concentration: function bbp is NEGATIVE.'
    CALL ereport(ModuleName//':'//RoutineName,errcode,ERRMSG)
  END IF
END DO

DO ipt = 1, nd_points
  IF (chloro_mgm3(ipt) > 2.0) THEN
    chl_v(ipt) = 0.0
    bbp(ipt) = (0.002+0.01 * (0.5-0.25 * LOG10(chloro_mgm3(ipt)))) * bp550(ipt)
  ELSE IF (chloro_mgm3(ipt) > 0.02) THEN
    chl_v(ipt) = 0.5 * ( LOG10(chloro_mgm3(ipt)) - 0.3 )
    bbp(ipt) = (0.002+0.01 * (0.5-0.25 * LOG10(chloro_mgm3(ipt))) *            &
               (wl_nm(1) / 550.0)**chl_v(ipt) ) * bp550(ipt)
  ELSE
    chl_v(ipt) = -1.0
    bbp(ipt) = 0.01125 * (550.0 / wl_nm(1)) * bp550(ipt)
  END IF
END DO

! Morel-Gentili(1991), Eq (12), doi:10.1364/AO.30.004427
DO ipt = 1, nd_points
  hb(ipt) = 0.5 * bw(1) / (0.5 * bw(1) + bbp(ipt))
END DO

! and Morel-Gentili(1991) again for f
IF (nd_cosz == 1) THEN
  DO ipt = 1, nd_points
    f(ipt) = 0.6279 - 0.2227 * hb(ipt) - 0.0513 * hb(ipt)**2 +                 &
             (-0.3119+0.2465 * hb(ipt)) * cosz(1)
  END DO
ELSE
  DO ipt = 1, nd_points
    f(ipt) = 0.6279 - 0.2227 * hb(ipt) - 0.0513 * hb(ipt)**2 +                 &
             (-0.3119+0.2465 * hb(ipt)) * cosz(ipt)
  END DO
END IF

! r0, rradiance reflectance, just below the surface: eq 8 of Jin 2011:
DO ipt = 1, nd_points
  r0(ipt) = f(ipt) * (0.5 * bw(1) + bbp(ipt)) / (aw(1) + ap(ipt))
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE Jin11_get_r0
!---------------------------------------------------------------------
END MODULE Jin11_osa_mod
