#if !defined(RECON)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Purpose:
! Calculates the anthropogenic contribution to surface heat fluxes for
! urban tiles by linear interpolation of monthly values. The value is
! then passed in anthrop_heat(n), that has a value of 0.0 except when
! n=6 (urban) and l_anthrop_heat=.true., and added to the surface
! heat fluxes in sf_expl and sf_impl2.

! A second option for anthropogenic heat flux applies Flanner (2009)
! which includes seasonal and diurnal cycles described by latitude- and
! longitude-dependent functions.

! Original code from Martin Best and Peter Clark (December 2005).
! Updated for UM7.1 by Jorge Bornemann (May 2008)
! Updated with diurnal seasonal variation option by Katty Huang (Sep 2025)

MODULE gen_anthrop_heat_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='GEN_ANTHROP_HEAT_MOD'
CONTAINS
SUBROUTINE generate_anthropogenic_heat(curr_day_number, curr_time,             &
              land_pts, frac, surft_pts, surft_index, land_index,              &
              !New arguments replacing USE statements
              !urban_param (IN)
              wrr_gb,                                                          &
              latitude, longitude,                                             &
              !Fluxes (IN OUT)
              anthrop_heat_surft)

!Use in scalar variables
USE jules_surface_types_mod, ONLY: urban, ntype, urban_canyon,                 &
                                    urban_roof

USE ancil_info, ONLY: nsurft

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE jules_urban_mod, ONLY: anthrop_heat_scale

USE urban_param_mod, ONLY: urban_month, fl_b1, fl_b2,                          &
                           fl_sigma, fl_mu, fl_a1, fl_f, fl_alpha, fl_eps,     &
                           fl_norm, fl_latlim, fl_n, fl_off

USE switches, ONLY: l_360

USE jules_surface_mod, ONLY: l_urban2t, l_anthrop_heat_use_wrr,                &
                             anthrop_heat_option, anthrop_heat_mean, dukes,    &
                             flanner

USE conversions_mod, ONLY: rsec_per_day, pi

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! IN time information for current timestep
INTEGER, INTENT(IN) ::                                                         &
  curr_day_number,                                                             &
  curr_time                   ! time of day in seconds

! IN Number of tiles
INTEGER, INTENT(IN) ::                                                         &
   land_pts,                                                                   &
              ! No.of land points being processed, can be 0.
   land_index(land_pts)      ! IN LAND_INDEX(I)=J => the Jth
                             !    point in ROW_LENGTH,ROWS is the
                             !    land point.

REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
   frac(land_pts,ntype)            ! IN Fractions of surface types.

! OUT

!New arguments replacing USE statements
!urban_param (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: wrr_gb(land_pts)

REAL(KIND=real_jlslsm), INTENT(IN) :: latitude(t_i_length,t_j_length),         &
                                      longitude(t_i_length,t_j_length)

!Fluxes (IN OUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: anthrop_heat_surft(land_pts,nsurft)

!Ancil_info
INTEGER, INTENT(IN) ::                                                         &
   surft_index(land_pts,ntype),                                                &
                              ! Index of tile points  : Only used for
   surft_pts(ntype)           ! Number of tile points : urban_canyon

REAL(KIND=real_jlslsm) ::                                                      &
   furb,                                                                       &
               ! Total urban fraction
   anthrop_heat_urban(land_pts),                                               &
               ! Anthropogenic heat on the total urbanised surface
   mm, dpm

INTEGER :: im,im1,n,k,l,i,j,offset,days_in_year,local_day_number

REAL(KIND=real_jlslsm) :: nm, e1, e2, h, a2, tod, lon, diurnal_weight, annual_weight

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='GENERATE_ANTHROPOGENIC_HEAT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Make sure that we have an urban tile
IF ( urban > 0 .OR. urban_canyon > 0 ) THEN

  ! Ignore leap years for now
  days_in_year = 365
  IF (l_360) days_in_year = 360

  IF (anthrop_heat_option == dukes) THEN

    ! Make sure we have the correct average days per month depending on
    ! whether we are running with a 360 day year
    dpm = REAL(days_in_year) / 12.0

    mm = curr_day_number / dpm - 0.5

    im  = INT(mm)
    mm  = mm - im
    im  = im + 1
    im1 = im + 1

    IF (im == 0) THEN
      im  = 12
    ELSE IF (im1 == 13) THEN
      im1 = 1
    END IF

    anthrop_heat_urban(:) = mm * urban_month(im1) + (1.0 - mm) * urban_month(im)

  ELSE IF (anthrop_heat_option == flanner) THEN

      ! This option applies the diurnal and seasonal cycle algorithms from
      ! Flanner 2009 Geophysical Research Letters 36: L02801.
      ! Currently there is a very crude time of day adjustment as function of
      ! longitude. This should be replaced with a time zone offset lookup table.
      ! There has also been an errata issued by Mark Flanner posted at
      ! http://aoss-research.engin.umich.edu/faculty/flanner/publications.php
      ! and downloaded by Mark McCarthy September 2011
      ! The URL has since been updated to https://flanner.engin.umich.edu/publications
      ! Errata: Eqs. A4 and A5 should include brackets to read: 0.5 {erf[...]+1.0},
      !         and the second line of Eq. A7 should be: A2(Theta) = 0

    DO l = 1, land_pts

      ! Diurnal cycle as function of time of day and longitude.

      j = (land_index(l) - 1) / t_i_length + 1
      i = land_index(l) - (j-1) * t_i_length

      ! Calculate simple timezone offset.
      lon = longitude(i,j)
      IF (lon > 180.0) lon = lon - 360.0
      offset = NINT(lon/15.0)

      ! Calculate time of day as a fraction including timezone offset.
      tod = (REAL(curr_time) / rsec_per_day) + (REAL(offset) / 24.0)

      ! If leap year, repeat day 365
      IF (curr_day_number == 366) THEN
        local_day_number = 365
      ELSE
        local_day_number = curr_day_number
      END IF

      ! When local date differs from UTC date
      IF (tod >= 1.0) THEN
        tod = tod - 1.0
        local_day_number = local_day_number + 1
      ELSE IF (tod < 0.0) THEN
        tod = tod + 1.0
        local_day_number = local_day_number - 1
      END IF

      ! Diurnal weighting is a function of normal distribution
      nm = (1.0 / (fl_sigma * SQRT(2.0 * pi))) * EXP(-1.0 * ((tod - fl_mu)**2) / (2.0 * (fl_sigma)**2))

      ! Harmonic function
      h = fl_a1 * COS(2.0 * pi * fl_f * tod)

      ! Error functions e1 and e2
      e1 = 0.5 * (ERF(fl_alpha * (tod - fl_mu + fl_eps) / fl_sigma) + 1.0)
      e2 = 0.5 * (ERF(-1.0 * fl_alpha * (tod - fl_mu - fl_eps) / fl_sigma) + 1.0)

      ! Diurnal weight calculation
      diurnal_weight = ((nm * e1 * fl_b1 + h * e1 * e2 + fl_b2) / fl_norm)


      ! Annual cycle as function of latitude.

      IF (latitude(i,j) > fl_latlim) THEN
        a2 = 1.0 - EXP(-1.0 * (latitude(i,j) - fl_latlim) / fl_n)
      ELSE IF (latitude(i,j) < -1.0*fl_latlim) THEN
        a2 = -1.0 * (1.0 - EXP((latitude(i,j) + fl_latlim) / fl_n))
      ELSE
        a2 = 0.0
      END IF

      annual_weight = 1.0 + a2 * SIN(2.0 * pi * (REAL(local_day_number)/REAL(days_in_year) + fl_off))

      anthrop_heat_urban(l) =  diurnal_weight * annual_weight * anthrop_heat_mean
    END DO

  END IF


    ! For the two-tile urban schemes, distribute the anthropogenic heat between
    ! the canyon and roof tiles depending on anthrop_heat_scale otherwise copy it
    ! to the urban tile.
  IF ( l_urban2t ) THEN
    ! anthrop_heat_surft(urban_roof) is a fraction (anthrop_heat_scale)
    ! of anthrop_heat_surft(urban_canyon). The total from the urban tile is
    ! conserved.
    IF ( l_anthrop_heat_use_wrr ) THEN
      ! W/R supplied so use it (bit comparability plus less calculations)
      DO k = 1,surft_pts(urban_canyon)
        l = surft_index(k,urban_canyon)
        anthrop_heat_surft(l,urban_canyon) = anthrop_heat_urban(l) /           &
           ( anthrop_heat_scale * ( 1.0 - wrr_gb(l) ) + wrr_gb (l) )
        anthrop_heat_surft(l,urban_roof) =                                     &
           anthrop_heat_scale * anthrop_heat_surft(l,urban_canyon)
      END DO
    ELSE
      DO k = 1,surft_pts(urban_canyon)
        l = surft_index(k,urban_canyon)
        furb = frac(l,urban_canyon) + frac(l,urban_roof)
        anthrop_heat_surft(l,urban_canyon) = anthrop_heat_urban(l) * furb /    &
           ( frac(l,urban_canyon) + anthrop_heat_scale * frac(l,urban_roof) )
        anthrop_heat_surft(l,urban_roof) =                                     &
           anthrop_heat_scale * anthrop_heat_surft(l,urban_canyon)
      END DO
    END IF
  ELSE IF ( urban > 0 ) THEN
    anthrop_heat_surft(:,urban) = anthrop_heat_urban(:)
  END IF
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE generate_anthropogenic_heat
END MODULE gen_anthrop_heat_mod
#endif
