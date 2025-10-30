! *****************************COPYRIGHT**************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! ****************************COPYRIGHT***************************************
! Optimum crops (non-rice) and plant date for each irrigated grid box.

MODULE crop_date_mod

IMPLICIT NONE

PRIVATE  ! Private scope by default.
PUBLIC calc_crop_date

CONTAINS

!#############################################################################

SUBROUTINE calc_crop_date(land_index, land_pts, row_length, rows,              &
                          nsurft, frac,                                        &
                          sw_surft, tstar_surft, lw_down, tl_1,                &
                          con_rain, ls_rain, con_snow, ls_snow,                &
                          prec_1_day_av_gb, prec_1_day_av_use_gb,              &
                          rn_1_day_av_gb, rn_1_day_av_use_gb,                  &
                          tl_1_day_av_gb, tl_1_day_av_use_gb,                  &
                          icntmax_gb, plant_n)

! Description:
!   Calculates average temperature, precipitation and net radiation
!     over NYAV years to determine best planting date for
!     non-rice crops for use in irrigation code
!
! Method:
!
! Notes hadrd 2011-07-15:
!   This subroutine was originally included within control.f90
!     but is now put in a separate file
!   Alternatively, it may be included in a separate irrigation subroutine?
! Notes hadrd 2011-09-30:
!   Variables that need to be saved between timesteps are now in
!   module irrcrop_ctl
!     and allocated at the start (and de-allocated at the end)
!     when <irr_crop> is set to 1 in the run control file
!
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in VEGETATION
! Code originally developed by Nic Gedney
!
! Code Description:
!   Language: Fortran 90.
!   This code is partially modified to JULES coding standards v1.
!   Rutger Dankers, July 2011
!
!-----------------------------------------------------------------------------

USE conversions_mod, ONLY:                                                     &
  ! imported scalar parameters
  isec_per_day

USE csigma, ONLY:                                                              &
  ! imported scalar parameters
  sbcon ! Stefan-Boltzmann constant (W/m**2/K**4).

USE crop_vars_mod, ONLY:                                                       &
  ! imported scalars
  iyear_old, nday_crop, nyav, ndpy,                                            &
  startyr, startmon, startday, starttime

USE jules_surface_types_mod, ONLY:                                             &
  ! imported scalars
  ntype

USE time_info_mod, ONLY: l_360, l_leap, current_model_time

USE datetime_utils_mod, ONLY: day_of_year

USE timestep_mod, ONLY: timestep

!-----------------------------------------------------------------------------

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(IN) :
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_pts,row_length,rows,nsurft

!-----------------------------------------------------------------------------
! Array arguments with intent(IN) :
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) :: land_index(land_pts)
REAL(KIND=real_jlslsm), INTENT(IN) :: frac(land_pts,ntype)
                                                ! Fractions of surface types

REAL(KIND=real_jlslsm), INTENT(IN) :: sw_surft(land_pts,nsurft)
                                                ! Surface net SW radiation on
                                                ! land tiles (W/m2).

REAL(KIND=real_jlslsm), INTENT(IN) :: tstar_surft(land_pts,nsurft)
                                                 ! Surface tile temperatures
                                                 ! (K).

REAL(KIND=real_jlslsm), INTENT(IN) :: lw_down(row_length,rows)
                                                ! Surface downward LW
                                                ! radiation (W/m2).
REAL(KIND=real_jlslsm), INTENT(IN) :: tl_1(row_length,rows)
                                                ! Liquid/frozen water
                                                ! temperature for lowest
                                                ! atmospheric layer (K).
REAL(KIND=real_jlslsm), INTENT(IN) :: con_rain(row_length,rows)
                                                ! Convective rain (kg/m2/s).
REAL(KIND=real_jlslsm), INTENT(IN) :: ls_rain(row_length,rows)
                                                ! Large-scale rain (kg/m2/s).
REAL(KIND=real_jlslsm), INTENT(IN) :: con_snow(row_length,rows)
                                                ! Convective snow (kg/m2/s).
REAL(KIND=real_jlslsm), INTENT(IN) :: ls_snow(row_length,rows)
                                                ! Large-scale snow (kg/m2/s).

!-----------------------------------------------------------------------------
! Array arguments with intent(IN OUT) :
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: prec_1_day_av_gb(land_pts)
    ! Average precipitation rate for the current day (kg m-2 s-1).
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  prec_1_day_av_use_gb(land_pts,ndpy,nyav)
    ! Daily average precipitation rate (kg m-2 s-1).
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  rn_1_day_av_use_gb(land_pts,ndpy,nyav)
    ! Daily average net radiation (W m-2).
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: rn_1_day_av_gb(land_pts)
    ! Average net radiation for the current day (W m-2).
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: tl_1_day_av_gb(land_pts)
    ! Average air temperature for the current day (K).
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  tl_1_day_av_use_gb(land_pts,ndpy,nyav)
    ! Daily average air temperature (K).

!-----------------------------------------------------------------------------
! Array arguments with intent(OUT) :
!-----------------------------------------------------------------------------
INTEGER, INTENT(OUT) :: icntmax_gb(land_pts)
    ! Counter for start date for non-rice crops.
INTEGER, INTENT(OUT) :: plant_n(land_pts)
    ! Best planting date for non-rice crops.

!-----------------------------------------------------------------------------
! LOCAL scalar variables :
!-----------------------------------------------------------------------------
INTEGER :: i,j,l,n                  ! Loop counters
INTEGER :: tspd                     ! timesteps per day

INTEGER :: iyear
INTEGER :: day_of_yr                ! current day of year (== IJULIAN)
INTEGER :: secs_in_year             ! Number of seconds in the year.

INTEGER :: year, month, day, time_of_day
LOGICAL :: datetime_ne

!-----------------------------------------------------------------------------
! LOCAL array variables :
!-----------------------------------------------------------------------------
INTEGER :: icount(ndpy,nyav)
REAL(KIND=real_jlslsm) :: rn(land_pts)
                                    ! Net downward radiation (W m-2).

!-----------------------------------------------------------------------------
! End of header
!-----------------------------------------------------------------------------

CALL current_model_time(year, month, day, time_of_day)

tspd         = isec_per_day / timestep ! #timesteps per day
day_of_yr    = day_of_year(year, month, day, l_360, l_leap)
secs_in_year = ndpy * isec_per_day

!-----------------------------------------------------------------------------
! Calculate grid box mean net radiation for use in crop irrigation:
!-----------------------------------------------------------------------------

rn(:) = 0.0 ! first set RN to zero
icount(:,:) = 0

DO n = 1,nsurft ! loop over tiles
  DO l = 1,land_pts
    rn(l) = rn(l) + frac(l,n) * ( sw_surft(l,n) & ! net SW per tile
            - sbcon * tstar_surft(l,n)**4.0 )     ! upward LW radiation
                                                  ! per tile
  END DO
END DO

! Add gridbox-average downward LW radiation
DO l = 1,land_pts
  j = (land_index(l) - 1) / row_length + 1
  i =  land_index(l) - (j-1) * row_length
  rn(l) = rn(l) + lw_down(i,j)
END DO

!-----------------------------------------------------------------------------
! Calculate averages and reset values at end of every cycle:
!-----------------------------------------------------------------------------
! nyav = nr of years averaged for crop plant estimates
! iyear varies between 1, 2, and 3 if nyav = 3
iyear = MOD ( ABS(year - startyr), nyav)

IF ( iyear == 0 ) iyear = nyav
IF ( day_of_yr <= ndpy ) THEN
  icount(day_of_yr,iyear) = icount(day_of_yr,iyear) + 1

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length

    ! Calculate average temperature, precip and net radiaton over NYAV years.
    ! First calculate average over 1 day.
    ! hadrd - daily averages are reset after 1 day and therefore do not need
    !         the dimensions (points, ijulian, iyear)
    tl_1_day_av_gb(l)   = tl_1_day_av_gb(l) + tl_1(i,j) / REAL(tspd)
    prec_1_day_av_gb(l) = prec_1_day_av_gb(l) +                                &
                          (con_rain(i,j) + con_snow(i,j)                       &
                          + ls_rain(i,j) + ls_snow(i,j))                       &
                          / REAL(tspd)
    rn_1_day_av_gb(l)   = rn_1_day_av_gb(l) + rn(l) / REAL(tspd)

  END DO

  IF ( icount(day_of_yr,iyear) == tspd) THEN ! after 1 day

    ! Save averages for cropping irrigation code:
    DO l = 1,land_pts
      tl_1_day_av_use_gb(l,day_of_yr,iyear)   = tl_1_day_av_gb(l)
      prec_1_day_av_use_gb(l,day_of_yr,iyear) = prec_1_day_av_gb(l)
      rn_1_day_av_use_gb(l,day_of_yr,iyear)   = rn_1_day_av_gb(l)
    END DO

    ! Reset to zero:
    icount(day_of_yr,iyear) = 0
    DO l = 1,land_pts
      tl_1_day_av_gb(l)    = 0.0
      prec_1_day_av_gb(l)  = 0.0
      rn_1_day_av_gb(l)    = 0.0
    END DO

  END IF

  !---------------------------------------------------------------------------
  ! Call cropping irrigation code at end of every YEAR of data stored:
  !---------------------------------------------------------------------------
  datetime_ne = ( year /= startyr )   .OR.                                     &
                ( month /= startmon ) .OR.                                     &
                ( day  /= startday )  .OR.                                     &
                ( time_of_day /= starttime )

  IF ( (iyear /= iyear_old) .AND. datetime_ne ) THEN
    ! When year has changed i.e. first day of new year

    CALL opt_crop_date(land_pts, ndpy, nyav, nday_crop,                        &
                       plant_n, icntmax_gb,                                    &
                       tl_1_day_av_use_gb,                                     &
                       prec_1_day_av_use_gb,                                   &
                       rn_1_day_av_use_gb)

  END IF

END IF
iyear_old = iyear

RETURN

END SUBROUTINE calc_crop_date

!#############################################################################

SUBROUTINE opt_crop_date(land_pts, ndpy, nyav, nday_crop,                      &
                         plant_n, icntmax,                                     &
                         tl_1_day_av_use,                                      &
                         prec_1_day_av_use,                                    &
                         epot_1_day_av_use)


! Description:
!   Calculates best planting date for non-rice
!   as a function of temperature, precipitation and
!     potential evaporation criteria over NYAV year
!   Assumes potential evap = RN
!   Note routine is only called after year has completed
!
! Method:
!
! Notes RD 2011-07-12:
!   This subroutine was originally included at the end of control.f90
!     but is now put in a separate file
!   Alternatively, it may be included in a separate irrigation subroutine?
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in VEGETATION
! Code originally developed by Nic Gedney
!
! Code Description:
!   Language: Fortran 90.
!   This code is partially modified to JULES coding standards v1.
!   Rutger Dankers, July 2011
!
!-----------------------------------------------------------------------------

USE conversions_mod, ONLY:                                                     &
  ! imported scalar parameters
  zerodegc  ! ZeroDegC (K)

USE water_constants_mod, ONLY:                                                 &
  ! imported scalar parameters
  lc   ! latent heat of condensation of water at 0degc (J kg-1).

USE conversions_mod, ONLY:                                                     &
  ! imported scalar parameters
  rsec_per_day

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts
INTEGER, INTENT(IN) :: ndpy           ! No. of days per year
INTEGER, INTENT(IN) :: nyav           ! No. of years averaged for crop plant
                                      ! estimates
INTEGER, INTENT(IN) :: nday_crop      ! Cropping date

REAL(KIND=real_jlslsm), INTENT(IN) :: tl_1_day_av_use(land_pts,ndpy,nyav)
  ! Daily mean air temperature (K).
REAL(KIND=real_jlslsm), INTENT(IN) :: prec_1_day_av_use(land_pts,ndpy,nyav)
  ! Daily mean precipitation rate (kg m-2 s-1).
REAL(KIND=real_jlslsm), INTENT(IN) :: epot_1_day_av_use(land_pts,ndpy,nyav)
  ! Daily mean net radiation (W m-2).

INTEGER, INTENT(OUT) :: plant_n(land_pts)
  ! Best planting date for non-rice crops.
INTEGER, INTENT(OUT) :: icntmax(land_pts)  ! general T criteria for non-rice

! Time periods for non-rice criteria:
INTEGER, PARAMETER :: bday_n1 = 1, bday_n2 = 21, bday_n3 = 51, eday_n3 = 110

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  tmin_n   = zerodegc +  5.0,                                                  &
  tmin_n23 = zerodegc + 15.0,                                                  &
  tmax_n23 = zerodegc + 30.0

REAL(KIND=real_jlslsm), PARAMETER :: prec_thr = 0.5

INTEGER :: iend,iend1,iend2,ibeg
INTEGER :: i,l,ii,iy,j,icc
INTEGER :: max_day  ! Max no. of days which reach general crop criteria
INTEGER :: eday_n1, eday_n2

INTEGER ::                                                                     &
  ic(land_pts,ndpy),                                                           &
    ! Counter.
  icnt(land_pts,ndpy),                                                         &
    ! General T criteria for non-rice.
  icn(land_pts,ndpy),                                                          &
    ! Criteria for non-rice WHOLE growing period.
  icn1(land_pts,ndpy),                                                         &
    ! Criteria for non-rice period 1.
  icn2(land_pts,ndpy),                                                         &
    ! Criteria for non-rice period 2.
  icn3(land_pts,ndpy) ,                                                        &
    ! Criteria for non-rice period 3.
  plant_n_tmp(land_pts),                                                       &
    ! Best plant date for non-rice.
  iicn(land_pts)
    ! Counter for start date for non-rice.

REAL(KIND=real_jlslsm) ::                                                      &
  tl_av(land_pts,ndpy),                                                        &
    ! Multi-year average daily temperature (K).
  prec_av(land_pts,ndpy),                                                      &
    ! Multi-year average daily precipitation (mm).
  prec_tmp(land_pts,ndpy),                                                     &
    ! Precipitation (mm).
  epot_av(land_pts,ndpy),                                                      &
    ! Multi-year average daily potential evaporation (mm).
  epot_tmp(land_pts,ndpy)
    ! Potential evaporation (mm).

!-----------------------------------------------------------------------------
! End of header
!-----------------------------------------------------------------------------

! First set to zero
DO l = 1,land_pts
  DO i = 1,ndpy
    tl_av(l,i)   = 0.0
    prec_av(l,i) = 0.0
    epot_av(l,i) = 0.0
    icnt(l,i)    = 0
    icn1(l,i)    = 0
    icn2(l,i)    = 0
    icn3(l,i)    = 0
    ic(l,i)      = 0
  END DO
END DO

DO l = 1,land_pts    ! loop over land points
  DO iy = 1,nyav     ! loop over 3 years
    DO i = 1,ndpy    ! loop over each day in year

      IF ( tl_1_day_av_use(l,i,iy) > 0.0) THEN  ! If data

        ic(l,i) = ic(l,i) + 1
        ! Calculate sum of daily temp, precip and net rad for this day over
        ! NYAV years.
        tl_av(l,i)   = tl_av(l,i) + tl_1_day_av_use(l,i,iy)
        prec_av(l,i) = prec_av(l,i) + prec_1_day_av_use(l,i,iy)
        epot_av(l,i) = epot_av(l,i) + epot_1_day_av_use(l,i,iy)

        ! Dont allow negative potential evaporation:
        IF ( epot_av(l,i) < 0.0 ) epot_av(l,i) = 0.0

        prec_tmp(l,i) = 0.0
        epot_tmp(l,i) = 0.0

      END IF ! TL_1_DAY_AV_USE GT 0
    END DO ! NDPY
  END DO ! NYAV
END DO ! LAND_PTS

!-----------------------------------------------------------------------------
! Calculate 10 day avg for EPOT and PREC
!-----------------------------------------------------------------------------
DO l = 1,land_pts
  DO i = 1,ndpy
    icc = 0 ! counter for calculating averages

    DO j = -4,5 ! loop from -4 to +5 days

      ii = i + j ! new index for day number
      IF ( ii < 1 )    ii = ii + ndpy
      IF ( ii > ndpy ) ii = ii - ndpy

      IF ( tl_av(l,ii) > 0.0 ) THEN ! If data
        icc = icc + 1
        prec_tmp(l,i) = prec_tmp(l,i) + prec_av(l,ii)
        epot_tmp(l,i) = epot_tmp(l,i) + epot_av(l,ii)
      END IF

    END DO ! loop from -4 to +5 days

    IF ( icc > 0 ) THEN
      ! Calculate average over ICC days
      prec_tmp(l,i) = prec_tmp(l,i) / REAL(icc)
      epot_tmp(l,i) = epot_tmp(l,i) / REAL(icc)
    END IF

  END DO ! NDPY
END DO ! LAND_PTS

! Copy average to PREC_AV and EPOT_AV
DO l = 1,land_pts
  DO i = 1,ndpy
    prec_av(l,i) = prec_tmp(l,i)
    epot_av(l,i) = epot_tmp(l,i)
  END DO
END DO

!-----------------------------------------------------------------------------
! Calculate multi-year avg for temperature, prec and epot
!-----------------------------------------------------------------------------
DO l = 1,land_pts
  DO i = 1,ndpy

    IF ( ic(l,i) > 0 ) THEN
      tl_av(l,i)   = tl_av(l,i) / REAL(ic(l,i))
      prec_av(l,i) = prec_av(l,i) / REAL(ic(l,i)) * rsec_per_day
        ! convert to mm/day
      epot_av(l,i) = epot_av(l,i) / REAL(ic(l,i)) * rsec_per_day / lc
        ! convert from W/m2 to mm/day
    END IF

  END DO
END DO

!-----------------------------------------------------------------------------
! Non-Rice
! khalla - similar code for rice exists in r1951_irrig_crop
!-----------------------------------------------------------------------------

! Temperature threshold for whole of the growing season:
DO l = 1,land_pts
  DO i = 1,ndpy
    iend  = i + nday_crop - 1
    iend1 = iend
    iend2 = 0

    ! At end of year, calculation should continue with data from start of year
    IF ( iend > ndpy ) THEN
      iend1 = ndpy
      iend2 = nday_crop - 1 + i - iend1
    END IF

    ! Counter for when general T criteria for non-rice are met
    DO ii = i,iend1
      IF ( tl_av(l,ii) > tmin_n ) icnt(l,i) = icnt(l,i) + 1
    END DO
    DO ii = 1,iend2
      IF ( tl_av(l,ii) > tmin_n ) icnt(l,i) = icnt(l,i) + 1
    END DO

    icntmax(l) = MAXVAL( icnt(l,1:ndpy) )

  END DO ! NDPY

  ! Now consider components of the growing season:
  !------------------------------------------------
  ! Period 1: Days 1-20 avg(Prec)>0.5 avg(Epot)
  eday_n1 = bday_n2 - 1 ! hadrd - BDAY_N2 set to 21 in header

  DO i = 1,ndpy
    ibeg = i + bday_n1 - 1 ! hadrd - BDAY_N1 set to 1 in header
    iend = i + eday_n1 - 1

    ! At end of year, calculation should continue with data from start of year
    IF ( ibeg > ndpy ) THEN
      ibeg = ibeg - ndpy
      iend = iend - ndpy
    END IF

    iend1 = iend
    iend2 = 0
    IF ( (iend > ndpy) .AND. (ibeg <= ndpy) ) THEN
      ! Calculation is split between remainder of year + start of year
      iend1 = ndpy
      iend2 = iend - iend1
    END IF

    DO ii = ibeg,iend1
      IF ( prec_av(l,ii) > prec_thr * epot_av(l,ii) ) THEN
        icn1(l,i) = icn1(l,i) + 1
      END IF
    END DO

    DO ii = 1,iend2
      IF ( prec_av(l,ii) > prec_thr * epot_av(l,ii) ) THEN
        icn1(l,i) = icn1(l,i) + 1
      END IF
    END DO
  END DO

  !------------------------------------------------
  ! Period 2: Days 21-50 T 18-30, avg(Prec)>0.5 avg(Epot)
  eday_n2 = bday_n3 - 1 ! hadrd - BDAY_N3 set to 51 in header

  DO i = 1,ndpy
    ibeg = i + bday_n2 - 1
    iend = i + eday_n2 - 1

    ! At end of year, calculation should continue with data from start of year
    IF ( ibeg > ndpy ) THEN
      ibeg = ibeg - ndpy
      iend = iend - ndpy
    END IF

    iend1 = iend
    iend2 = 0

    IF ( (iend > ndpy) .AND. (ibeg <= ndpy) ) THEN
      ! Calculation is split between remainder of year + start of year
      iend1 = ndpy
      iend2 = iend - iend1
    END IF

    DO ii = ibeg,iend1
      IF ( (tl_av(l,ii) >= tmin_n23) .AND. (tl_av(l,ii) <= tmax_n23) ) THEN
        icn2(l,i) = icn2(l,i) + 1
      END IF
      IF ( prec_av(l,ii) > prec_thr * epot_av(l,ii) ) THEN
        icn2(l,i) = icn2(l,i) + 1
      END IF
    END DO

    DO ii = 1,iend2
      IF ( (tl_av(l,ii) >= tmin_n23) .AND. (tl_av(l,ii) <= tmax_n23) ) THEN
        icn2(l,i) = icn2(l,i) + 1
      END IF
      IF ( prec_av(l,ii) > prec_thr * epot_av(l,ii) ) THEN
        icn2(l,i) = icn2(l,i) + 1
      END IF
    END DO
  END DO

  !------------------------------------------------
  ! Period 3: Days 51-110 T 15-30
  DO i = 1,ndpy
    ibeg = i + bday_n3 - 1
    iend = i + eday_n3 - 1

    ! At end of year, calculation should continue with data from start of year
    IF ( ibeg > ndpy ) THEN
      ibeg = ibeg - ndpy
      iend = iend - ndpy
    END IF

    iend1 = iend
    iend2 = 0

    IF ( iend > ndpy .AND. ibeg <= ndpy ) THEN
      ! Calculation is split between remainder of year + start of year
      iend1 = ndpy
      iend2 = iend - iend1
    END IF

    DO ii = ibeg,iend1
      IF ( (tl_av(l,ii) >= tmin_n23) .AND. (tl_av(l,ii) <= tmax_n23) ) THEN
        icn3(l,i) = icn3(l,i) + 1
      END IF
    END DO
    DO ii = 1,iend2
      IF ( (tl_av(l,ii) >= tmin_n23) .AND. (tl_av(l,ii) <= tmax_n23) ) THEN
        icn3(l,i) = icn3(l,i) + 1
      END IF
    END DO
  END DO

  !------------------------------------------------
  ! Combine criteria over WHOLE growing period
  DO i = 1,ndpy
    icn(l,i) = icn1(l,i) + icn2(l,i) + icn3(l,i)
  END DO

  !------------------------------------------------
  ! Now eliminate growing periods when T is not over minimum threshold
  ! (5C) for non-rice for 150 consecutive days:

  max_day        = MAXVAL(icnt(l,:)) ! max number of days that temp
                                     ! criterion is met
  plant_n_tmp(l) = 0
  iicn(l)        = -1000

  DO i = 1,ndpy
    IF ( icnt(l,i) >= max_day ) THEN ! temperature threshold
      IF ( icn(l,i) > iicn(l) ) THEN ! combined criteria
        plant_n_tmp(l) = i
        iicn(l)        = icn(l,i)
      END IF
    END IF
  END DO

  plant_n(l) = plant_n_tmp(l) ! best plant day for non-rice

END DO ! end l land points loop

END SUBROUTINE opt_crop_date

END MODULE crop_date_mod
