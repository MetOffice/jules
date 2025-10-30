! *****************************COPYRIGHT*************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use
! and distribution under the JULES collaboration agreement, subject
! to the terms and conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT*************************************
!
! Description:
! Routines to calculate the demand for and supply of irrigation water.
!-----------------------------------------------------------------------------

MODULE irrigation_mod

!-----------------------------------------------------------------------------
! Description:
!   Irrigation routines.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!
!-----------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport

USE um_types, ONLY: real_jlslsm

#if !defined(UM_JULES)
USE crop_date_mod,    ONLY: calc_crop_date
USE rivers_route_mod, ONLY: adjust_routestore
#endif

IMPLICIT NONE

INTEGER, PARAMETER ::                                                          &
  nlayer_irrig = 2
    ! Number of soil layers to irrigate.

PRIVATE
PUBLIC irrigation_control, irrigation_demand, nlayer_irrig

CONTAINS

!#############################################################################

SUBROUTINE irrigation_control( a_step, land_pts, land_index,                   &
                       con_rain_ij, con_snow_ij, dvi_cpft, frac_irr_soilt,     &
                       frac_surft, ls_rain_ij, ls_snow_ij, lw_down,            &
                       smvccl_soilt, smvcst_soilt, smvcwt_soilt,               &
                       sthf_soilt, sw_surft, tl_1_ij, tstar_surft,             &
                       icntmax_gb, plant_n_gb, irrDaysDiag_gb,                 &
                       prec_1_day_av_gb, prec_1_day_av_use_gb,                 &
                       rn_1_day_av_gb, rn_1_day_av_use_gb,                     &
                       tl_1_day_av_gb, tl_1_day_av_use_gb,                     &
                       smcl_soilt, sthu_irr_soilt, sthu_soilt, sthzw_soilt,    &
                       irrig_water_gb,                                         &
                       !New arguments replacing USE statements
                       !jules_rivers_mod
                       rivers_sto_per_m2_on_landpts, rivers_adj_on_landpts,    &
                       rivers )

! Description: Top-level routine for irrigation.

USE ancil_info, ONLY: nsoilt, nsurft

USE atm_fields_bounds_mod, ONLY: tdims

USE crop_vars_mod, ONLY: ndpy, nyav

USE jules_rivers_mod, ONLY: rivers_type

! Some usage inside ifdefs
USE jules_irrig_mod, ONLY: irr_crop_doell, irr_crop, l_irrig_limit,            &
                           nstep_irrig

USE jules_soil_mod, ONLY: sm_levels

USE jules_surface_types_mod, ONLY: ncpft, ntype

#if !defined(UM_JULES)
USE theta_field_sizes, ONLY: t_i_length, t_j_length
#endif

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

! Scalar arguments with intent(IN) :
INTEGER, INTENT(IN) ::                                                         &
  a_step,                                                                      &
    ! Atmospheric timestep number
  land_pts
    ! Number of land points.

! Array arguments with intent(IN) :
INTEGER, INTENT(IN) ::                                                         &
  land_index(land_pts)
    ! Index of land points.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  con_rain_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),            &
  con_snow_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),            &
  dvi_cpft(land_pts,ncpft),                                                    &
    ! Development index for crop tiles
  frac_irr_soilt(land_pts,nsoilt),                                             &
    ! Irrigation fraction for this year.
  frac_surft(land_pts,ntype),                                                  &
    ! Fractions of surface types.
  ls_rain_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),             &
  ls_snow_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),             &
  lw_down(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                &
  smvccl_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Critical volumetric SMC (cubic m per cubic m of soil).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  smvcwt_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric wilting point (m3/m3 of soil).
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  sw_surft(land_pts,nsurft),                                                   &
       !Surface net SW radiation on land tiles (W/m2)
  tl_1_ij(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),                &
  tstar_surft(land_pts,nsurft)

! Array arguments with intent(IN OUT) :
INTEGER, INTENT(IN OUT) ::                                                     &
  icntmax_gb(land_pts),                                                        &
    ! Counter for start date for non-rice crops.
  plant_n_gb(land_pts)
    ! Best planting date for non-rice crops.

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  irrDaysDiag_gb(land_pts),                                                    &
    ! Number of days on which irrigation is applied.
  prec_1_day_av_gb(land_pts),                                                  &
    ! Average precipitation rate for the current day (kg m-2 s-1).
  prec_1_day_av_use_gb(land_pts,ndpy,nyav),                                    &
    ! Daily average precipitation rate (kg m-2 s-1).
  rn_1_day_av_gb(land_pts),                                                    &
    ! Average net radiation for the current day (W m-2).
  rn_1_day_av_use_gb(land_pts,ndpy,nyav),                                      &
    ! Daily average net radiation (W m-2).
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg/m2).
  sthu_irr_soilt(land_pts,nsoilt,sm_levels),                                   &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation in irrigated fraction
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.
  sthzw_soilt(land_pts,nsoilt),                                                &
    ! Soil moisture fraction in deep layer.
  tl_1_day_av_gb(land_pts),                                                    &
    ! Average air temperature for the current day (K).
  tl_1_day_av_use_gb(land_pts,ndpy,nyav)
    ! Daily average air temperature (K).

! Array arguments with intent(OUT) :
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  irrig_water_gb(land_pts)
    ! Addition of irrigation water to soil (kg/m2/s).

!New arguments replacing USE statements
!jules_rivers_mod
REAL(KIND=real_jlslsm), INTENT(IN) :: rivers_sto_per_m2_on_landpts(land_pts)
REAL(KIND=real_jlslsm), INTENT(OUT) :: rivers_adj_on_landpts(land_pts)

TYPE(rivers_type), INTENT(IN OUT) :: rivers

!-----------------------------------------------------------------------------
! Local array variables
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  irrigwater_levels(land_pts,nsoilt,sm_levels)
    ! Addition of irrigation water to each soil layer (kg/m2)

LOGICAL ::                                                                     &
  l_irrigate_now(land_pts,nsoilt)
    ! TRUE when a tile can be irrigated on this timestep.
    ! FALSE at other times and tiles.

!-----------------------------------------------------------------------------
! End of header
!-----------------------------------------------------------------------------

! Initialise water applied through irrigation as zero:
irrigwater_levels(:,:,:) = 0.0

IF ( l_irrig_limit ) THEN
  ! Set the adjustment to 1.0 (no adjustment).
  rivers_adj_on_landpts(:) = 1.0
END IF

! Update irrigation every nstep_irrig timesteps
IF ( MOD(a_step, nstep_irrig) == 0 .AND. ( a_step > 1 ) ) THEN

  ! Calculate the demand for water for irrigation.
  CALL irrigation_demand( land_pts, sm_levels, plant_n_gb,                     &
            frac_irr_soilt, sthf_soilt, smvccl_soilt, smvcst_soilt, dvi_cpft,  &
            irrigwater_levels, smcl_soilt, sthu_irr_soilt, sthu_soilt,         &
            l_irrigate_now )

  ! Calculate actual irrigation, accounting for water availability.
  CALL irrigation_actual( land_pts, frac_irr_soilt,                            &
            rivers_sto_per_m2_on_landpts,                                      &
            smvccl_soilt, smvcst_soilt, smvcwt_soilt, sthf_soilt,              &
            l_irrigate_now, irrDaysDiag_gb, irrigwater_levels,                 &
            rivers_adj_on_landpts, smcl_soilt, sthu_soilt, sthzw_soilt,        &
            sthu_irr_soilt, irrig_water_gb )
END IF

#if !defined(UM_JULES)
! Update the best planting dates.
! irr_crop == irr_crop_doell is not advisable for UM use. It requires detailed
! information about planting dates to be calculated and there is a large
! amount of technical debt which needs to be sorted before going into the UM
IF ( irr_crop == irr_crop_doell ) THEN
  CALL calc_crop_date(land_index, land_pts,  t_i_length, t_j_length,           &
                      nsurft, frac_surft,                                      &
                      sw_surft, tstar_surft, lw_down, tl_1_ij,                 &
                      con_rain_ij, ls_rain_ij, con_snow_ij, ls_snow_ij,        &
                      prec_1_day_av_gb, prec_1_day_av_use_gb,                  &
                      rn_1_day_av_gb, rn_1_day_av_use_gb,                      &
                      tl_1_day_av_gb, tl_1_day_av_use_gb,                      &
                      icntmax_gb, plant_n_gb)
END IF

! Adjust river store to account for use by irrigation.
! Limitation code requires l_irrig_dmd = TRUE, l_top = TRUE, l_rivers = TRUE
! and i_river_UM = rivers_trip (3) . The technical parts of this code are
! designed only to run with standalone TRIP routing code. UM TRIP/RFM code
! is deprecated.
! Note that we only need to update the store on timesteps when irrigation
! occurs, but that would likely change results (KGO).
IF ( l_irrig_limit ) THEN
  CALL adjust_routestore(rivers_adj_on_landpts,                                &
                         rivers)
END IF
#endif

END SUBROUTINE irrigation_control

!#############################################################################

SUBROUTINE irrigation_demand ( land_pts, sm_levels, plant_n_gb,                &
           frac_irr_soilt, sthf_soilt, smvccl_soilt, smvcst_soilt, dvi_cpft,   &
           irrigwater_levels, smcl_soilt, sthu_irr_soilt, sthu_soilt,          &
           l_irrigate_now )

! Description:
!   Calculates irrigation demand over unfrozen soils as the amount of
!     water needed to alleviate soil water deficit
!
! Method:
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

USE water_constants_mod, ONLY: rho_water

USE ancil_info, ONLY: nsoilt

USE crop_vars_mod, ONLY: nday_crop

USE datetime_utils_mod, ONLY: day_of_year, days_in_year

USE jules_irrig_mod, ONLY: irr_crop, irr_crop_all_year, irr_crop_doell,        &
                           irr_crop_dvimax

USE jules_soil_mod, ONLY: dzsoil

USE jules_surface_types_mod,  ONLY: ncpft

USE time_info_mod, ONLY: l_360, l_leap, current_model_time

!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(IN) :
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts,                                                                    &
    ! Number of land points.
  sm_levels
    ! No. of soil moisture levels.

!-----------------------------------------------------------------------------
! Array arguments with intent(IN) :
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  plant_n_gb(land_pts)
    ! Best planting date for non-rice crops.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_irr_soilt(land_pts,nsoilt),                                             &
    ! Irrigation fraction for this year.
  sthf_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Frozen soil moisture content of each layer as a fraction of saturation.
  smvccl_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Critical volumetric SMC (cubic m per cubic m of soil).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  dvi_cpft(land_pts,ncpft)
    ! Development index for crop tiles

!-----------------------------------------------------------------------------
! Array arguments with intent(IN OUT) :
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  irrigwater_levels(land_pts,nsoilt,sm_levels),                                &
    ! Addition of irrigation water to each soil layer (kg/m2)
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg/m2).
  sthu_irr_soilt(land_pts,nsoilt,sm_levels),                                   &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation in irrigated fraction
  sthu_soilt(land_pts,nsoilt,sm_levels)
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
LOGICAL, INTENT(OUT) ::                                                        &
  l_irrigate_now(land_pts,nsoilt)
    ! TRUE when a tile can be irrigated on this timestep.
    ! FALSE at other times and tiles.

!-----------------------------------------------------------------------------
! Local parameters
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER :: RoutineName = 'IRRIGATION_DEMAND'

!-----------------------------------------------------------------------------
! Local scalar variables:
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  l,n,m,                                                                       &
    ! Loop counters.
  days_in_yr,                                                                  &
    ! Total number of days in current year.
  day_of_yr,                                                                   &
    ! Current day of year.
  day,                                                                         &
    ! Current day.
  month,                                                                       &
    ! Current month.
  year,                                                                        &
    ! Current year.
  errorstatus

REAL(KIND=real_jlslsm) ::                                                      &
  sthu_irr_tmp
    ! Value of sthu_irr_soilt that is input.

!-----------------------------------------------------------------------------
! LOCAL arrays:
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  dvimax_gb(land_pts)
    ! Maximum value of crop development index in each gridbox.

!-----------------------------------------------------------------------------
! Calculate irrigation demand
!-----------------------------------------------------------------------------

! Get information depending on the model to be used.
SELECT CASE ( irr_crop )

CASE ( irr_crop_doell )
  ! Get date and time information.
  CALL current_model_time(year, month, day)
  days_in_yr = days_in_year(year, l_360, l_leap)
  day_of_yr  = day_of_year(year, month, day, l_360, l_leap)

CASE ( irr_crop_dvimax )
  ! dvi_cpft is a prognostic variable calculated only by the standalone
  ! crop model. The crop model is required for this option and is not switched
  ! on in the UM. irr_crop=irr_crop_all_year is the only available option for
  ! UM JULES.
  ! Calculate the maximum dvi per grid cell.
  DO l = 1,land_pts
    dvimax_gb(l) = MAXVAL(dvi_cpft(l,:))
  END DO

END SELECT

!-----------------------------------------------------------------------------
! Calculate irrigation demand on each soil tile.
!-----------------------------------------------------------------------------
! Loop over soil tiles
DO m = 1, nsoilt
  ! Loop over land points
  DO l = 1, land_pts

    !-------------------------------------------------------------------------
    ! Determine if irrigation should be considered at this location.
    ! We can irrigate if frac_irr_soilt>0 and we are in the irrigation season.
    !-------------------------------------------------------------------------
    l_irrigate_now(l,m) = .FALSE.

    IF ( frac_irr_soilt(l,m) > 0.0 ) THEN

      SELECT CASE ( irr_crop )

      CASE ( irr_crop_all_year )
        ! Continuous irrigation
        l_irrigate_now(l,m) = .TRUE.

      CASE ( irr_crop_doell )
        ! Original crop model based on Doell & Siebert, 2002.
        IF ( plant_n_gb(l) <= 0 ) THEN
          ! The crop code has not yet set plant_n_gb. Don't irrigate.
          l_irrigate_now(l,m) = .FALSE.
        ELSE IF ( day_of_yr >= plant_n_gb(l)  .AND.                            &
                  day_of_yr < plant_n_gb(l) + nday_crop ) THEN
          ! Irrigate after planting and before harvesting.
          l_irrigate_now(l,m) = .TRUE.
        ELSE IF ( (plant_n_gb(l) + nday_crop > days_in_yr ) .AND.              &
                  (day_of_yr <= nday_crop - days_in_yr + plant_n_gb(l)) ) THEN
          ! Catch the case when a crop was planted last year but not
          ! harvested in the same year. If we are early in the next year,
          ! the crop is still in the ground. Note that this might be slightly
          ! wrong if this or last year was a leap year because days_in_yr is
          ! calculated for the current year.
          l_irrigate_now(l,m) = .TRUE.

        END IF

      CASE ( irr_crop_dvimax )
        ! use JULES-crop development index
        ! for the time being, the maximum dvi across all crop tiles is used
        ! to trigger irrigation. However, it may be better to vary the
        ! irrigated fraction according to which tiles have a suitable dvi
        IF ( dvimax_gb(l) > -1.0 .AND. dvimax_gb(l) < 2.0 ) THEN
          l_irrigate_now(l,m) = .TRUE.
        END IF

      CASE DEFAULT
        errorstatus = 101
        CALL ereport(RoutineName, errorstatus, 'Invalid value for irr_crop')

      END SELECT

    END IF  !  frac_irr_soilt(l,m) > 0.0

    IF ( l_irrigate_now(l,m) ) THEN

      ! Loop over soil layers
      DO n = 1,nlayer_irrig
        ! Only irrigate if there is no frozen soil in this layer, and the
        ! soil wetness is below the threshold for irrigating (the critical
        ! point).
        IF ( sthf_soilt(l,m,n) <= 0.0 ) THEN
          IF ( sthu_irr_soilt(l,m,n) <                                         &
               (smvccl_soilt(l,m,n) / smvcst_soilt(l,m,n)) ) THEN

            ! Save the input wetness.
            sthu_irr_tmp = sthu_irr_soilt(l,m,n)

            ! We will irrigate this location.

            ! Set the soil wetness in the irrigated fraction to the
            ! critical point.
            sthu_irr_soilt(l,m,n) = smvccl_soilt(l,m,n)                        &
                                    / smvcst_soilt(l,m,n)

            ! Ensure that irrigated soil moisture is less than saturation:
            sthu_irr_soilt(l,m,n) = MIN(sthu_irr_soilt(l,m,n),                 &
                                        1.0 - sthf_soilt(l,m,n) )

            ! Ensure that tile mean soil moisture is less than saturation:
            sthu_irr_soilt(l,m,n) = MIN(sthu_irr_soilt(l,m,n),                 &
                                        (( 1.0 - sthf_soilt(l,m,n)             &
                                           - sthu_soilt(l,m,n))                &
                                           / frac_irr_soilt(l,m)               &
                                           + sthu_irr_tmp )                    &
                                       )

            ! Calculate the amount of water required to reach the target.
            irrigwater_levels(l,m,n) = (sthu_irr_soilt(l,m,n) - sthu_irr_tmp)  &
                                         * rho_water * dzsoil(n)               &
                                         * smvcst_soilt(l,m,n)                 &
                                         * frac_irr_soilt(l,m)

            ! Calculate tile-mean wetness.
            sthu_soilt(l,m,n) = sthu_soilt(l,m,n) + frac_irr_soilt(l,m)        &
                                * (sthu_irr_soilt(l,m,n) - sthu_irr_tmp)

            IF ( sthu_soilt(l,m,n) + sthf_soilt(l,m,n) > 1.0 ) THEN
              errorstatus = 101
              CALL ereport(RoutineName, errorstatus, 'Super saturation')
            END IF

            ! Update tile moisture content.
            smcl_soilt(l,m,n) = (sthu_soilt(l,m,n) + sthf_soilt(l,m,n))        &
                                  * rho_water * dzsoil(n)                      &
                                  * smvcst_soilt(l,m,n)

          END IF  !  sthu_irr <  smvccl/smvcst
        END IF  !  sthf_soilt <= 0.0 )

      END DO  !  soil layers

    END IF  !  l_irrigate_now

  END DO  !  land points
END DO  !  soil tiles

RETURN

END SUBROUTINE irrigation_demand

!#############################################################################

SUBROUTINE irrigation_actual( land_pts, frac_irr_soilt,                        &
             rivers_sto_per_m2_on_landpts,                                     &
             smvccl_soilt, smvcst_soilt, smvcwt_soilt, sthf_soilt,             &
             l_irrigate_now, irrDaysDiag_gb, irrigwater_levels,                &
             rivers_adj_on_landpts, smcl_soilt, sthu_soilt, sthzw_soilt,       &
             sthu_irr_soilt, irrig_water_gb )

! Account for limitation of irrigation by water availability.
! If there is insufficient water available to meet the full demand for
! irrigation, revise the calculated soil moisture.
!
! The process followed, in which the soil moisture has previously been
! updated assuming plentiful water before it is revised here to account
! for finite water availability, reflects the need to reproduce the
! results of earlier versions of the model.

USE ancil_info, ONLY: nsoilt

USE jules_hydrology_mod, ONLY: zw_max

USE jules_irrig_mod, ONLY: l_irrig_limit, nstep_irrig

USE jules_soil_mod, ONLY: dzsoil, sm_levels

USE timestep_mod, ONLY: timestep

USE um_types, ONLY: real_jlslsm

USE water_constants_mod, ONLY: rho_water

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Scalar arguments with intent(in)
!-----------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  land_pts
    ! Number of land points.

!-----------------------------------------------------------------------------
! Array arguments with intent(in)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  frac_irr_soilt(land_pts,nsoilt),                                             &
    ! Irrigation fraction.
  rivers_sto_per_m2_on_landpts(land_pts),                                      &
    ! River water storage on land points (kg m-2).
  smvccl_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Critical volumetric SMC (cubic m per cubic m of soil).
  smvcst_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric saturation point (m3/m3 of soil).
  smvcwt_soilt(land_pts,nsoilt,sm_levels),                                     &
    ! Volumetric wilting point (m3/m3 of soil).
  sthf_soilt(land_pts,nsoilt,sm_levels)
    ! Frozen soil moisture content of each layer as a fraction of saturation.

LOGICAL, INTENT(IN) ::                                                         &
  l_irrigate_now(land_pts,nsoilt)
    ! TRUE when a tile can be irrigated on this timestep.
    ! FALSE at other times and tiles.

!-----------------------------------------------------------------------------
! Array arguments with intent(in out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
  irrDaysDiag_gb(land_pts),                                                    &
    ! Number of days on which irrigation is applied.
  irrigwater_levels(land_pts,nsoilt,sm_levels),                                &
    ! Addition of irrigation water to each soil layer (kg/m2).
  rivers_adj_on_landpts(land_pts),                                             &
    ! Adjustment factor for water storage on land points.
  smcl_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Soil moisture content of each layer (kg/m2).
  sthu_irr_soilt(land_pts,nsoilt,sm_levels),                                   &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation in irrigated fraction
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
    ! Unfrozen soil moisture content of each layer as a fraction of
    ! saturation.
  sthzw_soilt(land_pts,nsoilt)
    ! Soil moist fraction in deep layer.

!-----------------------------------------------------------------------------
! Array arguments with intent(out)
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  irrig_water_gb(land_pts)
    ! Addition of irrigation water to soil (kg/m2/s).

!----------------------------------------------------------------------------
! Local scalar parameters
!----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  water_epsilon = EPSILON( irrig_water_gb )
    ! Minimum amount of irrigation below which a point is not flagged as
    ! having been irrigated (kg m-2).

LOGICAL , PARAMETER :: irr_zw_all = .TRUE.
    ! Withdraw all available water from the groundwater store (T),
    ! or withdraw until the wilting point only (F).

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'IRRIGATION_ACTUAL'

!-----------------------------------------------------------------------------
! Local scalar variables
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  errorstatus,                                                                 &
    ! Error value.
  l, n, m
    ! Loop counters.

REAL(KIND=real_jlslsm) ::                                                      &
  irrig_sum,                                                                   &
    ! Total irrigwater_levels over soil layers (kg/m2).
    ! This assumes sufficient water is available.
  irrig_lim,                                                                   &
    ! Total irrigwater_levels constrained by water in deep soil (kg/m2).
  irrig_max,                                                                   &
    ! Maximum available water for irrigation from deep soil (kg/m2).
  irrig_dif,                                                                   &
    ! Amount by which irrigwater_levels is decreased because of limited
    ! water availability (kg/m2).
  timestep_irrig,                                                              &
    ! Irrigation timestep length (s).
  zsoil_total
    ! Depth to bottom of soil layers (m).

!----------------------------------------------------------------------------
! Local array variables
!----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  smclsatzw_soilt(land_pts,nsoilt),                                            &
    ! Moisture content in deep layer at saturation (kg/m2).
  smclwiltzw_soilt(land_pts,nsoilt),                                           &
    ! Moisture content in deep layer at wilting point (kg/m2).
  smclzw_soilt(land_pts,nsoilt),                                               &
    ! Actual moisture content in deep layer (kg/m2).
  smclzw_rest_soilt(land_pts,nsoilt),                                          &
    ! Deep moisture content after extraction of irrigwater_levels (kg/m2).
  irrig_riv(land_pts)
    ! Total irrigation water extracted from river routing storage (kg/m2).

LOGICAL ::                                                                     &
  l_irrigated(land_pts)
    ! Flag for grid boxes that receive irrigation water.

!-----------------------------------------------------------------------------
! End of header
!-----------------------------------------------------------------------------

! This routine does not yet fully support tiled soils - see notes below.
IF ( nsoilt > 1 ) THEN
  errorstatus = 101
  CALL ereport(RoutineName, errorstatus, 'Tiled soil not supported.')
END IF

! Initialise diagnostic total.
! Note that the irrig_water_gb diagnostic retains the same value between
! irrigation steps.

! Initialise flag for irrigated points.
l_irrigated(:) = .FALSE.

! Most calculations are only required is we are constraining the irrigation
! supply.
IF ( l_irrig_limit ) THEN

  ! Calculate depth of soil column.
  zsoil_total = 0.0
  DO n = 1,sm_levels
    zsoil_total = zsoil_total + dzsoil(n)
  END DO

  ! Calculate water availability in deep GW store
  smclsatzw_soilt(:,:)   = rho_water * smvcst_soilt(:,:,sm_levels)             &
                           * (zw_max - zsoil_total)
  smclzw_soilt(:,:)      = sthzw_soilt(:,:) * smclsatzw_soilt(:,:)
  IF ( .NOT. irr_zw_all ) THEN
    smclwiltzw_soilt(:,:) = rho_water * smvcwt_soilt(:,:,sm_levels)            &
                            * (zw_max -  zsoil_total)
  END IF
  ! initialise other variables
  smclzw_rest_soilt(:,:)  = smclzw_soilt(:,:)
  irrig_riv(:)            = 0.0

  ! Loop over soil tiles
  DO m = 1, nsoilt
    ! Loop over land points
    DO l = 1, land_pts

      IF ( l_irrigate_now(l,m) ) THEN

        irrig_sum = 0.0
        irrig_max = 0.0
        irrig_lim = 0.0
        irrig_dif = 0.0

        ! total irrigation water added
        ! note that irrigwater_levels is already multiplied by frac_irr_soilt
        ! i.e. units are kg/m2 for entire grid box
        DO n = 1,nlayer_irrig
          irrig_sum = irrig_sum + irrigwater_levels(l,m,n)
        END DO

        IF ( irrig_sum > 0.0 ) THEN
          ! amount of water added should not be more than available
          ! from deep groundwater store
          IF ( .NOT. irr_zw_all ) THEN
            ! option (1) withdraw water until the wilting point
            irrig_max = MAX(smclzw_soilt(l,m) - smclwiltzw_soilt(l,m), 0.0)
          ELSE
            ! option (2) withdraw all available water
            irrig_max = MAX( smclzw_soilt(l,m), 0.0)
          END IF

          irrig_lim = MIN(irrig_max, irrig_sum)
          smclzw_rest_soilt(l,m) = smclzw_soilt(l,m) - irrig_lim
                                                  !min(irrig_max,irrig_sum)

          ! re-calculate soil moisture fraction in deep layer
          sthzw_soilt(l,m) = MAX(smclzw_rest_soilt(l,m)                        &
                                 /smclsatzw_soilt(l,m)                         &
                                 ,0.0 )

          ! if irrigation is constrained, try extracting from river
          ! store
          ! This is not robust with soil tiling. We would need to consider
          ! the possibility that a soil tile could exhaust the river store,
          ! leaving nothing for later tiles.
          IF ( rivers_sto_per_m2_on_landpts(l) > 0.0) THEN
            irrig_riv(l) = MAX(irrig_sum - irrig_lim, 0.0)
            irrig_riv(l) = MIN(rivers_sto_per_m2_on_landpts(l),                &
                               irrig_riv(l))
            rivers_adj_on_landpts(l) = (rivers_sto_per_m2_on_landpts(l)        &
                                        - irrig_riv(l))                        &
                                       / rivers_sto_per_m2_on_landpts(l)
            irrig_lim = irrig_lim + irrig_riv(l)
          END IF ! rivers_sto_per_m2_on_landpts gt 0

          ! re-calculate soil moisture
          ! if irrigation is constrained, some or all of the extra water
          ! that was added should be subtracted again
          ! Note that this only needs to be done on the topmost
          ! nlayer_irrig layers, but doing that changes the KGO.
          DO n = 1,sm_levels
            ! amount of water to be subtracted
            irrig_dif = (1.0 - irrig_lim / irrig_sum ) *                       &
                          irrigwater_levels(l,m,n)
            ! update soil moisture variables
            irrigwater_levels(l,m,n) = MAX(irrigwater_levels(l,m,n)            &
                                           - irrig_dif                         &
                                           , 0.0)
            sthu_irr_soilt(l,m,n) = sthu_irr_soilt(l,m,n)                      &
                                    - irrig_dif                                &
                                      / (rho_water * dzsoil(n)                 &
                                         * smvcst_soilt(l,m,n)                 &
                                         * frac_irr_soilt(l,m))
            smcl_soilt(l,m,n) = smcl_soilt(l,m,n) - irrig_dif
            sthu_soilt(l,m,n) = smcl_soilt(l,m,n)                              &
                                / (rho_water * dzsoil(n)                       &
                                   * smvcst_soilt(l,m,n))                      &
                                - sthf_soilt(l,m,n)
          END DO ! n
        END IF ! irrigsum gt 0

      END IF  !  l_irrigate_now

    END DO  !  points
  END DO  !  tiles
END IF  !  l_irrig_limit

! Calculate irrigation timestep length.
timestep_irrig = timestep * REAL(nstep_irrig)

! Loop over land points
DO l = 1, land_pts
  ! Sum irrigation over soil tiles and irrigated layers, and convert to rate.
  irrig_water_gb(l) = SUM(irrigwater_levels(l,:,1:nlayer_irrig))               &
                      / timestep_irrig
  IF ( irrig_water_gb(l) > water_epsilon ) l_irrigated(l) = .TRUE.
END DO

! Calculate number of days on which irrigation is applied at each location.
WHERE ( l_irrigated ) irrDaysDiag_gb = irrDaysDiag_gb + 1

END SUBROUTINE irrigation_actual

END MODULE irrigation_mod
