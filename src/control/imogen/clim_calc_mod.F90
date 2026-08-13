#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE clim_calc_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE clim_calc(land_pts, mm, md, nsdmax, seed_rain, imgn_drive, ainfo)

USE missing_data_mod, ONLY: rmdi
USE model_time_mod, ONLY: timesteps_in_day
USE datetime_mod, ONLY: secs_in_day
USE imgn_drive_mod, ONLY: imgn_drive_type
USE ancil_info, ONLY: ainfo_type
USE theta_field_sizes, ONLY: t_i_length
USE imogen_run, ONLY: l_daily_metdata_climatol
USE day_calc_mod, ONLY: day_calc

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Calculates the daily and sub daily meteorology based on the monthly
!      climatology and any anomalies / patterns
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                         &
  mm,                                                                          &
           ! Number of months
  md,                                                                          &
           ! Number of days in month (in this case 30 expected)
  nsdmax,                                                                      &
           ! Maximum number of possible subdaily increments
  land_pts,                                                                    &
           ! Number of land points
  seed_rain(4)
           ! Seeding number for subdaily rainfall.

TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(ainfo_type), INTENT(IN) :: ainfo

! Create "subdaily" values of the arrays below using day_calc
REAL ::                                                                        &
  tl1_subdaily(land_pts,nsdmax),                                               &
           ! Calculated temperature (K)
  conv_rain_subdaily(land_pts,nsdmax),                                         &
           ! Calculated convective rainfall (mm/day)
  ls_rain_subdaily(land_pts,nsdmax),                                           &
           ! Calculated large scale rainfall (mm/day)
  ls_snow_subdaily(land_pts,nsdmax),                                           &
           ! Calculated large scale snowfall (mm/day)
  ql1_subdaily(land_pts,nsdmax),                                               &
           ! Calculated humidity (kg/kg)
  wind_subdaily(land_pts,nsdmax),                                              &
           ! Calculated wind  (m/s)
  pstar_subdaily(land_pts,nsdmax),                                             &
           ! Calculated pressure (Pa)
  swdown_subdaily(land_pts,nsdmax),                                            &
           ! Calculated shortwave radiation (W/m2)
  lwdown_subdaily(land_pts,nsdmax)
           ! Calculated longwave radiation (W/m2)

! Variables for daily climatology
REAL ::                                                                        &
  tl1_daily(land_pts,mm,md),                                                   &
           ! Calculated temperature (K)
  precip_daily(land_pts,mm,md),                                                &
           ! Calculated precipitation (mm/day)
  wind_daily(land_pts,mm,md),                                                  &
           ! Calculated wind  (m/s)
  diurnal_tl1_daily(land_pts,mm,md),                                           &
           ! Calculated diurnal Temperature (K)
  pstar_daily(land_pts,mm,md),                                                 &
           ! Calculated pressure (Pa)
  swdown_daily(land_pts,mm,md),                                                &
           ! Calculated shortwave radiation (W/m2)
  lwdown_daily(land_pts,mm,md),                                                &
           ! Calculated longwave radiation (W/m2)                                                                       &
  ql1_daily(land_pts,mm,md)
           ! Calculated specific humidity (kg/kg)

INTEGER ::                                                                     &
  l,i,j,id,im,idxtime,                                                         &
           ! Loop parameters
  istep
           ! Looping parameter over sub-daily periods

!---------------------------------------------------------------------
! Calculate monthly means and add anomalies.
! Anomalies will be zero, if l_change_metdata=.false.
DO im = 1,mm                ! Loop over months
  DO l = 1,land_pts         ! Loop over land points

    j = (ainfo%land_index(l) - 1) / t_i_length + 1
    i = ainfo%land_index(l) - (j-1) * t_i_length

    DO id = 1,md            ! Loop over the days

      IF ( l_daily_metdata_climatol ) THEN
        idxtime = (im-1)*30+id   ! 360 day calendar
      ELSE
        idxtime = im
      END IF

      tl1_daily(l,im,id) = imgn_drive%tl1_ij_clim(i,j,idxtime) +               &
                              imgn_drive%tl1_ij_anom(i,j,im)
      swdown_daily(l,im,id) = imgn_drive%swdown_ij_clim(i,j,idxtime) +         &
                              imgn_drive%swdown_ij_anom(i,j,im)
      ql1_daily(l,im,id) = imgn_drive%ql1_ij_clim(i,j,idxtime) +               &
                              imgn_drive%ql1_ij_anom(i,j,im)
      diurnal_tl1_daily(l,im,id) =                                             &
                          imgn_drive%diurnal_tl1_ij_clim(i,j,idxtime) +        &
                          imgn_drive%diurnal_tl1_ij_anom(i,j,im)
      precip_daily(l,im,id) = imgn_drive%precip_ij_clim(i,j,idxtime) +         &
                              imgn_drive%precip_ij_anom(i,j,im)

      ! Make sure precip anomalies do not produce negative rainfall
      ! Convert input precipitation from kg/m2/s to kg/m2/day
      precip_daily(l,im,id) = precip_daily(l,im,id) * 86400
      precip_daily(l,im,id) = MAX(precip_daily(l,im,id), 0.0)


      ! Check to make sure anomalies do not produce negative values
      swdown_daily(l,im,id) = MAX(swdown_daily(l,im,id), 0.0)
      diurnal_tl1_daily(l,im,id) = MAX(diurnal_tl1_daily(l,im,id), 0.0)

      ! Pressure (Pa)
      pstar_daily(l,im,id) = imgn_drive%pstar_ij_clim(i,j,idxtime) +           &
                                       imgn_drive%pstar_ij_anom(i,j,im)

      ! Longwave radiation (W/m2)
      lwdown_daily(l,im,id) = imgn_drive%lwdown_ij_clim(i,j,idxtime) +         &
                                       imgn_drive%lwdown_ij_anom(i,j,im)

      ! Wind
      wind_daily(l,im,id) = imgn_drive%wind_ij_clim(i,j,idxtime) +             &
                                      imgn_drive%wind_ij_anom(i,j,im)
      ! Check to make sure anomalies do not produce zero windspeed
      wind_daily(l,im,id) = MAX(wind_daily(l,im,id), 0.01)
    END DO
  END DO
END DO

! Disaggregate daily down to sub-daily

! Variables going in (with units) are SWdown (W/m2), Precip (mm/day), tl1
! (K), diurnal_tl1 (K), LWdown (W/m2), pstar(Pa), Wind (m/2) and ql1 (kg/kg)

! Variables coming out are subdaily estimates of above variables, except
! for diurnal_tl1 (which no longer has meaning), and the temperature dependent
! splitting of precipitation back into ls and conv snow and rainfall
! There is assumed no convective snow, and so conv_snow_ij_drive is set to zero.

DO im = 1,mm                ! Loop over the months
  DO id = 1,md              ! Loop over the days

    CALL day_calc(                                                             &
      land_pts,swdown_daily(:,im,id),precip_daily(:,im,id),                    &
      tl1_daily(:,im,id),diurnal_tl1_daily(:,im,id),lwdown_daily(:,im,id),     &
      pstar_daily(:,im,id),wind_daily(:,im,id),ql1_daily(:,im,id),             &
      swdown_subdaily,tl1_subdaily,lwdown_subdaily,                            &
      conv_rain_subdaily,ls_rain_subdaily,                                     &
      ls_snow_subdaily,pstar_subdaily,wind_subdaily,                           &
      ql1_subdaily,im,id,nsdmax,seed_rain)

    ! Finalise value and set unused output values as rmdi as a precaution.
    DO l = 1,land_pts

      j = (ainfo%land_index(l) - 1) / t_i_length + 1
      i = ainfo%land_index(l) - (j-1) * t_i_length

      DO istep = 1,timesteps_in_day
        imgn_drive%swdown_ij_drive(i,j,im,id,istep) =                          &
                                 swdown_subdaily(l,istep)
        imgn_drive%tl1_ij_drive(i,j,im,id,istep) =                             &
                                 tl1_subdaily(l,istep)
        imgn_drive%lwdown_ij_drive(i,j,im,id,istep) =                          &
                                 lwdown_subdaily(l,istep)
        imgn_drive%conv_rain_ij_drive(i,j,im,id,istep) =                       &
                                 conv_rain_subdaily(l,istep)/REAL(secs_in_day)
        imgn_drive%conv_snow_ij_drive(i,j,im,id,istep) = 0.0
        imgn_drive%ls_rain_ij_drive(i,j,im,id,istep) =                         &
                                 ls_rain_subdaily(l,istep)/REAL(secs_in_day)
        imgn_drive%ls_snow_ij_drive(i,j,im,id,istep) =                         &
                                 ls_snow_subdaily(l,istep)/REAL(secs_in_day)
        imgn_drive%pstar_ij_drive(i,j,im,id,istep) =                           &
                                 pstar_subdaily(l,istep)
        imgn_drive%wind_ij_drive(i,j,im,id,istep) =                            &
                                 wind_subdaily(l,istep)
        imgn_drive%ql1_ij_drive(i,j,im,id,istep) =                             &
                                 ql1_subdaily(l,istep)
      END DO

      DO istep = timesteps_in_day+1,nsdmax
        imgn_drive%swdown_ij_drive(i,j,im,id,istep)    = rmdi
        imgn_drive%tl1_ij_drive(i,j,im,id,istep)       = rmdi
        imgn_drive%lwdown_ij_drive(i,j,im,id,istep)    = rmdi
        imgn_drive%conv_rain_ij_drive(i,j,im,id,istep) = rmdi
        imgn_drive%conv_snow_ij_drive(i,j,im,id,istep) = rmdi
        imgn_drive%ls_rain_ij_drive(i,j,im,id,istep)   = rmdi
        imgn_drive%ls_snow_ij_drive(i,j,im,id,istep)   = rmdi
        imgn_drive%pstar_ij_drive(i,j,im,id,istep)     = rmdi
        imgn_drive%wind_ij_drive(i,j,im,id,istep)      = rmdi
        imgn_drive%ql1_ij_drive(i,j,im,id,istep)       = rmdi
      END DO
    END DO
  END DO                  ! End of loop over days
END DO                    ! End of loop over months

RETURN

END SUBROUTINE clim_calc
END MODULE clim_calc_mod
#endif
