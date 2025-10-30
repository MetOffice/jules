#if !defined(UM_JULES)
!******************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

MODULE impose_diurnal_cycle_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE impose_diurnal_cycle(forcing)

USE model_time_mod, ONLY: current_time, timestep_len,                          &
                           timesteps_in_day

USE model_grid_mod, ONLY: latitude, longitude

USE datetime_mod, ONLY: secs_in_day, secs_in_hour, l_360, l_leap

USE datetime_utils_mod, ONLY: day_of_year, days_in_year


USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE conversions_mod, ONLY: pi

USE disaggregated_precip, ONLY: ls_rain_disagg, con_rain_disagg,               &
                                 ls_snow_disagg, con_snow_disagg

USE qsat_mod, ONLY: qsat
USE sunny_mod, ONLY: sunny

USE jules_forcing_mod, ONLY: forcing_type

USE update_mod, ONLY: l_daily_disagg, l_disagg_const_rh, use_diff_rad

IMPLICIT NONE

! Arguments
TYPE(forcing_type), INTENT(IN OUT) :: forcing

REAL, ALLOCATABLE :: tl_1_nodiurnalcycle(:,:)

REAL :: qsat_1d(t_i_length * t_j_length)
REAL :: qsat_1d_day(t_i_length * t_j_length)
!      Saturated specific humidity given daily mean temperature (kg kg-1).
REAL :: qsat_2d(t_i_length,t_j_length)
REAL, ALLOCATABLE :: sun(:,:)
REAL :: rh_2d_day(t_i_length,t_j_length)
!     Daily mean relative humidity (1).
REAL :: sun_tstep(t_i_length,t_j_length)
REAL :: time_max_1d(t_i_length * t_j_length)
REAL :: time_max_2d(t_i_length,t_j_length)

INTEGER :: n_points
INTEGER :: daynumber
INTEGER :: x_tofday

!-----------------------------------------------------------------------------
! Description:
!     Imposes a diurnal cycle on top of the forcing variables tl_1_ij,
!     lw_down_ij and sw_down_ij
!     Copy ls_rain_ij, con_rain_ij, ls_snow_ij, forcing%con_snow_ij from
!     disaggregated_precip variables
!     Makes sure qw_1_ij isn't above qsat (if l_disagg_const_rh=F), or imposes
!     constant relative humidity (if l_disagg_const_rh=T)
!
!      Used only when l_daily_disagg = T
!
! Method:
!      Same as used in day_calc within IMOGEN
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IF ( .NOT. l_daily_disagg ) RETURN

ALLOCATE( tl_1_nodiurnalcycle( t_i_length, t_j_length ))

n_points = t_i_length * t_j_length

tl_1_nodiurnalcycle(:,:) = forcing%tl_1_ij(:,:)

x_tofday = current_time%TIME / timestep_len + 1 ! x_tofday = 1 at midnight

ALLOCATE(sun(n_points,timesteps_in_day))

!sunny calls solpos, which assumes 360 day years
daynumber  = day_of_year(                                                      &
  current_time%year, current_time%month, current_time%day, l_360, l_leap       &
)
daynumber  = NINT( REAL(daynumber) * 360.0                                     &
                / REAL(days_in_year(current_time%year, l_360, l_leap)))

CALL sunny( daynumber, timesteps_in_day, n_points, current_time%year,          &
            RESHAPE(latitude(:,:),  [ n_points ]),                             &
            RESHAPE(longitude(:,:), [ n_points ]),                             &
            sun, time_max_1d )

time_max_2d(:,:)   = RESHAPE(time_max_1d(:),                                   &
                     [ t_i_length, t_j_length ])

forcing%tl_1_ij(:,:) = tl_1_nodiurnalcycle(:,:) +                              &
               0.5 * forcing%diurnal_temperature_range_ij(:,:) *               &
               COS( 2.0 * pi * ( current_time%TIME - secs_in_hour *            &
                    time_max_2d(:,:) )                                         &
               / secs_in_day )

IF ( l_disagg_const_rh ) THEN
  !     Calculate relative humidity given daily mean temperature.

  CALL qsat(qsat_1d_day,                                                       &
          RESHAPE(tl_1_nodiurnalcycle(:,:), [ n_points ]),                     &
          RESHAPE(forcing%pstar_ij(:,:),[ n_points ]), n_points)

  rh_2d_day(:,:) = MIN( forcing%qw_1_ij(:,:) /                                 &
                 RESHAPE( qsat_1d_day(:), [ t_i_length, t_j_length ] ),        &
                 1.0 )
END IF

!   Calulate saturated humidity given current temperature.

CALL qsat(qsat_1d,                                                             &
          RESHAPE(forcing%tl_1_ij(:,:), [ n_points ]),                         &
          RESHAPE(forcing%pstar_ij(:,:),[ n_points ]), n_points)

sun_tstep(:,:) = RESHAPE(sun(:, x_tofday),                                     &
                   [ t_i_length, t_j_length ])
qsat_2d(:,:)   = RESHAPE(qsat_1d(:),                                           &
                   [ t_i_length, t_j_length ])

forcing%lw_down_ij(:,:) = forcing%lw_down_ij(:,:) *                            &
                  (4.0 * forcing%tl_1_ij(:,:) / tl_1_nodiurnalcycle(:,:) - 3.0)

forcing%sw_down_ij(:,:) = forcing%sw_down_ij(:,:) * sun_tstep

IF ( use_diff_rad ) THEN
  forcing%diff_rad_ij(:,:) = forcing%diff_rad_ij(:,:) * sun_tstep
END IF

IF ( l_disagg_const_rh ) THEN
  !     Constant relative humidity.
  forcing%qw_1_ij(:,:) = rh_2d_day(:,:) * qsat_2d(:,:)
ELSE
  !     Constant specific humidity.
  forcing%qw_1_ij(:,:) = MIN( forcing%qw_1_ij(:,:), qsat_2d(:,:) )
END IF

forcing%ls_rain_ij( :,:)  =  ls_rain_disagg( :,:, x_tofday)
forcing%con_rain_ij(:,:)  =  con_rain_disagg(:,:, x_tofday)
forcing%ls_snow_ij( :,:)  =  ls_snow_disagg( :,:, x_tofday)
forcing%con_snow_ij(:,:)  =  con_snow_disagg(:,:, x_tofday)

DEALLOCATE(tl_1_nodiurnalcycle)
DEALLOCATE(sun)

END SUBROUTINE impose_diurnal_cycle
END MODULE impose_diurnal_cycle_mod
#endif
