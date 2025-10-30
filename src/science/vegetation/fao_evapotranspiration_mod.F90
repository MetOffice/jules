!******************************COPYRIGHT**************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE fao_evapotranspiration

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
!-----------------------------------------------------------------------------
! Description:
!   Utilities for calculating FAO Penman-Monteith evapotranspiration for a
!   reference crop, as described in "Crop evapotranspiration - Guidelines for
!   computing crop water requirements - FAO Irrigation and Drainage paper 56".
!   Note that the approximations used in this module are taken from this
!   paper, and may be different to those used elsewhere in JULES.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

PRIVATE

PUBLIC :: fao_ref_evapotranspiration

CONTAINS

!#############################################################################
SUBROUTINE fao_ref_evapotranspiration(soil_pts, soil_index,                    &
                                      land_pts, land_index,                    &
                                      t1p5m, sw_down, lw_down,                 &
                                      surf_ht_flux, u10m, v10m, q1p5m, pstar,  &
                                      trad, fao_et0)

USE conversions_mod,          ONLY: zerodegc, secs_in_day => rsec_per_day
USE theta_field_sizes,        ONLY: t_i_length, t_j_length
USE csigma,                   ONLY: sbcon
USE qsat_mod,                 ONLY: qsat

!-----------------------------------------------------------------------------
! Description:
!   Calculates FAO Penman-Monteith evapotranspiration for a reference crop,
!   as defined by equation 6 in "Crop evapotranspiration - Guidelines for
!   computing crop water requirements - FAO Irrigation & Drainage paper 56"
!   and converts units to kg m-2 s-1.
!
! Method:
!   Use the Penman-Monteith equation for a reference crop, which is defined as
!   having a crop height of 0.12 m, a fixed surface resistance of 70 s m-1 and
!   an albedo of 0.23.
!   Additional assumptions (beyond those descibed in FAO I&D paper 56):
!    * temperature and humidity at 2m above ground can be approximated by
!      t1p5m and q1p5m.
!    * u and v components of wind at 10m above ground can be approximated by
!      u10m and v10m

! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) ::                                                         &
  soil_pts,                                                                    &
    ! number of soil points.
  land_pts,                                                                    &
    ! number of land points
  land_index(land_pts),                                                        &
    ! index of land points
  soil_index(land_pts)
    ! index of soil points

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  t1p5m(t_i_length,t_j_length),                                                &
    ! temperature at 1p5m (K)
  sw_down(t_i_length,t_j_length),                                              &
    ! Surface downward SW radiation (W/m2).
  lw_down(t_i_length,t_j_length),                                              &
    ! Surface downward LW radiation (W/m2).
  surf_ht_flux(t_i_length,t_j_length),                                         &
    ! Net downward heat flux at surface over land and sea-ice fraction of
    ! gridbox (W/m2)
  u10m(t_i_length,t_j_length),                                                 &
    ! x-cpt of wind at 10m (m s-1)
  v10m(t_i_length,t_j_length),                                                 &
    ! y-cpt of wind at 10m (m s-1)
  q1p5m(t_i_length,t_j_length),                                                &
    ! specific humidity at 1.5m
  pstar(t_i_length,t_j_length),                                                &
    ! surface pressure (Pascals).
  trad(land_pts)
    ! temperature to use when calculating upward longwave (K)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  fao_et0(land_pts)
    ! FAO Penman-Monteith evapotranspiration for
    ! reference crop in kg m-2 s-1

! Local variables
INTEGER :: spt, i, j, l ! do loop variables

REAL(KIND=real_jlslsm) ::                                                      &
  lw_net,                                                                      &
    ! net downward longwave radiation (W/m2)
  t2m_degC,                                                                    &
    ! air temperature at 2m in degrees C
  Rn,                                                                          &
    ! net downward radiation in MJ m-2 day-1
  g,                                                                           &
    ! net downward soil heat flux in MJ m-2 day-1
  ws10,                                                                        &
    ! wind speed at 10m above ground (m s-1)
  ws2,                                                                         &
    ! wind speed at 2m above ground (m s-1)
  delta,                                                                       &
    ! slope of the saturation vapour pressure curve in kPa per deg C
  relhumidity_frac,                                                            &
    ! relative humidity, as a fraction
  sat_spec_humidity,                                                           &
    ! specific humidity at saturation
  gam,                                                                         &
    ! psychrometric constant in kPa per deg C
  es_minus_ea,                                                                 &
    ! vapour pressure deficit in kPa
  denom
    ! denominator of the FAO P-M equation

!-----------------------------------------------------------------------------

fao_et0(:) = 0.0

DO spt = 1,soil_pts
  l = soil_index(spt)

  j = (land_index(l) - 1) / t_i_length + 1
  i = land_index(l) - (j-1) * t_i_length

  t2m_degC = t1p5m(i,j) - zerodegc

  ! Emissivity is assumed to be 1 as in the FAO calculation.
  lw_net = lw_down(i,j) - sbcon * trad(l)** 4.0

  ! The albedo of FAO reference crop field is defined to be 0.23.
  Rn = (sw_down(i,j) * (1.0-0.23) + lw_net) * secs_in_day / 1.0e6
  g = surf_ht_flux(i,j) * secs_in_day / 1.0e6

  CALL qsat(sat_spec_humidity, t1p5m(i,j), pstar(i,j))

  relhumidity_frac = q1p5m(i,j) / sat_spec_humidity

  ws10  = SQRT(u10m(i,j)** 2.0 + v10m(i,j)** 2.0)

  delta = fao_slope_vapour_pressure_curve(t2m_degC)

  ws2   = fao_2m_wind_speed(ws10, 10.0)

  gam   = fao_psychrometric_constant(pstar(i,j))

  es_minus_ea = fao_vapour_pressure_deficit(t2m_degC, relhumidity_frac)

  ! Eqn 6 FAO I&D paper 56 gives ET0 in mm day-1
  denom      = delta + gam * (1.0+0.34 * ws2)
  fao_et0(l) = (0.408 * delta * (Rn - g)                                       &
               + gam * (900.0 / (t2m_degC+273.0)) * ws2 * es_minus_ea)         &
               / denom

  ! convert from mm day-1 to kg m-2 s-1
  fao_et0(l) = fao_et0(l) / secs_in_day

END DO

END SUBROUTINE fao_ref_evapotranspiration

!#############################################################################

FUNCTION fao_slope_vapour_pressure_curve(temperature_degC) RESULT (delta)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the slope of the saturation vapour pressure curve in kPa
!   using approximations in FAO I&D paper 56.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) :: temperature_degC
  ! 2m air temperature in degrees Celsius

! Returns
REAL(KIND=real_jlslsm) :: delta
  ! slope of the saturation vapour pressure curve in kPa

! Local variables
REAL(KIND=real_jlslsm) :: denom
  ! denominator of equation to calculate delta

!-----------------------------------------------------------------------------

! Eqn 13 FAO I&D paper 56
denom = (temperature_degC+237.3)** 2.0
delta = 4098.0 *                                                               &
        (0.6108 * EXP(17.27 * temperature_degC / (temperature_degC+237.3)))    &
        / denom

END FUNCTION fao_slope_vapour_pressure_curve

!#############################################################################

FUNCTION fao_vapour_pressure_deficit(t2m_degC, relhumidity_frac)               &
         RESULT (es_minus_ea)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the vapour pressure deficit at 2m in kPa using approximations
!   in FAO I&D paper 56.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments
REAL(KIND=real_jlslsm) ::                                                      &
  t2m_degC,                                                                    &
    ! air temperature at 2m in degrees C
  relhumidity_frac
    ! relative humidity of the air at 2m, given as a fraction

! Returns
REAL(KIND=real_jlslsm) :: es_minus_ea
    ! vapour pressure deficit at 2m in kPa

! Local variables
REAL(KIND=real_jlslsm) ::                                                      &
  es,                                                                          &
    ! saturation vapour pressure in kPa
  ea
    ! actual vapour pressure in kPa

!-----------------------------------------------------------------------------
es = fao_saturation_vapour_pressure(t2m_degC)

! Eqn 19 FAO I&D paper 56
ea = relhumidity_frac * es

es_minus_ea = es - ea

END FUNCTION fao_vapour_pressure_deficit

!#############################################################################

FUNCTION fao_saturation_vapour_pressure(t2m_degC) RESULT (e0)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the saturation vapour pressure in kPa given the 2m air
!   temperature in degrees Celsius using approximations in FAO I&D paper 56.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) :: t2m_degC
    ! air temperature at 2m in degrees C

! Returns
REAL(KIND=real_jlslsm) :: e0
    ! saturation vapour pressure in kPa

!-----------------------------------------------------------------------------

! Eqn 11 FAO I&D paper 56
e0 = 0.6108 * EXP(17.27 * t2m_degC / (t2m_degC+237.3))

END FUNCTION fao_saturation_vapour_pressure

!#############################################################################

FUNCTION fao_2m_wind_speed(wind_speed_at_z, z) RESULT (wind_speed_at_2m)
!-----------------------------------------------------------------------------
! Description:
!   Calculates the wind speed at 2m given the wind speed at another height
!   using approximations in FAO I&D paper 56.
!
! Method:
!   Assumes a logarithmic wind speed profile
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  wind_speed_at_z,                                                             &
    ! wind speed at height z in m s-1
  z
    ! height in m at which the wind speed is measured

! Returns
REAL(KIND=real_jlslsm) :: wind_speed_at_2m
    ! wind speed at 2m in m s-1

!-----------------------------------------------------------------------------
! Eqn 47 FAO I&D paper 56
wind_speed_at_2m = wind_speed_at_z * 4.87 / ( LOG(67.8 * z - 5.42) )

END FUNCTION fao_2m_wind_speed

!#############################################################################

FUNCTION fao_psychrometric_constant(pressure_in_Pa) RESULT (gam)
!-----------------------------------------------------------------------------
! Description:
!   Calculates psychrometric constant in kPa per degree C from air pressure
!   using FAO I&D paper 56 equation 8. N.b. in FAO I&D paper 56, the pressure
!   is found using an approximation based on the elevation (equation 7)
!   but we use the actual air pressure here.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments
REAL(KIND=real_jlslsm), INTENT(IN) :: pressure_in_Pa
  ! pressure in Pa

! Returns
REAL(KIND=real_jlslsm) :: gam
  ! psychrometric constant in kPa per degree C

!-----------------------------------------------------------------------------

! Eqn 8 FAO I&D paper 56
! which is derived from gam = c_p pressure / (epsilon lambda)
! where
! c_p = 1.013e-3 MJ kg-1 per degree C
! is specific heat at constant pressure (average atmospheric conditions),
! epsilon = 0.622 is ratio molecular weight of water vapour/dry air
! and lambda = 2.45 MJ kg-1 is latent heat of vaporization for an air temp
! of ~20 degree C (lambda only varies slightly over normal temperature
! ranges)
gam = 0.665e-3 * pressure_in_Pa
! convert from Pa per degree C to kPa per degree C
gam = gam * 1.0e-3

END FUNCTION fao_psychrometric_constant

!#############################################################################

END MODULE fao_evapotranspiration
