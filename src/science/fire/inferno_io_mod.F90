! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************
!
! Code Description:
!   Language: FORTRAN 90
!
! Code Owner: Please refer to ModuleLeaders.txt
!

MODULE inferno_io_mod

USE parkind1,                       ONLY: jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_out = 1
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = "INFERNO_IO_MOD"

CONTAINS

SUBROUTINE inferno_io(                                                         &
  t1p5m_tile, q1p5m_tile, pstar, sthu_soilt                                    &
, sm_levels, frac, c_soil_dpm_gb, c_soil_rpm_gb, canht, ls_rain, con_rain      &
, fire_vars                                                                    &
, land_pts, ignition_method, nsurft, asteps_since_triffid                      &
! New Arguments to replace USE statements
! TRIF_VARS_MOD
, g_burn_pft_acc                      )

USE inferno_mod,  ONLY:                                                        &
  calc_flam, calc_ignitions, calc_burnt_area,                                  &
  calc_emitted_carbon, calc_emitted_carbon_soil, calc_emission

USE yomhook,      ONLY: lhook, dr_hook

USE pftparm,                  ONLY:                                            &
  fef_co2, fef_co, fef_ch4, fef_nox, fef_so2, fef_oc, fef_bc,                  &
  fef_c2h4, fef_c2h6, fef_c3h8, fef_hcho,                                      &
  fef_mecho, fef_nh3, fef_dms,                                                 &
    ! PFT Emission factors from namelists
  ccleaf_min, ccleaf_max, ccwood_min, ccwood_max,                              &
    ! PFT Combustion completeness from namelists
  avg_ba, fire_mort
    ! Average Burned Area per PFT, and fire mortality rate

USE jules_inferno_mod, ONLY:                                                   &
  l_trif_fire, flam_rhum_low, flam_rhum_up,                                    &
  flam_sm_low, flam_sm_up, flam_fuel_low,                                      &
  flam_fuel_up, flam_rain_const, flam_sm_func

USE qsat_mod, ONLY: qsat_wat

USE jules_surface_types_mod,        ONLY: npft
USE parkind1,                       ONLY: jprb
USE timestep_mod,                   ONLY: timestep
USE calc_c_comps_triffid_mod,       ONLY: calc_c_comps_triffid

USE ancil_info,                     ONLY: nsoilt

USE fire_vars_mod, ONLY: fire_vars_type

IMPLICIT NONE
!
! Description:
!   Called every model timestep, this subroutine updates INFERNO's
!   driving variables and calls the scientific routines.
!
! Note that this code is currently incompatible with soil tiling.
! The calculation of inferno_sm needs work.
! Therefore all soil tile dimensions are hard coded to 1
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!

INTEGER, INTENT(IN) ::                                                         &
  land_pts, sm_levels, ignition_method, asteps_since_triffid, nsurft

REAL(KIND=real_jlslsm),    INTENT(IN) ::                                       &
  t1p5m_tile(land_pts,nsurft),                                                 &
  q1p5m_tile(land_pts,nsurft),                                                 &
  pstar(land_pts),                                                             &
  sthu_soilt(land_pts,nsoilt,sm_levels),                                       &
  frac(land_pts,nsurft),                                                       &
  c_soil_dpm_gb(land_pts),                                                     &
         ! Gridbox soil C in the Decomposable Plant Material pool (kg m-2).
  c_soil_rpm_gb(land_pts),                                                     &
         ! Gridbox soil C in the Resistant Plant Material pool (kg m-2).
  canht(land_pts,npft),                                                        &
  ls_rain(land_pts),                                                           &
  con_rain(land_pts)

TYPE(fire_vars_type), INTENT(IN OUT) :: fire_vars

REAL(KIND=real_jlslsm) ::                                                      &
  c_root,                                                                      &
    ! Carbon in leaves (kg m-2).
  c_veg
    ! Carbon in vegetation (kg m-2).

! New Argunets to replace USE Statements
! TRIF_VARS_MOD
REAL(KIND=real_jlslsm), INTENT(OUT) :: g_burn_pft_acc(land_pts,npft)

  ! Local temporary variables used in the interactive fire code
REAL(KIND=real_jlslsm)                        ::                               &
  inferno_temp(land_pts),                                                      &
    ! The temperature (K)
  inferno_rhum(land_pts),                                                      &
    ! The Relative Humidity (%)
  inferno_sm(land_pts),                                                        &
    ! The Soil Moisture (Fraction of saturation)
  inferno_rain(land_pts),                                                      &
    ! The total rainfall (kg/m2/s)
  inferno_fuel(land_pts),                                                      &
    ! The fuel density (fine litter and leaves - kg/m3)
  qsat(land_pts),                                                              &
    ! Saturation humidity
  ignitions(land_pts),                                                         &
    ! The number of ignitions (#/m2/s)
  lai_bal_inf(land_pts,npft),                                                  &
    ! The balanced lai used to compute carbon pools
  leaf_inf(land_pts,npft),                                                     &
    ! The leaf carbon
  wood_inf(land_pts,npft),                                                     &
    ! The wood carbon
  dpm_fuel(land_pts),                                                          &
    ! The amount of DPM that is available to burn (kgC.m-2)
  rpm_fuel(land_pts),                                                          &
    ! The amount of RPM that is available to burn (kgC.m-2)
  ls_rain_filtered(land_pts),                                                  &
    ! Large scale rain from input after filtering negative values
  con_rain_filtered(land_pts)
    ! Convective rain from input after filtering negative values


REAL(KIND=real_jlslsm) ,   PARAMETER      ::                                   &
  fef_co2_dpm = 1637.0, fef_co_dpm  = 89.0,                                    &
  fef_ch4_dpm = 3.92, fef_nox_dpm = 2.51,                                      &
  fef_so2_dpm = 0.40,                                                          &
  fef_oc_dpm  = 8.2,   fef_bc_dpm  = 0.56,                                     &
  fef_c2h4_dpm = 0.0, fef_c2h6_dpm = 0.0,                                      &
  fef_c3h8_dpm = 0.0 , fef_hcho_dpm = 0.0,                                     &
  fef_mecho_dpm = 0.0,                                                         &
  fef_nh3_dpm = 0.0 , fef_dms_dpm = 0.0,                                       &
    ! HARDCODED Emission factors for DPM in g kg-1
  fef_co2_rpm = 1489.0, fef_co_rpm  = 127.0,                                   &
  fef_ch4_rpm = 5.96,  fef_nox_rpm = 0.90,                                     &
  fef_so2_rpm = 0.40,                                                          &
  fef_oc_rpm  = 8.2,   fef_bc_rpm  = 0.56,                                     &
  fef_c2h4_rpm = 0.0, fef_c2h6_rpm = 0.0,                                      &
  fef_c3h8_rpm = 0.0 , fef_hcho_rpm = 0.0,                                     &
  fef_mecho_rpm = 0.0,                                                         &
  fef_nh3_rpm = 0.0 , fef_dms_rpm = 0.0,                                       &
    ! HARDCODED Emission factors for DPM and RPM in g kg-1
  pmtofuel    = 0.7
    ! Plant Material that is available as fuel (on the surface)

REAL(KIND=real_jlslsm) ,   PARAMETER      ::                                   &
  rain_tolerance = 1.0e-18 ! kg/m2/s
  ! Tolerance number to filter non-physical rain values

INTEGER :: i, l ! counters for loops

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "INFERNO_IO"

! End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-------------------------------------------------
! Initialisation
!-------------------------------------------------
! Driving variables
inferno_temp(:) = 0.0
inferno_rhum(:) = 0.0
inferno_sm(:)   = 0.0
inferno_rain(:) = 0.0
inferno_fuel(:) = 0.0
! Work variables
qsat(:)         = 0.0
lai_bal_inf(:,:)= 0.0
leaf_inf(:,:)   = 0.0
wood_inf(:,:)   = 0.0
ignitions(:)    = 0.0

! INFERNO diagnostic variables
fire_vars%flammability_ft(:,:)  = 0.0
fire_vars%burnt_area(:)         = 0.0
fire_vars%burnt_area_ft(:,:)    = 0.0
fire_vars%emitted_carbon(:)     = 0.0
fire_vars%emitted_carbon_ft(:,:)= 0.0
fire_vars%emitted_carbon_DPM(:) = 0.0
fire_vars%emitted_carbon_RPM(:) = 0.0
fire_vars%fire_em_CO2(:)        = 0.0
fire_vars%fire_em_CO2_ft(:,:)   = 0.0
fire_vars%fire_em_CO2_DPM(:)    = 0.0
fire_vars%fire_em_CO2_RPM(:)    = 0.0
fire_vars%fire_em_CO(:)         = 0.0
fire_vars%fire_em_CO_ft(:,:)    = 0.0
fire_vars%fire_em_CO_DPM(:)     = 0.0
fire_vars%fire_em_CO_RPM(:)     = 0.0
fire_vars%fire_em_CH4(:)        = 0.0
fire_vars%fire_em_CH4_ft(:,:)   = 0.0
fire_vars%fire_em_CH4_DPM(:)    = 0.0
fire_vars%fire_em_CH4_RPM(:)    = 0.0
fire_vars%fire_em_NOx(:)        = 0.0
fire_vars%fire_em_NOx_ft(:,:)   = 0.0
fire_vars%fire_em_NOx_DPM(:)    = 0.0
fire_vars%fire_em_NOx_RPM(:)    = 0.0
fire_vars%fire_em_SO2(:)        = 0.0
fire_vars%fire_em_SO2_ft(:,:)   = 0.0
fire_vars%fire_em_SO2_DPM(:)    = 0.0
fire_vars%fire_em_SO2_RPM(:)    = 0.0
fire_vars%fire_em_OC(:)         = 0.0
fire_vars%fire_em_OC_ft(:,:)    = 0.0
fire_vars%fire_em_OC_DPM(:)     = 0.0
fire_vars%fire_em_OC_RPM(:)     = 0.0
fire_vars%fire_em_BC(:)         = 0.0
fire_vars%fire_em_BC_ft(:,:)    = 0.0
fire_vars%fire_em_BC_DPM(:)     = 0.0
fire_vars%fire_em_BC_RPM(:)     = 0.0
fire_vars%fire_em_C2H4(:)       = 0.0
fire_vars%fire_em_C2H4_ft(:,:)  = 0.0
fire_vars%fire_em_C2H4_DPM(:)   = 0.0
fire_vars%fire_em_C2H4_RPM(:)   = 0.0
fire_vars%fire_em_C2H6(:)       = 0.0
fire_vars%fire_em_C2H6_ft(:,:)  = 0.0
fire_vars%fire_em_C2H6_DPM(:)   = 0.0
fire_vars%fire_em_C2H6_RPM(:)   = 0.0
fire_vars%fire_em_C3H8(:)       = 0.0
fire_vars%fire_em_C3H8_ft(:,:)  = 0.0
fire_vars%fire_em_C3H8_DPM(:)   = 0.0
fire_vars%fire_em_C3H8_RPM(:)   = 0.0
fire_vars%fire_em_HCHO(:)       = 0.0
fire_vars%fire_em_HCHO_ft(:,:)  = 0.0
fire_vars%fire_em_HCHO_DPM(:)   = 0.0
fire_vars%fire_em_HCHO_RPM(:)   = 0.0
fire_vars%fire_em_MeCHO(:)       = 0.0
fire_vars%fire_em_MeCHO_ft(:,:)  = 0.0
fire_vars%fire_em_MeCHO_DPM(:)   = 0.0
fire_vars%fire_em_MeCHO_RPM(:)   = 0.0
fire_vars%fire_em_NH3(:)        = 0.0
fire_vars%fire_em_NH3_ft(:,:)   = 0.0
fire_vars%fire_em_NH3_DPM(:)    = 0.0
fire_vars%fire_em_NH3_RPM(:)    = 0.0
fire_vars%fire_em_DMS(:)        = 0.0
fire_vars%fire_em_DMS_ft(:,:)   = 0.0
fire_vars%fire_em_DMS_DPM(:)    = 0.0
fire_vars%fire_em_DMS_RPM(:)    = 0.0

!---------------------------------------------------------------
! Get the available DPM and RPM using a scaling parameter
!---------------------------------------------------------------
dpm_fuel(:) = pmtofuel * c_soil_dpm_gb(:)
rpm_fuel(:) = pmtofuel * c_soil_rpm_gb(:)

!---------------------------------------------------------------
! Get the inferno meteorological variables for the whole gridbox
!---------------------------------------------------------------

! Soil Humidity (inferno_sm)
inferno_sm(:)  = (sthu_soilt(:,1,1))

! Rainfall (inferno_rain)

! Rain fall values have a significant impact in the calculation
! of flamability. In some cases we may be presented with values
! that have no significant meaning - e.g in the UM context negative
! values or very small values can often be found/
DO l = 1, land_pts
  IF ( ls_rain(l) < rain_tolerance ) THEN
    ls_rain_filtered(l)  = 0.0
  ELSE
    ls_rain_filtered(l) = ls_rain(l)
  END IF

  IF ( con_rain(l) < rain_tolerance ) THEN
    con_rain_filtered(l) = 0.0
  ELSE
    con_rain_filtered(l) = con_rain(l)
  END IF
END DO

inferno_rain(:)  = ls_rain_filtered(:) + con_rain_filtered(:)

!---------------------------------------------------------------
! Diagnose the balanced-growth leaf area index and the carbon
! contents of leaves and wood.
!---------------------------------------------------------------
DO i = 1, npft
  DO l = 1, land_pts
    CALL calc_c_comps_triffid(i, canht(l,i), lai_bal_inf(l,i), leaf_inf(l,i),  &
                              c_root, wood_inf(l,i), c_veg)
  END DO
END DO

!---------------------------------------------------------------
! Fire calculations - per PFT
!---------------------------------------------------------------
DO i = 1, npft
  ! Calculate the fuel density
  ! We use normalised Leaf Carbon + the available DPM
  !inferno_fuel(:) = (leaf_inf(:,i) + dpm_fuel - flam_fuel_low)                 &
  !                  /(flam_fuel_up - flam_fuel_low)
  inferno_fuel(:) = (leaf_inf(:,i) + dpm_fuel - 0.02)                          &
                    /(0.2 - 0.02)
  WHERE (inferno_fuel < 0.0) inferno_fuel = 0.0

  WHERE (inferno_fuel > 1.0) inferno_fuel = 1.0

  inferno_temp(:) = t1p5m_tile(:,i)

  DO l = 1, land_pts
    !------------------------------------------------------------
    ! Conditional statements to make sure we are dealing with
    ! reasonable weather. Note initialisation to 0 already done.
    ! If the driving variables are singularities, we assume
    ! no burnt area.
    !------------------------------------------------------------

    ! Temperatures constrained akin to qsat (from the WMO)
    IF ((inferno_temp(l)>338.15) .OR. (inferno_temp(l)<183.15)) CYCLE

    ! The maximum rain rate ever observed is 38mm in one minute,
    ! here we assume 0.5mm/s stops fires altogether
    IF ((inferno_rain(l)>0.5)   .OR. (inferno_rain(l)<0.0   )) CYCLE

    ! Fuel Density is an index constrained to 0-1
    IF ((inferno_fuel(l)>1.0)   .OR. (inferno_fuel(l)<0.0   )) CYCLE

    ! Soil moisture is a fraction of saturation
    IF ((inferno_sm(l)  >1.0)   .OR. (inferno_sm(l)  <0.0   )) CYCLE

    ! Get the tile relative humidity using saturation routine
    CALL qsat_wat(qsat(l), inferno_temp(l), pstar(l))

    inferno_rhum(l)  = q1p5m_tile(l,i) / qsat(l) * 100.0

    ! Relative Humidity should be constrained to 0-100
    IF ((inferno_rhum(l)>100.0) .OR. (inferno_rhum(l)<0.0   )) CYCLE

    ! If all these checks are passes, start fire calculations
    CALL calc_ignitions(                                                       &
      !Point intent(IN)
      fire_vars%pop_den(l), fire_vars%flash_rate(l),                           &
      fire_vars%wealth_index(l), ignition_method,                              &
      !Point intent(OUT)
      ignitions(l))

    CALL calc_flam(                                                            &
      !Point Intent(IN)
      inferno_temp(l), inferno_rhum(l), inferno_fuel(l),                       &
      inferno_sm(l), inferno_rain(l),                                          &
      flam_rhum_low, flam_rhum_up, flam_sm_low, flam_sm_up,                    &
      flam_rain_const, flam_sm_func,                                           &
      !Point Intent(INOUT)
      fire_vars%flammability_ft(l,i))

    CALL calc_burnt_area(                                                      &
      !Point INTENT(IN)
      fire_vars%flammability_ft(l,i), ignitions(l), avg_ba(i),                 &
      !Point INTENT(OUT)
      fire_vars%burnt_area_ft(l,i))
  END DO

  CALL calc_emitted_carbon(                                                    &
    !Array INTENT(IN)
    land_pts, fire_vars%burnt_area_ft(:,i), inferno_sm(:),                     &
    leaf_inf(:,i), wood_inf(:,i),                                              &
    ccleaf_min(i), ccleaf_max(i),                                              &
    ccwood_min(i), ccwood_max(i),                                              &
    !Array INTENT(OUT)
    fire_vars%emitted_carbon_ft(:,i))

  CALL calc_emission(                                                          &
    !Array INTENT(IN)
    land_pts, fire_vars%emitted_carbon_ft(:,i),                                &
    fef_co2(i), fef_co(i), fef_ch4(i), fef_nox(i), fef_so2(i),                 &
    fef_oc(i), fef_bc(i), fef_c2h4(i), fef_c2h6(i), fef_c3h8(i),               &
    fef_hcho(i), fef_mecho(i),                                                 &
    fef_nh3(i), fef_dms(i),                                                    &
    !Array INTENT(OUT)
    fire_vars%fire_em_CO2_ft(:,i), fire_vars%fire_em_CO_ft(:,i),               &
    fire_vars%fire_em_CH4_ft(:,i), fire_vars%fire_em_NOx_ft(:,i),              &
    fire_vars%fire_em_SO2_ft(:,i),                                             &
    fire_vars%fire_em_OC_ft(:,i), fire_vars%fire_em_BC_ft(:,i),                &
    fire_vars%fire_em_C2H4_ft(:,i), fire_vars%fire_em_C2H6_ft(:,i),            &
    fire_vars%fire_em_C3H8_ft(:,i), fire_vars%fire_em_HCHO_ft(:,i),            &
    fire_vars%fire_em_MeCHO_ft(:,i),                                           &
    fire_vars%fire_em_NH3_ft(:,i), fire_vars%fire_em_DMS_ft(:,i) )

  ! We add pft-specific variables to the gridbox totals
  fire_vars%burnt_area(:)     = fire_vars%burnt_area(:)                        &
                      + frac(:,i) * fire_vars%burnt_area_ft(:,i)
  fire_vars%emitted_carbon(:) = fire_vars%emitted_carbon(:)                    &
                      + frac(:,i) * fire_vars%emitted_carbon_ft(:,i)
  fire_vars%fire_em_CO2(:)    = fire_vars%fire_em_CO2(:)                       &
                      + frac(:,i) * fire_vars%fire_em_CO2_ft(:,i)
  fire_vars%fire_em_CO(:)     = fire_vars%fire_em_CO(:)                        &
                      + frac(:,i) * fire_vars%fire_em_CO_ft(:,i)
  fire_vars%fire_em_CH4(:)    = fire_vars%fire_em_CH4(:)                       &
                      + frac(:,i) * fire_vars%fire_em_CH4_ft(:,i)
  fire_vars%fire_em_NOx(:)    = fire_vars%fire_em_NOx(:)                       &
                      + frac(:,i) * fire_vars%fire_em_NOx_ft(:,i)
  fire_vars%fire_em_SO2(:)    = fire_vars%fire_em_SO2(:)                       &
                      + frac(:,i) * fire_vars%fire_em_SO2_ft(:,i)
  fire_vars%fire_em_OC(:)     = fire_vars%fire_em_OC(:)                        &
                      + frac(:,i) * fire_vars%fire_em_OC_ft(:,i)
  fire_vars%fire_em_BC(:)     = fire_vars%fire_em_BC(:)                        &
                      + frac(:,i) * fire_vars%fire_em_BC_ft(:,i)
  fire_vars%fire_em_C2H4(:)   = fire_vars%fire_em_C2H4(:)                      &
                      + frac(:,i) * fire_vars%fire_em_C2H4_ft(:,i)
  fire_vars%fire_em_C2H6(:)   = fire_vars%fire_em_C2H6(:)                      &
                      + frac(:,i) * fire_vars%fire_em_C2H6_ft(:,i)
  fire_vars%fire_em_C3H8(:)   = fire_vars%fire_em_C3H8(:)                      &
                      + frac(:,i) * fire_vars%fire_em_C3H8_ft(:,i)
  fire_vars%fire_em_HCHO(:)   = fire_vars%fire_em_HCHO(:)                      &
                      + frac(:,i) * fire_vars%fire_em_HCHO_ft(:,i)
  fire_vars%fire_em_MeCHO(:)  = fire_vars%fire_em_MeCHO(:)                     &
                      + frac(:,i) * fire_vars%fire_em_MeCHO_ft(:,i)
  fire_vars%fire_em_NH3(:)    = fire_vars%fire_em_NH3(:)                       &
                      + frac(:,i) * fire_vars%fire_em_NH3_ft(:,i)
  fire_vars%fire_em_DMS(:)    = fire_vars%fire_em_DMS(:)                       &
                      + frac(:,i) * fire_vars%fire_em_DMS_ft(:,i)

END DO

DO i = 1, npft
  DO l = 1, land_pts

    ! Convert burnt_area into disturbance rate and accumulate to TRIFFID timestep
    ! Accumulate to TRIFFID timestep
    IF (l_trif_fire) THEN
      ! Reset accumulation on first step after TRIFFID
      IF (asteps_since_triffid == 1) g_burn_pft_acc(l,i) = 0.0
      g_burn_pft_acc(l,i) = g_burn_pft_acc(l,i)                                &
                            + (fire_vars%burnt_area_ft(l,i) * timestep) * fire_mort(i)

    END IF

  END DO
END DO

!---------------------------------------------------------------
! In addition we diagnose the soil carbon (DPM and RPM only).
! However, this is not added to the gridbox totals as it was
! observed to lead to unrealistic emission with the currently
! recommended configuration.
!---------------------------------------------------------------

CALL calc_emitted_carbon_soil(                                                 &
  ! Array INTENT(IN)
  land_pts, fire_vars%burnt_area, dpm_fuel, rpm_fuel,                          &
  inferno_sm,                                                                  &
  ! Array INTENT(OUT)
  fire_vars%emitted_carbon_DPM, fire_vars%emitted_carbon_RPM )

! Decomposable Plant Material
CALL calc_emission(                                                            &
  !Array INTENT(IN)
  land_pts, fire_vars%emitted_carbon_DPM(:),                                   &
  fef_co2_dpm, fef_co_dpm, fef_ch4_dpm,                                        &
  fef_nox_dpm, fef_so2_dpm, fef_oc_dpm, fef_bc_dpm,                            &
  fef_c2h4_dpm, fef_c2h6_dpm, fef_c3h8_dpm,                                    &
  fef_hcho_dpm, fef_mecho_dpm,                                                 &
  fef_nh3_dpm, fef_dms_dpm,                                                    &
  !Array INTENT(OUT)
  fire_vars%fire_em_CO2_DPM(:), fire_vars%fire_em_CO_DPM(:),                   &
  fire_vars%fire_em_CH4_DPM(:), fire_vars%fire_em_NOx_DPM(:),                  &
  fire_vars%fire_em_SO2_DPM(:), fire_vars%fire_em_OC_DPM(:),                   &
  fire_vars%fire_em_BC_DPM(:), fire_vars%fire_em_C2H4_DPM(:),                  &
  fire_vars%fire_em_C2H6_DPM(:), fire_vars%fire_em_C3H8_DPM(:),                &
  fire_vars%fire_em_HCHO_DPM(:), fire_vars%fire_em_MeCHO_DPM(:),               &
  fire_vars%fire_em_NH3_DPM(:), fire_vars%fire_em_DMS_DPM(:))

! Resistant Plant Material
CALL calc_emission(                                                            &
  !Array INTENT(IN)
  land_pts, fire_vars%emitted_carbon_RPM(:),                                   &
  fef_co2_rpm, fef_co_rpm, fef_ch4_rpm,                                        &
  fef_nox_rpm, fef_so2_rpm, fef_oc_rpm, fef_bc_rpm,                            &
  fef_c2h4_rpm, fef_c2h6_rpm, fef_c3h8_rpm,                                    &
  fef_hcho_rpm, fef_mecho_rpm,                                                 &
  fef_nh3_rpm, fef_dms_rpm,                                                    &
  !Array INTENT(OUT)
  fire_vars%fire_em_CO2_RPM(:), fire_vars%fire_em_CO_RPM(:),                   &
  fire_vars%fire_em_CH4_RPM(:), fire_vars%fire_em_NOx_RPM(:),                  &
  fire_vars%fire_em_SO2_RPM(:), fire_vars%fire_em_OC_RPM(:),                   &
  fire_vars%fire_em_BC_RPM(:), fire_vars%fire_em_C2H4_RPM(:),                  &
  fire_vars%fire_em_C2H6_RPM(:), fire_vars%fire_em_C3H8_RPM(:),                &
  fire_vars%fire_em_HCHO_RPM(:), fire_vars%fire_em_MeCHO_RPM(:),               &
  fire_vars%fire_em_NH3_RPM(:), fire_vars%fire_em_DMS_RPM(:))

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE inferno_io
END MODULE inferno_io_mod
