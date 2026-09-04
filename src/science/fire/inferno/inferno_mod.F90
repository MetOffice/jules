! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

! Code Description:
!   Language: FORTRAN 90
!
! Code Owner: Please refer to ModuleLeaders.txt
!

MODULE inferno_mod

USE conversions_mod, ONLY: s_in_day => rsec_per_day
USE parkind1,                       ONLY: jpim

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE
!
! Description:
!  Contains the computations needed by INFERNO (INteractive Fire
!  and Emissions algoRithm in Natural envirOnments).
!  Lightning and Population Density can be prescribed to influence
!  the number of ingnitions. The rest of the diagnostics depend
!  on weather (temperature, relatie humidity, precipitation)
!  and vegetation (available biomass).
!  The outputs are burnt area and emissions of key Species
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!    Language: Fortran 90.

PRIVATE ! Default everything as being private, unlock as needed

PUBLIC   ::  calc_ignitions, calc_flam, calc_burnt_area,                       &
             calc_emitted_carbon, calc_emitted_carbon_soil,                    &
             calc_emission, calc_soil_carbon_pools

REAL(KIND=real_jlslsm),    PARAMETER        ::                                 &
  s_in_month = 2.6280288e6,                                                    &
    ! Seconds in a month.
    ! Note that this is approx. 365 days/12months but is slightly larger.
    ! This should be changed in a future update.
  m2_in_km2  = 1.0e6

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
CHARACTER(LEN=*),  PARAMETER, PRIVATE :: ModuleName = "INFERNO_MOD"

CONTAINS

! Note: calc_ignitions, calc_flam and calc_burnt_area) are
! computed for each landpoint to ascertain no points with
! unrealistic weather contain fires.
! These are then aggregated in inferno_io_mod into pft arrays.

SUBROUTINE calc_ignitions(                                                     &
  ! Point intent(IN)
  pop_den_l, flash_rate_l, wealth_index_l, ignition_method,                    &
  ! Point intent(OUT)
  ignitions_l)

! Description:
!     Calculate the number of ignitions/m2/s at each gridpoint
!
! Method:
!     See original paper by Pechony and Shindell (2009),
!     originally proposed for monthly totals, here per timestep.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
!  Code Description:
!    Language:  Fortran 90

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb
USE jules_vegetation_mod, ONLY: ignition_constant, ignition_vary_natural,      &
                                ignition_vary_natural_human

IMPLICIT NONE

INTEGER,    INTENT(IN)    ::                                                   &
  ignition_method
    ! The integer defining the method used for ignitions:
    ! 1 = constant,
    ! 2 = constant (Anthropogenic) + Varying (lightning),
    ! 3 = Varying  (Anthropogenic and lightning)

REAL(KIND=real_jlslsm),       INTENT(IN)    ::                                 &
  flash_rate_l,                                                                &
    ! The Cloud to Ground lightning flash rate (flashes/km2)
  pop_den_l,                                                                   &
    ! The population density (ppl/km2)
  wealth_index_l
  ! The Human Development Index (1)

REAL(KIND=real_jlslsm),    INTENT(OUT)      ::                                 &
  ignitions_l
    ! The number of ignitions/m2/s

REAL(KIND=real_jlslsm)                      ::                                 &
  man_ign_l,                                                                   &
    ! Human-induced fire ignition rate (ignitions/km2/s)
  nat_ign_l,                                                                   &
    ! Lightning natural ignition rate (number/km2/sec)
  non_sup_frac_l
    ! Fraction of fire ignition non suppressed by humans

REAL(KIND=real_jlslsm),    PARAMETER        ::                                 &
  tune_MODIS = 7.7
    ! Parameter originally used by P&S (2009) to match MODIS

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_IGNITIONS"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF (ignition_method == ignition_constant) THEN
  nat_ign_l   = 2.7 / s_in_month / m2_in_km2 / 12.0 * 0.75
    ! Assume a multi-year annual mean of 2.7/km2/yr
    ! (Huntrieser et al. 2007) 75% are Cloud to Ground flashes
    ! (Prentice and Mackerras 1977)

  man_ign_l   = 1.5 / s_in_month / m2_in_km2
    ! We parameterised 1.5 ignitions/km2/month globally from GFED

  ignitions_l = (man_ign_l + nat_ign_l)

ELSE IF (ignition_method == ignition_vary_natural) THEN
  nat_ign_l   = MIN(MAX(flash_rate_l / m2_in_km2 / s_in_day,0.0),1.0)
    ! Flash Rate (Cloud to Ground) always lead to one fire

  man_ign_l   = 1.5 / s_in_month / m2_in_km2
    ! We parameterised 1.5 ignitions/km2/month globally from GFED

  ignitions_l = (man_ign_l + nat_ign_l)

ELSE IF (ignition_method == ignition_vary_natural_human) THEN
  nat_ign_l   = flash_rate_l / m2_in_km2 / s_in_day
    ! Flash Rate (Cloud to Ground) always lead to one fire

  man_ign_l   =  0.2 * pop_den_l**(0.4) * (1 - wealth_index_l)
  man_ign_l   = man_ign_l / (m2_in_km2 * s_in_month)
    ! Convert to the appropriate units required by INFERNO

  non_sup_frac_l =  0.05 + 0.9 * EXP(-0.05 * pop_den_l) * (1 - wealth_index_l)

  ignitions_l =  (nat_ign_l + man_ign_l) * non_sup_frac_l

  ! Tune ignitions to MODIS data (see Pechony and Shindell, 2009)
  ignitions_l =  ignitions_l * tune_MODIS
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE  calc_ignitions

SUBROUTINE calc_flam(                                                          &
  !Point Intent(IN)
  temp_l, rhum_l, fuel_l, sm_l, rain_l,                                        &
  flam_rhum_low, flam_rhum_up, flam_sm_low, flam_sm_up,                        &
  flam_rain_const, flam_sm_func,                                               &
  !Point Intent(INOUT)
  flam_l)

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE
!
! Description:
!   Performs the calculation of the flammibility
!
! Method:
!   In essence, utilizes weather and vegetation variables to
!   estimate how flammable a m2 is every second.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90

! Subroutine arguments

REAL(KIND=real_jlslsm) ,   INTENT(IN)       ::                                 &
  temp_l,                                                                      &
    ! Surface Air Temperature (K)
  rhum_l,                                                                      &
    ! Relative Humidity (%)
  sm_l,                                                                        &
    ! The INFERNO soil moisture fraction (sthu's 1st level)
  rain_l,                                                                      &
    ! The rain rate (kg.m-2.s-1)
  fuel_l,                                                                      &
    ! The Fuel Density (0-1)
  flam_rhum_up,                                                                &
    ! Upper boundary to the relative humidity
  flam_rhum_low,                                                               &
    ! Lower boundary to the relative humidity
  flam_sm_low,                                                                 &
    ! Below this soil moisture, flammability is 1.0
    ! Expressed as a fraction of saturation (between 0 and 1) (flam_sm_func=2)
  flam_sm_up,                                                                  &
    ! Exponential decay parameter for relationship between soil moisture
    !    and flammability (> 0.0) (flam_sm_func=2)
  flam_rain_const
    !

INTEGER, INTENT(IN)   ::                                                       &
   flam_sm_func
    ! The function used to parameterise the
    !      relationship between soil moisture and flammability
    ! 1 = linear,
    ! 2 = exponential

REAL(KIND=real_jlslsm),    INTENT(IN OUT)    ::                                &
  flam_l
    ! The flammability of the cell


! These are variables to the Goff-Gratch equation
REAL(KIND=real_jlslsm),    PARAMETER        ::                                 &
  a=-7.90298,                                                                  &
  d = 11.344,                                                                  &
  c=-1.3816e-07,                                                               &
  b = 5.02808,                                                                 &
  f = 8.1328e-03,                                                              &
  h=-3.49149,                                                                  &
  Ts = 373.16,                                                                 &
    ! Water saturation temperature
  cr=-2.0 * s_in_day
    ! Precipitation factor (-2(day/mm)*(kg/m2/s))

REAL(KIND=real_jlslsm)                      ::                                 &
  Z_l,                                                                         &
    ! Component of the Goff-Gratch saturation vapor pressure
  TsbyT_l,                                                                     &
    ! Reciprocal of the temperature times ts
  f_rhum_l,                                                                    &
    ! The factor dependence on relative humidity
  f_sm_l,                                                                      &
    ! The factor dependence on soil moisture
  rain_rate

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_FLAM"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

TsbyT_l   =  Ts / temp_l

Z_l       =  a * (TsbyT_l-1.0) + b * LOG10(TsbyT_l)                            &
           + c * (10.0**( d * (1.0 - TsbyT_l)) - 1.0)                          &
           + f * (10.0**( h * (TsbyT_l-1.0)) - 1.0)

f_rhum_l  = (flam_rhum_up - rhum_l) / (flam_rhum_up - flam_rhum_low)

! Create boundary limits
! First for relative humidity
IF (rhum_l < flam_rhum_low) f_rhum_l = 1.0
  ! Always fires for RH < flam_rhum_low
IF (rhum_l > flam_rhum_up)  f_rhum_l = 0.0
  ! No fires for RH > flam_rhum_up

IF ( flam_sm_func == 1 ) THEN   ! linear
  f_sm_l    = (1 - sm_l)
ELSE IF ( flam_sm_func == 2 ) THEN   ! exponential
  f_sm_l = EXP(-flam_sm_up * (sm_l - flam_sm_low) )
  f_sm_l = MAX(MIN(f_sm_l, 1.0), 0.0)
END IF


rain_rate = rain_l * s_in_day
  ! convert rain rate from kg/m2/s to mm/day

flam_l    = MAX(MIN(10.0**Z_l * f_rhum_l * fuel_l * f_sm_l                   &
                       * EXP( cr * rain_rate) ,1.0) ,0.0)
!flam_l    = MAX(MIN(10.0**Z_l * f_rhum_l * fuel_l * f_sm_l                   &
!                       * EXP( cr * s_in_day * rain_l), 1.0), 0.0)
! EJB add units here for rain or add rain as a separate function - make flam_rain > 0


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_flam

SUBROUTINE calc_burnt_area(                                                    &
  ! Point INTENT(IN)
  flam_l, ignitions_l, avg_ba_i,                                               &
  ! Point INTENT(OUT)
  burnt_area_i_l)

!
! Description:
!    Calculate the burnt area
!
! Method:
!    Multiply ignitions by flammability by average PFT burnt area
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

REAL(KIND=real_jlslsm)  ,    INTENT(IN)     ::                                 &
  flam_l,                                                                      &
    ! Flammability (depends on weather and vegetation)
  ignitions_l,                                                                 &
    ! Fire ignitions (ignitions/m2/s)
  avg_ba_i
    ! The average burned area (m2) for this PFT

REAL(KIND=real_jlslsm)  ,    INTENT(OUT)    ::                                 &
  burnt_area_i_l
    ! The burnt area (fraction of PFT per s)

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_BURNT_AREA"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

burnt_area_i_l = flam_l * ignitions_l * avg_ba_i

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_burnt_area

SUBROUTINE calc_emitted_carbon(                                                &
  ! Array INTENT(IN)
  land_pts, burnt_area_i, sm, leaf_i, wood_i,                                  &
  ccleaf_min_i, ccleaf_max_i, ccwood_min_i, ccwood_max_i,                      &
  ! Array INTENT(OUT)
  emitted_carbon_i )

!
! Description:
!   Calculate the total emitted carbon from burnt area
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90
!

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

INTEGER                   :: land_pts

REAL(KIND=real_jlslsm) ,   INTENT(IN)       ::                                 &
  burnt_area_i(land_pts),                                                      &
    ! PFT Burnt Area (in frac of PFT s-1)
  sm(land_pts),                                                                &
    ! The INFERNO soil moisture (1st level sthu)
  leaf_i(land_pts),                                                            &
    ! The PFT leaf carbon
  wood_i(land_pts),                                                            &
    ! The PFT wood carbon
  ccleaf_min_i,                                                                &
    ! Leaf min combustion completeness
  ccleaf_max_i,                                                                &
    ! Leaf max combustion completeness
  ccwood_min_i,                                                                &
    ! Wood min combustion completeness
  ccwood_max_i
    ! Wood max combustion completeness

REAL(KIND=real_jlslsm) ,   INTENT(OUT)      ::                                 &
  emitted_carbon_i(land_pts)
    ! The PFT emitted carbon (kgC.m-2.s-1)

INTEGER                   :: l      ! landpoints loop counter

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_EMITTED_CARBON"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise to no emitted carbon
emitted_carbon_i(:) = 0.0
DO l = 1, land_pts
  emitted_carbon_i(l) = MAX(burnt_area_i(l) * ( leaf_i(l) * (ccleaf_min_i +    &
                             (ccleaf_max_i - ccleaf_min_i) * (1.0 - sm(l))) +  &
                               wood_i(l) * (ccwood_min_i + (ccwood_max_i -     &
                               ccwood_min_i) * (1.0 - sm(l)))), 0.0 )

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_emitted_carbon

SUBROUTINE calc_emitted_carbon_soil(                                           &
  ! Array INTENT(IN)
  land_pts, burnt_area, dpm_fuel, rpm_fuel, sm,                                &
  ! Array INTENT(OUT)
  emitted_carbon_DPM, emitted_carbon_RPM )

!
! Description:
!   Calculate the total emitted carbon from burnt area for soil
!   carbon pools (DPM and RPM)

! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90
!

USE jules_inferno_mod, ONLY: ccdpm_min, ccdpm_max,                             &
                ccrpm_min, ccrpm_max

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

INTEGER ,   INTENT(IN)    :: land_pts

REAL(KIND=real_jlslsm) ,   INTENT(IN)       ::                                 &
  burnt_area(land_pts),                                                        &
    ! Gridbox mean burnt area fraction (s-1)
  dpm_fuel(land_pts),                                                          &
    ! Carbon in DPM available (kgC.m-2)
  rpm_fuel(land_pts),                                                          &
    ! Carbon in RPM available (kgC.m-2)
  sm(land_pts)
    ! The soil moisture fraction (sthu at 1st level - sm_wil)

REAL(KIND=real_jlslsm) ,   INTENT(OUT)      ::                                 &
  emitted_carbon_DPM(land_pts),                                                &
    ! The DPM emitted carbon (kg.m-2.s-1)
  emitted_carbon_RPM(land_pts)
    ! The RPM emitted carbon (kg.m-2.s-1)

INTEGER                   :: l      ! landpoint loop counter

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_EMITTED_CARBON_SOIL"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO l = 1,land_pts
  emitted_carbon_DPM(l) = MAX(burnt_area(l) * ( dpm_fuel(l) * (ccdpm_min +     &
                              (ccdpm_max - ccdpm_min) * (1.0 - sm(l)))) ,0.0)
  emitted_carbon_RPM(l) = MAX(burnt_area(l) * ( rpm_fuel(l) * (ccrpm_min +     &
                          (ccrpm_max - ccrpm_min) * (1.0 - sm(l)))) ,0.0)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_emitted_carbon_soil

SUBROUTINE calc_emission(                                                      &
  ! Array INTENT(IN)
  land_pts, emitted_carbon_i,                                                  &
  fef_co2_i, fef_co_i, fef_ch4_i, fef_nox_i, fef_so2_i,                        &
  fef_oc_i, fef_bc_i,                                                          &
  fef_c2h4_i, fef_c2h6_i, fef_c3h8_i, fef_hcho_i,                              &
  fef_mecho_i, fef_nh3_i, fef_dms_i,                                           &
  ! Array INTENT(OUT)
  emission_CO2, emission_CO, emission_CH4,                                     &
  emission_NOx, emission_SO2,                                                  &
  emission_OC, emission_BC,                                                    &
  emission_C2H4, emission_C2H6, emission_C3H8,                                 &
  emission_HCHO, emission_MeCHO,                                               &
  emission_NH3, emission_DMS) ! Add more as you see fit...

!
! Description:
!  Calculate the emission of each compound from the obtained emitted carbon
!  Uses a look-up table with PFT-emission factor (see Li et al., 2012)
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90
!

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

INTEGER, INTENT(IN)       :: land_pts

REAL(KIND=real_jlslsm) ,   INTENT(IN)       ::                                 &
  emitted_carbon_i(land_pts),                                                  &
    ! PFT (or Soil litter pool) emitted carbon (in kgC.m-2.s-1)
  fef_co2_i,                                                                   &
    ! PFT CO2 emission factor
  fef_co_i,                                                                    &
    ! PFT CO emission factor
  fef_ch4_i,                                                                   &
    ! PFT CH4 emission factor
  fef_nox_i,                                                                   &
    ! PFT NOx emission factor
  fef_so2_i,                                                                   &
    ! PFT SO2 emission factor
  fef_oc_i,                                                                    &
    ! PFT OC emission factor
  fef_bc_i,                                                                    &
    ! PFT BC emission factor
  fef_c2h4_i,                                                                  &
    ! PFT C2H4 emission factor
  fef_c2h6_i,                                                                  &
    ! PFT C2H6 emission factor
  fef_c3h8_i,                                                                  &
    ! PFT C3H8 emission factor
  fef_hcho_i,                                                                  &
    ! PFT HCHO emission factor
  fef_mecho_i,                                                                 &
    ! PFT MeCHO emission factor
  fef_nh3_i,                                                                   &
    ! PFT NH3 emission factor
  fef_dms_i
    ! PFT DMS emission factor

REAL(KIND=real_jlslsm) ,   INTENT(OUT)    ::                                   &
  emission_CO2(land_pts),                                                      &
    ! The emission of CO2 (in kg.m-2.s-1)
  emission_CO(land_pts),                                                       &
    ! The emission of CO  (in kg.m-2.s-1)
  emission_CH4(land_pts),                                                      &
    ! The emission of CH4 (in kg.m-2.s-1)
  emission_NOx(land_pts),                                                      &
    ! The emission of NOx (in kg.m-2.s-1)
  emission_SO2(land_pts),                                                      &
    ! The emission of SO2 (in kg.m-2.s-1)
  emission_OC(land_pts),                                                       &
    ! The emission of OC  (in kg.m-2.s-1) - Organic Carbon
  emission_BC(land_pts),                                                       &
    ! The emission of BC  (in kg.m-2.s-1) - Black Carbon
  emission_C2H4(land_pts),                                                     &
    ! The emission of C2H4  (in kg.m-2.s-1)
  emission_C2H6(land_pts),                                                     &
    ! The emission of C2H6  (in kg.m-2.s-1)
  emission_C3H8(land_pts),                                                     &
    ! The emission of C3H8  (in kg.m-2.s-1)
  emission_HCHO(land_pts),                                                     &
    ! The emission of HCHO  (in kg.m-2.s-1)
  emission_MeCHO(land_pts),                                                    &
    ! The emission of MeCHO  (in kg.m-2.s-1)
  emission_NH3(land_pts),                                                      &
    ! The emission of NH3  (in kg.m-2.s-1)
  emission_DMS(land_pts)
    ! The emission of DMS  (in kg.m-2.s-1)

REAL(KIND=real_jlslsm) ,   PARAMETER        ::                                 &
  ctob = 2.0,                                                                  &
    ! Conversion from Carbon to Biomass (assume 50% of biomass is C)
  gtokg = 1.0e-03
    ! To convert the emission factors in kg kg-1

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_EMISSION"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise the output fields
emission_CO2(:)  = 0.0
emission_CO(:)   = 0.0
emission_CH4(:)  = 0.0
emission_NOx(:)  = 0.0
emission_SO2(:)  = 0.0
emission_OC(:)   = 0.0
emission_BC(:)   = 0.0
emission_C2H4(:) = 0.0
emission_C2H6(:) = 0.0
emission_C3H8(:) = 0.0
emission_HCHO(:) = 0.0
emission_MeCHO(:)= 0.0
emission_NH3(:)  = 0.0
emission_DMS(:)  = 0.0

! Resistant Plant Material
emission_CO2(:)  = ctob * emitted_carbon_i(:) * fef_co2_i   * gtokg
emission_CH4(:)  = ctob * emitted_carbon_i(:) * fef_ch4_i   * gtokg
emission_CO(:)   = ctob * emitted_carbon_i(:) * fef_co_i    * gtokg
emission_NOx(:)  = ctob * emitted_carbon_i(:) * fef_nox_i   * gtokg
emission_SO2(:)  = ctob * emitted_carbon_i(:) * fef_so2_i   * gtokg
emission_OC(:)   = ctob * emitted_carbon_i(:) * fef_oc_i    * gtokg
emission_BC(:)   = ctob * emitted_carbon_i(:) * fef_bc_i    * gtokg
emission_C2H4(:) = ctob * emitted_carbon_i(:) * fef_c2h4_i  * gtokg
emission_C2H6(:) = ctob * emitted_carbon_i(:) * fef_c2h6_i  * gtokg
emission_C3H8(:) = ctob * emitted_carbon_i(:) * fef_c3h8_i  * gtokg
emission_HCHO(:) = ctob * emitted_carbon_i(:) * fef_hcho_i  * gtokg
emission_MeCHO(:)= ctob * emitted_carbon_i(:) * fef_mecho_i * gtokg
emission_NH3(:)  = ctob * emitted_carbon_i(:) * fef_nh3_i   * gtokg
emission_DMS(:)  = ctob * emitted_carbon_i(:) * fef_dms_i   * gtokg

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_emission


SUBROUTINE calc_soil_carbon_pools(land_pts, soil_pts, soil_index, dim_cs1,     &
                                  cs_pool_soilt,                               &
                                  c_soil_dpm_gb, c_soil_rpm_gb)

! calculate the decomposable and resistant soil carbon pools
! these are used as a proxy for litter

USE jules_soil_biogeochem_mod, ONLY: soil_bgc_model, soil_model_4pool,         &
                                     soil_model_1pool, l_layeredc
USE jules_inferno_mod, ONLY: z_burn_max
USE jules_soil_mod, ONLY: dzsoil

USE ancil_info, ONLY: nsoilt, dim_cslayer

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: land_pts,                                               &
                       soil_pts,                                               &
                       soil_index(land_pts),                                   &
                       dim_cs1
                       !Passed by arg because it lives in different modules
                       !in standalone and UM

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    cs_pool_soilt(land_pts,nsoilt,dim_cslayer,dim_cs1)


REAL(KIND=real_jlslsm), INTENT(OUT) :: c_soil_dpm_gb(land_pts),                &
                     c_soil_rpm_gb(land_pts)

!Local variables
REAL(KIND=real_jlslsm) ::                                                      &
  z2,                                                                          &
   ! depth of bottom of soil layer for soil C burnt calculation
  z1,                                                                          &
   ! depth of top of soil layer for soil C burnt calculation
  prop_cs_burnt
   ! proportion of soil carbon burnt

INTEGER :: i,j,m,n !Counters

!End of header

! Note that this code is currently incompatible with soil tiling, meaning we
! hard code the soilt index f cs_pool below, using m = 1
! See comments in INFERNO for more info
m = 1
c_soil_dpm_gb(:) = 0.0
c_soil_rpm_gb(:) = 0.0

! Calculate gridbox total soil C in DPM and RPM pools.
! Note we assume that DPM and RPM are pools 1 and 2 respectively.
IF ( soil_bgc_model == soil_model_4pool ) THEN

  c_soil_dpm_gb(:) = 0.0
  c_soil_rpm_gb(:) = 0.0

  DO j = 1,soil_pts
    i = soil_index(j)
    z2 = 0.0
    DO n=1,dim_cslayer
      IF ( l_layeredc ) THEN
        z1 = z2
        z2 = z2 + dzsoil(n)
        prop_cs_burnt = 0.0
        IF ( z2 <  z_burn_max ) THEN
          prop_cs_burnt = 1.0
        ELSE IF ( z2 >= z_burn_max .AND. z1 <  z_burn_max ) THEN
          prop_cs_burnt = (z_burn_max - z1) / (z2 - z1)
        END IF
      ELSE
        prop_cs_burnt = 1.0
      END IF

      c_soil_dpm_gb(i) = c_soil_dpm_gb(i) + cs_pool_soilt(i,m,n,1) *           &
                                                                  prop_cs_burnt
      c_soil_rpm_gb(i) = c_soil_rpm_gb(i) + cs_pool_soilt(i,m,n,2) *           &
                                                                  prop_cs_burnt
    END DO
  END DO

ELSE IF ( soil_bgc_model == soil_model_1pool ) THEN
  ! With a single soil pool, we estimate the relative amounts of DPM and RPM

  DO j = 1,soil_pts
    i = soil_index(j)
    c_soil_dpm_gb(i) = 0.0
    c_soil_rpm_gb(i) = 0.0

    DO n = 1,dim_cslayer
      c_soil_dpm_gb(i) = c_soil_dpm_gb(i) + 0.01 * cs_pool_soilt(i, m, n, 1)
      c_soil_rpm_gb(i) = c_soil_rpm_gb(i) + 0.2  * cs_pool_soilt(i, m, n, 1)
    END DO
  END DO

END IF

END SUBROUTINE calc_soil_carbon_pools

END MODULE inferno_mod

