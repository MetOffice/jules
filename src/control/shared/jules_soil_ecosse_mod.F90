#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE jules_soil_ecosse_mod

USE ancil_info, ONLY: dim_cslayer

! Import ECOSSE parameters.
USE ecosse_param_mod, ONLY:                                                    &
    bacteria_min_frac, bacteria_max_frac, bacteria_min_frac_pH,                &
    bacteria_max_frac_pH, cn_bacteria, cn_fungi,                               &
    decomp_rate, decomp_wrate_min_smith, decomp_wrate_min_clark,               &
    decomp_temp_coeff_smith, decomp_ph_min, decomp_ph_max,                     &
    decomp_ph_rate_min,                                                        &
    depth_nitrif, nitrif_frac_n2o_fc, nitrif_rate, nitrif_frac_gas,            &
    nitrif_frac_no, nitrif_max_factor, nitrif_wrate_min,                       &
    denit50, denit_frac_n2_fc, denit_nitrate_equal,                            &
    denit_water_coeff, denit_bio_factor,                                       &
    pi_sfc_depth, pi_sfc_frac,                                                 &
    amm_leach_min, n_inorg_max_conc


USE max_dimensions, ONLY:                                                      &
  ! imported scalar parameters
  cs_layer_max

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Contains options and parameters for the ECOSSE soil model, and a namelist
!   for setting them.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Public scope by default.

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Scalar parameters.
!-----------------------------------------------------------------------------
INTEGER, PARAMETER ::                                                          &
  !-------------------------------------------------------------------------
  ! Parameters identifying alternative forms for input of plant litterfall.
  ! These should be >0 and unique.
  !-------------------------------------------------------------------------
  plant_input_roots = 1,                                                       &
    ! Value of plant_input_profile that indicates that the fraction
    ! pi_sfc_frac of inputs are distributed uniformly in a surface layer of
    ! depth pi_sfc_depth, the remainder are distributed according to the
    ! root distribution.
  plant_input_exp = 2,                                                         &
    ! Value of plant_input_profile that indicates that inputs will follow
    ! an exponential profile with depth, with decay constant tau_lit.
  !-------------------------------------------------------------------------
  ! Parameters identifying alternative forms of the rate modifiers.
  ! These should be >0 and unique.
  !-------------------------------------------------------------------------
  temp_mod_q10 = 1,                                                            &
    ! Value of temp_modifier that indicates the Q10 form of the
    ! temperature rate modifier is used.
  temp_mod_smith = 2,                                                          &
    ! Value of temp_modifier that indicates Smith et al. (2010) form of the
    ! temperature rate modifier is used.
  water_mod_clark = 1,                                                         &
    ! Value of water_modifier that indicates Clark et al. (2011) form of the
    ! water rate modifier is used.
  water_mod_smith = 2
    ! Value of water_modifier that indicates Smith et al. (2010) form of the
    ! water rate modifier is used.

!-----------------------------------------------------------------------------
! Items set in namelist jules_soil_ecosse.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Namelist variables that are potentially used by more than one soil model,
! but currently only by ECOSSE.
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_soil_N = .TRUE.
    ! Switch for a prognostic model of soil Nitrogen.
    ! Set this to TRUE so that by default a soil model that is capable of
    ! simulating N will have N enabled.

REAL(KIND=real_jlslsm) :: depo_nit_frac = 1.0
    ! The fraction of total N deposition that is added to soil nitrate.
    ! The complement is added to soil ammonium.
    ! This really describes the driving data rather than the soil model,
    ! but it's declared here for now.

!-----------------------------------------------------------------------------
! Variables that describe the soil layers and timestep.
! At present these only apply to ECOSSE.
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_match_layers = .TRUE.
    ! Switch to match soil C and N layers to soil moisture layers.

REAL(KIND=real_jlslsm) ::                                                      &
  dt_soilc = -1.0
    ! Timestep length for soil biogeochem model (s).

! Variable length arrays that can be set using the namelist.
! We provide a fixed-length version that is read by the namelist, then point
! the actual version to the portion of it we will use.
REAL(KIND=real_jlslsm), POINTER ::                                             &
  dz_soilc(:)
    ! Thicknesses of the soil biogeochem layers (m).
    ! In the case of a single layer, bulk model (dim_cslayer=1), dz_soilc
    ! is interpreted as the representative or averaging depth (e.g. the
    ! temperature of the bulk model is taken as the average over this
    ! depth).
REAL(KIND=real_jlslsm), TARGET ::                                              &
  dz_soilc_io(cs_layer_max)
    ! Fixed length equivalent of dz_soilc for namelist IO.

! Initialise dz_soilc_io to negative values.
! We later verify that values used are > 0.
DATA dz_soilc_io / cs_layer_max * -1.0 /

!-----------------------------------------------------------------------------
! Switches that are read from namelist.
!-----------------------------------------------------------------------------
INTEGER ::                                                                     &
  plant_input_profile = plant_input_roots,                                     &
    ! Switch for distribution of litterfall inputs.
  temp_modifier = temp_mod_smith,                                              &
    ! Switch for form of temperature rate modifier for decomposition.
  water_modifier = water_mod_smith
    ! Switch for form of water rate modifier for decomposition and
    ! nitrification.

LOGICAL ::                                                                     &
  l_decomp_slow = .FALSE.,                                                     &
    ! Switch controlling how decomposition is altered when N is limiting.
    ! TRUE means the decomposition rate is slowed.
    ! FALSE means the efficiency of decomposition is reduced (more CO2).
  l_driver_ave = .TRUE.
    ! Switch controlling time-averaging of ECOSSE driving variables (e.g.
    ! soil temperature.
    ! TRUE means use averages over the ECOSSE timestep.
    ! FALSE means use instantaneous values (which is slightly misleading for
    ! flux variables that are themselves averages over a timestep, such as
    ! w_flux, but no further averaging is applied). Deposition of N is
    ! always averaged.

!-----------------------------------------------------------------------------
! Namelist definition.
!-----------------------------------------------------------------------------
NAMELIST  / jules_soil_ecosse/                                                 &
!   Variables from this module.
    dz_soilc_io, dt_soilc, plant_input_profile,                                &
    depo_nit_frac, temp_modifier,                                              &
    water_modifier, l_decomp_slow, l_driver_ave, l_match_layers, l_soil_N,     &
!   Parameters from ecosse_param_mod.
    pi_sfc_depth, pi_sfc_frac,                                                 &
    bacteria_min_frac, bacteria_max_frac, bacteria_min_frac_pH,                &
    bacteria_max_frac_pH, cn_bacteria, cn_fungi,                               &
    decomp_rate, decomp_wrate_min_smith, decomp_wrate_min_clark,               &
    decomp_temp_coeff_smith, decomp_ph_min, decomp_ph_max,                     &
    decomp_ph_rate_min,                                                        &
    depth_nitrif, nitrif_frac_n2o_fc, nitrif_rate, nitrif_frac_gas,            &
    nitrif_frac_no, nitrif_max_factor, nitrif_wrate_min,                       &
    denit50, denit_frac_n2_fc, denit_nitrate_equal,                            &
    denit_water_coeff, denit_bio_factor,                                       &
    amm_leach_min, n_inorg_max_conc,                                           &
!   Variables that are only set via namelist for ECOSSE, otherwise are set
!   via code. For the UM, this is currently a parameter.
    dim_cslayer

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'JULES_SOIL_ECOSSE_MOD'

#if !defined(RIVERS_ONLY)

CONTAINS

!#############################################################################

SUBROUTINE check_jules_soil_ecosse()

USE conversions_mod, ONLY:                                                     &
  ! imported scalar parameters
  secs_in_day => isec_per_day

USE jules_plant_n_uptake_mod, ONLY:                                            &
  ! imported scalar parameters
  n_model_triffid,                                                             &
  ! imported scalars
  n_uptake_model

USE timestep_mod, ONLY:                                                        &
  ! imported scalars
  timestep

USE jules_soil_mod, ONLY:                                                      &
  ! imported scalars
  sm_levels,                                                                   &
  ! imported arrays
  dzsoil

USE jules_vegetation_mod, ONLY:                                                &
  ! imported scalars
  l_crop, l_landuse, l_nitrogen, triffid_period

USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SOIL_ECOSSE namelist for consistency.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Local scalar parameters.
CHARACTER(LEN=*), PARAMETER ::                                                 &
   RoutineName = 'check_jules_soil_ecosse'   ! Name of this procedure.

! Local variables.
INTEGER :: errorstatus
!-----------------------------------------------------------------------------
! Temporary code, until later versions read a plant N uptake namelist.
! If a C-only version of ECOSSE is selected, indicate that no plant N
! uptake model is required.
!-----------------------------------------------------------------------------
IF ( .NOT. l_soil_N ) n_uptake_model = 0

!-----------------------------------------------------------------------------
! Check values associated with ECOSSE.
! Some of these could be used more generally, but currently only with ECOSSE.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! If soil N is not modelled, check for consistency with other flags.
!-----------------------------------------------------------------------------
IF ( .NOT. l_soil_N .AND.  n_uptake_model /= 0 ) THEN
  errorstatus = 101  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'Modelling plant N uptake requires that the soil model ' //    &
                'includes N.' )
END IF

!-----------------------------------------------------------------------------
! For now, insist that soil N and l_nitrogen flags are consistent.
! In time this could be relaxed, but it is not coded/tested yet.
! Testing separately to allow more informative messages.
!-----------------------------------------------------------------------------
IF ( .NOT. l_soil_N .AND. l_nitrogen ) THEN
  errorstatus = 102  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'Nitrogen limitation of NPP requires that the soil ' //        &
                'model includes N.')
ELSE IF ( l_soil_N .AND. .NOT. l_nitrogen ) THEN
  errorstatus = 103  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'A soil model with N must be used with N limitation.' )
END IF

!-----------------------------------------------------------------------------
! For now, don't allow crops (not coded), and urge caution with landuse!
!-----------------------------------------------------------------------------
IF ( l_crop ) THEN
  errorstatus = 104  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'ECOSSE cannot be used with crop model.' )
END IF

IF ( l_landuse ) THEN
  errorstatus = -101  ! a warning
  CALL ereport( RoutineName, errorstatus,                                      &
                'ECOSSE with varying landuse should be used with caution ' //  &
                'until further checking performed.')
END IF

!-----------------------------------------------------------------------------
! Check layers (and possibly finish setting them).
!-----------------------------------------------------------------------------
! l_match_layers takes precedence over other ways of setting layers.
IF ( l_match_layers ) THEN
  dim_cslayer = sm_levels
  IF ( dim_cslayer > cs_layer_max ) THEN
    errorstatus = 105  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  "Too many layers specified - increase cs_layer_max " //      &
                  "and recompile" )
  END IF
  ! dzsoil is associated in check_jules_soil.
  dz_soilc_io(1:dim_cslayer) = dzsoil
END IF

! Check that dim_cslayer has been set (and given a sensible value).
IF ( dim_cslayer < 1 ) THEN
  errorstatus = 106  ! a fatal error
  CALL ereport( RoutineName, errorstatus, "dim_cslayer must be >=1" )
END IF

! Check that there are not too many levels.
IF ( dim_cslayer > cs_layer_max ) THEN
  errorstatus = 107  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                "Too many layers specified - increase cs_layer_max and " //    &
                "recompile" )
END IF

! Associate the dz_soilc pointer with the appropriate section of
! dz_soilc_io.
dz_soilc => dz_soilc_io(1:dim_cslayer)

! Check we have sensible values for dz_soilc.
IF ( ANY(dz_soilc(:) <= 0.0) ) THEN
  errorstatus = 108  ! a fatal error
  CALL ereport( RoutineName, errorstatus, "dz_soilc must be > 0" )
END IF

! The total depth for the soil biogeochemical model should equal that of the
! soil moisture model, otherwise there is a danger of inconsistency between
! the two (e.g. water extracted by plants from deep layers but no nitrogen
! uptake because that model only sees a shallower soil column). The exception
! is the case of a bulk model with a single soil biogeochem layer which is
! given explicit special treatment at the required points in the code.
IF ( dim_cslayer > 1 ) THEN
  IF ( ABS( SUM(dz_soilc) - SUM(dzsoil) ) > EPSILON(dzsoil) ) THEN
    errorstatus = 109  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  "Total depth of soil C model must match that of the " //     &
                  "soil moisture model" )
  END IF
ELSE
  ! dim_cslayer = 1: a bulk model.
  ! Check that the representative (averaging) depth for the single bulk layer
  ! lies within the soil moisture model.
  IF ( dz_soilc(1) > SUM(dzsoil) + EPSILON(dzsoil) ) THEN
    errorstatus = 110  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  "Representative depth for bulk soil C model must lie " //    &
                  "within the total depth of the soil moisture model" )
  END IF
END IF

!-----------------------------------------------------------------------------
! Check timestep length.
!-----------------------------------------------------------------------------
! If no timestep length was given (or it was <= zero), set to the main
! timestep length.
IF ( dt_soilc <= 0.0 ) dt_soilc = timestep

! The main timestep length must be a multiple of that of the soil model.
! As we're comparing real values, we allow a small tolerance.
IF ( MOD( dt_soilc, timestep ) > EPSILON(dt_soilc) ) THEN
  errorstatus = 111  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                "Soil C timestep length must be a multiple of that of " //     &
                "JULES" )
END IF

! Check that the soil BGC model is called at least as frequently as the
! plant N uptake model and that both are synchronised.
! Currently N uptake is calculated by TRIFFID.
SELECT CASE ( n_uptake_model )
CASE ( n_model_triffid )
  IF ( MOD( triffid_period * secs_in_day, NINT(dt_soilc) ) /= 0 ) THEN
    errorstatus = 112  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  "Soil C model must be called on TRIFFID timesteps. " //      &
                  "Adjust timestep lengths." )
  END IF
END SELECT

!-----------------------------------------------------------------------------
! Check forms of rate modifiers.
!-----------------------------------------------------------------------------
SELECT CASE ( temp_modifier )
CASE ( temp_mod_q10, temp_mod_smith )
  ! These are OK.
CASE DEFAULT
  errorstatus = 113  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'Unrecognised value of temp_modifier.' )
END SELECT

SELECT CASE ( water_modifier )
CASE ( water_mod_clark, water_mod_smith )
  ! These are OK.
CASE DEFAULT
  errorstatus = 114  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'Unrecognised value of water_modifier.' )
END SELECT

!-----------------------------------------------------------------------------
! If soil timestep=JULES timestep, no need to average inputs.
!-----------------------------------------------------------------------------
IF ( ABS( dt_soilc - timestep ) < EPSILON(dt_soilc) ) THEN
  l_driver_ave = .FALSE.
END IF

!-----------------------------------------------------------------------------
! Check plant_input_profile.
!-----------------------------------------------------------------------------
SELECT CASE ( plant_input_profile )
CASE ( plant_input_exp, plant_input_roots )
  ! OK, nothing to do.
CASE DEFAULT
  errorstatus = 115  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'Invalid value for plant_input_profile.' )
END SELECT

!-----------------------------------------------------------------------------
! Check various ECOSSE rate modifier parameters.
!-----------------------------------------------------------------------------
IF ( decomp_ph_min > decomp_ph_max ) THEN
  errorstatus = 116  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'decomp_ph_min must be <= decomp_ph_max' )
END IF

!-----------------------------------------------------------------------------
! Check litterfall input parameters.
!-----------------------------------------------------------------------------
IF ( plant_input_profile == plant_input_roots .AND.                            &
   ( pi_sfc_frac < 0.0 .OR. pi_sfc_frac > 1.0 ) ) THEN
  errorstatus = 117  ! a fatal error
  CALL ereport( RoutineName, errorstatus,                                      &
                'pi_sfc_frac must lie in the interval [0,1]' )
END IF

IF ( l_soil_N ) THEN
  !-------------------------------------------------------------------------
  ! Check parameters for N model.
  !-------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  ! Check bacteria parameters.
  !-------------------------------------------------------------------------
  IF ( bacteria_min_frac < 0.0 .OR. bacteria_min_frac > 1.0 ) THEN
    errorstatus = 118  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  'bactera_min_frac must lie in the interval [0,1]' )
  END IF

  IF ( bacteria_max_frac < bacteria_min_frac .OR.                              &
       bacteria_max_frac > 1.0  ) THEN
    errorstatus = 119  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  'bacteria_max_frac must lie in the interval ' //             &
                  '[bacteria_min_frac,1]' )
  END IF

  IF ( bacteria_min_frac_pH > bacteria_max_frac_pH ) THEN
    errorstatus = 120  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  'bacteria_min_frac_pH must be <= bacteria_max_frac_pH' )
  END IF

  !-------------------------------------------------------------------------
  ! Check that depo_nit_frac lies in [0,1].
  !-------------------------------------------------------------------------
  IF ( depo_nit_frac < 0.0 .OR. depo_nit_frac > 1.0 ) THEN
    errorstatus = 121
    CALL ereport( RoutineName, errorstatus,                                    &
                  'depo_nit_frac must lie in the interval [0,1].' )
  END IF

  !-------------------------------------------------------------------------
  ! Check that various gas fractions lie in [0,1].
  !-------------------------------------------------------------------------
  IF ( nitrif_frac_gas < 0.0 .OR. nitrif_frac_gas > 1.0 ) THEN
    errorstatus = 122  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  'nitrif_frac_gas must lie in the interval [0,1]' )
  END IF

  IF ( nitrif_frac_n2o_fc < 0.0 .OR. nitrif_frac_n2o_fc > 1.0 ) THEN
    errorstatus = 123  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  'nitrif_frac_n2o_fc must lie in the interval [0,1]' )
  END IF

  IF ( nitrif_frac_no < 0.0 .OR. nitrif_frac_no > 1.0 ) THEN
    errorstatus = 124  ! a fatal error
    CALL ereport( RoutineName, errorstatus,                                    &
                  'nitrif_frac_no must lie in the interval [0,1]' )
  END IF

END IF  !  l_soil_N

END SUBROUTINE check_jules_soil_ecosse
#endif

END MODULE jules_soil_ecosse_mod
#endif
