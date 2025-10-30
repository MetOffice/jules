!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology, 2017.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT**************************************

MODULE ecosse_param_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module containing parameters for the ECOSSE soil model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGEOCHEMISTRY
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Public module variables

!-----------------------------------------------------------------------------
! Items that are in namelist.
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Parameters for litterfall input to soil.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm)  ::                                                     &
 pi_sfc_frac = 0.3,                                                            &
   ! Fraction of plant litterfall that is added to the surface soil layer
   ! (of depth pi_sfc_depth).
 pi_sfc_depth = 0.1
   ! Depth of soil over which fraction pi_sfc_frac of plant litterfall is
   ! added (m).

!-----------------------------------------------------------------------------
! Characteristics of bacterial and fungal communities (these affect soil C:N).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  bacteria_min_frac    = 0.2,                                                  &
    ! Minimum fraction of decomposer community that are bacteria.
    ! The community is considered to consist of bacteria and fungi.
  bacteria_max_frac    = 0.5,                                                  &
    ! Maximum fraction of decomposer community that are bacteria.
  bacteria_min_frac_pH = 4.0,                                                  &
    ! Soil pH at or below which the fraction of bacteria is at a minimum.
  bacteria_max_frac_pH = 5.5,                                                  &
    ! Soil pH at or above which the fraction of bacteria is at a maximum.
  cn_bacteria          = 5.5,                                                  &
    ! C:N ratio of soil bacteria.
  cn_fungi             = 11.5
    ! C:N ratio of soil fungi.

!----------------------------------------------------------------------------
! Parameters for decomposition (mineralisation).
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
!-----------------------------------------------------------------------------
!   Parameters for decomposition rate modifiers.
!-----------------------------------------------------------------------------
    decomp_ph_rate_min     = 0.2,                                              &
      ! Minimum allowed value of pH rate modifier for decomposition.
    decomp_ph_min          = 1.0,                                              &
      ! pH below which rate of decomposition is minimum.
    decomp_ph_max          = 4.5,                                              &
      ! pH above which rate of decomposition is maximum.
    decomp_wrate_min_smith  = 0.2,                                             &
      ! Minimum allowed value of the water rate modifier for decomposition
      ! when Smith et al., 2010 form is used.
    decomp_wrate_min_clark = 0.2
      ! Minimum allowed value of the water rate modifier for decomposition
      ! when the JULES form is used.

REAL(KIND=real_jlslsm) ::                                                      &
  decomp_rate(4) = [  3.22e-7, 9.65e-9, 2.12e-8, 6.43e-10 ],                   &
    ! Rate constants for decomposition of each pool (s-1).
  decomp_temp_coeff_smith(3) =  [ 47.9, 106.0, 18.3 ]
    ! Constants in Smith et al., 2010 C decomposition rate temperature modifier.

!-----------------------------------------------------------------------------
! Parameters for nitrification.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
  depth_nitrif       = 0.25,                                                   &
    ! Greatest depth at which nitrification and denitrification are
    ! allowed (m).
  nitrif_rate        = 9.921e-7,                                               &
    ! Rate constant for nitrification (s-1).
  nitrif_wrate_min   = 0.6,                                                    &
    ! Minimum allowed value of the water rate modifier for nitrification
    ! when Smith et al., 2010 form is used.
  nitrif_frac_gas    = 0.02,                                                   &
    ! Fraction of nitrification lost as gas through full nitrification.
  nitrif_frac_n2o_fc = 0.02,                                                   &
    ! Fraction of nitrification lost as N2O by partial
    ! nitrification at field capacity.
  nitrif_frac_no     = 0.4,                                                    &
    ! Fraction of nitrification gas loss through full nitrification that
    ! is NO.
  nitrif_max_factor  = 0.1
    ! Shape factor in rate modifier for nitrification (kgN m-3).
    ! 1000 kgN ha-1 m-1 is 50 kgN ha-1 per 5cm layer.
    ! In standalone ECOSSE this is hardwired as 50 kgN ha-1 per 5cm layer.

!-----------------------------------------------------------------------------
! Parameters for denitrification.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
   denit50            = 0.033,                                                 &
    ! Amount of nitrate at which denitrification rate is 50% of the
    ! potential rate (kgN m-3).
    ! 16.5 kgN ha-1 per (5cm layer) is 330 kgN ha-1 m-1.
  denit_frac_n2_fc    = 0.55,                                                  &
    ! Fraction of denitrified N that becomes N2 when soil moisture is at
    ! field capacity.
  denit_nitrate_equal = 0.4,                                                   &
    ! Amount of N in soil nitrate at which denitrified N is released as equal
    ! amounts of N2 and N2O (kgN m-3).
    ! 200.0 kgN ha-1 per (5cm layer) is 4000 kgN ha-1 m-1.
  denit_bio_factor    = 0.005
    ! Factor in denitrification calculation to convert (CO2+CH4)-C
    ! into a representation of biological activity (m2 [kg-C]-1).
    ! Note that units are the reciprocal of those of the CO2+CH4 flux.
    ! The product of denit_bio_factor*(CO2+CH4) emitted (where CO2+CH4 is
    ! used as a proxy of microbial activity) defines the fraction of
    ! nitrate in a layer that is denitrified over a timestep when other
    ! conditions are not limiting.

REAL(KIND=real_jlslsm) :: denit_water_coeff(3) = [  0.62, 0.38, 1.74 ]
   ! Fitted constants describing water modifier for denitrification using
   ! the NEMIS model.

!-----------------------------------------------------------------------------
! Parameters for leaching.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: amm_leach_min = 0.02
   ! Minimum allowed amount of N in NH4 after leaching (kg m-3).
   ! The greater of this and residual_N is used to limit leaching.
   ! This is to represent greater adsorption of NH4 than NO3.

!-----------------------------------------------------------------------------
! Maximum-allowed concentration of inorganic N.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) :: n_inorg_max_conc = -1.0
   ! Maximum-allowed concentration of inorganic N in a layer (kg m-3).
   ! This is essentially a fudge to cover for missing processes and is
   ! designed to prevent large accumulations in some dryland regions.
   ! Setting this value to be < 0 means that no maximum concentration is
   ! imposed.

!-----------------------------------------------------------------------------
! Items below here are parameters and so not in a namelist.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  cn_max_litter_pools = 300.0
    ! Maximum-allowed C:N ratio of soil litter pools (DPM and RPM).
    ! Note that the 4-pool soil C code calls this lit_cn.

!-----------------------------------------------------------------------------
! Parameters that define moisture contents (pressures).
! These are characteristics of the soils abut are currently only used by
! ECOSSE and so are kept here.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  psi_field_capac = 33.3e3,                                                    &
    ! Soil hydraulic head (suction pressure) at field capacity (Pa).
  psi_one_bar     = 1.0e5
    ! Soil hydraulic head (suction pressure) corresponding to 1 bar (Pa).

END MODULE ecosse_param_mod
