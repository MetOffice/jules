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

MODULE deposition_initialisation_aerod_mod

! ------------------------------------------------------------------------------
! Description:
!   JULES with atmospheric deposition
!   Initialisation for aerodynamic and quasi-laminar resistances.
!   Based on UKCA routine
!     ukca/src/science/chemistry/deposition/ukca_aerod.F90
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in BIOGENIC FLUXES.
!
! Code Description:
!   Language: Fortran 90.
!-----------------------------------------------------------------------------

USE planet_constants_mod,    ONLY: vkman, gg => g

! The following UKCA modules have been replaced:
! (1) asad_mod for ndepd, speci, nldepd
!     Replace asad_mod with deposition_species_mod
! (2) ukca_config_specification_mod for ukca_config
!     Use jules_science_fixes_mod for l_fix_improve_drydep

USE deposition_species_mod,  ONLY: dep_species_name, diffusion_coeff,          &
                                   dep_species_rmm

USE jules_deposition_mod,    ONLY: ndry_dep_species

USE jules_science_fixes_mod, ONLY: l_fix_improve_drydep

USE ereport_mod,             ONLY: ereport
USE jules_print_mgr,         ONLY: jules_message
USE missing_data_mod,        ONLY: rmdi
USE um_types,                ONLY: real_jlslsm
USE parkind1,                ONLY: jprb, jpim
USE yomhook,                 ONLY: lhook, dr_hook

IMPLICIT NONE

REAL(KIND=real_jlslsm), PARAMETER ::                                           &
  zero       =     0.0,                                                        &
    ! zero
  one        =     1.0,                                                        &
    ! one
  two        =     2.0,                                                        &
    ! two
  sixteen    =    16.0,                                                        &
    ! sixteen
  minus5     =    -5.0,                                                        &
    !   -5, used in alculations of Businger functions
  minus30    =   -30.0,                                                        &
    !  -30, used in deposition velocity of sulphate particles
  minus300   =  -300.0,                                                        &
    ! -300, used in deposition velocity of sulphate particles
  half       =     0.5,                                                        &
    !  0.5
  twothirds  =     2.0/3.0,                                                    &
    !  2/3
  cp         =  1004.67,                                                       &
    ! Heat capacity of air under const p (J K-1 kg-1)
  pr         =     0.72,                                                       &
    ! Prandtl number of air
  ustar_min  =     1.0e-2,                                                     &
    ! Minimum value of friction velocity (m/s), if ustar not defined
  zref       =    50.0,                                                        &
    ! Reference height for dry deposition (m).
  zpd_factor =     0.70,                                                       &
    ! Set zero-plane displacement to 0.7 of canopy height
  vair296    = 1830.0e-8,                                                      &
    ! Dynamic viscosity of air at 296 K (kg m-1 s-1).
  tkva296    =   296.0,                                                        &
    ! Temperature (K) used in calculation of kinematic viscosity of air
  kva_factor =     4.83e-8,                                                    &
    ! Factor used in calculation of kinematic viscosity of air
  z0_min     =     0.001,                                                      &
    ! Minimum value of roughness length (m), if not defined
  z0_coeff1  =     9.1,                                                        &
    ! Coefficient used in calculation of roughness lengths over oceans
  z0_coeff2  =     0.016,                                                      &
    ! Coefficient used in calculation of roughness lengths over oceans
  mo_neutral = 10000.0,                                                        &
    ! Monin-Obukhov length (m) for neutral boundary-layer conditions
  vd_coeff1  =     0.002,                                                      &
    ! Coefficient used in calculation of deposition velocity
    ! of sulphate particles
  vd_coeff2  =     0.0009,                                                     &
    ! Coefficient used in calculation of deposition velocity
    ! of sulphate particles
  d_h2o      =     2.08e-5
    ! Diffusion coefficent of water in air (m2 s-1)

REAL(KIND=real_jlslsm) ::                                                      &
  cp_vkman_g
    ! Holds -cp/(vkman*g)
    ! g is not declared as REAL, PARAMETER in planet_constants_mod

! Relative molecular masses taken from UKCA routine:
! ukca/src/science/core/chemistry/ukca_constants.F90
REAL(KIND=real_jlslsm), PARAMETER :: m_air        = 0.02897
  ! Air (in kg/mol)

! Relative molecular masses of chemical species (in g/mol), used
! in the UKCA chemical mechanisms
! Used in the calculation of the diffusion coefficient (and
! thence the quasi-laminar resistance term (Rb)), if no measurement provided
REAL(KIND=real_jlslsm), PARAMETER :: m_ho2     =  33.007
REAL(KIND=real_jlslsm), PARAMETER :: m_ch4     =  16.0
REAL(KIND=real_jlslsm), PARAMETER :: m_co      =  28.0
REAL(KIND=real_jlslsm), PARAMETER :: m_hcho    =  30.0
REAL(KIND=real_jlslsm), PARAMETER :: m_c2h6    =  30.0
REAL(KIND=real_jlslsm), PARAMETER :: m_c3h8    =  44.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mecho   =  44.0
REAL(KIND=real_jlslsm), PARAMETER :: m_no2     =  46.0
REAL(KIND=real_jlslsm), PARAMETER :: m_n2o5    = 108.01
REAL(KIND=real_jlslsm), PARAMETER :: m_me2co   =  58.0
REAL(KIND=real_jlslsm), PARAMETER :: m_isop    =  68.0
REAL(KIND=real_jlslsm), PARAMETER :: m_no      =  30.0
REAL(KIND=real_jlslsm), PARAMETER :: m_n       =  14.0
REAL(KIND=real_jlslsm), PARAMETER :: m_c       =  12.0
REAL(KIND=real_jlslsm), PARAMETER :: m_monoterp =  136.24

!     molecular masses of stratospheric species, for which surface
!     mmrs are prescribed
REAL(KIND=real_jlslsm), PARAMETER :: m_hcl        =  36.5
REAL(KIND=real_jlslsm), PARAMETER :: m_n2o        =  44.0
REAL(KIND=real_jlslsm), PARAMETER :: m_clo        =  51.5
REAL(KIND=real_jlslsm), PARAMETER :: m_hocl       =  52.5
REAL(KIND=real_jlslsm), PARAMETER :: m_oclo       =  67.5
REAL(KIND=real_jlslsm), PARAMETER :: m_clono2     =  97.5
REAL(KIND=real_jlslsm), PARAMETER :: m_cf2cl2     = 121.0
REAL(KIND=real_jlslsm), PARAMETER :: m_cfcl3      = 137.5
REAL(KIND=real_jlslsm), PARAMETER :: m_hbr        =  81.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mebr       =  95.0
REAL(KIND=real_jlslsm), PARAMETER :: m_bro        =  96.0
REAL(KIND=real_jlslsm), PARAMETER :: m_hobr       =  97.0
REAL(KIND=real_jlslsm), PARAMETER :: m_brcl       = 115.5
REAL(KIND=real_jlslsm), PARAMETER :: m_brono2     = 142.0
REAL(KIND=real_jlslsm), PARAMETER :: m_cf2clcfcl2 = 187.5
REAL(KIND=real_jlslsm), PARAMETER :: m_chf2cl     =  86.5
REAL(KIND=real_jlslsm), PARAMETER :: m_meccl3     = 133.5
REAL(KIND=real_jlslsm), PARAMETER :: m_ccl4       = 154.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mecl       =  50.5
REAL(KIND=real_jlslsm), PARAMETER :: m_cf2clbr    = 165.5
REAL(KIND=real_jlslsm), PARAMETER :: m_cf3br      = 149.0
REAL(KIND=real_jlslsm), PARAMETER :: m_ch2br2     = 173.835

! sulphur containing, etc.
REAL(KIND=real_jlslsm), PARAMETER :: m_ocs        =  60.0
REAL(KIND=real_jlslsm), PARAMETER :: m_cos        =  60.0
REAL(KIND=real_jlslsm), PARAMETER :: m_h2s        =  34.086
REAL(KIND=real_jlslsm), PARAMETER :: m_cs2        =  76.14
REAL(KIND=real_jlslsm), PARAMETER :: m_dms        =  62.1
REAL(KIND=real_jlslsm), PARAMETER :: m_dmso       =  78.13
REAL(KIND=real_jlslsm), PARAMETER :: m_me2s       =  62.1
REAL(KIND=real_jlslsm), PARAMETER :: m_msa        =  96.1
REAL(KIND=real_jlslsm), PARAMETER :: m_sec_org    =  150.0
REAL(KIND=real_jlslsm), PARAMETER :: m_sec_org_i  =  150.0
REAL(KIND=real_jlslsm), PARAMETER :: m_s          =  32.07
REAL(KIND=real_jlslsm), PARAMETER :: m_so2        =  64.06
REAL(KIND=real_jlslsm), PARAMETER :: m_so3        =  80.06
REAL(KIND=real_jlslsm), PARAMETER :: m_so4        =  96.06
REAL(KIND=real_jlslsm), PARAMETER :: m_h2so4      =  98.07
REAL(KIND=real_jlslsm), PARAMETER :: m_nh3        =  17.03
REAL(KIND=real_jlslsm), PARAMETER :: m_nh42so4    =  132.16

REAL(KIND=real_jlslsm), PARAMETER :: m_cl         = 35.5
REAL(KIND=real_jlslsm), PARAMETER :: m_cl2o2      = 103.0
REAL(KIND=real_jlslsm), PARAMETER :: m_br         = 80.0
REAL(KIND=real_jlslsm), PARAMETER :: m_h2         = 2.016
REAL(KIND=real_jlslsm), PARAMETER :: m_h2o        = 18.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mecoch2ooh = 90.0
REAL(KIND=real_jlslsm), PARAMETER :: m_isooh      = 118.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mpan       = 147.0
REAL(KIND=real_jlslsm), PARAMETER :: m_ppan       = 135.0
REAL(KIND=real_jlslsm), PARAMETER :: m_pan        = 121.0
REAL(KIND=real_jlslsm), PARAMETER :: m_hno3       = 63.0
REAL(KIND=real_jlslsm), PARAMETER :: m_hono2      = 63.0

!     Extra masses for RAQ or other chemistries
REAL(KIND=real_jlslsm), PARAMETER :: m_c5h8    =  68.0
REAL(KIND=real_jlslsm), PARAMETER :: m_c4h10   =  58.0
REAL(KIND=real_jlslsm), PARAMETER :: m_c2h4    =  28.0
REAL(KIND=real_jlslsm), PARAMETER :: m_c3h6    =  42.0
REAL(KIND=real_jlslsm), PARAMETER :: m_toluene =  92.0
REAL(KIND=real_jlslsm), PARAMETER :: m_oxylene = 106.0
REAL(KIND=real_jlslsm), PARAMETER :: m_ch3oh   =  32.0
REAL(KIND=real_jlslsm), PARAMETER :: m_meoh    =  32.0
REAL(KIND=real_jlslsm), PARAMETER :: m_buooh   =  90.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mvkooh  = 120.0
REAL(KIND=real_jlslsm), PARAMETER :: m_orgnit  = 160.0
REAL(KIND=real_jlslsm), PARAMETER :: m_macrooh = 120.0
REAL(KIND=real_jlslsm), PARAMETER :: m_gly     =  58.0

REAL(KIND=real_jlslsm), PARAMETER :: m_hono = 47.0
!     Extra masses for Wesely scheme
REAL(KIND=real_jlslsm), PARAMETER :: m_macr    = 70.0
REAL(KIND=real_jlslsm), PARAMETER :: m_etcho   = 58.0
REAL(KIND=real_jlslsm), PARAMETER :: m_nald    = 105.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mgly    = 72.0
REAL(KIND=real_jlslsm), PARAMETER :: m_hacet   = 74.0
REAL(KIND=real_jlslsm), PARAMETER :: m_hcooh   = 46.0
REAL(KIND=real_jlslsm), PARAMETER :: m_meco2h  = 60.0

!     Extra masses for EXTTC chemistry
REAL(KIND=real_jlslsm), PARAMETER :: m_apin     =  136.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mvk      =  70.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mek      =  72.0
REAL(KIND=real_jlslsm), PARAMETER :: m_alka     =  58.0
  ! as butane
REAL(KIND=real_jlslsm), PARAMETER :: m_arom     =  99.0
  ! (toluene + xylene)/2
REAL(KIND=real_jlslsm), PARAMETER :: m_bsvoc1   = 144.0
REAL(KIND=real_jlslsm), PARAMETER :: m_bsvoc2   = 144.0
REAL(KIND=real_jlslsm), PARAMETER :: m_asvoc1   = 99.0
REAL(KIND=real_jlslsm), PARAMETER :: m_asvoc2   = 99.0
REAL(KIND=real_jlslsm), PARAMETER :: m_isosvoc1 = 68.0
REAL(KIND=real_jlslsm), PARAMETER :: m_isosvoc2 = 68.0
REAL(KIND=real_jlslsm), PARAMETER :: m_onitu    = 102.0
REAL(KIND=real_jlslsm), PARAMETER :: m_bsoa     = 150.0
REAL(KIND=real_jlslsm), PARAMETER :: m_asoa     = 150.0
REAL(KIND=real_jlslsm), PARAMETER :: m_isosoa   = 130.0
REAL(KIND=real_jlslsm), PARAMETER :: m_alkaooh  = 90.0
REAL(KIND=real_jlslsm), PARAMETER :: m_aromooh  = 130.0
REAL(KIND=real_jlslsm), PARAMETER :: m_mekooh   = 104.0

!     The mass of organic nitrate is an approximation,
!     calculated as the average of ORGNIT formed by two
!     reacs. in UKCA_CHEMCO_RAQ:
!      NO2 + TOLP1 --> ORGNIT (A)
!      NO2 + OXYL1 --> ORGNIT (B)
!      * TOL  = methylbenzene       = C6H5(CH3)
!        OXYL = 1,2-dimethylbenzene = C6H4(CH3)2
!      * TOL  + OH --> TOLP1: C6H4(OH)(CH3)  = methyl phenol
!        OXYL + OH --> OXYL1: C6H3(OH)(CH3)2 = dimethyl phenol
!      * ORGNIT A: TOLP1 + NO2 ~ C6H3(CH3)(OH)NO2  ~
!                  C7H7NO3: methyl nitrophenol   -> 153
!        ORGNIT B: OXYL1 + NO2 ~ C6H2(CH3)2(OH)NO2 ~
!                  C8H9NO3: dimethyl nitrophenol -> 167
!  -------------------------------------------------------------------

!     Extra species for CRI chemistry
REAL(KIND=real_jlslsm), PARAMETER :: m_noa        = 119.08
REAL(KIND=real_jlslsm), PARAMETER :: m_meo2no2    = 93.039
REAL(KIND=real_jlslsm), PARAMETER :: m_etono2     = 91.066
REAL(KIND=real_jlslsm), PARAMETER :: m_prono2     = 105.09
REAL(KIND=real_jlslsm), PARAMETER :: m_c2h2       = 26.037
REAL(KIND=real_jlslsm), PARAMETER :: m_benzene    = 78.112
REAL(KIND=real_jlslsm), PARAMETER :: m_tbut2ene   = 56.106
REAL(KIND=real_jlslsm), PARAMETER :: m_proh       = 60.095
REAL(KIND=real_jlslsm), PARAMETER :: m_etoh       = 46.068
REAL(KIND=real_jlslsm), PARAMETER :: m_etco3h     = 90.078
REAL(KIND=real_jlslsm), PARAMETER :: m_hoch2co3   = 91.043
REAL(KIND=real_jlslsm), PARAMETER :: m_hoch2co3h  = 92.051
REAL(KIND=real_jlslsm), PARAMETER :: m_hoch2ch2o2 = 77.059
REAL(KIND=real_jlslsm), PARAMETER :: m_hoch2cho   = 60.052
REAL(KIND=real_jlslsm), PARAMETER :: m_hoc2h4ooh  = 78.067
REAL(KIND=real_jlslsm), PARAMETER :: m_hoc2h4no3  = 107.07
REAL(KIND=real_jlslsm), PARAMETER :: m_phan       = 137.05
REAL(KIND=real_jlslsm), PARAMETER :: m_ch3sch2oo  = 93.125
REAL(KIND=real_jlslsm), PARAMETER :: m_ch3s       = 47.01
REAL(KIND=real_jlslsm), PARAMETER :: m_ch3so      = 63.099
REAL(KIND=real_jlslsm), PARAMETER :: m_ch3so2     = 79.098
REAL(KIND=real_jlslsm), PARAMETER :: m_ch3so3     = 95.098
REAL(KIND=real_jlslsm), PARAMETER :: m_msia       = 80.106
! CRI intermediate species with well defined masses:
REAL(KIND=real_jlslsm), PARAMETER :: m_rn13no3    = 119.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn16no3    = 134.0
REAL(KIND=real_jlslsm), PARAMETER :: m_rn19no3    = 147.0
REAL(KIND=real_jlslsm), PARAMETER :: m_ra13no3    = 189.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ra16no3    = 203.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ra19no3    = 217.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rtx24no3   = 197.1
REAL(KIND=real_jlslsm), PARAMETER :: m_hoc2h4no2  = 107.0
REAL(KIND=real_jlslsm), PARAMETER :: m_rn9no3     = 121.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn12no3    = 135.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn15no3    = 149.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn18no3    = 163.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ru14no3    = 147.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn28no3   = 215.3
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn25no3   = 201.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn23no3   = 233.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtx28no3   = 215.3
REAL(KIND=real_jlslsm), PARAMETER :: m_rtx22no3   = 213.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rn16ooh    = 104.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn19ooh    = 118.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn14ooh    = 118.1 ! ~ m_isooh
REAL(KIND=real_jlslsm), PARAMETER :: m_rn17ooh    = 132.1
REAL(KIND=real_jlslsm), PARAMETER :: m_nru14ooh   = 163.1
REAL(KIND=real_jlslsm), PARAMETER :: m_nru12ooh   = 195.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn9ooh     = 92.09
REAL(KIND=real_jlslsm), PARAMETER :: m_rn12ooh    = 106.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn15ooh    = 120.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rn18ooh    = 134.1
REAL(KIND=real_jlslsm), PARAMETER :: m_nrn6ooh    = 123.1
REAL(KIND=real_jlslsm), PARAMETER :: m_nrn9ooh    = 137.1
REAL(KIND=real_jlslsm), PARAMETER :: m_nrn12ooh   = 151.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ra13ooh    = 160.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ra16ooh    = 174.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ra19ooh    = 188.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn28ooh   = 186.3
REAL(KIND=real_jlslsm), PARAMETER :: m_nrtn28ooh  = 231.3
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn26ooh   = 200.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn25ooh   = 172.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn24ooh   = 188.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn23ooh   = 204.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn14ooh   = 161.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn10ooh   = 146.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rtx28ooh   = 186.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtx24ooh   = 170.2
REAL(KIND=real_jlslsm), PARAMETER :: m_rtx22ooh   = 184.2
REAL(KIND=real_jlslsm), PARAMETER :: m_nrtx28ooh  = 231.2
REAL(KIND=real_jlslsm), PARAMETER :: m_carb14     = 86.13
REAL(KIND=real_jlslsm), PARAMETER :: m_carb17     = 100.2
REAL(KIND=real_jlslsm), PARAMETER :: m_carb11a    = 72.11
REAL(KIND=real_jlslsm), PARAMETER :: m_carb10     = 88.11
REAL(KIND=real_jlslsm), PARAMETER :: m_carb13     = 102.1
REAL(KIND=real_jlslsm), PARAMETER :: m_carb16     = 116.1
REAL(KIND=real_jlslsm), PARAMETER :: m_carb9      = 86.09
REAL(KIND=real_jlslsm), PARAMETER :: m_carb12     = 100.1
REAL(KIND=real_jlslsm), PARAMETER :: m_carb15     = 114.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ucarb12    = 100.1
REAL(KIND=real_jlslsm), PARAMETER :: m_nucarb12   = 145.1
REAL(KIND=real_jlslsm), PARAMETER :: m_udcarb8    = 84.07
REAL(KIND=real_jlslsm), PARAMETER :: m_udcarb11   = 98.07
REAL(KIND=real_jlslsm), PARAMETER :: m_udcarb14   = 112.1
REAL(KIND=real_jlslsm), PARAMETER :: m_tncarb26   = 168.2
REAL(KIND=real_jlslsm), PARAMETER :: m_tncarb10   = 114.1
REAL(KIND=real_jlslsm), PARAMETER :: m_tncarb12   = 144.1
REAL(KIND=real_jlslsm), PARAMETER :: m_tncarb11   = 142.1
REAL(KIND=real_jlslsm), PARAMETER :: m_ccarb12    = 126.0
REAL(KIND=real_jlslsm), PARAMETER :: m_tncarb15   = 98.0
REAL(KIND=real_jlslsm), PARAMETER :: m_rcooh25    = 184.2
REAL(KIND=real_jlslsm), PARAMETER :: m_txcarb24   = 138.2
REAL(KIND=real_jlslsm), PARAMETER :: m_txcarb22   = 152.2
REAL(KIND=real_jlslsm), PARAMETER :: m_ru12pan    = 177.1
REAL(KIND=real_jlslsm), PARAMETER :: m_rtn26pan   = 245.2
REAL(KIND=real_jlslsm), PARAMETER :: m_anhy       = 98.06 ! = MALANHY
REAL(KIND=real_jlslsm), PARAMETER :: m_aroh14     = 94.11 ! = PHENOL
REAL(KIND=real_jlslsm), PARAMETER :: m_aroh17     = 108.1 ! = CRESOL
REAL(KIND=real_jlslsm), PARAMETER :: m_arnoh14    = 139.1
REAL(KIND=real_jlslsm), PARAMETER :: m_arnoh17    = 153.1
REAL(KIND=real_jlslsm), PARAMETER :: m_dhpcarb9   = 136.102
REAL(KIND=real_jlslsm), PARAMETER :: m_hpucarb12  = 116.115
REAL(KIND=real_jlslsm), PARAMETER :: m_hucarb9    = 86.089
REAL(KIND=real_jlslsm), PARAMETER :: m_iepox      = 118.131
REAL(KIND=real_jlslsm), PARAMETER :: m_hmml       = 102.089
REAL(KIND=real_jlslsm), PARAMETER :: m_dhpr12ooh  = 182.127
REAL(KIND=real_jlslsm), PARAMETER :: m_dhcarb9    = 104.105
REAL(KIND=real_jlslsm), PARAMETER :: m_ru12no3    = 179.128
REAL(KIND=real_jlslsm), PARAMETER :: m_ru10no3    = 149.102
REAL(KIND=real_jlslsm), PARAMETER :: m_dhpr12o2   = 181.119
REAL(KIND=real_jlslsm), PARAMETER :: m_ru10ao2    = 119.096
REAL(KIND=real_jlslsm), PARAMETER :: m_maco3      = 101.081

!   Define dummy molar mass for all common representative intermediate species
!   in CRI mechanism, as these do not have defined molecular masses.
!   - Using m_cri = 150g/mol (same as Sec_org)
REAL(KIND=real_jlslsm), PARAMETER :: m_cri        = 150.0

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName = 'DEPOSITION_INITIALISATION_AEROD_MOD'

CONTAINS

! ------------------------------------------------------------------------------
SUBROUTINE deposition_initialisation_aerod_jules()
! ------------------------------------------------------------------------------

! Only need RMM of air and water, all others as dep_species_rmm
! from jules_deposition_species namelists

IMPLICIT NONE

! Local variables
INTEGER :: m     ! Loop index for deposited species

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER   ::                                               &
  RoutineName='DEPOSITION_INITIALISATION_AEROD_JULES'

! Ereport variables
INTEGER :: icode

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Define -cp/(vkman*g)
cp_vkman_g = -cp / (vkman * gg)

! Assign diffusion coefficients, units m2 s-1.
! If no value found in literature, diffusion_coeff (D) calculated using:
!   D(X) = D(H2O) * SQRT[RMM(H2O)/RMM(X)],
! where X is the species in question and D(H2O) = 2.08 x 10^-5 m2 s-1
! (Marrero & Mason, J Phys Chem Ref Dat, 1972).

! If diffusion coefficient = -1.0, no observed diffusion coefficient
! Already checked that it is set
DO m = 1, ndry_dep_species
  IF (diffusion_coeff(m) < 0.0) THEN
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o/dep_species_rmm(m))
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE deposition_initialisation_aerod_jules

! ------------------------------------------------------------------------------
SUBROUTINE deposition_initialisation_aerod_ukca()
! ------------------------------------------------------------------------------

IMPLICIT NONE

! Local variables
INTEGER :: m     ! Loop index for deposited species

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER   ::                                               &
  RoutineName='DEPOSITION_INITIALISATION_AEROD_UKCA'

! Ereport variables
INTEGER :: icode

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Define -cp/(vkman*g)
cp_vkman_g = -cp / (vkman * gg)

! Assign diffusion coefficients, units m2 s-1.
! Set to -1 unless species dry deposits
! If no value found in literature, diffusion_coeff (D) calculated using:
!   D(X) = D(H2O) * SQRT[RMM(H2O)/RMM(X)],
! where X is the species in question and D(H2O) = 2.08 x 10^-5 m2 s-1
! (Marrero & Mason, J Phys Chem Ref Dat, 1972).

! The values of diffusion_coeff will be used to flag those species
! that dry deposit

IF (.NOT. ALLOCATED(diffusion_coeff)) THEN
  ALLOCATE(diffusion_coeff(ndry_dep_species))
END IF
diffusion_coeff(:) = -1.0

DO m = 1, ndry_dep_species
  SELECT CASE (dep_species_name(m))
  CASE ('O3        ','NO2       ','O3S       ','NO3       ')
    diffusion_coeff(m) = 1.4e-5
  CASE ('NO        ')
    IF (l_fix_improve_drydep) THEN
      ! Tang et al 2014 https://doi.org/10.5194/acp-14-9233-2014
      diffusion_coeff(m) = 2.3e-5
    ELSE
      ! NO = 6*NO2 (following Gian. 1998)
      diffusion_coeff(m) = 8.4e-5
    END IF
    ! RU14NO3 ~ ISON
  CASE ('HNO3      ','HONO2     ','ISON      ','B2ndry    ',                   &
        'A2ndry    ','N2O5      ','HO2NO2    ','HNO4      ',                   &
        'RU14NO3   ')
    diffusion_coeff(m) = 1.2e-5
  CASE ('H2O2      ','HOOH      ')
    diffusion_coeff(m) = 1.46e-5
  CASE ('CH3OOH    ','MeOOH     ','HCOOH     ')
    diffusion_coeff(m) = 1.27e-5
    ! CARB7 ~ HACET
  CASE ('C2H5OOH   ','EtOOH     ','MeCO3H    ','MeCO2H    ',                   &
        'HACET     ','CARB7     ')
    diffusion_coeff(m) = 1.12e-5
    ! RN10OOH ~ n-PrOOH
  CASE ('n_C3H7OOH ','i_C3H7OOH ','n-PrOOH   ','i-PrOOH   ',                   &
        'PropeOOH  ','RN10OOH   ')
    diffusion_coeff(m) = 1.01e-5
  CASE ('MeCOCH2OOH')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_mecoch2ooh)
  CASE ('ISOOH     ','RU14OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_isooh)
  CASE ('HONO      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hono)
  CASE ('MACROOH   ','RU10OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_macrooh)
  CASE ('MEKOOH    ','RN11OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_mekooh)
  CASE ('ALKAOOH   ','RN8OOH    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_alkaooh)
  CASE ('AROMOOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_aromooh)
  CASE ('BSVOC1    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_bsvoc1)
  CASE ('BSVOC2    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_bsvoc2)
  CASE ('ASVOC1    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_asvoc1)
  CASE ('ASVOC2    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_asvoc2)
  CASE ('ISOSVOC1  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_isosvoc1)
  CASE ('ISOSVOC2  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_isosvoc2)
  CASE ('PAN       ')
    IF (l_fix_improve_drydep) THEN
      diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_pan)
    ELSE
      diffusion_coeff(m) = 0.31e-5
    END IF
  CASE ('PPAN      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ppan)
  CASE ('MPAN      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_mpan)
  CASE ('ONITU     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_onitu)
  CASE ('CO        ')
    diffusion_coeff(m) = 1.86e-5
  CASE ('CH4       ')
    IF (l_fix_improve_drydep) THEN
      ! Tang et al 2015 https://doi.org/10.5194/acp-15-5585-2015
      diffusion_coeff(m) = 2.2e-5
    ELSE
      diffusion_coeff(m) = 5.74e-5
    END IF
  CASE ('NH3       ')
    diffusion_coeff(m) = 2.08e-5
  CASE ('H2        ')
    diffusion_coeff(m) = 6.7e-5
  CASE ('SO2       ')
    diffusion_coeff(m) = 1.2e-5
  CASE ('DMSO      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_dmso)
  CASE ('Sec_Org   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_sec_org)
  CASE ('SEC_ORG_I ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_sec_org_i)
  CASE ('H2SO4     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_h2so4)
  CASE ('MSA       ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_msa)
  CASE ('MVKOOH    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_mvkooh)
  CASE ('s-BuOOH   ','RN13OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_buooh)
  CASE ('ORGNIT    ')
    diffusion_coeff(m) = 1.0e-5
  CASE ('BSOA      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_bsoa)
  CASE ('ASOA      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_asoa)
  CASE ('ISOSOA    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_isosoa)
    ! Species added for sonsitency with 2D scheme
    ! Use simple approximation a la ExTC
  CASE ('HCHO      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hcho)
  CASE ('MeCHO     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_mecho)
  CASE ('EtCHO     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_etcho)
  CASE ('MACR      ','UCARB10   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_macr)
  CASE ('NALD      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nald)
  CASE ('GLY       ','CARB3     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_gly)
  CASE ('MGLY      ','CARB6     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_mgly)
  CASE ('MeOH      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_meoh)
  CASE ('Monoterp  ', 'APINENE   ', 'BPINENE   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_monoterp)
  CASE ('HBr       ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hbr)
  CASE ('HOBr      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hobr)
  CASE ('HCl       ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hcl)
  CASE ('HOCl      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hocl)
    ! CRImech-only species:
  CASE ('EtOH      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_etoh)
  CASE ('i-PrOH    ', 'n-PrOH    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_proh)
  CASE ('HOC2H4OOH ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hoc2h4ooh)
  CASE ('HOCH2CHO  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hoch2cho)
  CASE ('EtCO3H    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_etco3h)
  CASE ('HOCH2CO3H ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hoch2co3h)
  CASE ('PHAN      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_phan)
  CASE ('NOA       ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_noa)
  CASE ('HOC2H4NO3 ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hoc2h4no3)
  CASE ('RTX24NO3  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtx24no3)
  CASE ('RN9NO3    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn9no3)
  CASE ('RN12NO3   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn12no3)
  CASE ('RN15NO3   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn15no3)
  CASE ('RN18NO3   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn18no3)
  CASE ('RTN28NO3  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn28no3)
  CASE ('RTN25NO3  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn25no3)
  CASE ('RTN23NO3  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn23no3)
  CASE ('RTX28NO3  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtx28no3)
  CASE ('RTX22NO3  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtx22no3)
  CASE ('RN16OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn16ooh)
  CASE ('RN19OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn19ooh)
  CASE ('RN14OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn14ooh)
  CASE ('RN17OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn17ooh)
  CASE ('NRU14OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nru14ooh)
  CASE ('NRU12OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nru12ooh)
  CASE ('RN9OOH    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn9ooh)
  CASE ('RN12OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn12ooh)
  CASE ('RN15OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn15ooh)
  CASE ('RN18OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rn18ooh)
  CASE ('NRN6OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nrn6ooh)
  CASE ('NRN9OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nrn9ooh)
  CASE ('NRN12OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nrn12ooh)
  CASE ('RA13OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ra13ooh)
  CASE ('RA16OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ra16ooh)
  CASE ('RA19OOH   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ra19ooh)
  CASE ('RTN28OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn28ooh)
  CASE ('NRTN28OOH ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nrtn28ooh)
  CASE ('RTN26OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn26ooh)
  CASE ('RTN25OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn25ooh)
  CASE ('RTN24OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn24ooh)
  CASE ('RTN23OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn23ooh)
  CASE ('RTN14OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn14ooh)
  CASE ('RTN10OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn10ooh)
  CASE ('RTX28OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtx28ooh)
  CASE ('RTX24OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtx24ooh)
  CASE ('RTX22OOH  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtx22ooh)
  CASE ('NRTX28OOH ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nrtx28ooh)
  CASE ('CARB14    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb14)
  CASE ('CARB17    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb17)
  CASE ('CARB11A   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb11a)
  CASE ('CARB10    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb10)
  CASE ('CARB13    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb13)
  CASE ('CARB16    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb16)
  CASE ('CARB9     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb9)
  CASE ('CARB12    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb12)
  CASE ('CARB15    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_carb15)
  CASE ('UCARB12   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ucarb12)
  CASE ('NUCARB12  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_nucarb12)
  CASE ('UDCARB8   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_udcarb8)
  CASE ('UDCARB11  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_udcarb11)
  CASE ('UDCARB14  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_udcarb14)
  CASE ('TNCARB26  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_tncarb26)
  CASE ('TNCARB10  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_tncarb10)
  CASE ('TNCARB12  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_tncarb12)
  CASE ('TNCARB11  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_tncarb11)
  CASE ('CCARB12   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ccarb12)
  CASE ('TNCARB15  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_tncarb15)
  CASE ('RCOOH25   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rcooh25)
  CASE ('TXCARB24  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_txcarb24)
  CASE ('TXCARB22  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_txcarb22)
  CASE ('RU12PAN   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ru12pan)
  CASE ('RTN26PAN  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_rtn26pan)
  CASE ('AROH14    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_aroh14)
  CASE ('AROH17    ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_aroh17)
  CASE ('ARNOH14   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_arnoh14)
  CASE ('ARNOH17   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_arnoh17)
  CASE ('ANHY      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_anhy)
  CASE ('IEPOX     ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_iepox)
  CASE ('HMML      ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hmml)
  CASE ('HUCARB9   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hucarb9)
  CASE ('HPUCARB12 ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_hpucarb12)
  CASE ('DHPCARB9  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_dhpcarb9)
  CASE ('DHPR12OOH ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_dhpr12ooh)
  CASE ('DHCARB9   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_dhcarb9)
  CASE ('RU12NO3   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ru12no3)
  CASE ('RU10NO3  ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ru10no3)
    ! RU12OOH happens to have same mass as default (150g/mol)
  CASE ('RU12OOH   ')
    ! m_cri = 150 (same as sec_org) => diffusion_coeff(m) = 0.72e-5
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_cri)
  CASE ('RA13NO3   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ra13no3)
  CASE ('RA16NO3   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ra16no3)
  CASE ('RA19NO3   ')
    diffusion_coeff(m) = d_h2o * SQRT(m_h2o / m_ra19no3)
  END SELECT
END DO

! The values of diffusion_coeff will be used to flag those species
! that dry deposit

DO m = 1, ndry_dep_species
  IF (diffusion_coeff(m) < 0.0) THEN
    jules_message = 'Warning: No dry deposition for ' //                       &
                dep_species_name(m) // ' will be calculated.'
    icode = -1
    CALL ereport(routinename, icode, jules_message)
  END IF
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN

END SUBROUTINE deposition_initialisation_aerod_ukca

END MODULE deposition_initialisation_aerod_mod
