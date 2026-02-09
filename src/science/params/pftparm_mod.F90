! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module holds surface parameters for each Plant Functional Type (but
! not parameters that are only used by TRIFFID).



! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.

MODULE pftparm

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Radiation and albedo parameters.
!-----------------------------------------------------------------------------
INTEGER, ALLOCATABLE ::                                                        &
 orient(:)
                 ! Flag for leaf orientation: 1 for horizontal,
!                    0 for spherical.

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 albsnc_max(:)                                                                 &
                 ! Snow-covered albedo for large LAI.
,albsnc_min(:)                                                                 &
                 ! Snow-covered albedo for zero LAI.
,albsnf_max(:)                                                                 &
                 ! Snow-free albedo for large LAI.
,albsnf_maxl(:)                                                                &
                 ! Min Snow-free albedo (max LAI) when scaled to obs
,albsnf_maxu(:)                                                                &
                 ! Max Snow-free albedo (max LAI) when scaled to obs
,alnir(:)                                                                      &
                 ! Leaf reflection coefficient for near infra-red.
,alnirl(:)                                                                     &
                 ! lower limit on alnir, when scaled to albedo obs
,alniru(:)                                                                     &
                 ! upper limit on alnir, when scaled to albedo obs
,alpar(:)                                                                      &
                 ! Leaf reflection coefficient for PAR.
,alparl(:)                                                                     &
                 ! lower limit on alpar, when scaled to albedo obs
,alparu(:)                                                                     &
                 ! upper limit on alpar, when scaled to albedo obs
,kext(:)                                                                       &
                 ! Light extinction coefficient - used to
!                    calculate weightings for soil and veg.
,kpar(:)                                                                       &
                 ! PAR Extinction coefficient
!                    (m2 leaf/m2 ground)
,lai_alb_lim(:)                                                                &
!                ! Lower limit on permitted LAI in albedo
,omega(:)                                                                      &
                 ! Leaf scattering coefficient for PAR.
,omegal(:)                                                                     &
                 ! lower limit on omega, when scaled to albedo obs
,omegau(:)                                                                     &
                 ! upper limit on omega, when scaled to albedo obs
,omnir(:)                                                                      &
                 ! Leaf scattering coefficient for near infra-red.
,omnirl(:)                                                                     &
                 ! lower limit on omnir, when scaled to albedo obs
,omniru(:)
                 ! upper limit on omnir, when scaled to albedo obs

!-----------------------------------------------------------------------------
! Parameters for phoyosynthesis and respiration.
!-----------------------------------------------------------------------------
INTEGER, ALLOCATABLE ::                                                        &
 c3(:)           ! Flag for C3 types: 1 for C3 Plants,
!                    0 for C4 Plants.

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 alpha(:)                                                                      &
                 ! Quantum efficiency of photosynthesis
!                    (mol CO2/mol PAR photons).
,can_struct_a(:)                                                               &
                 ! Pinty canopy structure factor corresponding to overhead
                 ! sun (dimensionless)
,dqcrit(:)                                                                     &
                 ! Critical humidity deficit (kg H2O/kg air), used with the
                 ! Jacobs closure.
,f0(:)                                                                         &
                 ! CI/CA for DQ = 0, used with the Jacobs closure.
,fd(:)                                                                         &
                 ! Dark respiration coefficient.
,g1_stomata(:)                                                                 &
                 ! Parameter g1 for the Medlyn et al. (2011) model of
                 ! stomatal conductance (kPa**0.5) - see Eqn.11 of
                 ! doi: 10.1111/j.1365-2486.2012.02790.x.
,kn(:)                                                                         &
                 ! Exponential for N profile in canopy, used with
                 ! can_rad_mod=4, 5 (decay is a function of layers).
,knl(:)                                                                        &
                 ! Decay coefficient for N profile in canopy, used with
                 ! can_rad_mod=6 (decay is a function of LAI).
,neff(:)                                                                       &
                ! Constant relating VCMAX and leaf N (mol/m2/s)
!                   from Schulze et al. 1994
!                   (AMAX = 0.4e-3 * NL  - assuming dry matter is
!                   40% carbon by mass)
!                   and Jacobs 1994:
!                   C3 : VCMAX = 2 * AMAX ;
!                   C4 : VCMAX = AMAX  ..
,nl0(:)                                                                        &
                 ! Top leaf nitrogen concentration
!                    (kg N/kg C).
,nr_nl(:)                                                                      &
                 ! Ratio of root nitrogen concentration to
!                    leaf nitrogen concentration.
,ns_nl(:)                                                                      &
                 ! Ratio of stem nitrogen concentration to
!                    leaf nitrogen concentration.
,r_grow(:)                                                                     &
                 ! Growth respiration fraction.
,tlow(:)                                                                       &
                 ! Lower temperature for photosynthesis (deg C).
,tupp(:)                                                                       &
                 ! Upper temperature for photosynthesis (deg C).
  !---------------------------------------------------------------------------
  ! Parameters for the Farquhar photosynthesis model.
  ! Jmax is the potential rate of electron transport.
  ! Vcmax is the maximum rate of carboxylation of Rubisco.
  !---------------------------------------------------------------------------
,act_jmax(:)                                                                   &
                 ! Activation energy for temperature response of Jmax
                 ! (J mol-1).
,act_vcmax(:)                                                                  &
                 ! Activation energy for temperature response of Vcmax
                 ! (J mol-1).
,alpha_elec(:)                                                                 &
                 ! Quantum yield of electron transport
                 ! (mol electrons/mol PAR photons).
,deact_jmax(:)                                                                 &
                 ! Deactivation energy for temperature response of Jmax
                 ! (J mol-1). This describes the rate of decrease
                 ! above the optimum temperature.
,deact_vcmax(:)                                                                &
                 ! Deactivation energy for temperature response of Vcmax
                 ! and Jmax (J mol-1). This describes the rate of decrease
                 ! above the optimum temperature.
,ds_jmax(:)                                                                    &
                 ! Entropy factor for temperature reponse of Jmax
                 ! (J mol-1 K-1).
,ds_vcmax(:)                                                                   &
                 ! Entropy factor for temperature reponse of Vcmax
                 ! (J mol-1 K-1).
,jv25_ratio(:)
                 ! Ratio of Jmax to Vcmax at 25 deg C
                 ! (mol electrons mol-1 CO2).

!-----------------------------------------------------------------------------
! Parameters for trait physiology
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 hw_sw(:)                                                                      &
                 ! Heart:Stemwood Ratio (kg N/kg N)
,lma(:)                                                                        &
                 ! Leaf mass by area (1/SLA), (kg leaf/ m2)
,nmass(:)                                                                      &
                 ! Leaf nitrogen dry weight (g N/g leaf)
,nr(:)                                                                         &
                 ! Root nitrogen concentration (kg N/kg C)
,nsw(:)                                                                        &
                 ! Stem nitrogen concentration (kg N/kg C)
,q10_leaf(:)                                                                   &
                 ! Factor for leaf respiration.
,vint(:)                                                                       &
                 ! Y intercept of the Narea to Vcmax relationship
                 ! from Kattge et al. (2009)
,vsl(:)
                 ! Slope of the Narea to Vcmax relationship
                 ! from Kattge et al. (2009)

!-----------------------------------------------------------------------------
! Allometric and other parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 a_wl(:)                                                                       &
                 ! Allometric coefficient relating the target
!                    woody biomass to the leaf area index
!                    (kg C/m2)
,a_ws(:)                                                                       &
                 ! Woody biomass as a multiple of live
!                    stem biomass.
,b_wl(:)                                                                       &
                 ! Allometric exponent relating the target
!                    woody biomass to the leaf area index.
,eta_sl(:)                                                                     &
                 ! Live stemwood coefficient (kg C m-1 (m2 leaf)-1)
,sigl(:)
                 ! Specific density of leaf carbon
!                    (kg C/m2 leaf).

!-----------------------------------------------------------------------------
! Phenology parameters.
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 dgl_dm(:)                                                                     &
                 ! Rate of change of leaf turnover rate with
!                    moisture availability.
,dgl_dt(:)                                                                     &
                 ! Rate of change of leaf turnover rate with
!                    temperature (/K)
,fsmc_of(:)                                                                    &
                 ! Moisture availability below which leaves
!                    are dropped.
,g_leaf_0(:)                                                                   &
                 ! Minimum turnover rate for leaves (/360days).

,tleaf_of(:)
                 ! Temperature below which leaves are
!                    dropped (K)

!-----------------------------------------------------------------------------
! Parameters for hydrological, thermal and other "physical" characteristics.
!-----------------------------------------------------------------------------
INTEGER, ALLOCATABLE ::                                                        &
 fsmc_mod(:)
                   ! Flag for whether water stress is calculated from
                   ! available water in layers weighted by root fraction (0)
                   ! or
                   ! whether water stress calculated from available
                   ! water in root zone (1)

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 catch0(:)                                                                     &
                 ! Minimum canopy capacity (kg/m2).
,dcatch_dlai(:)                                                                &
                 ! Rate of change of canopy capacity with LAI.
,dust_veg_scj(:)                                                               &
                 ! Dust emission scaling factor for  each PFT
,dz0v_dh(:)                                                                    &
                 ! Rate of change of vegetation roughness
!                    length with height.
,emis_pft(:)                                                                   &
                 !  Surface emissivity
,fsmc_p0(:)                                                                    &
                 ! parameter in calculation of the
                 ! soil moisture at which the plant begins to experience
                 ! water stress
,glmin(:)                                                                      &
                 ! Minimum leaf conductance for H2O (m/s).
,gsoil_f(:)                                                                    &
                 ! Soil evaporation enhancement factor (no units).
,infil_f(:)                                                                    &
                 ! Infiltration enhancement factor.
,psi_close(:)                                                                  &
                 ! soil matric potential (Pa) below which soil moisture
                 ! stress factor fsmc is zero. Should be negative.
,psi_open(:)                                                                   &
                 ! soil matric potential (Pa) above which soil moisture
                 ! stress factor fsmc is one. Should be negative.
,rootd_ft(:)                                                                   &
                 ! e-folding depth (m) of the root density.
,z0v(:)
                 ! Specified vegetation roughness length.
!                    used with l_spec_veg_z0 = .true.



!-----------------------------------------------------------------------------
! Parameters for ozone damage
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 dfp_dcuo(:)                                                                   &
                 ! Plant type specific O3 sensitivity parameter
                 ! (nmol-1 m2 s).
,fl_o3_ct(:)
                 ! Critical flux of O3 to vegetation (nmol/m2/s).

!-----------------------------------------------------------------------------
! Parameters for BVOC emissions
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 aef(:)                                                                        &
                 ! Acetone Emission Factor (ugC/g/h)
,ci_st(:)                                                                      &
                 ! Internal CO2 partial pressure (Pa)
                 !   at standard conditions
,gpp_st(:)                                                                     &
                 ! Gross primary productivity (KgC/m2/s)
                 !   at standard conditions
,ief(:)                                                                        &
                 ! Isoprene Emission Factor (ugC/g/h)
                 ! See Pacifico et al., (2011) Atm. Chem. Phys.
,mef(:)                                                                        &
                 ! Methanol Emission Factor (ugC/g/h)
,tef(:)
                 ! (Mono-)Terpene Emission Factor (ugC/g/h)

!-----------------------------------------------------------------------------
! Parameters for INFERNO combustion
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 avg_ba(:)                                                                     &
                 ! Average PFT Burnt Area per fire
,ccleaf_min(:)                                                                 &
                 ! Leaf minimum combustion completeness (kg/kg)
,ccleaf_max(:)                                                                 &
                 ! Leaf maximum combustion completeness (kg/kg)
,ccwood_min(:)                                                                 &
                 ! Wood (or Stem) minimum combustion completeness (kg/kg)
,ccwood_max(:)                                                                 &
                 ! Wood (or Stem) maximum combustion completeness (kg/kg)
,fire_mort(:)
                 ! Fire mortality per PFT

!-----------------------------------------------------------------------------
! Parameters for INFERNO emissions
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 fef_bc(:)                                                                     &
                 ! Fire BC Emission Factor (g/kg)
,fef_ch4(:)                                                                    &
                 ! Fire CH4 Emission Factor (g/kg)
,fef_co(:)                                                                     &
                 ! Fire CO Emission Factor (g/kg)
,fef_co2(:)                                                                    &
                 ! Fire CO2 Emission Factor (g/kg)
                 ! See Thonicke et al., (2005,2010)
,fef_nox(:)                                                                    &
                 ! Fire NOx Emission Factor (g/kg)
,fef_oc(:)                                                                     &
                 ! Fire OC Emission Factor (g/kg)
,fef_so2(:)                                                                    &
                 ! Fire SO2 Emission Factor (g/kg)
,fef_c2h4(:)                                                                   &
                 ! Fire C2H4 Emission Factor (g/kg)
,fef_c2h6(:)                                                                   &
                 ! Fire C2H6 Emission Factor (g/kg)
,fef_c3h8(:)                                                                   &
                 ! Fire C2H8 Emission Factor (g/kg)
,fef_hcho(:)                                                                   &
                 ! Fire HCHO Emission Factor (g/kg)
,fef_mecho(:)                                                                  &
                 ! Fire MeCHO Emission Factor (g/kg)
,fef_nh3(:)                                                                    &
                 ! Fire NH3 Emission Factor (g/kg)
,fef_dms(:)
                 ! Fire DMS Emission Factor (g/kg)

!-----------------------------------------------------------------------------
! Parameters for SUGAR
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 sug_grec(:)                                                                   &
                 ! Turnover of structural carbon into NSC (KgC/m2/s)
,sug_g0(:)                                                                     &
                 ! Specific structural C growth rate (KgC/m2/s)
,sug_yg(:)
                 ! Growth yield fraction

!-----------------------------------------------------------------------------
! Parameters for SOX
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 sox_a(:)                                                                      &
                 ! The shape parameter in the xylem vulnerability curve.
,sox_p50(:)                                                                    &
                 ! Xlem water potential at which xylem hydraulic
                 ! conductance is half its maximum value. (MPa)
,sox_rp_min(:)
                 ! Plant minimum hydraulic resistance. (m2 s MPa/mol)

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PFTPARM'

CONTAINS

SUBROUTINE pftparm_alloc(npft)

USE missing_data_mod, ONLY: imdi, rmdi

!No USE statements other than Dr Hook
USE parkind1,    ONLY: jprb, jpim
USE yomhook,     ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: npft

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PFTPARM_ALLOC'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!  ====pftparm module common====

! Radiation and albedo parameters.
ALLOCATE( albsnc_max(npft))
ALLOCATE( albsnc_min(npft))
ALLOCATE( albsnf_max(npft))
ALLOCATE( albsnf_maxl(npft))
ALLOCATE( albsnf_maxu(npft))
ALLOCATE( alnir(npft))
ALLOCATE( alnirl(npft))
ALLOCATE( alniru(npft))
ALLOCATE( alpar(npft))
ALLOCATE( alparl(npft))
ALLOCATE( alparu(npft))
ALLOCATE( kext(npft))
ALLOCATE( kpar(npft))
ALLOCATE( lai_alb_lim(npft))
ALLOCATE( omega(npft))
ALLOCATE( omegal(npft))
ALLOCATE( omegau(npft))
ALLOCATE( omnir(npft))
ALLOCATE( omnirl(npft))
ALLOCATE( omniru(npft))
ALLOCATE( orient(npft))

orient(:)       = imdi
albsnc_max(:)   = rmdi
albsnc_min(:)   = rmdi
albsnf_max(:)   = rmdi
albsnf_maxl(:)  = rmdi
albsnf_maxu(:)  = rmdi
alnir(:)        = rmdi
alnirl(:)       = rmdi
alniru(:)       = rmdi
alpar(:)        = rmdi
alparl(:)       = rmdi
alparu(:)       = rmdi
kext(:)         = rmdi
kpar(:)         = rmdi
lai_alb_lim(:)  = rmdi
omega(:)        = rmdi
omegal(:)       = rmdi
omegau(:)       = rmdi
omnir(:)        = rmdi
omnirl(:)       = rmdi
omniru(:)       = rmdi

! Photosynthesis and respiration parameters
ALLOCATE( act_jmax(npft))
ALLOCATE( act_vcmax(npft))
ALLOCATE( alpha(npft))
ALLOCATE( alpha_elec(npft))
ALLOCATE( c3(npft))
ALLOCATE( can_struct_a(npft))
ALLOCATE( deact_jmax(npft))
ALLOCATE( deact_vcmax(npft))
ALLOCATE( dqcrit(npft))
ALLOCATE( ds_jmax(npft))
ALLOCATE( ds_vcmax(npft))
ALLOCATE( f0(npft))
ALLOCATE( fd(npft))
ALLOCATE( g1_stomata(npft))
ALLOCATE( jv25_ratio(npft))
ALLOCATE( kn(npft))
ALLOCATE( knl(npft))
ALLOCATE( neff(npft))
ALLOCATE( nl0(npft))
ALLOCATE( nr_nl(npft))
ALLOCATE( ns_nl(npft))
ALLOCATE( r_grow(npft))
ALLOCATE( tlow(npft))
ALLOCATE( tupp(npft))

c3(:)           = imdi
act_jmax(:)     = rmdi
act_vcmax(:)    = rmdi
alpha(:)        = rmdi
alpha_elec(:)   = rmdi
can_struct_a(:) = rmdi
deact_jmax(:)   = rmdi
deact_vcmax(:)  = rmdi
dqcrit(:)       = rmdi
ds_jmax(:)      = rmdi
ds_vcmax(:)     = rmdi
f0(:)           = rmdi
fd(:)           = rmdi
g1_stomata(:)   = rmdi
jv25_ratio(:)   = rmdi
kn(:)           = rmdi
knl(:)          = rmdi
neff(:)         = rmdi
nl0(:)          = rmdi
nr_nl(:)        = rmdi
ns_nl(:)        = rmdi
r_grow(:)       = rmdi
tlow(:)         = rmdi
tupp(:)         = rmdi

! Traint physiology parameters
ALLOCATE( hw_sw(npft))
ALLOCATE( lma(npft))
ALLOCATE( nmass(npft))
ALLOCATE( nr(npft))
ALLOCATE( nsw(npft))
ALLOCATE( q10_leaf(npft))
ALLOCATE( vint(npft))
ALLOCATE( vsl(npft))

hw_sw(:)        = rmdi
lma(:)          = rmdi
nmass(:)        = rmdi
nr(:)           = rmdi
nsw(:)          = rmdi
q10_leaf(:)     = rmdi
vint(:)         = rmdi
vsl(:)          = rmdi

! Allometric parameters
ALLOCATE( a_wl(npft))
ALLOCATE( a_ws(npft))
ALLOCATE( b_wl(npft))
ALLOCATE( eta_sl(npft))
ALLOCATE( sigl(npft))

a_wl(:)         = rmdi
a_ws(:)         = rmdi
b_wl(:)         = rmdi
eta_sl(:)       = rmdi
sigl(:)         = rmdi

! Phenology parameters
ALLOCATE( dgl_dm(npft))
ALLOCATE( dgl_dt(npft))
ALLOCATE( fsmc_of(npft))
ALLOCATE( g_leaf_0(npft))
ALLOCATE( tleaf_of(npft))

dgl_dm(:)       = rmdi
dgl_dt(:)       = rmdi
fsmc_of(:)      = rmdi
g_leaf_0(:)     = rmdi
tleaf_of(:)     = rmdi

! Hydrological parameters
ALLOCATE( catch0(npft))
ALLOCATE( dcatch_dlai(npft))
ALLOCATE( dust_veg_scj(npft))
ALLOCATE( dz0v_dh(npft))
ALLOCATE( emis_pft(npft))
ALLOCATE( fsmc_mod(npft))
ALLOCATE( fsmc_p0(npft))
ALLOCATE( glmin(npft))
ALLOCATE( gsoil_f(npft))
ALLOCATE( infil_f(npft))
ALLOCATE( psi_close(npft))
ALLOCATE( psi_open(npft))
ALLOCATE( rootd_ft(npft))
ALLOCATE( z0v(npft))

fsmc_mod(:)     = imdi
catch0(:)       = rmdi
dcatch_dlai(:)  = rmdi
dust_veg_scj(:) = rmdi
dz0v_dh(:)      = rmdi
emis_pft(:)     = rmdi
fsmc_p0(:)      = rmdi
glmin(:)        = rmdi
gsoil_f(:)      = rmdi
infil_f(:)      = rmdi
psi_close(:)    = rmdi
psi_open(:)     = rmdi
rootd_ft(:)     = rmdi
z0v(:)          = rmdi

! Ozone damage parameters
ALLOCATE( dfp_dcuo(npft))
ALLOCATE( fl_o3_ct(npft))

dfp_dcuo(:) = rmdi
fl_o3_ct(:) = rmdi

! BVOC emission parameters
ALLOCATE( aef(npft))
ALLOCATE( ci_st(npft))
ALLOCATE( gpp_st(npft))
ALLOCATE( ief(npft))
ALLOCATE( mef(npft))
ALLOCATE( tef(npft))

aef(:)    = rmdi
ci_st(:)  = rmdi
gpp_st(:) = rmdi
ief(:)    = rmdi
mef(:)    = rmdi
tef(:)    = rmdi

! INFERNO combustion parameters
ALLOCATE( avg_ba(npft))
ALLOCATE( ccleaf_min(npft))
ALLOCATE( ccleaf_max(npft))
ALLOCATE( ccwood_min(npft))
ALLOCATE( ccwood_max(npft))
ALLOCATE( fire_mort(npft))

avg_ba(:)     = rmdi
ccleaf_min(:) = rmdi
ccleaf_max(:) = rmdi
ccwood_min(:) = rmdi
ccwood_max(:) = rmdi
fire_mort(:)  = rmdi

! INFERNO emission parameters
ALLOCATE( fef_bc(npft))
ALLOCATE( fef_ch4(npft))
ALLOCATE( fef_co(npft))
ALLOCATE( fef_co2(npft))
ALLOCATE( fef_nox(npft))
ALLOCATE( fef_oc(npft))
ALLOCATE( fef_so2(npft))
ALLOCATE( fef_c2h4(npft))
ALLOCATE( fef_c2h6(npft))
ALLOCATE( fef_c3h8(npft))
ALLOCATE( fef_mecho(npft))
ALLOCATE( fef_hcho(npft))
ALLOCATE( fef_nh3(npft))
ALLOCATE( fef_dms(npft))

fef_bc(:)   = rmdi
fef_ch4(:)  = rmdi
fef_co(:)   = rmdi
fef_co2(:)  = rmdi
fef_nox(:)  = rmdi
fef_oc(:)   = rmdi
fef_so2(:)  = rmdi
fef_c2h4(:) = rmdi
fef_c2h6(:) = rmdi
fef_c3h8(:) = rmdi
fef_mecho(:)= rmdi
fef_hcho(:) = rmdi
fef_nh3(:)  = rmdi
fef_dms(:)  = rmdi

! SUGAR parameters
ALLOCATE( sug_grec(npft))
ALLOCATE( sug_g0(npft))
ALLOCATE( sug_yg(npft))

sug_grec(:) = rmdi
sug_g0(:)   = rmdi
sug_yg(:)   = rmdi

! SOX parameters
ALLOCATE( sox_a(npft))
ALLOCATE( sox_p50(npft))
ALLOCATE( sox_rp_min(npft))

sox_a(:)      = rmdi
sox_p50(:)    = rmdi
sox_rp_min(:) = rmdi

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE pftparm_alloc

END MODULE pftparm
