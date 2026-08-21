! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module holds surface parameters for each Plant Functional Type (but
! not parameters that are only used by TRIFFID OR RED).



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


SUBROUTINE print_nlist_jules_pftparm()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('pftparm',                                                    &
    'Contents of namelist jules_pftparm')

#if !defined(UM_JULES)
WRITE(lineBuffer,*)' fsmc_mod = ',fsmc_mod
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' psi_close = ',psi_close
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' psi_open = ',psi_open
CALL jules_print('pftparm',lineBuffer)
#endif


WRITE(lineBuffer,*)' a_wl = ',a_wl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' a_ws = ',a_ws
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' act_jmax = ',act_jmax
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' act_vcmax = ',act_vcmax
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' aef = ',aef
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' albsnc_max = ',albsnc_max
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' albsnc_min = ',albsnc_min
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' albsnf_max = ',albsnf_max
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' alpha = ',alpha
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' alpha_elec = ',alpha_elec
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' alnir = ',alnir
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' alpar = ',alpar
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' avg_ba = ',avg_ba
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' b_wl = ',b_wl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' c3 = ',c3
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' can_struct_a = ',can_struct_a
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' catch0 = ',catch0
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ccleaf_max = ',ccleaf_max
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ccleaf_min = ',ccleaf_min
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ccwood_max = ',ccwood_max
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ccwood_min = ',ccwood_min
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ci_st = ',ci_st
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' dcatch_dlai = ',dcatch_dlai
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' deact_vcmax = ',deact_jmax
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' deact_vcmax = ',deact_vcmax
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' dfp_dcuo = ',dfp_dcuo
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' dgl_dm = ',dgl_dm
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' dgl_dt = ',dgl_dt
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' dqcrit = ',dqcrit
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ds_jmax = ',ds_jmax
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ds_vcmax = ',ds_vcmax
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' dust_veg_scj = ',dust_veg_scj
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' dz0v_dh = ',dz0v_dh
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' emis_pft = ',emis_pft
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' eta_sl = ',eta_sl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' f0 = ',f0
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fd = ',fd
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_bc = ',fef_bc
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_ch4 = ',fef_ch4
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_co = ',fef_co
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_co2 = ',fef_co2
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_nox = ',fef_nox
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_oc = ',fef_oc
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_so2 = ',fef_so2
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_c2h4 = ',fef_c2h4
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_c2h6 = ',fef_c2h6
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_c3h8 = ',fef_c3h8
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_hcho = ',fef_hcho
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_mecho = ',fef_mecho
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_nh3 = ',fef_nh3
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fef_dms = ',fef_dms
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fire_mort = ',fire_mort
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fl_o3_ct = ',fl_o3_ct
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fsmc_of = ',fsmc_of
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' fsmc_p0 = ',fsmc_p0
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' sug_g0 = ',sug_g0
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' g1_stomata = ',g1_stomata
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' g_leaf_0 = ',g_leaf_0
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' glmin = ',glmin
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' gpp_st = ',gpp_st
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' sug_grec = ',sug_grec
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' gsoil_f = ',gsoil_f
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' hw_sw = ',hw_sw
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ief = ',ief
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' infil_f = ',infil_f
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' jv25_ratio = ',jv25_ratio
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' kext = ',kext
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' kn = ',kn
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' knl = ',knl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' kpar = ',kpar
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' lai_alb_lim = ',lai_alb_lim
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' lma = ',lma
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' mef = ',mef
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' neff = ',neff
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' nl0 = ',nl0
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' nmass = ',nmass
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' nr_nl = ',nr_nl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' ns_nl = ',ns_nl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' nsw = ',nsw
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' nr = ',nr
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' omega = ',omega
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' omnir = ',omnir
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' orient = ',orient
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' q10_leaf = ',q10_leaf
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' r_grow = ',r_grow
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' rootd_ft = ',rootd_ft
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' sigl = ',sigl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' tef = ',tef
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' tleaf_of = ',tleaf_of
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' tlow = ',tlow
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' tupp = ',tupp
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' vint = ',vint
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' vsl = ',vsl
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' sug_yg = ',sug_yg
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' z0v = ',z0v
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' sox_a = ',sox_a
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' sox_p50 = ',sox_p50
CALL jules_print('pftparm',lineBuffer)
WRITE(lineBuffer,*)' sox_rp_min = ',sox_rp_min
CALL jules_print('pftparm',lineBuffer)

CALL jules_print('pftparm',                                                    &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_pftparm


SUBROUTINE check_jules_pftparm(npft,nnpft)

USE jules_soil_biogeochem_mod, ONLY: l_layeredC, soil_bgc_model,               &
                                      soil_model_4pool

USE jules_vegetation_mod, ONLY: can_rad_mod, l_crop, l_trait_phys,             &
                                 l_use_pft_psi, l_bvoc_emis,                   &
                                 l_o3_damage, photo_acclim_model,              &
                                 photo_act_model, photo_act_pft,               &
                                 photo_farquhar, photo_model,                  &
                                 stomata_jacobs, stomata_medlyn, stomata_sox,  &
                                 stomata_model, l_spec_veg_z0, l_sugar,        &
                                 l_scale_resp_pm

USE jules_inferno_mod, ONLY: l_inferno, l_trif_fire

USE jules_radiation_mod, ONLY: l_spec_albedo, l_albedo_obs, l_snow_albedo

USE missing_data_mod, ONLY: rmdi

USE ereport_mod,     ONLY: ereport
USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

!Arguments
INTEGER, INTENT(IN) :: npft, nnpft

! Work variables
INTEGER :: ERROR  ! Error indicator

CHARACTER(LEN=*), PARAMETER :: RoutineName='CHECK_JULES_PFTPARM'

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
! Some configurations don't need all parameters but in some cases these are
! still tested below.
!-----------------------------------------------------------------------------
ERROR = 0
#if !defined(UM_JULES)
! Trigger ignored in the UM for now use ifdef, but may be better regarding LFRic
! and other triggered off option to check the value if any > rmdi and then
! check if they should have a value in check_available_options or based on
! science options.
IF ( ANY( fsmc_mod(:) < 0 ) ) THEN  ! fsmc_mod was initialised to < 0
  ERROR = 1
  CALL jules_print(routinename, "No value for fsmc_mod")
END IF
#endif
IF ( ANY( orient(:) < 0 ) ) THEN  ! orient was initialised to < 0
  ERROR = 1
  CALL jules_print(routinename, "No value for orient")
END IF
IF ( ANY( ABS( kext(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for kext")
END IF
IF ( ANY( ABS( kpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for kpar")
END IF
IF ( ANY( ABS( lai_alb_lim(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for lai_alb_lim")
END IF
IF ( ANY( ABS( can_struct_a(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for can_struct_a")
END IF
IF ( ANY( ABS( gsoil_f(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for gsoil_f")
END IF
IF ( ANY( c3(:) < 0 ) ) THEN  ! c3 was initialised to < 0
  ERROR = 1
  CALL jules_print(routinename, "No value for c3")
END IF
IF ( ANY( ABS( alpha(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for alpha")
END IF
IF ( ANY( ABS( fd(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for fd")
END IF
IF ( ANY( ABS( nr_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for nr_nl")
END IF
IF ( ANY( ABS( ns_nl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for ns_nl")
END IF
IF ( ANY( ABS( r_grow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for r_grow")
END IF
! Note that tlow and tupp are always required, for some PFTs at least.
! If using the Farquhar model for C3 plants, we still need tlow and
! tupp for C4 plants.
IF ( ANY( ABS( tlow(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for tlow")
END IF
IF ( ANY( ABS( tupp(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for tupp")
END IF

SELECT CASE ( photo_model )
CASE ( photo_farquhar )
  !---------------------------------------------------------------------------
  ! First check parameters that are always required with this model.
  !---------------------------------------------------------------------------
  ! Note that these parameter values are not used for C4 plants, but
  ! here we're still checking that they have been provided.
  IF ( ANY( ABS( alpha_elec(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for alpha_elec")
  END IF
  IF ( ANY( ABS( deact_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for deact_jmax")
  END IF
  IF ( ANY( ABS( deact_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for deact_vcmax")
  END IF
  !---------------------------------------------------------------------------
  ! Check parameters that depend on any chosen acclimation model.
  !---------------------------------------------------------------------------
  IF ( photo_acclim_model == 0 ) THEN
    ! No acclimation.
    IF ( ANY( ABS( ds_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL jules_print(routinename, "No value for ds_jmax")
    END IF
    IF ( ANY( ABS( ds_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL jules_print(routinename, "No value for ds_vcmax")
    END IF
    IF ( ANY( ABS( jv25_ratio(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL jules_print(routinename, "No value for jv25_ratio")
    END IF
  END IF  !  photo_acclim_model == 0

  IF ( photo_acclim_model == 0 .OR.                                            &
      (photo_acclim_model /= 0 .AND. photo_act_model == photo_act_pft) ) THEN
    IF ( ANY( ABS( act_jmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL jules_print(routinename, "No value for act_jmax")
    END IF
    IF ( ANY( ABS( act_vcmax(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL jules_print(routinename, "No value for act_vcmax")
    END IF
  END IF  !  photo_acclim_model

END SELECT  !  photo_model

SELECT CASE ( stomata_model )
CASE ( stomata_jacobs )
  IF ( ANY( ABS( dqcrit(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for dqcrit")
  END IF
  IF ( ANY( ABS( f0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for f0")
  END IF
CASE ( stomata_medlyn )
  IF ( ANY( ABS( g1_stomata(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for g1_stomata")
  END IF
CASE ( stomata_sox )
  IF ( ANY( ABS( sox_p50(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for sox_p50")
  END IF
  IF ( ANY( ABS( sox_rp_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for sox_rp_min")
  END IF
  IF ( ANY( ABS( sox_a(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for sox_a")
  END IF
END SELECT

IF ( .NOT. l_spec_albedo .AND. can_rad_mod == 1 ) THEN
  ! These don't need to be set
ELSE
  IF ( ANY( ABS( alnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for alnir")
  END IF
  IF ( ANY( ABS( alpar(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for alpar")
  END IF
  IF ( ANY( ABS( omega(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for omega")
  END IF
  IF ( ANY( ABS( omnir(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for omnir")
  END IF
END IF

IF ( l_albedo_obs ) THEN
  IF ( ANY( ABS( alnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for alnirl")
  END IF
  IF ( ANY( ABS( alniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for alniru")
  END IF
  IF ( ANY( ABS( alparl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for alparl")
  END IF
  IF ( ANY( ABS( alparu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for alparu")
  END IF
  IF ( ANY( ABS( omegal(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for omegal")
  END IF
  IF ( ANY( ABS( omegau(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for omegau")
  END IF
  IF ( ANY( ABS( omnirl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for omnirl")
  END IF
  IF ( ANY( ABS( omniru(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for omniru")
  END IF
END IF

IF ( .NOT. l_spec_albedo ) THEN
  IF ( ANY( ABS( albsnf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for albsnf_max")
  END IF
  IF ( l_albedo_obs ) THEN
    IF ( ANY( ABS( albsnf_maxl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL jules_print(routinename, "No value for albsnf_maxl")
    END IF
    IF ( ANY( ABS( albsnf_maxu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
      ERROR = 1
      CALL jules_print(routinename, "No value for albsnf_maxu")
    END IF
  END IF
END IF

IF ( .NOT. l_snow_albedo ) THEN
  IF ( ANY( ABS( albsnc_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for albsnc_max")
  END IF
  IF ( ANY( ABS( albsnc_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for albsnc_min")
  END IF
END IF

IF (l_trait_phys) THEN
  IF ( ANY( ABS( lma(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for lma")
  END IF
  IF ( ANY( ABS( nmass(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for nmass")
  END IF
  IF ( ANY( ABS( vsl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for vsl")
  END IF
  IF ( ANY( ABS( vint(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for vint")
  END IF
  IF ( ANY( ABS( nr(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for nr")
  END IF
  IF ( ANY( ABS( nsw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for nsw")
  END IF
  IF ( ANY( ABS( hw_sw(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for hw_sw")
  END IF
ELSE
  IF ( ANY( ABS( neff(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for neff")
  END IF
  IF ( ANY( ABS( nl0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for nl0")
  END IF
  IF ( ANY( ABS( sigl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for sigl")
  END IF
END IF !l_trait_phys

IF ( ANY( ABS( kn(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for kn")
END IF
IF ( can_rad_mod == 6 .AND. ANY( ABS( knl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for knl")
END IF
IF ( ANY( ABS( q10_leaf(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for q10_leaf")
END IF
IF ( ANY( ABS( a_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for a_wl")
END IF
IF ( ANY( ABS( a_ws(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for a_ws")
END IF
IF ( ANY( ABS( b_wl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for b_wl")
END IF
IF ( ANY( ABS( eta_sl(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for eta_sl")
END IF
IF ( ANY( ABS( g_leaf_0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for g_leaf_0")
END IF
IF ( ANY( ABS( dgl_dm(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for dgl_dm")
END IF
IF ( ANY( ABS( fsmc_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for fsmc_of")
END IF
IF ( ANY( ABS( dgl_dt(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for dgl_dt")
END IF
IF ( ANY( ABS( tleaf_of(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for tleaf_of")
END IF
IF ( ANY( ABS( catch0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for catch0")
END IF
IF ( ANY( ABS( dcatch_dlai(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for dcatch_dlai")
END IF
IF ( ANY( ABS( infil_f(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for infil_f")
END IF
IF ( ANY( ABS( glmin(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for glmin")
END IF
IF ( .NOT. l_spec_veg_z0) THEN
  IF ( ANY( ABS( dz0v_dh(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for dz0v_dh")
  END IF
ELSE
  IF ( ANY( ABS( z0v(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for z0v")
  END IF
END IF
IF ( ANY( ABS( rootd_ft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for rootd_ft")
END IF

IF ( l_use_pft_psi ) THEN
  IF ( ANY( ABS( psi_close(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for psi_close")
  END IF
  IF ( ANY( ABS( psi_open(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for psi_open")
  END IF
ELSE
  IF ( ANY( ABS( fsmc_p0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fsmc_p0")
  END IF
END IF !l_use_pft_psi

IF ( l_bvoc_emis ) THEN
  IF ( ANY( ABS( ci_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for ci_st")
  END IF
  IF ( ANY( ABS( gpp_st(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for gpp_st")
  END IF
  IF ( ANY( ABS( ief(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for ief")
  END IF
  IF ( ANY( ABS( tef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for tef")
  END IF
  IF ( ANY( ABS( mef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for mef")
  END IF
  IF ( ANY( ABS( aef(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for aef")
  END IF
END IF

IF ( l_inferno ) THEN
  IF ( ANY( ABS( fef_co2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_co2")
  END IF
  IF ( ANY( ABS( fef_co(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_co")
  END IF
  IF ( ANY( ABS( fef_ch4(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_ch4")
  END IF
  IF ( ANY( ABS( fef_nox(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_nox")
  END IF
  IF ( ANY( ABS( fef_so2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_so2")
  END IF
  IF ( ANY( ABS( fef_c2h4(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_c2h4")
  END IF
  IF ( ANY( ABS( fef_c2h6(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_c2h6")
  END IF
  IF ( ANY( ABS( fef_c3h8(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_c3h8")
  END IF
  IF ( ANY( ABS( fef_hcho(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_hcho")
  END IF
  IF ( ANY( ABS( fef_mecho(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_mecho")
  END IF
  IF ( ANY( ABS( fef_nh3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_nh3")
  END IF
  IF ( ANY( ABS( fef_dms(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fef_dms")
  END IF
  IF ( ANY( ABS( ccleaf_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for ccleaf_min")
  END IF
  IF ( ANY( ABS( ccleaf_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for ccleaf_max")
  END IF
  IF ( ANY( ABS( ccwood_min(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for ccwood_min")
  END IF
  IF ( ANY( ABS( ccwood_max(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for ccwood_max")
  END IF
  IF ( ANY( ABS( avg_ba(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for avg_ba")
  END IF
END IF

IF ( l_trif_fire ) THEN
  IF ( ANY( ABS( fire_mort(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fire_mort")
  END IF
END IF

IF ( l_o3_damage ) THEN
  IF ( ANY( ABS( fl_o3_ct(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for fl_o3_ct")
  END IF
  IF ( ANY( ABS( dfp_dcuo(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for dfp_dcuo")
  END IF
END IF

IF ( l_sugar ) THEN
  IF ( ANY( ABS( sug_g0(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for sug_g0")
  END IF
  IF ( ANY( ABS( sug_grec(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for sug_grec")
  END IF
  IF ( ANY( ABS( sug_yg(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL jules_print(routinename, "No value for sug_yg")
  END IF
END IF

IF ( ANY( ABS( emis_pft(:) - rmdi ) < EPSILON(1.0) ) ) THEN
  ERROR = 1
  CALL jules_print(routinename, "No value for emis_pft")
END IF

IF ( ERROR /= 0 ) THEN
  CALL ereport(routinename, ERROR,                                             &
                 ": Variable(s) missing from namelist - see earlier " //       &
                 "message(s)")
END IF

!******************************************************************************
! Do we want this in the UM & LFRic???
!-----------------------------------------------------------------------------
! Check that glmin is >0.
! This ensures that wt_ext in subroutine soil_evap cannot become a NaN (which
! it would if gs=glmin and gsoil=0), or blow up, and might well be required
! elsewhere too.
!-----------------------------------------------------------------------------
ERROR = 0
IF ( ANY(glmin < 1.0e-10) ) THEN
  ERROR = -1
  CALL ereport(routinename, ERROR,                                             &
               "Increasing one or more values of glmin - very small " //       &
               "values can cause model to blow up or NaNs")
  WHERE ( glmin < 1.0e-10 )
    glmin = 1.0e-10
  END WHERE
END IF

IF ( l_crop ) THEN
  IF ( ANY( ABS( a_ws(nnpft+1: npft) - 1.0 ) > EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL ereport(routinename, ERROR, "crop tiles should have a_ws=1.0")
  END IF
END IF

IF ( l_use_pft_psi ) THEN
  IF ( ANY( psi_close(1: npft) > EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL ereport(routinename, ERROR, "psi_close should be negative")
  END IF
  IF ( ANY( psi_open(1: npft) > EPSILON(1.0) ) ) THEN
    ERROR = 1
    CALL ereport(routinename, ERROR, "psi_open should be negative")
  END IF
END IF

!-----------------------------------------------------------------------------
! fsmc_mod=1 should not be allowed with a layered 4-pool C model until this has
! been properly evaluated. (With fsmc_mod=1, subroutine root_frac does not
! return the exponential root profile that users might expect.)
!-----------------------------------------------------------------------------
IF ( l_layeredC .AND. ( soil_bgc_model == soil_model_4pool ) .AND.             &
     ANY( fsmc_mod(:) == 1 ) ) THEN
  ERROR = 1
  CALL ereport(routinename, ERROR,                                             &
               "fsmc_mod=1 is not allowed with l_layeredC and 4-pool C model")
END IF

!-----------------------------------------------------------------------------
! stomata_model = stomata_sox must be used with fsmc_mod = 1
! Cannot be run with l_scale_resp_pm
! Must be run with can_rad_mod = 1 (implementation for can_rad_mod = 6 ongoing)
!-----------------------------------------------------------------------------
IF ( stomata_model == stomata_sox ) THEN ! SOX
  IF ( l_scale_resp_pm ) THEN
    ERROR = 1
    CALL ereport(routinename, ERROR,                                           &
    'l_scale_resp_pm=T is incompatible with SOX (stomata_model=3)')
  END IF

  IF ( .NOT. ANY ( fsmc_mod(:) == 1 ) ) THEN
    ERROR = 1
    CALL ereport(routinename, ERROR,                                           &
    'SOX (stomata_model=3) must be used with fsmc_mod = 1')
  END IF

  IF ( .NOT. ( can_rad_mod == 1 ) ) THEN
    ERROR = 1
    CALL ereport(routinename, ERROR,                                           &
    'SOX (stomata_model=3) must be used with can_rad_mod = 1')
  END IF

  IF ( .NOT. ( photo_model == 3 ) ) THEN
    ERROR = 1
    CALL ereport(routinename, ERROR,                                           &
    'SOX (stomata_model=3) uses the SOX derivation of Collatz (photo_model=3)')
  END IF
END IF

END SUBROUTINE check_jules_pftparm

END MODULE pftparm
