! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE jules_ssi_sf_explicit_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
                  ModuleName='JULES_SSI_SF_EXPLICIT_MOD'

CONTAINS
!  SUBROUTINE JULES_SSI_SF_EXPLICIT ----------------------------------
!
!  Purpose: Calculate explicit surface fluxes of heat, moisture and
!           momentum over sea and sea-ice. Also calculates surface
!           exchange coefficients required for implicit update of
!           surface fluxes and surface information required by the
!           explicit boundary layer routine
!
!
!  Documentation: UMDP 24.
!
!---------------------------------------------------------------------
!    Arguments :-
SUBROUTINE jules_ssi_sf_explicit (                                             &
! IN values defining field dimensions and subset to be processed :
 nice, nice_use, n_wtrac_jls,                                                  &
! IN parameters required from boundary-layer scheme :
 bq_1,bt_1,z1_uv,z1_uv_top,z1_tq,z1_tq_top,qw_1,tl_1,                          &
! IN soil/vegetation/land surface data :
 flandg,                                                                       &
! IN sea/sea-ice data :
 ice_fract_cat,k_sice,                                                         &
 z0m_sice_fmd, z0m_sice_skin,                                                  &
! IN input data from the wave model
 charnock_w,                                                                   &
! IN everything not covered so far :
 pstar,lw_down,zh,ddmfx,l_mr_physics,ti,ti_cat,                                &
 tstar_sea,tstar_sice_cat,z_land,                                              &
! IN idealised and SCM things
 l_spec_z0, z0m_scm, z0h_scm,                                                  &
! IN calculated in land code
 l_cdr10m_snow,                                                                &
! INOUT diagnostics
 sf_diag,                                                                      &
! INOUT data :
 z0msea,rhostar,fqw_1,ftl_1,t1_sd,q1_sd,cdr10m,                                &
! OUT Diagnostic not requiring STASH flags :
 recip_l_mo_sea,radnet_sice,                                                   &
! OUT variables for message passing
 rhokm_ssi,                                                                    &
! OUT data required elsewhere in boundary layer or surface code
 alpha1_sea,alpha1_sice,ashtf_prime,ashtf_prime_sea,fqw_ice,ftl_ice,           &
 rhokh_1_sice_ncats,rhokh_sea,dtstar_sea,dtstar_sice,                          &
 chr1p5m_ssi_mean,vshr_ssi,                                                    &
! OUT required for classic aerosols
 cd_ssi,ch_ssi,rhokh_1_sice,rib_sea,z0h_sea,                                   &
 rib_ice,z0m_ice,z0h_ice,ice_fract,                                            &
 !New arguments replacing USE statements
 !ancil_info (IN)
 ssi_index, sea_index, fssi_ij, sea_frac, sice_index_ncat, sice_frac_ncat,     &
 sice_pts_ncat, ssi_pts, sea_pts,                                              &
 !fluxes (IN)
 sw_sicat, sw_sea,                                                             &
 !Water tracers (IN)
 qw_1_wtrac, r_sea_wtrac, r_sice_wtrac,                                        &
 !Water tracers (INOUT)
 fqw_1_wtrac,                                                                  &
 !Water tracers (OUT)
 fqw_sea_wtrac, fqw_sicat_wtrac)

USE atm_fields_bounds_mod,   ONLY: pdims_s, pdims, tdims
USE c_kappai,                ONLY: kappa_seasurf,dzsea
USE calc_air_dens_mod,       ONLY: calc_air_dens
USE csigma,                  ONLY: sbcon
USE fcdch_mod,               ONLY: fcdch
USE ice_formdrag_lupkes_mod, ONLY: ice_formdrag_lupkes
USE planet_constants_mod,    ONLY: g, vkman, r, c_virtual,epsil=>repsilon
USE qsat_mod,                ONLY: qsat, qsat_mix
USE sf_diags_mod,            ONLY: strnewsfdiag
USE sf_flux_mod,             ONLY: sf_flux
USE sf_rib_mod,              ONLY: sf_rib
USE sfl_int_mod,             ONLY: sfl_int
USE stdev1_mod,              ONLY: stdev1
USE theta_field_sizes,       ONLY: t_i_length,t_j_length
USE timestep_mod,            ONLY: timestep
USE water_constants_mod,     ONLY: lc, rhosea, tm

USE jules_surface_mod, ONLY: on, off, i_modiscopt, cor_mo_iter,                &
                              iscrntdiag, ls,                                  &
                              IP_ScrnDecpl2, IP_ScrnDecpl3

USE jules_science_fixes_mod, ONLY: ctile_orog_fix, l_accurate_rho

USE jules_sea_seaice_mod, ONLY: l_ctile, charnock,                             &
                                 SeaSalinityFactor, iseasurfalg, ip_ss_solid,  &
                                 ip_ss_fixed, ip_ss_surf_div,                  &
                                 ip_ss_surf_div_coupled,                       &
                                 z0miz, z0sice, z0h_z0m_miz,                   &
                                 z0h_z0m_sice, z0hsea,                         &
                                 emis_sea, emis_sice, hcap_sea,                &
                                 l_iceformdrag_lupkes, beta_evap,              &
                                 l_icerough_prognostic,                        &
                                 ip_hwdrag_limited, ip_hwdrag_reduced_v1,      &
                                 i_high_wind_drag, cdn_max_sea, cdn_hw_sea,    &
                                 u_cdn_max, u_cdn_hw, z_10m, l_use_dtstar_sea

USE jules_water_tracers_mod, ONLY: l_wtrac_jls
USE sf_flux_ssi_wtrac_mod,   ONLY: sf_flux_ssi_wtrac

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE
!-----------------------------------------------------------------------
!  Inputs :-
!-----------------------------------------------------------------------
! (a) Defining horizontal grid and subset thereof to be processed.
!    Checked for consistency.
INTEGER, INTENT(IN) ::                                                         &
 ssi_pts, sea_pts,                                                             &
 nice                                                                          &
            ! Total number of sea ice categories
,nice_use                                                                      &
            ! No. of sea ice categories used fully in surface calculations
,n_wtrac_jls
            ! No. of water tracers used in JULES
! Defining vertical grid of model atmosphere.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 bq_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! IN A buoyancy parameter
                             !    (beta q tilde).
,bt_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! IN A buoyancy parameter
                             !    (beta T tilde).
,z1_uv(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! IN Height of lowest uv level (m).
,z1_tq(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! IN Height of lowest tq level (m).
                             !    Note, if the grid used is
                             !    staggered in the vertical,
                             !    Z1_UV and Z1_TQ can be
                             !    different.
,qw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                     &
                             ! IN Total water content
,tl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Ice/liquid water temperature

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  charnock_w(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
! Charnock's coefficient from wave model

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    z1_uv_top(tdims%i_start:tdims%i_end,                       &
                              tdims%j_start:tdims%j_end)
                             ! Height of top of lowest uv-layer
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
                    z1_tq_top(tdims%i_start:tdims%i_end,                       &
                              tdims%j_start:tdims%j_end)
                             ! Height of top of lowest Tq-layer

! (c) Soil/vegetation/land surface parameters (mostly constant).
LOGICAL, INTENT(IN) ::                                                         &
 l_spec_z0
                             ! IN T if using prescribed
                             !    sea surface roughness lengths

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Land fraction on all tiles.
                             !    divided by 2SQRT(2) on land
                             !    points only (m)
! (d) Sea/sea-ice data.
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 ice_fract_cat(tdims%i_start:tdims%i_end,                                      &
               tdims%j_start:tdims%j_end,nice_use)                             &
                       ! IN Fraction of gridbox covered by
                       !    category sea-ice (decimal fraction).
                       !    If nice_use=1, this is the sum of the categories
,k_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use)          &
                       ! IN 2 * Sea ice thermal conductivity divided by
                       !    surface layer thickness  (in coupled mode, this
                       !    is passed in from the sea ice model) (W/m2/K)
,z0m_sice_fmd(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end)            &
                       ! IN prognostic sea ice form drag roughness length
,z0m_sice_skin(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end)
                       ! IN prognostic sea ice skin roughness length

! (f) Atmospheric + any other data not covered so far, incl control.

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 pstar(pdims%i_start:pdims%i_end,pdims%j_start:pdims%j_end)                    &
                             ! IN Surface pressure (Pascals).
,lw_down(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! IN Surface downward LW radiation
                             !    (W/m2).
,zh(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                       &
                             ! IN Height above surface of top of
                             !    boundary layer (metres).
,ddmfx(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! IN Convective downdraught
                             !    mass-flux at cloud base
,ti(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                       &
                             ! IN Sea-ice surface layer
                             !    temperature (K).
,ti_cat(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)              &
,tstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! IN Open sea surface temperature (K)
,tstar_sice_cat(tdims%i_start:tdims%i_end,                                     &
                tdims%j_start:tdims%j_end,nice_use)                            &
                             ! IN Sea-ice surface temperature (K).
,z_land(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! IN Land height (m).
,z0m_scm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! IN Fixed Sea-surface roughness
                             !    length for momentum (m).(SCM)
,z0h_scm(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! IN Fixed Sea-surface roughness
                             !    length for heat (m). (SCM)
                             !    momentum for aerosol deposition.
LOGICAL, INTENT(IN) ::                                                         &
 l_cdr10m_snow
                             ! IN Flag indicating if cdr10m
                             !    (an interpolation coefficient) is
                             !    to be calculated for use with
                             !    snow unloading.

LOGICAL, INTENT(IN) ::                                                         &
 l_mr_physics
                             ! IN Switch for when mixing ratios are used
!-----------------------------------------------------------------------
!  In/outs :-
!-----------------------------------------------------------------------
!Diagnostics
TYPE (strnewsfdiag), INTENT(IN OUT) :: sf_diag

REAL(KIND=real_jlslsm), INTENT(IN OUT) ::                                      &
 z0msea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! INOUT Sea-surface roughness
                             !       length for momentum (m).
,rhostar(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! INOUT Surface air density
,fqw_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT Moisture flux between layers
                             !       (kg per square metre per sec).
                             !       FQW(,1) is total water flux
                             !       from surface, 'E'.
,ftl_1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT FTL(,K) contains net turbulent
                             !       sensible heat flux into layer K
                             !       from below; so FTL(,1) is the
                             !       surface sensible heat, H.(W/m2)
,t1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT Standard deviation of turbulent
                             !       fluctuations of layer 1 temp;
                             !       used in initiating convection.
,q1_sd(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! INOUT Standard deviation of turbulent
                             !       fluxes of layer 1 humidity;
                             !       used in initiating convection.
,cdr10m(pdims_s%i_start:pdims_s%i_end,pdims_s%j_start:pdims_s%j_end)           &
,vshr_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! INOUT Magnitude of surface-to-lowest
                             !       atm level wind shear (m per s).

!-----------------------------------------------------------------------
!  Outputs :-
!-----------------------------------------------------------------------
!-1 Diagnostic (or effectively so - includes coupled model requisites):-
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 recip_l_mo_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)           &
                             ! OUT Reciprocal of the surface
                             !     Obukhov  length at sea
                             !     points. (m-1).
,radnet_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,              &
             nice_use)
                             ! OUT Surface net radiation on
                             !     sea-ice (W/m2)

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 rhokm_ssi(pdims_s%i_start:pdims_s%i_end,                                      &
           pdims_s%j_start:pdims_s%j_end)

!  (b) Not passed between lower-level routines (not in workspace at this
!      level) :-

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 alpha1_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! OUT ALPHA1 for sea.
,alpha1_sice(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! OUT ALPHA1 for sea-ice.
,ashtf_prime(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! OUT Adjusted SEB coefficient for sea-ice
,ashtf_prime_sea(tdims%i_start:tdims%i_end,                                    &
                 tdims%j_start:tdims%j_end)                                    &
                             ! OUT Adjusted SEB coefficient for sea
,fqw_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)                                   &
                             ! OUT Surface FQW for sea-ice
,ftl_ice(tdims%i_start:tdims%i_end,                                            &
         tdims%j_start:tdims%j_end,nice_use)                                   &
                             ! OUT Surface FTL for sea-ice
,rhokh_1_sice_ncats(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,       &
                                                        nice_use)              &
                             ! OUT Surface exchange coefficients
                             !     for sea-ice
,rhokh_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! OUT Surface exchange coefficients
                             !     for sea
,dtstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! OUT Change is TSTAR over timestep
                             !     for open sea
,dtstar_sice(tdims%i_start:tdims%i_end,                                        &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! OUT Change is TSTAR over timestep
                             !     for sea-ice
,chr1p5m_ssi_mean(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT CHR1P5M for sea and sea-ice
                             !     (leads ignored).

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 cd_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! OUT Bulk transfer coefficient for
                             !     momentum over sea mean.
,ch_ssi(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! OUT Bulk transfer coefficient for heat
                             !     and/or moisture over sea mean.
,rhokh_1_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)             &
                             ! OUT Surface exchange coefficient for
                             !     sea-ice.
,rib_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! OUT Bulk Richardson number
,z0h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! OUT Roughness length for heat and
                             !     moisture transport

REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
 rib_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                  &
                                                        nice_use)              &
                             ! OUT Bulk Richardson number
,z0m_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                  &
                                                        nice_use)              &
                             ! OUT Momentum Roughness length.
,z0h_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                  &
                                                        nice_use)              &
                             ! OUT Thermal Roughness length.
,ice_fract(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Sea ice fraction summed over all cats

!New arguments replacing USE statements
!ancil_info (IN)
INTEGER, INTENT(IN) ::                                                         &
 ssi_index(t_i_length * t_j_length),                                           &
 sea_index(t_i_length * t_j_length),                                           &
 sice_index_ncat(t_i_length * t_j_length,nice),                                &
 sice_pts_ncat(nice)

REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 fssi_ij(t_i_length,t_j_length),                                               &
 sea_frac(t_i_length * t_j_length),                                            &
 sice_frac_ncat(t_i_length * t_j_length,nice)


!fluxes (IN)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
 sw_sicat(t_i_length * t_j_length, nice_use),                                  &
 sw_sea(t_i_length * t_j_length)

! Water tracers (IN)
REAL(KIND=real_jlslsm), INTENT(IN) :: qw_1_wtrac(tdims%i_start:tdims%i_end,    &
                                         tdims%j_start:tdims%j_end,n_wtrac_jls)
                         ! IN Total water tracer content in lowest atmos level
REAL(KIND=real_jlslsm), INTENT(IN) :: r_sea_wtrac(tdims%i_start:tdims%i_end,   &
                                         tdims%j_start:tdims%j_end,n_wtrac_jls)
                         ! IN Open ocean surface water tracer ratio
REAL(KIND=real_jlslsm), INTENT(IN) :: r_sice_wtrac(tdims%i_start:tdims%i_end,  &
                                                   tdims%j_start:tdims%j_end,  &
                                                   nice_use,n_wtrac_jls)
                         ! IN Sea ice surface water tracer ratio per ice cat

! Water tracers (INOUT)
REAL(KIND=real_jlslsm), INTENT(IN OUT) :: fqw_1_wtrac(                         &
                                            tdims%i_start:tdims%i_end,         &
                                            tdims%j_start:tdims%j_end,         &
                                            n_wtrac_jls)
                         ! INOUT Surface water tracer moisture flux (kg/m2/s)

! Water tracers (OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) :: fqw_sea_wtrac(                          &
                                            tdims%i_start:tdims%i_end,         &
                                            tdims%j_start:tdims%j_end,         &
                                            n_wtrac_jls)
                         ! OUT Surface water tracer evaporation from sea
                         !     (kg/m2/s)

REAL(KIND=real_jlslsm), INTENT(OUT) :: fqw_sicat_wtrac(                        &
                                            tdims%i_start:tdims%i_end,         &
                                            tdims%j_start:tdims%j_end,         &
                                            nice_use,n_wtrac_jls)
                         ! OUT Surface water tracer sublimation from sea ice
                         !     categories (kg/m2/s)

!-----------------------------------------------------------------------
! LOCAL variables
!-----------------------------------------------------------------------
!  Workspace :-
REAL(KIND=real_jlslsm) ::                                                      &
 radnet_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                             ! Surface net radiation on open sea (W/m2)

REAL(KIND=real_jlslsm) ::                                                      &
 qs1(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                      &
                             ! Sat. specific humidity
                             ! qsat(TL_1,PSTAR)
,rhokm_ssi_nohalo(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)         &
                             ! like RHOKM_SSI, but with no halo
,lh0                         ! Latent heat for snow free surface
                             !   =LS for sea-ice, =LC otherwise

!  Workspace for sea and sea-ice leads
REAL(KIND=real_jlslsm) ::                                                      &
 cd_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! Drag coefficient
,ch_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! Transfer coefficient for heat and
                             ! moisture
,qstar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! Surface saturated sp humidity
,rhostar_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                             ! Surface air density
,rhostarmom_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)           &
                             ! Surface air density
,db_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! Buoyancy difference for sea points
,v_s_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! Surface layer scaling velocity
,hcons_sea(t_i_length * t_j_length)                                            &
                             ! Heat conductivity into sea
,canhc_sea(t_i_length * t_j_length)                                            &
                             ! Heat capacity for sea
,u_s_std_sea(t_i_length * t_j_length)                                          &
                             ! Surface friction velocity for sea
                             ! (dummy variable for sea)
,v_s_std_sea(t_i_length * t_j_length)                                          &
                             ! Surface layer scaling velocity
                             ! for sea excluding orographic
                             ! form drag (m/s).
                             ! (dummy variable for sea)
,cd_std_sea(t_i_length * t_j_length)
                             ! Local drag coefficient for calc
                             ! of interpolation coefficient
                             ! (dummy variable for sea)
                             ! for sea points (m/s).

REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 rhokm_1_sice(:,:)                                                             &
                             ! Surface momentum exchange coefficient
                             ! for sea-ice.
,rhokm_1_sice_ncats(:,:,:)                                                     &
                             ! Surface momentum exchange coefficient
                             ! for sea-ice.
,rhokm_1_sea(:,:)
                             ! Surface momentum exchange coefficient
                             ! for sea.


!  Workspace for sea-ice and marginal ice zone
REAL(KIND=real_jlslsm) ::                                                      &
 cd_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                   &
                                                        nice_use)              &
                             ! Drag coefficient
,cd_ice_mean(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                             ! Gridbox aggregate sea ice drag coefficient
,cd_miz(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! Drag coefficient
,ch_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                   &
                                                        nice_use)              &
                             ! Transfer coefficient for heat and
                             ! moisture
,chr1p5m_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,             &
                                                      nice_use)                &
                             ! CHR1P5M for sea ice, on categories
                             ! (leads ignored).
,chr1p5m_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                             ! CHR1P5M for sea
                             ! (leads ignored).
,ch_ice_mean(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
!                            ! Gridbox aggregate transfer
!                            ! coefficient for heat and moisture
,ch_miz(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                             ! Transfer coefficient for heat and
                             ! moisture
,qstar_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                &
                             ! Surface saturated sp humidity
,qstar_ice_cat(tdims%i_start:tdims%i_end,                                      &
               tdims%j_start:tdims%j_end,nice_use)                             &
                             ! Sea-ice surface sat sp humidity on cats
,qv_star(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! Surface air humidity
,rhostar_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                             ! Surface air density
,rhostarmom_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)           &
                             ! Surface air density
,z0m_miz(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! Momentum Roughness length.
,z0h_miz(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! Thermal Roughness length.
,db_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                   &
                                                        nice_use)              &
                             ! Buoyancy difference for sea ice
,db_ice_mean(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)              &
                             ! Gridbox mean of db_ice
,v_s_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,                  &
                                                        nice_use)              &
                             ! Surface layer scaling velocity
                             ! for sea ice (m/s).
,v_s_miz(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                             ! Surface layer scaling velocity
                             ! for marginal sea ice (m/s).
,recip_l_mo_ice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,           &
                                                           nice_use)           &
                             ! Reciprocal of the Monin-Obukhov
                             ! length for sea ice (m^-1).
,recip_l_mo_miz(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)           &
                             ! Reciprocal of the Monin-Obukhov
                             ! length for marginal sea ice (m^-1).
,u_s_std_ice(t_i_length * t_j_length,nice_use)                                 &
                             ! Surface friction velocity for sea-i
                             ! (dummy variable for sea-ice)
,u_s_std_miz(t_i_length * t_j_length)                                          &
                             ! Surface friction velocity for
                             ! marginal sea-ice
                             ! (dummy variable for marginal sea-ic
,v_s_std_ice(t_i_length * t_j_length,nice_use)                                 &
                             ! Surface layer scaling velocity
                             ! for sea-ice excluding orographic
                             ! form drag (m/s).
                             ! (dummy variable for sea-ice)
,v_s_std_miz(t_i_length * t_j_length)                                          &
                             ! Surface layer scaling velocity
                             ! for marginal sea-ice excluding
                             ! orographic form drag (m/s).
                             ! (dummy variable for marginal sea-ic
,cd_std_ice(t_i_length * t_j_length,nice_use)                                  &
                             ! Local drag coefficient for calc
                             ! of interpolation coefficient
                             ! (dummy variable for sea-ice)
,cd_std_miz(t_i_length * t_j_length)                                           &
                             ! Local drag coefficient for calc
                             ! of interpolation coefficient
                             ! (dummy variable for marginal sea-ic
,epot_sea(t_i_length * t_j_length)                                             &
                             ! Potential evaporation from
                             ! sea surface
                             ! (dummy variable for sea surface)
,epot_ice(t_i_length * t_j_length)                                             &
                             ! Potential evaporation from sea-ice
                             ! (dummy variable for sea-ice)
,ashtf(tdims%i_start:tdims%i_end,                                              &
             tdims%j_start:tdims%j_end,nice_use)                               &
                             ! OUT Coefficient to calculate
                             !     surface heat flux into sea-ice.
,ashtf_sea(tdims%i_start:tdims%i_end,                                          &
                 tdims%j_start:tdims%j_end)                                    &
                             ! OUT Coefficient to calculate
                             !     surface heat flux into sea.
! The next fields are temporary and used while the option is still
! available for sea ice exchange coeffs not to be calculated per
! category.  This functionality may be removed in future model versions.
,tstar_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)               &
                             ! Sea ice sfc T averaged over all cats
,sice_frac_use(ssi_pts)
                             ! Sea ice fractions

! The next fields are temporary and used while the option is still
! available for sea ice exchange coeffs not to be calculated per
! category.  This functionality may be removed in future model versions.
INTEGER::                                                                      &
 sice_pts_use,                                                                 &
 sice_index_use(ssi_pts)

REAL(KIND=real_jlslsm) ::                                                      &
 z1_tq_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end),               &
!                            ! Height of lowest model level
!                            ! relative to sea.
 z1_tq_ctile(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                            ! Height of lowest model level
!                            ! relative to sea dependent on coastal
!                            ! tiling option.
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
 z1_tq_top_sea(:,:)                                                            &
                             ! Top of lowest Tq layer over sea
,z1_tq_top_ctile(:,:)                                                          &
                             ! Top of lowest Tq layer over sea
                             ! dependent on coastal tiling option
,chr10m_sice(:,:,:)                                                            &
                             ! CHR10M for sea ice, on categories
                             ! (leads ignored).
,chr10m_sea(:,:)
                             ! CHR10M for sea
                             ! (leads ignored).

!  Workspace for land tiles
REAL(KIND=real_jlslsm) ::                                                      &
 e_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! Evaporation from sea times leads
                             ! fraction (kg/m2/s). Zero over land.
,h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                    &
                             ! Surface sensible heat flux over sea
                             ! times leads fraction (W/m2).
                             ! Zero over land.
,dzssi(t_i_length * t_j_length)                                                &
                             ! Surface layer thickness
                             ! (sea) (m)
,dzdummy(t_i_length * t_j_length)
                             ! Dummy field for surface layer thickness
                             ! (sea-ice) (m)

! dummy arrays required for sea and se-ice to create universal
! routines for all surfaces
REAL(KIND=real_jlslsm) ::                                                      &
 array_zero(t_i_length * t_j_length)                                           &
                             ! Array of zeros
,array_one(t_i_length * t_j_length)                                            &
                             ! Array of ones

,array_emis(t_i_length * t_j_length)                                           &
                             ! Emissivity expanded to an array
,zdt_dummy(t_i_length * t_j_length)
                             ! Dummy array for zdt

LOGICAL ::                                                                     &
 array_false(t_i_length * t_j_length)
                             ! Array of .FALSE.
INTEGER ::                                                                     &
 array_zero_int(t_i_length * t_j_length)    ! Array of zeros


!  Local scalars :-

INTEGER ::                                                                     &
 i,j                                                                           &
             ! Loop counter (horizontal field index).
,k                                                                             &
             ! Loop counter (tile field index).
,l                                                                             &
             ! Loop counter (land point field index).
,n                                                                             &
             ! Loop counter (tile index).
,jits                                                                          &
             ! Counter for iteration for Z0H
,first_counter                                                                 &
             ! Used for setting sea ice fields
,i_wt
             ! Loop counter (water tracers)

REAL(KIND=real_jlslsm) ::                                                      &
 d_t                                                                           &
             ! Temporary in calculation of alpha1.
,tau                                                                           &
             ! Magnitude of surface wind stress over sea.
,ustr_l                                                                        &
             ! Low-wind estimate of friction velocity
,ustr_n                                                                        &
             ! Neutral estimate of friction velocity
,tol_ustr_n                                                                    &
             ! Tolerance for USTR_N
,tol_ustr_l                                                                    &
             ! Tolerance for USTR_L (see below)
,z0msea_max                                                                    &
             ! Sea roughness at maximum neutral drag
,u10n                                                                          &
             ! Neutral wind speed at 10 m.
,cdn_lim_loc
             ! Local limiting value of the neutral drag coefficient


LOGICAL, PARAMETER :: l_vegdrag_ssi = .FALSE.
             ! Logical to indicate that the canopy drag scheme cannot
             ! be applied at sea or sea-ice points

REAL(KIND=real_jlslsm) :: sea_point

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='JULES_SSI_SF_EXPLICIT'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!-----------------------------------------------------------------------
! Sea and sea-ice surface calculations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  0. Initialisations
!-----------------------------------------------------------------------

IF (sf_diag%l_tau_surft .OR. sf_diag%l_tau_1) THEN
  ALLOCATE(rhokm_1_sice(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end))
  ALLOCATE(rhokm_1_sice_ncats(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice_use))
  ALLOCATE(rhokm_1_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE(rhokm_1_sice(1,1))
  ALLOCATE(rhokm_1_sice_ncats(1,1,nice_use))
  ALLOCATE(rhokm_1_sea(1,1))
END IF

IF (i_modiscopt == on) THEN
  ALLOCATE(z1_tq_top_sea(t_i_length,t_j_length))
  ALLOCATE(z1_tq_top_ctile(t_i_length,t_j_length))
ELSE
  ALLOCATE(z1_tq_top_sea(1,1))
  ALLOCATE(z1_tq_top_ctile(1,1))
END IF

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(I, j, k, l, n, i_wt)                      &
!$OMP SHARED(t_i_length,t_j_length,array_zero,array_one,array_false,           &
!$OMP array_zero_int,zdt_dummy,cd_std_ice,v_s_std_ice,u_s_std_ice,             &
!$OMP        tdims,radnet_sea,radnet_sice,z0h_sea,z0hsea,z0m_miz,z0miz,        &
!$OMP        z0h_miz,z0h_z0m_miz,rib_sea,rib_ice,db_sea,db_ice,rhokm_ssi,      &
!$OMP        rhokm_ssi_nohalo,cd_ssi,ch_ssi,ftl_ice,fqw_ice,h_sea,e_sea,       &
!$OMP        cd_ice,ch_ice,v_s_ice,recip_l_mo_ice,l_icerough_prognostic,       &
!$OMP        z0m_ice,z0m_sice_fmd,z0h_ice,z0h_z0m_sice,z0m_sice_skin,z0sice,   &
!$OMP        lw_down,sice_pts_ncat,sice_index_ncat,ssi_index,                  &
!$OMP        sw_sicat,emis_sice,tstar_sice_cat,nice_use, sf_diag,              &
!$OMP        sea_pts, sea_index, sw_sea, emis_sea, tstar_sea,                  &
!$OMP        z1_tq_sea,z1_tq,z1_tq_ctile,flandg,z_land,l_ctile,                &
!$OMP        ctile_orog_fix,z1_tq_top_sea,z1_tq_top,z1_tq_top_ctile,           &
!$OMP        i_modiscopt,n_wtrac_jls,fqw_sea_wtrac,fqw_sicat_wtrac,l_wtrac_jls)
!$OMP DO SCHEDULE(STATIC)
DO i = 1,t_i_length * t_j_length
  array_zero(i)     = 0.0
  array_one(i)      = 1.0
  array_false(i)    = .FALSE.
  array_zero_int(i) = 0
  zdt_dummy(i)      = 0.0
END DO
!$OMP END DO NOWAIT
DO j = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
  DO i = 1,t_i_length * t_j_length
    cd_std_ice(i,j)   = 0.0
    v_s_std_ice(i,j)  = 0.0
    u_s_std_ice(i,j)  = 0.0
  END DO
!$OMP END DO NOWAIT
END DO

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    radnet_sea(i,j)       = 0.0
    z0h_sea(i,j)          = z0hsea
    z0m_miz(i,j)          = z0miz
    z0h_miz(i,j)          = z0h_z0m_miz * z0m_miz(i,j)
    rib_sea(i,j)          = 0.0
    db_sea(i,j)           = 0.0
    rhokm_ssi(i,j)        = 0.0
    rhokm_ssi_nohalo(i,j) = 0.0
    cd_ssi(i,j)           = 0.0
    ch_ssi(i,j)           = 0.0
    h_sea(i,j)            = 0.0
    e_sea(i,j)            = 0.0
  END DO
END DO
!$OMP END DO

IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        fqw_sea_wtrac(i,j,i_wt) = 0.0
      END DO
    END DO
!$OMP END DO

    DO k=1,nice_use
!$OMP DO SCHEDULE(STATIC)
      DO j = tdims%j_start,tdims%j_end
        DO i = tdims%i_start,tdims%i_end
          fqw_sicat_wtrac(i,j,k,i_wt) = 0.0
        END DO
      END DO
!$OMP END DO
    END DO
  END DO
END IF

DO k=1,nice_use
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      radnet_sice(i,j,k)    = 0.0
      rib_ice(i,j,k)        = 0.0
      db_ice(i,j,k)         = 0.0
      ftl_ice(i,j,k)        = 0.0
      fqw_ice(i,j,k)        = 0.0
      cd_ice(i,j,k)         = 0.0
      ch_ice(i,j,k)         = 0.0
      v_s_ice(i,j,k)        = 0.0
      recip_l_mo_ice(i,j,k) = 0.0
      IF (l_icerough_prognostic) THEN
        ! prognostic z0sice for coupled model
        ! (makes no discrimination between miz and ice pack)
        z0m_ice(i,j,k)      = z0m_sice_fmd(i,j)
        z0h_ice(i,j,k)      = z0h_z0m_sice * z0m_sice_skin(i,j)
      ELSE
        ! non prognostic z0sice
        z0m_ice(i,j,k)      = z0sice
        z0h_ice(i,j,k)      = z0h_z0m_sice * z0m_ice(i,j,k)
      END IF
    END DO
  END DO
!$OMP END DO
END DO

!-----------------------------------------------------------------------
! Calculate net radiation on sea and sea-ice
!-----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
DO k = 1, sea_pts
  l = sea_index(k)
  j = (ssi_index(l) - 1) / t_i_length + 1
  i = ssi_index(l) - (j-1) * t_i_length
  radnet_sea(i,j) = sw_sea(l) + emis_sea *                                     &
             (lw_down(i,j) - sbcon * tstar_sea(i,j)**4)
END DO
!$OMP END DO NOWAIT

DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
  DO k = 1, sice_pts_ncat(n)
    l = sice_index_ncat(k,n)
    j=(ssi_index(l) - 1) / t_i_length + 1
    i = ssi_index(l) - (j-1) * t_i_length
    radnet_sice(i,j,n) = sw_sicat(l,n) + emis_sice *                           &
               ( lw_down(i,j) - sbcon * tstar_sice_cat(i,j,n)**4 )
  END DO
!$OMP END DO NOWAIT
END DO

IF (sf_diag%l_radnet_sea) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%radnet_sea(i,j) = radnet_sea(i,j)
    END DO
  END DO
!$OMP END DO
END IF

!-----------------------------------------------------------------------
!  Calculate height of lowest model level relative to sea.
!-----------------------------------------------------------------------

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    z1_tq_sea(i,j)   = z1_tq(i,j)
    z1_tq_ctile(i,j) = z1_tq(i,j)
  END DO
END DO
!$OMP END DO NOWAIT

IF (l_ctile) THEN !ij
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF ((flandg(i,j) > 0.0) .AND. (flandg(i,j) < 1.0)) THEN
        z1_tq_sea(i,j) = z1_tq(i,j) + z_land(i,j)
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

IF (l_ctile .AND. ctile_orog_fix == off) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF ((flandg(i,j) > 0.0) .AND. (flandg(i,j) < 1.0)) THEN
        z1_tq_ctile(i,j) = z1_tq(i,j) + z_land(i,j)
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

IF (i_modiscopt == on) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      z1_tq_top_sea(i,j) = z1_tq_top(i,j)
      z1_tq_top_ctile(i,j) = z1_tq_top(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT

  IF (l_ctile) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        IF ((flandg(i,j) > 0.0) .AND. (flandg(i,j) < 1.0)) THEN
          z1_tq_top_sea(i,j) = z1_tq_top(i,j) + z_land(i,j)
        END IF
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF

  IF (l_ctile .AND. ctile_orog_fix == off) THEN
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        IF ((flandg(i,j) > 0.0) .AND. (flandg(i,j) < 1.0)) THEN
          z1_tq_top_ctile(i,j) = z1_tq_top(i,j) + z_land(i,j)
        END IF
      END DO
    END DO
!$OMP END DO NOWAIT
  END IF
END IF

!$OMP END PARALLEL

IF (nice_use >  1) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i,j,n)                &
!$OMP SHARED(tdims,ice_fract,tstar_sice,nice_use,ice_fract_cat,tstar_sice_cat)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      ice_fract(i,j) = 0.0
      tstar_sice(i,j) = 0.0
      DO n = 1, nice_use
        IF (ice_fract_cat(i,j,n) >  0.0) THEN
          ice_fract(i,j)  = ice_fract(i,j) + ice_fract_cat(i,j,n)
          tstar_sice(i,j) = tstar_sice(i,j) +                                  &
                            tstar_sice_cat(i,j,n) * ice_fract_cat(i,j,n)

        END IF
      END DO
      IF (ice_fract(i,j) >  0.0)                                               &
         tstar_sice(i,j) = tstar_sice(i,j) / ice_fract(i,j)
    END DO
  END DO
!$OMP END PARALLEL DO
  sice_pts_use = 0
  DO i = 1, ssi_pts
    sice_index_use(i) = 0
    sice_frac_use(i) = 0.0
  END DO
  DO l = 1, ssi_pts
    first_counter = 0
    DO n = 1, nice_use
      IF (sice_frac_ncat(l,n) >  0.0) THEN
        IF (first_counter == 0) THEN
          sice_pts_use = sice_pts_use + 1
          sice_index_use(sice_pts_use) = l
          first_counter = 1
        END IF
        sice_frac_use(l) = sice_frac_use(l) + sice_frac_ncat(l,n)
      END IF
    END DO
  END DO
ELSE
  sice_pts_use = sice_pts_ncat(1)
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i, j)                 &
!$OMP SHARED(tdims, ice_fract, ice_fract_cat, tstar_sice, tstar_sice_cat)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      ice_fract(i,j) = ice_fract_cat(i,j,1)
      tstar_sice(i,j) = tstar_sice_cat(i,j,1)
    END DO
  END DO
!$OMP END PARALLEL DO
  DO i = 1, ssi_pts
    sice_index_use(i) = sice_index_ncat(i,1)
    sice_frac_use(i) = sice_frac_ncat(i,1)
  END DO
END IF

!-----------------------------------------------------------------------
!  2.  Calculate QSAT values required later.
!-----------------------------------------------------------------------
IF (l_mr_physics) THEN
  CALL qsat_mix(qs1,tl_1,pstar,t_i_length,t_j_length)
ELSE
  CALL qsat(qs1,tl_1,pstar,t_i_length,t_j_length)
END IF

IF (l_mr_physics) THEN
  CALL qsat_mix(qstar_sea,tstar_sea,pstar,t_i_length,t_j_length)
  CALL qsat_mix(qstar_ice,tstar_sice,pstar,t_i_length,t_j_length)
ELSE
  CALL qsat(qstar_sea,tstar_sea,pstar,t_i_length,t_j_length)
  CALL qsat(qstar_ice,tstar_sice,pstar,t_i_length,t_j_length)
END IF

!
! Also calculate surface specific saturated humidity on ice by category, as
! it's needed in the call to sf_flux.
IF (l_mr_physics) THEN
!$OMP PARALLEL DO IF(nice_use>1) SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(n)     &
!$OMP SHARED(nice_use, qstar_ice_cat, tstar_sice_cat, pstar, t_i_length,       &
!$OMP        t_j_length)
  DO n = 1, nice_use
    CALL qsat_mix(qstar_ice_cat(:,:,n),tstar_sice_cat(:,:,n),pstar,            &
                  t_i_length,t_j_length)
  END DO
!$OMP END PARALLEL DO
ELSE
!$OMP PARALLEL DO IF(nice_use>1) SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(n)     &
!$OMP SHARED(nice_use, qstar_ice_cat, tstar_sice_cat, pstar, t_i_length,       &
!$OMP        t_j_length)
  DO n = 1, nice_use
    CALL qsat(qstar_ice_cat(:,:,n),tstar_sice_cat(:,:,n),pstar,                &
                  t_i_length,t_j_length)
  END DO
!$OMP END PARALLEL DO
END IF
!-----------------------------------------------------------------------
! 2.1 If requested, improve accuracy of air density, rhostar
!     On input rhostar = pstar/R*Tstar
!-----------------------------------------------------------------------
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j)                                      &
!$OMP SHARED(tdims,rhostar,rhostar_sea,rhostarmom_sea,rhostar_ice,             &
!$OMP        rhostarmom_ice)
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    ! original approximation for surface air density
    rhostar_sea(i,j)    = rhostar(i,j)
    rhostarmom_sea(i,j) = rhostar(i,j)
    rhostar_ice(i,j)    = rhostar(i,j)
    rhostarmom_ice(i,j) = rhostar(i,j)
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

IF (l_accurate_rho) THEN
  ! More accurate expressions for surface air density
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j)                                      &
!$OMP SHARED(tdims,qv_star,SeaSalinityFactor,qstar_sea)
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start, tdims%j_end
    DO i = tdims%i_start, tdims%i_end
      ! use salinity weighted qsat over sea
      qv_star(i,j) = SeaSalinityFactor * qstar_sea(i,j)
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

  CALL calc_air_dens(l_mr_physics,qv_star,rhostar_sea,rhostarmom_sea)

  ! use level 1 humidity for sea ice as is done over land
  CALL calc_air_dens(l_mr_physics,qw_1,rhostar_ice,rhostarmom_ice)

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j)                                      &
!$OMP SHARED(tdims,rhostar,rhostar_sea,rhostar_ice,ice_fract)
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start, tdims%j_end
    DO i = tdims%i_start, tdims%i_end
      ! rhostar is gridbox mean scalar rho for sea+seaice
      rhostar(i,j) = (1.0 - ice_fract(i,j)) * rhostar_sea(i,j) +               &
                             ice_fract(i,j) * rhostar_ice(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL

END IF ! l_accurate_rho


!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,j,n, d_t)                               &
!$OMP SHARED(t_i_length, t_j_length, tdims, nice_use,                          &
!$OMP        dzssi, hcons_sea, kappa_seasurf, canhc_sea, hcap_sea, ashtf_sea,  &
!$OMP        ashtf, k_sice, dzdummy, alpha1_sea, alpha1_sice,                  &
!$OMP        tstar_sea, tl_1, qstar_sea, qs1, epsil,                           &
!$OMP        c_virtual, r, tstar_sice_cat, qstar_ice_cat, qstar_ice)

! Set up thermal properties for sea and sea-ice and initialise dQsat/dt to zero
!$OMP DO SCHEDULE(STATIC)
DO i = 1,t_i_length * t_j_length
  dzssi(i)       = dzsea
  hcons_sea(i)   = kappa_seasurf
  canhc_sea(i)   = hcap_sea
  dzdummy(i)     = 2.0       ! as k_sice equals 2*kappa/dz
  ! So in sf_flux, ashtf = 2 * k_sice/dzdummy, this will then give
  !        ashtf = k_sice as required
END DO
!$OMP END DO NOWAIT
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    ashtf_sea(i,j)  = 2.0 * kappa_seasurf / dzsea
    alpha1_sea(i,j) = 0.0
  END DO
END DO
!$OMP END DO NOWAIT
DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      ashtf(i,j,n)       = k_sice(i,j,n)
      alpha1_sice(i,j,n) = 0.0
    END DO
  END DO
!$OMP END DO NOWAIT
END DO

! Calculate dQsat/dt for sea
!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    d_t = tstar_sea(i,j) - tl_1(i,j)
    IF (d_t > 0.05 .OR. d_t < -0.05) THEN
      alpha1_sea(i,j) = (qstar_sea(i,j) - qs1(i,j)) / d_t
    ELSE IF (tl_1(i,j) > tm) THEN
      alpha1_sea(i,j) = epsil * lc * qs1(i,j) *                                &
                  (1.0 + c_virtual * qs1(i,j)) /                               &
                  ( r * tl_1(i,j) * tl_1(i,j))
    ELSE
      alpha1_sea(i,j) = epsil * ls * qs1(i,j) *                                &
                  (1.0 + c_virtual * qs1(i,j)) /                               &
                  ( r * tl_1(i,j) * tl_1(i,j))
    END IF
  END DO
END DO
!$OMP END DO NOWAIT

! Calculate dQsat/dt for sea-ice
IF (nice_use >  1) THEN
  DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        d_t = tstar_sice_cat(i,j,n) - tl_1(i,j)
        IF (d_t > 0.05 .OR. d_t < -0.05) THEN
          alpha1_sice(i,j,n) = (qstar_ice_cat(i,j,n) - qs1(i,j)) / d_t
        ELSE IF (tl_1(i,j) > tm) THEN
          alpha1_sice(i,j,n) = epsil * lc * qs1(i,j) *                         &
                      (1.0 + c_virtual * qs1(i,j)) /                           &
                      ( r * tl_1(i,j) * tl_1(i,j))
        ELSE
          alpha1_sice(i,j,n) = epsil * ls * qs1(i,j) *                         &
                      (1.0 + c_virtual * qs1(i,j)) /                           &
                      ( r * tl_1(i,j) * tl_1(i,j))
        END IF
      END DO
    END DO
!$OMP END DO NOWAIT
  END DO
ELSE
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      d_t = tstar_sice_cat(i,j,1) - tl_1(i,j)
      IF (d_t > 0.05 .OR. d_t < -0.05) THEN
        alpha1_sice(i,j,1) = (qstar_ice(i,j) - qs1(i,j)) / d_t
      ELSE IF (tl_1(i,j) > tm) THEN
        alpha1_sice(i,j,1) = epsil * lc * qs1(i,j) *                           &
                    (1.0 + c_virtual * qs1(i,j)) /                             &
                    ( r * tl_1(i,j) * tl_1(i,j))
      ELSE
        alpha1_sice(i,j,1) = epsil * ls * qs1(i,j) *                           &
                    (1.0 + c_virtual * qs1(i,j)) /                             &
                    ( r * tl_1(i,j) * tl_1(i,j))
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT
END IF
!$OMP END PARALLEL


!-----------------------------------------------------------------------
!  3. calculate surface scalar roughness length
!-----------------------------------------------------------------------

SELECT CASE (iseasurfalg)
  !
CASE (ip_ss_surf_div)

  !       Composite formulation for thermal roughness lengths,
  !       incoporating the smooth aerodynamic limit for low
  !       wind speeds and a value based on surface divergence
  !       theory for higher wind speeds.

  !       The friction velocity is diagnosed in the surface
  !       transfer scheme, using z0m from the previous time-step.
  !       z0[T,q] is also required but depends on u_* in this
  !       scheme. For practical purposes, it is sufficient to
  !       infer it from u_* determined from z0m, but in general
  !       a given value of z0m corresponds to two values of
  !       u_*, so we need to know whether we are on the low or
  !       high wind speed side of the minimum value of z0m.
  !       If we are on the high side, z0[T,q] will be inversely
  !       proportional to z0m, but on the low side it may follow
  !       this relationship, or be aerodynamically smooth. In
  !       the smooth case we iterate u_* from the expression for
  !       the roughness length and then take the maximum of the
  !       smooth and high-wind expressions for z0[T,q]. An
  !       iteration for the low-wind value of u_*, USTR_L is
  !       carried out. This will converge to the correct limit only
  !       on the low-wind side of the minimum, and the standard
  !       criterion that the gradient of a fixed-point iteration
  !       should be less than 1 in modulus gives a more precise
  !       boundary, TOL_USTR_L. For consistency with earlier versions
  !       of the modset, hard-wired values are retained for the
  !       operational value of Charnock's parameter. An additional
  !       check is required, since z0m can be large at low or at
  !       high wind-speeds. This is less precise and a fixed
  !       value of 0.07 is used to test USTR_N, which was determined
  !       by inspection of a graph of z0m against u_*: it is
  !       unlikely that this will need to be changed unless
  !       Charnock's constant is greatly altered.

  IF (charnock == 0.018) THEN
    tol_ustr_l = 0.055
    tol_ustr_n = 0.07
  ELSE
    tol_ustr_l = 0.75 * (1.54e-6 * g / (2.0 * charnock))**0.33333
    tol_ustr_n = 0.07
  END IF

!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i, j, jits, ustr_l, ustr_n)            &
!$OMP SHARED( tdims, vshr_ssi, z1_uv, z0msea, tol_ustr_n, tol_ustr_l,          &
!$OMP         charnock, g, z0h_sea)                                            &
!$OMP             SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      !           We need to infer u_* from Z0M.
      IF (vshr_ssi(i,j)  > 0.0) THEN
        !             Compute u_* using neutral stratification.
        ustr_n = vkman * vshr_ssi(i,j) /                                       &
          LOG(1.0 + z1_uv(i,j) / z0msea(i,j) )
        !             Compute u_* using low wind approximation.
        ustr_l = 1.54e-06 /  z0msea(i,j) - 1.0e-05
        !             Since Z0M could be large for low and high u_*, we use
        !             USTR_N as an additional check on the regime.
        IF ( (ustr_n < tol_ustr_n) .AND.                                       &
             (ustr_l < tol_ustr_l) ) THEN
          !               Iterate u_* for low winds.
          DO jits = 1, 5
            ustr_l = 1.54e-06 / (z0msea(i,j) - (charnock / g) * ustr_l**2)     &
              -1.0e-05
          END DO
          !               Take the maximum of the smooth and high-wind values.
          !               A lower limit is imposed on the friction velocity to
          !               allow for the exceptional case of very low winds: the
          !               value of 10^-5 is the same as the limit for the momentum
          !               roughness length.
          z0h_sea(i,j) = MAX( 2.52e-6 / (ustr_l+1.0e-05),                      &
            2.56e-9 / z0msea(i,j) )
        ELSE
          !               Take the high-wind value, but limit it to the molecular
          !               mean free path (we should not hit this limit
          !               in practice).
          z0h_sea(i,j) = MAX( 2.56e-9 / z0msea(i,j), 7.0e-08 )
        END IF
      END IF
    END DO
  END DO
!$OMP END PARALLEL DO

CASE (ip_ss_surf_div_coupled)

  ! tol_ustr_n = 0.07 - written as a constant below for optimization
!$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(i, j, jits, ustr_l, ustr_n, tol_ustr_l)&
!$OMP SHARED( tdims, vshr_ssi, z1_uv, z0msea, charnock_w, g, z0h_sea)          &
!$OMP             SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      tol_ustr_l = 0.75 * (1.54e-6 * g / (2.0 * charnock_w(i,j)))**0.33333

      !           We need to infer u_* from Z0M.
      IF (vshr_ssi(i,j)  > 0.0) THEN
        !             Compute u_* using neutral stratification.
        ustr_n = vkman * vshr_ssi(i,j) /                                       &
          LOG(1.0 + z1_uv(i,j) / z0msea(i,j) )
        !             Compute u_* using low wind approximation.
        ustr_l = 1.54e-06 /  z0msea(i,j) - 1.0e-05
        !             Since Z0M could be large for low and high u_*, we use
        !             USTR_N as an additional check on the regime.
        IF ( (ustr_n < 0.07) .AND.                                             &
             (ustr_l < tol_ustr_l) ) THEN
          !               Iterate u_* for low winds.
          DO jits = 1, 5
            ustr_l = 1.54e-06 / (z0msea(i,j) - (charnock_w(i,j) / g) * ustr_l**2) &
              -1.0e-05
          END DO
          !               Take the maximum of the smooth and high-wind values.
          !               A lower limit is imposed on the friction velocity to
          !               allow for the exceptional case of very low winds: the
          !               value of 10^-5 is the same as the limit for the momentum
          !               roughness length.
          z0h_sea(i,j) = MAX( 2.52e-6 / (ustr_l+1.0e-05),                      &
            2.56e-9 / z0msea(i,j) )
        ELSE
          !               Take the high-wind value, but limit it to the molecular
          !               mean free path (we should not hit this limit
          !               in practice).
          z0h_sea(i,j) = MAX( 2.56e-9 / z0msea(i,j), 7.0e-08 )
        END IF
      END IF
    END DO
  END DO
!$OMP END PARALLEL DO

CASE (ip_ss_fixed)

  !       Use a fixed thermal roughness length.
  z0h_sea(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end) = z0hsea

CASE DEFAULT
  !     Do not alter the roughness lengths at this point.
  !
END SELECT


IF ( l_spec_z0 ) THEN
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,z0h_scm,z0h_sea)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF (z0h_scm(i,j)  >   0.0) THEN

        ! Set Z0H from SCM namelist
        ! (if specified) for sea points
        z0h_sea(i,j) = z0h_scm(i,j)

      END IF  ! Z0H_SCM
    END DO  ! I
  END DO  ! J
!$OMP END PARALLEL DO
END IF

!-----------------------------------------------------------------------
!  3.2 Calculate bulk Richardson number for the lowest model level.
!-----------------------------------------------------------------------
!
! Sea, sea-ice and sea-ice leads
CALL sf_rib (                                                                  &
 ssi_pts,sea_pts,ssi_index,sea_index,                                          &
 bq_1,bt_1,qstar_sea,qw_1,array_one,tl_1,                                      &
 tstar_sea,vshr_ssi,z0h_sea,z0msea,array_zero,                                 &
 z1_tq_sea,z1_uv,l_vegdrag_ssi,                                                &
 rib_sea,db_sea                                                                &
 )
!
IF (nice_use >  1) THEN
  DO n = 1, nice_use
    CALL sf_rib (                                                              &
     ssi_pts,sice_pts_ncat(n),ssi_index,sice_index_ncat(:,n),                  &
     bq_1,bt_1,qstar_ice_cat(:,:,n),qw_1,array_one,tl_1,                       &
     tstar_sice_cat(:,:,n),vshr_ssi,z0h_ice(:,:,n),z0m_ice(:,:,n),array_zero,  &
     z1_tq_sea,z1_uv,l_vegdrag_ssi,                                            &
     rib_ice(:,:,n),db_ice(:,:,n)                                              &
      )
  END DO
ELSE
  CALL sf_rib (                                                                &
    ssi_pts,sice_pts_use,ssi_index,sice_index_use,                             &
    bq_1,bt_1,qstar_ice_cat(:,:,1),qw_1,array_one,tl_1,                        &
    tstar_sice_cat(:,:,1),vshr_ssi,z0h_ice(:,:,1),z0m_ice(:,:,1),array_zero,   &
    z1_tq_sea,z1_uv,l_vegdrag_ssi,                                             &
    rib_ice(:,:,1),db_ice(:,:,1)                                               &
     )

END IF
!
! Calculate gridbox mean db_ice
IF (nice_use >  1) THEN
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, k ,l, n)                            &
!$OMP SHARED(sice_pts_ncat, sice_index_ncat, ssi_index, t_i_length, nice_use,  &
!$OMP        ice_fract, db_ice_mean, sice_frac_ncat, db_ice, tdims)
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      db_ice_mean(i,j) = 0.0
    END DO
  END DO
!$OMP END DO
  DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,sice_pts_ncat(n)
      l = sice_index_ncat(k,n)
      j=(ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      IF (ice_fract(i,j) >  0.0) THEN
        db_ice_mean(i,j) = db_ice_mean(i,j)                                    &
            + (sice_frac_ncat(l,n) * db_ice(i,j,n)                             &
                         / ice_fract(i,j))
      END IF
    END DO   ! K
!$OMP END DO
  END DO   ! N
!$OMP END PARALLEL
ELSE
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i, j)                 &
!$OMP SHARED(tdims, db_ice_mean, db_ice)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      db_ice_mean(i,j) = db_ice(i,j,1)
    END DO
  END DO
!$OMP END PARALLEL DO
END IF
!
!-----------------------------------------------------------------------
!  3.4 Calculate CD, CH via routine FCDCH.
!-----------------------------------------------------------------------
!
CALL fcdch (                                                                   &
   cor_mo_iter,ssi_pts,sea_pts,                                                &
   sea_index,ssi_index,                                                        &
   db_sea,vshr_ssi,                                                            &
   z0msea,z0h_sea,zdt_dummy,zh,                                                &
   z1_uv,z1_uv_top,z1_tq_ctile,z1_tq_top_ctile,array_one,                      &
   ddmfx,iseasurfalg,charnock,                                                 &
   charnock_w,                                                                 &
   l_vegdrag_ssi,array_zero,array_zero,                                        &
   array_zero_int,0,.FALSE.,.FALSE.,array_false,                               &
   array_zero,array_zero,array_zero,array_zero,                                &
   array_zero,array_zero,canhc_sea,                                            &
   dzssi,qstar_sea,qw_1,radnet_sea,                                            &
   array_zero,timestep,tl_1,tstar_sea,tstar_sea,                               &
   array_zero,array_emis,array_one,array_zero,                                 &
   array_one,alpha1_sea,hcons_sea,ashtf_sea,                                   &
   rhostar,bq_1,bt_1,                                                          &
   cd_sea,ch_sea,cd_std_sea,                                                   &
   v_s_sea,v_s_std_sea,recip_l_mo_sea,                                         &
   u_s_std_sea                                                                 &
   )

!
IF (nice_use >  1) THEN
  DO n = 1, nice_use
    CALL fcdch (                                                               &
     cor_mo_iter,ssi_pts,sice_pts_ncat(n),                                     &
     sice_index_ncat(:,n),ssi_index,                                           &
     db_ice(:,:,n),vshr_ssi,                                                   &
     z0m_ice(:,:,n),z0h_ice(:,:,n),zdt_dummy,zh,                               &
     z1_uv,z1_uv_top,z1_tq_ctile,z1_tq_top_ctile,array_one,                    &
     ddmfx,ip_ss_solid,charnock,                                               &
     charnock_w,                                                               &
     l_vegdrag_ssi,array_zero,array_zero,                                      &
     array_zero_int,0,.FALSE.,.FALSE.,array_false,                             &
     array_zero,array_zero,array_zero,array_zero,                              &
     array_zero,array_zero,array_zero,                                         &
     dzdummy,qstar_ice_cat(:,:,n),qw_1,radnet_sice(:,:,n),                     &
     array_zero,timestep,tl_1,ti_cat(:,:,n),tstar_sice_cat(:,:,n),             &
     array_zero,array_emis,array_one,array_zero,                               &
     array_one,alpha1_sice(:,:,n),k_sice(:,:,n),ashtf(:,:,n),                  &
     rhostar,bq_1,bt_1,                                                        &
     cd_ice(:,:,n),ch_ice(:,:,n),cd_std_ice(:,n),                              &
     v_s_ice(:,:,n),v_s_std_ice(:,n),recip_l_mo_ice(:,:,n),                    &
     u_s_std_ice(:,n)                                                          &
     )
  END DO
ELSE
  CALL fcdch (                                                                 &
   cor_mo_iter,ssi_pts,sice_pts_use,                                           &
   sice_index_use,ssi_index,                                                   &
   db_ice(:,:,1),vshr_ssi,                                                     &
   z0m_ice(:,:,1),z0h_ice(:,:,1),zdt_dummy,zh,                                 &
   z1_uv,z1_uv_top,z1_tq_ctile,z1_tq_top_ctile,array_one,                      &
   ddmfx,ip_ss_solid,charnock,                                                 &
   charnock_w,                                                                 &
   l_vegdrag_ssi,array_zero,array_zero,                                        &
   array_zero_int,0,.FALSE.,.FALSE.,array_false,                               &
   array_zero,array_zero,array_zero,array_zero,                                &
   array_zero,array_zero,array_zero,                                           &
   dzdummy,qstar_ice_cat(:,:,1),qw_1,radnet_sice(:,:,1),                       &
   array_zero,timestep,tl_1,ti_cat(:,:,1),tstar_sice_cat(:,:,1),               &
   array_zero,array_emis,array_one,array_zero,                                 &
   array_one,alpha1_sice(:,:,1),k_sice(:,:,1),ashtf(:,:,1),                    &
   rhostar,bq_1,bt_1,                                                          &
   cd_ice(:,:,1),ch_ice(:,:,1),cd_std_ice(:,1),                                &
   v_s_ice(:,:,1),v_s_std_ice(:,1),recip_l_mo_ice(:,:,1),                      &
   u_s_std_ice(:,1)                                                            &
   )
END IF

IF ( .NOT. l_icerough_prognostic) THEN
  !
  ! If not using prognostic sea ice roughness length, calculate CD and CH
  ! for the MIZ (when prognostic sea ice roughness length is used there is no
  ! separate calculation for the MIZ).
  !
  IF (l_iceformdrag_lupkes) THEN
    !
    ! If using Lupkes form drag, put the appropriate coefficients into the arrays
    ! for marginal ice. Scalar transfer is assumed to take place only through
    ! the interfacial route.
    ! for form drag.
    !
    CALL ice_formdrag_lupkes (                                                 &
      flandg, ice_fract,                                                       &
      z0m_ice(:,:,1), z0msea, cd_ice(:,:,1), cd_sea,                           &
      z1_tq_sea, z1_tq_top_sea,                                                &
      cd_miz                                                                   &
      )
  ELSE
    CALL fcdch (                                                               &
      cor_mo_iter,ssi_pts,sice_pts_use,                                        &
      sice_index_use,ssi_index,                                                &
      db_ice_mean,vshr_ssi,                                                    &
      z0m_miz,z0h_miz,zdt_dummy,zh,                                            &
      z1_uv,z1_uv_top,z1_tq_ctile,z1_tq_top_ctile,array_one,                   &
      ddmfx,ip_ss_solid,charnock,                                              &
      charnock_w,                                                              &
      l_vegdrag_ssi,array_zero,array_zero,                                     &
      array_zero_int,0,.FALSE.,.FALSE.,array_false,                            &
      array_zero,array_zero,array_zero,array_zero,                             &
      array_zero,array_zero,array_zero,                                        &
      dzdummy,qstar_ice_cat(:,:,1),qw_1,radnet_sice(:,:,1),                    &
      array_zero,timestep,tl_1,ti_cat(:,:,1),tstar_sice_cat(:,:,1),            &
      array_zero,array_emis,array_one,array_zero,                              &
      array_one,alpha1_sice(:,:,1),k_sice(:,:,1),ashtf(:,:,1),                 &
      rhostar,bq_1,bt_1,                                                       &
      cd_miz,ch_miz,cd_std_miz,                                                &
      v_s_miz,v_s_std_miz,recip_l_mo_miz,                                      &
      u_s_std_miz                                                              &
      )
  END IF
END IF
!
! z1_tq_top_sea is no longer required.
DEALLOCATE(z1_tq_top_sea)
DEALLOCATE(z1_tq_top_ctile)
!
! Calculate gridbox mean CD and CH for sea ice:
IF (nice_use >  1) THEN
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, k, l, n)                            &
!$OMP SHARED(sice_pts_ncat, sice_index_ncat, ssi_index, t_i_length,            &
!$OMP        ice_fract, cd_ice_mean, sice_frac_ncat, cd_ice, ch_ice_mean,      &
!$OMP        ch_ice, tdims, nice_use)
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      cd_ice_mean(i,j) = 0.0
      ch_ice_mean(i,j) = 0.0
    END DO
  END DO
!$OMP END DO
  DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
    DO k = 1,sice_pts_ncat(n)
      l = sice_index_ncat(k,n)
      j=(ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      IF (ice_fract(i,j) >  0.0) THEN
        cd_ice_mean(i,j) = cd_ice_mean(i,j)                                    &
          + (sice_frac_ncat(l,n) * cd_ice(i,j,n) / ice_fract(i,j))
        ch_ice_mean(i,j) = ch_ice_mean(i,j)                                    &
          + (sice_frac_ncat(l,n) * ch_ice(i,j,n) / ice_fract(i,j))
      END IF
    END DO
!$OMP END DO
  END DO
!$OMP END PARALLEL
ELSE
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i, j)                 &
!$OMP SHARED(tdims, cd_ice_mean, ch_ice_mean, cd_ice, ch_ice)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      cd_ice_mean(i,j) = cd_ice(i,j,1)
      ch_ice_mean(i,j) = ch_ice(i,j,1)
    END DO
  END DO
!$OMP END PARALLEL DO
END IF
!
! Sea and sea-ice
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, n)                                  &
!$OMP SHARED(tdims, flandg, ice_fract, cd_ssi, cd_miz, cd_sea, ch_ssi, ch_miz, &
!$OMP        ch_sea, cd_ice_mean, ch_ice_mean, nice_use, rhokm_ssi, emis_sea,  &
!$OMP        vshr_ssi, rhokm_ssi_nohalo, rhokh_sea, rhokh_1_sice, sf_diag,     &
!$OMP        rhokh_1_sice_ncats, ice_fract_cat, ch_ice, l_iceformdrag_lupkes,  &
!$OMP        l_icerough_prognostic, t_i_length, t_j_length, array_emis,        &
!$OMP        rhokm_1_sea, rhokm_1_sice_ncats, rhokm_1_sice, cd_ice,            &
!$OMP        rhostar_sea, rhostar_ice, rhostarmom_sea, rhostarmom_ice)

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    rhokh_1_sice(i,j) = 0.0
  END DO
END DO
!$OMP END DO
DO n = 1, nice_use
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      rhokh_1_sice_ncats(i,j,n) = 0.0
    END DO
  END DO
!$OMP END DO
END DO

IF (l_iceformdrag_lupkes) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF (flandg(i,j) <  1.0 ) THEN
        cd_ssi(i,j) = (1.0 - ice_fract(i,j)) * cd_sea(i,j) +                   &
                      ice_fract(i,j) * ( cd_ice_mean(i,j) +                    &
                                         cd_miz(i,j) )
        !         No contribution to thermal roughness from form drag.
        ch_ssi(i,j) = (1.0 - ice_fract(i,j)) * ch_sea(i,j) +                   &
                      ice_fract(i,j) * ch_ice_mean(i,j)
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT
ELSE IF (l_icerough_prognostic) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF (flandg(i,j) <  1.0 ) THEN
        cd_ssi(i,j) = (1.0 - ice_fract(i,j)) * cd_sea(i,j) +                   &
                      ice_fract(i,j) * cd_ice_mean(i,j)
        ch_ssi(i,j) = (1.0 - ice_fract(i,j)) * ch_sea(i,j) +                   &
                      ice_fract(i,j) * ch_ice_mean(i,j)
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT
ELSE
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF (flandg(i,j) <  1.0 ) THEN
        IF ( ice_fract(i,j) <  0.7 ) THEN
          cd_ssi(i,j) = ( ice_fract(i,j) * cd_miz(i,j) +                       &
                (0.7 - ice_fract(i,j)) * cd_sea(i,j) ) / 0.7  ! P2430.5
          ch_ssi(i,j) = ( ice_fract(i,j) * ch_miz(i,j) +                       &
                (0.7 - ice_fract(i,j)) * ch_sea(i,j) ) / 0.7  ! P2430.4
        ELSE
          cd_ssi(i,j) = ( (1.0 - ice_fract(i,j)) * cd_miz(i,j) +               &
               (ice_fract(i,j) - 0.7) * cd_ice_mean(i,j) ) / 0.3  ! P2430.7
          ch_ssi(i,j) = ( (1.0 - ice_fract(i,j)) * ch_miz(i,j) +               &
               (ice_fract(i,j) - 0.7) * ch_ice_mean(i,j) ) / 0.3  ! P2430.7
        END IF
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

! Sea and sea ice surface drag coefficients
IF (sf_diag%l_cd_ssi) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%cd_ssi(i,j) = cd_ssi(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT
END IF
IF (sf_diag%l_ch_ssi) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      sf_diag%ch_ssi(i,j) = ch_ssi(i,j)
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

! Sea and sea-ice
IF (sf_diag%l_tau_surft .OR. sf_diag%l_tau_1) THEN
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF ( flandg(i,j) <  1.0 ) THEN
        IF (nice_use > 1) THEN
          !     ! Include effect of leads.
          rhokm_1_sea(i,j) = rhostarmom_sea(i,j) * cd_ssi(i,j) * vshr_ssi(i,j)
          DO n = 1,nice_use
            rhokm_1_sice_ncats(i,j,n) = rhostarmom_ice(i,j) *                  &
                                        cd_ice(i,j,n) * vshr_ssi(i,j)
            IF (ice_fract(i,j) /= 0.0) THEN
              rhokm_1_sice(i,j) = rhokm_1_sice(i,j) +                          &
                  (rhokm_1_sice_ncats(i,j,n) * ice_fract_cat(i,j,n)            &
                           / ice_fract(i,j))
            END IF
          END DO
        ELSE
          !     ! Original scheme without leads.
          rhokm_1_sice_ncats(i,j,1) = rhostarmom_ice(i,j) *                    &
                                      cd_ssi(i,j) * vshr_ssi(i,j)
          rhokm_1_sice(i,j) = rhokm_1_sice_ncats(i,j,1)
        END IF
      END IF
    END DO
  END DO
!$OMP END DO NOWAIT
END IF

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    IF ( flandg(i,j) <  1.0 ) THEN
      IF (nice_use > 1) THEN
        !     ! Include effect of leads.
        rhokh_sea(i,j) = rhostar_sea(i,j) * ch_ssi(i,j) * vshr_ssi(i,j)
        DO n = 1,nice_use
          rhokh_1_sice_ncats(i,j,n) = rhostar_ice(i,j) *                       &
                                      ch_ice(i,j,n) * vshr_ssi(i,j)
          IF (ice_fract(i,j) /= 0.0) THEN
            rhokh_1_sice(i,j) = rhokh_1_sice(i,j) +                            &
                (rhokh_1_sice_ncats(i,j,n) * ice_fract_cat(i,j,n)              &
                         / ice_fract(i,j))
          END IF
        END DO
        rhokm_ssi(i,j) = ((1.0 - ice_fract(i,j)) * rhostarmom_sea(i,j)         &
                       *cd_ssi(i,j) * vshr_ssi(i,j))                           &
                      + (ice_fract(i,j) * rhostarmom_ice(i,j)                  &
                       *cd_ice_mean(i,j) * vshr_ssi(i,j))
      ELSE
        !     ! Original scheme without leads.
        rhokh_1_sice_ncats(i,j,1) = rhostar_ice(i,j) * ch_ssi(i,j) *           &
                                    vshr_ssi(i,j)
        rhokh_1_sice(i,j) = rhokh_1_sice_ncats(i,j,1)
        rhokm_ssi(i,j) = rhostarmom_ice(i,j) * cd_ssi(i,j) * vshr_ssi(i,j)
      END IF
      !                                                           ! P243.124
      rhokm_ssi_nohalo(i,j) = rhokm_ssi(i,j)
      !
      !                                                           ! P243.125
    ELSE
      rhokm_ssi(i,j)            = 0.0
      rhokm_ssi_nohalo(i,j)     = 0.0
      rhokh_sea(i,j)            = 0.0
      rhokh_1_sice_ncats(i,j,:) = 0.0
      rhokh_1_sice(i,j)         = 0.0
    END IF
  END DO
END DO
!$OMP END DO

! Sea
!$OMP DO SCHEDULE(STATIC)
DO i = 1, t_i_length * t_j_length
  array_emis(i) = emis_sea
END DO
!$OMP END DO
!$OMP END PARALLEL
lh0 = lc
! This allows fluxes to be corrected for sea points
IF (l_use_dtstar_sea) THEN
  sea_point = 0.0
ELSE
  sea_point = 1.0
END IF

IF (nice_use > 1) THEN
  ! Include effect of leads
  CALL sf_flux (                                                               &
     ssi_pts,sea_pts,                                                          &
     ssi_index,sea_index,                                                      &
     array_zero_int,0,canhc_sea,dzssi,hcons_sea,ashtf_sea,                     &
     qstar_sea,qw_1,radnet_sea,                                                &
     array_one * beta_evap,array_zero,rhokh_sea,array_false,array_zero,        &
     timestep,tl_1,tstar_sea,tstar_sea,                                        &
     array_zero,array_zero,z0h_sea,z0msea,array_zero,z1_tq_sea,lh0,            &
     array_emis,array_one,                                                     &
     seasalinityfactor,array_zero,array_one,l_vegdrag_ssi,                     &
     alpha1_sea,ashtf_prime_sea,e_sea,epot_sea,h_sea,dtstar_sea,sea_point      &
     )

  ! Add sea contribution including leads to gridbox mean surface momentum flux
  ! diagnostic if requested
  IF (sf_diag%l_tau_1) THEN
!$OMP PARALLEL DO IF(sea_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)            &
!$OMP SHARED(sea_pts, sea_index, ssi_index, t_i_length, fssi_ij, sea_frac,     &
!$OMP        rhokm_1_sea, vshr_ssi, sf_diag) SCHEDULE(STATIC)
    DO k = 1, sea_pts
      l = sea_index(k)
      j = (ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      sf_diag%tau_1(i,j) = sf_diag%tau_1(i,j) +                                &
                           fssi_ij(i,j) * sea_frac(l) *                        &
                           rhokm_1_sea(i,j) * vshr_ssi(i,j)
    END DO
!$OMP END PARALLEL DO
  END IF

  ! Calculate water tracers open sea surface flux
  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
      CALL sf_flux_ssi_wtrac (i_wt, ssi_pts, sea_pts, sea_index,               &
                              sea_point,  seasalinityfactor,                   &
                              alpha1_sea, rhokh_sea, qstar_sea,                &
                              dtstar_sea, array_one * beta_evap,               &
                              r_sea_wtrac(:,:,i_wt), 'sea',                    &
                              qw_1_wtrac(:,:,i_wt), fqw_sea_wtrac(:,:,i_wt))
    END DO
  END IF

ELSE

  ! Do not include leads
  CALL sf_flux (                                                               &
       ssi_pts,sea_pts,                                                        &
       ssi_index,sea_index,                                                    &
       array_zero_int,0,canhc_sea,dzssi,hcons_sea,ashtf_sea,                   &
       qstar_sea,qw_1,radnet_sea,                                              &
       array_one * beta_evap,array_zero,rhokh_1_sice,array_false,array_zero,   &
       timestep,tl_1,tstar_sea,tstar_sea,                                      &
       array_zero,array_zero,z0h_sea,z0msea,array_zero,z1_tq_sea,lh0,          &
       array_emis,array_one,                                                   &
       seasalinityfactor,array_zero,array_one,l_vegdrag_ssi,                   &
       alpha1_sea,ashtf_prime_sea,e_sea,epot_sea,h_sea,dtstar_sea,sea_point    &
       )

  ! Add sea contribution excluding leads to gridbox mean surface momentum flux
  ! diagnostic if requested
  IF (sf_diag%l_tau_1) THEN
!$OMP PARALLEL DO IF(sea_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)            &
!$OMP SHARED(sea_pts, sea_index, ssi_index, t_i_length, fssi_ij, sea_frac,     &
!$OMP        rhokm_1_sice, vshr_ssi, sf_diag) SCHEDULE(STATIC)
    DO k = 1, sea_pts
      l = sea_index(k)
      j = (ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      sf_diag%tau_1(i,j) = sf_diag%tau_1(i,j) +                                &
                           fssi_ij(i,j) * sea_frac(l) *                        &
                           rhokm_1_sice(i,j) * vshr_ssi(i,j)
    END DO
!$OMP END PARALLEL DO
  END IF

  ! Calculate water tracers open sea surface flux
  ! (Note, the only difference in this call to the one for nice_use > 1 is
  !  the use of rhokh_1_sice instead of rhokh_sea.)
  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
      CALL sf_flux_ssi_wtrac (i_wt, ssi_pts, sea_pts, sea_index,               &
                              sea_point,  seasalinityfactor,                   &
                              alpha1_sea, rhokh_1_sice, qstar_sea,             &
                              dtstar_sea, array_one * beta_evap,               &
                              r_sea_wtrac(:,:,i_wt), 'sea',                    &
                              qw_1_wtrac(:,:,i_wt), fqw_sea_wtrac(:,:,i_wt))
    END DO
  END IF

END IF  ! nice_use > 1

! Add sea contribution to gridbox mean fluxes of heat and moisture
!$OMP PARALLEL DO IF(sea_pts > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)            &
!$OMP SHARED(sea_pts, sea_index, ssi_index, t_i_length, ftl_1, fssi_ij,        &
!$OMP        sea_frac, h_sea, fqw_1, e_sea) SCHEDULE(STATIC)
DO k = 1, sea_pts
  l = sea_index(k)
  j = (ssi_index(l) - 1) / t_i_length + 1
  i = ssi_index(l) - (j-1) * t_i_length
  ftl_1(i,j) = ftl_1(i,j) + fssi_ij(i,j) * sea_frac(l) * h_sea(i,j)
  fqw_1(i,j) = fqw_1(i,j) + fssi_ij(i,j) * sea_frac(l) * e_sea(i,j)
END DO
!$OMP END PARALLEL DO

! Water tracers - add sea contribution to gridbox mean surface flux
IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
!$OMP PARALLEL DO IF(sea_pts > 1) SCHEDULE(STATIC) DEFAULT(NONE)               &
!$OMP PRIVATE(i, j, k, l)                                                      &
!$OMP SHARED(sea_pts, sea_index, ssi_index, t_i_length, fssi_ij,               &
!$OMP        sea_frac, fqw_1_wtrac, fqw_sea_wtrac, i_wt)
    DO k = 1, sea_pts
      l = sea_index(k)
      j = (ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      fqw_1_wtrac(i,j,i_wt) = fqw_1_wtrac(i,j,i_wt)                            &
                                       + fssi_ij(i,j) * sea_frac(l)            &
                                         * fqw_sea_wtrac(i,j,i_wt)
    END DO
!$OMP END PARALLEL DO
  END DO

END IF

! rhokm_1_sea and rhokm_1_sice no longer required
DEALLOCATE(rhokm_1_sea)
DEALLOCATE(rhokm_1_sice)

! Sea ice
lh0 = ls
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i)                    &
!$OMP SHARED(t_i_length, t_j_length, array_emis, emis_sice)
DO i = 1, t_i_length * t_j_length
  array_emis(i) = emis_sice
END DO
!$OMP END PARALLEL DO
sea_point = 0.0

IF (nice_use >  1) THEN
  DO n = 1, nice_use
    CALL sf_flux (                                                             &
     ssi_pts,sice_pts_ncat(n),                                                 &
     ssi_index,sice_index_ncat(:,n),                                           &
     array_zero_int,0,array_zero,dzdummy,k_sice(:,:,n),ashtf(:,:,n),           &
     qstar_ice_cat(:,:,n),qw_1,radnet_sice(:,:,n),array_one,array_one,         &
     rhokh_1_sice_ncats(:,:,n),array_false,array_zero,                         &
     timestep,tl_1,ti_cat(:,:,n),                                              &
     tstar_sice_cat(:,:,n),                                                    &
     array_zero,array_zero,z0h_ice(:,:,n),z0m_ice(:,:,n),array_zero,           &
     z1_tq_sea,lh0,                                                            &
     array_emis,array_one,                                                     &
     1.0,array_zero,array_one,l_vegdrag_ssi,                                   &
     alpha1_sice(:,:,n),ashtf_prime(:,:,n),fqw_ice(:,:,n),epot_ice,            &
     ftl_ice(:,:,n),dtstar_sice(:,:,n),sea_point                               &
     )
  END DO

  ! Water tracers sea ice surface flux
  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
      DO n = 1, nice_use
        ! (Note, the '1.0' in the call is the seasalinityfactor argument)
        CALL sf_flux_ssi_wtrac (i_wt, ssi_pts, sice_pts_ncat(n),               &
                              sice_index_ncat(:,n), sea_point,  1.0,           &
                              alpha1_sice(:,:,n), rhokh_1_sice_ncats(:,:,n),   &
                              qstar_ice_cat(:,:,n),                            &
                              dtstar_sice(:,:,n), array_one,                   &
                              r_sice_wtrac(:,:,n,i_wt), 'ice',                 &
                              qw_1_wtrac(:,:,i_wt),                            &
                              fqw_sicat_wtrac(:,:,n,i_wt))
      END DO
    END DO
  END IF

ELSE
  CALL sf_flux (                                                               &
   ssi_pts,sice_pts_ncat(1),                                                   &
   ssi_index,sice_index_ncat(:,1),                                             &
   array_zero_int,0,array_zero,dzdummy,k_sice(:,:,1),ashtf(:,:,1),             &
   qstar_ice,qw_1,radnet_sice(:,:,1),array_one,array_one,                      &
   rhokh_1_sice_ncats(:,:,1),array_false,array_zero,                           &
   timestep,tl_1,ti,tstar_sice_cat(:,:,1),                                     &
   array_zero,array_zero,z0h_ice(:,:,1),z0m_ice(:,:,1),array_zero,             &
   z1_tq_sea,lh0,                                                              &
   array_emis,array_one,                                                       &
   1.0,array_zero,array_one,l_vegdrag_ssi,                                     &
   alpha1_sice(:,:,1),ashtf_prime(:,:,1),fqw_ice(:,:,1),epot_ice,              &
   ftl_ice(:,:,1),dtstar_sice(:,:,1),sea_point                                 &
   )

  ! Water tracers sea ice surface flux
  IF (l_wtrac_jls) THEN
    DO i_wt = 1, n_wtrac_jls
      ! (Note, the '1.0' in the call is the seasalinityfactor argument)
      CALL sf_flux_ssi_wtrac (i_wt, ssi_pts, sice_pts_ncat(1),                 &
                              sice_index_ncat(:,1), sea_point,  1.0,           &
                              alpha1_sice(:,:,1), rhokh_1_sice_ncats(:,:,1),   &
                              qstar_ice,                                       &
                              dtstar_sice(:,:,1), array_one,                   &
                              r_sice_wtrac(:,:,1,i_wt), 'ice',                 &
                              qw_1_wtrac(:,:,i_wt),                            &
                              fqw_sicat_wtrac(:,:,1,i_wt))
    END DO
  END IF

END IF

! Add sea-ice contribution to gridbox mean fluxes of heat and moisture
DO n = 1, nice_use
!$OMP PARALLEL DO IF(sice_pts_ncat(n) > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)   &
!$OMP SHARED(n, sice_pts_ncat, sice_index_ncat, ssi_index, t_i_length,         &
!$OMP        ftl_1, fssi_ij, sice_frac_ncat, ftl_ice, fqw_1,                   &
!$OMP        fqw_ice) SCHEDULE(STATIC)
  DO k = 1, sice_pts_ncat(n)
    l = sice_index_ncat(k,n)
    j = (ssi_index(l) - 1) / t_i_length + 1
    i = ssi_index(l) - (j-1) * t_i_length
    ftl_1(i,j) = ftl_1(i,j) +                                                  &
                 fssi_ij(i,j) * sice_frac_ncat(l,n) * ftl_ice(i,j,n)
    fqw_1(i,j) = fqw_1(i,j) +                                                  &
                 fssi_ij(i,j) * sice_frac_ncat(l,n) * fqw_ice(i,j,n)
  END DO
!$OMP END PARALLEL DO
END DO

! Water tracers - Add sea-ice contribution to gridbox mean surface flux
IF (l_wtrac_jls) THEN
  DO i_wt = 1, n_wtrac_jls
    DO n = 1, nice_use
!$OMP PARALLEL DO IF(sice_pts_ncat(n) > 1) SCHEDULE(STATIC) DEFAULT(NONE)      &
!$OMP PRIVATE(i, j, k, l)                                                      &
!$OMP SHARED(sice_pts_ncat, sice_index_ncat, ssi_index, t_i_length,            &
!$OMP        fssi_ij, sice_frac_ncat, fqw_1_wtrac, fqw_sicat_wtrac,            &
!$OMP        i_wt, n)
      DO k = 1, sice_pts_ncat(n)
        l = sice_index_ncat(k,n)
        j = (ssi_index(l) - 1) / t_i_length + 1
        i = ssi_index(l) - (j-1) * t_i_length
        fqw_1_wtrac(i,j,i_wt) = fqw_1_wtrac(i,j,i_wt)                          &
                                        + fssi_ij(i,j) * sice_frac_ncat(l,n)   &
                                         * fqw_sicat_wtrac(i,j,n,i_wt)
      END DO
!$OMP END PARALLEL DO
    END DO  ! n
  END DO    ! i_wt
END IF      ! l_wtrac_jls

! Add sea-ice contribution to gridbox mean surface momentum flux
! diagnostic if requested
IF (sf_diag%l_tau_1) THEN
  DO n = 1, nice_use
!$OMP PARALLEL DO IF(sice_pts_ncat(n) > 1) DEFAULT(NONE) PRIVATE(i, j, k, l)   &
!$OMP SHARED(n, sice_pts_ncat, sice_index_ncat, ssi_index, t_i_length,         &
!$OMP        fssi_ij, sice_frac_ncat, rhokm_1_sice_ncats, vshr_ssi,            &
!$OMP        sf_diag) SCHEDULE(STATIC)
    DO k = 1, sice_pts_ncat(n)
      l = sice_index_ncat(k,n)
      j = (ssi_index(l) - 1) / t_i_length + 1
      i = ssi_index(l) - (j-1) * t_i_length
      sf_diag%tau_1(i,j) = sf_diag%tau_1(i,j) +                                &
                           fssi_ij(i,j) * sice_frac_ncat(l,n) *                &
                           rhokm_1_sice_ncats(i,j,n) * vshr_ssi(i,j)
    END DO
!$OMP END PARALLEL DO
  END DO
END IF

! rhokm_1_sice_ncats no longer required
DEALLOCATE(rhokm_1_sice_ncats)

CALL stdev1 (                                                                  &
   ssi_pts,sea_pts,ssi_index,sea_index,fssi_ij,                                &
   bq_1,bt_1,e_sea,h_sea,rhokm_ssi_nohalo,                                     &
   rhostar_sea,vshr_ssi,z0msea,z1_tq_ctile,sea_frac,                           &
   q1_sd,t1_sd                                                                 &
   )
!
IF (nice_use >  1) THEN
  DO n = 1,nice_use
    CALL stdev1 (                                                              &
     ssi_pts,sice_pts_ncat(n),ssi_index,sice_index_ncat(:,n),fssi_ij,          &
     bq_1,bt_1,fqw_ice(:,:,n),ftl_ice(:,:,n),rhokm_ssi_nohalo,                 &
     rhostar_ice,vshr_ssi,z0m_ice(:,:,n),z1_tq_ctile,                          &
     sice_frac_ncat(:,n),                                                      &
     q1_sd,t1_sd                                                               &
     )
  END DO
ELSE
  CALL stdev1 (                                                                &
   ssi_pts,sice_pts_use,ssi_index,sice_index_use,fssi_ij,                      &
   bq_1,bt_1,fqw_ice(:,:,1),ftl_ice(:,:,1),rhokm_ssi_nohalo,                   &
   rhostar_ice,vshr_ssi,z0m_ice(:,:,1),z1_tq_ctile,                            &
   sice_frac_ncat(:,1),                                                        &
   q1_sd,t1_sd                                                                 &
   )

END IF
!
!-----------------------------------------------------------------------
!  4.6 For sea points, calculate the wind mixing energy flux and the
!      sea-surface roughness length on the P-grid, using time-level n
!      quantities.
!-----------------------------------------------------------------------

SELECT CASE (iseasurfalg)
CASE (ip_ss_fixed, ip_ss_surf_div)
  SELECT CASE (i_high_wind_drag)
  CASE (ip_hwdrag_limited)
    ! Limit the neutral drag coefficient at 10m by calculating the
    ! equivalent roughness length and capping the roughness length.
    ! The equivalent wind speed will depend on the value of Charnock's
    ! coefficient.
    z0msea_max = z_10m / ( EXP(vkman / SQRT(cdn_max_sea) ) - 1.0)
  CASE (ip_hwdrag_reduced_v1)
    ! Calculate the maximum roughness length and the high-wind limit
    ! to limit the drag coefficient.
    z0msea_max = z_10m / ( EXP(vkman / SQRT(cdn_max_sea)) - 1.0)
  END SELECT
END SELECT

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, tau, u10n, cdn_lim_loc)             &
!$OMP SHARED(tdims, flandg, rhokm_ssi, vshr_ssi, ice_fract, cd_sea,            &
!$OMP        sf_diag, iseasurfalg, charnock, g, z0msea, l_spec_z0, z0m_scm,    &
!$OMP        z0h_scm, z0h_sea, z0hsea, rhostarmom_sea,                         &
!$OMP        i_high_wind_drag, cdn_max_sea, cdn_hw_sea,                        &
!$OMP        u_cdn_max, u_cdn_hw, z0msea_max, charnock_w)

!$OMP DO SCHEDULE(STATIC)
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end

    IF (flandg(i,j) <  1.0) THEN
      tau = rhokm_ssi(i,j) * vshr_ssi(i,j)             ! P243.130
      IF (ice_fract(i,j) >  0.0)                                               &
        tau = rhostarmom_sea(i,j) * cd_sea(i,j)                                &
              * vshr_ssi(i,j) * vshr_ssi(i,j)

      IF (sf_diag%sfme)                                                        &
        sf_diag%fme(i,j) = (1.0 - ice_fract(i,j)) * tau * SQRT(tau / rhosea)
      !                                                            ! P243.96
      !     Recalculate the momentum roughness length under the older
      !     non-interactive treatments.
      SELECT CASE (iseasurfalg)
        !
      CASE (ip_ss_fixed, ip_ss_surf_div)

        ! Limit Z0MSEA to 0.154m for TAU very small
        IF ( rhostarmom_sea(i,j) > EPSILON(0.0) ) THEN
          z0msea(i,j) = 1.54e-6 / (SQRT(tau / rhostarmom_sea(i,j)) + 1.0e-5)   &
                      +  (charnock / g) * (tau / rhostarmom_sea(i,j))
          z0msea(i,j) = MAX ( z0hsea , z0msea(i,j) )
          !                                       ... P243.B6 (Charnock formula)
          !                    TAU/RHOSTAR is "mod VS squared", see eqn P243.131
        ELSE
          z0msea(i,j) = z0hsea
        END IF
        !
      CASE (ip_ss_surf_div_coupled)

        ! Limit Z0MSEA to 0.154m for TAU very small
        IF ( rhostarmom_sea(i,j) > EPSILON(0.0) ) THEN
          z0msea(i,j) = 1.54e-6 / (SQRT(tau / rhostarmom_sea(i,j)) + 1.0e-5)   &
                      +  (charnock_w(i,j) / g) * (tau / rhostarmom_sea(i,j))
          z0msea(i,j) = MAX ( z0hsea , z0msea(i,j) )
          !                                       ... P243.B6 (Charnock formula)
          !                    TAU/RHOSTAR is "mod VS squared", see eqn P243.131
        ELSE
          z0msea(i,j) = z0hsea
        END IF
        !
      CASE DEFAULT
        !
        !       The momentum roughness length has already been calculated
        !       within the iteration for the Obukhov length.
        !
      END SELECT
      SELECT CASE (i_high_wind_drag)
      CASE (ip_hwdrag_limited)
        z0msea(i,j) = MIN(z0msea(i,j), z0msea_max)
      CASE (ip_hwdrag_reduced_v1)
        !         Calculate 10-m neutral wind based on current stress
        u10n = (SQRT(tau / rhostarmom_sea(i,j)) / vkman) *                     &
               LOG (1.0 + z_10m / z0msea(i,j))
        !         Determine a limiting value of cd
        IF (u10n <= u_cdn_max) THEN
          cdn_lim_loc = cdn_max_sea
        ELSE IF ( (u10n > u_cdn_max) .AND. (u10n < u_cdn_hw) ) THEN
          cdn_lim_loc = cdn_max_sea - (cdn_max_sea - cdn_hw_sea) *             &
            (u10n - u_cdn_max) / (u_cdn_hw - u_cdn_max)
        ELSE
          cdn_lim_loc = cdn_hw_sea
        END IF
        !         Reset the roughness length consistently, leaving aside very
        !         light winds.
        IF (u10n > 1.0)                                                        &
          z0msea(i,j) = MIN(z0msea(i,j),                                       &
            z_10m / ( EXP(vkman / SQRT(cdn_lim_loc) ) - 1.0))
      END SELECT

    END IF

  END DO
END DO
!$OMP END DO


IF ( l_spec_z0 ) THEN
  ! Check for prescribed surface roughness lengths specified in SCM
  ! NAMELIST.  If specified in the &INPROF then they will be used
  ! instead of Model calculated values
!$OMP DO SCHEDULE(STATIC)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      IF ( z0m_scm(i,j) > 0.0 ) THEN
        ! Set z0m from SCM namelist for sea points
        z0msea(i,j)  = z0m_scm(i,j)
      END IF
      IF ( z0h_scm(i,j) > 0.0 ) THEN
        ! Set z0h from SCM namelist for sea points
        z0h_sea(i,j) = z0h_scm(i,j)
      END IF
    END DO
  END DO
!$OMP END DO
END IF
!$OMP END PARALLEL

! The following lines have been commented out since unless accompanied by
! similar changes for cd_std_sea and v_s_std_sea, which do not feature in
! JULES they will cause an inconsistency in the diagnosis of 10-m winds.
! The question of how winds and temperatures should be diagnosed over
! sea ice is being reconsidered.
!--!-----------------------------------------------------------------------
!--! If sea ice is present then set RECIP_L_MO_SEA to its value over
!--! the ice, a long-standing choice for the screen diagnostics.
!--! Note that RECIP_L_MO_SEA is also used in BDY_EXPL2 to diagnose
!--! shear-dominated boundary layer types.  To preserve bit
!--! reproducibility when screen diagnostics are switched on,
!--! this change has been moved outside the if-test on stash logicals
!--!-----------------------------------------------------------------------
!--!DO j=1,rows
!--!  DO i=1,row_length
!--!    IF (flandg(i,j) <  1.0 .AND. ice_fract(i,j) >  0.0 ) THEN
!--!      recip_l_mo_sea(i,j) = recip_l_mo_ice(i,j)
!--!    END IF
!--!  END DO
!--!END DO

IF (sf_diag%l_t10m .OR. sf_diag%l_q10m) THEN
  ALLOCATE(chr10m_sice(tdims%i_start:tdims%i_end,                              &
                       tdims%j_start:tdims%j_end,nice_use))
  ALLOCATE(chr10m_sea(tdims%i_start:tdims%i_end,                               &
                      tdims%j_start:tdims%j_end))
ELSE
  ALLOCATE(chr10m_sice(1,1,nice_use))
  ALLOCATE(chr10m_sea(1,1))
END IF

! Sea and sea-ice (leads ignored)
! The unloading rate for plant canopies is tested since we are
! using the GBM drag coefficient at coastal points for simplicity in the
! calculation of unloading.
IF (sf_diag%su10 .OR. sf_diag%sv10 .OR. sf_diag%sq1p5 .OR.                     &
    sf_diag%st1p5 .OR. sf_diag%suv10m_n .OR.                                   &
    sf_diag%l_t10m .OR. sf_diag%l_q10m .OR.                                    &
    l_cdr10m_snow .OR.                                                         &
    (IScrnTDiag == IP_ScrnDecpl2) .OR.                                         &
    (IScrnTDiag == IP_ScrnDecpl3)) THEN
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i, j)                 &
!$OMP SHARED(tdims, chr1p5m_sea)
  DO j = tdims%j_start,tdims%j_end
    DO i = tdims%i_start,tdims%i_end
      chr1p5m_sea(i,j) = 0.0
    END DO
  END DO
!$OMP END PARALLEL DO
  CALL sfl_int (                                                               &
     ssi_pts,sea_pts,l_cdr10m_snow,sea_index,ssi_index,fssi_ij,                &
     vshr_ssi,cd_std_sea,cd_sea,ch_sea,                                        &
     sea_frac,                                                                 &
     z0msea,z0msea,z0h_sea,                                                    &
     recip_l_mo_sea,                                                           &
     v_s_sea,v_s_std_sea,                                                      &
     z1_uv,z1_tq_ctile,db_sea,                                                 &
     sf_diag,                                                                  &
     cdr10m,sf_diag%cdr10m_n,sf_diag%cd10m_n,chr1p5m_sea,chr10m_sea            &
     )
  !
  DO n = 1,nice_use
!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE) PRIVATE(i, j)                 &
!$OMP SHARED(tdims, chr1p5m_sice, n)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        chr1p5m_sice(i,j,n) = 0.0        ! Initialise
      END DO
    END DO
!$OMP END PARALLEL DO
  END DO

  DO n = 1,nice_use
    CALL sfl_int (                                                             &
       ssi_pts,sice_pts_ncat(n),l_cdr10m_snow,sice_index_ncat(:,n),ssi_index,  &
       fssi_ij,vshr_ssi,cd_std_ice(:,n),cd_ice(:,:,n),ch_ice(:,:,n),           &
       sice_frac_ncat(:,n),                                                    &
       z0m_ice(:,:,n),z0m_ice(:,:,n),z0h_ice(:,:,n),                           &
       recip_l_mo_ice(:,:,n),                                                  &
       v_s_ice(:,:,n),v_s_std_ice(:,n),                                        &
       z1_uv,z1_tq_ctile,db_ice(:,:,n),                                        &
       sf_diag,                                                                &
       cdr10m,sf_diag%cdr10m_n,sf_diag%cd10m_n,chr1p5m_sice(:,:,n),            &
       chr10m_sice(:,:,n)                                                      &
       )
  END DO
  !
  IF (sf_diag%l_t10m .OR. sf_diag%l_q10m) THEN
    IF (nice_use > 1) THEN
      ! Take account of leads, and use multiple thickness categories,
      ! if these are present in CICE.
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, k ,l, n)                            &
!$OMP SHARED(sice_pts_ncat, sice_index_ncat, ssi_index, t_i_length, nice_use,  &
!$OMP        sf_diag, sice_frac_ncat, chr10m_sice, tdims,ice_fract, chr10m_sea)
!$OMP DO SCHEDULE(STATIC)
      DO j = tdims%j_start,tdims%j_end
        DO i = tdims%i_start,tdims%i_end
          sf_diag%chr10m(i,j) = ((1.0 - ice_fract(i,j)) * chr10m_sea(i,j))
        END DO
      END DO
!$OMP END DO

      DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
        DO k = 1,sice_pts_ncat(n)
          l = sice_index_ncat(k,n)
          j=(ssi_index(l) - 1) / t_i_length + 1
          i = ssi_index(l) - (j-1) * t_i_length
          sf_diag%chr10m(i,j) = sf_diag%chr10m(i,j)                            &
                    + (sice_frac_ncat(l,n) * chr10m_sice(i,j,n))
        END DO
!$OMP END DO
      END DO
!$OMP END PARALLEL
    ELSE
      ! Do not include leads or thickness categories.

!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,ice_fract,sf_diag,chr10m_sice,chr10m_sea)
      DO j = tdims%j_start,tdims%j_end
        DO i = tdims%i_start,tdims%i_end
          IF (ice_fract(i,j) >  0.0) THEN
            sf_diag%chr10m(i,j) = chr10m_sice(i,j,1)
          ELSE
            sf_diag%chr10m(i,j) = chr10m_sea(i,j)
          END IF
        END DO
      END DO
!$OMP END PARALLEL DO
    END IF
  END IF
  DEALLOCATE(chr10m_sice)
  DEALLOCATE(chr10m_sea)

  IF (nice_use > 1) THEN
    ! Take account of leads, and use multiple thickness categories,
    ! if these are present in CICE.
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i, j, k, l, n)                            &
!$OMP SHARED(sice_pts_ncat, sice_index_ncat, ssi_index, t_i_length,            &
!$OMP        chr1p5m_ssi_mean, sice_frac_ncat, chr1p5m_sice,                   &
!$OMP        tdims, ice_fract, chr1p5m_sea, nice_use)
!$OMP DO SCHEDULE(STATIC)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        chr1p5m_ssi_mean(i,j) = ((1.0 - ice_fract(i,j)) * chr1p5m_sea(i,j))
      END DO
    END DO
!$OMP END DO
    DO n = 1,nice_use
!$OMP DO SCHEDULE(STATIC)
      DO k = 1,sice_pts_ncat(n)
        l = sice_index_ncat(k,n)
        j=(ssi_index(l) - 1) / t_i_length + 1
        i = ssi_index(l) - (j-1) * t_i_length
        chr1p5m_ssi_mean(i,j) = chr1p5m_ssi_mean(i,j)                          &
                    + (sice_frac_ncat(l,n) * chr1p5m_sice(i,j,n))
      END DO
!$OMP END DO
    END DO
!$OMP END PARALLEL
  ELSE
    ! Do not include leads or thickness categories.
!$OMP PARALLEL DO                                                              &
!$OMP SCHEDULE(STATIC)                                                         &
!$OMP DEFAULT(NONE)                                                            &
!$OMP PRIVATE(j,i)                                                             &
!$OMP SHARED(tdims,ice_fract,chr1p5m_ssi_mean,chr1p5m_sice,chr1p5m_sea)
    DO j = tdims%j_start,tdims%j_end
      DO i = tdims%i_start,tdims%i_end
        IF (ice_fract(i,j) >  0.0) THEN
          chr1p5m_ssi_mean(i,j) = chr1p5m_sice(i,j,1)
        ELSE
          chr1p5m_ssi_mean(i,j) = chr1p5m_sea(i,j)
        END IF
      END DO
    END DO
!$OMP END PARALLEL DO
  END IF

END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

RETURN
END SUBROUTINE jules_ssi_sf_explicit
END MODULE jules_ssi_sf_explicit_mod
