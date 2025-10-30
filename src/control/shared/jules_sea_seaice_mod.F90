! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_sea_seaice_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains sea ice options and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY: rmdi

USE c_kappai, ONLY: kappai, kappai_snow, kappa_seasurf

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Parameters defining valid values for switches
!-----------------------------------------------------------------------------
! Options for iseasurfalg
INTEGER, PARAMETER ::                                                          &
  ip_ss_solid = -1
         ! Disabling option for solid points
INTEGER, PARAMETER ::                                                          &
  ip_ss_fixed = 0
         ! Original scheme with fixed thermal roughness length
INTEGER, PARAMETER ::                                                          &
  ip_ss_surf_div = 1
         ! Thermal roughness from surface divergence, but
         ! without interactive calculation of roughness lengths
INTEGER, PARAMETER ::                                                          &
  ip_ss_surf_div_int = 2
         ! Thermal roughness from surface divergence, with
         ! interactive calculation of roughness lengths
INTEGER, PARAMETER ::                                                          &
  ip_ss_coare_mq = 3
         ! COARE algorithm, with linear dependence of Charnock's
         ! coefficient on the wind speed, but using z0q for both
         ! heat and moisture
INTEGER, PARAMETER ::                                                          &
  ip_ss_surf_div_coupled = 4
         ! Thermal roughness from surface divergence, but
         ! without interactive calculation of roughness lengths
         ! Charnock's coefficient from the wave model
INTEGER, PARAMETER ::                                                          &
  ip_ss_surf_div_int_coupled = 5
         ! Thermal roughness from surface divergence, with
         ! interactive calculation of roughness lengths
         ! Charnock's coefficient from the wave model
!
! Options for i_high_wind_drag
INTEGER, PARAMETER ::                                                          &
  ip_hwdrag_null    = 0
         ! No special treatment of drag at high winds (default)
INTEGER, PARAMETER ::                                                          &
  ip_hwdrag_limited = 1
         ! The drag at high winds is capped.
INTEGER, PARAMETER ::                                                          &
  ip_hwdrag_reduced_v1 = 2
         ! The drag at high winds is reduced over a range of speeds.
!
!----------------------------------------------------------------------------
! Switches
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_tstar_sice_new = .FALSE.,                                                  &
      ! Calculate sea ice surface temperature in scheme compatible with
      ! multi-categories
      ! This logical is only used in a single category run
  l_ssice_albedo = .FALSE.,                                                    &
      ! Switch for including the effect of snow on the sea-ice albedo
  l_sice_scattering = .FALSE.,                                                 &
      ! Switch for seaice albedo internal scatter
  l_sice_swpen = .FALSE.,                                                      &
      ! Switch for penetration of SW radiation into sea ice
  l_sice_meltponds = .FALSE.,                                                  &
      ! Sea-ice albedo affected by meltponds (simple parameterisation)
  l_sice_meltponds_cice = .FALSE.,                                             &
      ! Sea-ice albedo affected by meltponds (from CICE meltponds scheme)
  l_sice_multilayers = .FALSE.,                                                &
      ! True if coupled to sea ice multilayer model
  l_cice_alb = .FALSE.,                                                        &
      ! T = use sea ice albedo scheme from the CICE model
      ! The sea ice radiation code in control.F90 assumes this is always
      ! FALSE (standalone JULES only)
  l_sice_heatflux = .FALSE.,                                                   &
      ! T: semi-implicit update of TI
  l_saldep_freeze   = .FALSE.,                                                 &
      ! Switch for salinity dependent freezing to form sea ice
  l_icerough_prognostic   = .FALSE.,                                           &
      ! Switch for prognostic sea ice roughness length
  l_ctile = .FALSE.,                                                           &
      ! True if coastal tiling is enabled
  l_use_dtstar_sea = .FALSE.
      ! Allow surface energy balance to modify SST

INTEGER ::                                                                     &
  nice = 0,                                                                    &
      ! Number of sea ice categories available
  nice_use = 0,                                                                &
      ! Number of sea ice categories in use
  iseasurfalg = ip_ss_fixed,                                                   &
      ! Switch for the definition of the roughness lengths over the sea
  buddy_sea = 0,                                                               &
      ! Switch to use the wind speed from adjacent sea points for the sea
      ! part of coastal grid points
  i_high_wind_drag = ip_hwdrag_null
      ! Option to impose a special treatment of drag at high wind speeds.
      ! Set to the null option by default.

! The following setting is needed for setting up (UM-JULES) pseudo level IDs
! for water tracer fields on multiple sea ice categories.  It is not used
! anywhere else in the code.
INTEGER, PARAMETER :: nice_max=25 ! maximum number of sea ice categories

!-----------------------------------------------------------------------------
! Parameters
!-----------------------------------------------------------------------------
REAL(KIND=real_jlslsm) ::                                                      &
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Roughness lengths for sea ice
    z0miz = 1.0e-1,                                                            &
        ! Roughness length for heat, moisture and momentum over the
        ! Marginal Ice Zone (m)
    z0sice = 3.0e-3,                                                           &
        ! Roughness length for heat, moisture and momentum over sea-ice (m)
    z0h_z0m_miz = 1.0,                                                         &
        ! Ratio of thermal to momentum roughness lengths for marginal ice
    z0h_z0m_sice = 1.0,                                                        &
        ! Ratio of thermal to momentum roughness lengths for sea ice
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Roughness lengths for sea
    z0hsea = 4.0e-5,                                                           &
        ! Roughness length for heat, moisture and momentum over sea (m) for
        ! fixed roughness length setting (iseasurfalg = ip_ss_fixed)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Emissivities of sea and sea-ice
    emis_sea = 1.0,                                                            &
        ! Emissivity of open sea
    emis_sice = 1.0,                                                           &
        ! Emissivity of sea-ice
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Items moved here from run_bl / bl_option_mod
    SeaSalinityFactor = 1.0,                                                   &
        ! Scaling of qsat allowing for salinity of sea water
    charnock = 0.011,                                                          &
        ! Charnock parameter used in roughness length calculation over the sea
    cdn_max_sea  = rmdi,                                                       &
        ! Maximum value of the neutral drag coefficient over open sea.
        ! The default is imposed as a cap and is taken from
        ! http://onlinelibrary.wiley.com/doi/10.1029/2004GL019460/pdf
    cdn_hw_sea = rmdi,                                                         &
        ! Neutral drag coefficient at high wind speeds. The default value
        ! and range of speeds are estimated based on Figure 1 of
        ! https://journals.ametsoc.org/doi/10.1175/JPO-D-16-0069.1
    u_cdn_max   = rmdi,                                                        &
        ! Neutral wind speed where the drag begins to be reduced from the
        ! maximum
    u_cdn_hw  = rmdi,                                                          &
        ! Neutral wind speed where the drag attains the high wind value
! Parameters for original HadGEM albedo scheme:
    alpham = 0.72,                                                             &
        ! Albedo of sea-ice at melting point (TM) if .not.l_ssice_albedo, or
        ! Albedo of snow on sea-ice at melting point (TM) if l_ssice_albedo.
        ! "M" for "melting"
    ssalpham = 0.72,                                                           &
        ! Namelist input for alpham if l_ssice_albedo - assigned to alpham in
        ! readlsta.F90.  "M" for "melting"
    alphac = 0.80,                                                             &
       ! Albedo of sea-ice at and below TM-DTICE if .not.l_ssice_albedo, or
       ! Albedo of snow on sea-ice at and below TM-DTICE if l_ssice_albedo
       ! "C" for "cold"
    ssalphac = 0.80,                                                           &
       ! Namelist input for alphac if l_ssice_albedo - assigned to alphac in
       ! readlsta.F90. "C" for "cold"
    alphab = 0.61,                                                             &
       ! Albedo of snow-free sea-ice if l_ssice_albedo.  "B" for "bare".
    dtice = 2.0,                                                               &
       ! Temperature range in which albedo of sea-ice, if .not.l_ssice_albedo,
       ! or of snow on sea-ice, if l_ssice_albedo, varies between its limits
    ssdtice = 2.0,                                                             &
       ! Namelist input for dtice if l_ssice_albedo - assignd to dtice in
       ! readlsta.F90
    dt_bare = 1.0,                                                             &
       ! Temperature range below TM over which meltponds form if l_sice_meltponds
       ! and l_ssice_albedo
    dalb_bare_wet = -0.075,                                                    &
       ! Increment to albedo for each degree temperature rises above TM-DT_BARE.
       ! Only used if l_sice_meltponds and l_ssice_albedo
    pen_rad_frac = 0.20,                                                       &
       ! Fraction of SW radiation that penetrates seaice and scatters back
       ! causing an addition to the albedo. Only active if l_ssice_albedo and
       ! l_sice_scattering
    sw_beta = 0.60,                                                            &
      ! Attenutation parameter for SW in seaice which controls the additional
      ! albedo due to internal scattering
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Parameters for 4-band CICE albedo scheme used within JULES:
    albicev_cice = 0.78,                                                       &
      ! Sea ice albedo (visible)
    albicei_cice = 0.36,                                                       &
      ! Sea ice albedo (near-infrared)
    albsnowv_cice = 0.98,                                                      &
      ! Snow albedo (visible)
    albsnowi_cice = 0.70,                                                      &
      ! Snow albedo (near-infrared)
    albpondv_cice = 0.27,                                                      &
      ! Meltpond albedo (visible)
    albpondi_cice = 0.07,                                                      &
      ! Meltpond albedo (near-infrared)
    ahmax = 0.3,                                                               &
      ! Sea ice albedo in CICE multi-band scheme is constant above this
      ! thickness (metres)
    dalb_mlt_cice = -0.075,                                                    &
      ! Increment to sea ice albedo for each degree temperature rises above
      ! TM-DT_BARE in CICE multi-band scheme:
    dalb_mlts_v_cice = -0.1,                                                   &
      ! Increment to visible albedo of snow on sea ice for each degree
      ! temperature rises above TM-DT_BARE in CICE multi-band scheme
      ! (values for each radiation band)
    dalb_mlts_i_cice = -0.15,                                                  &
      ! Increment to infrared albedo of snow on sea ice for each degree
      ! temperature rises above TM-DT_BARE in CICE multi-band scheme
      ! (values for each radiation band)
    dt_bare_cice = 1.0,                                                        &
       ! Temperature range below TM over which meltponds form
       ! (in CICE albedo scheme)
    dt_snow_cice = 1.0,                                                        &
       ! Temperature range in which temperature of snow on sea-ice varies
       ! between its limits (in CICE albedo scheme)
    pen_rad_frac_cice = 0.2,                                                   &
      ! Fraction of SW radiation that penetrates seaice and scatters back
      ! causing an addition to the albedo in CICE multi-band scheme
    sw_beta_cice = 0.6,                                                        &
      ! Attenutation parameter for SW in seaice which controls the additional
      ! albedo due to internal scattering in the CICE multi-band scheme
    snowpatch = 0.02,                                                          &
      ! Length scale for parameterizing non uniform snow coverage (m)
      ! (used in CICE multi-band albedo scheme)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    hcap_sea = 0.0,                                                            &
        ! Value for open sea heat capacity if required to be non-zero
    beta_evap = 1.0
        ! availability of surface moisture - 0.0 = none, 1.0 = open sea

!-----------------------------------------------------------------------------
! Parameters for the COARE algorithm
!-----------------------------------------------------------------------------
!
LOGICAL :: l_10m_neut = .TRUE.
      ! Use true neutral 10m wind
REAL(KIND=real_jlslsm), PARAMETER :: z_10m = 10.0
      ! Level of wind used in setting Charnock's coefficient
REAL(KIND=real_jlslsm) :: a_chrn_coare = 0.0017
      ! Linear coefficient in setting Charnock's coefficient
      ! in COARE algorithm (Default value from COARE3.5)
REAL(KIND=real_jlslsm) :: b_chrn_coare = -0.005
      ! Constant in setting Charnock's coefficient
      ! in COARE algorithm (Default value from COARE3.5)
REAL(KIND=real_jlslsm) :: u10_min_coare = 0.0
      ! Minimum wind speed for linear variation of Charnock's
      ! coefficient (Default value from COARE3.5)
REAL(KIND=real_jlslsm) :: u10_max_coare = 19.0
      ! Maximum wind speed for linear variation of Charnock's
      ! coefficient (Default value from COARE3.5)

!-----------------------------------------------------------------------------
! Parameters for ice form drag following Lupkes et al. (2012)
!-----------------------------------------------------------------------------
!
LOGICAL :: l_iceformdrag_lupkes = .FALSE.
      ! True to use the diagnostic form drag scheme of Lupkes et al.
      ! (2012) JGR, 117, D13112 or Lupkes & Gryanik (2015) JGR, 120,
      ! p. 552
LOGICAL :: l_stability_lupkes = .FALSE.
      ! True to include the stability dependence from
      ! Lupkes & Gryanik (2015), otherwise use the neutral form
      ! from Lupkes et al. (2012), but with the fetch-dependence
      ! from Lupkes & Gryanik (2015).
REAL(KIND=real_jlslsm)    ::  h_freeboard_min   = 0.286
      ! Minimum height of freeboard
REAL(KIND=real_jlslsm)    ::  h_freeboard_max   = 0.534
      ! Maximum height of freeboard
REAL(KIND=real_jlslsm)    ::  beta_floe         = 1.0
      ! Constant in parametrization of crosswind length of floe
REAL(KIND=real_jlslsm)    ::  d_floe_min        = 8.0
      ! Minimum crosswind length of floe
REAL(KIND=real_jlslsm)    ::  d_floe_max        = 300.0
      ! Maximum crosswind length of floe
REAL(KIND=real_jlslsm)    ::  ss_floe           = 0.5
      ! Sheltering constant
REAL(KIND=real_jlslsm)    ::  ce_floe           = 0.222
      ! Effective resistance coefficient: NB. This is scaled from
      ! the value given under E2016A in Elvidge et al. (2016) to
      ! account for the more accurate treatment of the airflow
      ! near the ice.

!-----------------------------------------------------------------------------
! Namelist used in UM only
!-----------------------------------------------------------------------------
NAMELIST  / jules_sea_seaice/                                                  &
! Switches
    nice, nice_use, l_tstar_sice_new, l_ssice_albedo, l_sice_scattering,       &
    l_sice_swpen, l_sice_meltponds,  l_sice_meltponds_cice,                    &
    l_sice_multilayers, l_cice_alb, l_sice_heatflux, l_saldep_freeze,          &
    l_icerough_prognostic,                                                     &
    l_ctile, l_iceformdrag_lupkes, l_stability_lupkes, iseasurfalg,            &
    l_10m_neut, buddy_sea, i_high_wind_drag, l_use_dtstar_sea,                 &
! Parameters
    z0miz, z0sice, z0h_z0m_miz, z0h_z0m_sice, z0hsea, emis_sea, emis_sice,     &
    kappai, kappai_snow, kappa_seasurf, SeaSalinityFactor, charnock,           &
    a_chrn_coare, b_chrn_coare, u10_min_coare, u10_max_coare,                  &
    cdn_max_sea, cdn_hw_sea, u_cdn_max, u_cdn_hw,                              &
    alpham, ssalpham, alphac, ssalphac, alphab, dtice, ssdtice,                &
    dt_bare,dalb_bare_wet,pen_rad_frac,sw_beta,                                &
    albicev_cice, albicei_cice, albsnowv_cice, albsnowi_cice,                  &
    albpondv_cice, albpondi_cice,                                              &
    ahmax, dalb_mlt_cice, dalb_mlts_v_cice, dalb_mlts_i_cice, dt_bare_cice,    &
    dt_snow_cice, pen_rad_frac_cice, sw_beta_cice, snowpatch,                  &
    h_freeboard_min, h_freeboard_max, beta_floe, d_floe_min, d_floe_max,       &
    ss_floe, ce_floe, hcap_sea, beta_evap



CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_SEA_SEAICE_MOD'

CONTAINS

SUBROUTINE check_jules_sea_seaice()

USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SEA_SEAICE namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errorstatus

! Check iseasurfalg takes one of the allowed values
SELECT CASE ( iseasurfalg )
CASE ( ip_ss_fixed:ip_ss_surf_div_int_coupled )
CASE DEFAULT
  errorstatus = 101
  CALL ereport("check_jules_sea_seaice", errorstatus,                          &
               "iseasurfalg must be 0, 1, 2, 3, 4 or 5")
END SELECT

! Check the option for drag at high wind speed.
IF ( i_high_wind_drag /= ip_hwdrag_null .AND.                                  &
     i_high_wind_drag /= ip_hwdrag_limited .AND.                               &
     i_high_wind_drag /= ip_hwdrag_reduced_v1 ) THEN
  errorstatus = 101
  CALL ereport("check_jules_sea_seaice", errorstatus,                          &
               "i_high_wind_drag must be 0 (null), 1 (limited), " //           &
               "or 2 (reduced).")
END IF

! Check buddy_sea takes one of the allowed values
IF ( buddy_sea /= 0 .AND. buddy_sea /= 1 ) THEN
  errorstatus = 101
  CALL ereport("check_jules_sea_seaice", errorstatus,                          &
               "buddy_sea must be 0 (off) or 1 (on)")
END IF

! Check that Lupkes form drag scheme and prognostic roughness length (from CICE
! in coupled model) are not being used at the same time
IF (  l_icerough_prognostic .AND. l_iceformdrag_lupkes) THEN
  errorstatus = 101
  CALL ereport("check_jules_sea_seaice", errorstatus,                          &
               "l_icerough_prognostic and l_iceformdrag_lupkes " //            &
               "cannot both be .true.")
END IF

END SUBROUTINE check_jules_sea_seaice


SUBROUTINE print_nlist_jules_sea_seaice()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_sea_seaice',                                           &
                 'Contents of namelist jules_sea_seaice')

WRITE(lineBuffer, *) '  nice = ', nice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  nice_use = ', nice_use
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_tstar_sice_new = ', l_tstar_sice_new
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_ssice_albedo = ', l_ssice_albedo
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_scattering = ', l_sice_scattering
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_swpen= ', l_sice_swpen
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_meltponds = ', l_sice_meltponds
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_meltponds_cice = ', l_sice_meltponds_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_multilayers = ', l_sice_multilayers
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_cice_alb = ', l_cice_alb
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_saldep_freeze = ', l_saldep_freeze
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_icerough_prognostic = ',l_icerough_prognostic
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_sice_heatflux = ', l_sice_heatflux
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_ctile = ', l_ctile
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_use_dtstar_sea = ', l_use_dtstar_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  iseasurfalg = ', iseasurfalg
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  i_high_wind_drag = ', i_high_wind_drag
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  l_10m_neut = ', l_10m_neut
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  a_chrn_coare = ', a_chrn_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  b_chrn_coare = ', b_chrn_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  u10_min_coare = ', u10_min_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  u10_max_coare = ', u10_max_coare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, '(A, E10.3)') '  cdn_max_sea = ', cdn_max_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, '(A, E10.3)') '  cdn_hw_sea = ', cdn_hw_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, '(A, E10.3)') '  u_cdn_max = ', u_cdn_max
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, '(A, E10.3)') '  u_cdn_hw = ', u_cdn_hw
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  buddy_sea = ', buddy_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0miz = ', z0miz
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0sice = ', z0sice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0h_z0m_miz = ', z0h_z0m_miz
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0h_z0m_sice = ', z0h_z0m_sice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  z0hsea = ', z0hsea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  emis_sea = ', emis_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  emis_sice = ', emis_sice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  kappai = ', kappai
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  kappai_snow = ', kappai_snow
CALL jules_print('jules_surface', lineBuffer)

WRITE(lineBuffer, *) '  kappa_seasurf = ', kappa_seasurf
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  SeaSalinityFactor = ', SeaSalinityFactor
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, *) '  charnock = ', charnock
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' alpham = ',alpham
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ssalpham = ',ssalpham
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' alphac = ',alphac
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ssalphac = ',ssalphac
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' alphab = ',alphab
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dtice = ',dtice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ssdtice = ',ssdtice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dt_bare = ',dt_bare
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_bare_wet = ',dalb_bare_wet
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' pen_rad_frac = ',pen_rad_frac
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' sw_beta = ',sw_beta
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albicev_cice  = ', albicev_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albicei_cice  = ', albicei_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albsnowv_cice  = ', albsnowv_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albsnowi_cice  = ', albsnowi_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albpondv_cice  = ', albpondv_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' albpondi_cice  = ', albpondi_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' ahmax = ',ahmax
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_mlt_cice = ',dalb_mlt_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_mlts_v_cice = ',dalb_mlts_v_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dalb_mlts_i_cice = ',dalb_mlts_i_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dt_bare_cice = ',dt_bare_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' dt_snow_cice = ',dt_snow_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' pen_rad_frac_cice = ',pen_rad_frac_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' sw_beta_cice = ',sw_beta_cice
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer,*)' snowpatch = ',snowpatch
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, L1)")' l_iceformdrag_lupkes = ', l_iceformdrag_lupkes
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, L1)")' l_stability_lupkes = ', l_stability_lupkes
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' h_freeboard_min = ', h_freeboard_min
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' h_freeboard_max = ', h_freeboard_max
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' beta_floe = ', beta_floe
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' d_floe_min = ', d_floe_min
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' d_floe_max = ', d_floe_max
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' ss_floe = ', ss_floe
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)")' ce_floe = ', ce_floe
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)") ' hcap_sea = ', hcap_sea
CALL jules_print('jules_sea_seaice', lineBuffer)

WRITE(lineBuffer, "(A, G11.4E2)") ' beta_evap = ', beta_evap
CALL jules_print('jules_sea_seaice', lineBuffer)

CALL jules_print('jules_sea_seaice',                                           &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_sea_seaice

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_sea_seaice (unitnumber)

! Description:
!  Read the jules_sea_seaice namelist

USE setup_namelist, ONLY: setup_nml_type

USE check_iostat_mod, ONLY: check_iostat

USE UM_parcore,       ONLY:   mype
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_SEA_SEAICE'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 5
INTEGER, PARAMETER :: n_real = 55
INTEGER, PARAMETER :: n_log = 16

TYPE :: my_namelist
  SEQUENCE
  INTEGER :: nice
  INTEGER :: nice_use
  INTEGER :: iseasurfalg
  INTEGER :: buddy_sea
  INTEGER :: i_high_wind_drag
  REAL(KIND=real_jlslsm) :: z0miz
  REAL(KIND=real_jlslsm) :: z0sice
  REAL(KIND=real_jlslsm) :: z0h_z0m_miz
  REAL(KIND=real_jlslsm) :: z0h_z0m_sice
  REAL(KIND=real_jlslsm) :: z0hsea
  REAL(KIND=real_jlslsm) :: emis_sea
  REAL(KIND=real_jlslsm) :: emis_sice
  REAL(KIND=real_jlslsm) :: kappai
  REAL(KIND=real_jlslsm) :: kappai_snow
  REAL(KIND=real_jlslsm) :: kappa_seasurf
  REAL(KIND=real_jlslsm) :: SeaSalinityFactor
  REAL(KIND=real_jlslsm) :: charnock
  REAL(KIND=real_jlslsm) :: a_chrn_coare
  REAL(KIND=real_jlslsm) :: b_chrn_coare
  REAL(KIND=real_jlslsm) :: u10_min_coare
  REAL(KIND=real_jlslsm) :: u10_max_coare
  REAL(KIND=real_jlslsm) :: cdn_max_sea
  REAL(KIND=real_jlslsm) :: cdn_hw_sea
  REAL(KIND=real_jlslsm) :: u_cdn_max
  REAL(KIND=real_jlslsm) :: u_cdn_hw
  REAL(KIND=real_jlslsm) :: alpham
  REAL(KIND=real_jlslsm) :: ssalpham
  REAL(KIND=real_jlslsm) :: alphac
  REAL(KIND=real_jlslsm) :: ssalphac
  REAL(KIND=real_jlslsm) :: alphab
  REAL(KIND=real_jlslsm) :: dtice
  REAL(KIND=real_jlslsm) :: ssdtice
  REAL(KIND=real_jlslsm) :: dt_bare
  REAL(KIND=real_jlslsm) :: dalb_bare_wet
  REAL(KIND=real_jlslsm) :: pen_rad_frac
  REAL(KIND=real_jlslsm) :: sw_beta
  REAL(KIND=real_jlslsm) :: albicev_cice
  REAL(KIND=real_jlslsm) :: albicei_cice
  REAL(KIND=real_jlslsm) :: albsnowv_cice
  REAL(KIND=real_jlslsm) :: albsnowi_cice
  REAL(KIND=real_jlslsm) :: albpondv_cice
  REAL(KIND=real_jlslsm) :: albpondi_cice
  REAL(KIND=real_jlslsm) :: ahmax
  REAL(KIND=real_jlslsm) :: dalb_mlt_cice
  REAL(KIND=real_jlslsm) :: dalb_mlts_v_cice
  REAL(KIND=real_jlslsm) :: dalb_mlts_i_cice
  REAL(KIND=real_jlslsm) :: dt_bare_cice
  REAL(KIND=real_jlslsm) :: dt_snow_cice
  REAL(KIND=real_jlslsm) :: pen_rad_frac_cice
  REAL(KIND=real_jlslsm) :: sw_beta_cice
  REAL(KIND=real_jlslsm) :: snowpatch
  REAL(KIND=real_jlslsm) :: h_freeboard_min
  REAL(KIND=real_jlslsm) :: h_freeboard_max
  REAL(KIND=real_jlslsm) :: beta_floe
  REAL(KIND=real_jlslsm) :: d_floe_min
  REAL(KIND=real_jlslsm) :: d_floe_max
  REAL(KIND=real_jlslsm) :: ss_floe
  REAL(KIND=real_jlslsm) :: ce_floe
  REAL(KIND=real_jlslsm) :: hcap_sea
  REAL(KIND=real_jlslsm) :: beta_evap
  LOGICAL :: l_tstar_sice_new
  LOGICAL :: l_ssice_albedo
  LOGICAL :: l_sice_scattering
  LOGICAL :: l_sice_swpen
  LOGICAL :: l_sice_meltponds
  LOGICAL :: l_sice_meltponds_cice
  LOGICAL :: l_sice_multilayers
  LOGICAL :: l_cice_alb
  LOGICAL :: l_saldep_freeze
  LOGICAL :: l_icerough_prognostic
  LOGICAL :: l_sice_heatflux
  LOGICAL :: l_ctile
  LOGICAL :: l_iceformdrag_lupkes
  LOGICAL :: l_stability_lupkes
  LOGICAL :: l_10m_neut
  LOGICAL :: l_use_dtstar_sea
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,               &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_sea_seaice, IOSTAT = errorstatus,       &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_sea_seaice", iomessage)

  my_nml % nice               = nice
  my_nml % nice_use           = nice_use
  my_nml % iseasurfalg        = iseasurfalg
  my_nml % buddy_sea          = buddy_sea
  my_nml % i_high_wind_drag   = i_high_wind_drag
  my_nml % z0miz              = z0miz
  my_nml % z0sice             = z0sice
  my_nml % z0h_z0m_miz        = z0h_z0m_miz
  my_nml % z0h_z0m_sice       = z0h_z0m_sice
  my_nml % z0hsea             = z0hsea
  my_nml % emis_sea           = emis_sea
  my_nml % emis_sice          = emis_sice
  my_nml % kappai             = kappai
  my_nml % kappai_snow        = kappai_snow
  my_nml % kappa_seasurf      = kappa_seasurf
  my_nml % SeaSalinityFactor  = SeaSalinityFactor
  my_nml % charnock           = charnock
  my_nml % a_chrn_coare       = a_chrn_coare
  my_nml % b_chrn_coare       = b_chrn_coare
  my_nml % u10_min_coare      = u10_min_coare
  my_nml % u10_max_coare      = u10_max_coare
  my_nml % cdn_max_sea        = cdn_max_sea
  my_nml % cdn_hw_sea         = cdn_hw_sea
  my_nml % u_cdn_max          = u_cdn_max
  my_nml % u_cdn_hw           = u_cdn_hw
  my_nml % alpham             = alpham
  my_nml % ssalpham           = ssalpham
  my_nml % alphac             = alphac
  my_nml % ssalphac           = ssalphac
  my_nml % alphab             = alphab
  my_nml % dtice              = dtice
  my_nml % ssdtice            = ssdtice
  my_nml % dt_bare            = dt_bare
  my_nml % dalb_bare_wet      = dalb_bare_wet
  my_nml % pen_rad_frac       = pen_rad_frac
  my_nml % sw_beta            = sw_beta
  my_nml % albicev_cice       = albicev_cice
  my_nml % albicei_cice       = albicei_cice
  my_nml % albsnowv_cice      = albsnowv_cice
  my_nml % albsnowi_cice      = albsnowi_cice
  my_nml % albpondv_cice      = albpondv_cice
  my_nml % albpondi_cice      = albpondi_cice
  my_nml % ahmax              = ahmax
  my_nml % dalb_mlt_cice      = dalb_mlt_cice
  my_nml % dalb_mlts_v_cice   = dalb_mlts_v_cice
  my_nml % dalb_mlts_i_cice   = dalb_mlts_i_cice
  my_nml % dt_bare_cice       = dt_bare_cice
  my_nml % dt_snow_cice       = dt_snow_cice
  my_nml % pen_rad_frac_cice  = pen_rad_frac_cice
  my_nml % sw_beta_cice       = sw_beta_cice
  my_nml % snowpatch          = snowpatch
  my_nml % h_freeboard_min    = h_freeboard_min
  my_nml % h_freeboard_max    = h_freeboard_max
  my_nml % beta_floe          = beta_floe
  my_nml % d_floe_min         = d_floe_min
  my_nml % d_floe_max         = d_floe_max
  my_nml % ss_floe            = ss_floe
  my_nml % ce_floe            = ce_floe
  my_nml % hcap_sea           = hcap_sea
  my_nml % beta_evap          = beta_evap
  my_nml % l_tstar_sice_new   = l_tstar_sice_new
  my_nml % l_ssice_albedo     = l_ssice_albedo
  my_nml % l_sice_scattering  = l_sice_scattering
  my_nml % l_sice_swpen       = l_sice_swpen
  my_nml % l_sice_meltponds   = l_sice_meltponds
  my_nml % l_sice_meltponds_cice = l_sice_meltponds_cice
  my_nml % l_sice_multilayers = l_sice_multilayers
  my_nml % l_cice_alb         = l_cice_alb
  my_nml % l_saldep_freeze    = l_saldep_freeze
  my_nml % l_icerough_prognostic = l_icerough_prognostic
  my_nml % l_sice_heatflux    = l_sice_heatflux
  my_nml % l_ctile            = l_ctile
  my_nml % l_iceformdrag_lupkes = l_iceformdrag_lupkes
  my_nml % l_stability_lupkes = l_stability_lupkes
  my_nml % l_10m_neut         = l_10m_neut
  my_nml % l_use_dtstar_sea   = l_use_dtstar_sea
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  nice               = my_nml % nice
  nice_use           = my_nml % nice_use
  iseasurfalg        = my_nml % iseasurfalg
  buddy_sea          = my_nml % buddy_sea
  i_high_wind_drag   = my_nml % i_high_wind_drag
  z0miz              = my_nml % z0miz
  z0sice             = my_nml % z0sice
  z0h_z0m_miz        = my_nml % z0h_z0m_miz
  z0h_z0m_sice       = my_nml % z0h_z0m_sice
  z0hsea             = my_nml % z0hsea
  emis_sea           = my_nml % emis_sea
  emis_sice          = my_nml % emis_sice
  kappai             = my_nml % kappai
  kappai_snow        = my_nml % kappai_snow
  kappa_seasurf      = my_nml % kappa_seasurf
  SeaSalinityFactor  = my_nml % SeaSalinityFactor
  charnock           = my_nml % charnock
  a_chrn_coare       = my_nml % a_chrn_coare
  b_chrn_coare       = my_nml % b_chrn_coare
  u10_min_coare      = my_nml % u10_min_coare
  u10_max_coare      = my_nml % u10_max_coare
  cdn_max_sea        = my_nml % cdn_max_sea
  cdn_hw_sea         = my_nml % cdn_hw_sea
  u_cdn_max          = my_nml % u_cdn_max
  u_cdn_hw           = my_nml % u_cdn_hw
  alpham             = my_nml % alpham
  ssalpham           = my_nml % ssalpham
  alphac             = my_nml % alphac
  ssalphac           = my_nml % ssalphac
  alphab             = my_nml % alphab
  dtice              = my_nml % dtice
  ssdtice            = my_nml % ssdtice
  dt_bare            = my_nml % dt_bare
  dalb_bare_wet      = my_nml % dalb_bare_wet
  pen_rad_frac       = my_nml % pen_rad_frac
  sw_beta            = my_nml % sw_beta
  albicev_cice       = my_nml % albicev_cice
  albicei_cice       = my_nml % albicei_cice
  albsnowv_cice      = my_nml % albsnowv_cice
  albsnowi_cice      = my_nml % albsnowi_cice
  albpondv_cice      = my_nml % albpondv_cice
  albpondi_cice      = my_nml % albpondi_cice
  ahmax              = my_nml % ahmax
  dalb_mlt_cice      = my_nml % dalb_mlt_cice
  dalb_mlts_v_cice   = my_nml % dalb_mlts_v_cice
  dalb_mlts_i_cice   = my_nml % dalb_mlts_i_cice
  dt_bare_cice       = my_nml % dt_bare_cice
  dt_snow_cice       = my_nml % dt_snow_cice
  pen_rad_frac_cice  = my_nml % pen_rad_frac_cice
  sw_beta_cice       = my_nml % sw_beta_cice
  snowpatch          = my_nml % snowpatch
  h_freeboard_min    = my_nml % h_freeboard_min
  h_freeboard_max    = my_nml % h_freeboard_max
  beta_floe          = my_nml % beta_floe
  d_floe_min         = my_nml % d_floe_min
  d_floe_max         = my_nml % d_floe_max
  ss_floe            = my_nml % ss_floe
  ce_floe            = my_nml % ce_floe
  hcap_sea           = my_nml % hcap_sea
  beta_evap          = my_nml % beta_evap
  l_tstar_sice_new   = my_nml % l_tstar_sice_new
  l_ssice_albedo     = my_nml % l_ssice_albedo
  l_sice_scattering  = my_nml % l_sice_scattering
  l_sice_swpen       = my_nml % l_sice_swpen
  l_sice_meltponds   = my_nml % l_sice_meltponds
  l_sice_meltponds_cice = my_nml % l_sice_meltponds_cice
  l_sice_multilayers = my_nml % l_sice_multilayers
  l_cice_alb         = my_nml % l_cice_alb
  l_saldep_freeze    = my_nml % l_saldep_freeze
  l_icerough_prognostic = my_nml % l_icerough_prognostic
  l_sice_heatflux    = my_nml % l_sice_heatflux
  l_ctile            = my_nml % l_ctile
  l_iceformdrag_lupkes = my_nml % l_iceformdrag_lupkes
  l_stability_lupkes = my_nml % l_stability_lupkes
  l_10m_neut         = my_nml % l_10m_neut
  l_use_dtstar_sea   = my_nml % l_use_dtstar_sea
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_sea_seaice
#endif

END MODULE jules_sea_seaice_mod
