! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

MODULE surf_couple_radiation_mod

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

PRIVATE
PUBLIC :: surf_couple_radiation

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SURF_COUPLE_RADIATION_MOD'

CONTAINS

!===============================================================================
! Public subroutine
!===============================================================================
SUBROUTINE surf_couple_radiation(                                              &
  !Misc INTENT(IN)
  ws10m, chloro,                                                               &
  n_band, max_n_swbands, wavelength_short, wavelength_long,                    &
  !Misc INTENT(OUT)
  sea_ice_albedo,                                                              &
  !(ancil_info mod)
  nsurft, land_pts, sea_pts, surft_pts, row_length, rows,                      &
  !(coastal mod)
  flandg,                                                                      &
  !(prognostics mod)
  ! Warning- snow_surft causes problems in the UM and passed as an array here
  snow_surft,                                                                  &
  !INTENT(OUT)
  albobs_sc_ij, open_sea_albedo,                                               &
  !TYPES containing field data (IN OUT)
  psparms,ainfo,urban_param,progs,coast,jules_vars,                            &
  fluxes,                                                                      &
  lake_vars,                                                                   &
  !forcing, &
  !rivers, &
  !veg3_parm, &
  !veg3_field, &
  !chemvars, &
  progs_cbl                                                                    &
  )

!Module imports

!TYPE definitions
USE p_s_parms, ONLY: psparms_type
USE ancil_info,    ONLY: ainfo_type
USE urban_param_mod, ONLY: urban_param_type
USE prognostics, ONLY: progs_type
USE coastal, ONLY: coastal_type
USE jules_vars_mod, ONLY: jules_vars_type
USE fluxes_mod, ONLY: fluxes_type
USE lake_mod, ONLY: lake_type
! USE forcing, ONLY: forcing_type
! USE jules_rivers_mod, ONLY: rivers_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
! USE jules_chemvars_mod, ONLY: chemvars_type

USE jules_ssi_albedo_mod,     ONLY: jules_ssi_albedo
USE jules_land_albedo_mod,    ONLY: jules_land_albedo
USE cable_land_albedo_mod,    ONLY: cable_land_albedo

!Common modules
USE ereport_mod,              ONLY:                                            &
  ereport

USE jules_sea_seaice_mod,     ONLY:                                            &
  nice, nice_use,                                                              &
  alpham, alphac, alphab, dtice, dt_bare, dalb_bare_wet,                       &
  pen_rad_frac, sw_beta,                                                       &
  albicev_cice, albicei_cice, albsnowv_cice, albsnowi_cice,                    &
  albpondv_cice, albpondi_cice,                                                &
  ahmax, dalb_mlt_cice, dalb_mlts_v_cice, dalb_mlts_i_cice,                    &
  dt_bare_cice, dt_snow_cice, pen_rad_frac_cice, sw_beta_cice,                 &
  snowpatch

!Potential troublemaker
USE theta_field_sizes,        ONLY:                                            &
  t_i_length, t_j_length

!for testing LSM switch
USE jules_print_mgr,          ONLY: jules_message, jules_print

USE jules_model_environment_mod,         ONLY:                                 &
  lsm_id, jules, cable

! In general CABLE utilizes a required subset of tbe JULES types, however;
USE progs_cbl_vars_mod, ONLY: progs_cbl_vars_type ! CABLE requires extra progs
USE cable_fields_mod,   ONLY: pars_io_cbl         ! and veg/soil parameters
!data: constants
USE cable_surface_types_mod,    ONLY: ICE_cable, lakes_cable

USE jules_surface_types_mod,    ONLY: npft

!Dr Hook
USE parkind1,                 ONLY:                                            &
  jprb, jpim

USE yomhook,                  ONLY:                                            &
  lhook, dr_hook


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Coupling routine between the UM or JULES system code and land surface
!   radiation science routines. Calls the appropriate LSM-specific code.
!
!   Some variables exist in modules only in JULES, others only in the UM
!   Options in order of preference
!   -UM and JULES share the same module names.
!   -UM and JULES have different module names and USE statements go on an ifdef
!   -The UM flavour of the variable does not live in a module. Pass in using a
!    an ifdef'ed argument list
!
!   If there are lots of ifs, then we could cosider splitting lsm_couple into
!   jules_couple and lsm_couple
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------


! Subroutine arguments

! Dimensioning variables
INTEGER, INTENT(IN) ::                                                         &
  n_band,                                                                      &
  max_n_swbands

  !UM-only args: INTENT(IN)
  !(ancil_info mod)
INTEGER, INTENT(IN)::                                                          &
  nsurft, land_pts, sea_pts, surft_pts(nsurft), row_length, rows

!(coastal mod)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
flandg(row_length, rows)

!(prognostics mod)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  snow_surft(land_pts, nsurft)

!UM-only args: INTENT(OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  albobs_sc_ij(t_i_length,t_j_length,nsurft,2),                                &
    !albedo scaling factors to obs
  open_sea_albedo(row_length,rows,2,max_n_swbands)
    !Surface albedo for Open Sea (direct and diffuse components, for each
    !band, with zeros for safety where no value applies)

!Misc INTENT(IN)
REAL(KIND=real_jlslsm), INTENT(IN) ::                                          &
  ws10m(row_length,rows),                                                      &
                                    !10m wind speed
  chloro(row_length,rows)           !nr surface chlorophyll content

REAL(KIND=real_jlslsm), INTENT(IN)    ::                                       &
  wavelength_short(n_band),                                                    &
  wavelength_long(n_band)

!Misc INTENT(OUT)
REAL(KIND=real_jlslsm), INTENT(OUT) ::                                         &
  sea_ice_albedo(row_length,rows,4)   !Surface Albedo for sea ice
                                      ! (*,1) - direct beam visible
                                      ! (*,2) - diffuse visible
                                      ! (*,3) - direct beam near-ir
                                      ! (*,4) - diffuse near-ir

!TYPES containing field data (IN OUT)
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(ainfo_type), INTENT(IN OUT)   :: ainfo
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(coastal_type), INTENT(IN OUT) :: coast
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
TYPE(lake_type), INTENT(IN OUT) :: lake_vars
!TYPE(forcing_type), INTENT(IN OUT) :: forcing
!TYPE(rivers_type), INTENT(IN OUT) :: rivers
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field
!TYPE(chemvars_type), INTENT(IN OUT) :: chemvars

!CABLE TYPES containing field data
TYPE(progs_cbl_vars_type), INTENT(IN) :: progs_cbl

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------

!Land point only versions of ij variables
REAL(KIND=real_jlslsm) :: soot_gb(land_pts)
REAL(KIND=real_jlslsm) :: cosz_gb(land_pts)

!Counters
INTEGER :: i,j,l
INTEGER :: errorstatus

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SURF_COUPLE_RADIATION'

!-----------------------------------------------------------------------------
!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

SELECT CASE( lsm_id )
CASE ( jules )

  !Compress gridded variables to land point only
  DO l = 1, land_pts
    j = (ainfo%land_index(l) - 1) / row_length + 1
    i = ainfo%land_index(l) - (j-1) * row_length
    soot_gb(l)    = progs%soot_ij(i,j)
    cosz_gb(l)    = psparms%cosz_ij(i,j)
  END DO

  CALL jules_land_albedo (                                                     &
    !INTENT(IN)
    t_i_length * t_j_length,                                                   &
    land_pts, nsurft,                                                          &
    ainfo%land_index, surft_pts, ainfo%surft_index,                            &
    psparms%albsoil_soilt, psparms%albobs_sw_gb, psparms%albobs_vis_gb,        &
    psparms%albobs_nir_gb,                                                     &
    cosz_gb, soot_gb, jules_vars%ho2r2_orog_gb,                                &
    progs%lai_pft, progs%canht_pft,                                            &
    progs%rgrain_surft, snow_surft, progs%tstar_surft, psparms%z0_surft,       &
    ainfo%frac_surft,                                                          &
    !INTENT(OUT)
    fluxes%alb_surft,albobs_sc_ij,fluxes%land_albedo_ij,                       &
    !New arguments replacing USE statements
    !jules_vars_mod (IN OUT)
    jules_vars%albobs_scaling_surft,                                           &
    !jules_vars_mod (OUT)
    jules_vars%snowdep_surft,                                                  &
    !urban_param (IN)
    urban_param%albwl_gb, urban_param%albrd_gb, urban_param%hwr_gb,            &
    !lake_mod (IN)
    lake_vars%lake_h_ice_gb,                                                   &
    !ancil_info (IN)
    ainfo%l_lice_point, ainfo%l_lice_surft,                                    &
    !prognostics (IN)
    progs%snowdepth_surft, progs%rho_snow_grnd_surft, progs%nsnow_surft,       &
    progs%sice_surft, progs%sliq_surft, progs%ds_surft)


CASE ( cable )

  CALL cable_land_albedo (                                                     &
    !OUT: (per rad band) albedos [GridBoxMean & per tile albedo]
    fluxes%land_albedo_ij, fluxes%alb_surft,                                   &
    !IN: JULES dimensions and associated
    row_length, rows, land_pts, nsurft, npft,                                  &
    surft_pts, ainfo%surft_index, ainfo%land_index,                            &
    !IN: JULES Surface descriptions generally parametrized
    ainfo%frac_surft, progs%LAI_pft, progs%canht_pft,                          &
    psparms%albsoil_soilt(:,1),                                                &
    !IN: JULES  timestep varying fields
    psparms%cosz_ij, snow_surft,                                               &
    !IN: CABLE specific surface_type indexes
    ICE_cable, lakes_cable,                                                    &
    !IN: CABLE Veg parameters. ICE surface_type index
    pars_io_cbl%vegin_xfang, pars_io_cbl%vegin_taul, pars_io_cbl%vegin_refl,   &
    !IN: CABLE prognostics. decl in progs_cbl_vars_mod.F90 [(:,:,1)= top layer]
    progs_cbl%SoilTemp_CABLE(:,:,1), progs_cbl%OneLyrSnowDensity_CABLE,        &
    progs_cbl%SnowAge_CABLE )


CASE DEFAULT
  errorstatus = 101
  WRITE(jules_message,'(A,I0)') 'Unrecognised surface scheme. lsm_id = ',      &
     lsm_id
  CALL ereport(RoutineName, errorstatus, jules_message)

END SELECT

CALL jules_ssi_albedo (                                                        &
  !INTENT(IN)
  !input fields
  flandg, ainfo%ice_fract_ij, fluxes%tstar_ij, coast%tstar_sice_sicat,         &
  psparms%cosz_ij, ws10m, chloro,                                              &
  progs%snow_mass_sea_sicat, progs%di_ncat_sicat,                              &
  ainfo%pond_frac_cat_sicat, ainfo%pond_depth_cat_sicat,                       &
  !max and min sea ice albedo specifications
  alpham, alphac, alphab, dtice,                                               &
  dt_bare, dalb_bare_wet, pen_rad_frac, sw_beta,                               &
  ! parameters for CICE multi-band albedo scheme:
  albicev_cice, albicei_cice, albsnowv_cice, albsnowi_cice,                    &
  albpondv_cice, albpondi_cice,                                                &
  ahmax, dalb_mlt_cice, dalb_mlts_v_cice, dalb_mlts_i_cice,                    &
  dt_bare_cice, dt_snow_cice,                                                  &
  pen_rad_frac_cice, sw_beta_cice, snowpatch,                                  &
  !size and control variables
  row_length * rows, max_n_swbands,                                            &
  n_band, nice, nice_use,                                                      &
  !spectral boundaries
  wavelength_short,                                                            &
  wavelength_long,                                                             &
  !INTENT(OUT)
  !output arguments
  sea_ice_albedo,                                                              &
  open_sea_albedo,                                                             &
  !ancil_info (IN)
  ainfo%sea_index, ainfo%ssi_index, ainfo%sice_index_ncat,                     &
  ainfo%sice_frac_ncat, ainfo%sice_pts_ncat, sea_pts,                          &
  !Fluxes (OUT)
  fluxes%alb_sicat, fluxes%penabs_rad_frac)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_radiation
END MODULE surf_couple_radiation_mod
