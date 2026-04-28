#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Initialise model from JULES namelist data
!
MODULE surf_couple_allocate_mod

IMPLICIT NONE

! Description:
!  init_jules_from_namelists initialises the relevant JULES arrays
!  from data read in via namelists.
!
! Method:
!  Arrays are allocated in the first call, then initialised from namelist data.
!  Should only be called after all the JULES namelists have been read in.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
!
! Code Description:
!  Language: FORTRAN 95.
!  This code is written to UMDP3 v8.5 programming standards.

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                        &
  ModuleName='SURF_COUPLE_ALLOCATE_MOD'

CONTAINS

SUBROUTINE surf_couple_allocate (land_field, ntiles, sm_levels,                &
                                      nice, nice_use)

USE um_parallel_mod,          ONLY: is_master_task
USE atm_fields_bounds_mod,    ONLY: tdims, vdims, udims
USE theta_field_sizes,        ONLY: t_i_length, t_j_length,                    &
                                    u_i_length, u_j_length,                    &
                                    v_i_length, v_j_length
USE ancil_info,               ONLY: dim_cs1, dim_cslayer,                      &
                                    land_pts, nsurft, nmasst
USE jules_soil_mod,           ONLY: sm_levels_jules => sm_levels

USE allocate_jules_arrays_mod, ONLY: allocate_jules_arrays
USE jules_surface_types_mod,   ONLY: npft, nnpft, nnvg, ntype, soil
USE jules_surface_mod,         ONLY: l_aggregate

USE jules_vegetation_mod, ONLY: l_triffid, l_phenol, can_rad_mod
USE jules_soil_biogeochem_mod,ONLY: l_layeredc

USE metstats_mod, ONLY: metstats_allocate

USE pftparm_io, ONLY: init_pftparm_allocated

USE nvegparm_io, ONLY: init_nvegparm_allocated

USE nvegparm, ONLY: z0_nvg, check_jules_nvegparm

USE trif_io, ONLY:                                                             &
! namelist variables:
  crop_io,         harvest_freq_io,                                            &
  harvest_type_io, ag_expand_io,                                               &
  g_area_io,       g_grow_io,                                                  &
  g_root_io,       g_wood_io,        lai_max_io,                               &
  lai_min_io,      alloc_fast_io,    alloc_med_io,                             &
  alloc_slow_io,   dpm_rpm_ratio_io, retran_l_io,                              &
  retran_r_io,     harvest_ht_io

USE trif, ONLY:                                                                &
  crop,            harvest_freq,                                               &
  harvest_type,    ag_expand,                                                  &
  g_area,          g_grow,                                                     &
  g_root,          g_wood,           lai_max,                                  &
  lai_min,         alloc_fast,       alloc_med,                                &
  alloc_slow,      dpm_rpm_ratio,    retran_l,                                 &
  retran_r,        harvest_ht

USE veg3_parm_mod, ONLY: check_jules_red_parms, veg3_parm_init

USE jules_irrig_mod, ONLY:                                                     &
   irrtiles, irrigtiles, nirrtile, l_irrig_dmd

USE c_elevate, ONLY:                                                           &
! namelist variables:
  surf_hgt_io

USE dust_parameters_mod, ONLY: z0_soil

USE jules_soil_mod, ONLY: check_jules_soil

! Compatibility checks to be run once all allocation has been performed
USE check_compatible_options_mod, ONLY: check_compatible_options

! JULES with atmospheric deposition
USE jules_deposition_mod, ONLY: l_deposition, ndry_dep_species,                &
  dry_dep_model, dry_dep_model_jules, dry_dep_model_ukca

USE deposition_initialisation_aerod_mod, ONLY:                                 &
  deposition_initialisation_aerod_jules, deposition_initialisation_aerod_ukca

USE deposition_initialisation_surfddr_mod, ONLY:                               &
  deposition_initialisation_surfddr_ukca

USE deposition_species_io_mod, ONLY: deposition_io_data_all
! Deposition io variables from set of "jules_deposition_species" namelists
! carried in structure deposition_io_data_all.

USE deposition_species_mod, ONLY:                                              &
! namelist variables
  ch4_scaling,     ch4dd_tundra,     ch4_up_flux,         ch4_mml,             &
  cuticle_o3,      dd_ice_coeff,     dep_species_name,                         &
  diffusion_coeff, diffusion_corr,   dep_species_rmm,                          &
  h2dd_c,          h2dd_m,           h2dd_q,                                   &
  r_tundra,        r_wet_soil_o3,    rsurf_std,                                &
! routines
  check_jules_deposition_species

USE jules_print_mgr,  ONLY: jules_print

USE missing_data_mod, ONLY: imdi

!Pointer association functions
USE crop_vars_mod, ONLY: crop_vars_assoc
USE p_s_parms, ONLY: psparms_assoc
USE top_pdm, ONLY: top_pdm_assoc
USE fire_vars_mod, ONLY: fire_vars_assoc
USE ancil_info, ONLY: ancil_info_assoc
USE trif_vars_mod, ONLY: trif_vars_assoc
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_assoc
USE aero, ONLY: aero_assoc
USE urban_param_mod, ONLY:urban_param_assoc
USE prognostics, ONLY: prognostics_assoc
USE trifctl, ONLY: trifctl_assoc
USE coastal, ONLY: coastal_assoc
USE jules_vars_mod, ONLY: jules_vars_assoc
USE fluxes_mod, ONLY: fluxes_assoc
USE jules_forcing_mod, ONLY: forcing_assoc
USE lake_mod, ONLY: lake_assoc
USE jules_chemvars_mod, ONLY: chemvars_assoc
USE water_resources_vars_mod, ONLY: water_resources_assoc
USE jules_rivers_mod, ONLY: rivers_assoc, np_rivers, l_rivers,                 &
                            i_river_vn, rivers_rfm
USE jules_wtrac_type_mod, ONLY: wtrac_jls_assoc

!USE in instances of the JULES types
USE atm_fields_mod, ONLY: crop_vars, crop_vars_data,                           &
                          psparms, psparms_data,                               &
                          toppdm, top_pdm_data,                                &
                          fire_vars, fire_vars_data,                           &
                          ainfo, ainfo_data,                                   &
                          trif_vars, trif_vars_data,                           &
                          soil_ecosse_vars_data, soilecosse,                   &
                          aero_data, aerotype,                                 &
                          urban_param_data, urban_param,                       &
                          progs_data, progs,                                   &
                          toppdm, top_pdm_data,                                &
                          trifctltype, trifctl_data,                           &
                          coastal_data, coast,                                 &
                          jules_vars_data, jules_vars,                         &
                          fluxes_data, fluxes,                                 &
                          forcing_data, forcing,                               &
                          lake_data, lake_vars,                                &
                          chemvars_data, chemvars,                             &
                          water_resources_data, water_resources,               &
                          rivers_data, rivers,                                 &
                          wtrac_jls_data, wtrac_jls

USE atm_land_sea_mask, ONLY: atmos_number_of_landpts

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: land_field   ! Number of land points
INTEGER, INTENT(IN) :: ntiles       ! Number of surface tiles
INTEGER, INTENT(IN) :: sm_levels    ! Number of soil layers
INTEGER, INTENT(IN) :: nice         ! Number of sea ice categories
INTEGER, INTENT(IN) :: nice_use     ! Number of sea ice categories used
                                    ! fully in surface exch and radiation

! Local variables
INTEGER             :: i            ! Looper

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*), PARAMETER   :: RoutineName='SURF_COUPLE_ALLOCATE'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Set sizes of dimensions

! Compute lengths in i and j direction. This is the earliest place that they
! are needed. They will be kept in the module from here onward.
t_i_length = tdims%i_end - tdims%i_start + 1
t_j_length = tdims%j_end - tdims%j_start + 1

u_i_length = udims%i_end - udims%i_start + 1
u_j_length = udims%j_end - udims%j_start + 1

v_i_length = vdims%i_end - vdims%i_start + 1
v_j_length = vdims%j_end - vdims%j_start + 1

!We need to set the sizes of a few dimensions. These are also exist in
!atm_step_local and set by the same logic in atm_step_phys_init. However,
!This happens after the call to this routine.
!Pending a better approach to JULES memory management we'll repeat the logic
!here
dim_cslayer = 1
IF (l_triffid) THEN
  dim_cs1 = 4
  IF (l_layeredc) THEN
    dim_cslayer = sm_levels
  END IF
ELSE
  dim_cs1 = 1
END IF

!Copy dimension sizes from UM modules to JULES ones.
!Rather ugly but removes the need for ifdefs elsewhere
land_pts        = land_field
nsurft          = ntiles
sm_levels_jules = sm_levels

np_rivers = 1
IF (l_rivers) THEN
  IF (i_river_vn == rivers_rfm .AND. is_master_task()) THEN
    np_rivers = atmos_number_of_landpts
  END IF
END IF

! Allocate JULES arrays
CALL allocate_jules_arrays(crop_vars_data,psparms_data,top_pdm_data,           &
                           fire_vars_data,ainfo_data,trif_vars_data,           &
                           soil_ecosse_vars_data, aero_data, urban_param_data, &
                           progs_data,trifctl_data, coastal_data,              &
                           jules_vars_data, fluxes_data, lake_data,            &
                           forcing_data, rivers_data, chemvars_data,           &
                           water_resources_data, wtrac_jls_data)

! Allocate global_land_index here until jules:#1558 consolidates the allocation
! of arrays between the UM & JULES. In standalone, this is allocated in
! SUBROUTINE allocate_river_vars_grid contained in:
! src/initialisation/standalone/ancillaries/init_rivers_props_mod.F90
IF ( l_rivers ) THEN
  ALLOCATE(rivers_data%global_land_index(atmos_number_of_landpts))
ELSE
  ALLOCATE(rivers_data%global_land_index(1))
END IF
rivers_data%global_land_index(:) = imdi

! Metstats is not available in the UM, but call this to prevent an unallocated
! pointer.
CALL metstats_allocate(land_pts)

!Associate the JULES pointer types to the data types.
!Doing this here reflects when the same is done in standalone JULES
!in init.F90
CALL crop_vars_assoc(crop_vars, crop_vars_data)
CALL psparms_assoc(psparms,psparms_data)
CALL top_pdm_assoc(toppdm, top_pdm_data)
CALL fire_vars_assoc(fire_vars, fire_vars_data)
CALL ancil_info_assoc(ainfo, ainfo_data)
CALL trif_vars_assoc(trif_vars, trif_vars_data)
CALL soil_ecosse_vars_assoc(soilecosse, soil_ecosse_vars_data)
CALL aero_assoc(aerotype, aero_data)
CALL urban_param_assoc(urban_param, urban_param_data)
CALL prognostics_assoc(progs, progs_data)
CALL trifctl_assoc(trifctltype, trifctl_data)
CALL coastal_assoc(coast, coastal_data)
CALL jules_vars_assoc(jules_vars, jules_vars_data)
CALL fluxes_assoc(fluxes, fluxes_data)
CALL forcing_assoc(forcing, forcing_data)
CALL lake_assoc(lake_vars, lake_data)
CALL chemvars_assoc(chemvars, chemvars_data)
CALL water_resources_assoc(water_resources, water_resources_data)
CALL rivers_assoc(rivers, rivers_data)
CALL wtrac_jls_assoc(wtrac_jls, wtrac_jls_data)

! Begin initialising arrays from:

! jules_pftparm
CALL init_pftparm_allocated()

! jules_nvegparm
CALL init_nvegparm_allocated()

! jules_triffid
! Space only allocated if at least phenology is enabled
IF ( l_triffid .OR. l_phenol ) THEN
  crop(:)    = crop_io(1:npft)
  harvest_freq(:) = harvest_freq_io(1:npft)
  harvest_type(:) = harvest_type_io(1:npft)
  ag_expand(:) = ag_expand_io(1:npft)
  g_area(:)  = g_area_io(1:npft)
  g_grow(:)  = g_grow_io(1:npft)
  g_root(:)  = g_root_io(1:npft)
  g_wood(:)  = g_wood_io(1:npft)
  lai_max(:) = lai_max_io(1:npft)
  lai_min(:) = lai_min_io(1:npft)
  alloc_fast(:) = alloc_fast_io(1:npft)
  alloc_med(:)  = alloc_med_io(1:npft)
  alloc_slow(:) = alloc_slow_io(1:npft)
  dpm_rpm_ratio(:) = dpm_rpm_ratio_io(1:npft)
  retran_l(:)      = retran_l_io(1:npft)
  retran_r(:)      = retran_r_io(1:npft)
  harvest_ht(:)    = harvest_ht_io(1:npft)
END IF

!jules_red
CALL veg3_parm_init(land_pts,nsurft,nnpft,npft,nmasst)

! jules_irrig
IF ( l_irrig_dmd ) THEN
  ! irrtiles is now allocated with SIZE(npft) instead of SIZE(npft_max).
  ! It should really be allocated with SIZE(nirrtile) though, this will
  ! be changed through ticket jules:#1065
  irrtiles(1:nirrtile) = irrigtiles(1:nirrtile)
END IF

! jules_deposition
IF ( l_deposition ) THEN

  CALL jules_print(RoutineName,                                                &
               'Check assignment from deposition_io_data_all')

  ! Loop over the species.
  DO i = 1,ndry_dep_species

    ! Load values into the final variables.
    ! Variables that have a value for every species
    ! (even if not necessarily used for all species).
    dep_species_name(i) = deposition_io_data_all % dep_species_name_io(i)
    dd_ice_coeff(i,:)   = deposition_io_data_all % dd_ice_coeff_io(i,:)
    diffusion_coeff(i)  = deposition_io_data_all % diffusion_coeff_io(i)
    dep_species_rmm(i)  = deposition_io_data_all % dep_species_rmm_io(i)
    diffusion_corr(i)   = deposition_io_data_all % diffusion_corr_io(i)
    r_tundra(i)         = deposition_io_data_all % r_tundra_io(i)
    rsurf_std(:,i)      = deposition_io_data_all % rsurf_std_io(1:ntype,i)

    ! Deal with variables that only apply to one species.
    ! If values are given for other species they will be ignored.
    SELECT CASE ( dep_species_name(i) )

    CASE ( 'CH4' )

      ch4_scaling       = deposition_io_data_all % ch4_scaling_io
      ch4_mml           = deposition_io_data_all % ch4_mml_io
      ch4dd_tundra(:)   = deposition_io_data_all % ch4dd_tundra_io(:)
      ch4_up_flux(:)    = deposition_io_data_all % ch4_up_flux_io(1:ntype)

    CASE ( 'H2' )
      h2dd_c(:)         = deposition_io_data_all % h2dd_c_io(1:ntype)
      h2dd_m(:)         = deposition_io_data_all % h2dd_m_io(1:ntype)
      h2dd_q(:)         = deposition_io_data_all % h2dd_q_io(1:ntype)

    CASE ( 'O3' )
      cuticle_o3        = deposition_io_data_all % cuticle_o3_io
      r_wet_soil_o3     = deposition_io_data_all % r_wet_soil_o3_io

    END SELECT

  END DO  ! Species

  CALL check_jules_deposition_species(ntype)

  ! Initialise the parameters for the calculation of
  ! (1) the aerodynamic (Ra) & quasi-laminar (Rb) resistance terms
  ! (2) surface resistance (Rc) terms (for dry_dep_model=dry_dep_model_ukca)
  SELECT CASE ( dry_dep_model )
  CASE ( dry_dep_model_ukca )
    CALL deposition_initialisation_aerod_ukca()
    CALL deposition_initialisation_surfddr_ukca()
  CASE ( dry_dep_model_jules )
    CALL deposition_initialisation_aerod_jules()
  END SELECT

END IF  ! l_deposition

! Check the jules_soil namelist for consistency
CALL check_jules_soil()


! jules_surf_param
! set diffuse fraction to 0.4 for CanRadMod 6
jules_vars%diff_frac(:) = 0.0
IF (can_rad_mod == 6) jules_vars%diff_frac(:) = 0.4

! jules_elevate
! Use same height for all land points.
IF ( l_aggregate ) THEN
  jules_vars%surf_hgt_surft(:,:) = 0.0
ELSE
  DO i = 1,ntiles
    jules_vars%surf_hgt_surft(:,i) = surf_hgt_io(i)
  END DO
END IF

!----------------------------------------------------------------------
! Set bare soil roughness for use in 1 tile dust scheme
!----------------------------------------------------------------------
! fix to enable CRUNs for two bin dust with 1 surface tile
! this mirrors code in sparm. Currently this is a simple real number
! in the future this may become an array and thus will need to be
! updated in line with the sparm code.

z0_soil = z0_nvg(soil - npft)

! Now that the arrays have been allocated and filled we can check them
CALL check_jules_nvegparm(nnvg,npft)
CALL check_jules_red_parms()
CALL check_compatible_options()

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_allocate

END MODULE surf_couple_allocate_mod
#endif
