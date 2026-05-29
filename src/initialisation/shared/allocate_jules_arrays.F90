MODULE allocate_jules_arrays_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='ALLOCATE_JULES_ARRAYS_MOD'

CONTAINS
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine ALLOCATE_JULES_ARRAYS
!
! Description: Routine that allocates memory to the JULES arrays
! This assume that the values in the jules_surface_types module have been set
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!
!   Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
!   This file belongs in section: Technical

SUBROUTINE allocate_jules_arrays(crop_vars_data,psparms_data,top_pdm_data,     &
                                 fire_vars_data,ainfo_data, trif_vars_data,    &
                                 soil_ecosse_vars_data, aero_data,             &
                                 urban_param_data,progs_data,trifctl_data,     &
                                 coastal_data,jules_vars_data,                 &
                                 fluxes_data,                                  &
                                 lake_data,                                    &
                                 forcing_data,                                 &
                                 rivers_data,                                  &
                                !veg3_parm_(data), &
                                !veg3_field_(data), &
                                 chemvars_data, water_resources_data,          &
                                 wtrac_jls_data                                &
                                )

!Variables- switches
USE jules_vegetation_mod,     ONLY: l_crop, l_triffid, l_phenol, l_use_pft_psi,&
                                    l_acclim, l_sugar, l_red
USE jules_irrig_mod,          ONLY: l_irrig_dmd, irr_crop, irr_crop_doell
USE jules_surface_mod,        ONLY: l_urban2t
USE jules_urban_mod,          ONLY: l_moruses
USE jules_surface_mod,        ONLY: l_flake_model
USE jules_soil_mod,           ONLY: l_bedrock, ns_deep
USE jules_radiation_mod,      ONLY: l_albedo_obs
USE jules_rivers_mod,         ONLY: l_minor_reservoirs
USE jules_soil_biogeochem_mod,ONLY: soil_model_ecosse, soil_bgc_model,         &
                                    l_label_frac_cs, l_layeredc, dim_ch4layer
USE jules_water_resources_mod,ONLY: l_have_groundwater, l_have_surface_water,  &
                                    l_water_domestic, l_water_industry,        &
                                    l_water_irrigation,l_water_livestock,      &
                                    l_water_resources, l_water_transfers,      &
                                    n_sw_source, nwater_use, sw_river_source
USE jules_deposition_mod,     ONLY: l_deposition, ndry_dep_species
USE jules_water_tracers_mod,  ONLY: l_wtrac_jls

!Variables- dimensions
USE jules_surface_types_mod,  ONLY: ncpft,nnpft
USE jules_snow_mod,           ONLY: nsmax, cansnowtile
USE jules_surface_types_mod,  ONLY: npft, nnvg, ntype
#if defined(UM_JULES)
USE atm_land_sea_mask, ONLY: global_land_pts => atmos_number_of_landpts
#else
USE model_grid_mod, ONLY: global_land_pts
#endif
USE theta_field_sizes,        ONLY: t_i_length, t_j_length,                    &
                                    u_i_length,u_j_length,                     &
                                    v_i_length,v_j_length
USE atm_fields_bounds_mod,    ONLY: pdims_s, pdims
USE ancil_info,               ONLY: nsoilt, dim_cs1, dim_cslayer,              &
                                    dim_soil_n_pool,  land_pts,                &
                                    nsurft, nmasst, rad_nband
USE dust_parameters_mod,      ONLY: ndiv
USE nlsizes_namelist_mod,     ONLY: bl_levels
USE jules_sea_seaice_mod,     ONLY: nice, nice_use
USE jules_soil_mod,           ONLY: sm_levels
USE jules_water_tracers_mod,  ONLY: n_wtrac_jls, n_evap_srce

!Subroutines
USE crop_vars_mod,            ONLY: crop_vars_alloc
USE cropparm,                 ONLY: cropparm_alloc
USE c_z0h_z0m,                ONLY: c_z0h_z0m_alloc
USE fire_vars_mod,            ONLY: fire_vars_alloc
USE fluxes_mod,               ONLY: fluxes_alloc
USE jules_vars_mod,           ONLY: jules_vars_alloc
USE jules_irrig_mod,          ONLY: irrig_vars_alloc
USE metstats_mod,             ONLY: metstats_allocate
USE nvegparm,                 ONLY: nvegparm_alloc
USE jules_chemvars_mod,       ONLY: chemvars_alloc
USE pftparm,                  ONLY: pftparm_alloc
USE prognostics,              ONLY: prognostics_alloc
USE p_s_parms,                ONLY: psparms_alloc
USE top_pdm,                  ONLY: top_pdm_alloc
USE trif,                     ONLY: trif_alloc
USE trifctl,                  ONLY: trifctl_alloc
USE trif_vars_mod,            ONLY: trif_vars_alloc
USE urban_param_mod,          ONLY: urban_param_alloc
USE lake_mod,                 ONLY: lake_alloc
USE ancil_info,               ONLY: ancil_info_alloc
USE veg3_parm_mod,            ONLY: veg3_parm_allocate
USE veg3_field_mod,           ONLY: veg3_field_allocate
USE jules_rivers_mod,         ONLY: jules_rivers_alloc
USE jules_forcing_mod,        ONLY: forcing_alloc
USE jules_wtrac_type_mod,     ONLY: wtrac_jls_alloc

#if !defined(UM_JULES)
USE gridmean_fluxes,          ONLY: gridmean_fluxes_alloc
#endif
!Moved from ifdef
USE soil_ecosse_vars_mod,     ONLY: soil_ecosse_vars_alloc
USE aero,                     ONLY: aero_alloc
USE coastal,                  ONLY: coastal_alloc
USE deposition_species_mod,   ONLY: deposition_species_alloc
USE water_resources_vars_mod, ONLY: water_resources_alloc

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_data_type
USE p_s_parms,     ONLY: psparms_data_type
USE top_pdm,       ONLY: top_pdm_data_type
USE fire_vars_mod, ONLY: fire_vars_data_type
USE ancil_info,    ONLY: ainfo_data_type
USE trif_vars_mod, ONLY: trif_vars_data_type
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_data_type
USE aero,          ONLY: aero_data_type
USE urban_param_mod, ONLY:urban_param_data_type
USE prognostics,   ONLY: progs_data_type
USE trifctl,       ONLY: trifctl_data_type
USE coastal,       ONLY: coastal_data_type
USE jules_vars_mod, ONLY: jules_vars_data_type
USE fluxes_mod, ONLY: fluxes_data_type
USE lake_mod, ONLY: lake_data_type
USE jules_forcing_mod,    ONLY: forcing_data_type
USE jules_rivers_mod, ONLY: rivers_data_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
USE jules_chemvars_mod, ONLY: chemvars_data_type
USE water_resources_vars_mod, ONLY: water_resources_data_type
USE jules_wtrac_type_mod,    ONLY: jls_wtrac_data_type

! The following are needed to get the local river grid size
! (which is needed for water tracers)
#if defined(UM_JULES)
USE UM_ParVars,      ONLY: lasize
USE UM_ParParams,    ONLY: halo_type_no_halo
USE Field_Types,     ONLY: fld_type_r
#endif

USE parkind1,                 ONLY: jprb, jpim
USE yomhook,                  ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments

!TYPES containing field data (IN OUT)
TYPE(crop_vars_data_type), INTENT(IN OUT) :: crop_vars_data
TYPE(psparms_data_type), INTENT(IN OUT) :: psparms_data
TYPE(top_pdm_data_type), INTENT(IN OUT) :: top_pdm_data
TYPE(fire_vars_data_type), INTENT(IN OUT) :: fire_vars_data
TYPE(ainfo_data_type), INTENT(IN OUT) :: ainfo_data
TYPE(trif_vars_data_type), INTENT(IN OUT) :: trif_vars_data
TYPE(soil_ecosse_vars_data_type), INTENT(IN OUT) :: soil_ecosse_vars_data
TYPE(aero_data_type), INTENT(IN OUT) :: aero_data
TYPE(urban_param_data_type), INTENT(IN OUT) :: urban_param_data
TYPE(progs_data_type), INTENT(IN OUT) :: progs_data
TYPE(trifctl_data_type), INTENT(IN OUT) :: trifctl_data
TYPE(coastal_data_type), INTENT(IN OUT) :: coastal_data
TYPE(jules_vars_data_type), INTENT(IN OUT) :: jules_vars_data
TYPE(fluxes_data_type), INTENT(IN OUT) :: fluxes_data
TYPE(lake_data_type), INTENT(IN OUT) :: lake_data
TYPE(forcing_data_type), INTENT(IN OUT) :: forcing_data
TYPE(rivers_data_type), INTENT(IN OUT) :: rivers_data
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm_(data)
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field_(data)
TYPE(chemvars_data_type), INTENT(IN OUT) :: chemvars_data
TYPE(water_resources_data_type), INTENT(IN OUT) :: water_resources_data

!Water tracer TYPES containing field data (IN OUT)
TYPE(jls_wtrac_data_type),  INTENT(IN OUT) :: wtrac_jls_data

!Local variables

INTEGER :: i_wt

! Local river grid size
! These are used for water tracers.  They are only set correctly if UM_JULES.
INTEGER :: river_row_length
INTEGER :: river_rows

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='ALLOCATE_JULES_ARRAYS'

!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!Any dimension sizes should be set before we get here. Some special cases for
!UM mode can be found in surf_couple_allocate.

CALL prognostics_alloc(land_pts, t_i_length, t_j_length,                       &
                      nsurft, npft, nsoilt, sm_levels, ns_deep, nsmax,         &
                      dim_cslayer, dim_cs1, dim_ch4layer,                      &
                      nice, nice_use, soil_bgc_model, soil_model_ecosse,       &
                      l_layeredc, l_triffid, l_phenol, l_bedrock, l_red,       &
                      nmasst, nnpft, l_acclim, l_sugar, progs_data)

CALL fluxes_alloc(land_pts, t_i_length, t_j_length,                            &
                  nsurft, npft, nsoilt, sm_levels,                             &
                  nice, nice_use,                                              &
                  fluxes_data)

CALL chemvars_alloc(land_pts, t_i_length, t_j_length, npft, ntype,             &
                    l_deposition, ndry_dep_species, chemvars_data)

CALL crop_vars_alloc(land_pts, t_i_length, t_j_length,                         &
                     nsurft, ncpft,nsoilt, sm_levels, l_crop, irr_crop,        &
                     irr_crop_doell, crop_vars_data)

CALL irrig_vars_alloc(npft, l_irrig_dmd)

CALL cropparm_alloc(ncpft,l_crop)

CALL fire_vars_alloc(land_pts,npft, fire_vars_data)

CALL c_z0h_z0m_alloc(ntype)

CALL jules_vars_alloc(land_pts,ntype,nsurft,rad_nband,nsoilt,sm_levels,        &
                t_i_length, t_j_length, npft, bl_levels, pdims_s, pdims,       &
                l_albedo_obs, cansnowtile, l_deposition,                       &
                jules_vars_data)

CALL metstats_allocate(land_pts)

CALL nvegparm_alloc(nnvg)

CALL pftparm_alloc(npft)

CALL psparms_alloc(land_pts,t_i_length,t_j_length,                             &
                   nsoilt,sm_levels,dim_cslayer,nsurft,npft,                   &
                   soil_bgc_model,soil_model_ecosse,l_use_pft_psi,             &
                   psparms_data)

CALL top_pdm_alloc(land_pts,nsoilt, top_pdm_data)

CALL trif_alloc(npft,                                                          &
                l_triffid, l_phenol)

CALL trifctl_alloc(land_pts,                                                   &
                   npft,dim_cslayer,dim_cs1,nsoilt,trifctl_data)

CALL trif_vars_alloc(land_pts,                                                 &
                     npft,dim_cslayer,nsoilt,dim_cs1,                          &
                     l_triffid, l_phenol, trif_vars_data)

CALL veg3_parm_allocate(land_pts,nsurft,nnpft,npft)
CALL veg3_field_allocate(land_pts,nsurft,nnpft,nmasst)

CALL urban_param_alloc(land_pts,                                               &
                       l_urban2t, l_moruses, urban_param_data)

CALL lake_alloc(land_pts,                                                      &
                l_flake_model, lake_data)

CALL ancil_info_alloc(land_pts,t_i_length,t_j_length,                          &
                      nice,nsoilt,ntype,                                       &
                      ainfo_data)

CALL jules_rivers_alloc(land_pts, t_i_length, t_j_length,                      &
                        sw_river_source, l_water_resources, rivers_data)

CALL forcing_alloc(t_i_length,t_j_length, u_i_length, u_j_length,              &
                   v_i_length, v_j_length, forcing_data)

#if !defined(UM_JULES)
CALL gridmean_fluxes_alloc(t_i_length,t_j_length)
#endif

!Moved from ifdef
CALL soil_ecosse_vars_alloc(land_pts,                                          &
                            nsoilt,dim_cslayer,dim_soil_n_pool,sm_levels,      &
                            soil_bgc_model,soil_model_ecosse,                  &
                            soil_ecosse_vars_data)

CALL aero_alloc(land_pts,t_i_length,t_j_length,                                &
                nsurft,ndiv, aero_data)

CALL coastal_alloc(land_pts,t_i_length,t_j_length,                             &
                   u_i_length,u_j_length,                                      &
                   v_i_length,v_j_length,                                      &
                   nice_use,nice,coastal_data)

CALL deposition_species_alloc(ntype, ndry_dep_species, l_deposition)

CALL water_resources_alloc( global_land_pts,                                   &
                            land_pts, n_sw_source, nwater_use, sw_river_source,&
                            l_have_groundwater, l_have_surface_water,          &
                            l_minor_reservoirs, l_water_domestic,              &
                            l_water_industry,                                  &
                            l_water_irrigation, l_water_livestock,             &
                            l_water_resources, l_water_transfers,              &
                            water_resources_data )

! Set up local river grid sizes  (Note, water tracers only work in UM_JULES)
#if defined(UM_JULES)
river_row_length = lasize(1,fld_type_r,halo_type_no_halo)
river_rows       = lasize(2,fld_type_r,halo_type_no_halo)
#else
river_row_length = 1
river_rows       = 1
#endif

CALL wtrac_jls_alloc(land_pts, t_i_length, t_j_length, nsurft, nsoilt,         &
                       sm_levels, nsmax, nice_use, n_wtrac_jls,                &
                       n_evap_srce, river_row_length, river_rows,              &
                       l_wtrac_jls, wtrac_jls_data)


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE allocate_jules_arrays

END MODULE allocate_jules_arrays_mod
