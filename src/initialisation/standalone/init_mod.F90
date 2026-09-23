! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE init_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init(nml_dir, crop_vars_data, crop_vars,                            &
                   psparms_data, psparms,                                      &
                   toppdm, top_pdm_data,                                       &
                   fire_vars, fire_vars_data,                                  &
                   ainfo, ainfo_data,                                          &
                   trif_vars, trif_vars_data,                                  &
                   soilecosse, soil_ecosse_vars_data,                          &
                   aero_data, aerotype,                                        &
                   urban_param, urban_param_data,                              &
                   progs, progs_data,                                          &
                   trifctl_data, trifctltype,                                  &
                   coastal_data, coast,                                        &
                   jules_vars_data, jules_vars,                                &
                   fluxes_data, fluxes,                                        &
                   lake_data, lake_vars,                                       &
                   forcing_data, forcing,                                      &
                   imgn_drive_data, imgn_drive,                                &
                   imgn_vars_data, imgn_vars,                                  &
                  !veg3_parm_(data), &
                  !veg3_field_(data), &
                   chemvars_data, chemvars,                                    &
                   rivers_data, rivers,                                        &
                   water_resources_data, water_resources,                      &
                   wtrac_jls_data, wtrac_jls,                                  &
                   !CABLE: progs, working vars
                   progs_cbl_vars_data, progs_cbl_vars,                        &
                   work_cbl_data, work_cbl                                     &
                  )

USE update_mod,                   ONLY: l_imogen
USE model_interface_mod,          ONLY: check_variable_metadata
USE init_jules_sf_diags_mod,      ONLY: allocate_sf_diags,                     &
                                        set_sf_diag_switches
USE jules_model_environment_mod,  ONLY: lsm_id, cable
USE time_varying_input_mod,       ONLY: seek_all_to_current_datetime
USE model_time_mod,               ONLY: is_spinup
USE init_grid_mod,                ONLY: init_grid
USE init_ancillaries_mod,         ONLY: init_ancillaries
USE init_params_mod,              ONLY: init_params
USE init_ic_mod,                  ONLY: init_ic
USE spinup_init_mod,              ONLY: spinup_init
USE write_dump_mod,               ONLY: write_dump
USE logging_mod,                  ONLY: log_info, init_prnt_control
USE init_output_mod,              ONLY: init_output
USE init_plant_n_uptake_mod,      ONLY: init_plant_n_uptake
USE init_vegetation_mod,          ONLY: init_vegetation
USE init_hydrology_mod,           ONLY: init_hydrology
USE init_model_environment_mod,   ONLY: init_model_environment
USE init_radiation_mod,           ONLY: init_radiation
USE init_snow_mod,                ONLY: init_snow
USE init_surface_types_mod,       ONLY: init_jules_surface_types,              &
                                        init_cable_surface_types
USE init_soil_biogeochem_mod,     ONLY: init_soil_biogeochem
USE init_soil_mod,                ONLY: init_soil
USE init_surface_mod,             ONLY: init_surface
USE init_time_mod,                ONLY: init_time
USE init_drive_mod,               ONLY: init_drive
USE init_rivers_mod,              ONLY: init_rivers
USE init_water_resources_mod,     ONLY: init_water_resources
USE metstats_init_mod,            ONLY: metstats_init
USE metstats_mod,                 ONLY: metstats_allocate
USE init_parms_mod,               ONLY: init_parms
USE init_soil_ecosse_mod,         ONLY: init_soil_ecosse
USE check_compatible_options_mod, ONLY: check_compatible_options
USE init_deposition_mod,          ONLY: init_deposition
USE jules_science_fixes_mod,      ONLY: init_science_fixes
USE init_irrigation_mod,          ONLY: init_irrigation
USE init_urban_mod,               ONLY: init_urban
USE init_fire_weather_index_mod,  ONLY: init_fire_weather_index
USE init_imogen_mod,              ONLY: init_imogen
USE init_prescribed_data_mod,     ONLY: init_prescribed_data
USE init_vars_tmp_mod,            ONLY: init_vars_tmp

! Get fields for veg3_init
USE jules_surface_types_mod,      ONLY: npft, nnpft, ntype
USE ancil_info,                   ONLY: land_pts, nsurft, nmasst
USE veg3_parm_mod,                ONLY: veg3_parm_init
USE veg3_field_mod,               ONLY: veg3_field_init

!TYPE definitions
USE crop_vars_mod,                ONLY: crop_vars_data_type,                   &
                                        crop_vars_type,                        &
                                        crop_vars_assoc
USE p_s_parms,                    ONLY: psparms_data_type,                     &
                                        psparms_type,                          &
                                        psparms_assoc
USE top_pdm,                      ONLY: top_pdm_data_type,                     &
                                        top_pdm_type,                          &
                                        top_pdm_assoc
USE fire_vars_mod,                ONLY: fire_vars_data_type,                   &
                                        fire_vars_type,                        &
                                        fire_vars_assoc
USE ancil_info,                   ONLY: ainfo_data_type,                       &
                                        ainfo_type,                            &
                                        ancil_info_assoc
USE trif_vars_mod,                ONLY: trif_vars_data_type,                   &
                                        trif_vars_type,                        &
                                        trif_vars_assoc
USE soil_ecosse_vars_mod,         ONLY: soil_ecosse_vars_data_type,            &
                                        soil_ecosse_vars_type,                 &
                                        soil_ecosse_vars_assoc
USE aero,                         ONLY: aero_data_type,                        &
                                        aero_type,                             &
                                        aero_assoc
USE urban_param_mod,              ONLY: urban_param_data_type,                 &
                                        urban_param_type,                      &
                                        urban_param_assoc
USE prognostics,                  ONLY: progs_data_type,                       &
                                        progs_type,                            &
                                        prognostics_assoc
USE trifctl,                      ONLY: trifctl_data_type,                     &
                                        trifctl_type,                          &
                                        trifctl_assoc
USE coastal,                      ONLY: coastal_data_type,                     &
                                        coastal_type,                          &
                                        coastal_assoc
USE jules_vars_mod,               ONLY: jules_vars_data_type,                  &
                                        jules_vars_type,                       &
                                        jules_vars_assoc
USE fluxes_mod,                   ONLY: fluxes_data_type,                      &
                                        fluxes_type,                           &
                                        fluxes_assoc
USE lake_mod,                     ONLY: lake_data_type,                        &
                                        lake_type,                             &
                                        lake_assoc
USE jules_forcing_mod,            ONLY: forcing_data_type,                     &
                                        forcing_type,                          &
                                        forcing_assoc
USE imgn_drive_mod,               ONLY: imgn_drive_data_type,                  &
                                        imgn_drive_type,                       &
                                        imgn_drive_assoc
USE imgn_vars_mod,                ONLY: imgn_vars_data_type,                   &
                                        imgn_vars_type,                        &
                                        imgn_vars_assoc
USE jules_rivers_mod,             ONLY: rivers_data_type,                      &
                                        rivers_type,                           &
                                        rivers_assoc
! USE veg3_parm_mod,              ONLY: in_dev
                                        !
                                        !
USE veg3_field_mod,               ONLY: veg3_field_assoc

USE jules_chemvars_mod,           ONLY: chemvars_data_type,                    &
                                        chemvars_type,                         &
                                        chemvars_assoc
USE water_resources_vars_mod,     ONLY: water_resources_data_type,             &
                                        water_resources_type,                  &
                                        water_resources_assoc

USE jules_soil_mod,               ONLY: sm_levels
USE progs_cbl_vars_mod,           ONLY: progs_cbl_vars_type,                   &
                                        progs_cbl_vars_data_type
USE work_vars_mod_cbl,            ONLY: work_vars_type,                        &
                                        work_vars_data_type
USE init_cable_progs_mod,         ONLY: init_cable_progs
USE init_cable_work_mod,          ONLY: init_cable_work

USE jules_wtrac_type_mod,         ONLY: jls_wtrac_type,                        &
                                        jls_wtrac_data_type,                   &
                                        wtrac_jls_assoc

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   The main initialisation routine - initialises the model by calling
!   specialised routines
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!TYPES containing the data passed down to be allocated when possible
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
TYPE(imgn_drive_data_type), INTENT(IN OUT) :: imgn_drive_data
TYPE(imgn_vars_data_type), INTENT(IN OUT) :: imgn_vars_data
TYPE(rivers_data_type), INTENT(IN OUT) :: rivers_data
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm_(data)
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field_(data)
TYPE(chemvars_data_type), INTENT(IN OUT) :: chemvars_data
TYPE(water_resources_data_type), INTENT(IN OUT) :: water_resources_data
TYPE(jls_wtrac_data_type), INTENT(IN OUT)      :: wtrac_jls_data
TYPE(progs_cbl_vars_data_type), INTENT(IN OUT) :: progs_cbl_vars_data
TYPE(work_vars_data_type), INTENT(IN OUT)      :: work_cbl_data

!TYPES pointing to data
TYPE(crop_vars_type), INTENT(IN OUT) :: crop_vars
TYPE(psparms_type), INTENT(IN OUT) :: psparms
TYPE(top_pdm_type), INTENT(IN OUT) :: toppdm
TYPE(fire_vars_type), INTENT(IN OUT) :: fire_vars
TYPE(ainfo_type), INTENT(IN OUT) :: ainfo
TYPE(trif_vars_type), INTENT(IN OUT) :: trif_vars
TYPE(soil_ecosse_vars_type), INTENT(IN OUT) :: soilecosse
TYPE(aero_type), INTENT(IN OUT) :: aerotype
TYPE(urban_param_type), INTENT(IN OUT) :: urban_param
TYPE(progs_type), INTENT(IN OUT) :: progs
TYPE(trifctl_type), INTENT(IN OUT) :: trifctltype
TYPE(coastal_type), INTENT(IN OUT) :: coast
TYPE(jules_vars_type), INTENT(IN OUT) :: jules_vars
TYPE(fluxes_type), INTENT(IN OUT) :: fluxes
TYPE(lake_type), INTENT(IN OUT) :: lake_vars
TYPE(forcing_type), INTENT(IN OUT) :: forcing
TYPE(imgn_drive_type), INTENT(IN OUT) :: imgn_drive
TYPE(imgn_vars_type), INTENT(IN OUT) :: imgn_vars
TYPE(rivers_type), INTENT(IN OUT) :: rivers
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm_(data)
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field_(data)
TYPE(chemvars_type), INTENT(IN OUT) :: chemvars
TYPE(water_resources_type), INTENT(IN OUT) :: water_resources
TYPE(jls_wtrac_type), INTENT(IN OUT)      :: wtrac_jls
TYPE(progs_cbl_vars_type), INTENT(IN OUT) :: progs_cbl_vars
TYPE(work_vars_type), INTENT(IN OUT)      :: work_cbl

!-----------------------------------------------------------------------------

! Check that the metadata for variables is acceptable.
CALL check_variable_metadata

! Set options for output of diagnostic and informative messages.
CALL init_prnt_control(nml_dir)

! Determine what environment JULES is being run in
CALL init_model_environment(nml_dir)

! Initialise the surface types
CALL init_jules_surface_types(nml_dir)

IF ( lsm_id == cable ) THEN
  CALL init_cable_surface_types(nml_dir)
END IF

! Initialise the surface options
CALL init_surface(nml_dir)

! Intialise the times for the run
CALL init_time(nml_dir)

! Initialise radiation options
CALL init_radiation(nml_dir)

! Initialise hydrology options
CALL init_hydrology(nml_dir)

! Initialise soil options
CALL init_soil(nml_dir)

! Initialise vegetation options
! Must be called after init_soil to check soil tiling flags
CALL init_vegetation(nml_dir)

! Initialise irrigation options
CALL init_irrigation(nml_dir)

! Initialise vegetation N uptake model options
CALL init_plant_n_uptake(nml_dir)

! Initialise soil biogeochemistry options.
CALL init_soil_biogeochem(nml_dir)

! Initialise ECOSSE soil biogeochemistry options.
CALL init_soil_ecosse(nml_dir)

! Initialise atmospheric deposition module.
CALL init_deposition(nml_dir)

! Initialise snow options
CALL init_snow(nml_dir)

! Initialise river routing parameters, ancils and grid
CALL init_rivers(nml_dir)

! Initialise water resource options.
CALL init_water_resources(nml_dir)

! Initialise two-tile urban schemes (not ancillary data) and must be called
! after init_radiation
CALL init_urban(nml_dir)

! Initialise science fixes
CALL init_science_fixes(nml_dir)

! Initialise the input, model and output grids **also allocates arrays**
CALL init_grid(nml_dir, crop_vars_data,psparms_data,top_pdm_data,              &
               fire_vars_data,ainfo_data,trif_vars_data,                       &
               soil_ecosse_vars_data, aero_data, urban_param_data, progs_data, &
               trifctl_data, coastal_data,jules_vars_data,                     &
               fluxes_data,                                                    &
               lake_data,                                                      &
               forcing_data,                                                   &
               imgn_drive_data,                                                &
               imgn_vars_data,                                                 &
               rivers_data,                                                    &
               !veg3_parm_(data), &
               !veg3_field_(data), &
               chemvars_data, water_resources_data,                            &
               wtrac_jls_data                                                  &
               )

!Associate the data and pointer types
CALL crop_vars_assoc(crop_vars, crop_vars_data)
CALL psparms_assoc(psparms,psparms_data)
CALL top_pdm_assoc(toppdm, top_pdm_data)
CALL fire_vars_assoc(fire_vars, fire_vars_data)
CALL ancil_info_assoc(ainfo, ainfo_data)
CALL trif_vars_assoc(trif_vars, trif_vars_data)
CALL soil_ecosse_vars_assoc(soilecosse, soil_ecosse_vars_data)
CALL aero_assoc(aerotype,aero_data)
CALL urban_param_assoc(urban_param, urban_param_data)
CALL prognostics_assoc(progs,progs_data)
CALL trifctl_assoc(trifctltype, trifctl_data)
CALL coastal_assoc(coast,coastal_data)
CALL jules_vars_assoc(jules_vars,jules_vars_data)
CALL fluxes_assoc(fluxes,fluxes_data)
CALL lake_assoc(lake_vars,lake_data)
CALL forcing_assoc(forcing,forcing_data)
CALL imgn_drive_assoc(imgn_drive,imgn_drive_data)
CALL imgn_vars_assoc(imgn_vars,imgn_vars_data)
CALL rivers_assoc(rivers,rivers_data)
!CALL veg3_parm_assoc(in_dev)
CALL veg3_field_assoc(progs,ainfo,trifctl_data,trif_vars_data)
CALL chemvars_assoc(chemvars,chemvars_data)
CALL water_resources_assoc(water_resources,water_resources_data)
CALL wtrac_jls_assoc(wtrac_jls,wtrac_jls_data)

! Initialise the model ancils
CALL init_ancillaries(nml_dir, crop_vars, ainfo, trif_vars, urban_param,       &
                      trifctltype, rivers, rivers_data )

IF ( lsm_id == cable ) THEN
  CALL init_cable_progs( land_pts, nsurft, sm_levels, lsm_id, cable,           &
                         progs_cbl_vars, progs_cbl_vars_data )
  CALL init_cable_work( land_pts, nsurft, sm_levels, lsm_id, cable,            &
                         work_cbl, work_cbl_data )
END IF

! Initialise model parameters
CALL init_params(nml_dir,progs,land_pts,nsurft,nnpft,npft,nmasst,ntype)

! Initialise fire module
CALL init_fire_weather_index(nml_dir,ainfo%land_index)

! Contains allocation of progs_data%seed_rain - hence passing in the data type
CALL init_drive(nml_dir,ainfo,progs_data)

! Initialise IMOGEN
! Contains allocation of progs_data%seed_rain - hence passing in the data type
IF ( l_imogen ) THEN
  CALL init_imogen(nml_dir,progs_data, trifctltype, imgn_drive, imgn_vars,     &
                   ainfo)
END IF

! Associate the data and pointer types again after the extra allocation
CALL prognostics_assoc(progs,progs_data)

! Initialise the metstats module.  Potentially any area of the model can
! request that metstats calculate a diagnostic (i.e. sets l_metstats=T), so
! this needs to be done after science options are finalised and before the dump
! is read by init_ic().
CALL metstats_allocate(land_pts)
CALL metstats_init(ainfo)

! Initialise other prescribed data
CALL init_prescribed_data(nml_dir)

! Initialise the model prognostics
CALL init_ic(nml_dir, crop_vars,psparms,toppdm,ainfo,trif_vars, soilecosse,    &
             urban_param, progs, jules_vars, lake_vars, rivers)

! Initialise output
CALL init_output(nml_dir)

!-----------------------------------------------------------------------------
! Other initialisation that does not depend on further user input.
!-----------------------------------------------------------------------------

! Check that the enabled schemes are compatible
CALL check_compatible_options()

! Set sf_diag switches that are required by the science configuration (not by
! the choice of diagnostics).
CALL set_sf_diag_switches

! Allocate for coupled model diagnostics (sf_diag).
CALL allocate_sf_diags

! Further initialisation of variables.
CALL init_vars_tmp(crop_vars,psparms,toppdm,ainfo,trif_vars, aerotype,         &
                   progs,trifctltype, coast, jules_vars, forcing)

! Set index arrays and initialise other variables.
CALL init_parms(psparms,ainfo,urban_param,progs,coast,jules_vars,fluxes)

! Setup and initialise veg3
CALL veg3_field_init(land_pts,nsurft,nnpft,npft,nmasst,ainfo,progs)

! Seek the input files to the start of the run
CALL seek_all_to_current_datetime()

! Save initial state if spinning up. Arrays are allocated here.
IF ( is_spinup ) CALL spinup_init(progs,trifctltype)

! Write an initial dump
CALL write_dump()

CALL log_info("init", "Initialisation is complete")

RETURN

END SUBROUTINE init
END MODULE init_mod
