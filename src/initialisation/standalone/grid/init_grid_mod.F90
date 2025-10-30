#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


MODULE init_grid_mod

CONTAINS

SUBROUTINE init_grid(nml_dir, crop_vars_data,psparms_data,top_pdm_data,        &
                              fire_vars_data,ainfo_data,trif_vars_data,        &
                              soil_ecosse_vars_data, aero_data,                &
                              urban_param_data,progs_data,trifctl_data,        &
                              coastal_data,jules_vars_data,                    &
                              fluxes_data,                                     &
                              lake_data,                                       &
                              forcing_data,                                    &
                              imgn_drive_data,                                 &
                              imgn_vars_data,                                  &
                              rivers_data,                                     &
                              !veg3_parm_data, &
                              !veg3_field_data, &
                              chemvars_data, water_resources_data,             &
                              wtrac_jls_data                                   &
                              )

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE errormessagelength_mod, ONLY: errormessagelength

USE logging_mod, ONLY: log_info, log_warn, log_fatal

USE init_model_grid_mod, ONLY: init_model_grid

USE init_model_grid_arrays_mod, ONLY: init_model_grid_arrays

USE init_input_grid_mod, ONLY: init_input_grid
USE init_latlon_mod,     ONLY: init_latlon
USE init_land_frac_mod,  ONLY: init_land_frac
USE init_surf_hgt_mod,   ONLY: init_surf_hgt
USE init_z_land_mod,     ONLY: init_z_land

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
USE prognostics, ONLY: progs_data_type
USE trifctl,       ONLY: trifctl_data_type
USE coastal,       ONLY: coastal_data_type
USE jules_vars_mod, ONLY: jules_vars_data_type
USE fluxes_mod, ONLY: fluxes_data_type
USE lake_mod, ONLY: lake_data_type
USE jules_forcing_mod, ONLY: forcing_data_type
USE imgn_drive_mod, ONLY: imgn_drive_data_type
USE imgn_vars_mod, ONLY: imgn_vars_data_type
USE jules_rivers_mod, ONLY: rivers_data_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
USE jules_chemvars_mod, ONLY: chemvars_data_type
USE water_resources_vars_mod, ONLY: water_resources_data_type
USE jules_wtrac_type_mod, ONLY: jls_wtrac_data_type

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads in information about the model grids
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
TYPE(coastal_data_type), INTENT(IN OUT) :: coastal_data
TYPE(jules_vars_data_type), INTENT(IN OUT) :: jules_vars_data
TYPE(fluxes_data_type), INTENT(IN OUT) :: fluxes_data
TYPE(lake_data_type), INTENT(IN OUT) :: lake_data
TYPE(forcing_data_type), INTENT(IN OUT) :: forcing_data
TYPE(imgn_drive_data_type), INTENT(IN OUT) :: imgn_drive_data
TYPE(imgn_vars_data_type), INTENT(IN OUT) :: imgn_vars_data
TYPE(rivers_data_type), INTENT(IN OUT) :: rivers_data
!TYPE(in_dev), INTENT(IN OUT) :: veg3_parm_data
!TYPE(in_dev), INTENT(IN OUT) :: veg3_field_data
TYPE(chemvars_data_type), INTENT(IN OUT) :: chemvars_data
TYPE(water_resources_data_type), INTENT(IN OUT) :: water_resources_data
TYPE(jls_wtrac_data_type), INTENT(IN OUT) :: wtrac_jls_data

! Work variables
INTEGER :: ERROR  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

TYPE(trifctl_data_type), INTENT(IN OUT) :: trifctl_data

!-----------------------------------------------------------------------------


! Open the grid namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'model_grid.nml'),           &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = ERROR, &
               IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_grid",                                                  &
                 "Error opening namelist file model_grid.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

! Defer to specialised routines to read namelist in model_grid.nml and
! initialise the different aspects of the grid.
CALL init_input_grid()
CALL init_latlon()
CALL init_land_frac()
CALL init_model_grid()
!-----------------------------------------------------------------------------
! Allocate model arrays and fill model grid arrays
! Note: CABLE model arrays are only allocated later on in init.F90 due to a
!       dependency on surft_pts, which has not yet been initialised
!-----------------------------------------------------------------------------
CALL init_model_grid_arrays(crop_vars_data,psparms_data,top_pdm_data,          &
                            fire_vars_data,ainfo_data,trif_vars_data,          &
                            soil_ecosse_vars_data, aero_data,                  &
                            urban_param_data, progs_data,trifctl_data,         &
                            coastal_data, jules_vars_data, fluxes_data,        &
                            lake_data, forcing_data, imgn_drive_data,          &
                            imgn_vars_data, rivers_data, chemvars_data,        &
                            water_resources_data, wtrac_jls_data)
! The following use arrays allocated in previous call.
CALL init_surf_hgt(jules_vars_data)
CALL init_z_land(ainfo_data,jules_vars_data)


CLOSE(namelist_unit, IOSTAT = ERROR, IOMSG = iomessage)
IF ( ERROR /= 0 )                                                              &
  CALL log_fatal("init_grid",                                                  &
                 "Error closing namelist file model_grid.nml " //              &
                 "(IOSTAT=" // TRIM(to_string(ERROR)) // " IOMSG=" //          &
                 TRIM(iomessage) // ")")

RETURN

END SUBROUTINE init_grid

END MODULE init_grid_mod
#endif
