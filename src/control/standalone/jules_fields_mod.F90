! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE jules_fields_mod

! Module containing instances of the data TYPES for JULES standalone

!TYPE definitions
USE crop_vars_mod, ONLY: crop_vars_type, crop_vars_data_type
USE p_s_parms,     ONLY: psparms_type, psparms_data_type
USE top_pdm,       ONLY: top_pdm_type, top_pdm_data_type
USE fire_vars_mod, ONLY: fire_vars_type, fire_vars_data_type
USE ancil_info,    ONLY: ainfo_type, ainfo_data_type
USE trif_vars_mod, ONLY: trif_vars_type, trif_vars_data_type
USE soil_ecosse_vars_mod, ONLY: soil_ecosse_vars_type, soil_ecosse_vars_data_type
USE aero,          ONLY: aero_type, aero_data_type
USE urban_param_mod, ONLY: urban_param_type, urban_param_data_type
USE prognostics, ONLY: progs_type, progs_data_type
USE trifctl,       ONLY: trifctl_type, trifctl_data_type
USE coastal,       ONLY: coastal_data_type, coastal_type
USE jules_vars_mod, ONLY: jules_vars_data_type, jules_vars_type
USE fluxes_mod, ONLY: fluxes_data_type, fluxes_type
USE lake_mod, ONLY: lake_data_type, lake_type
USE jules_forcing_mod, ONLY: forcing_data_type, forcing_type
USE jules_rivers_mod, ONLY: rivers_data_type, rivers_type
! USE veg3_parm_mod, ONLY: in_dev
! USE veg3_field_mod, ONLY: in_dev
USE jules_chemvars_mod, ONLY: chemvars_data_type, chemvars_type
USE water_resources_vars_mod, ONLY: water_resources_data_type,                 &
                                    water_resources_type
USE jules_wtrac_type_mod, ONLY: jls_wtrac_data_type, jls_wtrac_type

!Declare instances of the TYPES required to hold the data
!TYPES to hold the data
TYPE(crop_vars_data_type), TARGET :: crop_vars_data
TYPE(psparms_data_type), TARGET :: psparms_data
TYPE(top_pdm_data_type), TARGET :: top_pdm_data
TYPE(fire_vars_data_type), TARGET :: fire_vars_data
TYPE(ainfo_data_type), TARGET :: ainfo_data
TYPE(trif_vars_data_type), TARGET :: trif_vars_data
TYPE(soil_ecosse_vars_data_type), TARGET :: soil_ecosse_vars_data
TYPE(aero_data_type), TARGET :: aero_data
TYPE(urban_param_data_type), TARGET :: urban_param_data
TYPE(progs_data_type), TARGET :: progs_data
TYPE(trifctl_data_type), TARGET :: trifctl_data
TYPE(coastal_data_type), TARGET :: coastal_data
TYPE(jules_vars_data_type), TARGET :: jules_vars_data
TYPE(fluxes_data_type), TARGET :: fluxes_data
TYPE(lake_data_type), TARGET :: lake_data
TYPE(forcing_data_type), TARGET :: forcing_data
TYPE(rivers_data_type), TARGET :: rivers_data
!TYPE(in_dev), TARGET :: veg3_parm_data
!TYPE(in_dev), TARGET :: veg3_field_data
TYPE(chemvars_data_type), TARGET :: chemvars_data
TYPE(water_resources_data_type), TARGET :: water_resources_data
TYPE(jls_wtrac_data_type), TARGET :: wtrac_jls_data

!TYPES we pass around. These happen to be pointers to the data types above
!but this should be transparent
TYPE(crop_vars_type) :: crop_vars
TYPE(psparms_type) :: psparms
TYPE(top_pdm_type) :: toppdm
TYPE(fire_vars_type) :: fire_vars
TYPE(ainfo_type) ::  ainfo
TYPE(trif_vars_type) :: trif_vars
TYPE(soil_ecosse_vars_type) :: soilecosse
TYPE(aero_type) :: aerotype
TYPE(urban_param_type) :: urban_param
TYPE(progs_type) :: progs
TYPE(trifctl_type) :: trifctltype
TYPE(coastal_type) :: coast
TYPE(jules_vars_type) :: jules_vars
TYPE(fluxes_type) :: fluxes
TYPE(lake_type) :: lake_vars
TYPE(forcing_type) :: forcing
TYPE(rivers_type) :: rivers
!TYPE(in_dev) :: veg3_parm
!TYPE(in_dev) :: veg3_field
TYPE(chemvars_type) :: chemvars
TYPE(water_resources_type) :: water_resources
TYPE(jls_wtrac_type) :: wtrac_jls

END MODULE jules_fields_mod
