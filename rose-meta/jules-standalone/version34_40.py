import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn34_vn40(MacroUpgrade):

    """Upgrade macro for JULES vn3.4 to vn4.0 by Matt Pryor."""

    BEFORE_TAG = "vn3.4"
    AFTER_TAG = "vn4.0"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        config = self.upgrade_hydrology(config)
        config = self.upgrade_radiation(config)
        config = self.upgrade_snow(config)
        config = self.upgrade_surface(config)
        config = self.upgrade_surface_types(config)
        config = self.upgrade_soil(config)
        config = self.upgrade_vegetation(config)
        config = self.upgrade_crops(config)

        # Move remaining values from switches
        l_360 = self.get_setting_value(config, ["namelist:jules_switches","l_360"])
        if l_360:
            self.add_setting(config, ["namelist:jules_time","l_360"], l_360)

        l_cosz = self.get_setting_value(config, ["namelist:jules_switches","l_cosz"])
        if l_cosz:
            self.add_setting(config, ["namelist:jules_radiation","l_cosz"], l_cosz)

        l_imogen = self.get_setting_value(config, ["namelist:jules_switches","l_imogen"])
        if l_imogen:
            self.add_setting(config, ["namelist:jules_drive","l_imogen"], l_imogen)

        # Move co2_mmr to ancillaries.nml/JULES_CO2 and remove JULES_AERO
        co2_mmr = self.get_setting_value(config, ["namelist:jules_aero","co2_mmr"])
        self.remove_setting(config, ["namelist:jules_aero"])
        self.add_setting(config, ["namelist:jules_co2"])
        if co2_mmr:
            self.add_setting(config, ["namelist:jules_co2","co2_mmr"], co2_mmr)
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        self.change_setting_value(config, ["file:ancillaries.nml","source"], source + " namelist:jules_co2")

        # Remove any namelists and files that are no longer required but are affected by more than one method
        # of this macro
        self.remove_setting(config, ["namelist:jules_model_levels"])
        self.remove_setting(config, ["file:model_levels.nml"])
        self.remove_setting(config, ["namelist:jules_switches"])
        self.remove_setting(config, ["file:switches.nml"])
        self.remove_setting(config, ["file:misc_params.nml"])

        return config, self.reports


    def upgrade_crops(self, config):
        # First, remove the pft and nvgname variables
        self.remove_setting(config, ["namelist:jules_pftparm", "pftname_io"])
        self.remove_setting(config, ["namelist:jules_nvegparm", "nvgname_io"])

        # Add the JULES_CROP_PROPS namelist as an empty namelist in ancillaries.nml
        self.add_setting(config, ["namelist:jules_crop_props"])
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                      source.replace("namelist:jules_agric",
                                                     "namelist:jules_agric (namelist:jules_crop_props)"))

        # Add the crop_params.nml file
        self.add_setting(config, ["namelist:jules_cropparm"])
        self.add_setting(config, ["file:crop_params.nml", "source"], "namelist:jules_cropparm")

        self.add_setting(config, ["namelist:jules_cropparm", "cfrac_s_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "alpha1_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "delta_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "t_bse_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "allo1_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "cfrac_r_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "alpha3_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "crit_pp_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "t_max_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "beta2_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "rt_dir_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "beta3_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "pp_sens_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "tt_emr_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "remob_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "cfrac_l_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "gamma_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "beta1_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "alpha2_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "t_opt_io"], "-1e20")
        self.add_setting(config, ["namelist:jules_cropparm", "allo2_io"], "-1e20")

        return config


    def upgrade_hydrology(self, config):
        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_hydrology"])

        # Move members from switches to new hydrology namelist
        for member in ["l_top", "l_pdm", "l_baseflow_corr"]:
            value = self.get_setting_value(config, ["namelist:jules_switches",member])
            self.remove_setting(config, ["namelist:jules_switches",member])
            if value:
                self.add_setting(config, ["namelist:jules_hydrology",member], value)

        # Move all the members from the JULES_PDM namelist to JULES_HYDROLOGY and remove it
        for member in ["dz_pdm", "b_pdm"]:
            value = self.get_setting_value(config, ["namelist:jules_pdm",member])
            self.remove_setting(config, ["namelist:jules_pdm",member])
            if value:
                self.add_setting(config, ["namelist:jules_hydrology",member], value)
        self.remove_setting(config, ["namelist:jules_pdm"])
        # Remove the jules_pdm namelist from ancillaries.nml
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"], source.replace("namelist:jules_pdm", ""))

        # Move members from JULES_TOP to JULES_HYDROLOGY
        for member in ["zw_max", "ti_max", "ti_wetl"]:
            value = self.get_setting_value(config, ["namelist:jules_top",member])
            self.remove_setting(config, ["namelist:jules_top",member])
            if value:
                self.add_setting(config, ["namelist:jules_hydrology",member], value)

        # Add the new file containing the namelist
        self.add_setting(config, ["file:jules_hydrology.nml","source"], "namelist:jules_hydrology")

        return config


    def upgrade_radiation(self, config):
        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_radiation"])

        # Move values from switches to new radiation namelist
        for member in ["l_spec_albedo", "l_spec_alb_bs", "l_snow_albedo", "l_albedo_obs",
                       "l_dolr_land_black", "l_spec_sea_alb", "l_sea_alb_var_chl", "i_sea_alb_method"]:
            value = self.get_setting_value(config, ["namelist:jules_switches",member])
            self.remove_setting(config, ["namelist:jules_switches",member])
            if value:
                self.add_setting(config, ["namelist:jules_radiation",member], value)

        # Add the new file containing the namelist
        self.add_setting(config, ["file:jules_radiation.nml","source"], "namelist:jules_radiation")

        return config


    def upgrade_snow(self, config):
        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_snow"])

        # Move nsmax from jules_model_levels to new jules_snow namelist
        nsmax = self.get_setting_value(config, ["namelist:jules_model_levels","nsmax"])
        self.remove_setting(config, ["namelist:jules_model_levels","nsmax"])
        if nsmax:
            self.add_setting(config, ["namelist:jules_snow","nsmax"], nsmax)

        # Move switches from jules_switches to new jules_snow namelist
        for member in ["l_snowdep_surf", "l_rho_snow_corr", "frac_snow_subl_melt"]:
            value = self.get_setting_value(config, ["namelist:jules_switches",member])
            self.remove_setting(config, ["namelist:jules_switches",member])
            if value:
                self.add_setting(config, ["namelist:jules_snow",member], value)

        # Copy all items from jules_rad_param to jules_snow and remove jules_rad_param
        for member in ["r0", "rmax", "snow_ggr", "amax", "maskd", "dtland", "kland_numerator"]:
            value = self.get_setting_value(config, ["namelist:jules_rad_param",member])
            if value:
                self.add_setting(config, ["namelist:jules_snow",member], value)

        self.remove_setting(config, ["namelist:jules_rad_param"])

        # Copy all items from jules_snow_param to jules_snow and remove jules_snow_param
        for member in ["rho_snow_const", "rho_snow_fresh", "snow_hcon", "snow_hcap",
                       "snowliqcap", "snowinterceptfact", "snowloadlai", "snowunloadfact", "cansnowpft"]:
            value = self.get_setting_value(config, ["namelist:jules_snow_param",member])
            if value:
                self.add_setting(config, ["namelist:jules_snow",member], value)

        # dzsnow_io is renamed to dzsnow in the new namelist
        dzsnow = self.get_setting_value(config, ["namelist:jules_snow_param","dzsnow_io"])
        if dzsnow:
            self.add_setting(config, ["namelist:jules_snow","dzsnow"], dzsnow)

        self.remove_setting(config, ["namelist:jules_snow_param"])

        # Remove the now redundant snow_params.nml file
        self.remove_setting(config, ["file:snow_params.nml"])

        # Add the new file containing the namelist
        self.add_setting(config, ["file:jules_snow.nml","source"], "namelist:jules_snow")

        return config


    def upgrade_soil(self, config):
        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_soil"])

        # Move sm_levels from jules_model_levels to new jules_soil namelist
        sm_levels = self.get_setting_value(config, ["namelist:jules_model_levels","sm_levels"])
        self.remove_setting(config, ["namelist:jules_model_levels","sm_levels"])
        if sm_levels:
            self.add_setting(config, ["namelist:jules_soil","sm_levels"], sm_levels)

        # Move switches from jules_switches to new jules_soil namelist
        for member in ["l_vg_soil", "l_dpsids_dsdz", "l_soil_sat_down", "soilhc_method"]:
            value = self.get_setting_value(config, ["namelist:jules_switches",member])
            self.remove_setting(config, ["namelist:jules_switches",member])
            if value:
                self.add_setting(config, ["namelist:jules_soil",member], value)

        # Move values from JULES_SOIL_PARAM to JULES_SOIL and remove it
        for member in ["zsmc", "zst", "confrac", "dzsoil_io"]:
            value = self.get_setting_value(config, ["namelist:jules_soil_param",member])
            if value:
                self.add_setting(config, ["namelist:jules_soil",member], value)
        self.remove_setting(config, ["namelist:jules_soil_param"])
        # Remove the jules_soil_param namelist from ancillaries.nml
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"], source.replace("namelist:jules_soil_param", ""))

        # Move cs_min from JULES_CSMIN to JULES_SOIL and remove it
        cs_min = self.get_setting_value(config, ["namelist:jules_csmin","cs_min"])
        if cs_min:
            self.add_setting(config, ["namelist:jules_soil","cs_min"], cs_min)
        self.remove_setting(config, ["namelist:jules_csmin"])
        # Remove the jules_csmin namelist from misc_params.nml
        source = self.get_setting_value(config, ["file:misc_params.nml","source"])
        if source:
            self.change_setting_value(config, ["file:misc_params.nml","source"], source.replace("namelist:jules_csmin", ""))

        # Add the new file containing the namelist
        self.add_setting(config, ["file:jules_soil.nml","source"], "namelist:jules_soil")

        return config


    def upgrade_surface(self, config):
        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_surface"])

        # Move switches from jules_switches to new jules_surface namelist
        for member in ["l_flake_model", "l_epot_corr", "l_point_data", "l_aggregate",
                       "l_land_ice_imp", "l_anthrop_heat_src", "i_modiscopt", "all_tiles",
                       "cor_mo_iter", "iscrntdiag", "i_aggregate_opt", "formdrag",
                       "fd_stab_dep", "isrfexcnvgust", "orog_drag_param"]:
            value = self.get_setting_value(config, ["namelist:jules_switches",member])
            self.remove_setting(config, ["namelist:jules_switches",member])
            if value:
                self.add_setting(config, ["namelist:jules_surface",member], value)

        # Because the default value of l_epot_corr changed from false to true, we add it with the
        # new default, and it should override false if that is specified
        self.add_setting(config, ["namelist:jules_surface","l_epot_corr"], ".true.")

        # Move surface related values from jules_surf_param to new jules_surface namelist and remove it
        for member in ["hleaf", "hwood", "beta1", "beta2", "fwe_c3", "fwe_c4", "q10_leaf",
                       "q10_soil", "kaps", "kaps_roth"]:
            value = self.get_setting_value(config, ["namelist:jules_surf_param",member])
            if value:
                self.add_setting(config, ["namelist:jules_surface",member], value)
        self.remove_setting(config, ["namelist:jules_surf_param"])
        # Remove the jules_surf_param namelist from misc_params.nml
        source = self.get_setting_value(config, ["file:misc_params.nml","source"])
        if source:
            self.change_setting_value(config, ["file:misc_params.nml","source"], source.replace("namelist:jules_surf_param", ""))

        # Add the new file containing the namelist
        self.add_setting(config, ["file:jules_surface.nml","source"], "namelist:jules_surface")

        return config


    def upgrade_surface_types(self, config):
        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_surface_types"])

        # Move values from jules_model_levels to new jules_surface_types namelist
        for member in ["npft", "nnvg", "urban", "lake", "soil", "ice", "urban_canyon", "urban_roof"]:
            value = self.get_setting_value(config, ["namelist:jules_model_levels",member])
            self.remove_setting(config, ["namelist:jules_model_levels",member])
            if value:
                self.add_setting(config, ["namelist:jules_surface_types",member], value)

        # Add the new file containing the namelist
        self.add_setting(config, ["file:jules_surface_types.nml","source"], "namelist:jules_surface_types")

        return config


    def upgrade_vegetation(self, config):
        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_vegetation"])

        # Move switches from jules_switches to new jules_vegetation namelist
        for member in ["l_phenol", "l_trif_eq", "l_triffid", "l_veg_compete", "l_q10",
                       "l_bvoc_emis", "l_o3_damage", "can_model", "can_rad_mod", "ilayers"]:
            value = self.get_setting_value(config, ["namelist:jules_switches",member])
            self.remove_setting(config, ["namelist:jules_switches",member])
            if value:
                self.add_setting(config, ["namelist:jules_vegetation",member], value)

        # Move values from jules_time to new jules_vegetation namelist
        for member in ["phenol_period", "triffid_period"]:
            value = self.get_setting_value(config, ["namelist:jules_time",member])
            self.remove_setting(config, ["namelist:jules_time",member])
            if value:
                self.add_setting(config, ["namelist:jules_vegetation",member], value)

        # Copy any items from jules_seed and remove it
        for member in ["frac_min", "frac_seed"]:
            value = self.get_setting_value(config, ["namelist:jules_seed",member])
            if value:
                self.add_setting(config, ["namelist:jules_vegetation",member], value)
        self.remove_setting(config, ["namelist:jules_seed"])

        # Copy pow from jules_sigm and remove it
        pow = self.get_setting_value(config, ["namelist:jules_sigm","pow"])
        if pow:
            self.add_setting(config, ["namelist:jules_vegetation","pow"], pow)
        self.remove_setting(config, ["namelist:jules_sigm"])

        # Remove the jules_seed and jules_sigm namelists from misc_params.nml
        source = self.get_setting_value(config, ["file:misc_params.nml","source"])
        if source:
            self.change_setting_value(config, ["file:misc_params.nml","source"], source.replace("namelist:jules_seed", "")
                                                                                       .replace("namelist:jules_sigm", ""))

        # Add the new file containing the namelist
        self.add_setting(config, ["file:jules_vegetation.nml","source"], "namelist:jules_vegetation")

        return config
