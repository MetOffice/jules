import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

class vn48_t487(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.8"
    AFTER_TAG = "vn4.8_t487"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add prescribed_levels to any jules_prescribed_dataset namelists

        n_datasets = int(self.get_setting_value(config, ["namelist:jules_prescribed", "n_datasets"]))

        sm_levels = int(self.get_setting_value(config, ["namelist:jules_soil", "sm_levels"]))

        prescribed_levels = ", ".join([str(j) for j in range(1, sm_levels + 1)])

        for i in range(1, n_datasets + 1):

             nml_str = "namelist:jules_prescribed_dataset(" + str(i) + ")"

             # n.b. not checking var or setting state here because it will be
             # trig-ignored when appropriate
             self.add_setting(config, [nml_str, "prescribed_levels"], prescribed_levels)

        return config, self.reports

class vn48_t444(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn4.8_t487"
    AFTER_TAG = "vn4.8_t444"

    def upgrade(self, config, meta_config=None):
        """
         Create new namelist jules_soil_ecosse for the ECOSSE soil model.
        """

        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_soil_ecosse"])

        # Add the new compulsory namelist items.
        self.add_setting(config, ["namelist:jules_soil_ecosse", "l_soil_n"], ".true.")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "l_match_layers"], ".false.")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "dim_cslayer"], "4")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "dz_soilc_io"], "0.1, 0.25, 0.65, 2.0")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "dt_soilc"], "-1.0")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "depo_nit_frac"], "1.0")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "l_driver_ave"], ".true.")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "l_decomp_slow"], ".false.")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_iter_max"], "10")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "temp_modifier"], "2")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "water_modifier"], "2")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "bacteria_min_frac"], "0.2")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "bacteria_max_frac"], "0.5")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "bacteria_min_frac_ph"], "4.0")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "bacteria_max_frac_ph"], "5.5")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "cn_bacteria"], "5.5")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "cn_fungi"], "11.5")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_rate"], "3.22e-7, 9.65e-9, 2.12e-8, 6.43e-10")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_wrate_min_rothc"], "0.2")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_wrate_min_jules"], "0.2")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_temp_coeff_rothc"], "47.9 106.0 18.3")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_ph_min"], "1.0")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_ph_max"], "4.5")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "decomp_ph_rate_min"], "0.2")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "depth_nitrif"], "0.35")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "nitrif_rate"], "9.921e-7")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "nitrif_wrate_min"], "0.6")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "nitrif_frac_gas"], "0.02")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "nitrif_frac_n2o_fc"], "0.02")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "nitrif_frac_no"], "0.4")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "nitrif_max_factor"], "0.1")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "denit50"], "0.033")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "denit_frac_n2_fc"], "0.55")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "denit_ratio_n2_zero"], "10.0")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "denit_nitrate_equal"], "0.4")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "denit_water_coeff"], "0.62 0.38 1.74")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "denit_bio_factor"], "50.0")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "amm_leach_min"], "0.02")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "n_inorg_max_conc"], "0.16")

        # Add the new file containing the new namelist
        self.add_setting(config, ["file:jules_soil_ecosse.nml","source"], "namelist:jules_soil_ecosse")

        return config, self.reports

class vn48_t262(MacroUpgrade):
    """Upgrade macro from JULES by Alberto Martinez"""

    BEFORE_TAG = "vn4.8_t444"
    AFTER_TAG = "vn4.8_t262"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_hydrology", "l_spdmvar"], ".false.")
        self.add_setting(config, ["namelist:jules_hydrology", "slope_pdm_max"], "6.0")
        self.add_setting(config, ["namelist:jules_hydrology", "s_pdm"], "0.0")
        self.add_setting(config, ["namelist:jules_pdm", "file"], "")
        self.add_setting(config, ["namelist:jules_pdm", "nvars"], "0")
        self.add_setting(config, ["namelist:jules_pdm", "var"], "")
        self.add_setting(config, ["namelist:jules_pdm", "use_file"], ".false.")
        self.add_setting(config, ["namelist:jules_pdm", "var_name"], "")
        self.add_setting(config, ["namelist:jules_pdm", "tpl_name"], "")
        self.add_setting(config, ["namelist:jules_pdm", "const_val"], "0.0")
        return config, self.reports



class vn48_t541(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.8_t262"
    AFTER_TAG = "vn4.8_t541"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_pftparm", "psi_close_io"],','.join(['-1.5E6']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "psi_open_io"],','.join(['-0.033E6']*npft))

        self.add_setting(config, ["namelist:jules_vegetation", "l_use_pft_psi"], ".false.")
        self.add_setting(config, ["namelist:jules_vegetation", "fsmc_shape"], "0")

        return config, self.reports

class vn48_t518(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn4.8_t541"
    AFTER_TAG = "vn4.8_t518"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add the new compulsory namelist items.
        self.add_setting(config, ["namelist:jules_soil_ecosse", "plant_input_profile"], "1")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "pi_sfc_depth"], "0.05")
        self.add_setting(config, ["namelist:jules_soil_ecosse", "pi_sfc_frac"], "0.3")

        return config, self.reports

class vn48_t491(MacroUpgrade):

    """Upgrade macro from JULES by Kerry Smout-Day"""

    BEFORE_TAG = "vn4.8_t518"
    AFTER_TAG = "vn4.8_t491"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        '''
            Correcting the quotes used in the metadate.
        '''
        veg_value = self.get_setting_value(config, ["namelist:jules_vegetation", "l_irrig_limit"])
        if veg_value == 'trip':
            self.change_setting_value(config, ["namelist:jules_vegetation", "l_irrig_limit"], "'trip'")

        # Add settings
        return config, self.reports

class vn48_t468(MacroUpgrade):

    """Upgrade macro from JULES by Sarah Chadburn"""

    BEFORE_TAG = "vn4.8_t491"
    AFTER_TAG = "vn4.8_t468"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        l_npp = self.get_setting_value(config, ["namelist:jules_hydrology", "l_wetland_ch4_npp"])
        # Add the new compulsory namelist item.
        if l_npp == ".true.":
            self.add_setting(config, ["namelist:jules_soil_biogeochem", "ch4_substrate"], "2")
        else:
            self.add_setting(config, ["namelist:jules_soil_biogeochem", "ch4_substrate"], "1")

        self.add_setting(config, ["namelist:jules_soil_biogeochem", "l_ch4_interactive"], ".false.")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "l_ch4_tlayered"], ".false.")
        self.remove_setting(config, ["namelist:jules_hydrology", "l_wetland_ch4_npp"])
        return config, self.reports


class vn48_t294(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn4.8_t468"
    AFTER_TAG = "vn4.8_t294"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add elev_rock to the namelist with a value of 0 or -1"""
        l_elev_land_ice = self.get_setting_value(config, ["namelist:jules_surface", "l_elev_land_ice"])
        if l_elev_land_ice == ".true.":
            # Add elev_rock with -1 so existing configurations pass fail-if
            self.add_setting(config, ["namelist:jules_surface_types", "elev_rock"], "-1" )
        else:
            # Add an out of range value otherwise to alert user to change the
            # value to something sensible
            self.add_setting(config, ["namelist:jules_surface_types", "elev_rock"], "0" )
        return config, self.reports

class vn48_vn49(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.8_t294"
    AFTER_TAG = "vn4.9"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


'''
Do not edit any lines below this point - this is the template.
'''

class vn48_txxx(MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn4.8_txxx"
    AFTER_TAG = "vn4.8_txxx"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports
