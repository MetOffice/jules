import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn50_t289(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.0"
    AFTER_TAG = "vn5.0_t289"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_surface", "all_tiles"], "0")
        self.add_setting(config, ["namelist:jules_surface", "cor_mo_iter"], "1")

        # Move the urban_properties namelist from urban.nml to ancillaries.nml
        source = self.get_setting_value(config, ["file:urban.nml","source"])
        if source:
            self.change_setting_value(config, ["file:urban.nml","source"],
                                      source.replace("(namelist:jules_urban2t_param) (namelist:urban_properties)",
                                                     "(namelist:jules_urban2t_param)"))
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                      source.replace("(namelist:jules_rivers_props) namelist:jules_co2",
                                                     "(namelist:jules_rivers_props) (namelist:urban_properties) namelist:jules_co2"))

        return config, self.reports


class vn50_t656(MacroUpgrade):

    """Upgrade macro from JULES by dannyeisenberg"""

    BEFORE_TAG = "vn5.0_t289"
    AFTER_TAG = "vn5.0_t656"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add jules_lsm_switch namelist with default value
        self.add_setting(config, ["file:jules_lsm_switch.nml", "source"], "namelist:jules_lsm_switch")
        self.add_setting(config, ["namelist:jules_lsm_switch", "lsm_id"], "1")

        # Add settings
        return config, self.reports


class vn50_t533(MacroUpgrade):

    """Upgrade macro from JULES by J. M. Edwards"""

    BEFORE_TAG = "vn5.0_t656"
    AFTER_TAG = "vn5.0_t533"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config, ["namelist:jules_snow", "i_basal_melting_opt"], "0")
        return config, self.reports

class vn50_t679(MacroUpgrade):

    """Upgrade macro from JULES by Nic Gedney"""

    BEFORE_TAG = "vn5.0_t533"
    AFTER_TAG = "vn5.0_t679"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add the l_riv_overbank switch in a false state
        self.add_setting(config, ["namelist:jules_rivers", "l_riv_overbank"], ".false.")

        """ Create new namelist jules_overbank. """
        # Add the JULES_OVERBANK namelist
        self.add_setting(config, ["namelist:jules_overbank"])
        # Add it to jules_rivers.nml as an optional namelist
        source = self.get_setting_value(config, ["file:jules_rivers.nml","source"])
        if source:
            self.change_setting_value(config, ["file:jules_rivers.nml","source"],
                                              source.replace("namelist:jules_rivers",
                                                             "namelist:jules_rivers (namelist:jules_overbank)"))

        # Add the new compulsory namelist items.
        self.add_setting(config, ["namelist:jules_overbank", "coef_b"], "0.08")
        self.add_setting(config, ["namelist:jules_overbank", "ent_ratio"], "2.0")
        self.add_setting(config, ["namelist:jules_overbank", "exp_c"], "0.95")
        self.add_setting(config, ["namelist:jules_overbank", "l_riv_hypsometry"], ".false.")
        self.add_setting(config, ["namelist:jules_overbank", "riv_a"], "7.20")
        self.add_setting(config, ["namelist:jules_overbank", "riv_b"], "0.50")
        self.add_setting(config, ["namelist:jules_overbank", "riv_c"], "0.27")
        self.add_setting(config, ["namelist:jules_overbank", "riv_f"], "0.30")
        self.add_setting(config, ["namelist:jules_overbank", "use_rosgen"], ".false.")

        """ Create new namelist jules_overbank_props. """
        # Add the JULES_OVERBANK_PROPS namelist
        self.add_setting(config, ["namelist:jules_overbank_props"])
        # Add it to ancillaries.nml as an optional namelist
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                              source.replace("(namelist:jules_rivers_props)",
                                                             "(namelist:jules_rivers_props) (namelist:jules_overbank_props)"))


        # Add settings
        return config, self.reports


class vn50_t483(MacroUpgrade):

    """Upgrade macro from JULES by Edward Comyn-Platt"""

    BEFORE_TAG = "vn5.0_t679"
    AFTER_TAG = "vn5.0_t483"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "t0_ch4"], "273.15")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "const_ch4_cs"], "7.41e-12")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "const_ch4_npp"], "9.99e-3")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "const_ch4_resps"], "4.36e-3")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "q10_ch4_cs"], "3.7")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "q10_ch4_npp"], "1.5")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "q10_ch4_resps"], "1.5")

        return config, self.reports


class vn50_t171(MacroUpgrade):

    """Upgrade macro from JULES by Sarah Chadburn"""

    BEFORE_TAG = "vn5.0_t483"
    AFTER_TAG = "vn5.0_t171"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_soil", "l_holdwater"], ".false.")

        return config, self.reports


class vn50_vn51(MacroUpgrade):

    """Version bump macro"""

    BEFORE_TAG = "vn5.0_t171"
    AFTER_TAG = "vn5.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
