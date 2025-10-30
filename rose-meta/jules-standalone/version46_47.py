import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn46_t326(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.6"
    AFTER_TAG = "vn4.6_t326"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_drive", "l_perturb_driving"], ".false.")
        return config, self.reports


class vn46_t324(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.6_t326"
    AFTER_TAG = "vn4.6_t324"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        ncpft_in = self.get_setting_value(config, ["namelist:jules_surface_types", "ncpft"])
        if ncpft_in:
            ncpft = int(ncpft_in)
            self.add_setting(config, ["namelist:jules_cropparm", "initial_c_dvi_io"], ','.join(['0.0'] * ncpft))

        else:
            self.add_setting(config, ["namelist:jules_cropparm", "initial_c_dvi_io"], "-1e20")

        # Add settings
        return config, self.reports


class vn46_t298(MacroUpgrade):

    """Upgrade macro from JULES by J. M. Edwards"""

    BEFORE_TAG = "vn4.6_t324"
    AFTER_TAG = "vn4.6_t298"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_snow", "i_grain_growth_opt"], "0")
        self.add_setting(config, ["namelist:jules_snow", "i_relayer_opt"], "0")
        return config, self.reports


class vn46_t288(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn4.6_t298"
    AFTER_TAG = "vn4.6_t288"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_surface", "l_layeredc"], ".false.")
        self.add_setting(config, ["namelist:jules_surface", "bio_hum_cn"], "10.0")
        self.add_setting(config, ["namelist:jules_surface", "tau_resp"], "2.0")
        self.add_setting(config, ["namelist:jules_surface", "tau_lit"], "5.0")
        self.add_setting(config, ["namelist:jules_surface", "diff_n_pft"], "100.0")
        return config, self.reports


class vn46_vn47(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.6_t288"
    AFTER_TAG = "vn4.7"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

