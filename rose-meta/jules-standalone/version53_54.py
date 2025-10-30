import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class UpgradeError(Exception):

    """Exception created when an upgrade fails."""

    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        sys.tracebacklimit = 0
        return self.msg

    __str__ = __repr__


class vn53_t859(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.3"
    AFTER_TAG = "vn5.3_t859"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Do nothing. This macro will just bump the metadata version.
        return config, self.reports


class vn53_t872(MacroUpgrade):

    """Upgrade macro from JULES by Chantelle Burton"""

    BEFORE_TAG = "vn5.3_t859"
    AFTER_TAG = "vn5.3_t872"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings for fire mortality
        self.remove_setting(config, ["namelist:jules_pftparm", "fire_mort_io"])
        self.add_setting(config, ["namelist:jules_pftparm", "fire_mort_io"],"5*1.0")

        # Change if there are 9 PFTs
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        ncpft_in = self.get_setting_value(config, ["namelist:jules_surface_types", "ncpft"])
        if ncpft_in:
            ncpft = int(ncpft_in)
        else:
            ncpft = 0

        if npft == 9:
           self.change_setting_value(config, ["namelist:jules_pftparm", "fire_mort_io"],"9*1.0")

        # Change if there are 13 PFTs
        if npft == 13:
           self.change_setting_value(config, ["namelist:jules_pftparm", "fire_mort_io"],"13*1.0")

        return config, self.reports

class vn53_t610(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.3_t872"
    AFTER_TAG = "vn5.3_t610"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add new file, namelist & switch.
        self.add_setting(config, ["file:science_fixes.nml", "source"],
                                  "namelist:jules_temp_fixes")
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_moruses_roof_rad_coupling"],
                                  ".false.")

        # Add orginal JULES module defaults to namelist
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_dtcanfix"],".true.")
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_ctile_orog"],".true.")
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_ustar_dust"],".true.")
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_alb_ice_thick"], ".true.")
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_albsnow_ts"],".true.")
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_wind_snow"],".true.")

        return config, self.reports

class vn53_t766(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.3_t610"
    AFTER_TAG = "vn5.3_t766"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add new switch and related parameter.
        self.add_setting(config, ["namelist:jules_vegetation", "stomata_model"], "1")
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_pftparm", "g1_stomata_io"],','.join(['2.0']*npft))
        return config, self.reports

class vn53_t874(MacroUpgrade):

    """Upgrade macro for ticket #874 by Malcolm Brooks."""

    BEFORE_TAG = "vn5.3_t766"
    AFTER_TAG = "vn5.3_t874"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                 "l_fix_osa_chloro"],
                         ".false.")
        # Add settings
        return config, self.reports

class vn53_t867(MacroUpgrade):

    """Upgrade macro from JULES by Juan Castillo"""

    BEFORE_TAG = "vn5.3_t874"
    AFTER_TAG = "vn5.3_t867"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Rename namelist variables in jules_rivers
        self.rename_setting(config, ["namelist:jules_rivers", "l_riv_overbank"],
                                    ["namelist:jules_overbank", "l_riv_overbank"])
        self.rename_setting(config, ["namelist:jules_rivers", "rivers_timestep"],
                                    ["namelist:jules_rivers", "nstep_rivers"])

        # Add new namelist variables in jules_rivers
        self.add_setting(config, ["namelist:jules_rivers", "l_inland"], ".false.")
        self.add_setting(config, ["namelist:jules_rivers", "slfac"], "0.0")

        # Change from rivers_type (char) to i_river_vn (int)
        c_rivers_type = self.get_setting_value(config,
                                ["namelist:jules_rivers", "rivers_type"])

        if c_rivers_type == "'rfm'" :
            self.add_setting(config, ["namelist:jules_rivers", "i_river_vn"], "2")
        elif c_rivers_type == "'trip'" :
            self.add_setting(config, ["namelist:jules_rivers", "i_river_vn"], "3")
        else :
            self.add_setting(config, ["namelist:jules_rivers", "i_river_vn"], "2")

        self.remove_setting(config,["namelist:jules_rivers", "rivers_type"])

        return config, self.reports

class vn53_vn54(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.3_t867"
    AFTER_TAG = "vn5.4"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
