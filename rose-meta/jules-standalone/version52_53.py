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


class vn52_t791(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.2"
    AFTER_TAG = "vn5.2_t791"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Prevent the use of deleted options.
        can_rad_mod = int(self.get_setting_value(config,
                                             ["namelist:jules_vegetation","can_rad_mod"]))
        if can_rad_mod == 2 or can_rad_mod == 3:
            raise UpgradeError("The options can_rad_mod=2 and can_rad_mod=3 have been deleted. Please update your app to use an appropriate setting before upgrading.")

        return config, self.reports

class vn52_t742(MacroUpgrade):

    """Upgrade macro from JULES by Adrian Lock"""

    BEFORE_TAG = "vn5.2_t791"
    AFTER_TAG = "vn5.2_t742"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_surface", "beta_cnv_bl"], "0.08")
        return config, self.reports

class vn52_t781(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.2_t742"
    AFTER_TAG = "vn5.2_t781"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        # Remove a setting.
        self.remove_setting(config, ["namelist:jules_soil_ecosse", "denit_ratio_n2_zero"])
        return config, self.reports


class vn52_t633(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.2_t781"
    AFTER_TAG = "vn5.2_t633"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add new file, namelist & switch, which in JULES standalone should
        # always be false.
        self.add_setting(config, ["file:model_environment.nml", "source"],
                                  "namelist:jules_model_environment")
        self.add_setting(config, ["namelist:jules_model_environment",
                                  "l_jules_parent"],
                         "0")
        # formdrag should be zero in standalone until the appropriate plumbing
        # of ancillaries is undertaken. The other orographic drag parameters
        # have been added with the model default and are trigger ignored.
        self.add_setting(config, ["namelist:jules_surface", "formdrag"],
                                  "0")
        self.add_setting(config, ["namelist:jules_surface", "fd_stab_dep"],
                                  "0")
        self.add_setting(config, ["namelist:jules_surface", "orog_drag_param"],
                                  "0.3")
        # i_modiscopt and isrfexcnvgust should be zero in standalone and is
        # trigger ignored.  Default in code is zero.
        self.add_setting(config, ["namelist:jules_surface", "i_modiscopt"],
                                  "0")
        self.add_setting(config, ["namelist:jules_surface", "isrfexcnvgust"],
                                  "0")
        # The following are currently not available to standalone #499, #179.
        self.add_setting(config, ["namelist:jules_surface", "l_flake_model"],
                                  ".false.")
        self.add_setting(config, ["namelist:jules_surface", "l_vary_z0m_soil"],
                                  ".false.")
        return config, self.reports


class vn52_vn53(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.2_t633"
    AFTER_TAG = "vn5.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


'''
Do not edit any lines below this point - this is the template.
'''


class vn52_txxx(MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn5.2"
    AFTER_TAG = "vn5.2_txxx"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports

