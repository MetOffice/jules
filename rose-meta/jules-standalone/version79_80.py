import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn79_t1460(MacroUpgrade):
    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.9"
    AFTER_TAG = "vn7.9_t1460"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        # Remove redundant switch as River routing ancillaries should now be
        # consistent.
        self.remove_setting(
            config,
            ["namelist:jules_rivers_props", "l_ignore_ancil_rivers_check"],
        )

        return config, self.reports


class vn79_t1392(MacroUpgrade):
    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.9_t1460"
    AFTER_TAG = "vn7.9_t1392"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        # Add is_climatology flag to jules_rivers_props
        nvars = int(
            self.get_setting_value(
                config, ["namelist:jules_rivers_props", "nvars"]
            )
        )
        self.add_setting(
            config,
            ["namelist:jules_rivers_props", "is_climatology"],
            ",".join([".false."] * nvars),
        )
        return config, self.reports


class vn79_t1371(MacroUpgrade):

    """ Upgrade macro from JULES by Katty Huang """

    BEFORE_TAG = "vn7.9_t1392"
    AFTER_TAG = "vn7.9_t1371"

    def upgrade(self,config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_surface","anthrop_heat_option"], "0")
        self.add_setting(config, ["namelist:jules_surface","anthrop_heat_mean"], "20.0")

        return config, self.reports


class vn79_t1088(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.9_t1371"
    AFTER_TAG = "vn7.9_t1088"

    def upgrade(self,config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_water_resources", "partition_method"], "2")
        self.add_setting(config, ["namelist:jules_water_resources", "sfc_water_factor"], "2.0")
        return config, self.reports


class vn79_vn80(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.9_t1088"
    AFTER_TAG = "vn8.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
