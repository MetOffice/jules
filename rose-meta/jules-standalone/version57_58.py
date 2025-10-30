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

class vn57_t1021(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.7"
    AFTER_TAG = "vn5.7_t1021"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings

        # Add dump period unit default
        self.add_setting(
            config, ["namelist:jules_output", "dump_period_unit"], "'Y'")

        return config, self.reports


class vn57_t1049(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.7_t1021"
    AFTER_TAG = "vn5.7_t1049"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Remove a setting.
        self.remove_setting(config,["namelist:jules_rivers", "slfac"])

        return config, self.reports


class vn57_vn58(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.7_t1049"
    AFTER_TAG = "vn5.8"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
