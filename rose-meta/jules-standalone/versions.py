import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

from .version34_40 import *
from .version40_41 import *
from .version41_42 import *
from .version42_43 import *
from .version43_44 import *
from .version44_45 import *
from .version45_46 import *
from .version46_47 import *
from .version47_48 import *
from .version48_49 import *
from .version49_50 import *
from .version50_51 import *
from .version51_52 import *
from .version52_53 import *
from .version53_54 import *
from .version54_55 import *
from .version55_56 import *
from .version56_57 import *
from .version57_58 import *
from .version58_59 import *
from .version59_60 import *
from .version60_61 import *
from .version61_62 import *
from .version62_63 import *
from .version63_70 import *
from .version70_71 import *
from .version71_72 import *
from .version72_73 import *
from .version73_74 import *
from .version74_75 import *
from .version75_76 import *
from .version76_77 import *
from .version77_78 import *
from .version78_79 import *


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
    """Upgrade macro from JULES by Katty Huang"""

    BEFORE_TAG = "vn7.9_t1392"
    AFTER_TAG = "vn7.9_t1371"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(
            config, ["namelist:jules_surface", "anthrop_heat_option"], "0"
        )
        self.add_setting(
            config, ["namelist:jules_surface", "anthrop_heat_mean"], "20.0"
        )

        return config, self.reports


class vn79_t1088(MacroUpgrade):
    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.9_t1371"
    AFTER_TAG = "vn7.9_t1088"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(
            config, ["namelist:jules_water_resources", "partition_method"], "2"
        )
        self.add_setting(
            config,
            ["namelist:jules_water_resources", "sfc_water_factor"],
            "2.0",
        )
        return config, self.reports


class vn79_t1591(MacroUpgrade):
    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.9_t1088"
    AFTER_TAG = "vn7.9_t1591"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        # compulsory changed to true
        self.add_setting(config, ["namelist:jules_surface", "beta1"], "0.83")
        self.add_setting(config, ["namelist:jules_surface", "beta2"], "0.93")
        self.add_setting(config, ["namelist:jules_surface", "fwe_c3"], "0.5")
        self.add_setting(
            config, ["namelist:jules_surface", "fwe_c4"], "20000.0"
        )
        self.add_setting(config, ["namelist:jules_surface", "hleaf"], "5.7e4")
        self.add_setting(config, ["namelist:jules_surface", "hwood"], "1.1e4")
        return config, self.reports
