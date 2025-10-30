import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn75_t1514(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn7.5"
    AFTER_TAG = "vn7.5_t1514"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "z_burn_max"], "0.2")
        return config, self.reports


class vn75_vn76(MacroUpgrade):
    """Version bump macro"""
    
    BEFORE_TAG = "vn7.5_t1514"
    AFTER_TAG = "vn7.6"
    
    def upgrade(self, config, meta_config=None):
        # Nothing to do        
        return config, self.reports
