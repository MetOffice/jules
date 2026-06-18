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
from .version79_80 import *
from .version80_81 import *


class vn81_t70(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn8.1"
    AFTER_TAG = "vn8.1_t70"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        """Add cs_decomp_soil_moist_func to namelist jules_soil_biogeochem"""
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "cs_decomp_soil_moist_func"], "0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "fsthsat_cs_decomp_opt1"], "0.2")


        return config, self.reports


class vn81_t41(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn8.1_t70"
    AFTER_TAG = "vn8.1_t41"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            npft = int(
                self.get_setting_value(
                    config, ["namelist:jules_surface_types", "npft"]
                )
            )
            self.add_setting(
                config,
                ["namelist:jules_pftparm", "pft_name_io"],
                ",".join(["''"] * npft),
            )

        return config, self.reports


class vn81_t59(MacroUpgrade):

    """Upgrade macro from JULES by Carolina Duran Rojas"""

    BEFORE_TAG = "vn8.1_t41"
    AFTER_TAG = "vn8.1_t59"

    def upgrade(self,config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Adding logical and real to the jules biogeochemical namelist
        self.add_setting(config,
                ["namelist:jules_soil_biogeochem", "l_bgc_heat"], ".false.")
        self.add_setting(config,
                ["namelist:jules_soil_biogeochem", "heat_of_respiration"], "3.9e07")

        return config, self.reports


class vn81_t23(MacroUpgrade):

    """ Upgrade macro from JULES by Heather Rumbold """

    BEFORE_TAG = "vn8.1_t59"
    AFTER_TAG = "vn8.1_t23"

    def upgrade(self,config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        """
        1. Add switches to indicate whether a JULES surface tile is irrigated (pft)
        2. Add the surface types, c3_irrig and c4_irrig with a value of zero
        """
        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:

            # Add the pft size
            npft = int(
                self.get_setting_value(config, ["namelist:jules_surface_types", "npft"])
            )

            # Add new setting
            self.add_setting(
                config,
                ["namelist:jules_pftparm", "irrig_pft_io"],
                ",".join(["0"] * npft),
                False,
            )

            self.add_setting(config, ["namelist:jules_surface_types", "c3_irrig"], "0")
            self.add_setting(config, ["namelist:jules_surface_types", "c4_irrig"], "0")
            self.add_setting(config, ["namelist:jules_irrig", "irrig_option"], "0")

        return config, self.reports
