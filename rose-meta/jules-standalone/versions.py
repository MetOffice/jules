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
from .version81_82 import *


class vn82_t1(MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn8.2"
    AFTER_TAG = "vn8.2_t138"

    """Issue number 138"""

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        """Add l_calc_zw2 to namelist jules_hydrology"""
        self.add_setting(config, ["namelist:jules_hydrology", "l_calc_zw2"], ".false.")
        """Add l_darcy_lsh_all to namelist jules_hydrology"""
        self.add_setting(config, ["namelist:jules_hydrology", "l_darcy_lsh_all"], ".false.")
        return config, self.reports
