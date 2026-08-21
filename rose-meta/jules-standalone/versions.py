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



class vn82_t61(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn8.2"
    AFTER_TAG = "vn8.2_t61"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.rename_setting(config, ["namelist:fire_switches"],
                            ["namelist:jules_fire_weather_index"])
        self.rename_setting(config, ["namelist:jules_fire_weather_index",
                                      "l_fire"],
                            ["namelist:jules_fire_weather_index",
                             "l_fire_weather_index"])

        self.add_setting(config, ["namelist:jules_inferno"])

        source = self.get_setting_value(config, ["file:fire.nml","source"])
        source = source.replace("namelist:fire_switches",
                                "namelist:jules_fire_weather_index namelist:jules_inferno")
        self.change_setting_value(config, ["file:fire.nml","source"], source)

        l_inferno = self.get_setting_value(config, ["namelist:jules_vegetation", "l_inferno"])
        self.add_setting(config,
                ["namelist:jules_inferno", "l_inferno"], l_inferno)
        self.remove_setting(config, ["namelist:jules_vegetation", "l_inferno"])

        l_trif_fire = self.get_setting_value(config, ["namelist:jules_vegetation", "l_trif_fire"])
        self.add_setting(config,
                ["namelist:jules_inferno", "l_trif_fire"], l_trif_fire)
        self.remove_setting(config, ["namelist:jules_vegetation", "l_trif_fire"])

        ignition_method = self.get_setting_value(config, ["namelist:jules_vegetation", "ignition_method"])
        self.add_setting(config,
                ["namelist:jules_inferno", "ignition_method"], ignition_method)
        self.remove_setting(config, ["namelist:jules_vegetation", "ignition_method"])
        
        z_burn_max = self.get_setting_value(config, ["namelist:jules_soil_biogeochem", "z_burn_max"])
        self.add_setting(config,
                ["namelist:jules_inferno", "z_burn_max"], z_burn_max)
        self.remove_setting(config, ["namelist:jules_soil_biogeochem", "z_burn_max"])
        
        self.add_setting(config, ["namelist:jules_inferno", "triffire_ccdpm_min"], 0.8)
        self.add_setting(config, ["namelist:jules_inferno", "triffire_ccdpm_max"], 1.0)
        self.add_setting(config, ["namelist:jules_inferno", "triffire_ccrpm_min"], 0.0)
        self.add_setting(config, ["namelist:jules_inferno", "triffire_ccrpm_max"], 0.2)
        
        self.add_setting(config, ["namelist:jules_inferno", "flam_rhum_low"], 0.1)
        self.add_setting(config, ["namelist:jules_inferno", "flam_rhum_up"], 0.9)
        self.add_setting(config, ["namelist:jules_inferno", "flam_sm_low"], 0.0)
        self.add_setting(config, ["namelist:jules_inferno", "flam_sm_up"], 2.4)
        self.add_setting(config, ["namelist:jules_inferno", "flam_fuel_low"], 0.02)
        self.add_setting(config, ["namelist:jules_inferno", "flam_fuel_up"], 0.2)
        self.add_setting(config, ["namelist:jules_inferno", "flam_rain_const"], -14929920000.0)
        self.add_setting(config, ["namelist:jules_inferno", "flam_sm_func"], 1)
        
        return config, self.reports
