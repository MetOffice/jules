import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

from . import version34_40
from . import version40_41
from . import version41_42


class vn42_t103(MacroUpgrade):

    """Upgrade macro for JULES ticket #103 by Matt Pryor"""

    BEFORE_TAG = "vn4.2"
    AFTER_TAG = "vn4.2_t103"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just remove the setting for NPROC
        self.remove_setting(config, ["env", "NPROC"])

        return config, self.reports


class vn42_t75(MacroUpgrade):

    """Upgrade macro for JULES ticket #75 by Nic Gedney"""

    BEFORE_TAG = "vn4.2_t103"
    AFTER_TAG = "vn4.2_t75"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_hydrology", "l_wetland_unfrozen"], ".false.")

        return config, self.reports


class vn42_t50(MacroUpgrade):

    """Upgrade macro for ticket #50 by johnmedwards."""

    BEFORE_TAG = "vn4.2_t75"
    AFTER_TAG = "vn4.2_t50"

    def upgrade(self, config, meta_config=None):

        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))

        self.add_setting(config, ["namelist:jules_radiation", "l_embedded_snow"], ".false.")
        self.add_setting(config, ["namelist:jules_radiation", "l_mask_snow_orog"], ".false.")
        self.add_setting(config, ["namelist:jules_radiation", "wght_alb"], "0.0,0.5,0.0,0.5")

        self.add_setting(config, ["namelist:jules_snow", "l_et_metamorph"], ".false.")
        self.add_setting(config, ["namelist:jules_snow", "l_snow_nocan_hc"], ".false.")
        self.add_setting(config, ["namelist:jules_snow", "l_snow_infilt"], ".false.")
        self.add_setting(config, ["namelist:jules_snow", "i_snow_cond_parm"], "0")
        self.add_setting(config, ["namelist:jules_snow", "a_snow_et"], "0.0")
        self.add_setting(config, ["namelist:jules_snow", "b_snow_et"], "0.0")
        self.add_setting(config, ["namelist:jules_snow", "c_snow_et"], "0.0")
        self.add_setting(config, ["namelist:jules_snow", "rho_snow_et_crit"], "0.0")
        self.add_setting(config, ["namelist:jules_snow", "unload_rate_cnst"], ','.join(['0.0']*npft))
        self.add_setting(config, ["namelist:jules_snow", "unload_rate_u"], ','.join(['0.0']*npft))
        self.add_setting(config, ["namelist:jules_snow", "can_clump"], ','.join(['0.0']*npft))
        self.add_setting(config, ["namelist:jules_snow", "lai_alb_lim_sn"], ','.join(['0.5']*npft))
        self.add_setting(config, ["namelist:jules_snow", "n_lai_exposed"], ','.join(['0.0']*npft))

        self.add_setting(config, ["namelist:jules_pftparm", "lai_alb_lim_io"], ','.join(['0.5']*npft))

        self.add_setting(config, ["namelist:jules_vegetation", "l_vegcan_soilfx"], ".false.")

        return config, self.reports


class vn42_t116(MacroUpgrade):

    """Upgrade macro for JULES ticket #116 by Huw Lewis"""

    BEFORE_TAG = "vn4.2_t50"
    AFTER_TAG = "vn4.2_t116"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_rivers_props", "rivers_dx"], "0.0")

        return config, self.reports


class vn42_t78(MacroUpgrade):

    """Upgrade macro for JULES ticket #78 by Richard Gilham"""

    BEFORE_TAG = "vn4.2_t116"
    AFTER_TAG  = "vn4.2_t78"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # add the new compulsory namelist items with its default value
        self.add_setting(config, ["namelist:jules_frac", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_soil_props", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_top", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_agric", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_crop_props", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_irrig", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_co2", "read_from_dump"], ".false.")

        return config, self.reports


class vn42_vn43(MacroUpgrade):

    """Version bump macro"""

    BEFORE_TAG = "vn4.2_t78"
    AFTER_TAG  = "vn4.3"

    def upgrade(self, config, meta_config=None):
        return config, self.reports
