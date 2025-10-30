import re
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


class vn56_t863(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.6"
    AFTER_TAG = "vn5.6_t863"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings

        # Add new switch and related parameters for photosynthesis model.
        self.add_setting(config, ["namelist:jules_vegetation", "photo_acclim_model"], "0")
        self.add_setting(config, ["namelist:jules_vegetation", "photo_jv_model"], "1")
        self.add_setting(config, ["namelist:jules_vegetation", "dsj_slope"], "-0.75")
        self.add_setting(config, ["namelist:jules_vegetation", "dsj_zero"], "659.7")
        self.add_setting(config, ["namelist:jules_vegetation", "dsv_slope"], "-1.07")
        self.add_setting(config, ["namelist:jules_vegetation", "dsv_zero"], "668.39")
        self.add_setting(config, ["namelist:jules_vegetation", "jv25_slope"], "-0.035")
        self.add_setting(config, ["namelist:jules_vegetation", "jv25_zero"], "2.59")
        self.add_setting(config, ["namelist:jules_vegetation", "n_alloc_jmax"], "5.3")
        self.add_setting(config, ["namelist:jules_vegetation", "n_alloc_vcmax"], "3.8")
        self.add_setting(config, ["namelist:jules_vegetation", "n_day_photo_acclim"], "30.0")

        return config, self.reports


class vn56_t987(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.6_t863"
    AFTER_TAG = "vn5.6_t987"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # No changes, but triggering altered by metadata changes.
        return config, self.reports


class vn56_t821(MacroUpgrade):

    """Upgrade macro from JULES by Camilla Mathison"""

    BEFORE_TAG = "vn5.6_t987"
    AFTER_TAG = "vn5.6_t821"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_irrig",
                                  "set_irrfrac_on_irrtiles"], ".false.")
        self.add_setting(config, ["namelist:jules_irrig",
                                  "const_irrfrac_irrtiles"], "0")
        self.add_setting(config, ["namelist:jules_vegetation",
                                  "l_croprotate"], ".false.")
        return config, self.reports


class vn56_t940(MacroUpgrade):

    """Upgrade macro from JULES by Danny Eisenberg"""

    BEFORE_TAG = "vn5.6_t821"
    AFTER_TAG = "vn5.6_t940"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add cable_progs namelist to ancillaries.nml file
        contents = self.get_setting_value(config, ["file:ancillaries.nml", "source"])
        contents = re.sub(r'namelist:jules_co2', r'(namelist:cable_progs) namelist:jules_co2', contents)
        self.change_setting_value(config, ["file:ancillaries.nml", "source"], contents)

        # Add contents of cable_progs namelist
        self.add_setting(config, ["namelist:cable_progs", "const_val"], "10*0")
        self.add_setting(config, ["namelist:cable_progs", "file"], "'cable_vars.nc'")
        self.add_setting(config, ["namelist:cable_progs", "nvars"], "10")
        self.add_setting(config, ["namelist:cable_progs", "use_file"], "10*.true.")
        self.add_setting(config, ["namelist:cable_progs", "var"], "'SoilTemp_CABLE','SoilMoisture_CABLE','FrozenSoilFrac_CABLE','SnowDepth_CABLE','SnowMass_CABLE','SnowDensity_CABLE','SnowTemp_CABLE','SnowAge_CABLE','OneLyrSnowDensity_CABLE','ThreeLayerSnowFlag_CABLE'")
        self.add_setting(config, ["namelist:cable_progs", "var_name"], "'SoilTemp_CABLE','SoilMoisture_CABLE','FrozenSoilFrac_CABLE','SnowDepth_CABLE','SnowMass_CABLE','SnowDensity_CABLE','SnowTemp_CABLE','SnowAge_CABLE','OneLyrSnowDensity_CABLE','ThreeLayerSnowFlag_CABLE'")

        ## Add cable_surface_types namelist
        self.add_setting(config, ["file:cable_surface_types.nml", "source"], "(namelist:cable_surface_types)")
        self.add_setting(config, ["namelist:cable_surface_types", "barren_cable"], "14")
        self.add_setting(config, ["namelist:cable_surface_types", "ice_cable"], "17")
        self.add_setting(config, ["namelist:cable_surface_types", "lakes_cable"], "16")
        self.add_setting(config, ["namelist:cable_surface_types", "nnvg_cable"], "4")
        self.add_setting(config, ["namelist:cable_surface_types", "npft_cable"], "13")
        self.add_setting(config, ["namelist:cable_surface_types", "urban_cable"], "15")

        return config, self.reports


class vn56_t548(MacroUpgrade):

    """Upgrade macro from JULES by Richard Gilham"""

    BEFORE_TAG = "vn5.6_t940"
    AFTER_TAG = "vn5.6_t548"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.add_setting(config, ["namelist:jules_soil", "l_tile_soil"], ".false.")
        self.add_setting(config, ["namelist:jules_soil", "l_broadcast_ancils"], ".false.")
        self.add_setting(config, ["namelist:jules_initial", "l_broadcast_soilt"], ".false.")

        # Add settings
        return config, self.reports

class vn56_vn57(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.6_t548"
    AFTER_TAG = "vn5.7"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
