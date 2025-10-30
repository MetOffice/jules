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

class vn58_t1020(MacroUpgrade):

    """Upgrade macro from JULES by J. M. Edwards"""

    BEFORE_TAG = "vn5.8"
    AFTER_TAG = "vn5.8_t1020"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings

        self.add_setting(config, ["namelist:jules_radiation", "l_hapke_soil"], ".false.")
        self.add_setting(config, ["namelist:jules_radiation", "l_partition_albsoil"], ".false.")
        self.add_setting(config, ["namelist:jules_radiation", "ratio_albsoil"], "2.0")
        self.add_setting(config, ["namelist:jules_radiation", "swdn_frac_albsoil"], "0.5")

        return config, self.reports

class vn58_t1018(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.8_t1020"
    AFTER_TAG = "vn5.8_t1018"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add a new file, namelist and settings for water resource modelling.

        self.add_setting(config, ["file:jules_water_resources.nml", "source"],
                                  "namelist:jules_water_resources")
        self.add_setting(config, ["namelist:jules_water_resources", "l_prioritise"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "l_water_domestic"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "l_water_environment"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "l_water_industry"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "l_water_irrigation"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "l_water_livestock"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "l_water_resources"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "l_water_transfers"], ".false.")
        self.add_setting(config, ["namelist:jules_water_resources", "nr_gwater_model"], "0")
        self.add_setting(config, ["namelist:jules_water_resources", "nstep_water_res"], "1")
        self.add_setting(config, ["namelist:jules_water_resources", "priority"], "'irr'")
        self.add_setting(config, ["namelist:jules_water_resources", "rf_domestic"], "0.1")
        self.add_setting(config, ["namelist:jules_water_resources", "rf_industry"], "0.1")
        self.add_setting(config, ["namelist:jules_water_resources", "rf_livestock"], "0.1")

        # Add the JULES_WATER_RESOURCES_PROPS namelist.
        # Add it to ancillaries.nml as an optional namelist.

        """ Create new namelist jules_water_resources_props. """

        self.add_setting(config, ["namelist:jules_water_resources_props"])
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                              source.replace("(namelist:jules_overbank_props)",
                                                             "(namelist:jules_overbank_props) (namelist:jules_water_resources_props)"))

        return config, self.reports

class vn58_t499(MacroUpgrade):

    """Upgrade macro for JULES ticket #499 by Helen Johnson"""

    BEFORE_TAG = "vn5.8_t1018"
    AFTER_TAG = "vn5.8_t499"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add JULES_FLAKE namelist
        self.add_setting(config, ["namelist:jules_flake"])

        # Add it to ancillaries.nml as an optional namelist
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                              source.replace("namelist:jules_co2",
                                                             "(namelist:jules_flake) namelist:jules_co2"))

        # Add variables to the namelist
        self.add_setting(config, ["namelist:jules_flake", "nvars"], "0")
        self.add_setting(config, ["namelist:jules_flake", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_flake", "const_val"], "5.0")
        self.add_setting(config, ["namelist:jules_flake", "file"], "''")
        self.add_setting(config, ["namelist:jules_flake", "use_file"], ".false.")
        self.add_setting(config, ["namelist:jules_flake", "var"], "'lake_depth'")
        self.add_setting(config, ["namelist:jules_flake", "var_name"], "''")
        self.add_setting(config, ["namelist:jules_flake", "tpl_name"], "''")

        return config, self.reports


class vn58_t838(MacroUpgrade):

    """Upgrade macro from JULES by Heather Rumbold"""

    BEFORE_TAG = "vn5.8_t499"
    AFTER_TAG = "vn5.8_t838"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Remove namelist from file ancillaries.nml
        # get value of source
        namelsts = self.get_setting_value(config, ["file:ancillaries.nml","source"],)
        # test that it exists
        if namelsts is not None:
            # get rid of namelist by replacing it with nothing
            namelsts=namelsts.replace("(namelist:jules_irrig)", "")
            # write it back to file creation
            self.change_setting_value(config, ["file:ancillaries.nml","source"], namelsts)

        #Adding a new namelist file (jules_irrig.nml)
        self.add_setting(config, ["file:jules_irrig.nml", "source"],
                                 "namelist:jules_irrig")

        # move a namelist item from vegetation namelist to irrigation
        self.rename_setting(config, ["namelist:jules_vegetation", "l_irrig_dmd"], ["namelist:jules_irrig", "l_irrig_dmd"])
        self.rename_setting(config, ["namelist:jules_vegetation", "l_irrig_limit"], ["namelist:jules_irrig", "l_irrig_limit"])
        self.rename_setting(config, ["namelist:jules_vegetation", "irr_crop"], ["namelist:jules_irrig", "irr_crop"])

        # Add the JULES_IRRIG_PROPS namelist
        self.add_setting(config, ["namelist:jules_irrig_props"])
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source is not None:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                      source.replace("(namelist:jules_crop_props) ", "(namelist:jules_crop_props) (namelist:jules_irrig_props) "))

        #Populate jules_irrig_props with variables moved from namelist:jules_irrig
        self.rename_setting(config, ["namelist:jules_irrig", "read_from_dump"], ["namelist:jules_irrig_props", "read_from_dump"])
        self.rename_setting(config, ["namelist:jules_irrig", "read_file"], ["namelist:jules_irrig_props", "read_file"])
        self.rename_setting(config, ["namelist:jules_irrig", "file"], ["namelist:jules_irrig_props", "irrig_frac_file"])
        self.rename_setting(config, ["namelist:jules_irrig", "var_name"], ["namelist:jules_irrig_props", "var_name"])
        self.rename_setting(config, ["namelist:jules_irrig", "const_frac_irr"], ["namelist:jules_irrig_props", "const_frac_irr"])
        self.rename_setting(config, ["namelist:jules_irrig", "const_irrfrac_irrtiles"], ["namelist:jules_irrig_props", "const_irrfrac_irrtiles"])

        return config, self.reports

class vn58_t1066(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.8_t838"
    AFTER_TAG = "vn5.8_t1066"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Blank macro to update triggering.
        return config, self.reports

class vn58_vn59(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.8_t1066"
    AFTER_TAG = "vn5.9"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
