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


class vn61_t1184(MacroUpgrade):

    """Upgrade macro from JULES by Ian Boutle"""

    BEFORE_TAG = "vn6.1"
    AFTER_TAG = "vn6.1_t1184"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        fix_ctile_orog=self.get_setting_value(config, ["namelist:jules_temp_fixes","l_fix_ctile_orog"])
        if fix_ctile_orog == ".true.":
            self.add_setting(config, ["namelist:jules_temp_fixes","ctile_orog_fix"],"2")
        else:
            self.add_setting(config, ["namelist:jules_temp_fixes","ctile_orog_fix"],"0")
        self.remove_setting(config, ["namelist:jules_temp_fixes","l_fix_ctile_orog"])

        return config, self.reports


class vn61_t1131(MacroUpgrade):
    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn6.1_t1184"
    AFTER_TAG = "vn6.1_t1131"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Remove the cable_progs namelist from the ancillaries namelist
        namelsts = self.get_setting_value(config,
                                          ["file:ancillaries.nml", "source"], )
        if (namelsts):
            namelsts = namelsts.replace("(namelist:cable_progs)", "")
            self.change_setting_value(config,
                                      ["file:ancillaries.nml", "source"],
                                      namelsts)

        # Add the cable_progs namelist as an accessable file from .conf
        self.add_setting(config, ["file:cable_prognostics.nml", "source"],
                         "(namelist:cable_progs)")

        # Add settings
        self.add_setting(config, ["namelist:cable_progs", "const_val"],
                         "280.0, 0.3, 0.0, 0.0, 0.0, 0.0, 273.16, 0.0, 0.0, 0.0")

        return config, self.reports


class vn61_t1161(MacroUpgrade):

    """Upgrade macro from JULES by Dan Copsey"""

    BEFORE_TAG = "vn6.1_t1131"
    AFTER_TAG = "vn6.1_t1161"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_lake_ice_temperatures"], ".false.")
        return config, self.reports


class vn61_t470(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn6.1_t1161"
    AFTER_TAG = "vn6.1_t470"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add switch to label a fraction of soil carbon
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "l_layeredc"], ".false.")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "l_label_frac_cs"], ".false.")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "tau_lit"], "5.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "tau_resp"], "2.0")
        return config, self.reports


class vn61_vn62(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.1_t470"
    AFTER_TAG = "vn6.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports