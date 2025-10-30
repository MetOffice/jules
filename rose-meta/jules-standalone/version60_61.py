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


class vn60_t1119(MacroUpgrade):

    """Upgrade macro from JULES by C. Carouge"""

    BEFORE_TAG = "vn6.0"
    AFTER_TAG = "vn6.0_t1119"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:cable_pftparm", "lai_io"], "4.0,5.0,0.0,0.0,0.0,0.2,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0")

        return config, self.reports


class vn60_t949(MacroUpgrade):

    """Upgrade macro for JULES ticket #949 by Heather Rumbold"""

    BEFORE_TAG = "vn6.0_t1119"
    AFTER_TAG = "vn6.0_t949"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        # No macro commands added as only triggering changes were made.
        return config, self.reports

class vn60_t1146(MacroUpgrade):

    """Upgrade macro for JULES ticket #1146 by Heather Rumbold"""

    BEFORE_TAG = "vn6.0_t949"
    AFTER_TAG = "vn6.0_t1146"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        # Pull out timestep length
        timestep_len = int(self.get_setting_value(config, ["namelist:jules_time", "timestep_len"]))
        # Calculate a default nstep_irrig based on timestep_len and a daily update frequency
        # i.e. 86400 seconds, and add to the jules_irrig namelist:
        self.add_setting(config, ["namelist:jules_irrig", "nstep_irrig"], str(86400/timestep_len))

        return config, self.reports

class vn60_t1126(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn6.0_t1146"
    AFTER_TAG = "vn6.0_t1126"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.add_setting(config, ["namelist:imogen_run_list", "initial_co2_ch4_year"], "1860")
        self.add_setting(config, ["namelist:imogen_run_list", "co2_init_ppmv"], "286.085")
        self.add_setting(config, ["namelist:imogen_anlg_vals_list", "f_ocean"], "0.711")
        self.add_setting(config, ["namelist:imogen_anlg_vals_list", "kappa_o"], "383.3")
        self.add_setting(config, ["namelist:imogen_anlg_vals_list", "lambda_o"], "1.75")
        self.add_setting(config, ["namelist:imogen_anlg_vals_list", "lambda_l"], "0.52")
        self.add_setting(config, ["namelist:imogen_anlg_vals_list", "mu"], "1.87")
        self.add_setting(config, ["namelist:imogen_anlg_vals_list", "q2co2"], "3.74")
        self.add_setting(config, ["namelist:imogen_anlg_vals_list", "t_ocean_init"], "289.28")
        self.rename_setting(config, ["namelist:imogen_anlg_vals_list", "file_non_co2"], ["namelist:imogen_run_list", "file_non_co2"])
        self.rename_setting(config, ["namelist:imogen_anlg_vals_list", "nyr_non_co2"], ["namelist:imogen_run_list", "nyr_non_co2"])

        # Add settings
        return config, self.reports


class vn60_t1141(MacroUpgrade):

    """Upgrade macro from JULES by Phil Harris"""

    BEFORE_TAG = "vn6.0_t1126"
    AFTER_TAG = "vn6.0_t1141"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add the vegetation properties ancillary namelist.  This is currently
        # only used for growth temperature for thermal adaptation, which was
        # added at vn5.7 without an upgrade macro.
        self.add_setting(config, ["namelist:jules_vegetation_props"])

        self.add_setting(config, ["namelist:jules_vegetation_props", "nvars"], "1")
        self.add_setting(config, ["namelist:jules_vegetation_props", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_vegetation_props", "const_val"], "15.0")
        self.add_setting(config, ["namelist:jules_vegetation_props", "use_file"], ".false.")
        self.add_setting(config, ["namelist:jules_vegetation_props", "file"], "''")
        self.add_setting(config, ["namelist:jules_vegetation_props", "var"], "'t_growth_gb'")
        self.add_setting(config, ["namelist:jules_vegetation_props", "var_name"], "'t_growth_gb'")
        self.add_setting(config, ["namelist:jules_vegetation_props", "tpl_name"], "''")

        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                              source.replace("namelist:jules_frac",
                                                             "namelist:jules_frac (namelist:jules_vegetation_props)"))
        return config, self.reports


class vn60_t1143(MacroUpgrade):

    """Upgrade macro from JULES by tobymarthews"""

    BEFORE_TAG = "vn6.0_t1141"
    AFTER_TAG = "vn6.0_t1143"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Blank macro due to triggering adjustments in metadata
        return config, self.reports


class vn60_vn61(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.0_t1143"
    AFTER_TAG = "vn6.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
