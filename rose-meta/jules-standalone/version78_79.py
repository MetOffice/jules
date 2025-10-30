import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn78_t1425(MacroUpgrade):
    """Upgrade macro from JULES by Carolina Duran Rojas"""

    BEFORE_TAG = "vn7.8"
    AFTER_TAG = "vn7.8_t1425"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        """
           Add a switch to use Robust Ecosystem Demography (RED)
           into the jules_vegetation namelist.
        """

        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            # Switch to use RED
            self.add_setting(config, ["namelist:jules_vegetation", "l_red"], ".false.")

            # Add jules_red namelist
            self.add_setting(
                config, ["file:red_params.nml", "source"], "(namelist:jules_red)"
            )

            # Add the size
            npft = int(
                self.get_setting_value(config, ["namelist:jules_surface_types", "npft"])
            )

            """
            Add variables to the jules_red namelist.
            """
            # Parameters of length npft.
            self.add_setting(
                config,
                ["namelist:jules_red", "alpha_recrt"],
                ",".join(["0.0"] * npft),
                False,
            )
            self.add_setting(
                config,
                ["namelist:jules_red", "crwn_area0"],
                ",".join(["0.0"] * npft),
                False,
            )
            self.add_setting(
                config, ["namelist:jules_red", "dom_order"], ",".join(["0"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "height0"], ",".join(["0.0"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "lai_bal0"], ",".join(["0.0"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "mass0"], ",".join(["0.0"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "massi"], ",".join(["0.0"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "mclass"], ",".join(["0"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "mort_base"], ",".join(["0.0"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "phi_a"], ",".join(["0.50"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "phi_g"], ",".join(["0.75"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "phi_h"], ",".join(["0.25"] * npft), False
            )
            self.add_setting(
                config, ["namelist:jules_red", "phi_l"], ",".join(["0.25"] * npft), False
            )

        return config, self.reports


class vn78_t1588(MacroUpgrade):
    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.8_t1425"
    AFTER_TAG = "vn7.8_t1588"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            self.add_setting(config, ["namelist:jules_hydrology", "l_hydrology"], ".true.")
            self.add_setting(
                config, ["namelist:jules_hydrology", "l_var_rainfrac"], ".false."
            )
        return config, self.reports


class vn78_t1579(MacroUpgrade):
    """Upgrade macro from JULES by Amy Peace"""

    BEFORE_TAG = "vn7.8_t1588"
    AFTER_TAG = "vn7.8_t1579"

    def upgrade(self, config, meta_config=None):
        """Add fef_<species>_io to namelist jules_pftparm."""

        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            # Get pft number
            npft = int(
                self.get_setting_value(config, ["namelist:jules_surface_types", "npft"])
            )

            RMDI = str(-(2**30))

            # Add emission scaling factor values for 13 PFTs
            if npft == 13:
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c2h4_io"],
                    "1.11E+00,1.11E+00,1.11E+01,1.54E+00,1.54E+00,8.30E-01,1.00E+00,1.00E+00,1.99E+00,2.40E+00,2.40E+00,8.30E-01,8.30E-01",
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c2h6_io"],
                    "8.80E-01,8.80E-01,6.90E-01,9.70E-01,9.70E-01,4.20E-01,7.90E-01,7.90E-01,1.01E+00,1.90E+00,1.90E+00,4.20E-01,4.20E-01",
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c3h8_io"],
                    "5.30E-01,5.30E-01,2.80E-01,2.90E-01,2.90E-01,1.30E-01,1.70E-01,1.70E-01,3.12E-01,4.08E-01,4.08E-01,1.30E-01,1.30E-01",
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_hcho_io"],
                    "2.40E+00,2.40E+00,2.04E+00,1.75E+00,1.75E+00,1.23E+00,1.90E+00,1.80E+00,2.95E+00,4.32E+00,4.32E+00,1.23E+00,1.23E+00",
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_mecho_io"],
                    "2.26E+00,2.26E+00,1.21E+00,8.10E-01,8.10E-01,8.40E-01,1.80E+00,1.80E+00,2.02E+00,4.32E+00,4.32E+00,8.40E-01,8.40E-01",
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_nh3_io"],
                    "1.33E+00,1.33E+00,9.80E-01,2.50E+00,2.50E+00,8.90E-01,9.90E-01,9.90E-01,2.14E+00,2.38E+00,2.38E+00,8.90E-01,8.90E-01",
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_dms_io"],
                    "2.00E-03,2.00E-03,1.40E-02,2.00E-03,2.00E-03,8.00E-03,5.00E-02,5.00E-02,1.92E-02,1.20E-01,1.20E-01,8.00E-03,8.00E-03",
                )
            elif npft == 5:
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c2h4_io"],
                    ",".join(["1.11E+00", "1.54E+00", "8.30E-01", "1.99E+00", "8.30E-01"]),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c2h6_io"],
                    ",".join(["8.80E-01", "9.70E-01", "4.20E-01", "1.01E+00", "4.20E-01"]),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c3h8_io"],
                    ",".join(["5.30E-01", "2.90E-01", "1.30E-01", "3.12E-01", "1.30E-01"]),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_hcho_io"],
                    ",".join(["2.40E+00", "1.75E+00", "1.23E+00", "2.95E+00", "1.23E+00"]),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_mecho_io"],
                    ",".join(["2.26E+00", "8.10E-01", "8.40E-01", "2.02E+00", "8.40E-01"]),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_nh3_io"],
                    ",".join(["1.33E+00", "2.50E+00", "8.90E-01", "2.14E+00", "8.90E-01"]),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_dms_io"],
                    ",".join(["2.00E-03", "2.00E-03", "8.00E-03", "1.92E-02", "8.00E-03"]),
                )

            else:
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c2h4_io"],
                    ",".join([RMDI] * npft),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c2h6_io"],
                    ",".join([RMDI] * npft),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_c3h8_io"],
                    ",".join([RMDI] * npft),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_hcho_io"],
                    ",".join([RMDI] * npft),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_mecho_io"],
                    ",".join([RMDI] * npft),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_nh3_io"],
                    ",".join([RMDI] * npft),
                )
                self.add_setting(
                    config,
                    ["namelist:jules_pftparm", "fef_dms_io"],
                    ",".join([RMDI] * npft),
                )

        return config, self.reports


class vn78_t1590(MacroUpgrade):

    """Upgrade macro from JULES by Megan Brown"""

    BEFORE_TAG = "vn7.8_t1579"
    AFTER_TAG = "vn7.8_t1590"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        # jules_deposition
        self.add_setting(config, ["namelist:jules_deposition", "dep_h2_soil_scheme"], "1")

        return config, self.reports


class vn78_vn79(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.8_t1590"
    AFTER_TAG = "vn7.9"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
