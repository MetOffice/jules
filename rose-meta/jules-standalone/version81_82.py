import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


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


class vn81_t34(MacroUpgrade):
    """Upgrade macro from JULES #34 by Dan Copsey"""

    BEFORE_TAG = "vn8.1_t23"
    AFTER_TAG = "vn8.1_t34"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Move the l_inland switch from the rivers namelist to the hydrology
        # namelist. l_inland should always be false as it is only used
        # in um-jules and lfric-jules (not jules-standalone).
        # Also add l_use_land_fraction as false.

        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            self.add_setting(
                config, ["namelist:jules_hydrology", "l_inland"], ".false."
            )
        self.remove_setting(config, ["namelist:jules_rivers", "l_inland"])

        self.add_setting(
            config,
            ["namelist:jules_land_frac", "l_use_land_fraction"],
            ".false.",
        )
        return config, self.reports


class vn81_vn82(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn8.1_t34"
    AFTER_TAG = "vn8.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
