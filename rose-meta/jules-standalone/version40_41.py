import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
    from rose.config import ConfigNode
else:
    from metomi.rose.upgrade import MacroUpgrade
    from metomi.rose.config import ConfigNode


class vn40_t160(MacroUpgrade):

    """Upgrade macro for JULES ticket #160 by Matt Pryor"""

    BEFORE_TAG = "vn4.0"
    AFTER_TAG = "vn4.0_t160"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_time", "l_leap"], ".true.")

        return config, self.reports


class vn40_t116(MacroUpgrade):

    """Upgrade macro for ticket #116 by Gerd Folberth"""

    BEFORE_TAG = "vn4.0_t160"
    AFTER_TAG = "vn4.0_t116"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        RMDI = str(-2**30)

        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))

        # Add sensible values for the first (up to) 5 pfts and missing data for the rest
        # This will trigger a sensible runtime error in standalone JULES to allow the user
        # to fix the problem
        ci_st_io = ["33.46", "33.46", "34.26", "29.98", "34.26"] + ([RMDI] * (npft - 5))
        self.add_setting(config, ["namelist:jules_pftparm", "ci_st_io"], ", ".join(ci_st_io[:npft]))

        gpp_st_io = ["1.29E-07", "2.58E-08", "2.07E-07", "3.42E-07", "1.68E-007"] + ([RMDI] * (npft - 5))
        self.add_setting(config, ["namelist:jules_pftparm", "gpp_st_io"], ", ".join(gpp_st_io[:npft]))

        return config, self.reports


class vn40_t79(MacroUpgrade):

    """Upgrade macro for JULES ticket #79 by Kate Halladay."""

    BEFORE_TAG = "vn4.0_t116"
    AFTER_TAG = "vn4.0_t79"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_vegetation", "l_irrig_dmd"], ".false.")

        # Add the JULES_IRRIG namelist as an optional namelist in ancillaries.nml
        self.add_setting(config, ["namelist:jules_irrig"], state = ConfigNode.STATE_USER_IGNORED)
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                              source.replace("namelist:jules_co2",
                                                             "(namelist:jules_irrig) namelist:jules_co2"))

        return config, self.reports


class vn40_t167(MacroUpgrade):
    """Upgrade macro for JULES - Andy Wiltshire """

    BEFORE_TAG = "vn4.0_t79"
    AFTER_TAG = "vn4.0_t167"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # ADD the trait parameters to pft_params namelist
        self.add_setting(config, ["namelist:jules_pftparm", "kn_io"],   "5*0.78")
        self.add_setting(config, ["namelist:jules_pftparm", "q10_leaf_io"], "5*2.00")
        self.add_setting(config, ["namelist:jules_pftparm", "lma_io"], "0.0824,0.2263,0.0498,0.1370,0.0695")
        self.add_setting(config, ["namelist:jules_pftparm", "nmass_io"], "0.0210,0.0115,0.0219,0.0131,0.0219")
        self.add_setting(config, ["namelist:jules_pftparm", "vint_io"],"5.73,6.32,6.42,0.00,14.71")
        self.add_setting(config, ["namelist:jules_pftparm", "vsl_io"], "29.81,18.15,40.96,10.24,23.15")

        self.add_setting(config, ["namelist:jules_vegetation", "l_ht_compete"], ".FALSE.")
        self.add_setting(config, ["namelist:jules_vegetation", "l_trait_phys"], ".FALSE.")
        self.add_setting(config, ["namelist:jules_vegetation", "l_landuse"], ".FALSE.")

        self.remove_setting(config, ["namelist:jules_surface", "q10_leaf"])

        return config, self.reports


class vn40_vn41(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.0_t167"
    AFTER_TAG = "vn4.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
