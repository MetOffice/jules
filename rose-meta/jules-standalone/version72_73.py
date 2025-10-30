import re
import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

class vn72_t1180(MacroUpgrade):
    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.2"
    AFTER_TAG = "vn7.2_t1180"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Newly compulsory variables are added with the previous default values from the JULES code.
        self.add_setting(config, ["namelist:jules_hydrology", "b_pdm"], "1.0")
        self.add_setting(config, ["namelist:jules_hydrology", "dz_pdm"], "1.0")
        self.add_setting(config, ["namelist:jules_hydrology", "nfita"], "20")
        self.add_setting(config, ["namelist:jules_hydrology", "s_pdm"], "0.0")
        self.add_setting(config, ["namelist:jules_hydrology", "slope_pdm_max"], "6.0")
        self.add_setting(config, ["namelist:jules_hydrology", "ti_max"], "10.0")
        self.add_setting(config, ["namelist:jules_hydrology", "ti_wetl"], "1.5")
        self.add_setting(config, ["namelist:jules_hydrology", "zw_max"], "6.0")

        return config, self.reports

class vn72_t1385(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.2_t1180"
    AFTER_TAG = "vn7.2_t1385"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Make jules_spinup optional
        contents = self.get_setting_value(config,
                                          ["file:timesteps.nml", "source"])
        contents = re.sub(r'namelist:jules_spinup',
                          r'(namelist:jules_spinup)', contents)
        self.change_setting_value(config,
                                  ["file:timesteps.nml", "source"], contents)

        # Make jules_nlsizes optional
        contents = self.get_setting_value(config,
                                          ["file:model_grid.nml", "source"])
        contents = re.sub(r'namelist:jules_nlsizes',
                          r'(namelist:jules_nlsizes)', contents)
        self.change_setting_value(config,
                                  ["file:model_grid.nml", "source"], contents)

        # Make jules_triffid optional
        contents = self.get_setting_value(config,
                                          ["file:triffid_params.nml",
                                           "source"])
        if contents:
            contents = re.sub(r'namelist:jules_triffid',
                              r'(namelist:jules_triffid)', contents)
            self.change_setting_value(config,
                                      ["file:triffid_params.nml", "source"],
                                      contents)

        # Move namelist for l_riv_overbank to main rivers namelist
        self.rename_setting(config, ["namelist:jules_overbank",
                                     "l_riv_overbank"],
                                    ["namelist:jules_rivers",
                                     "l_riv_overbank"])

        return config, self.reports

class vn72_vn73(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.2_t1385"
    AFTER_TAG = "vn7.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
