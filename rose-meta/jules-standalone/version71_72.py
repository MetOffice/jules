import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

class vn71_t1191(MacroUpgrade):

    """Upgrade macro from JULES by Juan M Castillo"""

    BEFORE_TAG = "vn7.1"
    AFTER_TAG = "vn7.1_t1191"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.add_setting(config, ["file:oasis_rivers.nml", "source"],
                         "(namelist:oasis_rivers)")

        IMDI = str(-2**15)

        self.add_setting(config, ["namelist:oasis_rivers",
                                  "np_receive"], IMDI)

        self.add_setting(config, ["namelist:oasis_rivers",
                                  "np_send"], IMDI)

        self.add_setting(config, ["namelist:oasis_rivers",
                                  "cpl_freq"], IMDI)

        self.add_setting(config, ["namelist:oasis_rivers",
                                  "send_fields"], "''")

        self.add_setting(config, ["namelist:oasis_rivers",
                                  "receive_fields"], "''")

        self.add_setting(config, ["namelist:oasis_rivers",
                                  "riv_number_file"], "''")

        return config, self.reports

class vn71_t1348(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn7.1_t1191"
    AFTER_TAG = "vn7.1_t1348"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.rename_setting(config, ["namelist:jules_soil_biogeochem", "kaps_roth"], ["namelist:jules_soil_biogeochem", "kaps_4pool"])
        self.rename_setting(config, ["namelist:jules_soil_ecosse", "decomp_wrate_min_rothc"], ["namelist:jules_soil_ecosse", "decomp_wrate_min_smith"])
        self.rename_setting(config, ["namelist:jules_soil_ecosse", "decomp_temp_coeff_rothc"], ["namelist:jules_soil_ecosse", "decomp_temp_coeff_smith"])
        self.rename_setting(config, ["namelist:jules_soil_ecosse", "decomp_wrate_min_jules"], ["namelist:jules_soil_ecosse", "decomp_wrate_min_clark"])
        return config, self.reports


class vn71_t1317(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.1_t1348"
    AFTER_TAG = "vn7.1_t1317"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add setting.
        # Setting to TRUE as most applications likely use a lat-lon grid.
        self.add_setting(config, ["namelist:jules_latlon", "l_coord_latlon"], ".true.")
        # Change some names.
        self.rename_setting(config, ["namelist:jules_model_grid", "latlon_region"],
                                    ["namelist:jules_model_grid", "l_bounds"])
        self.rename_setting(config, ["namelist:jules_model_grid", "lat_bounds"],
                                    ["namelist:jules_model_grid", "y_bounds"])
        self.rename_setting(config, ["namelist:jules_model_grid", "lon_bounds"],
                                    ["namelist:jules_model_grid", "x_bounds"])
        return config, self.reports

class vn71_t1399(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.1_t1317"
    AFTER_TAG = "vn7.1_t1399"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Blank macro to update triggering.
        return config, self.reports


class vn71_vn72(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.1_t1399"
    AFTER_TAG = "vn7.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
