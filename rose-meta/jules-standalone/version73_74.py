import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

class vn73_t1441(MacroUpgrade):
    """Upgrade macro from JULES by Lucy Gordon"""

    BEFORE_TAG = "vn7.3"
    AFTER_TAG = "vn7.3_t1441"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Blank macro to apply triggers.
        return config, self.reports


class vn73_t1172(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.3_t1441"
    AFTER_TAG = "vn7.3_t1172"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Remove settings.
        self.remove_setting(config,["namelist:jules_rivers_props", "rivers_reglatlon"])

        # Add a setting, based on whether the area variable is listed.
        source = self.get_setting_value(config, ["namelist:jules_rivers_props","var"])
        if "area" in source:
           switch=".true."
        else:
           switch=".false."
        self.add_setting(config, ["namelist:jules_rivers_props", "l_use_area"], switch)

        # Rename settings.
        self.rename_setting(config, ["namelist:jules_rivers_props", "nx"],
                                    ["namelist:jules_rivers_props", "nx_rivers"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "ny"],
                                    ["namelist:jules_rivers_props", "ny_rivers"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "nx_grid"],
                                    ["namelist:jules_rivers_props", "nx_land_grid"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "ny_grid"],
                                    ["namelist:jules_rivers_props", "ny_land_grid"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "reg_dlat"],
                                    ["namelist:jules_rivers_props", "land_dy"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "reg_dlon"],
                                    ["namelist:jules_rivers_props", "land_dx"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "reg_lat1"],
                                    ["namelist:jules_rivers_props", "y1_land_grid"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "reg_lon1"],
                                    ["namelist:jules_rivers_props", "x1_land_grid"])
        self.rename_setting(config, ["namelist:jules_rivers_props", "rivers_dx"],
                                    ["namelist:jules_rivers_props", "rivers_length"])
        return config, self.reports


class vn73_t1405(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.3_t1172"
    AFTER_TAG = "vn7.3_t1405"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Set new value based on existing settings.
        l_riv_hypsometry= self.get_setting_value(config, ["namelist:jules_overbank","l_riv_hypsometry"])
        use_rosgen= self.get_setting_value(config, ["namelist:jules_overbank","use_rosgen"])
        if l_riv_hypsometry.lower() == ".false.":
           if use_rosgen.lower() == ".false.":
              ob_model = "1"
           else:
              ob_model = "2"
        else:
           ob_model = "3"

        self.add_setting(config, ["namelist:jules_overbank",
                                  "overbank_model"], ob_model)

        # Remove settings.
        self.remove_setting(config,["namelist:jules_overbank", "l_riv_hypsometry"])
        self.remove_setting(config,["namelist:jules_overbank", "use_rosgen"])

        return config, self.reports

class vn73_t1360(MacroUpgrade):

    """Upgrade macro from JULES #1360 by Dan Copsey"""

    BEFORE_TAG = "vn7.3_t1405"
    AFTER_TAG = "vn7.3_t1360"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config, ["namelist:jules_rivers",
                                  "lake_water_conserve_method"], "1")

        # Add settings
        return config, self.reports


class vn73_t1344(MacroUpgrade):
    """Upgrade macro from JULES by Simon Jones/Andy Wiltshire"""

    BEFORE_TAG = "vn7.3_t1360"
    AFTER_TAG = "vn7.3_t1344"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        # Add settings
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_pftparm", "sug_grec_io"],','.join(['1.0']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "sug_g0_io"],','.join(['1.0']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "sug_yg_io"],','.join(['1.0']*npft))

        self.add_setting(config, ["namelist:jules_vegetation", "l_sugar"], ".false.")

        return config, self.reports

class vn73_vn74(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.3_t1344"
    AFTER_TAG = "vn7.4"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
