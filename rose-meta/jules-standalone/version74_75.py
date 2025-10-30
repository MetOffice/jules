import sys
import re
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn74_t1361(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.4"
    AFTER_TAG = "vn7.4_t1361"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Move river number file to make accessible to standalone
        self.rename_setting(config, ["namelist:oasis_rivers",
                                     "riv_number_file"],
                            ["namelist:jules_rivers_props", "riv_number_file"])

        # Add switch to ignore newly added check to ensure that river routing &
        # coupling ancillaries are compatible. Default is to check compatibility
        self.add_setting(config, ["namelist:jules_rivers_props",
                                  "l_ignore_ancil_rivers_check"], ".false.")

        # Rename send field rflow_outflow in OASIS-Rivers namelist
        send_fields = self.get_setting_value(config,
                                             ["namelist:oasis_rivers",
                                              "send_fields"])
        send_fields = re.sub(r'rflow_outflow',
                             r'outflow_per_river', send_fields)
        self.change_setting_value(config,
                                  ["namelist:oasis_rivers", "send_fields"],
                                  send_fields)

        return config, self.reports


class vn74_t1374(MacroUpgrade):

    """Upgrade macro from JULES #1374 by Dan Copsey"""

    BEFORE_TAG = "vn7.4_t1361"
    AFTER_TAG = "vn7.4_t1374"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config, ["namelist:jules_rivers",
                                  "trip_globe_shape"], "2")

        # Add settings
        return config, self.reports


class vn74_t1482(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.4_t1374"
    AFTER_TAG = "vn7.4_t1482"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # compulsory=true added to be consistent with um-atmos.
        # Upgrade macros already exist, however we cannot assume that they
        # exist in apps.

        # From version42_43.py vn4.2_t50
        npft = int(self.get_setting_value(config,
                                          ["namelist:jules_surface_types",
                                           "npft"]))

        self.add_setting(config, ["namelist:jules_snow", "a_snow_et"], "0.0")
        self.add_setting(config, ["namelist:jules_snow", "b_snow_et"], "0.0")
        self.add_setting(config, ["namelist:jules_snow", "c_snow_et"], "0.0")
        self.add_setting(config,
                         ["namelist:jules_snow", "can_clump"],
                         ','.join(['0.0']*npft))
        self.add_setting(config,
                         ["namelist:jules_snow", "l_snow_infilt"],
                         ".false.")
        self.add_setting(config,
                         ["namelist:jules_snow", "l_snow_nocan_hc"],
                         ".false.")
        self.add_setting(config,
                         ["namelist:jules_snow", "lai_alb_lim_sn"],
                         ','.join(['0.5']*npft))
        self.add_setting(config,
                         ["namelist:jules_snow", "n_lai_exposed"],
                         ','.join(['0.0']*npft))
        self.add_setting(config,
                         ["namelist:jules_snow", "rho_snow_et_crit"],
                         "0.0")
        self.add_setting(config,
                         ["namelist:jules_snow", "unload_rate_cnst"],
                         ','.join(['0.0']*npft))
        self.add_setting(config,
                         ["namelist:jules_snow", "unload_rate_u"],
                         ','.join(['0.0']*npft))

        # From version45_46.py vn4.5_t214
        self.add_setting(config,
                         ["namelist:jules_snow", "aicemax"], "0.78, 0.36")
        self.add_setting(config,
                         ["namelist:jules_snow", "rho_firn_albedo"], "550.0")

        return config, self.reports


class vn74_t1380(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn7.4_t1482"
    AFTER_TAG = "vn7.4_t1380"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add a setting.
        self.add_setting(config, ["namelist:jules_rivers_props", "l_find_grid"], ".false.")

        return config, self.reports


class vn74_t1491(MacroUpgrade):

    """Upgrade macro from JULES by Giorgia Line"""

    BEFORE_TAG = "vn7.4_t1380"
    AFTER_TAG = "vn7.4_t1491"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Adding new stomatal conductance model option for SOX
        # Get number of plant functional types
        npft = self.get_setting_value(
                config, ["namelist:jules_surface_types", "npft"]
        )
        if npft is not None:
            npft = int(npft)
            # Add new namelist variables
            self.add_setting(
                config,
                ["namelist:jules_pftparm", "sox_a_io"],
                ",".join(["0.0"] * npft),
            )
            self.add_setting(
                config,
                ["namelist:jules_pftparm", "sox_p50_io"],
                ",".join(["0.0"] * npft),
            )
            self.add_setting(
                config,
                ["namelist:jules_pftparm", "sox_rp_min_io"],
                ",".join(["0.0"] * npft),
            )
        else:
            rivers_only = """
        !!!!!      Configuration assumed to be Rivers-standalone      !!!!!
        !!!!! * npft was not found                                    !!!!!
        !!!!! If this is in error please check your configuration.    !!!!!
            """
            self.add_report(info=rivers_only, is_warning=True)

        return config, self.reports


class vn71_t1396(MacroUpgrade):

    """Upgrade macro from JULES by J. M. Edwards"""

    BEFORE_TAG = "vn7.4_t1491"
    AFTER_TAG = "vn7.4_t1396"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_neg_snow"], ".true.")
        return config, self.reports


class vn74_t1265(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn7.4_t1396"
    AFTER_TAG = "vn7.4_t1265"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.rename_setting(config,["namelist:imogen_anlg_vals_list", "dir_anom"],
                                   ["namelist:imogen_anlg_vals_list", "file_base_anom"])
        self.rename_setting(config,["namelist:imogen_anlg_vals_list", "dir_patt"],
                                   ["namelist:imogen_anlg_vals_list", "file_patt"])
        self.rename_setting(config,["namelist:imogen_anlg_vals_list", "dir_clim"],
                                   ["namelist:imogen_anlg_vals_list", "file_clim"])
        self.remove_setting(config,["namelist:imogen_run_list","file_points_order"])
        return config, self.reports


class vn74_t1507(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.4_t1265"
    AFTER_TAG = "vn7.5"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
