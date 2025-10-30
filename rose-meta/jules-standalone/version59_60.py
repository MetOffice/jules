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


class vn59_t1095(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.9"
    AFTER_TAG = "vn5.9_t1095"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add a new file, namelist & switch to control what tasks write output.
        # Added with write to all tasks.
        self.add_setting(config, ["file:jules_prnt_control.nml", "source"],
                                  "namelist:jules_prnt_control")
        self.add_setting(config, ["namelist:jules_prnt_control",
                                  "prnt_writers"], "1")

        # Add switch to allow surface type IDs.
        self.add_setting(config, ["namelist:jules_surface",
                                  "l_surface_type_ids"], ".false.")

        # Move lsm_id to jules_model_environment
        self.rename_setting(config, ["namelist:jules_lsm_switch", "lsm_id"],
                            ["namelist:jules_model_environment", "lsm_id"])
        # Remove jules_lsm_switch namelist
        self.remove_setting(config, ["namelist:jules_lsm_switch"])
        self.remove_setting(config, ["file:jules_lsm_switch.nml"])

        return config, self.reports

class vn59_t1033(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.9_t1095"
    AFTER_TAG = "vn5.9_t1033"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Newly compulsory variables are added with the previous default values from the JULES code.
        self.add_setting(config, ["namelist:jules_drive", "dur_conv_rain"], "21600.0")
        self.add_setting(config, ["namelist:jules_drive", "dur_conv_snow"], "3600.0")
        self.add_setting(config, ["namelist:jules_drive", "dur_ls_rain"], "3600.0")
        self.add_setting(config, ["namelist:jules_drive", "dur_ls_snow"], "3600.0")
        self.add_setting(config, ["namelist:jules_drive", "precip_disagg_method"], "2")
        self.add_setting(config, ["namelist:jules_drive", "precip_rel_perturbation"], "1.0")
        self.add_setting(config, ["namelist:jules_drive", "t_for_con_rain"], "373.15")
        self.add_setting(config, ["namelist:jules_drive", "t_for_snow"], "274.0")
        self.add_setting(config, ["namelist:jules_drive", "temperature_abs_perturbation"], "0.0")
        self.add_setting(config, ["namelist:jules_drive", "z1_tq_in"], "10.0")
        self.add_setting(config, ["namelist:jules_drive", "z1_uv_in"], "10.0")
        return config, self.reports

class vn59_t847(MacroUpgrade):

    """Upgrade macro from JULES by Sarah Chadburn"""

    BEFORE_TAG = "vn5.9_t1033"
    AFTER_TAG = "vn5.9_t847"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "l_ch4_microbe"], ".false.")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "tau_ch4"], "6.5")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "ch4_cpow"], "1.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "k2_ch4"], "0.01")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "kd_ch4"], "0.0003")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "q10_mic_ch4"], "4.3")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "rho_ch4"], "47.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "cue_ch4"], "0.03")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "mu_ch4"], "0.00042")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "frz_ch4"], "0.5")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "alpha_ch4"], "0.001")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "ev_ch4"], "5.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem",
                                        "q10_ev_ch4"], "2.2")
        return config, self.reports


class vn59_t1093(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn5.9_t847"
    AFTER_TAG = "vn5.9_t1093"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_pftparm", "gsoil_f_io"],','.join(['1.0']*npft))

        self.add_setting(config, ["namelist:jules_hydrology", "l_limit_gsoil"], ".false.")

        return config, self.reports


class vn59_t194(MacroUpgrade):

    """Upgrade macro from JULES by Adrian Lock"""

    BEFORE_TAG = "vn5.9_t1093"
    AFTER_TAG = "vn5.9_t194"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_temp_fixes", "l_accurate_rho"], ".false.")
        return config, self.reports


class vn59_t1086(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.9_t194"
    AFTER_TAG = "vn5.9_t1086"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Change how latitude and longitude are provided.
        # Get details of the grid.
        grid = self.get_setting_value(config, ["namelist:jules_input_grid","grid_is_1d"],)
        if grid == ".true.":
           npoints = int(self.get_setting_value(config, ["namelist:jules_input_grid","npoints"],))
        else:
           nx = int(self.get_setting_value(config, ["namelist:jules_input_grid","nx"],))
           ny = int(self.get_setting_value(config, ["namelist:jules_input_grid","ny"],))
           npoints = nx*ny
        if npoints == 1:
           # Grid was a single point; use the latitude and longitude provided.
           latitude = self.get_setting_value(config, ["namelist:jules_latlon","latitude"],)
           longitude = self.get_setting_value(config, ["namelist:jules_latlon","longitude"],)
           const_val = str(latitude) + ',' + str(longitude)
           use_file = ".false.,.false."
           var_name = "''"
        else:
           # Use the variables in a file.
           lat_name = self.get_setting_value(config, ["namelist:jules_latlon","lat_name"],)
           lon_name = self.get_setting_value(config, ["namelist:jules_latlon","lon_name"],)
           const_val = "0.0,0.0"
           use_file = ".true.,.true."
           var_name = str(lat_name) + ',' + str(lon_name)
        # Remove settings.
        self.remove_setting(config,["namelist:jules_latlon", "latitude"])
        self.remove_setting(config,["namelist:jules_latlon", "longitude"])
        self.remove_setting(config,["namelist:jules_latlon", "lat_name"])
        self.remove_setting(config,["namelist:jules_latlon", "lon_name"])
        # Add settings.
        self.add_setting(config, ["namelist:jules_latlon", "nvars"], "2")
        self.add_setting(config, ["namelist:jules_latlon", "read_from_dump"], ".false.")
        self.add_setting(config, ["namelist:jules_latlon", "const_val"], const_val)
        self.add_setting(config, ["namelist:jules_latlon", "use_file"], use_file)
        self.add_setting(config, ["namelist:jules_latlon", "var"], "'latitude','longitude'")
        self.add_setting(config, ["namelist:jules_latlon", "var_name"], var_name)
        self.add_setting(config, ["namelist:jules_latlon", "tpl_name"], "'',''")
        return config, self.reports


class vn59_vn60(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.9_t1086"
    AFTER_TAG = "vn6.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
