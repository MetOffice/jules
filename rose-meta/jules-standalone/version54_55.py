import re
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


class vn54_t870(MacroUpgrade):

    """Upgrade macro from JULES by Adrian Lock"""

    BEFORE_TAG = "vn5.4"
    AFTER_TAG = "vn5.4_t870"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config,["namelist:jules_surface", "fd_hill_option"], "2")
        return config, self.reports



class vn54_t900(MacroUpgrade):

    """Upgrade macro from JULES by Danny Eisenberg"""

    BEFORE_TAG = "vn5.4_t870"
    AFTER_TAG = "vn5.4_t900"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Purpose: Take soil parameters out of nveg_parms.nml namelist file and
        #          put them in a new namelist in a new namelist file

        # Remove jules_nveg_parm_cable namelist from the nveg_parms.nml namelist file
        contents = self.get_setting_value(config, ["file:nveg_params.nml", "source"])
        contents = re.sub(r'\(namelist:jules_nvegparm_cable\)', r'', contents)
        self.change_setting_value(config, ["file:nveg_params.nml", "source"], contents)
        # Create new jules_soilparm_cable namelist in jules_soilparm_cable.nml file
        self.add_setting(config, ["file:jules_soilparm_cable.nml", "source"], "(namelist:jules_soilparm_cable)")
        # Move each of the variables in the old namelist to the new namelist
        variables = ["silt_io", "clay_io", "sand_io", "swilt_io", "sfc_io", "ssat_io", "bch_io", "hyds_io", "sucs_io", "rhosoil_io", "css_io"]
        for var in variables:
             value = self.get_setting_value(config, ["namelist:jules_nvegparm_cable", var])
             self.add_setting(config, ["namelist:jules_soilparm_cable", var], value)
             self.remove_setting(config, ["namelist:jules_nvegparm_cable", var])
        # Remove jules_nvegparm_cable namelist completely
        self.remove_setting(config, ["namelist:jules_nvegparm_cable"])

        return config, self.reports

class vn54_t662(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.4_t900"
    AFTER_TAG = "vn5.4_t662"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add a new file, namelist (jules_deposition) and variables for deposition.
        self.add_setting(config, ["file:jules_deposition.nml", "source"],
                                  "namelist:jules_deposition")
        self.add_setting(config, ["namelist:jules_deposition", "l_deposition"], ".false.")
        self.add_setting(config, ["namelist:jules_deposition", "l_deposition_flux"], ".false.")
        self.add_setting(config, ["namelist:jules_deposition", "l_ukca_ddep_lev1"], ".false.")
        self.add_setting(config, ["namelist:jules_deposition", "dry_dep_model"], "1")
        self.add_setting(config, ["namelist:jules_deposition", "ndry_dep_species"], "1")
        self.add_setting(config, ["namelist:jules_deposition", "tundra_s_limit"], "60.0")
        self.add_setting(config, ["namelist:jules_deposition", "dzl_const"], "20.0")

        # Add a new namelist (jules_deposition_species) for deposition species.
        # Add the new namelist as optional to jules_deposition.nml.
        source = self.get_setting_value(config, ["file:jules_deposition.nml","source"])
        if source:
            self.change_setting_value(config, ["file:jules_deposition.nml","source"],
                                              source.replace("namelist:jules_deposition",
                                                             "namelist:jules_deposition (namelist:jules_deposition_species(:))"))
        # Add the namelist.
        self.add_setting(config, ["namelist:jules_deposition_species(1)"])
        # Add variables.
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "dep_species_name_io"], "'O3'")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "dd_ice_coeff_io"], "-13.57,6841.9,-857410.6")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "ch4_scaling_io"], "15.0")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "ch4dd_tundra_io"], "-4.757e-6,4.0288e-3,-1.13592,106.636")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "cuticle_o3_io"], "5000.0")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "diffusion_coeff_io"], "1.4e-5")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "diffusion_corr_io"], "1.6")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "r_tundra_io"], "800.0")
        self.add_setting(config, ["namelist:jules_deposition_species(1)", "r_wet_soil_o3_io"], "500.0")
        # Note that we are not adding the variables that are dimensioned with the number
        # of surface types because adding those has revealed a bug in the current rose.

        # Add boundary layer height to jules_drive..
        self.add_setting(config, ["namelist:jules_drive", "bl_height"], "1000.0")

        # Add a new namelist (jules_nlsizes), with values, to model_grid.nml.
        namelist_source = self.get_setting_value(config, ["file:model_grid.nml","source"])
        if namelist_source:
            namelist_source = re.sub(r'namelist:jules_model_grid',
                                     r'namelist:jules_model_grid namelist:jules_nlsizes',
                                     namelist_source)
            self.change_setting_value(config, ["file:model_grid.nml","source"], namelist_source)
        self.add_setting(config, ["namelist:jules_nlsizes"])
        self.add_setting(config, ["namelist:jules_nlsizes", "bl_levels"], "1")

        return config, self.reports


class vn54_t434(MacroUpgrade):

    """Upgrade macro from JULES by Edward Comyn-Platt"""

    BEFORE_TAG = "vn5.4_t662"
    AFTER_TAG = "vn5.4_t434"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "land_feed_ch4"], ".false.")
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "fch4_ref"], "180.0")
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "tau_ch4_ref"], "8.4")
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "ch4_ppbv_ref"], "1751.02")
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "file_ch4_n2o"], "")
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "nyr_ch4_n2o"], "241")
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "yr_fch4_ref"], "2000")
        self.add_setting(config, ["namelist:imogen_run_list",
                                        "ch4_init_ppbv"], "774.1")

        return config, self.reports


class vn54_t903(MacroUpgrade):

    """Upgrade macro from JULES by Martin Best"""

    BEFORE_TAG = "vn5.4_t434"
    AFTER_TAG = "vn5.4_t903"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings for logical to limit vegetation canopy heat capacity
        self.add_setting(config, ["namelist:jules_vegetation", "l_limit_canhc"],".false.")

        # Add settings for logical to control specified roughness length
        self.add_setting(config, ["namelist:jules_vegetation", "l_spec_veg_z0"],".false.")

        # Add settings for specified pft roughness lengths
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))

        if npft == 5:
        # 5 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "z0v_io"],"1.1, 1.1, 0.22, 0.22, 1.0")

        elif npft == 9:
        # 9 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "z0v_io"],"1.1, 1.1, 1.1, 1.1, 1.1, 0.22, 0.22, 1.0, 1.0")

        elif npft == 10:
        # 10 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "z0v_io"],"1.1, 1.1, 1.1, 1.1, 1.1, 0.22, 0.22, 0.22, 1.0, 1.0")

        elif npft == 13:
        # 13 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "z0v_io"],"1.1, 1.1, 1.1, 1.1, 1.1, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 1.0, 1.0")

        else:
        # non-standard number for npft
          RMDI = str(-2**30)
          self.add_setting(config, ["namelist:jules_pftparm", "z0v_io"], ", " .join([RMDI] * npft))
          msg = "Non-standard number of npft, setting z0v_io values to missing data"
          self.add_report(info=msg, is_warning=True)

        return config, self.reports


class vn54_vn55(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.4_t903"
    AFTER_TAG = "vn5.5"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
