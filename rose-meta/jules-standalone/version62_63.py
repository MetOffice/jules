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


class vn62_t1084(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn6.2"
    AFTER_TAG = "vn6.2_t1084"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add switch for adding land fraction to output profile
        for obj in config.get_value():
            # is it an items namelist?
            if re.search(r'namelist:jules_output_profile', obj):
                self.add_setting(config,[obj,"l_land_frac"], ".false.")

        default = self.get_setting_value(config, ["command","default"])
        if default:
            self.change_setting_value(config, ["command","default"],
                                      default.replace("rose-jules-run",
                                                    "rose-run jules.exe"))
        self.add_setting(config, ["command", "rivers-only"],
                         "rose-run river.exe")

        return config, self.reports

class vn62_t1142(MacroUpgrade):

    """Upgrade macro from JULES by Phil Harris"""

    BEFORE_TAG = "vn6.2_t1084"
    AFTER_TAG = "vn6.2_t1142"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # The initial condition variable name for acclimation temperature has changed.
        source = self.get_setting_value(config, ["namelist:jules_initial","var"])
        if "temp_ave_nday" in source:
            self.change_setting_value(config, ["namelist:jules_initial","var"],
                                              source.replace("temp_ave_nday",
                                                             "t_growth_gb"))
            msg = ("WARNING: The jules_initial prognostic variable "
                   "temp_ave_nday has been renamed t_growth_gb, in line with "
                   "changes to the photosynthesis thermal acclimation scheme. "
                   "Existing dump files that contain temp_ave_nday may need "
                   "amending to remain usable (see TicketDetails page of "
                   "JULES ticket #1142).")
            self.add_report(section="namelist:jules_initial",
                            info=msg, is_warning=True)

        # Add default Kattge and Knorr (2007) acclimation parameters.
        self.add_setting(config, ["namelist:jules_vegetation", "photo_act_model"], "1")
        self.add_setting(config, ["namelist:jules_vegetation", "act_j_coef"], "50.0e3,0.0,0.0")
        self.add_setting(config, ["namelist:jules_vegetation", "act_v_coef"], "72.0e3,0.0,0.0")

        pam = self.get_setting_value(config, ["namelist:jules_vegetation", "photo_acclim_model"])

        if pam in ("0", "1"):
            tmp_coef = "%s,%s,0.0"
        else:
            tmp_coef = "%s,0.0,%s"

        for var in ["dsj", "dsv", "jv25"]:
            # Move existing acclimation parameters to new format.
            var_zero = self.get_setting_value(config, ["namelist:jules_vegetation", "%s_zero" % var])
            var_slope = self.get_setting_value(config, ["namelist:jules_vegetation", "%s_slope" % var])

            var_coef = tmp_coef % (var_zero, var_slope)
            self.add_setting(config, ["namelist:jules_vegetation", "%s_coef" % var], var_coef)

            # Remove existing acclimation parameters.
            self.remove_setting(config, ["namelist:jules_vegetation", "%s_zero" % var])
            self.remove_setting(config, ["namelist:jules_vegetation", "%s_slope" % var])

        # Changes to the adaptation temperature ancillary.
        const_val = self.get_setting_value(config, ["namelist:jules_vegetation_props", "const_val"])
        if const_val is not None:
            const_val = "%f" % (float(const_val) + 273.15)
            self.change_setting_value(config, ["namelist:jules_vegetation_props", "const_val"], const_val)

        self.change_setting_value(config, ["namelist:jules_vegetation_props", "var"], "'t_home_gb'")

        source = self.get_setting_value(config, ["namelist:jules_vegetation_props", "var"], no_ignore=True)
        if source is not None:
            msg = ("NOTE: Old var t_growth_gb was given in Celsius, "
                   "but new var t_home_gb should be given in kelvin.")
            self.add_report(section="namelist:jules_vegetation_props",
                            info=msg, is_warning=True)

        return config, self.reports

class vn62_t1206(MacroUpgrade):

    """Upgrade macro from JULES by Martin Best"""

    BEFORE_TAG = "vn6.2_t1142"
    AFTER_TAG = "vn6.2_t1206"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))

        if npft == 5:
        # 5 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "dust_veg_scj_io"],"0.0,0.0,1.0,1.0,0.5")

        elif npft == 9:
        # 9 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "dust_veg_scj_io"],"0.0,0.0,0.0,0.0,0.0,1.0,1.0,0.5,0.5")

        elif npft == 10:
        # 10 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "dust_veg_scj_io"],"0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,0.5,0.5")

        elif npft == 13:
        # 13 vegetation types
          self.add_setting(config, ["namelist:jules_pftparm", "dust_veg_scj_io"],"0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,0.5,0.5")

        else:
        # non-standard number for npft: Set all values to missing data
          RMDI = str(-2**30)
          self.add_setting(config, ["namelist:jules_pftparm", "dust_veg_scj_io"],','.join([RMDI]*npft))
          msg = "Non-standard number of npft, setting dust_veg_scj_io values to missing data"
          self.add_report(info=msg, is_warning=True)

        return config, self.reports

class vn62_t1077(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn6.2_t1206"
    AFTER_TAG = "vn6.2_t1077"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Combine jules_urban* namelists into one namelist jules_urban
        self.rename_setting(config,
                    ["namelist:jules_urban2t_param","anthrop_heat_scale"],
                    ["namelist:jules_urban","anthrop_heat_scale"])
        self.rename_setting(config,
                    ["namelist:jules_urban_switches","l_moruses_albedo"],
                    ["namelist:jules_urban","l_moruses_albedo"])
        self.rename_setting(config,
                    ["namelist:jules_urban_switches","l_moruses_emissivity"],
                    ["namelist:jules_urban","l_moruses_emissivity"])
        self.rename_setting(config,
                    ["namelist:jules_urban_switches","l_moruses_macdonald"],
                    ["namelist:jules_urban","l_moruses_macdonald"])
        self.rename_setting(config,
                    ["namelist:jules_urban_switches","l_moruses_rough"],
                    ["namelist:jules_urban","l_moruses_rough"])
        self.rename_setting(config,
                    ["namelist:jules_urban_switches","l_moruses_storage"],
                    ["namelist:jules_urban","l_moruses_storage"])
        self.rename_setting(config,
                    ["namelist:jules_urban_switches","l_moruses_storage_thin"],
                    ["namelist:jules_urban","l_moruses_storage_thin"])
        self.rename_setting(config,
                    ["namelist:jules_urban_switches","l_urban_empirical"],
                    ["namelist:jules_urban","l_urban_empirical"])

        self.remove_setting(config,["namelist:jules_urban_switches"])
        self.remove_setting(config,["namelist:jules_urban2t_param"])

        source = self.get_setting_value(config, ["file:urban.nml","source"])
        source = source.replace("(namelist:jules_urban_switches) ",
                                "(namelist:jules_urban)")
        source = source.replace("(namelist:jules_urban2t_param)", "")
        self.change_setting_value(config, ["file:urban.nml","source"], source)

        return config, self.reports

class vn62_t1214(MacroUpgrade):
    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn6.2_t1077"
    AFTER_TAG = "vn6.2_t1214"

    def upgrade(self, config, meta_config=None):
        """Create new namelist imogen_onoff_switch."""

        # Add settings
        self.add_setting(config, ["namelist:imogen_onoff_switch"])
        self.add_setting(config, ["file:imogen.nml", "source"],
                         "(namelist:imogen_onoff_switch)")

        source = self.get_setting_value(config, ["file:imogen.nml", "source"])
        if source:
            self.change_setting_value(config, ["file:imogen.nml", "source"],
                                      source.replace(
                                          "(namelist:imogen_run_list)",
                                          "(namelist:imogen_onoff_switch) (namelist:imogen_run_list)"))

        self.rename_setting(config, ["namelist:jules_drive", "l_imogen"],
                            ["namelist:imogen_onoff_switch", "l_imogen"])

        return config, self.reports

class vn62_t1234(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn6.2_t1214"
    AFTER_TAG = "vn6.2_t1234"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings in jules_soil - these are now compulsory in meta data.
        self.add_setting(config, ["namelist:jules_soil", "confrac"], "0.3")
        self.add_setting(config, ["namelist:jules_soil", "cs_min"], "1.0e-6")
        self.add_setting(config, ["namelist:jules_soil", "zsmc"], "1.0")
        self.add_setting(config, ["namelist:jules_soil", "zst"], "1.0")

        # Add settings in jules_soil_biogeochemistry - these are now compulsory in meta data.
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "diff_n_pft"], "100.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "bio_hum_cn"], "10.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "sorp"], "10.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "n_inorg_turnover"], "1.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "q10_soil"], "2.0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "kaps_roth"], "3.22e-7,9.65e-9,2.12e-8,6.43e-10")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "kaps"], "0.5e-8")
        return config, self.reports


class vn62_t1195(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn6.2_t1234"
    AFTER_TAG = "vn6.2_t1195"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Renaming to match LFRic metadata changes
        self.rename_setting(config, ["namelist:jules_surface", "fd_stab_dep"],
                            ["namelist:jules_surface", "fd_stability_dep"])
        self.rename_setting(config, ["namelist:jules_surface", "isrfexcnvgust"],
                            ["namelist:jules_surface", "srf_ex_cnv_gust"])
        return config, self.reports

class vn62_t1094(MacroUpgrade):

    """Upgrade macro from JULES by Helen Johnson"""

    BEFORE_TAG = "vn6.2_t1195"
    AFTER_TAG = "vn6.2_t1094"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Metadata changes to triggering only.
        return config, self.reports


class vn62_vn63(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.2_t1094"
    AFTER_TAG = "vn6.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports