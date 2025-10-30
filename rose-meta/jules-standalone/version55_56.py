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


class vn55_t822(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.5"
    AFTER_TAG = "vn5.5_t822"

    def _rename_section_fast(self, config, section, new_section):
        """Rename a setting in the configuration."""
        config.value[new_section] = config.value[section]
        config.unset([section])
        info = self.INFO_RENAMED_SECT.format(section, new_section)
        self.add_report(new_section, None, None, info)
        self.add_report(section, None, None, self.INFO_REMOVED)

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config,
                         ["namelist:jules_radiation", "fixed_sea_albedo"],
                         "0.0")
        self.add_setting(config,
                         ["namelist:jules_radiation", "i_sea_alb_method"],
                         "1")
        self.add_setting(config,
                         ["namelist:jules_radiation", "l_sea_alb_var_chl"],
                         ".false.")
        self.add_setting(config,
                         ["namelist:jules_radiation", "l_spec_sea_alb"],
                         ".false.")
        self.add_setting(config,
                         ["namelist:jules_radiation", "l_dolr_land_black"],
                         ".false.")

        # jules_vegetation
        self.add_setting(config, ["namelist:jules_vegetation",
                                  "l_nrun_mid_trif"],
                         ".false.")
        self.add_setting(config, ["namelist:jules_vegetation",
                                  "l_trif_init_accum"],
                         ".false.")
        # The default has been changed from .true. to .false. in the code,
        # however if l_triffid=F then the code changed this to .false. anyway.
        # It's also trigger ignored when l_triffid=F and the compulsory hasn't
        # changed, so probably doesn't make any difference. Adding for
        # completeness
        self.add_setting(config, ["namelist:jules_vegetation",
                                  "l_veg_compete"],
                         ".true.")
        self.add_setting(config, ["namelist:jules_vegetation", "frac_min"],
                         "1.0e-6")
        self.add_setting(config, ["namelist:jules_vegetation", "frac_seed"],
                         "0.01")
        self.add_setting(config, ["namelist:jules_vegetation", "ilayers"],
                         "10")
        self.add_setting(config, ["namelist:jules_vegetation", "pow"],
                         "20.0")

        # Rename jules_cable* to cable_* so CABLE namelists appear together and
        # separately from JULES namelists (copied from vn47_t334).
        namelist = []

        namelsts = self.get_setting_value(config, ["file:pft_params.nml",
                                                   "source"],)
        if (namelsts):
            namelsts=namelsts.replace("namelist:jules_pftparm_cable",
                                      "namelist:cable_pftparm")
            self.change_setting_value(config, ["file:pft_params.nml","source"],
                                      namelsts)

        namelsts = self.get_setting_value(config,
                                          ["file:jules_soilparm_cable.nml",
                                           "source"],)
        if (namelsts):
            namelsts=namelsts.replace("namelist:jules_soilparm_cable",
                                      "namelist:cable_soilparm")
            self.change_setting_value(config, ["file:jules_soilparm_cable.nml",
                                               "source"],
                                      namelsts)

        for obj in config.get_value():
            if re.search(r'namelist:jules_pftparm_cable',obj):
                namelist.append(obj)
            elif re.search(r'namelist:jules_soilparm_cable',obj):
                namelist.append(obj)

        for obj in namelist:
            if re.search(r'namelist:jules_pftparm_cable',obj):
                new_obj = obj.replace("namelist:jules_pftparm_cable",
                                      "namelist:cable_pftparm")
                self._rename_section_fast(config, obj,new_obj)
            elif re.search(r'namelist:jules_soilparm_cable',obj):
                new_obj = obj.replace("namelist:jules_soilparm_cable",
                                      "namelist:cable_soilparm")
                self._rename_section_fast(config, obj,new_obj)

        # Rename the CABLE namelist file to keep it consistent
        self.rename_setting(config, ["file:jules_soilparm_cable.nml"],
                            ["file:cable_soilparm.nml"])

        return config, self.reports

class vn55_t864(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.5_t822"
    AFTER_TAG = "vn5.5_t864"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add new switch and related parameters for photosynthesis model.
        self.add_setting(config, ["namelist:jules_vegetation", "photo_model"], "1")
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_pftparm", "act_jmax_io"],','.join(['50.0e3']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "act_vcmax_io"],','.join(['72.0e3']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "alpha_elec_io"],','.join(['0.4']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "deact_jmax_io"],','.join(['200.0e3']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "deact_vcmax_io"],','.join(['200.0e3']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "ds_jmax_io"],','.join(['646.0']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "ds_vcmax_io"],','.join(['649.0']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "jv25_ratio_io"],','.join(['1.97']*npft))
        return config, self.reports


class vn55_vn56(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.5_t864"
    AFTER_TAG = "vn5.6"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
