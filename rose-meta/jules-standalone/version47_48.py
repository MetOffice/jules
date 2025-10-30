import re
import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn47_t414(MacroUpgrade):

    """Upgrade macro from JULES by Adrian Lock"""

    BEFORE_TAG = "vn4.7"
    AFTER_TAG = "vn4.7_t414"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_snow", "graupel_options"],
                         "0")
        return config, self.reports

class vn47_t413(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn4.7_t414"
    AFTER_TAG = "vn4.7_t413"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        # If total_snow is not given, set it to true (as it was formerly).
        source = self.get_setting_value(config, ["namelist:jules_initial", "total_snow"])
        if not source:
            self.add_setting(config, ["namelist:jules_initial", "total_snow"], ".true.")

        return config, self.reports


class vn47_t456(MacroUpgrade):

    """Upgrade macro from JULES by Chantelle Burton"""

    BEFORE_TAG = "vn4.7_t413"
    AFTER_TAG = "vn4.7_t456"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_vegetation", "l_trif_fire"], ".false.")
        return config, self.reports

class vn47_t428(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn4.7_t456"
    AFTER_TAG = "vn4.7_t428"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add new setting.
        self.add_setting(config, ["namelist:jules_output", "dump_period"], "1")
        # Provide the previous default name for time dimension, if not provided..
        self.add_setting(config, ["namelist:jules_input_grid", "time_dim_name"], "'tstep'")
        return config, self.reports


class vn47_t334(MacroUpgrade):

    """Upgrade macro from JULES by Glenn Greed"""

    BEFORE_TAG = "vn4.7_t428"
    AFTER_TAG = "vn4.7_t334"

    def _rename_section_fast(self, config, section, new_section):
        """Rename a setting in the configuration."""
        config.value[new_section] = config.value[section]
        config.unset([section])
        info = self.INFO_RENAMED_SECT.format(section, new_section)
        self.add_report(new_section, None, None, info)
        self.add_report(section, None, None, self.INFO_REMOVED)

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        namelist = []

        namelsts = self.get_setting_value(config, ["file:urban.nml","source"],)
        if (namelsts):
            namelsts=namelsts.replace("namelist:urban_switches", "namelist:jules_urban_switches")
            namelsts=namelsts.replace("namelist:urban2t_param", "namelist:jules_urban2t_param")
            self.change_setting_value(config, ["file:urban.nml","source"],
                                                             namelsts)

        for obj in config.get_value():
            if re.search(r'namelist:urban2t_param',obj):
                namelist.append(obj)
            elif re.search(r'namelist:urban_switches',obj):
                namelist.append(obj)

        for obj in namelist:
            if re.search(r'namelist:urban2t_param',obj):
                new_obj = obj.replace("namelist:urban2t_param", "namelist:jules_urban2t_param")
                self._rename_section_fast(config, obj,new_obj)
            elif re.search(r'namelist:urban_switches',obj):
                new_obj = obj.replace("namelist:urban_switches", "namelist:jules_urban_switches")
                self._rename_section_fast(config, obj,new_obj)
        return config, self.reports

class vn47_t417(MacroUpgrade):

    """Upgrade macro from JULES by Kerry Smout-Day"""

    BEFORE_TAG = "vn4.7_t334"
    AFTER_TAG = "vn4.7_t417"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration.
           Point um_trip to trip, um_rfm to rfm as the
           values have been removed.
        """

        # Add settings
        river_value = self.get_setting_value(config, ["namelist:jules_rivers", "rivers_type"])
        if river_value == "'um_trip'":
           self.change_setting_value(config, ["namelist:jules_rivers", "rivers_type"], 'trip')
           trip_msg= """
           !!!!! NOTE: um_trip is now replaced by trip see jls #417    !!!!
           """
           self.add_report(info=trip_msg, is_warning=True)
        elif river_value == "'um_rfm'":
           self.change_setting_value(config, ["namelist:jules_rivers", "rivers_type"], 'rfm')
           rfm_msg= """
           !!!!! NOTE: um_rfm is now replaced by rfm see jls #417      !!!!
           """
           self.add_report(info=rfm_msg, is_warning=True)
        return config, self.reports


class vn47_t322(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn4.7_t417"
    AFTER_TAG = "vn4.7_t322"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        """
         Create new namelist jules_soil_biogeochem for soil biogeochemical options
         and populate it with new items and items moved from other namelists.
        """

        # Ensure that the setting exists for the new namelist
        self.add_setting(config, ["namelist:jules_soil_biogeochem"])

        # Check if l_triffid is selected.
        l_trif = self.get_setting_value(config, ["namelist:jules_vegetation", "l_triffid"])
        # Add the new compulsory namelist item.
        if l_trif == ".true.":
            self.add_setting(config, ["namelist:jules_soil_biogeochem", "soil_bgc_model"], "2")
        else:
            self.add_setting(config, ["namelist:jules_soil_biogeochem", "soil_bgc_model"], "1")

        # Move settings from jules_surface.
        self.rename_setting(config, ["namelist:jules_surface", "kaps"], ["namelist:jules_soil_biogeochem", "kaps"] )
        self.rename_setting(config, ["namelist:jules_surface", "kaps_roth"], ["namelist:jules_soil_biogeochem", "kaps_roth"] )
        self.rename_setting(config, ["namelist:jules_surface", "q10_soil"], ["namelist:jules_soil_biogeochem", "q10_soil"] )
        self.rename_setting(config, ["namelist:jules_surface", "bio_hum_cn"], ["namelist:jules_soil_biogeochem", "bio_hum_cn"] )
        self.rename_setting(config, ["namelist:jules_surface", "sorp"], ["namelist:jules_soil_biogeochem", "sorp"] )
        self.rename_setting(config, ["namelist:jules_surface", "n_inorg_turnover"], ["namelist:jules_soil_biogeochem", "n_inorg_turnover"] )
        self.rename_setting(config, ["namelist:jules_surface", "diff_n_pft"], ["namelist:jules_soil_biogeochem", "diff_n_pft"] )
        self.rename_setting(config, ["namelist:jules_surface", "l_layeredc"], ["namelist:jules_soil_biogeochem", "l_layeredc"] )
        self.rename_setting(config, ["namelist:jules_surface", "tau_resp"], ["namelist:jules_soil_biogeochem", "tau_resp"] )
        self.rename_setting(config, ["namelist:jules_surface", "tau_lit"], ["namelist:jules_soil_biogeochem", "tau_lit"] )

        # Move settings from jules_vegetation.
        self.rename_setting(config, ["namelist:jules_vegetation", "l_q10"], ["namelist:jules_soil_biogeochem", "l_q10"] )
        self.rename_setting(config, ["namelist:jules_vegetation", "l_soil_resp_lev2"], ["namelist:jules_soil_biogeochem", "l_soil_resp_lev2"] )

        # Add the new file containing the new namelist
        self.add_setting(config, ["file:jules_soil_biogeochem.nml","source"], "namelist:jules_soil_biogeochem")

        return config, self.reports


class vn47_vn48(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.7_t322"
    AFTER_TAG = "vn4.8"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
