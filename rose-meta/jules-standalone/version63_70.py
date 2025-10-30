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

class vn63_t1249(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn6.3"
    AFTER_TAG = "vn6.3_t1249"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.remove_setting(config,["namelist:jules_surface",
                                    "l_surface_type_ids"])

        npft = int(self.get_setting_value(config,
                                    ["namelist:jules_surface_types", "npft"]))

        if npft == 5:
        # 5 vegetation types
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "brd_leaf"], "1")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "ndl_leaf"], "2")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c3_grass"], "3")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c4_grass"], "4")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "shrub"],    "5")

        elif npft == 9 or npft == 13:
        # 9 or 13 vegetation types; the first 6 are common.
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "brd_leaf_dec"], "1")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "brd_leaf_eg_trop"], "2")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "brd_leaf_eg_temp"], "3")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "ndl_leaf_dec"], "4")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "ndl_leaf_eg"], "5")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c3_grass"], "6")
        else:
        # Unrecognised PFT configuration
          msg = "Non-standard number of npft; will need be added manually."
          self.add_report(info=msg, is_warning=True)


        if npft == 9:
        # 9 vegetation types; the remaining 3 differ from npft == 13
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c4_grass"], "7")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "shrub_dec"], "8")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "shrub_eg"], "9")

        elif npft == 13:
        # 13 vegetation types
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c3_crop"], "7")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c3_pasture"], "8")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c4_grass"], "9")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c4_crop"], "10")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "c4_pasture"], "11")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "shrub_dec"], "12")
            self.add_setting(config, ["namelist:jules_surface_types",
                                      "shrub_eg"], "13")


        # Now deal with the non-vegetated types
        # Most of these have been dealt with in previous upgrade macros as they
        # are attached to switches.

        nnvg = int(self.get_setting_value(config,
                                    ["namelist:jules_surface_types", "nnvg"]))

        if nnvg == 4:
        # 4 non-vegetated types; all other non-veg types are compulsory and thus
        # should already exist in configurations. This macro needs to only deal
        # with those that are not compulsory i.e. only lake and ice to add.
            if npft == 5:
            # 5 vegetation types
                self.add_setting(config, ["namelist:jules_surface_types",
                                          "lake"], "7")
                self.add_setting(config, ["namelist:jules_surface_types",
                                          "ice"], "9")
            elif npft == 9:
            # 9 vegetation types
                self.add_setting(config, ["namelist:jules_surface_types",
                                          "lake"], "11")
                self.add_setting(config, ["namelist:jules_surface_types",
                                          "ice"], "13")
            elif npft == 13:
            # 13 vegetation types
                self.add_setting(config, ["namelist:jules_surface_types",
                                          "lake"], "15")
                self.add_setting(config, ["namelist:jules_surface_types",
                                          "ice"], "17")

        return config, self.reports

class vn63_t1287(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn6.3_t1249"
    AFTER_TAG = "vn6.3_t1287"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Reuse the existing file name.
        file = self.get_setting_value(config, ["namelist:jules_rivers_props","file"])
        self.add_setting(config, ["namelist:jules_rivers_props", "coordinate_file"], file)
        return config, self.reports

class vn63_t1272(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn6.3_t1287"
    AFTER_TAG = "vn6.3_t1272"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Blank upgrade macro to ensure that the inherited metadata is tested
        return config, self.reports

class vn63_t1256(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn6.3_t1272"
    AFTER_TAG = "vn6.3_t1256"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Blank macro to update triggering.
        return config, self.reports


class vn63_t1242(MacroUpgrade):

    """Upgrade macro from JULES by Martin Best"""

    BEFORE_TAG = "vn6.3_t1256"
    AFTER_TAG = "vn6.3_t1242"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.add_setting(config, ["namelist:jules_surface", "l_mo_buoyancy_calc"],".false.")

        return config, self.reports

class vn63_t1279(MacroUpgrade):

    """Upgrade macro from JULES by Martin Best"""

    BEFORE_TAG = "vn6.3_t1242"
    AFTER_TAG = "vn6.3_t1279"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_temp_fixes",
                                  "l_fix_snow_frac"],
                                  ".false.")
        return config, self.reports

class vn63_t911(MacroUpgrade):
    """Upgrade macro from JULES by Emma Littleton"""
    BEFORE_TAG = "vn6.3_t1279"
    AFTER_TAG = "vn6.3_t911"
    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        """
         Add parameters for triffid and vegetation;
         add files for reading in bioenergy crop fractions.
        """

        # Parameters that are length (npft):
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_triffid", "harvest_freq_io"],
                        ','.join(['1']*npft))
        self.add_setting(config, ["namelist:jules_triffid", "ag_expand_io"],
                        ','.join(['0']*npft))
        self.add_setting(config, ["namelist:jules_triffid", "harvest_ht_io"],
                         ','.join(['3']*npft))
        self.add_setting(config, ["namelist:jules_triffid", "harvest_type_io"],
                         ','.join(['0']*npft))
        self.add_setting(config, ["namelist:jules_vegetation", "l_trif_biocrop"],
                         ".false.")
        self.add_setting(config, ["namelist:jules_vegetation", "l_ag_expand"],
                         ".false.")
        self.add_setting(config, ["namelist:jules_agric", "zero_biocrop"], ".true.")
        self.add_setting(config, ["namelist:jules_agric", "frac_biocrop"], "0")
        self.add_setting(config, ["namelist:jules_agric", "file_biocrop"], " '' ")
        self.add_setting(config, ["namelist:jules_agric", "biocrop_name"], " 'frac_biocrop' ")
        self.add_setting(config, ["namelist:jules_agric", "file_harvest_doy"]," '' ")
        self.add_setting(config, ["namelist:jules_agric", "read_harvest_doy_from_dump"], ".false.")
        self.add_setting(config,["namelist:jules_agric", "harvest_doy_name"]," '' ")
        return config, self.reports

class vn63_vn70(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.3_t911"
    AFTER_TAG = "vn7.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports