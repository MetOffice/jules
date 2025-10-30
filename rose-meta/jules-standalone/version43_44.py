import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

class vn43_t144(MacroUpgrade):

    """Upgrade macro from JULES vn4.3 to vn4.3_t144 by Eddy Robertson"""

    BEFORE_TAG = "vn4.3"
    AFTER_TAG = "vn4.3_t144"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings for wood products

        self.add_setting(config, ["namelist:jules_triffid", "alloc_fast_io"],  "0.6,0.6,1.0,1.0,0.8")
        self.add_setting(config, ["namelist:jules_triffid", "alloc_med_io"],  "0.3,0.4,0.0,0.0,0.2")
        self.add_setting(config, ["namelist:jules_triffid", "alloc_slow_io"],  "0.1,0.0,0.0,0.0,0.0")

        return config, self.reports


class vn43_t138(MacroUpgrade):

    """Upgrade macro for JULES ticket #138 by Nic Gedney"""

    BEFORE_TAG = "vn4.3_t144"
    AFTER_TAG = "vn4.3_t138"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_hydrology", "l_wetland_ch4_npp"], ".false.")

        return config, self.reports


class vn43_t166(MacroUpgrade):

    """Upgrade macro for JULES ticket #166 by Sarah Shannon"""

    BEFORE_TAG = "vn4.3_t138"
    AFTER_TAG  = "vn4.3_t166"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add a switch to adjust downward longwave radiation for elevated tiles.
        self.add_setting(config, ["namelist:jules_surface", "l_elev_lw_down"], ".false.")

        return config, self.reports


class vn43_t110(MacroUpgrade):

    """Upgrade macro for JULES ticket #110 by Sarah Shannon"""

    BEFORE_TAG = "vn4.3_t166"
    AFTER_TAG  = "vn4.3_t110"


    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""


        # Just add the new compulsory namelist item with its default value

        # Changes to jules_surf_hgt namelist. Namelist members are "user-ignored" state, since zero_height=.true.
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        nnvg = int(self.get_setting_value(config, ["namelist:jules_surface_types", "nnvg"]))

        self.add_setting(config, ["namelist:jules_surf_hgt", "l_elev_absolute_height"],','.join(['.false.']*(npft+nnvg)), info="Add a switch to use absolute elevations levels above sea-level")
        self.add_setting(config, ["namelist:jules_surf_hgt", "use_file"],".false.",info="Add use_file logical switch read elevations relative to the gridbox mean from a file or not")
        self.add_setting(config, ["namelist:jules_surf_hgt", "surf_hgt_io"],','.join(['0.0']*(npft+nnvg)))

        # Add a jules_z_land namelist.
        self.add_setting(config, ["namelist:jules_z_land"])

        # Namelist members are "user-ignored" state, since l_elev_absolute_height is false for all tiles
        self.add_setting(config, ["namelist:jules_z_land", "surf_hgt_band"],','.join(['0.0']*(npft+nnvg)))
        self.add_setting(config, ["namelist:jules_z_land", "use_file"], ".false.")
        self.add_setting(config, ["namelist:jules_z_land", "file"], " '' ")
        self.add_setting(config, ["namelist:jules_z_land", "z_land_name"], " '' ")
        self.add_setting(config, ["namelist:jules_z_land", "z_land_io"], "0.0")

        #  Add the JULES_Z_LAND namelist as an optional namelist in model_grid.nml
        source = self.get_setting_value(config, ["file:model_grid.nml","source"])
        if source:
            self.change_setting_value(config, ["file:model_grid.nml","source"],
                                      source.replace("namelist:jules_surf_hgt",
                                     "namelist:jules_surf_hgt (namelist:jules_z_land)"))

        return config, self.reports

class vn43_vn44(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.3_t110"
    AFTER_TAG = "vn4.4"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
