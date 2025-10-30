import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn44_t155(MacroUpgrade):

    """Upgrade macro from JULES by Eddy Robertson"""

    BEFORE_TAG = "vn4.4"
    AFTER_TAG = "vn4.4_t155"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings for wood products
        self.add_setting(config,
                         ["namelist:jules_triffid", "dpm_rpm_ratio_io"],
                         "0.25,0.25,0.67,0.67,0.33")

        return config, self.reports


class vn44_202(MacroUpgrade):

    """Upgrade macro for JULES ticket #202 by Nic Gedney"""

    BEFORE_TAG = "vn4.4_t155"
    AFTER_TAG = "vn4.4_t202"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_hydrology", "nfita"], "20")

        return config, self.reports


class vn44_t136(MacroUpgrade):

   """Upgrade macro for JULES ticket #136 (INFERNO) by Stephane Mangeon"""

   BEFORE_TAG = "vn4.4_t202"
   AFTER_TAG = "vn4.4_t136"

   def upgrade(self, config, meta_config=None):

       """Upgrade a JULES vegetation configuration"""
       self.remove_setting(config,  ["namelist:jules_vegetation", "l_inferno"])
       self.add_setting(config, ["namelist:jules_vegetation", "l_inferno"], ".false.")

       self.remove_setting(config,  ["namelist:jules_vegetation", "ignition_method"])
       self.add_setting(config, ["namelist:jules_vegetation", "ignition_method"], "1")

       """Upgrade the JULES pft parameters"""
       RMDI = str(-2**30)

       npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))

       ncpft_in = self.get_setting_value(config, ["namelist:jules_surface_types", "ncpft"])
       if ncpft_in:
           ncpft = int(ncpft_in)
       else:
           ncpft = 0

       if npft == 5:
          # Add sensible values for 5 pfts
          fef_co2_io = ["1631", "1576", "1576", "1654", "1576"]
          fef_co_io = ["100", "106", "106", "64", "106"]
          fef_ch4_io = ["6.8", "4.8", "4.8", "2.4", "4.8"]
          fef_nox_io = ["2.55", "3.24", "3.24", "2.49", "3.24"]
          fef_so2_io = ["0.40", "0.40", "0.40", "0.48", "0.40"]
          fef_oc_io = ["4.3", "9.1", "9.1", "3.2", "9.1"]
          fef_bc_io = ["0.56", "0.56", "0.56", "0.47", "0.56"]
          ccleaf_min_io = ["0.8", "0.8", "0.8", "0.8", "0.8"]
          ccleaf_max_io = ["1.0", "1.0", "1.0", "1.0", "1.0"]
          ccwood_min_io = ["0.0", "0.0", "0.0", "0.0", "0.0"]
          ccwood_max_io = ["0.4", "0.4", "0.4", "0.4", "0.4"]
          avg_ba_io = ["0.6E6","0.6E6","1.4E6","1.4E6","1.2E6"]

       if npft == 9:
          # Add sensible values for 9 pfts
          if ncpft == 0:
              fef_co2_io = ["1643","1637","1643","1637","1489","1637","1686","1637","1489"]
              fef_co_io = ["93","89","93","89","127","89","63","89","127"]
              fef_ch4_io = ["5.07","3.92","5.07","3.92","5.96","3.92","1.94","3.92","5.96"]
              fef_nox_io = ["2.55","2.51","2.55","2.51","0.90","2.51","3.9","2.51","0.90"]
              fef_so2_io = ["0.40","0.40","0.40","0.40","0.40","0.40","0.48","0.40","0.40"]
              fef_oc_io = ["4.71","8.2","4.71","8.2","8.2","8.2","2.62","8.2","8.2"]
              fef_bc_io = ["0.52","0.56","0.52","0.56","0.56","0.56","0.37","0.56","0.56"]
              ccleaf_min_io = ["0.8","0.8","0.8","0.8","0.8","0.8","0.8","0.8","0.8"]
              ccleaf_max_io = ["1.0","1.0","1.0","1.0","1.0","1.0","1.0","1.0","1.0"]
              ccwood_min_io = ["0.0","0.0","0.0","0.0","0.0","0.0","0.0","0.0","0.0"]
              ccwood_max_io = ["0.4","0.4","0.4","0.4","0.4","0.4","0.4","0.4","0.4"]
              avg_ba_io = ["0.6E6","0.6E6","0.6E6","0.6E6","0.6E6","1.4E6","1.4E6","1.2E6","1.2E6"]
          elif ncpft == 4:
              fef_co2_io = ["1631", "1576", "1576", "1654", "1576","1576","1576","1654","1576"]
              fef_co_io = ["100", "106", "106", "64", "106","106","106","64","106"]
              fef_ch4_io = ["6.8", "4.8", "4.8", "2.4", "4.8","4.8","4.8","2.4","4.8"]
              fef_nox_io = ["2.55", "3.24", "3.24", "2.49", "3.24","3.24","3.24","2.49","3.24"]
              fef_so2_io = ["0.40", "0.40", "0.40", "0.48", "0.40","0.40","0.40","0.48","0.40"]
              fef_oc_io = ["4.3", "9.1", "9.1", "3.2", "9.1","9.1","9.1","3.2","9.1"]
              fef_bc_io = ["0.56", "0.56", "0.56", "0.47", "0.56","0.56","0.56","0.47","0.56"]
              ccleaf_min_io = ["0.8","0.8","0.8","0.8","0.8","0.8","0.8","0.8","0.8"]
              ccleaf_max_io = ["1.0","1.0","1.0","1.0","1.0","1.0","1.0","1.0","1.0"]
              ccwood_min_io = ["0.0","0.0","0.0","0.0","0.0","0.0","0.0","0.0","0.0"]
              ccwood_max_io = ["0.4","0.4","0.4","0.4","0.4","0.4","0.4","0.4","0.4"]
              avg_ba_io = ["0.6E6","0.6E6","1.4E6","1.4E6","1.2E6","1.4E6","1.4E6","1.4E6","1.4E6"]

       # Now attribute these to jules_pftpar according to npft
       self.remove_setting(config, ["namelist:jules_pftparm", "fef_co2_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "fef_co2_io"], ", ".join(fef_co2_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "fef_co_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "fef_co_io"], ", ".join(fef_co_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "fef_ch4_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "fef_ch4_io"], ", ".join(fef_ch4_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "fef_nox_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "fef_nox_io"], ", ".join(fef_nox_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "fef_so2_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "fef_so2_io"], ", ".join(fef_so2_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "fef_oc_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "fef_oc_io"], ", ".join(fef_oc_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "fef_bc_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "fef_bc_io"], ", ".join(fef_bc_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "ccleaf_min_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "ccleaf_min_io"], ", ".join(ccleaf_min_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "ccleaf_max_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "ccleaf_max_io"], ", ".join(ccleaf_max_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "ccwood_min_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "ccwood_min_io"], ", ".join(ccwood_min_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "ccwood_max_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "ccwood_max_io"], ", ".join(ccwood_max_io[:npft]))

       self.remove_setting(config, ["namelist:jules_pftparm", "avg_ba_io"])
       self.add_setting(config, ["namelist:jules_pftparm", "avg_ba_io"], ", ".join(avg_ba_io[:npft]))

       return config, self.reports

class vn45_t132(MacroUpgrade):
    """Upgrade macro for JULES #132 - Anna Harper """

    BEFORE_TAG = "vn4.4_t136"
    AFTER_TAG = "vn4.4_t132"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # ADD the trait parameters to pft_params namelist
        self.add_setting(config, ["namelist:jules_pftparm", "nsw_io"],"0.0072,0.0083,0.01604,0.0202,0.0072")
        self.add_setting(config, ["namelist:jules_pftparm", "nr_io"], "0.01726,0.00784,0.0162,0.0084,0.01726")
        self.add_setting(config, ["namelist:jules_pftparm", "hw_sw_io"], "5*0.5")
        self.add_setting(config, ["namelist:jules_triffid", "retran_l_io"], "5*0.5")
        self.add_setting(config, ["namelist:jules_triffid", "retran_r_io"], "5*0.2")

        # IF there are 9 PFTs, need to change these parameters
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        source = self.get_setting_value(config, ["namelist:jules_surface_types","ncpft"])
        if source:
            ncpft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "ncpft"]))

        if npft==9:
            if ncpft==4:
                #The last 4 are crops
                self.change_setting_value(config, ["namelist:jules_pftparm", "nsw_io"], "0.0072,0.0083,0.01604,0.0202,0.0072,-1,-1,-1,-1")
                self.change_setting_value(config, ["namelist:jules_pftparm", "nr_io"], "0.01726,0.00784,0.0162,0.0084,0.01726,-1,-1,-1,-1")
                self.change_setting_value(config, ["namelist:jules_pftparm", "hw_sw_io"], "9*0.5")
                self.change_setting_value(config, ["namelist:jules_triffid", "retran_l_io"], "9*0.5")
                self.change_setting_value(config, ["namelist:jules_triffid", "retran_r_io"], "9*0.2")
            elif ncpft==0:
                #All natural vegetation PFTs
                self.change_setting_value(config, ["namelist:jules_pftparm", "nsw_io"],"0.0072,0.0072,0.0072,0.0083,0.0083,0.01604,0.0202,0.0072,0.0072")
                self.change_setting_value(config, ["namelist:jules_pftparm", "nr_io"], "0.01726,0.01726,0.01726,0.00784,0.00784,0.0162,0.0084,0.01726,0.01726")
                self.change_setting_value(config, ["namelist:jules_pftparm", "hw_sw_io"], "9*0.5")
                self.change_setting_value(config, ["namelist:jules_triffid", "retran_l_io"], "9*0.5")
                self.change_setting_value(config, ["namelist:jules_triffid", "retran_r_io"], "9*0.2")
        return config, self.reports


class vn44_t158(MacroUpgrade):

    """Upgrade macro from JULES by Eddy Robertson"""

    BEFORE_TAG = "vn4.4_t132"
    AFTER_TAG = "vn4.4_t158"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config,
            ["namelist:jules_vegetation", "l_trif_crop"],".false.")
        self.add_setting(config,
            ["namelist:jules_agric", "zero_past"],".true.")
        return config, self.reports

class vn44_t173(MacroUpgrade):

    """Upgrade macro for ticket 173 by Andy Wiltshire."""

    BEFORE_TAG = "vn4.4_t158"
    AFTER_TAG = "vn4.4_t173"

    def upgrade(self, config, meta_config=None):

        self.add_setting(config, ["namelist:jules_vegetation", "l_soil_resp_lev2"], ".false.")
        return config, self.reports

class vn44_vn45(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.4_t173"
    AFTER_TAG = "vn4.5"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
