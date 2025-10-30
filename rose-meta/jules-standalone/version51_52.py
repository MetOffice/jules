import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

class vn51_t694(MacroUpgrade):

    """Upgrade macro from JULES by Danny Eisenberg"""

    BEFORE_TAG = "vn5.1"
    AFTER_TAG = "vn5.1_t694"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add jules_pftparm_cable namelist
        self.remove_setting(config, ["file:pft_params.nml", "source"], "namelist:jules_pftparm")
        self.add_setting(config, ["file:pft_params.nml", "source"], "(namelist:jules_pftparm) (namelist:jules_pftparm_cable)")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "a1gs_io"], "6*9.000000,4.000000,9.000000,9.000000,4.000000,7*9.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "alpha_io"], "6*0.200000,0.050000,0.200000,0.200000,0.050000,7*0.200000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "canst1_io"], "17*0.100000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "cfrd_io"], "6*0.015000,0.025000,0.015000,0.015000,0.025000,7*0.015000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "clitt_io"], "20.000000,6.000000,10.000000,13.000000,2.000000,2.000000,0.300000,0.300000,0.000000,0.000000,2.000000,2.000000,5*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "conkc0_io"], "17*0.000302")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "conko0_io"], "17*0.256000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "convex_io"], "6*0.010000,0.800000,0.010000,0.010000,0.800000,7*0.010000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "cplant1_io"], "200.000000,300.000000,200.000000,300.000000,159.000000,250.000000,250.000000,250.000000,150.000000,150.000000,250.000000,1.000000,0.100000,0.000000,1.000000,1.000000,0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "cplant2_io"], "10217.000000,16833.000000,5967.000000,12000.000000,5000.000000,12*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "cplant3_io"], "876.000000,1443.000000,511.000000,1029.000000,500.000000,500.000000,500.000000,500.000000,607.000000,607.000000,500.000000,1.000000,0.100000,0.000000,1.000000,1.000000,0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "csoil1_io"], "184.000000,303.000000,107.000000,216.000000,100.000000,275.000000,275.000000,275.000000,149.000000,149.000000,275.000000,1.000000,0.100000,1.000000,1.000000,1.000000,1.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "csoil2_io"], "367.000000,606.000000,214.000000,432.000000,250.000000,314.000000,314.000000,314.000000,300.000000,300.000000,314.000000,1.000000,0.100000,1.000000,1.000000,1.000000,1.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "d0gs_io"], "17*1500.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "ejmax_io"], "17*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "ekc_io"], "17*59430.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "eko_io"], "17*36000.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "extkn_io"], "17*0.001000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "frac4_io"], "6*0.000000,1.000000,0.000000,0.000000,1.000000,7*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "froot1_io"], "0.050000,0.200000,0.200000,0.200000,0.200000,0.150000,11*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "froot2_io"], "0.050000,0.200000,0.200000,0.200000,0.200000,0.150000,11*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "froot3_io"], "0.050000,0.200000,0.200000,0.200000,0.200000,0.150000,11*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "froot4_io"], "0.050000,0.200000,0.200000,0.200000,0.200000,0.150000,11*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "froot5_io"], "0.050000,0.200000,0.200000,0.200000,0.200000,0.150000,11*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "froot6_io"], "0.050000,0.200000,0.200000,0.200000,0.200000,0.150000,11*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "g0_io"], "17*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "g1_io"], "2.346064,4.114762,2.346064,4.447321,4.694803,5.248500,1.616178,2.222156,5.789377,1.616178,5.248500,5.248500,0.000000,5.248500,5.248500,5.248500,5.248500")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "gswmin_io"], "6*0.010000,0.040000,0.010000,0.010000,0.040000,7*0.010000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "hc_io"], "17.000000,35.000000,15.500000,20.000000,0.600000,0.567000,0.567000,0.567000,0.550000,0.550000,0.567000,0.200000,6.017000,0.200000,0.200000,0.200000,0.200000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "length_io"], "0.055000,0.100000,0.040000,0.150000,0.100000,6*0.300000,0.030000,0.242000,0.030000,0.030000,0.030000,0.030000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "ratecp1_io"], "17*1.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "ratecp2_io"], "17*0.030000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "ratecp3_io"], "17*0.140000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "ratecs1_io"], "17*2.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "ratecs2_io"], "17*0.500000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "refl1_io"], "0.062000,0.076000,0.056000,0.092000,0.100000,0.110000,0.100000,0.117000,0.100000,0.090000,0.108000,0.055000,0.091000,0.238000,0.143000,0.143000,0.159000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "refl2_io"], "0.302000,0.350000,0.275000,0.380000,0.400000,0.470000,0.400000,0.343000,0.400000,0.360000,0.343000,0.190000,0.310000,0.457000,0.275000,0.275000,0.305000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "refl3_io"], "17*0.010000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "rootbeta_io"], "0.943000,0.962000,0.966000,0.961000,0.964000,0.943000,0.943000,0.943000,0.961000,0.961000,0.943000,0.975000,5*0.961000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "rp20_io"], "3.000000,0.600000,3.000000,2.200000,1.000000,1.500000,2.800000,2.500000,1.500000,1.000000,1.500000,6*1.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "rpcoef_io"], "17*0.083200")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "rs20_io"], "11*1.000000,0.000000,1.000000,0.000000,0.000000,0.000000,0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "shelrb_io"], "17*2.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "taul1_io"], "0.050000,0.050000,0.045000,0.050000,0.050000,0.070000,0.100000,0.080000,0.100000,0.090000,0.075000,0.023000,0.059000,0.039000,0.023000,0.023000,0.026000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "taul2_io"], "0.100000,0.250000,0.144000,0.250000,0.240000,0.250000,0.150000,0.124000,0.150000,0.225000,0.146000,0.198000,0.163000,0.189000,0.113000,0.113000,0.113000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "taul3_io"], "17*0.010000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "tmaxvj_io"], "10.000000,10.000000,10.000000,15.000000,13*10.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "tminvj_io"], "15.000000,15.000000,5.000000,5.000000,13*15.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "vbeta_io"], "2.000000,2.000000,2.000000,2.000000,4.000000,4.000000,4.000000,4.000000,2.000000,2.000000,4.000000,4.000000,2.000000,4.000000,4.000000,4.000000,4.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "vcmax_io"], "0.000040,0.000055,0.000040,0.000060,0.000040,0.000060,0.000010,0.000040,0.000080,0.000080,0.000060,0.000017,0.000001,0.000017,0.000017,0.000017,0.000017")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "vegcf_io"], "9.000000,14.000000,9.000000,8.000000,5.000000,7.000000,7.000000,5.000000,7.000000,1.000000,7.000000,6*1.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "wai_io"], "1.000000,1.000000,1.000000,1.000000,13*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "width_io"], "0.001000,0.050000,0.001000,0.080000,0.005000,6*0.010000,0.003000,0.015000,0.001000,0.001000,0.001000,0.001000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "xalbnir_io"], "17*1.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "xfang_io"], "0.010000,0.100000,0.010000,0.250000,0.010000,6*0.300000,0.100000,5*0.000000")
        self.add_setting(config, ["namelist:jules_pftparm_cable", "zr_io"], "1.800000,3.000000,2.000000,2.000000,2.500000,5*0.500000,1.800000,3.100000,3.000000,1.000000,1.000000,1.000000,1.000000")

        # Add settings
        return config, self.reports


class vn51_t570(MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn5.1_t694"
    AFTER_TAG = "vn5.1_t570"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Remove a setting.
        self.remove_setting(config, ["namelist:jules_soil_ecosse", "decomp_iter_max"])
        return config, self.reports


class vn51_t687(MacroUpgrade):

    """Upgrade macro from JULES by Douglas Clark"""

    BEFORE_TAG = "vn5.1_t570"
    AFTER_TAG = "vn5.1_t687"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Change "clay_frac" to "clay".
        setting = self.get_setting_value(config, ["namelist:jules_soil_props","var"])
        if setting:
            self.change_setting_value(config, ["namelist:jules_soil_props", "var"],
                                              setting.replace("clay_frac","clay"))

        # Add settings
        return config, self.reports


class vn51_t706(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.1_t687"
    AFTER_TAG = "vn5.1_t706"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add diff_frac_const_imogen to apps. If diff_frac_const already exists
        # then use this value to preserve behaviour.
        diff_frac_const = self.get_setting_value(config,
                                                 ["namelist:jules_drive",
                                                  "diff_frac_const"])
        if diff_frac_const is None:
            self.add_setting(config, ["namelist:imogen_anlg_vals_list",
                                      "diff_frac_const_imogen"], "0.4")
        else:
            self.add_setting(config, ["namelist:imogen_anlg_vals_list",
                                      "diff_frac_const_imogen"],
                             diff_frac_const)

        # diff_frac_const is now compulsory so add with a sensible value if it
        # wasn't being used before (i.e. diff_rad provided as a driving
        # variable). If it was being used before, then set it to the code
        # default.
        var = self.get_setting_value(config, ["namelist:jules_drive",
                                              "var"])
        var=var.split(',')
        if "'diff_rad'" not in var:
            self.add_setting(config, ["namelist:jules_drive",
                                      "diff_frac_const"], "0.0")
        else:
            self.add_setting(config, ["namelist:jules_drive",
                                      "diff_frac_const"], "0.4")

        return config, self.reports


class vn51_t430(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn5.1_t706"
    AFTER_TAG = "vn5.1_t430"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.rename_setting(config, ["namelist:imogen_run_list", "land_feed"], ["namelist:imogen_run_list", "land_feed_co2"] )
        return config, self.reports


class vn51_t748(MacroUpgrade):

    """Upgrade macro from JULES by Danny Eisenberg"""

    BEFORE_TAG = "vn5.1_t430"
    AFTER_TAG = "vn5.1_t748"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add jules_nvegparm_cable namelist
        self.remove_setting(config, ["file:nveg_params.nml", "source"], "namelist:jules_nvegparm")
        self.add_setting(config, ["file:nveg_params.nml", "source"], "(namelist:jules_nvegparm) (namelist:jules_nvegparm_cable)")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "silt_io"], "0.08,0.33,0.17,0.2,0.06,0.25,0.15,0.7,0.33")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "clay_io"], "0.09,0.3,0.67,0.2,0.42,0.48,0.27,0.17,0.3")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "sand_io"], "0.83,0.37,0.16,0.6,0.52,0.27,0.58,0.13,0.37")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "swilt_io"], "0.072,0.216,0.286,0.135,0.219,0.283,0.175,0.395,0.216")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "sfc_io"], "0.143,0.301,0.367,0.218,0.31,0.37,0.255,0.45,0.301")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "ssat_io"], "0.398,0.479,0.482,0.443,0.426,0.482,0.42,0.451,0.479")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "bch_io"], "4.2,7.1,11.4,5.15,10.4,10.4,7.12,5.83,7.1")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "hyds_io"], "0.000166,0.000004,0.000001,0.000021,0.000002,0.000001,0.000006,0.0008,0.000001")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "sucs_io"], "-0.106,-0.591,-0.405,-0.348,-0.153,-0.49,-0.299,-0.356,-0.153")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "rhosoil_io"], "1600,1600,1381,1373,1476,1521,1373,1537,910")
        self.add_setting(config, ["namelist:jules_nvegparm_cable", "css_io"], "850,850,850,850,850,850,850,1920,2100")

        return config, self.reports


class vn51_t754(MacroUpgrade):

    """Upgrade macro from JULES by Hiroshi Kusabiraki"""

    BEFORE_TAG = "vn5.1_t748"
    AFTER_TAG = "vn5.1_t754"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))

        # Add the new compulsory namelist items.
        self.add_setting(config, ["namelist:jules_vegetation", "l_vegdrag_pft"], ','.join(['.false.']*npft))
        self.add_setting(config, ["namelist:jules_vegetation", "l_rsl_scalar"], ".false.")
        self.add_setting(config, ["namelist:jules_vegetation", "cd_leaf"], "0.25")
        self.add_setting(config, ["namelist:jules_vegetation", "c1_usuh"], "0.32")
        self.add_setting(config, ["namelist:jules_vegetation", "c2_usuh"], "0.264")
        self.add_setting(config, ["namelist:jules_vegetation", "c3_usuh"], "8.0")
        self.add_setting(config, ["namelist:jules_vegetation", "stanton_leaf"], "0.3")

        # Add settings
        return config, self.reports

class vn51_t319(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn5.1_t754"
    AFTER_TAG = "vn5.1_t319"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        # Before this ticket:
        # 1. l_urban2t was an internal switch set to true if urban_roof > 0.
        # 2. Either urban or urban_canyon could be the other two-tile surface
        # After this ticket:
        # 1. l_urban2t has been changed to a namelist item
        # 2. urban_canyon and urban_roof need to be present
        # 3. urban not allowed
        # This is consistent with the UM.
        urban_roof = self.get_setting_value(config,
                                            ["namelist:jules_surface_types",
                                             "urban_roof"])
        # Set appropriate values for urban, urban_canyon and urban_roof and
        # l_urban2t switch depending on presence of urban_roof.
        if not urban_roof:
            # NOT two-tile urban scheme.
            # Set compulsory two-tile urban surface types to out of range value
            # to alert user to change the value to something sensible if turned
            # on. These are trigger ignored.
            l_urban2t    = '.false.'
            urban        = self.get_setting_value(config,
                                               ["namelist:jules_surface_types",
                                                "urban"])
            # It would be unusual for urban not to exist, but it's not
            # incorrect.
            if not urban: urban = -1
            urban_canyon = 0
            urban_roof   = 0

        if urban_roof > 0:
            # Two-tile urban scheme.
            l_urban2t    = '.true.'
            urban_canyon = self.get_setting_value(config,
                                               ["namelist:jules_surface_types",
                                                "urban_canyon"])
            if not urban_canyon:
                # If urban_canyon not present then urban should be present.
                urban = self.get_setting_value(config,
                                               ["namelist:jules_surface_types",
                                                "urban"])
                urban_canyon = urban
                # Remove for now to ensure it is added with the correct value.
                self.remove_setting(config, ["namelist:jules_surface_types",
                                             "urban"])
            # Set urban surface types to out of range value to alert user to
            # change the value to something sensible if required.
            urban = 0


        # Add urban, urban_canyon and urban_roof and l_urban2t switch.
        self.add_setting(config, ["namelist:jules_surface", "l_urban2t"],
                         l_urban2t)
        self.add_setting(config, ["namelist:jules_surface_types", "urban"],
                         str(urban))
        self.add_setting(config, ["namelist:jules_surface_types",
                                  "urban_canyon"],
                         str(urban_canyon))
        self.add_setting(config, ["namelist:jules_surface_types",
                                  "urban_roof"],
                         str(urban_roof))

        if l_urban2t == '.false.':
            # Make sure that all the MORUSES switches are also false.
            self.change_setting_value(config, ["namelist:jules_urban_switches",
                                               "l_moruses_albedo"],
                                               ".false.")
            self.change_setting_value(config, ["namelist:jules_urban_switches",
                                               "l_moruses_emissivity"],
                                               ".false.")
            self.change_setting_value(config, ["namelist:jules_urban_switches",
                                               "l_moruses_macdonald"],
                                               ".false.")
            self.change_setting_value(config, ["namelist:jules_urban_switches",
                                               "l_moruses_rough"],
                                               ".false.")
            self.change_setting_value(config, ["namelist:jules_urban_switches",
                                               "l_moruses_storage"],
                                               ".false.")
            self.change_setting_value(config, ["namelist:jules_urban_switches",
                                               "l_moruses_storage_thin"],
                                               ".false.")
            self.change_setting_value(config, ["namelist:jules_urban_switches",
                                               "l_urban_empirical"],
                                               ".false.")

        # Remove l_moruses as it is now an internal umbrella switch
        self.remove_setting(config, ["namelist:jules_urban_switches",
                                     "l_moruses"])

        return config, self.reports


class vn51_vn52(MacroUpgrade):

    """Version bump macro"""

    BEFORE_TAG = "vn5.1_t319"
    AFTER_TAG = "vn5.2"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports


'''
Do not edit any lines below this point - this is the template.
'''


class vn51_txxx(MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn5.1_txxx"
    AFTER_TAG = "vn5.1_txxx"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports
