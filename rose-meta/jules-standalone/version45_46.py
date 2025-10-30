import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade
class vn45_t250(MacroUpgrade):

    """Upgrade macro from JULES by Andy Wiltshire"""

    BEFORE_TAG = "vn4.5"
    AFTER_TAG = "vn4.5_t250"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_surface", "sorp"], "10.0")
        self.add_setting(config, ["namelist:jules_surface", "n_inorg_turnover"], "1.0")
        return config, self.reports


class vn45_t213(MacroUpgrade):

    """Upgrade macro from JULES vn4.5_t250 to vn4.5_t213 by robinsmith"""

    BEFORE_TAG = "vn4.5_t250"
    AFTER_TAG = "vn4.5_t213"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.add_setting(config, ["namelist:jules_surface", "l_elev_land_ice"], ".false.")
        self.add_setting(config, ["namelist:jules_soil", "dzsoil_elev"], "0.")
        self.add_setting(config, ["namelist:jules_surface_types", "elev_ice"], "0")

        return config, self.reports

class vn45_t257(MacroUpgrade):

    """Upgrade macro from JULES by J. M. Edwards"""

    BEFORE_TAG = "vn4.5_t213"
    AFTER_TAG = "vn4.5_t257"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_radiation", "l_niso_direct"], ".false.")
        return config, self.reports

class vn45_t258(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.5_t257"
    AFTER_TAG = "vn4.5_t258"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_pftparm", "fsmc_p0_io"],','.join(['0.0']*npft))
        self.add_setting(config, ["namelist:jules_pftparm", "fsmc_mod_io"],','.join(['0']*npft))

        return config, self.reports


class vn45_t279(MacroUpgrade):

    """Upgrade macro from JULES by Anna Harper"""

    BEFORE_TAG = "vn4.5_t258"
    AFTER_TAG = "vn4.5_t279"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.add_setting(config, ["namelist:jules_vegetation", "l_scale_resp_pm"], ".false.")
        return config, self.reports

class vn45_t268(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.5_t279"
    AFTER_TAG = "vn4.5_t268"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        ncpft_in = self.get_setting_value(config, ["namelist:jules_surface_types", "ncpft"])
        if ncpft_in:
            ncpft = int(ncpft_in)

            self.add_setting(config, ["namelist:jules_cropparm", "mu_io"], ','.join(['0.05'] * ncpft))
            self.add_setting(config, ["namelist:jules_cropparm", "yield_frac_io"], ','.join(['1.0'] * ncpft))
            self.add_setting(config, ["namelist:jules_cropparm", "initial_carbon_io"], ','.join(['0.01'] * ncpft))
            self.add_setting(config, ["namelist:jules_cropparm", "sen_dvi_io"], ','.join(['1.5'] * ncpft))

            t_bse = self.get_setting_value(config, ["namelist:jules_cropparm", "t_bse_io"])
            self.add_setting(config, ["namelist:jules_cropparm", "t_mort_io"], t_bse)

        else:
            self.add_setting(config, ["namelist:jules_cropparm", "mu_io"], "-1e20")
            self.add_setting(config, ["namelist:jules_cropparm", "yield_frac_io"], "-1e20")
            self.add_setting(config, ["namelist:jules_cropparm", "initial_carbon_io"], "-1e20")
            self.add_setting(config, ["namelist:jules_cropparm", "sen_dvi_io"], "-1e20")
            self.add_setting(config, ["namelist:jules_cropparm", "t_mort_io"], "-1e20")

        # Add settings
        return config, self.reports

class vn45_t272(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.5_t268"
    AFTER_TAG = "vn4.5_t272"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        ncpft_in = self.get_setting_value(config, ["namelist:jules_surface_types", "ncpft"])
        if ncpft_in:
            ncpft = int(ncpft_in)

            self.add_setting(config, ["namelist:jules_cropparm", "nu_io"], ','.join(['0.0'] * ncpft))

        else:
            self.add_setting(config, ["namelist:jules_cropparm", "nu_io"], "-1e20")

        # Add settings
        return config, self.reports

class vn45_t256(MacroUpgrade):

    """Upgrade macro from JULES by Karina Williams"""

    BEFORE_TAG = "vn4.5_t272"
    AFTER_TAG = "vn4.5_t256"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]))
        self.add_setting(config, ["namelist:jules_pftparm", "can_struct_a_io"],','.join(['1.0']*npft))

        return config, self.reports

class vn45_t214(MacroUpgrade):

    """Upgrade macro from JULES by robinsmith"""

    BEFORE_TAG = "vn4.5_t256"
    AFTER_TAG = "vn4.5_t214"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        self.add_setting(config, ["namelist:jules_snow", "aicemax"], "0.78, 0.36")
        self.add_setting(config, ["namelist:jules_snow", "rho_firn_albedo"], "550.0")

        # Add settings
        return config, self.reports

class vn45_t275(MacroUpgrade):

    """Upgrade macro from JULES by Anna Harper"""

    BEFORE_TAG = "vn4.5_t214"
    AFTER_TAG = "vn4.5_t275"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types", "npft"]) )
        crm = self.get_setting_value(config, ["namelist:jules_vegetation", "can_rad_mod"])
        kn_string = self.get_setting_value(config, ["namelist:jules_pftparm", "kn_io"])
        kn = kn_string.split(',')

        # Set kn and knl
        if crm == '6':
           # CRM6 uses knl, use previous value of Kn
           self.add_setting(config, ["namelist:jules_pftparm", "knl_io"], kn_string)
           self.change_setting_value(config, ["namelist:jules_pftparm", "kn_io"], ','.join(['0.78'] * npft))
           if any(float(a) > 0.5 for a in kn ):
              kn_warning='''
                    Warning! A smaller value of knl is required with can_rad_mod=6
                    than with previous canopy radiation schemes. Recommended value
                    is 0.20.
                    '''
              self.add_report(info=kn_warning, is_warning=True)
        else:
           # Leave kn as is
           self.add_setting(config, ["namelist:jules_pftparm", "knl_io"], ','.join(['0.20'] * npft))

        # Add l_leaf_n_resp_fix
        self.add_setting(config, ["namelist:jules_vegetation", "l_leaf_n_resp_fix"], ".false.")
        return config, self.reports

class vn45_vn46(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.5_t275"
    AFTER_TAG = "vn4.6"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
