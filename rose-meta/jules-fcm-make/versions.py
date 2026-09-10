import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn40_vn41(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.0"
    AFTER_TAG = "vn4.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn41_vn42(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.1"
    AFTER_TAG = "vn4.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn42_t58(MacroUpgrade):
    """FCM make upgrade macro for JULES #58 by Matt Pryor"""

    BEFORE_TAG = "vn4.2"
    AFTER_TAG = "vn4.2_t58"

    def upgrade(self, config, meta_config=None):
        # Add the new environment variables
        self.add_setting(config, ["env", "JULES_PLATFORM"], "custom")
        self.add_setting(config, ["env", "JULES_REMOTE"], "local")
        self.add_setting(config, ["env", "JULES_REMOTE_HOST"], "localhost")
        self.add_setting(config, ["env", "JULES_OMP"], "noomp")

        # Replacement for JULES_PARALLEL
        parallel = self.get_setting_value(config, ["env", "JULES_PARALLEL"])
        self.remove_setting(config, ["env", "JULES_PARALLEL"])
        if parallel and parallel == "serial":
            self.add_setting(config, ["env", "JULES_MPI"], "nompi")
        elif parallel and parallel == "mpi":
            self.add_setting(config, ["env", "JULES_MPI"], "mpi")
        elif parallel is not None:
            # This captures the possibility that JULES_PARALLEL
            # might be set to take the value of another environment variable
            self.add_setting(config, ["env", "JULES_MPI"], parallel)
        else:
            self.add_setting(config, ["env", "JULES_MPI"], "nompi")

        # Replacement for JULES_NETCDF
        netcdf = self.get_setting_value(config, ["env", "JULES_NETCDF"])
        self.remove_setting(config, ["env", "JULES_NETCDF"])
        if netcdf and netcdf == "actual":
            self.add_setting(config, ["env", "JULES_NETCDF"], "netcdf")
        elif netcdf and netcdf == "dummy":
            self.add_setting(config, ["env", "JULES_NETCDF"], "nonetcdf")
        elif netcdf is not None:
            # This captures the possibility that JULES_NETCDF might
            # be set to take the value of another environment variable
            self.add_setting(config, ["env", "JULES_NETCDF"], netcdf)
        else:
            self.add_setting(config, ["env", "JULES_NETCDF"], "nonetcdf")

        return config, self.reports


class vn42_vn43(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.2_t58"
    AFTER_TAG = "vn4.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn43_vn44(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.3"
    AFTER_TAG = "vn4.4"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn44_vn45(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.4"
    AFTER_TAG = "vn4.5"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn45_vn46(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.5"
    AFTER_TAG = "vn4.6"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn46_vn47(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.6"
    AFTER_TAG = "vn4.7"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn47_vn48(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.7"
    AFTER_TAG = "vn4.8"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn48_vn49(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.8"
    AFTER_TAG = "vn4.9"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn49_vn50(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.9"
    AFTER_TAG = "vn5.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn50_t632(MacroUpgrade):
    """Upgrade macro for #632 by Paul Cresswell"""

    BEFORE_TAG = "vn5.0"
    AFTER_TAG = "vn5.0_t632"

    def upgrade(self, config, meta_config=None):
        # Add the new compulsory variables
        self.add_setting(config, ["env", "JULES_FFLAGS_EXTRA"], "")
        self.add_setting(config, ["env", "JULES_LDFLAGS_EXTRA"], "")

        return config, self.reports


class vn50_vn51(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.0_t632"
    AFTER_TAG = "vn5.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn51_t735(MacroUpgrade):
    """Upgrade macro for #735 by Carolina Duran Rojas"""

    BEFORE_TAG = "vn5.1"
    AFTER_TAG = "vn5.1_t735"

    def upgrade(self, config, meta_config=None):
        platform = self.get_setting_value(config, ["env", "JULES_PLATFORM"])
        if platform == 'uoe':
            self.change_setting_value(config, ["env", "JULES_PLATFORM"],
                                      'uoe-linux-gfortran')
        return config, self.reports


class vn51_t755(MacroUpgrade):
    """Upgrade macro for #755 by Karina Williams"""

    BEFORE_TAG = "vn5.1_t735"
    AFTER_TAG = "vn5.1_t755"

    def upgrade(self, config, meta_config=None):

        platform = self.get_setting_value(config, ["env", "JULES_PLATFORM"])

        if platform == 'meto-xc40-cce':
            self.change_setting_value(config, ["env", "JULES_REMOTE"], "remote")
            self.change_setting_value(config, ["env", "JULES_REMOTE_HOST"], "'xcfl00'")

        return config, self.reports


class vn51_vn52(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.1_t755"
    AFTER_TAG = "vn5.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn52_vn53(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.2"
    AFTER_TAG = "vn5.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn53_t854(MacroUpgrade):
    """Upgrade macro for #854 by Paul Cresswell"""

    BEFORE_TAG = "vn5.3"
    AFTER_TAG = "vn5.3_t854"

    def upgrade(self, config, meta_config=None):
        # Change the default meto-xc40 build to a remote extract
        # (i.e. 'local' to that platform)
        platform = self.get_setting_value(config, ["env", "JULES_PLATFORM"])
        if platform is not None:
            if platform == 'meto-xc40-cce':
                self.change_setting_value(config, ["env", "JULES_REMOTE"], "local")

        return config, self.reports

class vn53_t855(MacroUpgrade):
    """Upgrade macro for #855 by Paul Cresswell"""

    BEFORE_TAG = "vn5.3_t854"
    AFTER_TAG = "vn5.3_t855"

    def upgrade(self, config, meta_config=None):
        # (Re-)Add compulsory=true variables to apps that don't use them
        self.add_setting(config, ['env', 'JULES_BUILD'], '$JULES_BUILD')
        self.add_setting(config, ['env', 'JULES_OMP'], '$JULES_OMP')
        self.add_setting(config, ['env', 'JULES_PLATFORM'], '$JULES_PLATFORM')
        return config, self.reports

class vn53_vn54(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.3_t855"
    AFTER_TAG = "vn5.4"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn54_vn55(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.4"
    AFTER_TAG = "vn5.5"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn55_vn56(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.5"
    AFTER_TAG = "vn5.6"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn56_t976(MacroUpgrade):

    """Upgrade macro from JULES by Toby Marthews"""

    BEFORE_TAG = "vn5.6"
    AFTER_TAG = "vn5.6_t976"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # No changes, but required by metadata changes.
        return config, self.reports


class vn56_vn57(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.6_t976"
    AFTER_TAG = "vn5.7"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn57_vn58(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.7"
    AFTER_TAG = "vn5.8"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn58_vn59(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.8"
    AFTER_TAG = "vn5.9"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn59_vn60(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn5.9"
    AFTER_TAG = "vn6.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn60_vn61(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.0"
    AFTER_TAG = "vn6.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn61_vn62(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.1"
    AFTER_TAG = "vn6.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports


class vn62_vn63(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.2"
    AFTER_TAG = "vn6.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn63_vn70(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn6.3"
    AFTER_TAG = "vn7.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn70_vn71(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.0"
    AFTER_TAG = "vn7.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn71_vn72(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.1"
    AFTER_TAG = "vn7.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn72_vn73(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.2"
    AFTER_TAG = "vn7.3"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn73_vn74(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.3"
    AFTER_TAG = "vn7.4"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn74_vn75(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.4"
    AFTER_TAG = "vn7.5"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn75_vn76(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.5"
    AFTER_TAG = "vn7.6"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn76_vn77(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.6"
    AFTER_TAG = "vn7.7"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn77_vn78(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.7"
    AFTER_TAG = "vn7.8"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn78_vn79(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.8"
    AFTER_TAG = "vn7.9"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn79_vn80(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.9"
    AFTER_TAG = "vn8.0"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn80_vn81(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn8.0"
    AFTER_TAG = "vn8.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn81_vn82(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn8.1"
    AFTER_TAG = "vn8.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports

class vn82_vn82_t138(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn8.2"
    AFTER_TAG = "vn8.2_t138"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
