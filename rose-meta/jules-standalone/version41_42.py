import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
    from rose.config import ConfigNode
else:
    from metomi.rose.upgrade import MacroUpgrade
    from metomi.rose.config import ConfigNode


class vn41_t2(MacroUpgrade):

    """Upgrade macro for JULES ticket #2 by Matt Pryor"""

    BEFORE_TAG = "vn4.1"
    AFTER_TAG = "vn4.1_t2"

    def upgrade(self, config, meta_config=None):
        """This upgrade macro quotes previously unquoted values in namelists"""

        # These are the variables whose values need quoting
        variables = [
            ['namelist:jules_spinup', 'var'],
            ['namelist:jules_soil_props', 'var'],
            ['namelist:jules_top', 'var'],
            ['namelist:jules_crop_props', 'var'],
            ['namelist:urban_properties', 'var'],
            ['namelist:jules_drive', 'var'],
            ['namelist:jules_drive', 'interp'],
            ['namelist:jules_initial', 'var'],
            # Note that we don't use any (1) or (:) syntax for these, even though they could
            # have multiple instances
            # This is because we use a 'startswith' comparison while walking the config below
            ['namelist:jules_prescribed_dataset', 'interp'],
            ['namelist:jules_output_profile', 'output_type']
        ]

        # In order to handle the situations where we could have multiple instances, we walk the
        # keys in the config object to find keys to replace
        for (key, _) in config.walk():
            # Make sure the key has two elements, even if the second is empty
            key = (key + ['', ''])[:2]
            # See if the key matches any of the keys in variables
            match = False
            for var in variables:
                if key[0].startswith(var[0]) and key[1] == var[1]:
                    match = True
                    break
            # If the key is not a match, move on to the next key
            if not match:
                continue
            # If the key is a match, replace the value with a quoted version
            value = self.get_setting_value(config, key)
            if value:
                # Split the value on commas
                # Remove any spaces and existing quotes from the start and end of the parts
                # Add single quotes to the parts and rejoin
                new_value = ",".join(["'" + v.strip(' \'"') + "'" for v in value.split(',')])
                self.change_setting_value(config, key, new_value)

        return config, self.reports


class vn41_t16(MacroUpgrade):

    """Upgrade macro for JULES ticket #16 by Karina Williams"""

    BEFORE_TAG = "vn4.1_t2"
    AFTER_TAG = "vn4.1_t16"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Just add the new compulsory namelist item with its default value
        self.add_setting(config, ["namelist:jules_model_grid", "force_1d_grid"], ".false.")

        return config, self.reports


class vn41_t22(MacroUpgrade):

    """Upgrade macro for JULES ticket #22 by Matt Pryor"""

    BEFORE_TAG = "vn4.1_t16"
    AFTER_TAG = "vn4.1_t22"

    def upgrade(self, config, meta_config=None):
        """This upgrade macro removes the logging namelist and logging.nml file"""

        self.remove_setting(config, ["namelist:logging"])
        self.remove_setting(config, ["file:logging.nml"])

        return config, self.reports


class vn41_t41(MacroUpgrade):

    """Upgrade macro for JULES ticket #41 by Andy Wiltshire"""

    BEFORE_TAG = "vn4.1_t22"
    AFTER_TAG = "vn4.1_t41"

    def upgrade(self, config, meta_config=None):
        self.add_setting(config, ["namelist:jules_vegetation", "l_stem_resp_fix"], ".FALSE.")

        return config, self.reports


class vn41_t33(MacroUpgrade):

    """Upgrade macro for JULES ticket #33 by Eleanor Burke"""

    BEFORE_TAG = "vn4.1_t41"
    AFTER_TAG = "vn4.1_t33"

    def upgrade(self, config, meta_config=None):
        # Add settings for bedrock

        self.add_setting(config, ["namelist:jules_soil", "l_bedrock"], ".false.")
        self.add_setting(config, ['namelist:jules_soil', "ns_deep"],   "100")
        self.add_setting(config, ['namelist:jules_soil', "hcapdeep"],  "2100000.0")
        self.add_setting(config, ['namelist:jules_soil', "hcondeep"],  "8.6")
        self.add_setting(config, ['namelist:jules_soil', "dzdeep"],    "0.5")

        return config, self.reports


class vn41_t15(MacroUpgrade):

    """Upgrade macro for JULES ticket #15 by Huw Lewis"""

    BEFORE_TAG = "vn4.1_t33"
    AFTER_TAG = "vn4.1_t15"

    def upgrade(self, config, meta_config=None):
        # Add the jules_rivers.nml file
        self.add_setting(config, ["namelist:jules_rivers"])
        self.add_setting(config, ["file:jules_rivers.nml", "source"], "namelist:jules_rivers")

        # Add the l_rivers switch in a false state
        self.add_setting(config, ["namelist:jules_rivers", "l_rivers"], ".false.")

        # Everything else should be added in a "user-ignored" state, since l_rivers is false
        ignored = ConfigNode.STATE_USER_IGNORED

        # Add the rest of the variables from JULES_RIVERS with their default values
        self.add_setting(config, ["namelist:jules_rivers", "rivers_type"], "'rfm'", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "rivers_timestep"], "-32768", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "cland"], "0.2", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "criver"], "0.62", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "cbland"], "0.1", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "cbriver"], "0.15", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "retl"], "0.0", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "retr"], "0.005", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "a_thresh"], "1", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "runoff_factor"], "1.0", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "rivers_speed"], "0.4", state = ignored)
        self.add_setting(config, ["namelist:jules_rivers", "rivers_meander"], "1.4", state = ignored)

        # Add the JULES_RIVERS_PROPS namelist as an empty namelist, forcing people to think about the
        # new compulsory parameters if they do enable river routing
        self.add_setting(config, ["namelist:jules_rivers_props"], state = ignored)
        # Add it to ancillaries.nml as an optional namelist
        source = self.get_setting_value(config, ["file:ancillaries.nml","source"])
        if source:
            self.change_setting_value(config, ["file:ancillaries.nml","source"],
                                              source.replace("namelist:jules_co2",
                                                             "(namelist:jules_rivers_props) namelist:jules_co2"))

        return config, self.reports


class vn41_t5(MacroUpgrade):

    """Upgrade macro for JULES ticket #5 by Richard Gilham"""

    BEFORE_TAG = "vn4.1_t15"
    AFTER_TAG  = "vn4.1_t5"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        #Add the fire.nml file
        self.add_setting(config, ["namelist:fire_switches"])
        self.add_setting(config, ["namelist:fire_switches", "l_fire"], ".false.")
        self.add_setting(config, ["namelist:fire_switches", "mcarthur_flag"], ".false.")
        self.add_setting(config, ["namelist:fire_switches", "mcarthur_opt"], "1")
        self.add_setting(config, ["namelist:fire_switches", "canadian_flag"], ".false.")
        self.add_setting(config, ["namelist:fire_switches", "canadian_hemi_opt"], ".false.")
        self.add_setting(config, ["namelist:fire_switches", "nesterov_flag"], ".false.")

        self.add_setting(config, ["file:fire.nml", "source"], "namelist:fire_switches")

        return config, self.reports


class vn41_t45(MacroUpgrade):

    """Upgrade macro for JULES ticket #45 by Anna Harper"""

    BEFORE_TAG = "vn4.1_t5"
    AFTER_TAG = "vn4.1_t45"

    def upgrade(self, config, meta_config=None):
        """This upgrade macro adds l_gleaf_fix to the jules_vegetation namelist"""

        self.add_setting(config, ['namelist:jules_vegetation', 'l_gleaf_fix'], '.false.')

        return config, self.reports


class vn41_vn42(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn4.1_t45"
    AFTER_TAG = "vn4.2"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
