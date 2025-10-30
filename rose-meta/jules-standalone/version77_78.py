import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn77_t1525(MacroUpgrade):
    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.7"
    AFTER_TAG = "vn7.7_t1525"

    def amend_rivers_props(
        self, config, item, current, amend, filenames, amend_file
    ):
        """
        Loop through items in jules_rivers_props to amend and create file
        list for read_list
        """

        if item == "nvars":
            self.change_setting_value(
                config,
                ["namelist:jules_rivers_props", item],
                str(int(current) + int(amend)),
            )
        else:
            updated = ",".join([current, amend])
            self.change_setting_value(
                config,
                ["namelist:jules_rivers_props", item],
                updated,
            )
            if item == "use_file":
                use_file = updated.split(",")
                if any(a == ".true." for a in use_file):
                    self.change_setting_value(
                        config,
                        ["namelist:jules_rivers_props", "read_list"],
                        ".true.",
                    )
                    if len(filenames) == 0:
                        filename = self.get_setting_value(
                            config,
                            ["namelist:jules_rivers_props", "file"],
                        )
                        filenames = self.create_filelist(
                            filenames, current, filename
                        )

                        filenames = self.create_filelist(
                            filenames, amend, amend_file
                        )
                        coordinate_file = self.get_setting_value(
                            config,
                            ["namelist:jules_rivers_props", "coordinate_file"],
                        )
                        if coordinate_file == "''":
                            self.change_setting_value(
                                config,
                                [
                                    "namelist:jules_rivers_props",
                                    "coordinate_file",
                                ],
                                filename,
                            )
                        self.change_setting_value(
                            config,
                            ["namelist:jules_rivers_props", "file"],
                            "''",
                        )

    def create_filelist(self, filenames, use_file, filename):
        """Create file list depending on use_file"""

        use_file = use_file.split(",")
        for l in use_file:
            if l == ".true.":
                filenames.append(filename)
            else:
                filenames.append("''")

        return filenames

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        namelists = [
            "namelist:jules_crop_props",
            "namelist:jules_flake",
            "namelist:jules_pdm",
            "namelist:jules_rivers_props",
            "namelist:jules_soil_props",
            "namelist:jules_top",
            "namelist:jules_vegetation_props",
            "namelist:jules_water_resources_props",
            "namelist:urban_properties",
        ]

        for namelist in namelists:
            self.add_setting(config, [namelist, "read_list"], ".false.")

        # Changes are motivated by jules_rivers_props requirements and as such
        # this namelist needs some reorganisation including some user
        # intervention.
        # If riv_number_file is present 'rivers_outflow_number' will need
        # adding to var, read_list added with true and other namelist items
        # amended. The user will need to create their own file list so make
        # changes that will ensure this has to happen.

        msg = """
        !!!!!    USER INTERVENTION IS REQUIRED: 'read_list=.true.'    !!!!!
        !!!!! Please create a text list of ancillary files and enter  !!!!!
        !!!!! under 'file='. This list should contain a separate line !!!!!
        !!!!! for each of nvars entries. If use_file=.false. for var  !!!!!
        !!!!! the appropriate line can contain ''.                    !!!!!
        !!!!! The ancillary files required in this instance are:      !!!!!
        !!!!! * {0:s}
        """

        filenames = []
        nml = [
            "const_val",
            "nvars",
            "tpl_name",
            "var",
            "var_name",
            "use_file",
        ]

        riv_number_file = self.get_setting_value(
            config, ["namelist:jules_rivers_props", "riv_number_file"]
        )
        self.remove_setting(
            config, ["namelist:jules_rivers_props", "riv_number_file"]
        )

        if riv_number_file and riv_number_file != "''":
            # Amend nml items with:
            amends = [
                # "const_val"
                "0.0",
                # "nvars"
                "1",
                # "tpl_name"
                "''",
                # "var"
                "'rivers_outflow_number'",
                # "var_name"
                "'river_number'",
                # "use_file"
                ".true.",
            ]

            amend_file = riv_number_file

            for item, amend in zip(nml, amends):
                current = self.get_setting_value(
                    config, ["namelist:jules_rivers_props", item]
                )
                self.amend_rivers_props(
                    config, item, current, amend, filenames, amend_file
                )
        else:
            self.add_setting(
                config, ["namelist:jules_rivers_props", "read_list"], ".false."
            )

        # Merge jules_overbank_props with jules_rivers_props
        overbank_model = self.get_setting_value(
                config, ["namelist:jules_overbank", "overbank_model"]
        )
        if overbank_model is None:
            rivers_only = """
        !!!!!      Configuration assumed to be Rivers-standalone      !!!!!
        !!!!! * overbank_model was not found                          !!!!!
        !!!!! If this is in error please check your configuration.    !!!!!
            """
            self.add_report(info=rivers_only, is_warning=True)
        elif int(overbank_model) == 3:
            amend_file = self.get_setting_value(
                config,
                ["namelist:jules_overbank_props", "file"],
            )
            if amend_file is not None:
                for item in nml:
                    current = self.get_setting_value(
                        config, ["namelist:jules_rivers_props", item]
                    )
                    amend = self.get_setting_value(
                        config, ["namelist:jules_overbank_props", item]
                    )
                    self.amend_rivers_props(
                        config, item, current, amend, filenames, amend_file
                    )

        if len(filenames) > 0:
            msg = msg.format("\n        !!!!! * ".join(filenames))
            self.add_report(info=msg, is_warning=True)

        # Remove jules_overbank_props
        self.remove_setting(config, ["namelist:jules_overbank_props"])
        source = self.get_setting_value(
            config, ["file:ancillaries.nml", "source"]
        )
        if source:
            self.change_setting_value(
                config,
                ["file:ancillaries.nml", "source"],
                source.replace(
                    "(namelist:jules_rivers_props) (namelist:jules_overbank_props)",
                    "(namelist:jules_rivers_props)",
                ),
            )

        return config, self.reports


class vn77_t1565(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn7.7_t1525"
    AFTER_TAG = "vn7.7_t1565"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            anom = self.get_setting_value(config, ["namelist:imogen_run_list","anom"])
            if anom.lower() == ".true.":
                self.add_setting(config, ["namelist:imogen_run_list", "l_change_metdata"], ".true.")
            else:
                self.add_setting(config, ["namelist:imogen_run_list", "l_change_metdata"], ".false.")

            self.add_setting(config, ["namelist:imogen_run_list", "change_metdata_method"], "1")

            l_drive_with_global_temps = self.get_setting_value(config, ["namelist:imogen_run_list","l_drive_with_global_temps"])
            if l_drive_with_global_temps.lower() == ".true.":
                self.change_setting_value(config, ["namelist:imogen_run_list", "change_metdata_method"], "3")

            anlg = self.get_setting_value(config, ["namelist:imogen_run_list","anlg"])
            if anlg.lower() == ".false.":
                self.change_setting_value(config, ["namelist:imogen_run_list", "change_metdata_method"], "2")

            self.remove_setting(config, ["namelist:imogen_run_list", "anom"])
            self.remove_setting(config, ["namelist:imogen_run_list", "anlg"])
            self.remove_setting(config, ["namelist:imogen_run_list", "l_drive_with_global_temps"])
            self.add_setting(config, ["namelist:imogen_onoff_switch", "l_daily_metdata_climatol"], ".false.")

        return config, self.reports


class vn77_vn78(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.7_t1565"
    AFTER_TAG = "vn7.8"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports
