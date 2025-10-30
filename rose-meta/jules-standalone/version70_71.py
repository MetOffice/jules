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

class vn70_t1327(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn7.0"
    AFTER_TAG = "vn7.0_t1327"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        self.add_setting(config, ["namelist:jules_time", "l_local_solar_time"], ".false.")

        # Add settings
        return config, self.reports


class vn70_t1255(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn7.0_t1327"
    AFTER_TAG = "vn7.0_t1255"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        #Bump metadata tag to pick up change to metadata stucture.

        return config, self.reports


class vn70_t931(MacroUpgrade):

    """Upgrade macro from JULES by Garry Hayman"""

    BEFORE_TAG = "vn7.0_t1255"
    AFTER_TAG = "vn7.0_t931"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Missing data for REALS
        rmdi = str(-2**30)

        # Add settings
        # jules_deposition
        self.add_setting(config, ["namelist:jules_deposition", "l_deposition_gc_corr"], ".false.")
        self.add_setting(config, ["namelist:jules_deposition", "l_deposition_from_ukca"], ".false.")
        self.add_setting(config, ["namelist:jules_deposition", "l_ukca_ddepo3_ocean"], ".false.")
        self.add_setting(config, ["namelist:jules_deposition", "l_ukca_dry_dep_so2wet"], ".false.")
        self.add_setting(config, ["namelist:jules_deposition", "l_ukca_emsdrvn_ch4"], ".false.")

        # Retain existing namelist entries if l_deposition is true
        # Use self.add_setting as this make no change if the entry already exists,
        # whereas self.change_setting_value will make a change
        l_deposition = self.get_setting_value(config, ["namelist:jules_deposition","l_deposition"])

        if l_deposition.lower() == ".true.":
            self.add_setting(config, ["namelist:jules_deposition", "tundra_s_limit"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition", "dzl_const"], rmdi)
        else:
            self.change_setting_value(config, ["namelist:jules_deposition", "tundra_s_limit"], rmdi)
            self.change_setting_value(config, ["namelist:jules_deposition", "dzl_const"], rmdi)

        # jules_temp_fixes
        self.add_setting(config, ["namelist:jules_temp_fixes", "l_fix_drydep_so2_water"], ".false.")
        self.add_setting(config, ["namelist:jules_temp_fixes", "l_fix_improve_drydep"], ".false.")
        self.add_setting(config, ["namelist:jules_temp_fixes", "l_fix_ukca_h2dd_x"], ".false.")

        # Namelists for deposition species:
        # (a) jules_deposition_species: existing, for deposition parameters common to more than one deposited species
        # (b) jules_deposition_species_specific: new, for deposition parameters specific to one deposited species

        # The number of jules_deposition_species namelists and the variable values depend on
        # (a) the UKCA chemical mechanism, (b) the surface tile configuration and (c) deposition switches

        source = self.get_setting_value(config, ["file:jules_deposition.nml","source"])
        if source and not "jules_deposition_species_specific" in source:
            self.change_setting_value(config, ["file:jules_deposition.nml","source"],
                source.replace("namelist:jules_deposition (namelist:jules_deposition_species(:))",
                "namelist:jules_deposition (namelist:jules_deposition_species(:)) (namelist:jules_deposition_species_specific)"))

        # jules_deposition_species and jules_deposition_species_specific
        npft = int(self.get_setting_value(config, ["namelist:jules_surface_types","npft"]))
        nnvg = int(self.get_setting_value(config, ["namelist:jules_surface_types","nnvg"]))
        ndry_dep_species = int(self.get_setting_value(config, ["namelist:jules_deposition","ndry_dep_species"]))

        # jules_deposition_species: existing namelist, amend/delete entries
        # Retain existing namelist entries if l_deposition is true
        # for use on new jules_deposition_species_specific namelist
        # Use self.add_setting as this make no change if the entry already exists,
        # whereas self.change_setting_value will make a change

        self.add_setting(config, ["namelist:jules_deposition_species(1)", "dep_species_rmm_io"], rmdi)
        if l_deposition.lower() == ".true.":
            # Existing deposition parameters for CH4
            ch4_scaling_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "ch4_scaling_io"])
            ch4dd_tundra_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "ch4dd_tundra_io"])
            ch4_up_flux_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "ch4_up_flux_io"])
            # Existing deposition parameters for O3
            cuticle_o3_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "cuticle_o3_io"])
            r_wet_soil_o3_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "r_wet_soil_o3_io"])
            # Existing deposition parameters for H2
            h2dd_c_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "h2dd_c_io"])
            h2dd_m_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "h2dd_m_io"])
            h2dd_q_io = self.get_setting_value(config, ["namelist:jules_deposition_species(1)", "h2dd_q_io"])

            self.add_setting(config, ["namelist:jules_deposition_species(1)", "dd_ice_coeff_io"], ",".join([rmdi]*3) )
            self.add_setting(config, ["namelist:jules_deposition_species(1)", "dep_species_name_io"], "'unset'")
            self.add_setting(config, ["namelist:jules_deposition_species(1)", "diffusion_coeff_io"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition_species(1)", "diffusion_corr_io"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition_species(1)", "r_tundra_io"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition_species(1)", "rsurf_std_io"], ",".join([rmdi]*(npft+nnvg)) )

        else:
            self.change_setting_value(config, ["namelist:jules_deposition_species(1)", "dd_ice_coeff_io"], ",".join([rmdi]*3) )
            self.change_setting_value(config, ["namelist:jules_deposition_species(1)", "dep_species_name_io"], "'unset'")
            self.change_setting_value(config, ["namelist:jules_deposition_species(1)", "diffusion_coeff_io"], rmdi)
            self.change_setting_value(config, ["namelist:jules_deposition_species(1)", "diffusion_corr_io"], rmdi)
            self.change_setting_value(config, ["namelist:jules_deposition_species(1)", "r_tundra_io"], rmdi)
            self.change_setting_value(config, ["namelist:jules_deposition_species(1)", "rsurf_std_io"], ",".join([rmdi]*(npft+nnvg)) )

        for ispecies in range(ndry_dep_species):
            nml_jules_deposition_species = "namelist:jules_deposition_species("+str(ispecies+1)+")"
            self.remove_setting(config, [nml_jules_deposition_species, "ch4_mml_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "ch4_scaling_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "ch4_up_flux_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "ch4dd_tundra_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "cuticle_o3_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "h2dd_c_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "h2dd_m_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "h2dd_q_io"])
            self.remove_setting(config, [nml_jules_deposition_species, "r_wet_soil_o3_io"])

        # jules_deposition_species_specific
        # Add the namelist.
        self.add_setting(config, ["namelist:jules_deposition_species_specific"])

        # Add namelist entries
        # Use existing namelist entries from jules-deposition_species if l_deposition is true
        # Otherwise, add entries with missing data values
        if l_deposition.lower() == ".true.":
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4_mml_io"], "1.0008e5")
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4_scaling_io"], ch4_scaling_io)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4dd_tundra_io"], ch4dd_tundra_io)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "cuticle_o3_io"], cuticle_o3_io)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "r_wet_soil_o3_io"], r_wet_soil_o3_io)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "h2dd_c_io"], h2dd_c_io)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "h2dd_m_io"], h2dd_m_io)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "h2dd_q_io"], h2dd_q_io)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4_up_flux_io"], ch4_up_flux_io)
        else:
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4_mml_io"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4_scaling_io"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4dd_tundra_io"], ",".join([rmdi]*4) )
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "cuticle_o3_io"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "r_wet_soil_o3_io"], rmdi)
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "h2dd_c_io"], ",".join([rmdi]*(npft+nnvg)) )
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "h2dd_m_io"], ",".join([rmdi]*(npft+nnvg)) )
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "h2dd_q_io"], ",".join([rmdi]*(npft+nnvg)) )
            self.add_setting(config, ["namelist:jules_deposition_species_specific", "ch4_up_flux_io"], ",".join([rmdi]*(npft+nnvg)) )

        # Message to warn user to use appropriate values
        msg = "Deposition parameters initialised to missing data; these parameter values will need to be added manually."
        self.add_report(info=msg, is_warning=True)

        return config, self.reports


class vn70_t1246(MacroUpgrade):

    """Upgrade macro from JULES by Phil Harris"""

    BEFORE_TAG = "vn7.0_t931"
    AFTER_TAG = "vn7.0_t1246"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports


class vn70_t1322(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn7.0_t1246"
    AFTER_TAG = "vn7.0_t1322"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        self.remove_setting(config, ["namelist:imogen_run_list", "file_non_co2"])
        self.remove_setting(config, ["namelist:imogen_run_list", "wgen"])
        self.rename_setting(config, ["namelist:imogen_run_list", "file_non_co2_vals"],
                  ["namelist:imogen_run_list", "file_non_co2_radf"])
        self.rename_setting(config, ["namelist:imogen_run_list", "include_non_co2"],
                  ["namelist:imogen_run_list", "include_non_co2_radf"])
        self.add_setting(config, ["namelist:imogen_run_list", "l_drive_with_global_temps"], ".false.")
        return config, self.reports

class vn70_vn71(MacroUpgrade):
    """Version bump macro"""

    BEFORE_TAG = "vn7.0_t1322"
    AFTER_TAG = "vn7.1"

    def upgrade(self, config, meta_config=None):
        # Nothing to do
        return config, self.reports