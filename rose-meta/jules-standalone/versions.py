import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade

from .version34_40 import *
from .version40_41 import *
from .version41_42 import *
from .version42_43 import *
from .version43_44 import *
from .version44_45 import *
from .version45_46 import *
from .version46_47 import *
from .version47_48 import *
from .version48_49 import *
from .version49_50 import *
from .version50_51 import *
from .version51_52 import *
from .version52_53 import *
from .version53_54 import *
from .version54_55 import *
from .version55_56 import *
from .version56_57 import *
from .version57_58 import *
from .version58_59 import *
from .version59_60 import *
from .version60_61 import *
from .version61_62 import *
from .version62_63 import *
from .version63_70 import *
from .version70_71 import *
from .version71_72 import *
from .version72_73 import *
from .version73_74 import *
from .version74_75 import *
from .version75_76 import *
from .version76_77 import *
from .version77_78 import *
from .version78_79 import *
from .version79_80 import *
from .version80_81 import *


class UpgradeError(Exception):

    """Exception created when an upgrade fails."""

    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        sys.tracebacklimit = 0
        return self.msg

    __str__ = __repr__

class vn81_t70(MacroUpgrade):

    """Upgrade macro from JULES by Eleanor Burke"""

    BEFORE_TAG = "vn8.1"
    AFTER_TAG = "vn8.1_t70"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            npft = int(
                self.get_setting_value(
                    config, ["namelist:jules_surface_types", "npft"]
                )
            )
            self.add_setting(
                config,
                ["namelist:jules_pftparm", "pft_name_io"],
                ",".join(["''"] * npft),
            )

        """Add cs_decomp_soil_moist_func to namelist jules_soil_biogeochem"""
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "cs_decomp_soil_moist_func"], "0")
        self.add_setting(config, ["namelist:jules_soil_biogeochem", "fsthsat_cs_decomp_opt1"], "0.2")

        # Add settings
        return config, self.reports


class vn81_t41(MacroUpgrade):

    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn8.1_t70"
    AFTER_TAG = "vn8.1_t41"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""
        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            npft = int(
                self.get_setting_value(
                    config, ["namelist:jules_surface_types", "npft"]
                )
            )
            self.add_setting(
                config,
                ["namelist:jules_pftparm", "pft_name_io"],
                ",".join(["''"] * npft),
            )

        return config, self.reports


class vn81_t115(MacroUpgrade):
    """Upgrade macro from JULES by Maggie Hendry"""

    BEFORE_TAG = "vn8.1_t41"
    AFTER_TAG = "vn8.1_t115"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        lsm_id = int(
            self.get_setting_value(
                config, ["namelist:jules_model_environment", "lsm_id"]
            )
        )
        if lsm_id != 3:
            npft = int(
                self.get_setting_value(
                    config, ["namelist:jules_surface_types", "npft"]
                )
            )
            # First rectify existing incorrect values (there are a lot of them!)
            RMDI = str(-(2**30))
            # INFERNO (l_inferno; vn4.4_t136)
            jules_pftparm = {}
            jules_pftparm["fef_co2_io"] = ""
            jules_pftparm["fef_co_io"] = ""
            jules_pftparm["fef_ch4_io"] = ""
            jules_pftparm["fef_nox_io"] = ""
            jules_pftparm["fef_so2_io"] = ""
            jules_pftparm["fef_oc_io"] = ""
            jules_pftparm["fef_bc_io"] = ""
            jules_pftparm["ccleaf_min_io"] = ""
            jules_pftparm["ccleaf_max_io"] = ""
            jules_pftparm["ccwood_min_io"] = ""
            jules_pftparm["ccwood_max_io"] = ""
            jules_pftparm["avg_ba_io"] = ""
            # Scale albedos of land-surface tiles to agree with observations
            # (l_albedo_obs; no macro)
            jules_pftparm["albsnf_maxl_io"] = ""
            jules_pftparm["albsnf_maxu_io"] = ""
            jules_pftparm["alnirl_io"] = ""
            jules_pftparm["alniru_io"] = ""
            jules_pftparm["alparl_io"] = ""
            jules_pftparm["alparu_io"] = ""
            jules_pftparm["omegal_io"] = ""
            jules_pftparm["omegau_io"] = ""
            jules_pftparm["omnirl_io"] = ""
            jules_pftparm["omniru_io"] = ""
            # Ozone damage for vegetation (l_o3_damage; no macro)
            jules_pftparm["dfp_dcuo_io"] = ""
            jules_pftparm["fl_o3_ct_io"] = ""
            # Explicit vegetation roughness lengths (l_spec_veg_z0; vn5.4_t903)
            # Upgrade macro was robust, but some congfigurations of non-standard
            # PFTs have incorrect incorrect number, so corrected with missing
            # data as per original macro.
            jules_pftparm["z0v_io"] = ""
            for item, values in jules_pftparm.items():
                config_value = self.get_setting_value(
                    config, ["namelist:jules_pftparm", item]
                )
                if len(config_value.split(",")) != npft:
                    self.change_setting_value(
                        config,
                        ["namelist:jules_pftparm", item],
                        ",".join([RMDI] * npft),
                    )
            # Dust emissions scaling factor for each PFT
            # (um-atmos dust_veg_emiss; vn6.2_t1206)
            item = "dust_veg_scj_io"
            config_value = self.get_setting_value(
                config, ["namelist:jules_pftparm", item]
            )
            if len(config_value.split(",")) != npft:
                if npft == 5:
                    # 5 vegetation types
                    self.change_setting_value(
                        config,
                        ["namelist:jules_pftparm", item],
                        "0.0,0.0,1.0,1.0,0.5",
                    )
                elif npft == 9:
                    # 9 vegetation types
                    self.change_setting_value(
                        config,
                        ["namelist:jules_pftparm", item],
                        "0.0,0.0,0.0,0.0,0.0,1.0,1.0,0.5,0.5",
                    )
                elif npft == 10:
                    # 10 vegetation types
                    self.change_setting_value(
                        config,
                        ["namelist:jules_pftparm", item],
                        "0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,0.5,0.5",
                    )
                elif npft == 13:
                    # 13 vegetation types
                    self.change_setting_value(
                        config,
                        ["namelist:jules_pftparm", item],
                        "0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,1.0,0.5,0.5",
                    )
                else:
                    # non-standard number for npft: Set all values to missing
                    # data
                    self.change_setting_value(
                        config,
                        ["namelist:jules_pftparm", item],
                        ",".join([RMDI] * npft),
                    )
                    msg = """
                    Non-standard number of npft, setting dust_veg_scj_io values
                    to missing data
                    """
                    self.add_report(info=msg, is_warning=True)
            # fire_mort_io; original prone to error
            # (l_trif_fire; vn5.3_t872)
            item = "fire_mort_io"
            config_value = self.get_setting_value(
                config, ["namelist:jules_pftparm", item]
            )
            if len(config_value.split(",")) != npft:
                self.change_setting_value(
                    config,
                    ["namelist:jules_pftparm", item],
                    ",".join(['1.0'] * npft),
                    )
            # SOX (stomata_model = 3; vn7.4_t1491)
            jules_pftparm = {}
            jules_pftparm["sox_a_io"] = ""
            jules_pftparm["sox_p50_io"] = ""
            jules_pftparm["sox_rp_min_io"] = ""
            for item, values in jules_pftparm.items():
                config_value = self.get_setting_value(
                    config, ["namelist:jules_pftparm", item]
                )
                if len(config_value.split(",")) != npft:
                    self.change_setting_value(
                        config,
                        ["namelist:jules_pftparm", item],
                        ",".join(["0.0"] * npft),
                    )
            # Can now define the jules_pftparm dicitonary in full since the
            # namelist entries have been corrected.
            jules_pftparm = {}
            jules_pftparm["a_wl_io"] = ""
            jules_pftparm["a_ws_io"] = ""
            jules_pftparm["act_jmax_io"] = ""
            jules_pftparm["act_vcmax_io"] = ""
            jules_pftparm["aef_io"] = ""
            jules_pftparm["albsnc_max_io"] = ""
            jules_pftparm["albsnc_min_io"] = ""
            jules_pftparm["albsnf_max_io"] = ""
            jules_pftparm["albsnf_maxl_io"] = ""
            jules_pftparm["albsnf_maxu_io"] = ""
            jules_pftparm["alnir_io"] = ""
            jules_pftparm["alnirl_io"] = ""
            jules_pftparm["alniru_io"] = ""
            jules_pftparm["alpar_io"] = ""
            jules_pftparm["alparl_io"] = ""
            jules_pftparm["alparu_io"] = ""
            jules_pftparm["alpha_elec_io"] = ""
            jules_pftparm["alpha_io"] = ""
            jules_pftparm["avg_ba_io"] = ""
            jules_pftparm["b_wl_io"] = ""
            jules_pftparm["c3_io"] = ""
            jules_pftparm["can_struct_a_io"] = ""
            jules_pftparm["canht_ft_io"] = ""
            jules_pftparm["catch0_io"] = ""
            jules_pftparm["ccleaf_max_io"] = ""
            jules_pftparm["ccleaf_min_io"] = ""
            jules_pftparm["ccwood_max_io"] = ""
            jules_pftparm["ccwood_min_io"] = ""
            jules_pftparm["ci_st_io"] = ""
            jules_pftparm["dcatch_dlai_io"] = ""
            jules_pftparm["deact_jmax_io"] = ""
            jules_pftparm["deact_vcmax_io"] = ""
            jules_pftparm["dfp_dcuo_io"] = ""
            jules_pftparm["dgl_dm_io"] = ""
            jules_pftparm["dgl_dt_io"] = ""
            jules_pftparm["dqcrit_io"] = ""
            jules_pftparm["ds_jmax_io"] = ""
            jules_pftparm["ds_vcmax_io"] = ""
            jules_pftparm["dust_veg_scj_io"] = ""
            jules_pftparm["dz0v_dh_io"] = ""
            jules_pftparm["emis_pft_io"] = ""
            jules_pftparm["eta_sl_io"] = ""
            jules_pftparm["f0_io"] = ""
            jules_pftparm["fd_io"] = ""
            jules_pftparm["fef_bc_io"] = ""
            jules_pftparm["fef_c2h4_io"] = ""
            jules_pftparm["fef_c2h6_io"] = ""
            jules_pftparm["fef_c3h8_io"] = ""
            jules_pftparm["fef_ch4_io"] = ""
            jules_pftparm["fef_co_io"] = ""
            jules_pftparm["fef_co2_io"] = ""
            jules_pftparm["fef_dms_io"] = ""
            jules_pftparm["fef_hcho_io"] = ""
            jules_pftparm["fef_mecho_io"] = ""
            jules_pftparm["fef_nh3_io"] = ""
            jules_pftparm["fef_nox_io"] = ""
            jules_pftparm["fef_oc_io"] = ""
            jules_pftparm["fef_so2_io"] = ""
            jules_pftparm["fire_mort_io"] = ""
            jules_pftparm["fl_o3_ct_io"] = ""
            jules_pftparm["fsmc_mod_io"] = ""
            jules_pftparm["fsmc_of_io"] = ""
            jules_pftparm["fsmc_p0_io"] = ""
            jules_pftparm["g1_stomata_io"] = ""
            jules_pftparm["g_leaf_0_io"] = ""
            jules_pftparm["glmin_io"] = ""
            jules_pftparm["gpp_st_io"] = ""
            jules_pftparm["gsoil_f_io"] = ""
            jules_pftparm["hw_sw_io"] = ""
            jules_pftparm["ief_io"] = ""
            jules_pftparm["infil_f_io"] = ""
            jules_pftparm["jv25_ratio_io"] = ""
            jules_pftparm["kext_io"] = ""
            jules_pftparm["kn_io"] = ""
            jules_pftparm["knl_io"] = ""
            jules_pftparm["kpar_io"] = ""
            jules_pftparm["lai_alb_lim_io"] = ""
            jules_pftparm["lai_io"] = ""
            jules_pftparm["lma_io"] = ""
            jules_pftparm["mef_io"] = ""
            jules_pftparm["neff_io"] = ""
            jules_pftparm["nl0_io"] = ""
            jules_pftparm["nmass_io"] = ""
            jules_pftparm["nr_io"] = ""
            jules_pftparm["nr_nl_io"] = ""
            jules_pftparm["ns_nl_io"] = ""
            jules_pftparm["nsw_io"] = ""
            jules_pftparm["omega_io"] = ""
            jules_pftparm["omegal_io"] = ""
            jules_pftparm["omegau_io"] = ""
            jules_pftparm["omnir_io"] = ""
            jules_pftparm["omnirl_io"] = ""
            jules_pftparm["omniru_io"] = ""
            jules_pftparm["orient_io"] = ""
            jules_pftparm["psi_close_io"] = ""
            jules_pftparm["psi_open_io"] = ""
            jules_pftparm["q10_leaf_io"] = ""
            jules_pftparm["r_grow_io"] = ""
            jules_pftparm["rootd_ft_io"] = ""
            jules_pftparm["sigl_io"] = ""
            jules_pftparm["sox_a_io"] = ""
            jules_pftparm["sox_p50_io"] = ""
            jules_pftparm["sox_rp_min_io"] = ""
            jules_pftparm["sug_g0_io"] = ""
            jules_pftparm["sug_grec_io"] = ""
            jules_pftparm["sug_yg_io"] = ""
            jules_pftparm["tef_io"] = ""
            jules_pftparm["tleaf_of_io"] = ""
            jules_pftparm["tlow_io"] = ""
            jules_pftparm["tupp_io"] = ""
            jules_pftparm["vint_io"] = ""
            jules_pftparm["vsl_io"] = ""
            jules_pftparm["z0hm_classic_pft_io"] = ""
            jules_pftparm["z0hm_pft_io"] = ""
            jules_pftparm["z0v_io"] = ""
            # Read values into dictionary
            error = 0
            for item, values in jules_pftparm.items():
                config_value = self.get_setting_value(
                    config, ["namelist:jules_pftparm", item]
                )
                jules_pftparm[item] = config_value.split(",")
                if len(jules_pftparm[item]) != npft:
                    if lsm_id == 2:
                        # jules_pftparm is not required by CABLE. As there are
                        # too many incorrect items to correct, pragmatically
                        # set them intead to missing data.
                        jules_pftparm[item] = [RMDI] * npft
                    else:
                        error += 1
                        print(f"ERROR: Length {item} is not npft.")
                if error > 0:
                    raise UpgradeError (
                        f"\n*************************************************" +
                        f"******************************"
                        f"\n{error} jules_pftparm items do not have the " +
                        f"correct length (see previous messages).\nThese "    +
                        f"will need to be corrected before applying macro."
                        f"\n*************************************************" +
                        f"******************************"
                    )
            self.remove_setting(config, ["namelist:jules_pftparm"])

            # Add unique descriptor used to identify instances of duplicate
            # namelist
            pft_name = [None] * npft
            jules_surface_types = {}
            jules_surface_types["brd_leaf"] = ""
            jules_surface_types["brd_leaf_dec"] = ""
            jules_surface_types["brd_leaf_eg_temp"] = ""
            jules_surface_types["brd_leaf_eg_trop"] = ""
            jules_surface_types["c3_crop"] = ""
            jules_surface_types["c3_grass"] = ""
            jules_surface_types["c3_pasture"] = ""
            jules_surface_types["c4_crop"] = ""
            jules_surface_types["c4_grass"] = ""
            jules_surface_types["c4_pasture"] = ""
            jules_surface_types["ndl_leaf"] = ""
            jules_surface_types["ndl_leaf_dec"] = ""
            jules_surface_types["ndl_leaf_eg"] = ""
            jules_surface_types["shrub"] = ""
            jules_surface_types["shrub_dec"] = ""
            jules_surface_types["shrub_eg"] = ""
            jules_surface_types["usr_type"] = ""
            # Read jules_surface_types into dictionary
            for item, values in jules_surface_types.items():
                levels = self.get_setting_value(
                    config, ["namelist:jules_surface_types", item]
                )
                if levels is not None:
                    levels = levels.split(",")
                    for l in range(len(levels)):
                        n = int(levels[l])
                        if n > 0:
                            if n > npft:
                                if item == "usr_type":
                                    # usr_type is also used by non-veg varieties
                                    # so need to prevent going out of bounds
                                    msg = """
    usr_type detected assumed to be non-veg
                                    """
                                    self.add_report(
                                        info=msg, is_warning=True
                                    )
                                else:
                                    raise UpgradeError (
                                        f"{item} is greater than npft"
                                    )
                            else:
                                pft_name[n-1] = item
                                if len(levels) > 1:
                                    if item == "usr_type":
                                        pft_name[n-1] += "#"+str(l+1)
                                    else:
                                        raise UpgradeError (
                                            f"{item} cannot be a list"
                                        )
                                pft_name[n-1] = "'{}'".format(pft_name[n-1])
            if None in pft_name:
                print(f"*************************************************" +
                      f"*************************************************"
                      )
                print(f"* ERROR: Surface type not recognised by macro. "       +
                      f"Please correct this, then reapply macro."
                      )
                print(f"*************************************************" +
                      f"*************************************************"
                      )
            jules_pftparm["pft_name_io"] = pft_name

            for i in range(npft):
                nml = "namelist:jules_pftparm({})".format(
                    pft_name[i].strip("'")
                )
                for item, value in sorted(jules_pftparm.items()):
                    self.add_setting(config, [nml, item], value[i])
                #for item, value in sorted(jules_pftparm.items()):
                #    self.add_setting(config, ["namelist:jules_pftparm", item],
                #                     ", " .join(value))

            # Replace with multiple namelist in file source
            source = self.get_setting_value(
                config, ["file:pft_params.nml", "source"]
            )
            if "namelist:jules_pftparm(:)" not in source:
                source = source.replace(
                    "namelist:jules_pftparm", "namelist:jules_pftparm(:)"
                )
                self.change_setting_value(
                    config, ["file:pft_params.nml", "source"], source
                )

        return config, self.reports
