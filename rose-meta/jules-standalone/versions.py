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
            # First rectify existing incorrect values.
            # Dust emissions scaling factor for each PFT (vn6.2_t1206)
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
                    # non-standard number for npft: Set all values to missing data
                    RMDI = str(-(2**30))
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
            # SOX (vn7.4_t1491) - Start dictionary here as more than one item
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
            # Define the rest of the jules_pftparm dicitonary
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
            for item, values in jules_pftparm.items():
                config_value = self.get_setting_value(
                    config, ["namelist:jules_pftparm", item]
                )
                jules_pftparm[item] = config_value.split(",")
            self.remove_setting(config, ["namelist:jules_pftparm"])

            # Add unique descriptor used to identify instances of duplicate
            # namelist
            # *** Will need to make this more generic and read
            # jules_surface_types to work out what should be where ***
            jules_pftparm["pft_name_io"] = [
                "'brd_leaf'",
                "'ndl_leaf'",
                "'c3_grass'",
                "'c4_grass'",
                "'shrub'",
            ]

            for i in range(npft):
                pft_name = jules_pftparm["pft_name_io"]
                nml = "namelist:jules_pftparm({})".format(
                    pft_name[i].strip("'")
                )
                for item, value in sorted(jules_pftparm.items()):
                    self.add_setting(config, [nml, item], value[i])

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
