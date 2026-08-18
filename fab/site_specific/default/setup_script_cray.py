#!/usr/bin/env python3

'''
This file contains a function that sets the default flags for the Cray
compilers and linkers in the ToolRepository.

This function gets called from the default site-specific config file
'''

import argparse
from typing import cast

from fab.api import (BuildConfig, Category, ContainFlags, Compiler, Linker,
                     ToolRepository)

from nf_config import NfConfig


def setup_script_cray(build_config: BuildConfig, args: argparse.Namespace):
    # pylint: disable=unused-argument
    '''
    Defines the default flags for ftn.

    :param build_config: the Fab build config instance from which
    required parameters can be taken.
    :type build_config: :py:class:`fab.BuildConfig`
    :param argparse.Namespace args: all command line options
    '''

    tr = ToolRepository()
    ftn = tr.get_tool(Category.FORTRAN_COMPILER, "crayftn-ftn")
    ftn = cast(Compiler, ftn)

    if not ftn.is_available:
        return

    # The base flags
    # ==============
    flags = ['-M E7208,E7212',    # Var used before defined
             '-hlist=ad', '-hfp0', '-hflex_mp=intolerant',
             '-dw', '-ec', '-eI', '-em', '-en']

    ftn.add_flags(flags, "base")

    # File override to cut down compile times - compiler version: cce/12.0.1
    if (12, 0, 1) <= ftn.get_version() < (12, 1):
        # Setting this for all modes
        ftn.add_flags(ContainFlags("/model_interface_mod.",
                                   ['-O0', '-Ovector0', '-hfp0',
                                    '-hflex_mp=strict', '-hipa0']),
                      "base")
        ftn.add_flags(ContainFlags("/init_prescribed_data.", ['-O1']),
                      "base")

    # Debug
    # =====
    ftn.add_flags(["-g"], "debug")

    # Normal
    # ======

    # Production
    # ==========
    ftn.add_flags(["-O3"], "production")

    # Set up the linker
    # =================
    linker = tr.get_tool(Category.LINKER, "linker-crayftn-ftn")
    linker = cast(Linker, linker)

    # As default, use nf-config to set NetCDF linker flags. If it's not
    # available (or not working properly), the site-specific setup must
    # add netcdf definitions.
    nf_config = NfConfig()
    if nf_config.is_available:
        linker.add_lib_flags("netcdf", nf_config.get_linker_flags())
