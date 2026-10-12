#!/usr/bin/env python3

'''This file contains a function that sets the default flags for all
Intel classic based compilers in the ToolRepository (ifx, icx).

This function gets called from the default site-specific config file
'''

import argparse
from typing import cast

from fab.api import BuildConfig, Category, Compiler, Linker, ToolRepository

from nf_config import NfConfig


def setup_script_intel_llvm(build_config: BuildConfig,
                            args: argparse.Namespace):
    # pylint: disable=unused-argument, too-many-locals
    '''Defines the default flags for all Intel llvm compilers.

    :para build_config: the build config from which required parameters
        can be taken.
    :param args: all command line options
    '''

    tr = ToolRepository()
    ifx = tr.get_tool(Category.FORTRAN_COMPILER, "ifx")
    ifx = cast(Compiler, ifx)

    if not ifx.is_available:
        # This can happen if ifx is not in path (in spack environments).
        # To support this common use case, see if mpif90-ifx is available,
        # and initialise this otherwise.
        ifx = tr.get_tool(Category.FORTRAN_COMPILER, "mpif90-ifx")
        ifx = cast(Compiler, ifx)
        if not ifx.is_available:
            # Since some flags depends on version, the code below requires
            # that the intel compiler actually works.
            return

    # The base flags
    # ==============
    # The following flags will be applied to all modes:
    common = ['-heap-arrays', '-std03', '-fpscomp', 'logicals',
              '-traceback',
              '-assume nosource_include,protect_parens',
              '-fp-model', 'precise', '-no-vec']
    ifx.add_flags(common, "base")

    # Debug
    # =====
    debug = ['-g', '-C', '-check', 'noarg_temp_created', '-fpe0', '-ftz',
             '-ftrapuv', '-init=arrays']
    ifx.add_flags(debug, "debug")

    # Normal
    # ======

    # Fast
    # ====
    ifx.add_flags(["-O3"], "fast")

    # Set up the linker
    # =================
    # This will implicitly affect all ifx based linkers, e.g.
    # linker-mpif90-ifx will use these flags as well.
    linker = tr.get_tool(Category.LINKER, "linker-ifx")
    linker = cast(Linker, linker)

    # As default, use nf-config to set NetCDF linker flags. If it's not
    # available (or not working properly), the site-specific setup must
    # add netcdf definitions.
    nf_config = NfConfig()
    if nf_config.is_available:
        linker.add_lib_flags("netcdf", nf_config.get_linker_flags())
