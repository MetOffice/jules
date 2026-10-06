#!/usr/bin/env python3

'''This file contains a function that sets the default flags for all
GNU based compilers in the ToolRepository.

This function gets called from the default site-specific config file
'''

import argparse

from typing import cast
from fab.api import BuildConfig, Category, ContainFlags, Linker, ToolRepository

from nf_config import NfConfig


def setup_script_gnu(build_config: BuildConfig, args: argparse.Namespace):
    # pylint: disable=unused-argument
    '''Defines the default flags for all GNU compilers.

    :para build_config: the build config from which required parameters
        can be taken.
    :param args: all command line options
    '''

    tr = ToolRepository()
    gfortran = tr.get_tool(Category.FORTRAN_COMPILER, "gfortran")

    if not gfortran.is_available:
        gfortran = tr.get_tool(Category.FORTRAN_COMPILER, "mpif90-gfortran")
        if not gfortran.is_available:
            return

    # The base flags
    # ==============
    gfortran.add_flags(
        ['-std=f2003', '-fall-intrinsics', '-fmax-identifier-length=63',
         '-ffree-line-length-132', '-fimplicit-none',
         '-Warray-bounds'],
        "base")

    if gfortran.get_version() >= (10, 0):
        # Required for certain MPI versions (since gfortran version 10)
        gfortran.add_flags(
            ContainFlags("/read_dump_mod", "-fallow-argument-mismatch"),
            "base")

    # Debug
    # =====
    gfortran.add_flags(['-g', '-pg', '-ffpe-trap=invalid,zero,overflow',
                        '-fbacktrace', '-Wall', '-Wextra'], "debug")

    # Normal
    # ======
    gfortran.add_flags(["-Werror"], "normal")

    if gfortran.get_version() >= (10, 0):
        # The -Werror flags turns the warning for argument mismatch back into
        # an error, since this flag comes after -fallow-argument-mismatch
        # (because "normal" in inherits from "base"). So, in case of normal
        # compilation mode we need to disable -Werror for this one file:
        gfortran.add_flags(
            ContainFlags("/read_dump_mod", "-Wno-error"),
            "normal")
    # Fast
    # ====
    gfortran.add_flags(["-O3"], "fast")

    # Set up the linker
    # =================
    linker = tr.get_tool(Category.LINKER, f"linker-{gfortran.name}")
    linker = cast(Linker, linker)

    # As default, use nf-config to set NetCDF linker flags. If it's not
    # available (or not working properly), the site-specific setup must
    # add netcdf definitions.
    nf_config = NfConfig()
    if nf_config.is_available:
        linker.add_lib_flags("netcdf", nf_config.get_linker_flags())
