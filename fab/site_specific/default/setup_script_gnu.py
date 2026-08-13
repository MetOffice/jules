#!/usr/bin/env python3

'''This file contains a function that sets the default flags for all
GNU based compilers in the ToolRepository.

This function gets called from the default site-specific config file
'''

import argparse

from typing import cast
from fab.api import BuildConfig, Category, Linker, ToolRepository


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
        ['-ffree-line-length-none', '-Wall',
         '-g',
         # TODO: Jules river cannot be compiled with this:
         # '-Werror=unused-value',
         # We might either try to fix the sources if possible,
         # or see if it's worth setting this option only for one
         # specific file (though that then means compiler-specific
         # options in the generic fab script :( )
         '-Werror=tabs',
         '-std=f2008',
         ],
        "base")

    if gfortran.get_version() >= (10, 0):
        # Required for certain MPI versions (since gfortran version 10)
        gfortran.add_flags("-fallow-argument-mismatch", "base")

    runtime = ["-fcheck=all", "-ffpe-trap=invalid,zero,overflow"]
    init = ["-finit-integer=31173",  "-finit-real=snan",
            "-finit-logical=true", "-finit-character=85"]
    # Full debug
    # ==========
    gfortran.add_flags(runtime + ["-O0"] + init, "full-debug")

    # Fast debug
    # ==========
    gfortran.add_flags(runtime + ["-Og"], "fast-debug")

    # Production
    # ==========
    gfortran.add_flags(["-Ofast"], "production")

    # unit-tests
    # ==========
    gfortran.add_flags(runtime + ["-O0"] + init, "unit-tests")

    # Set up the linker
    # =================
    # This will implicitly affect all gfortran based linkers, e.g.
    # linker-mpif90-gfortran will use these flags as well.
    linker = tr.get_tool(Category.LINKER, "linker-gfortran")
    linker = cast(Linker, linker)

    # ATM we don't use a shell when running a tool, and as such
    # we can't directly use "$()" as parameter. So query these values using
    # Fab's shell tool (doesn't really matter which shell we get, so just
    # ask for the default):
    shell = tr.get_default(Category.SHELL)

    try:
        # We must remove the trailing new line, and create a list:
        nc_flibs = shell.run(additional_parameters=["-c", "nf-config --flibs"],
                             capture_output=True).strip().split()
    except RuntimeError:
        nc_flibs = []

    linker.add_lib_flags("netcdf", nc_flibs)
    linker.add_lib_flags("hdf5", ["-lhdf5"])
