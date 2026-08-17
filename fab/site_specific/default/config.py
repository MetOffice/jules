#! /usr/bin/env python3


'''
This module contains the default Baf configuration class.
'''

import argparse
from typing import List

from fab.api import BuildConfig, Category, ToolRepository

from site_specific.default.setup_script_cray import setup_script_cray
from site_specific.default.setup_script_gnu import setup_script_gnu
from site_specific.default.setup_script_intel_classic import (
    setup_script_intel_classic)
from site_specific.default.setup_script_intel_llvm import (
    setup_script_intel_llvm)
from site_specific.default.setup_script_nvidia import setup_script_nvidia


class Config:
    '''
    This class is the default Configuration object for Baf builds.
    It provides several callbacks which will be called from the build
    scripts to allow site-specific customisations.
    '''

    def __init__(self):
        self._args = None

    @property
    def args(self) -> argparse.Namespace:
        '''
        :returns argparse.Namespace: the command line options specified by
            the user.
        '''
        return self._args

    def get_valid_profiles(self) -> List[str]:
        '''
        Determines the list of all allowed compiler profiles. The first
        entry in this list is the default profile to be used. This method
        can be overwritten by site configs to add or modify the supported
        profiles.

        :returns List[str]: list of all supported compiler profiles.
        '''
        return ["debug", "normal", "fast"]

    def update_toolbox(self, build_config: BuildConfig) -> None:
        '''
        Set the default compiler flags for the various compiler
        that are supported.

        :param build_config: the Fab build configuration instance
        :type build_config: :py:class:`fab.BuildConfig`
        '''
        # First create the default compiler profiles for all available
        # compilers and preprocessor. While we have a tool box with exactly
        # one compiler in it, compiler wrappers will require more than one
        # compiler to be initialised - so we just initialise all of them
        # (including the linker). Similarly, preprocessing might use
        # profiles as well, so set them up:
        tr = ToolRepository()
        for compiler in (tr[Category.C_COMPILER] +
                         tr[Category.FORTRAN_COMPILER] +
                         tr[Category.FORTRAN_PREPROCESSOR] +
                         tr[Category.LINKER]):
            if compiler.is_available:
                # Define a base profile, which contains the common
                # compilation flags. This 'base' is not accessible to
                # the user, so it's not part of the profile list. Also,
                # make it inherit from the default profile '', so that
                # a user does not have to specify the base profile
                compiler.define_profile("base", inherit_from="")
                for profile in self.get_valid_profiles():
                    compiler.define_profile(profile, inherit_from="base")

        self.setup_intel_classic(build_config)
        self.setup_intel_llvm(build_config)
        self.setup_gnu(build_config)
        self.setup_nvidia(build_config)
        self.setup_cray(build_config)

        # Define compiler-specific preprocessing flag
        tb = build_config.tool_box
        fortran = tb.get_tool(Category.FORTRAN_COMPILER)
        pre_fortran = tb.get_tool(Category.FORTRAN_PREPROCESSOR)
        if fortran.suite == "gnu":
            pre_fortran.add_flags("-DGNU_FORTRAN")
        elif fortran.suite in ["intel-classic", "intel-llvm"]:
            pre_fortran.add_flags("-DINTEL_FORTRAN")
        elif fortran.suite in ["cray"]:
            pre_fortran.add_flags("-DCRAY_FORTRAN")

    def handle_command_line_options(self, args: argparse.Namespace) -> None:
        '''
        Additional callback function executed once all command line
        options have been added. This is for example used to add
        Vernier profiling flags, which are site-specific.

        :param argparse.Namespace args: the command line options added in
        the site configs
        '''
        # Keep a copy of the args, so they can be used when
        # initialising compilers
        self._args = args

    def setup_cray(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Cray compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        :type build_config: :py:class:`fab.BuildConfig`
        '''
        setup_script_cray(build_config, self.args)

    def setup_gnu(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Gnu compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        :type build_config: :py:class:`fab.BuildConfig`
        '''
        setup_script_gnu(build_config, self.args)

    def setup_intel_classic(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Intel classic compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        :type build_config: :py:class:`fab.BuildConfig`
        '''
        setup_script_intel_classic(build_config, self.args)

    def setup_intel_llvm(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Intel LLVM compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        :type build_config: :py:class:`fab.BuildConfig`
        '''
        setup_script_intel_llvm(build_config, self.args)

    def setup_nvidia(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Nvidia compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        :type build_config: :py:class:`fab.BuildConfig`
        '''
        setup_script_nvidia(build_config, self.args)
