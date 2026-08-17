#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT
# which you should have received as part of this distribution
##############################################################################

'''
This module contains a BAF-based build script for Jules.
'''

import argparse
import logging
from pathlib import Path
import sys
from typing import cast, Iterable, Optional, Union


from fab.api import (FabBase, fcm_export, Exclude, find_source_files,
                     git_checkout, Include, root_inc_files)

logger = logging.getLogger(__name__)


class JulesBuild(FabBase):
    '''
    A class to build Jules using BAF as base class.

    :param str name: name of the build.
    '''

    def __init__(self, name: str):
        '''
        Build Jules using Fab.

        :name: The base name to use for the project directory
        '''
        # If the sources need to be checked out or not
        self._checkout = False

        # The revision number to check out (if requested)
        self._revision = None

        # The root of the jules repository. Defined in
        # handle_command_line_options
        self._root = None

        super().__init__(name)

    def define_command_line_options(
            self,
            parser: Optional[argparse.ArgumentParser] = None
            ) -> argparse.ArgumentParser:
        '''
        This adds a revision option to the command line options inherited
        from the base class.

        :param Optional[argparse.ArgumentParser] parser: a pre-defined
        argument parser. If not, a new instance will be created.
        :returns: the argument parser with the jules specific options added.
        :rtype :py:class:`argparse.ArgumentParser`
        '''

        parser = super().define_command_line_options(parser)
        parser = cast(argparse.ArgumentParser, parser)
        parser.add_argument(
            "--revision", "-r", type=str, default="vn7.8",
            help="Sets the Jules revision to checkout (only used if"
                 "--checkout is used). Defaults to 'vn7.8'.")
        parser.add_argument(
            "--checkout", default=False, action="store_true",
            help="If specified, will checkout jules from git or svn, "
                 "otherwise this script is excepted to be in a cloned "
                 "git version.")
        parser.add_argument(
            "--rivers", default=False, action="store_true",
            help="If specified, the stand-alone rivers binary will be "
                 "compiled.")
        parser.add_argument(
            "--ascii-out", default=False, action="store_true",
            help="If specified, NetCDF will be disabled and output "
                 "will be in ASCII instead.")
        return parser

    def handle_command_line_options(self,
                                    parser: argparse.ArgumentParser) -> None:
        '''
        Grab the requested (or default) arguments for checkout and Jules
        revision to use and store it in an attribute. Do consistency checks
        to make sure a revision is only specified if also a checkout is
        requested.

        :param parser: the argument parser.

        :raises ValueError: if a revision is specified, but no checkout
            is requested.
        '''

        super().handle_command_line_options(parser)
        self._checkout = self.args.checkout
        self._revision = self.args.revision
        self._ascii_out = self.args.ascii_out
        self._rivers = self.args.rivers
        if self._rivers:
            self.set_root_symbol("river")

        if not self._checkout and "--revision" in sys.argv[1:]:
            raise ValueError(f"You specified revision '{self._revision}', "
                             f"but did not request a checkout.")

    def define_project_name(self, name: str) -> str:
        '''
        This method adds version number, ascii output (if selected), and
        MPI and OpenMP information to the project name, so these different
        binaries can be distinguished.

        :returns: the project directory name to use.
        '''
        if self._rivers:
            name = name + "-rivers"
        if self._revision:
            name = name + f"-{self._revision}"
        if self.args.ascii_out:
            name = name + "-ascii"
        if self.args.mpi:
            name = name + "-mpi"
        if self.args.openmp:
            name = name + "-openmp"
        return super().define_project_name(name)

    def grab_files_step(self) -> None:
        '''
        Extracts all the required Jules source files from the repositories.

        :raises RuntimeError: if no checkout is required, but expected Jules
            directory (rose-meta/jules-shared) does not exist, indicating
            an invalid directory structure.
        '''
        # If no checkout was requested, make sure we have the expected
        # repository structure.
        if not self._checkout:
            # Get the root directory of this Jules:
            self._root = Path(__file__).resolve().parents[1]
            jules_shared = self._root / "rose-meta" / "jules-shared"
            if not jules_shared.exists():
                raise RuntimeError(f"The expected directory '{jules_shared}' "
                                   f"does not exist.")
            return

        # Try to grab sources from GitHub, fallback to FCM if that fails

        try:
            git_checkout(
                self.config,
                src="git@github.com:MetOffice/jules",
                revision=self._revision,
                dst_label="jules.git",
            )
            self._root = (self.config.project_workspace
                          / "source" / "jules.git")
        except Exception as e:
            logging.warning(f"git_checkout failed: {e}, "
                            f"falling back to fcm_export")
            # We export the whole svn repository, to be consistent with
            # using either a local checkout or git checkout
            fcm_export(
                self.config,
                src="fcm:jules.xm_tr",
                revision=self._revision,
                dst_label="jules.svn",
            )
            self._root = self.config.project_workspace / "source" / "jules.svn"

    def find_source_files_step(
            self,
            path_filters: Optional[Iterable[Union[Exclude, Include]]] = None):
        '''
        Finds all the Jules sources files to analyse.

        :param path_filters: optional list of path filters to be passed to
            Fab find_source_files, default is None.
        '''
        if path_filters:
            local_filters = path_filters.copy()
        else:
            local_filters = []
        local_filters.extend([
            Exclude("src/control/um/"),
            Exclude("src/initialisation/um/"),
            Exclude("src/params/shared/cable_maths_constants_mod.F90"),
            ]
        )
        if self._rivers:
            # The order is important, the last include/exclude statement
            # takes precedence. So exclude standalone must come before
            # including init_initial_mod
            local_filters.extend([
                Exclude("src/control/standalone/jules.F90"),
                Exclude("src/initialisation/standalone/"),
                Include("src/initialisation/standalone/init_initial_mod.F90"),
                Include("src/initialisation/standalone/initial_conditions/"
                        "jules_initial_mod.F90"),
                Include("src/initialisation/standalone/init_output_mod.F90"),
                Include("src/initialisation/standalone/init_rivers.F90"),
                Include("src/initialisation/standalone/init_time_mod.F90"),
                Include("src/initialisation/standalone/init_drive_mod.F90"),
                Include("src/initialisation/standalone/"
                        "init_model_environment_mod.F90"),
                Include("src/initialisation/standalone/grid/"
                        "fill_model_grid_arrays_mod.F90"),
                Include("src/initialisation/standalone/grid/"
                        "init_input_grid_mod.F90"),
                Include("src/initialisation/standalone/grid/"
                        "init_river_out_grid_mod.F90"),
                Include("src/initialisation/standalone/grid/"
                        "init_latlon_mod.F90"),
                Include("src/initialisation/standalone/grid/"
                        "init_land_frac_mod.F90"),
                Include("src/initialisation/standalone/grid/"
                        "init_model_grid_mod.F90"),
                Include("src/initialisation/standalone/rivers-standalone/"
                        "ancillaries"),
                Include("src/initialisation/standalone/ancillaries/"
                        "init_rivers_props_mod.F90"),
                Include("src/initialisation/standalone/ancillaries/"
                        "ancil_namelist_mod.F90"),
                Include("src/initialisation/standalone/ancillaries/"
                        "init_ancillaries_coupling_mod.F90"),
                Include("src/initialisation/standalone/ancillaries/"
                        "init_rivers_process_data_mod.F90"),
                Include("src/initialisation/standalone/ancillaries/"
                        "jules_overbank_props_mod.F90"),
                Include("src/initialisation/standalone/ancillaries/"
                        "jules_rivers_props_mod.F90"),
                Exclude("src/initialisation/shared/"
                        "check_compatible_options_mod.F90"),
                Exclude("src/control/lfric/check_unavailable_options_mod.F90"),
                Exclude("src/io/dump/read_dump_mod.F90"),
                Exclude("src/io/dump/write_dump_mod.F90"),
                ])
        else:
            local_filters.extend([
                Exclude("src/control/rivers-standalone/"),
                Exclude("src/io/rivers-standalone"),
                Exclude("src/initialisation/rivers-standalone/"),
                Exclude("src/control/lfric/check_unavailable_options_mod.F90"),
                ])

        find_source_files(self.config,
                          source_root=self._root / "src",
                          path_filters=local_filters)

        # Add the utility files as required
        # ---------------------------------
        utils = self._root / "utils"
        # For now assume dr hook is always disabled, so use dummy
        find_source_files(self.config,
                          source_root=utils / "drhook_dummy")
        if not self.config.mpi:
            find_source_files(self.config,
                              source_root=utils / "mpi_dummy")
        if self._ascii_out:
            find_source_files(self.config,
                              source_root=utils / "netcdf_dummy")

        # move inc files to the root for easy tool use
        root_inc_files(self.config)

    def define_preprocessor_flags_step(self) -> None:
        '''
        Defines the preprocessor flags.
        '''
        super().define_preprocessor_flags_step()
        flags = ["-I$output"]
        if not self.config.mpi:
            flags.append("-DMPI_DUMMY")
        if self._ascii_out:
            flags.append("-DNCDF_DUMMY")

        self.add_preprocessor_flags(flags)

    def get_linker_flags(self) -> list[str]:
        '''
        Base class for setting linker flags.
        :returns: list of flags for the linker.
        '''
        libs = []
        if not self._ascii_out:
            libs.extend(["netcdf"])
        return libs


if __name__ == "__main__":
    logger = logging.getLogger(__name__)
    logger.setLevel(logging.DEBUG)

    class NoFabFilter(logging.Filter):
        '''A dummy class that disables all Fab noise.
        '''
        def filter(self, record):
            return not record.name.startswith("fab")

    # root_logger = logging.getLogger()
    # root_logger.setLevel(logging.DEBUG)
    last_resort_handler = logging.lastResort
    last_resort_handler.setLevel(logging.DEBUG)

    fab_logger = logging.getLogger("fab")
    fab_logger.setLevel(logging.DEBUG)
    fab_handlers = fab_logger.handlers
    # fab_handlers[0].addFilter(NoFabFilter())

    jb = JulesBuild("jules")
    jb.build()
