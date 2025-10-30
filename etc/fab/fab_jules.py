#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT
# which you should have received as part of this distribution
##############################################################################
import logging
import os
import json
from argparse import ArgumentParser
from typing import Dict
from fab.build_config import AddFlags, BuildConfig
from fab.steps.analyse import Analyse
from fab.steps.compile_c import CompileC
from fab.steps.compile_fortran import CompileFortran
from fab.steps.grab import GrabFcm, GrabFolder
from fab.steps.link_exe import LinkExe
from fab.steps.preprocess import c_preprocessor, fortran_preprocessor
from fab.steps.root_inc_files import RootIncFiles
from fab.steps.walk_source import FindSourceFiles, Exclude, Include
from fab.steps.archive_objects import ArchiveObjects
from fab.dep_tree import AnalysedFile
from fab.constants import BUILD_OUTPUT
from pathlib import Path


def jules_config(config_dict: Dict, revision=None):

    config = BuildConfig(project_label='jules', verbose=True)

    logger = logging.getLogger('fab')
    logger.info(f'building jules revision {revision}')
    logger.info(f"OMPI_FC is {os.environ.get('OMPI_FC') or 'not defined'}")

    host_source_jules_base = os.environ.get('HOST_SOURCE_JULES_BASE')
    if host_source_jules_base.startswith('fcm:') \
        or host_source_jules_base.startswith('svn:') \
            or host_source_jules_base.startswith('https:'):
        grab_fcm = (
            GrabFcm(src=host_source_jules_base+'/src', revision='head',
                    dst='src'),
            GrabFcm(src=host_source_jules_base+'/utils', revision='head',
                    dst='utils'))
    else:
        grab_fcm = (
            GrabFolder(src=host_source_jules_base+'/src',
                       dst='src'),
            GrabFolder(src=host_source_jules_base+'/utils',
                       dst='utils'))
    # Assemble Analysis options
    if config_dict['symbol_deps']:
        analyse_options = Analyse(
            root_symbol=config_dict['root_symbol'],
            unreferenced_deps=config_dict['unreferenced_dependencies'],
            # fparser2 fails to parse this file, but it does compile.
            special_measure_analysis_results={
                AnalysedFile(fpath=Path(
                    config.project_workspace / BUILD_OUTPUT /
                    "src/initialisation/standalone/init_output_mod.f90"
                ),
                    # This line needs to be removed at some point
                    # see ticket #1354 and
                    # https://github.com/metomi/fab/issues/161
                    # 'file-hash' needs to be defined in order the function to
                    # work. This will be amended in a future release
                    file_hash=1,
                    symbol_defs={'init_output_mod'},
                    symbol_deps=config_dict['symbol_deps']
                )})
    else:
        analyse_options = Analyse(
            root_symbol=config_dict['root_symbol'],
            unreferenced_deps=config_dict['unreferenced_dependencies'])

    # Assemble path flag options
    path_flags = []
    if "compiler_path_flags" in config_dict:
        for key, value in config_dict["compiler_path_flags"].items():
            path_flags.append(AddFlags(key, value))

    config.steps = [
        *grab_fcm,
        FindSourceFiles(path_filters=[
            Exclude(*config_dict['source_files']['exclude']),
            Include(*config_dict['source_files']['include'])]),
        RootIncFiles(),
        c_preprocessor(),
        fortran_preprocessor(
            preprocessor='cpp',
            common_flags=config_dict['preprocessor_flags']
        ),
        analyse_options,
        CompileC(),
        CompileFortran(compiler=config_dict['compiler_type'],
                       common_flags=config_dict['compiler_flags'],
                       path_flags=path_flags),
        ArchiveObjects(),
        LinkExe(
            linker=config_dict['compiler_type'],
            flags=config_dict['linker_flags'])
    ]

    return config


def parse_args():

    arg_parser = ArgumentParser()
    arg_parser.add_argument('--revision',
                            default=os.getenv('JULES_REVISION', 'vn7.0'))
    arg_parser.add_argument('-c', '--config', type=str, required=True,
                            help='Path to JSON config file.')
    return arg_parser.parse_args()


if __name__ == '__main__':

    args = parse_args()
    f = open(args.config + '.json')
    config_file = json.load(f)
    jules_config(config_file, args.revision).run()
