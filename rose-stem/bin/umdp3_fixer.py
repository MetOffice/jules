#!/usr/bin/env python3
#
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
'''
## NOTE ##
This module is one of several for which the Master copy is in the
UM repository. When making changes, please ensure the changes are made in the UM
repository or they will be lost during the release process when the UM copy is
copied over.

Top level module for the UMDP3 fixer / code styling tool

Usage:
 To apply UMDP3 styling to a specific file or set of files:

   umdp3_fixer.py [--c_code] <filename1> [<filename2> <filename3> ...]

 By default files are assumed to be Fortan, unless the
 --c_code flag is used.

 Or to apply UMDP3 styling to files showing differences to
 the fcm controlled trunk:

   umdp3_fixer.py --branch-diff

 Fortran files must end in .f90 or .F90 extension for the
 branch-diff version to apply to them. C files must have the
 .c or .h extension.
'''
import os
import re
import sys
import subprocess
from argparse import ArgumentParser
from shutil import which
from indentation import apply_indentation
from styling import apply_styling
from ampersands import apply_ampersand_shift
from whitespace import apply_whitespace_fixes


def get_branch_diff():
    '''If in a local working copy of an FCM branch, return a
    list of files returned by running a branch-diff command'''

    # Use the bdiff command to extract a list of files that have changed
    # on the user's branch
    bdiff = subprocess.Popen("fcm bdiff --summarize",
                             stdout=subprocess.PIPE,
                             shell=True)

    bdiff_stdout, _ = bdiff.communicate()
    bdiff_stdout = bdiff_stdout.decode(sys.stdout.encoding)

    bdiff_files = None

    if bdiff.returncode == 0:
        bdiff_files = bdiff_stdout.strip().split("\n")
        if len(bdiff_files) == 1 and bdiff_files[0] == "":
            bdiff_files = None
        else:
            bdiff_files = [bfile[1:].strip() for bfile in bdiff_files
                           if bfile[0] == "M" or bfile[0] == "A"]

    if bdiff_files is None:
        raise ValueError("Unable to run fcm bdiff command")

    # Use the fcm info command to extract the root name of the repository
    info = subprocess.Popen("fcm info",
                            stdout=subprocess.PIPE,
                            shell=True)

    info_out, _ = info.communicate()
    info_out = info_out.decode(sys.stdout.encoding)

    path = ""
    wcr_path = ""
    if info.returncode == 0:
        info_out = info_out.strip().split("\n")
        for line in info_out:
            search = re.match("^Path: (?P<url>.*)", line)
            if search:
                path = search.group("url")
            search = re.match("^Working Copy Root Path: (?P<url>.*)", line)
            if search:
                wcr_path = search.group("url")

    if path == "" or wcr_path == "":
        raise ValueError("Unable to run fcm info command")

    # Use the fcm binfo command to extract the relative root name of
    # the repository
    binfo = subprocess.Popen("fcm binfo",
                             stdout=subprocess.PIPE,
                             shell=True)

    binfo_out, _ = binfo.communicate()
    binfo_out = binfo_out.decode(sys.stdout.encoding)

    pbranch = ""
    if binfo.returncode == 0:
        binfo_out = binfo_out.strip().split("\n")
        for line in binfo_out:
            search = re.match("^Branch Parent: (?P<url>.*?)@", line)
            if search:
                pbranch = search.group("url")

    if pbranch == "":
        raise ValueError("Unable to run fcm binfo command")

    url_del = re.sub(re.escape(os.path.realpath(wcr_path)), "",
                     os.path.realpath(path))

    if len(url_del) > 0:
        if url_del[0] == '/':
            url_del = url_del[1:]

    bdiff_files = [os.path.relpath(bfile,
                   os.path.join(pbranch, url_del))
                   for bfile in bdiff_files]

    bdiff_files = [os.path.join(os.getcwd(), bfile)
                   for bfile in bdiff_files]

    # remove C files
    regex = re.compile(r'(.*\/include\/other\/.*\.h$)|(.*\.c$)')
    bdiff_files_f = [i for i in bdiff_files if not regex.search(i.strip())]
    bdiff_files_c = [i for i in bdiff_files if i not in bdiff_files_f]

    return bdiff_files_f, bdiff_files_c


def main():
    '''Main toplevel function'''
    parser = ArgumentParser(usage="""
    %(prog)s [--branch-diff] [--c_mode] [file_1 [file_2] [file_3] ...]

    This script will attempt to apply UMDP3 conformant styling to a single or
    set of source files. These are assumed to be Fortran, unless the --c_mode
    flag is set, in which case the source is assumed to be C. It accepts an
    unlimited number of filenames as arguments and will apply styling to each
    file in turn.

    NOTE: Changes to C code utilise clang-format. Because clang-format versions
    are not mutually compatible, a supported version of clang-format must be
    available. Additionally, as clang-format may not converge to a consistent
    layout, changes to C code are not enabled by default. To enable changes to
    be made, the environmnet variable RUNCCODE=1 must be set.

    NOTE: The script will overwrite the contents of the files so it is
    recommended to run only on working copies which do not have uncommitted
    changes - making it easy to revert should the results be undesired.

    The optional --branch-diff flag will instead assume your current directory
    is within a working copy and apply the styling only to files listed by the
    \"fcm branch-diff\" command.
    """)
    parser.add_argument("--branch-diff", dest="bdiff",
                        action="store_true", help="Run on a branch diff")
    parser.add_argument("--c_mode", dest="c_mode",
                        action="store_true",
                        help="Assume source file(s) are C")
    parser.add_argument("--col", dest="col",
                        type=int,
                        default=80,
                        help="Column to put &s in")
    (opts, args) = parser.parse_known_args()

    if len(sys.argv) == 1:
        parser.print_help()

    amp_column = opts.col

    failed = False
    modified = []

    if opts.bdiff:
        if len(args) > 0:
            sys.exit("ERROR: Cannot specify filenames and --branch-diff")
        f_files, c_files = get_branch_diff()
    else:
        if opts.c_mode:
            f_files = []
            c_files = args
        else:
            f_files = args
            c_files = []

    # Style Fortran Files
    if opts.bdiff or not opts.c_mode:
        if len(f_files) > 0:
            print("\nProcessing Fortran Files")
        for input_file in f_files:
            print("Processing: {0:s}".format(input_file))
            sys.stdout.flush()
            if (input_file.split(".")[-1] != "F90" and
                input_file.split(".")[-1] != "f90" and
                input_file.split(".")[-1] != "inc"):
                if input_file.split(".")[-1] == "h":
                    if (re.search(r'.*\/include\/other\/.*', input_file) is not
                       None):

                        print("Input file {0:s} not a "
                              "Fortran include file,"
                              " skipping".format(input_file))
                        continue
                else:
                    print("Input file {0:s} not a "
                          "Fortran file, skipping".format(input_file))
                    continue

            with open(input_file, "r+", errors="replace") as file_in:
                lines = file_in.read().split("\n")

                file_in.seek(0)

                reiterate = True
                modify_lines = list(lines)

                while (reiterate) and (failed is False):
                    old_lines = list(modify_lines)

                    amp_lines = None
                    amp_not_parsed = []

                    if failed is False:
                        amp_lines, amp_not_parsed = apply_ampersand_shift(
                                                modify_lines, preclean=True,
                                                col=amp_column)

                    if len(amp_not_parsed) > 0:
                        print("Ampersand Alignment Failed for:"
                              " {0:s}".format(input_file))
                        print("failed on lines:\n")
                        for i in amp_not_parsed:
                            print(str(i)+": \""+modify_lines[i]+"\"")
                        print("\n")
                        failed = True

                    white_lines = None

                    if failed is False:
                        white_lines = apply_whitespace_fixes(amp_lines)

                    if white_lines is None:
                        print("Whitespace Fixes Failed for:"
                              " {0:s}".format(input_file))
                        failed = True

                    styled_lines = None

                    if failed is False:
                        styled_lines = apply_styling(white_lines)

                    if styled_lines is None:
                        print("Styling Failed for: {0:s}".format(input_file))
                        failed = True

                    indented_lines = None

                    if failed is False:
                        indented_lines = apply_indentation(styled_lines)

                    if indented_lines is None:
                        print("Indentation Failed for: {0:s}".format(
                                                                   input_file))
                        failed = True

                    amp_lines = None
                    amp_not_parsed = []

                    if failed is False:
                        amp_lines, amp_not_parsed = apply_ampersand_shift(
                                             indented_lines, col=amp_column)

                    if len(amp_not_parsed) > 0:
                        print("Ampersand Alignment Failed for:"
                              " {0:s}".format(input_file))
                        print("failed on lines:\n")
                        for i in amp_not_parsed:
                            print(str(i)+": \""+indented_lines[i]+"\"")
                        print("\n")
                        failed = True

                    if failed is False:
                        modify_lines = amp_lines

                    if modify_lines[:] == old_lines[:]:
                        reiterate = False

                if failed is False:
                    if modify_lines != lines:
                        modified.append(input_file)
                    file_in.write("\n".join(modify_lines))
                    file_in.truncate()

            if failed is True:
                break

    # Style C Files
    if (opts.bdiff or opts.c_mode) and os.environ.get('RUNCCODE') == "1":
        # check if clang-format is available
        if which('clang-format') is not None:
            # interogate clang-format
            clang_format = subprocess.Popen("clang-format --version",
                                            stdout=subprocess.PIPE,
                                            shell=True)

            clang_format_stdout, _ = clang_format.communicate()
            if clang_format_stdout is not None:
                clang_format_stdout = clang_format_stdout.decode(
                                                           sys.stdout.encoding)
            else:
                clang_format_stdout = ''

            clang_format_stdout = clang_format_stdout.strip().split("\n")

            regex = re.compile(r'.*version\s*(\d+)\..*')
            for i in clang_format_stdout:
                clang_format_ver = regex.match(i)
                if clang_format_ver is not None:
                    clang_format_ver = clang_format_ver.group(1)
                    break

            # clang-format is not backwards compatible, so set up a version to
            # config file mapping
            format_config_map = {'14': 'um-clang_format-v12.cfg',
                                 '12': 'um-clang_format-v12.cfg',
                                 '11': 'um-clang_format-v11.cfg',
                                 '10': 'um-clang_format-v10.cfg',
                                 '3': 'um-clang_format-v3.cfg'}
            format_command_map = {'14': "clang-format --style=file -i {0}",
                                  '12': "clang-format --style=file -i {0}",
                                  '11': "clang-format --style=file -i {0}",
                                  '10': "clang-format --style=file -i {0}",
                                  '3': "bash -c 'grep -i -F "
                                       "\"/* clang-format off */\""
                                       " {0} ||"
                                       " clang-format --style=file -i {0}'"}

            if clang_format_ver is not None:
                clang_format_config = None
                try:
                    clang_format_config = format_config_map[clang_format_ver]
                except KeyError:
                    print("\nSkipping C files:")
                    print("\nThis utilises clang-format,"
                          " but the version available (version " +
                          clang_format_ver +
                          ") is not supported.")
                if clang_format_config is not None:
                    # symlink the clang-format config corresponding to the
                    # installed version onto the analysis path
                    format_confing_root = os.path.dirname(os.path.realpath(
                                                                     __file__))
                    clang_format_config = os.path.join(format_confing_root,
                                                       clang_format_config)

                    clang_format_symlink_target = os.path.dirname(
                                                 os.path.commonprefix(c_files))
                    clang_format_symlink_target = os.path.join(
                                  clang_format_symlink_target, "_clang-format")

                    os.symlink(clang_format_config,
                               clang_format_symlink_target)

                    print("\nProcessing C Files")
                    for input_file in c_files:
                        print("Processing: {0}".format(input_file))
                        # Use the clang-format command corresponding to the
                        # installed version on this system
                        args = format_command_map[clang_format_ver].format(
                                                                    input_file)

                        clang_format = subprocess.Popen(args,
                                                        stdout=subprocess.PIPE,
                                                        stderr=subprocess.PIPE,
                                                        shell=True)

                        clang_format_stdout, clang_format_stderr = (
                          clang_format.communicate())

                        if clang_format.returncode != 0:
                            clang_format_stdout = clang_format_stdout.decode(
                                                           sys.stdout.encoding)
                            clang_format_stderr = clang_format_stderr.decode(
                                                           sys.stdout.encoding)
                            print(clang_format_stderr)
                            print(clang_format_stdout)
                            failed = True

                    os.unlink(clang_format_symlink_target)
            else:
                print("\nSkipping C files:")
                print("\nThis utilises clang-format,"
                      " but the version available can not be determined.")
        else:
            print("\nSkipping C files:")
            print("\nThis utilises clang-format,"
                  " which is not available on this system")

    if failed:
        sys.exit(1)
    if modified:
        for item in modified:
            print(f"\nModified: {item}\n", file=sys.stderr)
        raise Exception("Some files were modified, see stderr for info")


if __name__ == "__main__":
    main()
