#!/usr/bin/env python3
#
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

'''
Rose-stem test for checking the branch working copy with the trunk.

Fail if any files are changed by the umdp3_checker.py script.

Return a list of any files changed by the umdp3_checker.py script.

Usage:
 Fortran files must end in .f90, .F90 or .inc extension.
 Copy files to work/tmp.
 Run the script on the whole tmp dir.
 Diff with the working copy.
 Fail if changes and return files that need changing.

'''

from optparse import OptionParser
import sys
import os
import shutil
import subprocess
import tempfile


def copy_working_branch(model_source):
    '''Copy the working version of the branch to a tmp dir.'''
    # Make the tmp dir in the cwd which is the work/ of the task name.
    tmpdir = tempfile.mkdtemp()
    tmp_filename = 'model_diff'

    # Ensure the file is read/write by the creator only
    saved_umask = os.umask(0o077)

    tmp_path = os.path.join(tmpdir, tmp_filename)

    # Copy the src/ from the working copy to the tmp dir.

    src_wkcopy = os.path.join(model_source, "src")
    try:
        shutil.copytree(src_wkcopy, tmp_path)
    # Directories are the same
    except shutil.Error as err:
        print('Directory not copied. Error: %s' % err)
    # Any error saying that the directory doesn't exist
    except OSError as err:
        print('Directory not copied. Error: %s' % err)
    return tmp_path, saved_umask, tmpdir


def diff_cwd_working(model_source, path, saved_umask, tmpdir):
    '''Diff the tmp dir with the working branch and report diff.'''
    diff = subprocess.run("diff -qr " + model_source + "/src " + path,
                            stdin=subprocess.DEVNULL,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                            shell=True,
                            universal_newlines=True)

    if diff.returncode == 0:
        print("[OK] No changes were made by the UMDP3 checker script and "
              "the working copy complies with the coding standards. "
              "No action required.")
    else:
        diff_stdout = diff.stdout
        diff_files = diff_stdout.strip().split("\n")
        print("[FAIL] The following files were changed when the "
              "umdp3_fixer.py script was run:")
        for diff_filesname in diff_files:
            # diffs are of the form "Files <x> and <y> differ"
            # we select only <x>
            print("[FAIL] " + diff_filesname.split()[1])
        print("Please run rose-stem/bin/umdp3_fixer.py on each of the "
              "failed files in your working copy and check the changes. "
              "Then commit the changes to your branch and then re-run all "
              "rose-stem testing.")
        os.umask(saved_umask)
        shutil.rmtree(tmpdir)
        raise ValueError("Ran fcm diff command and changes were made by " +
                         "the umdp3_fixer.py script.")

    return


def run_umdp3checker(model_source, path, amp_column):
    '''Run the umdp3 fixer script in the tmp dir copy of the working branch.'''
    try:
        subprocess.run(model_source + "/rose-stem/bin/umdp3_fixer.py " +
                       "--col {0:} ".format(amp_column) +
                       "$(find " + path +
                       " -name '*.[F|f]90' -o -name '*.inc' | xargs)",
                       capture_output=True,
                       check=True,
                       stdin=subprocess.DEVNULL,
                       shell=True)

    except subprocess.CalledProcessError as exc:
        if "Exception: Some files were modified" in exc.stderr.decode():
            # umpd3_fixer.py raises an exception on finding any modified
            # files, but we can ignore it here.
            print('[WARN] The following files were found to be modified:',
                  file=sys.stderr)
            for line in exc.stderr.decode().split('\n'):
                if line.strip().startswith("Modified:"):
                    print("   * " + line.replace("Modified:",""),
                          file=sys.stderr)
            print("", file=sys.stderr)
        else:
            print("[FAIL] Problem while attempting to run umdp3_fixer.py")
            raise
    return


def main():
    '''Take in the location of working branch location and run the tests.'''
    # Initialise the command line parser.
    description = 'Args for the source code...'
    parser = OptionParser(description=description)
    parser.add_option('--source',
                      dest='source',
                      action='store',
                      help='source of the model branch',
                      default='None')
    # e.g. "--source model_source_branch"

    parser.add_option('--col',
                      dest='col',
                      action='store',
                      help='Column to put "&"s in',
                      type="int",
                      default='80')
    # e.g. "--col 80"

    # Parse the command line.
    (opts, _) = parser.parse_args()
    model_source = opts.source
    amp_column   = opts.col

    (path, saved_umask, tmpdir) = copy_working_branch(model_source)
    run_umdp3checker(model_source, path, amp_column)
    diff_cwd_working(model_source, path, saved_umask, tmpdir)

    os.umask(saved_umask)
    shutil.rmtree(tmpdir)

if __name__ == "__main__":
    main()
