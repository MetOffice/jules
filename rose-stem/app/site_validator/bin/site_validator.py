#!/usr/bin/env python3
# *****************************COPYRIGHT******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT******************************
#
"""
Loop through a dictionary of sites with known working groups
(WORKING_CONFIGS) to validate each one.
Expected to be run as part of a rose-stem suite
"""

import sys
from pathlib import Path
import subprocess
import argparse

WORKING_CONFIGS = {
    "cehwl1": ["all"],
    "jasmin": ["all"],
    "meto": ["all", "ex1a", "azspice"],
    "nci": ["all"],
    "vm": ["all"],
}

def run_command(command):
    """
    Launch a subprocess command, capture the output and return the result
    """
    pobj = subprocess.Popen(
        command.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE
    )
    pobj.wait()
    retcode, stdout, stderr = (
        pobj.returncode,
        pobj.stdout.read().decode("utf-8"),
        pobj.stderr.read().decode("utf-8"),
    )
    return retcode, stdout, stderr


def generate_validate_command(source, site, group):
    """
    Generate a command to determine if rose-stem suite is valid.
    """

    suitename = f"test_validate_jules_{site}_{group}"

    install_cmd = (
        "cylc validate --debug --check-circular "
        f"-z g={group} "
        f"-S SITE='{site}' "
        f"{source}"
    )

    return install_cmd, suitename


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Validate rose-stem Jules suites for different sites"
    )
    parser.add_argument(
        "-s",
        "--source",
        help="The Jules Source",
        required=True,
    )
    args = parser.parse_args()

    failures = False
    for site in WORKING_CONFIGS:
        for group in WORKING_CONFIGS[site]:
            print(f"[INFO] Validating {site} with {group}")
            command, suitename = generate_validate_command(
                args.source, site, group
            )
            retcode, stdout, stderr = run_command(command)
            if retcode:
                print(f"[FAIL] {site} with {group} failed to validate")
                print(stdout)
                print(stderr, file=sys.stderr)
                failures = True
            else:
                print(f"[Pass] {site} with {group} validated successfully")

    if failures:
        sys.exit(1)
