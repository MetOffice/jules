#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""
Build versioned Jules docs
Expects gh to be available
Expects to be run in an environment that can build the Jules docs
"""

import argparse
import json
import logging
import subprocess
import shutil
from pathlib import Path
from shlex import split

logger = logging.getLogger(__name__)


def run_command(
    command: str,
    cwd: Path = None,
) -> subprocess.CompletedProcess:
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """

    logger.debug(f"Running Command: '{command}'")
    command = split(command)
    result = subprocess.run(
        command,
        capture_output=True,
        text=True,
        timeout=300,
        shell=False,
        check=False,
        cwd=cwd,
    )
    if result.returncode:
        print(result.stdout, end="\n\n\n")
        raise RuntimeError(
            f"[FAIL] Issue found running command {command}\n\n{result.stderr}"
        )
    return result


def get_releases() -> list[str]:
    """
    Use gh to get a list of releases in Jules
    remove git_migration release and sort by version number
    """
    result = run_command("gh release list -R MetOffice/jules --json tagName")
    releases = json.loads(result.stdout)
    releases = [x["tagName"] for x in releases]
    releases.remove("git_migration")
    releases = sorted(releases, reverse=True, key=lambda x: float(x.removeprefix("vn")))
    return releases


def build_jules_docs(
    ref: str, name: str, jules: Path, artifact: Path, output: Path, force: bool = False
) -> None:
    """
    Checkout a git ref and build Jules docs at that ref
    Copy built html to the output directory with subdirectory "name"
    """

    if not force and artifact:
        artifact_version = artifact / name
        if artifact_version.exists():
            logger.info(f"Copying from artifact for version {name}")
            shutil.copytree(artifact_version, output / name)
            return

    # Checkout git ref
    run_command(f"git -C {jules} checkout {ref}")

    # Build Jules Docs
    logger.info(f"Building docs for ref {ref}")
    run_command("make clean html", cwd=jules / "doc")

    # Copy Built docs to output
    logger.info(f"Copying built docs to output for ref {ref}")
    shutil.copytree(jules / "doc" / "build" / "html", output / name)


def edit_index(index_file_path: Path, releases: list[str]) -> None:
    """
    Edit the index.html to point at the different releases
    """
    logger.info("Updating template index file")

    releases.append("latest")

    lines = index_file_path.read_text()
    lines = lines.split("\n")

    for i, line in enumerate(lines):
        if "LOCATION FOR AUTOMATIC UPDATING" in line:
            index = i + 1

    for release in releases:
        vn = f'                <li><a href="{release}/index.html">{release}</a></li>'
        lines.insert(index, vn)

    with open(index_file_path, "w") as f:
        for line in lines:
            f.write(f"{line}\n")


def parse_args():
    """
    Parse Command line arguments
    """

    parser = argparse.ArgumentParser(description="Build versioned jules docs")
    parser.add_argument(
        "-j",
        "--jules",
        default=Path("."),
        type=Path,
        help="Path to Jules clone (toplevel). Needs to have the full history to build "
        "all Jules versions",
    )
    parser.add_argument(
        "-o",
        "--output",
        default=Path().home() / "jules_docs",
        type=Path,
        help="Output directory for builds to be stored in",
    )
    parser.add_argument(
        "-a",
        "--artifact",
        default=None,
        type=Path,
        help="Path to an existing build directory to copy old versions from. Expected "
        "to be from a github artifact",
    )
    parser.add_argument(
        "-l",
        "--log",
        default="WARNING",
        help="Set the logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)",
    )

    return parser.parse_args()


def main():
    """
    Main Function
    """

    args = parse_args()
    logging.basicConfig(level=args.log, format="%(levelname)s: %(message)s")

    # Ensue output directory is empty and then move template html file
    if args.output.exists():
        logger.warning("Removing existing output directory")
        shutil.rmtree(args.output)
    args.output.mkdir(parents=True)
    index_file_path = args.output / "index.html"
    shutil.copy(
        Path(__file__).parent.resolve() / "template_index.html", index_file_path
    )

    # Get a list of releases
    releases = get_releases()
    logger.info(f"Releases: {releases}")

    # Build Docs for latest using main branch
    build_jules_docs("main", "latest", args.jules, args.artifact, args.output, True)

    # Build or Copy docs for releases
    for release in releases:
        build_jules_docs(release, release, args.jules, args.artifact, args.output)

    # Edit template index.html
    edit_index(index_file_path, releases)


if __name__ == "__main__":
    main()
