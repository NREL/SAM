#!/usr/bin/env python

"""Installs LandBOSSE via pip."""

import argparse
import json
import logging
import os
import shlex
import subprocess
import sys


LANDBOSSE_CONFIG_FILE = "landbosse.json"
PYTHON_CONFIG_FILE = "python_config.json"

logger = None


def install_landbosse(base_path, pip):
    """Installs LandBOSSE via pip.

    Parameters
    ----------
    base_path : str
        path containing the config file
    pip : str
        path to pip executable

    """
    version = get_landbosse_version(base_path)
    cmd = f"{pip} install landbosse=={version} --no-warn-script-location"
    ret = run_command(cmd)
    if ret != 0:
        msg = "Failed to install LandBOSSE"
        logger.error(msg)
        raise Exception(msg)

    logger.info("Installed LandBOSSE v%s", version)


def get_landbosse_version(base_path):
    """Read the version from the LandBOSSE config file.

    Parameters
    ----------
    base_path : str
        path containing the config file

    Returns
    -------
    str

    """
    filename = os.path.join(base_path, LANDBOSSE_CONFIG_FILE)
    with open(filename) as f_in:
        data = json.load(f_in)

    return data["landbosse_version"]


def update_python_path(base_path, path):
    """Update the Python config file with the python executable path.

    Parameters
    ----------
    base_path : str
        path containing the config file
    path : str
        full path of the Python executable

    """
    filename = os.path.join(base_path, PYTHON_CONFIG_FILE)
    with open(filename) as f_in:
        data = json.load(f_in)

    data["exec_path"] = path
    with open(filename, "w") as f_out:
        json.dump(data, f_out, indent=4)

    logger.debug("Updated %s with %s", filename, path)


def run_command(cmd, output=None, cwd=None):
    """Runs a command as a subprocess.

    Parameters
    ----------
    cmd : str
        command to run
    output : None | dict
        If a dict is passed then return stdout and stderr as keys.
    cwd: str, default None
        Change the working directory to cwd before executing the process.
    Returns
    -------
    int
        return code from system; usually zero is good, non-zero is error

    Caution: Capturing stdout and stderr in memory can be hazardous with
    long-running processes that output lots of text. In those cases consider
    running subprocess.Popen with stdout and/or stderr set to a pre-configured
    file descriptor.

    """
    logger.debug(cmd)
    # Disable posix if on Windows.
    command = shlex.split(cmd, posix="win" not in sys.platform)

    if output is not None:
        pipe = subprocess.Popen(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, cwd=cwd,
        )
        out, err = pipe.communicate()
        output["stdout"] = out.decode("utf-8")
        output["stderr"] = err.decode("utf-8")
        ret = pipe.returncode
    else:
        ret = subprocess.call(command, cwd=cwd)

    if ret != 0:
        logger.debug("Command [%s] failed: %s", cmd, ret)
        if output:
            logger.debug(output["stderr"])

    return ret


def _parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--python-base-path",
        required=True,
        help="path to python installation; must contain config files"
    )
    parser.add_argument(
        "--python-exec",
        required=True,
        help="full path of python executable"
    )
    parser.add_argument(
        "--pip",
        required=True,
        help="full path to pip"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="enable debug logging",
    )

    args = parser.parse_args()
    return args


def _setup_logging(name, level):
    global logger
    logger = logging.getLogger(name)
    logger.setLevel(level)

    console_handler = logging.StreamHandler()
    console_handler.setLevel(level)

    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)


def main():
    """Main execution"""
    args = _parse_args()
    level = logging.DEBUG if args.verbose else logging.INFO
    _setup_logging("install_landbosse", level)
    install_landbosse(args.python_base_path, args.pip)
    update_python_path(args.python_base_path, args.python_exec)


if __name__ == "__main__":
    main()
