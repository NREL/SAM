#!/usr/bin/env python

"""Installs LandBOSSE via pip."""

import argparse
import logging
import os
import shlex
import subprocess
import sys


LANDBOSSE_VERSION = "1.1.2"
logger = None


def install_landbosse(python_path, landbosse_version=LANDBOSSE_VERSION):
    """Installs LandBOSSE via pip."""
    cmd = f"{python_path}/bin/pip3 install landbosse=={landbosse_version}"
    if sys.platform == "win32":
        pip = os.path.join(python_path, "Scripts", "pip3.exe")
    else:
        pip = os.path.join(python_path, "bin", "pip3")
    cmd = f"{pip} install landbosse=={landbosse_version}"

    ret = run_command(cmd)
    if ret != 0:
        msg = "Failed to install LandBOSSE"
        logger.error(msg)
        raise Exception(msg)

    logger.info("Installed LandBOSSE v%s", landbosse_version)


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
        "python_path",
        help="path to python installation"
    )
    parser.add_argument(
        "-l", "--landbosse-version",
        default=LANDBOSSE_VERSION,
        help="LandBOSSE version, default=%(default)s",
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
    install_landbosse(args.python_path)


if __name__ == "__main__":
    main()
