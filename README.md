# System Advisor Model (SAM)
![Build](https://github.com/NREL/SAM/actions/workflows/ci.yml/badge.svg)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2FNREL%2FSAM.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2FNREL%2FSAM?ref=badge_shield)

The SAM Open Source Project repository contains the source code, tools, and instructions to build a desktop version of the National Renewable Energy Laboratory's System Advisor Model (SAM). SAM is a simulation program for electricity generation projects. It has models for different kinds of renewable energy systems and financial models for residential, commercial, and utility-scale projects. For more details about SAM's capabilities, see the SAM website at [https://sam.nrel.gov/](https://sam.nrel.gov/).

For a short video describing the SAM repositories, see https://youtu.be/E5z1iiZfZ3M.

The [SAM release notes](https://nrel.github.io/SAM/doc/releasenotes.html) are in https://github.com/NREL/SAM/blob/gh-pages/doc/releasenotes.html.

The desktop version of SAM for Windows, Mac, or Linux builds from the following open source projects:

* [SSC](https://github.com/nrel/ssc) is a set of "compute modules" that simulate different kinds of power systems and financial structures. It can be run directly using the [SAM Software Development Kit](https://sam.nrel.gov/sdk). **This is the source code for SAM's models, and is the repository to use for researching the algorithms underlying the models.**

* [LK](https://github.com/nrel/lk) is a scripting language that is integrated into SAM. SAM's user interface uses LK to calculate values to display on input pages. The user interface includes a script editor that allows users to write their own scripts from the user interface.

* [wxWidgets](https://www.wxwidgets.org/) is a cross-platform graphical user interface platform used for SAM's user interface, and for the development tools included with SSC (SDKtool) and LK (LKscript). The current version of SAM uses wxWidgets 3.1.5.

* [WEX](https://github.com/nrel/wex) is a set of extensions to wxWidgets for custom user-interface elements developed specifically for SAM, LK script, and DView.

* [Google Test](https://github.com/google/googletest) is a C++ test framework that enables comprehensive unit-testing of software.  Contributions to the project will eventually be required to have associated unit tests written in this framework.

* [jsoncpp](https://github.com/open-source-parsers/jsoncpp) is a C++ library that allows manipulating JSON values, including serialization and deserialization to and from strings.

* [Python](https://www.python.org)/[Miniconda](https://docs.conda.io/) is for integration of Python scripts with the SAM user interface.

This repository, **SAM**, contains the code for SAM's user interface that assigns values to inputs of the SSC compute modules, runs the modules in the correct order, and displays simulation results. It also includes tools for editing LK scripts, viewing time series results, and generating shade data from a 3-dimensional representation of a photovoltaic array or solar hot water collector and nearby shading objects.

The SAM repository also includes [two libraries](https://github.com/NREL/SAM/tree/develop/Sandia) from Sandia National Laboratories, [stepwise](https://dakota.sandia.gov/content/packages/stepwise), and [LHS](https://dakota.sandia.gov/content/packages/lhs), which are distributed as part of the Dakota platform, licensed under [LGPL](https://www.gnu.org/licenses/lgpl-3.0.en.html).

# Quick Steps for Building SAM

For detailed build instructions see the [wiki](https://github.com/NREL/SAM/wiki) with specific instructions for:

  * [Windows](https://github.com/NREL/SAM/wiki/Windows-Build-Instructions)
  * [Mac](https://github.com/NREL/SAM/wiki/Mac-Build-Instructions)
  * [Linux](https://github.com/NREL/SAM/wiki/Linux-Build-Instructions)

These are the general quick steps you need to follow to set up your computer for developing SAM:

1. Set up your development tools:

    * Windows: Visual Studio 2019 Community or other editions available at https://www.visualstudio.com/.
    * Mac: Apple Command Line Tools, available at https://developer.apple.com/download/more/ (requires Apple ID and password).
    * Linux: g++ compiler available at [https://gcc.gnu.org](https://gcc.gnu.org/) or installed via your Linux distribution.

2. Download the wxWidgets 3.1.5 source code for your operating system from https://www.wxwidgets.org/downloads/.

3. Build wxWidgets.

5. In Windows, create the WXMSW3 environment variable on your computer to point to the wxWidgets installation folder, or in MacOS and Linux, create the dynamic link `/usr/<USERNAME>/local/bin/wx-config-3` to point to `/path/to/wxWidgets/bin/wx-config`.

6. As you did for wxWidgets, for each of the following projects, clone (download) the repository, build the project, and then (Windows only) create an environment variable pointing to the project folder. Build the projects in the following order, and assign the environment variable for each project before you build the next one:

<table>
<tr><th>Project</th><th>Repository URL</th><th>Windows Environment Variable</th></tr>
<tr><td>LK</td><td>https://github.com/NREL/lk</td><td>LKDIR</td></tr>
<tr><td>WEX</td><td>https://github.com/NREL/wex</td><td>WEXDIR</td></tr>
<tr><td>SSC</td><td>https://github.com/NREL/ssc</td><td>SSCDIR</td></tr>
<tr><td>SAM</td><td>https://github.com/NREL/SAM</td><td>SAMNTDIR</td></tr>
<tr><td>Google Test</td><td>https://github.com/google/googletest</td><td>GTEST</td></tr>
</table>

# Contributing

If you would like to report an issue with SAM or make a feature request, please let us know by adding a new issue on the [issues page](https://github.com/NREL/SAM/issues).

If you would like to submit code to fix an issue or add a feature, you can use GitHub to do so. Please see [Contributing](CONTRIBUTING.md) for instructions.

# License
SAM's open source code is copyrighted by the Alliance for Sustainable Energy and licensed with BSD-3-Clause terms, found [here](https://github.com/NREL/SAM/blob/develop/LICENSE).

The stepwise and LHS [LGPL](https://www.gnu.org/licenses/lgpl-3.0.en.html) licensed libraries from Sandia National Laboratories are pre-compiled Fortran libraries that are included in the SAM repository as binaries in the [Sandia folder](https://github.com/NREL/SAM/tree/develop/Sandia). You can replace the binaries with different versions by compiling your own version and replacing the binary/executable viles in the Sandia folder.

# Citing this package

System Advisor Model Version 2021.12.02 (2021.12.02). SAM source code. National Renewable Energy Laboratory. Golden, CO. Accessed December 15, 2021. https://github.com/NREL/SAM
