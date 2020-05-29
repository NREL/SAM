# System Advisor Model (SAM)
[![TravisCI](https://travis-ci.org/NREL/SAM.svg?branch=develop)](https://travis-ci.org/NREL/SAM)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2FNREL%2FSAM.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2FNREL%2FSAM?ref=badge_shield)

The SAM Open Source Project repository contains the source code, tools, and instructions to build a desktop version of the National Renewable Energy Laboratory's System Advisor Model (SAM). SAM is a simulation program for electricity generation projects. It has models for different kinds of renewable energy systems and financial models for residential, commercial, and utility-scale projects. For more details about SAM's capabilities, see the SAM website at [https://sam.nrel.gov/](https://sam.nrel.gov/).

For a short video describing the SAM repositories, see https://youtu.be/E5z1iiZfZ3M.

The [SAM release notes](https://nrel.github.io/SAM/doc/releasenotes.html) are in https://github.com/NREL/SAM/blob/gh-pages/doc/releasenotes.html.

The desktop version of SAM for Windows, Mac, or Linux builds from the following open source projects:

* [SSC](https://github.com/nrel/ssc) is a set of "compute modules" that simulate different kinds of power systems and financial structures. It can be run directly using the [SAM Software Development Kit](https://sam.nrel.gov/sdk). **If you are looking for the algorithms underlying the models, they are located in this repository.**

* [LK](https://github.com/nrel/lk) is a scripting language that is integrated into SAM and allows users to add functionality to the program.

* [wxWidgets](https://www.wxwidgets.org/) is a cross-platform graphical user interface platform used for SAM's user interface, and for the development tools included with SSC (SDKtool) and LK (LKscript). The current version of SAM uses wxWidgets 3.1.1.

* [WEX](https://github.com/nrel/wex) is a set of extensions to wxWidgets for custom user-interface elements used by SAM, and by LKscript and DView, which are integrated into SAM.

* [Google Test](https://github.com/google/googletest) is a C++ test framework that enables comprehensive unit-testing of software.  Contributions to the project will eventually be required to have associated unit tests written in this framework.

* [jsoncpp](https://github.com/open-source-parsers/jsoncpp) is a C++ library that allows manipulating JSON values, including serialization and deserialization to and from strings.

* [Python](https://www.python.org)/[Miniconda](https://docs.conda.io/) Necessary additional information is included in the install directory.


* This repository, **SAM**, provides the user interface to assign values to inputs of the SSC compute modules, run the modules in the correct order, and display simulation results. It also includes tools for editing LK scripts, viewing time series results, and generating shade data from a 3-dimensional representation of a photovoltaic array or solar hot water collector and nearby shading objects.

We also include two Sandia libraries, [stepwise](https://dakota.sandia.gov/content/packages/stepwise), and [LHS](https://dakota.sandia.gov/content/packages/lhs), which are distributed as part of the Dakota platform, licensed under [LGPL](https://www.gnu.org/licenses/lgpl-3.0.en.html).  These libraries may be found [here](https://github.com/NREL/SAM/tree/develop/Sandia).


# Quick Steps for Building SAM

For detailed build instructions see the [wiki](https://github.com/NREL/SAM/wiki), with specific instructions for:

  * [Windows](https://github.com/NREL/SAM/wiki/Windows-Build-Instructions)
  * [Mac](https://github.com/NREL/SAM/wiki/Mac-Build-Instructions)
  * [Linux](https://github.com/NREL/SAM/wiki/Linux-Build-Instructions)

These are the general quick steps you need to follow to set up your computer for developing SAM:

1. Set up your development tools:

    * Windows: Visual Studio 2017 Community or other editions available at https://www.visualstudio.com/.
    * Mac: Apple Command Line Tools, available at https://developer.apple.com/download/more/ (requires Apple ID and password).
    * Linux: g++ compiler available at http://www.cprogramming.com/g++.html or as part of the Linux distribution.

2. Download the wxWidgets 3.1.1 source code for your operating system from https://www.wxwidgets.org/downloads/.

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

SAM also includes two [LGPL](https://www.gnu.org/licenses/lgpl-3.0.en.html) licensed libraries from Sandia National Laboratory.  These Fortran libraries have been pre-compiled and included as binaries [here](https://github.com/NREL/SAM/tree/develop/Sandia).  To swap in a new version of these libraries, you may compile them as binary/executables and copy them into the respective folder.

# Citing this package

System Advisor Model Version 2020.2.29 (2020.2.29). SAM source code. National Renewable Energy Laboratory. Golden, CO. Accessed May 27, 2020. https://github.com/NREL/ssc
