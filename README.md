# System Advisor Model(SAM)

The SAM Open Source Project repository contains the source code and tools required to build a desktop version of the National Renewable Energy Laboratory's System Advisor Model (SAM). SAM is a simulation program for electricity generation projects. It has models for different kinds of renewable energy systems and financial models for residential, commercial, and utility-scale projects. For more details about SAM's capabilities, see the SAM website at [https://sam.nrel.gov/](https://sam.nrel.gov/).

# Quick Steps for Building SAM

These are quick steps to set up your computer for developing SAM. For more detailed instructions for Windows, Mac, and Linux see https://github.com/NREL/SAM-private/wiki.

The build instructions are based on the following integrated development environments:

* Windows: Visual Studio 2013 (professional or express edition). Download from list of older versions at https://www.visualstudio.com/vs/older-downloads/].
* Linux: 
* Mac: Xcode 9, available at https://developer.apple.com/xcode/.

After you have set up your development environment:

1. Download the wxWidgets 3.1.0 source code for your operating system from https://www.wxwidgets.org/downloads/

3. Build wxWidgets.

3. Create the WXMSW3 environment variable on your computer to point to the wxWidgets installation folder.

2. As you did for wxWidgets, for each of the following projects, clone (download) the repository, build the project, and then create an environment variable pointing to the project folder. Build the projects in the following order, and assign the environment variable for each project before you build the next one:

	* LK: https://github.com/NREL/lk, LKDIR
	* WEX: https://github.com/NREL/wex, WEXDIR
	* SSC: https://github.com/NREL/ssc, SSCDIR
	* SAMnt: https://github.com/NREL/SAM-private, SAMNTDIR


# License

SAM is licensed uder a mixed MIT/GPL V3 [license](/license).
