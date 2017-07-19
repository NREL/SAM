# System Advisor Model(SAM)

The SAM Open Source Project repository contains the source code, tools, and instructions to build a desktop version of the National Renewable Energy Laboratory's System Advisor Model (SAM). SAM is a simulation program for electricity generation projects. It has models for different kinds of renewable energy systems and financial models for residential, commercial, and utility-scale projects. For more details about SAM's capabilities, see the SAM website at [https://sam.nrel.gov/](https://sam.nrel.gov/).

The desktop version of SAM for Windows, Mac, or Linux builds from the following open source projects:

* [SSC](https://github.com/nrel/ssc) is a set of "compute modules" that simulate different kinds of power systems and financial structures. It can be run directly using the [SAM Sofware Develoment Kit](https://sam.nrel.gov/sdk).

* [LK](https://github.com/nrel/lk) is a scripting language that is integrated into SAM and allows users to add functionality to the program.

* [wxWidgets](https://www.wxwidgets.org/) is a cross-platform graphical user interface platform used for SAM's user interface, and for the development tools included with SSC (SDKtool) and LK (LKscript).

* [WEX](https://github.com/nrel/wex) is a set of exensions to wxWidgets for custom user-interface elements used by SAM, and by LKscript and DView, which are integrated into SAM.

* *SAM* provides the user interface to assign values to inputs of the SSC compute modules, run the modules in the correct order, and display simulation results. It also includes tools for editing LK scripts, viewing time series results, and generating shade data from a 3-dimenional representation of a photovoltaic array or solar hot water collector and nearby shading objects.

# Quick Steps for Building SAM

These are the general quick steps you need to follow to set up your computer for developing SAM. For more detailed instructions specific to Windows, Mac, and Linux see https://github.com/NREL/SAM-private/wiki.

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

# Contributing

If you have found an issue with SAM or would like to make a feature request, please let us know by adding a new issue on the issues page.

If you would like to submit code to fix an issue or add a feature, you can use GitHub to do so. The overall steps are to create a fork on GitHub.com using the link above, and then install GitHub on your computer and use it to clone your fork, create a branch for your changes, and then once you have made your changes, commit and push the changes to your fork. You can then create a pull request that we will review and merge into the repository if approved.

We have not yet set up a test framework for the open-source project. We eventually hope to create a framework that will automatically test your commits to ensure that your contribution does not cause any problems with the software. For now, you can help to ensure that your code works with the rest of SAM by:

1. Compiling SAM with your contribution for Windows, Mac, and Linux and running simulations in compiled program with several configurations. For example, if you made changes to how weather file processor works with solar resource data, you might want to run simulations with the different photovoltaic, concentrating solar power, and solar hot water heating model to make sure those models all work with your new code.

2. Making sure your code does not generate additional compiler warning messages.

SAM currently generates so many warnings during compilation that it is nearly impossible to use them to help debug your code. across it's multiple projects that they are borderline useless for debugging problems. One useful contribution to the project would be to clean up the source code to eliminate those warnings.

# License

SAM is licensed uder a mixed MIT/GPL V3 [license](LICENSE.md).
