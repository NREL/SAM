# Contributing to SAM

You can contribute to SAM by letting us know about problems or suggesting new features, or by making your own changes or additions to the code. You may want to help us fix an [issue someone else reported](https://github.com/NREL/SAM/issues), fix an issue you discovered, or add a new feature to SAM.

## Let us know about a problem or suggest a new feature

If you find a problem with SAM, or would like to request a new feature, let us know by [submitting an issue](https://github.com/NREL/SAM/issues/new).

If you have a question about using SAM, you can ask us on the [SAM support forum](https://sam.nrel.gov/support).

## Contribute code to the SAM project

If you are ready to contribute code to SAM, there are a couple of things you should know first:

* SAM consists of [several code repositories](https://github.com/NREL/SAM/wiki/Software-Dependencies), so you will need to determine where to make your contribution. For example, if you are making a change to the way SAM's detailed photovoltaic model makes a calculation, you would work with the [SSC](https://github.com/NREL/SSC) repository. If you are adding a new feature that changes both calculations and the user interface, like adding a new photovoltaic module model, then you would work with both the SSC repository and the SAM repository. If you need help figuring out where your contribution should go, please [let us know](mailto://sam.support@nrel.gov).

* We use GitHub to manage the open source project, so you will need to learn how to use it to fork, clone, branch, check out, pull, add, commit, and push your work. 

### Instructions

Here are the steps we would like you to follow when you contribute code to SAM:

1. Install GitHub on your computer.
1. Follow the instructions on the [SAM wiki](https://github.com/NREL/sam/wiki) to clone the SAM repositories and build SAM.
1. Create a fork on GitHub.com for the repository (SAM, SSC, LK, or WEX) you are contributing to.
1. Clone your fork to create a local copy on your computer.
1. Create a branch for your changes.
1. Make your changes to the code.
1. Build SAM and test it to make sure your code works as expected (see [below](#test-protocol)).
1. Commit and push the changes to the branch.
1. Create a pull request for us to review the branch. If the changes meet our requirements, we will merge the branch into the main repository.

### Resources for Learning GitHub

If you are new to GitHub, you can find helpful articles to help you learn how it works on the web. Some examples are:

* [Using the Fork-and-Branch Git Workflow](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/) from Scott's Weblog is a good introduction.

* [Git Concepts: Branches, Forks, and Pull Requests](http://willi.am/blog/2014/05/12/git-concepts-branches-forks-and-pull-requests/) from Will Anderson is useful, although the video on the page does not work.

* [3.2 Git Branching - Basic Branching and Merging](https://www.git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging) from the Git documentation.

* [Fork a Repo](https://help.github.com/articles/fork-a-repo/) from GitHub Help.

* [About pull requests](https://help.github.com/articles/about-pull-requests/) from GitHub Help.

### Test Protocol

We are in the process of setting up a Google Test framework for testing your contribution to ensure that it does not cause any problems with the software. 

For now, you can help to ensure that your code works with the rest of SAM by:

1. Compiling SAM with your contribution for Windows, Mac, and Linux.

3. Fixing any compiler warning messages.

3. Runing simulations in the compiled program with several configurations.

For example, if you made changes to how the weather file processor works with solar resource data, you might want to run simulations with the photovoltaic, concentrating solar power, and solar hot water heating models to make sure those models all work with your new code.
