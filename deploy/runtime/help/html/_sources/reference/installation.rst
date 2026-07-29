Software Installation
=====================

To install SAM, download the package for your computer's operating system from the `SAM website's Download page <https://sam.nlr.gov/download>`__, and follow the installation instructions.

System Requirements
~~~~~~~~~~~~~~~~~~~

SAM runs on most recent versions of Windows, Mac, and Linux operating systems. For information about requirements for different versions of SAM, see `SAM website Download page <https://sam.nlr.gov/download>`__. SAM requires an internet connection for :doc:`registration <registration>` and :doc:`access to online databases <configure_proxy_server>`.

.. _installationfolder:

SAM Installation Folder
~~~~~~~~~~~~~~~~~~~~~~~

Your SAM installation contains the SAM executable file and supporting files and folders. You may need to access the SAM installation to find resource data files, SAM libraries, or to :ref:`remove SAM <removesam>` from your computer. Be careful not to modify, move, or delete any files in the folder unless you understand how SAM uses the file.

The SAM installation folder is the folder that contains the SAM executable and its supporting files:

**Windows**

By default, the SAM installation is in c:\SAM\, although you can install SAM in any folder on your computer.

**Mac**

On a Mac, the SAM installation is the SAM application bundle, which is distributed on a disk image named sam-osx.dmg.

SAM runs directly from the disk image, which can be in the Applications folder, desktop, or other folder on your computer.

To open the application bundle, in Finder, control-click (or right-click) the SAM name and click **Show Contents**. If SAM is in your Applications folder, command-click the name to open it in Finder.

**Linux**

In Linux, SAM installs by default in home/<user>/SAM/, although you can install SAM in any folder on your computer.

.. _removesam:

Removing SAM from your computer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you install a new version of SAM or no longer need the software, you can remove it from your computer:

**Windows**

#. In the Windows Control Panel, under **Programs**, click **Uninstall a Program** (Control Panel\Programs\Programs and Features  ), and select the SAM version you want to uninstall from the list. The SAM program is listed as SAM <version number>.

#. Click **Uninstall**.

.. note:: Any files in the :ref:`SAM installation folder <installationfolder>` that you added or modified are not removed by the uninstall process. You can safely delete the folder if you do not need those files.

**Mac**

* Drag the SAM application bundle to the Trash.

**Linux**

* Open a terminal window and run   *rm -rf /path/to/SAM/*  .

Running multiple versions of SAM
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can keep different versions of SAM installed on your computer and run them simultaneously as separate pieces of software. When you install a new version of SAM, unless you install it in the same folder as another version of SAM, it installs as a separate installation. You may want to keep multiple versions of SAM on your computer to compare results between versions or if your work with SAM requires that you use a specific version.

Installing SAM in a laboratory environment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are installing SAM on more than one computer in a teaching laboratory or other setting, please `email us <mailto:sam.support@nlr.gov?subject=SAM%20HELP:%20Registration%20in%20laboratory%20environment>`__ for instructions to register the entire laboratory with a single key.
