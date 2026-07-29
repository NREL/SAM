You can choose a weather file from the solar resource library, or you can disable the library and choose a file directly from a folder on your computer.

**Solar Resource Library**
  SAM's solar resource library displays information from weather files in your solar resource data folders. As you use SAM, use **Add/remove weather file folders** to build a library of files for locations you frequently use as described in :doc:`Folders and Libraries <../weather-data/weather_manage_folders>`.

  To choose a file from the solar resource library, click the location name in the list. You can type a few letters of the file name in the Search box to filter the list.

  The solar resource library is disabled when you check **Choose weather file**.

  .. note::  SAM comes with a few weather files that it loads when you create a new .sam file. These files are always in your library and are required for SAM to work properly.

**Weather file from library**
  The full path and file name for the currently selected file in the library.

**Add/remove weather file folders**
  Click the button to open the :doc:`Solar Resource Data Folder Settings window <../weather-data/weather_manage_folders>` where you choose folders on your computer that SAM scans to build the solar resource library. SAM adds any files it can identify as valid weather files in each folder you specify to the library.

  Before adding a file to the library, SAM checks the data in the file displays a message if it finds any problems with the data in the file.

  SAM only adds valid weather files to the library. If you add a folder that contains CSV files that are not in the SAM CSV format, it will not add those files to the library.

  The list of solar resource folders are the folders that SAM scans for weather files to build the solar resource library.

**Refresh library**
  Refresh the library after adding files to the weather file folder. In most cases, SAM should automatically refresh the library as needed, but you do not see files you just added to the library, click the button to refresh it.

**Choose a weather file**
  Check this box to disable the solar resource library and enable the **Browse** button.

**Browse**
  When the library is disabled, click **Browse** to choose a weather file directly from your computer. SAM checks the file and displays a message if it finds problems with the data in the file.