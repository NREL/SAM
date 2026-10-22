Folders and Libraries
=====================

The resource libraries are lists of weather files on your computer. As you work with SAM, you are likely to collect weather files from different sources. Organize your weather files into folders on your computer, and then click **Add/remove weather file folders** on the Location and Resource, Wind Resource, Wave Resource, or Tidal Resource page depending on the kind of system you are modeling to open the Folder Settings window where you can manage the folders that SAM scans to create the library.

.. image:: ../images/SS_FoldeSettings-library.png
   :align: center
   :alt: SS_FoldeSettings-library.png

The list of weather file folders lists folders on your computer that contain weather files that SAM scans to create the library.

**Folder for Downloaded Files** is a folder that you designate for SAM to store weather files it downloads. SAM scans this folder in addition to the ones you add for valid weawther files. By default, the folder is `<user>/SAM Downloaded Weather Files`, where `<user>` is your computer's user folder.

.. image:: ../images/SS_FolderSettings-window.png
   :align: center
   :alt: SS_FolderSettings-window.png

SAM comes with a special weather file folder in the SAM installation folder that stores a few weather files for the default configurations that SAM uses when you create a new project or case. You should avoid storing your own weather files in this special folder because you may lose them when you remove SAM or update to a new version of the software

SAM stores list of weather files shown in the libraries in CSV files named SolarResourceData.csv, WindResourceData.csv, etc. that SAM creates when you first install it and updates as you add and remove files from the folders. These files are stored in your operating system's application data folder, which is a hidden folder used by the programs on your computer. The library files contain a list of the weather files in your weather file folders with summary information from each file that SAM displays on the Location and Resource or Wind Resource input pages. You should not modify these files. If you delete them, SAM automatically regenerates them the next time you start the software.

.. _foldersettings:

Folder Settings
~~~~~~~~~~~~~~~

The Folder Settings window allows you to manage folders that SAM scans for weather files, and to specify where to store files you download from SAM.

To add a folder to the list of folders SAM scans for weather files:

#. On the weather file input page, click **Add/remove weather file folders**.

#. In the Folder Settings window, click **Add**.

#. Navigate to the folder on your computer that contains the weather file(s).

You can add as many folders as you wish.

#. Click **OK** to return to the input page.

SAM displays the search paths you added at the end of the list of weather files.

.. image:: ../images/ss-solar_resource_folder_settings.PNG
   :align: center
   :alt: ss-solar_resource_folder_settings.PNG

To remove a folder from the list of folders SAM scans for weather files:

#. Click **Add/remove weather file folders**.

#. In the Folder Settings window, choose the name of the folder you want to remove.

#. Click **Remove**. 

Removing a search path does not delete any files or folders from your computer.

#. Click **OK**.

To specify the folder for weather files you download with the Download Weather Data feature:

#. On the weather file input page, click **Add/remove weather file folders**.

#. In the Folder Settings window, under **Folder for Downloaded Weather Files**, navigate to the folder where you want to store downloaded weather files.

#. Click **OK**.

.. image:: ../images/SS_WeatherFileFolderDownload.png
   :align: center
   :alt: SS_WeatherFileFolderDownload.png
