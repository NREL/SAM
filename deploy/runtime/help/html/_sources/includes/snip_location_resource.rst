
The Location and Resource page is where you choose a weather file for the simulation. It provides access to the solar resource library and tools for downloading files from the National Solar Radiation Database (NSRDB).

The solar resource library is a list of CSV files with valid and correctly formatted solar resource data. SAM builds the library from the folders that you identify as containing weather files. When you first install SAM, it comes with a few default weather files stored in the `<user>/SAM Downloaded Weather Files` folder that are automatically added to the library. As you use SAM for your own projects, you can add and remove folders to manage the library.

There are two ways to add weather files to your solar resource library:

* Use the **Download** button or the Advanced NSRDB Options window to download files from the NSRDB and add them to your library.

* Use the **Add/remove weather file folders** button to add files that are on your computer.

.. tip:: You may want to model your system using weather data for several different locations around your project site, and if available, from different data sources to understand how sensitive your analysis results are to the weather assumptions, and how much variation there is in the data from the different weather files.

   You can compare results for a system using more than one weather file in a single case by using SAM's :doc:`parametric simulation <../simulation-options/parametrics>` option.

   For a list of sources of solar resource data other than the NSRDB, see the `Weather Data page on the SAM website <https://sam.nlr.gov/weather-data.html>`__.

   For a helpful discussion of weather data and power system simulation, see the `Best Practices Handbook for the Collection and Use of Solar Resource Data for Solar Energy Applications <https://doi.org/10.69766/ENEH5295>`__.

.. seealso::

   * :doc:`Weather File Formats <../weather-file-formats/weather_format>`
   * :doc:`Weather Data Elements <../weather-data/weather_data_elements>`
   * :doc:`Typical and Single Year <../weather-data/weather_typical_single>`
   * :doc:`Time and Sun Position <../weather-data/weather_time_convention>`
   * :doc:`Folders and Libraries <../weather-data/weather_manage_folders>`
   * :doc:`Weather Data and LK <../weather-data/accessing-weather-data-from-lk>`

 




