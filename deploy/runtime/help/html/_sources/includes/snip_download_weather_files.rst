
The Download Weather File options are for downloading weather files from the National Solar Radiation Database (NSRDB). For information about other sources, see the `Weather Data <https://sam.nlr.gov/weather-data>`__ page on the SAM website.

**Type an address or coordinates to download latest NSRDB data**
  Choose this option to download files from the latest available dataset for one location or for a list of locations. You can download a TMY file, a file for a specific year, or files for all available years.

**Use Advanced NSRDB Options window to access all available data**
  Choose this option to download 5-minute or 15-minute data for a specific year, or to download files from a specific NSRDB dataset. The :doc:`Advanced NSRDB Options window <../window-reference/win_nsrdb_advanced_download>` displays a list of files from all available datasets for a given location that you can choose from to download. You can also download files directly from the NSRDB using the `NSRDB Viewer <https://nsrdb.nlr.gov/>`__.

**Download**
  The **Download** button is available when you choose **Type an address or coordinates to download NSRBD data**. Click the button to start the download after typing an address.

**Advanced NSRDB Options**
  The **Advanced NSRDB Options** button is available when you choose **Use Advanced NSRDB Options window to access all available data**. Click the button to open the :doc:`Advanced NSRDB Options window <../window-reference/win_nsrdb_advanced_download>`.

The following inputs are only visible for the **Type an address or coordinates to download latest NSRDB data** option.

**Address or coordinates**
  Type a street address, city and state or country name, zip code, or latitude longitude pair. For example:

  * Golden, Colorado
  * golden, co
  * 15013 Denver West Parkway, Golden CO 80401
  * 80401
  * thimpu, bhutan

**File options**
  Choose the files you want to download from the NSRDB.

  **Default TMY file** downloads a `typical meteorological year <https://nsrdb.nlr.gov/data-sets/tmy>`__ weather file for the location or locations you specified. TMY data is appropriate when you are using simulation results for a single year to make cash flow calculations over a multi-year period.

  **Choose year** displays a list of available years when you click **Download** so you can choose a single-year file to download. Single-year files are appropriate when you want to see how the system would perform in a particular year. For example, for a residential or commercial system, if you have measured building load data for a given year, you could use weather data for the same year to see how the system's power output compares to the load.

  **Download files for all years (P50/P90)** downloads a set of files for all available years. When you click **Download**, SAM creates a folder in your weather file download folder to store the single-year files based on the location address or latitude-longitude pair you typed. It also downloads the typical meteorological year (TMY) file for the location and stores it in your weather file download folder. SAM appends an underscore with the year to the single-year file names so that the P50 P90 simulations work correctly. See :doc:`P50/P90 Simulations <../simulation-options/p50p90>` for details. If you choose to download files for all years with **Multiple locations**, SAM downloads a set of files for each location, and puts the files for each location in a separate folder.

**One location, Multiple locations**
  Choose **One location** to download a weather file for a single location. Choose **Multiple locations** to download weather files for more than one location.

**List of locations**
  The list of locations for the **Multiple locations** option. The button is disabled for the **One location option**. Click **Edit data** to open the Edit Data window where you can enter the list of locations. When the window opens, click **Number of Values** to enter the number of locations in the list, and then type or import a list of addresses and/or coordinates.

**60-minute, 30-minute**
  Choose **60-minute** to download weather files with hourly time steps or 8,760 data records. Choose **30-minute** to download files with 30-minute time steps or 17,520 data records. This option is disabled for the **Default TMY file** option because TMY files contain hourly data. (To download 5- or 15-minute data, click **Use Advanced Options window to access all available data**).

**Show Download Log**
  Click this button to open the `<user>/SAM Downloaded Weather Files/sam_nsrdb_download_log.txt` file. This file is a running log of your NSRDB downloads and provides information that can be helpful for troubleshooting weather file downloads.

.. note:: Downloading multiple weather files from the NSRDB can take a long time. If you download files for multiple locations or years, be prepared to wait for the files to download. The download log file reports the time it takes for each file to download. Download times depend on your internet connection and computer. For a new computer with a fast connection, downloading and processing a single file takes about 10 seconds.
    
   The NSRDB API on the NLR Developer Network that SAM uses for weather file downloads requires an email address to download weather file. When you download a weather file from SAM, SAM submits the email address that you used to :doc:`register SAM <../reference/registration>`.

   When you download a file using the **Type an address or coordinates to download latest NSRDB data** option, SAM uses the `NSRDB Data Query <https://developer.nlr.gov/docs/solar/nsrdb/nsrdb_data_query/>`__ API on the NLR Developer Network to identify available `NSRDB datasets <https://developer.nlr.gov/docs/solar/nsrdb/>`__ for a the requested location, and then downloads a file from one of the following datasets depending on the location. For TMY files:
   
   * GOES TMY PSM v4.0.0
   * Himarawi TMY PSM v3
   * NSRDB Polar Typical Meteorological Year v4.0.0
   * Meteosat Prime Meridian TMY PSM v4
   
   And, for single-year files:
   
   * GOES Aggregated PSM v4.0
   * Himawari 2016-2020 PSM v3
   * METEOSAT IODC Region PSM v3
   * NSRDB Polar v4.0.0
   * GOES Full Disc PSM v4 (only if files are not available for the location in GOES Aggregated PSM v4).
   
   SAM uses the Mapquest geocoding service through a private NLR Developer Network API to identify the geographic coordinates (latitude and longitude) of a location when you type a street address or city and state name to identify the location.


