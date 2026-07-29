NSRDB Advanced Download
=======================

The NSRDB Download window provides access to the `NLR National Solar Radiation Database (NSRDB) <https://developer.nlr.gov/docs/solar/nsrdb/>`__ for SAM's solar power models. When you submit a query, it lists all NSRDB weather files available for the different NSRDB datasets for the given location.

To open the Advanced NSRDB Download window:

#. On the Location and Resource page for one of the solar models, click **Advanced download**.

#. Click **Download and add to library**.

The advanced download window provides access to the following NSRDB datasets:

* Typical meteorological year (TMY) files with 60-minute data from the PSM V3 TMY dataset. These files are constructed from historical data between 1998 and the current year, and are available for TMY-2016 to the most recent year in the database. These files are also available from standard downloads on the Solar Resource page.

* Single-year files with 30- or 60-minute data for 1998 up to the most recent year from the PSM V3 dataset. These files are also available from standard downloads on the Solar Resource page.

* Single-year files with 5-, 15-, 30-, or 60-minute data for 2018 up to the most recent year from the PSM V3 Five Minute Temporal Resolution dataset.

* Single-year files with 15-, 30-, or 60-minute data for 2017 up to the most recent year for locations outside of the Western Hemisphere from the PSM MSG IODC dataset.

* TMY data with 60-minute data constructed from historical data between 2000 and 2014 from the SUNY International TMY dataset. The SUNY International data covers India, Bangladesh, Bhutan, Nepal, Sri Lanka, and parts of Pakistan and Afghanistan.

* Single-year files with 60-minute data for 2000 - 2014 from the SUNY International Dataset.

Since NLR began producing the PSM data in 2016, it updates the PSM files every year to add data from previous years, so the date range and available TMY and TGY files changes over time. For example, as of late 2020, PSM data was available for 1998 through 2019, and included TMY-2017 and TGY-2017 files processed from the 1998 - 2017 data, and TMY-2018 and TGY-2018 files processed from the 1998 - 2018 data, etc., up to 2019.

Typical year data is available as typical meteorological year (TMY), typical global horizontal irradiance year (TGY), and typical direct normal irradiance year (TDY). For a discussion of the difference between these files, see Habte (2014) "Temporal and Spatial Comparison of Gridded TMY, TDY, and TGY Data Sets" available from the `SAM website Weather Data Publications page <https://sam.nlr.gov/weather-data/weather-data-publications.html>`__.

.. note:: Legacy NSRDB datasets are not available from the NSRDB web service. You can find links to archived data on the `SAM website Weather Data page <https://sam.nlr.gov/weather-data.html>`__.

Downloading Data
~~~~~~~~~~~~~~~~

To download one or more weather files using the Advanced NSRDB Download window:

#. The location is automatically populated from the location information on the Location and Resource page.

To change the location, type a new location and click **Find**. The location can be a location name, street address, or latitude-longitude pair. For example:

* Golden, Colorado

* golden, co

* 15013 Denver West Parkway, Golden CO 80401

* 80401

* thimpu, bhutan

#. In the list of available files, check the box for each file you want to download. Use the file name to identify the type of data it contains:

* *psm3*, *psm3-tmy*, *psm3-5min*, *msg-iodc*, *suny-india, or suny-india-tmy*, indicates the dataset.

* *5, 15, 30*, *60* indicates the temporal resolution.

* *tmy*, *tgy, tdy* indicates typical meteorological year, typical global horizontal irradiance year, or typical direct normal irradiance year, respectively.

* *1998*, *1999*, etc. indicates the year

You can use the or Filter box or check boxes to filter the list.

.. image:: ../images/SS_NSRDB-select-filtered.png
   :align: center
   :alt: SS_NSRDB-select-filtered.png

#. By default, SAM downloads files to your default weather file folder, which is *<user>/SAM Downloaded Weather Files*. To change the download path, type a different folder name, or click the ellipses (**..**.) button.

#. Click **OK** to start the download.

SAM connects to the NSRDB online database and downloads all checked files to the folder you specified. When the download finishes, SAM adds the downloaded files to your solar resource library so you can find them in the list on the Location and Resource page.