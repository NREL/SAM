Wave Resource
=============

The Wave Resource page is where you choose wave energy resource data, which provides information about sea states at a given location. The sea state is the significant wave height (Hs) and wave energy period (Te) at a particular time.

SAM provides two options for wave resource data:

**Frequency of Occurrence**
  Choose the Frequency of Occurrence option to model sea states as an annual probability distribution.

**Time Series**
  Choose the Time Series option to model sea states in three-hour time steps over one year.

For more about wave energy resource data and its application for wave energy converter modeling, see:

* Dallman, A.; Neary, V. (2014) Characterization of U.S. Wave Energy Converter (WEC) Test Sites: A Catalogue of Met-Ocean Data. 125 pp.; SAND2014-18206. Sandia National Laboratories. (`PDF 8 MB <https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2014/1418206.pdf>`__)

* LaBonte, A. et al. (2013). `Standardized cost and performance reporting for marine and hydrokinetic technologies <https://www.researchgate.net/publication/299410672_STANDARDIZED_COST_AND_PERFORMANCE_REPORTING_FOR_MARINE_AND_HYDROKINETIC_TECHNOLOGIES>`__. 11 pp. Proceedings of 1st Marine Energy Technology Symposium, Washington DC.

For information about sources of wave energy resource data, see the following websites:

* `Marine Energy Data Repository <https://mhkdr.openei.org/>`__
* `Water Power Technologies Office Wave Hindcast Dataset <https://www.nlr.gov/water/wave-hindcast-dataset>`__

.. _wavelibrary:

Wave Energy Resource Library
----------------------------

The wave energy resource library is a collection of wave energy resource files from folders on your computer that you have identified as containing waver energy resource files.

For a description of the file format for wave energy resource files see :doc:`CSV Format for Marine Energy <../weather-file-formats/weather_format_csv_marine_energy>`.

To add wave energy resource files to your library:

* Click **Add/remove weather file folders** and add any folders on your computer that contain wave energy resource files. See :doc:`Folders and Libraries <../weather-data/weather_manage_folders>` for details.

To choose a file from the wave energy resource library:

* Click an item in the list. You can click a column heading in the list to sort the library.

.. note:: You can see the resource data on the :doc:`Results <../getting-started/results_page>` page after you run a simulation. For the Frequency of Occurrence option, go to :doc:`Data Tables <../results/data>`, **Matrix Data**, **Frequency distribution of resource**. For the Time Series option go to  :doc:`Data Tables <../results/data>`, **Three Hour Data**, and plot **Wave height time series data** and **Wave period time series data**.

For the Frequency of Occurrence option, SAM shows dominant wave parameters calculated from the data in the diagram for reference. You can use the color coding of the table to identify the dominant wave parameters.

**Probability of dominant wave**
  The maximum frequency of occurrence in the resource scatter diagram.

**Dominant wave height, m**
  The significant wave height in meters of the maximum frequency of occurrence in the diagram.

**Dominant wave period, s**
  The wave energy period in seconds of the maximum frequency of occurrence in the diagram.

**Frequency sum, %**
  The sum of frequency of occurrence values in the diagram. The sum should be 100%, but may vary slightly from 100 due to rounding error.

Download Marine Energy Resource Files
-------------------------------------

For the Time Series option, SAM can download three-hour marine energy resource data from the `Hindcast Data Downloads web API <https://developer.nlr.gov/docs/wave/hindcast/>`__ for the U.S. West Coast, East Coast, Hawaii, and Alaska.

.. note:: The marine energy resource data download web service can be slow to respond to data requests. If you get a message about the the web service being down, try again to see if the service responds after a few tries.

To download a marine energy resource file:

#. Click **Download and add to library**.

#. Complete the information in the Wave Resource Data Download window and click **OK**. Click **Help** in the window for details.

#. After the download, SAM should add the file to your resource library and select it.

Convert Time Series Data to Frequency of Occurrence Data (JPD File Generation)
------------------------------------------------------------------------------

You can convert data from a file with time series data to frequency of occurrence (also called joint probability distribution, or JPD) data.

The JPD file generator creates a representative joint probability distribution file of significant wave height and wave period bins from time series data in a set of weather files stored in a folder on your computer. These files must be in the format provided by the High Resolution Ocean Surface Wave Hindcast. The IEC/TS 62600-100 standard recommends a minimum of 10 years of data when creating a JPD of wave resource.

Before converting time series data files to a JPD file, put your set of time series data files for a given location into single folder that contains only those files and follow the instructions below.

To convert time series data to frequency of occurrence data:

#. Choose **Time series**.

#. Click **Create JPD file from time series weather file(s)** and follow the prompts to navigate to the folder containing the file(s) and to choose a .csv file for the JPD file.

#. When the conversion finishes, switch from **Time series** to **Frequency of occurrence** and choose the JPD file from the library. You may need to click **Add/remove weather file folders** to add the folder containing the JPD file to the folders SAM scans to build the library.