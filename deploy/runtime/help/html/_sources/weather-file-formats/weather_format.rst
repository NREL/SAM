Weather File Formats
====================

A SAM weather file is a text file that contains data describing the solar, wind, or marine energy resource at a particular location for a period of one year. SAM uses the following weather file formats:

* :doc:`CSV format for solar <weather_format_sam_csv_solar>`

* :doc:`CSV format for wind <weather_format_csv_wind>`

* :doc:`CSV format for marine energy <weather_format_csv_marine_energy>`

For a description of how the different performance models use weather data, see :doc:`Weather Data Elements <../weather-data/weather_data_elements>`. 

You can use a spreadsheet program, text editor, or other software to create your own SAM weather file using data from a resource measurement program, meteorological stations, or other sources.

Other Weather File Formats
~~~~~~~~~~~~~~~~~~~~~~~~~~

SAM can read solar resource data from files in the TMY3 and TMY2 legacy formats from the NLR National Solar Radiation Database (NSRDB). These older formats are from datasets that have been replaced by the latest NSRDB PSM datasets. SAM can also read weather files in the EPW format developed for the Energy Plus software.

.. note:: You can use SAM's Weather File Converter :doc:`macro <../reference/macros>` to convert from these and other weather file formats including PVsyst CSV, PVGIS 5 CSV, and SolarAnywhere to the SAM CSV format.

* The `TMY3 file format <https://nsrdb.nlr.gov/data-sets/archives.html>`__ is a comma-delimited text format with the extension .csv developed for the NSRDB 1991-2005 dataset. The first row of a TMY3 file stores data describing the location's name, and the geographic coordinates, time zone, and elevation above sea level data required for sun angle calculations. The second row stores the column headings showing units for each data element. Rows 3-8762 store weather data elements used by SAM's performance models. Many of the data elements are not used by the SAM performance models. Note. Opening and saving a TMY3 file in Excel can cause formatting changes that renders the file unreadable by SAM's weather file reader. Use SAM's :doc:`time series data <../reference/time_series_viewer>` viewer to explore the data without modifying it, or to export it to CSV or Excel files. For a complete description of the TMY3 file format, see the TMY3 user's manual (`PDF 1.7 MB <https://docs.nlr.gov/docs/fy07osti/41364.pdf>`__).

* The `TMY2 file format <https://nsrdb.nlr.gov/data-sets/archives.html>`__ is a text format with the extension .tm2 developed for the NSRDB 1961-1990 dataset. The TMY2 format is not delimited, which makes the data in the text file difficult to read. You can use SAM's time series data viewer to examine and export the data in a TMY2 file. For a description of the format see the `TMY2 user's manual <https://www.osti.gov/biblio/87130>`__.

* The EPW file format was developed for the U.S. Department of Energy's EnergyPlus building simulation model. EPW files store comma-delimited data, and use the extension .epw. The first eight rows of a file in EPW format stores header data. SAM's performance models use only the latitude, longitude, elevation, and time zone data from the header to calculate solar angles. The remaining 8,760 rows store weather data used by the SAM performance models and other data describing the quality of the data that SAM ignores. For more details about the EPW format see the `Weather Data Format Definition <https://bigladdersoftware.com/epx/docs/8-2/auxiliary-programs/epw-csv-format-inout.html>`__ page.

* The SRW format was the original wind resource format for SAM's Wind Power model. It is now obsolete and has been replaced by the `CSV format for wind <weather_format_csv_wind>`.