The weather data information variables display information about data in the current weather file.

**Weather File**
  The path and name of the weather file SAM will use for simulation.

**View data**
  Display weather file data in the :doc:`time series data viewer <../reference/time_series_viewer>`.

  .. note:: You can also see the data after running a simulation on the Results page :doc:`data tables <../results/data>` and :doc:`time series graphs <../results/timeseries>`.

Header data is information in the weather file that describes the location and type of data in the file. SAM uses the following information from the header to calculate the sun position during simulations:

* **Latitude**
* **Longitude**
* **Time zone**
* **Elevation**

**Time step** is the temporal resolution of the weather file in minutes.

**Location** and **Data Source** are optional items that provide information about the source of the data that SAM does not use.

When you add a weather file to the solar resource library, SAM reads weather data from the file and calculates annual values to display for your reference. It does not use these annual values during simulations.

**Global horizontal, Direct normal (beam), Diffuse horizontal**
  The sum of solar irradiance data (W/m  \ :sup:`2`\   ) in the weather file converted to kW and divided by 365 days/year.

**Average temperature**
  The sum of temperature data (°C) in the weather file divided by the number of records in the file (8760 for hourly data).

**Average wind speed**
  The sum of wind speed data (m/s) in the weather file divided by the number of records in the file (8760 for hourly data).

**Maximum snow depth**
  If the weather file contains snow depth data, the maximum value of snow depth data (cm) in the weather file. NaN indicates the file does not contain snow depth data. Snow depth data is required for to model snow losses for the Detailed Photovoltaic and PVWatts models.

  .. note:: If your weather file does not contain snow depth data, and you are using the Detailed Photovoltaic model, you can download data from the Soiling Shading Snow page.

**Annual albedo**
  If the weather file contains albedo (ground reflectance) data, the average value of albedo values in the weather file. NaN indicates the file does not contain albedo data.
