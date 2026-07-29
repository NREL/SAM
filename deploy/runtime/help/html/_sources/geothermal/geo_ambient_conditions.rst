Ambient Conditions
==================

The Ambient Conditions page allows you to choose a weather file to specify ambient conditions for the :doc:`geothermal <geo_overview>` system's power block model when you specify either **Power Block Monthly** or **Power Block Hourly** as the Model option on the Power Block page.

The geothermal resource is specified on the :doc:`Resource <geo_geothermal_resource>` page.

.. note:: You do not need to specify a weather file with the GETEM power block option. With the GETEM power block option, SAM ignores the weather file you choose on the Ambient Conditions page.

The geothermal performance model runs a simulation over the life of the plant (defined by **Analysis Period** on the Financial Parameters page) in order to account for the annual decline in resource temperature. SAM assumes that the data in the weather file represents typical ambient conditions at the power block over the entire analysis period. Because the weather file contains data for a single year, SAM reads data from the weather file multiple times to complete the multi-year simulation:

* For an hourly simulation (**Power Block Hourly** option on the :doc:`Power Block <geo_power_block>` page), SAM reads hourly data from the weather file, and uses it to represent ambient conditions in each hour for each year of the analysis period. For example, for an analysis period of 30 years, SAM would use the same temperature, pressure, and humidity values for each July 2nd at 2 pm for each of the 30 years.

* For a monthly simulation (**Power Block Monthly** option on the :doc:`Power Block <geo_power_block>` page), SAM calculates average temperature, pressure, and humidity values from the hourly values in the weather file, and uses them to represent the average ambient conditions for each month of the year. SAM uses the same set of twelve monthly average values for each year of the plant's life.

.. note:: The geothermal model uses weather data from a weather file formatted in the :doc:`SAM CSV for solar format <../weather-file-formats/weather_format_sam_csv_solar>`. The inputs on the Ambient Conditions page are the same as those on the Location and Resource page of SAM's solar power models and use nomenclature from the solar power models. 

.. include:: ../includes/snip_location_resource.rst

Download Weather Files
~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_download_weather_files.rst

Choose Weather File
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_choose_weather_file.rst

Weather Data Information
~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_weather_data_information.rst
