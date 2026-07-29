Ambient Conditions
==================

The Ambient Conditions page allows you to choose a weather file to specify ambient conditions for the Steam Rankine Cycle for a biomass power system. The weather file contains solar resource data, but is also suitable for the biomass power model.

The :doc:`biomass power <biopower>` model uses data from a weather file to describe ambient conditions for the :doc:`Rankine steam cycle <biopower_plant_specs>`, and for modeling feedstock drying. It uses separate data set to describe the biomass resource (:doc:`feedstock <biopower_feedstock>`). The geographical coordinates in the weather file determines the location for the :doc:`feedstock <biopower_feedstock>`.

Ambient conditions also affect biomass composition, but on a monthly rather than hourly timescale. SAM calculates average monthly temperature, pressure, and humidity values from the hourly values in the weather file, and uses those values to represent the average ambient conditions for each month of the year. SAM uses the same set of twelve monthly average values for each year of the plant's life.

.. note:: The biomass power model requires a weather file with global horizontal irradiance (GHI) and relative humidity data that is not available in all files from the National Solar Radiation Database (NSRDB).

:ref:`Typical meteorological year (TMY) <typicalyear>` files from the NSRDB PSM V3 dataset do not have relative humidity data, but the single year data does. To download a single year file, use the choose year option to download a file as described under "Download Weather Files" below.

.. note:: The legacy NSRDB MTS1 (NSRDB 1961 - 1990 TMY2) weather files have both GHI and relative humidity data, but the MTS1 data is out of date and only has files for a few hundred weather stations in the United States.

   You can access these legacy files from the `NSRDB archives <https://nsrdb.nlr.gov/data-sets/archives.html>`__.

   Please contact SAM Support at `sam.support@nlr.gov <mailto:sam.support@nlr.gov>`__ if you need help finding weather data to use for your project.

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