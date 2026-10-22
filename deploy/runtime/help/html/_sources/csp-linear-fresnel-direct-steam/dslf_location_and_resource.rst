Location and Resource
=====================

.. include:: ../includes/snip_location_resource.rst

.. note:: SAM's CSP models assume that each DNI value in the weather file represents the average irradiance over the time step, and calculate sun angles for the midpoint of the time step. For hourly weather files from the NSRDB, the irradiance data for each time step is measured instantaneously at the time indicated by the minute column. For hourly data, minute = 30, so the irradiance data is measured at midpoint of each hour. For subhourly data, the irradiance data is measured at the beginning of each time step. As a result, if you use an NSRDB weather file with a CSP model with subhourly data, the sun angle for each time step will be for a different time than the time for the instantaneous irradiance values. See :doc:`Time Convention and Sun Position <../weather-data/weather_time_convention>` for additional details.

Download Weather Files
~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_download_weather_files.rst

Choose Weather File
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_choose_weather_file.rst

Weather Data Information
~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_weather_data_information.rst