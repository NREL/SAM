Weather Data Elements
=====================

Each performance model uses different data elements from the weather file.

The following abbreviations represent the different performance models in the table below:

* PV: :doc:`Detailed photovoltaic model <../detailed-photovoltaic-model/pv_overview>`

* PVW: :doc:`PVWatts <../pvwatts/pvwatts>`

* HCPV: :doc:`High concentration photovoltaic model <../high-concentration-photovoltaic/hcpv_overview>`

* CSP: Concentrating solar power, includes:

  * Parabolic trough: :doc:`Physical <../csp-physical-trough-model/troughphysical_overview>` and :doc:`Empirical <../csp-empirical-trough-model/troughempirical_overview>`

  * :doc:`Power tower <../csp-power-tower-molten-salt/mspt_overview>`

  * Linear fresnel: :doc:`Molten Salt <../csp-linear-fresnel-molten-salt/mslf_overview>` and :doc:`Direct Steam <../csp-linear-fresnel-direct-steam/dslf_overview>`

  * Industrial process heat: :doc:`Parabolic Trough <../iph-parabolic-trough/iph_trough>` and :doc:`Linear Direct Steam <../iph-linear-fresnel-direct-steam/iph_linear_ds>`.

  * :doc:`Generic Solar System <../csp-generic-solar/gss_overview>`

* SWH: :doc:`Solar Water Heating <../solar-water-heating/swh_overview>`

* GP: :doc:`Geothermal Power <../geothermal/geo_overview>`

* BP: :doc:`Biomass Power <../biopower/biopower>`

* WP: :doc:`Wind Power <../wind-power/wind_power>`

.. note:: The :doc:`custom generation profile <../custom-generation/custom_generation>` model does not use weather data.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Weather Data Element
     - PV
     - PVW
     - HCPV
     - CSP
     - SWH
     - GP
     - BP
     - WP\ :sup:`6`\ 
   * - Latitude  (decimal degrees)
     - •
     - •
     - •
     - •
     - •
     - 
     - 
     - 
   * - Longitude  (decimal degrees)
     - •
     - •
     - •
     - •
     - •
     - 
     - 
     - 
   * - Elevation above  sea level (m)
     - •
     - 
     - •
     - •
     - •
     - 
     - 
     - •
   * - Hour of the day
     - •
     - •
     - •
     - •
     - •
     - 
     - •
     - 
   * - Diffuse horizontal  irradiance (W/m²)
     - • \ :sup:`2`\ 
     - •
     - •
     - 
     - 
     - 
     - 
     - 
   * - Direct normal  irradiance (W/m²)
     - •
     - •
     - •
     - •
     - •
     - 
     - 
     - 
   * - Global horizontal  irradiance (W/m²)
     - • \ :sup:`2`\ 
     - 
     - 
     - 
     - •
     - 
     - 
     - 
   * - Plane-of-array irradiance (W/m²)
     - •\ :sup:`2`\ 
     - 
     - 
     - 
     - 
     - 
     - 
     - 
   * - Albedo
     - • \ :sup:`3`\ 
     - • \ :sup:`3`\ 
     - 
     - 
     - 
     - 
     - 
     - 
   * - Atmospheric  pressure (mbar)\ :sup:`1`\ 
     - •\ :sup:`5`\ 
     - 
     - 
     - •
     - 
     - •
     - •
     - •
   * - Dry bulb  temperature (°C)
     - •
     - •
     - •
     - •
     - •
     - •
     - •
     - •
   * - Dew point  temperature (°C)
     - •\ :sup:`5`\ 
     - 
     - 
     - •\ :sup:`7`\ 
     - 
     - 
     - 
     - 
   * - Wet bulb  temperature (°C)
     - •\ :sup:`5`\ 
     - 
     - 
     - •\ :sup:`8`\ 
     - 
     - •
     - •
     - 
   * - Relative humidity  (%)
     - 
     - 
     - 
     - •
     - 
     - •
     - •
     - 
   * - Wind velocity  (m/s)
     - •
     - •
     - •
     - •
     - 
     - 
     - •
     - •
   * - Wind direction  (degrees)
     - •\ :sup:`5`\ 
     - 
     - 
     - 
     - 
     - 
     - 
     - •
   * - Snow depth (cm)
     - •\ :sup:`4`\ 
     - •\ :sup:`4`\ 
     - 
     - 
     - 
     - 
     - 
     - \ :sup:`1`\ Weather files in EPW format store pressure data in pascal (Pa). SAM converts those values into millibar (mbar).

\ :sup:`2`\ For the :doc:`detailed photovoltaic model <../detailed-photovoltaic-model/pv_overview>`, the **Radiation Components** settings on the :doc:`Location and Resource <../detailed-photovoltaic-model/pv_location_and_resource>` page determine what combination of diffuse horizontal, direct normal, and global horizontal irradiance data SAM uses. The weather file may contain any two of those irradiance components. The weather file may also contain a single plane-of-array irradiance value in place of the irradiance component data... note:: The module model requires direct normal and diffuse components, and uses a decomposition model to calculate those values when needed.

\ :sup:`3`\ The :doc:`detailed photovoltaic model <../detailed-photovoltaic-model/pv_overview>` and :doc:`PVWatts model <../pvwatts/pvwatts>` use the albedo values from the weather file if the file contains albedo data, otherwise they use a default albedo value of 0.2 if the weather file does not contain the data. The :doc:`detailed photovoltaic model <../detailed-photovoltaic-model/pv_overview>` provides an additional option on the :doc:`Location and Resource <../detailed-photovoltaic-model/pv_location_and_resource>` page where you can choose whether to use the albedo value from the weather file or monthly values you specify on the input page.

\ :sup:`4`\ The :doc:`detailed photovoltaic model <../detailed-photovoltaic-model/pv_overview>` and :doc:`PVWatts model <../pvwatts/pvwatts>` can model snow losses due to snow covering the photovoltaic array when the weather file contains snow depth data.

\ :sup:`5`\ The detail photovoltaic model's heat transfer cell temperature model requires atmospheric pressure and either wet bulb or dew point temperature data along with dry bulb temperature and wind speed data. It requires wind direction data for the Gap mounting configuration with either vertical or horizontal mounting structure orientation.

\ :sup:`6`\ The :doc:`wind power <../wind-power/wind_power>` model requires wind speed, velocity, and temperature data at three different heights above the ground. See :doc:`Weather File Formats <../weather-file-formats/weather_format>` for details.

\ :sup:`7`\ When dew-point temperature data is missing from the file, SAM calculates the value from the dry-bulb temperature and relative humidity values when they are present in the file. SAM uses the *Magnus* formula approximation described in the Wikipedia `article <https://en.wikipedia.org/wiki/Dew_point#Calculating_the_dew_point>`__ on Dew Point with parameters from the 1947 *Phsychrometry and Psychrometric Charts* and the simple approximation.

\ :sup:`8`\ When wet-bulb temperature data is missing from the file, SAM calculates the value from the dry-bulb temperature, atmospheric pressure, and relative humidity values when they are all present in the file. SAM uses the method described by A. T. Martinez in the 1994 `article <http://www.ejournal.unam.mx/atm/Vol07-3/ATM07304.pdf>`__ published in the Journal *Atmósfera*.