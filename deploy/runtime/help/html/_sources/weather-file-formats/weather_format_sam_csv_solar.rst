SAM CSV Format for Solar
========================

The SAM CSV format is a comma-separated text format for SAM's solar performance models. It uses a standard CSV format that you can edit in any spreadsheet program, text editor, or other appropriate software.

.. image:: ../images/SS_SolarResource-samcsv-file.png
   :align: center
   :alt: SS_SolarResource-samcsv-file.png

For examples of files in the SAM CSV format, see files in the *solar_resource* folder of your :ref:`SAM installation folder <installationfolder>`.

The SAM CSV format supports hourly and subhourly data with up to a one-minute resolution. SAM recognizes the weather file's time resolution based on the number of data rows in the weather file. For example, SAM recognizes a file with 8,670 data rows as an hourly data file, and a file with 35,040 rows as 15-minute data. A minute column is optional for hourly data, and required for sub-hourly data. By default, for hourly data SAM calculates sun position angles at the mid-point of the hour. However, if you specify the optional minute column for the hourly data, SAM calculates the sun position at the minute indicated in that column. For sub-hourly data, sun angles are calculated for the minute indicated in the Minute column. See :doc:`Time Convention and Sun Position <../weather-data/weather_time_convention>` for additional details.

SAM requires a valid value for all time steps for each data element. It does not fill data gaps. It does perform some checks on the weather data before running a simulation, and displays messages about problems with the data in the simulation :doc:`notices <../results/notices>`. You can also run the **Solar Resource File Checker** :doc:`macro <../reference/macros>` to perform the checks without running a simulation.

Header
~~~~~~

The first two rows are header rows that provide location information and metadata, and identify the data columns.

Row 1
-----

Row 1 contains labels for the location data and metadata, and must include at least the following, which may be in any order and with any capitalization:

* *Latitude*

* *Longitude*

* *Time zone*

* *Elevation*

The optional **Units flag** indicates that your weather file includes an extra header row for measurement units. By default, the SAM CSV format does not include a row for measurement units in the header. To include a units row in the header, add *hasunits* to Row 1, and set its value in Row 2 to *yes*. Then add a row after the data column heading row (described as Row 3 below) with units for each column. If *hasunits* is *no*, or is omitted from Row 1, SAM assumes that the header does not include a row for units. SAM ignores the information in the units row, but it can be useful to help you keep track of units.

For example, a valid Row 1 might look like this:

*Source,Location ID,City,State,Region,Country,Latitude,Longitude,Time Zone,Elevation*

See the table below for a complete list of Row 1 header fields, accepted labels, and and units. 

Row 2
-----

Row 2 contains values for the location data and metadata identified by the labels in Row 1. The required latitude, longitude, time zone, and elevation are numbers that SAM uses in sun position calculations during simulations:

* Latitude is a value between -90.0 and 90.0 decimal degrees north of the equator, e.g., 39.75 for Denver, and -33.95 for Sydney, Australia.

* Longitude is a value between -180.0 and 180.0 decimal degrees East of the prime meridian, e.g., -104.95 for Denver, and 116.28 for Beijing, China.

* Time zone is for standard time in hours ahead of GMT, e.g., -7 for Denver, and 5.5 for India.

* Elevation is in meters above sea level, e.g., 1615 for Denver.

 For example, given the Row 1 example above, Row 2 for Phoenix, Arizona might look like this:

*TMY2,23183,Phoenix,AZ,USA,33.433333,-112.016667,-7,339*

The remaining header fields, such as source, location ID, city, state, and others listed in the table below are text values (strings) that provide information about the data, but that SAM does not use in calculations. These values are not required for SAM simulations, and can be omitted from the weather file. 

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Header Field
     - Units
     - Accepted Labels for Row 1
   * - Latitude
     - degrees
     - *latitude, lat*
   * - Longitude
     - degrees
     - *longitude, lon, long, lng*
   * - Time zone
     - hours offset from GMT
     - *tz, timezone, time zone*
   * - Site elevation
     - meters above sea level
     - *el, elev, elevation, site elevation*
   * - Year
     - n/a
     - *year*
   * - Location ID
     - n/a
     - *id, location, location id, station,station id, wban, wban#*
   * - City
     - n/a
     - *city*
   * - State
     - n/a
     - *state, province, region*
   * - Country
     - n/a
     - *country*
   * - Source
     - n/a
     - *source, src*
   * - Description
     - n/a
     - *description, desc*
   * - URL
     - n/a
     - *url*
   * - Units flag
     - yes or no
     - *hasunits, units*
   * - Version
     - n/a
     - *version*

Solar Resource Data
~~~~~~~~~~~~~~~~~~~

Rows 3 and higher contain solar resource data.

Row 3
-----

Labels identifying the data columns. SAM uses the labels to identify the columns, so they can be in any order. SAM requires a complete column of data for each data element. The Minute column is optional for hourly data. 

For example, a valid Row 3 might look like this::

    Year,Month,Day,Hour,Minute,GHI,DNI,DHI,Tdry,Tdew,RH,Pres,Wspd,Wdir,Snow Depth

See the table below for a complete list of valid Row 3 header values and units. See the description of the **Units flag** above if you want to include an additional row for units in your file.

Row 4-8,760 (for hourly data, more rows for sub-hourly data)

Data identified in Row 3. For example::

    1988,1,1,0,0,0,0,5.6,-3.3,53,983,2.1,200,0

SAM assumes that the weather data uses the following units:

.. note:: The format does not require leading zeros in one-digit numbers.

* Year is a four-digit integer (1988)

* Month is a one- or two-digit integer (Month = 1 is January)

* Day is a one- or two-digit integer indicating the day of month (Day = 1 is the first day of the month).

* Hour is a one- or two-digit integer indicating the hour of day (Hour = 0 is the first hour of the day, or the hour ending at 1 am).

* Minute is a one-or two-digit integer indicating the minute (Minute = 0 is the first minute of the hour). This value is required for sub-hourly data, and optional for hourly data. For hourly data, the minute column indicates the time you want SAM to use for sun position calculations. If you omit the value for hourly data, SAM uses the midpoint of the hour for the calculations. See :doc:`Time Convention and Sun Position <../weather-data/weather_time_convention>` for details.

* Solar irradiance in W/m\ :sup:`2`\ . SAM's photovoltaic models assume instantaneous irradiance. The CSP models assume average irradiance over the time step.

* Temperature in °C.

* Relative humidity in %.

* Atmospheric pressure in millibar.

* Wind speed at 10 meters above the ground in m/s.

* Wind direction at 10 meters above the ground in degrees east of North, with zero degrees indicating wind from the north.

* Snow depth in centimeters.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Data Field
     - Units
     - Accepted Labels for Row 3
   * - Year
     - 1950-2050
     - *year, yr*
   * - Month
     - 1-12
     - *month, mo*
   * - Day
     - 1-31
     - *day*
   * - Hour
     - 0-23
     - *hour, hr*
   * - Minute
     - 0-59
     - *min, minute*
   * - Global horizontal irradiance
     - W/m²
     - *gh, ghi, global, global horizontal, global horizontal irradiance*
   * - Beam normal irradiance
     - W/m²
     - *dn, dni, beam, direct normal, direct normal irradiance*
   * - Diffuse horizontal irradiance
     - W/m²
     - *df, dhi, diffuse, diffuse horizontal, diffuse horizontal irradiance*
   * - Plane-of-array irradiance
     - W/m²
     - *poa, pa, plane,plane of array,plane of array irradiance, poa_global*
   * - Ambient dry bulb temperature
     - °C
     - *tdry, dry bulb, dry bulb temp, temperature, ambient, ambient temp, temp_air*
   * - Wet bulb temperature
     - °C
     - *twet, wet bulb, wet bulb temperature*
   * - Dew point temperature
     - °C
     - *Tdew, dew point, dew point temperature, temp_dew*
   * - Wind speed
     - m/s
     - *wspd, wind speed, wind_speed*
   * - Wind direction
     - degrees
     - *wdir, wind direction, wind_direction*
   * - Relative humidity
     - %
     - *rh, rhum, relative humidity, humidity, relative_humidity*
   * - Atmospheric pressure
     - Millibar
     - *pres, pressure*
   * - Snow depth
     - cm
     - *snow, snow cover, snow depth*
   * - Ground reflectance (albedo)
     - 0..1
     - *albedo, alb, surface albedo*
   * - Aerosol optical depth
     - 0..1
     - *aod, aerosol, aerosol optical depth*
