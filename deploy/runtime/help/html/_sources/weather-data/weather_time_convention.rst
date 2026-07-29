Time and Sun Position
=====================

SAM assumes that the weather file contains data for a single year. The weather file may contain hourly or subhourly data.

The first row of data in the weather file is for the time step beginning at 12 a.m. (midnight) on January 1, local standard time. For hourly data, the first row is for the hour ending at 1 a.m. on January 1. For 15-minute data, the first row is for the quarter hour ending at 1:15 a.m. on January 1. For financial model time-dependent calculations, January 1 is a Monday, weekdays are Monday through Friday, and weekends are Saturday and Sunday.

SAM does not account for leap years or for daylight savings time. 

Wind Resource Data
~~~~~~~~~~~~~~~~~~

For wind resource data (:doc:`SAM CSV format for wind <../weather-file-formats/weather_format_csv_wind>`), the last two columns of the first header row indicate the time step in hours (1=hourly, 0.25=15-minute, etc.), and number of time steps (8,760 for hourly, 35,040 for 15-minute, etc.). The values in the header must match the number of rows.

Solar Resource Data
~~~~~~~~~~~~~~~~~~~

For solar resource data (:doc:`SAM CSV format for solar <../weather-file-formats/weather_format_sam_csv_solar>`), the weather file's time step depends on the number of weather data rows (not including the header rows) in the weather file. A file for hourly simulations should contain 8,760 data rows. A weather file for 15-minute simulations should have 8,760 hours per year × 4 time steps per hour = 35,040 data rows.

Air mass calculations use the site elevation value from the weather file header and the solar zenith angle calculated for each time step.

SAM's photovoltaic model determines the time for sun position calculations as follows:

* For hourly data with no Minute column, the sun position is for the midpoint of the hour. For the sunrise time step, the sun position is at the midpoint between sunrise time and the end of the time step. Similarly, for the sunset hour, the sun position is the midpoint between the beginning of the time step and sunset time.

* For hourly data with a Minute column and subhourly data (which must have a Minute column), use the minute value as the time for the sun position calculation for all hours, including the sunrise and sunset hours.

SAM's CSP models assume that each DNI value in the weather file represents the average irradiance over the time step, and calculate sun angles for the midpoint of the time step, regardless of the value of the minute column in the weather file.