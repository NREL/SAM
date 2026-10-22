Irradiance Error Codes
======================

The solar irradiance processor is an internal part of SAM's code that makes solar irradiance calculations and performs checks on the data in a weather file for the solar performance models.

If the code identifies a potential problem with the data, it returns a simulation message similar to this with a code number:

```exec fail(pvsamv1): Failed to calculate POA irradiance 1 (code: -105) [y:2017 m:1 d:1 h:0 minute:30]```

The following is a list of conditions for each code:

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Code
     - Conditions
   * - -101
     - Negative time stamp or time step greater than one.
   * - -102
     - Latitude or longitude value out of range.
   * - -103
     - Irradiance mode or sky diffuse choice set incorrectly.
   * - -104
     - Tracking mode choice set incorrectly. Valid values are between zero and four.
   * - -105
     - Irradiance value out of range for DNI / DHI mode: DNI or DHI value is negative or greater than 1,500 W/m².
   * - -106
     - Irradiance value out of range for DNI / GHI mode: DNI or GHI value is negative or greater than 1,500 W/m².
   * - -107
     - Albedo value is negative or greater than one.
   * - -108
     - Array tilt angle is negative or greater than 90 degrees.
   * - -109
     - Array azimuth angle is negative or greater than 360 degrees.
   * - -110
     - Array rotation limit (for one-axis tracking) is less than -90 degrees or greater than 90 degrees.
   * - -111
     - Irradiance value out of range for GHI / DHI mode: GHI or DHI value is negative or greater than 1,500 W/m².
   * - -112
     - Array stow angle (for one-axis tracking) is less than -90 degrees or greater than 90 degrees.

