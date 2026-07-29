Results
=======

After running a simulation, SAM displays time series results for the high concentration photovoltaic (HCPV) model on the Results page :doc:`Tables <../results/data>` and :doc:`Time Series <../results/timeseries>` graphs.

Hourly Data
~~~~~~~~~~~
.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Variable Name
     - Units
     - Description
   * - AC gross 
     - kWh
     - System gross AC output, before AC derate.
   * - Beam 
     - W/m2
     - Direct normal radiation from weather file.
   * - Cell eff 
     - %
     - Cell conversion efficiency at POA on cell (from table on Module page).
   * - Cell temp 
     - 'C
     - Cell temperature.
   * - DC gross 
     - kWh
     - Array gross DC output, before DC derate.
   * - DC net 
     - kWh
     - Array DC output, after DC derate.
   * - Dry bulb temp 
     - 'C
     - Ambient temperature from weather file.
   * - Hourly Energy 
     - kWh
     - System net AC output, after AC derate.
   * - Hourly Energy Delivered
     - kWh
     - System output
   * - Input radiation 
     - kWh
     - Product of total radiation incident on array and array area.
   * - Module backplate temp 
     - 'C
     - Temperature of back of module.
   * - Module eff 
     - %
     - Module conversion efficiency (input radiation to DC gross).
   * - POA on cell 
     - W/m2
     - Total radiation incident on cell.
   * - Relative air mass
     - 
     - Atmospheric optical length.
   * - Shading derate
     - 
     - DC derate factor due to array shading.
   * - Solar azimuth 
     - deg
     - Azimuth angle of the sun.
   * - Solar zenith 
     - deg
     - Zenith angle of the sun.
   * - Sun up 
     - 0/1
     - 0 = no sun, 1 = sun.
   * - Tracker azimuth 
     - deg
     - Array angle from due north.
   * - Tracker tilt 
     - deg
     - Array tilt angle from horizontal.
   * - Wind speed 
     - m/s
     - Wind speed from weather file.

Monthly Data
~~~~~~~~~~~~
.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Variable Name
     - Units
     - Description
   * - AC net 
     - kWh
     - Total system AC output, after AC derate.
   * - Beam 
     - Wh/m2
     - Total direct normal radiation incident on array.
   * - DC net 
     - kWh
     - Total array DC output, after DC derate.
   * - Input Radiation 
     - kWh
     - Product of monthly beam radiation and module area.
   * - Monthly Energy Delivered
     - kWh
     - Total AC output of the system by month.