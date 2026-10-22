Array
=====

The Concentrating Array page displays inputs for the high concentration photovoltaic (HCPV) performance model.

SAM assumes that HCPV modules are mounted on 2-axis trackers, and calculates tracking power consumption based on the tracking power that you specify. SAM allows you to set tracker limits and to set a maximum wind speed to move modules into stow position during periods of high wind.

Array Configuration
~~~~~~~~~~~~~~~~~~~

The array configuration variables determine the number of modules and inverters in the system.

**Number of trackers**
  The total number of trackers in the array.

**Modules on each tracker**
  The number of modules on each tracker.

**Single tracker nameplate capacity, kWdc**
  The capacity in DC kilowatts of a single tracker at the reference radiation value specified on the :doc:`Module <hcpv_module>`   page.

*Single Tracker Nameplate Capacity (kWdc) = Modules on Each Tracker × Module Maximum Power (Wdc) / 1000 (Wdc/kWdc)*

  The module maximum power is Pmp from the :doc:`Module <hcpv_module>`   page.

**System nameplate capacity, kWdc**
  The array's nameplate capacity in DC kilowatts at the reference radiation value specified on the :doc:`Module <hcpv_module>`   page.

*System Nameplate Capacity (kWdc) = Single Tracker Nameplate Capacity (kWdc) × Number of Trackers*

**Number of inverters**
  The number of inverters specified on the :doc:`Inverter <../detailed-photovoltaic-model/pv_inverter>`   page required to match the array's capacity.

*Number of Inverters = System Nameplate Capacity (kWdc) / Inverter DC Rated Power (Wdc) / 1000 (Wdc/kWdc)*

  The number of inverters is the nearest integer greater than the nameplate capacity divided by a single inverter's rated DC capacity.

  The inverter's rated DC capacity is Power DCo from the :doc:`Inverter <../detailed-photovoltaic-model/pv_inverter>`   page.

.. note:: The HCPV model works only in conjunction with the Sandia inverter model option on the Inverter page. The inverter single-point efficiency model is only available for the flat-plate/low-x PV model.

**Inverter AC capacity, kWac**
  The total AC capacity of inverters in the system.

*Inverter AC Capacity (kWac) = Number of Inverters × Inverter AC Rated Power (Wac) / 1000 (Wac/kWac)*

  The inverter's rated AC capacity is Power ACo from the :doc:`Inverter <../detailed-photovoltaic-model/pv_inverter>`   page.

System Availability
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_system_availability.rst

Tracker
~~~~~~~

The tracker options determine the type of two-axis active tracking used by the system.

**General tracking error factor**
  Defines losses due to tracker error.

**Single tracker power during operation, W**
  The total rated power of the tracking mechanism for a single tracker.

**Tracker elevation angle limits (degrees)**
  Minimum and maximum values for the tracker elevation angle. SAM limits the tracker movement to values between the minimum and maximum.

**Tracker azimuth angle limits (degrees)**
  Minimum and maximum values for the tracker azimuth angle. SAM limits the tracker movement to values between the minimum and maximum.

Soiling and derates
~~~~~~~~~~~~~~~~~~~

The soiling and derate factors account for losses in the system that the module and inverter models do not calculate, such as electrical losses in the DC wiring that connects modules in the array.

.. note:: For a discussion of derate factors in the context of the NLR PVWatts model that includes suggested values, see the Help system for the `online PVWatts Calculator <https://pvwatts.nlr.gov/>`__. Note that SAM only includes derate factors for losses that the module, inverter, and shading models do not calculate.

**Monthly soiling factors**
  Click **Edit Values** to specify the soiling derate factor that applies for each month of the year. A derate factor of 100 represents an array with no soiling, a value of 0 would be for an array that receives no sunlight. SAM assumes that Hour 1 of the 8,760 hours in a year is the hour ending at 1 am on Monday, January 1.

  SAM applies the soiling derate factor to the direct radiation incident on the array.

**DC wiring loss factor**
  A loss factor that SAM applies to the array's hourly DC output during the simulation.

**DC module mismatch loss factor**
  A loss factor that SAM applies to the array's hourly DC output during the simulation.

**Diodes and connections loss factor**
  A loss factor that SAM applies to the array's hourly DC output during the simulation.

**AC wiring loss factor**
  A loss factor that SAM applies to the system's hourly AC output during the simulation.

**Estimated overall system conversion efficiency, %**
  An estimate of the system's DC-to-AC conversion efficiency at nameplate conditions. SAM displays this value for reference only. During the simulation, it calculates the system's efficiency for each hour based on the available solar resource and system properties.

*Estimated Efficiency = Estimated Module Efficiency × General Tracking Error × DC Wiring Loss × Average Annual Soiling Loss × DC Module Mismatch × Diode and Connections × AC wiring × Inverter AC Capacity ÷ Inverter DC Capacity*

  Average Annual Soiling loss is the average of the 12 monthly soiling factors in the Edit Values window.

Stowing
~~~~~~~

**Max allowed wind speed before stowing, m/s**
  Determines the wind speed that causes the trackers to move to stow position.

Land Area
~~~~~~~~~

**Packing Factor**
  The packing factor is a multiplier that makes it possible to estimate the land area required by a project based on the total module area of the array.

.. note:: The packing factor only has an effect on simulation results when you specify land costs in $/acre on the :doc:`Installation costs <../installation-costs/cc_pv>` page.

**Total Land Area**
  The total land area is an estimate of the land area required by the PV system:

*Total Land Area = Overall Module Area (m²) × Number of Trackers × Modules on Each Tracker × Packing Factor ÷ 4,047 (m²/acre)*

  Where Overall Module Area is from the :doc:`Module <hcpv_module>`   page.

Shading Derate
~~~~~~~~~~~~~~

The azimuth-by-altitude table is a two-dimensional look-up table of beam shading factors. Each column in the table contains a set of shading factors for the solar azimuth value shown in the column heading. Each row in the table contains a set of shading factors for the solar altitude value in the row heading. 

Solar azimuth values in the column headings must increase monotonically from left to right. Solar altitude values must increase monotonically from bottom to top.

For each hour in the simulation, SAM calculates the position of the sun as a set of solar azimuth and altitude angles. SAM uses a linear interpolation method to estimate the value of the beam shading factor for the hour based on the nearest values in the look-up table.

.. note:: Azimuth values use the following convention: 0 = north, 90 = east, 180 = south, 270 = west.

To define the azimuth-altitude shading factor table by hand:

#. In **Rows** and **Cols**, type the number of rows and columns in the table.

Specify a number of rows that is one greater than the number of azimuth values: For example for a table with ten rows of solar azimuth values, specify a **Rows** value of 11. Similarly, specify a **Cols** value that is one greater than the number of altitude values.

#. Type a set of column headings, solar azimuth values increasing from left to right.

#. Type a set of row headings with solar altitude values decreasing from top to bottom.

#. Type a beam shading factor value (between zero and one) in each cell of the table.

To import or export azimuth-by-altitude beam shading factors:

SAM allows you to import and export the azimuth-altitude lookup table as a comma-delimited text file that contains shading factors separated by commas. The file should have row or column headings.

* To export the shading matrix as a text file, click **Export**.

* To import a data from a comma-delimited text file, click **Import**.