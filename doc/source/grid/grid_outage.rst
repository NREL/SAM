Grid Outage
===========

The Grid Outage inputs are for behind-the-meter battery storage systems designed to cover critical loads during grid outages. A grid outage is a period of time when the grid is off line. A critical load is the electric load that SAM must meet during grid outages.

.. note:: If your analysis does not involve grid outages or critical loads, choose **Calculate critical load as percentage of electric load** and set **Critical load percent of electric load** to zero.

Use the options on the Grid Outage page to determine:

* Whether your system design can meet critical loads during grid outages that you specify.

* How many hours your system design would be able to meet critical loads for a hypothetical grid outage.

* Both whether the system design can meet critical loads during grid outages, and how many hours it could meet critical loads during a hypothetical grid outage.

Modeling grid outages requires that you specify critical electric loads, and depending on your analysis, the timing of grid outages. It also requires that you specify the battery minimum state of charge during grid outages.

Grid Outage Input Combinations
------------------------------

How SAM uses grid outage and critical load data in the simulation calculations depends on the combination of grid outage inputs. Use the table below to help you choose the right combination of inputs for your analysis.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - 
     - Critical Load
     - Grid Outage
     - Meet critical load during outage time steps
     - Calculate hours of autonomy for hypothetical outage
     - Result
   * - 1
     - Yes
     - Yes
     - Yes
     - No
     - SAM dispatches the battery to attempt to meet critical loads during grid outages. During times with no grid outage, it dispatches the battery based on the options on the Battery Dispatch page.
   * - 2
     - Yes
     - No
     - No
     - Yes
     - SAM models a hypothetical outage in every time step to see how long the system could meet the critical load. Autonomy metrics in the results indicate how long of a grid outage the system could cover critical loads given the system design and battery dispatch option. SAM does not dispatch the battery specifically to cover outages.
   * - 3
     - Yes
     - Yes
     - No
     - Yes
     - Combines 1 and 2: SAM dispatches the battery to cover critical loads and models a hypothetical outage in each time step. The results of this combination tell you how well the system can meet critical loads during grid outages and how long of a grid outage it could cover critical loads.
   * - 4
     - No
     - No
     - Yes
     - No
     - No autonomy calculations. This is the default setup for a behind-the-meter storage system with no grid outage or critical load.
   * - 5
     - No
     - No
     - No
     - Yes
     - Invalid, need critical load. SAM prevents you from choosing this combination.
   * - 6
     - No
     - Yes
     - Yes
     - No
     - Invalid, need critical load. SAM displays messages and generates a simulation error if you run a simulation with this combination.
   * - 7
     - No
     - Yes
     - No
     - Yes
     - Invalid, need critical load. SAM displays messages and generates a simulation error if you run a simulation with this combination.
   * - 8
     - Yes
     - No
     - Yes
     - No
     - Critical load doesn't do anything. Either specify grid outages or switch to **Calculate hours of autonomy for a hypothetical outage**.

Critical Electric Load
~~~~~~~~~~~~~~~~~~~~~~

A critical electric load is either a fixed percentage of the electric load specified on the :doc:`Electric Load <../electricity-rates-and-load/electricity_load>` page or an array of kW values that represents electric usage the system must meet during grid outages.

.. note:: You must specify a critical load to model grid outages.

**Calculate critical load as percentage of electric load**
  Choose this option to specify the critical load as a percentage of the electric load. Use this option when the critical load is a portion of the electric load in each time step.

**Use time series critical load data**
  Choose this option to specify the critical load in kW for each time step. Use this option when the critical load has a different profile than the electric load.

**Critical load percent of electric load, %**
  A percentage of the electric load data to consider as critical load. Applies to every time step of the simulation.

**Time series critical load data**
  Critical load data in kW for each time step of the simulation.

**Critical load annual growth rate, %/yr**
  The annual growth rate of the critical load. You can specify a single value or click the |SS_AnnSched-valschedbutton|   button to specify growth rates by year as an :doc:`annual schedule <../window-reference/win_edit_data_table_column>`  . The annual growth rate applies to the critical load when you specify it as a percentage or as time series data.

To specify time series critical load data:

#. Click **Edit array** to open the Edit Array window.

#. Click **Change time step** to set the number of rows in the table by typing the number of minutes per time step.   The number of rows in the array must be consistent with the simulation time step, which is determined by the weather file. For example, if you are using a weather file with hourly time steps, the time steps is 60 minutes.

#. Type values, or use the Import or Paste buttons to enter critical load data in kW for one year. SAM will use this data as the critical load for Year 1 of the project cash flow, and apply the critical load annual growth rate to calculate the critical load in Years 2 and later.

Grid Outage
~~~~~~~~~~~

A grid outage is a period of time when the grid is off line. During an outage, the grid cannot deliver electricity to the electric load, battery, or for system self consumption, and the system cannot deliver electricity to the grid.

**Grid outage**
  To enable the grid outage input, specify a critical load as described above. You do not need to specify a grid outage if you are using SAM to calculate hours of battery autonomy for a hypothetical outage.

To specify grid outage data:

#. Click **Edit array** to open the Edit Array window.

#. Click **Change time step** to set the number of rows in the table by typing the number of minutes per time step.   The number of rows in the array must be consistent with the simulation time step, which is determined by the weather file. For example, if you are using a weather file with hourly time steps, the time steps is 60 minutes.

#. Type values, or use the Import or Paste buttons to enter grid outage data for on year. Use a zero for each time step with no grid outage (grid on), and one for each time step with a grid outage (grid off).

Autonomy Calculations
~~~~~~~~~~~~~~~~~~~~~

The autonomy calculations determine how SAM dispatches the battery during grid outages. Hours of autonomy is a measure of how long the system can meet critical load with no power from the grid.

**Meet critical load during outage time steps (grid outage data required)**
  Choose this option when you have grid outage data and want to see whether your system design can meet the critical load during the grid outages you specify.

**Calculate hours of autonomy for a hypothetical outage (grid outage data optional)**
  Use this option when you want SAM to calculate how many hours your system design could meet the critical load for a hypothetical outage. When you choose this option, SAM reports autonomy metrics in the simulation results.

Battery State of Charge
~~~~~~~~~~~~~~~~~~~~~~~

During normal operation when there is not a grid outage, the battery minimum and maximum state of charge (SOC) is determined by the inputs on the :doc:`Battery Dispatch <../battery-storage/battery_dispatch_btm>` page. These SOC limits are designed to prevent over-charging or over-discharging the battery during normal operation to maximize the battery's life. 

During a grid outage, SAM assumes that meeting the critical load is more important than protecting the battery over the relatively short period of a grid outage, so it allows the battery to charge up to 100%. It also allows the battery to discharge below the minimum SOC on the Battery Dispatch page, but will not discharge it below the **Minimum state of charge during grid outage** on the Grid Outage page.

**Minimum state of charge during grid outage, %**
  The battery minimum state of charge during grid outages. SAM prevents the battery from discharging below this minimum during grid outages.

.. |SS_AnnSched-valschedbutton| image:: ../images/SS_AnnSched-valschedbutton.png
