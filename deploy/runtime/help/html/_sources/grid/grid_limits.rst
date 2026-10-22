Grid Limits
===========

The Grid page provides inputs for setting a grid power export limit, and for curtailing the system's exports to the grid.

SAM uses the interconnection limit and curtailment inputs to adjust the value calculated for the system AC power output in each hourly our subhourly time step by applying an adjustment factor to the AC power output value.

The grid interconnection limit and curtailment do not limit electricity imports from the grid.
 
SAM assumes that the system has a mechanism for adjusting the power output to meet these grid limits. For example a photovoltaic system might adjust its power output using power electronics to adjust DC operating voltages, or a wind system might feather the turbine blades. You should be sure to account for any costs associated with these control systems on the Installation costs page and/or Operating costs page.
 
.. note:: For systems with battery storage, you can choose whether to consider the grid interconnection limit and curtailment for some dispatch options.

Grid Interconnection Limit
--------------------------

The grid interconnection limit is the maximum AC power that the system can deliver to the grid. SAM considers any power above the limit to be curtailed power.

**Enable interconnection limit**
  Check this box if there is a limit to the amount of power the system is allowed to deliver to the grid.

**Grid interconnection limit, kWac**
  The maximum power the system is allowed to deliver to the grid in AC kilowatts. For each time step that the system AC power output exceeds the limit, SAM sets the system power generated to the interconnection limit.

The **Annual energy loss from interconnection limit** result variables (available on the :doc:`Data Tables <../results/data>` tab of the Results page) shows the amount of energy that would have been generated without the interconnection limit. The **System power before grid interconnection** time series result variable shows what the system power would have been without the limit.

Grid Curtailment
----------------

Grid curtailment is a reduction in the amount of power the system is allowed to deliver to the grid. SAM models curtailment as a maximum system AC power output in each time step.

For the Single Owner and Merchant Plant financial models, curtailed power is compensated at the curtailment compensation rate on the Revenue page.

**Curtailment**
  A table of system AC power output values values for each simulation time step over one year.

  Click **Edit array** to provide a table of power values. The number of rows in the table should match the number of records in the weather file. 

  For a weather file with hourly data, the time step is 60 minutes and the table should have 8,760 values. If you are using subhourly weather data, click **Change time step**, and enter the weather file time step in minutes.

*   To paste data from a spreadsheet or other program, select the data in the spreadsheet with no header and copy it to your computer's clipboard. Then, in SAM click **Paste** in the Edit Array window.

*   To import data from a text file, prepare a file with one column of data and one header row. SAM ignores the first row in the file and imports data in the remaining rows. Click **Import** in the Edit Array window and navigate to the file.

The **Annual energy loss from curtailment limit** result variables (available on the :doc:`Data Tables <../results/data>` tab of the Results page) shows the amount of energy that would have been generated without curtailment. The **System power before curtailment limit** time series result variable shows what the system power would have been without curtailment. The project :doc:`cash flow <../results/cashflow>` shows the impact of curtailment on revenue.

