DC Degradation
==============

The DC Degradation inputs allow you to model a decline in a photovoltaic system's output over time due, for example, to the degradaton of module cover material.

.. note:: Use inputs on the :doc:`../grid/grid_limits` page to model an interconnection limit and grid curtailment. Use :ref:`pvavailability` inputs on the Electrical Losses page to model system availability losses.

Annual Degradation for Multi-year Simulation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The multi-year simulation uses the same weather file for each year, so it does not accurately represent the effect of changes in the solar resource and weather from year to year over the analysis period.

**Annual DC degradation rate**
  The reduction in system DC output in each time step as a percentage of the system's DC output in each time step by year.

.. _lifetimedaily:

Lifetime Daily Losses
~~~~~~~~~~~~~~~~~~~~~

In addition to the annual DC degradation, you can define tables of daily AC and DC losses that vary over the analysis period. This requires that you have a set of losses for each day over the analysis period (defined on the Financial Parameters page). For example, for an analysis period of 25 years, you would need a table of *1 value/day × 365 days/year × 25 years = 9,125 values*.

**Enable lifetime daily DC losses**
  Check the box, and then click **Edit time series data** to :doc:`enter a table of DC loss percentages <../window-reference/win_edit_data_lifetime>`   as a percentage of the system's DC output in each day for each year in the analysis period. The table requires a set of 365 values for each year in the analysis period.

**Enable lifetime daily AC losses**
  Check the box, and then click **Edit time series data** to :doc:`enter a table of AC loss percentages <../window-reference/win_edit_data_lifetime>`   as a percentage of the system's AC output in each day for each year in the analysis period. The table requires a set of 365 values for each year in the analysis period. 

.. _degdc-valueschedule:

Value and Schedule Mode
-----------------------

For most situations where you want to model an annual reduction in the system's output, you can use the "value mode" for a single annual degradation rate that applies to all years of the project cash flow. SAM applies the percentage to the system's total annual electrical output value in years 2 and later.

.. topic:: Value Mode Example
  
   A DC degradation rate of 1% for a system with a annual DC output of 100,000 kWh in Year one results in annual output values of 100,000 kWh in year 1, 99,000 kWh in year 2, 98,010 kWh in year 3, 97,029.9 kWh in year 4, etc.

In some situations, you may want to use "schedule mode" to assign a different degradation rate to different years of the project cash flow. To do that, you use the annual schedule option to specify a table of degradation rate values. In Schedule mode, SAM applies the degradation rate for a given year to the annual output in Year one to calculate the annual output for that year.

.. topic:: Schedule Mode Example
  
   For a system with a Year 1 DC output of 10,000 kWh, if you specify a DC degradation rate of 0.5% for Year 5, SAM would calculate the annual output in Year 5 as follows: 10,000 kWh × (1 - 0.005) = 9,950 kWh.

   Because Year 1 is the first year of operation before degradation begins, you should set the degradation rate for Year 1 (the first row in the degradation table) to zero.

**To enter an annual schedule of degradation rates:**

.. include:: ../includes/snip_annual_values.rst

Memory Saving Option for Subhourly Simulations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you are running subhourly simulations over the analysis period, the number of values for each output variable can be very large. For, example, one-minute simulations over a 25-year analysis period would result in 1 time step/min × 60 min/hr × 8,760 hr/yr × 25 yr = 13,140,000 time steps, so each output variable would store over 13 million values.

If your computer is experiencing memory problems, or if the tables and graphs on the Results page are not working properly, you can clear **Save all output variables over analysis period** to reduce the amount of memory required to display simulation results.That will cause SAM to save only a selection of variables in "lifetime mode" with values over the entire analysis period. The remaining variables will only store one year's worth of data so you can still examine hourly data to see how the system performs.

.. note:: The option must be checked for PV Battery configurations that require lifetime outputs for battery life calculations.