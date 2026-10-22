Thermal Load
============

The Thermal Load page allows you to specify a thermal load for systems that generate usable heat. You can input heat usage data by importing hourly or subhourly heat usage data from a text file, or paste it from a spreadsheet or other program. It also provides options for scaling the time series load data to match annual or monthly heating requirements.

Thermal Load Data
-----------------

**Heat usage**
  Click **Edit data** to either cut and paste load data from another program or to import data from a properly formatted text file. See :ref:`Working with Time Series Load Data <electric-load-timeseries>` for instructions.

**Scaling factor (optional)**
  Use the scaling factor to scale up or down the entire data set by the same amount. This option is useful when both the daily profile and seasonal pattern of your load data reasonably approximate the building's heat usage, but the annual total does not.

  When you change the scaling factor value, SAM recalculates the monthly and annual energy and peak values shown in the monthly load summary so you can verify that the value is correct.

  To calculate the load value during simulation, SAM multiplies the hourly or subhourly load data you by the scaling factor that you specify. For example, if you specify a scaling factor of 1.5, and the load at 2 p.m. on March 18th is 1.2 kW, SAM would use a load value of 1.5 × 1.2 = 1.8 kW for that hour.

**Normalize supplied load profile to monthly utility bill data**
  This option allows you to specify the building's total monthly heat usage in kWh. SAM scales the hourly or subhourly data for each month accordingly. This is useful when the daily profiles of the load data reasonably approximate the building's load, but the monthly totals do not match the monthly heat usage.

  For example, if the sum of the original load data for January is 1,000 kWh, and you enter a value of 1,500 kWh for January, SAM will scale each load value in January by 1,500 ÷ 1,000 = 1.5. In that example, if the original load value on January 5 at 10 a.m. is 3 kW, the scaled value would be 4.5 kW.

**Monthly energy usage (kWh)**
  Click **Edit values** to enter the building's monthly heat usage in kWh. SAM scales the hourly or subhourly load to match the values you enter as described above.

**View load data**
  Opens the :doc:`time series data viewer <../reference/time_series_viewer>`   to display graphs of the load data.

Monthly Load Summary
--------------------

SAM displays the table of monthly and annual totals to help you verify that the load data is correct.

**Thermal load (kWh-t)**
  The Energy column displays the building's total thermal load for each month.

  SAM calculates the value by adding the hourly or subhourly values for each month and multiplying by the number of hours per time step. For example, for hourly data, the total energy value for January would be the sum of hourly load values in kW for Hours 1 to 744 multiplied by 1 hour per time step. For 15-minute load data, the value would be the sum of the subhourly values in kW multiplied by 0.25 hours per time step.

**Peak (kW-t)**
  The maximum load value in kW that occurs in each month.

**Annual Total**
  The total amount of heat required by the building over an entire year, equivalent to the sum of the monthly energy values.

  The annual total applies to year one of the analysis period. If you specify a load growth rate rate under **Annual Adjustment**, the annual total does not reflect load data in years two and later.

**Annual Peak**
  The maximum load value that occurs in the year.

**Load annual growth rate, %/yr**
  The load growth rate scales the load in years two and later by the percentage you specify. For example, if you specify a load growth rate of 0.5% per year, for each year in the analysis period specified on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, SAM would increase the load value in each time step by 0.5% of the previous year's load value for the same time step.

  You can also assign load growth rates to specific years using the annual schedule:

.. include:: /includes/snip_annual_values.rst 

