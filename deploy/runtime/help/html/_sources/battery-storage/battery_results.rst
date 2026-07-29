Battery Results
===============

You can find time series results for the battery storage models on the :doc:`Results <../getting-started/results_page>` page after running a simulation. SAM displays time series data under Lifetime, and Hourly, or n-minute headings, depending on the simulation time step. 

* The Lifetime results are outputs of the performance model simulation, which runs over the analysis period from the Financial Parameters page.

* The Hourly (or n-minute) Data results are for weather file records and other inputs that are for a single year. The Detailed Photovoltaic model uses the same weather data for each year of the lifetime simulation.

The time series results are available on the following tabs of the Results page:

* :doc:`Data tables <../results/data>` 

* :doc:`Time series <../results/timeseries>`

* :doc:`Profiles <../results/profiles>`

* :doc:`Statistics <../results/statistics>`

* :doc:`Heat map <../results/heatmap>`

* :doc:`PDF/CDF <../results/pdfcdf>`

SAM also reports monthly and annual totals of some of the time series values under the headings **Single Values**, **Monthly Data**, and **Annual Data**.

Battery Storage Results
~~~~~~~~~~~~~~~~~~~~~~~

Metrics
-------

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Name
     - Units
     - Description
   * - Battery roundtrip efficiency
     - %
     - The ratio of total battery discharge energy to battery charge energy over the life of the project..
   * - Battery charge energy from system
     - %
     - The percentage of total battery charge energy delivered by the power system (PV array in the case of the PV Battery model). If the percentage is less than 100%, the remaining charge energy was supplied by the grid.

Lifetime Data
-------------

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Name
     - Units
     - Description
   * - Battery average cycle DOD
     - 
     - The average depth of discharge over the life of the battery, used for cycle degradation.
   * - Battery capacity percent for temperature 
     - %
     - The maximum capacity degradation or enhancement related to the battery temperature.
   * - Battery cell voltage
     - V
     - The voltage of each cell in the battery.
   * - Battery computed cycle degradation
     - $/cycle-kWh
     - The automatically computed cycle degradation penalty for automated dispatch, when the cycle degradation penalty option on the Battery Dispatch page is set to "calculate automatically."
   * - Battery current
     - A
     - The current flow through the battery.
   * - Battery cycle depth of discharge
     - %
     - The depth of discharge for the last elapsed cycle.
   * - Battery loss from ancillary equipment
     - kW (DC or AC)
     - Power loss due to ancillary equipment losses, if any are specified on the Battery Cell and System page under "Battery Losses." The kW values are DC for DC-connected battery and AC for AC-connected battery.
   * - Battery loss from power electronics
     - kW
     - Power loss due to power converters losses, if any are specified on the Battery Cell and System page under Power Converters.
   * - Battery max charge
     - Ah
     - The maximum charge capacity of the battery.
   * - Battery maximum charge at temperature
     - Ah
     - The maximum charge capacity of the battery at the current time step due to temperature.
   * - Battery maximum charge with degradation
     - Ah
     - The maximum charge capacity of the battery at the current time step due to lifetime and cycling degradation.
   * - Battery number of cycles
     - 
     - A running count of how many charge/discharge cycles have elapsed.
   * - Battery relative capacity to nameplate
     - %
     - The maximum battery capacity relative to the its original maximum capacity. For the cycle and calendar degradation option, this is the minimum value of cycling and lifetime degradation.
   * - Battery relative capacity to nameplate (calendar)
     - %
     - For the Cycle and Calendar Degradation option on the Battery life page, the remaining capacity relative to the original capacity, where the degradation caused by the lifetime mechanisms.
   * - Battery relative capacity to nameplate (cycling)
     - %
     - For the Cycle and Calendar Degradation option on the Battery life page, the remaining capacity relative to the original capacity, where the degradation caused by the cycling mechanisms.
   * - Battery state of charge
     - %
     - The percentage of charge present in the battery relative to the maximum possible charge.
   * - Battery temperature
     - °C
     - The bulk average temperature of the battery.
   * - Battery total charge
     - Ah
     - The amount of charge currently present in the battery.
   * - Battery voltage
     - V
     - The battery bank voltage.
   * - Electricity battery power target for automated dispatch
     - kW
     - The target battery charge power (positive) or discharge power (negative) determined by the dispatch algorithm. If you choose the Input battery power targets option on the Battery Dispatch page, these targets reported in the results are adjusted from the input target values to meet battery operating constraints.
   * - Electricity to battery from grid AC
     - kWac
     - AC power delivered to the battery from the grid for charging. Measured on the AC side of the inverter, before AC losses.
   * - Electricity to battery from system AC
     - kWac
     - Power delivered to the battery from the power generating system. For DC-connected PV-battery configurations, this value is calculated to account for DC losses that are not itemized in the time series results. Measured on the AC side of the inverter, before AC losses.
   * - Electricity to battery from system DC
     -  kWdc
     - Power delivered to the battery from the power generating system. For DC-connected PV-battery configurations, this value is calculated to account for DC losses that are not itemized in the time series results. Measured on the DC side of the inverter or battery management system.
   * - Electricity to grid from battery AC
     - kWac
     - AC power discharged to the grid from the battery. Measured at the grid interconnection point, after AC losses.
   * - Electricity to grid from system AC
     - kWac
     - AC power delivered to the grid from the power generating system. Measured at the grid interconnection point, after AC losses.
   * - Electricity to inverter from battery DC
     - kWdc
     - DC Power at the inverter input delivered by the battery. 
   * - Electricity to load from battery AC
     - kWac
     - AC power discharged to the load from the battery. Measured at the load connection point, after AC losses.
   * - Electricity to load from grid AC
     - kWac
     - AC power delivered to the load by the grid. Measured at the load connection point, after AC losses.
   * - Electricity to load from system AC
     - kWac
     - AC power delivered to the load by the power generating system. Measured at the load connection point, after AC losses.
   * - Electricity to system loads from battery
     - kWac
     - For AC-connected PV-battery systems, AC power discharged from the battery to the power generating system parasitic loads. For DC-connected systems, the inverter shuts off when the PV array is not generating power, so the battery cannot discharge to the AC side of the inverter.
   * - Electricity to/from battery AC
     - kWac
     - Power discharged from the battery (positive) to the load or grid, and power to charge the battery (negative) from the grid and/or power generating system. Measured on the AC side of the inverter, before AC losses.
   * - Electricity to/from battery DC
     - kWdc
     - Power discharged from the battery (positive) to the load or grid, and power to charge the battery (negative) from the grid and/or power generating system. Measured on the DC side of the inverter or battery management system.
   * - Electricity to/from grid
     - kWac
     - AC power delivered to the grid by the power generating system, battery, or both.
   * - Inverter AC output power
     - kWac
     - Power from all sources measured on the AC side of the inverter, before AC losses.
   * - Power produced without the battery or curtailment
     - kW
     - Power generated by the power generating system excluding the battery and before any curtailment is applied.