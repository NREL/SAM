Merchant Revenue
================

The Revenue page provides inputs to define the sources of revenue for the merchant plant. A merchant plant can earn revenue for energy and ancillary services and from capacity payments. For energy and ancillary service revenue, the project only earns revenue from the cleared capacity portion of generation for each source of revenue. SAM calculates reports the annual revenue for each type of service in the project :doc:`cash flow <../results/cashflow>`.

For energy and ancillary services, you can specify cleared capacity and price values for the following time scales over the entire analysis period:

* Single Value: The same cleared capacity (MW) and price ($/MWh) applies over the entire analysis period.

* Monthly: Cleared capacity and price values change from month to month.

* Daily: Values change from day to day.

* Hourly: Values change hourly.

* Subhourly: As long as you use a weather file with the same time step as the pricing time scale, values apply to each simulation time step (30, 20, 15, 10, 5, or 1-minute time steps are allowed).

.. note:: If the power generation (kW) in a given time step is less than the cleared capacity (MW) for energy and ancillary services in that time step, SAM generates a simulation error because this condition would be a violation of the merchant contract.

   The merchant plant does not earn any revenue if the cleared capacity is zero for all time steps.

Annual revenue from capacity payments can be calculated as a fixed price or a price per unit of eligible capacity with an optional annual capacity payment escalation rate.

Example Compensation Scenarios
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following examples show how SAM calculates compensation for energy services, ancillary services, and capacity payments under different conditions for a single time step.

Scenario 1 : Energy service with generation in time step sufficient to meet energy cleared capacity
---------------------------------------------------------------------------------------------------

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Generation
     - 65 MW
   * - Energy cleared capacity
     - 50 MW
   * - Energy price
     - 30 $/MWh

*Energy Compensation ($) = 50 MW × 1 hour × $30/MWh = $1,500*

Scenario 2: Energy service with generation in time step not sufficient to meet energy cleared capacity.
-------------------------------------------------------------------------------------------------------

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Generation
     - 42 MW
   * - Energy cleared capacity
     - 50 MW
   * - Energy price
     - 30 $/MWh

This scenario causes the simulation to stop because the generation of 42 MW is less than the energy cleared capacity of 50 MW. To resolve the error, either the generation in that time step would need to be increased, or the energy cleared capacity would have to be decreased.

Scenario 3: Energy and ancillary service with generation in time step sufficient to clear both energy and ancillary capacity requirements.
------------------------------------------------------------------------------------------------------------------------------------------

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Generation
     - 65 MW
   * - Energy cleared capacity
     - 50 MW
   * - Ancillary cleared capacity
     - 10 MW
   * - Energy price
     - $30/MWh
   * - Ancillary price
     - $15/MWh

*Energy Compensation ($) = 50 MW × 1 hour × $30/MWh = $1,500*

*Remaining Capacity Available for Ancillary Service (MW) = 65 MW – 50 MW = 15 MW*

*Ancillary Compensation ($) = 10 MW × 1 hour × $15/MWh = $150*

*Total Compensation ($) = $1,500 + $150 = $1,650*

Scenario 4: Energy and ancillary service with generation in time step sufficient to clear energy capacity requirement but not ancillary capacity requirement.
-------------------------------------------------------------------------------------------------------------------------------------------------------------

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Nameplate capacity
     - 100 MW
   * - Generation
     - 65 MW
   * - Energy cleared capacity
     - 50 MW
   * - Ancillary cleared capacity
     - 20 MW
   * - Energy price
     - 30 $/MWh
   * - Ancillary price
     - 15 $/MWh

*Energy Compensation ($) = 50 MW × 1 hour × $30/MWh = $1,500*

*Remaining Capacity Available for Ancillary Service (MW) = 65 MW – 50 MW = 15 MW*

  This scenario causes a simulation error because the remaining capacity of 15 MW is less than the ancillary cleared capacity of 20 MW. To resolve the error, either the generation in that time step would need to be increased, or the ancillary cleared capacity would have to be reduced.

Scenario 5 : Energy service and capacity payments with generation in time step sufficient to meet energy cleared capacity.
--------------------------------------------------------------------------------------------------------------------------

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Nameplate capacity
     - 100 MW
   * - Generation
     - 65 MW
   * - Energy cleared capacity
     - 50 MW
   * - Energy price
     - 30 $/MWh
   * - Capacity payment amount
     - $100/MW
   * - Capacity credit
     - 10%

*Energy Compensation ($) = 50 MW × 1 hour × $30/MWh = $1,500*

*Annual capacity revenue ($/year) = 100 MW × $100/MW × 10% = $1,000/year*

Capacity Payments
~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_revenue_capacity_payments.rst

NLR Cambium Hourly Price Data Download
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you do not have time series price data for your analysis, you can download hourly marginal cost data for modeled futures of the U.S. electricity sector from the `NLR Scenario Viewer <https://scenarioviewer.nlr.gov/>`__.

To download Cambium price data:

#. Click the Energy Market or Ancillary Service tab at the bottom of the Revenue page for the market you want to assign the downloaded price data.

#. Choose the option you want to use for price data: **Time series cleared capacity and price** or **Fixed cleared capacity and time series price**. (See below for descriptions.)

#. Choose values for the Cambium API parameters as appropriate. Click the |SS_Button-Information| button to read a description of each parameter, or see the `Cambium documentation <https://www.nlr.gov/analysis/cambium.html>`__ for more details.

#. If you choose the **Multiple years** option for the **Year** parameter, choose the start year.

#. Specify the optional **Annual price escalation rate** if you want SAM to apply an escalation rate to the Cambium price data. SAM does not automatically apply the inflation rate from the :doc:`Financial Parameters <../financial-parameters/fin_merchant_plant>` page when it downloads the Cambium data, so you can use this escalation rate to represent inflation or any other price increase. 

Cambium price data is in real terms for a constant dollar year for the year preceding the release. For example, for Cambium 2022, prices are in 2021 dollars.

#. Click the **Download to [...]** button. SAM downloads the price data to the energy market or ancillary service price option you chose in Step 2.

#. Click **Edit lifetime data** to verify that the price data downloaded correctly.

.. note:: The Multiple Years option downloads 8760 hourly values for more than one year, which may take a few minutes to complete.

   Only some years are available from the Cambium database. For example, Cambium 2023 data is available for 2025, 2030, 2035, 2040, 2045, and 2050. If you set the start date to 2030 for a project with a 25-year analysis period, SAM downloads cambium data for the available years between 2030 and 2050. For missing years, it copies data from the previous available year, so it would use 2030 data for 2031, 2032, 2033, and 2034.

   If you specify an escalation rate, SAM treats the start year as Year 0, and uses the escalation rate to adjust the values for later years. It does not apply the inflation rate from the Financial Parameters page to the downloaded price data.

Energy Market and Ancillary Service
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Energy market and ancillary service revenue is revenue for the cleared capacity available in each time step at the specified price in $/MWh.

The cleared capacity is the portion of the generated capacity in a given time step that is compensated at the energy price. See "Example Compensation Scenarios" above for examples.

SAM allows the project to earn revenue from any combination of energy market and up to four ancillary services. Click the tab for each market to enable it and specify prices and cleared capacity.

.. note:: When you enable one or more ancillary services, SAM assumes that the system is capable of providing the type of service represented by each ancillary service. It does not model reactive power, frequency, spinning reserves or startup capabilities that would be required to provide these services in an actual system.

**Enable energy market revenue**
  Check the box to enable energy market revenue.

**Time series cleared capacity and price**
  Choose this option when you have time series data for both the cleared capacity in MW and price in $/MWh.

  Click **Edit lifetime data** for **Energy market cleared capacity and price** to specify time series data.

**Fixed cleared capacity and time series price**
  Choose this option when you have time series data for price in $/MWh and want SAM to calculate the cleared capacity as a percentage of generated power in each time step.

  Click **Edit lifetime data** for **Energy market price for fixed cleared capacity** to specify time series price data.

  For **Energy market fixed cleared capacity**, enter the cleared capacity as a percentage of generation in each time step.

.. |SS_Button-Information| image:: ../images/SS_Button-Information.png
