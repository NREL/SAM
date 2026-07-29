PV Installation costs
=====================

.. include:: ../includes/snip_installation_costs.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

Module Cost, $/Wdc or $/Unit
----------------------------

For the detailed photovoltaic model, the module cost is expressed per unit or per DC Watt:

* Dollars per DC watt multiplied by **Nameplate Capacity (at reference conditions)** on the :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>` page, or

* Dollars per unit multiplied by Total Modules on the System Design page.

For PVWatts, the module cost is expressed per unit or per DC Watt:

* Dollars per watt multiplied by **Nameplate Capacity** on the :doc:`PVWatts System Design <../pvwatts/pvwatts_system_design>` page, or

* Dollars per unit, where the number of modules is assumed to be one.

Inverter, $/Wac or $/Unit
-------------------------

For the detailed photovoltaic model, the cost of inverters in the system is expressed in dollars per AC Watt or dollars per inverter:

* Dollars per AC watt multiplied by **Total Inverter Capacity** on the :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>` page, or

* Dollars per unit multiplied by **Number of Inverters** in the **Actual Layout** column on the System Design page.

For PVWatts, the inverter cost is either dollars per watt or dollars per inverter:

* Dollars per watt multiplied by the product of **DC Rating** and **DC to AC Derate Factor** on the :doc:`System Design <../pvwatts/pvwatts_system_design>` page, or

* Dollars per unit where the number of inverters is assumed to be one.

Battery, $/kWh and $/kW
-----------------------

For systems with batteries, the battery installation cost is expressed in $/kWh of total battery capacity, $/kW of maximum charge/discharge power, or both. Specify the battery replacement cost separately using the $/kWh cost on the :doc:`Operating costs <../operating-costs/oc_battery>` page.

PV Battery:

* Dollars per kilowatt-hour multiplied by **Nominal bank capacity** on the Battery Cell and System page.

* Dollars per kilowatt multiplied by the **Maximum discharge power** on the Battery Cell and System page.

PVWatts Battery:

* Dollars per kilowatt-hour multiplied by **Battery capacity** on the :doc:`System Design <../pvwatts/pvwatts_system_design>` page.

* Dollars per kilowatt multiplied by the **Battery power** on the :doc:`System Design <../pvwatts/pvwatts_system_design>` page.

Balance of System, Installation Labor, Margin and Overhead
----------------------------------------------------------

The other direct capital cost categories, **Tracking Equipment** (HCPV only), **Balance of system**, **Installation labor**, and **Installer margin and overhead** can be specified using three units. SAM calculates the total amount for each category as shown below.

**$**
  A fixed cost in dollars.

**$/Wdc**
  A cost proportional to the system's DC nameplate capacity, equal to the **Nameplate Capacity** on the :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>`   page for the detailed photovoltaic model, or the **DC Rating** on the :doc:`PVWatts System Design <../pvwatts/pvwatts_system_design>`   page for the PVWatts model.

**$/m2**
  Applies only to the detailed photovoltaic model. A cost proportional to the total module area of the array in square meters, equal to the **Total Area** on the :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>`   page. This option is not active for the PVWatts model.

Contingency (%)
---------------

A percentage of the sum of the equipment, balance-of-system, installation labor, and installer margin and overhead costs that you can use to account for expected uncertainty in the direct cost estimates.

Total Direct Cost ($)
---------------------

The sum of equipment, system, installation labor, installer margin and overhead costs, and contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.

The five indirect cost categories, **Permitting - Environmental Studies**, **Engineering**, **Grid interconnection** are each calculated as the sum of the three values expressed with the following units:

% of Direct Cost
----------------

A percentage of the **Total Direct Cost** value shown under Direct Capital costs.

Cost $/Wdc
----------

A cost proportional to the system's DC nameplate capacity, equal to the **Nameplate Capacity** on the :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>` page for the   detailed photovoltaic model, or the **DC Rating** on the :doc:`PVWatts System Design <../pvwatts/pvwatts_system_design>` page for the PVWatts model.

Fixed Cost
----------

A fixed cost in dollars.

Total
-----

For each category, the total is the sum of the three units:

*Total = % of Direct Cost × Total Direct Cost ($) + Cost $/Wdc × Nameplate Capacity (Wdc) + Fixed Cost ($)*

Land costs
----------

SAM calculates the total land cost as the sum of **Land** and **Land Preparation**. The land cost categories use the Cost $/acre category in addition to the categories for the other indirect costs (% of Direct Cost, Cost $/Wdc, Fixed Cost).

**Total Land Area**
  The total land area from the :doc:`System Size<../detailed-photovoltaic-model/pv_system_size>`   page.

**Cost $/acre**
  A cost in dollars per acre of total land area.

**Total**
  For each land cost category, the total is the sum of the three units:

*Total = Cost $/acre + % of Direct Cost × Total Direct Cost ($) + Cost $/Wdc × Nameplate Capacity (Wdc) + Fixed Cost ($)*

Total Indirect Cost ($)
-----------------------

The sum of the five indirect cost categories.

Sales Tax (%)
~~~~~~~~~~~~~

.. include:: ../includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

