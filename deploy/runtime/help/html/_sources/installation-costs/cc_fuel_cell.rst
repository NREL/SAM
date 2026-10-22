Fuel Cell Installation costs
============================

.. include:: ../includes/snip_installation_costs.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

**Module, $/Wdc or $/Unit**
  You can specify the module cost either in $/Wdc or $/unit:

* Dollars per watt multiplied by **System nameplate capacity** from the :doc:`PV System <../pvwatts/pvwatts_system_design>` page, or

* Dollars per unit, where the number of modules is assumed to be one.

**Inverter, $/Wac or $/Unit**
  For PVWatts, the inverter cost is either dollars per watt AC or DC, or dollars per inverter:

* Dollars per watt DC multiplied by the system nameplate capacity from the :doc:`PV System <../pvwatts/pvwatts_system_design>` page, or

* Dollars per watt AC multiplied by the system nameplate capacity and divided by the DC to AC Derate Factor on the :doc:`System Design <../pvwatts/pvwatts_system_design>` page, or

* Dollars per unit where the number of inverters is assumed to be one.

**Battery rated power, $/kW**
  Battery cost per unit of **Battery rated power**, which is the maximum discharge power (kW AC) from the :doc:`Battery Storage <../battery-storage/battery_storage>` page.

**Battery rated capacity, $/kWh**
  Battery cost per unit of **Battery rated capacity**, which is the nominal bank capacity (kWh DC) from the :doc:`Battery Storage <../battery-storage/battery_storage>` page.

.. note:: If the battery is not enabled on the Battery Storage page, SAM sets the battery costs to zero.

The other direct capital cost categories, **Balance of system equipment**, **Installation labor**, and **Installer margin and overhead** can be specified using five units. SAM calculates the total amount for each category as shown below.

**$**
  A fixed cost in dollars.

**PV $/Wdc**
  A cost proportional to the PV array's DC nameplate capacity, equal to the **System nameplate size on the** on the :doc:`PV System <../pvwatts/pvwatts_system_design>` page.

**Battery $/kW**
  A cost proportional to the battery's nominal power rating, equal to the **Maximum charge power (AC)** on the :doc:`Battery Storage <../battery-storage/battery_storage>` page.

**Fuel Cell $/kW**
  A cost proportional to the fuel cell stack nameplate capacity, equal to the **Cell stack nameplate** on the :doc:`Fuel Cell <../fuel-cell/fuelcell_fuel_cell>`  page.

**Contingency, %**
  A percentage of the sum of the module, inverter, balance-of-system, installation labor, and installer margin and overhead costs that you can use to account for expected uncertainties in direct cost estimates.

**Total Direct Cost, $**
  The sum of module, inverter, balance-of-system, installation labor, installer margin and overhead costs, and contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.

The five indirect cost categories, **Permitting and environmental studies**, **Engineering and developer overhead**, **Grid interconnection** are each calculated as the sum of four values expressed in the following units:

**% of Direct Cost**
  A percentage of the **Total direct cost** value shown under Direct Capital costs.

**PV $/Wdc**
  A cost proportional to the PV array's DC nameplate capacity, equal to the **System nameplate size on the** on the :doc:`PV System <../pvwatts/pvwatts_system_design>` page.

**Battery $/kW**
  A cost proportional to the battery's nominal power rating, equal to the **Maximum charge power (AC)** on the :doc:`Battery Storage <../battery-storage/battery_storage>` page.

**Fuel Cell $/kW**
  A cost proportional to the fuel cell stack nameplate capacity, equal to the **Cell stack nameplate** on the :doc:`Fuel Cell <../fuel-cell/fuelcell_fuel_cell>` page.

**$**
  A fixed cost in dollars.

Land costs
----------

SAM calculates the total land cost as the sum of **Land area**, **Land purchase**, and **Land prep. and transmission**. The land cost categories use the Cost $/acre category in addition to the categories for the other indirect costs.

.. note:: The land area input is independent of the PV array size or other input parameters. If you want to include land area costs in your analysis, be sure to specify a land area in acres that is appropriate for your system's design.

Sales Tax
~~~~~~~~~

.. include:: ../includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

