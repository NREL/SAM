Battery Installation costs
==========================

.. include:: ../includes/snip_installation_costs.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

The battery installation cost is the cost of purchasing and installing all battery-related equipment, including labor and other associated costs. Specify the battery replacement cost separately using the $/kWh cost on the :doc:`Operating costs <../operating-costs/oc_battery>` page.

**Battery pack, kWhdc**
  The **Nominal bank capacity** in DC kWh from the Battery Cell and System page.

**Battery power, kWdc**
  The **Maximum discharge power** in DC kW from the Battery Cell and System page.

**Battery cost per capacity, $/kWhdc**
  The battery cost in $/kWhdc costs that scale with the battery's nominal energy storage capacity.

**Battery cost per kW, $/kWdc**
  The battery cost in $/kWdc for costs that scale with the battery's maximum discharge power.

**Contingency, %**
  A percentage of the battery cost that you can use to account for expected uncertainty in the direct cost estimates.

**Total Direct Cost, $**
  The sum of battery and contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.

The indirect cost categories are each calculated as the sum of the three values expressed with the following units:

**% of direct cost**
  A percentage of the **Total Direct Cost** value shown under Direct Capital costs. 

*Nonfixed Cost ($) = % of Direct Cost × Total Direct Cost ($)*

**Fixed $**
  A fixed cost in dollars.

For each category, the total is the sum of nonfixed and fixed costs:

*Total = Nonfixed Cost ($) + Fixed Cost ($)*

**Total Indirect Cost ($)**
  The sum of the two  indirect cost categories.

Sales Tax (%)
~~~~~~~~~~~~~

.. include:: ../includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

