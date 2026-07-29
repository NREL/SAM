Custom Generation Installation costs
====================================

.. include:: ../includes/snip_installation_costs.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

**Plant Cost, $**
  A fixed dollar amount.

**Plant cost per capacity, $/W**
  A cost per Watt of nameplate capacity from the :doc:`Power Plant <../custom-generation/custom_generation_profile>`   page.

If the system includes a battery, you can specify the battery installation cost in $/kWh of battery capacity, $/kW of maximum discharge power, or both:

**Battery cost per capacity, $/kWh DC**
  Battery installation cost in $/kWh of nominal bank capacity from the Battery Cell and System page.

**Battery cost per kW, $/kW DC**
  Battery installation cost in $/kW of maximum charge power from the Battery Cell and System page.

**Contingency cost, %**
  A percentage of the sum of the plant and battery costs to account for expected uncertainties in direct capital cost estimates.

**Total Direct Cost, $**
  The sum of plant cost, plant cost per capacity, and contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.

**Engineering and other EPC costs, % and $**
  costs associated with the design and construction of the project.

**% of Direct Cost** is a value that you type as a percentage of **Total direct cost** (under **Direct Capital Cost**). SAM displays the equivalent dollar amount.

**$** is a value that you type as an amount in dollars.

**Permitting and other EPC costs, % and $**
  costs associated with land purchases, permitting, and other costs.

**% of Direct Cost** is a value that you type as a percentage of **Total direct cost** (under **Direct Capital Cost**). SAM displays the equivalent dollar amount.

**$** is a value that you type as a fixed amount in dollars.

**Total Indirect Cost,  $**
  The sum of indirect capital costs.

Sales Tax
~~~~~~~~~

.. include:: ../includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

