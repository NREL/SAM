SWH Installation costs
======================

.. include:: ../includes/snip_installation_costs.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

**Collector cost, $/m****\ :sup:`2`\****or $/Unit or $/W**
  The cost of collectors in the system. You can either include labor costs for collector installation in the collector cost, or account for it separately using the installation cost category. The total collector cost is calculated as either:

  * Dollars per square meter multiplied by collector area on the :doc:`SWH System page <../solar-water-heating/swh_system>`, or

  * Dollars per unit, representing the total collector cost, or

  * Dollars per thermal watt of collector capacity multiplied by the nameplate capacity on the :doc:`SWH System page <../solar-water-heating/swh_system>`  .

**Storage cost, $/m****\ :sup:`3`\****or $/Unit**
  The cost of the solar storage tanks. The total storage cost is either:

  * Dollars per cubic meters multiplied by the storage volume on the :doc:`SWH System page <../solar-water-heating/swh_system>`, or

  * Dollars per unit, representing the total storage cost.

**Balance of system, $**
  A fixed cost that can be used to account for costs not included in the collector and storage cost categories, for example, the mounting racks and piping.

**Installation cost, $**
  A fixed cost that can be used to account for labor or other costs not included in the other cost categories.

**Contingency, %**
  A percentage of the sum of the collector, storage, balance-of-system, and installation costs to account for expected uncertainties in direct cost estimates.

**Total direct cost, $**
  The sum of collector, storage, balance-of-system, installation, and contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.

**Engineer Procure Construct, % and $**
  Engineer-procure-construct costs, sometimes abbreviated as EPC costs, are costs associated with the design and construction of the project, which SAM calculates as the sum of a "non-fixed cost" and a fixed cost.

**% of Direct Cost** is a value that you type as a percentage of **Total Direct Cost** under **Direct Capital Cost**).

**Non-fixed Cost** is the product of **% of Direct Cost** and **Total Direct Cost**.

**Fixed Cost** is a value that you type as a fixed amount in dollars.

  The total engineer-procure-construct cost is the sum of **Non-fixed Cost** and **Fixed Cost**.

**Project Land Maintenance, % and $**
  costs associated with land purchases, permitting, and other costs which SAM calculates as the sum of a "non-fixed cost" and a fixed cost.

  SAM does not use the land area value shown on the solar field page for trough and tower systems in the land cost calculation.

**% of Direct Cost** is a value that you type as a percentage of **Total Direct Cost** (under **Direct Capital Cost**).

**Non-fixed Cost** is the product of **% of Direct Cost** and **Total Direct Cost**.

**Fixed Cost** is a value that you type as a fixed amount in dollars.

  The total project-land-miscellaneous cost is the sum of **Non-fixed Cost** and **Fixed Cost**.

**Total indirect cost,  $**
  The sum of engineer-procure-construct costs, project-land-miscellaneous costs.

Sales tax
~~~~~~~~~

.. include:: ../includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

