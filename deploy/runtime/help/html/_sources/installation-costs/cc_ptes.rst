PTES Installation costs
=======================

.. include:: ../includes/snip_csp_installation_notes.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

**Heater cost, $/kWt**
  A cost per kW of heater thermal power for expenses related to the electric heater equipment, including equipment and labor.

**Thermal energy storage cost, $/kWht**
  Cost per thermal megawatt-hour of storage capacity to account for expenses related to installation of the thermal storage system, including equipment and labor.

**Balance of plant cost, $/kWe**
  Cost per electric kilowatt of power cycle gross capacity to account for additional costs.

**Power cycle cost, $/kWe**
  Cost per electric kilowatt of power cycle gross capacity to account for the installation of the power cycle, including equipment and labor.

**Contingency, %**
  A percentage of the sum of the heater, thermal energy storage, and power cycle costs to account for expected uncertainties in direct cost estimates.

**Total Direct Cost, $**
  The sum of heater, thermal energy storage, and power cycle, and contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.

**EPC and Owner Cost**
  EPC (engineer-procure-construct) and owner costs are associated with the design and construction of the project. SAM calculates the total cost as the sum of the Non-fixed Cost and Fixed Cost.

  Typical costs that may be appropriate to include in the EPC and Owner category are: Permitting, royalty payments, consulting, management or legal fees, geotechnical and environmental surveys, interconnection costs, spare parts inventories, commissioning costs, and the owner's engineering and project development activities.

**Total Land costs**
  costs associated with land purchases, which SAM calculates as the sum of a non-fixed cost and a fixed cost. Use the Land category described below for land costs, and inputs on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page for financing costs.

  SAM calculates the total EPC and Owner costs and Total Land costs by adding the four costs that you can specify using different units.

**$/acre**
  A cost in dollars per total land area in acres.

**% of direct cost**
  A cost as a percentage of **Total Direct Cost** under **Direct Capital Cost**.

**$/We**
  A cost in dollars per AC Watt of nameplate capacity.

**$**
  A fixed dollar amount

**Total Indirect Cost ($)**
The sum of engineer-procure-construct costs, project-land-miscellaneous costs, and sales tax.

Sales Tax
~~~~~~~~~

.. include:: ../includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

