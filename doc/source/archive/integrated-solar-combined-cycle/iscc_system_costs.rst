System costs
============

SAM uses the variables on the System costs page to calculate the project investment cost and annual operating costs reported in the project :doc:`cash flow <../results/cashflow>` and used to calculate cost metrics reported in the Metrics table on the :doc:`Results page <../getting-started/results_page>`.

Because only the total installed cost value affects the cash flow calculations, you can assign capital costs to the different cost categories in whatever way makes sense for your analysis. For example, you could assign the cost of designing the solar field to the solar field cost category or to the engineer-procure-construct category with equivalent results. The categories are provided to help you keep track of the different costs, but do not affect the economic calculations. After assigning costs to the categories, verify that the total installed costs value is what you expect.

Variable values in boxes with white backgrounds are values that you can edit. Boxes with blue backgrounds contain calculated values or values from other pages that SAM displays for your information.

.. include:: ../includes/snip_csp_installation_notes.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

**Site Improvements ($/m****\ :sup:`2`\****)**
  A cost per square meter of total reflective area from the Heliostat Field page to account for expenses related to site preparation and other equipment not included in the heliostat field cost category.

**Heliostat Field ($/m****\ :sup:`2`\****)**
  A cost per square meter of total reflective area from the Heliostat Field page to account for expenses related to installation of the heliostats, including heliostat parts, field wiring, drives, labor, and equipment.

**Balance of Plant ($/kWe)**
  A cost per electric kilowatt of power cycle gross capacity from the Power Cycle page expenses related to installation of the balance-of-plant components and controls, and construction of buildings, including labor and equipment.

**Power Block ($/kWe)**
  A cost per electric kilowatt of power cycle gross capacity from the Power Cycle page expenses related to installation of the power block components, including labor and equipment. The Power Block and Balance of Plant costs are rolled together into a single number for calculation purposes.

**Fossil Backup ($/kWe)**
  Cost per electric kilowatt of power block gross capacity to account for the installation of a fossil backup system, including equipment and labor.

**Storage ($/kWht)**
  Cost per thermal megawatt-hour of storage capacity from the Thermal Storage page to account for the installation of a thermal energy storage system, including equipment and labor.

**Fixed Solar Field Cost ($)**
  An additional fixed cost in dollars to include as a direct cost that is not accounted for by any of the above categories.

**Fixed Tower Cost ($)**
  A fixed cost to account for tower construction, materials and labor costs. The fixed tower cost serves as the multiplier in the tower cost scaling equation shown below.

**Tower Cost Scaling Exponent**
  SAM uses the tower cost in the optimization calculations. The tower cost scaling exponent defines the nonlinear relationship between tower cost and tower height. See Total Tower Cost below.

**Total Tower Cost ($)**
  Total Tower Cost = Fixed Tower costs x e ^ ( Tower Cost Scaling Exponent x ( Tower Height - Receiver Height ÷ 2 + Heliostat Height ÷ 2 ) )

**Receiver Reference Cost ($)**
  The cost per receiver reference area to account for receiver installation costs, including labor and equipment.

**Receiver Reference Area (m****\ :sup:`2`\****)**
  The receiver area on which the receiver reference cost is based.

**Receiver Cost Scaling Exponent**
  SAM uses the receiver cost in the optimization calculations. The receiver cost scaling exponent defines the nonlinear relationship between receiver cost and receiver area based on the reference cost conditions provided.

**Total Receiver Cost ($)**
  Receiver Cost = Receiver Reference Cost x ( Receiver Area / Receiver Reference Area ) ^ Receiver Cost Scaling Exponent

**Contingency (%)**
  A percentage of the sum of the site improvements, heliostat field, balance of plant, power block, storage system, fixed solar field, total tower, and total receiver costs to account for expected uncertainties in direct cost estimates.

**Total Direct Cost ($)**
  The sum of improvements, site improvements, heliostat field, balance of plant, power block, storage system, fixed solar field, total tower, total receiver, and contingency costs.

Indirect Capital Costs
~~~~~~~~~~~~~~

.. include:: ../includes/snip_csp_indirect_om_costs.rst

Sales Tax
~~~~~~~~~

.. include:: /includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: /includes/snip_total_installed_cost.rst

Entering Periodic Operation and Maintenance costs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_o_and_m_periodic_costs.rst

About the CSP Default Cost Assumptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_csp_cost_assumptions.rst

