Trough Installation costs
=========================

.. include:: ../includes/snip_csp_installation_notes.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

**Site Improvements, $/m****\ :sup:`2`\**
  A cost per square meter of solar field area to account for expenses related to site preparation and other equipment not included in the solar field cost category.

**Solar Field, $/m****\ :sup:`2`\**
  A cost per square meter of solar field area to account for expenses related to installation of the solar field, including labor and equipment.

**HTF System, $/m****\ :sup:`2`\**
  A cost per square meter of solar field area to account for expenses related to installation of the heat transfer fluid pumps and piping, including labor and equipment.

**Storage, $/kWht**
  Cost per thermal megawatt-hour of storage capacity to account for expenses related to installation of the thermal storage system, including equipment and labor.

**Fossil Backup, $/kWe**
  For CSP Systems only. Cost per electric megawatt of power block gross capacity to account for the installation of a fossil backup system, including equipment and labor.

 .. note:: In versions of SAM released after February 2020, fossil backup is not available for the Physical Trough model because it was not incorporated into the new dispatch controller logic at the time of the software release, so the Fossil Backup cost should be zero. If you want to use fossil backup, use version SAM 2018.11.11, available on the SAM website `Download page <https://sam.nlr.gov/download>`__.

**Power Plant, $/kWe**
  For CSP Systems only. Cost per electric megawatt of power block gross capacity to account for the installation of the power block, including equipment and labor.

**Heat Sink, $/kWht**
  For IPH systems only. Cost per thermal kilowatt-hour of heat sink capacity for expenses related to installation of the heat sink, including labor and equipment.

**Balance of Plant, $/kWe for CSP, $/kWt for IPH**
  Cost per electric megawatt of power block gross capacity or thermal heat sink capacity to account for additional costs.

**Contingency, %**
  A percentage of the sum of the site improvements, solar field, HTF system, storage, fossil backup, and power plant costs to account for expected uncertainties in direct cost estimates.

**Total Direct Cost, $**
  The sum of improvements, solar field, HTF system, storage, fossil backup, power plant costs, and contingency costs.

Indirect Capital Costs
~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_csp_indirect_om_costs.rst

Sales Tax
~~~~~~~~~

.. include:: /includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: /includes/snip_total_installed_cost.rst

About the CSP Default Cost Assumptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_csp_cost_assumptions.rst

