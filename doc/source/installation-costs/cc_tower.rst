Tower Installation costs
========================

.. include:: ../includes/snip_csp_installation_notes.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

Heliostat Field
---------------

**Site improvement cost, $/m****\ :sup:`2`\**
  A cost per square meter of total reflective area from the Heliostat Field page to account for expenses related to site preparation and other equipment not included in the heliostat field cost category.

**Heliostat field cost, $/m****\ :sup:`2`\**
  A cost per square meter of total reflective area from the Heliostat Field page to account for expenses related to installation of the heliostats, including heliostat parts, field wiring, drives, labor, and equipment.

**Heliostat field cost fixed, $**
  A fixed cost to account for any epenses related to installation of the heliostats that do not scale with the reflective area of the field.

Tower
-----

**Tower cost fixed, $**
  A fixed cost to account for tower construction, materials and labor costs. The fixed tower cost serves as the multiplier in the tower cost scaling equation shown below.

**Tower cost scaling exponent**
  SAM uses the tower cost in the optimization calculations. The tower cost scaling exponent defines the nonlinear relationship between tower cost and tower height. See Total Tower Cost below.

The total tower cost shown in blue is calculated as follows:

*Total Tower Cost = Fixed Tower costs x e ^ ( Tower Cost Scaling Exponent x ( Tower Height - Receiver Height ÷ 2 + Heliostat Height ÷ 2 ) )*

Reciever
--------

**Receiver reference cost, $**
  The cost per receiver reference area to account for receiver installation costs, including labor and equipment.

**Receiver reference area, m****\ :sup:`2`\**
  The receiver area on which the receiver reference cost is based.

**Receiver cost scaling exponent**
  SAM uses the receiver cost in the optimization calculations. The receiver cost scaling exponent defines the nonlinear relationship between receiver cost and receiver area based on the reference cost conditions provided.

The total receiver cost shown in blue is calculated as follows:

*Receiver Cost = Receiver Reference Cost x ( Receiver Area / Receiver Reference Area ) ^ Receiver Cost Scaling Exponent*

Thermal Energy Storage
----------------------

**Thermal energy storage cost, $/kWht**
  Cost per thermal megawatt-hour of storage capacity from the Thermal Storage page to account for the installation of a thermal energy storage system, including equipment and labor.

Power Cycle (CSP Only)
----------------------

**Fossil backup cost, $/kWe**
  Cost per electric kilowatt of power block gross capacity to account for the installation of a fossil backup system, including equipment and labor.

.. note:: In versions of SAM released after June 2015, thermocline storage, and fossil backup are not available because they were not incorporated into the new dispatch controller logic at the time of the software release. If you want to use those features, you can use the legacy version SAM 2015.6.30, available on the SAM website `Download page <https://sam.nlr.gov/download>`__.

**Balance of plant cost, $/kWe**
  A cost per electric kilowatt of power cycle gross capacity from the Power Cycle page for expenses related to installation of the balance-of-plant components and controls, and construction of buildings, including labor and equipment.

**Power cycle cost, $/kWe**
  A cost per electric kilowatt of power cycle gross capacity from the Power Cycle page for expenses related to installation of the power cycle  components, including labor and equipment. The power cycle and balance of plant costs are rolled together into a single number for calculation purposes.

Heat Sink (IPH Only)
--------------------

**Balance of plant cost, $/kWt**
  A cost per thermal kilowatt of heat sink capacity from the System Design page for expenses related to installation of the heat sink, including labor and equipment.

Parallel Heater
---------------

**Heater cost, $/kWt**
  Applies only to systems with the electric heater enabled on the System Design page. A cost per unit of heater capacity for expenses related to the electric heater components, including labor and equipment.

Contingency
-----------

**Contingency, % of subtotal**
  A percentage of the sum of the above direct costs to account for expected uncertainties in direct cost estimates.

Total Direct Cost, $
--------------------

The total direct cost is the sum of heliostat field, tower, receiver, thermal energy storage, power cycle (heat sink for IPH), parallel heater, and contingency costs.

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

