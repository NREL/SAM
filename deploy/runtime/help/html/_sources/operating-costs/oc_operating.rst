Operating costs
===============

.. include:: ../includes/snip_o_and_m_costs_no_fossil.rst

For technologies that consume a fuel, such as some concentrating solar power (CSP) systems that have  fossil backup or the Custom Generation Profile model, there is an additional input for the cost of fuel.

**Fossil Fuel Cost ($/MMBtu)**
  The cost per million British thermal units for fuel. SAM uses the conversion factor 1 MWh = 3.413 MMBtu. SAM calculates a fossil fuel cost for concentrating solar power systems with fossil backup, and for the custom generation profile model when the heat rate on the :doc:`Generation Profile <../custom-generation/custom_generation_profile>`   page is not zero.

.. note:: If you are using the :doc:`custom generation profile <../custom-generation/custom_generation>` model to represent a system that does not consume fuel, you should use a fuel cost of zero.

Land Lease costs
~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_operating_cost_land_lease.rst

Using Annual Schedules to Specify Operating costs in Specific Years
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_o_and_m_periodic_costs.rst
