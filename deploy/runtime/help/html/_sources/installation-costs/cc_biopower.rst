Biomass Installation costs
==========================

.. include:: ../includes/snip_installation_costs.rst

Because only the **Total Installed Cost** value affects the cash flow calculations, you can assign capital costs to the different cost categories in whatever way makes sense for your analysis. For example, you could assign the cost of designing the power plant to the direct plant cost category or to the engineer-procure-construct category with equivalent results. The categories are provided to help you keep track of the different costs, but do not affect the economic calculations. After assigning costs to the categories, verify that the total installed costs value is what you expect.

Variable values in boxes with white backgrounds are values that you can edit. Boxes with blue backgrounds contain calculated values or values from other pages that SAM displays for your information. 

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow. Below are the major unit operations that contribute to the Total Direct Capital Cost calculation, and the appropriate units. Based on earlier inputs, SAM calculates the capacity of each item. 

**Boiler(s)**
  A cost per lb/hr steam 

**Turbine & Generator**
  A cost per kW of gross nameplate capacity to account for turbine and generator installation costs.

**Fuel Handling Equipment**
  A cost per kW of gross nameplate capacity to account for fuel handling equipment costs.

**Dryer**
  A cost per kW of gross nameplate capacity to account for feedstock drying equipment. This cost is only available when you choose **Dry to Specified Moisture Content** as the Biomass Feedstock Handling option on the :doc:`Plant Specs <../biopower/biopower_plant_specs>`   page.

**Other Equipment**
  A cost per kW of gross nameplate capacity to account for equipment not included in the categories above.

**Balance of Plant**
  A cost per kW of gross nameplate capacity to account for plant-related costs not associated with specific components of the plant.

**Contingency**
  A percentage of the sum of the above costs to account for expected uncertainties in direct cost estimates.

**Total Direct Capital Cost, $**
  The sum of the direct capital costs, including contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

**Engineer Procure Construct, % and $**
  Engineer-Procure-Construct costs, sometimes abbreviated as EPC costs, are costs associated with the design and construction of the project, which SAM calculates as the sum of a “non-fixed cost” and with a fixed cost.

**Project Land Miscellaneous, % and $**
  Project-Land-Miscellaneous costs are those associated with the purchase and preparation of land, and other indirect costs not included in the EPC category.

**% of Direct Cost**
  A value that you type as a percentage of **Total Direct Capital Cost** (under **Direct Capital costs**)

**Non-fixed Cost**
  A value that SAM calculates as the product of **% of Direct Cost** and **Total Direct Capital Cost**.

**Fixed Cost**
  A value that you type as a fixed amount in dollars.

**Total**
  A value that SAM calculates as the sum of **Non-fixed Cost** and **Fixed Cost**.

**Total Indirect Cost, $**
  The sum of engineer-procure-construct costs, project-land-miscellaneous costs.

Sales Tax
~~~~~~~~~

.. include:: ../includes/snip_sales_tax.rst

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

