Geothermal Installation costs
=============================

As with all of the SAM models, the Geothermal Installation costs page is used to calculate a total installed cost for use in the financial models. The inputs on this page do not impact the performance of the system.

.. note:: costs that would be applied in per-Year 0 construction years in the Excel version of GETEM need to be translated to Year 0 costs in order to be accurately modeled in SAM’s financial models.

.. include:: ../includes/snip_installation_costs.rst

This is an overview of the geothermal cost inputs (which come from the GETEM model). For more details of how geothermal system costs are specified in the SAM's geothermal model, see the following sections of the GETEM documentation at the following link: https://www.energy.gov/eere/geothermal/geothermal-electricity-technology-evaluation-model.

Details on the inputs and models used for geothermal cost calculations can be found in the `GETEM User Manual <https://www1.eere.energy.gov/geothermal/pdfs/getem_vol_i_technical_manual.pdf>`__ (2016).

Number of Wells to Drill
~~~~~~~~~~~~~~~~~~~~~~~~

GETEM calculates the number of production wells necessary based on inputs on the :doc:`Plant and Equipment <../geothermal/geo_plant_equipment>` page. Based on the number of production wells required, you can specify how many wells have to be drilled in this section.

Because confirmation wells can sometimes be used for production, this section has an input to specify what portion of the confirmation wells will be used on this way. The number of confirmation wells that will be used for production is calculated by multiplying the number of confirmation wells (entered in the "Drilling and Associated costs" section) by the "% of Confirmation Wells Used for Production" and is shown in the "Number of Confirmation Wells" textbox.

The rest of the production wells have to be drilled and will incur the production well drilling cost. The number of production wells that will have to be drilled is calculated by subtracting the production wells to be drilled from the total production wells required and is shown in the "Number of Production Wells to be Drilled" textbox.

The number of injection wells is typically a function of the number of production wells. You can specify this ration in the "Ratio of Injection Wells to Production Wells" input. (Keep in mind that the ratio is the injection wells to the total number of production wells, not the number of production wells that have to be drilled.) This value will be multiplied by the "Total Production Wells Required" value to calculate the "Number of Injection Wells to be Drilled."

PPI
~~~

The Producer Price Index year selection affects the cost multipliers used in the calculation of drilling, plant capital cost, pumping, and pumping costs for the plant. The PPI multipliers used in SAM are normalized to the 2022 index values. 

Drilling and associated costs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Exploration and Confirmation**
  The cost for exploration and confirmation wells is expressed as a function of the cost of a production well. The "Cost multiplier" column is multiplied by the production well cost to calculate the "Cost per well." This value is multiplied by the "# of wells" input to calculate the "Drilling cost" component of the exploration and confirmation costs. You can specify other costs for exploration and confirmation in the "Non-drilling cost" column, which will be added to the "Drilling cost" to calculate the total.

**Production and Injection**
  Production and injection drilling costs are specified as a function of the depth of the wells, the selected cost curve from the GeoVision Study, the well type, and the well diameter. The well depth is assumed to be the resource depth, specified on the Resource input page. The drilling cost per well is calculated using the depth and the chosen cost curve, which vary depending on whether the well is Vertical Open Hole or Deviated Liner as well as for Larger Diameter vs. Smaller Diameter wells. The 4 Cost Curves (Baseline, Intermediate 1, Intermediate 2, and Ideal) bracket the drilling cost curve data from the GeoVision Study. The graph shown below shows the 4 different cost curves for the well type and well diameter specifications and also shows where along the cost curve your current drilling specifications lie relative to the specified resource depth. The cost per well for production and injection wells (derived from the depth and the cost curve) is shown in the "Cost per well" column. The number of wells shown in the "# of wells" column comes from the values in the "Number of Wells to Drill" section above. The "Drilling cost" is calculated by multiplying the "# of wells" by the "Cost per well." The total number of wells and drilling cost is displayed below the Production and Injection wells. You can specify non-drilling costs in this row, which will be added to the drilling cost, to calculate the total cost for production and injection wells.

  .. image:: ../images/SS_Geothermal_CostCurve.png
     :align: center
     :alt: SS_Geothermal_CostCurve.png

**Surface Equipment, Installation and Stimulation Cost**
  Surface equipment and well stimulation costs are assumed to be a function of the total number of production and injection wells. The value entered in the "Cost per well" column will be multiplied by the value in the "# of wells" column to calculate the total, shown in the "Non-drilling cost" column.

**Over-riding calculated costs**
  You can choose to override drilling and associated cost calculations and enter their own cost. If the "Calculate" box is unchecked, the total for the section will be the value entered in the "Specified Total Drilling, Surface Equipment, and Stimulation Cost" input.

Plant Capital Cost
~~~~~~~~~~~~~~~~~~

The plant capital costs are entered on a "per Kw" basis, meaning that you enter the dollars per kilowatt, and SAM will multiply this by the size of the unit (from the "Plant and Equipment" input page) to calculate the Power Plant Cost.

The calculated value can be over-ridden by un-checking the "Calculated" checkbox and entering a value in the "Specified Plant Cost" input.

Pump Cost Inputs
----------------

The pump costs are entered as a function of the pump depth and pump size (calculated on the Plant and Equipment input page). The installation and casing cost is specified on a per foot basis and multiplied by the calculated pump depth to determine the total cost. Pump cost is specified on a per horsepower basis and is multiplied by a function of the pump size to determine the cost per pump.

The total installed cost per pump is the sum of the pump cost and the installation and casing cost. This is multiplied by the total number of production wells required to calculate the total pump cost.

The calculated value can be over-ridden by un-checking the "Calculated" checkbox and entering a value in the "Specified Pump Cost" input.

Indirect Capital costs
----------------------

Indirect capital costs are broken down into three types: engineering, procurement, and construction; project, land, and misc; and sales tax. The first two can be entered as a percentage of direct costs, as a stand alone value, or both (which will be summed to calculate a total). The sales tax percentage is entered on the Financial Parameters page and is applied to some portion of the direct cost. These three types of indirect costs are summed to calculated the total indirect cost.

The calculated value can be over-ridden by un-checking the "Calculated" checkbox and entering a value in the "Specified Indirect Cost" input.

Recapitalization Cost
---------------------

The recapitalization cost will be added each time the resource has to be re-drilled to reach a new section of the geothermal resource in order to increase the production well temperature. Recapitalization costs can be specified directly, or calculated by checking the "Calculate" checkbox. The calculated value is the sum of confirmation, production, and injection drilling costs (excluding non-drilling costs), the surface equipment and installation cost, and the total pump costs.

Total Installed costs
---------------------

.. include:: ../includes/snip_total_installed_cost.rst

