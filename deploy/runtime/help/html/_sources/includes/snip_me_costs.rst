
When you use the marine energy wave or marine energy tidal performance model with the LCOE calculator financial model, SAM uses a fixed-charge-rate (FCR) method to calculate the levelized cost of energy (LCOE) from inputs on the :doc:`Financial Parameters <../financial-parameters/fin_lcoefcr>` page. The FCR method requires a total installed cost and annual operating cost value as input, along with a fixed charge rate representing the financial assumptions for the project. 

When you use the single owner financial model with either the wave or wave battery performance model, SAM uses an annual cash flow model to calculate financial metrics such as the net present value (NPV), internal rate of return (IRR), power purchase price (PPA), LCOE, and other metrics.

Each Capital Costs page and the O&M (operation and maintenance) Cost page provide a detailed cost structure that SAM uses to calculate the total capital cost and fixed annual operating cost for the financial model calculations:

* The total capital cost is the sum of the total device cost, total balance-of-system (BOS) cost, and total financial cost.

* The fixed operating cost is the sum of the O&M costs.

SAM's marine energy models are based on the U.S Department of Energy (DOE) Reference Model Project (RMP). You can find links to documents for the RMP on the Sandia National Laboratories website at https://energy.sandia.gov/programs/renewable-energy/water-power/projects/reference-model-project-rmp/. Excel workbooks with cost category details are available for each technology:

* Mirko Previsic (2012). `Reference Model 1 Cost Breakdown Structure for Tidal Current Device <https://energy.sandia.gov/download/21275/>`__

* Mirko Previsic (2012). `Cost Breakdown Structure for WEC Rated at 286 kW <https://energy.sandia.gov/download/23667/>`__

Marine Energy Cost Structure
----------------------------

The cost structure has several levels, so you can specify each cost as a single value, or break it down into more detailed categories and subcategories.

**Override cost structure**
  This is the highest level in the cost structure. Use this option to enter a total cost at the Device, BOS, Financial, or O&M Cost level. When you check **Override cost structure**, SAM uses the value you type and ignores the detailed cost values.

If you choose to enter costs using more detailed categories, for each cost, click the Expand button |SS_Panel-expand| to show details, and choose an option for entering cost values:

.. image:: /images/ss-me-cost-options.png
   :align: center
   :alt: ss-me-cost-options.png

**Enter in in $/kW**
Type a cost per unit of rated capacity. The rated capacity is from the Array page. SAM calculates the category cost by multiplying the "user input" cost you enter in $/kW by the array capacity in kW from the Array page. SAM ignores the modeled value, but shows it for your reference.

  .. image:: /images/ss-me-cost-per-kw.png
     :align: center
     :alt: ss-me-cost-per-kw.png

**Enter in $**
  Type a dollar value for the cost. SAM ignores the modeled value, but shows it for your reference.

  .. image:: /images/ss-me-cost-fixed.png
     :align: center
     :alt: ss-me-cost-fixed.png

**Use Modeled Value ($)**
  Choose this option for SAM to automatically calculate the cost. The category cost is equal to the modeled value, and SAM ignores the "user input" value. All modeled values are based on an assumed currency year of 2024.

  .. image:: /images/ss-me-cost-modeled.png
     :align: center
     :alt: ss-me-cost-modeled.png

.. note:: For the wave energy model, if you choose a WEC from the library on the Wave Energy Converter page, the default device costs are based on the associated costs for each design based on reference model projects. Balance-of-system costs and financial costs are based on costs curves developed from existing reference model data, a database with wave and tidal cost data from DOE-funded projects, and a literature review of wave, tidal, and offshore wind energy costs. The cost curves are based on an assumed currency year of 2024.


.. note:: If you use your own WEC parameters instead of choosing a WEC from the library, device costs are from cost curves based on array rated power or percentage of costs. These costs curves were developed from existing reference model data, a database with wave and tidal cost data from DOE-funded projects, and a literature review of wave, tidal, and offshore wind energy costs. 


.. note:: For the tidal energy model, default device costs are based on the associated costs the Reference Model 1 tidal converter. Balance-of-system capital costs and financial costs are based on costs curves developed from existing reference model data, a database with wave and tidal cost data from DOE-funded projects, and a literature review of wave, tidal, and offshore wind energy costs.

**Use Cost Breakdown ($)**
  Choose this option to enter costs at the most detailed level. When you choose the option for a cost category, SAM automatically expands the Cost Breakdown section for that category.

  You can either enter a total cost for the category, 

  .. image:: /images/ss-me-cost-itemized.png
     :align: center
     :alt: ss-me-cost-itemized.png

  or check **Enter detailed costs** to enter itemized costs for each subcategory. SAM automatically calculates the total based on the costs you enter.

  .. image:: /images/ss-me-cost-itemized-detailed.png
     :align: center
     :alt: ss-me-cost-itemized-detailed.png

  For information about a cost item, click the Information button next to the item's label.

  .. image:: /images/SS_MHKCosts-information.png
     :align: center
     :alt: SS_MHKCosts-information.png

**Use Array Scaling**
  Choose this option for SAM to scale the array cost based on the cost of a single device. The "user input" value is the cost of a single device in dollars. The array scaling R-value scales the single device cost as follows:

  .. image:: /images/EQ_MARINE_costscalingb.png
     :align: center
     :alt: EQ_MARINE_costscalingb.png

  .. image:: /images/EQ_MARINE_costscalingcost.png
     :align: center
     :alt: EQ_MARINE_costscalingcost.png

  Where:

* *R*   = **Array scaling R-value**

* *Cost*\ :sub:`device`\    = **User input** for the **Structural assembly** option, representing the cost of a single device in dollars

* *N*\ :sub:`devices`\    = **Number of devices in array** from Array page

  The following screenshot is for an example with 100 devices to illustrate how the calculation works with *R = 0.1*, *Cost*\ :sub:`device`\ *= 10*, and *N*\ :sub:`devices`\ *=100*  :

.. image:: /images/ss-me-cost-scaling.png
   :align: center
   :alt: ss-me-cost-scaling.png



.. |SS_Panel-expand| image:: /images/SS_Panel-expand.png
