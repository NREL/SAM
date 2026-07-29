
Installation Costs are costs associated with installing the system, and include equipment, labor, engineering, permitting, and any other costs that apply in Year 0 of the project cash flow. Some costs, such as debt-related and sales tax costs are specified on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page.

SAM uses the variables on the Installation Costs page to calculate the project investment cost.

Recurring costs that apply in Years 1 and later of the project cash flow are on the :doc:`Operating Costs <../operating-costs/operating_costs>` page.

Variable values in boxes with white backgrounds are values that you can edit. Boxes with blue backgrounds contain calculated values or values from other pages that SAM displays for your information.

SAM provides the categories under **Direct Capital Costs** and **Indirect Capital Costs** for your convenience to help keep track of project installation costs. Only the **Total Installed Cost** value affects the cash flow calculations, so you can assign capital costs to the different cost categories in whatever way makes sense for your analysis. For example, you could assign the cost of designing the array to the module cost category or to the engineering category with equivalent results. After you assign costs to the categories, you should verify that the total installed cost value is what you expect.

.. note:: The default cost values that appear when you create a file or case are intended to illustrate SAM's use. The cost data are meant to be realistic, but not to represent actual costs for a specific project. For more information and a list of online resources for cost data, see the `SAM website <https://sam.nlr.gov/concentrating-solar-power/csp-cost-data>`__.

   The direct capital costs in $/kWe are in kilowatts of gross power block capacity rather than nameplate capacity because the size and cost of the power block is determined by the gross capacity, not the net capacity. The total installed cost in $/kWe (actually overnight installed cost because it does not include financing during construction costs, which are accounted for on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page) is in kilowatts of nameplate capacity, because that is what is delivered to the grid and is consistent with how costs are reported for utility generation technologies. The indirect costs in $/Wac are in Watts of nameplate power block capacity because those costs that use the entire plant as the basis, not just the power block.

   The Installation Costs page is only available for cases with a cash-flow-based financial mode. It is not available with the No Financial Model option or the LCOE Calculator financial model.
