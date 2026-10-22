System costs
============

To view the Dish System costs page, click **Dish System costs** on the main window's navigation menu. Note that for the dish input pages to be available, the technology option in the :doc:`Technology and Market <../getting-started/choose_models>` window must be Concentrating Solar Power - Dish Stirling System.

SAM uses the variables on the Dish System costs page to calculate the project investment cost and annual operating costs reported in the project :doc:`cash flow <../results/cashflow>` and used to calculate cost metrics reported in the Metrics table on the :doc:`Results page <../getting-started/results_page>`.

Because only the total installed cost value affects the cash flow calculations, you can assign capital costs to the different cost categories in whatever way makes sense for your analysis. For example, you could assign the cost of designing the solar field to the site improvements cost category or to the engineer-procure-construct category with equivalent results. The categories are provided to help you keep track of the different costs, but do not affect the economic calculations. After assigning costs to the categories, verify that the total installed costs value is what you expect.

Variable values in boxes with white backgrounds are values that you can edit. Boxes with blue backgrounds contain calculated values or values from other pages that SAM displays for your information.

.. include:: ../includes/snip_csp_installation_notes.rst

Direct Capital costs
~~~~~~~~~~~~~~~~~~~~

A direct capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

**Site Improvements ($/m2)**
  A cost per square meter of solar field area to account for expenses related to site preparation and other equipment not included in the solar field cost category.

**Collector Cost (Projected Area) ($/m2)**
  A cost per square meter of projected mirror area from the Collector page to account for expenses related to installation of the collectors, including labor and equipment.

**Receiver Cost ($/kW)**
  A cost per kW of engine rated capacity from the Stirling Engine page to account for expenses related to installation of the receiver, including labor and equipment.

**Engine Cost ($/kW)**
  Cost per kW of engine rated capacity from the Stirling Engine page to account for expenses related to installation of the Stirling engine components, including labor and equipment.

**Contingency (%)**
  A percentage of the sum of the site improvements, solar field, HTF system, storage, fossil backup, and power plant costs to account for expected uncertainties in direct cost estimates.

  Total Direct Cost ($)

  The sum of site improvements, collector cost, receiver cost, engine cost, and contingency costs.

Indirect Capital costs
~~~~~~~~~~~~~~~~~~~~~~

An indirect cost is typically one that cannot be identified with a specific piece of equipment or installation service.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different indirect capital cost categories does not affect the final results.

Total Land Area
---------------

The total land area required for the project, from the Solar Field or Heliostat Field page.

Nameplate
---------

The system's nameplate capacity from the Power Block or Power Cycle page.

EPC and Owner costs
-------------------

EPC (engineer-procure-construct) and owner costs are associated with the design and construction of the project. SAM calculates the total cost as the sum of the Non-fixed Cost and Fixed Cost.

Typical costs that may be appropriate to include in the EPC and Owner category are: Permitting, royalty payments, consulting, management or legal fees, geotechnical and environmental surveys, interconnection costs, spare parts inventories, commissioning costs, and the owner's engineering and project development activities.

Total Land costs
----------------

costs associated with land purchases, which SAM calculates as the sum of a non-fixed cost and a fixed cost. Use the Land category described below for land costs, and inputs on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page for financing costs.

Units for Land and EPC costs
----------------------------

SAM calculates the total EPC and Owner costs and Total Land costs by adding the four costs that you can specify using different units:

**Cost per acre**
  A cost in dollars per total land area in acres.

**% of Direct Cost**
  A cost as a percentage of **Total Direct Cost** under **Direct Capital Cost**.

**Cost per Wac**
  A cost in dollars per AC Watt of nameplate capacity.

**Fixed Cost**
  A fixed dollar amount

Sales Tax (%)
-------------

.. include:: ../includes/snip_sales_tax.rst

Total Indirect Cost ($)
-----------------------

The sum of engineer-procure-construct costs, project-land-miscellaneous costs, and sales tax.

Total Installed Cost
~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_total_installed_cost.rst

Operation and Maintenance costs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_o_and_m_costs_no_fossil.rst

Entering Periodic Operation and Maintenance costs
-------------------------------------------------

.. include:: ../includes/snip_o_and_m_periodic_costs.rst

About the CSP Default Cost Assumptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_csp_cost_assumptions.rst

