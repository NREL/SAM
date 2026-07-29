Wind Installation costs
=======================

The Wind Installation costs page allows you to specify the costs of a wind power project. For information about sources of wind cost data, see https://sam.nlr.gov/wind.html.

.. note:: SAM 2025.4.16 is the last version of SAM to include built-in wind capital costs models.

   The Land-based Balance-of-system Systems Engineering model (LandBOSSE) is now available as part of the Wind-plant Integrated System Design and Engineering model (WISDEM). For documentation, see https://wisdem.readthedocs.io/en/master/wisdem/landbosse/.

   The Offshore Balance-of-system model is no longer supported. Maness, M.; Maples, B.; Smith, A.; NREL [https://research-hub.nlr.gov/en/publications/nrel-offshore-balance-of-system-model/](Offshore Balance-of-System Model). National Renewable Energy Laboratory, NREL/TP-6A20-66874.

.. include:: ../includes/snip_installation_costs.rst

Capital costs
~~~~~~~~~~~~~

A capital cost represents an expense for a specific piece of equipment or installation service that applies in year zero of the cash flow.

.. note:: Because SAM uses only the total installed cost value in cash flow calculations, how you distribute costs among the different direct capital cost categories does not affect the final results.

For each direct cost category, you can specify the cost in $/kW of wind farm capacity, a fixed cost in $, or a cost per turbine in $/turbine. If you specify more than one cost, for example a foundation cost in both $/kW and $/turbine, SAM adds the values together to calculate the total category cost.

**Turbine cost**
  The cost of a single turbine. You can type values in $/kW, $/turbine, fixed amount, or a combination of the three. The total turbine cost is the sum of three values.

**Balance of system cost**
  Material, labor, and other costs associated with building turbine foundations for the entire wind farm. You can type values in $/kW, $/turbine, fixed amount, or a combination of the three. The total turbine cost is the sum of three values.

**Wind farm capacity**
  The wind farm's nameplate capacity from the :doc:`Wind Farm <../wind-power/wind_farm>`   page.

**Number of turbines**
  The number of turbines in the project from the :doc:`Wind Farm <../wind-power/wind_farm>`   page.

**Sales tax basis, %**
  The percentage of total direct cost used to the calculate sales tax amount.

  SAM calculates the total sales tax amount by multiplying the sales tax rate from the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page by the sales tax basis on the Installation costs page: 

  *Total Sales Tax ($) = Sales Tax Rate (%) × Sales Tax Basis (%) × Total Direct Cost ($)*

  For an explanation of the effect of sales tax on income tax, see **Sales Tax** on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   topic for the financial model you are using (Residential, Commercial, Single Owner, etc.).

**Total installed cost**
  The sum of the total turbine cost and total balance-of-system cost.

**Total installed cost per kW**
  The total installed cost divided by the wind farm nameplate capacity.
