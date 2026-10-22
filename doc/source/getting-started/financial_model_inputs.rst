Financial Model Inputs
======================

Annual Electrical Output
------------------------

The cash flow models for the different financial models require a single value representing the system's total electrical output in a single year to determine the project's annual income (PPA models) or savings (distributed generation models). The :doc:`performance model <../introduction/technology_options>` calculates this value by adding up the results of an hourly (or sub-hourly) simulation of the system's performance over the year. The :doc:`weather data <../weather-data/weather_data>` and system's technical specifications from the performance model's input pages determine the annual output of the system.

When you specify a **degradation rate** on the :doc:`Degradation <../degradation/degradation>` page, SAM reduces the annual output from year to year in Years Two and later. When the value rate is zero, SAM assumes that the annual output is the same for all years in the analysis period.

.. note:: For :doc:`geothermal systems <../geothermal/geo_overview>`, SAM uses a different method for calculating annual output that depends on the long-term resource data rather than the degradation rate.

Financial Model Inputs
----------------------

SAM's input pages are organized so that groups of related input variables appear together. Variables on the following input pages are inputs to the financial models. The input pages that are available depend on the financial model:

:doc:`Installation Costs <../installation-costs/index>`: Costs associated with installing the project, including equipment and land purchases, labor, and engineering, permitting, and construction costs.

:doc:`Operating Costs <../operating-costs/operating_costs>`: Costs associated with operating the project, including operating, maintenance, and equipment replacement costs.

Financial Parameters: Financial structure, debt parameters, tax and insurance rates, partner shares, etc:

* :doc:`Residential <../financial-parameters/fin_residential>`


* :doc:`Commercial <../financial-parameters/fin_commercial>`


* :doc:`Third Party Ownership - Host <../financial-parameters/fin_tpo_host>`


* :doc:`Third Party Ownership - Host / Developer <../financial-parameters/fin_tpo_host_developer>`


* :doc:`Single Owner <../financial-parameters/fin_single_owner>`


* :doc:`Merchant Plant <../financial-parameters/fin_merchant_plant>`


* :doc:`All Equity Partnership Flip <../financial-parameters/fin_leveraged_partnership>`


* :doc:`Leveraged Partnership Flip <../financial-parameters/fin_leveraged_partnership>`


* :doc:`Sale Leaseback <../financial-parameters/fin_sale_leaseback>`


* :doc:`LCOE Calculator <../financial-parameters/fin_lcoefcr>`


Incentives:

* :doc:`Incentives <../incentives-and-depreciation/cash_incentives>` (all models): Tax credits and cash incentives.

* :doc:`Depreciation <../incentives-and-depreciation/tax_credits_depreciation>` (commercial and PPA models): Accelerated depreciation tax benefit.

Electricity Rates and Load:

* :doc:`Electricity Rates <../electricity-rates-and-load/electricity_rates>` (residential and commercial models): Retail electricity pricing.

* :doc:`Electric Load <../electricity-rates-and-load/electricity_load>` (residential and commercial models): Building electricity usage.

Thermal Rates and Load:

* :doc:`Thermal Rates <../thermal-rates-and-load/thermal_rates>` (fuel cell model with commercial financial model): Retail thermal energy pricing.

* :doc:`Thermal Load <../thermal-rates-and-load/thermal_load>` (fuel cell model): Thermal energy usage.

Financial Model Results
-----------------------

The :doc:`Results page <results_page>` displays the :doc:`cash flow <../results/cashflow>` and economic :doc:`metrics <../results/summary>` that summarize the cash flow, such as the :doc:`levelized cost of energy (LCOE) <../financial-metrics/mtf_lcoe>`, :doc:`net present value (NPV) <../financial-metrics/mtf_npv>`, and others depending on the financial model.