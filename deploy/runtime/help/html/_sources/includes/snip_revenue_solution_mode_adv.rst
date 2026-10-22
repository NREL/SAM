
The Solution Mode inputs are where you determine how SAM calculates the PPA price, IRR, and other results that appear in the Metrics table on the :doc:`Summary <../results/summary>` page.

The PPA price is the bid price in a power purchase agreement (PPA), and is the price that the project receives for each unit of electricity that the system generates. The :doc:`internal rate of return (IRR) <../financial-metrics/mtf_irr>` is a measure of the project's profitability, and is defined as the nominal discount rate that corresponds to a net present value (NPV) of zero.

The solution mode determines whether SAM calculates a PPA price based on an IRR target that you specify, or an IRR based on a PPA price that you specify:

* **Specify IRR Target** allows you to specify the IRR as an input, and SAM uses a search algorithm to find the PPA price required to meet the target IRR.

* **Specify PPA Price** allows you to specify the :doc:`PPA price <../financial-metrics/mtf_ppa_price>` as an input, and SAM calculates the resulting :doc:`IRR <../financial-metrics/mtf_irr>`.

Solution Mode 1: Specify IRR Target
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Specify IRR Target option allows you to specify an IRR target value and the year that you would like the IRR to be achieved. SAM finds the :doc:`PPA price <../financial-metrics/mtf_ppa_price>` required to meet the target given your assumptions including financial parameters, :doc:`incentives <../incentives-and-depreciation/cash_incentives>` and :doc:`depreciation <../incentives-and-depreciation/tax_credits_depreciation>`, and the project's :doc:`installation <../installation-costs/index>` and :doc:`operating <../operating-costs/operating_costs>` costs.

SAM uses an iterative algorithm to search for the PPA price that meets the IRR target in the year you specify. If it cannot find a solution, it finds the PPA price that results in an IRR and year as close as possible to the target values.

.. note:: For the Specify IRR Target option, SAM does not know the PPA price at the start of the simulation. In some situations, SAM requires knowledge of the PPA price when the simulation starts, so you must use the Specify PPA Price option:

   1. For the PPA Price option on the :doc:`Electricity Purchases <../electricity-rates-and-load/electricity_purchases>` page, SAM needs to know the PPA price as the simulation runs to calculate the cost of purchasing electricity to meet parasitic loads that occur when the system is not generating power, such as for photovoltaic inverter night-time consumption, battery charging, or concentrating solar power freeze protection.

   2. For battery dispatch options on the :doc:`Battery Dispatch <../battery-storage/battery_dispatch_fom>` page that charge or discharge the battery the battery in response to the power price.

**IRR target**
  The desired IRR target as a percentage:

  * For the Single Owner model the required IRR is the project IRR.

  * For the All Equity and Leveraged Partnership Flip and Sale Leaseback models, the target IRR is the tax investor IRR. SAM calculates the developer IRR as a function of the value in excess of the tax investor IRR.

**IRR target year**
  The year in which you want the target IRR to be achieved. For the partnership flip options, this is the flip year when project returns switch from the tax investor (pre-flip) to the developer (post-flip).

Solution Mode 2: Specify PPA Price
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Specify PPA Price option allows you to specify the PPA price. SAM calculates the resulting :doc:`IRR <../financial-metrics/mtf_irr>`:

* For the Single Owner model, SAM calculates the project IRR.

* For All Equity and Leveraged Partnership Flip, and Sale Leaseback models that involve two parties, SAM calculates two IRR values: One from the tax investor perspective, and one from the developer perspective. For the partnership flip options, SAM also calculates the flip year when project returns switch from the tax investor to the developer.

**PPA Price**
  The power price in dollars per kWh. This is the price that would be negotiated as part of a power purchase agreement. 

  You can also click |SS_AnnSched-valschedbutton|   and then **Edit** to use the :doc:`Edit Schedule <../window-reference/win_edit_data_table_column>`   window to enter a different PPA price for each year of the analysis period instead of a single value.

.. note:: Regardless of the solution mode you choose, the PPA price is associated with the annual PPA price escalation rate if it increases from year to year, and the set of multipliers defined on the :ref:`Revenue <revenue-ppa-tod>` page. Be sure to use the "Uniform Dispatch" option for projects that do not involve time-of-delivery adjustments to the PPA price.

**IRR target year**
  SAM reports the IRR at the end of the analysis period and the IRR in the target year that you specify. You can use the target year to see what the IRR is in a given year. You can also see the IRR for each year in the :doc:`cash flow <../results/cashflow>`  . In Specify PPA mode, the IRR target year does not affect the cash flow.

Escalation Rate
~~~~~~~~~~~~~~~

An escalation rate applied to the PPA price in Year One to calculate the power purchase price in years two and later in the project cash flow.

SAM does not apply the inflation rate to the PPA price. If you do not specify a PPA price escalation rate, SAM assumes that the same price applies in all years of the analysis period.

.. |SS_AnnSched-valschedbutton| image:: /images/SS_AnnSched-valschedbutton.png
