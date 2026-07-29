Industrial Process Heat
=======================

Industrial Process Heat (IPH) systems earn revenue through sales of thermal energy at a power price with optional escalation rate.

Price of Heat
-------------

**PPA price**
  The price in $/MMBtu that the project receives for each unit of thermal energy it delivers.

**PPA price escalation**
  An escalation rate applied to the PPA price in Year One to calculate the power purchase price in years two and later in the project cash flow.

  You can also click |SS_AnnSched-valschedbutton|   and then **Edit** to use the :doc:`Edit Schedule <../window-reference/win_edit_data_table_column>`   window to enter a different PPA price for each year of the analysis period instead of a single value.

  SAM does not apply the inflation rate to the PPA price. If you do not specify a PPA price escalation rate, SAM assumes that the same price applies in all years of the analysis period.

**IRR target year**
  SAM reports the IRR at the end of the analysis period and the IRR in the target year that you specify. You can use the target year to see what the IRR is in a given year. You can also see the IRR for each year in the :doc:`cash flow <../results/cashflow>`  . In Specify PPA mode, the IRR target year does not affect the cash flow.

.. _revenue-iph-tod:

Time of Delivery
~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_revenue_tod_factors.rst



.. |SS_AnnSched-valschedbutton| image:: ../images/SS_AnnSched-valschedbutton.png
