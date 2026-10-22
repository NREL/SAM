
The Project Term Debt inputs determine the :ref:`size of debt <debt>` or amount borrowed, and debt-related costs. You can see the annual debt interest, principal and total payments on the :doc:`Cash Flow <../results/cashflow>` tab of the :doc:`Results <../getting-started/results_page>` page.

Debt Sizing Options
-------------------

SAM allows you to size the debt as a percentage of the total installed cost or based on the debt service coverage ratio:

* For the debt percent option, you specify the size of debt as a percentage of the total installed cost plus any additional fees and incentives. This option is appropriate for projects with mortgage-style debt, where annual total principal and interest payments are constant, and the annual debt-service coverage ratio (DSCR) varies from year to year.

* For the debt-service coverage ratio (DSCR) option, you specify the DSCR, and allow SAM to size the debt based on the annual cash available for debt service from the project cash flow. The size of debt depends on annual revenue and operating costs rather than the total installed cost. This type of debt is sometimes called sculpted debt, where the total annual debt payment varies from year to year, and the annual DSCR is constant throughout the debt period.

Project Term Debt
-----------------

**Debt percent, % of total installed cost**
  The size of debt as a percentage of the installed cost.

*size of debt = debt percent / 100 × ( total installed cost  + total construction financing cost - total IBI amount - total CBI amount )*

  SAM displays the weighted average cost of capital (WACC) for reference (see below).

  SAM reports the size of :ref:`debt <debt>`   and minimum debt service coverage ratio in the Metrics table on the :doc:`Summary <../results/summary>`   page after you run simulations.

**DSCR**
  The debt service coverage ratio is the ratio of annual cash available for debt service (CAFDS) to the sum of the annual principal and interest payment. For a given year, the total annual debt payment (principal and interest) is:

*annual debt payment = CAFDS ÷ DSCR*

  Annual cash available for debt service is equal to the earnings before interest, taxes, depreciation, and amortization (EBITDA) less cash used to fund the major equipment replacement reserves.

  For this option, the size of debt (amount borrowed) is the present value of the annual CAFDS discounted at the debt interest rate.

  SAM assumes that the debt service coverage ratio remains constant over the analysis period.

  SAM reports the size of :ref:`debt <debt>`   in the Metrics table on the Summary page after you run simulations.

 


.. note:: For the DSCR option, because the size of debt depends on annual revenue, if the project revenue is very high compared to its costs, the resulting size of debt can be unrealistically high. This may be an indication that the PPA price or incentives are too high, or that project costs are too low, or that there is a problem with some other financial parameter. Be sure to check the values in the Metrics table on the Summary tab of the results page to make sure that the debt percent, NPV, PPA price, and IRR are reasonable.


.. note:: The DSCR generally ranges between 1.40 and 1.50 for proven wind technology. For solar, the ratios are slightly lower: In the 1.30 to 1.40 range for PV, and perhaps slightly lower for CSP and CPV technologies.

**Equal payments (standard amortization)**
  Available only with the debt percent option. The size of annual debt payments remains constant over the debt payment period, the annual interest payment decreases each year with the unpaid balance, and the annual principal payment increases.

**Fixed principal declining interest**
  Available only with the debt percent option. The size of annual principal payments remains constant over the debt payment period, and the size of annual interest payments decreases with the unpaid balance so that the total annual payment decreases over the payment period.

**Maximum debt fraction, %**
  Available only with the DSCR option for the Single Owner financial model. The maximum size of debt as a percentage of the total installed cost you want to allow.

  Check the box to limit the debt fraction for a project with relatively high revenue compared to costs.

  This option adjusts the debt service coverage ratio so that the resulting size of debt is less than the maximum debt fraction you specify. If the DSCR you specify results in a size of debt greater than the limit, you can use the Minimum DSCR output on the Data Tables tab of the Results page to see the adjusted DSCR.

**Tenor, years**
  The debt period in years.

**Annual interest rate, %**
  Annual nominal debt interest rate.

**Moratorium, years**
  Defines a grace period before debt payments begin, available for debt percent option only. The number of years starting at the beginning of the debt payment period (Year 1 of the project cash flow) before debt principal payments begin. For example, for a moratorium of 5 years and a 20-year debt period, principal payments would begin in Year 6 and end in Year 20.

**Debt closing costs**
  A dollar amount representing debt closing costs. The debt closing costs are part of the project's capital costs included in the :doc:`net capital cost <../financial-metrics/mtf_costs>` value in the Metrics table.

**Up-front fee, % of total debt**
  A percentage of the total debt representing debt closing costs. The up-front fee is part of the project's capital costs included in the :doc:`net capital cost <../financial-metrics/mtf_costs>` value in the Metrics table.

.. include:: /includes/snip_wacc.rst

