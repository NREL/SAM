Debt and Equity
===============

The debt and equity metrics show the amount borrowed and initial cash investment amount for financial models with project term debt. Project term debt is debt used to finance the project and repaid over the analysis period, or life of the project. The all equity partnership flip and sale leaseback models do not include debt.

To see how debt is calculated, see the project cash flow on the :doc:`Cash Flow <../results/cashflow>` tab on the Results page.

.. _equity:

Equity
~~~~~~

For projects with term debt, equity is the amount of cash invested by the project owner or investor, based on debt as described below and :doc:`net capital cost <mtf_costs>`:

*Equity ($) =**Net capital cost**($) - Debt ($)*

.. _debt:

Debt
~~~~

For projects with term debt, debt is the amount borrowed by the project. How SAM calculates the debt amount depends on the financial model.

Residential and Commercial
--------------------------

For the residential and commercial financial models, size of debt depends on the debt percent and :doc:`net capital cost <mtf_costs>`:

*Debt = debt percent (%) × net capital cost ($*

Where the debt percent is from the Financial Parameters page, and the net capital cost is the total installed cost less cash incentives.

Single Owner, Leveraged Partnership Flip, Community Solar
---------------------------------------------------------

For the single owner and leveraged partnership flip financial models, on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page, you can either specify the debt size as a fraction of :doc:`net capital cost <mtf_costs>`, or specify a debt-service coverage ratio (DSCR).

**Debt percent**
  When you specify the debt percent as an input, SAM assumes constant debt payments, and reports the debt size as a fraction of the :doc:`net capital cost <mtf_costs>`  :

*Debt = debt percent (%) × net capital cost ($)*

  The minimum DSCR is the smallest annual debt service coverage ratio from the cash flow, where:

*DSCR in Year n = cash available for debt service in Year n ÷ Total P&I payment in Year n*

**DSCR**
  When you specify the DSCR as an input, SAM calculates the size of debt from the project's annual revenue and expenses assuming a constant annual debt service coverage ratio (DSCR). The DSCR in a given year is the ratio of cash available for debt service (CAFDS) to the total debt payment in that year.

  SAM calculates the debt funding amount from the CAFDS in the :doc:`cash flow <../results/cashflow>`   and the term debt interest rate *r*   and constant *DSCR*   from the Financial Parameters page:

.. image:: ../images/EQ_Debt-so-lpf.png
   :align: center
   :alt: EQ_Debt-so-lpf.png

**Cash Available for Debt Service**
  The cash available for debt service (CAFDS) depends on project revenue, expenses, and equipment reserve funding:

*CAFDS in Year n= EBIDTA in Year n - total equipment reserve funding in Year n*

  Where EBIDTA is the project's net pre-tax earnings and equipment reserve funding are from the :doc:`cash flow <../results/cashflow>`  .

.. note:: When you use the DSCR option to size debt, if the project revenue is very high compared to the project costs, you may get an unrealistically high size of debt. Use the debt percent value shown in the Metrics table on the :doc:`Summary <../results/summary>` tab of the Results page to determine whether the size of debt is reasonable for your analysis. See :ref:`Interpreting the NPV <interpreting>`.