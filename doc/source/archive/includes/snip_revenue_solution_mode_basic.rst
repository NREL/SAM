
SAM offers two solution modes for the Commercial PPA and Independent Power Producer financial models:

* Specify IRR Target allows you to specify the internal rate of return (IRR) as an input, and SAM uses a search algorithm to find the PPA price required to meet the target IRR.

* Specify PPA Price allows you to specify the :doc:`PPA price <../financial-metrics/mtf_ppa_price>` as an input, and SAM calculates the resulting IRR.

Solution Mode 1: Specify IRR Target
-----------------------------------

The Specify IRR Target option allows you to specify a desired target IRR. SAM finds the PPA price required to meet the target given the financing assumptions and costs you specify.

SAM uses an iterative algorithm to search for the PPA price that meets the target IRR. If it cannot find a solution, it finds the PPA price that results in an IRR as close as possible to the target value, and reports the :doc:`IRR <../financial-metrics/mtf_irr>` in results as an "actual" value.

 


.. note:: In some cases, the actual IRR may differ significantly from the target IRR. If you are not satisfied with the actual values, you can adjust your assumptions and rerun a simulation until the actual and target values match.


.. note:: If your analysis involves time-of-delivery (TOD) factors, see the Notes under the PPA Price description below (Under the Solution Mode 2 description).

After running a simulation, SAM shows both the target values and the actual values in the :doc:`Metrics table <../results/summary>`.

**Minimum Required IRR**
  The project's minimum internal rate of return target.

**PPA Escalation Rate**
  An escalation rate applied to the PPA price in Year One of the cash flow to calculate the electricity sales price in Years two and later. When the financial optimization option is checked, the PPA escalation rate is a result instead of an input variable.

.. note:: SAM does not apply the inflation rate to the PPA price.

**Constraint: Require a Minimum DSCR**
  A requirement that the debt-service coverage ratio not be allowed to fall below the specified level.

**Minimum Required DSCR**
  The lowest value of the DSCR required for the project to be financially feasible. The DSCR is the ratio of operating income to costs in a given year.

**Constraint: Require a positive cash flow**
  A requirement that the annual project cash flow be positive throughout the project life.

The financial optimization options allow you to automatically optimize the debt percent and PPA escalation rate to minimize the levelized cost of energy. When you optimize the value of these variables, SAM finds the debt percent and PPA escalation rates that result in the lowest levelized cost of energy. This optimization is often necessary to minimize project costs when you specify constraints on the internal rate of return (IRR), debt-service coverage ratio (DSCR), and positive cash flow (See Wiser 1996 in References).

**Allow SAM to pick a debt percent to minimize the LCOE**
  Check this option instead of entering a value for Debt Percent to allow SAM to find the debt percent value that results in the lowest levelized cost of energy. When you check this option, SAM disables the debt percent input variable and reports it as a result in the Metrics table on the Results page.

**Allow SAM to pick a PPA escalation rate to minimize the LCOE**
  Check this option instead of entering a value PPA Escalation Rate to allow SAM to find the PPA escalation rate value that results in the lowest levelized cost of energy. When you check this option, SAM disables the PPA escalation rate input variable and reports it as a result in the Metrics table on the Results page.

Solution Mode 2: Specify PPA Price
----------------------------------

Choose this option when you want SAM to calculate the IRR based on a power purchase bid price that you specify.

After simulations, SAM shows the :doc:`project IRR <../financial-metrics/mtf_irr>` that it calculated in the :doc:`Metrics table <../results/summary>`, along with the :doc:`PPA price <../financial-metrics/mtf_ppa_price>` you specify.

**PPA Price**
  The power purchase bid price in dollars per kilowatt-hour.


 
.. note:: For most analyses, the PPA price is equal to the price in Year One of the :doc:`cash flow <../results/cashflow>`. The price in Years two and later is the PPA price adjusted by the optional escalation rate.


.. note:: If your analysis involves TOD factors, the Year One price is the PPA price that you specify on the Financial Parameters page adjusted by the time-of-delivery (TOD) factors and schedule that you specify either on the :ref:`Revenue <revenue-ppa-tod>` page, or for CSP systems, on the Thermal Energy Storage page.

**PPA Escalation Rate**
  An optional annual power price escalation rate.

.. note:: SAM does not apply the inflation rate to the PPA price.