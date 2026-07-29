
SAM allows you to specify parameters for up to five construction loans to approximate interest during construction (IDC) that SAM considers to be a cost to the project.

SAM assumes that 100% of the construction balance is outstanding for half of the construction period, which is equivalent to an even monthly draw schedule with an average loan life of half of the construction period. To approximate a different draw schedule, you could adjust the loan's interest rate accordingly.

.. note:: To model a project with no construction period loans, set the **Percent of Installed Costs** value for each of the five loans to zero.

The construction financing cost is part of the project's :doc:`net capital cost <../financial-metrics/mtf_costs>`. SAM includes the construction financing cost in the basis for calculating the basis for depreciation and the investment tax credit (ITC). 

**Construction Loans**
  SAM allows you to specify up to five construction loans. You can type a name describing each loan or use the default names.

**Percent of Installed Costs**
  The amount borrowed for the construction loan as a percentage of the total installed cost, assuming that all construction costs are included in the installation costs you specify on the Installation Costs page. Specify a non-zero percentage for each construction period loan you want to include in the analysis.

  The sum of the up to five percentage values you specify for each construction loan must be 100%.

**Up-front Fee**
  A percentage of the principal amount, typically between 1% and 3% that SAM adds to the interest amount for each construction loan to calculate the total construction financing cost. Note that no interest applies to the up-front fee.

*Up-front Fee Amount ($) = Principal Amount ($) × Up-front Fee Percentage (%)*

**Months Prior to Operation**
  The loan period for the construction loan in months.

**Annual Interest Rate**
  The construction loan interest rate as an annual percentage.

**Principal**
  The amount borrowed for each construction period loan:

*Principal Amount ($) = Total Installed Cost ($) × Percent of Installed Costs (%)*

**Interest**
  The total interest payment due for each construction period loan, assuming that 100% of the construction balance is outstanding for half of the construction period  .

*Interest ($) = Principal Amount ($) × Loan Rate (%/yr) ÷ 12 (mos/yr) × Months prior to operation ÷ 2*

**Total Construction Financing Cost**
  The total construction financing cost is part of the project's capital costs included in the :doc:`net capital cost <../financial-metrics/mtf_costs>` value in the Metrics table.

*Total Construction Financing Cost = Interest + Up-front Fee Amount*