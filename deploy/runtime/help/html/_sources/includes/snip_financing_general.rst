
The analysis parameters specify the analysis period, inflation rate and discount rate.

**Analysis Period**
  Number of years covered by the analysis. Typically equivalent to the project or investment life. The analysis period determines the number of years in the project :doc:`cash flow <../results/cashflow>`  .

**Inflation Rate**
  Annual rate of change of costs, typically based on a price index, expressed as a percentage. SAM uses the inflation rate to calculate the value of costs in years two and later of the project cash flow based on Year One dollar values that you specify on the :doc:`Operating Costs <../operating-costs/operating_costs>`   page, Financial Parameters page, :doc:`Electricity Rates <../electricity-rates-and-load/electricity_rates>`   page, and :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page.

  The default value of 2.5% is based on consumer price index data from the U.S. Department of Labor Bureau of Labor Statistics, and is the average of the annual average consumer price index between 1991 and 2012.

  The inflation rate may be either a positive or negative value.

**Real Discount Rate**
  A measure of the time value of money expressed as an annual percentage. SAM uses the real discount rate to calculate the present value (value in year one) of dollar amounts in the project cash flow over the analysis period and to calculate annualized costs.

  SAM's financial model results are very sensitive to the real discount rate input. If you plan to compare metrics that depend on discount rate (NPV, PPA price, IRR, LCOE, etc.) to market values of those metrics, you should carefully choose the discount rate for your analysis. If you are comparing these metrics for different scenarios within SAM and use the same discount rate for each scenario, the value of the real discount rate input is less critical because you can evaluate the scenarios based on the relative values of the metrics, e.g., Scenario A with NPV =$1000 is worth more than Scenario B with NPV = $800.

  SAM's default value of the real discount rate is based on a reasonable guess for renewable energy projects in the United States. Because discount rates are very subjective and project developers are typically reluctant to share information about discount rates, published documents on renewable energy finance typically do not include detailed information about discount rates.

.. note:: For projects with one of the PPA financial models, SAM includes both a discount rate and internal rate of return (IRR) in the analysis. For these projects, the discount rate represents the value of an alternative investment, and the IRR can represent a profit requirement or the risk associated with the project. For example, the IRR may be higher than the discount rate for a renewable energy project with higher risk than an alternative investment.

**Nominal Discount Rate**
  SAM calculates the nominal discount based on the values of the real discount rate and the inflation rate: 

*Nominal Discount Rate = [ ( 1 + Real Discount Rate ÷ 100 ) × ( 1 + Inflation Rate ÷ 100 ) - 1 ] × 100*

.. note:: Although the nominal discount rate is an input, SAM also reports its value in the results (see the Data Tables tab on the Results page).