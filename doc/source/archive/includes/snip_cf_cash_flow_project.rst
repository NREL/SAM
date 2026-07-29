
The project cash flows include those from operating activities, investing activities, incentives, and issuance of equity. The pre-tax cash flow is the total project cash flow.

Cash Flow from Operating Activities
-----------------------------------

The cash flow from operating activities are the project earnings including interest on reserves and production-based incentives payments less interest paid on debt.

**EBITDA**
  The earnings before interest, taxes, depreciation and amortization described above.

**Interest on reserves**
*Interest on Reserves = Total Reserves ($) × Interest on Reserves (%)*

  Where *Total Reserves*   is the sum of *Major Equipment Reserves*, *Working Capital Reserve*, and *Debt Service Reserves*, and *Interest on Reserves*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**PBI (Federal, State, Utility, Other, Total)**
  These values represent income from production-based incentive payments, calculated as described below under Incentives.

**Total project cash flow from operating activities**
*Total = EBITDA + Interest on Reserves + Total PBI - Interest*

Cash Flow from Investing Activities
-----------------------------------

**Purchase of property cost**
  The purchase of property cost applies only in Year Zero of the cash flow. It is the sum of the total installed cost from the Installation Costs page and debt-related and financing costs from the Financial Parameters page less any investment-based incentives defined on the Incentives page:

*Purchase of Property Cost = Total Installed Cost*
 *+ Debt Closing Costs*
 *+ Debt Up-front Fee*
 *+ Financing Cost*
 *+ Construction Financing Cost*
 *- Total IBI*
 *- Total CBI*

  The debt up-front fee is the product of the percent of total debt from the Financial Parameters page and the size of debt.

**(Increase)/Decrease in working capital reserve account**
  The working capital reserve amount in Year Zero depends on the Months of Operating Costs from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page and the Year One total operating expense:

*Capital Reserve in Year Zero = Total Operating Expense in Year One ($) × Months of Operating Costs (months) ÷ 12 (months)*

  In Year One and later:

*Capital Reserve in Year n>0 = Capital Reserve in Year n-1 ($) - Total Operating Expense in Year n ($) × Months of Operating Costs (months) ÷ 12 (months)*

  Where *Months of Operating Costs*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**(Increase)/Decrease in major equipment reserve accounts**
  For each of the up to three major equipment reserve accounts that you specify on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, SAM calculates the annual amount required to ensure that sufficient funds are available in the replacement year to cover the replacement cost:

  For example, in the example described below under Major Equipment Capital Spending, the annual increase in reserves is $530,682/year = $2,653,400 ÷ 5 years.

*Increase in Major Equipment Reserve = Inflation Adjusted Replacement Cost in Replacement Year ($) ÷ Replacement Year (year)*

  The increase is shown in the cash flow as a negative value.

  In the year that the replacement occurs (in this example, Year 5), the decrease in major equipment reserve account is $2,122,727 = $2,653,400 - $430,682.

*Decrease in Major Equipment Reserve in Replacement Year = Inflation Adjusted Replacement Cost in Replacement Year - Increase in Major Equipment Reserve*

  The decrease is shown in the cash flow as a positive value.

**Major equipment capital spending**
  For each of the up to three major equipment reserve accounts that you specify on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, the inflation-adjusted amount is reported in the year that you specify.

  For example, if you specify a $2,500 (in Year One dollars) replacement cost with a 5 year replacement frequency, and an inflation rate of 1.5% on the Financial Parameters page, SAM reports a major equipment capital spending amounts in years 5, 10, 15 etc. For Year 5, the amount would be $2,653,400 = $2,500,000 × (1 + 0.015)^4.

  In each year that the replacement occurs:

*Capital Spending = - Replacement Cost (Year One $) × (1 + Inflation Rate)^(Replacement Year - 1)*

  The value is negative because it represents an expense or outflow.

**Total cash flow from investing activities**
  The total cash flow from investing activities is the sum of purchase property cost, reserve account deposits or withdrawals, and capital spending:

*Total in Year Zero = Purchase of Property Cost + Debt Service (increase)/decrease + Working Capital (increase)/decrease*

*Total in Year n>0 = Working Capital (increase)/decrease + Major Equipment (increase)/decrease + Reserve Capital Spending*

Cash Flow from Financing Activities
-----------------------------------

Cash flows from financing activities include income from incentive payments, equity capital, and, for Single Owner and Leveraged Partnership Flip, debt capital.

**Total IBI**
  The total investment-based incentive amount, described below.

**Total CBI**
  The total capital-based incentive amount, described below.

**Equity Capital**
  The equity capital invested in the project.

*Issuance of Equity in Year Zero = Total Installed Cost + Financing Cost - ( Debt Funding + Total IBI + Total CBI )*

  Where *Total Installed Cost*   is the project capital cost from the :doc:`Installation Costs <../installation-costs/index>`   page, *Financing Cost*   is the total debt costs and development fees, *Debt*   (Single Owner and Leveraged Partnership Flip only) is the debt funding amount described below, and *Total IBI*   and *Total CBI*   are the investment-based incentive and capacity based incentive amounts described below.

**Total project cash flow from financing activities**
  The total cash flow from financing activities is the sum of equity capital, incentive payments, and debt capital.

*Total in Year Zero = Issuance of Equity + Total IBI + Total CBI + Debt Funding*

*Total in Year n>0 = Debt Repayment*

**Project total pre-tax cash flow**
  The pre-tax cash flow is the cash flow and accounts for operating expenses, investment earnings, and incentive payments.

*Total Project Pre-tax Cash Flow in Year Zero = Issuance of Equity*

  Where *Issuance of Equity*   is the equity investment in the project described above.

*Total Project Pre-tax Cash Flow in Year n>0 = Total Cash Flows from Operating Activities + Total Cash Flows from Investing Activities + Total Incentives*