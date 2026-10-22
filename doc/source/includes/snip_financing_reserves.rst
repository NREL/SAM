
Reserve accounts are funds set aside to cover unexpected costs. Project financial partners may require that the project owner(s) establish and fund reserve accounts. Reserve account funding is a project cost. Interest on reserves contribute to the project's cash flow.

**Interest on Reserves**
  Annual interest rate earned on funds in reserve accounts. The different financial models have different reserve accounts, and the interest on reserves rate applies to all of the accounts available for a given option: 

*   Working capital reserve account, specified under Cost of Acquiring Financing.

*   Major equipment reserve account, specified under Major Equipment Replacement Reserves.

*   Debt service reserve account (Leveraged Partnership Flip, Single Owner), specified under Debt Service.

*   Lessee reserve account (Sale Leaseback), specified under Sale Leaseback.

**Working Capital Reserve Account**
  The working capital reserve account covers cash flow delays, and is sized in months of operating costs. The account is funded in Year zero, earns interest in Years 1 through the end of the analysis period. Funds are released at the end of the analysis period.

  The size of the working capital reserve in months of operation.

*Working Capital Reserve Amount = Months of Operating Costs (months) / 12 months/yr × Year One Total Expenses ($/yr)*

**Debt Service Reserve Account**
  A debt service reserve account is a fund that may be required by the project debt provider. The account is funded in Year 0 and earns interest in Years 1 and later at the reserve interest rate specified under Reserves. The funds in the account are released at the end of the debt period.

  The number of months of principal and interest payments in Year One whose value is equivalent to the size of the debt reserve account in Year 0.

  SAM calculates the reserve account size in Year 0 based on the principal and interest amounts in Year One:

*Year 0 Debt Service Reserve Amount = ( Year One Principal ($/yr) + Year One Interest ($/yr) ) × Debt Service Reserve Account (months) / 12 (months/yr)*

.. note:: Debt Service Reserve Accounts for utility-scale projects are typically sized to cover 6 to 12 months of principal and interest payments.

Major Equipment Replacement Reserve Accounts
--------------------------------------------

Major equipment replacement reserve accounts are funds that the project sets aside to cover the cost of replacing equipment during the analysis period. You can specify up to three replacement reserve accounts.

SAM assumes that the cost of each major equipment replacement is capitalized rather than expensed. You can specify a depreciation schedule for each account.

SAM calculates the inflation-adjusted cost of each major equipment replacement based on the replacement cost you specify, and funds a reserve account in each replacement cycle. At the time of each major equipment replacement, funds are released from the reserve account in an amount sufficient to cover the cost in that year.

.. note:: In SAM, equipment replacement reserve funding is separate from the operating costs that you specify on the :doc:`Operating Costs <../operating-costs/operating_costs>` page. You should use either the replacement reserve account or the operating costs to account for equipment replacements.

**Account Name**
  The name of the reserve account for your reference. SAM reports value associated with each account in the cash flow and other graphs and tables using the name Reserve Account 1, 2, and 3, regardless of the name you enter.

**Replacement Cost**
  The cost in Year One dollars per kW of nameplate capacity.

*Replacement Cost ($) = Replacement Cost (Year One $/kW) × Nameplate Capacity (kW)*

**Replacement Frequency**
  The frequency in years that the replacement cost occurs.

  For example, a replacement cost of $10,000 and frequency of 5 years results in an inflation-adjusted major equipment capital spending amount of $10,000 occurring in Years 5, 10, 15, 20, etc.

**Depreciation Treatment For All Capital Expenditure**
  Specify a federal and state depreciation method for the major equipment replacement cost.

  SAM includes major equipment replacement reserves in the annual total depreciation amount in the cash flow.