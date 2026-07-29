
**Federal and State Income Tax Rates**
  The annual federal and state income tax rate applies to taxable income and is used to calculate the project's tax benefits and liabilities.

  You can specify either a single annual tax rate, or use the :doc:`Edit Schedule <../window-reference/win_edit_data_table_column>`   window to specify a tax rate for each year. The latter option is useful for modeling a tax holiday where the tax rate for the first few years is zero.

  For all projects, taxable income includes income from any incentives marked on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page as taxable.

  For residential and commercial projects, SAM does not consider the value of electricity saved by the system to be taxable income. For commercial projects, because those savings represent the value of electricity purchases that would have been a tax-deductible operating expense to the commercial entity, SAM does reduce the project cash flow by the amount of federal and state income tax on the value of the electricity. In other words, with the renewable energy system in place, the commercial entity must pay tax on that portion of its income that it would have deducted as an operating expense.

  For PPA projects, taxable income includes earnings before interest, taxes, depreciation and amortization (EBIDTA) and interest earned on reserve accounts. EBIDTA is revenue from electricity sales revenue (PPA revenue) less annual operation and maintenance, property tax, and insurance expenses.

**Sales Tax**
  The sales tax is a one-time tax that SAM includes in the project's total installed cost. SAM calculates the sales tax amount by multiplying the sales tax rate on the Financial Parameters page by the rate you specify under Indirect Capital Costs and the Total Direct Cost on the :doc:`Operating Costs <../operating-costs/operating_costs>`   page.

  For tax purposes, because SAM includes the sales tax amount in the total installed cost, it treats sales tax as part of the cost of property. For projects with depreciation (Commercial and PPA financial models only), SAM includes the sales tax amount in the depreciable basis. See IRS Publication 551, Basis of Assets, for more details.

  Some states and other jurisdictions offer a sales tax exemption for renewable energy projects. To model a sales tax exemption in SAM, reduce the sales tax percentage as appropriate. For example, for a 100% sales tax exemption, enter a sales tax rate of zero.

  For projects with debt, because SAM includes the sales tax amount in the total installed cost, the sales tax influences the debt amount and debt interest payment. For projects where debt interest payments are deductible from federal and state income tax (all financial models except Residential with standard loan), SAM includes sales tax in the calculation of the deductions.

**Insurance Rate (Annual)**
  SAM treats annual insurance payments as part of the annual operating costs. The insurance cost in year one of the project cash flow is the insurance rate multiplied by the total installed cost from the Operating Costs page. The first year cost is then increased by inflation in each subsequent year. For commercial and PPA projects, the insurance cost is a tax- deductible operating expense.