
Property tax is an annual project expense that SAM includes under Operating Expenses in the :doc:`cash flow <../results/cashflow>`.

SAM treats property tax as a tax-deductible operating expense for each year. In each year of the project cash flow, the property tax cost is the property tax rate multiplied by the assessed value for that year. 

SAM determines the annual property tax payment by calculating an assessed value for each year in the cash flow, and applying the assessed percent to that value. The assessed value may decline from year to year at the rate you specify. The assessed percent and tax rate both remain constant from year to year.

For residential projects, the property tax amount is the only operating cost that can be deducted from state and federal income tax.

**Assessed Percent**
  The assessed value of property subject to property taxes as a percentage of the system total installed cost specified on the :doc:`Installation Costs <../installation-costs/index>`   page. SAM uses this value to calculate the assessed property value in year one of the project cash flow.

**Assessed Value**
  The assessed property value in Year One of the project cash flow:

  *Assessed Value ($) = Assessed Percent (%) × Total Installed Cost ($)*

  Where Total Installed Cost is from the Installation Costs page.

**Assessed Value Decline**
  The annual decline in the assessed property value. SAM uses this value to calculate the property assessed value in years two and later of the project cash flow. For an assessed value that does not decrease annually, specify a value of zero percent per year.

**Property Tax**
  The annual property tax rate applies to the assessed value of the project in each year of the project cash flow.