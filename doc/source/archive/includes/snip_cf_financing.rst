
**Debt Balance**
  The debt balance in Year One represents the debt portion of the investment costs, less incentives.

  For residential and commercial:

*Debt Balance in Year One = - (Total Installed Costs - Total IBI - Total CBI) × Debt Percent*

  Where *Total Installed Costs*   is from the :doc:`Installation Costs <../installation-costs/index>`   page, *Total IBI* and *Total CBI*   are the sums of all investment-based and capacity-based incentives specified on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page, and *Debt Percent*   is the value specified on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page. *Construction Financing Costs*   is from the Financial Parameters page.

  In Years Two and later, the debt balance is calculated from the previous year's debt balance and debt repayment amounts:

*Debt Balance in Year n>1 = Debt Balance in Year n-1 + Debt Repayment in Year n-1*

 


.. note:: SAM shows the debt balance as a negative value to indicate net annual outflow. A value of zero indicates all debt is paid off.


.. note:: The debt balance in Year One is different from the Principal Amount on the Financial Parameters page when your analysis includes either capacity-based or investment-based incentives.

**Debt Interest Payment**
  The debt interest payment is the annual interest paid on debt:

*Debt Interest Payment in Year n = - Debt Balance in Year n × Loan Rate*

  Where *Debt Balance*   is described above, and *Loan Rate*   is from the Financial Parameters page.

**Debt Repayment**
  The debt repayment amount is the annual payment on principal amount assuming constant payments over the loan term. SAM calculates the amount using the levelized mortgage payment methodology equivalent to Excel's PPMT function:

*Debt Repayment in Year n = -PPMT(Loan Rate,n,Loan Term,Principal Amount,0,0)*

  Where *Loan Rate*, *Loan Term*, and *Principal Amount*   are from the Financial Parameters page.

**Debt Total Payment**
  The total debt payment is the sum of interest and principal payments:

*Debt Total Payment = Debt Interest Payment + Debt Repayment*