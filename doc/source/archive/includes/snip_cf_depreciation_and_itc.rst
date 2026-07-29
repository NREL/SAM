
The federal and state Depreciation and ITC tables show the depreciable basis calculation for each of the up to seven depreciation classes for federal and state tax purposes:

* The total depreciable amount includes the total installed cost and financing costs.

* The depreciation class allocations on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page determine how the depreciable basis is allocated to the different depreciation classes (MACRS 5-yr, Straight Line, etc.).

* For each state and federal IBI and CBI with **Reduces Depreciation and ITC Bases** checked on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>` page, the full incentive amount reduces the depreciable basis.

* For each state and federal ITC with **Reduces Depreciation Basis** checked on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>` page, 50% of the tax credit amount reduces the depreciable basis.

* For each depreciation class with a checked box under **Bonus Depreciation** on the Financial Parameters page, the depreciable basis is the product of the bonus depreciation percentage and the adjusted depreciable basis. (The adjusted depreciable basis is the total depreciable after incentives and tax credit adjustments.)

* For state and federal taxes, depreciation for major equipment replacement reserves uses the class specified on the Financial Parameters page.

The depreciation amounts depend on the total installed cost from the :doc:`Installation Costs <../installation-costs/index>` page, and the following inputs from other pages.

From the Financial Parameters page:

* Development Fee

* Debt Closing Costs

* Debt Closing Fee

* Equity Closing Cost

* Construction Financing

* Other Financing Cost

From the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>` page:

* State ITC as Percentage

* State ITC Maximum

* State ITC as Amount

* Federal ITC as Percentage

* Federal ITC Maximum

* Federal ITC as Amount

* Status of **Reduces Depreciation Basis** check boxes

* The amounts and percentages of any incentive with **Reduces Depreciation and ITC Bases** checked

From the :doc:`Depreciation <../incentives-and-depreciation/tax_credits_depreciation>` page:

* Allocations for each depreciation class

* Bonus depreciation amounts and applicable depreciation classes

* ITC qualification status of depreciation classes

The depreciation amounts also depend on the following cash flow values:

* Debt service reserve

* Working capital reserve

* IBI (only for incentives shown to reduce depreciation basis on Incentives page)

* CBI (only for incentives shown to reduce depreciation basis on Incentives page)

* Debt funding

* For state depreciation, the federal ITC basis disallowance amounts for each depreciation class

* For federal depreciation, the state ITC basis disallowance amounts for each depreciation class

Gross Depreciable Basis with IBI and CBI Reductions Before ITC Reductions
-------------------------------------------------------------------------

For each depreciable class, the depreciable basis before reduction by the ITC is the gross depreciable basis less IBI and CBI amounts for incentives on the Cash Incentives page with **Reduces Depreciation and ITC Bases** checked.

.. note:: The IBI and CBI reduce the depreciation basis for state taxes only when **State** under **Reduces Depreciation and ITC Bases** is checked. Similarly, the IBI and CBI reduce the depreciation basis for federal taxes only when **Federal** under **Reduces Depreciation and ITC Bases**.

**% of Total Depreciable Basis**
  The normalized allocation for each depreciation class allocation:

*% of Total Depreciable Basis = Allocation ÷ Sum of Allocations*

  Where *Allocation*   is the percentage for the given depreciable class, and *Sum of Allocations*   is the sum of all allocations from the Financial Parameters page.

**Gross Amount Allocated**
  The gross depreciable basis before reductions for each depreciation class:

*Gross Amount Allocated = % of Total Depreciable Basis × Total Depreciable Amount*

  Where % of Total Depreciable Basis is described above, and

*Total Depreciable Amount = Total Installed Cost + Development Fee + Equity Closing Cost + Debt Closing Costs + Debt Closing Fee × Funding + Debt Service Reserve + Working Capital Reserve + Lease Payment Reserve*

  Where *Total Installed Cost*   is from the :doc:`Installation Costs <../installation-costs/index>`   page; *Development Fee*, *Equity Closing Cost*, *Debt Closing Costs*, and *Debt Closing Fee*   are from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page; and *Debt Service Reserve*, *Working Capital Reserve*   and *Lease Payment Reserve*   (Sale Leaseback financial model only) are other values in the cash flow described above

**Reduction: IBI**
  The reduction in depreciation basis from IBI payments:

*Reduction IBI = Total IBI that Reduce Depreciation × % of Total Depreciable Basis*

  Where *Total IBI that Reduce Depreciation*   is the sum of IBI values in the cash flow for incentives with **Reduces Depreciation and ITC Bases** checked on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page. For the state depreciation table, **State** must be checked for the incentive to reduce the state depreciation basis. For the federal depreciation table, **Federal** must be checked. *% of Total Depreciable Basis*   is the allocation for the depreciation class described above.

**Reduction: CBI**
  The reduction in depreciation basis from CBI payments:

*Reduction CBI = Total CBI that Reduce Depreciation × % of Total Depreciable Basis*

  Where *Total CBI that Reduce Depreciation*   is the sum of CBI values in the cash flow for incentives with **Reduces Depreciation and ITC Bases** checked on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page. For the state depreciation table, **State** must be checked for the incentive to reduce the state depreciation basis. For the federal depreciation table, **Federal** must be checked. *% of Total Depreciable Basis*   is the allocation for the depreciation class described above.

**Depreciable Basis Prior to ITC**
  The depreciable basis reduced by CBI and IBI amounts:

*Depreciable Basis Prior to ITC = Gross Amount Allocated - Reduction: IBI - Reduction: CBI*

ITC Reduction
-------------

For each ITC on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>` page with **Reduces Depreciation Basis** checked, 50% of the ITC amount can be included in the depreciable basis for each depreciable class with **ITC Qualification** checked on the :doc:`Depreciation <../incentives-and-depreciation/tax_credits_depreciation>` page. SAM calculates the ITC reduction amount for ITCs that you specify on the Incentives page a percentage of the total installed costs with a maximum amount, and ITCs that you specify as a fixed amount.

.. note:: The ITC reduces the depreciation basis for state taxes only when **State** under **Reduces Depreciation Basis** is checked on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>` page, and when **State** is checked under **ITC Qualification** for the depreciation class on the :doc:`Depreciation <../incentives-and-depreciation/tax_credits_depreciation>` page. Similarly, the ITC reduces the depreciation basis for federal taxes only when **Federal** is checked under **Reduces Depreciation Basis** and under **ITC Qualification** for the depreciation class.

For each ITC specified as a percentage and maximum on the Incentives page, the *ITC Basis Disallowance* is the amount that may be available for depreciation basis reduction. (The ITC Reduction amounts are the amounts actually available.):

**ITC Qualifying Costs**
  For depreciation classes with **ITC Qualification** checked on the :doc:`Depreciation <../incentives-and-depreciation/tax_credits_depreciation>`   page:

*ITC Qualifying Costs = Depreciable Basis Prior to ITC*

**% of ITC Qualifying Costs**
  For each depreciable class:

*% of ITC Qualifying Costs = ITC Qualifying Costs ÷ Total Depreciable Basis Prior to ITC*

  Where *Total Depreciable Basis Prior*   to ITC is the sum of *Depreciable Basis Prior to ITC*   for all depreciable classes.

**ITC Amount**
*ITC Amount = % of ITC Qualifying Costs × ITC Qualifying Costs*

**ITC Basis Disallowance**
  The ITC depreciation basis disallowance is 50% of the ITC amount:

*ITC Basis Disallowance = ITC Amount × 0.5*

For each ITC specified as a fixed amount on the Incentives page, the ITC Basis Disallowance is the amount that may be available for depreciation basis reduction. (The ITC Reduction amounts are the amounts actually available.):

**ITC Amount**
  For each depreciable class, the Total ITC Amount is the amount that may be available for depreciation reduction:

*ITC Amount = Total ITC Amount × % of ITC Qualifying Costs*

  Where, for the state depreciation table, *Total ITC Amount*   is the state ITC amount from the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page. For the federal depreciation table, *Total ITC Amount*   is the federal ITC amount from the Incentives page. 

**ITC Basis Disallowance**
  The ITC depreciation basis disallowance is 50% of the ITC amount:

*ITC Basis Disallowance = ITC Amount × 0.5*

The depreciable basis after ITC reduction is the sum of the total ITC basis disallowance values for the ITCs with **Reduces Depreciation Basis** checked on the Incentives pages.

**ITC Reduction: State**
*ITC Reduction: State = ITC Basis Disallowance (ITC as %) + ITC Basis Disallowance (ITC as Fixed Amount)*

  Where the ITC basis disallowance values are from the state depreciation table for the ITCs that reduce depreciation basis.

**ITC Reduction: Federal**
*ITC Reduction: Federal = ITC Basis Disallowance (ITC as %) + ITC Basis Disallowance (ITC as Fixed Amount)*

  Where the ITC basis disallowance values are from the federal depreciation table for the ITCs that reduce depreciation basis.

**Depreciable Basis after ITC Reduction**
*Depreciable Basis after ITC Reduction = Depreciable Basis Prior to ITC - ITC Reduction:State - ITC Reduction:Federal*

Bonus Depreciation
------------------

For each depreciation class that qualifies for bonus depreciation as indicated by the check boxes under Bonus Depreciation on the :doc:`Depreciation <../incentives-and-depreciation/tax_credits_depreciation>` page, bonus depreciation percentage applies to the depreciable basis.

.. note:: The bonus depreciation percentage applies to the depreciation basis for state taxes only when **State** under **Bonus Depreciation** is checked. Similarly, the bonus depreciation percentage applies for federal taxes only when **Federal** under **Reduces Depreciation Basis** is checked.

**First Year Bonus Depreciation**
  For each depreciation class with **Bonus Depreciation** checked on the Depreciation page:

*First Year Bonus Depreciation = Depreciable Basis after ITC Reduction × Bonus Depreciation Percentage*

  Where Bonus Depreciation Percentage is from the Depreciation page: The state bonus percentage applies to the state depreciation table, and the federal percentage applies to the federal depreciation table.

Depreciable Basis
-----------------

The depreciable basis after IBI, CBI, ITC and bonus depreciation reduction is the basis to which the depreciation percentages defined by the depreciation class apply.

*Depreciable Basis after Bonus Reduction = Depreciable Basis after ITC Reduction - First Year Bonus Depreciation*

The following table shows the depreciation percentage by year for each depreciation class. For each depreciation class, the percentage is applied to the depreciable basis amount for the given year in the cash flow:

.. image:: /images/TBL_DepreciationClasses.png
   :align: center
   :alt: TBL_DepreciationClasses.png

