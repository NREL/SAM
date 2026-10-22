
The incentive cash flow rows show the value of cash incentives and tax credits, which are used to calculate cash flows described above.

**IBI (Investment Based Incentives)**
  Each IBI (federal, state, utility, other) applies in Year Zero of the project cash flow. 

  Because you can specify each IBI on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page as either an amount or a percentage, SAM calculates the value of each IBI as the sum of two values:

*IBI as Amount = Amount*

*IBI as Percentage = Total Installed Cost ($) × Percentage (%), up to Maximum*

*IBI in Year 0 = IBI as Amount + IBI as Percentage*

  Where *Amount*, *Percentage*   and *Maximum*   are the values   that you specify on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page, and *Total Installed Cost*   is from the :doc:`Installation Costs <../installation-costs/index>`   page.

*Total IBI*   is the sum of the four IBI values (federal, state, utility, other).

.. note:: The IBI amount reduces the after tax cost flow in Year Zero, and the debt balance in Year One. This is because SAM assumes that debt payments begin in Year One, when the project is generating or saving electricity.

**CBI (Capacity Based Incentives))**
  Each CBI (federal, state, utility, other) applies in Year Zero of the project cash flow:

*CBI in Year 0 = System Capacity (W) × Amount ($/W), up to Maximum*

  Where System Capacity is the rated capacity of the system, and *Amount*   and *Maximum*   are the values you specify on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page.

*Total CBI*   is the sum of the four CBI values (federal, state, utility, other).

.. include:: /includes/snip_system_capacity.rst

**PBI (Performance Based Incentive)**
  Each PBI (federal, state, utility, other) applies in Years One and later of the project cash flow, up to the number of years you specify:

*PBI in Year n = Amount ($/kWh) × Energy in Year n (kWh) × (1 + Escalation)^(n-1), up to Term*

  Where *Amount*, *Term*, and *Escalation*   are the values you specify on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page, and *Energy*   is the value displayed in Energy row of the cash flow table (described above).

.. note:: If you use an annual schedule to specify year-by-year PBI amounts, SAM ignores the escalation rate.

*Total PBI*   is the sum of the four PBI amounts (federal, state, utility, other).

.. note:: If you specify a PBI amount on the Cash Incentives page, be sure to also specify the incentive term. If you specify a term of zero, the incentive will not appear in the cash flow table.

**PTC (Production Tax Credit)**
  The state and federal PTC each apply in Year One and later of the project cash flow, up to the number of years you specify:

*PTC in Year n = Amount ($/kWh) × (1 + Escalation)^(n-1) × Energy in Year n (kWh)*

  Where *Amount*, *Term*, and *Escalation*   are the values you specify on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page, and *Energy*   is the value displayed in the Energy row of the cash flow table (described above).

  SAM rounds the product *Amount ($/kWh) × (1 + Escalation)^(n-1)*   to the nearest multiple of 0.1 cent as described in Notice 2010-37 of `IRS Bulletin 2010-18 <https://www.irs.gov/irb/2010-18_IRB/ar11.html>`__  .

.. note:: If you specify year-by-year PTC rates on the Incentives page using an Annual Schedule instead of a single value, SAM ignores the PTC escalation rate.

**ITC (Investment Tax Credit)**
  The state and federal ITC each apply only in Year One of the project cash flow. For ITCs that you specify as a fixed amount:

*ITC in Year One = Amount*

  Where *Amount*   is the value you specify in the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page.

  For ITCs that you specify as a percentage of total installed costs:

*ITC in Year One = ( Total Installed Cost ($) - ITC Basis Reduction ($) ) × Percentage (%), up to Maximum*

  Where *Total Installed Cost*   is from the :doc:`Installation Costs <../installation-costs/index>`   page, and *Percentage*   and *Maximum*   are the values you specify on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page.

*ITC Basis Reduction*   applies only to the Commercial and PPA financial models, and depends on whether the project includes any investment-based incentives (IBI) or capacity-based incentives (CBI) specified on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page with checked boxes under **Reduces Depreciation and ITC Bases**. For each IBI or CBI with a check mark, SAM subtracts the incentive amount from the total installed cost to calculate the ITC.

*ITC Basis Reduction = IBI + CBI*

  Where *IBI*   and *CBI*   are the incentives that you have specified reduce the ITC basis on the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>`   page.