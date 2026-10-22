Net Capital Cost
================

The net capital cost is total installed cost from the :doc:`Installation costs <../installation-costs/index>` page less any cash investment-based and capacity-based incentives from the :doc:`Incentives <../incentives-and-depreciation/cash_incentives>` page and plus any additional financing costs and fees from the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page. The exact equation depends on the financial model as described below.

Residential and Commercial
~~~~~~~~~~~~~~~~~~~~~~~~~~

For the residential and commercial models, the net capital cost is the total installed cost from the Installation costs page:

*Net capital cost = total installed cost*
 *- total IBI*
 *- total CBI*

PPA Models
~~~~~~~~~~

For the PPA financial models, the net capital cost is the sum of the total installed cost, financing costs and development fees, and debt service and working capital reserve account funding less investment-based incentives:

*Net capital cost = total installed cost*
 *+ financing costs and fees*
 *+ debt service reserve funding*
 *+ working capital reserve funding*
 *- total IBI*
 *- total CBI*

costs and Fees
--------------

costs and fees are costs that you specify on the Financial Parameters page. SAM adds these costs to the total installed cost on the Installation costs page to calculate the net capital cost for the PPA financial models:

**Single Owner**
  For the single owner model, costs and fees include debt costs, construction financing cost, and debt and equipment replacement reserves.

*Financing costs and fees = debt closing cost*
 *+ debt up-front fee percentage × debt*
 *+ financing cost*
 *+ debt service reserve funding*
 *+ construction financing cost*
 *+ major equipment replacement reserve*

  You can see these values in the :doc:`cash flow <../results/cashflow>`   under "Cash Flows from Investing Activities."

**Leveraged Partnership Flip**
  For the leveraged (with debt) partnership flip model, costs and fees include debt costs, construction financing cost, debt and equipment replacement reserves, and development and equity costs.

*Financing costs and fees = debt closing cost*
 *+ debt up-front fee percentage × debt*
 *+ other financing cost*
 *+ debt service reserve*
 *+ construction financing cost*
 *+ major equipment replacement reserve*
 *+ development fee percentage × total installed cost*
 *+ equity closing cost*

**All Equity Partnership Flip**
  For the all equity (without debt) leveraged partnership flip model, costs and fees include construction financing cost, debt and equipment replacement reserves, and development and equity costs.

*Financing costs and fees = other financing cost*
 *+ construction financing cost*
 *+ major equipment replacement reserve*
 *+ development fee percentage × total installed cost*
 *+ equity closing cost*

**Sale Leaseback**
  For the all sale leaseback model, costs and fees include construction financing cost, debt and equipment replacement reserves, development and equity costs, and the lease payment reserve.

*Financing costs and fees = other financing cost*
 *+ construction financing cost*
 *+ major equipment replacement reserve*
 *+ development fee percentage × total installed cost*
 *+ equity closing cost*
 *+ lease payment reserve*