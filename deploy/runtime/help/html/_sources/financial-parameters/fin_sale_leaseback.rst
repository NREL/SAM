Sale Leaseback
==============

This topic describes the inputs on the Financial Parameters page for the Sale Leaseback financial model. For a general description of financial structures SAM can model, see :doc:`Financial Models <../introduction/fin_overview>`.

SAM displays results of the financial model in the :doc:`cash flow <../results/cashflow>`.

.. _slb-solutionmode:

Solution Mode
~~~~~~~~~~~~~

.. include:: ../includes/snip_revenue_solution_mode_adv.rst

Sale Leaseback
~~~~~~~~~~~~~~

The Sale Leaseback input variables determine the developer's operating margin and size of the lease payment reserve account. SAM includes the developer's margin as a project expense in the :doc:`cash flow <../results/cashflow>`.

**Lessee Operating Margin**
  The developer's margin in dollars per kilowatt of system nameplate capacity.

**Lessee Margin Escalation**
  An annual escalation rate that applies to the developer's margin. The inflation rate does not apply to the developer's margin.

**Lessor Required Lease Payment Reserve**
  The size of the lease payment reserve account in months. (In some cases, the tax investor may require that the developer fund a reserve account.) 

*Lease Reserve Amount ($) = Lessor Required Lease Payment Reserve (months) / 12 (months/yr) × Year 1 Pre-tax Operating Cash Flow ($/yr)*

  The lease reserve amount is one of the fees included in the :doc:`net capital cost <../financial-metrics/mtf_costs>`   value reported on the Metrics table, which is included in the issuance of equity value in the :doc:`cash flow <../results/cashflow>`  .

**Amount**
  This amount of the developer's margin.

*Amount ($) = Lessee Operating Margin ($/kW) × System Nameplate Capacity (kW)*

Analysis Parameters
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_financing_general.rst

Tax and Insurance Rates
~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_financing_taxes_insurance.rst

Property Tax
------------

.. include:: ../includes/snip_financing_property_tax.rst

Salvage Value
~~~~~~~~~~~~~

.. include:: ../includes/snip_financing_salvage_value.rst

Construction Financing
~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_financing_construction_period.rst

Cost of Acquiring Financing
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_financing_cost_of_financing.rst

Reserve Accounts
~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_financing_reserves.rst

