PPA Revenue
===========

The Revenue page for the PPA models provides inputs to define sources of revenue for frpnt-of-meter (FOM) projects. Possible sources of revenue include:

* PPA revenue is from sales of electricity to the grid through a power purchase agreement (PPA). This is the primary source of revenue for most projects. You can specify a power price, or you can specify a target internal rate of return (IRR) and have SAM calculate the power price required to meet the target.

* Capacity payments are optional payments to the project for available capacity.

* Curtailment payments are optional payments to the project for reducing or stopping power generation during ceratain times.

* Non-energy revenue is revenue earned from non-energy activities such as agricultural production.

You can also specify optional time-of-delivery (TOD) factors that modify the power price for projects with prices that vary on a daily, seasonal, or time series basis. 

If you are modeling a merchant plant project with power prices at market rates, you can :doc:`choose <../getting-started/choose_models>` the Merchant Plant :doc:`financial model <../introduction/fin_overview>` instead of the PPA Single Owner model.

.. note:: For the PPA Partnership Flip and Sale Leaseback financial models, the PPA revenue inputs are on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page.

To see the project revenue after a simulation, go to the :doc:`Cash Flow <../results/cashflow>` tab on the results page. You can also see the revenue data on the :doc:`Data Tables <../results/data>` tab by filtering the table for variables with "revenue" in the variable name.

.. _solutionmode:

Solution Mode
~~~~~~~~~~~~~

.. include:: ../includes/snip_revenue_solution_mode_adv.rst

.. _revenue-ppa-tod:

Time of Delivery
~~~~~~~~~~~~~~~~
.. include:: ../includes/snip_revenue_tod_factors.rst

Capacity Payments
~~~~~~~~~~~~~~~~~
.. include:: ../includes/snip_revenue_capacity_payments.rst

Curtailment Payments
~~~~~~~~~~~~~~~~~~~~
.. include:: ../includes/snip_revenue_curtailment.rst

Non-energy Revenue and Expenses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_revenue_non_energy.rst