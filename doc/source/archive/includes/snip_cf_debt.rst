
The debt amounts depend on the following inputs from the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page:

* Debt Rate

* DSCR

Debt Repayment
--------------

The debt repayment rows show the size of annual debt payments.

Debt Sizing (Constant Payments)
-------------------------------

When you specify the debt percent as an input on the Financial Parameters page, SAM assumes constant total annual debt payments so that the annual debt service coverage ratio (DSCR) varies from year to year. In this mode, SAM reports the minimum DSCR in the Metrics table.

Debt Sizing (Constant DSCR)
---------------------------

When you specify the DSCR as an input on the Financial Parameters page, SAM assumes a constant annual DSCR and sculpted debt, where annual debt payments vary to match cash available for debt services. In this mode, SAM reports the debt percent in the Metrics table.