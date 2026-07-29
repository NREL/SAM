Internal Rate of Return (IRR)
=============================

The internal rate of return is the *nominal* discount rate that corresponds to a net present value (NPV) of zero for the PPA financial models.

.. image:: ../images/EQ_IRR.png
   :align: center
   :alt: EQ_IRR.png

Where *C*\ :sub:`n`\  is the after-tax cash flow.

You choose a solution mode on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page that determines whether SAM calculates the IRR to meet a desired PPA price that you specify, or calculates a PPA price to meet a desired target IRR that you specify:

* When the solution mode is **Specify IRR target**, you specify a desired IRR and the year you want the IRR to be achieved. SAM adjusts the PPA price until the IRR in that year is as close as possible to the IRR target.

* When the solution mode is **Specify PPA price**, SAM calculates the IRR that results from the PPA price you specify. You can specify the year to achieve the IRR to display the IRR for that year in the Metrics table.

For the single owner, merchant plant, and community solar financial models, the IRR and year it is achieved is from the owner's perspective. For the leveraged partnership flip, all equity partnership flip, and sale leaseback financial models that involve two project partners, SAM reports the IRR from the perspective of each partner, and the year in which the investor's IRR is achieved.


 
.. note:: SAM returns a value of "NaN" (Not a Number) when it cannot find an IRR that results in NPV = 0. This usually indicates that either the project revenue is too low (NPV < 0) to cover project costs, or that the revenue is excessively high given the project costs (very high NPV). See :ref:`Interpeting the NPV <interpreting>` for tips on how to adjust inputs when this happens.


.. note:: SAM also returns NaN when the Year 0 cost is zero, for example, for a project with 100% debt.


.. note:: If you try to replicate the IRR equation in Excel or other spreadsheet software by exporting the cash flow table and using the NPV formula on the appropriate after-tax cash flow row, you may find that the IRR value that SAM calculates does not appear to result in an NPV of zero. (It will always be close to zero compared to the magnitude of the cash flows.) This is because SAM reports the IRR to four significant digits, and for projects with large cash flows, a higher accuracy may be required to show the value that results in an NPV closer zero. For example, for a $74 million project with average annual after-tax cash flows of $3.9 million, SAM reports an IRR of 22.56% in the :doc:`Metrics table <../results/summary>`, and 22.5589 on the :doc:`Data Tables <../results/data>` tab. In Excel, the formula =NPV(0.2256,B60:AF60) results in an NPV of -$573.96 dollars, while the formula =(0.225589632,B60:AF60) results in an NPV value of -$0.02. For this example, it would be reasonable to consider the IRR to be 22% or 23%.

.. _year:

Year IRR is Achieved
--------------------

When you specify either a PPA price or target IRR on the Financial Parameters page, you also specify the year in which you want the IRR to be achieved. SAM calculates an IRR value for each year in the analysis period and reports it as the after-tax cumulative IRR the :doc:`cash flow <../results/cashflow>`. The year the IRR is achieved is as close as possible to the target year that you specify on the Financial Parameters page.

.. _endanalysisperiod:

IRR at End of Analysis Period
-----------------------------

The single owner financial model reports the IRR at the end of the analysis period in addition to the IRR in the target year.