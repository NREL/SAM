PPA Price
=========

The PPA price is the power purchase price for the PPA financial models. SAM assumes that these projects sell electricity at a price negotiated through a power purchase agreement (PPA).


 
.. note:: When you specify a :doc:`PPA price escalation <mtf_ppa_escalation>` rate on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page, the PPA price increases from year to year. The PPA price does not increase with inflation.


.. note:: If your project results in an unrealistically high PPA price or IRR, you may need to adjust the inputs. See :ref:`Interpreting the NPV <interpreting>` for tips.


.. note:: You can see the annual PPA price on the :doc:`Data Tables <../results/data>` tab of the results page by displaying the PPA price variable under **Annual Data**.

The PPA price in the Metrics table is either the value that you specify as an input on the Financial Parameters page, or a value that SAM calculates based on the target internal rate of return (IRR) that you specify. 

* When the solution mode on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page is **Specify PPA Price**, the Year one PPA price in the Metrics table is the value that you specify.

* When the solution mode is **Specify IRR Target**, SAM determines the PPA price required for project revenues to meet the internal rate of return target that you specify on the Financial Parameters page.

The Year one PPA price that SAM reports in the Metrics table is equivalent to the bid price in a power purchase agreement.

The PPA price is just one measure that is useful for determining whether a project is financially feasible. For example, a project that meets your target IRR with a reasonable PPA price may not be feasible if the net present value (NPV) is negative, or if the size of debt is not within the range that the project can realistically expect to receive a loan.

.. _mtf-ppa-price-tod:

PPA Price and TOD Factors
~~~~~~~~~~~~~~~~~~~~~~~~~

:ref:`Time-of-delivery (TOD) factors <revenue-ppa-tod>` allow the power price to vary with time within the structure of a power purchase agreement with a single bid price. The agreement consists of a PPA price and a set of PPA price multipliers and the periods to which they apply.

For analyses involving PPA price time-of-delivery (TOD) factors, for each hour of the year, SAM multiplies the PPA bid price by the TOD factor to calculate the power price in that hour.