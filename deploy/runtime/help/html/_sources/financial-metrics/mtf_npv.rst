Net Present Value (NPV)
=======================

A project's net present value (NPV) is a measure of a project's economic feasibility that includes both revenue (or savings for residential and commercial projects) and cost. In general, given the discount rate you assume, a positive NPV indicates an economically feasible project, while a negative NPV indicates an economically infeasible project. You should evaluate the NPV along with other metrics including capacity factor, internal rate of return, PPA price, payback period, size of debt, etc.

For more information about using the NPV and other economic metrics to evaluate renewable energy projects, see pages 39-42 of Short W et al, 1995. *Manual for the Economic Evaluation of Energy Efficiency and Renewable Energy Technologies. National Renewable Energy Laboratory*. NREL/TP-462-5173. (`PDF 6.6 MB) <https://www.osti.gov/biblio/35391>`__

The NPV is the present value of the after tax cash flow discounted to year one using the nominal discount rate:

.. image:: ../images/EQ_NPV.png
   :align: center
   :alt: EQ_NPV.png

Where *C*\ :sub:`n`\  is the after-tax cash flow in Year *n* for the residential and commercial models, and the after-tax project returns for the PPA models, *N* is the analysis period in years that you specify on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page, and *d*\ :sub:`nominal`\  is the nominal discount rate from the Financial Parameters page.

.. _interpreting:

Interpreting the NPV
--------------------

The NPV is useful indicator of the financial feasibility of a project. In general, a positive NPV indicates a profitable project, an NPV of zero indicates a project that breaks even, and a negative NPV indicates a project that costs more than it earsn in revenue.

SAM generates simulation warnings under the following conditions:

* NPV is less than zero. This indicates that the project revenues may be too low to cover project costs.

* Internal rate of return is "NaN" (Not a Number). This indicates that project revenues may either be too low to cover project costs, or unrealistically high.

* Internal rate of return is greater than 50%. This may indicate that project revenues are unrealistically high.

* Debt fraction is greater than 100%. This may indicate that project revenues are much higher than needed to cover project costs.

.. note:: These simulation warnings are designed to inform you when there may be a problem with your financial model inputs. They do not prevent the simulation from finishing. You can ignore the warnings if they do not apply to your analysis. The :doc:`Notices <../results/notices>` tab on the Results page stores the warning messages so you can see them after running a simulation, 

When evaluating the financial feasibility of a project, it is a good idea to check the NPV, :doc:`internal rate of return (IRR) <mtf_irr>` and/or :doc:`PPA price <mtf_ppa_price>`, and :doc:`size of debt <mtf_debt_and_equity>` metrics together to make sure they are all reasonable. For example, a positive NPV with an unrealistically high IRR may indicate that the project revenues are unrealistically high compared to the project costs. Similarly, a project that requires a high PPA price to achieve a positive NPV may not be competitive in a bidding process.

Strategies for increasing the NPV include:

* Reduce installation and/or operating costs.

* Increase incentives.

* Increase the discount rate.

* For PPA projects (PPA, merchant plant, third party ownership, community solar): Increase revenue (increase PPA price, PPA price escalation, or decrease IRR target).

* For distributed projects (residential, commercial): Increase savings (adjust retail electricity rate or load).

Strategies for decreasing the NPV are similar as above, except you should make changes to increase costs and decrease revenue or savings.

.. _communitysolar:

Community Solar Subscriber Net Present Value
--------------------------------------------

For the Community Solar financial model, *C*\ :sub:`n`\  for each subscriber class is the difference between the each subscriber class's bill credit amount and cost of participation, where the cost of participation is the total value of payments the subscriber class makes to the project.