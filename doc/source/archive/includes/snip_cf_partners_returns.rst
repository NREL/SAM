
The partners returns are the IRR, NPV, and taxes from each project partner's perspective.

Tax Investor
~~~~~~~~~~~~

The tax investor cash flows represent the tax investor share of project cash flows.

Tax Investor Pre-Tax Returns
----------------------------

**Total tax investor pre-tax returns**
  In Year Zero, the tax investor pre-tax cash flow is the portion of equity capital allocated to the tax investor:

*Tax Investor Pre-Tax Cashflow in Year Zero = Tax Investor Share of Equity Contribution (%) × Issuance of Equity ($)*

  Where *Tax Investor Share of Equity Contribution*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

  For years before the flip target is reached:

*Tax Investor Pre-Tax Cashflow in Year n>1 = Tax Investor Pre-flip Share of Project Cash (%) × Total Project Pre-tax Cash Flow ($)*

  For years after the flip target is reached:

*Tax Investor Pre-Tax Cashflow in Year n>1 = Tax Investor Post-flip Share of Project Cash (%) × Total Project Pre-tax Cash Flow ($)*

  Where the pre- and post-flip share of project cash percentages are from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**Tax investor cumulative IRR**
  Cumulative pre-tax IRR from the tax investor perspective for each year N of the cash flow. The cumulative IRR in a given Year N is the discount rate that results in a value of zero for the present value of the discounted pre-tax cash flows up to Year N:

  .. image:: /images/EQ_IRRPreTax.png
     :align: center
     :alt: EQ_IRRPreTax.png

  Where *C*\ :sub:`PreTax,n`\    is the tax investor pre-tax cash flow described above.

  For years when there is no mathematical solution, SAM reports a value of zero.

**Tax investor cumulative NPV**
  Cumulative pre-tax net present value from the tax investor perspective in a given Year N is the sum of the present values of the pre-tax cash flows up to Year N:

  .. image:: /images/EQ_NPVPreTax.png
     :align: center
     :alt: EQ_NPVPreTax.png

  Where *C*\ :sub:`PreTax,n`\    is the tax investor pre-tax cash flow described above, *d*\ :sub:`nominal`\    is the nominal discount rate from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

Tax Investor After-Tax Returns
------------------------------

**Tax investor cash (total pre-tax returns)**
  The tax investor pre-tax cash flow, described above.

**ITC**
  The tax investor's share of the ITC amount applies to the tax investor's Year One cash flow:

*Investment Tax Credit in Year One = Tax Investor Share of Equity Contribution (%) × ( Federal ITC ($) + State ITC ($) )*

  Where *Tax Investor Share of Equity Contribution*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, and *Federal ITC*   and *State ITC*   are described below.

**PTC**
  The tax investor's share of the PTC applies in Years One and later of the tax investor's cash flow.

  For years before the flip target is reached:

*Production Tax Credit in Year n = Tax Investor Pre-flip Share of Project Cash (%) × ( Federal PTC ($) + State PTC ($) )*

  For years after the flip target is reached:

*Production Tax Credit in Year n = Tax Investor Post-flip Share of Project Cash (%) × ( Federal PTC ($) + State PTC ($) )*

  Where the pre- and post-flip share of project cash values are from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, and *Federal PTC*   and *State PTC*   are described below.

**Tax investor share of project tax**
  The tax investor's share of the project's tax payment or refund. A positive value indicates a tax refund, and a negative value indicates a tax payment.

  For years before the flip target is reached:

*Tax Investor Share of Project Tax in Year n = Tax Investor Pre-flip Share of Project Cash (%) × Project Tax Benefit/(Liability) ($)*

  For years after the flip target is reached:

*Tax Investor Share of Project Tax in Year n = Tax Investor Post-flip Share of Project Cash (%) × Project Tax Benefit/(Liability) ($)*

  Where the pre- and post-flip share of project cash values are from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, and Project Tax Benefit/(Liability) is the tax amount owed.

**Total tax investor after-tax returns**
  The tax investor's total after-tax cash flow.

*Total = Cash + Investment Tax Credit + Production Tax Credit + Share of Project Tax Benefit/(Liability)*

**Tax investor cumulative IRR**
  Cumulative after-tax IRR from the tax investor perspective for each year N of the cash flow. The cumulative IRR in a given Year N is the discount rate that results in a value of zero for the present value of the discounted pre-tax cash flows up to Year N:

  .. image:: /images/EQ_IRR.png
     :align: center
     :alt: EQ_IRR.png

  Where *C*\ :sub:`PreTax,n`\    is the tax investor after-tax cash flow described above.

  For years when there is no mathematical solution, SAM reports a value of zero.

**Tax investor cumulative NPV**
  Cumulative after-tax net present value from the tax investor perspective in a given Year N is the sum of the present values of the pre-tax cash flows up to Year N:

  .. image:: /images/EQ_NPVAfterTax.png
     :align: center
     :alt: EQ_NPVAfterTax.png

  Where *C*\ :sub:`PreTax,n`\    is the tax investor after-tax cash flow described above, *d*\ :sub:`nominal`\    is the nominal discount rate from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**Tax investor maximum IRR**
  The greater of the Year One Cumulative IRR and the Year n Cumulative IRR.

Developer
~~~~~~~~~

The Developer cash flows represent the developer share of project cash flows.

Developer Pre-Tax Returns
-------------------------

**Pre-tax development fee**
  A fee received by the developer.

*Pre-tax Developer Development Fee in Year Zero = Development Fee*

  Where *Development Fee*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**Total developer pre-tax returns**
  The pre-tax cash flow from the developer's perspective.

*Developer Pre-tax Cash Flow in Year Zero = Equity Investment in Year Zero + Pre-tax Developer Development Fee*

*Developer Pre-tax Cash Flow in Year n>0 = Total Project After-tax Cash Flow - Tax Investor Pre-tax Cash Flow*

**Developer pre-tax cumulative IRR**
  Cumulative pre-tax IRR from the developer perspective for each year N of the cash flow. The cumulative IRR in a given Year N is the discount rate that results in a value of zero for the present value of the discounted pre-tax cash flows up to Year N:

  .. image:: /images/EQ_IRRPreTax.png
     :align: center
     :alt: EQ_IRRPreTax.png

  Where *C*\ :sub:`PreTax,n`\    is the tax investor after-tax cash flow described above.

  For years when there is no mathematical solution, SAM reports a value of zero.

**Developer pre-tax cumulative NPV**
  Cumulative pre-tax net present value from the developer perspective in a given Year N is the sum of the present values of the pre-tax cash flows up to Year N:

  .. image:: /images/EQ_NPVPreTax.png
     :align: center
     :alt: EQ_NPVPreTax.png

  Where *C*\ :sub:`PreTax,n`\    is the tax investor after-tax cash flow described above, *d*\ :sub:`nominal`\    is the nominal discount rate from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**Developer pre-tax IRR**
  The cumulative tax investor pre-tax IRR in the flip year defined on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

  For years when there is no mathematical solution, SAM reports a value of zero.

**Developer pre-tax NPV**
  The cumulative tax investor pre-tax NPV in the flip year defined on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

Developer After-Tax Returns
---------------------------

**Equity Investment**
  The developer's share of the project equity capital.

*Equity Investment in Year Zero = Total Project Equity Investment in Year Zero - Tax Investor Equity Investment in Year Zero*

**Development Fee**
  A fee received by the developer.

*Development Fee in Year Zero = Development Fee*

  Where *Development Fee*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**Cash**
  The pre-tax cash flow from the developer perspective.

*Cash in Year Zero = Equity Investment + Development Fee*

*Cash in Year n>0 = Developer Pre-tax Cash Flow*

**ITC**
  The developer's share of the ITC amount applies to the tax developer's Year One cash flow:

*Investment Tax Credit in Year One = Developer Share of Equity Contribution (%) × ( Federal ITC ($) + State ITC ($) )*

  Where *Developer Share of Equity Contribution*   is from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, and *Federal ITC*   and *State ITC*   are described below.

**PTC**
  The developer's share of the PTC applies in Years One and later of the developer's cash flow.

  For years before the flip target is reached:

*Production Tax Credit in Year n = Developer Pre-flip Share of Project Cash (%) × ( Federal PTC ($) + State PTC ($) )*

  For years after the flip target is reached:

*Production Tax Credit in Year n = Developer Post-flip Share of Project Cash (%) × ( Federal PTC ($) + State PTC ($) )*

  Where the pre- and post-flip share of project cash values are from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page, and *Federal PTC*   and *State PTC*   are described below.

**Developer share of project tax**
  The developer's share of the project's tax payment or refund. A positive value indicates a tax refund, and a negative value indicates a tax payment.

  For years before the flip target is reached:

*Developer Share of Project Tax in Year n = Developer Pre-flip Share of Project Cash (%) × Project Tax Benefit/(Liability) ($)*

  For years after the flip target is reached:

*Developer Share of Project Tax in Year n = Developer Post-flip Share of Project Cash (%) × Project Tax Benefit/(Liability) ($)*

  Where the pre- and post-flip share of project cash values are from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page

**Total developer after-tax returns**
  The developer's total after-tax cash flow.

*Total Developer After-tax Returns = Cash + Investment Tax Credit + Production Tax Credit + Share of Project Tax Benefit/(Liability)*

**Developer after-tax cumulative IRR**
  Cumulative after-tax IRR from the developer perspective for each year n of the cash flow. The cumulative IRR in a given Year n is the discount rate that results in a value of zero for the present value of the discounted pre-tax cash flows up to Year N:

  .. image:: /images/EQ_IRR.png
     :align: center
     :alt: EQ_IRR.png

  Where *C*\ :sub:`PreTax,n`\    is the developer after-tax cash flow described above.

  For years when there is no mathematical solution, SAM reports a value of zero.

**Developer after-tax cumulative NPV**
  Cumulative after-tax net present value from the developer perspective in a given Year n is the sum of the present values of the pre-tax cash flows up to Year N:

  .. image:: /images/EQ_NPVAfterTax.png
     :align: center
     :alt: EQ_NPVAfterTax.png

  Where *C*\ :sub:`PreTax,n`\    is the developer after-tax cash flow described above, *d*\ :sub:`nominal`\    is the nominal discount rate from the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

**Developer after-tax IRR**
  The cumulative developer pre-tax IRR in the flip year defined on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.

  For years when there is no mathematical solution, SAM reports a value of zero.

**Developer after-tax NPV**
  The cumulative developer pre-tax NPV in the flip year defined on the :doc:`Financial Parameters <../financial-parameters/fin_overview>`   page.