PPA Price Escalation
====================

The PPA price escalation rate causes the PPA price to increase annually over the analysis period. It is an annual escalation rate that SAM uses to calculate the :doc:`PPA price <mtf_ppa_price>` in Years Two and later of the cash flow:

*PPA Price in Year n>1 = PPA Price in Year n=1 × ( 1 + PPA Price Escalation ) ^ n*

The PPA escalation rate is an input that you specify on the :doc:`Financial Parameters <../financial-parameters/fin_overview>` page.

SAM does not apply the inflation rate to the PPA price.

.. note:: You can see the annual PPA price on the :doc:`Data Tables <../results/data>` tab of the results page by displaying the PPA price variable under **Annual Data**.