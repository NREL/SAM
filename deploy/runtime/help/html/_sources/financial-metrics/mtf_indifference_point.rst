Indifference Point
==================

The :doc:`Third Party - Host / Developer <../financial-parameters/fin_tpo_host_developer>` financial model calculates the host indifference point, which quantifies host's annual electricity bill from the utility without the renewable energy system as a rate per unit of electricity ($/kWh), accounting for the host's electricity rate structure and consumption. 

The indifference point is the maximum power purchase agreement (PPA) price that makes the PPA more cost effective to the host than purchasing power from the utility: The host is unlikely to accept a PPA if the developer offers a PPA price that is greater than the indifference point. Like the :doc:`PPA price <../financial-metrics/mtf_ppa_price>`, SAM reports the Year 1, nominal levelized, and real levelized indifference point. Be sure to compare the same form of the indifference point and PPA price.

The host indifference point in each year is the :doc:`electricity savings <../financial-metrics/mtf_revenues>` divided by the total renewable energy production:

*Host Indifference Point in Year n = Electricity Savings in Year n / Annual Energy in Year n*

The levelized host indifference point is the present value of the annual values.

.. note:: The variable name for the indifference point is "NTE," which stands for "not to exceed" because the indifference point can also be thought of as the value the PPA price is not to exceed.