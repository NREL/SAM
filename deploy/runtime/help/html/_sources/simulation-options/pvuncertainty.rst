Uncertainty
===========

The Uncertainty simulation option is available for the Detailed PV and PVWatts performance models. For the Wind Power model, the :doc:`Uncertainties <../wind-power/wind_uncertainties>` input page provides a way to model uncertainties in wind resource or predicted power output of the model. For other models, the :doc:`P50 / P90 <p50p90>` simulation option calculates P-values given a set of weather files.

For descriptions of this implementation of uncertainty modeling, see:

* Prilliman, M.; Hansen, C.; Keith, J.; Janzou, S.; Theristis, M.; Scheiner, A.; Ozakyol, E. (2023) Quantifying Uncertainty in PV Energy Estimates Final Report. National Renewable Energy Laboratory. NREL/TP-7A40-84993 (`PDF 541 KB <https://docs.nlr.gov/docs/fy23osti/84993.pdf>`__)

* C.; Moser, D.; Belluardo, G.; Ingenhoven, P. (2018) `Uncertainties in PV System Yield Predictions and Assessments <https://iea-pvps.org/wp-content/uploads/2020/01/Uncertainties_in_PV_System_Yield_Predictions_and_Assessments_by_Task_13.pdf>`__, PVPS Report IEA-PVPS T13-12:2018, International Energy Agency (IEA) Photovoltaic Power Systems Programme (PVPS).

The uncertainty simulation model helps you understand the uncertainty in SAM's Year 1 annual energy estimate. It calculates P-values and plots graphs based on uncertainty distributions and a set of specific-year weather files you provide.

 


.. note:: Uncertainty simulations require a folder with at least ten valid weather files that each contain data for one year. For example, the folder might contain 23 weather files downloaded from the NLR National Solar Radiation Database (NSRDB) for 1998 through 2020.


.. note:: In this description, "annual energy" means the total electrical output of the system in Year 1.

To model uncertainty:

#. For each source of uncertainty, click **Edit** to define a distribution. Use the check box to enable or disable the source: Disabling a source of uncertainty removes its effect from the resulting graphs.

Click the information icon ("i") next to the uncertainty source to see a description. If you are unsure of the distribution, you can use the default distribution, which is based on Table 15 in the IEA report cited above.

#. Click **Choose folder** to choose a weather file folder that contains at least ten valid weather files. If you do not have such a folder, first click **Download from NSRDB** to automatically download a set of weather files from the NSRDB, and then choose the folder that contains the downloaded files.

#. Click **Run uncertainty simulations**.

#. When the simulation finishes, SAM displays three graphs, each with a blue line on the graph and number under the graph indicating a P-value. Type a value for **Custom Px** to set the P-value shown in the graphs. For example, type 50 to show the P50 value in the graphs, or type 90 to show the P90 value.

Uncertainty Graphs and P-value
------------------------------

A P-value is a value that is expected to be met or exceeded a given percentage of time. For example, a P90 value of 35 GWh for annual energy calculated from 20 years of weather data would indicate that the system generates 35 GWh or more for 18 of the 20 years.

The Overall Uncertainty and Uncertainty Sources graphs each show a probability distribution function (PDF) histogram and a cumulative distribution function (CDF) line:

* Overall uncertainty accounts for both inter-annual variability from the set of weather files, and sources of uncertainty defined by the input distributions (irradiance transposition, horizon shading, row shading, etc.).

* Uncertainty sources accounts only for the uncertainty defined by the input distributions.

The Interannual Variability graph shows the annual energy for each weather file. It only accounts for the variation in solar resource over time represented by the data in the weather files.

Note that the P-value for each graph is different because it accounts for different sources of uncertainty (input distributions, inter-annual variability, or both).