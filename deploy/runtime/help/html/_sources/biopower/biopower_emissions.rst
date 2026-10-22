Emissions
=========

The Life-Cycle Impacts page allows you to specify inputs for a set of life-cycle greenhouse gas emissions calculations for the biomass supply chain. You can use these inputs to estimate the independent contributions of biomass collection, transport, pre-processing, combustion, and CO2 re-uptake on the life-cycle carbon dioxide emissions of the project.

For a technical description of the biopower model, see Jorgenson, J.; Gilman, P.; Dobos, A. (2011). Technical Manual for the SAM Biomass Power Generation Model. 40 pp.; NREL Report No. TP-6A20-52688. https://docs.nlr.gov/docs/fy11osti/52688.pdf

.. note:: The life-cycle impacts calculations are independent of the power plant energy modeling calculations. The inputs on the life-cycle impacts page are for a separate set of calculations than the inputs on the :doc:`Feedstock <biopower_feedstock>` and :doc:`Plant Specs <biopower_plant_specs>` pages.

After running the simulation, SAM displays a graph on the :doc:`Results <../getting-started/results_page>` page similar to the following one showing the percent difference in CO2 equivalent emissions from the different sources.

.. image:: ../images/SS_Results-Graphs-BiopowerEmissions.png
   :align: center
   :alt: SS_Results-Graphs-BiopowerEmissions.png

Inside farmgate
~~~~~~~~~~~~~~~

**Diesel-powered biomass collection vehicle / Biodiesel-powered biomass collection vehicle**
  Specifies the type of fuel used to power the field biomass collection vehicle. Please note that this input only affects the life-cycle analysis portion of the model. 

**Assume biomass was not grown dedicated to power (waste)**
  Specifies the intended use of the biomass. When waste biomass goes unused (e.g., in a landfill), it undergoes decomposition which can result in methane emissions. If a biopower plant utilizes biomass that would otherwise decompose, the plant receives a greenhouse gas "credit" for avoiding more harmful methane emissions.

  Clearing this box signifies that the biomass was grown explicitly for power generation, meaning that no decomposition emissions were avoided.

From farmgate to biopower facility
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Diesel-powered vehicle for truck transport / Biodiesel-powered vehicle for truck transport**
  This input specifies the type of fuel used to power the vehicle used to haul the biomass from the farmgate to the biopower facility. Please note that this input only affects the life-cycle analysis portion of the model. For instance, if using biodiesel increases the delivery cost of biomass, you must reflect the increased cost on the :doc:`Feedstock costs <biopower_feedstock_costs>`   page.

**One-stage truck transport (no separate pre-processing facility) / Two-stage truck transport (separate pre-processing facility)**
  Occasionally, regional biomass will be pre-processed at a separate facility before being used in a biopower plant. This input

  considers the vehicle-miles traveled for biomass to arrive at the biopower plant. For instance, the vehicle-miles traveled may increase if the biomass must travel to a pre-processing facility that is not on the way to the plant. Again, please note that this input only affects the life-cycle analysis portion of the model.

**Enable long-distance transport after xx miles: Freight rail transport for long distances / Freight barge transport for long distances**
  Currently, most biomass is transported via truck for short distances. However, after a certain amount of miles, rail or barge transport may become more economical. This option allows you to see the emissions benefits of using a more efficient transport option for a specified "longer" distance. Again, please note that this input only affects the life-cycle analysis portion of the model. For instance, if using railroad transport decreases the delivery cost of biomass, you must reflect the changed cost on the :doc:`Feedstock costs <biopower_feedstock_costs>`   page.

Preprocessing Options
~~~~~~~~~~~~~~~~~~~~~

**Pre-processing includes grinding or chipping / Pre-processing includes pelletization**
  Most biomass typically undergoes some pre-processing before being fed into a biopower plant. The pre-processing may include light grinding or chipping, heavy grinding, and/or pelletization. In most cases each pre-processing step would be subsequent to the previous step. For example, biomass pelletization only occurs after heavy grinding. 

  Pre-processing increases the resources required to prepare the biomass and thus increase the emissions impact of biomass power

Electricity grid carbon intensity, g CO2 eq/kWh
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  This input allows you to specify the carbon intensity of the electricity being used for pre-processing, fertilizer production, and biomass storage. 

  For example, if a carbon-efficient technology is used to generate the power required for pre-processing, the overall greenhouse burden of the biopower facility would be less than a less efficient technology.

  You can choose from several US regional values with sample values for 100% coal and 100% renewable energy. The default value is the 2010 United States average. 