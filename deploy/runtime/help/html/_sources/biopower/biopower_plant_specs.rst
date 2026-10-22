Plant Specs
===========

The Plant Specs page inputs define the major unit operations that make up a biomass power plant: biomass processing, combustion system and boiler, and steam turbine.

For a technical description of the biopower model, see Jorgenson, J.; Gilman, P.; Dobos, A. (2011). Technical Manual for the SAM Biomass Power Generation Model. 40 pp.; NREL Report No. TP-6A20-52688. https://docs.nlr.gov/docs/fy11osti/52688.pdf

Biomass Feedstock Handling
~~~~~~~~~~~~~~~~~~~~~~~~~~

The biopower model has three options for specifying biomass moisture content.

**Fed as received**
  The biomass feedstock does not undergo any substantial drying before being fed to the combustor. This option avoids drying costs but penalizes the boiler efficiency since evaporation of biomass moisture requires energy input.

**Allow feedstock to air-dry to atmospheric Equilibrium Moisture Content (EMC)**
  The biomass is exposed to the ambient atmospheric conditions for a sufficient amount of time to reach EMC. However, moisture composition doesn’t change instantly, and thus the equilibrium moisture levels are calculated on a monthly basis.

**Dry to specified moisture content**
  The feedstock handling system includes a dryer as an additional capital expenditure that you specify on the :doc:`Installation costs <../installation-costs/cc_biopower>`   page. Adding a dryer also increases the parasitic load of the plant and may add an incremental operation and maintenance cost. Although adding a dryer can increase the boiler efficiency by several percent, dryers are not widely used in practice because of the additional costs and parasitic loads.

Combustion System
~~~~~~~~~~~~~~~~~

SAM can model three common combustion systems encountered in biomass power plants. For a more detailed description of the combustion systems with suggestions for choosing input values, see Section 3.1.2 of the SAM biomass power technical manual referenced at the top of this page.

**Grate Stoker Furnace**
  A grate stoker furnace is designed to feed solid fuel onto a grate where burning occurs, with combustion air passing through the grate. Stokers are generally the least expensive of the three boiler types and are best suited for large fuel feed rates, typically between 75,000 lb/hr and 700,000 lb/hr.

**Fluidized Bed Combustor (FBC)**
  Fluidized bed combustion features a bed of fuel and sand or other inert substance that becomes suspended by the combustion air flowing upward. This technology reduces the fluctuations in steam production associated with changeable feedstocks, and features a lower combustion temperature and reduced formation of pollutants. However, capital costs and operating costs are typically higher for the FBC than the other combustion systems.

**Cyclone Furnace**
  Cyclone furnaces allow for flexibility in fuel types and increase combustion efficiency over stoker boilers by feeding the fuel in a spiral manner. Additionally, cyclone furnaces are smaller and have a lower capital cost than FBCs.

.. note:: SAM does not automatically change the cost assumptions on the Installation costs page when you change the combustion system option. Be sure to use costs appropriate for the type of plant you specify.

Boiler Parameters
-----------------

You can specify the main parameters that determine boiler and furnace efficiency. 

.. note:: The values of the parameters depend on the type of combustor. The default values are for a boiler for a steam grade of 900 F, 900 psig. If you choose a different steam grade, be sure to change the value of the other parameters accordingly.

**Steam Grade**
  The severity of the steam grade is often determined by the type of boiler. For example, lower combustion temperatures in fluidized bed combustors often result in lower steam grades. The steam grade directly determines the enthalpy of the steam produced in the boiler. 

**Percent excess fed air, %**
  By convention, the percent excess air is specified on a volumetric/molar basis. Combustion air from the atmosphere is only 21% oxygen by volume (and the balance nitrogen). Therefore, most of the enthalpy losses result from heating the nitrogen that accompanies the combustion oxygen. Increasing the excess fed air percentage decreases the boiler efficiency because more energy is required to heat the combustion air. If the excess air fed air percentage is too low, the fuel will not combust fully and the boiler can emit carbon monoxide and smoke.

**Number of boilers**
  Using fewer boilers reduces installation costs. Using more boilers may also reduce costs by offering the benefit of economies of scale. Typically, small utility scale-sized biopower plants use between one and three boilers.

**Flue gas temperature, °F**
  The flue gas is the mixture of gases exiting the plant through the stack. All useable heat has been collected when the combustion gases reach the specified flue gas temperature. Flue gas heat is often used to preheat other process streams, such as the boiler feedwater. The most efficient boilers utilize as much of the flue gas heat as possible before it exits the plant.

**Estimated steam produced, lb/hr steam**
  This metric is calculated based on the estimated efficiency of the boiler and the enthalpy of the steam produced. The steam produced in the boiler directly powers the steam turbine.

**Boiler overdesign factor, %**
  Boilers are generally oversized to prevent operating above capacity and for the ability to accommodate more biomass throughput. A higher value increases the boiler capital cost. Too low of a value results in lower overall efficiency.

**Design capacity of each boiler, lb/hr steam**
  Boilers are generally oversized to accommodate fluctuations in steam production and to allow for additional capacity. However, highly oversized boilers can result in increased efficiency loss and capital cost. The boiler overdesign factor input will directly adjust the design capacity of each boiler metric.

Estimated Efficiency Losses (HHV)
---------------------------------

**Dry flue gas losses, %**
  Combustion air enters the furnace at ambient temperature, where it is immediately subject to preheating by waste process heat. Regardless of how the air is preheated, a significant loss of enthalpy occurs when the combustion gas exits the plant at a much higher temperature than the temperature at which it was fed. The Dry Flue Gas Loss is largely determined by the input percent excess fed air. Combustion air from the atmosphere is only 21% oxygen by volume (and the balance nitrogen). Thus, much of the enthalpy losses result from heating up the nitrogen that accompanies the combustion oxygen.

**Moisture in flue, %**
  Moisture in fuel adversely affects plant efficiency in two primary ways. First, water in biomass imposes extra mass that must be consequently hauled and processed with the biomass itself. Additionally, the water absorbs heat from the combustion reaction that is unlikely to be recovered. Some power plants employ pre-combustion biomass drying to reduce moisture content and efficiency loss in this category. SAM allows the user to add a dryer under the dry to specified moisture content input on the Plant Specs page.

**Latent heat, %**
  Loss of latent heat results when elemental hydrogen in biomass combusts to form water. The water produced will leave the stack at the flue gas temperature as water vapor, thus requiring the latent heat of vaporization of water as well as the sensible heat of the vapor at the flue gas temperature.

**Unburned fuel, %**
  Unburned fuel losses simply result from incomplete combustion in the boiler. In practice, the unburned fuel percentage depends on the type of boiler and excess fed air. This efficiency loss is one of the most difficult to predict, but for well-maintained boilers at proper levels of excess air, the degree of incomplete combustion should be similar among various technologies. Therefore, the boiler type input will determine this value. 

**Radiation and miscellaneous, %**
  This category encompasses radiation losses and various other losses that are difficult to quantify or predict, such as moisture in air, sensible heat in ash, and radiation in ash pit. The other derates are lumped together under a “manufacturer’s margin” derate, which is taken to be 2.03%. For more information about this category, consult the Technical Manual.

**Total Boiler Efficiency (HHV Basis), %**
*Total Boiler Efficiency =100 - Dry Flue Gas Losses - Moisture in Fuel - Latent Heat - Unburned Fuel - Radiation and Miscellaneous*

Steam Rankine Cycle
~~~~~~~~~~~~~~~~~~~

Steam produced in the boiler at the specified grade drives a steam turbine and electric generator to convert the thermal energy of the steam to electricity. 

.. note:: The biomass power's steam turbine model is based on the empirical parabolic trough model's power block model. For a description of how SAM uses the part-load and temperature adjustment coefficients, see :ref:`Power Block Simulation Calculations <powerblock_pbsimulationcalculations>`.

**Estimated max gross nameplate capacity, kW**
  The estimated nameplate capacity calculated based on type of biomass, amount of biomass, and performance parameters specified on the :doc:`Feedstock <biopower_feedstock>`   and Plant Specs pages. In order to increase the capacity, the biomass supplied on the Feedstock page must be directly increased.

  SAM does not use the estimated max gross nameplate capacity value in simulations. It is shown purely for reference. The simulation engine computes the actual efficiency, whereas the estimated nameplate capacity is based on an estimated efficiency. The simulation engine takes into account variations like ambient conditions or the dispatch schedule. To capture this temporality, the simulation engine averages the hourly efficiencies. 

**Rated cycle conversion efficiency**
  The rated efficiency of the turbine, equivalent to average conversion efficiency of the steam's thermal energy to electricity.

**Minimum load**
  Most turbines do not operate below a certain fraction of full load, when the turbine performance is difficult to predict and the economics may become unfavorable. The fractional value for minimum load represents the threshold below which the turbine will not operate.

**Max overdesign operation**
  Prevents the turbine from operating above a certain fraction of the design load.

**Power cycle design temperature, °F**
  The design temperature of the turbine. The actual efficiency of the turbine is temperature dependent. Fluctuations of the temperature cause changes in the efficiency.

**Part Load and Temperature Efficiency Adjustments**
  The effect of temperature and part load on efficiency can be adjusted with the coefficients F0 – F4. These coefficients define a polynomial equation for adjusting the amount of heat supplied to the power block based on deviations from full load and design temperature.

**Temperature Correction Mode**
  Choose either an air-cooled condenser (dry bulb) or evaporative cooling (wet bulb).

  Dry-bulb temperature refers to the thermodynamic temperature of the air that can be found with a standard thermometer. The wet-bulb temperature also captures the moisture content of the air, and is always less than the dry-bulb temperature (except at 100% relative humidity, when the two are equal).

  Evaporative cooling uses the evaporation of water to cool the process condensate to near the wet-bulb temperature. Dry cooling uses air and thus the minimum heat rejection is the dry-bulb temperature.

  Typically, air-cooled systems require more capital, are less thermodynamically efficient, and use more energy. However, evaporative cooling demands more water and might not be suitable in some regions.

System Availability
-------------------

.. include:: ../includes/snip_system_availability.rst

Parasitics
~~~~~~~~~~

**Parasitic load (% of nameplate), %**
  The electric load requirement as a percentage of the nameplate capacity for plant loads such as pumps, compressors, fans, lighting, etc.

**Total plant parasitic load, kWe**
*Total Plant Parasitic Load (kW) = Parasitic Load (% of Nameplate) ÷ 100 × Estimated Max Gross Nameplate Capacity (kW)*

Time of Dispatch Schedule
~~~~~~~~~~~~~~~~~~~~~~~~~

The Time of Dispatch controls allow you to specify at what times the plant operates, and at what fraction of its nameplate capacity.

If you want the plant to operate at its full capacity at all times, do not check **Enable Time of Dispatch Schedule**.

Check **Enable Time of Dispatch Schedule** to specify fractional generation levels for up to nine periods. For each period, you can specify a fraction of the nameplate capacity.

Use your mouse to select blocks of hours in the schedule matrix and type a period number to specify the hours of each month that the period applies. For example, to specify the hours for Period 2, use your mouse to select a block of hours, and then type the number 2. See the :doc:`Weekday Weekend Schedule <../reference/weekday_schedule>` reference topic for step-by-step instructions for using assigning periods to a schedule matrix.

You can use the dispatch schedule to model:

* Scheduled seasonal outages by specifying a fraction of zero for times when the plant will be down.

* Periods of high demand when the plant can operate above its nameplate capacity, for example during summer months.

.. note:: If you specify a Fractional Generation value greater than the Max Over Design Operation value, the simulation will fail.

* Periods of feedstock shortages and surpluses when the plant is forced to operate below or above capacity.

Ramp Rate
~~~~~~~~~

SAM provides three options for specifying the ramp rate, or the rate at which a plant can increase or decrease its generation. 

**Do not specify ramp rate**
  Assumes that the plant can operate at the fraction of nameplate capacity in each hour that the fraction applies. This option is appropriate when the ramp rate is less than SAM's hourly simulation time step, or when you model the ramp rate explicitly using a series of ramp rates over a period of hours.

**Specify ramp rate in kW per hour**
  Model the ramp rate as an energy requirement as a kW per hour value during periods when the plant operates at a fraction of the nameplate capacity.

**Specify ramp rate in percent of capacity per hour**
  Model the ramp rate as an energy requirement as a percentage of the nameplate capacity during periods when the plant operates at a fraction of the nameplate capacity.