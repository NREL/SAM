hmLoadTopic({
hmKeywords:"",
hmTitle:"Plant Specs",
hmDescription:"The Plant Specs page inputs define the major unit operations that make up a biomass power plant: biomass processing, combustion system and boiler, and steam turbine.",
hmPrevLink:"biopower_feedstock.html",
hmNextLink:"biopower_emissions.html",
hmParentLink:"biopower.html",
hmBreadCrumbs:"<a href=\"biopower.html\">Biomass Combustion<\/a>",
hmTitlePath:"Biomass Combustion > Plant Specs",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Plant Specs<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The Plant Specs page inputs define the major unit operations that make up a biomass power plant: biomass processing, combustion system and boiler, and steam turbine.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For a technical description of the biopower model, see Jorgenson, J.; Gilman, P.; Dobos, A. (2011). Technical Manual for the SAM Biomass Power Generation Model. 40 pp.; NREL Report No. TP-6A20-52688. <a href=\"http:\/\/www.nrel.gov\/docs\/fy11osti\/52688.pdf\" target=\"_blank\" class=\"weblink\">http:\/\/www.nrel.gov\/docs\/fy11osti\/52688.pdf<\/a><\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Biomass Feedstock Handling<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The biopower model has three options for specifying biomass moisture content.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Fed as received<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The biomass feedstock does not undergo any substantial drying before being fed to the combustor. This option avoids drying costs but penalizes the boiler efficiency since evaporation of biomass moisture requires energy input.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Allow feedstock to air-dry to atmospheric Equilibrium Moisture Content (EMC)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The biomass is exposed to the ambient atmospheric conditions for a sufficient amount of time to reach EMC. However, moisture composition doesn’t change instantly, and thus the equilibrium moisture levels are calculated on a monthly basis.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Dry to specified moisture content<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The feedstock handling system includes a dryer as an additional capital expenditure that you specify on the <a href=\"cc_biopower.html\" class=\"topiclink\">Installation Costs<\/a> page. Adding a dryer also increases the parasitic load of the plant and may add an incremental operation and maintenance cost. Although adding a dryer can increase the boiler efficiency by several percent, dryers are not widely used in practice because of the additional costs and parasitic loads.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Combustion System<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM can model three common combustion systems encountered in biomass power plants. For a more detailed description of the combustion systems with suggestions for choosing input values, see Section 3.1.2 of the SAM biomass power technical manual referenced at the top of this page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Grate Stoker Furnace<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A grate stoker furnace is designed to feed solid fuel onto a grate where burning occurs, with combustion air passing through the grate. Stokers are generally the least expensive of the three boiler types and are best suited for large fuel feed rates, typically between 75,000 lb\/hr and 700,000 lb\/hr.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Fluidized Bed Combustor (FBC)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Fluidized bed combustion features a bed of fuel and sand or other inert substance that becomes suspended by the combustion air flowing upward. This technology reduces the fluctuations in steam production associated with changeable feedstocks, and features a lower combustion temperature and reduced formation of pollutants. However, capital costs and operating costs are typically higher for the FBC than the other combustion systems.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Cyclone Furnace<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Cyclone furnaces allow for flexibility in fuel types and increase combustion efficiency over stoker boilers by feeding the fuel in a spiral manner. Additionally, cyclone furnaces are smaller and have a lower capital cost than FBCs.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> SAM does not automatically change the cost assumptions on the Installation Costs page when you change the combustion system option. Be sure to use costs appropriate for the type of plant you specify.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Boiler Parameters<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">You can specify the main parameters that determine boiler and furnace efficiency. <\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> The values of the parameters depend on the type of combustor. The default values are for a boiler for a steam grade of 900 F, 900 psig. If you choose a different steam grade, be sure to change the value of the other parameters accordingly.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Steam Grade<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The severity of the steam grade is often determined by the type of boiler. For example, lower combustion temperatures in fluidized bed combustors often result in lower steam grades. The steam grade directly determines the enthalpy of the steam produced in the boiler. <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Percent excess fed air, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">By convention, the percent excess air is specified on a volumetric\/molar basis. Combustion air from the atmosphere is only 21% oxygen by volume (and the balance nitrogen). Therefore, most of the enthalpy losses result from heating the nitrogen that accompanies the combustion oxygen. Increasing the excess fed air percentage decreases the boiler efficiency because more energy is required to heat the combustion air. If the excess air fed air percentage is too low, the fuel will not combust fully and the boiler can emit carbon monoxide and smoke.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Number of boilers<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Using fewer boilers reduces installation costs. Using more boilers may also reduce costs by offering the benefit of economies of scale. Typically, small utility scale-sized biopower plants use between one and three boilers.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Flue gas temperature, °F<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The flue gas is the mixture of gases exiting the plant through the stack. All useable heat has been collected when the combustion gases reach the specified flue gas temperature. Flue gas heat is often used to preheat other process streams, such as the boiler feedwater. The most efficient boilers utilize as much of the flue gas heat as possible before it exits the plant.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimated steam produced, lb\/hr steam<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">This metric is calculated based on the estimated efficiency of the boiler and the enthalpy of the steam produced. The steam produced in the boiler directly powers the steam turbine.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Boiler overdesign factor, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Boilers are generally oversized to prevent operating above capacity and for the ability to accommodate more biomass throughput. A higher value increases the boiler capital cost. Too low of a value results in lower overall efficiency.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Design capacity of each boiler, lb\/hr steam<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Boilers are generally oversized to accommodate fluctuations in steam production and to allow for additional capacity. However, highly oversized boilers can result in increased efficiency loss and capital cost. The boiler overdesign factor input will directly adjust the design capacity of each boiler metric.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Estimated Efficiency Losses (HHV)<\/span><\/h4>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Dry flue gas losses, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Combustion air enters the furnace at ambient temperature, where it is immediately subject to preheating by waste process heat. Regardless of how the air is preheated, a significant loss of enthalpy occurs when the combustion gas exits the plant at a much higher temperature than the temperature at which it was fed. The Dry Flue Gas Loss is largely determined by the input percent excess fed air. Combustion air from the atmosphere is only 21% oxygen by volume (and the balance nitrogen). Thus, much of the enthalpy losses result from heating up the nitrogen that accompanies the combustion oxygen.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Moisture in flue, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Moisture in fuel adversely affects plant efficiency in two primary ways. First, water in biomass imposes extra mass that must be consequently hauled and processed with the biomass itself. Additionally, the water absorbs heat from the combustion reaction that is unlikely to be recovered. Some power plants employ pre-combustion biomass drying to reduce moisture content and efficiency loss in this category. SAM allows the user to add a dryer under the dry to specified moisture content input on the Plant Specs page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Latent heat, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Loss of latent heat results when elemental hydrogen in biomass combusts to form water. The water produced will leave the stack at the flue gas temperature as water vapor, thus requiring the latent heat of vaporization of water as well as the sensible heat of the vapor at the flue gas temperature.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Unburned fuel, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Unburned fuel losses simply result from incomplete combustion in the boiler. In practice, the unburned fuel percentage depends on the type of boiler and excess fed air. This efficiency loss is one of the most difficult to predict, but for well-maintained boilers at proper levels of excess air, the degree of incomplete combustion should be similar among various technologies. Therefore, the boiler type input will determine this value. <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Radiation and miscellaneous, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">This category encompasses radiation losses and various other losses that are difficult to quantify or predict, such as moisture in air, sensible heat in ash, and radiation in ash pit. The other derates are lumped together under a “manufacturer’s margin” derate, which is taken to be 2.03%. For more information about this category, consult the Technical Manual.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total Boiler Efficiency (HHV Basis), %<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Total Boiler Efficiency =100 - Dry Flue Gas Losses - Moisture in Fuel - Latent Heat - Unburned Fuel - Radiation and Miscellaneous<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Steam Rankine Cycle<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Steam produced in the boiler at the specified grade drives a steam turbine and electric generator to convert the thermal energy of the steam to electricity. <\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> The biomass power\'s steam turbine model is based on the empirical parabolic trough model\'s power block model. For a description of how SAM uses the part-load and temperature adjustment coefficients, see <a href=\"troughempirical_power_block.html#powerblock_pbsimulationcalculations\" class=\"topiclink\">Power Block Simulation Calculations<\/a>.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimated max gross nameplate capacity, kW<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The estimated nameplate capacity calculated based on type of biomass, amount of biomass, and performance parameters specified on the <a href=\"biopower_feedstock.html\" class=\"topiclink\">Feedstock<\/a> and Plant Specs pages. In order to increase the capacity, the biomass supplied on the Feedstock page must be directly increased.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">SAM does not use the estimated max gross nameplate capacity value in simulations. It is shown purely for reference. The simulation engine computes the actual efficiency, whereas the estimated nameplate capacity is based on an estimated efficiency. The simulation engine takes into account variations like ambient conditions or the dispatch schedule. To capture this temporality, the simulation engine averages the hourly efficiencies. <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Rated cycle conversion efficiency<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The rated efficiency of the turbine, equivalent to average conversion efficiency of the steam\'s thermal energy to electricity.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Minimum load<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Most turbines do not operate below a certain fraction of full load, when the turbine performance is difficult to predict and the economics may become unfavorable. The fractional value for minimum load represents the threshold below which the turbine will not operate.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Max overdesign operation<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Prevents the turbine from operating above a certain fraction of the design load.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Power cycle design temperature, °F<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The design temperature of the turbine. The actual efficiency of the turbine is temperature dependent. Fluctuations of the temperature cause changes in the efficiency.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Part Load and Temperature Efficiency Adjustments<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The effect of temperature and part load on efficiency can be adjusted with the coefficients F0 – F4. These coefficients define a polynomial equation for adjusting the amount of heat supplied to the power block based on deviations from full load and design temperature.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Temperature Correction Mode<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Choose either an air-cooled condenser (dry bulb) or evaporative cooling (wet bulb).<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Dry-bulb temperature refers to the thermodynamic temperature of the air that can be found with a standard thermometer. The wet-bulb temperature also captures the moisture content of the air, and is always less than the dry-bulb temperature (except at 100% relative humidity, when the two are equal).<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Evaporative cooling uses the evaporation of water to cool the process condensate to near the wet-bulb temperature. Dry cooling uses air and thus the minimum heat rejection is the dry-bulb temperature.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Typically, air-cooled systems require more capital, are less thermodynamically efficient, and use more energy. However, evaporative cooling demands more water and might not be suitable in some regions.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">System Availability<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">System availability losses are reductions in the system\'s output due to operational requirements such as maintenance down time or other situations that prevent the system from operating as designed. <\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">To model curtailment, or forced outages or reduction in power output required by the grid operator, use the inputs on the <a href=\"grid_limits.html\" class=\"topiclink\">Grid<\/a> page.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">For the PV Battery model, battery dispatch is affected by the system availability losses. For the PVWatts Battery, Custom Generation Battery, and Standalone Battery battery dispatch ignores the system availability losses.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">To edit the system availability losses, click <\/span><span class=\"f_CHInterfaceElement\">Edit losses<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The <a href=\"edit_losses.html\" class=\"topiclink\">Edit Losses<\/a> window allows you to define loss factors as follows:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Constant loss is a single loss factor that applies to the system\'s entire output. You can use this to model an availability factor.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Hourly losses apply to specific hours of the year.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM reduces the system\'s output in each hour by the loss percentage that you specify for that hour. For a given hour, a loss of zero would result in no adjustment. A loss of 5% would reduce the output by 5%, and a loss of -5% would increase the output value by 5%. For subhourly simulations, SAM applies the loss factor for a given hour to each time step in that hour<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Parasitics<\/span><\/h3>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Parasitic load (% of nameplate), %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The electric load requirement as a percentage of the nameplate capacity for plant loads such as pumps, compressors, fans, lighting, etc.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Total plant parasitic load, kWe<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Total Plant Parasitic Load (kW) = Parasitic Load (% of Nameplate) ÷ 100 × Estimated Max Gross Nameplate Capacity (kW)<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Time of Dispatch Schedule<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The Time of Dispatch controls allow you to specify at what times the plant operates, and at what fraction of its nameplate capacity.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">If you want the plant to operate at its full capacity at all times, do not check <\/span><span class=\"f_CHInterfaceElement\">Enable Time of Dispatch Schedule<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Check <\/span><span class=\"f_CHInterfaceElement\">Enable Time of Dispatch Schedule<\/span><span class=\"f_Text\"> to specify fractional generation levels for up to nine periods. For each period, you can specify a fraction of the nameplate capacity.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Use your mouse to select blocks of hours in the schedule matrix and type a period number to specify the hours of each month that the period applies. For example, to specify the hours for Period 2, use your mouse to select a block of hours, and then type the number 2. See the <a href=\"weekday-schedule.html\" class=\"topiclink\">Weekday Weekend Schedule<\/a> reference topic for step-by-step instructions for using assigning periods to a schedule matrix.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">You can use the dispatch schedule to model:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Scheduled seasonal outages by specifying a fraction of zero for times when the plant will be down.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Periods of high demand when the plant can operate above its nameplate capacity, for example during summer months.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> If you specify a Fractional Generation value greater than the Max Over Design Operation value, the simulation will fail.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Periods of feedstock shortages and surpluses when the plant is forced to operate below or above capacity.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Ramp Rate<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM provides three options for specifying the ramp rate, or the rate at which a plant can increase or decrease its generation. <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Do not specify ramp rate<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Assumes that the plant can operate at the fraction of nameplate capacity in each hour that the fraction applies. This option is appropriate when the ramp rate is less than SAM\'s hourly simulation time step, or when you model the ramp rate explicitly using a series of ramp rates over a period of hours.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Specify ramp rate in kW per hour<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Model the ramp rate as an energy requirement as a kW per hour value during periods when the plant operates at a fraction of the nameplate capacity.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Specify ramp rate in percent of capacity per hour<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Model the ramp rate as an energy requirement as a percentage of the nameplate capacity during periods when the plant operates at a fraction of the nameplate capacity.<\/span><\/p>\n\r"
})
