hmLoadTopic({
hmKeywords:"",
hmTitle:"System Design",
hmDescription:"The System Design page shows inputs for design point parameters that determine the system\'s nameplate capacity. Use the System Design inputs to define the nominal ratings of...",
hmPrevLink:"mslf_location_and_resource.html",
hmNextLink:"mslf_solar_field.html",
hmParentLink:"molten_salt_linear_fresnel.html",
hmBreadCrumbs:"<a href=\"molten_salt_linear_fresnel.html\">CSP Linear Fresnel Molten Salt<\/a>",
hmTitlePath:"CSP Linear Fresnel Molten Salt > System Design",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">System Design<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The System Design page shows inputs for design point parameters that determine the system\'s nameplate capacity. Use the System Design inputs to define the nominal ratings of the system, and then specify details for each part of the system on the appropriate input pages.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> All of the system design inputs are nominal values, or values at the system\'s design point. SAM calculates actual values during simulation and reports them in the <a href=\"results_page.html\" class=\"topiclink\">results<\/a>.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Solar Field<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The solar field design parameters determine the size of the solar field. See the <a href=\"mslf_solar_field.html\" class=\"topiclink\">Solar Field<\/a>, and <a href=\"mslf_collector_and_receiver.html\" class=\"topiclink\">Collector and Receiver<\/a> pages to set detailed parameters for the field.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Option 1 and Option 2<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">For Option 1 (solar multiple mode), SAM calculates the field aperture based on the value you enter for the solar multiple. <\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">For option 2 (field aperture mode), SAM calculates the solar multiple based on the field aperture value you enter. Note that SAM does not use the value that appears dimmed for the inactive option. See <a href=\"mslf_system_design.html#solarmultiple\" class=\"topiclink\">Solar Multiple<\/a> for details.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Solar Multiple<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The field aperture area expressed as a multiple of the aperture area required to operate the power cycle at its design capacity. See <a href=\"mslf_system_design.html#solarmultiple\" class=\"topiclink\">Solar Multiple<\/a> for details.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Field Aperture (m²)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The total solar energy collection area of the solar field in square meters. Note that this is less than the total mirror surface area.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> For simulations, SAM uses the calculated solar multiple and field aperture values shown in blue to the right of the inputs. The calculated value of the inactive option may differ from the value you see on the System Design page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Design point DNI (W\/m²)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The design point direct normal radiation value, used in solar multiple mode to calculate the aperture area required to drive the power cycle at its design capacity. Also used to calculate the design mass flow rate of the heat transfer fluid for header pipe sizing.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Field thermal power, MWt<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The field thermal power output at design.<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Field Thermal Power (MWt) = Solar Multiple × Cycle Thermal Power (MWt)<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Loop inlet HTF temperature (ºC)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The temperature of HTF at the loop inlet under design conditions. The actual temperature during operation may differ from this value. SAM sets the power cycle\'s design outlet temperature equal to this value.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Loop outlet HTF temperature (ºC)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The temperature of the HTF at the outlet of the loop under design conditions. During operation, the actual value may differ from this set point. This value represents the target temperature for control of the HTF flow through the solar field and will be maintained when possible.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Number of loops<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The number of loops in the field, equal to the solar multiple times the required number of loops at a solar multiple of 1.0. The required number of loops is rounded to the nearest integer to represent a realistic field layout.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Power Cycle<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The power cycle design parameters determine the capacity of the power cycle, and the nameplate capacity of the system. See the <a href=\"mslf_power_cycle.html\" class=\"topiclink\">Power Cycle<\/a> page for more detailed power cycle options and detailed parameters.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Design turbine gross output (MWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The power cycle\'s design output, not accounting for parasitic losses. SAM <\/span>uses this value to size system components, such as the solar field area when you use the solar multiple to specify the solar field size.<span style=\"font-size: 1.20rem; font-family: \'Times New Roman\',Times,Georgia,serif;\"> <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimated gross to net conversion factor<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">An estimate of the ratio of the electric energy delivered to the grid to the power cycle\'s gross output. SAM uses the factor to calculate the power cycle\'s nameplate capacity for capacity-related calculations, including the estimated total cost per net capacity value on the <a href=\"cc_linear_fresnel.html\" class=\"topiclink\">Installation Costs<\/a> and <a href=\"oc_operating.html\" class=\"topiclink\">Operating Costs<\/a> pages capacity-based incentives on the <a href=\"incentives.html\" class=\"topiclink\">Incentives<\/a> page, and the capacity factor reported in the <a href=\"results_page.html\" class=\"topiclink\">results<\/a>.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimated net output at design (nameplate) (MWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The power cycle\'s nameplate capacity, calculated as the product of the design gross output and estimated gross to net conversion factor.<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Estimated Net Output at Design (MWe) = Design Gross Output (MWe) × Estimated Gross to Net Conversion Factor<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Cycle thermal efficiency<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The thermal to electric conversion efficiency of the power cycle under design conditions<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Cycle thermal power, MWt<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The power cycle thermal power input at design.<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Cycle Thermal Power, MWt = Design Turbine Gross Output (MWe) ÷ Cycle Thermal Efficiency<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Thermal Storage<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The thermal storage design parameters determine the size of the thermal energy storage system (TES). See the <a href=\"mslf_thermal_storage.html\" class=\"topiclink\">Thermal Storage<\/a> page for detailed TES parameters.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Hours of storage at design point (hours)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The thermal storage capacity expressed in number of hours of thermal energy delivered at the design power cycle thermal power. The physical capacity is the number of hours of storage multiplied by the power cycle design thermal input. Used to calculate the thermal energy system\'s maximum storage capacity.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><a id=\"solarmultiple\" class=\"hmanchor\"><\/a><span class=\"f_Heading3_atocs_\">Solar Multiple<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Sizing the solar field of a parabolic trough or linear Fresnel system in SAM involves determining the optimal solar field aperture area for a system at a given location. In general, increasing the solar field area increases the system\'s electric output, thereby reducing the project\'s LCOE. However, during times when there is enough solar resource, too large of a field will produce more thermal energy than the power block and other system components can handle. Also, as the solar field size increases beyond a certain point, the higher installation and operating costs outweigh the benefit of the higher output.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">An optimal solar field area should:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Maximize the amount of time in a year that the field generates sufficient thermal energy to drive the power block at its rated capacity.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Minimize installation and operating costs.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Use thermal energy storage and backup power equipment efficiently and cost effectively.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The problem of choosing an optimal solar field area involves analyzing the tradeoff between a larger solar field that maximizes the system\'s electrical output and project revenue, and a smaller field that minimizes installation and operating costs.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The levelized cost of energy (LCOE) is a useful metric for optimizing the solar field size because it includes the amount of electricity generated by the system, the project installation costs, and the cost of operating and maintaining the system over its life. Optimizing the solar field involves finding the solar field aperture area that results in the lowest LCOE. For systems with thermal energy storage systems, the optimization involves finding the combination of field area and storage capacity that results in the lowest LCOE.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Option 1 and Option 2<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM\'s parabolic trough and linear Fresnel models provide two options for specifying the solar field aperture area on the System Design page:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Option 1: You specify the solar field area as a multiple of the power block\'s rated capacity (design turbine gross output), and SAM calculates the solar field aperture area in square meters required to meet power block rated capacity.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Option 2: You specify the aperture area in square meters independently of the power block\'s rated capacity.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">If your analysis involves a known solar field area, you should use Option 2 to specify the solar field aperture area.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">If your analysis involves optimizing the solar field area for a specific location, or choosing an optimal combination of solar field aperture area and thermal energy storage capacity, then you should choose Option 1, and follow the procedure described below to size the field.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Solar Multiple<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The solar multiple makes it possible to represent the solar field aperture area as a multiple of the power block rated capacity. A solar multiple of one (SM=1) represents the solar field aperture area that, when exposed to solar radiation equal to the design point DNI (or irradiation at design), generates the quantity of thermal energy required to drive the power block at its rated capacity (design gross output), accounting for thermal and optical losses.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Because at any given location the number of hours in a year that the actual solar resource is equal to the design point DNI &nbsp;is likely to be small, a solar field with SM=1 will rarely drive the power block at its rated capacity. Increasing the solar multiple (SM&gt;1) results in a solar field that operates at its design point for more hours of the year and generates more electricity. <\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For example, consider a system with a power block design gross output rating of 111 MW and a solar multiple of one (SM=1) and no thermal storage. The following frequency distribution graph shows that the power block never generates electricity at its rated capacity, and generates less than 80% of its rated capacity for most of the time that it generates electricity:<\/span><\/p>\n\r<p class=\"p_Text\" style=\"text-align: center;\"><img alt=\"IMG_TroughPhysical-GrossPowerPDFSM1\" style=\"margin:0 auto 0 auto;width:28.4375rem;height:9.9375rem;border:none\" src=\".\/images\/img_troughphysical-grosspowerpdfsm1.png\"\/><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For the same system with a solar multiple chosen to minimize LCOE (in this example SM=1.5), the power block generates electricity at or slightly above its rated capacity almost 15% of the time:<\/span><\/p>\n\r<p class=\"p_Text\" style=\"text-align: center;\"><img alt=\"IMG_TroughPhysical-GrossPowerPDFSM15\" style=\"margin:0 auto 0 auto;width:22.7500rem;height:9.9375rem;border:none\" src=\".\/images\/img_troughphysical-grosspowerpdfsm15.png\"\/><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Adding thermal storage to the system changes the optimal solar multiple, and increases the amount of time that the power block operates at its rated capacity. In this example, the optimal storage capacity (full load hours of TES) is 3 hours with SM=1.75, and the power block operates at or over its rated capacity over 20% of the time:<\/span><\/p>\n\r<p class=\"p_Text\" style=\"text-align: center;\"><img alt=\"IMG_TroughPhysical-GrossPowerPDFSM175TES3\" style=\"margin:0 auto 0 auto;width:22.7500rem;height:9.9375rem;border:none\" src=\".\/images\/img_troughphysical-grosspowerpdfsm175tes3.png\"\/><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> For clarity, the frequency distribution graphs above exclude nighttime hours when the gross power output is zero.<\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Reference Weather Conditions for Field Sizing<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The design weather conditions values are reference values that represent the solar resource at a given location for solar field sizing purposes. The field sizing equations require three reference condition variables:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Ambient temperature<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Direct normal irradiance (DNI)<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Wind velocity<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The values are necessary to establish the relationship between the field aperture area and power block rated capacity for solar multiple (SM) calculations.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> The design values are different from the data in the weather file. SAM uses the design values to size the solar field before running a simulation. During the simulation, SAM uses data from the weather file you choose on the Location and Resource page.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The reference ambient temperature and reference wind velocity variables are used to calculate the design heat losses, and do not have a significant effect on the solar field sizing calculations. Reasonable values for those two variables are the average annual measured ambient temperature and wind velocity at the project location. For the physical trough model, the reference temperature and wind speed values are hard-coded and cannot be changed. The linear Fresnel and generic solar system models allow you to specify the reference ambient temperature value, but not the wind speed. The empirical trough model allows you to specify both the reference ambient temperature and wind speed values. <\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The reference direct normal irradiance (DNI) value, on the other hand, does have a significant impact on the solar field size calculations. For example, a system with reference conditions of 25°C, 950 W\/m<\/span><span class=\"f_Text\" style=\"font-size: 0.70rem; vertical-align: super;\">2<\/span><span class=\"f_Text\">, and 5 m\/s (ambient temperature, DNI, and wind speed, respectively), a solar multiple of 2, and a 100 MWe power block, requires a solar field area of 871,940 m<\/span><span class=\"f_Text\" style=\"font-size: 0.70rem; vertical-align: super;\">2<\/span><span class=\"f_Text\">. The same system with reference DNI of 800 W\/m<\/span><span class=\"f_Text\" style=\"font-size: 0.70rem; vertical-align: super;\">2<\/span><span class=\"f_Text\"> requires a solar field area of 1,055,350 m<\/span><span class=\"f_Text\" style=\"font-size: 0.70rem; vertical-align: super;\">2<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">In general, the reference DNI value should be close to the maximum actual DNI on the field expected for the location. For systems with horizontal collectors and a field azimuth angle of zero in the Mohave Desert of the United States, we suggest a design irradiance value of 950 W\/m2. For southern Spain, a value of 800 W\/m2 is reasonable for similar systems. However, for best results, you should choose a value for your specific location using one of the methods described below.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Linear collectors (parabolic trough and linear Fresnel) typically track the sun by rotating on a single axis, which means that the direct solar radiation rarely (if ever) strikes the collector aperture at a normal angle. Consequently, the DNI incident on the solar field in any given hour will always be less than the DNI value in the resource data for that hour. The cosine-adjusted DNI value that SAM reports in simulation results is a measure of the incident DNI.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Using too low of a reference DNI value results in excessive &quot;dumped&quot; energy: Over the period of one year, the actual DNI from the weather data is frequently greater than the reference value. Therefore, the solar field sized for the low reference DNI value often produces more energy than required by the power block, and excess thermal energy is either dumped or put into storage. On the other hand, using too high of a reference DNI value results in an undersized solar field that produces sufficient thermal energy to drive the power block at its design point only during the few hours when the actual DNI is at or greater than the reference value.<\/span><\/p>\n\r<p class=\"p_TitleProcedure\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_TitleProcedure\">To choose a reference DNI value:<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">1.<\/span><span class=\"f_ProcedureStep\">Choose a weather file on the Location and Resource page.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">2.<\/span><span class=\"f_ProcedureStep\">For systems with storage, specify the storage capacity and maximum storage charge rate defined on the System Design page.<\/span><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">3.<\/span><span class=\"f_ProcedureStep\">Click <\/span><span class=\"f_CHInterfaceElement\">Simulate<\/span><span class=\"f_ProcedureStep\">.<\/span><\/p>\n\r<p class=\"p_ProcedureStepNoNumber\" style=\"text-align: center; page-break-after: avoid;\"><img alt=\"SS_MainWindow-RunAllSimulationsButton\" style=\"margin:0 auto 0 auto;width:16.5000rem;height:7.3125rem;border:none\" src=\".\/images\/ss_mainwindow-runallsimulationsbutton.png\"\/><\/p>\n\r<p class=\"p_ProcedureStep\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">4.<\/span><span class=\"f_ProcedureStep\">On the Results page, click <\/span><span class=\"f_CHInterfaceElement\">Statistics<\/span><span class=\"f_ProcedureStep\">.<\/span><\/p>\n\r<p class=\"p_ProcedureStepLast\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_ProcedureStep\" style=\"display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">5.<\/span><span class=\"f_ProcedureStepLast\">Read the maximum annual value of <\/span><span class=\"f_CHInterfaceElement\">Field collector DNI-cosine product (W\/m2)<\/span><span class=\"f_ProcedureStepLast\">, and use this value for the reference DNI.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">An alternative method is to choose a reference DNI value on the System Design page to minimize collector defocusing, to do this, try different values for Design point DNI on the System Design page until you find a value that minimizes the total Field fraction of focused SCAs output variable on the Statistics tab. You could also use <a href=\"parametrics.html\" class=\"topiclink\">parametric simulations<\/a> to find the reference DNI value.<\/span><\/p>\n\r"
})
