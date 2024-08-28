hmLoadTopic({
hmKeywords:"",
hmTitle:"Power Block",
hmDescription:"Capacity Design gross output (MWe) The power cycle\'s design output, not accounting for parasitic losses. SAM uses this value to size system components, such as the solar field...",
hmPrevLink:"gss_solar_field.html",
hmNextLink:"gss_thermal_storage.html",
hmParentLink:"generic_solar_system.html",
hmBreadCrumbs:"<a href=\"generic_solar_system.html\">CSP Generic Solar<\/a>",
hmTitlePath:"CSP Generic Solar > Power Block",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Power Block<\/span><\/h1>\n\r",
hmBody:"<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Capacity<\/span><\/h3>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Design gross output (MWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The power cycle\'s design output, not accounting for parasitic losses. SAM uses this value to size system components, such as the solar field area when you use the solar multiple to specify the solar field size.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimated gross to net conversion factor<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">An estimate of the ratio of the electric energy delivered to the grid to the power cycle\'s gross output. SAM uses the factor to calculate the system\'s nameplate capacity for capacity-related calculations.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimated net output at design (MWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The power cycle\'s nominal capacity, calculated as the product of the design gross output and estimated gross to net conversion factor. SAM uses this value for capacity-related calculations, including the estimated total cost per net capacity value on the Installation Costs page, capacity-based incentives on the Cash Incentives page, and the capacity factor reported in the results.<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Estimated Net Output at Design (MWe) = Design Gross Output (MWe) × Estimated Gross to Net Conversion Factor<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><a id=\"availabilitycurtailment\" class=\"hmanchor\"><\/a><span class=\"f_Heading3_atocs_\">System Availability<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">System availability losses are reductions in the system\'s output due to operational requirements such as maintenance down time or other situations that prevent the system from operating as designed. <\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">To model curtailment, or forced outages or reduction in power output required by the grid operator, use the inputs on the <a href=\"grid_limits.html\" class=\"topiclink\">Grid<\/a> page.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">For the PV Battery model, battery dispatch is affected by the system availability losses. For the PVWatts Battery, Generic Battery, and Standalone Battery battery dispatch ignores the system availability losses.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">To edit the system availability losses, click <\/span><span class=\"f_CHInterfaceElement\">Edit losses<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The <a href=\"edit_losses.html\" class=\"topiclink\">Edit Losses<\/a> window allows you to define loss factors as follows:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Constant loss is a single loss factor that applies to the system\'s entire output. You can use this to model an availability factor.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Hourly losses apply to specific hours of the year.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM reduces the system\'s output in each hour by the loss percentage that you specify for that hour. For a given hour, a loss of zero would result in no adjustment. A loss of 5% would reduce the output by 5%, and a loss of -5% would increase the output value by 5%. For subhourly simulations, SAM applies the loss factor for a given hour to each time step in that hour<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Conversion<\/span><\/h3>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Note\"> The generic solar system model\'s steam turbine model is based on the empirical parabolic trough model\'s power block model. For a description of how SAM uses the part-load and temperature adjustment coefficients, see <a href=\"troughempirical_power_block.html#powerblock_pbsimulationcalculations\" class=\"topiclink\">Power Block Simulation Calculations<\/a>.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Reference conversion efficiency<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Total thermal to electric efficiency of the reference turbine at design. Used to calculate the design turbine thermal input and required solar field aperture area.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Max over design operation<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The turbine\'s maximum output expressed as a fraction of the design turbine thermal input. Used by the dispatch module to set the power block thermal input limits. In cases where the normalized thermal power delivered to the power block by the solar field exceeds this fraction, the field will dump excess energy.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Minimum load<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The turbine\'s minimum load expressed as a fraction of the design turbine thermal input. Used by the dispatch module to set the power block thermal input limits. In cases where the solar field, thermal storage, and\/or fossil backup system are unable to produce enough energy to meet this fractional requirement, the power cycle will not produce electricity.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Power cycle startup energy<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Hours of equivalent full-load operation of the power cycle required to bring the system to operating temperature after a period of non-operation. Used by the dispatch module to calculate the required start-up energy.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Boiler LHV Efficiency<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The back-up boiler\'s lower heating value efficiency. Used by the power block module to calculate the quantity of gas required by the back-up boiler.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Power cycle design ambient temperature<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The ambient temperature at which the power cycle conversion efficiency is equal to the reference conversion efficiency. The temperature corresponds to either the wet-bulb or dry-bulb temperature, depending on the value selected by the user in the Temperature correction mode list. The temperature is used in the Temperature adjustment polynomial in the Parasitics group on the Power Block page to determine cycle conversion efficiency.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Part load efficiency adjustment<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Coefficients for the turbine thermal-to-electric efficiency polynomial equation. This polynomial is used to adjust the cycle conversion efficiency as the thermal load into the power cycle varies from its design-point value. The resulting value from the evaluated polynomial multiplies the reference conversion efficiency, where the polynomial is formulated as follows:<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><img alt=\"EQ_GSS_Feff-load\" style=\"margin:0;width:28.2500rem;height:2.5625rem;border:none\" src=\".\/images\/eq_gss_feff-load.png\"\/><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Temperature efficiency adjustment<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Factors for polynomial equation adjusting power cycle efficiency based on the difference between the power cycle design temperature and ambient temperature (either wet bulb or dry bulb temperature from the weather file, depending on the option you choose for Temperature Correction Mode.) The polynomial is formulated as follows:<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><img alt=\"EQ_GSS_Feff-temp1\" style=\"margin:0;width:30.2500rem;height:1.3125rem;border:none\" src=\".\/images\/eq_gss_feff-temp1.png\"\/><\/p>\n\r<p class=\"p_VariableDescription\"><img alt=\"EQ_GSS_Feff-temp2\" style=\"margin:0;width:7.5000rem;height:1.3125rem;border:none\" src=\".\/images\/eq_gss_feff-temp2.png\"\/><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">where <\/span><span class=\"f_CHEquationVariable\">T<\/span><span class=\"f_CHVariableSubscript\">amb<\/span><span class=\"f_VariableDescription\"> is the wet or dry bulb temperature, depending on the <\/span><span class=\"f_CHInterfaceElement\">Temperature Correction Mode<\/span><span class=\"f_VariableDescription\"> selection.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span style=\"font-family: Arial,Helvetica,sans-serif;\">The power cycle conversion efficiency <\/span><img alt=\"EQ_GSS_eff-cycles-pb\" style=\"margin:0;width:2.2500rem;height:1.3750rem;border:none\" src=\".\/images\/eq_gss_eff-cycles-pb.png\"\/><span style=\"font-family: Arial,Helvetica,sans-serif;\"> is calculated as the product of the Reference conversion efficiency <\/span><img alt=\"EQ_GSS_eff-cycle-ref-pb\" style=\"margin:0;width:3.5000rem;height:1.3750rem;border:none\" src=\".\/images\/eq_gss_eff-cycle-ref-pb.png\"\/><span style=\"font-family: Arial,Helvetica,sans-serif;\"> and the summation of the two polynomial efficiency adjustment terms.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><img alt=\"EQ_GSS_eff-cycle-pb\" style=\"margin:0;width:14.8125rem;height:1.4375rem;border:none\" src=\".\/images\/eq_gss_eff-cycle-pb.png\"\/><span style=\"font-family: Arial,Helvetica,sans-serif;\"> <\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Temperature Correction Mode<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">In the dry bulb mode, SAM calculates a temperature correction factor to account for cooling tower losses based on the ambient temperature from the weather data set. In wet bulb mode, SAM calculates the wet bulb temperature from the ambient temperature and relative humidity from the weather data.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Parasitics<\/span><\/h3>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Fixed parasitic load (MWe\/MWcap)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A fixed hourly loss calculated as a fraction of the power block nameplate capacity.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Production based parasitic (MWe\/MWe)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A variable hourly loss calculated as a fraction of the system\'s hourly electrical output. The total production-based parasitic is evaluated as follows:<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><img alt=\"EQ_GSS_Wpar-prod\" style=\"margin:0;width:27.8750rem;height:1.4375rem;border:none\" src=\".\/images\/eq_gss_wpar-prod.png\"\/><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">where <\/span><span class=\"f_CHEquationVariable\">F<\/span><span class=\"f_CHVariableSubscript\">par,prod,ref<\/span><span class=\"f_VariableDescription\"> <\/span><span style=\"font-family: Arial,Helvetica,sans-serif;\">is the production based parasitic factor, Fpar,load is the load-based parasitic adjustment factor (defined below), and <\/span><span style=\"font-size: 1.10rem; font-family: \'Times New Roman\',Times,Georgia,serif; font-style: italic;\">F<\/span><span style=\"font-size: 0.80rem; font-family: \'Times New Roman\',Times,Georgia,serif; font-style: italic; vertical-align: sub;\">par,temp<\/span><span style=\"font-family: Arial,Helvetica,sans-serif;\"> is the temperature-based parasitic adjustment factor (also defined below), and <\/span><span class=\"f_CHEquationVariable\">F<\/span><span class=\"f_CHVariableSubscript\">par,DNI<\/span><span style=\"font-family: Arial,Helvetica,sans-serif;\"> is the solar resource-based parasitic adjustment factor (also defined below).<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Part load adjustment<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Coefficients for a polynomial that adjusts the parasitic consumption as a function of power cycle gross power output. The result of the polynomial is denoted as <\/span><span class=\"f_CHEquationVariable\">F<\/span><span class=\"f_CHVariableSubscript\">par,load<\/span><span class=\"f_VariableDescription\"> in the Production based parasitic description above.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Temperature adjustment<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Coefficients for a polynomial that adjusts the parasitic consumption as a function of the difference between ambient temperature and the reference power cycle ambient temperature. The result of the polynomial is denoted as <\/span><span class=\"f_CHEquationVariable\">F<\/span><span class=\"f_CHVariableSubscript\">par,temp<\/span><span class=\"f_VariableDescription\"> in the Production based parasitic description above.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">DNI adjustment<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Coefficients for a polynimal that adjusts the parasitic consumption as a function of the solar resource value. The result of the polynomial is denoted as F_(par,DNI) in the Production based parasitic description above.<\/span><\/p>\n\r"
})
