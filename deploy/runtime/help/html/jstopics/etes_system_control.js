hmLoadTopic({
hmKeywords:"",
hmTitle:"System Control",
hmDescription:"The System Control inputs determine the operating parameters of the system.",
hmPrevLink:"etes_thermal_storage.html",
hmNextLink:"ptes.html",
hmParentLink:"etes.html",
hmBreadCrumbs:"<a href=\"etes.html\">Electric Thermal Energy Storage<\/a>",
hmTitlePath:"Electric Thermal Energy Storage > System Control",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">System Control<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The System Control inputs determine the operating parameters of the system.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Plant Energy Consumption<\/span><\/h3>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Fraction of gross power consumed at all times, MWe\/MWcap<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A fixed electric load applied to all hours of the simulation, expressed as a fraction of rated gross power at design from the <a href=\"mspt_system_design.html\" class=\"topiclink\">System Design<\/a> page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Balance of Plant Parasitic, MWe\/MWcap<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Losses as a fraction of the power block nameplate capacity that apply in hours when the power block operates.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Aux heater boiler parasitic (MWe\/MWcap)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">A parasitic load that is applied as a function of the thermal output of the auxiliary fossil-fired heaters.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The BOP and auxiliary heater parasitic at design are calculated as follows:<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Parasitic Loss (MWe) = P (MWe\/MWcap) × F × ( C0 + C1 + C2 ) × Design Turbine Gross Output (MWe)<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">System Availability<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">System availability losses are reductions in the system\'s output due to operational requirements such as maintenance down time or other situations that prevent the system from operating as designed. <\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Notes.<\/span><span class=\"f_Note\"> <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">To model curtailment, or forced outages or reduction in power output required by the grid operator, use the inputs on the <a href=\"grid_limits.html\" class=\"topiclink\">Grid<\/a> page.<\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">For the PV Battery model, battery dispatch is affected by the system availability losses. For the PVWatts Battery, Custom Generation Battery, and Standalone Battery battery dispatch ignores the system availability losses.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">To edit the system availability losses, click <\/span><span class=\"f_CHInterfaceElement\">Edit losses<\/span><span class=\"f_Text\">.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The <a href=\"edit_losses.html\" class=\"topiclink\">Edit Losses<\/a> window allows you to define loss factors as follows:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Constant loss is a single loss factor that applies to the system\'s entire output. You can use this to model an availability factor.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Hourly losses apply to specific hours of the year.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM reduces the system\'s output in each hour by the loss percentage that you specify for that hour. For a given hour, a loss of zero would result in no adjustment. A loss of 5% would reduce the output by 5%, and a loss of -5% would increase the output value by 5%. For subhourly simulations, SAM applies the loss factor for a given hour to each time step in that hour<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Dispatch Optimization<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">When you run a simulation without enabling optimization, the plant controller determines operation based on the value of the PPA multiplier at that time step. If the multiplier is less than one, then TES charges if the TES can accept the energy. If the multiplier is greater than one, then the TES discharges if TES has available energy.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Enable dispatch optimization<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Check the box to enable automatic optimization.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Time horizon for dispatch optimization<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The time period that SAM uses as a basis for the optimization. Default value is 48 hours.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Frequency for dispatch reoptimization<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The time period to determine how often SAM runs the optimization. Default value is 24 hours.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Cycle startup cost penalty, \$\/start<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The penalty in the dispatch optimization algorithm associated with starting up the power cycle. The cost is applied any time the power cycle goes from an “off” state to an “on” or “standby” state in the next time period. This penalty affects the optimal solution, which seeks to maximize revenue. This value <\/span><span class=\"f_Emphasis\">does not<\/span><span class=\"f_VariableDescription\"> affect actual operating costs or the calculated SAM financial metrics.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Heater startup cost penalty, \$\/start<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The penalty in the dispatch optimization algorithm associated with starting up the solar field and receiver. The cost is applied any time the solar field goes from an “off” state to an “on” state in the next time period. This penalty affects the optimal solution, which seeks to maximize revenue. This value <\/span><span class=\"f_Emphasis\">does not<\/span><span class=\"f_VariableDescription\"> affect actual operating costs or the calculated SAM financial metrics.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Power generation ramping penalty<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The penalty imposed for changing power cycle electrical production from one time step to the next. By penalizing changes, the resulting dispatch profile exhibits improved stability and is potentially more realizable in practice. Increasing this penalty may reduce achieved revenue for the project. This penalty affects the optimal solution, which seeks to maximize revenue. This value does not affect actual operating costs or the calculated SAM financial metrics.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Minimum cycle downtime<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The minimum duration of time the cycle must remain off after turning off. This parameter, along with the startup penalty, can help eliminate frequent cycle startup and shutdown events.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Minimum cycle uptime<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The minimum duration of time the cycle must remain on after turning on. This parameter, along with the startup penalty, can help eliminate frequent cycle startup and shutdown events.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Time horizon for dispatch optimization<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The period over which a single call to mixed integer linear program is optimized to maximize revenue.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Frequency for dispatch reoptimization<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The period over which the engineering model solves using the dispatch signal before the problem is reoptimized for the next horizon. For example, if the time horizon is 48 hours and the frequency is 24 hours, then the simulation rolling time horizon begins as follows: 1) dispatch is optimized for hours 1-48, 2) engineering model uses dispatch signal to solve for hours 1-24, 3) dispatch is optimized for hours 25-72 using initial conditions from last timestep solved by engineering model, 4) engineering model uses dispatch signal to solve for hours 25-48. 5) repeat over annual simulation.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Objective function time weighting exponent<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The relative weight due to time in the dispatch optimization objective function. A weighting factor of 0.99 indicates that the objective function terms are multiplied by <\/span><span class=\"f_CHEquationVariable\">0.99<\/span><span class=\"f_CHEquationVariable\" style=\"font-size: 0.70rem; vertical-align: super;\">t<\/span><span class=\"f_VariableDescription\"> &nbsp;for each timestep t in the optimization horizon (48 hours, by default). A value of 1.0 indicates no time weighting, a value less than one indicates that – all things equal – generation is preferred sooner than later, and a value greater than one indicates that generation is preferred later than sooner. As the value is displaced from unity, the optimization algorithm is typically able to solve the dispatch problem more quickly, but the resulting revenue may decrease. Note that a value of 0.99 corresponds to an objective discounting in the 24th time period (one day ahead) of <\/span><span class=\"f_CHEquationVariable\">0.99<\/span><span class=\"f_CHEquationVariable\" style=\"font-size: 0.70rem; vertical-align: super;\">24<\/span><span class=\"f_CHEquationVariable\"> = 0.79<\/span><span class=\"f_VariableDescription\">, which is to say that the optimization routine values revenue generated in hour one 21% more than in hour 24, though revenue multipliers and efficiency terms may be identical.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Maximum branch and bound iterations<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Limits the number of iterations in the optimization routine. If you are experiencing problems with the optimization, you can increase the number. The default value is 30,000.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Solution optimality gap tolerance<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Determines the tolerance for the optimization solution. You can decrease the tolerance if you are experiencing problems with the optimization. Default value is 0.001.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Optimization solver timeout limit, seconds<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Limits the amount of time the optimization will attempt to find a solution. You can increase the timeout limit if you are experiencing problems with the optimization. Default value is 5 seconds.<\/span><\/p>\n\r"
})
