hmLoadTopic({
hmKeywords:"",
hmTitle:"Marine Energy Installation Costs",
hmDescription:"When you use the marine energy wave or marine energy tidal performance model with the LCOE calculator financial model, SAM uses a fixed-charge-rate (FCR) method to calculate...",
hmPrevLink:"cc_ptes.html",
hmNextLink:"operating_costs.html",
hmParentLink:"installation_costs.html",
hmBreadCrumbs:"<a href=\"installation_costs.html\">Installation Costs<\/a>",
hmTitlePath:"Installation Costs > Marine Energy Installation Costs",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Marine Energy Installation Costs<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">When you use the marine energy wave or marine energy tidal performance model with the LCOE calculator financial model, SAM uses a fixed-charge-rate (FCR) method to calculate the levelized cost of energy (LCOE) from inputs on the <a href=\"fin_lcoefcr.html\" class=\"topiclink\">Financial Parameters<\/a> page. The FCR method requires a total installed cost and annual operating cost value as input, along with a fixed charge rate representing the financial assumptions for the project. <\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">When you use the single owner financial model with either the wave or wave battery performance model, SAM uses an annual cash flow model to calculate financial metrics such as the net present value (NPV), internal rate of return (IRR), power purchase price (PPA), LCOE, and other metrics.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">Each Capital Costs page and the O&amp;M (operation and maintenance) Cost page provide a detailed cost structure that SAM uses to calculate the total capital cost and fixed annual operating cost for the financial model calculations:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">The total capital cost is the sum of the total device cost, total balance-of-system (BOS) cost, and total financial cost.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">The fixed operating cost is the sum of the O&amp;M costs.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">SAM\'s marine energy models are based on the U.S Department of Energy (DOE) Reference Model Project (RMP). You can find links to documents for the RMP on the Sandia National Laboratories website at <a href=\"https:\/\/energy.sandia.gov\/programs\/renewable-energy\/water-power\/projects\/reference-model-project-rmp\/\" target=\"_blank\" class=\"weblink\">https:\/\/energy.sandia.gov\/programs\/renewable-energy\/water-power\/projects\/reference-model-project-rmp\/<\/a>. Excel workbooks with cost category details are available for each technology:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Mirko Previsic (2012). <a href=\"https:\/\/energy.sandia.gov\/download\/21275\/\" class=\"weblink\">Reference Model 1 Cost Breakdown Structure for Tidal Current Device<\/a><\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Mirko Previsic (2012). <a href=\"https:\/\/energy.sandia.gov\/download\/23667\/\" class=\"weblink\">Cost Breakdown Structure for WEC Rated at 286 kW<\/a><\/span><\/p>\n\r<h4 class=\"p_Heading4\"><span class=\"f_Heading4\">Marine Energy Cost Structure<\/span><\/h4>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The cost structure has several levels, so you can specify each cost as a single value, or break it down into more detailed categories and subcategories.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Override cost structure<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">This is the highest level in the cost structure. Use this option to enter a total cost at the Device, BOS, Financial, or O&amp;M Cost level. When you check <\/span><span class=\"f_CHInterfaceElement\">Override cost structure<\/span><span class=\"f_VariableDescription\">, SAM uses the value you type and ignores the detailed cost values.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">If you choose to enter costs using more detailed categories, for each cost, click the Expand button <\/span><img alt=\"SS_Panel-expand\" style=\"margin:0;width:2.2500rem;height:2.2500rem;border:none\" src=\".\/images\/ss_panel-expand.png\"\/> to show details, and choose an option for entering cost values:<\/p>\n\r<p class=\"p_Text\" style=\"text-align: center;\"><img alt=\"ss-me-cost-options\" style=\"margin:0 auto 0 auto;width:9.6250rem;height:5.6250rem;border:none\" src=\".\/images\/ss-me-cost-options.png\"\/><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Enter in in \$\/kW<\/span><\/p>\n\r<p class=\"p_VariableDescription\">Type a cost per unit of rated capacity. The rated capacity is from the Array page. SAM calculates the category cost by multiplying the &quot;user input&quot; cost you enter in \$\/kW by the array capacity in kW from the Array page. SAM ignores the modeled value, but shows it for your reference.<\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><img alt=\"ss-me-cost-per-kw\" style=\"margin:0 auto 0 auto;width:45.7500rem;height:8.5625rem;border:none\" src=\".\/images\/ss-me-cost-per-kw.png\"\/><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Enter in \$<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Type a dollar value for the cost. SAM ignores the modeled value, but shows it for your reference.<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><img alt=\"ss-me-cost-fixed\" style=\"margin:0 auto 0 auto;width:36.1875rem;height:2.7500rem;border:none\" src=\".\/images\/ss-me-cost-fixed.png\"\/><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Use Modeled Value (\$)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Choose this option for SAM to automatically calculate the cost. The category cost is equal to the modeled value, and SAM ignores the &quot;user input&quot; value.<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><img alt=\"ss-me-cost-modeled\" style=\"margin:0 auto 0 auto;width:36.1875rem;height:2.7500rem;border:none\" src=\".\/images\/ss-me-cost-modeled.png\"\/><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Use Array Scaling<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Choose this option for SAM to scale the array cost based on the cost of a single device. The &quot;user input&quot; value is the cost of a single device in dollars. The array scaling R-value scales the single device cost as follows:<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">&nbsp;<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><img alt=\"EQ_MARINE_costscalingb\" style=\"margin:0 auto 0 auto;width:6.9375rem;height:2.3750rem;border:none\" src=\".\/images\/eq_marine_costscalingb.png\"\/><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><img alt=\"EQ_MARINE_costscalingcost\" style=\"margin:0 auto 0 auto;width:17.7500rem;height:1.3750rem;border:none\" src=\".\/images\/eq_marine_costscalingcost.png\"\/><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Where:<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-indent: 0; padding-left: 0.8125rem; margin-left: 1.5000rem;\"><span class=\"f_CHInterfaceElement\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:0.8125rem;margin-left:-0.8125rem\">&#8226;<\/span><span class=\"f_CHEquationVariable\">R<\/span><span class=\"f_VariableDescription\"> = <\/span><span class=\"f_CHInterfaceElement\">Array scaling R-value<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-indent: 0; padding-left: 0.8125rem; margin-left: 1.5000rem;\"><span class=\"f_CHInterfaceElement\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:0.8125rem;margin-left:-0.8125rem\">&#8226;<\/span><span class=\"f_CHEquationVariable\">Cost<\/span><span class=\"f_CHVariableSubscript\">device<\/span><span class=\"f_VariableDescription\"> = <\/span><span class=\"f_CHInterfaceElement\">User input<\/span><span class=\"f_VariableDescription\"> for the <\/span><span class=\"f_CHInterfaceElement\">Structural assembly<\/span><span class=\"f_VariableDescription\"> option, representing the cost of a single device in dollars<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-indent: 0; padding-left: 0.8125rem; margin-left: 1.5000rem;\"><span class=\"f_CHInterfaceElement\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:0.8125rem;margin-left:-0.8125rem\">&#8226;<\/span><span class=\"f_CHEquationVariable\">N<\/span><span class=\"f_CHVariableSubscript\">devices<\/span><span class=\"f_VariableDescription\"> = <\/span><span class=\"f_CHInterfaceElement\">Number of devices in array<\/span><span class=\"f_VariableDescription\"> from Array page<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The following screenshot is for an example with 100 devices to illustrate how the calculation works with <\/span><span class=\"f_CHEquationVariable\">R = 0.1<\/span><span class=\"f_VariableDescription\">, <\/span><span class=\"f_CHEquationVariable\">Cost<\/span><span class=\"f_CHVariableSubscript\">device<\/span><span class=\"f_CHEquationVariable\"> = 10<\/span><span class=\"f_VariableDescription\">, and <\/span><span class=\"f_CHEquationVariable\">N<\/span><span class=\"f_CHVariableSubscript\">devices<\/span><span class=\"f_CHEquationVariable\">=100<\/span><span class=\"f_VariableDescription\">:<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><span class=\"f_VariableDescription\">&nbsp;<\/span><\/p>\n\r<p style=\"text-align: center; line-height: 1.28; margin: 0 0 0.6875rem 0;\"><img alt=\"ss-me-cost-scaling\" style=\"margin:0 auto 0 auto;width:51.7500rem;height:3.4375rem;border:none\" src=\".\/images\/ss-me-cost-scaling.png\"\/><\/p>\n\r<p style=\"line-height: 1.28; margin: 0 0 0.6875rem 0;\"><span style=\"font-family: Arial,Helvetica,sans-serif; color: #000000;\">&nbsp;<\/span><\/p>\n\r<p style=\"line-height: 1.28; margin: 0 0 0.6875rem 0;\"><span style=\"font-family: Arial,Helvetica,sans-serif; color: #000000;\">&nbsp;<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note about modeled values.<\/span><span class=\"f_Note\"> For the wave energy model, if you choose a WEC from the library on the Wave Energy Converter page, the default device costs are based on the associated costs for each design based on reference model projects. Balance-of-system costs and financial costs are based on costs curves developed from existing reference model data, a database with wave and tidal cost data from DOE-funded projects, and a literature review of wave, tidal, and offshore wind energy costs. <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">If you use your own WEC parameters instead of choosing a WEC from the library, device costs are from cost curves based on array rated power or percentage of costs. These costs curves were developed from existing reference model data, a database with wave and tidal cost data from DOE-funded projects, and a literature review of wave, tidal, and offshore wind energy costs. <\/span><br \/>\n\r<span class=\"f_Note\">&nbsp;<\/span><br \/>\n\r<span class=\"f_Note\">For the tidal energy model, default device costs are based on the associated costs the Reference Model 1 tidal converter. Balance-of-system capital costs and financial costs are based on costs curves developed from existing reference model data, a database with wave and tidal cost data from DOE-funded projects, and a literature review of wave, tidal, and offshore wind energy costs.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Use Cost Breakdown (\$)<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Choose this option to enter costs at the most detailed level. When you choose the option for a cost category, SAM automatically expands the Cost Breakdown section for that category.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">You can either enter a total cost for the category, <\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><img alt=\"ss-me-cost-itemized\" style=\"margin:0 auto 0 auto;width:17.6250rem;height:4.2500rem;border:none\" src=\".\/images\/ss-me-cost-itemized.png\"\/><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">or check <\/span><span class=\"f_CHInterfaceElement\">Enter detailed costs<\/span><span class=\"f_VariableDescription\"> to enter itemized costs for each subcategory. SAM automatically calculates the total based on the costs you enter.<\/span><\/p>\n\r<p class=\"p_VariableDescription\" style=\"text-align: center;\"><img alt=\"ss-me-cost-itemized-detailed\" style=\"margin:0 auto 0 auto;width:16.5000rem;height:12.7500rem;border:none\" src=\".\/images\/ss-me-cost-itemized-detailed.png\"\/><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For information about a cost item, click the Information button next to the item\'s label.<\/span><\/p>\n\r<p class=\"p_Text\" style=\"text-align: center;\"><img alt=\"SS_MHKCosts-information\" style=\"margin:0 auto 0 auto;width:25.4375rem;height:7.1250rem;border:none\" src=\".\/images\/ss_mhkcosts-information.png\"\/><\/p>\n\r"
})
