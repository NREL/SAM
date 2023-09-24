hmLoadTopic({
hmKeywords:"",
hmTitle:"Thermal Storage",
hmDescription:"The parameters on the Thermal Storage page describe the properties thermal energy storage system. Dispatch controls are on the System Control page.",
hmPrevLink:"",
hmNextLink:"",
hmParentLink:"3dshad-reference.html",
hmBreadCrumbs:"",
hmTitlePath:"3D Shade Calculator > Reference > File Types",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">Thermal Storage<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The parameters on the Thermal Storage page describe the properties thermal energy storage system. Dispatch controls are on the System Control page.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The power tower storage model requires that the heat transfer fluid volume, tank loss coefficients, and tank temperatures be specified. SAM calculates the storage tank geometry to ensure that the storage system can supply energy to the power block at its design thermal input capacity for the number of hours specified by the Full Load TS Hours variable.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note. <\/span><span class=\"f_Note\">Because the storage capacity is not tied to the solar multiple on the Heliostat Field page, be careful to choose a storage capacity that is reasonable given the system\'s thermal capacity. Mismatched storage and solar thermal capacities will result in high levelized cost of energy (LCOE) values.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">System Design Parameters<\/span><\/h3>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The system design parameters are from the System Design page, where you can define the design-point parameters of the entire power tower system.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Cycle thermal power, MWt<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The thermal power required at the power cycle inlet for it to operate at its design point.<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">Cycle Thermal Power (MWt) = Design Turbine Gross Output (MWe) ÷ Cycle Thermal Efficiency<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Hours of storage at power cycle full load, hours<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The nominal thermal storage capacity expressed in hours at full load: The number of hours that the storage system can supply energy at the cycle\'s design point. <\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">SAM displays the equivalent storage capacity in MWht on the Installation Costs page.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">HTF hot temperature, °C<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The &nbsp;temperature of the hot heat transfer fluid at the receiver outlet when the power cycle operates at its design point.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">HTF cold temperature, °C<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The temperature of the cold heat transfer fluid at the receiver inlet when the power cycle operates at its design point.<\/span><\/p>\n\r<h3 class=\"p_Heading3_atocs_\" style=\"page-break-inside: avoid; page-break-after: avoid; border-top: none; border-right: none; border-left: none;\"><span class=\"f_Heading3_atocs_\">Storage System<\/span><\/h3>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Storage Type<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">SAM can model only two-tank storage systems for power towers with separate hot and cold storage tanks.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">TES thermal capacity<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The nominal thermal capacity of the storage system.<\/span><\/p>\n\r<p class=\"p_EquationExcel\"><span class=\"f_EquationExcel\">TES Thermal Capacity = Hours of Storage at Power Cycle Full Load × Cycle Thermal Input Power at Design<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Available HTF volume, m³<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The total volume of storage fluid in both storage tanks.<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">SAM calculates the total heat transfer fluid volume in storage based on the storage hours at full load and the power block design turbine thermal input capacity. The total heat transfer fluid volume is divided among the total number of tanks so that all hot tanks contain the same volume of fluid, and all cold tanks contain the same volume of fluid.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Tank height, m <\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The height of the cylinder-shaped heat transfer fluid volume in each tank. SAM calculates the height based on the diameter and storage volume of a single tank.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Tank fluid minimum height, m<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The minimum allowable height of fluid in the storage tank(s). The mechanical limits of the tank determine this value.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Parallel tank pairs<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The number of parallel hot-cold storage tank pairs. Increasing the number of tank-pairs also increases the volume of the heat transfer fluid exposed to the tank surface, which increases the total tank thermal losses. SAM divides the total heat transfer fluid volume among all of the tanks, and assumes that each hot tank contains an equal volume of fluid, and each cold tank contains and equal volume.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Tank diameter, m<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The diameter of the cylinder-shaped heat transfer fluid volume in each storage tank.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Wetted loss coefficient, W\/m²\/K<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The thermal loss coefficient that applies to the portion of the storage tank holding the storage heat transfer fluid.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Estimated heat loss, MWt<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">Heat loss from the storage system at the design-point.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Initial Hot HTF Percent, %<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The fraction of the storage heat transfer fluid in the hot storage tank at the beginning of the simulation.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Cold tank heater temperature set point, °C<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The minimum allowed cold tank temperature. Whenever the heat transfer fluid temperature in storage drops below the set-point value, the system adds sufficient thermal energy from an electric tank heater to storage to reach the set-point.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Cold tank heater capacity, MWe<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The maximum electric load of the cold tank electric heater.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Hot tank heater temperature set point, °C<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The minimum allowed hot tank temperature. Whenever the heat transfer fluid temperature in storage drops below the set-point value, the system adds sufficient thermal energy from an electric tank heater to storage to reach the set-point.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Hot tank heater capacity, MWe<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The maximum electric load of the hot tank electric heater.<\/span><\/p>\n\r<p class=\"p_VariableName\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_VariableName\">Tank heater efficiency<\/span><\/p>\n\r<p class=\"p_VariableDescription\"><span class=\"f_VariableDescription\">The electric-to-thermal conversion efficiency of the hot tank and cold tank heaters.<\/span><\/p>\n\r"
})
