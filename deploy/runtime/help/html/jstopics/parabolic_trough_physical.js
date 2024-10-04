hmLoadTopic({
hmKeywords:"",
hmTitle:"CSP Physical Trough Model",
hmDescription:"The physical trough model calculates the electricity delivered to the grid by a parabolic trough solar field that delivers thermal energy to a power block for electricity...",
hmPrevLink:"custom_generation_profile_plant.html",
hmNextLink:"troughphysical_location_and_resource.html",
hmParentLink:"index.html",
hmBreadCrumbs:"",
hmTitlePath:"CSP Physical Trough Model",
hmHeader:"<h1 class=\"p_Heading1\" style=\"page-break-inside: avoid; page-break-after: avoid;\"><span class=\"f_Heading1\">CSP Physical Trough Model<\/span><\/h1>\n\r",
hmBody:"<p class=\"p_Text\"><span class=\"f_Text\">The physical trough model calculates the electricity delivered to the grid by a parabolic trough solar field that delivers thermal energy to a power block for electricity generation, with an optional thermal energy storage system. The physical trough model characterizes many of the system components from first principles of heat transfer and thermodynamics, rather than from empirical measurements as in the empirical trough system model. While the physical trough model is more flexible than the <a href=\"parabolic_trough_empirical.html\" class=\"topiclink\">empirical trough<\/a> model, it adds more uncertainty to performance predictions than the empirical model.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">For more information about the model, including a demonstration video and technical reference manuals, see the <a href=\"https:\/\/sam.nrel.gov\/concentrating-solar-power.html\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\/concentrating-solar-power.html\">SAM website<\/a>.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">A parabolic trough system is a type of concentrating solar power (CSP) system that collects direct normal solar radiation and converts it to thermal energy that runs a power block to generate electricity. The components of a parabolic trough system are the solar field, power block, and in some cases, thermal energy storage and fossil backup systems. The solar field collects heat from the sun and consists of parabolic, trough-shaped solar collectors that focus direct normal solar radiation onto tubular receivers. Each collector assembly consists of mirrors and a structure that supports the mirrors and receivers, allows it to track the sun on one axis, and can withstand wind-induced forces. Each receiver consists of a metal tube with a solar radiation absorbing surface in a vacuum inside a coated glass tube. A heat transfer fluid (HTF) transports heat from the solar field to the power block (also called power cycle) and other components of the system. The power block is based on conventional power cycle technology, using a turbine to convert thermal energy from the solar field to electric energy. The optional fossil-fuel backup system delivers supplemental heat to the HTF during times when there is insufficient solar energy to drive the power block at its rated capacity.<\/span><\/p>\n\r<p class=\"p_Text\" style=\"text-align: center;\"><img alt=\"IMG_TroughPhysical-diagram\" style=\"margin:0 auto 0 auto;width:44.6875rem;height:15.6875rem;border:none\" src=\".\/images\/img_troughphysical-diagram.png\"\/><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The physical trough system model approaches the task of characterizing the performance of the many of the system components from first principles of heat transfer and thermodynamics, rather than from empirical measurements as in the empirical trough model. The physical model uses mathematical models that represent component geometry and energy transfer properties, which gives you the flexibility to specify characteristics of system components such as the absorber emissivity or envelope glass thickness. The empirical model, on the other hand, uses a set of curve-fit equations derived from regression analysis of data measured from real systems, so you are limited to modeling systems composed of components for which there is measured data. While the physical model is more flexible than the empirical model, it adds more uncertainty to performance predictions than the empirical model. In a physical model, uncertainty in the geometry and property assumptions for each system component results in an aggregated uncertainty at the system level that tends to be higher than the uncertainty in an empirical model. We\'ve included both models in SAM so that you can use both in your analyses.<\/span><\/p>\n\r<p class=\"p_Text\"><span class=\"f_Text\">The following are some key features of the physical model:<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Includes transient effects related to the thermal capacity of the heat transfer fluid in the solar field piping, headers, and balance of plant.<\/span><\/p>\n\r<p class=\"p_List\" style=\"text-indent: 0; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_List\">Allows for flexible specification of solar field components, including multiple receiver and collector types within a single loop.<\/span><\/p>\n\r<p class=\"p_ListLast\" style=\"text-indent: 0; page-break-inside: avoid; padding-left: 1.5000rem; margin-left: 1.5000rem;\"><span class=\"f_List\" style=\"font-family: Arial,\'Lucida Sans Unicode\',\'Lucida Grande\',\'Lucida Sans\';display:inline-block;width:1.5000rem;margin-left:-1.5000rem\">&#8226;<\/span><span class=\"f_ListLast\">Relatively short simulation times to allow for <a href=\"parametrics.html\" class=\"topiclink\">parametric<\/a> and <a href=\"stochastic.html\" class=\"topiclink\">statistical<\/a> analyses that require multiple simulation runs.<\/span><\/p>\n\r<p class=\"p_Note\"><span class=\"f_CHNote\">Note.<\/span><span class=\"f_Text\"> <\/span><span class=\"f_Note\">In versions of SAM released after February 2020, fossil backup is not available for the Physical Trough model because it was not incorporated into the new dispatch controller logic at the time of the software release. If you want to use fossil backup, use version SAM 2018.11.11, available on the SAM website <a href=\"https:\/\/sam.nrel.gov\/download\" target=\"_blank\" class=\"weblink\" title=\"https:\/\/sam.nrel.gov\/download\">Download page<\/a>.<\/span><\/p>\n\r"
})
