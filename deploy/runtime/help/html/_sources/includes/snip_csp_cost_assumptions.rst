
The default values the Installation Costs and Operating Costs pages for the CSP models reflect the National Laboratory of the Rockies best estimate of representative costs for CSP systems in the United States at the time of the release of the current version of SAM. The values are based on cost studies undertaken by NLR, review of published literature, and conversations with developers and industry insiders. Costs are reviewed prior to each new SAM release.

.. note:: Always review all of the inputs for your SAM project to determine whether they are appropriate for your analysis.

The parabolic trough and power tower default cost values were largely developed from the following studies:

Kurup, P.; Turchi, C. S.  (2015). “Parabolic Trough Collector Cost Update for the System Advisor Model (SAM),” NREL/TP-6A20-65228, 2015. (`PDF 2.1 MB <https://docs.nlr.gov/docs/fy16osti/65228.pdf>`__)

Turchi, C. S.;  Heath, G. A. (2013). “Molten Salt Power Tower Cost Model for the System Advisor Model (SAM),” NREL/TP-5500-57625, 2013. (`PDF 2.5 MB <https://docs.nlr.gov/docs/fy13osti/57625.pdf>`__)

Kolb, G. J.; Ho, C. K. (2011), T. R. Mancini, and J. A. Gary, "Power Tower Technology Roadmap and Cost Reduction Plan," SAND2011-2419. (`PDF 503 KB <http://prod.sandia.gov/techlib/access-control.cgi/2011/112419.pdf>`__)

Turchi, C. S. (2010). "Parabolic Trough Reference Plant for Cost Modeling with the Solar Advisor Model (SAM)," NREL/TP-550-47605. (`PDF 7.2 MB <https://docs.nlr.gov/docs/fy10osti/47605.pdf>`__)

These studies differed in some important assumptions. The differences in the location and cooling method assumptions for the default cases of the CSP technologies are outlined in the following table. The choice of location affects solar resource and the assumed labor costs associated with the case. Construction labor rates in Arizona tend to be lower than in California, which reduces installed costs. For the Winter 2016 version of SAM, all CSP systems are assumed to use dry cooling. SAM can model systems with dry cooling, wet cooling, or a hybrid of the two. If you change the type of cooling system on the Power Cycle input page, you should also change other parameters as appropriate because SAM does not make those changes automatically. For example, different types of cooling systems would require different power cycle costs and design-point performance parameters. Perhaps less obvious, the site preparation costs are lower for dry-cooled (air-cooled) systems than for wet-cooled (evaporative) systems because of the elimination of the large blowdown evaporation ponds required for wet systems.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Technology
     - Default Location
     - Default Cooling method
   * - Physical Trough
     - SW Arizona (Tucson weather file)
     - Dry
   * - Empirical Trough
     - SW Arizona (Tucson weather file)
     - Dry
   * - Molten Salt Power Tower
     - Southern California (Daggett weather file)
     - Dry
   * - Linear Fresnel
     - SW Arizona (Tucson weather file)
     - Dry
   * - Generic Solar
     - SW Arizona (Tucson weather file)
     - Dry

The trough, tower, and linear Fresnel models assume the “balance of plant” cost category is composed of the steam generation system (Kolb, 2011). This choice is made to allow users to highlight the effect of a direct steam generation (DSG) design. In a DSG design, the balance of plant cost category is zero because steam generation occurs in the solar receiver.

The lesser commercial activity in linear Fresnel systems makes cost values for those technologies more uncertain than for troughs and towers. Linear Fresnel costs are estimated based on discussions with developers and researchers.