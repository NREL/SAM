
The land area inputs determine the total land area in acres that you can use to estimate land-related costs in $/acres on the :doc:`Installation Costs <../installation-costs/cc_trough>` and :doc:`Operating Costs <../operating-costs/oc_operating>` pages.

**Solar field area, acres**
  The actual aperture area converted from square meters to acres:

*Solar Field Area (acres) = Actual Aperture (m²) × Row Spacing (m) / Maximum SCA Width (m) × 0.0002471 (acres/m²)*

  The maximum SCA width is the aperture width of SCA with the widest aperture in the field, as specified in the loop configuration and on the :doc:`Collectors (SCA) <../csp-physical-trough-model/troughphysical_collectors_scas>`   page.

**Non-solar field land area multiplier**
  Land area required for the system excluding the solar field land area, expressed as a fraction of the solar field aperture area. A value of one would result in a total land area equal to the total aperture area.

**Total land area, acres**
  Land area required for the entire system including the solar field land area

*Total Land Area (acres) = Solar Field Area (acres) × (1 + Non-Solar Field Land Area Multiplier)*

  The land area appears on the Installation Costs page, where you can specify land costs in dollars per acre.