Marine Energy Tidal
===================

The marine energy tidal model is for a system that uses a tidal energy converter (TEC) to convert the energy of ocean tides into electricity. The model requires the following inputs:

* A table of tidal current flow versus column depth to characterize the :doc:`tidal resource <me_tidal_resource>`.

* A table of electrical power versus tidal current flow :doc:`tidal energy converter <me_tidal_converter>`.

* A table of x-y positions if the system is an :doc:`array <me_tidal_array>` of multiple TECs.

* Data specifying expected :doc:`losses <me_tidal_losses>` due to array spacing, resource prediction uncertainty, electrical transmission, and other sources.

* :doc:`Installation <../installation-costs/cc_marine_energy>` and :doc:`operating <../operating-costs/oc_marine_energy>` costs for levelized cost of energy (LCOE) calculations (optional).

* :doc:`Financial parameters <../financial-parameters/fin_lcoefcr>` for LCOE calculations (optional).

SAM's marine energy tidal performance model is coupled with the LCOE Calculator to facilitate calculating the levelized cost of energy (LCOE) using an approach consistent with the method discussed in LaBonte, A. et al. (2013).

Marine Energy References
------------------------

* Reference Model 1 of the U.S. DOE Water Power `Reference Model Project (RMP) <https://energy.sandia.gov/programs/renewable-energy/water-power/projects/reference-model-project-rmp/>`__.

* Dallman, A.; Neary, V. (2014) Characterization of U.S. Wave Energy Converter (WEC) Test Sites: A Catalogue of Met-Ocean Data. 125 pp.; SAND2014-18206. Sandia National Laboratories. (`PDF 8 MB <https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2014/1418206.pdf>`__)

* LaBonte, A. et al. (2013). `Standardized cost and performance reporting for marine and hydrokinetic technologies <https://www.researchgate.net/publication/299410672_STANDARDIZED_COST_AND_PERFORMANCE_REPORTING_FOR_MARINE_AND_HYDROKINETIC_TECHNOLOGIES>`__. 11 pp. Proceedings of 1st Marine Energy Technology Symposium, Washington DC.

For information about sources of wave energy resource data, see the following websites:

* `Water Power Technologies Office Wave Hindcast Dataset <https://www.nlr.gov/water/wave-hindcast-dataset>`__
