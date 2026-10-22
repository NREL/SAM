Marine Energy Wave
==================

The marine energy wave model is for a system that uses a wave energy converter (WEC) to convert the energy of ocean waves into electricity. The model requires the following inputs:

* A table of wave height versus wave energy period to characterize the :doc:`wave resource <me_wave_resource>`.

* Performance parameters for the :doc:`wave energy converter <me_wave_converter>`.

* A table of x-y positions if the system is an :doc:`array <me_wave_array>` of multiple WECs.

* Data specifying expected :doc:`losses <me_wave_losses>` due to array spacing, resource prediction uncertainty, electrical transmission, and other sources.

* :doc:`Installation <../installation-costs/cc_marine_energy>` and :doc:`operating <../operating-costs/oc_marine_energy>` costs for levelized cost of energy (LCOE) calculations (optional).

* :doc:`Financial parameters <../financial-parameters/fin_lcoefcr>` for LCOE calculations (optional).

SAM's built-in libraries provide default data for sample systems and locations. You can run the model with this default data or provide your own. The :ref:`wave resource library <wavelibrary>` includes resource data for a few locations in the United States:

* California Central Coast

* California Humboldt Bay

* Oregon

* North Carolina

* Hawaii

The :ref:`WEC library <weclibrary>` includes parameters for three types of WECs:

* Heaving buoy

* Oscillating surge weave converter

* Backward bent duct buoy floating oscillating column

The WECs in the library are based on open-source wave energy point designs as part of the Reference Model Project (RMP) sponsored by the U.S. Department of Energy (DOE).

SAM's marine energy wave performance model is coupled with the LCOE Calculator to facilitate calculating the levelized cost of energy (LCOE) using an approach consistent with the method discussed in LaBonte, A. et al. (2013).

Marine Energy References
------------------------

* Reference Model 3 of the U.S. DOE Water Power `Reference Model Project (RMP) <https://energy.sandia.gov/programs/renewable-energy/water-power/projects/reference-model-project-rmp/>`__.

* Dallman, A.; Neary, V. (2014) Characterization of U.S. Wave Energy Converter (WEC) Test Sites: A Catalogue of Met-Ocean Data. 125 pp.; SAND2014-18206. Sandia National Laboratories. (`PDF 8 MB <https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2014/1418206.pdf>`__)

* LaBonte, A. et al. (2013). `Standardized cost and performance reporting for marine and hydrokinetic technologies <https://www.researchgate.net/publication/299410672_STANDARDIZED_COST_AND_PERFORMANCE_REPORTING_FOR_MARINE_AND_HYDROKINETIC_TECHNOLOGIES>`__. 11 pp. Proceedings of 1st Marine Energy Technology Symposium, Washington DC.

For information about sources of wave energy resource data, see the following websites:

* `OpenEI NLR Wave Energy Assessment for the United States and Puerto Rico <https://openei.org/doe-opendata/dataset/nrel-wave-energy-assessment-for-the-us>`__
* `Water Power Technologies Office Wave Hindcast Dataset <https://www.nlr.gov/water/wave-hindcast-dataset>`__
