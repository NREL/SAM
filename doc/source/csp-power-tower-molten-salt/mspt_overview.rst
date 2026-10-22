CSP Power Tower Molten Salt
===========================

A power tower system (also called a central receiver system) is a type of concentrating solar power (CSP) system that consists of a heliostat field, tower and receiver, power block, and optional thermal energy storage system. The field of flat, sun-tracking mirrors called heliostats focus direct normal solar radiation onto a receiver at the top of the tower, where a heat-transfer fluid is heated and pumped to the power block. The power block generates steam that drives a conventional steam turbine and generator to convert the thermal energy to electricity.

The code for SAM's power tower performance model, including the Rankine cycle model, is adapted from the model developed at the University of Wisconsin and described in *Simulation and Predictive Performance Modeling of Utility-Scale Central Receiver System Power Plants*, Wagner (2008) (`ZIP 32 MB <http://digital.library.wisc.edu/1793/45001>`__).

The solar field optimization algorithm uses `SolarPILOT™ <https://www.nlr.gov/csp/solarpilot.html>`__ to generate the heliostat field layout and characterize optical performance.

For additional technical documentation, see the `SAM website <https://sam.nlr.gov/concentrating-solar-power/csp-publications>`__.

.. note:: The supercritical Carbon dioxide (sCO2) option is not available on the Power Cycle page for versions later than SAM 2020.2.29. It is replaced by the **sCO2 Cycle Integration** :doc:`macro <../reference/macros>` with the` NLR Supercritical Carbon Dioxide (sCO2) Python model <https://github.com/NatLabRockies/SAM/tree/develop/samples/CSP/sco2_analysis_python_V2>`__.

   Thermocline storage and fossil backup are not available for versions later than SAM 2015.6.30 because they were not incorporated into the new dispatch controller logic at the time of the software release.

   If you want to use these features, you can use download a legacy version of SAM from the website `Download page <https://sam.nlr.gov/download>`__.

