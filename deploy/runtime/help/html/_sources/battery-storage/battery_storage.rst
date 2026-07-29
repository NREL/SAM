Battery Overview
================

SAM's battery storage models are for an electric battery of electrochemical cells that stores electricity from the grid and/or an on-site power power generation system.

The following configurations combine battery storage with a power generation system:

* Detailed PV-Battery adds battery storage to the :doc:`Detailed Photovoltaic <../detailed-photovoltaic-model/pv_overview>` model.

* Custom Generation Profile - Battery adds battery storage to the :doc:`Custom Generation Profile <../custom-generation/custom_generation>` model, which can be used to represent any type of power generating system.

* Marine Wave Battery adds battery storage to the :doc:`Marine Energy Wave <../marine-energy-wave/me_wave>` model.

The following configuration does not include a power generation system:

* Standalone battery for a grid-connected battery with no power generation equipment.

.. note:: The thermal energy storage models are for standalone storage systems that store energy as heat instead of in a chemical battery. See :doc:`Electric Thermal Energy Storage <../electric-thermal-energy-storage/etes>` and :doc:`Pumped Thermal Energy Storage <../pumped-thermal-energy-storage/ptes>`.

SAM can model behind-the-meter (BTM) and front-of-meter (FOM) storage applications, determined by the financial model:

* The distributed financial models (Residential, Commercial, and Third Party Ownership) are for behind-the-meter storage, where power from the system is used to reduce a residential or commercial customer's electricity bill.

* The PPA and Merchant Plant financial models are for front-of-meter storage, where power from the system is sold to generate revenue.

The battery storage model allows you to analyze the performance of the following types of batteries:

* Lead-acid

* Lithium-ion

* Vanadium redox flow

* All iron flow

SAM has automatic dispatch controller options for peak shaving applications or applications that respond to time-varying power prices. It also has "manual" dispatch options where you provide a schedule or time-series instructions for when to charge and discharge the battery. SAM's detailed voltage, temperature, and degradation models account for how operating conditions affect battery degradation over time to determine when battery replacements are required.

For more about the SAM's battery model, see the `Battery Publications <https://sam.nlr.gov/battery-storage/battery-publications.html>`__ page on the SAM website. 