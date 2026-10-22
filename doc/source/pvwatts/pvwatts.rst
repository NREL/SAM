PVWatts
=======

SAM's PVWatts model uses the same underlying code as NLR's online PVWatts® Calculator (https://pvwatts.nlr.gov). SAM has the following features that are not available in the online calculator:

* You can choose a weather file on the :doc:`Location and Resource <pvwatts_location_and_resource>` page. The online calculator requires you to use weather data from an internal database.

* Optional grid interconnection and curtailment inputs on the :doc:`Grid <../grid/grid_limits>` page.

* You can use PVWatts with a :doc:`financial model <../introduction/fin_overview>` to calculate metrics like net present value, payback period, and levelized cost of energy.

* When you combine PVWatts with the Residential or Commercial financial model, you can include a battery in the system design.

For a detailed descriptions of the model, see publications listed under "PVWatts" on the `PV Publications <https://sam.nlr.gov/photovoltaic/pv-publications.html>`__ page of the SAM website.

Basic steps to run PVWatts:

#. On the :doc:`Location and Resource <pvwatts_location_and_resource>` page, choose or download a weather file to represent the solar resource at the project location.

#. On the :doc:`System Design <pvwatts_system_design>` page, enter the system's DC nameplate capacity for Nameplate Capacity.

#. Choose a tracking option and specify the tilt angle.

#. If you are using PVWatts with a financial model, on the Installation costs and Operating costs pages, specify the project costs and review the inputs on the Financial Parameters and Incentives pages.

#. For the Residential and Commercial financial models, review the inputs on the Electricity Rates and Electric Load input pages. These inputs are used for monthly electricity bill calculations and to account billing options such as net metering.

#. Run a simulation and review results on the :doc:`Results page <../getting-started/results_page>` for details.

.. image:: ../images/SS_MainWindow-RunAllSimulationsButton.png
   :align: center
   :alt: SS_MainWindow-RunAllSimulationsButton.png

.. _versions:

About PVWatts Versions
~~~~~~~~~~~~~~~~~~~~~~

The current version of PVWatts is Version 8 for SAM, the online PVWatts® Calculator (https://pvwatts.nlr.gov), and the PVWatts Web API (https://developer.nlr.gov/docs/solar/pvwatts/) available from the NLR Developer Network. Differences in results between SAM's implementation of PVWatts and the online calculator and API may be caused by different weather data or by the differences between these versions.

The following lists PVWatts versions in different versions of SAM. All versions of PVWatts are available in the `SAM Software Development Kit (SDK) <https://sam.nlr.gov/software-development-kit-sdk>`__ and `PySAM <https://NLR-pysam.readthedocs.io/en/master/>`__.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - SAM Versions
     - PVWatts SSC Module Version
   * - 2021.12.02 and newer
     - *pvwattsv8*
   * - 2020.2.29
     - *pvwattsv7*
   * - 2014.11.24
     - *pvwattsv5*
   * - Older than 2014.11.24
     - *pvwattsv1*

New features added in PVWatts Version 8
---------------------------------------

For SAM 2021.12.02, we updated PVWatts to Version 8 (*pvwattsv8*) and added a collapsible panel for "advanced inputs" so we could expose additional features.

Updates to PVWatts Version 8 (*pvwattsv8*) included:

* Internal module and module thermal models based on same model as Detailed PV model.

* Cell temperature model uses the Nominal Operating Cell Temperature (NOCT) method with T_noct = 49 for the Fixed Roof Mount array type and T_noct = 45 for other array types.

* Internal inverter model based on same model as Detailed PV model.

Features now available as options under **Advanced Inputs**:

* Bifacial modules.

* Albedo input options.

* Soiling loss input as a beam irradiance loss.

Differences between PVWatts Version 5 and Version 7
---------------------------------------------------

The following list summarizes the features added in SAM 2020.2.29 when we updated from PVWatts Version 5 (*pvwattsv5*) to Version 7 (*pvwattsv7*). For the default PVWatts - No Financial configuration in SAM, the total annual energy predicted by Version 7 is 2.8% higher than Version 5.

* Self-shading applies to the fixed open rack and 1-axis tracking array types and accounts for diffuse shading. In Version 5, self-shading only applies to 1-axis tracking.

* Diffuse shading applies to the 1-axis backtracking array type. In Version 5, it only applies to the 1-axis tracking array type.

* Account for incident angle reflection losses in the module cover using the `physical IAM model <https://pvpmc.sandia.gov/modeling-guide/1-weather-design-inputs/shading-soiling-and-reflection-losses/incident-angle-reflection-losses/physical-iam-model/>`__ for incident angle modifier to account for reflection losses of both direct and diffuse irradiance.

* Update parameter values for module type (see table below).

* Apply air mass modifier to account for solar spectrum.

* Optional snow loss model, only available when weather file contains snow depth data.

The following features are available to programmers using the SAM Software Development Kit to access PVWatts Version 7 (*pvwattsv7*). These features are adapted from SAM's Detailed Photovoltaic model's CEC Module model. They are not available either in SAM's implementation of PVWatts or the online calculator:

* Stow trackers when wind speed exceeds an upper limit (to avoid wind damage).

* Bifacial modules.

* Transformer losses.

* Soiling loss input as a beam irradiance loss.

Module Type Parameters for PVWatts Versions 5, 7, and 8
-------------------------------------------------------

The following table shows the differences in module type parameters between *pvwattsv5*, *pvwattsv7*, and *pvwattsv8*.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Module Type
     - Approximate Nominal Efficiency (%)V5 / V7 / V8
     - Module Cover V5 / V7 and V8
     - Temperature Coefficient of Power (%/°C) V5 / V7 and V8
     - Fill Factor (for self-shading) V5 / V7 / V8
   * - Standard (crystalline Silicon)
     - 15 / 17 / 19
     - Glass / Anti-reflective
     - -0.47 / -0.37
     - None / 77.1% / 77.8%
   * - Premium (crystalline Silicon)
     - 19 / 20.1 / 21
     - Anti-reflective
     - -0.35 / -0.35
     - None / 80.1% / 78.0%
   * - Thin film
     - 10 / 15.6 / 18
     - Glass / Anti-reflective
     - -0.20 / -0.32
     - None / 70.6% / 77.7%