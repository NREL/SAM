Getting Started with the Detailed Photovoltaic Model
====================================================

The following overview is to help you get started modeling a photovoltaic system with the detailed photovoltaic model. For a description of the model, see :doc:`Performance Models <../introduction/technology_options>`.

.. note:: If you only have basic information about the photovoltaic system's design like the system size and orientation, you can use the :doc:`../pvwatts/pvwatts` model instead of the Detailed Photovoltaic model. PVWatts does not require detailed information about the module, inverter, and array layout.

For a complete technical description of SAM's photovoltaic model, see Gilman, P. (2015). SAM Photovoltaic Model Technical Reference. National Renewable Energy Laboratory. 59 pp.; NREL/TP-6A20-64102. (`PDF 840 KB <https://docs.nlr.gov/docs/fy15osti/64102.pdf>`__)

Follow the steps below to get started using the detailed photovoltaic model. If you are unsure of an input, use its default value. After running simulations, exploring results, and collecting more information about your project, you can refine the inputs.

**1. Choose a weather file**

* On the :doc:`Location and Resource <pv_location_and_resource>` page, choose or download a weather file to represent the solar resource at the project location.

**2. Specify the system's characteristics**

#. On the :doc:`Module <pv_module>` page, choose a model option and module.

   For most applications, use the CEC Performance Model with Module Database model. If your module is not in the list, use the CEC Performance Model with User Specifications.

#. On the :doc:`Inverter <pv_inverter>` page, choose a model option and inverter. 

   Use the Inverter CEC Database option. For an inverter that is not in the list, if you have the manufacturer's data sheet, choose the Inverter Datasheet model.

#. On the :doc:`pv_system_size` page, specify the system's size and array tracking options.

   Use **Specify desired size and DC/AC ratio** for preliminary analysis, and then use **Specify number of modules and inverters** to refine the system design.

#. To account for soiling, shading, or snow losses, use the :doc:`pv_soiling_shading_snow` page. By default, SAM assumes no shading or snow losses, and 5% soiling losses.

#. Account for DC power losses, AC power losses, and losses due to system availability requirements on the :doc:`pv_electrical_losses` page. You can use the default values if you do not have detailed information about those losses for your system.

**3. Specify the project costs**

If you are using the detailed photovoltaic model with a financial model, specify capital costs on the :doc:`Installation costs <../installation-costs/cc_pv>` page, and operation and maintenance costs on the :doc:`Operating costs <../operating-costs/oc_operating>` pages.

**4. Review financial assumptions**

The inputs on the remaining pages are inputs to the financial model. For your first simulation runs as you learn to use SAM, use the default values and review results to see how the model works. The default values are reasonable for a typical project in the United States. As you become familiar with the model, you can modify the financial inputs as appropriate for your analysis.

**4. Run a simulation and review results**

* Click **Simulate**, and review :doc:`results <pv_results>` on the :doc:`Results Page <../getting-started/results_page>`.

.. image:: ../images/SS_MainWindow-RunAllSimulationsButton.png
   :align: center
   :alt: SS_MainWindow-RunAllSimulationsButton.png
