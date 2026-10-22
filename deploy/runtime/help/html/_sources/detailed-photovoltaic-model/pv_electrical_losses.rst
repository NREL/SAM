Electrical Losses
=================

Electrical losses account for losses that the module and inverter models do not account for. After running a simulation, you can see the effect of these losses on  :doc:`results <../getting-started/results_page>` by displaying the results variables described below in :doc:`data tables <../results/data>` and :doc:`time series graphs <../results/timeseries>`.

Subarrays
~~~~~~~~~

To enable and disable subarrays, use the check boxes under "Subarrays" on the :doc:`pv_system_size` page.

.. _dclosses:

DC Losses
~~~~~~~~~

The DC losses account for DC electrical losses on the DC side of the system that the module model does not calculate, such as electrical losses in the DC wiring that connects modules in the array.

The five DC loss categories (mismatch, diodes and connections, etc) are to help you keep track of factors influencing the total DC loss. You can see the effect of the total DC loss in the hourly results:

*Net DC array power = Subarray 1 Gross DC power × ( 1 - Subarray 1 Total DC power loss ÷ 100% )*
 *+ Subarray 2 Gross DC power × ( 1 - Subarray 2 Total DC power loss ÷ 100% )*
 *+ Subarray 3 Gross DC power × ( 1 - Subarray 3 Total DC power loss ÷ 100% )*
 *+ Subarray 4 Gross DC power × ( 1 - Subarray 4 Total DC power loss ÷ 100% )*

The five DC loss categories represent the following sources of DC electrical loss for each subarray:

**Module mismatch, %**
  Slight differences in performance of individual modules in the array.

**Diodes and connections, %**
  Voltage drops across blocking diodes and electrical connections.

**DC wiring, %**
  Resistive losses in wiring on the DC side of the system.

**Tracking error, %**
  Inaccuracies in the tracking mechanisms ability to keep the array oriented toward the sun. The default value of zero assumes a fixed array with no tracking. Applies only to systems with one- or two-axis tracking arrays.

**Nameplate, %**
  Accounts for accuracy of the manufacturer's nameplate rating, often for the performance degradation modules may experience after being exposed to light. SAM allows a negative nameplate rating of up to -5% to account for modules that exceed their nameplate rating.

**Bifacial electrical mismatch, %**
  For bifacial modules, accounts for electrical mismatch between modules in the array caused by variation of irradiance on the the rear-side of the array.

  Click **Calculate** to automatically calculate the bifacial electrical mismatch based on the method described in Deline, C.; Silvana Ayala P.; Sara M.; Carlos O. (2020) `Estimating and parameterizing mismatch power loss in bifacial photovoltaic systems <https://www.osti.gov/servlets/purl/1606308>`__. *Progress in Photovoltaics: Research and Applications* 28, no. 7 (2020): 691-703.

**DC power optimizer loss, %**
  Accounts for power losses of any power conditioning equipment installed with the array. SAM does not explicitly model DC/DC conversion losses, but you can account for them here.

The total DC power loss for each subarray represents the subarray's total DC electrical loss:

**Total DC power loss, %**
  The total DC power loss is the total loss that applies to each subarray:

  *Total DC power loss = 100% × { 1 - [ ( 1 - Mismatch ÷ 100% ) × ( 1 - Diodes and connections ÷ 100% ) × ( 1 - DC wiring ÷ 100% ) × ( 1 - Tracking error ÷ 100% ) × ( 1 - Nameplate ÷ 100% ) × ( 1 - Bifacial electrical mismatch ÷ 100% ) × ( 1 - DC power optimizer loss ÷ 100% )] }*

Default DC Losses
-----------------

If you are not sure what values to use for the DC loss percentages, you can apply default losses appropriate for your system design.

.. note:: When you click one of the default loss buttons, SAM replaces the values of all six DC loss categories with the default values shown in the table below below. After applying the default DC losses, you should change the tracking error and nameplate loss percentages as needed because those two losses are not affected by the type of inverter or presence of DC optimizers in the system.

**Central Inverters**
  Each string in the array is connected to an inverter.

**Microinverters**
  Each module is connected to its own :ref:`microinverter <microinverters>`  .

**DC optimizers**
  Power electronics minimize array losses.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - 
     - Central inverters
     - Microinverters
     - DC optimizers
   * - Module mismatch
     - 2
     - 0
     - 0
   * - Diodes and connections
     - 0.5
     - 0.5
     - 0.5
   * - DC wiring
     - 2
     - 2
     - 2
   * - Tracking error
     - 0
     - 0
     - 0
   * - Nameplate
     - 0
     - 0
     - 0
   * - DC power optimizer loss
     - 0
     - 0
     - 1

.. _aclosses:

AC Losses
~~~~~~~~~

The AC losses account for electrical wiring losses on the AC side of the system that the inverter model does not account for. During simulations, SAM uses the total AC loss to reduce the inverter AC electric output calculated by the inverter model.

.. note:: For the PV Battery model, battery dispatch is affected by AC losses.

**AC wiring**
  Loss to account for electrical losses in AC wiring between the inverter and the grid connection point.

  For PV-Battery systems and AC-connected batteries, the AC Wiring loss is not applied, so should be included in the DC to AC conversion efficiency on the Battery Cell and System page. The AC wiring loss is applied for DC-connected batteries.

Subhourly Clipping Loss Correction
----------------------------------

Subhourly clipping loss correction accounts for inverter power limiting that may occur within the hour. For a real system, on partly cloudy days, there may be several times during an hour when the instantaneous array DC power is greater than the inverter's rated nameplate power and the inverter limits its output power to the rated power. Without subhourly clipping correction, SAM's power output value may overestimate the actual output of the system. 

Subhourly clipping loss correction is only available for hourly simulations. The time step of weather file on the Location and Resource page determines the simulation time step: SAM runs hourly simulations when the weather file contains hourly data, and subhourly simulations when the weather file contains subhourly data (data in 1, 5, 10, 15, or 30-minute time steps).

**Enable subhourly clipping loss correction**
  Choose this option to enable subhourly clipping loss correction. The option is only available for hourly simulations.

.. note:: If you change the weather file on the Location and Resource page to one with subhourly time steps, SAM automatically clears and disables the check box.

**Matrix lookup method**
  The matrix lookup method uses an empirical matrix-based model that uses clipping potential and clearness index to estimate Average-then-Clip (AtC) bias.

* For a Python implementation of this method and a list of technical references, see Allen, J.; Dhakal, R.; Hobbs, W.; Li, W. (2024). `Allen Method for PV Subhourly Clipping Correction <https://github.com/epri-dev/allen-subhourly-clipping-correction>`__  .

**Distribution of PV output method**
  The distribution of PV Output method uses a distribution function of power output to estimate the power limiting loss.

* Walker, A.; Desai, J.; McDonald, B. (2020) Solar Photovoltaic Systems Time-Series Simulation: Subinterval Distribution vs. Steady-State Assumption. ASME 14th International Conference on Energy Sustainability. (`PDF 1.8 MB <https://www.nlr.gov/docs/fy20osti/76859.pdf>`__  )

Transformer Losses
------------------

Use transformer losses to calculate electrical losses from a distribution or substation transformer in a large photovoltaic system.

SAM assumes that the transformer capacity is equal to the total AC capacity from the :doc:`pv_system_size` page. To see the effect of these losses on the inputs, see the **Transformer load loss**, **Transformer no load loss**, and **Transformer total loss** outputs on the :doc:`Results <../getting-started/results_page>` page.

.. note:: For PV-Battery systems with AC-connected batteries, SAM does not apply the transformer losses, so you should account for these losses using the DC to AC conversion efficiency on the Battery Cell and System page. The transformer losses are applied for DC-connected batteries.

**Transformer no load loss**
  The transformer's rated no load loss as a percentage of the total inverter AC capacity. This is a constant loss caused by the magnetizing current in the transformer's core.

**Transformer load loss**
  The transformer's rated load loss as a percentage of the inverter's AC output. These represent losses in the transformer's primary and secondary coil wiring that vary with the inverter's electrical output.

Transmission Losses
-------------------

Use transmission losses to calculate electrical losses from transmission lines in a large photovoltaic system.

**Transmission losses**
  A reduction of the photovoltaic system's AC output due to wire losses in a transmission line.

.. _pvavailability:

System Availability
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_system_availability.rst