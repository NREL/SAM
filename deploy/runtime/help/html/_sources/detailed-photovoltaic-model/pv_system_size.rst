System Size
===========

The System Size inputs determine the size and electrical layout of the photovoltaic system and array orientation and tracking. The array may consist of up to four subarrays, each of which may have different string lengths, orientation, and tracking.

For more about sizing the system, see :ref:`PV Sizing and Configuration <pv-sizing>`.

Basic Sizing Steps
~~~~~~~~~~~~~~~~~~

If you just want to quickly size the system for an array with a single subarray, follow these steps:

#. Choose a :doc:`module <pv_module>` and :doc:`inverter <pv_inverter>`.

#. Choose **Specify desired size and DC/AC ratio**.

#. Type a value for **Desired array size in DC kW**. For example, type 5 for a 5 kWdc array, 500 for a 500 kWdc array, or 10,000 for a 10 MWdc array. There is no limit to the array size.

#. Type a value for **Desired DC to AC ratio**, or use the default value. SAM automatically calcualtes the total inverter capacity. For example, given a 454 kWac inverter, for a 227 kWdc array with a DC to AC ratio of 1.2, the number of inverters is 2: *500 kWdc ÷ 227 kWdc = 2.2*.

#. If your system includes battery storage, configure the battery bank on the Battery Cell and System page.

See :ref:`pv-sizing` for configuration examples and detailed sizing instructions.

.. admonition:: Sizing Considerations and Limitations

   SAM can only model systems with one type of module and one type of inverter.

   Choosing an appropriate module and inverter for your system depends on many factors, some of which are outside of the scope of SAM. Finding the right combination of inverter and module to model for your system in SAM will probably require some trial and error and iteration.

   Reference conditions depend on the conditions used to define the parameters on the :doc:`Module <pv_module>` page. For the Sandia and CEC module models, reference conditions are at Standard Test Conditions (STC), which is defined as 1,000 W/m² incident radiation and 25ºC cell temperature.

   You can run the **System Sizing** :doc:`macro <../reference/macros>` to generate a report to help you ensure the system is sized correctly.

System Size
~~~~~~~~~~~

The System Size inputs include options and high-level parameters for sizing the system.

**Specify number of modules and inverters**
  Use this option to size the system manually, or to size a system with more than one subarray.

**Specify desired size and DC/AC ratio**
  This option only works for systems with a single subarray. Use this option to have SAM automatically determine the number of modules per string, strings in parallel, and number of inverters.
  
  SAM calculates the number of modules and inverters to get as close as possible to the desired size. Use this option for a rough estimate of an array layout.
  
For detailed sizing instructions see :ref:`PV Sizing and Configuration <pv-sizing>`  .

.. note:: The desired array size and DC to AC ratio is likely to be different from the actual nameplate capacity and DC to AC ratio because the desired array size is typically not a multiple of the module capacity.

**Set row dimensions when modules per string changes**
  Check this box if you want SAM to automatically change the row dimensions inputs on the :doc:`pv_tracking_layout_land` page when **Modules per string in subarray** changes. 

  If the box is checked, on the Tracking Layout Land page, SAM sets **Number of modules along bottom of row** equal to **Modules per string in subarray** and sets **Number of modules along side of row** to one. This ensures that the number of rows is an integer for self shading, snow loss, and bifacial model calculations. These variables are always enabled, so you can change the row dimensions even when **Set row dimensions when modules per string changes** is checked.

**Desired array size, kWdc**
  The array capacity in DC kilowatts that you want SAM to use for automatic sizing. This is only enabled when you choose **Specify desired size and DC/AC ratio**.

**Desired DC to AC ratio** 
  The DC to AC ratio for automatic sizing. This is only enabled when you choose **Specify desired size and DC/AC ratio**.

**Number of inverters**
  The total number of inverters in the system. The number of inverters determines the total AC capacity.

  SAM assumes that all inverters operate at the same voltage. For systems with multiple inverters, the inverters are connected in parallel so that the inverter voltage ratings are the same as those of a single inverter.

.. note:: If you are modeling a system with microinverters, see :ref:`Modeling Microinverters <microinverters>` for instructions. For systems with multiple power point trackers, see :ref:`Systems with Multiple Power Point Trackers <multiplemppt>`.

**Nameplate DC capacity, kWdc**
  The maximum DC power output of the array at the reference conditions shown on the :doc:`Module <pv_module>` page:

  *Nameplate Capacity (kWdc) = Module Maximum Power (Wdc) × 0.001 (kW/W) × Number of Modules*

  The module's maximum power rating is from the :doc:`pv_module` page..

**DC to AC ratio**
  The ratio of total inverter DC capacity to total AC capacity. This is a value that SAM calculates and displays for reference.

  High DC to AC ratios may result in inverter power clipping.

**Total AC capacity, kWac**
  The total inverter capacity in AC kilowatts:

  *Total AC Capacity (kWac) = Inverter Maximum AC Power (Wac) × 0.001 (kW/W) × Number of Inverters*

  The inverter's maximum AC power rating is from the :doc:`pv_inverter` page.

**Number of modules**
  The number of modules in the array, which is the sum of the number of modules in each subarray:

  *Total Number of Modules = ∑ Number of Modules in Subarrays 1 - 4*

**Total module area, m²**
  The total area in square meters of modules in the array, not including space between modules:

  *Total Area (m²) = Module Area (m²) × Number of Modules*

  The area of a single module is from the :doc:`pv_module` page.

System Sizing Messages
----------------------

The system sizing messages help you ensure that electrical layout of the array is compatible with the inverter under design conditions. These messages are based on module and inverter ratings. The messages do not prevent you from running a simulation, so you can use the operating voltages in the results to explore designs that exceed conventional limits.

.. image:: ../images/SS_PV-ArraySizingMessages.png
   :align: center
   :alt: SS_PV-ArraySizingMessages.png

The system sizing messages display the following information for each subarray:

* Subarray open circuit voltage is greater than the inverter maximum DC voltage:

  *String Voc > Vdc_max*

  Reduce the number of modules per string, choose an inverter with a higher maximum DC voltage, or choose a module with lower open circuit voltage.

* Subarray maximum power voltage is greater than the inverter maximum MPPT voltage:

  *String Vmp > Vmppt_high*

  Reduce the number of modules per string, choose an inverter with a higher maximum MPPT voltage, or choose a module with lower open circuit voltage.

* Subarray maximum power voltage is less than the inverter minimum MPPT voltage:

  *String Vmp < Vmppt_low*

  Increase the number of modules per string, choose an inverter with a lower minimum MPPT voltage, or choose a module with higher maximum power voltage.

Subarrays
~~~~~~~~~

Subarrays make it possible to divide the array into up to four sections that operate at different voltages. Modeling multiple subarrays may be useful for the following applications:

* A rooftop system with modules installed on different roof surfaces with different orientations.

* A rack-mounted system with racks in different orientations or a mix of tracking options.

* An array with groups of modules with different string lengths, DC wiring lengths, or exposed to different shading scenes or soiling conditions.

**Enable**
  For a system with single subarray, you do not need to enable additional subarrays. Subarray 1 is always enabled.

  For a system with up to four subarrays, check **Enable** for each additional Subarray 2, 3, or 4.

.. note:: You can enable any combination of subarrays. For example, you can model a system with two subarrays by enabling Subarrays 1 and 3, and disabling Subarrays 2 and 4.

   SAM can only model systems with one type of module and one type of inverter. You cannot use subarrays to model a system that combines different types of modules or inverters.

Subarrays and String Size
~~~~~~~~~~~~~~~~~~~~~~~~~

The Subarrays and String Size inputs determine the electrical layout of each subarray.

**Modules per string in subarray**
  The number of modules connected in series in a single string, or string length for each subarray. The number of modules per string determines the subarray's open circuit string voltage (Voc) rating and maximum power rated string voltage (Vmp):

  *Subarray Voc (V) = Module Voc (V) × Modules Per String in Subarray*

  *Subarray Vmp (V) = Module Vmp (V) × Modules Per String in Subarray*

  As an initial rule of thumb, choose a number of modules per string so that the string Voc is less than the inverter's maximum DC voltage rating, and the string Vmp is between the inverter's minimum and maximum MPPT voltage rating.
  
  Run a simulation and look at the operating voltages in the results to see how they compare to the voltage ratings and your design requirements. You can also use the System Sizing :doc:`macro <../reference/macros>` to help refine your design.

**Strings in parallel in subarray**
  The number of strings of modules connected in parallel to form a subarray, which along with the number of modules per string in each subarray determines the array nameplate DC capacity.

  For each subarray that has more than one string in parallel, SAM calculates the subarray voltage using the :ref:`PV subarray voltage <pvmismatchoption>` mismatch method you choose.

**Number of modules in subarray**
  The number of modules in each subarray depends on the number of modules per string, and number of strings in parallel in each subarray:

  *Number of Modules in Subarray = Modules per String in Subarray × Strings in Parallel in Subarray*

**String Voc at reference conditions, Vdc**
  The open circuit DC voltage of each string of modules at the module reference conditions shown on the :doc:`Module <pv_module>` page:

  *String Voc (V) = Module Open Circuit Voltage (V) × Modules per String*

**String Vmp at reference coinditions, Vdc**
  The DC voltage at the module maximum power point of each string of modules at the module reference conditions shown on the :doc:`Module <pv_module>` page:

  *String Vmp (Vdc) = Module Max Power Voltage (Vdc) × Modules per String*

.. _multiplemppt:

Inverter Input Voltage
~~~~~~~~~~~~~~~~~~~~~~

The Inverter Input Voltage inputs determine how SAM calculates the voltage at the inverter input.

Multiple MPPT Inputs
--------------------

If your system has one inverter and supports up to four maximum power point trackers (MPPT), set **Number of MPPT inputs** page to the number of MPPTs, and assign an MPPT number to each subarray. This allows each subarray to operate at its own operating DC voltage.

.. note:: The term "MPPT input" refers to the electrical connection to a maximum power point tracker (MPPT). The MPPT hardware in your system may be integrated with the inverter or separate.

If your system has one inverter and more than four MPPTs, model the system with one MPPT. Alternatively, depending on the design of your system, it may be possible to use a single MPPT number to represent a group of MPPTs in your system that are connected to modules with the same orientation, tracking, and shading.

If your system has more than one inverter and more than one subarray, SAM assumes that all inverters operate at the same voltage determined by a single MPPT. The inverter input voltage is either the average of the subarray voltages or calculated using an iterative method, depending on the method you choose for :ref:`pvmismatchoption`.

See :doc:`pv_sizing_instructions` for more details and examples.

**Number of MPPTs**
  The number of maximum power point trackers in your system. This is only enabled when **Number of inverters** is 1.

  The number of MPPTs should be equal to the number of enabled subarrays. The maximum value is 4.

**MPPT number**
  The MPPT number assigned to each subarray. These are enabled when **Number of MPPTs** is 2, 3, or 4.

  The MPPT number must be between 1 and the number of MPPTs and can be assigned to any subarray. For example, if Subarrays 1 and 3 are enabled and there are two MPPTs, you can assign MPPT number 1 to Subarray 1 and MPPT number 2 to Subarray 3 (or vice versa), but you cannot use MPPT number 3 because there are only two MPPTs.

Inverter Voltage Limits
-----------------------

Inverter voltage limits are the inverter's minimum and maximum input voltage ratings. If the inverter operating input voltage is outside the range of these ratings, SAM clips the operatintg voltage to the voltage rating.

For systems with more than one inverter, SAM assumes that inverters are connected in parallel so that the rated voltages of the inverter bank are the same as those of a single inverter.

When you change the inverter parameters on the :doc:`pv_inverter` page, SAM automatically sets the **Vmppt_low**, **Vmppt_high**, and **Vdc_max** values to the values from the Inverter page. You can change the values here for a system that has different ratings than those on the inverter page.

**Vmppt_low, Vdc**
  The inverter minimum voltage rating from the :doc:`pv_inverter` page. If the array or subarray voltage is less than Vmppt_low, SAM sets the inverter input voltage equal to Vmppt_low.

**Vdc_max, Vdc**
  The inverter's maximum rated input DC voltage from the :doc:`pv_inverter` page. Set this value equal to **Vmppt_high** (see note below).

**Vmppt_high, Vdc**
  The inverter minimum and maximum operating voltages, as specified by the manufacturer, from the :doc:`pv_inverter` page. If the array or subarray voltage is greater than Vmppt_high, SAM sets the inverter input voltage to Vmppt_high.

.. note:: SAM prevents the inverter input voltage from exceeding Vmppt_high and does not allow Vdc_max to be greater than Vmppt_high, so Vdc_max has no effect on the results. Older versions of the inverter model did not have Vmppt_low and Vmppt_high inputs and limited the DC voltage to Vdc_max. 

**Copy ratings from Inverter page**
  Click this button to copy values of **Vmppt_low**, **Vmppt_high**, and **Vdc_max** from the inverter page.

.. _pvmismatchoption:

Inverter Input Voltage for Multiple Subarrays
---------------------------------------------

For a system with one MPPT and more than one subarray, SAM must estimate the inverter input voltage because the different subarrays operate at different voltages if they have different orientations, tracking, or shading.

**Average of subarray voltages**
  SAM calculates each subarray's output at its maximum power point voltage (Vmp), and assumes that the inverter DC input voltage is the average of the subarray Vmp values.

  Use this method for most analyses because it is a reasonable approximation, is fast, and works with all module model options.

**Estimate subarray mismatch loss (long simulation run time)**
  SAM iterates over many string voltages to find the value that results in the maximum power from the array. For each test voltage, it finds the current from each subarray, calcualtes the total current as the sum of subarray currents, and calculates the power as the product of the test voltage and total current. The inverter input voltage is set to the test voltage that yields the maximum power.

  This iterative method is not available for the Sandia or Simple Efficiency module models because those models only allow the array to operate at its maximum power point. 

  This method takes on the order of 10-30 seconds for a system with two or more subarrays.
 