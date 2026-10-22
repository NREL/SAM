Solar Field
===========

System Design Point Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These values are from the :doc:`System Design page <iph_trough_system_design>` are for reference.

Solar Field Design Point
~~~~~~~~~~~~~~~~~~~~~~~~

The design point variables show values at the Irradiation at Design Value that SAM uses to determine the system capacity in sizing calculations.

**Single loop aperture (m²)**
  The aperture area of a single loop of collectors, equal to the product of aperture width, reflective area, times the structure length times the number of collector assemblies per loop according to the distribution of the up to four collector types in the field. This area does not include non-reflective surface on the collector or non-reflective space between collectors.

*Single Loop Aperture (m²) = Sum of the SCA Reflective Aperture Area (m²) values for each SCA in the loop*

  The SCA reflective aperture area for each SCA type is specified on the :doc:`Collectors (SCAs) <../csp-physical-trough-model/troughphysical_collectors_scas>`   page. The number of each type of SCA in a single loop is specified under **Single Loop Configuration** as described in :ref:`Specifying the Loop Configuration <physical_sfloopconfiguration>`  .

**Loop optical efficiency**
  The optical efficiency when incident radiation is normal to the aperture plane, not including end losses or cosine losses. This value does not include thermal losses from piping and the receivers.

*Loop Optical Efficiency = SCA Optical Efficiency at Design × HCE Optical Derate*

  The SCA and HCE optical efficiency values are from the :doc:`Collectors (SCA) <../csp-physical-trough-model/troughphysical_collectors_scas>`   page and :doc:`Receivers (HCEs) <../csp-physical-trough-model/troughphysical_receivers_hces>`   page, respectively.

**Total loop conversion efficiency**
  The total conversion efficiency of the loop, including optical losses and estimated thermal losses. Used to calculate the required aperture area of the solar field.

**Total required aperture, SM=1 (m²)**
  The exact mirror aperture area required to meet the design thermal output for a solar multiple of 1.0. SAM uses the required aperture to calculate the total aperture reflective area. The total aperture reflective area may be slightly more or less than the required aperture, depending on the collector dimensions you specify on the :doc:`Collectors page <../csp-physical-trough-model/troughphysical_collectors_scas>`  .

**Required number of loops, SM=1**
  The exact number of loops required to produce the total required aperture at a solar multiple of 1.0. This number may be a non-integer value, SAM rounds this value to the nearest integer to calculate the value of the actual number of loops variable.

**Actual number of loops**
  The actual number of loops in the field, equal to the solar multiple times the required number of loops at a solar multiple of 1.0. The required number of loops is rounded to the nearest integer to represent a realistic field layout.

**Total aperture reflective area (m²)**
  The actual aperture area based on the actual number of loops in the field, equal to the single loop aperture times the actual number of loops.

**Actual solar multiple**
  For Option 1 (solar multiple mode), the calculated solar multiple based on the actual (rounded) number of loops in the field. For Option 2 (field aperture mode), the solar multiple value corresponding to the thermal output of the field based at design point: The total aperture reflective area divided by the field thermal output. 

**Actual field thermal output (MWt)**
  The thermal energy delivered by the solar field under design conditions at the actual solar multiple.

Solar Field Parameters
~~~~~~~~~~~~~~~~~~~~~~

**Row spacing (m)**
  The centerline-to-centerline distance in meters between rows of collectors, assuming that rows are laid out uniformly throughout the solar field. Default is 15 meters.

**Stow angle (degrees)**
  The collector angle during the hour of stow. A stow angle of zero for a northern latitude is vertical facing east, and 180 degrees is vertical facing west. Default is 170 degrees.

**Deploy angle (degrees)**
  The collector angle during the hour of deployment. A deploy angle of zero for a northern latitude is vertical facing due east. Default is 10 degrees.

**Header pipe roughness (m)**
  The header pipe roughness is a measure of the internal surface roughness of the header and runner piping. SAM uses this value in calculation of the shear force and piping pressure drop in the headers.

  Surface roughness is important in determining the scale of the pressure drop throughout the system. As a general rule, the rougher the surface, the higher the pressure drop (and parasitic pumping power load). The surface roughness is a function of the material and manufacturing method used for the piping.

**HTF pump efficiency**
  The electrical-to-mechanical energy conversion efficiency of the field heat transfer fluid pump. This value accounts for all mechanical, thermodynamic, and electrical efficiency losses.

**Piping thermal loss coefficient (W/m****\ :sup:`2`\****-K)**
  The thermal loss coefficient that is used to calculate thermal losses from piping between receivers, crossover piping, header piping, and runner piping. The coefficient specifies the number of thermal watts lost from the system as a function of the piping surface area, and the temperature difference between the fluid in the piping and the ambient air (dry bulb temperature). The length of crossover piping depends on the row spacing variable above, and the piping distance between assemblies on the :doc:`Collectors <../csp-physical-trough-model/troughphysical_collectors_scas>`   page.

**Wind stow speed, m/s**
  When the wind speed in the weather file exceeds this value, the collectors in field defocus and the field thermal power incident on the field goes to zero to model the effect of collectors moving to a safe position to avoid wind damage. When the wind speed falls below the wind stow speed, the field power returns to normal.

**Tracking power (W/sca)**
  The amount of electrical power consumed by a single collector tracking mechanism. SAM only calculates tracking losses during hours when collectors are actively tracking the sun. The total field tracking power is calculated by multiplying this value by the number of loops in the field and number of assemblies per loop.

**Total tracking power**
  Calculated as the product of the tracking power and number of collectors from the :doc:`Collectors page <iph_trough_collectors>`  .

**Number of field subsections**
  SAM assumes that the solar field is divided into between two and 12 subsections. Examples of solar field with 2, 4, and 6 subsections are shown below:

  .. image:: ../images/IMG_solar-field-layout-subsections.png
     :align: center
     :alt: IMG_solar-field-layout-subsections.png

  The number of field subsections determine the location and shape of header piping that delivers heat transfer fluid to the power block, which affects the heat loss calculation.

**Model piping through heat sink?**
  Choose this option to model heat losses for piping through a heat sink.

**Length of piping through heat sink**
  Length of field piping that passes through a heat sink.

Heat Transfer Fluid
~~~~~~~~~~~~~~~~~~~

**Field HTF fluid**
  The heat transfer fluid (HTF) used in the heat collection elements and headers of the solar field. SAM includes the following options in the HTF library: Solar salt, Caloria, Hitec XL, Therminol VP-1, Hitec salt, Dowtherm Q, Dowtherm RP, Therminol 59, and Therminol 66. You can also define your own HTF using the user-defined HTF fluid option.

.. note:: During the simulation, SAM counts the number of instances that the HTF temperature falls outside of the operating temperature limits in the table below. If the number of instances exceeds 50, it displays a simulation :doc:`notice <../results/notices>` with the HTF temperature and time step number for the 50th instance.


.. note:: If you define a custom fluid, SAM disables the minimum and maximum operating temperature variables and displays zero because it does not have information about the operating limits for the fluid you defined. You can check the time series temperature data in the results to ensure that they do not exceed the limits for your custom fluid.

.. include:: ../includes/snip_htf_properties.rst

**User-defined HTF fluid**
  To define your own HTF, choose User-defined for the Field HTF fluid and specify a table of material properties for use in the solar field. You must specify at least two data points for each property: temperature, specific heat, density, viscosity, and conductivity. See :ref:`Custom HTF <iph-trough-customhtf>` for details.

**Field HTF min operating temp (ºC)**
  The minimum HTF operating temperature recommended by the HTF manufacturer.

  In some cases the minimum operating temperature may be the same as the fluid's freeze point. However, at the freeze point the fluid is typically significantly more viscous than at design operation temperatures, so it is likely that the "optimal" minimum operating temperature is higher than the freeze point.

**Field HTF max operating temp (ºC)**
  The minimum HTF operating temperature recommended by the HTF manufacturer.

  Operation at temperatures above this value may result in degradation of the HTF and be unsafe. To avoid this, you may want to include a safety margin and use a maximum operating temperature value slightly lower than the recommended value.

 


.. note:: SAM displays the operating temperature limits for your reference so you can compare them to the field temperatures reported in the results to ensure that they do not exceed the limits. SAM does not adjust the system's performance to avoid exceeding these operating limits.


.. note:: SAM only displays these limits for fluids that are in SAM's library. If you use a custom HTF instead of one from the SAM library, SAM disables the HTF operating temperature limits. In this case, you should use data from the fluid manufacturer specifications.

**Freeze protection temp (ºC)**
  The minimum temperature that the heat transfer fluid is allowed to reach in the field. The temperature at which freeze protection equipment is activated.

  SAM assumes that electric heat trace equipment maintains the fluid at the freeze protection temperature during hours that freeze protection is operating.**Design loop inlet temp (ºC)**
  The temperature of HTF at the loop inlet under design conditions. The actual temperature during operation may differ from this value.

**Min single loop flow rate (kg/s)**
  The minimum allowable flow rate through a single loop in the field.

  During time steps that produce a solar field flow rate that falls below the minimum value, the HTF temperature leaving the solar field will be reduced in temperature according to the heat added and minimum mass flow rate.

**Max single loop flow rate (kg/s)**
  The maximum allowable flow rate through a single loop in the field.

  During time steps that produce a solar field flow rate that exceeds the maximum value, the solar field will be defocused according to the strategy selected by the user on the Solar Field page until the absorbed energy and corresponding mass flow rate fall below the maximum value.

**Min field flow velocity (m/s)**
  The minimum allowable HTF flow velocity through the field.

**Max field flow velocity (m/s)**
  The maximum allowable HTF flow velocity through the field.

  The minimum and maximum solar field HTF flow velocity depend on the minimum and maximum HTF mass flow rates, HTF density at the design loop inlet temperature, and the absorber tube inner diameter specified on the Receivers page. SAM calculates the field HTF flow velocity for the receiver type with the smallest diameter.

**Header design min flow velocity (m/s)**
  The minimum allowable HTF flow velocity in the header piping under design conditions.

**Header design max flow velocity (m/s)**
  The maximum allowable HTF flow velocity in the header piping under design conditions. The minimum/maximum header flow velocities are used to determine the diameter of the header piping as flow is diverted to each loop in the field. After flow is distributed (or collected) to/from the loops, System Advisor calculates the flow velocity and resizes the piping to correspond to the maximum velocity if the calculated value falls outside of the user-specified range.

Collector Orientation
~~~~~~~~~~~~~~~~~~~~~

**Collector tilt (degrees)**
  The angle of all collectors in the field in degrees from horizontal, where zero degrees is horizontal. A positive value tilts up the end of the array closest to the equator (the array's south end in the northern hemisphere), a negative value tilts down the southern end. SAM assumes that the collectors are fixed at the tilt angle.

**Collector azimuth (degrees)**
  The azimuth angle of all collectors in the field, where zero degrees is pointing toward the equator, equivalent to a north-south axis. West is 90 degrees, and east is -90 degrees. SAM assumes that the collectors are oriented 90 degrees east of the azimuth angle in the morning and track the daily movement of the sun from east to west.

Mirror Washing
~~~~~~~~~~~~~~

SAM reports the water usage of the system in the results based on the mirror washing variables. The annual water usage is the product of the water usage per wash and 365 (days per year) divided by the washing frequency.

**Water usage per wash (L/m²,aper)**
  The volume of water in liters per square meter of solar field aperture area required for periodic mirror washing.

**Washes per year**
  The number of washes in a single year.

Plant Heat Capacity
~~~~~~~~~~~~~~~~~~~

The plant heat capacity values determine the thermal inertia due to the mass of hot and cold headers, and of SCA piping, joints, insulation, and other components whose temperatures rise and fall with the HTF temperature. SAM uses the thermal inertia values in the solar field energy balance calculations.

You can use the hot and cold piping thermal inertia inputs as   empirical adjustment factors to help match SAM results with observed plant performance.

**Hot piping thermal inertia (kWht/K-MWt)**
  The thermal inertia of the hot header to account for any thermal inertia not accounted for in the HTF volume calculations: Thermal energy in kilowatt-hours per gross electricity capacity in megawatts needed to raise the hot side temperature one degree Celsius. The default value is 0.2 kWht/K-MWt.

**Cold piping thermal inertia (kWht/K-MWt)**
  The thermal inertia of the cold header to account for any thermal inertia not accounted for in the HTF volume calculations: Thermal energy in kilowatt-hours per gross electricity capacity in megawatts needed to raise the hot side temperature one degree Celsius. The default value is 0.2 kWht/K-MWt.

**Field loop piping thermal inertia (Wht/K-m)**
  The thermal inertia of piping, joints, insulation, and other SCA components: The amount of thermal energy per meter of SCA length required to raise the temperature of piping, joints, insulation, and other SCA components one degree K. The default value is 4.5 Wht/K-m.

.. _iph-trough-landarea:

Land Area
~~~~~~~~~

.. include:: ../includes/snip_land_area_trough.rst

Single Loop Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~

The loop configuration and collector defocusing for the process heat trough model is the same as for the CSP physical trough model. For details about loop configuration. For details, see:

* :ref:`Specifying the Loop Configuration <physical_sfloopconfiguration>`


* :ref:`Defining Collector Defocusing <physical_sfcollectordefocusing>`


**Number of SCA/HCE assemblies per loop**
  The number of individual solar collector assemblies (SCAs) in a single loop of the field. Computationally, this corresponds to the number of simulation nodes in the loop. See :ref:`Specifying the Loop Configuration <physical_sfloopconfiguration>` for details.

**Edit SCAs**
  Click **Edit SCAs** to assign an SCA type number (1-4) to each of the collectors in the loop. Use your mouse to select collectors, and type a number on your keyboard to assign a type number to the selected collectors. SAM indicates the SCA type by coloring the rectangle representing the collector in the diagram, and displaying the type number after the word "SCA." 

**Edit HCEs**
  Click **Edit HCEs** to assign a receiver type number (1-4) to each of the collectors in the loop. Use your mouse to select collectors, and type a number on your keyboard to assign a type number. SAM indicates the HCE type by coloring the line representing the receiver, and displaying the type number after the word "HCE."

**Edit Defocus Order**
  Click **Edit Defocus Order** to manually define the defocus order of the collectors in the field. Click an assembly to assign the defocus order. You should assign each collector a unique defocus order number.

**Reset Defocus**
  Click to reset the defocus order to the default values, starting at the hot end of the loop and proceeding sequentially toward the cold end of the loop. 

The collector defocusing inputs are the same as for the CSP physical trough model. 

.. _iph-trough-customhtf:

Custom HTF
~~~~~~~~~~

.. include:: ../includes/snip_custom_htf.rst

