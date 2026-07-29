Solar Field
===========

Solar Field Parameters
~~~~~~~~~~~~~~~~~~~~~~

**Number of collector modules in a loop**
  Each collector module is composed of mirror collectors and a receiver, defined on the Collector and Receiver page. Every loop in the field has the same number of collector modules.

**Number of subfield headers**
  The solar field is divided into subfields, connected with header piping. The number of field subsections determine the location and shape of header piping that delivers heat transfer fluid to the power block, which affects the heat loss calculation.

**Header pipe roughness, m**
  The header pipe roughness is a measure of the internal surface roughness of the header and runner piping. SAM uses this value in calculation of the shear force and piping pressure drops.

  Surface roughness is important in determining the scale of the pressure drop throughout the system. As a general rule, the rougher the surface, the higher the pressure drop (and parasitic pumping power load). The surface roughness is a function of the material and manufacturing method used for the piping.

**Piping thermal loss coefficient, W/m****\ :sup:`2`\****-K**
  The thermal loss coefficient that is used to calculate thermal losses from piping between receivers, crossover piping, header piping, and runner piping. The coefficient specifies the number of thermal watts lost from the system as a function of the piping surface area, and the temperature difference between the fluid in the piping and the ambient air (dry bulb temperature). The length of crossover piping and the piping distance between assemblies are defined on the :doc:`Collector and Receiver <iph_mslf_collector_receiver>`   page.

**Header pipe roughness, m**
  Average surface roughness of the header piping. The pipe roughness is used to calculate pressure loss through the piping system during the time-series simulation. 

**HTF pump efficiency**
  The isentropic efficiency of the heat transfer fluid pump in the solar field. The total work required to propel the HTF is divided by this efficiency value to give the electrical parasitic pumping requirement.

**Tracking power per SCA, W/sca**
  The electric power in Watts required by the tracking mechanism of one collector during hours of operation.

**Stow angle, degrees**
  The solar elevation angle (above the horizon) that sets the operational limit of the collector field in the evening hours. When the solar elevation angle falls below this value, the collector field will cease operation.

**Deploy angle, degrees**
  The solar elevation angle (above the horizon) that sets the operational limit of the collector field in the morning hours. When the solar elevation angle rises above this value, the collector field will begin operation.

**Design point wind velocity, m/s**
  The wind velocity at which receiver heat loss performance is evaluated for the design point (if applicable). This value is used to calculate receiver thermal loss at design when the Polynomial heat loss model is used on the :doc:`Collector and Receiver <../csp-linear-fresnel-molten-salt/mslf_collector_and_receiver>`   page under the Receiver Geometry and Heat Loss group.

**Design point ambient temperature,****°C**

  The reference ambient temperature for the solar field, used as a basis for calculating thermal losses from the receivers and piping. Note that this value is not used as a reference for receiver thermal losses if the evacuated tube receiver option is selected on the :doc:`Collector and Receiver <../csp-linear-fresnel-molten-salt/mslf_collector_and_receiver>`   page.

**Startup temperature,****°C**

  Temperature at which the field stops recirculating HTF and begins delivering to storage and heat sink.

**Collector startup energy, kWhe/sca**

  Amount of energy collector assembly needs to start delivering heat to the HTF. This represents the heat the collector absorbs to bring its temperature from ambient to operating conditions. 

**Length of runner pipe in power block, m**

  Length of piping connecting the power block to other system components. This value is used to calculate thermal losses in the system. 

Heat Transfer Fluid
~~~~~~~~~~~~~~~~~~~

**Field HTF fluid**
  The heat transfer fluid (HTF) used in the heat collection elements and headers of the solar field. SAM includes the following options in the HTF library: Solar salt, Caloria, Hitec XL, Therminol VP-1, Hitec salt, Dowtherm Q, Dowtherm RP, Therminol 59, and Therminol 66. You can also define your own HTF using the user-defined HTF fluid option.

.. note:: During the simulation, SAM counts the number of instances that the HTF temperature falls outside of the operating temperature limits in the table below. If the number of instances exceeds 50, it displays a simulation :doc:`notice <../results/notices>` with the HTF temperature and time step number for the 50th instance.


.. note:: If you define a custom fluid, SAM disables the minimum and maximum operating temperature variables and displays zero because it does not have information about the operating limits for the fluid you defined. You can check the time series temperature data in the results to ensure that they do not exceed the limits for your custom fluid.

.. include:: ../includes/snip_htf_properties.rst

**User-defined solar field HTF fluid**
  To define your own HTF, choose User-defined for the Field HTF fluid and specify a table of material properties for use in the solar field. You must specify at least two data points for each property: temperature, specific heat, density, viscosity, and conductivity. See :ref:`Custom HTF <iph-mslf-customhtf>` for details.

**Field HTF min operating temp, ºC**
  The minimum HTF operating temperature recommended by the HTF manufacturer.

  In some cases the minimum operating temperature may be the same as the fluid's freeze point. However, at the freeze point the fluid is typically significantly more viscous than at design operation temperatures, so it is likely that the "optimal" minimum operating temperature is higher than the freeze point.

**Field HTF max operating temp, ºC**
  The minimum HTF operating temperature recommended by the HTF manufacturer.

  Operation at temperatures above this value may result in degradation of the HTF and be unsafe. To avoid this, you may want to include a safety margin and use a maximum operating temperature value slightly lower than the recommended value.

 


.. note:: SAM displays the operating temperature limits for your reference so you can compare them to the field temperatures reported in the results to ensure that they do not exceed the limits. SAM does not adjust the system's performance to avoid exceeding these operating limits.


.. note:: SAM only displays these limits for fluids that are in SAM's library. If you use a custom HTF instead of one from the SAM library, SAM disables the HTF operating temperature limits. In this case, you should use data from the fluid manufacturer specifications.

**Freeze point heat trace activation temperature, °C**
  The solar field temperature below which auxiliary fossil backup heat is supplied to the solar field to prevent water from freezing in the equipment. You should set this value such that a reasonable margin exists between activation of the electric heat trace freeze protection equipment and the actual freezing point of water.

**Min single loop flow rate, kg/s**
  The minimum allowable flow rate through a single loop in the field.

  During time steps that produce a solar field flow rate that falls below the minimum value, the HTF temperature leaving the solar field will be reduced in temperature according to the heat added and minimum mass flow rate.

**Max single loop flow rate, kg/s**
  The maximum allowable flow rate through a single loop in the field.

  During time steps that produce a solar field flow rate that exceeds the maximum value, the solar field will be defocused according to the strategy selected by the user on the Solar Field page until the absorbed energy and corresponding mass flow rate fall below the maximum value.

**Header design min flow velocity, m/s**
  The minimum allowable HTF flow velocity in the header piping under design conditions.

**Header design max flow velocity, m/s**
  The maximum allowable HTF flow velocity in the header piping under design conditions. The minimum/maximum header flow velocities are used to determine the diameter of the header piping as flow is diverted to each loop in the field. After flow is distributed (or collected) to/from the loops, System Advisor calculates the flow velocity and resizes the piping to correspond to the maximum velocity if the calculated value falls outside of the user-specified range.

Plant Heat Capacity
~~~~~~~~~~~~~~~~~~~

**Heat capacity of the balance of plant on the hot side, kWht/K-MWt**
  The thermal inertia of the hot header to account for any thermal inertia not accounted for in the HTF volume calculations: Thermal energy in kilowatt-hours per gross electricity capacity in megawatts needed to raise the hot side temperature one degree Celsius. The default value is 0.2 kWht/K-MWt.

**Heat capacity of the balance of plant on the cold side, kWht/K-MWt**
  The thermal inertia of the cold header to account for any thermal inertia not accounted for in the HTF volume calculations: Thermal energy in kilowatt-hours per gross electricity capacity in megawatts needed to raise the hot side temperature one degree Celsius. The default value is 0.2 kWht/K-MWt.

**Non-HTF heat capacity associated with each SCA, Wht/K-m**
  The thermal inertia of piping, joints, insulation, and other SCA components: The amount of thermal energy per meter of SCA length required to raise the temperature of piping, connectors, insulation, and other SCA components one degree K. The default value is 4.5 Wht/K-m.

**HTF volume in single collector unit, L/m2-ap**
  The quantity of HTF contained in the solar field per square meter of aperture area. This value is used to calculate the internal energy of the HTF in the solar field during the time series simulation. You can calculate this value by considering the piping (or other) internal volume in the solar field transport system and dividing by the total reflector aperture area.

  Note that this volume is used for both the Polynomial heat loss model and the Evacuated Tube Receiver model.

.. _iph-mslf-landarea:

Land Area
~~~~~~~~~

.. include:: ../includes/snip_land_area_trough.rst

Design Point
~~~~~~~~~~~~

The design point variables show values at the Irradiation at Design Value that SAM uses to determine the system capacity in sizing calculations, and for area-based costs on the :doc:`Installation costs <../installation-costs/cc_linear_fresnel>` page.

For a description of the equations for the design point variables, see   See :ref:`Equations for Calculated Values <iph-mslf-sf-calcvalues>`  .

**Single loop aperture (m²)**
  The aperture area of a single loop of collectors, equal to the product of aperture width, reflective area, times the structure length times the number of collector assemblies per loop according to the distribution of the up to four collector types in the field. This area does not include non-reflective surface on the collector or non-reflective space between collectors.

*Single Loop Aperture (m²) = Sum of the SCA Reflective Aperture Area (m²) values for each SCA in the loop*

  The SCA reflective aperture area for each SCA type is specified on the :doc:`Collector and Receiver <../csp-linear-fresnel-molten-salt/mslf_collector_and_receiver>`   page.

**Loop optical efficiency**
  The optical efficiency when incident radiation is normal to the aperture plane, not including end losses or cosine losses. This value does not include thermal losses from piping and the receivers.

*Loop Optical Efficiency = Collector Optical Efficiency at Design × Receiver Optical Derate*

  The receiver and collector optical efficiency values are from the :doc:`Collector and Receiver <../csp-linear-fresnel-molten-salt/mslf_collector_and_receiver>`   page.

**Total loop conversion efficiency**
  The total conversion efficiency of the loop, including optical losses and estimated thermal losses. Used to calculate the required aperture area of the solar field.

**Total required aperture, SM=1, m²**
  The exact mirror aperture area required to meet the design thermal output for a solar multiple of 1.0. SAM uses the required aperture to calculate the total aperture reflective area. The total aperture reflective area may be slightly more or less than the required aperture, depending on the collector dimensions you specify on the :doc:`Collectors page <../csp-physical-trough-model/troughphysical_collectors_scas>`  .

**Required number of loops, SM=1**
  The exact number of loops required to produce the total required aperture at a solar multiple of 1.0. This number may be a non-integer value, SAM rounds this value to the nearest integer to calculate the value of the actual number of loops variable.

**Actual number of loops**
  The actual number of loops in the field, equal to the solar multiple times the required number of loops at a solar multiple of 1.0. The required number of loops is rounded to the nearest integer to represent a realistic field layout.

**Total aperture reflective area, m²**
  The actual aperture area based on the actual number of loops in the field, equal to the single loop aperture times the actual number of loops.

**Actual solar multiple**
  For Option 1 (solar multiple mode), the calculated solar multiple based on the actual (rounded) number of loops in the field. For Option 2 (field aperture mode), the solar multiple value corresponding to the thermal output of the field based at design point: The total aperture reflective area divided by the field thermal output. 

**Field thermal output (MWt)**
  The thermal energy delivered by the solar field under design conditions at the actual solar multiple.

**Loop inlet HTF temperature (ºC)**
  The temperature of HTF at the loop inlet under design conditions. The actual temperature during operation may differ from this value. SAM sets the power cycle's design outlet temperature equal to this value.

**Loop outlet HTF temperature (ºC)**
  The temperature of the HTF at the outlet of the loop under design conditions. During operation, the actual value may differ from this set point. This value represents the target temperature for control of the HTF flow through the solar field and will be maintained when possible.

**Steady State Design Point Results**
  **NEED GENERAL DESCRIPTION OF HOW THE DESIGN POINT VARIABLES ABOVE**

.. _iph-mslf-sf-calcvalues:

Equations for Calculated Values
-------------------------------

The following table shows the equations SAM uses to calculate the values for the variables above that you cannot edit. (In Windows, the calculated values appear in blue.)

**Single loop aperture**
  Sum of aperture area of each individual collector in the loop

  .. image:: ../images/EQ_TRP-SingleLoopAperture.png
     :align: center
     :alt: EQ_TRP-SingleLoopAperture.png

**Loop optical efficiency**
  Efficiencies are calculated elsewhere

  = Aggregate Collector Efficiency * Aggregate Receiver Optical Efficiency

**Total loop conversion efficiency**
|EQ_TRP-LoopTotalEfficiency|   used in Total Required Aperture equation

  = Loop Optical Efficiency * Receiver Heat Loss Efficiency

**Total required aperture, SM=1**
|EQ_TRP-AsfSM|   used in Required Number of Loops equation

  .. image:: ../images/EQ_TRP-TotalReqAperture.png
     :align: center
     :alt: EQ_TRP-TotalReqAperture.png

**Required number of loops, SM=1**
  .. image:: ../images/EQ_TRP-ReqNumberLoops.png
     :align: center
     :alt: EQ_TRP-ReqNumberLoops.png

**Actual number of loops**
  Equation depends on solar field option: Option 1 uses the solar multiple value that you specify, Option 2 uses the field aperture area that you specify.

  .. image:: ../images/EQ_TRP-ActualNumberLoops1.png
     :align: center
     :alt: EQ_TRP-ActualNumberLoops1.png

  .. image:: ../images/EQ_TRP-ActualNumberLoops2.png
     :align: center
     :alt: EQ_TRP-ActualNumberLoops2.png

**Total aperture reflective area**
  .. image:: ../images/EQ_TRP-ActualAperture.png
     :align: center
     :alt: EQ_TRP-ActualAperture.png

**Actual solar multiple**
  Equation depends on solar field option: Option 1 is the solar multiple value that you specify:

  .. image:: ../images/EQ_TRP-SM.png
     :align: center
     :alt: EQ_TRP-SM.png

  Option 2 is the ratio:

  .. image:: ../images/EQ_TRP-SM1.png
     :align: center
     :alt: EQ_TRP-SM1.png

**Field thermal output**
  pb = power block

  .. image:: ../images/EQ_TRP-FieldThermalOutput.png
     :align: center
     :alt: EQ_TRP-FieldThermalOutput.png

**Solar field area**
|EQ_TRP-Wscai|   is the aperture width of each collector i in the loop

  .. image:: ../images/EQ_TRP-SolarFieldArea.png
     :align: center
     :alt: EQ_TRP-SolarFieldArea.png

**Total land area**
  = Solar field area * Non-Solar field land area multiplier

Mirror Washing
~~~~~~~~~~~~~~

SAM reports the water usage of the system in the results based on the mirror washing variables. The annual water usage is the product of the water usage per wash and 365 (days per year) divided by the washing frequency.

**Water usage per wash**
  The volume of water in liters per square meter of solar field aperture area required for periodic mirror washing.

**Washes per year**
  The number of washes in one year.

.. _iph-mslf-customhtf:

Custom HTF
~~~~~~~~~~

.. include:: ../includes/snip_custom_htf.rst



.. |EQ_TRP-AsfSM| image:: ../images/EQ_TRP-AsfSM.png
.. |EQ_TRP-LoopTotalEfficiency| image:: ../images/EQ_TRP-LoopTotalEfficiency.png
.. |EQ_TRP-Wscai| image:: ../images/EQ_TRP-Wscai.png
