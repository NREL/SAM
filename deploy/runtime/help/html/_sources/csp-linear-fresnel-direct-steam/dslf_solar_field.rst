Solar Field
===========

Solar Field Parameters
~~~~~~~~~~~~~~~~~~~~~~

**Option 1 and Option 2**
  For Option 1 (solar multiple mode), SAM calculates the total required aperture and number of loops based on the value you enter for the solar multiple.

  For option 2 (field aperture mode), SAM calculates the solar multiple based on the field aperture value that you enter.

**Solar Multiple**
  The field aperture area expressed as a multiple of the aperture area required to operate the power cycle at its design capacity. 

.. note:: For a discussion of solar multiple and design-point conditions, see :ref:`Solar Multiple <dslf-solarmultiple>`.

**Design point irradiation, W/m²**
  The design point direct normal radiation value, used in solar multiple mode to calculate the aperture area required to drive the power cycle at its design capacity. Also used to calculate the design mass flow rate of the heat transfer fluid for header pipe sizing.

**Design point ambient temperature,****°C**

  The reference ambient temperature for the solar field, used as a basis for calculating thermal losses from the receivers and piping. Note that this value is not used as a reference for receiver thermal losses if the evacuated tube receiver option is selected on the :doc:`Collector and Receiver <dslf_collector_and_receiver>`   page.

**Loop flow configuration**
  The loop flow configuration determines whether the boiler is configured as once-through or recirculated. 

  In the **recirculated boiler** design, a portion of the collectors in the loop are dedicated to boiling the subcooled feedwater, but the boiler mass flow rate is controlled such that the boiling mixture exits the boiler section with a vapor fraction (quality) equal to the value that you specify in the Boiler outlet steam quality input on the Solar Field page. The liquid fraction is extracted and recirculated to the inlet of the solar field loop where it mixes with the subcooled liquid from the power cycle outlet. The saturated steam at the outlet of the boiler section does not recirculate, but instead passes into the dedicated superheater section where it continues to increase in temperature before entering the power cycle.

  In the **once-through** design, subcooled feedwater from the power cycle outlet enters the solar field collector loop, is boiled to steam as it passes through the loop, and exits to the hot header as superheated steam. 

  The most common configuration for existing linear Fresnel plants is the recirculated boiler design, though developments in the technology show the once-through design to be promising. Both options are included in this model for comparative purposes.

**Superheater has unique geometry**
  SAM allows you to specify different geometries and optical characteristics for the boiler and superheater sections of the field.

  To specify a field with different boiler and superheater geometries and optical characteristics, check the box, and specify the characteristics for each section on the :doc:`Collector and Receiver <dslf_collector_and_receiver>`   page.

  To specify a field with the same properties for the boiler and superheater sections, clear the check box, and specify characteristics for the boiler section only on the Collector and Receiver page. SAM will apply those properties to the entire field, and ignore the **Superheater Geometry** inputs on the Collector and Receiver page.

**Number of modules in boiler section**
  The number of boiler units in series in a single loop, each with geometry as defined on the :doc:`Collector and Receiver <dslf_collector_and_receiver>`   page under **Boiler Geometry**.

**Number of modules in superheater section**
  The number of superheater units in series in a single loop, each with geometry as defined on the :doc:`Collector and Receiver <dslf_collector_and_receiver>`   page under **Superheater Geometry**. 

  If **Superheater has unique geometry** is checked, the properties of the boiler section and superheater section will be different, as defined by the boiler and superheater inputs on the Collector and Receiver page. Otherwise, the boiler inputs to both the boiler and superheater sections.

.. note:: Take special care in selecting the number of boiler and superheater sections. The steam conditions at the outlet of the solar field depend on the ratio between the heat absorbed by the boiler and the superheater. As the heat absorbed in the superheater sections increases relative to the boiler sections, the outlet steam temperature will also increase beyond the design point. Consequently, the number of superheater modules should correspond to the desired thermal input in addition to the saturated steam produced by the boiler.

**Field pump efficiency**
  The isentropic efficiency of the feedwater and recirculation pump (if applicable) in the solar field. The total work required to propel the feedwater is divided by this efficiency value to give the electrical parasitic pumping requirement.

**Collector azimuth angle (degrees)**
  The azimuth angle of all collectors in the field, where zero degrees is pointing toward the equator, equivalent to a north-south axis. West is 90 degrees, and east is -90 degrees. SAM assumes that the collectors are oriented 90 degrees east of the azimuth angle in the morning and track the daily movement of the sun from east to west.

  The collector azimuth angle variable is not active with the **Solar position table option** on the :doc:`Collector and Receiver <dslf_collector_and_receiver>`   page. The variable is only active with either the two incidence angle options for specifying the solar field.

**Thermal inertia per unit of solar field**
  The amount of energy required to increase the working temperature of the solar field, per unit aperture area of the solar field. The thermal inertia term is used to model the startup and shutdown transient behavior of the solar field. During startup, the thermal energy produced by the solar field is reduced according to the energy that goes into heating the working fluid, receiver components, piping, fittings, and insulation. This input captures all of those aspects of transient startup in the solar field.

Steam Conditions at Design
~~~~~~~~~~~~~~~~~~~~~~~~~~

This set of inputs defines the design-point operating conditions of the steam in the solar field. The field inlet and outlet temperatures, the pressure constraints, and the boiler outlet quality (if applicable) are used to calculate the enthalpy of steam during the simulation at each collector module in the loop.

**Field inlet temperature, °C**
  The estimated temperature of feedwater from the power cycle at the inlet of the solar field. This value is used to calculate estimated thermal losses from the solar field at design, and is not directly used in calculating the hourly performance for the annual simulation. The field inlet temperature is calculated during performance runs based on the power cycle conversion efficiency and the steam temperature at the inlet of the power cycle.

**Field outlet temperature, °C**
  The estimated design-point steam outlet temperature from the solar field. The actual field outlet temperature is calculated in the performance runs based on the Loop flow configuration (once-through or recirculated boiler), the boiler outlet quality (for recirculated designs), collector performance, and flow rate constraints. The actual field outlet temperature during performance calculations is highly sensitive to the ratio of superheater to boiler aperture area, and consequently, the Field outlet temperature that you specify may differ substantially from the actual outlet temperature if care is not taken in selecting the correct number of superheater modules in the recirculated boiler design. Refer to the documentation on the number of boiler/superheater modules for more information.

**Boiler outlet steam quality**
  For the recirculated boiler configuration, the boiler outlet steam quality is used to calculate the mass flow rate of steam in the boiler section. This value represents the fraction of fluid exiting the boiler section that is in vapor phase. The balance of the unevaporated fluid recirculates to the inlet of the solar field and is mixed with the subcooled feedwater from the power cycle outlet.

  This value is not used for the once-through Loop flow configuration. 

**Turbine inlet pressure**
  The steam pressure at the inlet of the turbine at design conditions. The actual steam pressure during the performance calculations will vary as a function of the steam mass flow rate into the power cycle. The minimum steam pressure is limited to 50% of the design-point rating. Note that the steam mass flow rate into the power cycle may differ from the steam mass flow rate in the solar field if auxiliary fossil backup is used.

**Cold header pressure drop fraction**
  The fractional pressure drop across the cold header section of the solar field at design. The absolute pressure drop at design is equal to the fractional drop times the rated turbine inlet pressure.

**Boiler pressure drop fraction**
  The fractional pressure drop across the boiler section of the solar field at design. The absolute pressure drop at design is equal to the fractional drop times the rated turbine inlet pressure.

**Pressure drop fraction between boiler and superheater**
  The fractional pressure drop across any piping or steam separation equipment between the boiler and superheater sections at design. The absolute pressure drop at design is equal to the fractional drop times the rated turbine inlet pressure.

**Design point pressure drop across the superheater fraction**
  The fractional pressure drop across the superheater section of the solar field at design. The absolute pressure drop at design is equal to the fractional drop times the rated turbine inlet pressure.

**Average design point hot header pressure drop fraction**
  The fractional pressure drop across the hot header section of the solar field at design. The absolute pressure drop at design is equal to the fractional drop times the rated turbine inlet pressure.

**Total solar field pressure drop**
  The total calculated solar field pressure drop at design. The calculated value is based on a sum of the fractional pressure drops from individual solar field subsystems and is multiplied by the rated turbine pressure at design.

  .. image:: ../images/EQ_LF_PSFDesign.png
     :align: center
     :alt: EQ_LF_PSFDesign.png

  Note that the pressure at the inlet of the solar field is equal to the pressure at the inlet of the turbine plus the total pressure drop across the solar field. You should choose a turbine design-point pressure to maintain operable steam pressures at the solar field inlet. 

  The steam property algorithms currently used in the SAM performance runs limit the maximum steam pressure to 190 bar, and values exceeding this limit will be reset during the simulation. This limit has been known to cause convergence issues in cases where design-point pressures are too high or where the solar field is designed to frequently operate with mass flow rates significantly higher than the design-point flow rate on which the pressure drop relationship is based.

Design Point
~~~~~~~~~~~~

**Single loop aperture, ****m²**
This calculated value indicates the total reflective aperture area of the collectors in a single loop. The value is calculated by multiplying the number of nodes in the boiler and superheater sections by their corresponding reflective aperture area on the Collector and Receiver page. The total aperture area calculation is as follows:

  .. image:: ../images/EQ_LF_A-loop.png
     :align: center
     :alt: EQ_LF_A-loop.png

**Loop optical efficiency**

The total loop optical efficiency at design, where the solar position is normal to the collector aperture. The efficiency is the weighted product of the boiler and superheater sections, if applicable.

**Loop thermal efficiency**

The estimated thermal efficiency at design conditions corresponding to the input selections on the Collector and Receiver page. If the Polynomial fit heat loss model is used, the polynomial equation for temperature-based heat loss is evaluated at the average design-point solar field temperature, where that temperature is equal to the average of the outlet and inlet solar field temperatures on the Solar Field page.

  .. image:: ../images/EQ_LF_T-sf.png
     :align: center
     :alt: EQ_LF_T-sf.png

If the Evacuated tube heat loss model is used, SAM estimates thermal efficiency based on the user-specified Estimated avg. heat loss and Variant weighting fraction values on the Collector and Receiver page. 

.. note:: The design-point thermal efficiency value is used only to size the solar field aperture area and is not part of the simulation calculations.

**Piping thermal efficiency**

The estimated non-collector piping thermal efficiency at design. This value is calculated based on the Piping thermal loss coefficient on the Parasitics page. The estimated efficiency is equal to the compliment of the product of the average solar field operating temperature at design and the heat loss coefficient divided by the design-point solar irradiation.

.. image:: ../images/EQ_LF_Eff-piping.png
   :align: center
   :alt: EQ_LF_Eff-piping.png

**Total loop conversion efficiency**

The total estimated loop conversion efficiency at design, including collector optical performance, receiver thermal losses, and piping thermal losses. This value is used to size the solar field aperture area given a solar multiple and required power cycle thermal input.

**Total required aperture, SM=1**

The calculated aperture area that provides a thermal output from the solar field that exactly matches the power cycle design-point thermal input (i.e. a solar multiple of 1). This value is used to calculate the corresponding number of loops at a solar multiple of 1.

**Required number of loops, SM=1**

The number of loops that fulfills the thermal output requirement of the solar field at a solar multiple of 1.

**Actual number of loops**

The number of loops in the solar field that produces a thermal output at design equal to the power cycle thermal input rating times the solar multiple.

**Actual aperture**

The actual aperture area is a calculated value equal to the product of the actual number of loops and the reflective aperture area of a single loop, as calculated above.

**Actual solar multiple**

The actual solar multiple is calculated using the thermal power produced at design with an aperture area equal to the Actual aperture calculated value, the design-point ambient and irradiation conditions, and the thermal power requirement of the power cycle. 

The actual solar multiple may differ from the user-specified input value if the sum of the thermal output provided by the integer number of loops matches the product of the solar multiple (user input) and the power cycle thermal requirement.

**Field thermal output**

Thermal energy output from the solar field at design conditions. This value is calculated based on the actual aperture area of the field and the estimated loop conversion efficiency at design.

.. image:: ../images/EQ_LF_Q-sf-design.png
   :align: center
   :alt: EQ_LF_Q-sf-design.png

.. _dslf-landarea:

Land Area
~~~~~~~~~

.. include:: ../includes/snip_land_area_trough.rst

Mirror Washing
~~~~~~~~~~~~~~

SAM reports the water usage of the system in the results based on the mirror washing variables. The annual water usage is the product of the water usage per wash and 365 (days per year) divided by the washing frequency.

**Water usage per wash**
  The volume of water in liters per square meter of solar field aperture area required for periodic mirror washing.

**Washes per year**
  The number of washes per year.

Field Control
~~~~~~~~~~~~~

**Min single loop flow rate, kg/s**
  The minimum allowable steam flow rate in a single loop of the solar field. During night-time or low-insolation operation, the field will recirculate at a mass flow rate equal to this value. The minimum solar field mass flow rate is equal to the Min single loop flow rate times the actual number of loops in the field.

**Freeze protection temperature, °C**
  The temperature below which auxiliary fossil backup heat is supplied to the solar field to prevent water from freezing in the equipment. You should set this value such that a reasonable margin exists between activation of the electric heat trace freeze protection equipment and the actual freezing point of water.

**Stow wind speed, m/s**
  The maximum allowable wind velocity before the collectors defocus and enter safety stow position. The solar field cannot produce thermal energy during time steps in which the ambient wind velocity exceeds this limit.

**Solar elevation for collector nighttime stow, deg**
  The solar elevation angle (above the horizon) that sets the operational limit of the collector field in the evening hours. When the solar elevation angle falls below this value, the collector field will cease operation.

**Solar elevation for collector morning deploy, deg**
  The solar elevation angel (above the horizon) that sets the operational limit of the collector field in the morning hours. When the solar elevation angle rises above this value, the collector field will begin operation.

.. _dslf-solarmultiple:

Solar Multiple
~~~~~~~~~~~~~~

.. include:: ../includes/snip_solar_multiple.rst

