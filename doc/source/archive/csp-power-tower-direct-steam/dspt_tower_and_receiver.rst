Tower and Receiver
==================

The Tower and Receiver page displays variables that specify the geometry of the heat collection system. The receiver model uses semi-empirical heat transfer and thermodynamic relationships to determine the thermal performance of the receiver. This allows the model to represent a wide array of geometries without deviating from a hypothetical reference system.

The model makes several assumptions about the system geometry for external receivers:

* The receiver consists of a discrete number of panels, in multiples of two.

* Each panel in the receiver consists of a set of parallel tubes in thermal contact that share a common steam header.

* The panel tubing is vertical and the heat transfer fluid flows through each sequential panel in a serpentine pattern (up one panel and down the adjacent panel). 

* The number of tubes per panel is a function of the Number of Panels, Receiver Diameter, and Tube Outer Diameter variables.

The steam tower receiver is composed of three receiver sections: a boiler, superheater and reheater. The recirculating boiler accepts feedwater from the power cycle and generates a two-phase (boiling) flow at a user-specified quality. The dry steam from the boiler then passes through a superheater where flux heats the steam to a temperature that you specify. SAM also models a reheat loop, where steam from the high pressure turbine is redirected through a dedicated portion of the receiver and reaches a user-specified target temperature before passing through the remainder of the power cycle.

SAM allows you to choose from two steam flow patterns through the receiver. The following diagram shows Option 1, where steam flows through the receiver from north to south:

.. image:: ../images/IMG_TowerSteam-ReceiverFlow.png
   :align: center
   :alt: IMG_TowerSteam-ReceiverFlow.png

The configuration of multiple receiver sections on the tower requires a strategy to allocate flux from the field to each of the receiver sections. The strategy developed for the direct steam power tower employs a simplifying assumption that the flux from the field can be allocated to any one of the receiver sections at any time. SAM uses an iterative procedure to solve for the flux distribution on the receiver for each simulation time step that maintains the boiler outlet quality and steam temperature requirements in the superheater and reheater sections. The iterative procedure progresses as follows:

* First, a portion of the total flux is assigned to the boiler and superheater. A fraction of this portion is then assigned to the boiler and the steam mass flow rate that results in the target outlet quality is calculated. 

* The outlet temperature of the superheater is calculated based on the guessed incident flux and the steam conditions exiting the boiler. If the calculated temperature does not meet the target, flux is redistributed between the boiler and superheater until the required steam outlet temperature is achieved. 

* Once the superheater outlet temperature is resolved, SAM determines the reheater performance using the remaining un-apportioned flux. If the calculated outlet temperature does not match the target, then the portion of total flux assigned to the boiler and superheater is readjusted, and the process is repeated until the target reheater outlet temperature is met.

Direct Steam Receiver
---------------------

**Receiver diameter, m**
  Total diameter in meters of the receiver. The distance from center of the receiver to center of a receiver panel. The width of a single panel is the circumference of receiver divided by number of panels.

**Receiver height, m**
  Height in meters of the receiver panels.

**Number of groups of two panels**
  The number of pairs of receiver panels.

**Number of panels**
  Number of vertical panels in the receiver. The number of panels must be divisible by two to accommodate the available flow patterns.

**Coating emittance**
  Average receiver-temperature weighted thermal emittance of coatings on boiler, superheater and reheater tubes. This value is used in the calculation of radiation losses to the surroundings.

**Coating Absorptance**
  Solar-weighted absorptance of coatings on boiler, superheater, and reheater tubes: The fraction of incident radiation absorbed by the receiver in the solar spectrum.

Boiler
------

**Boiler height, m**
  Height in meters of the boiler section of the receiver.

**Maximum boiler flux, kWt/m²**
  The upper limit of the reflected irradiation from the solar field incident on the boiler at any point. For systems optimized with SolarPILOT, SAM ensures that the optimal receiver size and heliostat positions result in a receiver flux that does not exceed this value.

**Outside diameter of boiler tubes, m**
  The outer diameter in meters of the individual parallel boiler tubes that carry the water/steam through the boiler panels.

**Thickness of boiler tubes, m**
  The thickness in meters of the boiler tube walls.

**Boiler tube material**
  The material of the boiler tubes. Stainless-steel and T-91 steel are available in the current version of SAM. The tubing material properties and geometry are used to calculate the heat transfer behavior of the receiver.

**Target boiler output steam quality**
  The quality (or mass-based vapor fraction) of steam at the boiler exit. The model will ensure that this quality is reached within a convergence tolerance whenever the receiver is operating. Since a recirculating boiler is modeled, only qualities between 0.25 and 0.75 should be evaluated.

**Reference boiler efficiency**
  Estimate of the thermal efficiency of the boiler at design conditions. Note that this value is not used in the annual hourly performance calculation but is only used to help size the boiler during design.

Superheater
-----------

**Superheater height, m**
  Height in meters of the superheater section of the receiver.

**Maximum superheater flux, kWt/m²**
  The upper limit of the reflected irradiation from the solar field incident on the superheater at any point. For systems optimized with SolarPILOT, SAM ensures that the optimal receiver size and heliostat positions result in a receiver flux that does not exceed this value.

**Outside diameter of superheater tubes, m**
  The outer diameter in meters of the individual parallel superheater tubes that carry the steam through the superheater panels.

**Thickness of superheater tubes, m**
  The thickness in meters of the superheater tube walls.

**Superheater material**
  The material of the superheater tubes. Stainless-steel and T-91 steel are available in the current version of SAM. The tubing material properties and geometry are used to calculate the heat transfer behavior of the receiver.

**Superheater outlet temperature set point, °C**
  The temperature of the steam at the superheater exit. The model will ensure that this temperature is reached within a convergence tolerance whenever the receiver is operating.

**Reference superheater efficiency**
  Estimate of the thermal efficiency of the superheater at design conditions. Note that this value is not used in the annual hourly performance calculation but is only used to help size the superheater during design.

Reheater
--------

**Reheater height, m**
  Height in meters of the reheater section of the receiver.

**Maximum reheater flux, kWt/m²**
  The upper limit of the reflected irradiation from the solar field incident on the reheater at any point. For systems optimized with SolarPILOT, SAM ensures that the optimal receiver size and heliostat positions result in a receiver flux that does not exceed this value.

**Outside diameter of reheater tubes, m**
  The outer diameter in meters of the individual parallel reheater tubes that carry the steam through the reheater panels.

**Thickness of reheater tubes, m**
  The thickness in meters of the reheater tube walls.

**Reheater material**
  The material of the boiler tubes. Stainless-steel and T-91 steel are available in the current version of SAM.

**Reheater outlet temperature set point, °C**
  The temperature of the steam at the reheater exit. The model will ensure that this temperature is reached within a convergence tolerance whenever the receiver is operating. Note that this value is not used in the annual hourly performance calculation but is only used to help size the reheater during design.

**Reference reheater efficiency**
  Estimate of the thermal efficiency of the reheater at design conditions.

Flow
----

**Flow Pattern**
  The direction of flow of steam through the receiver. Choose Option 1 if steam flows through the receiver from north to south. Choose Option 2 if the steam flows in the opposite direction, from south to north.

Tower
-----

**Tower height, m**
  The optical height in meters of the tower structure from the heliostat pivot point to the vertical center of the receiver.

Thermal Design and Operation
----------------------------

**Solar multiple**
  The solar multiple represents the ratio of the thermal power produced by the solar field at design conditions to the thermal power required by the power cycle at design. The optimal solar multiple will typically be higher for molten salt tower systems with thermal storage, though direct steam systems without storage may also have an optimal solar multiple greater than one.

**Min fraction of design thermal power**
  The minimum allowable fraction of the design thermal power transferred to the steam by the receiver.

**Receiver design thermal power, MWt**
  Product of solar multiple and power cycle design thermal power on the Power Cycle page.

  Note that this value may not correspond with the realized thermal power output from the receiver during the performance simulation if the user-specified solar multiple deviates from the ratio of power produced by the heliostat field/receiver to the power cycle input requirement. For more information, refer to the Solar Multiple input reference notes.

**Receiver startup delay time, hr**
  The minimum time in hours required to start the receiver. The receiver starts whenever the radiation incident on the field in the previous hour is zero, and there is sufficient thermal energy in the current hour to meet the thermal design requirement. SAM calculates the start up energy as the product of the available thermal energy, startup delay time, and startup delay energy fraction.

**Receiver startup delay energy fraction**
   Energy required to startup receiver calculated as hours of design point operation. 

**Heat loss factor**
  A receiver heat loss adjustment factor that can be used to adjust the receiver thermal losses as appropriate for the receiver design under consideration. The default value is 1, corresponding to no correction of the heat loss correlations implemented in the annual hourly performance model. The calculated receiver heat loss is the sum of convection and radiation losses from the receiver, reported in the hourly results as rec_conv_loss and rec_rad_loss, respectively.