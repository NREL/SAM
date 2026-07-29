Solar Field
===========

To view the Solar Field page, click **Solar Field** on the main window's navigation menu. Note that for the empirical trough input pages to be available, the technology option in the :doc:`Technology and Market <../getting-started/choose_models>` window must be Concentrating Solar Power - Empirical Trough System.

The Solar Field page displays variables and options that describe the size and properties of the solar field, properties of the heat transfer fluid, reference design specifications of the solar field, and collector orientation.

Field Layout
~~~~~~~~~~~~

**Options 1 and 2**
  For Option 1, (solar multiple mode), you specify a value for **Solar Multiple**, and SAM calculates the solar field area and displays it under **Calculated Values** as **Aperture Reflective Area**. In this mode, SAM ignores the solar field area value under **Field Layout**.

  For Option 2 (solar field area mode), you specify a value for **Solar Field Area**, and SAM calculates the solar multiple and displays it under **Calculated Values**. In this mode, SAM ignores the Solar Multiple value under **Field Layout**.

  See :ref:`Solar Multiple <troughempirical-solarmultiple>` for details.

**Distance Between SCAs in Row (m)**
  The end-to-end distance in meters between SCAs (solar collection elements, or collectors) in a single row, assuming that SCAs are laid out uniformly in all rows of the solar field. SAM uses this value to calculate the end loss. This value is not part of the SCA library on the :doc:`SCA / HCE page <troughempirical_collectors_scas>`, and should be verified manually to ensure that it is appropriate for the SCA type that appears on the SCA / HCE page. 

**Row spacing, center-to-center (m)**
  The centerline-to-centerline distance in meters between rows of SCAs, assuming that rows are laid out uniformly throughout the solar field. SAM uses this value to calculate the row-to-row shadowing loss factor. This value is not part of the SCA library, and should be verified manually to ensure that it is appropriate for the SCA type that appears on the :doc:`SCA / HCE page <troughempirical_collectors_scas>`  .

**Number of SCAs per Row**
  The number of SCAs in each row, assuming that each row in the solar field has the same number of SCAs. SAM uses this value in the SCA end loss calculation.

**Deploy Angle (degrees)**
  The SCA angle during the hour of deployment. A deploy angle of zero for a northern latitude is vertical facing due east. SAM uses this value along with sun angle values to determine whether the current hour of simulation is the hour of deployment, which is the hour before the first hour of operation in the morning. SAM assumes that this angle applies to all SCAs in the solar field.

**Stow Angle (degrees)**
  The SCA angle during the hour of stow. A stow angle of zero for a northern latitude is vertical facing east, and 180 degrees is vertical facing west. SAM uses this value along with the sun angle values to determine whether the current hour of simulation is the hour of stow, which is the hour after the final hour of operation in the evening.

Heat Transfer Fluid
~~~~~~~~~~~~~~~~~~~

**Solar Field HTF Type**
  The solar field heat transfer fluid (HTF) absorbs heat as it circulates through the heat collection elements in the solar field and transports the heat to the power block where it is used to run a turbine. Several types of heat transfer fluid are used for trough systems, including hydrocarbon (mineral) oils, synthetic oils, silicone oils and nitrate salts.

  When you choose a heat transfer fluid, SAM populates the minimum HTF temperature variable with that oil's minimum operating temperature value. SAM will not allow the system to operate at a temperature below the minimum HTF temperature. Electric heaters in the system maintain the fluid temperature. SAM accounts for the electric power requirement for heating on the :doc:`Parasitics page <troughempirical_parasitics>`  .

  The remaining heat transfer fluid parameters describe characteristics of the solar field that affect the performance of the heat transfer fluid. The two area-related parameters refer to square meters of solar field area.

.. note:: During the simulation, SAM counts the number of instances that the HTF temperature falls outside of the operating temperature limits in the table below. If the number of instances exceeds 50, it displays a simulation :doc:`notice <../results/notices>` with the HTF temperature and time step number for the 50th instance.

.. note:: If you define a custom fluid, SAM disables the minimum and maximum operating temperature variables and displays zero because it does not have information about the operating limits for the fluid you defined. You can check the time series temperature data in the results to ensure that they do not exceed the limits for your custom fluid.

.. include:: ../includes/snip_htf_properties.rst

**Solar Field Inlet Temp (ºC)**
  Design temperature of the solar field inlet in degrees Celsius used to calculate design solar field average temperature, and design HTF enthalpy at the solar field inlet. SAM also limits the solar field inlet temperature to this value during operation and solar field warm up, and uses this value to calculate the actual inlet temperature when the solar field energy is insufficient for warm-up.

**Solar Field Outlet Temp (ºC)**
  Design temperature of the solar field outlet in degrees Celsius, used to calculate design solar field average temperature. It is also used to calculate the design HTF enthalpy at the solar field outlet, which SAM uses to determine whether solar field is operating or warming up. SAM also uses this value to calculate the actual inlet temperature when the solar field energy is insufficient for warm-up.

**Solar Field Initial Temp (ºC)**
  Initial solar field inlet temperature. The solar field inlet temperature is set to this value for hour one of the simulation.

**Piping Heat Losses @ Design Temp (W/m****\ :sup:`2`\****)**
  Solar field piping heat loss in Watts per square meter of solar field area when the difference between the average solar field temperature and ambient temperature is 316.5ºC. Used in solar field heat loss calculation.

**Piping Heat Loss Coeff (1-3)**
  These three values are used with the solar field piping heat loss at design temperature to calculate solar field piping heat loss.

**Solar Field Piping Heat Losses (W/m****\ :sup:`2`\****)**
  Design solar field piping heat losses. This value is used only in the solar field size equations. This design value different from the hourly solar field pipe heat losses calculated during simulation.

*Solar Field Piping Heat Losses = ( PHLTC3 × ΔT³ + PHLTC2 × ΔT² + PHLTC1 × ΔT ) × Solar Field Piping Heat Losses @ Design T*

*ΔT = Average Solar Field Temperature - Ambient Temp*

*Average Solar Field Temperature = ( Solar Field Inlet Temp + Solar Field Outlet Temp ) ÷ 2*

  Where PHLTC1-3 are the Piping Heat Loss Coefficients you specify, and the temperature value are design point values that you specify as inputs. During the simulation, SAM calculates the actual piping heat losses using simulated field temperatures and the ambient temperature from the weather file you specify on the :doc:`Location and Resource <troughempirical_location_and_resource>`   page.

**Minimum HTF Temp (ºC)**
  Minimum heat transfer fluid temperature in degrees Celsius. SAM automatically populates the value based on the properties of the solar field HTF type, i.e., changing the HTF type changes the minimum HTF temperature. The value determines when freeze protection energy is required, is used to calculate HTF enthalpies for the freeze protection energy calculation, and is the lower limit of the average solar field temperature. SAM assumes that heat protection energy is supplied by electric heat trace equipment.

**HTF Gallons Per Area (gal/m****\ :sup:`2`\****)**
  Volume at 25°C of HTF per square meter of solar field area, used to calculate the total mass of HTF in the solar field, which is used to calculate solar field temperatures and energies during hourly simulations. The volume includes fluid in the entire system including the power block and storage system if applicable. Example values are: SEGS VI: 115,000 gal VP-1 for a 188,000 m2 solar field is 0.612 gal/m2, SEGS VIII 340,500 gal VP-1 and 464,340 m2 solar field is 0.733 ga/m2.

.. _troughempirical-landarea:

Land Area
~~~~~~~~~

.. include:: ../includes/snip_land_area_trough.rst

Solar Multiple (Design Point)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note:: The ambient temperature, direct normal radiation, and wind velocity reference variables differ from the hourly weather data that SAM uses for system output calculations. SAM uses the reference ambient condition variables to size the solar field. Hourly data from the weather file shown on the :doc:`Location and Resource <troughempirical_location_and_resource>` page determine the solar resource at the site.

Calculated Values
-----------------

The two calculated values variables depend on whether you choose Option 1 or Option 2 to specify the solar field size. When you choose Option 1, the solar multiple calculated value is equal to the value you specify under **Field Layout** and SAM calculates the aperture reflective area. When you choose Option 2, the aperture reflective area is equal to the Solar Field Area value you specify, and SAM calculates the solar multiple.

**Solar Multiple**
  The solar field area expressed as a multiple of the exact reflective area for a solar multiple of 1 (see "Reference Conditions (SM=1)" below). SAM uses the calculated solar multiple value to calculate the design solar field thermal energy and the maximum thermal energy storage charge rate.

*Solar Multiple = Aperture Reflective Area ÷ Exact Aperture Reflective Area at SM=1*

**Aperture Reflective Area (m****\ :sup:`2`\****)**
  The total reflective area of collectors in solar field expressed in square meters. SAM uses this value in the delivered thermal energy calculations. This area is the total collection aperture area, which is less than the mirror area. The solar field area does not include space between collectors or the land required by the power block.

*Aperture Reflective Area = Solar Multiple × Exact Aperture Reflective Area at SM=1*

Solar Multiple Reference Conditions
-----------------------------------

**Ambient Temp (ºC)**
  Reference ambient temperature in degrees Celsius. Used to calculate the design solar field pipe heat losses.

**Direct Normal Radiation (W/m****\ :sup:`2`\****)**
  Reference direct normal radiation in Watts per square meter. Used to calculate the solar field area that would be required at this insolation level to generate enough thermal energy to drive the power block at the design turbine thermal input level. SAM also uses this value to calculate the design HCE heat losses displayed on the :doc:`SCA / HCE page <troughempirical_collectors_scas>`  . The appropriate value depends on the system location. For example, 950 W/m2 is an appropriate value for the Mohave Desert and typical locations under consideration for development in the U.S., and 800 W/m2 is appropriate for southern Spain.

.. note:: Direct Normal Radiation does not represent weather conditions at the site, but is the reference radiation value used to calculate the solar field area when the solar multiple is one. The radiation values used during simulation are from the weather file specified on the :doc:`Location and Resource <troughempirical_location_and_resource>` page.

**Wind Velocity (m/s)**
  Reference wind velocity in meters per second. SAM uses this value to calculate the design HCE heat losses displayed on the :doc:`SCA / HCE page <troughempirical_collectors_scas>`  .

Reference Condition (SM=1)
--------------------------

**Exact Aperture Reflective Area (m****\ :sup:`2`\****)**
  The solar field area required to deliver sufficient solar energy to drive the power block at the design turbine gross output level under reference weather conditions. It is equivalent to a solar multiple of one, and used to calculate the solar field area when the Layout mode is Solar Multiple.

*Exact Aperture Reflective Area = Design Turbine Thermal Input*
 *÷ ( Direct Normal Radiation × Optical Efficiency*
 *- HCE Thermal Losses*
 *- Solar Field Piping Heat Losses )*

**Exact Num. SCAs**
  The exact aperture reflective area divided by the SCA aperture reflective area. SAM uses the nearest integer greater than or equal to this value in the solar field size equations to calculate value of the Aperture Reflective Area variable described above. The exact number of SCAs represents the number of SCAs in a solar field for a solar multiple of one.

*Exact Num SCAs = Exact Aperture Reflective Area ÷ Aperture Area per SCA*

Values from Other Pages
-----------------------

**Aperture Area per SCA (m****\ :sup:`2`\****)**
  SCA aperture reflective area variable from the :doc:`SCA / HCE page <troughempirical_collectors_scas>`  . SAM uses this value in the solar field size equations to calculate the value of the Aperture Reflective Area variable described above.

**HCE Thermal Losses (W/m****\ :sup:`2`\****)**
  Design HCE thermal losses based on the heat loss parameters from the :doc:`SCA / HCE page <troughempirical_collectors_scas>`  . SAM uses this value only in the solar field size equations. This design value is different from the hourly HCE thermal losses calculated during simulation.

**Optical Efficiency**
  Weighted optical efficiency variable from the :doc:`SCA / HCE page <troughempirical_collectors_scas>`  . SAM uses this design value only in the solar field size equations. This design value is different from SCA efficiency factor calculated during the simulation.

**Design Turbine Thermal Input (MWt)**
  Design turbine thermal input variable from the :doc:`Power Block page <troughempirical_power_block>`  . Used to calculate the exact aperture reflective area described above.

Orientation
~~~~~~~~~~~

**Collector Tilt (degrees)**
  The collector angle from horizontal, where zero degrees is horizontal. A positive value tilts up the end of the array closest to the equator (the array's south end in the northern hemisphere), a negative value tilts down the southern end. Used to calculate the solar incidence angle and SCA tracking angle. SAM assumes that the SCAs are fixed at the tilt angle.

**Collector Azimuth (degrees)**
  The azimuth angle of the collector, where zero degrees is pointing toward the equator, equivalent to a north-south axis. Used to calculate the solar incidence angle and the SCA tracking angle. SAM calculates the SCAs' tracking angle for each hour, assuming that the SCAs are oriented 90 degrees east of the azimuth angle in the morning and track the daily movement of the sun from east to west.

.. _troughempirical-solarmultiple:

Solar Multiple
~~~~~~~~~~~~~~

.. include:: ../includes/snip_solar_multiple.rst

