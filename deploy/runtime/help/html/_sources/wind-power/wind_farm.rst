Wind Farm
=========

The Wind Farm page allows you to specify the number of turbines in the project and includes a simple representation of the wind farm layout to estimate wake effect losses that result when upwind turbines interfere with wind flow to downwind turbines.

For a technical description of the wind farm model, see Chapter 8 of Freeman, J.; Gilman, P.; Jorgenson, J.; Ferguson, T. (2014). "Reference Manual for the System Advisor Model's Wind Performance Model." National Renewable Energy Laboratory, NREL/TP-6A20-60570. (`PDF 738 KB <https://docs.nlr.gov/docs/fy14osti/60570.pdf>`__)

System Sizing
~~~~~~~~~~~~~

The system sizing options are only available with the **Wind Resource File** option on the :doc:`Wind Resource <wind_resource>` page. The options are disabled when you run the model with a Weibull distribution for the wind resource.

**Use a single turbine**
  Choose this option to model a project with one wind turbine. For this option, the Turbine Layout options are disabled.

**Specify desired farm size**
  SAM calculates the number of turbines for a given wind farm capacity. For this option, SAM automatically defines a basic turbine layout for the wind farm that you cannot modify.

**Specify number of turbines and farm layout**
  You specify the number of turbines in the project and how they are laid out. SAM assumes that all of the turbines in the wind farm are of the same type and at the same elevation above sea level.

**Desired farm size**
  The desired nameplate capacity of the wind farm when you choose the Specify desired farm size option. 

  SAM displays a message when the capacity you specify is not an even multiple of the nameplate capacity of a single turbine. For example, if the capacity of a single turbine is 1,500 kW and you specify a farm capacity of 5,000 kW, SAM uses three turbines for a farm capacity of 4,500 kW and displays a message telling you that it cannot meet the desired capacity exactly.

**Number of Turbines**
  The calculated number of turbines in the project. The value is either the number you specify under **Turbine Layout** or based on the desired farm capacity you specify.

  When you use the turbine layout generator, for a square, rectangle, or parallelogram shape:

*Turbines in Layout = Turbines per Row × Number of Rows*

  For a triangle or trapezoid shape, SAM assumes that each row has one less turbine than the previous row:

*Turbines in Layout = Turbines in First Row + Turbines in Second Row + Turbines in Third Row*

*+ ... + Turbines in Last Row*

**System Nameplate Capacity**
  Total capacity of the project in kilowatts, equal to the product of the number of turbines in the wind farm and the nameplate capacity of a single turbine (**Rated Output** from the :doc:`Turbine <wind_turbine>` page).

Wake Effects
~~~~~~~~~~~~

The wake effects losses account for the reduction in output of turbines positioned in the wake of other turbines in the wind farm.

 


.. note:: The wake effect model calculates internal wake losses. You can specify additional wake effects losses on the :doc:`Losses <wind_losses>` page.


.. note:: If you are using SAM's wind power model in a coding project in Python or another language, you can access wake model inputs that are not available in SAM's user interface. If you are interested in working with these variables, please `contact us <mailto:sam.support@nlr.gov>`__ for more information.

The following output variables report wake losses. You can find them after running a simulation on the Results page :doc:`Data Tables <../results/data>` tab.

* **Annual internal wake loss (kWh)**

* **Annual internal wake loss percentage (%)**

* **Annual total wake loss percentage (%)**

* **Internal wake loss in kW (kW)**

* **Internal wake loss percent (%)**

**Wake Model**
  SAM allows you to choose from three different wake effect models to estimate the effect of upwind turbines on downwind turbine performance. For details see :ref:`wake effect model <wakeeffect>`  .

.. note:: If you want to include internal wake losses in an uncertainty analysis, choose the **Constant loss** option.

* **Simple Wake Model** uses a thrust coefficient to calculate the wind speed deficit at each turbine due to wake effects of the upwind turbines. This is the original wake effect model used in SAM versions 2013.1.15 and earlier.

* **Park (WAsP)** calculates the wind speed deficit behind each turbine using a decay constant, and calculates the overlap of that wake profile with the downwind turbine to calculate the wind speed at the downwind turbine. This model was originally developed for the `Risø DTU WAsP <https://www.wasp.dk/>`__ wind farm model.

* **Eddy-Viscosity** is similar to the Park model, except that the wind speed deficit behind each turbine is assumed to have a Gaussian shape (there is no decay constant).

* **Constant Loss** models wake loss as a constant percentage reduction in the wind farm output.

**Turbulence Coefficient**
  The ambient turbulence intensity representing variation in wind speed caused by terrain or local thermal effects as air moves across the wind farm. SAM uses this value in the :ref:`wake effect model <wakeeffect>`  .  Wake effects are more significant for a wind farm with a lower turbulence coefficient than for one with a higher coefficient.

  The turbulence intensity is the standard deviation of the wind speed at a short time step divided by the mean wind speed. For smooth terrain such as a flat plain with little vegetation and a low turbulence coefficient, a typical value might be 0.1 (or less over water for offshore wind farms). For a forest or area with air mixing caused by thermal effects with a high turbulence coefficient, a typical value might be 0.5.

Turbine Layout
~~~~~~~~~~~~~~

The Turbine Layout options allow you to specify the parameters of a project with two or more turbines. You must check **Specify number of turbines and farm layout** to make the parameters active.

You can either specify the turbine layout using the turbine and row spacing inputs, or by importing a text file of turbine locations.

**Import wind turbine location data file**
  Use this option to import a text file of turbine locations. When you import the file, SAM displays the turbine locations in the turbine layout map.

  Click **Import turbine layout file** to open the file. SAM looks for a file with the *.csv*   extension with the following format:

*   A text file.

*   The first row is a header that SAM ignores.

*   Rows 2 to up to 502 each contain comma-separated values of x,y coordinates in meters indicating a single turbine's position as it would appear on the turbine layout map.

**Define wind farm using layout generator (below)**
  Use this option to specify the turbine locations using turbine and row spacing inputs.

**Turbine Layout Map**
  A diagram showing the locations of turbines in the field. Each blue dot in the map represents a turbine.

**Turbines per row**
  For the **Square / Rectangle / Parallelogram** shape, **Turbines per row** is the number of turbines in each row.

  For the **Triangle / Trapezoid** shape, **Turbines per row** determines the number of turbines at the base of a triangle or trapezoid. SAM assumes that the number of turbines in each successive row is one less than in the previous row.

**Number of rows**
  Number of rows of turbines in the wind farm.

**Shape**
  Choose the shape defined by lines connecting outermost turbines in the project.

    
 
  To specify a triangle shape, **Number of Rows** should be equal to **Turbines in First Row**. For a trapezoid, Number of Rows should be less than **Turbines in First Row**.
 
  If you specify more rows than there are turbines in the first row, SAM assumes a triangle shape, and sets the number of rows equal to the number of turbines in the first row.

**Turbine Spacing**
  Distance in meters between turbines in each row.

**Row Spacing**
  Distance in meters between rows.

**Offset for Rows**
  The distance in meters between a line drawn through a turbine perpendicular to its row, and a similar line drawn through a turbine in the nearest neighboring row.

**Offset Type**
**Every Other Row** applies the offset distance to alternating rows.

**Each Row** applies the distance to every row.

**Row Orientation**
  The angle west of north of a line perpendicular to the rows of turbines. A value of either zero or 180 degrees means the rows are parallel to the equator.

  You can set the value by either typing a number or dragging the slider with your mouse.

  The compass rose indicates the cardinal directions.