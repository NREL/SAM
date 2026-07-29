Location and Resource
=====================

.. include:: ../includes/snip_location_resource.rst

Download Weather Files
~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_download_weather_files.rst

Choose Weather File
~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_choose_weather_file.rst

Weather Data Information
~~~~~~~~~~~~~~~~~~~~~~~~

.. include:: ../includes/snip_weather_data_information.rst

Albedo - Sky Diffuse Model - Irradiance Data (Advanced)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The albedo, sky diffuse model, and irradiance data inputs are advanced inputs for the detailed photovoltaic model that you can ignore unless you have a reason to change them. These inputs are not available for the PVWatts model, except for some albedo options, which are under **Advanced Inputs** on the :doc:`System Design <../pvwatts/pvwatts_system_design>` page.

.. note:: Use the default **DNI and DHI** and **Perez** options unless you have a reason to change them.

**DNI:** Direct normal irradiance, sometimes called beam normal irradiance is the amount solar radiation per unit area that reaches a surface that is normal to the rays of solar radiation from the sun.

**DHI:** Diffuse horizontal irradiance is the solar radiation per unit area that reaches a horizontal surface from the sky dome, but not directly from the sun. DHI does not include solar energy reflected from the ground. SAM accounts for that separately using the albedo input described above.

**GHI:** Global horizontal irradiance is the total solar radiation per unit area that reaches a horizontal surface

**POA:** Plane of array, POA irradiance is equivalent to incident irradiance, and is the total solar radiation per unit area that reaches the surface of the photovoltaic array.

.. _albedo:

Albedo
------

SAM uses the albedo (also called ground reflectance) to make a small adjustment to the amount of solar irradiance incident on the array to represent diffuse irradiance reflected onto the array from the ground. SAM also uses albedo to calculate irradiance incident on the rear side of bifacial modules.

Albedo values must be greater than zero and less than one, where zero represents completely non-reflective ground and 1 represents completely reflective ground.

For most analyses, you can use the default albedo value of 0.2,   which is reasonable for grassy ground. A value of 0.6 would be reasonable for snow-covered ground.

After running a simulation, you can see the time series albedo data that SAM uses for simulations on the Results page, and the spatial data on the Spatial tab of the results page.

"Uniform" albedo data represents the albedo that is the same for the entire ground under the array. "Spatial" albedo data represents albedo that is different under and between rows in the array.

**Use monthly uniform albedo values**
  Choose this option to specify albedo values by month.

  Click **Edit Values** to edit monthly albedo values for each month.

**Use monthly spatial albedo values**
  Choose this option when you have spatial albedo data by month. The option is available for fixed, one-axis, and seasonal-axis tracking options on the System Design page.

  Each column in the spatial albedo matrix represents a rectangular strip of ground under one row of the array and the space between one row and its adjacent row. The leftmost column represents the row closest to the equator. Each row represents a month. The diagram changes with the tracking option on the System Design page and indicates how the matrix represents the ground:

  For fixed arrays or arrays with seasonal tilt adjustments, the rectangle is defined by the bottom edges of modules in adjacent rows.

  .. image:: ../images/ALBEDO_spatial-fixed.png
     :align: center
     :alt: ALBEDO_spatial-fixed.png

  For arrays with one-axis tracking, the rectangle is defined by the tracker posts in adjacent rows.

  .. image:: ../images/ALBEDO_spatial-one-axis.png
     :align: center
     :alt: ALBEDO_spatial-one-axis.png

  You can specify up to 10 rectangular strips: Use your mouse to select cells in the matrix to represent the strip, and type a number between 0 and 9 for each strip. Then for each strip type an albedo value under **Albedos**.

  To see the resulting albedo, ground irradiance, and rear side irradiance, click the :doc:`Spatial <../results/spatial>`   tab on the Results page after running a simulation.

**Use uniform albedo in weather file if it specified**
  Check this option if you want SAM to use hourly or subhourly albedo data from the weather file instead of the monthly albedo values. You should also specify monthly albedo values because SAM uses those values for any time steps that have invalid albedo values: For each time step in the simulation, SAM checks the value in the albedo column of the weather file to see if it is between zero and one. If it is within that range, SAM uses that value for the albedo in that hour. If the value in the weather file is outside of that range for a given time step, then SAM uses the appropriate monthly albedo input value for that hour.

  If you want to add albedo data to a weather file, you can edit the file using a text editor or spreadsheet software. See the :doc:`SAM CSV format description <../weather-file-formats/weather_format_sam_csv_solar>`   for details.

Diffuse Sky Model
-----------------

SAM's detailed photovoltaic model uses DNI and DHI data with sun and subarray angles to calculate the irradiance incident on each subarray. Calculating the incident direct component from the DNI is straightforward, but there are several methods for estimating the incident diffuse component from DHI. The incident diffuse component includes both ground-reflected diffuse irradiance, and sky diffuse irradiance from the sky dome outside of the sun's circle. SAM allows you to choose the method it uses to convert DHI data to incident sky diffuse irradiance.

The isotropic model tends to under-predict the global radiation on a tilted surface, and is included as an option for analysis comparing SAM results with those from other models using this approach. The HDKR and Perez methods provide comparable estimates of the incident diffuse irradiance.

For more details about these methods, see the photovoltaic reference manual, which is available for download from the `SAM website <https://sam.nlr.gov/photovoltaic/pv-publications.html>`__.

**Isotropic**
  The isotropic method assumes that diffuse radiation is uniformly distributed across the sky, called isotropic diffuse radiation.

**HDKR**
  The Hay-Davies-Kluchr-Reindl combination method accounts for the increased intensity of diffuse radiation in the area around the sun, called circumsolar diffuse radiation, in addition to isotropic diffuse radiation.

**Perez**
  The Perez method is the default value and is best for most analysis. It accounts for horizon brightening, circumsolar and isotropic diffuse radiation using a more complex computational method than the Reindl and Hay and Davies methods.

Weather File Irradiance Data
----------------------------


A weather file in :doc:`SAM CSV format <../weather-file-formats/weather_format_sam_csv_solar>` for the photovoltaic performance models must contain at least two columns for the solar irradiance components or a single column for plane-of-array (POA) irradiance. The weather file may contain columns for all three irradiance components in addition to POA data. SAM determines which columns to use for the simulation based on the Weather File Irradiance Data options you choose.

Unless you choose one of the POA options, SAM's detailed photovoltaic model always requires the DNI and DHI components to calculate the irradiance incident on each subarray. When you choose the **DNI and GHI** or **GHI and DHI** options, SAM calculates values for the missing irradiance component from the two components that you specify, even if the weather file contains data for the missing component. When you choose a POA option, SAM bypasses the incident irradiance calculations.

**DNI and DHI**
  SAM reads the direct normal irradiance (beam) and diffuse horizontal irradiance data from the weather file. For this option, SAM calculates incident irradiance using the DNI and DHI data from the weather file without any additional calculations. This is the default option, and is best for most analyses because it minimizes the number of irradiance calculations. 

**DNI and GHI**
  SAM reads the direct normal irradiance (beam) and global horizontal irradiance (total) data from the weather file, and calculates the diffuse horizontal irradiance values for simulations. SAM calculates the incident irradiance using the DNI data from the weather file and the calculated DHI data.

**GHI and DHI**
  SAM reads the global horizontal irradiance (total) and diffuse horizontal irradiance data from the weather file, and calculates direct normal irradiance values for simulations. SAM calculates the incident irradiance using DHI data from the weather file and the calculated DNI data.

**POA from reference cell**
  Use this option if your weather file contains irradiance data measured in the plane of the array by a reference photovoltaic cell that has the same optical characteristics as the cells in the array. SAM assumes that the POA data accounts for angle-of-incidence (AOI) effects and removes the calculation of AOI-related reduction in incident DNI from the performance model.

**POA from pyranometer**
  Use this option if your weather file contains irradiance data measured in the plane of the array by a pyranometer. SAM uses a POA decomposition model to calculate the DNI and DHI components of the POA irradiance for angle-of-incidence effect calculations.

  .. note:: For a technical description of the POA option, see Freeman (2016) Using Measured Plane-of-Array Data Directly in Photovoltaic Modeling: Methodology and Validation, available from https://sam.nlr.gov/photovoltaic/pv-publications.

     When you choose a POA option, SAM uses an irradiance decomposition model to calculate DNI and DHI values, which it reports in the results for your reference. It only uses the calculated DNI and DHI values under the following conditions:

     * With the **POA from pyranometer** option to calculate a reduction in DNI due to angle-of-incidence effects.

     * When you enable external shading, SAM applies the beam and diffuse irradiance shading factors to the calculated DNI and DHI values, and then calculates the irradiance incident on the subarray to account for shading.

     * When you use the CEC module model with the heat transfer method for temperature correction.

     When you use POA data, be careful to check that your array orientation, shading, soiling, and snow model inputs are consistent with your POA data. If your system has more than one subarray, SAM uses the POA data for each subarray. This requires that all subarrays have the same orientation and tracking, but SAM does not enforce this requirement. SAM also allows you to enable shading, soiling, and the snow model with POA data. If the irradiance data already accounts for these effects, you should disable those inputs.

Irradiance Data in Results
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note:: If your weather file contains data for all three irradiance components, the value of the calculated third component that SAM uses for the simulation may differ from the value in the weather file.

You can see the calculated data on the Results page after running a simulation, for example, on the **Data tables** tab and **Time series** tab:

**Irradiance GHI/DNI/DHI/POA from weather file (W/m2)**
  The data in the weather file. If the column does not exist in the weather file, SAM reports the values in the results as NaN (not a number).

**Irradiance GHI/DNI/DHI calculated (W/m2)**
  The values of the third irradiance component that SAM calculates from the other two in the weather file. For example, if you choose the **DNI and GHI** option, you should see **Irradiance DHI calculated** in the results. SAM uses the following equations for the calculated values, where *z* is the sun zenith angle:

  *GHI = DHI + DNI × cos(z)*

  *DHI = GHI - DNI × cos(z)*

  *DNI = ( GHI - DHI ) ÷ cos(z)*

  For the POA options, SAM calculates the DHI and DNI values from the POA data in the weather file using the method described in Marion, B. (2015) “A model for deriving the direct normal and diffuse horizontal irradiance from the global tilted irradiance.” Solar Energy, vol. 122, pp. 1037-1046.

**Subarray n POA total irradiance after shading and soiling (W/m2)**
  The total irradiance incident on each subarray *n*.
