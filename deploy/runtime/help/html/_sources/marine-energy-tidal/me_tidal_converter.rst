Tidal Energy Converter
======================

The tidal energy converter (TEC) is characterized by a power curve derived from a table of tide velocities in meters per second (m/s) and electrical power in kilowatts (kW). The power should be the the net electrical power at the TEC device output terminals accounting for power used by onboard electrical systems.

Tidal Turbine Design
--------------------

There are three options for specifying the characteristics of the TEC.

**Choose tidal converter from library**
  Choose a TEC from the Tidal Energy Converter library.

**Import tidal converter**
  Import TEC characteristics from a properly formatted .csv file.

**Design tidal converter**
  Enter physical characteristics of the TEC that SAM uses to calculate the converter power curve.

Choose Tidal Converter from Library
-----------------------------------

The Tidal Energy Converter library contains parameters for reference TEC from the   U.S. Department of Energy (DOE) Reference Model Project (RMP) `Reference Model 1: Tidal Current Turbine <https://energy.sandia.gov/programs/renewable-energy/water-power/projects/reference-model-project-rmp/>`__  .

To choose a converter from the library:

#. Click **Choose tidal converter from library**.

#. Click a TEC in the library table. SAM populates the tidal device power curve with data from the library.

The data for the Tidal Energy Converter library is stored in the Tidal Energy Converters.csv file in the libraries folder of the :ref:`SAM installation folder <installationfolder>`. If you have your own data, use the Import Tidal Converter option rather than editing the library CSV file to ensure you don't lose your data when you remove or install the SAM software.

Import Tidal Converter
----------------------

If you have your own TEC power curve data, you can use it in SAM.

The TEC power curve file format is a simple comma-separated (CSV) text file with two columns and no headers. The first column is the surface current velocity in meters per second (m/s), and the second column is net power output of the TEC in kilowatts (kW) at each velocity.

To import TEC data from a CSV file:

#. Click **Import tidal converter**.

#. In the Tidal Turbine Converter Input table, click **Import** and navigate to the file.

To paste TEC data from the clipboard:

#. Click **Import tidal converter**.

#. Open the .csv file in a spreadsheet program and copy it to your clipboard. The data should consist of two columns of tab-delimited surface velocity in m/s and power in kW values.

#. In the Tidal Turbine Converter Input table, click **Paste**.

The following screenshot shows a TEC power curve file and the resulting power curve imported to SAM.

.. image:: ../images/MHKTidal-wec-power-curve.png
   :align: center
   :alt: MHKTidal-wec-power-curve.png

**Device rated power**
  The device rated power is the maximum power value in the TEC power curve and determines the device and array rated power for cost calculations. SAM automatically determines the device rated power from the power curve data.

Design Tidal Converter
----------------------

You can specify the basic TEC characteristics, and SAM will calculate a TEC power curve.

**Rated power output, kW**
  The nameplate capacity of the TEC in AC kilowatts, equivalent to the maximum value in the TEC power curve. It is calculated as the sum of the products of the resource frequency distribution at a given fluid velocity and the power output at that velocity divided by the capacity factor.

**Rated power per rotor, kW**
  The nameplate capacity of a single rotor of the TEC in AC kilowatts, calculated as the rated power output divided by the number of rotors.

**Target capacity factor, %**
  The target capacity factor sets the maximum value in the TEC power curve, which in effect limits the TEC power output to the maximum value at higher fluid velocities.

**User-defined rotor diameter, m**
  The diameter of a single rotor in meters.

**Number of rotors**
  The number of rotors in the TEC.

**PTO efficiency, %**
  The conversion efficiency of the power take-off (PTO) that converts mechanical energy of the rotors into electricity. Enter a single value, or click |SS_AnnSched-valschedbutton| to specify a value for each velocity from the :doc:`Tidal Resource <me_tidal_resource>` page.

**Maximum Cp**
  The rotor power efficiency. The fraction of the rotor's total available power that the blades can convert to mechanical power. Enter a single value, or click |SS_AnnSched-valschedbutton| to specify a value for each velocity from the :doc:`Tidal Resource <me_tidal_resource>` page.

**Cut-in tidal surface velocity, m/s**
  The minimum fluid velocity at which the TEC generates power.

**Cut-out tidal surface velocity, m/s**
  The maximum fluid velocity at which the TEC generates power. At fluid velocities above the cut-out velocity, the TEC output is zero.

.. |SS_AnnSched-valschedbutton| image:: ../images/SS_AnnSched-valschedbutton.png
