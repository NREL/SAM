
The user-defined power cycle model requires that you provide data from your own power cycle model that describes the cycle's performance for a range of operating conditions. If you do not have such data, you should use the Rankine cycle model.

For a description of the input data requirements, see Neises, T.; Boyd, M. (DRAFT 2021). Description of SAM's CSP User-defined Power Cycle Model (`PDF 235 KB <https://sam.nlr.gov/images/web_page_files/Neises-DRAFT-user-defined-power-cycle-v4-2021.pdf>`__). It is available with other documentation of SAM's CSP models on the SAM website at https://sam.nlr.gov/concentrating-solar-power/csp-publications.html.

If you are modeling a supercritical Carbon dioxide (sCO2) cycle, you can use the sCO2 Cycle Integration :doc:`macro <../reference/macros>` with the` NLR Supercritical Carbon Dioxide (sCO2) Python model <https://github.com/NatLabRockies/SAM/tree/develop/samples/CSP/sco2_analysis_python_V2>`__ to generate input data for the User Defined Power Cycle model.


User-defined Power Cycle Design Parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The cooling parasitic load and water usage parameters are optional. SAM subtracts the cooling power requirement from the power cycle's electric output.

**Cooling system water usage, kg/s**
  The cooling system water mass flow rate at the design point.

**Gross power consumed by cooling system, %**
  The electrical power consumed by the cooling system as a percentage of the power cycle gross output at the design point.

**Gross power consumed by cooling system, MWe**
  The cooling system gross power in megawatts of electricity, equal to the product of the gross cooling power percentage and the power cycle gross output at design.

Performance as a Function of HTF Temperature, HTF Mass Flow Rate and Ambient Temperature
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The input table for user-defined power cycle model defines the relationship between the independent variables for hot HTF temperature, HTF mass flow rate and ambient temperature and the dependent variables for cycle gross electrical output power, thermal input power, electrical consumption for cooling, and water mass flow rate.

The values in the table must use the following structure but may be in any order. For details, see the description of Table 1 in the draft Description of SAM's CSP User-defined Power Cycle Model cited above.

.. image:: /images/SS_MSPT_power-cycle-table-format.png
   :align: center
   :alt: SS_MSPT_power-cycle-table-format.png

Custom Power Cycle Simulations Required to Populate SAM's Data Table

.. note:: The inputs for the table are normalized values, where 1 is the value at the design point. The table uses the following shorthand: **Ẇ cycle** = Power cycle gross output, **Heat in** = Cycle thermal input power, **Ẇ cooling** = Electrical consumption for cooling, **ṁ water** = Water usage.

Importing Cycle Performance Data
--------------------------------

You can import data into the cycle performance table from either a text file a spreadsheet. The image below shows the first few rows of the performance table that you can use as a guide to store the data in a spreadsheet.

.. image:: /images/SS_MSPT_power-cycle-table-in-sam.png
   :align: center
   :alt: SS_MSPT_power-cycle-table-in-sam.png

To store the same data in a text file that SAM can read, the text file should contain comma-separated values and no headers:

.. image:: /images/SS_MSPT_power-cycle-table-in-text.png
   :align: center
   :alt: SS_MSPT_power-cycle-table-in-text.png

To import cycle performance data:

* To import from a text file, click **Import** and navigate to the text file containing the correctly formatted data.

To import from a spreadsheet, select and copy the data in the spreadsheet and click **Paste**.

.. image:: /images/SS_MSPT_power-cycle-udpc-table-import.png
   :align: center
   :alt: SS_MSPT_power-cycle-udpc-table-import.png

SAM displays a summary of the HTF temperature, normalized HTF mass flow rate, and ambient temperature data from the cycle performance data under **Independent variable information calculated from power cycle data in table below**. The summary data is a quick way to verify that the data imported correctly.

Creating a Template for your Performance Data
---------------------------------------------

You can use SAM to generate a template for your data. The template contains the ranges of values you specify for HTF temperature, HTF mass flow rate, and ambient temperature. After you generate the template, you can add your own data for cycle gross output, cycle thermal input power, cycle cooling power, and water usage.

To create a template for your performance data:

#. Click **Enable table inputs and macro**.

#. Type low and high values for HTF temperature in °C, normalized HTF mass flow rate in kg/s, and ambient temperature in °C. 

The design values are from the :doc:`System Design <../csp-power-tower-molten-salt/mspt_system_design>` page, except for ambient temperature.

#. Type the number of parametric values you want to include in the cycle performance table for each of the three parameters. 

In this example, SAM creates 20 values between 500 and 580 °C for the HTF temperature column, 20 values for the normalized HTF mass flow rate, and 20 values for the ambient temperature. This results in a table with 180 rows (20 values per parameter × 3 parameters):

.. image:: /images/SS_MSPT_power-cycle-generate-table-inputs.png
   :align: center
   :alt: SS_MSPT_power-cycle-generate-table-inputs.png

#. Click **Generate New Independent Variable Combinations in Table Below**.

SAM replaces the data in the cycle performance table with the ranges you defined in the **HTF Temp**, **HTF ṁ**, and **Ambient Temp** columns.

SAM also sets the values in the **Ẇ cycle**, **Heat in**, **Ẇ cooling**, and **ṁ water** columns to zero. These are the values you need to replace with data from your own model.

.. image:: /images/SS_MSPT_power-cycle-generate-table-results.png
   :align: center
   :alt: SS_MSPT_power-cycle-generate-table-results.png

#. Click **Export** to export the data to a comma-separated text file. You can then edit the file to add your data and import it back into SAM. Or, click **Copy** to copy and paste the data into a spreadsheet.

Variable Descriptions
---------------------

**Low, design, and high HTF temperature,  °C**
  The low and high hot HTF temperatures define the range of high HTF temperature values in the cycle performance table.

  The design value is defined on the :doc:`System Design <../csp-power-tower-molten-salt/mspt_system_design>`   page.

**Low, design, and high normalized HTF**
  The low and high HTF mass flow rates define the range of HTF mass flow rate values in the cycle performance table.

  These values are normalized to the design point **Cycle design HTF mass flow rate** under **General Design Parameters** above, where 1 is equivalent to the design point value.

**Low, design, and high ambient temperature**
  The low, design, and high ambient temperatures define the range of ambient temperature values in the cycle performance table.

**Cycle Performance Table**
  Columns 1 to 3 are the input parameters hot HTF temperature, HTF mass flow rate and ambient temperature.

  Column 4 is the normalized cycle gross electrical output power from your model.

  Column 5 is the normalized thermal input power from your model.

  Column 6 is the normalized electrical consumption for cooling from your model.

  Column 7 is the water mass flow rate from your model.