Cash Flow
=========

The **Cash flow** tab displays the project cash flow calculated by the financial model.

To see details of the cash flow calculation in a Windows version of SAM, you can use Send-to-Excel with Equations to export inputs from your SAM file to an Excel workbook that replicates SAM's cash flow calculations as Excel formulas. This workbook approximates SAM's calculations and will give slightly different results than SAM, but is a useful way to explore how SAM's financial models work.

.. note:: Send-to-Excel with Equations only works for the Windows version of SAM. If you are working with a Mac or Linux version of SAM, you can download the cash flow workbooks from the SAM website. See the page for the financial model you are using listed under "Financial Models" at https://sam.nlr.gov/financial-models.html.

See :doc:`Annual Electricity to/from Grid <../performance-metrics/mtp_annual_energy>` for a description of the electricity line items in the cash flow.

To view the project cash flow:

* On the Results page, click the **Cash flow** tab.

.. image:: ../images/SS_Results-ButtonsCashFlow.png
   :align: center
   :alt: SS_Results-ButtonsCashFlow.png

SAM offers three options for exporting cash flow data:

**Copy to clipboard**
  Copies the table to your clipboard. You can paste the entire table into a word processing document, spreadsheet, presentation or other software.

**Save as CSV**
  Saves the table in a comma-delimited text file that you can open in a spreadsheet program or text editor.

**Send to Excel (Windows only)**
  Creates an Excel workbook that contains the data from the cash flow, but no formulas.

**Send to Excel with Equations**
  (Windows only) exports the cash flow data to an existing Excel workbook that contains formulas to illustrate how SAM's internal cash flow calculations work. The workbooks are in the \runtime\spreadsheets folder of your :ref:`SAM installation folder <installationfolder>`  .

.. note:: SAM makes cash flow calculations internally during the simulation. It does not use Excel to make the calculations. The two **Send to Excel** options are to help you analyze the cash flow data and understand how SAM's internal calculations work. 

.. note:: You can also display cash flow variables on the :doc:`Data Tables <data>` tab of the Results page.