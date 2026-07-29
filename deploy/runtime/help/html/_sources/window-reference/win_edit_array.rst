Edit Array
==========

The Edit Array window allows you to enter a single column of data. You can also import data from a text file, or export it.

Depending on the data you are entering you may see one of the following options:

**Change time step**
  Click change time step to enter the simulation time step in minutes: 60 for hourly time steps, 15 for 15-minute time steps, etc.

**Hourly Values (8760)**
  The input requires one column of 8760 values.

.. include:: ../includes/snip_time_convention_note.rst

**Number of values**
  Click Number of values to set the number of rows in the table.

You can copy data from your computer's clipboard or import it from a text file.

**Copy**
  Click **Copy** to copy the data in the table to your computer's clipboard. You can then paste the data into a spreadsheet or text file.

**Paste**
  Click **Paste** to paste time series data from your computer's clipboard into the table. For example, for hourly data, you could copy a column of 8,760 values from a spreadsheet or text file and paste it into SAM.

**Import**
  Click **Import** to import time series data from a properly formatted text file. The file should contain one column of data, with a header in Row 1. SAM ignores the data in Row 1. To see an example of the file format, click **Export** and open the resulting file in a text editor.

**Export**
  Click **Export** to export data from the table to a text file. Be sure to include a file extension (.txt, .csv, etc.) when you save the file.