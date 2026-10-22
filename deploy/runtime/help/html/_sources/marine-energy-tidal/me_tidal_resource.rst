Tidal Resource
==============

The tidal energy resource is characterized by a probability distribution function of surface tidal current velocities in meters per second.

You can import tidal resource data from a text file, paste it from your computer's clipboard, or type values by hand in the tidal resource table.

The tidal energy resource file format is a simple comma-separated (CSV) text file with two columns and no headers. The first column is the surface current velocity in meters per second (m/s), and the second column is the probability distribution fraction for each velocity.

.. note:: The probability distributions in the CSV file should be fractions, not percentages. SAM displays the values in the probability distribution graph as percentages, but the data in the table should be fractions.

The following screenshot shows a tidal energy resource file with the resulting probability distribution curve plotted in SAM.

.. image:: ../images/MHKTidal-resource-file.png
   :align: center
   :alt: MHKTidal-resource-file.png

To import tidal energy resource data:

* Click Import and navigate to the file.

To paste tidal energy resource data from the clipboard:

#. Select the data in a spreadsheet program and copy it to your clipboard. The data should consist of two columns of tab-delimited surface velocity and probability fraction values.

#. In SAM, on the Tidal Resource page, click **Paste**.