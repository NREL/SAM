PDF / CDF
=========

The PDF/CDF tab shows the probability density function and cumulative distribution function for a single variable.

The probability density function (PDF) is a histogram that shows the distribution of the variable you choose. You can choose from the following options to determine the number of bins in the histogram. The cumulative distribution function (CDF) shows the percentage of data points at or below a certain value. SAM displays the empirical CDF, which is a step function that increases at each data point, and is flat in between. For a large number of data points, this is a very good approximation to the true CDF.

Choose **Exclude Zero Values** to only show non-zero values in the graph. This is useful for data like a system's hourly generation profile that has many night-time hours of no generation. 

.. note:: For the Detailed PV system and other models with night-time consumption, night-time consumption results in a bar that appears to be for zero values but is actually for the relatively small negative night-time consumption values.

A blue line on the graph indicates the position for the **p-value** percentage that you type, and a number under the graph indicates the P-value for the variable being displayed. For example, a P-value of 20% for system power generated is the kW value on the x-axis for a CDF of 80%: Indicating that the system generated that amount of power 20% or more of the time.

For the PDF number of bins:

* **Sturge’s formula** calculates the number of bins as log2(n) + 1, where n is the number of data points. S

* **SQRT (Excel)** uses the same method as Microsoft Exce where the number of bins is the square root of the number of data points.

* **20**, **50**, and **100** set the number of bins to the value you choose.

The scaling options allow you to change the units on the Y axis:

* **Histogram** shows the number of data points in each bin.

* **Scaled histogram** shows each bar height as a percentage of the total number of data points.

To export an image of the graph or the graph data, right-click the graph (control-click on a mouse with one button).