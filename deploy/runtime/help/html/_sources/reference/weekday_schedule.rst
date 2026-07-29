Weekday Weekend Schedules
=========================

Weekday and weekend schedule matrices associate time periods with different times of day and months of year for the following applications:

* Time-of-use periods for retail electricity rates

* Time-of-delivery periods for power price multipliers

* Storage dispatch schedules

To use the matrices, draw rectangles on the matrix with your mouse, and type a number with your keyboard for the period that applies to the times represented by the rectangles.

SAM arbitrarily defines the first day of time series data (the first 24 hours for hourly data) to be Monday on January 1, and assigns the remaining days of the year accordingly. SAM assumes that Monday through Friday are weekdays, and weekends are Saturday and Sunday. SAM does not account for holidays or other special days. It also does not account for leap years, or daylight savings time.

.. note:: You must specify a time (or times) in the weekday and/or weekend schedule for each time-of-use, time-of-delivery, or battery dispatch period that you define. For example, for an electricity rate structure with six time-of-use periods, you must define the times that apply to all six periods.

To specify a weekday or weekend schedule:

#. Use your mouse to draw a rectangle in the matrix for the first block of time that applies to the period you are defining.

.. image:: ../images/SS_Schedule-select-block.png
   :align: center
   :alt: SS_Schedule-select-block.png

#. Type the period number, in this example, the number 2.

.. image:: ../images/SS_Schedule-type-value.png
   :align: center
   :alt: SS_Schedule-type-value.png

 


.. note:: The weekday and weekend schedule matrices support numbers between 1 and 36. However the maximum acceptable value depends on the context. For example, the energy charge matrix on the Electricity Rates page supports up to 36 time-of-use periods, the demand charge matrix supports up to 12 periods, and the battery dispatch schedule on the Battery Dispatch page only supports up to 6 periods.


.. note:: To assign the number 10, type a zero. To assign a number greater than 10 and up to 36, use the letters a=11, b=12, c=13, ..., z=36.

#. Repeat Steps 2-4 for each of the remaining periods that apply to the schedule.

For retail electricity rate time-of-use periods, the numbers in the schedules correspond to the numbers in the Period column of the rate tables:

.. image:: ../images/SS_UtilityRate-sample-tou-rate.png
   :align: center
   :alt: SS_UtilityRate-sample-tou-rate.png

For time-of-delivery periods, and dispatch schedules, the color of the squares matches the text of the period number:

.. image:: ../images/SS_Schedule-energy-payment-dispatch.png
   :align: center
   :alt: SS_Schedule-energy-payment-dispatch.png

