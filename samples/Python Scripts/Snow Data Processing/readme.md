# Snow Data Processing
SAM has a model for processing snow depth data to estimate snow losses
during a simulation. However, the NSRDB doesn't have snow depth data in
the PSMV3 files (as of 4/18/24) so this script provides an example
of converting USDA NRCS SNOTEL data into a format usable by SAM.

SNOTEL sites are mostly available in mountanous regions of the Western U.S.
Sites in the Midwest or Northeast are unlikely to find this methodology useful.
For other regions, regional climate centers may be able to provide average snow
depths to assist with loss estimates.

### Requirements:

```
csv
pandas==1.5.3
numpy==1.23.5
ipython==8.12.0
ipykernel==6.19.2
```

## Steps to run:
1. Install the above requirements in a Python environment.
2. Go to https://nwcc-apps.sc.egov.usda.gov/imap and see if there is a relevant station.
3. If there is such a station click on it in the map.
4. Click on the graph to open a page with historical data.
5. From the graph page, click on "csv" next to "link to data" at the top center of the graph. Save the data
to the appropriate folder.
6. Open "snotel depth data process.ipynb", set the environment to step (1) and follow the instructions there. This will create a CSV file with one column of snow depth data.
7. Use a spreadsheet program to add the column of snow depth data to the appropriate weather file with the column heading "Snow Depth."

### SNOTEL data access
SNOTEL data is published in the public domain. See https://catalog.data.gov/dataset/snowpack-telemetry-network-snotel for more information.