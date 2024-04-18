# Snow Data Processing
SAM has a model for processing snow depth data to estimate snow losses
during a simulation. However, the NSRDB doesn't have snow depth data in
the PSMV3 files (as of 4/18/24) so this script provides an example
of converting USDA NRCS SNOWTEL data into a format usable by SAM.

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
3. If there is such a station click on it, then click on the graph to see the historical data.
4. From the graph page, click on "csv" next to "link to data" on the top. Save the data
to the appropriate folder
5. Open "snowtel depth data process.ipynb", set the environment to step (1) and follow the instructions there
6. Add the data to the appropriate weather file by adding a column with the title "Snow Depth" 
and pasting the output file from step 5 into it.

### SNOTEL data access
SNOTEL data is published in the public domain. See https://catalog.data.gov/dataset/snowpack-telemetry-network-snotel for more information.