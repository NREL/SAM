# SAM Sample Files

The `samples` folder contains example files to use with the System Advisor Model (SAM).

> **Note.** To download a sample file, click the file name, and then click the **Download** button above the box displaying the file contents. Right-clicking the file name in the list to save it will download an HTML file instead of the file itself.

## 3D Shade LK Scripts (.lk)

These are LK scripts that work from the script editor in the 3D Shade Calculator to draw objects in the scene, import and export .s3d files, generate shading data, etc.

## Excel Exchange

Files demonstrating how to use SAM's Excel Exchange feature to read SAM input values from an Excel file or write SAM input values to Excel. Also includes an LK script that replicates the feature.

Excel Exchange only works for Windows versions of SAM. Mac and Linux users can use the LK script.

## LK Scripts for SAM (.lk)

These LK scripts run from the script editor in SAM.

### To use a sample LK script:

1. Download the script to your computer as an .lk file.

2. Start SAM and open or create a .sam file.

3. On the <strong>File</strong> menu, click <strong>Open script</strong> and open the .lk file.

4. Click <strong>Run</strong> at the top of the script editor.

## 3D Shade Scene Files (.s3d)

These are files created with SAM's 3D Shade Calculator to show how to use shade objects to represent things like buildings, trees, roof protrusions.

### To open a sample shade scene file:

1. Download the .s3d file from the 3D Shade Scene Files folder.

2. Start SAM and open or create a .sam file with a Detailed Photovoltaic, PVWatts, or Solar Water Heating case.

3. Click <strong>Open 3d shade calculator</strong> on the Shading and Snow page (Detailed Photovoltaic model), System Design page (PVWatts model), or Solar Water Heating page (Solar Water Heating model).

4. In the Edit 3D Shading Scene window, click <strong>Import</strong> and navigate to the .s3d file you downloaded in Step 1.

To use the files in a SAM simulation, you will need to add active surfaces to represent a photovoltaic array or solar water heating collector and ensure that the dimensions, orientation and other properties of the active surface matches the parameters in SAM.

See SAM's Help system for information about how to use the 3D Shade Calculator.

## SAM Sample Files (.sam)

These SAM files are annotated with notes and demonstrate how to use SAM to model different scenarios.

## Contributing

We encourange you to add your own sample files to these collections, or to update existing ones.

If you add a SAM file, please use the Notes feature to include descriptions of the inputs and results in the file. See the SAM Help system to read about how Notes work.

If you make any changes to an LK script in this collection, please add a description of the change you made to the comment block at the top of the script. If you add a new script to the collection, please include a comment block at the top of the script with a description of what it does and the version number of SAM you used to develop and test the script.

### To add a sample file to the collection from the GitHub.com website

1. If you do not have a free GitHub account, register for an account.

2. Navigate to the folder for the kind of sample file you are uploading (.sam, .lk, .s3d).

3. Click <strong>Upload files</strong> and navigate to the file.

4. Write a short commit message title describing the file, something like "SAM sample file for a power tower system in Morocco." You can add a longer description if you like in the description box.

5. Commit your changes.
