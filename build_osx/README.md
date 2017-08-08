# Mac Build Files

See the [wiki](https://github.com/NREL/SAM/wiki/Mac-Build-Instructions) for Mac build instructions using these files.

- ```copy_runtime.py```: Python script used by the makefile to copy files that the compiled program uses. Runtime files include weather files, user interface configuration files, data library files, etc.

- ```Info_SAM.plist```: Property list in XML format with properties and configuration settings for SAM.

- ```Info-webupd.plist```: Property list in XML format with properties and configuration settings.

- ```makedmg.sh```: Shell script for making a disk image (DMG) after you build SAM.

- ```SAM.icns```: SAM icon image.

- ```webupd.icns```: Old icon image

- ```wxconfigure.macos64```: Contains a configuration command that you run during the Mac build process.

