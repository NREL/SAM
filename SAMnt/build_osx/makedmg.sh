#!/bin/bash
IMAGENAME="SystemAdvisorModel"
SIZE="900M"
echo "Creating uncompressed disk image:"
hdiutil create -size $SIZE -fs HFS+J -volname $IMAGENAME $IMAGENAME-temp.dmg
echo "Mounting uncompressed disk image:"
hdiutil attach $IMAGENAME-temp.dmg -readwrite -mount required
echo "Ready to copy files... press enter to continue"
read 
echo "Copying..."
cp -R SAM.app  /Volumes/$IMAGENAME
echo "Removing .svn folders..."
find /Volumes/$IMAGENAME -name .svn -print0 | xargs -0 rm -rf
echo "Unmounting uncompressed image"
hdiutil detach /Volumes/$IMAGENAME
echo "Creating compressed image..."
hdiutil convert $IMAGENAME-temp.dmg -format UDZO -imagekey zlib-level=9 -o $IMAGENAME.dmg
echo "Removing temporary image..."
rm $IMAGENAME-temp.dmg
echo "Done."

