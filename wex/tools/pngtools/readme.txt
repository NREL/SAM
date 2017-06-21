Tools for working with .png images and wxWidgets

1. Remove all ancilliary junk in the png file and strip it down:
		pngcrush.exe -rem alla -d <output_folder> *.png
		
2. Convert to C language data array:
		png2c.py image.png > image.cpng

		
Updated 25 july 2013, aron dobos

Reference:
http://www.geenat.com/?p=233
http://pmt.sourceforge.net/pngcrush/

note: binary pngcrush.exe compiled 7/25/2013 using mingw32 gcc