/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/
function hmFS() {

		var baseSize=null, changeSize=null, testBaseSize=null, fontOffset = 0;
		
		function doFontSize(args) {
			
			function doBase(fOffset) {
				if (typeof fOffset === "string") fOffset = parseInt(fOffset,10);
				baseSize = hmDevice.baseFontSize * fOffset;
				changeSize = baseSize;
				doFS();
			}
			
			function setBaseSize(dv){
				
				switch (dv) {
				case "phone":
					doBase(1.0,10);
					break;
				case "tablet": 
					doBase(1.0,10);
					break;
				case "desktop":
					doBase(1,10);
					break;
				default:
					doBase(1,10);
				}
				
				} // setBaseSize
		
		function globalFontSize (mode) {
			hmWebHelp.extFuncs('fontSize',mode);
			xMessage.sendObject("hmnavigation", {action: "callfunction", fn: "extFuncs", fa: ['fontSize',mode]});
			xMessage.sendObject("hmsearch", {action: "callfunction", fn: "extFuncs", fa:['fontSize',mode]});
		}
			
			function doFS()	{ 
				var global = false, mode;
				if (typeof args == "object") {
					global = args[1];
					mode = args[0];
				} else mode = args;
				if (global && global=="global") {
					globalFontSize(mode);
					return;
				} 
					else {
				if (mode) {
					if (changeSize < hmDevice.maxFontSize) {
						if (changeSize + 5 <= hmDevice.maxFontSize)
							changeSize+=5;
						else
							changeSize = hmDevice.maxFontSize;
						}
				} else {
					if (changeSize > hmDevice.minFontSize) {
						if (changeSize - 5 >= hmDevice.minFontSize)
							changeSize-=5;
						else
							changeSize = hmDevice.minFontSize;
						}

				}
				document.getElementsByTagName("html")[0].style.fontSize = changeSize + "%";
				if (pageName == "mainPage") {
					sessionVariable.setPV("hmFontSize",changeSize.toString());
					setTimeout(function() {
					if (!hmDevice.phone)
						hmWebHelp.resizePanes(hmpage.FnavWidth());
					},100);
					} 
				}
			} // doFS()
		
			if (baseSize===null)
			{
			setBaseSize(hmDevice.device);
			} else {
			doFS();
			}
		
		} // doFontSize()
			
			return doFontSize;

		}
if (typeof hmWebHelp != "undefined") {
	hmWebHelp.funcs.fontSize = hmFS();
	}
	else {
		funcs.fontSize = hmFS();
	}
