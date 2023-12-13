/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/
hmWebHelp.funcs.tapImage = function(elem) {		
		var firstTap = new Date().getTime();
		var xPos, yPos;
		
		function showImage(speed) {
		document.querySelector('meta[name="viewport"]').content = 'user-scalable=yes, initial-scale=1.0, maximum-scale=5.0, minimum-scale=0.5, width=device-width';
		yPos = $("div#hmpagebody_scroller").scrollTop();
		hmWebHelp.funcs.doPagePos.getPos();
		xPos = 0;
			$("div#tapImageWrapper").fadeIn(speed, function(){
			});
			$("div#helpwrapper").fadeOut(speed);

			}
		function hideImage(speed) {
			document.querySelector('meta[name="viewport"]').content = 'width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no, minimum-scale=1';
			$("div#helpwrapper").fadeIn(speed, function(){
			hmWebHelp.funcs.doPagePos.setPos();
			});
			$("div#tapImageWrapper").fadeOut(speed);
			}
		function closeTap() {
			var thisTap = new Date().getTime();
			var dTap = thisTap - firstTap;
				if (dTap > 120 && dTap < 500)
				{
				hideImage(300);
				}
			firstTap = thisTap;
			}

		var $I = $("div#tapImageWrapper");

		if ($I.length > 0)
			{
				$("img#tapImage").attr("src",$(elem).attr("src")).attr("width",$(elem).attr("width"));
				showImage(300);
			} else {
		var newElem = '<div id="tapImageWrapper" style="position: absolute; top: 0; left: 0; background-color: #ffffff; z-index: 10000"><img src="./images/ZoomClose.png" id="closeTapImage" width="32" height="32" border="0" style="position: absolute; top: 5px; left: 5px;" /><img id="tapImage" src="'+ $(elem).attr("src") + '" width="'+ $(elem).attr("width")+'" height="auto" style="max-width: 100%" /></div>';
		
		$("body").append(newElem);
		showImage(300);
		$("img#closeTapImage").on(hmBrowser.touchstart, function(event) {
			var ev = event.originalEvent;
			hideImage(300);
			});
		$("div#tapImageWrapper").on(hmBrowser.touchstart, function(event){
			var ev = event.originalEvent;
			closeTap(ev);
			});
			
			}

	};
