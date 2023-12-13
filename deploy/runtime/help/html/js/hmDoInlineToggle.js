/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/		
hmWebHelp.funcs.hmDoInlineToggle = function(tg) {
	
	function showHideInline(tg,show) {
	var text = tg.text(),
		textChopper,
		textCounter = 0,
		textLength = text.length,
		$toggle = $("a#" + tg.attr("id") + "_LINK"),
		$icon = $("img#" + tg.attr("id") + "_ICON");

	if (show) {
		tg.text("");
		tg.show();
		textChopper = setInterval(function(){
			tg.text(tg.text() + text.charAt(textCounter));
			textCounter++;
			if (textCounter == textLength) {
				clearInterval(textChopper);
				return;
			}
		},0);
		if ($icon.length > 0)
		$icon.attr("src",$toggle.attr("data-src1"));
	} else { 
		textChopper = setInterval(function(){
			tg.text(tg.text().substr(0,tg.text().length-1));
			textCounter++;
			if (textCounter == textLength) {
				tg.hide().text(text);
				clearInterval(textChopper);
				return;
			}
		},0);
		if ($icon.length > 0)
		$icon.attr("src",$toggle.attr("data-src0"));
	}
}
	
var currentState = tg.attr("data-state"),
		thisID = tg.attr("id"),
		togID = thisID.substr(0,thisID.indexOf("_")),
		$thisToggle = $("span#"+togID),
		toggleOn = $thisToggle.is(":visible");
	if (currentState === "0" || !toggleOn) {
		showHideInline($thisToggle,true);
		tg.attr("data-state","1");
	} else {
		showHideInline($thisToggle,false);
		tg.attr("data-state","0");
	}
};
