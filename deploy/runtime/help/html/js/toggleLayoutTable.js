/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/
 function layouttables() {
	
	var	tableID,
		$tableanchor,
		speed = 300,
		$tablewrapper = $("div#ltTableWrapper"),
		$pagewrapper = $("div#helpwrapper"),
		$napkin = $("div#ltTableNapkin");

	function showTable() {
	
	hmWebHelp.funcs.doPagePos.getPos();
		$napkin.append($(tableID).show());
		$napkin.show();

		$pagewrapper.fadeOut(300, function(){
			$tablewrapper.fadeIn(600);
			document.querySelector('meta[name="viewport"]').content = 'user-scalable=yes initial-scale=1.0 maximum-scale=5.0 minimum-scale=1.0 width=device-width';
			});
	}

	function hideTable(instant) {

		$tablewrapper.fadeOut(speed, function() {
			$pagewrapper.fadeIn(speed * 2, function(){
			$tableanchor.after($(tableID));
			$(tableID).hide();
			});
			document.querySelector('meta[name="viewport"]').content = 'width=device-width initial-scale=1 maximum-scale=1 user-scalable=no minimum-scale=1';
			hmWebHelp.funcs.doPagePos.setPos(false);
			});
			
	}

	return function(args) {

			switch(args.action) {
	
			case "show":
			if ($tablewrapper.is(":hidden")) {
				tableID = args.table;
				$tableanchor = $(args.obj);
				hmFlags.layoutTable = true;
				showTable();
				} else {
					hmFlags.layoutTable = false;
					$tablewrapper.hide();
					$pagewrapper.show();
				}
			break;
	
			case "hide":
			if ($tablewrapper.is(":visible")) {
					speed = typeof args.instant == 'undefined' || args.instant ? 0 : 300; 
					hideTable();
				}
			hmFlags.layoutTable = false;
			break;
	
		}
};

		}
$('body').prepend(
	'<div id="ltTableWrapper">' +
	'<img src="./images/ZoomClose.png" id="closeLTable" width="32" height="32" border="0" onclick="hmWebHelp.funcs.toggleLayoutTable({action: \'hide\', instant: false})" />' +
	'<div id="ltTableNapkin">' +
	'</div></div>'
	);
$("div#ltTableWrapper").on(hmBrowser.touchmove, function(){
	$("img#closeLTable").css("opacity","0.2");
		}).on(hmBrowser.touchend, function(){
			$("img#closeLTable").css("opacity","1.0");
		});

hmWebHelp.funcs.toggleLayoutTable = new layouttables();

