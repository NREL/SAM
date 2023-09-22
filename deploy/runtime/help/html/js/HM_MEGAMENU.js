function bindCloseMenu() {
	$(document).off(hmBrowser.touchstart + '.closemenu, keydown.closemenu').on(hmBrowser.touchstart + '.closemenu, keydown.closemenu', function(event) {
		var currentFocus = document.getElementById("menu-wrapper");
		if (event.type == "keydown") {
			if (![9,13,27].includes(event.keyCode)) return;
			if (event.keyCode == 27) {
				
				let $thisMenu = $(event.target).parents("li").last().find("a.main-menu-entry").first();

				if ($thisMenu.length > 0 && (document.activeElement != $thisMenu[0])) {
					$thisMenu[0].focus();
					return;
					}
			}
			if (event.keyCode == 9) {
				if (!currentFocus.contains(document.activeElement))
				$("ul#main-menu a")[0].focus();
			}
		} // keydown
		
		if (
		(!currentFocus.contains(event.target) && event.type != "keydown") || (
		event.type == "keydown" && event.keyCode == 27
		)) {
			
			if ($(window).width() > 768) { 
				$("ul.mega-menu").removeClass('display');
				}
			$("#main-menu").removeClass('display');
			if ($(window).width() < 769) { 
				document.getElementById("phonemenulink").focus();
				}
			$(document).off(hmBrowser.touchstart + '.closemenu, keydown.closemenu');
		}
	});
}
function menuhandler(el) {
	if (el.id == 'phonemenulink') {
		var mm = $('#main-menu');
		if (!$(mm).hasClass('display')) {
			$(mm).addClass('display');
			bindCloseMenu();
		}
		else {
			$(mm).removeClass('display');
		}
	}
	else {
		var p = el.parentElement;
		$('#main-menu > li').each(function (i, val) {
			var m = $(val).find('ul');
			if ((p == val) && (!$(m).hasClass('display'))) {
				$(m).addClass('display');
				$(m).css('height', 'auto');
				var curHeight = $(m).height();
				$(m).height(0).animate({height: curHeight}, 300);
				bindCloseMenu();
			}
			else {
				
				$(m).animate({height: 0}, 300, function(){
					$(m).removeClass('display');
				});
				
			}
		});
	}
	//event.stopPropagation();
}

/* Initialize menu handlers */
$("a#phonemenulink").on("click", function(){
	menuhandler($(this)[0]);
});
$("img#main-menu-closer").on("click", function(event){
	event.stopPropagation();
	$("a#phonemenulink").trigger("click");
});
