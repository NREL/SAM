// Close header drop-down menus
hmWebHelp.closeTopNav = function(instant) {
	
	let currentFocus = document.activeElement,
		$thisMenu = $(currentFocus).parents("li.header").last().find("a").first();
	
	if ($thisMenu.length > 0 && currentFocus != $thisMenu[0]) {
		$thisMenu[0].focus();
		return;
		}
	
	var speed = instant ? 0 : "fast";
	$("ul.topnav li ul.subnav").slideUp(speed);
	$("ul.topnav > span > li > a.current").removeClass("current");
	$("ul.topnav li.header svg use").attr("xlink:href","#menu-closed");
}

// Initialize banner menu system
hmWebHelp.initTopNav = function() {

	var $usermenu = $("nav#header_menu");
	
	// Make sure the menus are visible in broad mode
	$(window).on("resize", function(){
		if ($(window).width() > 768) {
			if ($usermenu.is(":hidden"))
			$usermenu.show();
		} else   {
			$usermenu.hide();
			}
		});
	
	// Initialize the phone version button
	$("a#phonemenulink").on(hmBrowser.touchstart + " keydown", 
	function(event) {
		if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
		hmWebHelp.closeMenus();
		if ($usermenu.is(":hidden")) {
			$usermenu.slideDown(300, function() {
				
			var currentFocus = document.getElementById("topnav")
			
			$(document).on(hmBrowser.touchstart + '.closemenu', function(event){
				if (!currentFocus.contains(event.target)) {
					$(document).off(hmBrowser.touchstart + '.closemenu');
					hmWebHelp.closeMenus();
					if ($(window).width() < 769)
						$usermenu.hide();
				};
			});
				
				});
		} else
			$usermenu.hide();
		});

	// Set vertical position of menu.
	 function topInit() {
		 $("ul.topnav li ul.subnav").css({top:($($("ul.topnav")[0]).height()) + "px"});
	 }
	
	// Top-level entries can only include an URL with a target if they have no submenu entries
	
	$("ul.topnav > span > li:has(ul) > a").on(hmBrowser.touchstart + " keydown", function(event) {
		if (typeof(event.button) != "undefined" && event.button !== 0) return;
		if (event.type != "keydown") {	
			event.preventDefault();
			event.stopPropagation();
			}
		if (event.type == "keydown" && event.keyCode != 13) return;
		var $menuTarget = $(this).parent().find("ul.subnav");
		var $menuSwitcher = $("svg:first-child use", this);
		var menuOpen = $menuTarget.is(":visible");
		
		if (!hmDevice.phone) {
			hmWebHelp.closeTopNav(true);
		}
		
		if (menuOpen && !hmDevice.phone) return;
		
		$(this).addClass("current");

	// Close any other menus
	hmWebHelp.closeMenus();
	// Close menus on a click or tap anywhere outside the menu
	
			hmWebHelp.unClicker('header_menu');

			if ($menuTarget.is(":hidden")) {
				topInit();
				$menuSwitcher.attr("xlink:href","#menu-open");
				// Accordeon close any open submenus on phone to save space
				if (hmDevice.phone) { // $$$
					$("li.header ul.subnav").slideUp('fast');
				}
				$menuTarget.slideDown('fast', function(){
					$(this).find("li:visible").last().not(".last").addClass("last");
				});
				} else {
					$menuTarget.slideUp('fast', function(){
						$menuSwitcher.attr("xlink:href","#menu-closed");
					});
					$("ul.topnav > span > li > a.current").removeClass("current");
				}
		if (hmDevice.phone) {
			hmWebHelp.funcs.doVibrate();
		}
		});
		
		// Also close menus when a link in a submenu is clicked, except on phones
		$("ul.subnav li a").on("click", function(){
			if (!hmDevice.phone) { // $$$
				hmWebHelp.closeTopNav(true);
			} else {
			hmWebHelp.funcs.doVibrate();
			}
			});	
	topInit();
};
