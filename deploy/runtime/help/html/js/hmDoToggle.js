/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/

// Constructor

function hmDropDownToggles() {
	
	var self = this;
	self.clicked = false;
	
	// Main individual toggle function
	this.HMToggle = function($obj,mode,sp,mlt) {
	var action = (typeof mode === "undefined") ? "toggle" : mode,
		speed = (typeof sp === "undefined") ? 150 : sp,
		icon = $obj.attr("data-icon"),
		type = $obj.attr("data-type"),
		icon0 = icon ? $obj.attr("data-src0") : null,
		icon1 = icon ? $obj.attr("data-src1") : null,
		state = $obj.attr("data-state"),
		$target = $("div#" + $obj.attr("id").replace(/_LINK$/i,"")),
		$icon = icon ? $("img#" + icon) : null,
		multi = typeof mlt !== "undefined",
		doflash = true;
		
		function repositionToggle() {
			if (typeof hmxtoggle == "undefined")
				hmxtoggle = false;
			var $activeScrollBox = $(window).width() > 768 ? hmpage.$scrollBox : $("main#topicbox"),
				$activeScrollContainer = $(window).width() > 768 ? hmpage.$scrollContainer : $("main#topicbox"),
				$scrollBox = !hmxtoggle ? $activeScrollBox : $("html"),
				hdTop = hmxtoggle ? $obj.offset().top : $obj.offset().top - $scrollBox.offset().top + $scrollBox.scrollTop(),
				bdTop =  hmxtoggle ? $target.offset().top : $target.offset().top - $scrollBox.offset().top + $scrollBox.scrollTop(),
				
				toggleHeight = $obj.height() + $target.height(),
				wdHeight = !hmxtoggle ? $activeScrollContainer.height() : $(window).height(),
				currentScroll = !hmxtoggle ? $scrollBox.scrollTop() : $("body").scrollTop(),
				toggleOffset = hdTop - currentScroll,
				toggleOverlap = toggleOffset + toggleHeight - wdHeight,
				scrollTarget = Math.round(currentScroll + toggleOverlap);
				
				var conditionalFlash = function() {
					if ($obj.attr("class") == "dropdown-toggle" && !self.clicked && !hmxtoggle)
					hmWebHelp.flashTarget($obj,3,200);
					}
				
				if ((toggleOffset + toggleHeight) > wdHeight) {
					if (toggleHeight > wdHeight) {
						$scrollBox.scrollTo(hdTop-10,300,{axis: 'y', onAfter: conditionalFlash
						});
					} else {
						$scrollBox.scrollTo((scrollTarget+30 > hdTop-10 ? hdTop-10 : scrollTarget+30),300,{axis: 'y', onAfter: conditionalFlash});
						}
				} else {
					conditionalFlash();
				}
		}
		
		function openToggle() {
			$target.slideDown(speed, function(){
				if (!multi) {
					setTimeout(repositionToggle,50);
				}
				});
			$obj.attr("data-state","1");
			if (icon) $icon.attr("src",icon1);
			// Change hamburger menu entries if this is not a multi-toggle operation
			if (!multi) {
				$("svg#showhide_toggles_icon").find("use").attr("xlink:href","#eye-off");
				$("li#showhide_toggles span").first().html("Hide Expanding Text");
			}
		}
		
		function closeToggle() {
			$target.slideUp(speed);
			$obj.attr("data-state","0");
			if (icon) $icon.attr("src",icon0);
			// Change hamburger menu entries if this is not a multi-toggle operation
			if (!multi) {
				if ($("a.dropdown-toggle[data-state='1']").length < 1) {
				$("svg#showhide_toggles_icon").find("use").attr("xlink:href","#eye");
				$("li#showhide_toggles span").first().html("Show Expanding Text");
				}
			}
			
		}
		
		switch(action) {
			
			case "toggle":
			if (state === "0" || $target.is(":hidden")) {
				openToggle();
			} else {
				closeToggle();
			}
			break;
			
			case "expand":
			openToggle();
			break;
			
			case "collapse":
			closeToggle();
			break;
		}
	}; // HMToggle	

	
	// Iterative toggle function, also for links
	// Opens chains of toggles from below or above
	
	this.hmToggleToggles = function() {
	
	var index, 
		$theseToggles = {}, 
		tCount, speed, mode, targetState, $scrolltarget, $doToggle, args;
		// $scrollBox = hmDevice.phone ? $("main#topicbox") : $("div#hmpagebody_scroller");
	if (arguments.length > 0 && typeof arguments[0] === "object") {
		args = arguments[0];
		$theseToggles = typeof args.toggles === "undefined" ? $("a.dropdown-toggle") : args.toggles;
		tCount = $theseToggles.length;
		speed = typeof args.speed === "undefined" && tCount < 21 ? 150 : tCount > 20 ? 0 : args.speed;
		mode = typeof args.mode === "undefined" ? "toggle" : args.mode;
		targetState = mode !== "collapse" ? "1" : "0";
		$scrolltarget = typeof args.scrolltarget === "undefined" ? null : args.scrolltarget;
		$doToggle = args.dotoggle;
		index = mode === "collapse" ? tCount-1 : 0;
	} else {
		$theseToggles = $("a.dropdown-toggle");
		tCount = $theseToggles.length;
		speed = tCount < 21 ? 150 : 0;
		index = 0;
		mode = "toggle";
		targetState = "1";
		$scrolltarget = null;
	}
	
	if (document.location.search.indexOf("anchor=") > -1 && !$scrolltarget) {
		History.replaceState(null,null,document.location.pathname);
		}
		
	if (mode === "toggle") {
	$theseToggles.each(function(){
			if ($(this).attr("data-state") === "1") {
				mode = "collapse";
				targetState = "0";
				index = tCount-1;
				return false;
				}
		});
		if (mode === "toggle") {
			mode = "expand";
			targetState = "1";
			index = 0;
		}
	}
	function doIterate(){
		var toggleSpeed = speed,
			timeoutSpeed = speed >  0 ? speed + 20 : 0,
			$currentToggle = $($theseToggles[index]);
		// Get the toggle link if we're processing an object of toggle bodies
		if ($currentToggle.attr("class") === "dropdown-toggle-body")
			$currentToggle = $("a.dropdown-toggle[id^="+$currentToggle.get(0).id+"]");
		
		if ($currentToggle.attr("data-state") === targetState) {
					toggleSpeed = timeoutSpeed = 0;
						}
		if (mode === "collapse") {
			self.HMToggle($currentToggle,mode,speed,"multi");
			index--;
			if (index > -1) {
				setTimeout(function(){doIterate();},timeoutSpeed);
				} else if ($scrolltarget && $scrolltarget.is(":visible")){
					setTimeout(function(){hmpage.$scrollBox.scrollTo($scrolltarget,300,{axis: 'y', offset:{top:-10},onAfter:function(){
							if ($doToggle)
								self.HMToggle($doToggle);
						}});},timeoutSpeed+50);
					} 
		$("svg#showhide_toggles_icon").find("use").attr("xlink:href","#eye");
		$("li#showhide_toggles span").first().html("Show Expanding Text");
		} else if (mode === "expand") {
			self.HMToggle($currentToggle,mode,speed,"multi");
			index++;
			if (index < tCount) {
				setTimeout(function(){doIterate();},timeoutSpeed);
				} else if ($scrolltarget){
					setTimeout(function(){
						hmpage.$scrollBox.scrollTo($scrolltarget,300,{axis: 'y', offset:{top:-10},onAfter:function(){
							if ($doToggle)
								self.HMToggle($doToggle);
							else if (self.doflash)
								hmWebHelp.flashTarget($scrolltarget.parent(),3,200);
						}});
						},timeoutSpeed+50);
					} 
		$("svg#showhide_toggles_icon").find("use").attr("xlink:href","#eye-off");
		$("li#showhide_toggles span").first().html("Hide Expanding Text");
		}
	}
	
	doIterate();	
	}; // toggle toggles 
	
	function parseToggle(args) {
		var method, $obj, mode, speed;

		if (args && typeof args === "object") {
			method = args.method;
			self.clicked = typeof args.clicked != "undefined" && args.clicked;
			$obj = args.obj;
			self.doflash = typeof args.doFlash == "undefined" ? true : args.doFlash;
	
		switch(method) {
			
			case "HMToggle":
			if (typeof $obj.target !== 'undefined') {
				self.HMToggle($obj.target,$obj.mode,$obj.speed);
			} else {
			self.HMToggle($obj);
			}
			break;
			
			case "HMToggleIcon":
				var objID = $obj.attr("id");
				objID = objID.replace(/_ICON$/,"_LINK");
				objID = objID.replace(/_icon$/,"_link");
				$obj = $("a#" + objID);
				self.HMToggle($obj);
			break;
			
			case "hmToggleToggles":
			if (typeof $obj === "object" && $obj !== null) {
				self.hmToggleToggles($obj);
				}
			else {
				self.hmToggleToggles();
				}
			break;
			} 
		}
	
	}
	
	return parseToggle;
} // Constructor

hmWebHelp.funcs.hmDoToggle = new hmDropDownToggles();
