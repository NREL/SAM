/*! Main Help+Manual WebHelp 3.0 Functions
	Copyright (c) 2015-2023 by Tim Green. All rights reserved. 
	Contact: https://www.helpandmanual.com
*/

// General variables
hmFlags.defaultExt = hmFlags.hmDefaultPage.substr(hmFlags.hmDefaultPage.lastIndexOf("."));
hmFlags.topicExt = hmFlags.hmTOC.substr(hmFlags.hmTOC.lastIndexOf("."));

// var statePrefix = window.history.pushState ? "" : hmFlags.hmMainPage;

// Key groups for keyboard control
var hmKeys = {
	keynames: ["Enter","Tab","Escape","Esc","Home","End","ArrowLeft","ArrowUp","ArrowRight","ArrowDown"," ","Left","Up","Right","Down","PageUp","PageDown"],
	entermenu: ["Enter","ArrowDown"," ","Down"],
	doenter: ["Enter", " "],
	escaper: ["Esc","Escape"],
	godown: ["ArrowDown","ArrowRight","Down"],
	goup: ["ArrowUp","ArrowLeft","Up"]
	}

//** Browser capabilities object  **//
var hmBrowser = {}; 
	hmBrowser.addEventListener = !!window.addEventListener; //Unused
	hmBrowser.orientation = !!("onorientationchange" in window);
	hmBrowser.orientationevent = typeof window.orientation == "undefined" ? "resize" : "orientationchange";
	hmBrowser.server = /^https??:\/\//im.test(document.location);
	// Touch browser *with* hardware support
	hmBrowser.touch = !!(('ontouchstart' in window && !window.opera) || ('msmaxtouchppoints' in window.navigator) || ('maxtouchppoints' in window.navigator) || (navigator.maxTouchPoints > 0) || (navigator.msMaxTouchPoints > 0));
	hmBrowser.nonDeskTouch = ((hmBrowser.touch && !/win32|win64/i.test(navigator.platform)) || (hmBrowser.touch && /win32|win64/i.test(navigator.platform) && /mobile/i.test(navigator.userAgent)));	
	hmBrowser.transitions = (function(temp) {
	  var props = ['transitionProperty', 'WebkitTransition', 'MozTransition', 'OTransition', 'msTransition'];
	  for ( var i in props ) if (temp.style[ props[i] ] !== undefined) return true;
	  return false;
	})(document.createElement('rabbit'));
	hmBrowser.Flandscape = function() {if (hmBrowser.orientation) {return (Math.abs(window.orientation)==90);} else {return (window.innerHeight < window.innerWidth);}};
	hmBrowser.MobileFirefox = (/Android.+?Gecko.+?Firefox/i.test(navigator.userAgent));
	hmBrowser.isaspx = (/\.aspx??$/i.test(document.location.pathname));
	hmBrowser.eventType = (('onmousedown' in window && !hmBrowser.nonDeskTouch) ? "mouse" : ('ontouchstart' in window) ? "touch" : ('msmaxtouchpoints' in window.navigator || navigator.msMaxTouchPoints > 0) ? "mstouchpoints" : ('maxtouchpoints' in window.navigator || navigator.maxTouchPoints > 0) ? "touchpoints" : "mouse");
	
	switch(hmBrowser.eventType) {
	case "mouse":
		hmBrowser.touchstart = "mousedown.startevents";
		hmBrowser.touchend = "mouseup.endevents";
		hmBrowser.touchmove = "mousemove.moveevents";
	break;
	case "touch":
		hmBrowser.touchstart = "touchstart.startevents";
		hmBrowser.touchend = "touchend.endevents";
		hmBrowser.touchmove = "touchmove.moveevents";
	break;
	case "mstouchpoints":
		hmBrowser.touchstart = "MSPointerDown.startevents";
		hmBrowser.touchend = "MSPointerUp.endevents";
		hmBrowser.touchmove = "MSPointerMove.moveevents";
	break;
	case "touchpoints":
		hmBrowser.touchstart = "pointerdown.startevents";
		hmBrowser.touchend = "pointerup.endevents";
		hmBrowser.touchmove = "pointermove.moveevents";
	break;
	default: // Generic fallback, just in case
		hmBrowser.touchstart = "mousedown.startevents";
		hmBrowser.touchend = "mouseup.endevents";
		hmBrowser.touchmove = "mousemove.moveevents";
	}
	
	hmBrowser.hover = hmDevice.desktop ? "mouseenter.startevents" : hmBrowser.touchstart;


// Global page variables, references, dimensions etc.
var hmpage = {
		$navwrapper: $("div#navwrapper"),
		$helpwrapper: $("div#helpwrapper"),
		$hamburgermenu: $("div#navigationmenu"),
		$main_searchbox: $("div#main_searchbox"),
		$dragwrapper: $("div#dragwrapper"),
		navboxoffset: 0.25,
		$navcontainer: $("nav#navcontainer"),
		$navsplitbar: $("div#navsplitbar"),
		$contcontainers: $("div#navwrapper, main#topicbox"),
		$headermenu: $("nav#header_menu"),
		atocmode: "mouse",
		hmDescription: "",
		topicfooter: "",
		hmPicture: "",
		topicadjust: false,
		FtabsWidth: function(){
			return 215;
			/*let thisnavwidth = 20;
			$("ul#topictabs li").each(function(){
				if ($(this).is(":visible"))
					thisnavwidth += $(this).outerWidth();
				});
			return thisnavwidth;*/
		},
		FnavWidth: function() {
			return hmpage.$navcontainer.width();
			},
		navWidth: $("div#navwrapper").width(),
		borderWidth: parseInt($("div#helpwrapper").css("border-left-width")),
		FmaxNavWidth: function() { return hmDevice.phone ? hmpage.$helpwrapper.width() * 0.9 : hmDevice.tablet ? (hmBrowser.Flandscape() ?  hmpage.$helpwrapper.width() * 0.5 :  hmpage.$helpwrapper.width() * 0.9) : hmpage.topicleft ? hmpage.$helpwrapper.width() * 0.8 : hmpage.$helpwrapper.width() * 0.5;},
		FminNavWidth: function() { return (hmpage.FtabsWidth() < 200 ? 200 : hmpage.FtabsWidth()) + 40 + (hmpage.FnavWidth() - hmpage.$navcontainer.width());},
		FnavOffset: function() {return parseFloat($("div.navbox").css("border-left-width"),10) + parseFloat($("div.navbox").css("border-right-width"),10);},
		hmHelpUrl: {},
		splitter: {},
		currentnav: 0, // Index of current navigation tab
		currentTopicID: "",
		$headerbox: $("header#headerbox_wrapper"),
		$topicbox: $("main#topicbox"),
		$topicboxTop: $("main#topicbox").offset().top,
		$topicheader: $("table#topicheadertable td"),
		$topichdwrap: $("div#topicheaderwrapper"),		
		topichdoffset: $("td#topicnavcell").innerHeight() + 1 - $("div#topicheaderwrapper > h1.p_Heading1").outerHeight(),
		$contentsbox: $("div#contentsbox"),
		$indexbox: $("div#indexbox"),
		$searchbox: $("div#searchbox"),
		$headerwrapper: $("div#headerwrapper"),
		$pagebody: $("div#hmpagebody"),
		$pageheader: $("nav#hmpageheader"),
		$navhandle: $("div#dragwrapper"),
		$navtools: $("div#dragwrapper,div#toolbutton_wrapper"),
		FheaderHeight: function() {
			return $("div#headerbox").height();
			},
		headerheightStatic: parseInt($("div#headerbox").css("height"),10),
		$headerpara: $("div#headerwrapper h1.page_header"),
		topicleft: $("main#topicbox").position().left < 50,
		navclosed: Math.round($("div#navwrapper").position().left) < 0,
		narrowpageX: false,
		Fnarrowpage: function() {
			return (
			(!hmDevice.phone && hmpage.$helpwrapper.width() <= ((hmFlags.tocInitWidth+20)*2)) ||
			(hmDevice.phone && !hmBrowser.Flandscape())|| 
			(hmDevice.tablet && !hmBrowser.Flandscape() && hmpage.$helpwrapper.width() <= ((hmFlags.tocInitWidth+20)*3)) ||
			(hmDevice.desktop && hmpage.$helpwrapper.width() < 769)
			);},
		headerclosedonopen: (function(){
			var storedHeaderState = sessionVariable.getSV("headerState");
			return (false || (storedHeaderState !== null && storedHeaderState === "closed"))})(),
		headerclosed: !!(this.headerclosedonopen || $("div#headerbox").is(":hidden")),
		tocclosedonopen: (function(){
			var storedTocState = sessionVariable.getSV("tocState");
			if ((!true && hmDevice.desktop) || (!true && !hmDevice.desktop))
				return((storedTocState !== null && storedTocState === "closed"));
			else
				return true;
		})(),
		breadcrumbs: true,
		parenthome: false,
		defaulttopic: "index.html",
		shortpageX: false,
		Fshortpage: function() {
			return (
				(!hmDevice.phone && $(window).height() < 400) ||
				(hmDevice.phone && hmBrowser.Flandscape())
				);
			},
		navShadowOn: false,
		initialized: false,
		projectBaseFontRel: "100",
		projectStoredFont: function(){
			var testVal = sessionVariable.getPV("hmFontSize");
			if (testVal !== null)
			return (parseInt(testVal,10)/100) * 16;
			else return NaN},
		FbaseFontSize: function(){
			var cookieFont = this.projectStoredFont();
			return (
			!isNaN(cookieFont) ? cookieFont :
			parseFloat(window.getComputedStyle(document.getElementsByTagName("html")[0],null).getPropertyValue("font-size"),10)
			);},
		Fpix2em: function(pix) {return (pix / this.FbaseFontSize());},
		Fem2pix: function(em) {return (em * this.FbaseFontSize());},
		anchorX: hmBrowser.server ? "\?anchor\=" : "\!anchor\=",
		anchorY: hmBrowser.server ? "\?" : "\!",
		embeddedBorder: true
	};
// Check for correct encoding in author's project
if (/%|pt|px/i.test(hmpage.projectBaseFontRel)) {
	$("*").css("visibility","hidden");
alert("ERROR! The font size encoding of your project is not set to ems, which is required for this skin.\r\n\r\nPlease set the font size encoding of your project to ems in 'Configuration - Publishing Options - WebHelp - HTML Export Options' before using this skin.\r\n\r\nYou can adjust the base font size in your output with the percentage setting after the font size encoding setting.");
	} else { 
		hmpage.projectBaseFontRel = parseFloat(hmpage.projectBaseFontRel,10);
	}
	
// Main function encapsulating object
var hmWebHelp = {
		// Container object for externally loaded functions
		funcs: {},
		currentBS: 0,
		lastBS: false,
		visitedTopics: {},
		userParams: {paramsCount: 0}
	};

	// String trimmer
hmWebHelp.trimString = function(str){
   return str.replace(/^\s+/g, '').replace(/\s+$/g, '');  
};

	// Header closer and opener for calling to embedded help
hmWebHelp.closeHeaderIfOpen = function() {
	if ($("div#headerbox").is(":visible")) {
		hmWebHelp.pageDimensions.pageHeaderUpDown(false);
		}
	};
hmWebHelp.openHeaderIfClosed = function() {
	if ($("div#headerbox").is(":hidden")) {
		hmWebHelp.pageDimensions.pageHeaderUpDown(false);
		}
	};

/* Keyboard menu control */
hmWebHelp.keyMenuControl = function() {

	var $keyEntries = {},
		keyCount = 0,
		currentIndex = 0;
		
	// Get key  entries
	$keyEntries = $("div#keyboardinfo > table a");
	keyCount = $keyEntries.length - 1;
	currentIndex = 0;
		
	var headNavigate = function(event) {
		event.preventDefault();
		if (!hmKeys.keynames.includes(event.key)) return;
		$(document).off("keydown.keyentries");
		$("h1#keynavtitle").removeAttr("tabindex");
		$keyEntries[0].focus();
		$(document).on("keydown.keyentries", keyNavigate);
	};

	var keyNavigate = function(event) {
		event.preventDefault();
		if (!hmKeys.keynames.includes(event.key)) return;
			if (hmKeys.escaper.includes(event.key)) {
			$(document).off("keydown.keyentries");
			setTimeout(function(){
				$("div#keyboardinfo").show();
				$("h1#keynavtitle").attr("tabindex","0").focus();
				$(document).on("keydown.keyentries", headNavigate);
			},10);
		};
		
		if (hmKeys.doenter.includes(event.key)) {
			setTimeout(function(){
				$(document).off("keydown.keyentries").off("keydown.keyentries");
				Function($keyEntries[currentIndex].href.substring(11))();
				$("div#keyboardinfo").hide();
				return;
			},10);
		};

		if (!hmKeys.keynames.includes(event.key)) return;
			
			if (event.key == "PageDown") {
				currentIndex = keyCount;
				$keyEntries[keyCount].focus();
			}
			if (event.key == "PageUp") {
				currentIndex = 0;
				$keyEntries[0].focus();
			}
			
			if ((hmKeys.godown.includes(event.key) || (event.key == "Tab" && !event.shiftKey)) && currentIndex+1 > keyCount) 
		currentIndex = -1;
	else if ((hmKeys.goup.includes(event.key) || (event.key == "Tab" && event.shiftKey)) && currentIndex-1 < 0) 
		currentIndex = keyCount+1;
	
		if (hmKeys.goup.includes(event.key) || (event.key == "Tab" && event.shiftKey)) {
			$keyEntries[currentIndex-1].focus();
		} else if (hmKeys.godown.includes(event.key) || (event.key == "Tab" && !event.shiftKey)) {
			$keyEntries[currentIndex+1].focus();
			}
	
	if (hmKeys.godown.includes(event.key) || (event.key == "Tab" && !event.shiftKey)) {
		currentIndex++;
		} else if (hmKeys.goup.includes(event.key) || ( event.key == "Tab" && event.shiftKey)) {
			currentIndex--;
			}
	}; // KeyNavigate
		
	if ($("div#keyboardinfo").is(":hidden")) {
		$(document).off("keydown.keyentries");
		$("div#keyboardinfo").show();
		$("div#keyboardinfo").off("click").on("click", function(event){
			$("h1#keynavtitle").attr("tabindex","0").focus();
		});
		hmWebHelp.unClicker('keyboardinfo');
		setTimeout(function(){
				$("h1#keynavtitle").attr("tabindex","0").focus();
			},300);
		$(document).on("keydown.keyentries", headNavigate);
	}
}// keyMenuControl()


// Set focus for header and topic for keyboard control
hmWebHelp.pageFocus = function(mode) {

	hmWebHelp.closeMenus();
	
	if (!hmDevice.desktop) return;
	
	switch(mode) {

		case "info":
		if ($("div#keyboardinfo").is(":hidden")) {
			hmWebHelp.keyMenuControl();
		} else {
			$(document).trigger(hmBrowser.touchstart);
			}
		break;
		case "topic":
		$("h1.topictitle").first().attr("tabindex","0").focus();
		break;
		case "body":
		let $bod = $("div#hmpagebody_scroller span, div#hmpagebody_scroller p, div#hmpagebody_scroller a").first();
		let $par = $bod.parent("p");
		if ($par.length > 0) $bod = $par;
		if ($bod[0].tagName.toLowerCase() == "span") {
			let $toggle = $bod.children("a.dropdown-toggle").first();
			if ($toggle.length > 0) $bod = $toggle;
			}
		$bod.attr("tabindex","0").focus();
		break;
		case "header":
		if ($("div#headerbox").is(":hidden")) {
		hmWebHelp.pageDimensions.pageHeaderUpDown(true);
		}
				var $head = $("div#headerbox").find("a").first();
		$head.focus();
		$head.css({"box-shadow": "0 0 0 1px #444444"});
		$head.on( hmBrowser.touchstart + ".headerclick, keydown.headerclick", function(){
			$(this).removeAttr("style").off(".headerclick");
		});
		break;
		}
		if (!["info", "start"].includes(mode)) {
			$("div#keyboardinfo").hide();
			$(document).off("keydown.keyentries");
		}
}; // pageFocus

hmWebHelp.embedBorderSwitch = function(mode, winWidth) {
	var borderWidth = "thin";
	
	if (["zoomin", "zoomout"].includes(mode)) {
		if (mode == "zoomout") {
			if (!hmpage.embeddedBorder) {
				$("div#helpwrapper").css("border", borderWidth + " solid #888888");
				} 
			$("div#helpwrapper").css("border-width", "0 " + borderWidth + " 0 " + borderWidth);
			
			// Clear borders if the window is narrower than the viewport
				if ($('div#helpwrapper').width() < winWidth) {
						$('div#helpwrapper').css("border-width", "0 " + borderWidth);
					} else {
						$('div#helpwrapper').css("border-width", "0 0");
					}
		} else {
			if (hmpage.embeddedBorder) {
				$("div#helpwrapper").css("border-width", borderWidth + " " +  borderWidth + " "  + borderWidth + " " + borderWidth);
				} else {
				$("div#helpwrapper").css("border-width", "0 0 0 0");	
				} 
		}
	} else if (["bordersOn", "bordersOff"].includes(mode)){
		if (mode == "bordersOn") {
			$("div#helpwrapper").css({"border-left-width": borderWidth,  "border-right-width": borderWidth});
		} else {
			$("div#helpwrapper").css({"border-left-width": "0",  "border-right-width": "0"});
		}
	}
}

// Set up vibration support if available
navigator.vibrate = navigator.vibrate || navigator.webkitVibrate || navigator.mozVibrate || navigator.msVibrate;
hmWebHelp.funcs.doVibrate = function(duration) {
	if (!duration) duration = 20;
	if (navigator.vibrate) {
				navigator.vibrate(duration);
			} 
	};

// Scroll to top for script link use
   var ScrollTop = function() {
	   hmpage.$scrollBox.scrollTo(0,600);
   }

// Function for getting topic js file from cache instead of reloading it
jQuery.cachedScript = function( url, options ) {

  // Use .done(function(script,textStatus){}); for callbacks

  // Allow user to set any option except for dataType, cache, and url
  options = $.extend( options || {}, {
    dataType: "script",
    cache: true,
    url: url
  });
  return jQuery.ajax( options );
};

// Handler for post-loading functions from files
hmWebHelp.extFuncs = function(func, args) {	
		var newScript = "";
	if (typeof hmWebHelp.funcs != "object") hmWebHelp.funcs = {};
	
	if (typeof hmWebHelp.funcs[func] == "function") {
		hmWebHelp.funcs[func](args);
	} else {
		// Get name of script and load it
		newScript= "./js/" + func + ".js";
		$.getScript(newScript, function (data, textStatus, jqxhr) {

		if (textStatus === "success" && typeof hmWebHelp.funcs[func] == "function") {
			try {
			hmWebHelp.funcs[func](args);
			} catch(err) {
			// This catches bugs in a semantically correct extFunc
			alert(err);
			}
		} else {
			// This catches source that fails to validate as a function
			alert("External function script " + func + ".js failed to load as a function");
			}
		});
	}
	}; // hmWebHelp.extFuncs()
	
// Multi-browser preventDefault, not really needed with jQuery but can be used
// when not specifically using jQuery functions for speed or other reasons
hmWebHelp.PreventDefault = function(event) {
	if (event.preventDefault)
		event.preventDefault();
	else
	 	event.returnValue = false;
	};

// Close open popup if present
hmWebHelp.closePopup = function() {

	if (typeof hmXPopup === "object" && typeof hmXPopup.closePopup === "function" && hmXPopup.$popup.is(":visible")) {
				hmXPopup.closePopup();
			}
};

// TOC navigation management object
hmWebHelp.tocNav = new function() {
	
	// Main handle
	var self = this,
	
	// Operational vars
		action = "",
		thisbs = null,
		thishref = null,
		isset = false,
		skipnav = false,
	// Storage vars
		lastbs = 0,
		lasthref = "";
	

	// Set up object for later execution
	var doset = function(args,nonav) {
		lastbs = thisbs;
		lasthref = thishref;
		thisbs = nonav ? false : args.bs;
		thishref = nonav ? false: args.href;
		skipnav = nonav;
		isset = true;
		if (args.href != hmFlags.hmCurrentPage && args.href != "") {
			History.pushState(null,null,args.href);
		} 
		else if (typeof(args.bs) == "number" && args.bs >= 0) {
		xMessage.sendObject("hmnavigation",{action: "callfunction",fn: "tocSource.findElement", fa: args.bs});
		}
		
	};
	
	// Execute nav with href
	var donav = function(args) {

		// Automatic href if not previously set
		if (!isset) {
			if (args.href !== lasthref) {
			lasthref = args.href;
			thishref = "";
			xMessage.sendObject("hmnavigation",{ action: "href", href: args.href, bs: false});
			return;
			} else {
			}
		}
		
		// Read and execute settings if the object has been set
		if (isset) {
			if (thishref || thisbs >=0) {
				if (thishref && !thisbs && thisbs !== 0) {
					if (thishref !== lasthref) {
						xMessage.sendObject("hmnavigation",{ action: "href", href: thishref.replace(/[\!\?]anchor=/,"#"), bs: false});
					}
					lasthref = thishref;
				} else if (!skipnav && thisbs >=0) {
					if (thisbs !== lastbs)
						xMessage.sendObject("hmnavigation",{ action: "bs", href: false, bs: thisbs});
					lastbs = thisbs;
				}
			} 
		}
	
		isset = false;
		thisbs = false;
		thishref = false;
		return;
	};
	
	// Update breadcrumbs
	var dobread = function(args) {
			var bakedbread = "Navigation: ",
				tempslice = [],
				stop = "",
				nobread = " \<span\>&raquo; No topics above this level &laquo;\<\/span\>",
				notoc = " \<span\>&raquo; No TOC entry for this topic &laquo;\<\/span\>";
		
			if (args.breadmode == "full") {
			for (var x in args) {
				tempslice = args[x].split(stop);
				if (tempslice.length == 3) {
					if (tempslice[1] !== "") {
						bakedbread += '\<a href="javascript:void(0)" title="'+tempslice[2]+'" aria-label="'+tempslice[2]+'" onclick="hmWebHelp.tocNav({action: \'set\', bs: '+tempslice[0]+', href: \''+hmWebHelp.targetCheck(tempslice[1])+'\'})"\>' + tempslice[2] + '\</a\> &gt; ';
					}
					else
						bakedbread += tempslice[2] + '\</a\> &gt; ';
				}
			}
			bakedbread = bakedbread.replace(/&gt;\s$/,"");
			$("p#ptopic_breadcrumbs").html(bakedbread);
			} else {
			if (args.breadmode == "top")
				$("p#ptopic_breadcrumbs").html(bakedbread + nobread);
			else
				$("p#ptopic_breadcrumbs").html(bakedbread + notoc);
			}
	};
	

	// Initial setup
	var parseargs = function(args) {
		if (typeof args != "object") return false;
		if (!args.hasOwnProperty('action')) return false;
		action = args.action;
		return true;
	};
	
	// Initializer
	return function(args) {
		if (!parseargs(args)) {
			return;
			}
		switch(action) {
			case "set":
			doset(args,false);
		break;
			case "load":
			doset(args,true);
		break;
			case "href":
			donav(args);
		break;
			case "bread":
			dobread(args.crumbs);
		break;
		}
	};
	}();

// Close all drop-down menus before continuing with something else
hmWebHelp.closeMenus = function() {
		var active = 0;
		if ($("div#navigationmenu").is(":visible")) {
			hmWebHelp.hamburgerMenu("close");
			active++;
			}
		if (!hmDevice.phone) {
			if (typeof bindCloseMenu == "undefined" && typeof hmWebHelp.closeTopNav != "undefined")
				hmWebHelp.closeTopNav(true);
			active++;
			}
		if (hmDevice.phone && $("nav#header_menu").is(":visible")) {
			if (typeof bindCloseMenu == "undefined" && typeof hmWebHelp.closeTopNav != "undefined")
				hmWebHelp.closeTopNav();
			else
				$(document).trigger(hmBrowser.touchstart);
			active++;
			}
		if ($("div#autoTocWrapper").is(":visible") && !hmDevice.phone) {
			hmWebHelp.funcs.hm_autotoc("close");
			active++;
			}
		if ($("div#keyboardinfo").is(":visible")) {
			$(document).off("keydown.keyentries");
			$("div#keyboardinfo").hide();
			active++;
			}
		if ($("div#hmpopupbox").is(":visible")) {
			hmWebHelp.closePopup();
			active++;
		}
	}

// Initialize page-specific event handlers
hmWebHelp.hmTopicPageInit = function() {
	
	// Toggles on page
	var $dropToggles = $("a.dropdown-toggle"),
		$dropIcons = $("img.dropdown-toggle-icon"),
		$imageToggles = $("img.image-toggle"),
		$videoToggles = $("div.video-lightbox"),
		$inlineToggles = $("a.inline-toggle");
		
	// Copy Code boxes
	var	$copyCodeBoxes = $("span.f_CodeSampleLink");
	
	if ($copyCodeBoxes.length > 0) {
		$copyCodeBoxes.on("click", function(event) {
			hmWebHelp.extFuncs('hmDoCopyCode', event.originalEvent);
			});
		};
		
	// Refresh scrollbox reference for new content
	hmpage.$scrollBox = $("div#hmpagebody_scroller");
	hmpage.$scrollContainer = $("div#hmpagebody");
	
	// Display atoc scroll menu on pages with atoc links
	if (true && $("span[class*='_atoc']").length >= parseInt("3",10) && !hmDevice.phone) {
		$("a#atoclink").css("visibility","visible").off(hmBrowser.touchstart + " keydown").on(hmBrowser.touchstart + " keydown", 
			function(event){
			hmpage.atocmode = "keyboard";
			if (event.type == "keydown" && !hmKeys.entermenu.includes(event.key)) return;
			//Catch right-clicks
			if (typeof(event.button) != "undefined" && event.button !== 0) return;
			if (event.type != "keydown") {
			event.preventDefault(); event.stopPropagation();
			hmpage.atocmode = "mouse";
			}
			hmWebHelp.extFuncs('hm_autotoc',event);
			});
		} else {
			$("a#atoclink").css("visibility","hidden");
		}
		
	// Open web, file and script links with Enter and Space
	$("a.weblink, a.filelink, a.scriptlink").off(hmBrowser.touchstart + " keydown").on(hmBrowser.touchstart + " keydown", function(event) { 
		if (event.type !== "keydown" || (event.type == "keydown" && !hmKeys.doenter.includes(event.key))) return;
		$(this)[0].click();
	});
	
	// Topic links incl. anchors, anchor links within topics
	
	// Process button topic links
	$("input.topiclink").each(function(){
		var target = $(this).attr("onclick");
		if (/^self\.location\.href=/.test(target))
			{
				target = target.replace(/^self\.location\.href='(.+?)'$/g, "$1");
				$(this).attr("data-target",target);		
				$(this).removeAttr("onclick");
			}
		});
	
	$("a.topiclink, a.topichotspot, p#ptopic_breadcrumbs a, input.topiclink").off(hmBrowser.touchstart + " keydown").on("click", function(event){event.preventDefault();}).on(hmBrowser.touchstart + " keydown", function(event) {
		if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
		//Catch right-clicks
		if (typeof(event.button) != "undefined" && event.button !== 0) return;
		event.preventDefault(); event.stopPropagation();
		var target = $(this).attr("href");
		// Button link?
		if (typeof target == "undefined") 
			target = $(this).attr("data-target");
		// Handle disabled links/tools
		if (!target) return;
		var thisPage, newPage,
			thisAnchor = target.indexOf("#") > 1 ? target.split("#").pop() : false,
			targetPage = target.indexOf("#") > 1 ? target.substr(0,target.lastIndexOf("#")) : target;
		if (hmBrowser.server) {
			thisPage = document.location.pathname.split("\/").pop();
		} else {
			thisPage = document.location.hash.substr(1);
		}
			newPage = thisPage !== targetPage;
		
		// Link in a layout table?

		if (!hmDevice.desktop && hmFlags.layoutTable) {
			hmWebHelp.funcs.toggleLayoutTable({action: "hide", instant: true});
		}

		// Filter out anchor links to targets in current topic and scrolltop links 
		if (thisAnchor && !newPage) { 
			hmWebHelp.scrollTopic(thisAnchor);
		} else if (!thisAnchor && !newPage) {
			hmpage.$scrollBox.scrollTo(0,400);
		} else {
				target = hmWebHelp.targetCheck(target);
				if (hmWebHelp.hmMainPageCheck(target)) {
					hmWebHelp.tocNav({action: "set", href: target, bs: false});
				}
			}
		});
		
		// Popup links
			$("a.popuplink,a.popuphotspot,input.popuplink,a.topichotspot[href^='javascript:void']").on(hmBrowser.touchstart + " keydown", function(event){
			
			if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
			if (event.type != "keydown") {
				event.preventDefault();
				}
			if (typeof(event.button) != "undefined" && event.button !== 0) return;
			var popupTarget = $(this).attr("data-target"),
				popupPath = $(this).attr("data-path"),
				popupTitle = $(this).attr("data-title"),
				// Need original event for touch coordinates
				ev = event.originalEvent,
				phonetop = $("nav#hmpageheader").height() - 10;
				
			// Change extension for ASPX version
			if (hmBrowser.isaspx) {
				var newTarget = popupTarget;
				switch (popupTarget) {
					
					case "_hmpermalink.html":
						newTarget = "_hmpermalink.aspx";
						break;
				}
			}
				
			if (typeof hmXPopup === "object") {
				hmXPopup.clickX = hmDevice.phone ? 0 : ev.pageX;
				hmXPopup.clickY = hmDevice.phone ? phonetop : ev.pageY;
				hmXPopup.loadPopup(popupTarget,popupPath,popupTitle);
			} else {	
				newScript= "./js/hmSmartPopup.js";
				$.getScript(newScript, function (data, textStatus, jqxhr) {
				if (textStatus === "success" && typeof hmXPopup.loadPopup == "function") {
					try {
					hmXPopup.clickX = hmDevice.phone ? 0 : ev.pageX;
					hmXPopup.clickY = hmDevice.phone ? phonetop : ev.pageY;
					hmXPopup.loadPopup(popupTarget,popupPath,popupTitle);
					} catch(err) {
					// Catches bugs in semantically correct function
					alert(err);
					}
				} else {
					// Catches source that fails to validate as a function
					alert("Syntax or other error in popup function");
					}
				});	
			}
		});
		
		/* Hamburger Menu */
		$("a#hamburgerlink").off(hmBrowser.touchstart + " keydown").on(hmBrowser.touchstart + " keydown", function(event) {
			
			if (event.type != "keydown") {
				//Catch right-clicks
				if (typeof(event.button) != "undefined" && event.button !== 0) return;
				event.stopPropagation();
				event.preventDefault();
			} else {
				if (!hmKeys.keynames.includes(event.key)) return;
				var $hamburgerEntries = {},
					hamburgerCount = 0,
					currentIndex = 0;
				}
			
			var hamburgerNavigate = function(event) {
				
				if (!hmKeys.keynames.includes(event.key)) {
					return;
				}
				
				if (hmKeys.escaper.includes(event.key)) {
				$("a#hamburgerlink")[0].focus();
				return;
				}

				if (event.key == "PageUp") {
				currentIndex = 0;
				$hamburgerEntries[0].focus();
			}
			if (event.key == "PageDown") {
				currentIndex = hamburgerCount;
				$hamburgerEntries[hamburgerCount].focus();
			}

				if (!hmKeys.doenter.includes(event.key)) event.preventDefault();	
				
				if ((hmKeys.godown.includes(event.key) ||  (event.key == "Tab" && !event.shiftKey)) && currentIndex+1 > hamburgerCount) 
				currentIndex = -1;
				else if ((hmKeys.goup.includes(event.key) ||  (event.key == "Tab" && event.shiftKey)) && currentIndex-1 < 0)
				currentIndex = hamburgerCount+1;
			
				if (hmKeys.goup.includes(event.key) || (event.key == "Tab" && event.shiftKey)) {
					$hamburgerEntries[currentIndex-1].focus();
				} else if (hmKeys.godown.includes(event.key) || (event.key == "Tab" && !event.shiftKey)) {
					$hamburgerEntries[currentIndex+1].focus();
					}
			
			if (hmKeys.godown.includes(event.key) || (event.key == "Tab" && !event.shiftKey)) {
				currentIndex++;
				} else if (hmKeys.goup.includes(event.key) || ( event.key == "Tab" && event.shiftKey)) {
					currentIndex--;
					}
			
			if (event.key == " ") {

				let thisBurger = $($hamburgerEntries[currentIndex]).parent().attr("id");
				
				var finishUp = function(f,h) {
				setTimeout(function(){
					if (!h) hmWebHelp.hamburgerMenu();
					if (f) $("a#hamburgerlink").focus();
				},1000);
				}
				
				switch(thisBurger) {
					
					case "showhide_pageheader":
					finishUp(true);
					break;
					case "showhide_toggles":
					hmWebHelp.extFuncs('hmDoToggle',{method: 'hmToggleToggles', obj: {speed: 80}});
					finishUp(true);
					break;
					case "hm_printable_version":
					let thistopic = document.location.pathname;
					thistopic = thistopic.substr(thistopic.lastIndexOf("\/")+1);
					thistopic = thistopic.replace(/\?.*$/,"");
					window.open("_hm_print_window.htm?"+thistopic, 'hm_print_window');
					finishUp(true);
					break;
					case "mailfeedback":
					hmWebHelp.extFuncs('hm_mail_feedback')
					finishUp(true,true);
					break;
					case "sharetopic":
					finishUp(false,true);
					break;
					case "savepermalink":
					finishUp(false);
					break;
					case "changefontsize":
					case "toggle_fullscreen":
					$($hamburgerEntries[currentIndex]).click();
					break;
					}
				}
			}
			
			if (event.type == "keydown" && hmKeys.entermenu.includes(event.key)) {

			setTimeout(function(){

				// Get visible hamburger menu  entries
				$hamburgerEntries = $("ul#hamburgermenu a:visible");
				hamburgerCount = $hamburgerEntries.length - 1;
				$hamburgerEntries[0].focus();
				$(document).on("keydown.hamburger", hamburgerNavigate);
				},350);	
			} 
			
			if ($("div#navigationmenu").is(":visible")) {
				hmWebHelp.closeMenus();
				return;
			};
			if (hmDevice.tablet) $(this).addClass('navhilite');
			hmWebHelp.closeMenus();
			if (event.type != "keydown" || hmKeys.entermenu.includes(event.key)) {
				hmWebHelp.hamburgerMenu();
			}
			}).on(hmBrowser.touchend, function(event){
			if (hmDevice.tablet)
				$(this).removeClass('navhilite');
			});
		// Printable version link in hamburger menu
		if (!hmDevice.phone && hmBrowser.server) {
			let printwindow = "_hm_print_window.htm?";
			if (hmBrowser.isaspx) {
				printwindow = "_hm_print_window.aspx?";
			}
			$("a#hm_printable_link").attr("href",printwindow + hmFlags.hmCurrentPage).attr("target","hm_print_window");
			}
		
		// Hide server-only stuff when local
		if (!hmBrowser.server)
			$(".server").hide();
		
		// Dropdown Toggles
		if ($dropToggles.length < 1) {
			$(".toggles").hide();
		}
		else {
			$(".toggles").show();
			$dropToggles.on(hmBrowser.touchstart + " keydown", function(event){
			if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
			if (event.type != "keydown") {
				event.preventDefault();
				}
			var toggleArgs = {method: "HMToggle", obj: $(this), clicked: true};
			hmWebHelp.extFuncs("hmDoToggle",toggleArgs);
			});
			if ($dropIcons.length > 0)
			$dropIcons.on(hmBrowser.touchstart, function(event){
			event.preventDefault();
			var toggleArgs = {method: "HMToggleIcon", obj: $(this), clicked: true};
			hmWebHelp.extFuncs("hmDoToggle",toggleArgs);
			});
			// Set hamburger menu item based on whether there are toggles open on the page
			if ($dropToggles.filter("[data-state='1']").length > 0) {
				$("svg#showhide_toggles_icon").find("use").attr("xlink:href","#eye-off");
				$("li#showhide_toggles span").first().html("Hide Expanding Text");
			} else {
				$("svg#showhide_toggles_icon").find("use").attr("xlink:href","#eye");
				$("li#showhide_toggles span").first().html("Show Expanding Text");
			}
			
		}

		// Page border in narrow windows when not embedded
		function manageBorders() {
		var toWide = false,
			toNarrow = false,
			activeBorderWidth = "thin",
			bOffset = parseInt((activeBorderWidth == "thin" ? "1" : activeBorderWidth)) * 2,
			viewportWidth = $("div#helpwrapper").width(),
			windowWidth = $(window).width() - bOffset;

		if (windowWidth <= viewportWidth) {
			$("div#helpwrapper").css("border-width","0 0");
			}
			
		function doBorders() {
				viewportWidth = $("div#helpwrapper").width(),
				windowWidth = $(window).width() - bOffset;
				
			if (windowWidth > viewportWidth && !toWide) {
				toWide = true;
				toNarrow = false;
				$("div#helpwrapper").css("border-width","0 " + activeBorderWidth);
				}
			else if (windowWidth <= viewportWidth && !toNarrow) {
				toNarrow = true;
				toWide = false;
				$("div#helpwrapper").css("border-width","0 0");
			}
		}
		let resizeEnd = new Date().getTime();
		$(window).on("resize", function() {
			let resizeStart = new Date().getTime();
			if (resizeStart-resizeEnd < 500) return;
			doBorders();
			resizeEnd = new Date().getTime();
			});
	  }
	if (!hmDevice.embedded && hmDevice.desktop) { 
		var mB = new manageBorders();
		}
	
		// Page border and Hamburger Menu entry and page border for aspx under SharePoint

		if (hmDevice.embedded && !hmDevice.phone) {
			
			if (hmDevice.desktop) {
				if (true) {
					$("div#helpwrapper").css("border", "thin solid #888888");
				}
				else {
					$("div#helpwrapper").css("border", "0 solid #888888");
				}
				
			}
			
			if (hmBrowser.isaspx ) {
				let $hmb_zoomentry = $("a[onclick='hmWebHelp.doFullEmbedWindow(this)']");
				$hmb_zoomentry.attr("title","Open this documentation in a new tab").attr("aria-label","Open this documentation in a new tab");
				$hmb_zoomentry.children("span").first().text("Open in New Window");
			}
		}
		
		// Inline Text Toggles
		if ($inlineToggles.length > 0) {
			$inlineToggles.on(hmBrowser.touchstart, function(event){
				event.preventDefault();
				hmWebHelp.extFuncs("hmDoInlineToggle",$(this));
			});
		}
		
		// Image Toggles

		$("a.imagetogglelink").on("click", function(event){
			event.preventDefault();
			}).on("keydown", function(event){
				if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
				$(this).find("img").click();
			});

		$("img.image-toggle,svg.image-toggle-magnifier").on("click", function(event){
			event.preventDefault();
			let $thisImg = $(this).parent().children("img").first();
			if (hmDevice.device === "phone")
				hmWebHelp.extFuncs("hmImageToggleMobile",$thisImg);
			else
				hmWebHelp.extFuncs("hmImageToggle",$thisImg);
		});
		
		// Video lightboxes 
		if ($videoToggles.length > 0) {
			$videoToggles.each(function(){
				$(this).children().first("div").on("click", function(event){
				event.preventDefault();
				event.stopPropagation();
				var vData = {};
				vData.data = $(this).attr("data-lightbox");
				vData.vWidth = $(this).attr("data-width");
				vData.vHeight = $(this).attr("data-height");
				vData.$obj = $(this);
				hmWebHelp.extFuncs("hmVideoBox",vData);
				});
			});
		} // video lightboxes
		
		// Responsive xTables, tap images and layout images for mobile devices
		if (!hmDevice.desktop) {	
			
			// xTables	
			var $XTables = $("table.xResponsive");
			if ($XTables.length > 0) {
				hmWebHelp.extFuncs("hm_xTablesInit",$XTables);
			} else {
				$(window).off(hmBrowser.orientationevent + ".xTables");
				}
			
			// Tap images
			hmWebHelp.initTapImages();

			// Layout Tables
				var tableCounter = 0;
			var tableType = hmDevice.phone ? "table.layout-table,table.layout-table-phone,table.layout_table,table.layout_table_phone" : "table.layout-table,table.layout-table-tablet,table.layout_table,table.layout_table_tablet";
				$(tableType).each(function() {
					tableCounter++;
					$(this).hide().attr("id","ltable" + tableCounter).before(
						'<div class="openLTable" onclick="hmWebHelp.extFuncs(\'toggleLayoutTable\',{action: \'show\',table:\'table#ltable'+tableCounter+'\',obj:this})">Tap to View Table</div>');
					});
			} // Mobile browsers

		
};

// Splitter object for horizontal width adjustments, must be instantiated as an object
hmWebHelp.hmCreateVSplitter = function(leftdiv, rightdiv) {
	var oldX,
	navWidthV,
	minWidthV,
	oldLeftV,
	oldWidthV,
	oldSplitL,
	navWidthOffset = hmpage.$navwrapper.width() - hmpage.$navcontainer.width(),
	startTime = 0,
	dragTime = 0,
	$dragsurface = $('div#dragsurface'),
	$dragarrows = $('img#leftright'),
	dragcount = 0;
	
	$dragarrows.hide();
	
	// Get the type of interaction event from user
	function EventType(e) {
		if (e.pointerType == "mouse" || e.pointerType == 4)
			return "mouse";
		else if (e.pointerType == "touch" || e.pointerType == 2 || e.pointerType == "pen" || e.pointerType == 3)
			return "touch";
		else if (/^mouse/i.test(e.type)) 
			return "mouse";
		else if (/^touch/i.test(e.type) || /^pen/i.test(e.type)) 
			return "touch";
		else return "none";
		}
	
	// Perform this at the end of a drag operation
	function endDrag(e) {
		dragTime = new Date().getTime() - startTime;
		if (dragTime < 200 || hmpage.$navwrapper.width() < 161 || hmpage.$navwrapper.offset().left < 0) {
			var navw = hmpage.$navwrapper.width();
			hmTocWidth = navw > 161 ? navw : 220;
			hmWebHelp.pageDimensions.dragHandle(false);
		} else if (hmpage.$navwrapper.width() >= 161 ) {
			hmTocWidth = hmpage.$navwrapper.width();
			setTimeout(hmWebHelp.adjustTopicPos,300);
			}
		$dragarrows.off(".endevents");
		hmpage.$navsplitbar.off(".moveevents");
		hmpage.$navsplitbar.off(".endevents");
		hmpage.$navhandle.off(".moveevents");
		hmpage.$navhandle.off(".endevents");
		$dragsurface.off(".moveevents");
		$dragsurface.off(".endevents");
		$dragsurface.hide();
		$dragarrows.hide();
		hmpage.navWidth = hmpage.FnavWidth();
		sessionVariable.setPV("navWidth",hmpage.navWidth.toString());
		hmWebHelp.nsheader();
		hmWebHelp.fHeadUpdate();
		hmWebHelp.PreventDefault(e);
		}	
	
	// Triggered at beginning of a drag
    function startDrag(e) {
		hmWebHelp.PreventDefault(e);
		startTime = new Date().getTime();
		var touchobj;
		if (typeof e.changedTouches != 'undefined') 
		touchobj = e.changedTouches[0];
		else touchobj = e;
		oldX = (!(document.all && !window.opera)) ? touchobj.pageX : touchobj.clientX;
		oldY = (!(document.all && !window.opera)) ? touchobj.pageY : touchobj.clientY;
		navWidthV = hmpage.FnavWidth();
		maxWidthV = hmpage.FmaxNavWidth();
		minWidthV = hmpage.FminNavWidth();
		oldLeftV  = $(rightdiv).position().left;
		oldWidthV = $(rightdiv).outerWidth();
		oldSplitL = hmpage.$navwrapper.offset().left;

		// Activate the drag surface overlay
		if (hmBrowser.touch || hmDevice.winphone || EventType(e) == "mouse") {
		$dragsurface.show();
		$dragsurface.on(hmBrowser.touchmove, function(event) {
			var ev = event.originalEvent; 
			performDrag(ev);
			});
		$dragsurface.on(hmBrowser.touchend, function(event) {
			var ev = event.originalEvent; 
			endDrag(ev);
			});
		} 
		
		hmpage.$navsplitbar.on(hmBrowser.touchmove, function(event) {
			var ev = event.originalEvent; 
			performDrag(ev);
			});
		hmpage.$navsplitbar.on(hmBrowser.touchend, function(event) {
			var ev = event.originalEvent; 
			endDrag(ev);
			});
		hmpage.$navhandle.on(hmBrowser.touchmove, function(event) {
			var ev = event.originalEvent; 
			performDrag(ev);
			});
		hmpage.$navhandle.on(hmBrowser.touchend + " keyup.endevents", function(event) {
			if (event.type == "keyup" && !hmKeys.doenter.includes(event.key)) return;
			var ev = event.originalEvent; 
			endDrag(ev);
			});

	}
	
	// Drag action
	function performDrag(e) {
		hmWebHelp.PreventDefault(e);
		var touchobj;
		if (typeof e.changedTouches != 'undefined') { 
				touchobj = e.changedTouches[0];
			} else {
				touchobj = e;
			}
		// Only move once every x events on mobile for lower processor load 
		dragcount++;
		if ( hmDevice.desktop || dragcount > 2 ) {
		dragcount = 0;
		dragTime = new Date().getTime() - startTime;
		
		// Show the drag arrows indicator on touch devices
		if (hmBrowser.touch && EventType(e) != "mouse" && $dragarrows.is(":hidden") && dragTime > 80) {
			$dragarrows.show();
		$dragarrows.css("top", (hmpage.$pageheader.is(":visible") ? (oldY-(hmpage.$pageheader.height()+30)) + "px" : (oldY) + "px"));
		$dragarrows.on(hmBrowser.touchend, function(event) {
			var ev = event.originalEvent; 
			endDrag(ev);
			});
			} 
		
		var moveX = (!(document.all && !window.opera)) ? touchobj.pageX - oldX : touchobj.clientX - oldX;
		var moveY = (!(document.all && !window.opera)) ? touchobj.pageY : touchobj.clientY;
		var newNavW = navWidthV + navWidthOffset + moveX;
		if ((newNavW <= maxWidthV) && (newNavW >= minWidthV) && !hmpage.navclosed) {
			hmpage.$navwrapper.css("width", (newNavW + 'px'));
			if (!hmpage.topicleft) {
				hmpage.$topicbox.css("left",(oldLeftV + newNavW - (navWidthV + navWidthOffset)) + 'px');
				}
			$dragarrows.css("top", (hmpage.$pageheader.is(":visible") ? moveY-(hmpage.$pageheader.height()+30) + "px" : (moveY) + "px"));
			}
		hmTocWidth = hmpage.$navwrapper.width();
		}
		
	} // performDrag();
	
	 $("div#dragwrapper").on(hmBrowser.touchstart + " keydown", function(event) {
		if (typeof(event.button) != "undefined" && event.button !== 0) return;
		if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
		event.stopPropagation();
		var ev = event.originalEvent; 
		startDrag(ev);
		}); 
	
	$("div#navsplitbar").on(hmBrowser.touchstart, function(event) {
		var ev = event.originalEvent; 
		startDrag(ev);
		});

}; // createSplitter

// Close menus on click outside the current menu
hmWebHelp.unClicker = function(elem1, elem2) {
	
	var currentFocus = document.getElementById(elem1),
		secondFocus = elem2 ? document.getElementById(elem2) : currentFocus;
		
		$(document).on(hmBrowser.touchstart + '.closemenu', function(event){
			if (!currentFocus.contains(event.target) && !secondFocus.contains(event.target)) {
				$(document).off(hmBrowser.touchstart + '.closemenu');
				hmWebHelp.closeMenus();
			}
		});
	} //  unClicker

// Initialize banner menu system
// Load scripts and initialize main page
hmWebHelp.hmMainPageInit = function() {

	  var setNavWidth = sessionVariable.getPV("navWidth");
	  hmpage.splitter = new hmWebHelp.hmCreateVSplitter("div#navwrapper","main#topicbox");

		if (setNavWidth !== null)
		  hmWebHelp.resizePanes(parseInt(setNavWidth,10));
		else
		hmWebHelp.resizePanes(parseInt(hmpage.FnavWidth(),10));
	   hmWebHelp.pageDimensions = new hmWebHelp.pageDims();
	   if (hmDevice.embedded) hmWebHelp.pageDimensions.embedInit();
	   if (((hmpage.headerclosedonopen && !hmDevice.embedded) || (true && hmDevice.embedded)) && !hmDevice.phone) {
		   hmWebHelp.pageDimensions.pageHeaderUpDown(false);
		   $("svg#toolbar_updown_close").hide();
	   } else {
		   $("svg#toolbar_updown_open").hide();
	   }
	   if (hmDevice.ipad && !window.navigator.standalone) {
		$("body").css({"bottom": "-4rem"});
	   }
		//if (hmDevice.desktop)
			hmWebHelp.pageDimensions.doDims();
		//else
			hmWebHelp.pageDimensions.navShadow();
		if (hmpage.tocclosedonopen && !hmpage.navclosed && hmpage.topicleft)
		hmWebHelp.pageDimensions.dragHandle(true);
		// Tell TOC and search panes that main page is ready
		xMessage.sendObject("hmnavigation", {action: "sendvalue", vn: "hmDevice.mainPageInitialized", vv: "*true"});
		xMessage.sendObject("hmsearch",{action: "sendvalue", vn: "hmDevice.mainPageInitialized", vv: "*true"});


	// Bind to history StateChange Event
		History.Adapter.bind(window,'statechange', function(){ 
		
		// Checking for empty currentTopicID prevents double load on start
		// if the URL is a path without a file name
		var State = History.getState(),
			stateID = State.hash,
			serverBang = hmBrowser.server ? "\?" : "\!",
			stateID = stateID.substr(stateID.lastIndexOf("\/")+1),
			searchQ = State.hash.lastIndexOf("q\=") > -1 ? State.hash.substr(State.hash.lastIndexOf("q\=")) : "",
			anchorQ = State.hash.lastIndexOf("anchor\=") > -1 ? State.hash.substr(State.hash.lastIndexOf("anchor\=")) : "";
			searchQ = searchQ.length > 0 ? serverBang + searchQ.split("\&")[0] : "";
			anchorQ = anchorQ.length > 0 ? serverBang + anchorQ.split("\&")[0] : "";
			stateID = stateID.indexOf(serverBang) > -1 ? stateID.substr(0,stateID.lastIndexOf(serverBang)) : stateID;

			if ((stateID + anchorQ != hmpage.currentTopicID || hmFlags.searchHighlight != "") && hmpage.currentTopicID !== "") {
				hmpage.currentTopicID = stateID + anchorQ;
				hmpage.hmHelpUrl = hmWebHelp.parseState(hmpage.currentTopicID);
				hmpage.currentURI = encodeURI(document.location.href);
				hmWebHelp.tocNav({action: "href", href: hmpage.hmHelpUrl.topic, bs: false});
				hmWebHelp.loadTopic(hmpage.hmHelpUrl);
			}
		});
	
		if (hmpage.hmHelpUrl.topic != hmFlags.hmCurrentPage) {
			hmWebHelp.currentAnchor = hmpage.hmHelpUrl.anchor !== "" ? hmpage.hmHelpUrl.anchor : false;			
			hmWebHelp.loadTopic(hmpage.hmHelpUrl);
		}

	if (hmpage.hmHelpUrl.topic !== "") {
		if (hmpage.hmHelpUrl.anchor !== "") {
			History.replaceState(null,null,hmpage.hmHelpUrl.topic + hmpage.anchorX + hmpage.hmHelpUrl.anchor);
			if (hmpage.currentnav === 0) {
				setTimeout(function(){
				xMessage.sendObject("hmnavigation",{action: "callfunction", fn: "tocSource.findElement", fa: (hmpage.hmHelpUrl.topic + "#" + hmpage.hmHelpUrl.anchor)});
					},300);
				}
			} else {
			// Reset title to current title to stop it getting deleted
			History.replaceState(null,$("title").text(),hmpage.hmHelpUrl.topic);
		}
	}

	// Tap response on navhandle
	hmpage.$navhandle.on(hmBrowser.touchstart, function(event){
		$('div#dragwrapper').addClass('draghilite');
		setTimeout(function(){
		$('div#dragwrapper').removeClass('draghilite');
		},50);
	});
	
	// Main page header on/off on desktop browsers
		$("li#showhide_pageheader").on(hmBrowser.touchstart, function(event){
			//Catch right-clicks
			if (typeof(event.button) != "undefined" && event.button !== 0) return;
			hmWebHelp.pageDimensions.pageHeaderUpDown(false);
		});
		$("li#showhide_pageheader a").on("keydown", function(event){
			if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
			hmWebHelp.pageDimensions.pageHeaderUpDown(false);
		});
	
	// Button tab for show/hide toolbar
		$("div#toolbutton_wrapper").on(hmBrowser.touchstart + " keydown", function(event){
			if (typeof(event.button) != "undefined" && event.button !== 0) return;
			if (event.type == "keydown" && !hmKeys.doenter.includes(event.key)) return;
			hmWebHelp.pageDimensions.pageHeaderUpDown(false);
		});

	// Bind tab switching click/tap handlers

	$("input#search_input").on(hmBrowser.touchstart + " keydown", function(event) {
		if ($("div#contentsbox").is(":visible")) {
			hmWebHelp.switchNavTab('search');
			}
			$(this).attr("placeholder","");
			}).attr("placeholder", "Search");

	$("a#contentslink").on(hmBrowser.touchstart + " keydown", function(event) {
		if (typeof(event.button) != "undefined" && event.button !== 0) return;
		event.preventDefault();
		if ($("div#searchbox").is(":visible")) {
			hmWebHelp.switchNavTab('contents');
			$("input#search_input").val("").attr("placeholder", "Search");
			xMessage.sendObject("hmsearch",{action: "callfunction", fn: "clearSearchField"});
			}});

	$("svg#clear_search_button").on(hmBrowser.touchstart, function(event) {
		if (typeof(event.button) != "undefined" && event.button !== 0) return;
		if ($("div#searchbox").is(":visible")) {
			$("input#search_input").val("").attr("placeholder", "Search");
			xMessage.sendObject("hmsearch",{action: "callfunction", fn: "clearSearchField"});
			}});
	$("a#searchlink").on(hmBrowser.touchstart + " keydown", function(event){
		if (typeof(event.button) != "undefined" && event.button !== 0) return;
		event.preventDefault();
		if (event.type == "keydown" && event.keyCode != 13) return;
		hmWebHelp.switchNavTab('search');
		}).on("click", function(event){event.preventDefault();});

		if (hmTocLoaded) {
			hmWebHelp.syncToc(true);
		} else {
		var tocCheck = setInterval(function() {
			if (hmTocLoaded) {
				hmWebHelp.syncToc(true);
				clearInterval(tocCheck);
			}
			},100);
		}

/* Mobile browsers  */
if (!hmDevice.desktop) {
	
	// Disable permalink on mobile
	$("li#savepermalink").hide();
	
	hmWebHelp.funcs.doubleTap = new hmWebHelp.dT();
	hmWebHelp.funcs.doPagePos = new hmWebHelp.pagePos("div#hmpagebody");
	if (hmDevice.mobileSleepReload) 
		hmWebHelp.reloadAfterSleep();

	} // mobile browsers

	hmWebHelp.hmTopicPageInit();
	
	// Set current ref ID for the first page loaded 
	// (Set by loadtopic for subsequent pages)
	hmpage.currentTopicID = History.getState().hash;
	hmpage.currentTopicID = hmpage.currentTopicID.lastIndexOf("\/") > -1 ? hmpage.currentTopicID.substr(hmpage.currentTopicID.lastIndexOf("\/")+1) : hmpage.currentTopicID;
	if (hmpage.currentTopicID.indexOf("\?") > -1)
	hmpage.currentTopicID = hmpage.currentTopicID.substr(0,hmpage.currentTopicID.lastIndexOf("\?"));
	
	// Initialize the dropdown menu
	if (hmWebHelp.initTopNav) hmWebHelp.initTopNav();
	
	// Flag the page as initialized 
	hmpage.initialized = true;

};  // hmWebHelp.hmMainPageInit()

// Main page dimensions function, encapsulates all its own functions and variables
// Must be instantiated as an object
hmWebHelp.pageDims = function() {
		
		var animate = false,
		resizedragcount = 0,
		staticWindow = true,
		widthChange = false,
		heightChange = false,
		leftmargin = 0;
		
		// Shadow for navigation pane when it overlaps the rest of the page
		function navShadow() {

		var mtoolbar = false;
		if (typeof hmpage.$mToolbar != "undefined") {
			if (hmpage.$mToolbar.attr('data') == 'open')
			mtoolbar="open";
				else mtoolbar="closed";
			}
		if (hmpage.navclosed) {
			$("svg#draghandleicon_l").hide();
			$("svg#draghandleicon_r").show();
		} else {
			$("svg#draghandleicon_r").hide();
			$("svg#draghandleicon_l").show();
		}
			} // navShadow()
		
		// Current width of the nav pane
		function tocWidth() {
			// return hmTocWidth;
			var currentTocWidth = 0;
			if (!hmpage.navclosed) {
				do {
					currentTocWidth = hmFlags.tocInitWidth;
				}
				while (hmFlags.tocInitWidth === 0); 
			} else  {
			// currentTocWidth = hmFlags.tocInitWidth;
			currentTocWidth = parseInt(currentTocWidth,10) + 20;
			}
			hmFlags.tocInitWidth = currentTocWidth;
			return currentTocWidth;
			}
		
		/* Individual functions for each nav pane / topic move operation */	
		
		function phUpDown() {
		
		var headerPos = "0.0rem",
			navboxPosDown = hmpage.Fpix2em(hmpage.$navwrapper.position().top) + "rem",
			topicPosDown = hmpage.Fpix2em(hmpage.$topicbox.position().top) + "rem",
			topicPosUp = "0rem",
			navboxPosUp = "0rem",
			menuPosUp = hmpage.Fpix2em(hmpage.$pageheader.outerHeight()) + "rem",
			menuPosDown = hmpage.Fpix2em(hmpage.$headerwrapper.outerHeight() +  hmpage.$pageheader.outerHeight()) + "rem",
			$bothBoxes = $("div#navwrapper, main#topicbox"),
			headerOn = function(){return (hmpage.$headerbox.height() > 0);},
			inProgress =  false,
			hboxHeight = (3.0 + hmpage.Fpix2em(parseInt("1px",10))) + "rem";
			
		return function(animOff) {

			if (inProgress) return;
			inProgress = true;
			var reset = false;
			if ((animOff && animate) || (!hmpage.initialized && animate)) {
				animate = false;
				reset = true;
				} else animate = true;

			if (headerOn()) {
				$("svg#toolbar_updown_close").hide();
				$("svg#toolbar_updown_open").show();
				navboxPosDown = hmpage.Fpix2em(hmpage.$navwrapper.position().top) + "rem";
				topicPosDown = hmpage.Fpix2em(hmpage.$topicbox.position().top) + "rem";
				hmpage.$navwrapper.animate({
					"top": navboxPosUp
				},(animate ? 400 : 0));
				hmpage.$hamburgermenu.animate({
					"top": menuPosUp
				},(animate ? 400 : 0));
				hmpage.$topicbox.animate({
					"top": topicPosUp
				},(animate ? 400 : 0));
				hmpage.$headerbox.animate({
					"height": 0
				},(animate ? 400 : 0), function(){
				if (reset) animate = true;
				hmpage.$headermenu.hide();
				$("div#headerwrapper").css("border-bottom","none");
				$("svg#showhide_header_icon").find("use").attr("xlink:href","#expand");
				$("li#showhide_pageheader a").first().attr("title","Show Page Header").attr("aria-label","Show Page Header");
				$("div#toolbutton_wrapper").attr("aria-label","Show Page Header");
				$("li#showhide_pageheader span").first().html("Show Page Header");
				$("ul#main-menu, div#homebutton").hide();
				inProgress = false;
				hmWebHelp.nsheader();
				sessionVariable.setSV("headerState","closed");
				$("div#headerbox").hide();
				});
			} else {
			$("svg#toolbar_updown_open").hide();
			$("svg#toolbar_updown_close").show();
			$("div#headerbox").show();
			navboxPosUp = hmpage.Fpix2em(hmpage.$navwrapper.position().top) + "rem";
			topicPosUp = hmpage.Fpix2em(hmpage.$topicbox.position().top) + "rem";
			$("ul#main-menu, div#homebutton").show();
			hmpage.$headermenu.show();
			hmpage.$navwrapper.animate({
					"top": navboxPosDown
				},(animate ? 400 : 0), function(){
					inProgress = false;
				});
			hmpage.$hamburgermenu.animate({
					"top": menuPosDown
				},(animate ? 400 : 0));
			hmpage.$topicbox.animate({
					"top": topicPosDown
			},(animate ? 400 : 0));
			$("div#headerwrapper").attr("style","");
			hmpage.$headerbox.animate({
				"height": hboxHeight
			},(animate ? "400" : "0"), function(){
				if (reset) animate = true;
				$("svg#showhide_header_icon").find("use").attr("xlink:href","#collapse");
				$("li#showhide_pageheader a").first().attr("title","Hide Page Header");
				$("li#showhide_pageheader span").first().html("Hide Page Header");
				$("div#toolbutton_wrapper").attr("aria-label","Hide Page Header");
				hmWebHelp.nsheader();
				sessionVariable.setSV("headerState","open");
			});
			}
			};
		}
		
		var pageHeaderUpDown = new phUpDown();
		
		function moveTopicLeft(animOff) {	
		hmpage.topicadjust = true;
		var reset = false;
			if ((animOff && animate) || (!hmpage.initialized && animate)) {
				animate = false;
				reset = true;
				} else animate = true;
			hmpage.topicleft = true;
			
			var leftVal = hmpage.borderWidth * -1;

			var bottomVal = hmFlags.skinType != "flat" ? "0.5rem" : "0.0rem";
			
			hmpage.$topicbox.animate({
				left: leftVal,
				// top: topVal,
				bottom: bottomVal
				},animate ? 250 : 0, function(){
					if (!hmpage.navclosed && !hmpage.$navtools.first().hasClass("over")) {
						hmpage.$navtools.addClass("over");
					}
					else {
						hmpage.$navtools.removeClass("over");
					}
					hmWebHelp.nsheader();
					hmWebHelp.fHeadUpdate();
					if (reset) animate = true; 
					setTimeout(hmWebHelp.adjustTopicPos,300);
					});
			navShadow();
			}
		
		function moveTopicRight(animOff) {
			hmpage.topicadjust = true;
			hmpage.$navtools.removeClass("over");
			var reset = false,
			
			//leftVal = hmpage.$navwrapper.width() + leftmargin + hmpage.Fem2pix(hmpage.navboxoffset),
			leftVal = hmpage.$navcontainer.width(),
			bottomVal = hmFlags.skinType != "flat" ? "0.5rem" : "0.0rem";
			
			if ((animOff && animate) || (!hmpage.initialized && animate)) {
				animate = false;
				reset = true;
				} else animate = true;
			hmpage.topicleft = false;
			hmpage.$topicbox.animate({
			left: leftVal,
			bottom: bottomVal
			},animate ? 250 : 0, function() {
				hmWebHelp.nsheader();
				hmWebHelp.fHeadUpdate();
				if (reset) animate = true; 
				setTimeout(hmWebHelp.adjustTopicPos,300);
				});
			navShadow();
			}
			
		function moveTOCLeft(animOff) {
			hmpage.navWidth = hmpage.FnavWidth();
			var reset = false;
			if ((animOff && animate) || (!hmpage.initialized && animate)) {
				animate = false;
				reset = true;
				} else animate = true;
			hmpage.navclosed = true;
			hmTocWidth = hmpage.$navwrapper.width();
			hmpage.$navwrapper.animate({
					left: -(hmpage.$navcontainer.width() + hmpage.borderWidth) + "px"
				},animate ? 250 : 0, function(){
					$("svg#draghandleicon_l").hide();
					$("svg#draghandleicon_r").show();
					hmpage.$dragwrapper.attr("aria-label","Show Navigation Pane");
					hmpage.$navtools.removeClass("over");
					if (reset) animate = true; 
					navShadow();
					sessionVariable.setSV("tocState","closed");
				});
			}
			
		function moveTOCRight(animOff) {
			var reset = false;
			$("svg#draghandleicon_r").hide();
			$("svg#draghandleicon_l").show();
			hmpage.$dragwrapper.attr("aria-label","Hide Navigation Pane");
			if (hmpage.topicleft && !hmpage.$navtools.first().hasClass("over")) {
				hmpage.$navtools.addClass("over");
			}
			else {
				hmpage.$navtools.removeClass("over");
			}
			if ((animOff && animate) || (!hmpage.initialized && animate)) {
				animate = false;
				reset = true;
				} else animate = true;
			hmpage.navclosed = false;
			hmpage.$navwrapper.animate({
					left: leftmargin
				},animate ? 250 : 0, function() {
					if (reset) animate = true; 
					navShadow();
					sessionVariable.setSV("tocState","open");
					});
		}

		// Init for embedded WebHelp $$$
		function embedInit() {
			if (!hmDevice.embedded && hmDevice.desktop) {
				if (hmpage.$headerbox.is(":hidden") && !false)
					hmWebHelp.pageDimensions.pageHeaderUpDown(true);
				if (hmpage.breadcrumbs)
					$("p#ptopic_breadcrumbs").show();
			} else if (hmDevice.embedded && (hmDevice.tablet || hmDevice.phone)) {
				parentdomain = (/^(https?:\/\/.*?)\/.*?$/i).exec(document.referrer)[1];
				xMessage.sendObject("parent",{action: "callfunction", fn: "hmHelp.doFullWindow", domain: parentdomain});
			}
		} // embedInit()
	
	// Check for switch between narrow/wide and short/tall window
	function aspectChange(mode) {
			var changed = false;
			switch (mode) {
				case "both":
				if (hmpage.Fnarrowpage() !== hmpage.narrowpageX || hmpage.Fshortpage() !== hmpage.shortpageX) {
					changed = true;
					hmpage.narrowpageX = !hmpage.narrowpageX;
					hmpage.shortpageX = !hmpage.shortpageX;
					} 
				break;
				case "width":
				if (hmpage.Fnarrowpage() !== hmpage.narrowpageX) {
					changed = true;
					hmpage.narrowpageX = !hmpage.narrowpageX;
					} 
				break;
				case "height":
				if (hmpage.Fshortpage() !== hmpage.shortpageX) {
					changed = true;
					hmpage.shortpageX = !hmpage.shortpageX;
					} 
				break;
			
			}
			return changed;	
		}
	// Adjust relation between panes after changes
	function resetNavRelation() {
		var navwidth = hmpage.FnavWidth(),
			maxwidth = hmpage.FmaxNavWidth(),
			minwidth = hmpage.FminNavWidth();
		if (navwidth > maxwidth) {
		hmWebHelp.resizePanes(maxwidth);
		}
		if (navwidth < minwidth) {
		hmWebHelp.resizePanes(minwidth);
		}
	}
	// Check and adjust page dimensions
	function doDims() {
		// Resize panes if the page resize makes nav pane too wide
		// in relation to the topic pane
		resetNavRelation();
		// Narrow page layout for desktop with narrow window
		if (!hmpage.topicleft && hmpage.Fnarrowpage()) {
			moveTopicLeft();
			if (true)
				moveTOCLeft();
			}
		if (hmpage.topicleft && !hmpage.navclosed && !hmpage.Fnarrowpage()) {
			moveTopicRight();
			if (hmpage.navclosed)
			moveTOCRight();
			}
		navShadow();
		// Exit directly if there is no change in window aspect
		// This reduces processing overhead considerably!!
		if (!aspectChange("both") && hmpage.initialized && hmDevice.desktop) {
			return;
			}
			var timeout = 50;
			var dragcount = 0;
			if (hmBrowser.touch && animate) animate = false;
		// Width changed or first open or non-desktop device?
		if ( !hmpage.initialized || aspectChange("width")) {
			if (hmpage.Fnarrowpage()) {
					moveTopicLeft();
					if ((true && hmDevice.desktop) || (true && !hmDevice.desktop))
						moveTOCLeft();
					navShadow();
			} else if (hmpage.Fnarrowpage() && hmpage.topicleft) {
					moveTopicRight();
				if (!hmpage.navclosed) {
					if (staticWindow) moveTopicRight();
				}
				else {
					// tocSet();
					moveTOCRight();
				}
				navShadow();
				} 
		} // if widthChange 
		
		// Turn animation back on after initializing page on first load
		animate = true;
		hmpage.initialized = true;
		} // doDims() function
		

		// Nav open/close function 
		function dragHandle(animOff) {
			if (!animOff) animOff = false;
			if (hmWebHelp.funcs.doVibrate && !animOff) hmWebHelp.funcs.doVibrate();
			if (hmpage.Fnarrowpage()) {
				if (!hmpage.navclosed) {
					moveTOCLeft(animOff);
					if (!hmpage.topicleft && hmDevice.tablet && hmBrowser.Flandscape())
						moveTopicLeft();//$$$
				} else {
					// tocWidth();
					moveTOCRight(animOff);
					if (hmpage.topicleft && hmDevice.tablet && hmBrowser.Flandscape())
						moveTopicRight(); //$$$
				}
				navShadow();
			} else  {
				if (!hmpage.navclosed) {
					// tocWidth();
					moveTOCLeft(animOff);
					moveTopicLeft(animOff);
				}
				else {
					// tocWidth();
					//$$$if (!hmDevice.phone && !(hmDevice.tablet && !hmBrowser.Flandscape())) 
					moveTopicRight(animOff);
					moveTOCRight(animOff);
				}
				navShadow();
				}
				animate = true;
			} // Callable dragHandle() function:
	
		
		// Expose these methods for external calling
		return {
			tocWidth: tocWidth,
			doDims: doDims,
			embedInit: embedInit,
			dragHandle: dragHandle,
			navShadow: navShadow,
			moveTOCLeft: moveTOCLeft,
			moveTOCRight: moveTOCRight,
			moveTopicLeft: moveTopicLeft,
			moveTopicRight: moveTopicRight,
			pageHeaderUpDown: pageHeaderUpDown
			};
		
		}; // End of main PageDims function

/* Topic scroll position handler for leaving and returning to page */
/* Must be instantiated as an object */
/* Can be instantiated as often as needed for multiple calls */
hmWebHelp.pagePos = function(elem) {
	var xPos = 0, yPos = 0;
	var scrollNode = elem;
	function getP() {
			yPos = $(scrollNode).scrollTop();
			xPos = $(scrollNode).scrollLeft();
	}
	function setP(speed) {
			if (!speed) {
				$(scrollNode).scrollTop(yPos).scrollLeft(xPos);
			} else {
				$(scrollNode).animate(
				{scrollTop: yPos, scrollLeft: xPos},
				speed
				);
			}
			yPos = 0; xPos = 0;
	}
	
	return {
		getPos: function() {
			getP();
		},
		setPos: function(speed) {
			setP(speed);
		}
	};
	
	}; // pagePos()

/* Double-tap handler for external functions */
hmWebHelp.dT = function() {
	var firstTap = new Date().getTime();
	var f = null;	
	
		return function(func, args) {

			var newTap = new Date().getTime();
			var checkTap = newTap - firstTap;
			firstTap = newTap;
			if ((checkTap > 120) && (checkTap < 500) && f === func) {
				hmWebHelp.extFuncs(func,args);
				}
			f = func;
		};
	};

// Check invalid hrefs and convert anchor references to ?anchor= format required by history
hmWebHelp.targetCheck = function(t) {
	if (!t || t.substr(0,11) == "javascript:")
			return "";
		if (t.indexOf("#") > -1) t = t.replace("#",hmpage.anchorX);
			return t;
	};

// Set a value for a specific period, then reset it
// Currently only used for the toc clicked flag, which should only be active briefly
hmWebHelp.timedReset = function(vari,valu,timer) {
		vari = valu;
		setTimeout(function(){
			vari = !valu;
			},timer);
	};

// Flash paragraph of target anchor on arrival
hmWebHelp.flashTarget = function(obj,repeat,delay) {
	if (!true) return;
	repeat--;
	doFlash();
	function doFlash() {
	setTimeout(function() {
	$(obj).each(function() {
		$(obj).css("visibility","hidden");
		setTimeout(function() {
			repeat--;
			$(obj).css("visibility","visible");
			if (repeat > 0) doFlash();
			},delay);
	});
	},delay);
	} // doFlash;
}; // flashTarget	

// Universal topic scroller with togglejump for hidden anchors
hmWebHelp.scrollTopic = function(anchor, topic) {
	var $anchor, 
		$targetTParents = null,
		$targetThisToggle = null,
		$scrollBox = $("div#hmpagebody_scroller");
	
	// Target is already a jQ object
	if (typeof anchor !== "undefined" && typeof anchor !== "string") {
		$anchor = anchor;
	}
	else if (typeof anchor === "string") {
	// Make jQ target object
		anchor = anchor.replace(/\./g,"\\.");
		$anchor = anchor === "" ? false : $("a#" + anchor);
		if ($anchor && $anchor.length === 0) {
			$anchor = false;
		}
	}
	
	if ($anchor) {
		$targetTParents = $anchor.parents("div.dropdown-toggle-body");
		$targetThisToggle = $anchor.siblings("img.dropdown-toggle-icon").first();
		if ($targetThisToggle.length === 0)
			$targetThisToggle = $anchor.siblings("a.dropdown-toggle").first();
		if ($targetThisToggle.length == 1) {
			$targetThisToggle = $targetThisToggle.attr("id");
			$targetThisToggle = $targetThisToggle.substr(0,$targetThisToggle.indexOf("_"));
			$targetThisToggle = $("a#" + $targetThisToggle + "_LINK").first();
		}
		else 
			$targetThisToggle = false;
	}
	
	if ($targetTParents && $targetTParents.length > 0) {
		hmWebHelp.extFuncs('hmDoToggle',{method: 'hmToggleToggles', obj: {toggles: $targetTParents, mode: "expand", scrolltarget: $anchor, dotoggle: $targetThisToggle}});
	} else if ($anchor) {
		// Allow time for navpane width to be adjusted before scrolling
		var scrollcount = 0;
		var checkscroll = setInterval(function(){
		scrollcount++; // Break after 4 seconds
		if (!hmpage.topicadjust || scrollcount > 80) {
		hmpage.topicadjust = false; // Reset, just in case
		$scrollBox.scrollTo($anchor,300,{axis: 'y', offset:{top:-20},
		onAfter:function(){
			if ($targetThisToggle) {				
				hmWebHelp.extFuncs('hmDoToggle',{method: 'HMToggle', obj: $targetThisToggle});
			} else 
				hmWebHelp.flashTarget($anchor.parent(),3,200);
		}});
		clearInterval(checkscroll);
		}
		},50);
	} else if (topic) {
		$scrollBox.scrollTop(topic);
	}
};
	
// Functions for loading topics

// Executed when a JS topic is loaded

function hmLoadTopic(topicObj) {
	// hmWebHelp.currentTopic = topicObj;
	
	var titleBarText = topicObj.hmTitle;
	switch ("topic") {
		case "project":
		titleBarText = $("h1#hm_pageheader").text();
		break;
		
		case "project_topic":
		titleBarText = $("h1#hm_pageheader").text() + " \> " + topicObj.hmTitle; 
		break;
	}
	titleBarText = $("<textarea/>").html(titleBarText).text();
	$("title").text(titleBarText);
	
	$("meta[name='keywords']").attr("content",topicObj.hmKeywords);
	//$("p#ptopic_breadcrumbs").html("Navigation: " +(topicObj.hmBreadCrumbs !== "" ? topicObj.hmBreadCrumbs : " \<span\>&raquo; No topics above this level" + " &laquo;\<\/span\>"));
	hmpage.hmDescription = typeof topicObj.hmDescription == "undefined" ? "" : topicObj.hmDescription;
	$("meta[name='description']").attr("content",hmpage.hmDescription);
	hmpage.hmPicture = typeof topicObj.hmPicture == "undefined" ? "" : topicObj.hmPicture;
	
	// Insert formatted or plain text topic header
	if (!hmFlags.hdFormat) {
			$("h1.topictitle").html(topicObj.hmTitle);
		} else {
			if (topicObj.hmHeader !== "")
			$("span.hdFormat").html(topicObj.hmHeader);
		else
			$("span.hdFormat").html('<h1 class="p_Heading1" style="page-break-after: avoid;"><span class="f_Heading1">'+topicObj.hmTitle+'</span></h1>');
		}
	$("div#hmpagebody_scroller").html(topicObj.hmBody + hmpage.topicfooter);
	
	// $$$ Initialize featured images if present and enabled
		if (false && hmpage.hmPicture !== "") {
			$("div#featureheader").remove();
			if (!hmDevice.phone) {
				hmWebHelp.extFuncs('hmFeatureHeader');
				}
			else  {
				hmWebHelp.extFuncs('hmFeatureHeaderM',"init");
			}
		}			
		else {
			$("div#featureheader").remove();
			if (!hmDevice.phone)
				$('div#hmpagebody_scroller').css({"padding-top": "0.5rem"});
			else 
				$('main#topicbox').css({"padding-top": "0.5rem"});
		}
}

// Main load routine, called by statechange event
hmWebHelp.loadTopic = function(newTopic) {

	var cacheTopic = "";
	if (hmpage.currentnav === 0)
		hmWebHelp.currentAnchor = false;
	var args = [];
	if (newTopic.topic === "") return;
	if (newTopic.anchor === "")
		hmWebHelp.currentAnchor = "";
	else
		hmWebHelp.currentAnchor = newTopic.anchor;
	// Null the autoTOC for the current topic
	$("#autoTocWrapper").html("");
	
	// Close the hamburger menu if it's  open
	if ($("div#navigationmenu").is(":visible"))
		hmWebHelp.hamburgerMenu();

	// Close image toggle if it's open
	if ($("div#imagetogglebox").is(":visible"))
		hmWebHelp.funcs.closeImageToggle(null,0);

	function topicPostLoad() {
		var acTarget = "";
		$("div#topicheaderwrapper span").addClass("wraptext");
		var $scrollBox = $("div#hmpagebody_scroller");
		if (newTopic.anchor !== ""){
			acTarget = hmpage.anchorX + newTopic.anchor;
			hmWebHelp.scrollTopic(newTopic.anchor,"");
		}
			else {
				hmWebHelp.scrollTopic("", newTopic.topic);
			}

		hmFlags.hmCurrentPage = newTopic.topic + acTarget;
		
		// Configure the topic navigation links etc. in the newly-loaded topic
		hmWebHelp.hmTopicPageInit();
		
		// Non-scrolling header update
		hmWebHelp.nsheader();
		hmWebHelp.fHeadUpdate();
		if (hmFlags.searchHighlight != "") {
			hmWebHelp.extFuncs("highlight");
		}
		// User function injection to execute after topic load
			try {
				
			}
			catch(err) {
			alert("ERROR executing user topic post-load function: \r\n\r\n" + err);
			}

	}

	cacheTopic = newTopic.jstopic.substr(newTopic.jstopic.lastIndexOf("\/")+1);
	
	if (Object.keys(hmWebHelp.visitedTopics).length > 300)
		hmWebHelp.visitedTopics = {};

	if (!hmWebHelp.visitedTopics.hasOwnProperty(cacheTopic)){

	// Load the new topic
	$.getScript(newTopic.jstopic, function(data, textStatus, jqxhr) {
			topicPostLoad();
			hmWebHelp.visitedTopics[cacheTopic] = true;
			}).fail(function(){
				History.pushState(null,null,hmFlags.hmMainPage);
				alert("ERROR Topic Not Found " + newTopic.topic);
			});
	} else {
		$.cachedScript(newTopic.jstopic).done(function(script,textStatus){
			topicPostLoad();
			});
	}
	}; // loadtopic

// Initialize images with "tap-image" class to make them toggle-expandable in touch devices
hmWebHelp.initTapImages = function() {
	var $tapimages = $("img.tap-image");
	if ($tapimages.length > 0){
		$("img.tap-image").each(function() {
			$(this).on(hmBrowser.touchstart, function(event) {
			var ev = event.originalEvent;			
			hmWebHelp.funcs.doubleTap('tapImage',this);
			});
		});}
	};

// Do an automatic reload after inactivity (sleep, off) on mobile devices

hmWebHelp.reloadAfterSleep = function() {

	function getTime() {
    return (new Date()).getTime();
	}

	var systole = getTime(),
		diastole = getTime(),
		flutter = 0,
		interval = 5000;

	function ecg() {
		diastole = getTime();
		flutter = diastole - systole;
		if (flutter > 120000){
	    	location.reload();
	    }
		systole = getTime();
	}
	setInterval(ecg,interval);
};

// Call full window function from embedded window

hmWebHelp.doFullEmbedWindow = function(obj) {
	var $objCaption = $(obj).children("span").first(),
		currentCaption = $objCaption.text(),
		current,
		newCaption = currentCaption == "Zoom Window Out" ? "Zoom Window In" : "Zoom Window Out",
		currentIcon = $("svg#fullscreen_toggle").find("use").attr("xlink:href"),
		newIcon = currentIcon == "#resize-full" ? "#resize-small" : "#resize-full",
		parentdomain = (/^(https?:\/\/.*?)\/.*?$/i).exec(document.referrer)[1];
		
		if (!hmBrowser.isaspx) {
			xMessage.sendObject("parent",{action: "callfunction", fn: "hmHelp.doFullWindow", domain: parentdomain});
			hmDevice.embedded = newIcon.indexOf("-small") < 0;
			hmWebHelp.pageDimensions.embedInit();
			$("svg#fullscreen_toggle").find("use").attr("xlink:href",newIcon);
			$objCaption.text(newCaption);
			hmWebHelp.hamburgerMenu();
		} else {
			window.open(document.location,"webhelp_window");
		}
	};

// Switch between the stacked navigation panes	
hmWebHelp.switchNavTab = function(targetTab) {

	hmWebHelp.closeMenus();
	
	$("li.current").removeClass("current").attr("aria-selected","false");
	$("li#" + targetTab + "tab").addClass("current").attr("aria-selected","true");
	$("div.navbox.on").removeClass("on").addClass("off");
	$("div#" + targetTab + "box").removeClass("off").addClass("on");

	// Load search page on first view 
	if (targetTab === "search" && $("iframe#hmsearch").attr("src") === "") {
		$("iframe#hmsearch").attr("src",hmFlags.hmSearchPage);
		}
	if (hmpage.navclosed) {
			hmWebHelp.pageDimensions.dragHandle(true);
			}
	switch (targetTab) {

		case "contents":
		xMessage.sendObject("hmnavigation",{action: "callfunction", fn: "tocSource.findElement", fa: hmFlags.hmCurrentPage});
		if (hmWebHelp.currentAnchor !== "")
			xMessage.sendObject("hmnavigation",{action: "callfunction", fn: "tocSource.findElement", fa: (hmFlags.hmCurrentPage + "#" + hmWebHelp.currentAnchor)});
		$("div#contents_link_wrapper, svg#contents_button").hide();
		hmWebHelp.currentAnchor = false;
		$("input#search_input").css("width", "calc(100% - 3rem)");
		hmpage.currentnav = 0;
		document.activeElement.blur();
		$("iframe#hmnavigation")[0].focus();
		break;
		case "search":
		$("input#search_input").css("width", "calc(100% - 4rem)");
		$("div#contents_link_wrapper, svg#contents_button").show();
		hmpage.currentnav = 2;
		document.activeElement.blur();
		$("input#search_input")[0].focus();
		break;
	}
};
// Adjust relationship between panes and set to rems
hmWebHelp.adjustTopicPos = function() {
	
	var navRight =  hmpage.$navwrapper.position().left + hmpage.$navcontainer.width(),
		topicLeft = hmpage.$topicbox.position().left,
		setTopicPos = navRight != topicLeft ? navRight : topicLeft;
	
	if (hmpage.topicleft && !hmpage.navclosed)
			return;		
	hmpage.$topicbox.css("left", hmpage.Fpix2em(setTopicPos) + "rem");
	
	hmpage.topicadjust = false;
	return;
	
	var newTopicLeft = 0,
		//navRightPos = hmpage.Fpix2em(hmpage.$navwrapper.position().left + hmpage.$navwrapper.width()),
		navRightPos = hmpage.Fpix2em(hmpage.$navwrapper.position().left + hmpage.$navcontainer.width()),
		//navRemWidth = hmpage.Fpix2em(hmpage.$navwrapper.width()),
		navRemWidth = hmpage.Fpix2em(hmpage.$navcontainer.width()),
		navRemPos = hmpage.Fpix2em(hmpage.$navwrapper.position().left);
		hmpage.$navwrapper.css({"left": navRemPos + "rem", "width": navRemWidth + "rem"});
		hmpage.topicadjust = false;
		
};
// Adjust widths of navigation and topic panes for font resizing etc.
hmWebHelp.resizePanes = function(refWidth) {
	if (typeof refWidth == "undefined")
		return;
	if (refWidth > hmpage.FmaxNavWidth()) refWidth = hmpage.FmaxNavWidth();
	if (refWidth < hmpage.FminNavWidth()) refWidth = hmpage.FminNavWidth();
	hmpage.$navwrapper.width(refWidth + "px");
	hmWebHelp.adjustTopicPos();
	hmWebHelp.nsheader();
	hmWebHelp.fHeadUpdate();
	};

// Check whether we are on the main page
hmWebHelp.hmMainPageCheck = function(t) {
	if (!window.history.pushState) {
		if (location.pathname.substr(location.pathname.lastIndexOf("/")+1) != hmFlags.hmMainPage) 
		{
		document.location = hmFlags.hmMainPage + "#" + t;
		return false;
		} else return true;
		} else
		return true;
	};

// Decode query components from URL
hmWebHelp.getQueryComponent = function(q,qC) {
	
	var rx = new RegExp('^' + qC + '=(.+?)$','i');
	var queryComponent = ""; 
	
	for (var c in q) {
			var match = rx.exec(q[c]);
			if (match !== null) {
				queryComponent = match[1];
				break;
				} 
			}
	return decodeURIComponent(queryComponent);
	};

// URL parser for all the possible valid URL syntaxes
hmWebHelp.parseUrl = function() {

	var getExt = function(f) {
		if (f === "") return false;
		var ext = f.substr(f.lastIndexOf('.'));
		if (ext === "") return false;
			else return ext;
		};

	var url = {
		topic: "",
		jstopic:"",
		anchor: "",
		switches: "",
		valid: false
	};
	
	var mainFile = location.pathname.substr(location.pathname.lastIndexOf("/")+1);


	var urlQuery, urlHashQuery, urlQueryTopic, urlQueryExt, urlHash, urlHashTemp = [], urlHashTopic, plainUrl;

	urlQuery = location.search.substr(1).split("&");
	urlQueryExt = getExt(urlQuery[0]);
	urlQueryTopic = urlQueryExt == hmFlags.topicExt || urlQueryExt + "l" == hmFlags.topicExt ? urlQuery[0] : "";
	
	// Set the search highlight if this is a search query posted from outside
	
	if (urlQuery.length > 0 && urlQuery[0] != "") {
		urlQuery.forEach(function(item, index){
		if (item.substr(0,14) === "zoom_highlight"){
			hmFlags.searchHighlight = item;
			}
		}); 
	}
	
	// Check for hash syntax hash value components
	if (location.hash !== "") {
		
		if (!hmBrowser.server) {
		urlQuery = [];
		urlQueryTopic = "";
		}
		
		if (location.hash.indexOf(hmpage.anchorY) > -1) {
			urlHash = location.hash.substr(1).split(hmpage.anchorY);
			if (urlHash[1].indexOf("&") > -1) {
				urlHashTemp[0] = urlHash[0];
				urlHash = urlHash[1].split("&");
				urlHash = urlHashTemp.concat(urlHash);
				}
		} else { 
		urlHash = location.hash !== "" ? location.hash.substr(1).split("&") : [""];
		} 
	} else urlHash = [""];
	urlHashTopic = getExt(urlHash[0]) == hmFlags.topicExt ? urlHash[0] : ""; 

	plainUrl = (location.search === "" && location.hash === "");
	
	// Correct existing old references to .htm if we're using .html for topics
	if (urlQueryExt == ".htm" && hmFlags.topicExt == ".html" && urlQueryTopic !== "") {
		urlQueryTopic = urlQueryTopic.replace(/htm$/,"html");
	}
	
	// Possible valid URL contents:
	// 1: Completely empty
	var emptyUrl = (mainFile === "" && plainUrl);
	// 2: Main index file only
	var mainIndexUrl = (mainFile == hmFlags.hmMainPage);
	var mainIndexUrlOnly = (mainIndexUrl && plainUrl);
	// 3: Main index file + topic file old syntax 
	var oldSyntaxUrl = (mainFile == hmFlags.hmMainPage && urlQueryTopic !== ""); 
	// 4: Main index file + topic file hash syntax
	var indexHashUrl = (!hmBrowser.server && mainFile == hmFlags.hmMainPage && urlHashTopic !== "");
	// 5: Topic file hash syntax without index file
	var noIndexHashUrl = (!hmBrowser.server && mainFile !== "" && mainFile !== hmFlags.hmMainPage && urlHashTopic === "");
	// 6: Topic file only
	// var topicFileUrl = (mainFile !== "" && mainFile != hmFlags.hmMainPage && getExt(mainFile) == hmFlags.topicExt);
	var topicFileUrl = (!indexHashUrl && !noIndexHashUrl && mainFile !== "" && !oldSyntaxUrl && getExt(mainFile) == hmFlags.topicExt);

	// Filters needed for
		// Additional query components
		// Additional hash components
		
	// Reload on non-server if the main file is not index.html
	if (noIndexHashUrl) {
		document.location = hmFlags.hmMainPage + "#" + mainFile + (urlHashTopic !== "" ? "!" + urlHashTopic : "");
	}

	// New topic file syntax
	if (topicFileUrl || indexHashUrl) {
			if (urlQuery.length > 0) {
				url.anchor = hmWebHelp.getQueryComponent(urlQuery,"anchor").toLowerCase();
			} else if (urlHash.length > 1 && !hmBrowser.server) {
				url.anchor = hmWebHelp.getQueryComponent(urlHash,"anchor").toLowerCase();
			}
				url.topic = hmBrowser.server ? mainFile : urlHashTopic !== "" ? urlHashTopic : mainFile;
				url.jstopic = "./jstopics/" + url.topic.replace(/(.*)\..+?$/ig, "$1.js");
			
			if (!hmBrowser.server && urlHashTopic !== "" && mainFile !== hmpage.defaulttopic) {
					var hashAnchor = url.anchor !== "" ? "!anchor=" + url.anchor : "",
						urlSwitches = location.hash.substr(location.hash.indexOf("&"));
					document.location = hmpage.defaulttopic + "#" + urlHashTopic + hashAnchor + urlSwitches;
			}
			
			url.valid = true;
			if (url.anchor !== "" ) {
				hmWebHelp.scrollTopic(url.anchor);
			}

		/** Parse additional URL switches **/
		
		var doContext, doIndex, doSearch,
			thisQuery = hmBrowser.server ? urlQuery : urlHash;

		// Context IDs

		if (hmFlags.contextID) {
		url.topic = hmGetContextId(hmFlags.contextID);
		if (/\#/.test(url.topic)) {
			url.anchor = url.topic.substr(url.topic.indexOf("\#")+1).toLowerCase();
			url.topic = url.topic.substr(0,url.topic.indexOf("\#"));
		}
		if (url.topic == "undefined") {
			// Return the default topic if something goes wrong...	
			url.topic = hmFlags.hmDefaultPage;
		} 			
		url.jstopic = "./jstopics/" + url.topic.replace(/(.*)\..+?$/ig, "$1.js");
		url.valid = true;
	}
		// User Parameters Object

		var userArgs = thisQuery,
			tempArg, tempVal;

		for (var x = 0; x < userArgs.length; x++) {
			if (userArgs[x].indexOf("\=") < 0) continue;
			tempArg = userArgs[x].substr(0,userArgs[x].indexOf("\="));
			tempVal = userArgs[x].substr(userArgs[x].indexOf("\=")+1);
			
			if ($.inArray(tempArg,["kwindex","ftsearch","contextid"]) > -1) continue; 
			
			hmWebHelp.userParams.paramsCount++;
			hmWebHelp.userParams[tempArg] = tempVal;
		}
		// Execute user code for saving URL parameters to session variables
		hmGetUrlParams();

		// Search
		
		doSearch = hmWebHelp.getQueryComponent(thisQuery,"ftsearch");
		
		if (doSearch !== "") {
			hmWebHelp.switchNavTab('search');
			if (doSearch != "$hmsearch")
				setTimeout(function(){
				xMessage.sendObject("hmsearch",{action: "callfunction", fn: "hmDoFtSearch", fa: doSearch});
			},1000);
		}
		return url;
	}

	// Default topic
	if (emptyUrl || mainIndexUrlOnly) {
			url.topic = hmFlags.hmDefaultPage;
			url.jstopic = "./jstopics/" + url.topic.replace(/(.*)\..+?$/ig, "$1.js");
			url.valid = true;
		return url;
	}

	if (oldSyntaxUrl) {
			
			if (!hmBrowser.server) {
			var oldHref = document.location.href,
				newHref = oldHref.replace(/\?/,"\#");
			if (document.location.hash !== "") {
				var rN = document.location.hash,
					rX = new RegExp(rN);
				rN = rN.replace(/\#/,hmpage.anchorX).toLowerCase();
				newHref = newHref.replace(rX,rN);
		} 			
			document.location.href = newHref;
			return false;
	} 
	
			url.topic = urlQueryTopic;
			url.jstopic = "./jstopics/" + url.topic.replace(/(.*)\..+?$/ig, "$1.js");
			url.anchor = urlHash[0].toLowerCase();
			url.valid = true;
		return url;
	}

			url.topic = hmFlags.hmDefaultPage;
			url.jstopic = "./jstopics/" + url.topic.replace(/(.*)\..+?$/ig, "$1.js");
			url.valid = true;
		return url;	
};

// Sync the current page to the TOC
hmWebHelp.syncToc = function() {
	
	if (hmFlags.isHmTopic || hmFlags.hmCurrentPage !== "") {
		if (hmFlags.hmCurrentPage === "") {
		xMessage.sendObject("hmnavigation", {action: "callfunction", fn: "tocSource.findElement", fa: hmFlags.thisTopic});
		}
		else {
			if (hmpage.hmHelpUrl.anchor !== "") {
		xMessage.sendObject("hmnavigation", {action: "callfunction", fn: "tocSource.findElement", fa: (hmpage.hmHelpUrl.topic + "#" + hmpage.hmHelpUrl.anchor)});
			}
			else if (hmFlags.hmCurrentPage != hmpage.hmHelpUrl.topic)
		xMessage.sendObject("hmnavigation", {action: "callfunction", fn: "tocSource.findElement", fa: hmpage.hmHelpUrl.topic});
		else
		xMessage.sendObject("hmnavigation", {action: "callfunction", fn: "tocSource.findElement", fa: hmFlags.hmCurrentPage});
		
		}
	}
	else {
		xMessage.sendObject("hmnavigation", {action: "callfunction", fn: "tocSource.findElement", fa: hmFlags.hmDefaultPage});
	}
};

hmWebHelp.parseState = function(state) {
	state = state.substr(state.lastIndexOf("/")+1);
	var urlQuery = [""];	
	 var url = {
			topic: "",
			jstopic:"",
			anchor: "",
			switches: "",
			valid: false
		};

		url.topic = state.indexOf(hmpage.anchorY) > -1 ? state.substr(0,state.indexOf(hmpage.anchorY)) : state;
		url.jstopic = "\.\/jstopics\/" + url.topic.replace(/(.*)\..+?$/ig, "$1.js");
		
		if (state.indexOf(hmpage.anchorY) > -1) {
		urlQuery = state.substr(state.indexOf(hmpage.anchorY)+1).split("&");
			url.anchor = hmWebHelp.getQueryComponent(urlQuery,"anchor").toLowerCase();
		}
			return url;
};

// Non-scrolling header update for topic area
hmWebHelp.nsheader = function() {
		var tHdheight = $("table#topicheadertable").height(),
			menuOffset = tHdheight + $("header#headerbox_wrapper").height(),
		tNavMinHeight = $("a#hamburgerlink").outerHeight()+7;
		tNavHeight = tHdheight;
		$("div#hmpagebody, div.navbox").css("top",(tNavHeight + 5) + "px");
		if ($(window).width() > 768) {
			$("div#navigationmenu").css("top",(menuOffset) + "px");
		} else {
			$("div#navigationmenu").css("top", "");
		}
		hmpage.$main_searchbox.css("height", tNavHeight + "px");
	};

hmWebHelp.fHeadUpdate = function() {
	if (!false) return;
	if ($("div#featureheader").length < 1) return;
	if (typeof hmWebHelp.funcs.hmFeatureHeader !== "undefined")
			hmWebHelp.funcs.hmFeatureHeader("resize");
	if (typeof hmWebHelp.funcs.hmFeatureHeaderM !== "undefined")
			hmWebHelp.funcs.hmFeatureHeaderM("resize");
	};

// Additional functions menu in topic area
hmWebHelp.hamburgerMenu = function(swCommand) {

	var switcher = false;
	if (typeof swCommand != "undefined")
		switcher = swCommand;
	var $hMenu = $("div#navigationmenu");

	if ($("#autoTocWrapper").is(":visible")) {
		hmWebHelp.extFuncs("hm_autotoc","snap");
		}

	function openMenu() {
		hmWebHelp.unClicker("hamburgermenu");
		$hMenu.slideDown(150, function(){
		$("ul#hamburgermenu li:visible").last().not(".last").addClass("last");
		});
		if (hmDevice.phone && !hmpage.navclosed) {
			hmWebHelp.pageDimensions.moveTOCLeft();
			}
	}
	
	function closeMenu(instant) {
		if (!instant)
			$hMenu.slideUp(150);
		else
			$hMenu.hide();
		$(document).off(hmBrowser.touchstart + '.closemenu');
		$(document).off("keydown.hamburger");
		}
	
	if (!switcher) {
		if ($hMenu.is(":hidden")) {
			openMenu();
			}
		else
			closeMenu();
			} else {
				if (switcher == "close")
					closeMenu(true);
				else
					openMenu();
					
			}
}; // hamburger menu

/* Stuff that needs to be done immediately on loading this script */
// Parse the current URL before it's gone...
hmpage.hmHelpUrl = hmWebHelp.parseUrl();
hmpage.currentURI = encodeURI(document.location.href);
// Set the starting values for the page aspects
hmpage.narrowpageX = hmpage.Fnarrowpage();
hmpage.shortpageX = hmpage.Fshortpage();

$(document).ready(function() {

	/* Redirect button for ASPX in SharePoint on Phones */

	if ( hmDevice.embedded && hmDevice.phone && hmBrowser.isaspx && /SharePoint for iOS|SharePoint for Android/i.test(navigator.userAgent) ) {
		$("#fouc").prepend('<p class="aspxlinkpara"><a class="aspxbutton" href="' + document.location.href + '" target="_blank">Open WebHelp</a></p>');
		} else if (!hmFlags.isEWriter) {
			$("#fouc").remove();
			}

	$("div#topicheaderwrapper span").addClass("wraptext");
		var nsDelay = hmDevice.desktop ? 0 : 800;
		$(window).on(hmBrowser.orientationevent, function() {
			setTimeout(function() {
			window.scrollTo(0,0);
			hmWebHelp.pageDimensions.doDims();
			hmWebHelp.nsheader();
			// Close popups if user changes mobile device orientation
			if (hmBrowser.orientationevent !== "resize")
				hmWebHelp.closePopup();
			},nsDelay);
			});
		hmWebHelp.nsheader();
		hmpage.$scrollBox = $("div#hmpagebody_scroller");
		hmpage.$scrollContainer = $("div#hmpagebody");
	
	// Manage TOC header and open closer width
	var resizeEnd = new Date().getTime();
	$(window).on("resize", function() {
		let resizeStart = new Date().getTime();
		if (resizeStart-resizeEnd < 500) return;
		if ($(window).width() > 768 && hmpage.navclosed) {
			hmWebHelp.pageDimensions.moveTOCRight();
			};
		if ($(window).width() > 768) {
			xMessage.sendObject("hmnavigation",{action: "callfunction", fn: "manageLogo", fa: "hide"});
			} else {
			xMessage.sendObject("hmnavigation",{action: "callfunction", fn: "manageLogo", fa: "show"});	
			};
		resizeEnd = new Date().getTime();
		});
	
	
	// Keyboard control
	
	$(window).on("keydown", function(event){
		
		if (![27,81,83,84,72,66,67].includes(event.keyCode)) return;
		
		if (event.altKey) {
			$(window).off("keydown.firstTab");
			}

		switch(event.keyCode) {
			case 27:
			hmWebHelp.closeMenus();
			break;
			case 81:
			if (event.altKey) {
				hmWebHelp.pageFocus("info");
				event.preventDefault();
				}
			break;
			case 67:
			if (event.altKey) {
				event.preventDefault();
				hmWebHelp.switchNavTab("contents");
				}
			break;
			case 83:
			if (event.altKey) {
				event.preventDefault();
				hmWebHelp.switchNavTab("search");
				}
			break;
			case 84:
			if (event.altKey) {
				event.preventDefault();
				hmWebHelp.pageFocus("topic");
				}
			break;
			case 72:
			if (event.altKey) {
				event.preventDefault();
				hmWebHelp.pageFocus("header");
				}
			break;
			case 66:
			if (event.altKey) {
				event.preventDefault();
				hmWebHelp.pageFocus("body");
				}
			break;
		}
	});

	// Close nav on click in topic if nav overlapping
	$("div#hmpagebody_scroller").on(hmBrowser.touchstart, function(event) {
		if ($(this)[0].contains(event.target)) {
		if (!hmpage.navclosed && (hmpage.Fnarrowpage() || hmDevice.phone)) {
			hmWebHelp.pageDimensions.dragHandle(false);
			}
		}
	});

	// Activate keyboard info on first navigation key 
	if (hmDevice.desktop && false) {
		
		hmWebHelp.pageFocus("info");
		/*$(window).on("keydown.firstTab", function(event){
		if ([9,32,37,38,39,40].includes(event.keyCode)) {
				event.preventDefault();
				event.stopPropagation();
				hmWebHelp.pageFocus("info");
				$(window).off("keydown.firstTab");
			}
		}); */
	} // First nav key

	// Load search frame a little later
	
	setTimeout(function(){
		
	setTimeout(function(){
	if ($("iframe#hmsearch").attr("src") === "" && hmSearchActive) {
			$("iframe#hmsearch").attr("src",hmFlags.hmSearchPage);
		}

	},1000);
		},1000);
});

