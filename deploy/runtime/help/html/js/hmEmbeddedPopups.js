/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/

// Get the path to the WebHelp folder and domains for remote check
window.name = "wpwebhelp";

var scriptEls = document.getElementsByTagName('script'),
	hmPopupPath = (scriptEls[scriptEls.length - 1]).src;
	hmPopupPath = hmPopupPath.substr(0, hmPopupPath.lastIndexOf('\/js\/')+1);
var	hmTargetDomain,
	hmLocalDomain = document.location.protocol + "\/\/" + document.location.hostname,
	domainmatch = /((?:^https?:\/\/|^\/\/).*?)\//i,
	match = domainmatch.exec(hmPopupPath),
	hmIsAspx = (/\.aspx??$/i.test(document.location.pathname));
	
var xpopupFile = !hmIsAspx ? "_hmXpopup.htm" : "_hmXpopup.aspx",
	xtopicFile = !hmIsAspx ? "_hmXtopic.htm" : "_hmXtopic.aspx";

	if (match != null) {
		hmTargetDomain = match[1];
	} else {
		hmTargetDomain = "";
	}
	
//** Main Object **//
var hmXPopup = {};
	hmXPopup.topicTarget = "";
	hmXPopup.popupMode = "";
	hmXPopup.topicExtension = "";
	hmXPopup.visitedTopics = {}; 
	hmXPopup.remoteAccess = hmLocalDomain !== hmTargetDomain;
	hmXPopup.ProjectName = "SAM Help";
	hmXPopup.contentWidth = 0; 
	hmXPopup.contentHeight = 0;
	hmXPopup.topicWidth = 0;
	hmXPopup.topicHeight = 0;
	hmXPopup.popupTitles = {};
	
	hmXPopup.setContentDims = function(dims) {
		var pHW = dims.split(",");
		hmXPopup.contentHeight = parseInt(pHW[0],10);
		hmXPopup.contentWidth = parseInt(pHW[1],10);
		};
	// Extract domain from a path
	hmXPopup.getDomain = function(path) {
			let domainmatch = /((?:^https?:\/\/|^\/\/).*?)\//i,
				match = domainmatch.exec(path);
			return(match[1]);
		};
	// Individual titles (popup name) for popups
	hmXPopup.setPopupName = function(title) {
		hmXPopup.$popuptitle.html(title);
	}
	// General titles (project name) for inline topics
	hmXPopup.setPopupTitle = function(title) {
		if (!hmXPopup.popupTitles.hasOwnProperty(hmXPopup.refPath)) {
			hmXPopup.popupTitles[hmXPopup.refPath] = title;
		}
		hmXPopup.$popuptitle.html(title);
	}
	// Set the outgoing link to open in full page
	hmXPopup.setPopupTopiclink = function(mainfile) {
		hmXPopup.topicExtension = mainfile.substr(mainfile.lastIndexOf("\."));
		
		let target = hmXPopup.topicTargetJS;
			target = hmXPopup.refPath + target.substr(0,target.lastIndexOf("\.")) + hmXPopup.topicExtension;

		hmXPopup.$topicLinkHeader.attr("href",target); 
	}
	// Set up xMessage and initalize
	hmXPopup.initializeX = function() {
		
		if (jQuery("link#popupstyles").length < 1) {
			jQuery("head").append('<link id="popupstyles" rel="stylesheet" type="text/css" href="'+ hmPopupPath + "css/hm_popup.css" + '" />');
		}
		
		if (typeof xMessage == "undefined") {
		jQuery.getScript(hmPopupPath + "js/xmessage.js", function( data, textStatus, jqxhr ) {
			xMessage = new xMsg("EMBED PARENT: ");
			doHmXPopup();
			});
		} else {
			doHmXPopup();
		}
	}

hmXPopup.hmDevice = {
	ppversion: 4.1
	};

var doHmXPopup = function() {
	
	// Multi-browser preventDefault for non-jQuery functions
	hmXPopup.PreventDefault = function(event) {
	if (typeof event == "undefined" || event === null) return;
	if (event.preventDefault)
		event.preventDefault();
	else
		event.returnValue = false;
	};
	
	jQuery.cachedScript = function( url, options ) {
	  // Allow user to set any option except for dataType, cache, and url
	  options = jQuery.extend( options || {}, {
	    dataType: "script",
	    cache: true,
	    url: url
	  });
	  return jQuery.ajax( options );
	};

	// Browser capabilities reference 
	var agent = navigator.userAgent,
	platform = navigator.platform;
	hmXPopup.hmBrowser = {};
	hmXPopup.hmBrowser.touch = !!(('ontouchstart' in window) || ('msmaxtouchpoints' in window.navigator) || ('maxtouchpoints' in window.navigator) || (navigator.maxTouchPoints > 0) || (navigator.msMaxTouchPoints > 0));
	hmXPopup.hmBrowser.nonDeskTouch = !!((hmXPopup.hmBrowser.touch && !/win32|win64/i.test(platform)) || (hmXPopup.hmBrowser.touch && /win32|win64/i.test(platform) && /mobile/i.test(agent)));
 
	hmXPopup.hmBrowser.eventType = (('onmousedown' in window && !hmXPopup.hmBrowser.nonDeskTouch) ? "mouse" : ('ontouchstart' in window) ? "touch" : ('msmaxtouchpoints' in window.navigator || navigator.msMaxTouchPoints > 0) ? "mstouchpoints" : ('maxtouchpoints' in window.navigator || navigator.maxTouchPoints > 0) ? "touchpoints" : "mouse");
		 switch(hmXPopup.hmBrowser.eventType) {
			case "mouse":
				hmXPopup.hmBrowser.touchstart = "mousedown.startevents";
				hmXPopup.hmBrowser.touchend = "mouseup.endevents";
				hmXPopup.hmBrowser.touchmove = "mousemove.moveevents";
			break;
			case "touch":
				hmXPopup.hmBrowser.touchstart = "touchstart.startevents";
				hmXPopup.hmBrowser.touchend = "touchend.endevents";
				hmXPopup.hmBrowser.touchmove = "touchmove.moveevents";
			break;
			case "mstouchpoints":
				hmXPopup.hmBrowser.touchstart = "MSPointerDown.startevents";
				hmXPopup.hmBrowser.touchend = "MSPointerUp.endevents";
				hmXPopup.hmBrowser.touchmove = "MSPointerMove.moveevents";
			break;
			case "touchpoints":
				hmXPopup.hmBrowser.touchstart = "pointerdown.startevents";
				hmXPopup.hmBrowser.touchend = "pointerup.endevents";
				hmXPopup.hmBrowser.touchmove = "pointermove.moveevents";
			break;
		 }

	// Device capabilities reference
	
	
	hmXPopup.hmDevice.agent = navigator.userAgent.toLowerCase();
	hmXPopup.hmDevice.platform = navigator.platform.toLowerCase();
	hmXPopup.hmDevice.ipad = /ipad/.test(hmXPopup.hmDevice.platform);
	hmXPopup.hmDevice.iphone = /iphone/.test(hmXPopup.hmDevice.platform);
	hmXPopup.hmDevice.winphone7 =  (/windows phone os [7]\.\d\d??; .*?trident\/[5]\.\d\d??; iemobile\/9/.test(hmXPopup.hmDevice.agent));
	hmXPopup.hmDevice.winphone = (/windows phone [89]\.\d\d??; .*?trident\/[67]\.\d\d??;.*?touch; /.test(hmXPopup.hmDevice.agent));
	hmXPopup.hmDevice.w8desktop = (/windows nt 6\.[2345]|windows nt 10\.[0123456]/m.test(hmXPopup.hmDevice.agent));
	hmXPopup.hmDevice.msBrowser = (/edge|trident/m.test(hmXPopup.hmDevice.agent));
	hmXPopup.hmDevice.w8metro = (function(){var supported = true; 
				try {new ActiveXObject("htmlfile");} catch(e) {supported = false;} 
				return (!supported && hmXPopup.hmDevice.w8desktop && hmXPopup.hmDevice.msBrowser);})();
	hmXPopup.hmDevice.touch = !!(('ontouchstart' in window && !window.opera) || ('msmaxtouchppoints' in window.navigator) || ('maxtouchppoints' in window.navigator) || (navigator.maxTouchPoints > 0) || (navigator.msMaxTouchPoints > 0));
	// Treat all Windows 8 devices as desktops unless in metro mode with touch
	hmXPopup.hmDevice.tb = (/tablet/.test(hmXPopup.hmDevice.agent) && (!/trident/.test(hmXPopup.hmDevice.agent) || (hmXPopup.hmDevice.w8metro && hmXPopup.hmDevice.touch)));
	hmXPopup.hmDevice.goodandroid = (/android.+?applewebkit\/(?:(?:537\.(?:3[6-9]|[4-9][0-9]))|(?:53[8-9]\.[0-9][0-9])|(?:54[0-9]\.[0-9][0-9]))|android.+?gecko\/[345][0-9]\.\d{1,2} firefox/.test(hmXPopup.hmDevice.agent));
	hmXPopup.hmDevice.deadandroid = (/android.+?applewebkit\/(?:53[0-6]\.\d{1,2})|firefox\/[0-2]\d\.\d{1,2}/.test(hmXPopup.hmDevice.agent));
	hmXPopup.hmDevice.android = (/android/.test(hmXPopup.hmDevice.agent) && !hmXPopup.hmDevice.deadandroid);
	hmXPopup.hmDevice.mb = /mobi|mobile/.test(hmXPopup.hmDevice.agent);
	
	/* Main Device References */
	hmXPopup.hmDevice.phone = (hmXPopup.hmDevice.mb && !hmXPopup.hmDevice.ipad && !hmXPopup.hmDevice.tb);
	hmXPopup.hmDevice.tablet = (hmXPopup.hmDevice.ipad || hmXPopup.hmDevice.tb || (!hmXPopup.hmDevice.phone && hmXPopup.hmDevice.android));
	hmXPopup.hmDevice.aspectRatio = (screen.height / screen.width > 1 ? screen.height / screen.width : screen.width / screen.height).toFixed(2);
	hmXPopup.hmDevice.narrowTablet = (hmXPopup.hmDevice.tablet && hmXPopup.hmDevice.aspectRatio > 1.4);
	hmXPopup.hmDevice.desktop = ((!hmXPopup.hmDevice.tablet && !hmXPopup.hmDevice.phone));	
	hmXPopup.hmDevice.device = hmXPopup.hmDevice.phone ? "phone" : hmXPopup.tablet ? "tablet" : hmXPopup.hmDevice.desktop ? "desktop" : "default";
	
	
	// Get the parent container
	var $popparent = jQuery("body");
	
	// Create the popup components
	$popparent.prepend('<div id="dragsurface" style="display: none; position: fixed; top: 0; right: 0; bottom: 0; left: 0; background-image: url(\''+hmPopupPath+'images/spacer.gif\')" ></div><div id="hmpopupbox" class="hmpopup"><div id="hmpopuptitlebar" class="hmpopup"><div id="hmpopuptitle"  class="hmpopup"><p class="hmpopup"></p></div><div id="hmclosepopup" class="hmpopup"><span>X</span></div><div id="hmpopuptitlelink"><p class="linkheader"><a id="topiclinkheader" href="" target="_blank">Click here to open this topic in a full help window</a></p></div></div><div id="hmpopupbody"><iframe id="hmXPopupFrame" src="" class="hmpopup" frameborder="0"></iframe></div></div>'); 

	// Get the JQ variables once for faster reuse
	hmXPopup.$popup = jQuery("div#hmpopupbox");
	hmXPopup.$popuptitle = jQuery("div#hmpopuptitle > p");
	hmXPopup.$popupheader = jQuery("div#hmpopuptitlebar");
	hmXPopup.$popupdragger = jQuery("div#hmpopuptitle");
	hmXPopup.$dragsurface = jQuery("div#dragsurface");
	hmXPopup.$popframe = jQuery("iframe#hmXPopupFrame");
	hmXPopup.$topicLinkHeader = jQuery("a#topiclinkheader");
	hmXPopup.refPath = hmPopupPath; //"./";
	hmXPopup.currentPopup = "";
	hmXPopup.clickX = 0; 
	hmXPopup.clickY = 0;
	hmXPopup.relposX = 0;
	hmXPopup.relposY = 0;

	// Inside the popup for same origin access
	hmXPopup.$popupbody = null;
	hmXPopup.$popupscroller = null;
	hmXPopup.$popupwindow = null;

	hmXPopup.hmResizePopupRemote = function() {
		
		var vpWidth = jQuery(window).width(),
			vpHeight = jQuery(window).height(),
			pWidth = hmXPopup.$popup.width(),
			pHeight = hmXPopup.$popup.height(),
			newWidth = 600, newHeight = 600;
			
			// Standard 600 px wide x 600 px high or 90% of window width/height, whichever is smaller
			
			if (newWidth > vpWidth * 0.9 || newHeight > vpHeight * 0.9 || pHeight < 100 || pWidth < 350) {
				if (newWidth > vpWidth * 0.9) newWidth = vpWidth * 0.9;
				if (newHeight > vpHeight * 0.9) newHeight = vpHeight * 0.9;
				hmXPopup.$popup.css({"width": newWidth + "px", "height": newHeight + "px"});
			}

			hmXPopup.positionPopup();
	}

	hmXPopup.hmResizePopup = function() {
		var popBodyWidth = 0, popBodyHeight = 0,
			windowWidth = function(){return jQuery(window).width();},
			windowHeight = function(){return jQuery(window).height();},
			newWidth = 0, newHeight = 0,
			verticalScroller = function() {return hmXPopup.$popupscroller.height() + 35 > hmXPopup.$popupwindow.height();},
			horizontalScroller = function() {return hmXPopup.$popupscroller.width() > hmXPopup.$popupwindow.width();},
			popFixedWidth = false;
			popBodyWidth = hmXPopup.$popupbody.outerWidth(true);
			popBodyHeight = hmXPopup.$popupbody.outerHeight(true);
			
			// Poptable for fixed width
			let $poptable = hmXPopup.$popupscroller.find("table#poptable"); 
			if ($poptable.length > 0 ) {
				hmXPopup.$popup.width($poptable.width() + 30);
				popFixedWidth = true;
				}

			// Horizontal scrollbar?
			if (!popFixedWidth && horizontalScroller()) {
				hmXPopup.$popup.width((hmXPopup.$popupscroller.width() + 10) + "px");
			}
			
			// Adjust both dimensions for vertical scrollbar
			
			if (verticalScroller()) {
				newWidth = hmXPopup.$popup.width();
				newHeight = hmXPopup.$popup.height();
				let widthOffset = popFixedWidth ? 0 : 5;
				do {
					newWidth+=widthOffset; newHeight+=5;
					hmXPopup.$popup.width(newWidth + "px");
					hmXPopup.$popup.height(newHeight + "px");
				} while (verticalScroller() && (newWidth < windowWidth() * 0.5));
					
			if (horizontalScroller()) {
				hmXPopup.$popup.width((hmXPopup.$popupscroller.width() + widthOffset) + "px");
			}

			if (verticalScroller()) { 
				do {
				hmXPopup.$popup.height((hmXPopup.$popup.height() + 2) + "px");
				}
				while (verticalScroller());
				}
			}
			
		// Now position the popup
		
		hmXPopup.positionPopup();
		
	};

	// Position popup
	hmXPopup.positionPopup = function() {

		var wW = jQuery(window).width()-5, pW = hmXPopup.$popup.outerWidth(),
			wH = jQuery(window).height()-5, pH = hmXPopup.$popup.outerHeight(),
			pT = hmXPopup.$popup.position().top,
			pL = hmXPopup.$popup.position().left;
		
		// Does it fit in the standard position (just below and right of click position)?
		if (
			((hmXPopup.popupMode == "popup") && (pH + hmXPopup.relposY + 15 < wH-5) && (pW + hmXPopup.relposX  + 30 < wW-5))
			||
			((hmXPopup.popupMode == "topic") && (pT > 0 && pL > 0) && (pT + pH < wH && pL + pW < wW))
			)
			{
				if (hmXPopup.popupMode == "popup") {
					hmXPopup.$popup.css({"top": hmXPopup.clickY +=15, "left":  hmXPopup.clickX +=30});
				} else {
					hmXPopup.$popup.css({"top": hmXPopup.clickY += 10, "left":  hmXPopup.clickX });
				}
		} else {
		  // Vertical: Move up minimum amount to fit
			if (pH + hmXPopup.relposY + 15 > wH-5) {
				
				 hmXPopup.clickY = jQuery(document).scrollTop() + (wH-pH);
				 hmXPopup.clickY = hmXPopup.clickY < 0 ? 5 : hmXPopup.clickY;
				 
			 } else {
				 
				 hmXPopup.clickY+=15;
			 }
			 
			 // Horizontal: Move left to fit
			 if (pW + hmXPopup.relposX  + 30 > wW-5) {
				 
				 // Fits left of the click point?
				 if (pW+5 < hmXPopup.relposX) {
					 
					 hmXPopup.clickX = jQuery(document).scrollLeft() + (wW-pW);
					 
				 } else { 
				 
					hmXPopup.clickX = (jQuery(document).scrollLeft() + wW) - (pW+5);
					
				 }
				 
				 hmXPopup.clickX = hmXPopup.clickX < 0 ? 5 : hmXPopup.clickX;
				 
			 } else {
				 hmXPopup.clickX+=30;
			 }
			
			hmXPopup.$popup.css({"top": hmXPopup.clickY, "left": hmXPopup.clickX})
			
			/* hmXPopup.$popup.animate({
			top: hmXPopup.clickY, 
			left: hmXPopup.clickX},
			300);*/
			 
		} // reposition
		
	}
	
	// Display inline topic finalize for local and remote
	
	hmXPopup.displayInlineTopic = function() {
	
	jQuery("div#hmpopupbody").css("top",(jQuery("div#hmpopuptitlebar").height() + 2) +"px");
			
			hmXPopup.$topicLinkHeader.attr("href",hmXPopup.topicTarget);
			jQuery("div#hmpopuptitlelink").show();
			hmXPopup.$popup.show();
			hmXPopup.$popup.css({"width": hmXPopup.topicWidth + "px",  "height": hmXPopup.topicHeight + "px"});
			hmXPopup.hmResizePopupRemote();
			hmXPopup.positionPopup();
			
			if (hmXPopup.hmDevice.desktop && !hmXPopup.noresize) {
			jQuery("body").on("mousemove.resizepopuplistener", function(event){
			var ev = event.originalEvent;
			hmXPopup.resizeListener(ev);
			});
			jQuery("body").on("mousedown.resizepopup", function(event){
				var e = event.originalEvent;
				hmXPopup.startResizePopup(e);
			});
			}
	}

	// Work out the topic extension that is in use
	var setTopicExtension = function(obj) {
		var ext = "";
		if (obj.hmPrevLink !== "")
			ext = obj.hmPrevLink;
		else if (obj.hmNextLink !== "")
			ext = obj.hmNextLink;
		else if (obj.hmParentLink !== "")
			ext = obj.hmParentLink !== "";
		if (ext === "") 
			hmXPopup.topicExtension = false;
		else 
			hmXPopup.topicExtension = ext.substr(ext.lastIndexOf("\."));
		};

	hmXPopup.closePopup = function() {

		if (hmXPopup.$popup.is(":hidden"))
			return;
		
		// Save topic dimensions
		if (hmXPopup.popupMode == "topic") {
			hmXPopup.topicWidth = hmXPopup.$popup.width();
			hmXPopup.topicHeight = hmXPopup.$popup.height();
		} else {
			hmXPopup.topicWidth = 0;
			hmXPopup.topicHeight = 0;
		}
		
		// Reset reference values
		hmXPopup.contentWidth = 0;
		hmXPopup.contentHeight = 0;
		
		// Unbind all the resizing on the desktop
		if (hmXPopup.hmDevice.desktop) {
			jQuery("body").off(".resizepopup").off(".resizepopuplistener");
			hmXPopup.$dragsurface.off(".resizepopup");
		}
		
		//if (hmXPopup.remoteAccess) {
			hmXPopup.$popup.fadeOut(300, function(){
			// Clear frame contents
			hmXPopup.$popframe.attr("src","");
			// Clear header text and topic outlink
			hmXPopup.$popuptitle.html("");
			jQuery("div#hmpopuptitlelink").attr("src","");
			// Clear up any drag residue
			endDrag();
			});
			return;
		//}
	};

	var fixTarget = function(t) {
		t = t.toLowerCase();
		if (/\.js$|\.html?$|\.php\d?$|\.asp$/im.test(t)) {
			return t;
		} else {
			return t + ".js";
		}
	};

	hmXPopup.initPopup = function() {
		
		//hmXPopup.$popup.show();
		
		hmXPopup.noresize = false;
		jQuery("div#hmpopuptitlelink").hide();
		jQuery("div#hmpopupbody").css("top","1.6rem");
	   
	   hmXPopup.$popup.css({"top": hmXPopup.clickY, "left": hmXPopup.clickX});
	   hmXPopup.$popup.show();
	  // Interval to wait for dimensions from source
		var intCount = 0,
		
			popDims = setInterval(function(){
		 
		 intCount++;
		 
		  if (intCount > 10) {
			  clearInterval(popDims);
			  return;
		  }
		  if (hmXPopup.contentHeight > 0 ) {
			  
			  clearInterval(popDims);
				
			  // Never more than 600 px wide/high or 90% of window width/height, whichever is smaller
			  
			  let targetWidth = hmXPopup.contentWidth + 16,
				  targetHeight = hmXPopup.contentHeight + 16;
			
			  if (targetWidth > 600 || targetWidth > jQuery(window).width()) {
				  if (jQuery(window).width() > 670)	
					  targetWidth = 600;
				  else {
					  targetWidth = jQuery(window).width() * 0.9;
				  }
			  }
			  if (targetHeight > 600 || targetHeight > jQuery(window).height()) {
				  if (jQuery(window).height() > 670) {	
					  targetHeight = 600;
					} else {
					  targetHeight = jQuery(window).height() * 0.9;
					}
			  }
			  
			  hmXPopup.$popup.animate({
				  width: targetWidth, 
				  height: targetHeight},
				  0, function() {
				  hmXPopup.positionPopup();
				 
				  });
		  }
		  
	  },100);
	  
	  if (hmXPopup.hmDevice.desktop && !hmXPopup.noresize) {
		jQuery("body").on("mousemove.resizepopuplistener", function(event){
		var ev = event.originalEvent;
		hmXPopup.resizeListener(ev);
		});
		jQuery("body").on("mousedown.resizepopup", function(event){
			var e = event.originalEvent;
			hmXPopup.startResizePopup(e);
		});
		} // Resizable for desktop browsers

	}

	hmXPopup.loadPopup = function(e, thisPopup, refPath) {

		var cacheTopic = thisPopup,
			loadThis = "",
			topicHeader = "";

		if (Object.keys(hmXPopup.visitedTopics).length > 300)
			hmXPopup.visitedTopics = {};

		hmXPopup.clickX = e.pageX;
			hmXPopup.relposX = hmXPopup.clickX - jQuery(document).scrollLeft();
		hmXPopup.clickY = e.pageY;
			hmXPopup.relposY = hmXPopup.clickY - jQuery(document).scrollTop();
		
		// Handle undefined and remote refpaths
		
		if (typeof refPath == "undefined") {
			
			refPath = hmPopupPath + "jspopups/";
		
		} else if (refPath == "remotetopic") {

			hmXPopup.remoteAccess = true;
			hmXPopup.topicTarget = hmXPopup.refPath + thisPopup;
			hmXPopup.topicTargetJS = thisPopup.substr(0,thisPopup.lastIndexOf("\.")) + ".js";
			hmXPopup.$topicLinkHeader.attr("href",hmXPopup.topicTarget);
			hmXPopup.$popframe.attr("src", hmXPopup.refPath + xtopicFile + "?" + hmXPopup.topicTargetJS);
			
			if (hmXPopup.popupTitles.hasOwnProperty(hmXPopup.refPath)) {
				hmXPopup.$popuptitle.html(hmXPopup.popupTitles[hmXPopup.refPath]);
			} else {
				setTimeout(function(){		
					xMessage.sendObject("hmXPopupFrame",{action: "getvalue", vn: "hmProjectInfo.title", cbf: "hmXPopup.setPopupTitle", domain: hmXPopup.getDomain(hmXPopup.refPath)});
				},500);
			}
			
			hmXPopup.displayInlineTopic();
			return;
			
		} else if (refPath == "remotepopup") {
			hmXPopup.remoteAccess = true;
			jQuery("div#hmpopuptitlelink").hide();
			jQuery("div#hmpopupbody").css("top","1.2rem");
			hmXPopup.$popframe.attr("src", hmXPopup.refPath + xpopupFile + "?" + thisPopup);
			hmXPopup.initPopup();
			return;
		}
		
		let pExt = thisPopup.substr(thisPopup.lastIndexOf("\."));
		if (pExt != "\.js") {
			thisPopup = thisPopup.replace(pExt,"\.js");
		}
		
		hmXPopup.currentPopup = thisPopup;
		loadThis = refPath + thisPopup;
		
	// Do local version (popup or topic on same domain)
	if (!hmXPopup.remoteAccess) {
			hmXPopup.topicTargetJS = thisPopup.substr(0,thisPopup.lastIndexOf("\.")) + ".js";
			
			if (hmXPopup.popupMode != "popup") {
					hmXPopup.$popframe.attr("src", hmXPopup.refPath + xtopicFile + "?" + hmXPopup.topicTargetJS); 
					
					if (hmXPopup.popupTitles.hasOwnProperty(hmXPopup.refPath)) {
					hmXPopup.$popuptitle.html(hmXPopup.popupTitles[hmXPopup.refPath]);
					}
				
				setTimeout(function(){	
				if (!hmXPopup.popupTitles.hasOwnProperty(hmXPopup.refPath)) {	
					xMessage.sendObject("hmXPopupFrame",{action: "getvalue", vn: "hmProjectInfo.title", cbf: "hmXPopup.setPopupTitle", domain: hmXPopup.getDomain(hmXPopup.refPath)});
					}
					xMessage.sendObject("hmXPopupFrame",{action: "getvalue", vn: "hmProjectInfo.mainfile", cbf: "hmXPopup.setPopupTopiclink", domain: hmXPopup.getDomain(hmXPopup.refPath)});
				},500);
				
				hmXPopup.displayInlineTopic();
				
			} else {				 
				jQuery("div#hmpopuptitlelink").hide();
				jQuery("div#hmpopupbody").css("top","1.2rem");
				hmXPopup.$popframe.attr("src", hmXPopup.refPath + xpopupFile + "?" + thisPopup); 
				hmXPopup.initPopup();
			}

		} // !remoteAccess 
		
	};

	// Bind the events for popup links on the page 
	jQuery("a.hmpopuplink").on("click", function(event){
			event.preventDefault();

			hmXPopup.popupMode = "popup";
			
			 hmXPopup.clickX = event.pageX;
			 hmXPopup.clickY = event.pageY;

			jQuery("iframe#hmXPopupFrame").css({"top": "0", "height": "100%"});
			
			var linkTarget = fixTarget(jQuery(this).attr("data-target")),
				targetPath = linkTarget.substr(0,linkTarget.lastIndexOf("\/") + 1);
				
			if (targetPath !== "") {
				hmXPopup.refPath = targetPath;
				hmXPopup.remoteAccess = hmXPopup.getDomain(targetPath) != hmLocalDomain;
				linkTarget = linkTarget.substr(linkTarget.lastIndexOf("\/") + 1);
			} else {
				hmXPopup.refPath = hmPopupPath;
				targetPath = hmPopupPath;
				hmXPopup.remoteAccess = hmXPopup.getDomain(hmPopupPath) != hmLocalDomain;
			}
			
			// Set the default size for area calculation
			hmXPopup.$popup.css({"width": "20rem", "height": "3.6rem"});
			
			if (hmXPopup.remoteAccess) {
				
				// Remote link
				hmXPopup.topicExtension = linkTarget.substr(linkTarget.lastIndexOf("\."));
				hmXPopup.loadPopup(event, linkTarget, "remotepopup");
				
			} else {
				// Local link
				hmXPopup.loadPopup(event, linkTarget, targetPath + "jspopups/");
				
			}
		});
		
	jQuery("a.hmtopiclink").on("click", function(event){
			
			event.preventDefault();
			
			hmXPopup.popupMode = "topic";

			var linkTarget = fixTarget(jQuery(this).attr("data-target")),
				targetPath = linkTarget.substr(0,linkTarget.lastIndexOf("\/") + 1);
				
			if (targetPath !== "") {
				hmXPopup.refPath = targetPath;
				hmXPopup.remoteAccess = hmXPopup.getDomain(targetPath) != hmLocalDomain;
				linkTarget = linkTarget.substr(linkTarget.lastIndexOf("\/") + 1);
			} else {
				hmXPopup.refPath = hmPopupPath;
				targetPath = hmPopupPath;
				hmXPopup.remoteAccess = hmXPopup.getDomain(hmPopupPath) != hmLocalDomain;
			}
			
			if (!hmXPopup.remoteAccess) {
				// Local link
				hmXPopup.loadPopup(event, linkTarget, targetPath + "jstopics/");
			} else {
				// Remote link
				hmXPopup.topicExtension = linkTarget.substr(linkTarget.lastIndexOf("\."));
				hmXPopup.loadPopup(event, linkTarget, "remotetopic");
			}
			
		});
		
	// Popup closing routines
	jQuery("div#hmclosepopup").on(hmXPopup.hmBrowser.touchstart,hmXPopup.closePopup);
	
	jQuery(document).keyup(function(e) {
     if (e.keyCode == 27) { 
		hmXPopup.closePopup();
		}
	});
	
	/*** Draggable Popups ***/
	
	// General variables
	var startTime = 0,
		dragTime = 0,
		dragcount = 0,
		oldX,
		oldY,
		oldLeftPos, 
		oldTopPos;
		
	
	// Get the type of interaction event from user
	var EventType = function(e) {
		if (e.pointerType == "mouse" || e.pointerType == 4)
			return "mouse";
		else if (e.pointerType == "touch" || e.pointerType == 2 || e.pointerType == "pen" || e.pointerType == 3)
			return "touch";
		else if (/^mouse/i.test(e.type)) 
			return "mouse";
		else if (/^touch/i.test(e.type) || /^pen/i.test(e.type)) 
			return "touch";
		else return "none";
		};
	
	// Perform this at the end of a drag operation
	var endDrag = function(e) {
		dragTime = new Date().getTime() - startTime;
		if (dragTime < 200) {
			}
		hmXPopup.$popupdragger.off(".endevents");
		hmXPopup.$popupdragger.off(".moveevents");
		hmXPopup.$dragsurface.off(".endevents");
		hmXPopup.$dragsurface.off(".moveevents");
		hmXPopup.$dragsurface.hide().css("cursor","default");
		hmXPopup.PreventDefault(e);
		};
			// Drag action
	var performDrag = function(e) {
		
		dragTime = new Date().getTime() - startTime;
		if (dragTime < 50) {
			return;
		}
		
		
		var touchobj;
		if (typeof e.changedTouches != 'undefined') { 
				touchobj = e.changedTouches[0];
			} else {
				touchobj = e;
			}
		
		// Only move once every x events on mobile for lower processor load 
		dragcount++;
		if ( hmXPopup.hmDevice.desktop || dragcount > 2 ) {
		dragcount = 0;
		var moveX = (!(document.all && !window.opera)) ? touchobj.pageX - oldX : touchobj.clientX - oldX;
		var moveY = (!(document.all && !window.opera)) ? touchobj.pageY - oldY: touchobj.clientY - oldY;
		hmXPopup.$popup.css({"left": (oldLeftPos + moveX) + 'px'});
		hmXPopup.$popup.css({"top": (oldTopPos + moveY) + 'px'});
		}
	};
	
	// Triggered at beginning of a drag
    var startDrag = function(e) {
		hmXPopup.PreventDefault(e);
		startTime = new Date().getTime();
		var touchobj;
		if (typeof e.changedTouches != 'undefined') 
			touchobj = e.changedTouches[0];
		else 
			touchobj = e;
		oldX = (!(document.all && !window.opera)) ? touchobj.pageX : touchobj.clientX;
		oldY = (!(document.all && !window.opera)) ? touchobj.pageY : touchobj.clientY;
		oldLeftPos = hmXPopup.$popup.position().left;
		oldTopPos = hmXPopup.$popup.position().top;

		// Activate the drag surface overlay
		if (hmXPopup.hmBrowser.touch || hmXPopup.hmDevice.winphone || EventType(e) == "mouse") {
		hmXPopup.$dragsurface.css("cursor","all-scroll").show();
		hmXPopup.$dragsurface.on(hmXPopup.hmBrowser.touchmove, function(event) {
			var ev = event.originalEvent; 
			performDrag(ev);
			});
		hmXPopup.$dragsurface.on(hmXPopup.hmBrowser.touchend, function(event) {
			var ev = event.originalEvent; 
			endDrag(ev);
			});
		} 
		
		hmXPopup.$popupdragger.on(hmXPopup.hmBrowser.touchmove, function(event) {
			var ev = event.originalEvent; 
			performDrag(ev);
			});
		hmXPopup.$popupdragger.on(hmXPopup.hmBrowser.touchend, function(event) {
			var ev = event.originalEvent; 
			endDrag(ev);
			});

	};

	hmXPopup.$popupdragger.on(hmXPopup.hmBrowser.touchstart, function(event) {
		var ev = event.originalEvent; 
		startDrag(ev);
		});	

if (!hmXPopup.hmDevice.phone) {
	hmXPopup.resizeListener = function(e) {
	 var popPos = hmXPopup.$popup.position(),
	 popWd = hmXPopup.$popup.outerWidth(),
	 popHt = hmXPopup.$popup.outerHeight(),
	 bWidth = 2;
	 hmXPopup.ewResize = "ew-resize";
	 hmXPopup.nsResize = "ns-resize";
	// Body cursor for resizing, based on proximity to left and right borders of popup
	var rBd = ((e.pageX > (popPos.left + (popWd-4))) && (e.pageX < (popPos.left + popWd+8)) && (e.pageY > popPos.top) && (e.pageY < popPos.top + popHt+8));
	var bBd = ((e.pageY > (popPos.top + (popHt-8-bWidth))) && (e.pageY < (popPos.top + popHt+10)) && (e.pageX < (popPos.left + popWd+10)) && (e.pageX > popPos.left+4));
	var corner = ((rBd && (e.pageY > (popPos.top + popHt-10))) || (bBd && e.pageX > (popPos.left + popWd-10)));
	hmXPopup.bdDrag = rBd || bBd;
	jQuery("body").css("cursor", function(){
	return corner ? "nw-resize" : rBd && !bBd ? hmXPopup.ewResize : bBd && !rBd ? hmXPopup.nsResize : "auto";
	});
	}; // resize listener
	

	// Deselect during drag
	hmXPopup.deSelect = function(){
		if (window.getSelection){
			window.getSelection().removeAllRanges();
		}
		else if (document.selection){
			document.selection.empty();
		}
	return false;
	}; // End deSelect()
	
	hmXPopup.doResizePopup = function(event,direction) {
		hmXPopup.deSelect();
		var moveX = event.pageX - hmXPopup.resizeX,
			moveY = event.pageY - hmXPopup.resizeY,
			newX = hmXPopup.popdims.w + moveX,
			newY = hmXPopup.popdims.h + moveY;
			
			switch (direction) {
				case "horizontal":
				newY = hmXPopup.popdims.h;
				break;
				case "vertical":
				newX = hmXPopup.popdims.w;
				break;
			}
			if (newY < 50) newY = 50;
			if (newX < 200) newX = 200;
			hmXPopup.$popup.css({"width": newX + "px", "height": newY + "px"});
	};
	
	hmXPopup.endResizePopup = function(event) {
		hmXPopup.$dragsurface.off("mousemove.resizepopup");
		jQuery("body").off("mouseup.resizepopup");
		jQuery("body").css("cursor","default");
		hmXPopup.$dragsurface.css("cursor","default").hide();
		
		if (hmXPopup.popupMode == "topic") {
			hmXPopup.topicWidth = hmXPopup.$popup.width();
			hmXPopup.topicHeight = hmXPopup.$popup.height();
		}
	};
	
	hmXPopup.startResizePopup = function(e) {
		var thisCursor = jQuery("body").css("cursor");
		function initialize(direction) {
			hmXPopup.resizeX = e.pageX;
			hmXPopup.resizeY = e.pageY;
			hmXPopup.popdims = {};
			hmXPopup.popdims.w = hmXPopup.$popup.width();
			hmXPopup.popdims.h = hmXPopup.$popup.height();
			jQuery("body").on("mouseup.resizepopup", function(event) {
			var e = event.orginalEvent;
			hmXPopup.endResizePopup(e);
			});
			hmXPopup.$dragsurface.css("cursor",thisCursor);
			jQuery("body").css("cursor",thisCursor);
			hmXPopup.$dragsurface.show().on("mousemove.resizepopup", function(event){
			var e = event.originalEvent;
			hmXPopup.doResizePopup(e,direction);
			});
		}
		switch (thisCursor) {
			case "ew-resize":
			initialize("horizontal");
			break;
			case "nw-resize":
			initialize("diagonal");
			break;
			case "ns-resize":
			initialize("vertical");
			break;
		}
	};
	
} // If Desktop for resizable popups
	
}; // doHMXPopup

if (typeof jQuery === "undefined") {
	var jQ = document.createElement('script');
	jQ.setAttribute('type','text/javascript');
	jQ.setAttribute('src', hmPopupPath + 'js/jquery.js');
	document.getElementsByTagName("head")[0].appendChild(jQ);
	var jQLoader = setInterval(function(){
		if (typeof jQuery === 'function') {
			clearInterval(jQLoader);
			jQuery(document).ready(function(){
			hmXPopup.initializeX();
			});
		} 
	},50);
} else {
	jQuery(document).ready(function(){
	hmXPopup.initializeX();
	});
}	
