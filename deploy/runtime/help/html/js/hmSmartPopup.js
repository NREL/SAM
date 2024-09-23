/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/

//** Main Object **//
var hmXPopup = {};

// Get the parent container
var $popparent = $("body");

$popparent.append('<div id="hmpopupbox" class="hmpopup"><div id="hmpopuptitlebar" class="hmpopup"><div id="hmpopuptitle" class="hmpopup"><p class="hmpopup">This is the title bar</p></div><div id="hmclosepopup" class="hmpopup"><span>X</span></div></div><div id="hmpopupbody"></div></div>');

// General variables
	hmXPopup.$popup = $("div#hmpopupbox");
	hmXPopup.$popupcontainer = $("main#topicbox");
	hmXPopup.$popuptitle = $("div#hmpopuptitle > p");
	hmXPopup.$popupheader = $("div#hmpopuptitlebar");
	hmXPopup.$popupdragger = $("div#hmpopuptitle");
	hmXPopup.$popupbody = $("div#hmpopupbody");
	hmXPopup.popupwindow = hmXPopup.$popupbody[0];
	hmXPopup.$dragsurface = $('div#dragsurface');
	hmXPopup.refPath = "./jspopups/";
	hmXPopup.clickX = 0;  hmXPopup.clickY = 0;
	hmXPopup.posX = 0;  hmXPopup.posY = 0;
	
	
hmXPopup.PreventDefault = function(event) {
	if (typeof event == "undefined" || event === null) return;
	if (event.preventDefault)
		event.preventDefault();
	else
		event.returnValue = false;
	};

// Auto resize and reposition executed when the popup becomes visible
hmXPopup.sizeAndPosition = function() {
		
		hmXPopup.posX = hmXPopup.clickX;
		hmXPopup.posY = hmXPopup.clickY;
		
		var getPercent = function(px,pt) {
				if (typeof px === "string" && px.charAt(px.length-1) === "%") {
					return parseFloat(px,10);
				} else {
				pt = typeof pt === "string" ? parseFloat(pt,10) : pt;
				px = typeof px === "string" ? parseFloat(px,10) : px;
				return ((px/pt) * 100);
				}
			};
		
		var	windowWidth = function(){return $(window).width();},
			wW = 0, pW = 0,
			windowHeight = function(){return $(window).height();},
			wH = 0, pH = 0,
			hasTable = $("div#hmpopupbody").has("table[style*='width']").length,
			shortText = (function() {
				if (hasTable) return false;
				var maxLength = 0, curLength = 0;
				$("div#hmpopupbody p").each(function() {
					curLength = $(this).text().trim().length;
					if (curLength > maxLength) maxLength = curLength;
				});
				return maxLength < 100;
			})(),
			verticalScroller = function() {return hmXPopup.popupwindow.clientHeight < hmXPopup.popupwindow.scrollHeight;},
			horizontalScroller = function() {return hmXPopup.popupwindow.clientWidth < hmXPopup.popupwindow.scrollWidth;},
			newWidth = 0, newHeight = 0,
			adjustedDims = false,
			popMaxWidth = getPercent(hmXPopup.$popup.css("max-width"),windowWidth())/100,
			popMaxHeight = getPercent(hmXPopup.$popup.css("max-height"),windowHeight())/100,
			$popWidthReference = hmDevice.desktop ? hmXPopup.$popupcontainer : $(window);
			
			var setPercentageDimensions = function() { 
			var wwd = windowWidth(), 
				pxw = hmXPopup.$popup.width(),
				wwh = windowHeight(),
				pxh = hmXPopup.$popup.height();
			hmXPopup.$popup.css({"width": getPercent(pxw,wwd) + "%", "height": getPercent(pxh,wwh) + "%"});
			};
			// Horizontal scrollbar? (triggered by graphics etc)
			if (horizontalScroller()) {
				
				hmXPopup.$popup.width((hmXPopup.popupwindow.scrollWidth + 5) + "px");
				adjustedDims = true;
			}
			
			// Adjust both dimensions for vertical scrollbar
			if (verticalScroller()) {
				var newMaxHeight = popMaxHeight * windowHeight();
				newWidth = hmXPopup.$popup.width();
				newHeight = hmXPopup.$popup.height();
				adjustedDims = true;
				do {
					newWidth+=(hasTable || shortText ? 0 : 5); 
					newHeight+=5;
					hmXPopup.$popup.width(newWidth + "px");
					hmXPopup.$popup.height(newHeight + "px");
				} while (verticalScroller() && (newHeight < newMaxHeight));
					
			if (horizontalScroller()) {
				var expandWidth = hmXPopup.$popup.width();
				var expandMax = windowWidth() * popMaxWidth;
				adjustedDims = true;
				do {
					expandWidth+=5;
					hmXPopup.$popup.width(expandWidth + "px");
				} while (horizontalScroller() && expandWidth < expandMax);
				if (expandWidth < expandMax)
					hmXPopup.$popup.width((expandWidth+5) + "px");
			}
			if (!horizontalScroller() && hasTable) {
				var expandWidth = hmXPopup.$popup.width();
				var expandMin = 200;
				adjustedDims = true;
				do {
					expandWidth-=5;
					hmXPopup.$popup.width(expandWidth + "px");
				} while (!horizontalScroller() && expandWidth > expandMin);
				hmXPopup.$popup.width((expandWidth+8) + "px");
			}
				
			/*if (verticalScroller()) {					 
				hmXPopup.$popup.height((hmXPopup.$popupheader.height() + hmXPopup.popupwindow.scrollHeight + 2) + "px");
				} */
			} 
			
		
		// Now position the popup
		
		wW = windowWidth()-5; pW = hmXPopup.$popup.width();
		wH = windowHeight()-5; pH = hmXPopup.$popup.height();
		
		// VERTICAL
		
		// Does it fit in the standard position (just below and right of click position)?
		if ((pH + hmXPopup.posY + 15 < wH-5) && (pW + hmXPopup.posX  + 30 < wW-5)) {
			hmXPopup.posY+=15;
			hmXPopup.posX+=30;
		} else {
			
		 // Vertical: Move up minimum amount to fit with 30 px from bottom of window
		 if (pH + hmXPopup.posY + 30 > wH-5) {
			 hmXPopup.posY = wH - (pH+30);
			 hmXPopup.posY = hmXPopup.posY < 0 ? 5 : hmXPopup.posY;
		 } else {
			 hmXPopup.posY+=15;
		 }
		 
		 // HORIZONTAL
		 
		 // Horizontal: Move left to fit
		 if (pW + hmXPopup.posX  + 30 > wW-5) {
			 // Fits left of the click point?
			 if (pW+5 < hmXPopup.posX) {
				 hmXPopup.posX = hmXPopup.posX - (pW+5);
			 } else { 
				hmXPopup.posX = wW - (pW+30);
			 }
			 hmXPopup.posX = hmXPopup.posX < 0 ? 5 : hmXPopup.posX;
		 } else hmXPopup.posX+=30;
		 
		}
		if (hmDevice.ipad) {
			hmXPopup.posX-=20;
			hmXPopup.posY-=20;
		}
		// Apply the calculated position coordinates
		hmXPopup.$popup.css({"top": (hmXPopup.posY) + "px", "left": (hmXPopup.posX) + "px"});
		
		// Convert dimensions to responsive for larger popups
		if (adjustedDims && (hmXPopup.$popup.width() > hmXPopup.$popupcontainer.width() * 0.5 || hmXPopup.$popup.height() > hmXPopup.$popupcontainer.height() * 0.5)) {
			setPercentageDimensions();
		}
		
	};

// Close the popup (only destroys contents, not the actual popup)
hmXPopup.closePopup = function() {
	hmXPopup.$popup.fadeOut(300);

	// Unbind all the resizing on the desktop
	if (hmDevice.desktop) {
		$("body").off(".resizepopup").off(".resizepopuplistener");
		hmXPopup.$dragsurface.off(".resizepopup").attr("style","");
		$('input#bookmarkPermalink').off("mousedown");
	}
	
	// Kill Google Plus One in sharing popup
	
	if(hmXPopup.social && $('script#gplus_script').length > 0) {
		$('script#gplus_script').remove();
	}
	
	// Kill any video iframes and objects to prevent hangovers and crashes
	if (!hmXPopup.social && !hmXPopup.permalink) {
		hmXPopup.$popupbody.find("iframe").attr("src","");
		var $videoBits = hmXPopup.$popupbody.find("object,embed,param");
		if ($videoBits.length > 0) {
			// In IE the only a reload gets rid of the buffered video object
			if (/trident|edge/i.test(window.navigator.userAgent)) {
				document.location.reload();
			}
			else {
				$videoBits.attr("data","").attr("src","").attr("value","").remove();
			}
		}
	}
	// Clear the inline style settings of the popup and contents of the container
	hmXPopup.$popup.attr("style","");
	hmXPopup.$popupbody.html("");
};

// Bookmarker
hmXPopup.bookmarkPermalink = function() {
	if (hmBrowser.server) {
	        alert("Right-click on permalink to copy, then press CTRL+D or CMD+D to bookmark.");
	} else {
			alert("This page is stored locally. You can only bookmark pages on a web server.");
	}
	}; // Bookmark()

// Global load popup executed by the JS popup when it is loaded
hmLoadPopup = function(popObj) {
	$(document).on("keydown.popescape", function(event){
		if (event.which === 27) {
			$(document).off(".popescape");
			hmXPopup.closePopup();
		}
	});
	hmXPopup.$popup.attr("style","");
	hmXPopup.$popupbody.html(popObj.hmBody);
	hmXPopup.$popuptitle.html(popObj.hmTitle);
	if (typeof popObj.dims === 'undefined')
		hmXPopup.$popup.css({"height": "3.6rem", "width": "20rem"});
	else
		hmXPopup.$popup.css(popObj.dims);
	hmXPopup.noresize = false;
	
	// Permalink popup
	if (typeof popObj.permalink !== 'undefined' && popObj.permalink){
		hmXPopup.noresize = true;
		$("textarea#plinkBox").text(document.location.protocol + "\/\/" + document.location.hostname + (document.location.port === "80" || document.location.port === "" ? "" : ":" + document.location.port) +  document.location.pathname);
		$("p#permalink_tip").text("Right-click on permalink to copy to clipboard");
		$("input#bookmarkPermalink").attr("value","Bookmark Topic");
		$('input#selectPermalink').on(hmBrowser.touchstart, function(){
			$('textarea#plinkBox').focus().select();
		});
		// Prevent deselection
		$('textarea#plinkBox').on("mousedown", function(event){
			if (event.button == 2)
			$(this).focus().select();
		});
		$('input#bookmarkPermalink').on("mousedown", function(){
			hmXPopup.bookmarkPermalink();
		});
	}
	
	hmXPopup.$popup.show();
	hmXPopup.sizeAndPosition();

	// Social sharing popup
	/*if (typeof hmXPopup.social !== 'undefined' && hmXPopup.social) {
		// hmXPopup.noresize=true;
		setTimeout(hmXPopup.sizeAndPosition,3000);
	}*/
	
	// Permalink popup
	if (typeof popObj.permalink !== 'undefined' && popObj.permalink){
	var newH = (hmXPopup.$popup.height() + 7) + "px",
		newW = ($("textarea#plinkBox").width() + 25) + "px";
	hmXPopup.$popup.css({"width": newW, "min-width": newW, "height": newH, "min-height": newH});
	$('textarea#plinkBox').focus().select();
	}
	
	if (hmDevice.desktop && !hmXPopup.noresize) {
		$("body").off("mousemove.resizepopuplistener").on("mousemove.resizepopuplistener", function(event){
		var ev = event.originalEvent;
		hmXPopup.resizeListener(ev);
		});
		$("body").off("mousedown.resizepopup").on("mousedown.resizepopup", function(event){
			var e = event.originalEvent;
			hmXPopup.startResizePopup(e);
		});
	}
	
};

// This is the function called by the click on the popup link
hmXPopup.loadPopup = function(thisPopup, refPath, refTitle) {
	var htmlSource = false,
		popExtension = thisPopup.substr(thisPopup.lastIndexOf('.')+1,2),
		target = "",
		thisPath = hmXPopup.refPath;

	if (typeof refPath !== "undefined") {
		thisPath = refPath;
		if (popExtension !== "js")
			htmlSource = true;
	}
	
	if (Object.keys(hmWebHelp.visitedTopics).length > 300)
		hmWebHelp.visitedTopics = {};

	
	// Check for missing trailing slash
	if (thisPath.charAt(thisPath.length-1) !== "/")
		thisPath +="\/";
	target = thisPath + thisPopup;
	if (!htmlSource) {
		hmXPopup.social = !!(thisPopup === 'socialsharing.js');

		if (!hmWebHelp.visitedTopics.hasOwnProperty(thisPopup)){
		$.getScript(target, function(data, textStatus, jqxhr) {
			if (textStatus !== 'success')
				alert("Error loading popup file: " + target);
					else 
						hmWebHelp.visitedTopics[thisPopup] = true;
				});} else {
					$.cachedScript(target).done(function(script,textStatus){
						if (textStatus !== 'success')
						alert("Error loading popup file: " + target);
		});
				}
	} else {
		var popObj = {};
		popObj.hmTitle = typeof refTitle === 'string' ? refTitle : "";
		popObj.permalink = !!(thisPopup === '_hmpermalink.html');
		$.get(target, function(ref) {
			popObj.hmBody = ref;
			hmLoadPopup(popObj);
		},'html'); 
		
	}
};

// Delegated event binding for topic links in the popup
hmXPopup.$popupbody.on("click",
	"a.topiclink, a.topichotspot",
	function(event){
		event.preventDefault(); event.stopPropagation();
		}).on(hmBrowser.touchstart, "a.topiclink, a.topichotspot",
		function(event) {
		event.preventDefault(); event.stopPropagation();
		var target = $(this).attr("href");
		// Handle disabled links/tools
		if (!target) return;
		var thisAnchor = target.indexOf("#") > 1 ? target.split("#").pop() : false,
			thisPage = document.location.pathname.split("\/").pop(),
			targetPage = target.indexOf("#") > 1 ? target.substr(0,target.lastIndexOf("#")) : target,
			newPage = thisPage !== targetPage;
		
		// Filter out anchor links to targets in current topic 
		if (thisAnchor && !newPage) { 
			hmWebHelp.scrollTopic(thisAnchor);
		} else {
				target = hmWebHelp.targetCheck($(this).attr("href"));
				if (hmWebHelp.hmMainPageCheck(target)) {
					History.pushState(null,null,target);
				}
			}
	});	
// Delegated event binding for web links in the popup
hmXPopup.$popupbody.on(hmBrowser.touchstart,
	"a.weblink, a.webhotspot",
	function(event){
		if (hmDevice.desktop)
			$(this).attr("target","_blank");
		else
			$(this).attr("target","_self");
	});

// Delegated event binding for popup links in the popup
hmXPopup.$popupbody.on(hmBrowser.touchstart,
	"a.popuplink,a.popuphotspot,a.topichotspot[href^='javascript:void']",
	function(event){
		event.preventDefault();
		hmXPopup.clickX = hmXPopup.$popup.position().left;
		hmXPopup.clickY = hmXPopup.$popup.position().top;
		var popupTarget = $(this).attr("data-target");
		hmXPopup.loadPopup(popupTarget);
	});	

	
// Binding for the close icon in the top right corner
$("div#hmclosepopup").on(hmBrowser.touchstart,hmXPopup.closePopup);

/*** Draggable Popups ***/

// Get the type of interaction event from user
hmXPopup.EventType = function(e) {
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

	
	// General variables
	hmXPopup.startTime = 0;
	hmXPopup.dragTime = 0;
	hmXPopup.dragcount = 0;
	hmXPopup.oldX = undefined;
	hmXPopup.oldY = undefined;
	hmXPopup.oldLeftPos = undefined;
	hmXPopup.oldTopPos = undefined;

	
	// Perform this at the end of a drag operation
	hmXPopup.endDrag = function(e) {
		hmXPopup.dragTime = new Date().getTime() - hmXPopup.startTime;
		if (hmXPopup.dragTime < 200) {
			}
		hmXPopup.$popupdragger.off(".endevents");
		hmXPopup.$popupdragger.off(".moveevents");
		hmXPopup.$dragsurface.off(".endevents");
		hmXPopup.$dragsurface.off(".moveevents");
		hmXPopup.$dragsurface.hide().css("cursor","ew-resize");
		hmXPopup.PreventDefault(e);
		};
			// Drag action
	hmXPopup.performDrag = function(e) {
		hmXPopup.dragTime = new Date().getTime() - hmXPopup.startTime;
		if (hmXPopup.dragTime < 50) {
			return;
		}
		
		var touchobj;
		if (typeof e.changedTouches != 'undefined') { 
				touchobj = e.changedTouches[0];
			} else {
				touchobj = e;
			}
		
		// Only move once every x events on mobile for lower processor load 
		hmXPopup.dragcount++;
		if ( hmDevice.desktop || hmXPopup.dragcount > 2 ) {
		hmXPopup.dragcount = 0;
		var moveX = (!(document.all && !window.opera)) ? touchobj.pageX - hmXPopup.oldX : touchobj.clientX - hmXPopup.oldX;
		var moveY = (!(document.all && !window.opera)) ? touchobj.pageY - hmXPopup.oldY: touchobj.clientY - hmXPopup.oldY;
		hmXPopup.$popup.css({"left": (hmXPopup.oldLeftPos + moveX) + 'px'});
		hmXPopup.$popup.css({"top": (hmXPopup.oldTopPos + moveY) + 'px'});
		}
	};
	// Triggered at beginning of a drag
    hmXPopup.startDrag = function(e) {
		hmXPopup.PreventDefault(e);
		hmXPopup.startTime = new Date().getTime();
		var touchobj;
		if (typeof e.changedTouches != 'undefined') 
			touchobj = e.changedTouches[0];
		else
			touchobj = e;
		hmXPopup.oldX = (!(document.all && !window.opera)) ? touchobj.pageX : touchobj.clientX;
		hmXPopup.oldY = (!(document.all && !window.opera)) ? touchobj.pageY : touchobj.clientY;
		hmXPopup.oldLeftPos = hmXPopup.$popup.position().left;
		hmXPopup.oldTopPos = hmXPopup.$popup.position().top;

		// Activate the drag surface overlay
		if (hmBrowser.touch || hmDevice.winphone || hmXPopup.EventType(e) == "mouse") {
		hmXPopup.$dragsurface.css("cursor","all-scroll").show();
		hmXPopup.$dragsurface.on(hmBrowser.touchmove, function(event) {
			var ev = event.originalEvent; 
			hmXPopup.performDrag(ev);
			});
		hmXPopup.$dragsurface.on(hmBrowser.touchend, function(event) {
			var ev = event.originalEvent; 
			hmXPopup.endDrag(ev);
			});
		} 
		
		hmXPopup.$popupdragger.on(hmBrowser.touchmove, function(event) {
			var ev = event.originalEvent; 
			hmXPopup.performDrag(ev);
			});
		hmXPopup.$popupdragger.on(hmBrowser.touchend, function(event) {
			var ev = event.originalEvent; 
			hmXPopup.endDrag(ev);
			});

	};

	hmXPopup.$popupdragger.off(hmBrowser.touchstart).on(hmBrowser.touchstart, function(event) {
		var ev = event.originalEvent; 
		hmXPopup.startDrag(ev);
		});

// Resizable popups only on the desktop
if (hmDevice.desktop) {

hmXPopup.resizeListener = function(e) {
	 var popPos = hmXPopup.$popup.position(),
	 popWd = hmXPopup.$popup.outerWidth(),
	 popHt = hmXPopup.$popup.outerHeight(),
	 bWidth = 2;
	 hmXPopup.ewResize = "ew-resize";
	 hmXPopup.nsResize = "ns-resize";
	// Body cursor for resizing, based on proximity to left and right borders of popup
	var rBd = ((e.pageX > (popPos.left + (popWd-4))) && (e.pageX < (popPos.left + popWd+8)) && (e.pageY > popPos.top) && (e.pageY < popPos.top + popHt+8));
	var bBd = ((e.pageY > (popPos.top + (popHt-4-bWidth))) && (e.pageY < (popPos.top + popHt+8)) && (e.pageX < (popPos.left + popWd+8)) && (e.pageX > popPos.left+4));
	var corner = ((rBd && (e.pageY > (popPos.top + popHt-10))) || (bBd && e.pageX > (popPos.left + popWd-10)));
	hmXPopup.bdDrag = rBd || bBd;

	$("body").css("cursor", function(){
	return corner ? "nw-resize" : rBd && !bBd ? hmXPopup.ewResize : bBd && !rBd ? hmXPopup.nsResize : "auto";
	});
};
	
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
		var moveX = event.clientX - hmXPopup.resizeX,
			moveY = event.clientY - hmXPopup.resizeY,
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
			hmXPopup.$popup.css({"width": newX + "px", "max-width": newX + "px", "height": newY + "px", "max-height": newY + "px"});
	};
	
	hmXPopup.endResizePopup = function(event) {
		// $("body").off("mousemove.resizepopup");
		hmXPopup.$dragsurface.off("mousemove.resizepopup");
		$("body").off("mouseup.resizepopup");
		hmXPopup.$popupbody.css("overflow","auto");
		$("body").css("cursor","default");
		hmXPopup.$dragsurface.css("cursor","default").hide();
	};
	
	hmXPopup.startResizePopup = function(e) {
		var thisCursor = $("body").css("cursor");
		function initialize(direction) {
			hmXPopup.resizeX = e.pageX;
			hmXPopup.resizeY = e.pageY;
			hmXPopup.$popupbody.css("overflow","hidden");
			hmXPopup.popdims = {};
			hmXPopup.popdims.w = hmXPopup.$popup.width();
			hmXPopup.popdims.h = hmXPopup.$popup.height();
			$("body").on("mouseup.resizepopup", function(event) {
			var e = event.orginalEvent;
			hmXPopup.endResizePopup(e);
			});
			hmXPopup.$dragsurface.css("cursor",thisCursor);
			$("body").css("cursor",thisCursor);
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
} // End popup resizer block / desktop only
