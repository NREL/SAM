/* Project settings */
var hmAnimate = true;
var hmPopupSticky = true;
var hmImageLightbox = true;
var hmVideoLightbox = true;
var hmLightboxConstrained = true;
var hmForceRedirect = true;
var hmTocSingleClick = true;
var autocollapse = false;
var gaaccount = "";
var initialtocstate = "expandall";
var agent = navigator.userAgent,
  platform = navigator.platform,
  hmBrowser = {};
  hmBrowser.touch = !!(('ontouchstart' in window && !window.opera) || ('msmaxtouchpoints' in window.navigator) || ('maxtouchpoints' in window.navigator) || (navigator.maxTouchPoints > 0) || (navigator.msMaxTouchPoints > 0));
  hmBrowser.nonDeskTouch = ((hmBrowser.touch && !/win32|win64/i.test(platform)) || (hmBrowser.touch && /win32|win64/i.test(platform) && /mobile/i.test(agent)));

  hmBrowser.eventType = (('onmousedown' in window && !hmBrowser.nonDeskTouch) ? "mouse" : ('ontouchstart' in window) ? "touch" : ('msmaxtouchpoints' in window.navigator || navigator.msMaxTouchPoints > 0) ? "mstouchpoints" : ('maxtouchpoints' in window.navigator || navigator.maxTouchPoints > 0) ? "touchpoints" : "mouse");
  	 switch(hmBrowser.eventType) {
  	  case "mouse":
  	 		hmBrowser.touchstart = "mousedown";
  	 		hmBrowser.touchend = "mouseup";
  	 	break;
  	  case "touch":
  	  	hmBrowser.touchstart = "touchstart";
  	 		hmBrowser.touchend = "touchend";
  	 	break;
  	 	case "mstouchpoints":
  	 	  hmBrowser.touchstart = "MSPointerDown";
  	 		hmBrowser.touchend = "MSPointerUp";
  	 	break;
  	 	case "touchpoints":
  	 		hmBrowser.touchstart = "pointerdown";
  	 		hmBrowser.touchend = "pointerup";
  	 	break;
  	 }
