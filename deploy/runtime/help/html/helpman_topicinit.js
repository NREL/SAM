/* --------------- Script (c) 2006-2015 EC Software ---------------
This script was created by Help & Manual. It is designed for use 
in combination with the output of Help & Manual and must not
be used outside this context.     http://www.helpandmanual.com

Do not modify this file! It will be overwritten by Help & Manual.
-----------------------------------------------------------------*/

var topicInitScriptAvailable = true;
var HMToggles = new Array();
var HMGallery = new Array();
var HMTogglesAllExpanded = false;

function hmmin(v1, v2) { if (v1<v2) return v1; return v2 }
function hmmax(v1, v2) { if (v1>v2) return v1; return v2 }

var HMSyncTOC = function(indexPageUrl, selfUrl) {
   if (location.search.lastIndexOf("toc=0")<=0) {
     if (parent.hmNavigationFrame) { parent.lazysync(selfUrl); }
     else if ((hmForceRedirect) && (parent.location) && (parent.location.href)) { parent.location.href = indexPageUrl+'?'+selfUrl; }
   }
}
	
var HMToggleExpandAll = function(value) {
  if (HMToggles.length != null){ 
    for (i=0; i<HMToggles.length; i++){ 
      HMToggleExpand(HMToggles[i], value, (value && hmAnimate)); 
	}
  }
  HMTogglesAllExpanded = value;
}

var HMAnyToggleOpen = function() {
  var anyOpen = false;
  if (HMToggles.length != null){ 
    for (i=0; i<HMToggles.length; i++){ 
  	  if (HMToggles[i].getAttribute("hm.state")=='1') anyOpen = true; 
	}
  }
  if (!anyOpen) HMTogglesAllExpanded = false;
  return anyOpen;
}

var HMToggle = function() { 
	var op = HMToggle.arguments[0];
	for (i=1; i<HMToggle.arguments.length; i++) {
	  var objID = HMToggle.arguments[i]; 
       	  var obj = document.getElementById(objID);
 	  switch (op) {
		case "toggle": 
		  var state = obj.getAttribute("hm.state");
		  if (state == null) { state = "0" }; 
		  HMToggleExpand(obj, (state != "1"), hmAnimate);
		  break;

		case "expand":
		  HMToggleExpand(obj, true, false);
		  break;

		case "collapse":
		  HMToggleExpand(obj, false, false);
		  break;
	  }
	}
}

var HMToggleExpand = function(obj, value, animate) {
	tagName = obj.nodeName.toLowerCase();
	switch (tagName) {
		case "span":
		  HMToggleExpandText(obj, value, animate);
		  break;
		case "div":
		  HMToggleExpandDropdown(obj, value, animate);
		  break;
		case "img":
		  HMToggleExpandPicture(obj, value, animate);
		  break;
	}
	obj.setAttribute("hm.state", value ? "1" : "0");
}

var HMToggleExpandText = function(obj, value, animate) {
  obj.style.display = (value ? "inline" : "none");  //cannot be animated
}

var HMToggleExpandDropdown = function(obj, value, animate) {
  if (animate) {
	/* $(obj).stop(); don't stop here */ 
    if (value) {
      $(obj).slideDown('fast');
    }
    else {
	  $(obj).animate({ height: 'toggle' }, 'fast', function() {
		if (document.all && !window.opera) { // Avoid collapsing margins bug in IE
	  	  var dummy = $(obj).prev();
	  	  if ($(dummy).outerHeight!=0) dummy = $('<div style="height:1px"></div>').insertBefore(obj);
	  	  else $(dummy).css('display', 'block');
          $(dummy).css('display', 'none');
        } 
  	  });
    } 
  }
  else {
    obj.style.display = (value ? "block" : "none");
  }
}

var HMToggleExpandPicture = function(obj, value, animate) {
  var oldFile = (value ? obj.getAttribute("hm.src0") : obj.getAttribute("hm.src1"));
  var newFile = (value ? obj.getAttribute("hm.src1") : obj.getAttribute("hm.src0"));
  var newSrc = obj.src.replace(oldFile, newFile);
  var isToggleIcon = (obj.getAttribute("hm.type")=="dropdown");

  if ((!isToggleIcon) && (animate)) {
	$(obj).stop(); 
	
	var newImg = new Image();
    newImg.onload = function() {
	  var newWidth  = newImg.width;
	  var newHeight = newImg.height;
  	  var oldWidth  = obj.width;
  	  var oldHeight = obj.height;

      if ((newWidth > 0) && (newHeight > 0)) {
        if ((newWidth == oldWidth) && (newHeight == oldHeight)) {
          obj.src = newSrc;
        }
        else {
          $(obj).animate({ width: newWidth, height: newHeight }, 'fast', function() {
  	        obj.src = newSrc;
          });
  	    }
  	  }
    };
	newImg.src = newSrc;
  }
  else { 
    obj.src = newSrc;
  }
  var newTitle = (value ? obj.getAttribute("hm.title1") : obj.getAttribute("hm.title0"));
  if (newTitle != null) { obj.title = newTitle; }
  var newCaption = (value ? obj.getAttribute("hm.caption1") : obj.getAttribute("hm.caption0"));
  if (newCaption != null) { obj.parentNode.parentNode.parentNode.nextSibling.firstChild.firstChild.innerHTML = newCaption; }
}

var HMShowPictureLightbox = function(objID) {
  var obj = document.getElementById(objID); /* our <img> clicked */

  var startL = $(obj).offset().left;
  var startT = $(obj).offset().top;
  var startW = $(obj).outerWidth();
  var startH = $(obj).outerHeight();

  var oldFile = obj.getAttribute("hm.src0");
  var newFile = obj.getAttribute("hm.src1");
  var newSrc = obj.src.replace(oldFile, newFile);
  var newTitle = obj.getAttribute("hm.title1");
  var newCaption = obj.getAttribute("hm.caption1");

  var htmlCode = '<img id="hmlightboximage" src="' + newSrc + '" alt="' + newTitle + '"/>';
  var imgPreloader = new Image();
  imgPreloader.onload = function() {
  	HMShowLightbox(htmlCode, startL, startT, startW, startH, imgPreloader.width, imgPreloader.height, newCaption, true, false); 
  };
  imgPreloader.src = newSrc;

}

var HMShowVideoLightbox = function(event, obj, htmlcode, vWidth, vHeight) {
  var startL = $(obj).offset().left;
  var startT = $(obj).offset().top;
  var startW = $(obj).outerWidth();
  var startH = $(obj).outerHeight();

  if (event.stopPropagation) { event.stopPropagation(); } else { event.cancelBubble = true; } //MSIE

  HMShowLightbox(htmlcode, startL, startT, startW, startH, vWidth, vHeight, '', false, true);
}

var HMShowLightbox = function(htmlCode, startL, startT, startW, startH, endW, endH, newCaption, doAnimate, isVideo) {
  /* Find our scope: 
   * 1) Is this an orphan window or part of a webhelp frame?
   * 2) If it is a webhelp frame, is it an iframe or classic <frame>?
   * 3) Can we do cross-frame scripting? (not possible in Chrome locally) 
   */
  var parentScope = ((parent.hmNavigationFrame) && (parent.document.getElementById('hmnavigation')) && (parent.document.getElementById('hmnavigation').nodeName.toLowerCase()=='iframe'));
  var lightboxWindow = $(window);
  var lightboxDocument = $(document);
  var lightboxBody = $('body');
  var lighboxScrollLayer = null;
  if (parentScope) {          /* use entire window for lightbox */
  	lightboxBody = parent.$('body');
   	lightboxDocument = parent.$(document);
    lightboxWindow = parent.window;
  }

  $(lightboxBody).prepend('<div id="hmlightboxbackground" style="z-index:99997;border:none;padding:0;margin:0;position:absolute;left:0;top:0;background-color:#7F7F7F"></div>');  
  var lightboxBackground = parentScope ? parent.$('#hmlightboxbackground') : $('#hmlightboxbackground'); 
  lightboxBackground.css('opacity', '0.5');

  if (parentScope) {
  	$(lightboxBody).prepend('<div id="hmlightboxscrolllayer" style="z-index:99998;border:none;padding:0;margin:0;position:absolute;left:0;top:0;background:none;overflow:auto"></div>');
  	lighboxScrollLayer = parent.$('#hmlightboxscrolllayer');
  	lightboxBody = lighboxScrollLayer;  
  } 

  $(lightboxBody).prepend('<div id="hmlightbox" style="z-index:99999;position:absolute;display:none"></div>');
  var lightbox = parentScope ? parent.$('#hmlightbox') : $('#hmlightbox');  
  var lightboxObject = $(htmlCode).appendTo(lightbox);
  var lightboxCaption = null;
  if (newCaption != null) {
  	$(lightbox).append('<p id="hmlightboxcaption">' +newCaption+ '</p>');
  	lightboxCaption = parentScope ? parent.$('#hmlightboxcaption') : $('#hmlightboxcaption');
  }  	

  var lightboxSpeed = 300;
  var sizeStart; /* keep initial size for hide animation */
  var maxW = endW;
  var maxH = endH;
  if (hmLightboxConstrained) {
    if (endW > ($(lightboxWindow).width()-40)) {
	  endW = $(lightboxWindow).width()-40;
	  if (endW < (maxW/2)) endW = maxW/2;
	  endH = maxH * endW / maxW;
    }
  }

  lightboxObject.css({'width': endW+'px', 'height': endH+'px'});
  if (lightboxCaption!=null) lightboxCaption.css('width', endW+'px');
  	
  if (hmAnimate&&doAnimate) {
    var sizeEnd = lightboxGetsize();
  	if (lightboxCaption!=null) lightboxCaption.css('display', 'none'); /* hide caption during animation */
    lightboxObject.css({'width': startW + 'px', 'height': startH + 'px'});
  	sizeStart = lightboxGetsize();
	lightboxResize();
     	
    sizeStart[0] = startL;
    sizeStart[1] = startT;
    if (parentScope) {
      sizeStart[0] = sizeStart[0] + parent.$('#hmcontent').offset().left - $(document).scrollLeft();
      sizeStart[1] = sizeStart[1] + parent.$('#hmcontent').offset().top - $(document).scrollTop();
    }
    lightbox.css({'left': sizeStart[0]+'px', 'top': sizeStart[1]+'px'});
    lightbox.show();
    	
    lightbox.animate({ left: sizeEnd[0]-lightboxObject.position().left, top: sizeEnd[1]-lightboxObject.position().top }, 
                       lightboxSpeed, 
                       function() { 
                         if (lightboxCaption!=null) lightboxCaption.css('display', 'block');
                       }
                     );
                         
    lightboxObject.animate({ width: endW, height: endH }, lightboxSpeed); 
  }
  else {
    var sizeEnd = lightboxGetsize();
    lightboxResize();
	lightbox.show();  	
  }  

  $(lightboxWindow).bind('resize.hmlightbox', lightboxResize);
  $(lightboxWindow).bind('scroll.hmlightbox', lightboxScroll);
  $(lightboxBody).bind('click.hmlightbox', lightboxClick);
  $(lightboxBody).bind('keydown.hmlightbox', lightboxKeydown);
  $(lightbox).focus();

  function lightboxScroll() {
    lightboxBackground.css('width', (($(lightboxDocument).scrollLeft() > 0) ? $(lightboxDocument).width() : $(lightboxWindow).width()) +'px');
    lightboxBackground.css('height', (($(lightboxDocument).scrollTop() > 0) ? $(lightboxDocument).height() : $(lightboxWindow).height()) +'px');
  }
  	
  function lightboxResize() {
    if (hmLightboxConstrained) {
	  var tmpW = endW;
	  endW = $(lightboxWindow).width()-40;
	  if (endW > maxW) endW = maxW;
	  else if (endW < (maxW/2)) endW = maxW/2;
      if (tmpW != endW) {
        endH = maxH * endW / maxW;
        lightboxObject.css({'width': endW+'px', 'height': endH+'px'});
  	    if (lightboxCaption!=null) lightboxCaption.css('width', endW+'px');
  	  }
  	}

  	var size = lightboxGetsize();
    lightbox.css({left: size[0]+'px', top:size[1]+'px'});
    
    if (lighboxScrollLayer!=null) { 
      lighboxScrollLayer.css({'width': $(lightboxWindow).width()+'px', 'height': $(lightboxWindow).height()+'px'});
    }
    lightboxScroll();
  }

  function lightboxGetsize() {
  	var lbW  = lightbox.width();
  	var lbH  = lightbox.height();

  	if (isVideo) {
  	  lbW = endW;
  	  lbH = endH;
    }
  	var newW = hmmax(lbW + 40, lightboxDocument.width());
  	var newH = hmmax(lbH + 40, lightboxDocument.height());

	var newL = hmmax(20, parseInt(($(lightboxWindow).width() - lbW)/2) + (parentScope ? 0 : lightboxDocument.scrollLeft()));
	var newT = hmmax(20, parseInt(($(lightboxWindow).height() - lbH)/2) + (parentScope ? 0 : lightboxDocument.scrollTop()));

  	var size = new Array(newL, newT, lbW, lbH);
	return size;
  }

  function lightboxKeydown(e) { 
  	if (e.keyCode == 27) lightboxClose(); 
  }

  function lightboxClick(e) { 
    var	canClose = (!isVideo) ||
                   (e.pageX < lightbox.position().left) || (e.pageY < lightbox.position().top) ||
                   (e.pageX > lightbox.position().left+lightbox.width()) || (e.pageY > lightbox.position().top+lightbox.height());  	 
    if (canClose) lightboxClose(); 
  }

  function lightboxClose() {
  	if (hmAnimate&&doAnimate) {
      if (lightboxCaption!=null) lightboxCaption.css('display', 'none'); /* hide caption during animation */
      lightboxObject.animate({ width: startW, height: startH }, lightboxSpeed); 
      lightbox.animate({ left: sizeStart[0]-lightboxObject.position().left, top: sizeStart[1]-lightboxObject.position().top }, 
      	               lightboxSpeed, 
      	               function() { 
      	               	 lightbox.remove(); 
      	               	 if (lighboxScrollLayer!=null) lighboxScrollLayer.remove(); 
  	                     lightboxBackground.remove();
                       }
                      );
  	}
  	else {
      lightbox.remove();
      if (lighboxScrollLayer!=null) lighboxScrollLayer.remove(); 
  	  lightboxBackground.remove();
  	}
    $(lightboxWindow).unbind('.hmlightbox');
    $(lightboxBody).unbind('.hmlightbox');
  }
  
}



var HMSearchCheck = function(obj) {
  var foundHilite = window.location.search.lastIndexOf("zoom_highlight") > 0;
  if (!foundHilite) {
    var fontTags = obj.getElementsByTagName("FONT");
if (fontTags.length == 0)
fontTags = obj.getElementsByTagName("font");
    if (fontTags.length > 0) {
      var hStyle = "";
      for (var cCheck = 0; cCheck < fontTags.length; cCheck++) {
        hStyle = fontTags[cCheck].style.cssText;
        if (hStyle.indexOf("BACKGROUND-COLOR") == 0 || hStyle.indexOf("background-color") == 0) {
          foundHilite = true;
          break; 
        }
      }
    }
  }
  return foundHilite;     
}

var HMInitToggle = function() {
  if (document.getElementById) {
	var node = document.getElementById(HMInitToggle.arguments[0]);
	var isPicture = false;
	for (i=1; i<HMInitToggle.arguments.length-1; i=i+2) { 
		if (HMInitToggle.arguments[i] == "onclick") {
		  node.onclick = Function(HMInitToggle.arguments[i+1]); 
		}
		if (HMInitToggle.arguments[i].substring(0,6) == "hm.src") {
		  node.setAttribute(HMInitToggle.arguments[i], decodeURI(HMInitToggle.arguments[i+1]));
	      var img = new Image();
		  img.src = HMInitToggle.arguments[i+1];
		}
		else { 
		  node.setAttribute(HMInitToggle.arguments[i], HMInitToggle.arguments[i+1]);
		  if ((HMInitToggle.arguments[i] == "hm.type") && (HMInitToggle.arguments[i+1] == "picture")) { isPicture = true; } 
		}
	}
	if (isPicture) {
	   var aLink = node.parentNode;
	   if (aLink.nodeName.toLowerCase() == "a") {
	   	 if (hmImageLightbox) {
	  	   HMGallery[HMGallery.length] = node;
           aLink.href = "javascript:HMShowPictureLightbox('" + HMInitToggle.arguments[0] +"')";
	   	 }
	   	 else {
	  	   HMToggles[HMToggles.length] = node;
           aLink.href = "javascript:HMToggle('toggle','" + HMInitToggle.arguments[0] +"')";
	  	 }
	   }
	}
	else {
	  var mustExpand = HMSearchCheck(node); 
	  HMToggles[HMToggles.length] = node;
      if (mustExpand) { 
        node.setAttribute("hm.state", "1"); 
        var nodeImg = document.getElementById(node.getAttribute("id")+'_ICON');
        if (nodeImg) { HMToggleExpand(nodeImg, true); }
      }
	  HMToggleExpand(node, ((node.getAttribute("hm.state") == "1") || mustExpand));
    }
  }
}

var HMTrackTopiclink = function(obj) {
    if (parent.frames.length>0) {
      if (parent.gaaccount) { parent.track("exit", obj.href); }
    }
}

var hmshowPopup = function(event, txt, trigger) {

	$('#hmpopupdiv').stop().remove();
		 
	var pop = $('<div id="hmpopupdiv"></div>').appendTo('body');
	if (hmPopupSticky) { 
      $('body').bind(hmBrowser.touchend + '.hmpopup', hmhidePopup);
      $('body').bind('keydown.hmpopup', function(e) { if (e.keyCode == 27) hmhidePopup(); } ); 
    }	      
	pop.html(txt);
    var posLeft = event.clientX+$(document).scrollLeft();
    var posTop =  event.clientY+$(document).scrollTop();

	var maxW = $(window).width()/1.5;
	var w = pop.width();
	if (w > maxW) pop.width(maxW);
	var t = 20 + posTop;           
	var l = (posLeft - w/2);
	if (l < 10) l = 10;
	if ((l + pop.outerWidth()) > $(window).width()) l = $(window).width() - pop.outerWidth();
	if (l < 0) l = 0;
	pop.css( { left: l+'px', top: t+'px'} );
	if (hmAnimate) pop.show('fast');
	else pop.show();
}

var hmhidePopup = function() { 
    if (hmPopupSticky) $("body").unbind(".hmpopup");
    var pop = $('#hmpopupdiv');
    if (pop.length>0) {  //hide, don't remove
      if (hmAnimate) pop.hide('fast');
      else $(hmpopupdiv).hide();
    } 
}