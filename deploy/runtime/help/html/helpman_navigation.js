/* ------------ Script copyright 2005-2015 EC Software -------------
   This script was created by Help & Manual and is part of the      
   Webhelp export format. This script is designed for use in 
   combination with the output of Help & Manual and must not 
   be used outside this context. http://www.helpandmanual.com   
                                                                    
   Do not modify this file! It will be overwritten by Help & Manual.
   ----------------------------------------------------------------- */

var usecookie = false;
var tocselecting = false;
var abspossupported = 0;
var currentselection = null;
var currenttocstate = "";

if (gaaccount != "") {
  var _gaq = _gaq || [];
  _gaq.push(["_setAccount", gaaccount]);
  _gaq.push(["_trackPageview"]);
  (function() {
    var ga = document.createElement("script");
    ga.type = "text/javascript"; ga.async = true;
    ga.src = ("https:" == document.location.protocol ? "https://ssl" : "http://www") + ".google-analytics.com/ga.js";
    var s = document.getElementsByTagName("script")[0]; s.parentNode.insertBefore(ga, s);
    })();
}

function hmAddCss(adoc, cssCode) {
  var styleElement = adoc.createElement("style");
  styleElement.type = "text/css";
  if (styleElement.styleSheet) {
    styleElement.styleSheet.cssText = cssCode;
  }  
  else {
    styleElement.appendChild(adoc.createTextNode(cssCode));
  }
  adoc.getElementsByTagName("head")[0].appendChild(styleElement);
}

function hmSupportsAbspos() {
  if (abspossupported==0) {	
    tmp = $('<div style="position:absolute;left:0;right:0;display:none">&nbsp;</div>').appendTo('body');
    if (($(tmp).outerWidth()+30) > $(window).width()) abspossupported = 1;
    else abspossupported = -1;
  	tmp.remove();
  }
  if (abspossupported==1) return true;
  return false;
}

function hmNoAbsposResize(jdiv, w, h) {
  var windowWidth, windowHeight;
  if (self.innerHeight) {	// all except Explorer
    if(document.documentElement.clientWidth){
	  windowWidth = document.documentElement.clientWidth; 
    } else {
      windowWidth = self.innerWidth;
    }
    windowHeight = self.innerHeight;
  } else if (document.documentElement && document.documentElement.clientHeight) { // Explorer 6 Strict Mode
    windowWidth = document.documentElement.clientWidth;
    windowHeight = document.documentElement.clientHeight;
  } else if (document.body) { // other Explorers
    windowWidth = document.body.clientWidth;
    windowHeight = document.body.clientHeight;
  }	
  if (w) jdiv.css('width', ( windowWidth - parseInt(jdiv.css('left')) - parseInt(jdiv.css('right')) ));
  if (h) jdiv.css('height', ( windowHeight - parseInt(jdiv.css('top')) - parseInt(jdiv.css('bottom')) ));
}	

function hmNavigationFrame() {
    var actFrames = new Array(
        window.frames['hmnavigation'],
        window.frames['hmcontent']
    );
    for (var i=0;i<actFrames.length;i++) {
        if (actFrames[i].name=='hmnavigation') return actFrames[i];
    }
    return self;
}

function hmContentFrame() {
  if (!document.getElementById("hmcontent")) return null;
  return hmcontent;  	
}

var lastTrackEvent = "";

function track(action, data) {
  if (gaaccount != "") {	
    if ((window._gat) && (lastTrackEvent != action+data)) {
      lastTrackEvent = action+data;	
      var pageTracker = window._gat._getTrackerByName('hmga');
	  if (!pageTracker) {
		pageTracker = window._gat._createTracker(gaaccount, 'hmga');
	  }
      switch(action) {
      case "topic":
        data = data.substring(data.indexOf("//")+1,data.length);
        data = data.substring(data.lastIndexOf("/")+1,data.length);
        pageTracker._trackPageview(data);
        break;

      case "search":
        pageTracker._trackPageview(data);
        break;

      case "index":
        pageTracker._trackEvent("Help|IndexTerm", data, null, null);
        break;

      case "exit":
        pageTracker._trackEvent("Help|ExitTo", data, null, null);
        break;
      }
    }
  }
}

function fullexpand(animate) { switchall(true, animate); }
function fullcollapse(animate) { switchall(false, animate); }

function levelexpand(divID, animate) {
	var thisDIV = hmNavigationFrame().document.getElementById(divID); 
    var items = thisDIV.getElementsByTagName("li");
    for(var i = 0; i < items.length; i++) {
      if (items[i].parentNode==thisDIV) {
        var thisUL = hmULfromID(items[i].id);	
        if (thisUL) hmSwitchNode(thisUL, true, animate);
      }	
    }
}

function switchall(nodevisible, animate) {
    var toc = hmNavigationFrame().document.getElementById("toc");
    if (toc) {
       var items = toc.getElementsByTagName("ul");
       for(var i = 0; i < items.length; i++) hmSwitchNode(items[i], nodevisible, animate);
       if ((nodevisible) && (currentselection)) intoview(currentselection, toc, false);
    }
}

function loadstate(toc) {
	var tmpAnimate = hmAnimate;
    hmAnimate = false;
    currentselection = null;
   
    if (currenttocstate=="") {
      if (usecookie) currenttocstate = document.cookie;
      else { /* load default toc state */
	    if (initialtocstate == 'expandall') fullexpand(false);
	    else { 
	      if (initialtocstate == 'expandtoplevel') { fullcollapse(false); levelexpand('toc', false); }
	      else fullcollapse(false);
        }
      }
    }
    if (currenttocstate != "") {
      var toc = hmNavigationFrame().document.getElementById("toc");
      if (toc) {
        var items = toc.getElementsByTagName("ul");
        for(var i = 0; i < items.length; i++) hmSwitchNode(items[i], (currenttocstate.indexOf(items[i].id+',')>-1), false);
      }
    }
    if (hmContentFrame()) {
      var topicID = hmContentFrame().location.href.substring(hmContentFrame().location.href.lastIndexOf("/")+1,hmContentFrame().location.href.length);
      if (topicID.lastIndexOf("#") != -1) topicID = topicID.substring(0,topicID.lastIndexOf("#"));
      if (topicID.lastIndexOf("?") != -1) topicID = topicID.substring(0,topicID.lastIndexOf("?"));
      tocselecting = false;
      lazysync(topicID);
    }
    hmAnimate = tmpAnimate;
}

function savestate(toc) {
    currenttocstate = "";
    var items = toc.getElementsByTagName("ul");
    for(var i = 0; i < items.length; i++) if (items[i].style.display!="none") currenttocstate = currenttocstate.concat(items[i].id + ",");
    if (usecookie) document.cookie = currenttocstate;
}

function toggle(nodeID) {
    var thisUL = hmULfromID(nodeID);
    if (thisUL) hmSwitchNode(thisUL, ((thisUL.style.display=="none")?true:false), true);
}

function hmNodeClicked(node, event) {
    if (event.stopPropagation) { event.stopPropagation(); } else { event.cancelBubble = true; } //MSIE
    if ($(node).css('direction') != 'rtl') {
      var scrOfX = $(hmNavigationFrame()).scrollLeft();
      var scrOfY = $(hmNavigationFrame()).scrollTop();
      var offset = parseInt( $(node).css('padding-left'));
      var thisClicked = ( (event.clientY+scrOfY > $(node).offset().top) && (event.clientY+scrOfY < ($(node).offset().top+offset)) &&
                          (event.clientX+scrOfX > $(node).offset().left) && (event.clientX+scrOfX < ($(node).offset().left+offset+$(node.firstChild).outerWidth())) );
      var iconClicked = (thisClicked && (event.clientX+scrOfX > $(node).offset().left) && (event.clientX+scrOfX < ($(node).offset().left+offset)));
	}
	else {
      var scrOfX = $(hmNavigationFrame()).scrollLeft();
      var scrOfY = $(hmNavigationFrame()).scrollTop();
      var offset = parseInt( $(node).css('padding-right'));
      var nodeW = $(node).outerWidth();
      var textW = $(node.firstChild).outerWidth();
      var thisClicked = ( (event.clientY+scrOfY > $(node).offset().top) && (event.clientY+scrOfY < ($(node).offset().top+offset)) &&
                          (event.clientX+scrOfX < $(node).offset().left+nodeW) && (event.clientX+scrOfX > ($(node).offset().left+nodeW-offset-textW)) );
      var iconClicked = (thisClicked && (event.clientX+scrOfX < $(node).offset().left+nodeW) && (event.clientX+scrOfX > ($(node).offset().left+nodeW-offset)));
	}	
    var thisID = node.id;
    if (iconClicked) {
      toggle('ul'+thisID.substring(1,thisID.length));
    }
    else if (thisClicked) {
      if (hmTocSingleClick) {
        hilightexpand('s'+thisID.substring(1,thisID.length));
      }
      else {
        hilight('s'+thisID.substring(1,thisID.length));
      }    
    }
}		

function hmNodeDblclicked(node) {
    thisID = node.id;
    toggle('ul'+thisID.substring(1,thisID.length));
}		

function hmLIfromID(thisID) {
  return hmNavigationFrame().document.getElementById('i'+thisID.replace(/[isaul]/g,''));
}

function hmULfromID(thisID) {
  return hmNavigationFrame().document.getElementById('ul'+thisID.replace(/[isaul]/g,''));
}

function hmSwitchNode(thisUL, nodevisible, animate) {
  if ((thisUL.style.display!='none')!=nodevisible) { 
    var thisLI = hmLIfromID(thisUL.id);
    if (thisLI) { 
       var thisIcon = thisLI.getAttribute('data-bg');
       if (thisIcon!='') $(thisLI).css('background-image', 'url(' + (nodevisible ? thisIcon.substr(thisIcon.indexOf(';')+1, thisIcon.length) : thisIcon.substr(0, thisIcon.indexOf(';')) ) + ')'); 

       if (nodevisible) {
         if (animate && hmAnimate) $(thisUL).slideDown('fast'); 
         else thisUL.style.display = 'block';
       }
       else {
         if (animate && hmAnimate) $(thisUL).slideUp('fast'); 
         else thisUL.style.display = 'none';
       }
    }
  }
}

function hilightexpand(spanID) {
    if (hilight(spanID)) {
      var thisUL = hmULfromID(spanID);
      if (thisUL) hmSwitchNode(thisUL, true, true);
      return true;
    }
    else return false; 
}

function hilight(spanID) {
    tocselecting = true;
    var thisnode = null;
    var selectionchanged = false;
    thisnode = hmNavigationFrame().document.getElementById(spanID);
    if (thisnode) {
       try {
          if ((currentselection) && (currentselection != thisnode)) currentselection.className = "heading" + currentselection.className.substr(7,1);
       }
       catch(e){
       }
       thisnode.className = "hilight"+thisnode.className.substr(7,1);
       selectionchanged = (currentselection != thisnode);
       currentselection = thisnode;
    }
    return selectionchanged;
}

function intoview(thisnode, tree, selectionchanged) {
    var thisparent = thisnode;
    while (thisparent != tree) {
       if ((selectionchanged) && (thisparent.nodeName.toLowerCase()=="ul")) hmSwitchNode(thisparent,true,true);
       thisparent = thisparent.parentNode;
    }
    thisparent = thisnode;
    for (var t=0; thisparent!=null; t+=thisparent.offsetTop, thisparent=thisparent.offsetParent);
    var bt = (hmNavigationFrame().window.pageYOffset)?hmNavigationFrame().window.pageYOffset:hmNavigationFrame().document.body.scrollTop;
    var bh = (hmNavigationFrame().window.innerHeight)?hmNavigationFrame().window.innerHeight:hmNavigationFrame().document.body.offsetHeight;
    if ((t+thisnode.offsetHeight-bt) > bh) hmNavigationFrame().window.scrollTo(0,(t+24-bh))
    else if (t < bt) hmNavigationFrame().window.scrollTo(0,t);              
}

function collapseunfocused(toc, selectedID) {
    if (toc) {
       var nodepath = 'ul'+selectedID.replace(/[isaul]/g,'') + ".";
       var nodeCompare = "";
       var items = toc.getElementsByTagName("ul");
       for (var i = 0; i < items.length; i++) {
         if (items[i].id.indexOf(".")<0) nodeCompare = items[i].id + ".";
         else nodeCompare = items[i].id;
         if (nodepath.lastIndexOf(nodeCompare)<0) {
           hmSwitchNode(items[i], false, false);
         }
       }
    }
}

function quicksync(aID) {
    if (aID != "") {
       var toc = hmNavigationFrame().document.getElementById("toc");
       if (toc) {
          if (!tocselecting) {
          	 aID = "s"+aID.substring(1,aID.length);
             var thisspan = hmNavigationFrame().document.getElementById(aID);
             if (thisspan) {
                var selectionchanged = hilight(aID);
                intoview(thisspan, toc, selectionchanged);
             }
          }
          if (autocollapse) {
             if (currentselection) collapseunfocused(toc, currentselection.id);
             else collapseunfocused(toc, "");
          }
       }
       track('topic', topicID);
    }
    tocselecting = false;
}

function lazysync(topicID) {
    if (topicID != "") {
       var toc = hmNavigationFrame().document.getElementById("toc");
       if (toc) {
          if (!tocselecting) {
            var currentTopic = $("a[href^='"+topicID+"']",toc);
            if (currentTopic.length > 0) {
              var currentSpanID = $(currentTopic).children("span").attr("id");
              var selectionchanged = false;
              if (hmTocSingleClick) {
                selectionchanged = hilightexpand(currentSpanID);
              }
              else { 
                selectionchanged = hilight(currentSpanID);
              }
              intoview(currentTopic[0], toc, selectionchanged);
            }
          }
          if (autocollapse) {
             if (currentselection) collapseunfocused(toc, currentselection.id);
             else collapseunfocused(toc, "");
          }
       }
       track('topic', topicID);
    }
    tocselecting = false;
}

function hmPreloadIcons() {
    var icons = new Array();
    for (i=0; i<arguments.length; i++) {
       icons[i] = new Image();
       icons[i].src = arguments[i];
    }
}

function hmCreateVSplitter(leftdiv, rightdiv) {
    var splitWidth = ($(rightdiv).offset().left - ($(leftdiv).offset().left+$(leftdiv).outerWidth()));
    $('body').append('<div id="hmsplitter" style="border:none;margin:0;padding:0;position:absolute;cursor:col-resize;background-color:transparent;overflow:hidden;'+
                     'height:'+$(leftdiv).outerHeight()+
                     'px;top:'+$(leftdiv).offset().top+
                     'px;left:'+($(leftdiv).offset().left+$(leftdiv).outerWidth())+
                     'px;width:'+splitWidth+'px">&nbsp;</div>');

	var oldX;
	var	navWidth;
	var	minWidth;
	var	oldLeft;
	var	oldWidth;
	var oldSplitL;
    var split = $('#hmsplitter');

	$(window).resize(function() {  /* resize splitter when window changes */
      split.css({'height': $(leftdiv).outerHeight()+'px', 'left': ($(leftdiv).offset().left+$(leftdiv).outerWidth())+'px'}); 
	}); 

    split.bind('mousedown', startDrag); 
    
    function startDrag(e) {
		oldX = (!document.all) ? e.clientX : event.clientX;
	  	navWidth = parseInt($(leftdiv).css('width'),10);
		minWidth = parseInt($(leftdiv).css('min-width'));
		oldLeft  = $(rightdiv).offset().left;
		oldWidth = $(rightdiv).outerWidth();
		oldSplitL = split.offset().left;
		
        var bg = $('<div id="hmcurtain" style="border:none;padding:0;margin:0;position:absolute;cursor:col-resize;width:100%;height:100%;background-color:transparent"></div>').appendTo('body');
		bg.bind('mousemove', performDrag); 
		bg.bind('mouseup', endDrag);
		split.bind('mousemove', performDrag); 
		split.bind('mouseup', endDrag);
		
		function endDrag() {
		  bg.unbind('mousemove').unbind('mouseup').remove();
		  split.unbind('mousemove').unbind('mouseup');
		}
	}
	
	function performDrag(e) {
    	//deselect:
		if (window.getSelection) window.getSelection().removeAllRanges();
		else if (document.selection) document.selection.empty();

		var moveX = (!document.all) ? e.clientX - oldX : event.clientX - oldX;
		var newNavW = navWidth + moveX < minWidth ? minWidth : navWidth + moveX;
		$(leftdiv).css('width', (newNavW) + 'px');
		//split.css('left', ($(leftdiv).offset().left+newNavW) + 'px');
		split.css('left', (oldSplitL + newNavW - navWidth) + 'px');

		$(rightdiv).css('left', (oldLeft + newNavW - navWidth) + 'px');
		if (!hmSupportsAbspos()) $(rightdiv).css('width', (oldWidth - newNavW + navWidth) + 'px'); 
	}
	
}
