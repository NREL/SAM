/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/
function videoBoxConstructor() {
	var hmmin = function(v1, v2) { if (v1<v2) return v1; return v2; };
	var hmmax = function (v1, v2) { if (v1>v2) return v1; return v2; };
	var HMShowLightbox = function(htmlCode, startL, startT, startW, startH, endW, endH, newCaption, doAnimate, isVideo) {
	  var lightboxWindow = $(window);
	  var lightboxDocument = $(document);
	  var lightboxBody = $('body');
	  var lighboxScrollLayer = null;
	  $(lightboxBody).prepend('<div id="hmlightboxbackground" style="z-index:99997;border:none;padding:0;margin:0;position:absolute;left:0;top:0;background-color:#7F7F7F"></div>');  
	  var lightboxBackground = $('#hmlightboxbackground');
	  lightboxBackground.css('opacity', '0.5');

		$(lightboxBody).prepend('<div id="hmlightboxscrolllayer" style="z-index:99998;border:none;padding:0;margin:0;position:absolute;left:0;top:0;background:none;overflow:visible;"></div>');
		lighboxScrollLayer = $('#hmlightboxscrolllayer');
		lightboxBody = lighboxScrollLayer;  

	  $(lightboxBody).prepend('<div id="hmlightbox" style="z-index:99999;position:absolute;display:none"></div>');
	  var lightbox = $('#hmlightbox');  
	  var lightboxObject = $(htmlCode).appendTo(lightbox);
	  lightboxObject.children("object").first().attr("id","hmVideoBox");
	  var lightboxCaption = null;
	  if (newCaption !== null) {
		$(lightbox).append('<p id="hmlightboxcaption">' +newCaption+ '</p>');
		lightboxCaption =  $('#hmlightboxcaption');
	  }  	

	  var lightboxSpeed = 300;
	  var sizeStart,sizeEnd; 
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
	  if (lightboxCaption!==null) lightboxCaption.css('width', endW+'px');
		
	  if (hmAnimate&&doAnimate) {
		sizeEnd = lightboxGetsize();
		if (lightboxCaption!==null) lightboxCaption.css('display', 'none'); /* hide caption during animation */
		lightboxObject.css({'width': startW + 'px', 'height': startH + 'px'});
		sizeStart = lightboxGetsize();
		lightboxResize();
			
		sizeStart[0] = startL;
		sizeStart[1] = startT;
		lightbox.css({'left': sizeStart[0]+'px', 'top': sizeStart[1]+'px'});
		lightbox.show();
			
		lightbox.animate({ left: sizeEnd[0]-lightboxObject.position().left, top: sizeEnd[1]-lightboxObject.position().top }, 
						   lightboxSpeed, 
						   function() { 
							 if (lightboxCaption!==null) lightboxCaption.css('display', 'block');
						   }
						 );
							 
		lightboxObject.animate({ width: endW, height: endH }, lightboxSpeed); 
	  }
	  else {
		sizeEnd = lightboxGetsize();
		lightboxResize();
		lightbox.show();  	
	  }  

	  $(lightboxWindow).on('resize.hmlightbox', lightboxResize);
	  $(lightboxWindow).on('scroll.hmlightbox', lightboxScroll);
	  $(lightboxBody).on('click.hmlightbox', lightboxClick);
	  $(document).on('keydown.hmlightbox', lightboxKeydown);
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
			if (lightboxCaption!==null) lightboxCaption.css('width', endW+'px');
		  }
		}

		var size = lightboxGetsize();
		lightbox.css({left: size[0]+'px', top:size[1]+'px'});
		
		if (lighboxScrollLayer!==null) { 
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
		var newL = hmmax(20, parseInt(($(lightboxWindow).width() - lbW)/2) + (lightboxDocument.scrollLeft()));
		var newT = hmmax(20, parseInt(($(lightboxWindow).height() - lbH)/2) + (lightboxDocument.scrollTop()));

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
		  if (lightboxCaption!==null) lightboxCaption.css('display', 'none'); /* hide caption during animation */
		  lightboxObject.animate({ width: 0, height: 0 }, lightboxSpeed); 
		  lightbox.animate({ left: startL + startW/2, top: startT + startH/2 }, 
						   lightboxSpeed, 
						   function() { 
							 $("object#hmVideoBox").remove();
							 lightbox.remove(); 
							 if (lighboxScrollLayer!==null) lighboxScrollLayer.remove(); 
							 lightboxBackground.remove();
						   }
						  );
		}
		else {
		  lightboxObject.css({ width: 0, height: 0 });
		  lightbox.remove();
		  if (lighboxScrollLayer!==null) lighboxScrollLayer.remove(); 
		  lightboxBackground.remove();
		}
		$(lightboxWindow).off('.hmlightbox');
		$(lightboxBody).off('.hmlightbox');
		$(document).off('.hmlightbox');
	}
	  
	}; // showLightbox
	
	var parseVBox = function(args) {
		
	var htmlcode = args.data,
		startL = args.$obj.offset().left,
		startT = args.$obj.offset().top,
		startW = args.$obj.outerWidth(),
		startH = args.$obj.outerHeight(),
		vWidth = args.vWidth,
		vHeight = args.vHeight;
		
		HMShowLightbox(htmlcode, startL, startT, startW, startH, vWidth, vHeight, '', true, true);
		
	};
	
	return parseVBox;
} // constructor

hmWebHelp.funcs.hmVideoBox = videoBoxConstructor();
