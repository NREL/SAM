/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/
var HMImageToggle = function($img){
	var	$imgbox, $captionbox = false,
		imgID = $img.attr("id"),
		src0 = $img.attr("data-src0"),
		src1 = $img.attr("data-src1"),
		istate = $img.attr("data-state"),
		title0 = $img.attr("data-title0") ? $img.attr("data-title0") : null,
		title1 = $img.attr("data-title1") ? $img.attr("data-title1") : null,
		caption0 = $img.attr("data-caption0") ? $img.attr("data-caption0") : null,
		caption1 = $img.attr("data-caption1") ? $img.attr("data-caption1") : null,
		captionbox = caption0 ? $img.parents("div")[0] : null,
		startLeft = $img.offset().left,
		startTop = $img.offset().top,
		closeddims = {},
		maxdims = {},
		windowdims = {},
		maxImageHeight,
		vBorderWidth = 0,
		newImage = new Image();
		
		windowdims.w = $(window).width() - 30;
		windowdims.h = $(window).height() - 30;
		closeddims.w = $img.width();
		closeddims.h = $img.height();
		maxImageHeight = windowdims.h;
		hFactor = closeddims.h/closeddims.w;

	// No Caption
	if (!caption1) {
			$("body").append('<div id="imagetogglebox"></div>');
		} else {
			$("body").append('<div id="imagetogglebox"><div id="imagecaptionbox"><p class="zoomedcaption">'+caption1+'</p></div></div>');
			$captionbox = $("div#imagecaptionbox");
		}
	
	$imgbox = $("div#imagetogglebox");
	$(newImage).css({"width": "100%", "height": "auto"});

		// Close image toggle
		function unClicker (elem) {
			
			
			// Click inside element
			$(elem).on((typeof hmBrowser == "undefined" ? "click" : hmBrowser.touchstart) + '.closemenu', function(){
				closeImage();
				$(document).off(".closemenu");
				$(elem).off(".closemenu");
			});
			
			
			// ESC key
			$(document).on("keydown.closemenu", function(event){
				let x = event.which || event.keyCode;
				if (x == 27) {
					closeImage();
					$(document).off(".closemenu");
					$(elem).off(".closemenu");
				}
			});
		}
		
		function closeImage(event,anispeed) {
			var speed = (typeof anispeed == "undefined") ? "fast" : anispeed;
			$imgbox.animate({width: closeddims.w, height: closeddims.h, top: startTop, left: startLeft},speed, function(){
				$imgbox.hide();
				$imgbox.remove();
				$img.attr("data-state","0");
				// Only reset if we're not in a field-level topic
				if (typeof hmxtoggle == "undefined")
				$("body,html").css("overflow","hidden");
			});
		}
		
		// Expose the function for external calls
		hmWebHelp.funcs.closeImageToggle = closeImage;
		
		function maximizeImage(event) {

			if ($imgbox.width() < maxdims.w) {
				if (maxdims.w > $(window).width() || maxdims.h > $(window).height()) {
				$("body,html").css("overflow","auto");
				}
				if ($captionbox)
					$captionbox.hide();
				$imgbox.animate({
					"top": 0,
					"left": 0,
					"width": maxdims.w,
					"height": maxdims.h
				},"fast", function(){
					$imgbox.css("position","absolute");
					$("div#imagezoom").hide();
				});
			} else {
				closeImage();
			}
		}
		
		function dynamicResize(dims, refdims) {
			while (dims.w > refdims.w || dims.h > refdims.h) {
				dims.w -= 5;
				dims.h = dims.w * hFactor;
			}
			return dims;
		}
		
		newImage.onload = function() {
			var opendims = {}, newTop, newLeft,
				hOffset = 0, cboxHeight = 0;
			maxdims.w = opendims.w = newImage.width;
			maxdims.h = opendims.h = newImage.height;
			
			
			opendims = dynamicResize(opendims,windowdims);
			$imgbox.append(newImage);
			$imgbox.append('<div id="imagezoom"><img id="imagezoomer" src="./images/ZoomIn.png" border="0"/></div>');

			unClicker(newImage);

			$imgbox.css({"left": startLeft + "px", "top": startTop + "px", "width": closeddims.w + "px", "height": closeddims.h + "px"});
			$imgbox.show();	
			vBorderWidth = parseFloat(getComputedStyle($imgbox[0]).getPropertyValue("border-top-width"),10);
			if ($captionbox) {
				$captionbox.css({
					"width": opendims.w + "px"
					});
				while (opendims.h >  maxImageHeight - ($captionbox.height()+10)) {
					opendims.w -= 5;
					opendims.h = opendims.w * hFactor;
				}
				cboxHeight = $captionbox.outerHeight();
				$captionbox.css({
					"width": ""
					});
				
			}
			newTop = ($(window).height() - (opendims.h + cboxHeight + vBorderWidth*2)) / 2;
			newLeft = ($(window).width() - (opendims.w + vBorderWidth*2)) / 2;
			if ($captionbox) {
				hOffset = cboxHeight+vBorderWidth*2;
			}
			if (maxdims.w > opendims.w) {
				$("div#imagezoom").on("click, keydown", function(event) {
					event.preventDefault();
					event.stopPropagation();
				}).on(hmBrowser.touchstart, maximizeImage).show();
			}
			$imgbox.animate({width: opendims.w, height: opendims.h + hOffset, top: newTop, left: newLeft },'fast',
			function() {
				$img.attr("data-state","1");
				if ($captionbox) {
				var imgBoxFinalHeight = $(newImage).height() + $captionbox.outerHeight();
				$imgbox.css("height",imgBoxFinalHeight + "px");
				}
			});
		};
		newImage.src = src1;
};
hmWebHelp.funcs.hmImageToggle = HMImageToggle;
