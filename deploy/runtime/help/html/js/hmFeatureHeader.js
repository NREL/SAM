/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/

// Constructor

function fh() {
	
	var ScrollbarWidth = (function() {
		var outer = document.createElement("div");
		outer.style.visibility = "hidden";
		outer.style.width = "100px";
		outer.style.msOverflowStyle = "scrollbar"; // needed for WinJS apps

		document.body.appendChild(outer);

		var widthNoScroll = outer.offsetWidth;
		// force scrollbars
		outer.style.overflow = "scroll";

		// add innerdiv
		var inner = document.createElement("div");
		inner.style.width = "100%";
		outer.appendChild(inner);        

		var widthWithScroll = inner.offsetWidth;

		// remove divs
		outer.parentNode.removeChild(outer);

		return widthNoScroll - widthWithScroll;
	})();
	var self = this,
		$pscroller,
		$fhead,
		dEvent = hmDevice.tablet ? hmBrowser.touchstart : "mouseover",
		hh,
		$fp,
		sb = hmDevice.tablet ? 0 : ScrollbarWidth,
		imwidth,
		imheight,
		imheightP,
		imheightL,
		imaspect;
	
	self.fhHeight = function() {

		var wh = $(window).height(),
			toffset = hmDevice.tablet ? 10 : 10;

		imheightP = $fhead.width() * imaspect;
		imheightL = wh * imaspect;

		hh = $fp.height();
		
		wh = Math.round(wh/3);
		hh = hh > imheightP ? hh : imheightP;
		hh = hh > wh ? wh : hh;
				
		$pscroller.css({"padding-top": (hh+toffset) + "px"});
		$fhead.css({"height": hh + "px"});
			};

	self.fhInit = function() {

		if ($("div#featureheader").length < 1)
			$("div#hmpagebody").prepend('<div id="featureheader" title="Click to show/hide description"><p id="featuretext"></p></div>');
		$fhead = $("div#featureheader");
		$pscroller = $('div#hmpagebody_scroller');
		$fp = $("p#featuretext");
		self.image = new Image();
		self.image.src = hmpage.hmPicture;
		$fhead.css("background-image","url('"+hmpage.hmPicture+"')");
		$fp.html(hmpage.hmDescription);
		
		setTimeout(function(){
			
			
		// It can take a while to get the image dimensions on first load
		var imDimCount = 0;
		var getImDims = setInterval(function(){
			imwidth = self.image.width;
			imheight = self.image.height;
			imaspect = imheight/imwidth;
			imDimCount++;
			if (imwidth > 0 || imDimCount > 50) {
			self.fhHeight();
			clearInterval(getImDims);
			}
		},50);
			
		$fhead.css("border-bottom","2px solid #bababa");

		$(window).off('resize.features').on('resize.features', self.fhHeight);
		
		$pscroller.off('scroll.features').on('scroll.features', function() {
			var y = $pscroller.scrollTop();
			$fhead.css({'height': (hh-y) + "px"});
			if (hh-y < 3 )
				 $fhead.css("border-bottom","0");
			else 
				$fhead.css("border-bottom","2px solid #bababa");
			}); 
			
			// Hide description bar on touch or click
			$fhead.off(hmBrowser.touchstart).on(hmBrowser.touchstart, function(){
				if ($fp.is(":visible")) {
					$fp.fadeOut("fast");
				} else {
					$fp.fadeIn("fast");
				}
			});
			
		},100); // Image timeout
		
	};
	

	
	var doFH = function(mode) {
		
		if (typeof mode == "undefined")
			self.fhInit();
		else {
			
		switch(mode) {
			case "init":
			self.fhInit();
			break;
			case "resize":
			self.fhHeight();
			break;
		}
			
		}

	};
	return doFH;
}

hmWebHelp.funcs.hmFeatureHeader = new fh();			
