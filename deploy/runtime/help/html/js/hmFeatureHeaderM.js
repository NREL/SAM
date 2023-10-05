/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/

// Constructor

function fh() {

	var image,
		$fhead,
		$pscroller,
		$pheader,
		$pbody,
		$fp,
		imwidth,
		imheight,
		imaspect,
		imheightL,
		imheightP,
		hh,
		hht;
			
	doFeatureHeight = function() {

		var wh = $(window).height();

		imheightP = $fhead.width() * imaspect;
		imheightL = wh * imaspect;

		hh = $fp.height();
		hht = $pheader.height();
			
		if (hmBrowser.Flandscape()) {
			wh = wh/2;
			hh = hh > imheightL ? hh : imheightL;
		} else {
			wh = wh/2;
			hh = hh > imheightP ? hh : imheightP;
		}
			
		hh = hh > wh ? wh : hh;
		
	  $pscroller.css({"padding-top": hh + "px"});
		$fhead.css({"top": ($pheader.is(":visible") ? hht : 0) + "px", "border-bottom": "2px solid #bababa", "height": hh + "px"});
	};
			
	init = function() {
		if ($("div#featureheader").length < 1)
			$("div#topicbody").prepend('<div id="featureheader"><p id="featuretext"></p></div>');
		$fhead = $("div#featureheader");
		$pscroller = $('main#topicbox');
		$pheader = $('div#headerbox');
		$pbody = $("div#hmpagebody");
		$fp = $("p#featuretext");
		image = new Image();
		image.src = hmpage.hmPicture;
		$fhead.css("background-image","url('"+hmpage.hmPicture+"')");
		$fp.html(hmpage.hmDescription);
	

	setTimeout(function(){
		
		// It can take a while to get the image dimensions on first load
		var imDimCount = 0;
		var getImDims = setInterval(function(){
			imwidth = image.width;
			imheight = image.height;
			imaspect = imheight/imwidth;
			imDimCount++;
			if (imwidth > 0 || imDimCount > 50) {
			doFeatureHeight();
			clearInterval(getImDims);
			}
		},50);
		
	 $(window).off('orientationchange.features').on('orientationchange.features', function() {
			setTimeout(function(){
				doFeatureHeight();
			},100);
		});
	  $pscroller.off('scroll.features').on('scroll.features', function() {
		        var y = $pscroller.scrollTop(),
					newhh = hh-y;
					newhh = newhh <= hh ? newhh : hh;
					if (newhh < 1) newhh = 1;
          		$fhead.css({'height': newhh + "px"});
				if (newhh < 3 )
					 $fhead.css("border-bottom","0");
				else 
					$fhead.css("border-bottom","2px solid #bababa");
				
			});
			
	$fhead.on("selectstart", function(){return false;});

	$fhead.off(hmBrowser.touchstart).on(hmBrowser.touchstart, function(){
				if ($fp.is(":visible")) {
					$fp.fadeOut("fast");
				} else {
					$fp.fadeIn("fast");
				}
			});
	},100);
	}; // Init
	
	var doFeatureImage = function(mode) {
		if (!mode) mode = "init";
		switch(mode) {
			case "init":
			init();
			break;
			case "resize":
			doFeatureHeight();
			break;
		}
	};
	
	return doFeatureImage;
}

hmWebHelp.funcs.hmFeatureHeaderM = new fh();			
