// Project information

hmProjectInfo = {
	title: "SAM Help",
	author: "National Renewable Energy Laboratory",
	copyright: "© 2024 National Renewable Energy Laboratory",
	summary: "&nbsp;",
	version: "0.0.1",
	date: "Monday, December 9, 2024",
	language: "en-us",
	mainfile: "index.html"
	}

// Handler for post-loading functions from files
	var hmWebHelp = {}, hmxtoggle = true;
	hmWebHelp.extFuncs = function(func, args) {	
	
			var newScript = "";
		if (typeof hmWebHelp.funcs != "object") hmWebHelp.funcs = {};
		
		if (typeof hmWebHelp.funcs[func] == "function") {
			hmWebHelp.funcs[func](args);
		} else {
			// Get name of script and load it
			newScript= "./js/" + func + ".js";
			$.getScript(newScript).done(function (script, textStatus) {
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
			}).fail(function(jqxhr, settings, exception) { 
			console.log("failed: " + exception)
			});
		}
		};
	
	// Cleanup to be performed when embedded topic exits
	
	hmCleanupTopic = function() {
		
		// Kill any video iframes and objects to prevent hangovers and crashes
		$("iframe").attr("src","");
		
		// Kill regular videos
		var $videoBits = $("object,embed,param");
		if ($videoBits.length > 0) {
			// In IE the only a reload gets rid of the buffered video object
			if (/trident|edge/i.test(window.navigator.userAgent)) {
				document.location.reload();
				$videoBits.attr("data","").attr("src","").attr("value","").remove();
			}
			else {
				$videoBits.attr("data","").attr("src","").attr("value","").remove();
			}
		}
		
		// Kill any image toggle boxes
		$("div#imagetogglebox").remove();

		// Remove the code
		$("div#hmxpopupbody").html("");
		
	}
	
	// Tracking not supported for embedded topics
	function HMTrackTopiclink() {
		return false;
	}
	
	// Global load function to execute from the JSON file for remote connections
	hmLoadTopic = function(popObj) {

			$(window).on("beforeunload", function(){
				hmCleanupTopic();
			});
			var $popupscroller = $("body"),
				popupheader = '<h1 class="p_Heading1 xpageheader"><span class="f_Heading1">'+popObj.hmTitle+'</span></h1>';
				$("div#hmxpopupbody").html(popupheader + popObj.hmBody);
				$("div#hmpopuptitle > p").html(popObj.hmTitle);
	}

	hmInitContents = function() {
	
	// Topic and web links on the page
	$("div#hmxpopupbody").on(
	"click", 
	"a.topiclink,a.weblink,a.topichotspot,a.webhotspot",
	function(event){
		$(this).attr("target","_blank");
		if ($(this).attr("class").indexOf("topiclink") > -1)
			$(this).attr("href",$(this).attr("href").replace(/\#/,"?anchor="));
		});
	
	$textpopup = $("div#textpopup");
	$textpopup.attr("title","Click outside popup to close. Popups in field-level topics are plain text only.").attr("aria-label","Click outside popup to close. Popups in field-level topics are plain text only.");
	
	var hmClosePopup = function(reset) {
		if (reset)
			$textpopup.html("").attr("style","");
		else
			$textpopup.fadeOut("fast", function(){
			$textpopup.html("").attr("style","");
			});
	};
	
	$textpopup.on("click", function(event){
		event.stopPropagation();
	});
	
	$(document).on("click", function(){
		if ($textpopup.is(":visible"))
			hmClosePopup(false);
	});
	
	// Popup links on the page
	$("div#hmxpopupbody").on(
	"click", 
	"a.popuplink,a.popuphotspot,a.topichotspot[href^='javascript:void']",
	function(event){
		event.preventDefault();
		var target = $(this).attr("data-target");
		hmWebHelp.popX = event.clientX;
		hmWebHelp.popY = event.clientY;
		$.getScript("./jspopups/" + target, function(data, textStatus, jqxhr) {
		});
	});
		
		
	// Dropdown Text Toggles
	
	$("div#hmxpopupbody a.dropdown-toggle").on(
	"click", 
	function(event){
		event.preventDefault();
		event.stopPropagation();
		var toggleArgs = {method: "HMToggle", obj: $(this)};
		hmWebHelp.extFuncs("hmDoToggle",toggleArgs);
	});
	
	$("div#hmxpopupbody img.dropdown-toggle-icon").on(
	"click", 
	function(event){
		event.preventDefault();
		var toggleArgs = {method: "HMToggleIcon", obj: $(this)};
		hmWebHelp.extFuncs("hmDoToggle",toggleArgs);
	});
	
	// Inline Text Toggles
	
	$("div#hmxpopupbody a.inline-toggle").on(
	"click",
	function(event){
	event.preventDefault();
	hmWebHelp.extFuncs("hmDoInlineToggle",$(this));
	});
	
	// Image Toggles

	$("a.imagetogglelink").on("click",
	function(event){
		event.preventDefault();
		});
	$("div#hmxpopupbody").on(
	"click",
	"img.image-toggle, svg.image-toggle-magnifier",
	function(event){
	event.preventDefault();
	var $img = $(this).parent().find("img").first();
	hmWebHelp.extFuncs("hmImageToggle",$img);
	});
	
	// Video lightboxes 
	
	$("div#hmxpopupbody").on(
	"click",
	"div.video-lightbox",
	function(event){
		event.preventDefault();
		event.stopPropagation();
		alert("Video lightboxes are not supported in field-level mode. You need to open this page in the main help to view this video.");
	});
	}
	
	$(document).ready(function(){
	if (document.location.search.length > 7 && /\.js$/im.test(document.location.search)) {
	
	var topicJSFile = "./jstopics/" + document.location.search.substr(1);
	$.getScript(topicJSFile)
		.done(function( script, textStatus ) {
			hmInitContents();
		})
	  .fail(function( jqxhr, settings, exception ) {
	  alert("ERROR -- Topic with ID '" + document.location.search.substr(1,document.location.search.lastIndexOf("\.")-1) + "' not found.");
	  return;
		});
	} else {
		let loadCounter = 0, 
			loadTest = setInterval(function(){
				loadCounter++;
			if ($("div#hmxpopupbody p").length > 1) {
				clearInterval(loadTest);
				hmInitContents();
			} else if (loadCounter > 60) {
				clearInterval(loadTest);
				console.log("ERROR: Field level topic load error");
			}
		},50);
	} 

	// Global load popup function
	hmLoadPopup = function(popObj) {
		var textBody = popObj.hmBody, 
			wnheight = $(window).height(),
			wnwidth = $(window).width(),
			pheight, pwidth,
			spaceabove, spacebelow,
			spaceright, spaceleft,
			fixdims;
		textBody = textBody.replace(/<p.*?>(.*?)<\/p>/ig, "\[\[\$\$\$\]\]$1\[\[\%\%\]\]");
		textBody = textBody.replace(/<\/??.*?>/ig, "");
		textBody = textBody.replace(/\[\[\$\$]](.*?)\[\[\%\%]]/ig, '<p class="ppara">$1</p>');
		hmClosePopup(true);
		$textpopup.html(textBody).show();
		
		// Resize popup
		if ($textpopup.width() > $textpopup.height()) {
			fixdims = ($textpopup.width() + $textpopup.height()) / 2;
			$textpopup.css({"width": fixdims + "px"});
			if ($textpopup.height() > (wnheight - 20))
				$textpopup.css({"width": "95%"});
			}
		// Position poupup
		pwidth = $textpopup.width();
		pheight = $textpopup.height();
		
		// Vertical
		spaceabove = hmWebHelp.popY - 50;
		spacebelow = wnheight - (hmWebHelp.popY);
		if (spaceabove > pheight + 15)
			$textpopup.css("top", (hmWebHelp.popY - (pheight + 12)) + "px");
		else if (spacebelow > pheight + 15)
			$textpopup.css("top", (hmWebHelp.popY + 10) + "px");
		else 
			$textpopup.css("top", "2em");
		
		// Horizontal
		spaceright = wnwidth - (hmWebHelp.popX);
		spaceleft = hmWebHelp.popX;
		
		if (spaceright > pwidth + 15)
			$textpopup.css("left", (hmWebHelp.popX + 12) + "px");
		else 
			$textpopup.css("left", (wnwidth - pwidth + 12) + "px");
	};

		
	});
