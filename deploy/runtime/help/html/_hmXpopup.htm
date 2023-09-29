<!DOCTYPE HTML>
<html>
	<head>
	<title>Popup Window Template</title>
	<meta http-equiv="X-UA-Compatible" content="IE=edge" />
	<meta name='viewport' content='width=device-width, initial-scale=1, minimal-ui'/>
	<meta http-equiv="Content-Type" content="text/html" charset="UTF-8" />
	<meta name="generator" content="Help+Manual" />
	<meta name="robots" content="noindex, nofollow" />
	<meta name="viewport" id="pageviewport" content="width=device-width, initial-scale=1, user-scalable=yes, minimal-ui" />
	<link id="pagestylesheet" type="text/css" href="./css/hmprojectstyles.css" rel="stylesheet" />
	<link id="customstylesheet" type="text/css" href="./css/custom_topic_styles.css" rel="stylesheet" />
	<script type="text/javascript" src="./js/jquery.js"></script>
	<script type="text/javascript" src="./js/xmessage.js"></script>
	<style type="text/css">

	
	html, body {
	position: absolute;
	top: 0; right: 0; bottom: 0; left: 0;
	overflow: hidden;
	}
	body, html {
	background-color: white;
	margin: 0 0 0 0; padding: 0.25em;
	overflow-x: auto;
	overflow-y: auto;
	-webkit-overflow-scrolling: touch;
	}
	
	div#hmxpopupbody  {
		margin: 0; padding: 5px 10px;
	}
	
</style> 
<script type="text/javascript">
	
// Set the window name for postMessage communication

	window.name = "hmpopupwindow";
	var parentdomain = document.referrer.replace(/^(https?:\/\/.*?)(?:\/.*?$|$)/i,"$1");
	
	$.ajaxPrefilter( "json script", function(options) {options.crossDomain = true;});
	xMessage = new xMsg("EMBED POPUP");

	// Cleanup function when popup is closed
	var hmCleanupPopup = function() {
		// Remove the code
		$("div#hmxpopupbody").html("");
	}

	// Initialize and sanitize the popup contents
	var hmInitContents = function() {
	
		// Open topic and web links on the page in a new tab/window
		$("div#hmxpopupbody").on(
		"click", 
		"a.topiclink,a.weblink,a.topichotspot,a.webhotspot",
		function(event){
			$(this).attr("target","_blank");
			if ($(this).attr("class").indexOf("topiclink") > -1)
				$(this).attr("href",$(this).attr("href").replace(/\#/,"?anchor="));
			});
			
		// Open popup links on the page in the current popup window
		$("div#hmxpopupbody").on(
		"click", 
		"a.popuplink,a.popuphotspot,a.topichotspot[href^='javascript:void']",
		function(event){
			event.preventDefault();
			var target = $(this).attr("data-target");
			if (target && typeof parent.window.hmXPopup === "object"){
				parent.window.hmXPopup.loadPopup(event, target);
				}
			else 
				alert("Invalid popup link!")
			});
	} 
	
	// Global load function to execute from the JSON file
	hmLoadPopup = function(popObj) {

			// Set the header text in the parent
			xMessage.sendObject("parent",{action: "callfunction", fn: "hmXPopup.setPopupName", fa: popObj.hmTitle, domain: parentdomain});

			$(window).on("beforeunload", function(){
				hmCleanupPopup();
			});
			var $popupscroller = $("body");
	
				$("div#hmxpopupbody").html(popObj.hmBody);
				hmInitContents();
			
				setTimeout(function() {
						// Figure out the 2 x 3 dimensions

						var pH = $("div#hmxpopupbody").height(),
							pW = $("div#hmxpopupbody").width(),
							pA = pH * pW,
							pF = Math.sqrt(pA/6);
							
							pH = Math.round(pF * 2) + 20;
							pW = Math.round(pF * 3) + 20;
							
							pW = pW < 180 ? 180 : pW;

							if ( $("table#poptable").length > 0 ) {
								pW = $("table#poptable").width() + 20;
								}
						xMessage.sendObject("parent",{action: "callfunction", fn: "hmXPopup.setContentDims", fa: pH + "," + pW, domain: parentdomain});
					},50);
	}	
	
	// No tracking in field-level popups
	function HMTrackTopiclink() {
		return;
	}
	
	
	$(document).ready(function(){
	
	if (document.location.search.length > 7 && /\.js$/im.test(document.location.search)) {

	var topicJSFile = "./jspopups/" + document.location.search.substr(1);
	
	$.getScript( topicJSFile )
		.done(function( script, textStatus ) {
		})
	  .fail(function( jqxhr, settings, exception ) {
	   alert("Popup file not found:\r\n" + document.location.protocol + "//" + document.location.hostname + document.location.pathname.substr(0,document.location.pathname.lastIndexOf("\/")) + topicJSFile.substr(1));
		});
	
	} else {
		hmInitContents();
	}
	
	});
	</script>
</head>
<body>
<div id="hmxpopupbody">
<p>Error: Popup content not loaded</p> 
</div> 
</body>
</html>
