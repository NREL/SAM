/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/
	var hmDevice = {};

	hmDevice.agent = navigator.userAgent.toLowerCase();
	hmDevice.platform = navigator.platform.toLowerCase();
	hmDevice.touch = /touch/.test(hmDevice.agent);
	hmDevice.mb = /mobi|mobile/.test(hmDevice.agent);
	hmDevice.ipad = /ipad/.test(hmDevice.platform);
	hmDevice.iphone = /iphone/.test(hmDevice.platform);
	hmDevice.goodandroid = (/android.+?applewebkit\/(?:(?:537\.(?:3[6-9]|[4-9][0-9]))|(?:53[8-9]\.[0-9][0-9])|(?:54[0-9]\.[0-9][0-9]))|android.+?gecko\/[345][0-9]\.\d{1,2} firefox/.test(hmDevice.agent));
	hmDevice.deadandroid = (/android.+?applewebkit\/(?:53[0-6]\.\d{1,2})|firefox\/[0-2]\d\.\d{1,2}/.test(hmDevice.agent));
	hmDevice.android = (/android/.test(hmDevice.agent) && !hmDevice.deadandroid);
	hmDevice.w8desktop = (/windows nt 6\.[2345]/m.test(hmDevice.agent));
	hmDevice.w8metro = (function(){var supported = true; 
				try {new ActiveXObject("htmlfile");} catch(e) {supported = false;} 
				return (!supported && hmDevice.w8desktop);})();
	hmDevice.tb = (/tablet/.test(hmDevice.agent) && (!/trident/.test(hmDevice.agent) || (hmDevice.w8metro && hmDevice.touch)));
	hmDevice.phone = (hmDevice.mb && !hmDevice.ipad && !hmDevice.tb);
	hmDevice.tablet = (hmDevice.ipad || hmDevice.tb || (!hmDevice.phone && hmDevice.android));
	hmDevice.desktop = ((!hmDevice.tablet && !hmDevice.phone));
	hmDevice.device = hmDevice.phone ? "phone" : hmDevice.tablet ? "tablet" : hmDevice.desktop ? "desktop" : "default";
	hmDevice.ppversion = 4.1;

	var hmHelpVars = {
		
		defaultState: jQuery("div#helpwrapper").is(":visible"),
		hmPopupPath: "",
		hmAllowedDomains: "",
		hmDefaultTopic: "index.html",
		hmDefaultCallback: undefined,
		localdomain: document.location.protocol + "\/\/" + document.location.hostname,
		targetdomain: "",
		remoteAccess: false,
		$pageLinks: {},
		hmCallbacks: {},
		currentHelpPage: "",
		topicChanged: false,
		zoomOpening: false,
		embeddedBorder: true,

		//File exists check for local projects
		FileExists: function(url) {
			if (/\?/i.test(url)) {
				url = url.substr(0,url.indexOf("\?"));
			}
			var http = new XMLHttpRequest();
			http.open('HEAD', url, false);
			http.send();
			return http.status!=404;
			},
		
		// Is the help window open?
		hmHelpOpen: function() {
			return jQuery("div#helpwrapper").is(":visible")
			},
		
		// Path for use as object property 
		escapePathName: function(p) {
			return  p.replace(/[ -\._~:\/\?#\[\]@!\$&'\(\)\*\+,;=<>%{}\|\\\^`]/ig, "");
		},
		
		// Get full path from relative
		getFullPath: function(path) {
			if (/^[.\/]$/i.test(path.substr(0,1))) {
			let testa = document.createElement("a");
				testa.href = path;
				path = testa.href;
				}
			return path;
		},
		
		// Get path without filename
		getPath: function(path) {
			path = this.getFullPath(path);
			path = path.substr(0,path.lastIndexOf("\/")+1);
			return path;
		},
		
		// Extract domain from a path
		getDomain: function(path) {
			path = this.getFullPath(path);
			let domainmatch = /((?:^https?:\/\/|^\/\/).*?)\//i,
				match = domainmatch.exec(path);
			return(match[1]);
		},
		
		// Downcase an HTML filename that may have parameters
		downCaseFilename: function(filename) {
			if (filename.indexOf("\?") > 0) {
				let firstHalf = filename.substr(0,filename.indexOf("\?")+1).toLowerCase();
				filename = firstHalf + filename.substr(filename.indexOf("\?")+1);
			} else {
				filename = filename.toLowerCase();
			}
			return filename;
		},
		
		// Sanitize path specifications with trailing slash
		slashIt: function(p) {
		if (p.substr(p.length-1) != "\/") {
			return p + "/";
			} else {
				return p;
				}
		},
		
		// Save callback functions with sanitized names
		saveCallback: function(f,p) {
			let confirmation = false;
			if (typeof f == "function") {
				p = hmHelpVars.escapePathName(p);
			if (!hmHelpVars.hmCallbacks.hasOwnProperty(p)) {
				hmHelpVars.hmCallbacks[p] = f;
				confirmation = true;
				}
			}
			return confirmation;
		},
		
		// Update open/close embedded help buttons and links
		updatePageLinks: function(path, topic) {
		
		if (hmDevice.device != "desktop") return;
			
			hmHelpVars.$pageLinks = jQuery(".hmHelpToggle");
			 
			if (hmHelpVars.hmHelpOpen()) {
				hmHelpVars.$pageLinks.html("Hide Embedded Help");
			} else {
				hmHelpVars.$pageLinks.html("Show Embedded Help");
			}
		},

		// Alternative opener on phones and small tablets
		mobileHelp: function(path, topic, hmFunction) {
			
			if (typeof hmHelpWindow != "undefined" && hmHelpWindow !== null) {
				hmHelpWindow.close();
				hmHelpWindow = null;
			} 
			
			hmHelpWindow = window.open(path + topic,"hm_mobilehelp","",false);
			
			if (typeof hmFunction == "function")
				setTimeout(function(){
					hmFunction("mobile");
				},500);
		}
		
	}, // HmHelpVars
	
	// Open/Close the help
	hmToggleHelp = function(md) {
	
		var mode = md,
			helpopen = jQuery('div#helpwrapper').is(":visible");
		
		if (typeof md == "undefined") {
			mode = "auto";
		}
		
		function doCallback(mode) {
			
			var cbName = jQuery("iframe#hmhelp").attr("src");
				cbName = cbName.substr(0,cbName.lastIndexOf("/")+1);
				cbName = hmHelpVars.escapePathName(cbName);
				
				if (hmHelpVars.hmCallbacks.hasOwnProperty(cbName)) {
					try  {
						hmHelpVars.hmCallbacks[cbName](mode);
					} catch(error) {
						console.log("Callback function error:" + error);
					}
				}
		}
		
		function doPageLinks() {
			
			if (hmDevice.device != "desktop") return;
			
			var frameSource = jQuery("iframe#hmhelp").attr("src"),
				thisPath = frameSource.substr(0,frameSource.lastIndexOf("/")+1),
				thisTopic = frameSource.substr(frameSource.lastIndexOf("/")+1);
		
				hmHelpVars.updatePageLinks(thisPath,thisTopic);
			}

		if (mode == "auto") {
			if (!helpopen) {
				jQuery('div#helpwrapper').hide().css('visibility','visible').fadeIn(400, function(){
				doPageLinks();
				doCallback("open");	
				let currentPath = jQuery("iframe#hmhelp").attr("src");
				currentPath = currentPath.substr(0,currentPath.lastIndexOf("/")+1);
				jQuery('div#helpcloser').attr("onclick","HMshowHelpInstance(\'" + currentPath + "\')");
			});
			} else {
				jQuery('div#helpwrapper').fadeOut(400, function(){
				jQuery(this).css('visibility','hidden').css("display","none");
				doPageLinks();
				doCallback("close");
			});
			}
		} else if (mode == "close") {
				jQuery('div#helpwrapper').fadeOut(400, function(){
				jQuery(this).css('visibility','hidden').css("display","none");
				doPageLinks();
				doCallback("close");
			});
		} else if (mode == "open") {
				jQuery('div#helpwrapper').hide().css('visibility','visible').fadeIn(400, function(){
				doPageLinks();
				doCallback("open");	
			});
		}
	}

	// Help object constructor
		hmH = function(helppath, helptopic, cbfunction) {

		helppath = hmHelpVars.slashIt(helppath);
		jQuery('div#helpcloser').attr("onclick","HMshowHelpInstance(\'" + helppath + "\')");
		
		this.showhelp = false;
		this.startupOn = jQuery("div#helpwrapper").is(":visible");
		this.firstLoad = true;
		
		var helpTopic = helptopic, 
			helpPath = helppath,
			callbackFunction = cbfunction,
			desktopMode = hmDevice.device == "desktop",
			helpUrlTopic,
			topicAnchor = "",
			helpLoaded = false,
			helpwrapper = document.getElementById("helpwrapper"),
			helpbutton = document.getElementById("hmHelpButton"),
			showhelpTemp = false,
			hmHelpWindow = null;		
		
		// Expose helpwrapper 
		this.helpWrapper = helpwrapper;
		
		this.showhelp = showhelpTemp;
		
		this.showHelp = function(hmTopic, hmFunction) {
		
		var tempTopic = "", hmPath = "", uParam = false, newTarget = "", frameSrc = "", thisPath, thisTopic;
		
		if (typeof hmTopic == "object") {
			tempTopic = hmHelpVars.downCaseFilename(hmTopic.topic);
			hmPath = hmHelpVars.getFullPath(hmTopic.path);
			hmFunction = typeof hmTopic.func == "undefined" ? undefined : hmTopic.func;
			hmHelpVars.remoteAccess = hmHelpVars.getDomain(hmPath) != hmHelpVars.localdomain;
		} else if (typeof hmTopic == "string" && hmTopic != "") {
			hmPath = hmHelpVars.getFullPath(hmHelpVars.hmPopupPath);
			hmTopic = hmHelpVars.downCaseFilename(hmTopic);
		}

		if (tempTopic != "") hmTopic = tempTopic;
		
		let activeFrame = jQuery("iframe#hmhelp"),
			activeSrc = activeFrame.attr("src");
			
		if (typeof activeSrc == "undefined") {
			activeSrc = "";
			}

		hmHelpVars.saveCallback(hmFunction,(hmPath == "" ? hmHelpVars.hmPopupPath : hmPath));
			
		if (desktopMode && activeFrame.length != 0 && typeof hmTopic != "undefined") {

			let currentDomain = activeSrc != "" ? hmHelpVars.getDomain(activeFrame.attr("src")) : "";
			
			if (hmPath == "") {
			if (currentDomain != hmHelpVars.targetdomain) {
				HMShowThisHelp(hmHelpVars.hmPopupPath, hmTopic);
				return;
				} 
			} else if (currentDomain != hmHelpVars.getDomain(hmPath)) {
				HMShowThisHelp(hmPath, hmTopic);
				return;
				}
		}

		hmTopic = typeof hmTopic != "undefined" ? hmTopic.toLowerCase() : undefined;

		if (!desktopMode) {
			
			if (hmPath == "") {
				hmPath = hmHelpVars.hmPopupPath;
			} 

			if (typeof hmTopic !== "undefined" && hmTopic !== null) {
				hmHelpVars.mobileHelp(hmPath, hmTopic, hmFunction);
			}
			
			// If a topic was specified in the URL the standard opener will open that topic the first time if no other topic is called with showHelp()
			
			else if (typeof helpUrlTopic !== "undefined") {	
				hmHelpVars.mobileHelp(hmPath + helpUrlTopic, hmFunction);
				helpUrlTopic = undefined;
			} else {
				hmHelpVars.mobileHelp(hmPath, helpTopic, hmFunction);
				}

			return;
		}
		
		if (!helpLoaded) {

		if (hmPath != "") {
			frameSrc = hmPath + hmTopic;
		}
		else if (typeof hmTopic != "undefined" && typeof helpUrlTopic == "undefined" && hmTopic !== null) {
				frameSrc = helpPath + hmTopic;
				helpTopic = hmTopic;
		} 
		
		else if (typeof helpUrlTopic != "undefined") {
			frameSrc = helpPath + helpUrlTopic;
			helpTopic = helpUrlTopic;
		} else {
			frameSrc = helpPath + helpTopic;
		}
		
		if (jQuery("iframe#hmhelp").length > 0) {
			jQuery("iframe#hmhelp").attr("src",frameSrc);
			} else {
				
		// Handle unknown topic if iFrame not yet loaded

		let testurl = frameSrc,
			reloadurl = window.location.protocol + "\/\/" + window.location.hostname + window.location.pathname;
				if (frameSrc.indexOf("\?") > 0) {
					testurl = frameSrc.substr(0,frameSrc.indexOf("\?"));
				}
		
		if (!hmHelpVars.remoteAccess) {

			if (hmHelpVars.FileExists(frameSrc)) {
				jQuery('div#helpwrapper').append('\<iframe id="hmhelp" class="webhelp" src="'+frameSrc+'" frameborder="0"\>\<\/iframe\>');
			} else {
				alert("ERROR File Not Found " + frameSrc.substr(frameSrc.lastIndexOf("\/")+1));
				location.assign(reloadurl);
				return;
			}
		} else {
		
		jQuery(function() {
			jQuery.ajax({
				url: testurl,
				dataType: "jsonp",
				timeout: 5000,
			success: function () {
				jQuery('div#helpwrapper').append('\<iframe id="hmhelp" class="webhelp" src="'+frameSrc+'" frameborder="0"\>\<\/iframe\>');
			}, // Success
			error: function (parsedhtml) {
				if(parsedhtml.status == "200") {
					jQuery('div#helpwrapper').append('\<iframe id="hmhelp" class="webhelp" src="'+frameSrc+'" frameborder="0"\>\<\/iframe\>');
					} else {
						alert("ERROR File Not Found " + testurl.substr(testurl.lastIndexOf("\/")+1));
						location.assign(reloadurl);
						return;
					}
				} // Error
			});
		}); 
		} // Unknown topic check, remote version
			} // iframe not yet lodaed
		}

		if (helpLoaded && typeof hmTopic != "undefined") {
			let currentPath = hmHelpVars.getPath(jQuery("iframe#hmhelp").attr("src")),
				targetPath = hmPath != "" ? hmHelpVars.getPath(hmPath) : currentPath,
				targetSource = "";
				
			hmDevice.targetDomain = hmHelpVars.getDomain(targetPath);
			
			if (currentPath == targetPath) {
				xMessage.sendObject("hmhelp",{action: "loadtopic", href: hmTopic, bs: false, domain: hmHelpVars.getDomain(targetPath)});
			} else {
				jQuery("iframe#hmhelp").attr("src","");
					hmHelp = {};
					setTimeout(function(){
						hmHelp = new hmH(targetPath,hmTopic);
						hmHelp.showHelp();
						return;
					},100);
					hmHelpVars.topicChanged = true;
					return;
			}
			
			hmHelpVars.topicChanged = true;
		} else if (helpLoaded && typeof hmTopic == "undefined") {
				
				let thisPath = hmHelpVars.getPath(hmHelpVars.hmPopupPath),
					thatPath = hmHelpVars.getPath(jQuery("iframe#hmhelp").attr("src"));
				
				if (thisPath != thatPath && !hmHelpVars.hmHelpOpen()) {
					jQuery("iframe#hmhelp").attr("src","");
					hmHelp = {};
					setTimeout(function(){
						hmHelp = new hmH(hmHelpVars.hmPopupPath,hmHelpVars.hmDefaultTopic);
						hmHelp.showHelp();
						return;
					},100);
					return;
				}
		}
			helpLoaded = true;
			hmHelpVars.currentHelpPage = frameSrc;

				if (hmHelpVars.topicChanged || (this.firstLoad && hmHelpVars.hmHelpOpen())) {
						hmToggleHelp("open");
						hmHelpVars.topicChanged = false;
					}  else {
							hmToggleHelp("auto");
					}

		this.firstLoad = false;
		
		}; // hmHelp.showHelp();

		// Manage embedded borders on resize
		function manageEmbedBorders() {
			var toWide = false,
				toNarrow = false,
				activeBorderWidth = "thin";
			function doBorders() {
				var viewportWidth = jQuery("div#helpwrapper").width();
					
				if (jQuery(window).width() > viewportWidth && !toWide) {
					toWide = true;
					toNarrow = false;
					xMessage.sendObject("hmhelp",{action: "callfunction", fn: "hmWebHelp.embedBorderSwitch", fa: "bordersOn", domain: hmDevice.targetDomain});
					}
				else if (jQuery(window).width() <= viewportWidth && !toNarrow) {
					toNarrow = true;
					toWide = false;
					xMessage.sendObject("hmhelp",{action: "callfunction", fn: "hmWebHelp.embedBorderSwitch", fa: "bordersOff", domain: hmDevice.targetDomain});
				}
			}
			jQuery(window).on("resize",doBorders);
		}
		var mEB = new manageEmbedBorders();
	
		// Define full window variables in advance
		var FWcurrentCSS = "",
		FWcurrentFrameCSS = "",
		FWfullwindow = false,
		FWcurrentOverflow = jQuery("body").css("overflow"),
		FWcurrentOverflowH = jQuery("html").css("overflow");
		
		// Full window method
		this.doFullWindow = function() {
		
		// Prevent double bounce from init and execution without help loaded 
		if (!helpLoaded || hmHelpVars.zoomOpening) return;

		hmHelpVars.zoomOpening = true;
		
		if (FWfullwindow) {

			jQuery("div#helpwrapper").attr("style",FWcurrentCSS);
			jQuery("iframe#hmhelp").attr("style",FWcurrentFrameCSS);
			jQuery("body").css("overflow",FWcurrentOverflow);
			jQuery("html").css("overflow",FWcurrentOverflowH);
			FWfullwindow = false;
			
			if (!true) {
			xMessage.sendObject("hmhelp",{action: "callfunction", fn: "hmWebHelp.openHeaderIfClosed", domain: hmDevice.targetDomain}); 
			} else {
			xMessage.sendObject("hmhelp",{action: "callfunction", fn: "hmWebHelp.closeHeaderIfOpen", domain: hmDevice.targetDomain});
			}
			xMessage.sendObject("hmhelp",{action: "callfunction", fn: "hmWebHelp.embedBorderSwitch", fa: ["zoomin", jQuery(window).width()], domain: hmDevice.targetDomain});
			
			setTimeout(
				function(){
					hmHelpVars.zoomOpening = false;
				}, 400);

		} else {
			FWcurrentCSS = ("position:" + jQuery("div#helpwrapper").css("position") + "; float: " + jQuery("div#helpwrapper").css("float") + "; display: block;" );
			
			FWcurrentFrameCSS = jQuery("iframe#hmhelp").attr("style");
			jQuery("body,html").css("overflow", "hidden");
			
			jQuery("div#helpwrapper").attr("style","display: block; position: fixed; float: none; border-radius: 0; border: none; left: 50%; transform: translateX(-50%); max-height: 100%; max-width: 1024px;");

			xMessage.sendObject("hmhelp",{action: "callfunction", fn: "hmWebHelp.embedBorderSwitch", fa: ["zoomout", jQuery(window).width()], domain: hmDevice.targetDomain});

			jQuery("div#helpwrapper").animate({
				height: "100%",
				width: "100%",
				top: "0",
				bottom: "0"
			}, 400, function(){
				var finalCSS = {}, targetCSS, sourceCSS = "top: -1em; bottom: 0;".split(";");
				for (var x=0; x < sourceCSS.length-1; x++) {
					targetCSS = sourceCSS[x].split(":");
					if (targetCSS.length == 2) {
						finalCSS[targetCSS[0].trim()] = targetCSS[1].trim(); 
					}
				}
				jQuery("div#helpwrapper").css(finalCSS);

				hmHelpVars.zoomOpening = false;

				window.scrollTo(0,0);
				FWfullwindow = true;
				if (this.showhelp) {
					this.showHelp();
					}
			});

			jQuery("iframe#hmhelp").attr("style","");
			
			} 
		}; // hmHelp.doFullWindow
		
	} // hmHelp object constructor

	// Call help from widget instance
	HMshowHelpInstance = function(path, topic, callback) {
		
		hmHelpVars.saveCallback(callback,path);
		
		path = hmHelpVars.slashIt(path);
		
		let desktopMode = hmDevice.device == "desktop";
		
		if (!desktopMode) {
			hmHelpVars.mobileHelp(path,topic);
			return;
		}
		
		let currentSource = jQuery("iframe#hmhelp").attr("src");

		if (typeof currentSource == "undefined") {
				HMShowThisHelp(path, topic);
			}
		else {
			
			let currentTopic = currentSource.substr(currentSource.lastIndexOf("/")+1),
				currentPath = currentSource.substr(0,currentSource.lastIndexOf("/")+1);

			if (currentPath == path) {
				HMShowThisHelp(currentPath, currentTopic);
			} else {
				HMShowThisHelp(path, topic);
			}
		}
	}
	
	HMShowThisHelp = function(path, topic) {

		path = hmHelpVars.slashIt(path);
		
		hmHelpVars.remoteAccess = hmHelpVars.getDomain(path) != hmHelpVars.localdomain;

			var ifsrc = jQuery("iframe#hmhelp").attr("src");
			ifsrc = typeof ifsrc == "undefined" ? undefined : ifsrc.substr(0,ifsrc.lastIndexOf("\/")+1);
			if (typeof ifsrc == "undefined" || (ifsrc != "" && ifsrc != path)) {

				let showIt = function() {
					jQuery("iframe#hmhelp").attr("src","");
					hmHelp = {};
					setTimeout(function(){
						hmHelp = new hmH(path,topic);
						hmHelp.showHelp();
					},100);
				};
				
				if (hmHelpVars.hmHelpOpen()) {
				jQuery('div#helpwrapper').fadeOut(200, function(){
					jQuery(this).css('visibility','hidden').css("display","none");
					showIt();
				});
				} else {
					showIt();
				}
			} else {
				hmHelp.showHelp();
			}
	} // HMShowthisHelp()
 
   var HMEmbedInit = function() {

	// Check for open with help from URL
   var helpUrl = document.location.search.substr(1),
	   helpAnchor = document.location.hash.substr(1),
	   urlTopic = false;
  

	if (helpUrl.length > 10) {
		helpUrl = helpUrl.split("\&");
		for (var x = 0; x < helpUrl.length; x++) {
			if (helpUrl[x].substr(0,7) == "hmhelp=") {
				helpUrl = helpUrl[x].substr(7);
				urlTopic = true;
				break;
				}
			}
		} 
	
	if (helpAnchor.length > 1) {
		helpAnchor = "?anchor=" + helpAnchor;
		} else {
			helpAnchor = "";
			}
	if (urlTopic) {
		setTimeout(function(){hmHelp.showHelp(helpUrl + helpAnchor)},1000);
		}

	if (!urlTopic && hmHelpVars.defaultState != "none") {
		hmHelp.showHelp();
	}	
		};



	function initHmHelp(path, topic, callback) {
		
		// Is jQuery loaded?
		if (!window.jQuery) {
			var scrRef = document.createElement("script");
			scrRef.setAttribute("type","text/javascript");
			scrRef.setAttribute("src", path + "js/jquery.js");
			document.getElementsByTagName("head")[0].appendChild(scrRef);			
		}
		
		path = hmHelpVars.getFullPath(hmHelpVars.slashIt(path));
		hmHelpVars.hmPopupPath = path;
		
		if (typeof callback == "function") {
			hmHelpVars.saveCallback(callback,path);
			hmHelpVars.hmDefaultCallback = callback;
		} 

		hmHelpVars.targetdomain = hmHelpVars.getDomain(hmHelpVars.hmPopupPath);
		hmHelpVars.remoteAccess = hmHelpVars.targetDomain != hmHelpVars.localDomain;

	// Load and initialize 
	var loadCheck = setInterval(function() {
		if (window.jQuery) {
			clearInterval(loadCheck);
			jQuery(document).ready(function(){
			
			// Non-phone mode with embedded window
			if (!hmDevice.phone) {
					jQuery.ajaxPrefilter( "json script", function(options) {options.crossDomain = true;});

					var xinit = function() {
						jQuery("div#helpwrapper").prepend('<div id="helpcloser" onclick="" title="Hide Embedded Help" style="position: absolute; top: 0; right: 0; width: 30px; height: 30px; background-color: #transparent; background-size: 20px 20px; background-repeat: no-repeat; background-position: 5px 5px; z-index: 20000; cursor: pointer;"></div>');
					
						let popPosition = jQuery("div#helpwrapper").css("position");
						
						jQuery("div#helpwrapper").css("position", (["relative","absolute"]).includes(popPosition) ? popPosition : "relative");
						
						jQuery("div#helpcloser").css("background-image","url(" + hmHelpVars.hmPopupPath + "images/closehelp.png)");
						
						hmHelp = new hmH(path, topic, callback);
						if (hmHelp.startupOn) {
							hmHelp.showHelp();
							if (jQuery(".hmHelpToggle").length < 1) {
								jQuery("div#helpcloser").hide();
								}
							}
						};
					
					if (typeof xMessage == "undefined") {
						jQuery.getScript(path + "js/xmessage.js", function( data, textStatus, jqxhr ) {
						xMessage = new xMsg("EMBED PARENT: ");
						xinit();
						});
					} else {
						xinit();
					}
			} // End of desktop routine

			// Small tablet and phone mode with new window
			else {
				hmHelp = new hmH(path,topic,"mobile");
				if (hmHelp.startupOn) 
					hmHelp.helpWrapper.style.display = "none";
			} // If tablet or mobile
			});
			}
		},50);
	}
