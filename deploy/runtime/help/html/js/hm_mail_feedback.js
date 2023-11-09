/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/

// Constructor
function mFb() {
	
	// Reference to the main object
	var self = this;

	// Clean up email link strings
	var unQuote = function(varStr) {
	varStr = varStr.replace(/&gt;/g, ">");
	varStr = varStr.replace(/&lt;/g, "<");
	varStr = varStr.replace(/&quot;/g, '"');
	varStr = varStr.replace(/&amp;/g, '&');
	varStr = varStr.replace(/&nbsp;|&NBSP;/g, ' ');
	if (!false) {
		varStr = encodeURIComponent(varStr);
		varStr = varStr.replace(/%24CRLF%24/g,'%0A%0D');
		return varStr;
		}
	varStr = escape(varStr);
	varStr = varStr.replace(/%E2|%E0|%E5|%E1|%E3/g,'a');
	varStr = varStr.replace(/%C5|%C0|%C1|%C2|%C3/g,'A');
	varStr = varStr.replace(/%C7/g,'C');
	varStr = varStr.replace(/%E7/g,'c');
	varStr = varStr.replace(/%E9|%EA|%EB|%E8/g,'e');
	varStr = varStr.replace(/%C9|%CA|%C8|%CB/g,'E');
	varStr = varStr.replace(/%u0192/g,'f');
	varStr = varStr.replace(/%EF|%EE|%EC|%ED/g,'i');
	varStr = varStr.replace(/%CF|%CD|%CE|%CC/g,'I');
	varStr = varStr.replace(/%F1/g,'n');
	varStr = varStr.replace(/%D1/g,'N');
	varStr = varStr.replace(/%F4|%F2|%F3|%F5|%F8/g,'o');
	varStr = varStr.replace(/%D4|%D2|%D3|%D5|%D8/g,'O');
	varStr = varStr.replace(/%u0161/g,'s');
	varStr = varStr.replace(/%u0160/g,'S');
	varStr = varStr.replace(/%FB|%FA|%F9/g,'u');
	varStr = varStr.replace(/%DB|%DA|%D9/g,'U');
	varStr = varStr.replace(/%FF|%FD/g,'y');
	varStr = varStr.replace(/%DD|%u0178/g,'Y');
	varStr = varStr.replace(/%FC/g,'ue');
	varStr = varStr.replace(/%DC/g,'Ue');
	varStr = varStr.replace(/%E4|%E6/g,'ae');
	varStr = varStr.replace(/%C4|%C6/g,'Ae');
	varStr = varStr.replace(/%F6|%u0153/g,'oe');
	varStr = varStr.replace(/%D6/g,'Oe');
	varStr = varStr.replace(/%DF/g,'ss');
	varStr = varStr.replace(/%24CRLF%24/g,'%0A%0D');
	return (varStr);
	};	// End unQuote

	// Main variables
	var topicTitle = function(){ return unQuote($("h1.topictitle").first().text());},
	topicRef = function() {return unQuote("Reference:");},
	fbbody = unQuote("Dear Support Staff,"),
	mailsubject = function(){return(unQuote("Feedback on:" +  " " + $("h1#hm_pageheader").text() + " > " + $("h1.topictitle").text()));},
	mailrecipient = helpman_mailrecipient,
	simplerecipient = "sam.support@nrel.gov",
	simplesubject = "Documentation%20Feedback",
	mailurl = "",
	query = "" !== "" ? "" : false;

	// User has set an URL instead of normal feedback:
	var doMailURL = function(url) {
		url = hmWebHelp.trimString(url);
		url = url.replace(/\?$/,"").replace(/\%20/g," ");
		url = encodeURI(url);
		if (query) {
			query = hmWebHelp.trimString(query);
			query = query.replace(/^\?/,"").replace(/&amp;/g,"&").replace(/&gt;/g,">").replace(/&lt;/g,"<").replace(/\%20/g," ").replace(/&quot;/g,"\"").replace(/&apos;/g,"\'");
			url = url+"?"+query;
		}
		var fbWindow = window.open(url,"fbWin","",false);
	};

	// Expose the callable function
	return function() {
	
	// Simplified version for problems with special character sets
	var simplefb = false;
	
	// Reference to the topic
	var topicReference = window.location.protocol + "%2F%2F" + window.location.hostname + encodeURIComponent(window.location.pathname);
	
	// Close hamburger
	if ($("div#navigationmenu").is(":visible"))
		hmWebHelp.hamburgerMenu();
	
	// Use URL for alternative mail feedback page
	if (mailurl !== "") {
		doMailURL(mailurl);
		return;
		}

	// De-obfuscate address if necessary
	var deObfuscate = function(s){
		if (s.substr(0,2) !== "$$") return s;
		var temp = "";
		s=s.replace(/^\$\$/g,"").replace(/\$\$$/,"").replace(/\/\//g,"/").replace(/\"/g,"").replace(/\*/g,".");
		s = s.split("/");
		if (s.length === 4) {
			temp = '\"' + s[0] + '\" <' + s[1] + '@' + s[2] + '.' + s[3] + ">"; 
		} else if (s.length === 3) {
			temp = s[0] + '@' + s[1] + '.' + s[2];
		} else {
		alert("Error: Invalid feedback address format!");
		return;
		}
		return temp;
	}; 
	mailrecipient = deObfuscate(mailrecipient);
	simplerecipient = deObfuscate(simplerecipient);
	var fb, fb1, fb2;
	if (!simplefb){
		fb1 = "mailto:" + escape(mailrecipient) + "?subject=" + mailsubject();
		fb2 = "&body="+topicRef()+"%20" + topicTitle() + "%20%28" + topicReference + "%29%0A%0D" + fbbody + "%0A%0D";
		} else {
			fb1 = "mailto:" + escape(simplerecipient) + "?subject=" + simplesubject;
			fb2 = "&body=URL:%20" + topicReference + "%0A%0D";
			}
	fb = fb1 + fb2;
	document.location.href=fb;

	};

}

hmWebHelp.funcs.hm_mail_feedback = new mFb();
