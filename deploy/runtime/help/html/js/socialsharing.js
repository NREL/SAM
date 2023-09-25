/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/
window.onerror = function(){return false;};
	
	var hmSharePage = function(target) {
		
		var title = document.title,
			url = hmpage.currentURI;

		switch(target) {
			case 'facebook':
				window.open('https://www.facebook.com/sharer.php?u=' + encodeURI(url), '_blank');
				break;
			case 'twitter':
				window.open('https://twitter.com/intent/tweet/?url=' + encodeURI(url) + '&text=' + encodeURIComponent(title) + ": ", '_blank');
				break;
			case 'linkedin':
				window.open('https://www.linkedin.com/shareArticle?mini=true&url=' + encodeURI(url) + '&title=' + encodeURIComponent(title), '_blank');
				break;
			case 'email':
				window.location.href = 'mailto:?subject=' + encodeURIComponent(title) + '&body=' + encodeURI(url);
				break;
		}
	}

// Close any menus before loading the popup
	hmWebHelp.closeMenus();

hmLoadPopup({
hmTitle:"Share This Topic",
hmBody:
'<div class="sharebox">' +
'<a id="xplShareFacebook" class="sharebutton" title="Share topic on Facebook" onclick="hmSharePage(\'facebook\')">' +
'<svg viewBox="0 0 32 32" style="width:50px;height:50px">' +
'<rect width="32" height="32" style="fill:#3B5998;stroke:none"></rect>' +
'<path d="M22,32V20h4l1-5h-5v-2c0-2,1.002-3,3-3h2V5c-1,0-2.24,0-4,0c-3.675,0-6,2.881-6,7v3h-4v5h4v12H22z" style="fill:#FFFFFF;stroke:none"></path>' +
'</svg>' +
'</a>' +
'<a class="sharebutton" title="Tweet link to topic" onclick="hmSharePage(\'twitter\')">' +
'<svg viewBox="0 0 32 32" style="width:50px;height:50px">' +
'<g>' +
'<rect width="32" height="32" style="fill:#00ACED;stroke:none"></rect>' +
'<path d="M25.987,9.894c-0.736,0.322-1.525,0.537-2.357,0.635c0.85-0.498,1.5-1.289,1.806-2.231   c-0.792,0.461-1.67,0.797-2.605,0.978C22.083,8.491,21.017,8,19.838,8c-2.266,0-4.1,1.807-4.1,4.038   c0,0.314,0.036,0.625,0.104,0.922c-3.407-0.172-6.429-1.779-8.452-4.221c-0.352,0.597-0.556,1.29-0.556,2.032   c0,1.399,0.726,2.635,1.824,3.36c-0.671-0.022-1.304-0.203-1.856-0.506c-0.001,0.017-0.001,0.034-0.001,0.052   c0,1.955,1.414,3.589,3.29,3.96c-0.343,0.09-0.705,0.142-1.081,0.142c-0.264,0-0.52-0.024-0.77-0.072   c0.52,1.604,2.034,2.771,3.828,2.805C10.67,21.594,8.9,22.24,6.979,22.24c-0.33,0-0.658-0.018-0.979-0.056   c1.814,1.145,3.971,1.813,6.287,1.813c7.541,0,11.666-6.154,11.666-11.491c0-0.173-0.005-0.35-0.012-0.521   C24.741,11.414,25.438,10.703,25.987,9.894z" style="fill:#FFFFFF;stroke:none"></path>' +
'</g>' +
'</svg>' +
'</a>' +
'<a class="sharebutton" onclick="hmSharePage(\'email\')" title="Share topic by mail">' +
'<svg viewBox="0 0 32 32" style="width:50px;height:50px">' +
'<g>' +
'<rect width="32" height="32" style="fill:#DD4B39;stroke:none"></rect>'+
'<path d="M15.5,17 L26.5,6 L4.5,6 L15.5,17 Z M12.5,16 L15.5,19 L18.5,16 L26.5,25 L4.5,25 L12.5,16 Z M4.5,24 L4.5,7 L11.5,15 L4.5,24 Z M27.5,25 L27.5,6 L19.5,15 L27.5,25 Z" style="fill:#FFFFFF;stroke:none"></path>' +
'</g>' +
'</svg>' +
'</a>'+
'<a class="sharebutton" title="Share topic on LinkedIn" onclick="hmSharePage(\'linkedin\')">' +
'<svg viewBox="0 0 32 32" style="width:50px;height:50px">' +
'<g>' +
'<rect width="32" height="32" style="fill:#007BB6;stroke:none"></rect>' +
'<rect height="14" width="4" x="7" y="11" style="fill:#FFFFFF;stroke:none"></rect>' +
'<path d="M20.499,11c-2.791,0-3.271,1.018-3.499,2v-2h-4v14h4v-8c0-1.297,0.703-2,2-2c1.266,0,2,0.688,2,2v8h4v-7 C25,14,24.479,11,20.499,11z" style="fill:#FFFFFF;stroke:none"></path>' +
'<circle cx="9" cy="8" r="2" style="fill:#FFFFFF;stroke:none"></circle>' +
'</g>' +
'</svg>' +
'</a>' +
'</div>', 
dims: {"height": "1rem", "width": "20rem"}
});

