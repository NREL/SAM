hmWebHelp.funcs.hmDoCopyCode = function(event) {
	
	var $codeLink = $(event.srcElement || event.target), 
		$codeLinkHTML = $codeLink.html(),
		$thisTable = $codeLink.parents("table").first(),
		$codeCell = $thisTable.find("td").first(),
		codeCell = $codeCell[0],
		range = document.createRange(),
		sel;
				
		range.setStartBefore(codeCell.firstChild);
		range.setEndAfter(codeCell.lastChild);

		sel = window.getSelection();
		sel.removeAllRanges();	
		sel.addRange(range);

		setTimeout(function(){
		
		try {  
			var successful = document.execCommand('copy');  
			
			if (successful) {
				sel.removeAllRanges();
				$codeLink.html("Copied!");
				
				setTimeout(function() {
					$codeLink.html($codeLinkHTML);
					},1000);
				}
			} catch(err) {  
				alert('Sorry, cannot copy to clipboard with this browser!'); 
				} 		
		},300);
}
