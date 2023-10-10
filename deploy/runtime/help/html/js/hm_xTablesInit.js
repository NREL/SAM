/*! Help+Manual WebHelp 3 Script functions
Copyright (c) 2015-2023 by Tim Green. All rights reserved. Contact: https://www.helpandmanual.com
*/

function xTableConstructor($thisTable) {

	var tableID = $thisTable.attr("id"),
		fontSize = parseFloat($("body").css("font-size")),
		fixedTableWidth = widthFromStyle($thisTable.parent("div").attr("style")),
		initialized = false,
		emsMinimum = 20,
		noOfColumns = $($thisTable.find("tr")[0]).children("th").length,
		lastWindowWidth = $(window).width(),
		currentWindowWidth = $(window).width(),
		windowExpanding = false,
		resizeCounter = 0,
		dataTitles,
		hideDataTooltip="Hide row data display",
		showDataTooltip="Show hidden row data";
	
	function widthFromStyle(str) {
		var fixedWidth = 0,
			fixedWidthType = "",
			wrx = /width:\s{0,2}(\d+?)(px|em)/i,
			match = wrx.exec(str),
			returnWidth = 0;
		if (match !== null) {
			fixedWidth = match[1];
			fixedWidthType = match[2];
			if (fixedWidthType == "rem")
				returnWidth = parseInt(fixedWidth,10);
			else
				returnWidth = Math.round(parseInt(fixedWidth,10)/fontSize);
			} 
		return returnWidth;
		}
	
	function tableDiff(expanding) {
		
		var $visibleColumns = $($thisTable.find("tr")[0]).children("th:visible"),
			visColCount = $visibleColumns.length,
			windowEms = Math.round(($(window).width()/fontSize)),
			tableEms = 0;
		
		for (var x = 0 ; x < visColCount; x++) {		
			var colWidth = widthFromStyle($($visibleColumns[x]).attr("style"));
			// No fixed width set
			if (colWidth === 0) {
				// Set flexibile columns to current or minimum width
				colWidth = Math.round($($visibleColumns[x]).width()/fontSize);
				if (expanding) {
					colWidth = colWidth >= emsMinimum ? colWidth-8 : colWidth;
					}
				else {
					colWidth = colWidth >= emsMinimum ? colWidth : emsMinimum;
					}
			}
		tableEms += colWidth;
		}
		
		return (windowEms - tableEms);
		}
	
	
	function visibleWidth() {

		var $visibleColumns = $($thisTable.find("tr")[0]).children("th:visible");
		var visColCount = $visibleColumns.length;
		var totalWidth = 0;
		$visibleColumns.each(function(){
			
			totalWidth += $(this).width();
			
			});
			
		totalWidth = Math.round(totalWidth / fontSize);
		return totalWidth;
	}
	
	
	function visibleColumns() {
		var $visCols = $($thisTable.find("tr")[0]).children("th:visible");
		return $visCols.length;
	}
	
	
	function updateDataBlocks() {
		
		var visCols = visibleColumns(),
			colDiff = noOfColumns - visCols,
			$dataRows = $("tr."+tableID+".datarows"),
			$dataIcons = $("div." + tableID + ".xtable_dataiconwrapper");
		
		// Update colspan value for number of visible columns
		$dataRows.children("td:first-child").attr("colspan",visCols);
		
		// Show/hide data blocks as needed
		if (visCols == noOfColumns) {
			// Hide all data blocks and switch icons when full table is visible
			$dataRows.hide();
			$dataIcons.hide();
			} else {
				$dataIcons.show();
				// Show data of hidden columns
				for (x=noOfColumns;x>1&&x>noOfColumns-colDiff;x--)
					{
					$("div."+tableID+".xtable_datablock.column" + x).show();
					}
				// Hide data of visible columns
				for (x=2;x<=visCols;x++)
					{
					$("div."+tableID+".xtable_datablock.column" + x).hide();
					}
				// Open data blocks row if it is in the open state
				var $openRows = $("img[id^='"+tableID+"_'][src='images/close_data\\.png']").parents("tr:first-of-type").next("tr");				
				if ($openRows.length > 0 && $($openRows[0]).is(":hidden")) {
					$openRows.show();
				}
				
			}
			
	}
	
	function updateColumns(init) {
	
	// Is window expanding or contracting?
		currentWindowWidth = $(window).width();
		windowExpanding = lastWindowWidth < currentWindowWidth;
		lastWindowWidth = currentWindowWidth;

		var lastColumn = visibleColumns();
		if (!windowExpanding) {
			while ((tableDiff(windowExpanding) < -8) && (lastColumn > 1)) {
					lastColumn = visibleColumns();
					$thisTable.find("th:nth-child("+lastColumn+"),td:nth-child("+lastColumn+")").hide();
				}
			}
		else if (lastColumn < noOfColumns) {
				while (tableDiff(windowExpanding) > 8  && lastColumn+1 <= noOfColumns) {
				lastColumn = visibleColumns();
				$thisTable.find("th:nth-child("+(lastColumn+1)+"),td:nth-child("+(lastColumn+1)+")").show();
				}
				}
		
		updateDataBlocks();
	} // updateColumns
	
	// Resize handler
	$(window).on(hmBrowser.orientationevent + ".xTables", function(){
		
		if (hmBrowser.orientationevent == "orientationchange") {
			setTimeout(function(){
			if (fixedTableWidth > 0 && Math.round($(window).width()/fontSize) > fixedTableWidth) return;
			updateColumns(false);
			},300);
		} else {
				resizeCounter++;
				if (resizeCounter < 10) return;
				resizeCounter = 0;
		
		// Quit if the window is still bigger than the table
		if (fixedTableWidth > 0 && Math.round($(window).width()/fontSize) > fixedTableWidth) {
			return;
			}
		updateColumns(false);		
		}
		
		});
		
	// Startup update
	if (!initialized && (fixedTableWidth === 0 || (fixedTableWidth > 0 && Math.round($(window).width()/fontSize) > fixedTableWidth))) {	
	updateColumns(true);
		}
	
		
	// return;
	// Basic variables
	var allCells = $thisTable.find("td,th"),
	allRows = $thisTable.find("tbody tr"),
	$allSwitches, $dataHeaders,
	switchCol = 1, // parseInt($thisTable.attr("switch-col"),10),
	titleCols = 1,// parseInt($thisTable.attr("title-cols"),10),
	rowCounter = 1;
	// Set up the data and switch cells
	allCells.each(function() {
		var el = $(this),
		pos = el.index();
		if (pos+1 > titleCols)
			allCells.filter(":nth-child(" + (pos+1) + ")").addClass("data");
		if (pos == switchCol)
			allCells.filter("td:nth-child(" + (pos) + ")").addClass("switch");
	});
	
	$allSwitches = $thisTable.find("td.switch");
	$dataHeaders = $thisTable.find("th.data");
	
	// Set up the row IDs
	allRows.each(function() {
		$(this).attr("id",tableID + "_row" + (rowCounter++));
		$(this).addClass("dataoff");
		});
	// Attach switch handlers to open/close data blocks
	$allSwitches.each(function(){
		var tIconID = $(this).parent().attr("id") + "_icon",
			tempHTML = $(this).html(),
			$switchTD = $(this);
			
			
		$(this).html("<div class='"+tableID+" xtable_dataiconwrapper'><img class='xtable_dataicon' id=\""+tIconID+"\"src=\"images/open_data.png\" title=\""+showDataTooltip+" aria-label=\""+showDataTooltip+"\"/></div><div class='xtable_switchwrapper'>" + tempHTML+"</div>");
		
		$("img#"+tIconID).on("click", function(event){
			hmWebHelp.funcs.xTables[tableID](event,$switchTD);
			
		});
		
		
	});

	// Store the data headers array
	$dataHeaders.each(function(){
		if (typeof dataTitles == "undefined") {
			dataTitles = [];	
			}
			dataTitles.push($(this).text());
	});
	
	if (noOfColumns == visibleColumns() && !initialized) {
		// alert("div." +tableID+".xtable_dataiconwrapper");
		$("div." +tableID+".xtable_dataiconwrapper").hide();
	}
	
	initialized = true;
	
	/*** Handler for data blocks  ***/
	return function(event,$obj) {
		$obj.parent("tr").toggleClass("dataon dataoff");
		var $siblings = $obj.siblings(),
			siblingsHTML = "",
			$row = $obj.parent("tr"),
			rowid = $row.attr("id"),
			dataid = rowid + "_data",
			divid = rowid + "_div",
			iconid = rowid + "_icon",
			inlinestyle = typeof $obj.attr("style") == "undefined" ? "" :  $obj.attr("style"),
			colspan = 0,
			colcounter = 0,
			datatitle_class = "xtable_datatitle";
				
		// Create data blocks row if it doesn't exist already
		// Only create if actually needed (user tries to display it)
		if ($("tr#" + dataid).length === 0) {
			colcounter = 0;
			$siblings.each(function(){
				siblingsHTML += "<div class='"+tableID+" xtable_datablock column"+($(this).index()+1)+"'><p class='xtable_datatitle'>" + dataTitles[colcounter] + ":</p>" + $(this).html() + "</div>\r\n";
				// colspan = $(this).index();
				colcounter++;
				});
			colspan = visibleColumns();
			siblingsHTML = '<tr id="'+dataid+'" class="'+tableID+' datarows"><td style="'+inlinestyle+'" class="xResponsive" colspan="'+colspan+'"><div id="'+divid+'">'+siblingsHTML+'</div></td></tr>';
			$row.after(siblingsHTML);
		}
		
		updateDataBlocks();
		
		if ($obj.parent("tr").hasClass("dataon") && visibleColumns() < noOfColumns) {
			$("div#" + divid).hide();
			$("tr#" + dataid).show();
			$("div#" + divid).slideDown("fast", function(){
			$("img#" + iconid).attr("src","images/close_data.png").attr("title",hideDataTooltip).attr("aria-label",hideDataTooltip);
			});
				
			} else if ($obj.parent("tr").hasClass("dataoff") || visibleColumns() == noOfColumns) {
				$("div#" + divid).slideUp("fast", function(){
				$("tr#" + dataid).hide();
				$("img#" + iconid).attr("src","images/open_data.png").attr("title",showDataTooltip).attr("aria-label",showDataTooltip);
				});
				
				}
	
	}; // expandEntry
	
} // xTable Constructor

hmWebHelp.funcs.hm_xTablesInit = function($XTables) {
	var xTableCounter = 0;
	hmWebHelp.funcs.xTables = {};
	
	if ($XTables.length > 0)
		$XTables.each(function(){
		$(this).attr("id","xtable" + xTableCounter);	
		hmWebHelp.funcs.xTables[$(this).attr("id")] = new xTableConstructor($(this));
		xTableCounter++;
		});
}; // end hm_xTables// 	

