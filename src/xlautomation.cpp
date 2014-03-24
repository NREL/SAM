#include "xlautomation.h"

#include <wx/clipbrd.h>
#include <wx/msw/ole/oleutils.h>


wxString UnsplitCells(const matrix_t<wxString> &table, char colsep, char rowsep, bool quote_colsep)
{
	wxString result = "";
	int r, c;

	for (r = 0; r<table.nrows(); r++)
	{
		for (c = 0; c<table.ncols(); c++)
		{
			if (quote_colsep && table.at(r, c).Find(colsep) != wxNOT_FOUND)
			{
				result = result + '"' + table.at(r, c) + '"';
			}
			else
			{
				result = result + table.at(r, c);
			}

			if (c < table.ncols() - 1)
			{
				result = result + colsep;
			}
		}

		if (r < table.nrows() - 1)
		{
			result = result + rowsep;
		}
	}
	return result;
}


XLAutomation::XLAutomation()
{
	m_pdispExcelApp = NULL;
	m_pdispWorkbook = NULL;
	m_pdispWorksheet = NULL;

	ClearArgs();
}

XLAutomation::~XLAutomation()
{
	if (m_pdispWorksheet) delete m_pdispWorksheet;
	if (m_pdispWorkbook) delete m_pdispWorkbook;
	if (m_pdispExcelApp) delete m_pdispExcelApp;
}


bool XLAutomation::StartExcel()
{
	// if Excel is already running, return with current instance
	if (m_pdispExcelApp != NULL)
		return true;

	if (m_pdispExcelApp) delete m_pdispExcelApp;
	m_pdispExcelApp = NULL;

	CLSID clsExcelApp;

	/* Obtain the CLSID that identifies EXCEL.APPLICATION
	 * This value is universally unique to Excel versions 5 and up, and
	 * is used by OLE to identify which server to start.  We are obtaining
	 * the CLSID from the ProgID.
	 */

	wxBasicString unicodeName( wxString("Excel.Application").mb_str() );

	if (FAILED(CLSIDFromProgID( (BSTR)unicodeName, &clsExcelApp))) 
	{
		wxMessageBox("Cannot obtain CLSID from ProgID", "Failed");
		return false;
	}

	IDispatch *pdisp = NULL;
	// start a new copy of Excel, grab the IDispatch interface
	if (FAILED(CoCreateInstance(clsExcelApp, NULL, CLSCTX_LOCAL_SERVER, IID_IDispatch, (void**)&pdisp))) 
	{
		wxMessageBox("Cannot start an instance of Excel for Automation.", "Failed");
		return false;
	}

	m_pdispExcelApp = new wxAutomationObject;
	m_pdispExcelApp->SetDispatchPtr( (WXIDISPATCH*) pdisp );

	return true;

	/*
	if (!m_pdispExcelApp->CreateInstance("Excel.Application"))
	{
		m_errStr = "Failed to create Excel OLE invoke object.";
		delete m_pdispExcelApp;
		m_pdispExcelApp = NULL;
		return false;
	}

	return true;
	*/
}

bool XLAutomation::AttachExcel(bool start_if_fail)
{
	if (m_pdispExcelApp) delete m_pdispExcelApp;

	m_pdispExcelApp = new wxAutomationObject;
	if (!m_pdispExcelApp->GetInstance("Excel.Application"))
	{
		if (start_if_fail && m_pdispExcelApp->CreateInstance("Excel.Application"))
			return true;

		m_errStr = "Failed to attach to running Excel instance.";
		delete m_pdispExcelApp;
		m_pdispExcelApp = NULL;
		return false;
	}
	
	return true;
}

bool XLAutomation::QuitExcel()
{
	m_errStr = "";

	if (!m_pdispExcelApp)
		return true;

	if (!CloseAllNoSave())
		m_errStr = "Failed to close all workbooks.";
	
	if (m_errStr == "")
		m_pdispExcelApp->CallMethod("Quit");
	
	// delete object anyways
	delete m_pdispExcelApp;
	m_pdispExcelApp = NULL;

	delete m_pdispWorkbook;
	m_pdispWorkbook = NULL;

	delete m_pdispWorksheet;
	m_pdispWorksheet = NULL;

	return (m_errStr == "");
}

bool XLAutomation::Show(bool b)
{
	if (m_pdispExcelApp)
		return m_pdispExcelApp->PutProperty("Visible", wxVariant(b));
	else
	{
		m_errStr = "Excel OLE not initialized.";
		return false;
	}
}

bool XLAutomation::CloseAllNoSave()
{
	int count = 0;
	bool ok = true;
	while ( (ok=WorkbookCount(count)) && count > 0)
	{
		if (!SetWorkbook(1))
			return false;

		if (!CloseWorkbookNoSave())
			return false;
	}

	if (!ok)
		m_errStr = "Failed to determine workbook count.";

	return ok;
}

bool XLAutomation::OpenFile(const wxString &fn)
{
	if (!m_pdispExcelApp)
	{
		m_errStr = "Excel OLE not initialized.";
		return false;
	}

	wxAutomationObject books;
	if (!m_pdispExcelApp->GetObject(books, "Workbooks"))
	{
		m_errStr = "Could not get 'Workbooks' object";
		return false;
	}

	ClearArgs();
	m_argList[0].SetName("Filename");
	m_argList[0] = fn;

	if (!books.Invoke("Open", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Could not open file:\n\n" + fn;
		return false;
	}

	// close the current workbook if possible
	CloseWorkbookNoSave();

	if (m_pdispWorkbook) delete m_pdispWorkbook;
	
	m_pdispWorkbook = new wxAutomationObject;
	m_pdispWorkbook->SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	// get first worksheet

	ClearArgs();
	m_argList[0] = (long) 1;
	if (!m_pdispWorkbook->Invoke("Worksheets", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Could not get first worksheet of opened workbook.";
		return false;
	}

	if (m_pdispWorksheet) delete m_pdispWorksheet;

	m_pdispWorksheet = new wxAutomationObject;
	m_pdispWorksheet->SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	return true;

}

bool XLAutomation::WorkbookCount(int &count)
{
	count = 0;

	if (!m_pdispExcelApp)
	{
		m_errStr = "Excel OLE not initialized.";
		return false;
	}

	m_retVal = m_pdispExcelApp->GetProperty("Workbooks.Count");
	if (m_retVal.IsNull())
	{
		m_errStr = "Could not determine count, 'Workbooks.Count' gave NULL";
		return false;
	}

	if (m_retVal.GetType().Lower() != "long")
	{
		m_errStr = "Bad return type[" + m_retVal.GetType() + "] from 'Workbooks.Count'";
		return false;
	}

	count = (int) m_retVal.GetInteger();
	return true;
}

bool XLAutomation::SetWorkbook(int idx)
{

	if (!m_pdispExcelApp)
	{
		m_errStr = "Excel OLE not initialized.";
		return false;
	}

	wxAutomationObject wkbks;
	if (!m_pdispExcelApp->GetObject( wkbks, "Workbooks" ))
	{
		m_errStr = "Failed to get 'Workbooks' object";
		return false;
	}

	ClearArgs();
	m_argList[0] = (long)idx;

	if (!wkbks.Invoke("Item", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr.Format("Could not get Workbooks.Item(%d)",idx);
		return false;
	}

	if (m_pdispWorkbook) delete m_pdispWorkbook;

	m_pdispWorkbook = new wxAutomationObject;
	m_pdispWorkbook->SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );


	if (!m_pdispWorkbook->Invoke("ActiveSheet", DISPATCH_PROPERTYGET, m_retVal, 0, m_argList))
	{
		m_errStr.Format("Could not get 'ActiveSheet' of Workbook(%d)", idx);
		return false;
	}

	if (m_pdispWorksheet) delete m_pdispWorksheet;
	
	m_pdispWorksheet = new wxAutomationObject;
	m_pdispWorksheet->SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	return true;

}

bool XLAutomation::CloseWorkbookNoSave()
{
	if (!m_pdispExcelApp)
	{
		m_errStr = "Excel OLE not initialized.";
		return false;
	}

	if (!m_pdispWorkbook)
	{
		m_errStr = "No current workbook initialized, cannot close.";
		return false;
	}

	ClearArgs();
	m_argList[0] = (bool) false;

	if (!m_pdispWorkbook->Invoke("Close", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Failed to close current workbook.";
		return false;
	}

	delete m_pdispWorkbook;
	m_pdispWorkbook = NULL;

	return true;
}


bool XLAutomation::NewWorkbook()
{
	if (!m_pdispExcelApp)
	{
		m_errStr = "Excel OLE not initialized.";
		return false;
	}

	// create a new workbook and worksheet
	wxAutomationObject wkbks;
	if (!m_pdispExcelApp->GetObject( wkbks, "Workbooks" ))
	{
		m_errStr = "Could not get 'Workbooks' object";
		return false;
	}

	ClearArgs();
	// Set wb = [application].Workbooks.Add(template := xlWorksheet)
	m_argList[0].SetName("Template");
	m_argList[0] = ((long)-4167);
	if (!wkbks.Invoke("Add", DISPATCH_METHOD, m_retVal, 1, m_argList))
	{
		m_errStr = "Failed to add workbook with new worksheet";
		return false;
	}

	if (m_pdispWorkbook) delete m_pdispWorkbook;

	m_pdispWorkbook = new wxAutomationObject;
	m_pdispWorkbook->SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	// clear the current sheet pointer
	if (m_pdispWorksheet) delete m_pdispWorksheet;

	m_pdispWorksheet = new wxAutomationObject;

	ClearArgs();
	m_argList[0] = (long)1;
	if (!m_pdispWorkbook->Invoke("Worksheets", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Could not get first worksheet in new workbook";
		return false;
	}

	m_pdispWorksheet->SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	return true;
}

bool XLAutomation::AddWorksheet()
{
	if (!m_pdispWorkbook)
	{
		m_errStr = "No active workbook, cannot create new worksheet.";
		return false;
	}

	if (!m_pdispWorkbook->Invoke("Worksheets.Add", DISPATCH_PROPERTYGET, m_retVal, 0, m_argList))
	{
		m_errStr = "Could not add a new worksheet in current workbook";
		return false;
	}

	if (m_pdispWorksheet) delete m_pdispWorksheet;

	m_pdispWorksheet = new wxAutomationObject;
	m_pdispWorksheet->SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	return true;
}

bool XLAutomation::SetWorksheetName(const wxString &name)
{
	if (!m_pdispWorksheet)
	{
		m_errStr = "No active worksheet, cannot set name";
		return false;
	}

	ClearArgs();
	m_argList[0] = name;
	if (!m_pdispWorksheet->Invoke("Name", DISPATCH_PROPERTYPUT, m_retVal, 1, m_argList))
	{
		m_errStr = "Could not invoke name property on worksheet";
		return false;
	}

	return true;
}


bool XLAutomation::PasteNewWorksheet(const wxString &name, const matrix_t<wxString> &cells, bool std_formatting, bool in_current_worksheet)
{
	if (!in_current_worksheet && !AddWorksheet()) return false;
	if (!SetWorksheetName(name)) return false;

	wxString tab_data = UnsplitCells( cells, '\t', '\n', false);
	wxTheClipboard->SetData( new wxTextDataObject( 
		tab_data ) );
	wxTheClipboard->Close();

	wxTheClipboard->Flush();	
	wxMilliSleep(90);

	bool ok = PasteClipboard();
	wxMilliSleep(50);

	if (ok && std_formatting)
	{
		SetSelectedCellsFontSize(9);
		AutoFitColumns();
		SetRowColBold(1, true);
		SetRowColBold(-1, true);
	}
	
	return ok;
}

bool XLAutomation::AutoFitColumns()
{
	if (m_pdispWorksheet)
	{
		m_pdispWorksheet->CallMethod("Columns.AutoFit",0,NULL);
		return true;
	}
	else return false;
}

bool XLAutomation::SetRowColBold(int rowcol, bool bold)
{
	if (!m_pdispWorksheet) return false;

	ClearArgs();
	m_argList[0] = (long)abs(rowcol);

	wxString objname = rowcol<0?"Columns":"Rows";
	wxAutomationObject rowobj;
	if (!m_pdispWorksheet->Invoke(objname, DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Could not obtain " + objname + " object";
		return false;
	}
	rowobj.SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );
	
	ClearArgs();
	m_argList[0] = bold;
	
	if (!rowobj.PutProperty("Font.Bold", 1, m_argList))
	{
		m_errStr = "Could not set " + objname + "(...).Font.Bold property";
		return false;
	}

	return true;
}

bool XLAutomation::SetSelectedCellsFontSize(int sz)
{
	if (!m_pdispWorksheet) return false;

	ClearArgs();
	m_argList[0] = (long)sz;

	return m_pdispWorksheet->PutProperty("Cells.Font.Size", 1, m_argList);
}

bool XLAutomation::SetCellValue(int row, int col, const wxString &val)
{
	if (!m_pdispWorksheet)
	{
		m_errStr = "No active worksheet, cannot set cell";
		return false;
	}

	ClearArgs();
	m_argList[0] = (long)row;
	m_argList[1] = (long)col;

	wxAutomationObject cell;
	if (!m_pdispWorksheet->Invoke("Cells", DISPATCH_PROPERTYGET, m_retVal, 2, m_argList))
	{
		m_errStr = "Could not get 'Cells' object ref.";
		return false;
	}

	cell.SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );
	ClearArgs();
	m_argList[0] = val;

	if (!cell.Invoke("Value", DISPATCH_PROPERTYPUT, m_retVal, 1, m_argList))
	{
		m_errStr.Format("Could not set Cells(%d,%d).Value = '%s'", row, col, val.c_str());
		return false;
	}

	return true;
}

bool XLAutomation::GetCellValue(int row, int col, wxString &val)
{
	if (!m_pdispWorksheet)
	{
		m_errStr = "No active worksheet, cannot set cell";
		return false;
	}

	ClearArgs();
	m_argList[0] = (long)row;
	m_argList[1] = (long)col;

	wxAutomationObject cell;
	if (!m_pdispWorksheet->Invoke("Cells", DISPATCH_PROPERTYGET, m_retVal, 2, m_argList))
	{
		m_errStr = "Could not get 'Cells' object ref.";
		return false;
	}

	cell.SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );
	m_retVal.Clear();
	if (!cell.Invoke("Value", DISPATCH_PROPERTYGET, m_retVal, 0, NULL))
	{
		m_errStr.Format("Could not get Cells(%d,%d).Value", row, col);
		return false;
	}

	wxString type = m_retVal.GetType().Lower();
	type.Replace(" ","");
	if (type == "void*")
		val = "";
	else
		val = m_retVal.GetString();

	return true;
}

bool XLAutomation::SetRangeValue(const wxString &range, const wxString &val)
{
	if (!m_pdispWorksheet)
	{
		m_errStr = "No active worksheet, cannot set cell";
		return false;
	}

	ClearArgs();
	m_argList[0] = range;
	if (!m_pdispWorksheet->Invoke("Range", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Failed to access range(" + range + ")";
		return false;
	}

	wxAutomationObject raobj;
	raobj.SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	ClearArgs();
	m_argList[0] = val;

	if (!raobj.Invoke("Value", DISPATCH_PROPERTYPUT, m_retVal, 1, m_argList))
	{
		m_errStr.Format("Could not set Range(%s).Value = '%s'", range.c_str(), val.c_str());
		return false;
	}

	return true;	
}

bool XLAutomation::SetNamedRangeArray(const wxString &namedRange, wxArrayString &val, int rowsOrColumns)
{
	// Added to set cash flow ranges
	// if named range specified then anywhere in active workbook
	// if 'A1:B1' specified then in active sheet
	// if 'Sheet3!A1:C3 specified then sheet and range in active workbook
	if (!m_pdispExcelApp)
	{
		m_errStr = "No Excel instance found, cannot set range values";
		return false;
	}

	wxAutomationObject raobj;
	ClearArgs();
	m_argList[0] = namedRange;
	if (!m_pdispExcelApp->GetObject( raobj, "Range", 1, m_argList ))
	{
		m_errStr = "Failed to access range(" + namedRange + ")";
		return false;
	}

	switch ( rowsOrColumns )
	{
		case xlRows:
			for ( int i=0; i<(int)val.Count(); i++)
				raobj.PutProperty("Cells",i+1,1, val[i]);
			break;
		case xlColumns:
			for ( int i=0; i<(int)val.Count(); i++)
				raobj.PutProperty("Cells",1,i+1, val[i]);
			break;
		default:
			return false;
	}

	return true;	
}

bool XLAutomation::SetNamedRangeValue(const wxString &namedRange, const wxString &val)
{
	// Added to set named range value anywhere in active workbook - not just in active sheet as in SetRangeValue
	// if named range specified then anywhere in active workbook
	// if 'A1:B1' specified then in active sheet
	// if 'Sheet3!A1:C3 specified then sheet and range in active workbook
	if (!m_pdispExcelApp)
	{
		m_errStr = "No Excel instance found, cannot set range values";
		return false;
	}

	wxAutomationObject raobj;
	ClearArgs();
	m_argList[0] = namedRange;
	if ( !m_pdispExcelApp->GetObject( raobj, "Range", 1, m_argList ) )
	{
		m_errStr = "Failed to access range(" + namedRange + ")";
		return false;
	}

	if ( !raobj.PutProperty("Value", val) )
	{
		m_errStr = "Failed to set named range value (" + namedRange + " = " + val + ")";
		return false;
	}

	return true;	
}

bool XLAutomation::GetNamedRangeValue(const wxString &namedRange, wxString &val)
{
	// Added to get named range value anywhere in active workbook - not just in active sheet as in GetRangeValue
	// if named range specified then anywhere in active workbook
	// if 'A1:B1' specified then in active sheet
	// if 'Sheet3!A1:C3 specified then sheet and range in active workbook
	if (!m_pdispExcelApp)
	{
		m_errStr = "No Excel instance found, cannot get range values";
		return false;
	}

	wxAutomationObject raobj;
	ClearArgs();
	m_argList[0] = namedRange;
	if ( !m_pdispExcelApp->GetObject( raobj, "Range", 1, m_argList ) )
	{
		m_errStr = "Failed to access range(" + namedRange + ")";
		return false;
	}

	m_retVal.Clear();
	if ( !raobj.GetProperty("Value", m_retVal) )
	{
		m_errStr.Format("Could not get Range(%s).Value", namedRange.c_str());
		return false;
	}

	wxString type = m_retVal.GetType().Lower();
	type.Replace(" ","");
	if (type == "void*")
		val = "";
	else
		val = m_retVal.GetString();


	return true;	
}



bool XLAutomation::GetRangeValue(const wxString &range, wxString &val)
{
	if (!m_pdispWorksheet)
	{
		m_errStr = "No active worksheet, cannot set cell";
		return false;
	}

	ClearArgs();
	m_argList[0] = range;
	if (!m_pdispWorksheet->Invoke("Range", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Failed to access range(" + range + ")";
		return false;
	}

	wxAutomationObject raobj;
	raobj.SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );

	raobj.SetDispatchPtr( (WXIDISPATCH*) m_retVal.GetVoidPtr() );
	m_retVal.Clear();
	if (!raobj.Invoke("Value", DISPATCH_PROPERTYGET, m_retVal, 0, NULL))
	{
		m_errStr.Format("Could not get Range(%s).Value", range.c_str());
		return false;
	}

	wxString type = m_retVal.GetType().Lower();
	type.Replace(" ","");
	if (type == "void*")
		val = "";
	else
		val = m_retVal.GetString();

	return true;
}

bool XLAutomation::PasteClipboard()
{
	if (!m_pdispWorkbook)
	{
		m_errStr = "No current workbook, cannot paste clipboard.";
		return false;
	}

	wxAutomationObject sht;
	if (!m_pdispWorkbook->GetObject(sht, "ActiveSheet"))
	{
		m_errStr = "Could not get 'ActiveSheet' for clipboard paste";
		return false;
	}

	ClearArgs();
	m_argList[0] = "A1";
	if (!sht.Invoke("Range", DISPATCH_PROPERTYGET, m_retVal, 1, m_argList))
	{
		m_errStr = "Could not get range A1 for clipboard paste";
		return false;
	}

	wxAutomationObject a1obj;
	a1obj.SetDispatchPtr( (WXIDISPATCH*)m_retVal.GetVoidPtr() );
	if (!a1obj.Invoke("Select", DISPATCH_PROPERTYGET, m_retVal, 0, m_argList))
	{
		m_errStr = "Could not select A1 for clipboard paste";
		return false;
	}


	if (!sht.CallMethod( "Paste" ))
	{
		m_errStr = "Clipboard paste to 'ActiveSheet:A1' failed.";
		return false;
	}

	return true;
}


wxString XLAutomation::GetLastError()
{
	return m_errStr;
}


void XLAutomation::ClearArgs()
{
	for (int i=0;i<_XL_NIVKARGS;i++)
		m_argList[i].Clear();
}