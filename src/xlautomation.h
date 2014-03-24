#ifndef __xlautomation_h
#define __xlautomation_h


#ifdef __WXMSW__

#include <wx/wx.h>
#include <wx/string.h>
#include <wx/msw/ole/automtn.h>
#include "object.h"

#define xlWorksheet -4167
#define xl3DPie -4102
#define xlRows 1
#define xlXYScatter -4169
#define xlXYScatterLines 74
#define xlXYScatterSmoothNoMarkers 73
#define xlXYScatterSmooth 72
#define xlXYScatterLinesNoMarkers 75
#define xlColumns 2
#define xlNormal -4143
#define xlUp -4162


wxString UnsplitCells(const matrix_t<wxString> &table, char colsep, char rowsep, bool quote_colsep);


class XLAutomation  
{
public:
	XLAutomation();
	virtual ~XLAutomation();

	bool StartExcel();
	bool AttachExcel(bool start_if_fail=false);
	bool QuitExcel();
	bool CloseAllNoSave();

	bool Show(bool b);

	bool OpenFile(const wxString &fn);

	bool WorkbookCount(int &count);
	bool SetWorkbook(int idx);
	bool CloseWorkbookNoSave();

	bool NewWorkbook();

	bool AddWorksheet();
	bool SetWorksheetName(const wxString &name);

	bool PasteNewWorksheet(const wxString &name, const matrix_t<wxString> &cells,
		bool std_formatting=false, bool in_current_worksheet=false);
	
	bool SetCellValue(int row, int col, const wxString &val);
	bool GetCellValue(int row, int col, wxString &val);
	bool SetRangeValue(const wxString &range, const wxString &val);
	bool GetRangeValue(const wxString &range, wxString &val);

	bool SetNamedRangeArray(const wxString &range, wxArrayString &val, int rowsOrColumns=xlColumns);
	bool SetNamedRangeValue(const wxString &namedRange, const wxString &val);
	bool GetNamedRangeValue(const wxString &namedRange, wxString &val);

	bool AutoFitColumns();
	bool SetRowColBold(int rowcol, bool bold); // negative for col, positive for row
	bool SetSelectedCellsFontSize(int sz);

	bool PasteClipboard();

	wxString GetLastError();


protected:
	void ClearArgs();

#define _XL_NIVKARGS 4

	wxString m_errStr;
	wxVariant m_argList[_XL_NIVKARGS];
	wxVariant m_retVal;
	wxAutomationObject *m_pdispExcelApp;
	wxAutomationObject *m_pdispWorkbook;
	wxAutomationObject *m_pdispWorksheet;

};

#endif // __WXMSW__  (excel automation only on windows)

#endif
