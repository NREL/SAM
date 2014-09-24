#ifndef __excelexch_h
#define __excelexch_h

#include <vector>

#include <wx/string.h>
#include <wx/stream.h>

#include <wex/ole/excelauto.h>

class Simulation;
class CaseWindow;

class ExcelExchange
{
public:
	ExcelExchange();

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
	
	bool Enabled;
	wxString ExcelFile;

	enum { SEND_TO, CAPTURE_FROM };

	struct ExchVar {
		wxString Name;
		wxString Range;
		int Type;
	};

	std::vector<ExchVar> Vars;


	struct Captured {
		wxString Name;
		wxString Range;
		wxString Value;
	};
	std::vector<Captured> Summary;
	

	static bool ShowExcelExchangeDialog( ExcelExchange &exch, CaseWindow *cw );
	static int RunExcelExchange( ExcelExchange &exch, VarTable &inputs, Simulation *sim );
	
#ifdef __WXMSW__
	static bool ParseAndCaptureRange( const wxString &range, wxString &val, wxExcelAutomation &xl );
#endif
};


#endif