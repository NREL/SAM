#ifndef __excelexch_h
#define __excelexch_h

#include <vector>

#include <wx/string.h>
#include <wx/stream.h>

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
	

	static bool ShowExcelExchangeDialog( ExcelExchange *exch, CaseWindow *cw );
	static bool RunExcelExchange( ExcelExchange *exch, Simulation *sim );
};


#endif