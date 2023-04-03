/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


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

	void Copy(ExcelExchange &rhs);
	void Write(wxOutputStream &);
	bool Read(wxInputStream &);
	
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
	static wxArrayString EnumerateAlphaIndex(const wxString &_start, const wxString &_end);
#endif
};


#endif