/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <wx/dialog.h>
#include <wx/datstrm.h>
#include <wx/grid.h>
#include <wx/statline.h>
#include <wx/busyinfo.h>

#include <wex/extgrid.h>
#include <wex/exttext.h>
#include <wex/radiochoice.h>
#include <wex/utils.h>
#ifdef __WXMSW__
#include <wex/ole/excelauto.h>
#endif

#include "simulation.h"
#include "casewin.h"
#include "case.h"
#include "main.h"
#include "excelexch.h"


ExcelExchange::ExcelExchange()
{
	Enabled = false;
}


void ExcelExchange::Copy(ExcelExchange &rhs)
{
	Enabled = rhs.Enabled;
	ExcelFile = rhs.ExcelFile;

	Vars.clear();
	for (size_t i = 0; i < rhs.Vars.size(); i++)
	{
		ExchVar ev;
		ev.Name = rhs.Vars[i].Name;
		ev.Range = rhs.Vars[i].Range;
		ev.Type = rhs.Vars[i].Type;
		Vars.push_back(ev);
	}

	Summary.clear();
	for (size_t i = 0; i < rhs.Summary.size(); i++)
	{
		Captured c;
		c.Name = rhs.Summary[i].Name;
		c.Range = rhs.Summary[i].Range;
		c.Value = rhs.Summary[i].Value;
		Summary.push_back(c);
	}
}


void ExcelExchange::Write( wxOutputStream &_O )
{
	wxDataOutputStream out(_O);

	out.Write8( 0x4c );
	out.Write8( 2 );

	out.Write8( Enabled ? 1 : 0 );
	out.WriteString( ExcelFile );

	out.Write32( Vars.size() );
	for( size_t i=0;i<Vars.size();i++ )
	{
		out.WriteString( Vars[i].Name );
		out.WriteString( Vars[i].Range );
		out.Write8( (wxUint8)Vars[i].Type );
	}
	
	out.Write32( Summary.size() );
	for( size_t i=0;i<Summary.size();i++ )
	{
		out.WriteString( Summary[i].Name );
		out.WriteString( Summary[i].Range );
		out.WriteString( Summary[i].Value );
	}

	out.Write8( 0x4c );
}

bool ExcelExchange::Read( wxInputStream &_I )
{
	wxDataInputStream in(_I);

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();
	
	Enabled = (in.Read8() != 0);
	ExcelFile = in.ReadString();

	Vars.clear();
	size_t n = in.Read32();
	for( size_t i=0;i<n;i++ )
	{
		ExchVar ev;
		ev.Name = in.ReadString();
		ev.Range = in.ReadString();
		ev.Type = in.Read8();
		Vars.push_back( ev );
	}
	
	if ( ver >= 2 )
	{
		n = in.Read32();
		Summary.clear();
		for( size_t i=0;i<n;i++ )
		{
			Captured c;
			c.Name = in.ReadString();
			c.Range = in.ReadString();
			c.Value = in.ReadString();
			Summary.push_back( c );
		}
	}

	return in.Read8() == code;
}

enum {
  ID_btnSelectExcelFile=wxID_HIGHEST+492,
  ID_txtExcelFile,
  ID_txtExcelRange,
  ID_rbgToFrom,
  ID_chkEnableExch,
  ID_lstVariables,
  ID_btnRemoveAll,
  ID_btnRemoveVar,
  ID_btnAddVar };

class ExcelExchDialog : public wxDialog
{
	wxExtTextCtrl *txtExcelFile;
	wxExtTextCtrl *txtExcelRange;
	wxRadioChoice *rbgToFrom;
	wxCheckBox *chkEnableExch;
	wxListBox *lstVariables;

	Case *m_case;
	ConfigInfo *m_ci;
	CaseWindow *m_caseWin;
	ExcelExchange m_exch;

public:

	ExcelExchDialog(wxWindow *parent, int id)
		 : wxDialog( parent, id, "Excel Exchange", wxDefaultPosition, wxScaleSize(600,350),
		 wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
	{
		//SetBackgroundColour( *wxWHITE );

		txtExcelFile = new wxExtTextCtrl(this, ID_txtExcelFile, "<Select an Excel file>");
		txtExcelRange = new wxExtTextCtrl(this, ID_txtExcelRange, "A1");
			
		lstVariables = new wxListBox(this, ID_lstVariables, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_SINGLE);
		
		chkEnableExch = new wxCheckBox(this, ID_chkEnableExch, "Turn on Excel Exchange for the base case simulation");

		wxArrayString _data_rbgToFrom;
		_data_rbgToFrom.Add("Send variable value to Excel");
		_data_rbgToFrom.Add("Capture variable value from Excel");
		rbgToFrom = new wxRadioChoice(this, ID_rbgToFrom, wxDefaultPosition, wxDefaultSize);
		rbgToFrom->Add( _data_rbgToFrom);
			
		wxBoxSizer *sz1 = new wxBoxSizer( wxHORIZONTAL );
		sz1->Add( new wxStaticText( this, wxID_ANY, "Excel file:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		sz1->Add( txtExcelFile, 1, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		sz1->Add( new wxButton(this, ID_btnSelectExcelFile, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		
		wxBoxSizer *szvvl = new wxBoxSizer( wxVERTICAL );
		szvvl->Add( new wxStaticText( this, wxID_ANY, "Variables:" ), 0, wxALL|wxEXPAND, 4 );
		szvvl->Add( new wxButton(this, ID_btnAddVar, "Add...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 1 );
		szvvl->Add( new wxButton(this, ID_btnRemoveVar, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 1 );
		szvvl->Add( new wxButton(this, ID_btnRemoveAll, "Clear all", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 1 );
		szvvl->AddStretchSpacer();

		wxBoxSizer *szvvr = new wxBoxSizer( wxVERTICAL );
		szvvr->Add( new wxStaticText( this, wxID_ANY, "Excel range:"), 0, wxALL, 4 );
		szvvr->Add( txtExcelRange, 0, wxALL|wxEXPAND, 4 );
		szvvr->Add( rbgToFrom, 0, wxALL|wxEXPAND, 4 );
		szvvr->AddStretchSpacer();

		wxBoxSizer *sz2 = new wxBoxSizer( wxHORIZONTAL );
		sz2->Add( szvvl, 0, wxALL|wxEXPAND, 2 );
		sz2->Add( lstVariables, 1, wxALL|wxEXPAND, 2 );
		sz2->Add( szvvr, 0, wxALL|wxEXPAND, 2 );

		wxStaticText *notes = new wxStaticText( this, wxID_ANY, 
			"The Excel range can be a cell reference (such as C7) or a named range, except for annual schedule variables,\n"
			"which require a cell reference (A1:A12, B10:G10) rather than a named range.\n"
			"You can specify an Excel file with no path if the workbook is in the same folder as the SAM file." );
		notes->Wrap( 590 );
		notes->SetForegroundColour( wxColour(0,0,0) );
		
		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( chkEnableExch, 0, wxALL|wxEXPAND, 10 );
		sizer->Add( new wxStaticLine( this, wxID_ANY ), 0, wxALL|wxEXPAND, 5 );
		sizer->Add( sz1, 0, wxALL|wxEXPAND, 5 );
		sizer->Add( sz2, 1, wxALL|wxEXPAND, 5 );
		sizer->Add( notes, 0, wxLEFT|wxRIGHT|wxEXPAND, 10 );
		sizer->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );
		SetSizerAndFit( sizer );
	}
	
	~ExcelExchDialog()
	{
	}
	
	void Set( CaseWindow *cwin, const ExcelExchange &ex )
	{
		m_caseWin = cwin;
		m_case = cwin->GetCase();
		m_ci = m_case->GetConfiguration();

		m_exch = ex;
	
		rbgToFrom->SetSelection(0);
		UpdateFromInfo();
		UpdateUI();
	}

	void Get( ExcelExchange &ex )
	{
		ex = m_exch;
	}

	void UpdateFromInfo(int sel = -1)
	{
		if (!m_case)
			return;

		chkEnableExch->SetValue( m_exch.Enabled );
		txtExcelFile->ChangeValue( m_exch.ExcelFile );
			
		lstVariables->Freeze();
		lstVariables->Clear();
		for (size_t i=0;i<m_exch.Vars.size();i++)
		{
			wxString lab = m_ci->Variables.Label( m_exch.Vars[i].Name );
			if ( lab.IsEmpty() ) lab = "{" + m_exch.Vars[i].Name + "}";
			lstVariables->Append( lab );
		}
		lstVariables->Thaw();

		if (lstVariables->GetCount() > 0)
		{
			int selidx = 0;

			if (sel>=0)
			{
				selidx = sel;
				if (selidx >= (int)lstVariables->GetCount())
					selidx = ((int)lstVariables->GetCount())-1;

				if (sel < 0) sel = 0;
			}

			lstVariables->Select(selidx);

			DoSelectVar();
		}

		chkEnableExch->SetValue( m_exch.Enabled );
	}

	void OnSelVar(wxCommandEvent &)
	{
		DoSelectVar();
	}

	void OnDataChange(wxCommandEvent &evt)
	{
		if (!m_case || !m_caseWin)
			return;
		
		int idx = lstVariables->GetSelection();
	
		switch(evt.GetId())
		{
		case ID_txtExcelFile:
			m_exch.ExcelFile = txtExcelFile->GetValue();
			break;
		case ID_txtExcelRange:
			{
				if (idx < 0 || idx >= (int)m_exch.Vars.size())
					return;
				m_exch.Vars[idx].Range = txtExcelRange->GetValue();
			}
		break;
		case ID_rbgToFrom:
			{
				if (idx < 0 || idx >= (int)m_exch.Vars.size())
					return;
				unsigned long vf = m_ci->Variables.Flags(m_exch.Vars[idx].Name);
				if ( vf & VF_CALCULATED || vf & VF_LIBRARY )
				{
					wxMessageBox("'" + m_ci->Variables.Label( m_exch.Vars[idx].Name ) + "' is a calculated value or a library selection, and as a result its value cannot be captured from Excel.");
					rbgToFrom->SetSelection(0);
				}
				else
					m_exch.Vars[idx].Type = (rbgToFrom->GetSelection()==0) 
						? ExcelExchange::SEND_TO : ExcelExchange::CAPTURE_FROM;
			}
			break;
		case ID_chkEnableExch:
			m_exch.Enabled = chkEnableExch->GetValue();
			break;
		}
	}

	void DoSelectVar()
	{
		int idx = lstVariables->GetSelection();
		if (idx < 0)
		{
			txtExcelFile->SetValue("");
			txtExcelRange->SetValue("");
		}
		else if (m_case)
		{
			txtExcelRange->SetValue( m_exch.Vars[idx].Range );
			
			unsigned long vf = m_ci->Variables.Flags( m_exch.Vars[idx].Name );
			if ( vf & VF_CALCULATED || vf & VF_LIBRARY )
				m_exch.Vars[idx].Type = ExcelExchange::SEND_TO;

			rbgToFrom->SetSelection( m_exch.Vars[idx].Type == ExcelExchange::SEND_TO  ? 0 : 1 );
		}
	
		UpdateUI();
	}

	void UpdateUI()
	{
		int idx = lstVariables->GetSelection();
		rbgToFrom->Enable( idx >= 0);
		txtExcelRange->Enable( idx >= 0 );		
	}

	bool ChooseVarsDialog( wxArrayString &list )
	{
		wxArrayString names;
		wxArrayString labels;

		for( VarInfoLookup::iterator it = m_ci->Variables.begin();
			it != m_ci->Variables.end();
			++it )
		{
			VarInfo &vi = *(it->second);

			if ( !vi.Label.IsEmpty()
				&& !(vi.Flags & VF_INDICATOR) )
			{
				if ( vi.Type == VV_NUMBER && vi.IndexLabels.size() > 0 )
					continue;

				wxString label;
				if ( !vi.Group.IsEmpty() )
					label = vi.Group + "/" + vi.Label;
				else
					label = vi.Label;

				if ( vi.Flags & VF_CALCULATED || vi.Flags & VF_LIBRARY )
					label += " (Send only)";
				
				names.Add( it->first );
				labels.Add( label );
			}
		}

		wxSortByLabels(names, labels);
		return SelectVariableDialog::Run("Choose Excel Exchange Variables", names, labels, list);
	}

	void OnAddVar(wxCommandEvent &)
	{
		wxArrayString varlist;
		for (size_t i=0;i<m_exch.Vars.size();i++)
			varlist.Add( m_exch.Vars[i].Name );

		if (!ChooseVarsDialog(varlist))
			return;

		// remove those no longer selected
		wxArrayString toremove;
		for (size_t i=0;i<m_exch.Vars.size();i++)
			if (varlist.Index( m_exch.Vars[i].Name ) < 0)
				toremove.Add( m_exch.Vars[i].Name );

		for (size_t i=0;i<toremove.Count();i++)
		{
			for( size_t k=0;k<m_exch.Vars.size();k++ )
			{
				if ( toremove[i] == m_exch.Vars[k].Name )
				{
					m_exch.Vars.erase( m_exch.Vars.begin() + k );
					break;
				}
			}
		}	

		int sel_idx = lstVariables->GetSelection();

		// add any not currently in list
		for (size_t i=0;i<varlist.Count();i++)
		{
			int idx = -1;
			for( size_t k=0;k<m_exch.Vars.size();k++ )
				if ( m_exch.Vars[k].Name == varlist[i] )
					idx = k;

			if (idx < 0)
			{
				ExcelExchange::ExchVar x;
				x.Name = varlist[i];
				x.Range = "A1";
				x.Type = ExcelExchange::SEND_TO;			

				m_exch.Vars.push_back( x );
				sel_idx = m_exch.Vars.size()-1;
			}
		}

		UpdateFromInfo( sel_idx );
	}

	void OnRemoveVar(wxCommandEvent &)
	{
		int idx = lstVariables->GetSelection();
		if (idx < 0 || idx >= (int)m_exch.Vars.size())
			return;

		m_exch.Vars.erase( m_exch.Vars.begin() + idx );
		UpdateFromInfo(idx);
	}

	void OnRemoveAll(wxCommandEvent &)
	{
		if (!m_case) return;

		if (wxYES==wxMessageBox("Really remove all Excel exchange variables?", "Query", wxYES_NO))
		{
			m_exch.Vars.clear();
			UpdateFromInfo();
		}
	}

	void OnExcelFile(wxCommandEvent &)
	{
		if (!m_case) return;

		wxFileDialog dlg(this, "Choose an Excel File", "", "", "Excel Files (*.xls *.xlm *.xlsx *.xlsm)|*.xls;*.xlm;*.xlsx;*.xlsm");
		if (dlg.ShowModal() == wxID_OK)
		{
			wxString fn = dlg.GetPath();
			if (wxPathOnly(fn) == wxPathOnly( SamApp::Window()->GetProjectFileName() ) )
				fn = dlg.GetFilename();

			txtExcelFile->SetValue( fn );
			m_exch.ExcelFile = fn;
		}
	}

	void OnHelp( wxCommandEvent & )
	{
		SamApp::ShowHelp( "excel_exchange");
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( ExcelExchDialog, wxDialog )
	EVT_BUTTON( ID_btnAddVar, ExcelExchDialog::OnAddVar)
	EVT_BUTTON( ID_btnRemoveVar, ExcelExchDialog::OnRemoveVar)
	EVT_BUTTON( ID_btnRemoveAll, ExcelExchDialog::OnRemoveAll)
	EVT_BUTTON( ID_btnSelectExcelFile, ExcelExchDialog::OnExcelFile)
	EVT_LISTBOX( ID_lstVariables, ExcelExchDialog::OnSelVar)
	EVT_TEXT( ID_txtExcelRange, ExcelExchDialog::OnDataChange)
	EVT_TEXT( ID_txtExcelFile, ExcelExchDialog::OnDataChange)
	EVT_RADIOBUTTON( ID_rbgToFrom, ExcelExchDialog::OnDataChange)
	EVT_CHECKBOX( ID_chkEnableExch, ExcelExchDialog::OnDataChange )
	EVT_BUTTON( wxID_HELP, ExcelExchDialog::OnHelp )
END_EVENT_TABLE()

bool ExcelExchange::ShowExcelExchangeDialog( ExcelExchange &exch, CaseWindow *cw )
{
	ExcelExchDialog dialog(  SamApp::Window(), wxID_ANY );
	dialog.CenterOnParent();
	dialog.Set( cw, exch );
	if ( wxID_OK == dialog.ShowModal() )
	{
		dialog.Get( exch );
		return true;
	}
	else
		return false;
}


#define ALPHA_MIN 'a'
#define ALPHA_MAX 'z'
#define LASTCHAR(x) tolower(x[x.Len()-1])

#ifdef  __WXMSW__

static wxString ConvertToBase26(unsigned int val)
{
	wxString result;
	do
	{
		result.Prepend((char)( (val-1) % 26 + 'A'));
		val = (val-1)/26;
	}while(val>0);
	return result;
}

static unsigned int ConvertFromBase26(const wxString &val)
{
	unsigned int result = 0;
	const char *cval = val.c_str();
	
	while (cval && *cval)
		result = result*26 + toupper(*cval++)-'A'+1;

	return result;
}

wxArrayString ExcelExchange::EnumerateAlphaIndex(const wxString &_start, const wxString &_end)
{
	unsigned int istart = ConvertFromBase26(_start);
	unsigned int iend = ConvertFromBase26(_end);
	
	wxArrayString values;
	while (istart <= iend)
	{
		values.Add( ConvertToBase26(istart) );
		istart++;
	}
	return values;
}

bool ExcelExchange::ParseAndCaptureRange( const wxString &range, wxString &val, wxExcelAutomation &xl )
{
	val.Empty();

	int i;
	int colonpos = range.Find(':');
	if (colonpos > 0 && colonpos < (int)range.Len()-1)
	{
		wxArrayString ranges;
		wxArrayString values;

		if (isdigit(range[colonpos+1]))
		{
			// numerical range,i.e A1:3

			wxString s_colname; // collect the column name
			for (i=0;i<(int)range.Len() && isalpha(range[i]);i++)
				s_colname += range[i];

			wxString s_startrow,s_endrow; // collect the start/end rows
			for (i;i<(int)range.Len() && isdigit(range[i]);i++)
				s_startrow += range[i];

			i++; // skip colon

			for (i;i<(int)range.Len() && isdigit(range[i]);i++)
				s_endrow += range[i];

			int startrow = atoi(s_startrow.c_str());
			int endrow = atoi(s_endrow.c_str());

			if (startrow < 1) startrow = 1;
			if (endrow < startrow) endrow = startrow;

			bool ok = true;
			for (i=startrow;i<=endrow && ok;i++)
			{
				wxString cur_range = s_colname + wxString::Format("%d",i);
				wxString data;
				ok = xl.GetRangeValue(cur_range, data);
				if (ok)	values.Add( data );
			}

			if (ok) val = wxJoin(values, ',');

			return ok;
		}
		else
		{
			// alphabetical range, i.e. A:EE3
			wxString s_startcol, s_endcol, s_row;
			for (i=0;i<(int)range.Len() && isalpha(range[i]);i++)
				s_startcol += range[i];

			i++; // skip colon;
			for (i;i<(int)range.Len() && isalpha(range[i]);i++)
				s_endcol += range[i];

			for (i;i<(int)range.Len() && isdigit(range[i]);i++)
				s_row += range[i];

			wxArrayString colnames = EnumerateAlphaIndex(s_startcol, s_endcol);

			bool ok = true;
			for (i=0;i<(int)colnames.Count();i++)
			{
				wxString cur_range = colnames[i] + s_row;
				wxString data;
				ok = xl.GetRangeValue( cur_range, data );
				if (ok)	values.Add( data );
			}

			if (ok) val = wxJoin(values, ',');
			return ok;
		}
	}
	else
		return xl.GetRangeValue(range, val);
}

#endif



int ExcelExchange::RunExcelExchange( ExcelExchange &ex, VarTable &inputs, Simulation *sim )
{
	ex.Summary.clear();

#ifdef __WXMSW__
	if ( !ex.Enabled || ex.Vars.size() == 0 ) return 0;


	wxExcelAutomation xl;
	wxArrayString msgs;
	
	wxBusyInfo busy("Exchanging data with Excel...");

	// open the referenced excel file
	wxString fn = ex.ExcelFile;
	wxString sam_file_path = wxPathOnly( SamApp::Window()->GetProjectFileName() );
	if (wxPathOnly(fn) == "" && sam_file_path != "")
		fn = sam_file_path + "/" + fn;

	fn.Replace("\\", "/");
	fn.Replace("//", "/");
	fn.Replace("${SAMPLES}", SamApp::GetRuntimePath() + "/samples");

	if (!wxFileExists(fn))
	{
		wxMessageBox("The referenced Excel file does not exist:\n\n" + fn);
		return -1;
	}

	if (!xl.StartExcel() || !xl.OpenFile( fn ))
	{
		wxMessageBox("Excel Exchange Error:\n\n" + xl.GetLastError());
		return -1;
	}

	xl.Show(true); // for now show the spread sheets

	// handle all 'sent items'
	for (size_t i=0;i<ex.Vars.size();i++)
	{
		if ( ex.Vars[i].Type != SEND_TO )
			continue; // skip capture from items

		VarValue *vv = inputs.Get( ex.Vars[i].Name );
		if ( !vv )
		{
			msgs.Add("Could not find variable " + ex.Vars[i].Name + " so skipping");
			continue;
		}

		if ( !xl.SetRangeValue( ex.Vars[i].Range,  vv->AsString() ) )
		{
			msgs.Add("Could not set range " + ex.Vars[i].Range + " to value " + vv->AsString() );
			continue;
		}
	}

	int ncaptured = 0;

	// handle all 'capture items'
	for (size_t i=0;i<ex.Vars.size();i++)
	{
		if ( ex.Vars[i].Type != CAPTURE_FROM )
			continue; // skip sent to items
		
		VarValue *v = inputs.Get( ex.Vars[i].Name );
		if (!v)
		{
			msgs.Add("Could not find TARGET variable " + ex.Vars[i].Name + " so skipping");
			continue;
		}

		wxString sval;
		if (!ParseAndCaptureRange( ex.Vars[i].Range, sval, xl ))
		{
			msgs.Add("Could not GET range " + ex.Vars[i].Range);
			continue;
		}

		VarValue vval;
		if ( !VarValue::Parse( v->Type(), sval, vval ) )
		{
			msgs.Add("Invalid value '" + sval + "' at range '" + ex.Vars[i].Range + "' for variable '" + ex.Vars[i].Name);
			continue;
		}
		else
		{
			// override the variable value in the simulation object
			sim->Override( ex.Vars[i].Name, vval );

			// update ui input value per Paul 3/6/15
			Case *c = sim->GetCase();
			if (VarValue *vv = c->Values().Get(ex.Vars[i].Name))
			{
				vv->Copy(vval);
				c->VariableChanged(ex.Vars[i].Name);
			}

			// log successful exchange transaction
			// will be shown in results summary window

			Captured cc;
			cc.Name = ex.Vars[i].Name;
			cc.Value = sval;
			cc.Range = ex.Vars[i].Range;
			ex.Summary.push_back(cc);
		}

		ncaptured++;
	}

	xl.CloseAllNoSave();
	xl.QuitExcel();
		
	// notify errors
	if (msgs.Count() > 0)
	{
		wxMessageBox("Excel Exchange did not finish successfully:\n\n" + wxJoin(msgs, '\n'));
		return ncaptured;
	}

	return ncaptured;
#else
	return 0;
#endif
}
