#include <wx/dialog.h>
#include <wx/datstrm.h>
#include <wx/grid.h>
#include <wx/statline.h>

#include <wex/extgrid.h>
#include <wex/exttext.h>
#include <wex/radiochoice.h>
#include <wex/utils.h>

#include "simulation.h"
#include "casewin.h"
#include "case.h"
#include "main.h"
#include "excelexch.h"


ExcelExchange::ExcelExchange()
{
	Enabled = false;
}

void ExcelExchange::Write( wxOutputStream &_O )
{
	wxDataOutputStream out(_O);

	out.Write8( 0x4c );
	out.Write8( 1 );

	out.Write8( Enabled ? 1 : 0 );
	out.WriteString( ExcelFile );

	out.Write32( Vars.size() );
	for( size_t i=0;i<Vars.size();i++ )
	{
		out.WriteString( Vars[i].Name );
		out.WriteString( Vars[i].Range );
		out.Write8( Vars[i].Type );
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
		 : wxDialog( parent, id, "Excel Exchange", wxDefaultPosition, wxSize(600,350),
		 wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
	{
		txtExcelFile = new wxExtTextCtrl(this, ID_txtExcelFile, "<Select an Excel file>", wxPoint(303,30), wxSize(334,21));
		txtExcelRange = new wxExtTextCtrl(this, ID_txtExcelRange, "A1", wxPoint(417,132), wxSize(121,21));
			
		lstVariables = new wxListBox(this, ID_lstVariables, wxPoint(111,30), wxSize(182,123), 0, 0, wxLB_SINGLE);
		
		chkEnableExch = new wxCheckBox(this, ID_chkEnableExch, "Turn on Excel Exchange for base case simulation");

		wxArrayString _data_rbgToFrom;
		_data_rbgToFrom.Add("Send Variable Value To Excel Range");
		_data_rbgToFrom.Add("Capture Variable Value From Excel Range");
		rbgToFrom = new wxRadioChoice(this, ID_rbgToFrom, wxPoint(303,84), wxSize(332,44));
		rbgToFrom->Add( _data_rbgToFrom);
			
		wxBoxSizer *sz1 = new wxBoxSizer( wxHORIZONTAL );
		sz1->Add( new wxStaticText( this, wxID_ANY, "Excel file:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		sz1->Add( txtExcelFile, 1, wxALL, 4 );
		sz1->Add( new wxButton(this, ID_btnSelectExcelFile, "Browse..." ), 0, wxALL, 4 );
		
		wxBoxSizer *szvvl = new wxBoxSizer( wxVERTICAL );
		szvvl->Add( new wxStaticText( this, wxID_ANY, "Variables:" ), 0, wxALL|wxEXPAND, 4 );
		szvvl->Add( new wxButton(this, ID_btnAddVar, "Add..."), 0, wxALL|wxEXPAND, 2 );
		szvvl->Add( new wxButton(this, ID_btnRemoveVar, "Remove"), 0, wxALL|wxEXPAND, 2 );
		szvvl->Add( new wxButton(this, ID_btnRemoveAll, "Clear All"), 0, wxALL|wxEXPAND, 2 );
		szvvl->AddStretchSpacer();

		wxBoxSizer *szvvr = new wxBoxSizer( wxVERTICAL );
		szvvr->Add( new wxStaticText( this, wxID_ANY, "Excel range:"), 0, wxALL, 4 );
		szvvr->Add( txtExcelRange, 0, wxALL|wxEXPAND, 4 );
		szvvr->Add( rbgToFrom, 0, wxALL|wxEXPAND, 4 );
		szvvr->AddStretchSpacer();

		wxBoxSizer *sz2 = new wxBoxSizer( wxHORIZONTAL );
		sz2->Add( szvvl, 0, wxALL|wxEXPAND, 0 );
		sz2->Add( lstVariables, 1, wxALL|wxEXPAND, 0 );
		sz2->Add( szvvr, 0, wxALL|wxEXPAND, 0 );

		wxStaticText *notes = new wxStaticText( this, wxID_ANY, 
			"If you specify an Excel file name with no path, SAM searches the folder containing the SAM file (*.sam).\n\n"
			"Excel Range can be a cell reference (such as C7) or a named range.\n\n"
			"To exchange values with an annual schedule variable, use a reference (A1:12, B:G1) rather than a named range.\n\n" );
		notes->Wrap( 450 );
		
		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( chkEnableExch, 0, wxALL|wxEXPAND, 10 );
		sizer->Add( new wxStaticLine( this, wxID_ANY ), 0, wxALL|wxEXPAND, 3 );
		sizer->Add( sz1, 0, wxALL|wxEXPAND, 0 );
		sizer->Add( sz2, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( notes, 0, wxALL|wxEXPAND, 5 );
		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );
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

	void OnSelVar(wxCommandEvent &evt)
	{
		DoSelectVar();
	}

	void OnDataChange(wxCommandEvent &evt)
	{
		if (!m_case || !m_caseWin)
			return;
		
		int idx = lstVariables->GetSelection();
		if (idx < 0 || idx >= m_exch.Vars.size())
			return;
	
		switch(evt.GetId())
		{
		case ID_txtExcelFile:
			m_exch.ExcelFile = txtExcelFile->GetValue();
			break;
		case ID_txtExcelRange:
			m_exch.Vars[idx].Range = txtExcelRange->GetValue();
			break;
		case ID_rbgToFrom:
			{
				unsigned long vf = m_ci->Variables.Flags( m_exch.Vars[idx].Name );
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
		txtExcelFile->Enable( idx >= 0);
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
		return m_caseWin->ShowSelectVariableDialog("Choose Excel Exchange Variables", names, labels, list);
	}

	void OnAddVar(wxCommandEvent &evt)
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

	void OnRemoveVar(wxCommandEvent &evt)
	{
		int idx = lstVariables->GetSelection();
		if (idx < 0 || idx >= m_exch.Vars.size())
			return;

		m_exch.Vars.erase( m_exch.Vars.begin() + idx );
		UpdateFromInfo(idx);
	}

	void OnRemoveAll(wxCommandEvent &evt)
	{
		if (!m_case) return;

		if (wxYES==wxMessageBox("Really remove all Excel exchange variables?", "Query", wxYES_NO))
		{
			m_exch.Vars.clear();
			UpdateFromInfo();
		}
	}

	void OnExcelFile(wxCommandEvent &evt)
	{
		if (!m_case) return;

		wxFileDialog dlg( this, "Choose an Excel File", "", "", "Excel Files (*.xls *.xlsx)|*.xls;*.xlsx");
		if (dlg.ShowModal() == wxID_OK)
		{
			wxString fn = dlg.GetPath();
			if (wxPathOnly(fn) == wxPathOnly( SamApp::Window()->GetProjectFileName() ) )
				fn = dlg.GetFilename();

			txtExcelFile->SetValue( fn );
			m_exch.ExcelFile = fn;
		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( ExcelExchDialog, wxDialog )
	EVT_BUTTON( ID_btnAddVar, OnAddVar)
	EVT_BUTTON( ID_btnRemoveVar, OnRemoveVar)
	EVT_BUTTON( ID_btnRemoveAll, OnRemoveAll)
	EVT_BUTTON( ID_btnSelectExcelFile, OnExcelFile)
	EVT_LISTBOX( ID_lstVariables, OnSelVar)
	EVT_TEXT( ID_txtExcelRange, OnDataChange)
	EVT_TEXT( ID_txtExcelFile, OnDataChange)
	EVT_RADIOBUTTON( ID_rbgToFrom, OnDataChange)
	EVT_CHECKBOX( ID_chkEnableExch, OnDataChange )
END_EVENT_TABLE()

bool ExcelExchange::ShowExcelExchangeDialog( ExcelExchange *exch, CaseWindow *cw )
{
	//wxFrame *trans = CreateTransparentOverlay( SamApp::Window() );

	ExcelExchDialog dialog(  SamApp::Window(), wxID_ANY );
	dialog.CenterOnParent();
	dialog.Set( cw, *exch );
	if ( wxID_OK == dialog.ShowModal() )
	{
		dialog.Get( *exch );
		//trans->Destroy();
		return true;
	}
	else
	{
		//trans->Destroy();
		return false;
	}
}

bool ExcelExchange::RunExcelExchange( ExcelExchange *exch, Simulation *sim )
{
	return false;
}
