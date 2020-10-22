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

#include <wx/wx.h>
#include <wx/splitter.h>
#include <wx/simplebook.h>
#include <wx/statline.h>
#include <wx/tokenzr.h>
#include <wx/dir.h>

#include <wex/exttree.h>
#include <wex/exttext.h>
#include <wex/metro.h>
#include <wex/lkscript.h>
#include <wex/extgrid.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>

#include <wex/icons/cirplus.cpng>
#include <wex/icons/cirminus.cpng>

#include <wex/utils.h>

#include "reports.h"
#include "results.h"
#include "main.h"
#include "case.h"
#include "casewin.h"
#include "ipagelist.h"
#include "simulation.h"
#include "parametric.h"
#include "stochastic.h"
#include "p50p90.h"
#include "macro.h"

#include "../resource/graph.cpng"

class CollapsePaneCtrl : public wxPanel
{
	bool m_state;
	wxMetroButton *m_button;
	wxStaticText *m_label;
	static wxBitmap m_bitMinus, m_bitPlus;
public:
	CollapsePaneCtrl( wxWindow *parent, int id, const wxString &label )
		: wxPanel( parent, id )
	{
		if (!m_bitMinus.IsOk() )
		{
			m_bitMinus = wxBITMAP_PNG_FROM_DATA( cirminus );
			m_bitPlus = wxBITMAP_PNG_FROM_DATA( cirplus );
		}

		SetBackgroundColour( wxColour(243,243,243) );
		m_state = false;
		m_button = new wxMetroButton( this, wxID_ANY, wxEmptyString, m_bitMinus );

		m_label = new wxStaticText( this, wxID_ANY, label );
		m_label->SetFont( wxMetroTheme::Font( wxMT_LIGHT, 12 ) );
		
		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( m_button, 0, wxALL|wxEXPAND, 0 );
		sizer->Add( m_label, 1, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		SetSizer(sizer);
	}
	
	void OnButton( wxCommandEvent & )
	{
		SetValue( !m_state );
		wxCommandEvent evt( wxEVT_CHECKBOX, GetId() );
		evt.SetEventObject( this );
		ProcessEvent( evt );
	}

	void SetValue( bool b )
	{
		m_state = b;
		m_button->SetBitmap( m_state ? m_bitMinus : m_bitPlus );
		Layout();
	}

	bool GetValue() const { return m_state; }

	DECLARE_EVENT_TABLE();
};

wxBitmap CollapsePaneCtrl::m_bitMinus;
wxBitmap CollapsePaneCtrl::m_bitPlus;

BEGIN_EVENT_TABLE( CollapsePaneCtrl, wxPanel )
	EVT_BUTTON( wxID_ANY, CollapsePaneCtrl::OnButton )
END_EVENT_TABLE()


enum { ID_INPUTPAGELIST = wxID_HIGHEST + 142,
	ID_SIMULATE, ID_RESULTSPAGE, ID_ADVANCED, ID_PARAMETRICS, ID_STOCHASTIC, ID_P50P90, ID_MACRO,
	ID_COLLAPSE,ID_EXCL_BUTTON, ID_EXCL_TABLIST, ID_EXCL_OPTION, ID_EXCL_OPTION_MAX=ID_EXCL_OPTION+25,
	ID_PAGES, ID_BASECASE_PAGES };

BEGIN_EVENT_TABLE( CaseWindow, wxSplitterWindow )
	EVT_BUTTON( ID_SIMULATE, CaseWindow::OnCommand )
	EVT_BUTTON( ID_RESULTSPAGE, CaseWindow::OnCommand )
	EVT_BUTTON( ID_ADVANCED, CaseWindow::OnCommand )
	EVT_BUTTON( ID_PARAMETRICS, CaseWindow::OnCommand )
	EVT_BUTTON( ID_STOCHASTIC, CaseWindow::OnCommand )
	EVT_BUTTON( ID_P50P90, CaseWindow::OnCommand )
	EVT_BUTTON( ID_MACRO, CaseWindow::OnCommand )
	EVT_MENU( ID_PARAMETRICS, CaseWindow::OnCommand )
	EVT_MENU( ID_STOCHASTIC, CaseWindow::OnCommand )
	EVT_MENU( ID_P50P90, CaseWindow::OnCommand )
	EVT_MENU( ID_MACRO, CaseWindow::OnCommand )
	EVT_LISTBOX( ID_INPUTPAGELIST, CaseWindow::OnCommand )
	EVT_BUTTON( ID_EXCL_BUTTON, CaseWindow::OnCommand )
	EVT_CHECKBOX( ID_COLLAPSE, CaseWindow::OnCommand )
	EVT_MENU_RANGE( ID_EXCL_OPTION, ID_EXCL_OPTION_MAX, CaseWindow::OnCommand )
	EVT_LISTBOX( ID_EXCL_TABLIST, CaseWindow::OnCommand )

	EVT_NOTEBOOK_PAGE_CHANGED( ID_PAGES, CaseWindow::OnSubNotebookPageChanged )
	EVT_NOTEBOOK_PAGE_CHANGED( ID_BASECASE_PAGES, CaseWindow::OnSubNotebookPageChanged )
END_EVENT_TABLE()

enum { PG_INPUTS, PG_RESULTS, PG_PARAMETRICS, PG_STOCHASTIC, PG_P50P90, PG_MACROS };

CaseWindow::CaseWindow( wxWindow *parent, Case *c )
	: wxSplitterWindow( parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER | wxSP_LIVE_UPDATE | wxSP_3DSASH ),
	m_case( c )
{
	m_case->AddListener( this );

	m_pageNote = 0;
	m_currentGroup = 0;

	wxColour lafore( *wxWHITE ), laback( 100,100,100 );

	wxPanel *left_panel = new wxPanel( this );
	left_panel->SetBackgroundColour( laback );
	
	m_inputPageList = new InputPageList( left_panel, ID_INPUTPAGELIST );
	m_inputPageList->SetCaseWindow( this );
	m_inputPageList->SetBackgroundColour( wxColour(243,243,243) );

	wxFont lafont( *wxNORMAL_FONT );
	lafont.SetWeight( wxFONTWEIGHT_BOLD );
	m_configLabel = new wxStaticText( left_panel, wxID_ANY, "-technology-" );
	m_configLabel->SetBackgroundColour( laback );
	m_configLabel->SetForegroundColour( lafore );
	m_configLabel->SetFont( lafont );
	

	
	m_simButton = new wxMetroButton( left_panel, ID_SIMULATE, "Simulate", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW );
	m_simButton->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 14) );
	m_resultsButton = new wxMetroButton( left_panel, ID_RESULTSPAGE, wxEmptyString, wxBITMAP_PNG_FROM_DATA( graph ) );

	wxBoxSizer *szhl = new wxBoxSizer( wxHORIZONTAL );
	szhl->Add( m_simButton, 1, wxALL|wxEXPAND, 0 );
	szhl->Add( m_resultsButton, 0, wxALL|wxEXPAND, 0 );

	wxSizer *szsims = new wxGridSizer(2, 0, 0);
	szsims->Add( new wxMetroButton( left_panel, ID_PARAMETRICS, "Parametrics" ), 0, wxALL|wxEXPAND, 0 );
	szsims->Add( new wxMetroButton( left_panel, ID_STOCHASTIC, "Stochastic" ), 0, wxALL|wxEXPAND, 0 );
	szsims->Add( new wxMetroButton( left_panel, ID_P50P90, "P50 / P90" ), 0, wxALL|wxEXPAND, 0 );
	szsims->Add( new wxMetroButton( left_panel, ID_MACRO, "Macros" ), 0, wxALL|wxEXPAND, 0 );

	wxBoxSizer *szvl = new wxBoxSizer( wxVERTICAL );
	szvl->Add( m_configLabel, 0, wxALIGN_CENTER|wxTOP|wxBOTTOM, 3 );
	szvl->Add( m_inputPageList, 1, wxALL|wxEXPAND, 0 );
	szvl->Add( szhl, 0, wxALL|wxEXPAND, 0 );
	szvl->Add( szsims, 0, wxALL|wxEXPAND, 0 );
	left_panel->SetSizer( szvl );

	m_pageFlipper = new wxSimplebook( this, ID_PAGES, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );
	
	m_inputPageScrollWin = new wxScrolledWindow( m_pageFlipper );
	m_inputPageScrollWin->SetBackgroundColour( *wxWHITE );
	
	m_exclPanel = new wxPanel( m_inputPageScrollWin );
	m_exclPanel->SetBackgroundColour( *wxWHITE );
	m_exclPageButton = new wxMetroButton( m_exclPanel, ID_EXCL_BUTTON, "Change...", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW );
	m_exclPageTabList = new wxMetroTabList( m_exclPanel, ID_EXCL_TABLIST, wxDefaultPosition, wxDefaultSize, wxMT_LIGHTTHEME );
	m_exclPageTabList->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 11 ) );
	
	m_exclPanelSizer = new wxBoxSizer( wxHORIZONTAL );
	m_exclPanelSizer->Add( m_exclPageButton, 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	m_exclPanelSizer->Add( m_exclPageTabList, 1, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	m_exclPanelSizer->AddStretchSpacer();
	m_exclPanel->SetSizer( m_exclPanelSizer );
		
	m_pageFlipper->AddPage( m_inputPageScrollWin, "Input Pages", true );

	
	m_baseCaseResults = new ResultsViewer( m_pageFlipper, ID_BASECASE_PAGES );
	m_pageFlipper->AddPage( m_baseCaseResults, "Base Case" );
		
	m_parametrics = new ParametricViewer( m_pageFlipper, m_case );
	m_pageFlipper->AddPage( m_parametrics, "Parametric", false );

	m_stochastic = new StochasticPanel( m_pageFlipper, m_case );
	m_pageFlipper->AddPage( m_stochastic, "Stochastic", false );

	m_p50p90 = new P50P90Form( m_pageFlipper, m_case );
	m_pageFlipper->AddPage( m_p50p90, "P50/P90", false );

	m_macros = new MacroPanel( m_pageFlipper, m_case );
	m_pageFlipper->AddPage( m_macros, "Macros", false );

	double xScale, yScale;
	wxDevicePPIToScale( wxClientDC(this).GetPPI(), &xScale, &yScale );
	
	SetMinimumPaneSize( 50 );
	SplitVertically( left_panel, m_pageFlipper, (int)(210*xScale) );
	
	
	m_pageNote = new PageNote( this );

	// load page note window geometry
	int nw_xrel, nw_yrel, nw_w, nw_h;
	double sf = wxGetScreenHDScale();
	nw_xrel = (int)( wxAtoi( m_case->GetProperty("NoteWindowXRel") ) * sf );
	nw_yrel = (int)( wxAtoi( m_case->GetProperty("NoteWindowYRel") ) * sf );
	nw_w = (int)( wxAtoi( m_case->GetProperty("NoteWindowWidth") ) * sf );
	nw_h = (int)( wxAtoi( m_case->GetProperty("NoteWindowHeight") ) * sf );
	
	wxLogStatus("nw_xrel = %d, nw_yrel = %d  (%d x %d)", nw_xrel, nw_yrel, nw_w, nw_h );

	if (nw_w > 50 && nw_w < 1024 && nw_h > 21 && nw_h < 1024)
		m_pageNote->SetClientSize(nw_w, nw_h);

	if (nw_xrel != 0 && nw_yrel != 0)
	{
		int px,py,pw,ph;
		SamApp::Window()->GetPosition(&px,&py);
		SamApp::Window()->GetSize(&pw,&ph);

		int notex, notey;
		notex = px + nw_xrel;
		notey = py + nw_yrel;

		if (notex < 0) notex = 0;
		if (notey < 21) notey = 21;

		if (notex > px+pw) notex = px+pw-100;
		if (notey > py+ph) notey = py+ph-100;

		m_pageNote->SetPosition(wxPoint(notex,notey));
	}
	

	UpdateConfiguration();

	// load graphs and perspective from case
	std::vector<Graph> gl;
	m_case->GetGraphs( gl );
	
	
	if (m_case->GetConfiguration()->Technology == "Wind Power")
	{
		// testing Uncertainties - remove after added for other technologies and add to uncertainties.lk (like autographs.lk)
		std::vector<Uncertainties> ul;
		Uncertainties u1, u2, u3;
		u1.Title = "Figure2";
		u2.Title = "Figure5";
		u3.Title = "Figure10";
		ul.push_back(u1);
		ul.push_back(u2);
		ul.push_back(u3);
		m_baseCaseResults->SetUncertainties(ul);
	}

	m_baseCaseResults->SetGraphs( gl );
	m_baseCaseResults->LoadPerspective( m_case->Perspective() );

	UpdateResults();

}

CaseWindow::~CaseWindow()
{
	m_baseCaseResults->Clear();
	
	// detach forms if any shown on input pages.
	DetachCurrentInputPage();
	m_currentGroup = 0;

	m_case->RemoveListener( this );
}


void CaseWindow::SaveCurrentViewProperties()
{
	UpdatePageNote(); // save the current note if it has changed

	int px,py;
	int x,y,w,h;
	SamApp::Window()->GetPosition(&px,&py);
	m_pageNote->GetPosition(&x,&y);
		
	x = x-px;
	y = y-py;
	double sf = wxGetScreenHDScale();
	m_pageNote->GetClientSize(&w,&h);
	m_case->SetProperty("NoteWindowXRel", wxString::Format("%d", (int)(x/sf) ));
	m_case->SetProperty("NoteWindowYRel", wxString::Format("%d",  (int)(y/sf) ));
	m_case->SetProperty("NoteWindowWidth", wxString::Format("%d", (int)(w/sf) ));
	m_case->SetProperty("NoteWindowHeight", wxString::Format("%d", (int)(h/sf) ));

	m_case->SetProperty( "DataBrowserVariables", 
		wxJoin( m_baseCaseResults->GetTabularBrowser()->GetSelectedVariables(), ',') );
}

bool CaseWindow::RunBaseCase( bool silent, wxString *messages )
{
	Simulation &bcsim = m_case->BaseCase();
	m_inputPageList->Select( -1 );	
	bcsim.Clear();

	ExcelExchange &ex = m_case->ExcelExch();
	if ( ex.Enabled )
		ExcelExchange::RunExcelExchange( ex, m_case->Values(), &bcsim );

	SimulationDialog tpd( "Simulating...", 1 );

	int nok = 0;
	if ( bcsim.Prepare() )
	{
		std::vector<Simulation*> list;
		list.push_back( &bcsim );
		nok += Simulation::DispatchThreads( tpd, list, 1 );
	}
	else
	{
		tpd.Log( bcsim.GetErrors() );
		tpd.Log("Error preparing simulation.");
	}

	if ( !silent ) tpd.Finalize( nok == 0 
			? "Simulation failed." 
			: "Simulation finished with warnings." );
	
	if (messages) *messages = tpd.Dialog().GetMessages();
	
	if ( nok == 1 )
	{
		if ( !silent ) {
			UpdateResults();
			m_pageFlipper->SetSelection( 1 );
		}
		return true;
	}
	else
	{
		wxArrayString err;
		m_case->BaseCase().Clear(); // clear notices 3/27/17
		err.Add("Last simulation failed.");
		bcsim.SetErrors(err);
		UpdateResults(); // clear notices 3/27/17
		return false;
	}
}

void CaseWindow::UpdateResults()
{
	m_baseCaseResults->Setup( &m_case->BaseCase() );
	m_baseCaseResults->GetTabularBrowser()->SelectVariables( 
		wxStringTokenize(m_case->GetProperty("DataBrowserVariables"), ',') );
}

static bool CheckValidTemplate( const wxString &fp, const wxString &tech, const wxString &fin )
{
	SamReportTemplate templ;
	if ( templ.Read( fp ))
	{
		if ( templ.GetSpecificModelsOnly())
		{
			wxArrayString tt, tf;
			templ.GetModels( &tt, &tf );

			if (tt.Index(tech) != wxNOT_FOUND
				&& tf.Index(fin) != wxNOT_FOUND)
				return true;
		}
		else
			return true;
	}
	
	return false;
}

bool CaseWindow::GenerateReport( wxString pdffile, wxString templfile, VarValue *meta )
{
	// run base case automatically 
	if ( !RunBaseCase() )
	{
		wxMessageBox( "Base case simulation did not succeed.  Please check your inputs before creating a report");
		return false;
	}

	wxString tech, fin;
	if ( ConfigInfo *ci = m_case->GetConfiguration() )
	{
		tech = ci->Technology;
		fin = ci->Financing;
	}
	else
	{
		wxMessageBox( "Internal error - invalid case configuration during report generation");
		return false;
	}

	
	if ( templfile.IsEmpty() )
	{
		wxArrayString validfiles;
		wxString path = SamApp::GetRuntimePath() + "/reports";
		wxDir dir( path );
		if ( dir.IsOpened() )
		{
			wxString file;
			bool has_more = dir.GetFirst( &file, "*.samreport", wxDIR_FILES  );
			while( has_more )
			{
				wxString fp( path + "/" + file );
				if ( CheckValidTemplate( fp, tech, fin ) )					
					validfiles.Add( fp );

				has_more = dir.GetNext( &file );
			}
		}
		dir.Close();

		
		if (validfiles.Count() == 0)
		{
			wxMessageBox( "SAM could not find any templates valid for the current technology and financing combination.\n\nPlease contact SAM user support at sam.support@nrel.gov for more information." );
			return false;
		}

		int index = 0;
		SamReportTemplate templ;

		// prompt when more than one report available
		if ( validfiles.Count() > 1)
		{
			wxArrayString choices;
			for (size_t i=0;i<validfiles.Count();i++)
				choices.Add( wxFileNameFromPath( validfiles[i] ) );

			index = ::wxGetSingleChoiceIndex( "Select a report template", "Report generation", choices, this );
		}

		if (index < 0)
			return false;

		templfile = validfiles[ index ] ;
	}
	else
	{
		if ( !CheckValidTemplate( templfile, tech, fin ) )
			return false;
	}


	if ( pdffile.IsEmpty() )
	{
		wxString casename = SamApp::Project().GetCaseName( m_case );
		wxString folder = wxPathOnly( SamApp::Window()->GetProjectFileName() );

		wxFileDialog fdlg( this, "Create PDF report for: " + casename, folder,
			casename + ".pdf", "Portable Document Format (*.pdf)|*.pdf", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );

		if ( fdlg.ShowModal() != wxID_OK )
			return false;

		 pdffile = fdlg.GetPath();
	}

	SamReportTemplate templ;
	if ( templ.Read( templfile ))
	{
		if (templ.RenderPdf( pdffile, m_case, meta ))
		{
			if ( !pdffile.IsEmpty() )
			{
				wxString new_file = wxFileSystem::FileNameToURL(pdffile);
				::wxLaunchDefaultBrowser(new_file, wxBROWSER_NEW_WINDOW);
			}
			return true;
		}
		else
			wxMessageBox("Failed to write to selected PDF file:\n\n" + pdffile);
	}

	return false;
}

void CaseWindow::OnCommand( wxCommandEvent &evt )
{
	if ( evt.GetId() == ID_SIMULATE )
	{
		RunBaseCase();
	}
	else if (evt.GetId() == ID_RESULTSPAGE )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 1 );
	}
	else if ( evt.GetId() == ID_ADVANCED )
	{
		wxPoint pos(wxDefaultPosition);
		if ( wxWindow *win = dynamic_cast<wxWindow*>(evt.GetEventObject()) )
		{
			pos = win->GetScreenPosition();
			pos.x += win->GetClientSize().x;
		}

		wxMetroPopupMenu menu;
		menu.Append( ID_PARAMETRICS, "Parametrics" );
		menu.Append( ID_STOCHASTIC, "Stochastic" );
		menu.Append( ID_P50P90, "P50 / P90" );
		menu.Append( ID_MACRO, "Scripting" );
		
		menu.Popup( this, pos, wxBOTTOM|wxRIGHT );
	}
	else if ( evt.GetId() == ID_PARAMETRICS )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 2 );
	}
	else if ( evt.GetId() == ID_STOCHASTIC )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 3 );
	}
	else if ( evt.GetId() == ID_P50P90 )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 4 );
	}
	else if ( evt.GetId() == ID_MACRO )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 5 );
	}
	else if ( evt.GetId() == ID_INPUTPAGELIST )
	{
		m_pageFlipper->SetSelection( 0 );
		SwitchToInputPage( m_inputPageList->GetStringSelection() );
	}
	else if ( evt.GetId() == ID_EXCL_BUTTON )
	{
		if ( m_currentGroup && m_currentGroup->OrganizeAsExclusivePages )
		{
			VarValue *vv = m_case->Values().Get( m_currentGroup->ExclusivePageVar );
			if ( !vv ) return;
			int sel = vv->Integer();
			
			wxMetroPopupMenu menu;
			menu.SetFont( m_exclPageButton->GetFont() );
			for( size_t i=0;i<m_currentGroup->Pages.size();i++)
				if ( m_currentGroup->Pages[i].size() > 0 )
					menu.AppendCheckItem( ID_EXCL_OPTION+i, m_currentGroup->Pages[i][0].Caption, (int)i == sel );

			wxPoint pos( 0, m_exclPageButton->GetClientSize().GetHeight() );
			pos = m_exclPageButton->ClientToScreen( pos );
			menu.Popup( this, pos, wxTOP|wxLEFT );
		}
	}
	else if ( evt.GetId() == ID_COLLAPSE )
	{
		PageDisplayState *pds = 0;
		for( size_t i=0;i<m_currentActivePages.size();i++ )
			if ( m_currentActivePages[i]->CollapseCheck == evt.GetEventObject() )
				pds = m_currentActivePages[i];

		if ( pds != 0 )
		{
			wxBusyCursor wait;
//			m_inputPageScrollWin->Freeze();
			
			m_case->Values().Set( pds->CollapsibleVar, VarValue( pds->CollapseCheck->GetValue() ) );
			m_case->VariableChanged( pds->CollapsibleVar ); // this will re-layout the page
			
//			m_inputPageScrollWin->Thaw();
		}
	}
	else if ( evt.GetId() == ID_EXCL_TABLIST ||
		( evt.GetId() >= ID_EXCL_OPTION && evt.GetId() < ID_EXCL_OPTION_MAX ) )
	{
		if ( 0 == m_currentGroup ) return;
		
		int sel = (evt.GetId()==ID_EXCL_TABLIST) ? m_exclPageTabList->GetSelection() : evt.GetId() - ID_EXCL_OPTION;

		VarValue *vv = m_case->Values().Get( m_currentGroup->ExclusivePageVar );
		if ( vv != 0 && sel != vv->Integer() )
		{
			wxBusyCursor wait;			
			vv->Set( sel );
			m_case->VariableChanged( m_currentGroup->ExclusivePageVar ); // this will redo the view
		}
	}
}

wxUIObject *CaseWindow::FindActiveObject( const wxString &name, ActiveInputPage **ipage )
{
	for( size_t i=0;i<m_currentActivePages.size();i++ )
	{
		if ( m_currentActivePages[i]->ActivePage != 0 )
		{
			if ( wxUIObject *obj = m_currentActivePages[i]->ActivePage->Find( name ) )
			{
				if ( ipage ) *ipage = m_currentActivePages[i]->ActivePage;
				return obj;
			}
		}
	}

	if ( ipage ) *ipage = 0;
	return 0;
}

void CaseWindow::OnCaseEvent( Case *, CaseEvent &evt )
{
	if ( evt.GetType() == CaseEvent::VARS_CHANGED )
	{
		// update UI objects for the ones that changed
		wxArrayString &list = evt.GetVars();
		for( size_t i=0;i<list.size();i++ )
		{
			ActiveInputPage *ipage = 0;
			wxUIObject *obj = FindActiveObject( list[i], &ipage );
			VarValue *vv = m_case->Values().Get( list[i] );
			if ( ipage && obj && vv )
			{
				ipage->DataExchange( obj, *vv, ActiveInputPage::VAR_TO_OBJ );
			
				// lookup and run any callback functions.
				if ( lk::node_t *root = m_case->QueryCallback( "on_change", obj->GetName() ) )
				{
					UICallbackContext cbcxt( ipage, obj->GetName() + "->on_change" );
					if ( cbcxt.Invoke( root, &m_case->CallbackEnvironment() ) )
					  {
						wxLogStatus("callback script " + obj->GetName() + "->on_change succeeded");
					  }
				}
			}


			// update views if the variable controls an
			// exclusive set of input pages or a collapsible pane
			if( VarInfo *info = m_case->Variables().Lookup( list[i] ) )
			{
				if ( info->Flags & VF_COLLAPSIBLE_PANE )
				{
					// determine if this variable is in the current view
					for( size_t j=0;j<m_currentActivePages.size();j++ )
					{
						PageDisplayState *pds = m_currentActivePages[j];
						if ( pds->CollapsibleVar == list[i] )
						{
							VarValue *vv = m_case->Values().Get( pds->CollapsibleVar );
							if( vv && vv->Boolean() )
							{
								if( pds->ActivePage == 0 ) 
								{
									pds->ActivePage = new ActiveInputPage( m_inputPageScrollWin, pds->Form, this );
									pds->ActivePage->Initialize();
								}
								pds->CollapseCheck->SetValue( true );
							}
							else
							{
								if( pds->ActivePage != 0 )
									pds->ActivePage->Destroy();

								pds->ActivePage = 0;
								pds->CollapseCheck->SetValue( false );
							}
							
							LayoutPage();
							break;
						}
					}
				}
				else if ( info->Flags & VF_EXCLUSIVE_PAGES 
					&& m_currentGroup != 0
					&& m_currentGroup->ExclusivePageVar == list[i] )
				{
					DetachCurrentInputPage();
					SetupActivePage();
				}
			}
		}
		
		SamApp::Project().SetModified( true );
	}
	else if ( evt.GetType() == CaseEvent::CONFIG_CHANGED )
	{
		wxString sel = m_inputPageList->GetStringSelection();
		UpdateConfiguration();
		if (!sel.empty()) 
			SwitchToInputPage( sel );
		else
			m_pageFlipper->SetSelection(0);

		// make sure at least the first input page is selected
		// if nothing else
		if ( m_pageFlipper->GetSelection() == 0
			&& m_currentGroup == 0 
			&& m_pageGroups.size() > 0 )
			SwitchToInputPage( m_pageGroups[0]->SideBarLabel );
		
		m_baseCaseResults->Clear();

		m_macros->ConfigurationChanged();

		SamApp::Project().SetModified( true );
	}
	else if ( evt.GetType() == CaseEvent::SAVE_NOTIFICATION )
	{
		// this event is issued before the case is written to a stream (disk)
		// here we need to save any perspective information or other data
		// generated in the UI
		
		// save the user created graphs
		std::vector<Graph> gl;
		m_baseCaseResults->GetGraphs( gl );
		m_case->SetGraphs( gl );

		// save the perspective of the results browser
		m_case->Perspective().clear();
		m_baseCaseResults->SavePerspective( m_case->Perspective() );		
	}
}

void CaseWindow::DetachCurrentInputPage()
{
	m_exclPanel->Show( false );

	m_currentForms.clear();
	
	for( size_t i=0;i<m_currentActivePages.size();i++ )
	{
		PageDisplayState *pds = m_currentActivePages[i];

		if ( pds->ActivePage != 0 )
			pds->ActivePage->Destroy();

		if ( pds->CollapseCheck != 0 )
			pds->CollapseCheck->Destroy();

		delete pds;
	}

	m_currentActivePages.clear();
}

wxArrayString CaseWindow::GetInputPages()
{
	wxArrayString list;
	for( size_t i=0; i<m_pageGroups.size();i++ )
		list.Add( m_pageGroups[i]->SideBarLabel );
	return list;
}

bool CaseWindow::SwitchToInputPage( const wxString &name )
{
	wxBusyCursor wait;
//	m_inputPagePanel->Freeze();

	DetachCurrentInputPage();

	m_currentGroup = 0;
	for( size_t i=0;i<m_pageGroups.size();i++ )
		if ( m_pageGroups[i]->SideBarLabel == name )
			m_currentGroup = m_pageGroups[i];

	if ( !m_currentGroup ) return false;

	for( size_t i=0;i<m_currentGroup->Pages.size();i++ )
		for( size_t j=0;j<m_currentGroup->Pages[i].size();j++ )
			if ( wxUIFormData *form = m_forms.Lookup( m_currentGroup->Pages[i][j].Name ) )
				m_currentForms.push_back( form );
			
	SetupActivePage();
	UpdatePageNote();

//	m_inputPagePanel->Thaw();

	if ( m_inputPageList->GetStringSelection() != name )
		m_inputPageList->Select( m_inputPageList->Find( name ) );

	return true;
}

bool CaseWindow::SwitchToPage( const wxString &name )
{
	wxString ln( name.Lower() );
	if ( ln.Left(7) == "results" )
	{
		m_pageFlipper->SetSelection( PG_RESULTS );
		int ipos = ln.Find(':');
		if ( ipos > 0 )
		{
			wxString page = ln.Mid( ipos+1 ).Lower();
			for( size_t i=0;i<m_baseCaseResults->GetPageCount();i++ )
			{
				if ( m_baseCaseResults->GetText(i).Lower() == page )
				{
					m_baseCaseResults->SetSelection( i );
					return true;
				}
			}

			return false;
		}
	}
	else if ( ln == "parametrics" )
		m_pageFlipper->SetSelection( PG_PARAMETRICS );
	else if ( ln == "p50p90" )
		m_pageFlipper->SetSelection( PG_P50P90 );
	else if ( ln == "macros" )
		m_pageFlipper->SetSelection( PG_MACROS );
	else if ( ln == "stochastic" )
		m_pageFlipper->SetSelection( PG_STOCHASTIC );
	else
	{
		m_pageFlipper->SetSelection( PG_INPUTS );
		return SwitchToInputPage( name );
	}

	return true;
}

void CaseWindow::LoadPageList( const std::vector<PageInfo> &list, bool header )
{
	for( size_t ii=0;ii<list.size();ii++ )
	{
		const PageInfo &pi = list[ii];

		PageDisplayState *pds = new PageDisplayState;

		// must register the PDS here so that the case knows 
		// about it when the ActiveInputPages are initialized.
		// this allows the 'on_load' callbacks to find objects via FindActiveObject
		m_currentActivePages.push_back( pds ); 


		pds->Form = m_forms.Lookup( pi.Name );
		if ( !pds->Form )
			wxMessageBox( "error locating form data " + pi.Name );

		pds->Collapsible = pi.Collapsible;
		pds->HeaderPage = header;

		bool load_page = true;

		if( pds->Collapsible )
		{
			pds->CollapsibleVar = pi.CollapsiblePageVar;
			wxString label = pi.Caption;
			if( !pi.ShowHideLabel.IsEmpty() ) label = pi.ShowHideLabel;
			pds->CollapseCheck = new CollapsePaneCtrl( m_inputPageScrollWin, ID_COLLAPSE, label );

			if ( VarValue *vv = m_case->Values().Get( pds->CollapsibleVar ) )
			{
				load_page = vv->Boolean();
				pds->CollapseCheck->SetValue( load_page );
			}
		}
		
		if( load_page && pds->Form != 0 )
		{
			pds->ActivePage = new ActiveInputPage( m_inputPageScrollWin, pds->Form, this );
			pds->ActivePage->Initialize();
		}

	}
}

void CaseWindow::SetupActivePage()
{
	m_exclPanel->Show( false );

	if ( !m_currentGroup ) return;
	
	std::vector<PageInfo> *active_headers = 0;
	std::vector<PageInfo> *active_pages = 0;
	
	if ( m_currentGroup->Pages.size() > 1 && !m_currentGroup->ExclusivePageVar.IsEmpty() )
	{
		size_t excl_idx = 9999;
		VarValue *vv = m_case->Values().Get( m_currentGroup->ExclusivePageVar );
		if ( !vv )
		{
			wxMessageBox( "could not locate exclusive page variable " + m_currentGroup->ExclusivePageVar );
			return;
		}
		else
			excl_idx = vv->Integer();

		if ( excl_idx < m_currentGroup->Pages.size() 
			&& m_currentGroup->Pages[excl_idx].size() > 0 )
		{
			m_exclPageButton->Show( false );
			m_exclPageTabList->Show( false );
			m_exclPanelSizer->Clear();

			if ( m_currentGroup->ExclusiveTabs )
			{
				m_exclPageTabList->Clear();
				for( size_t i=0;i<m_currentGroup->Pages.size();i++)
					if ( m_currentGroup->Pages[i].size() > 0 )
						m_exclPageTabList->Append( m_currentGroup->Pages[i][0].Caption );

				m_exclPageTabList->SetSelection( excl_idx );
				m_exclPageTabList->Show( true );
				m_exclPanelSizer->Add( m_exclPageTabList, 1, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
			}
			else
			{
				m_exclPageButton->SetLabel( m_currentGroup->Pages[excl_idx][0].Caption );
				m_exclPageButton->Show( true );
				m_exclPanelSizer->Add( m_exclPageButton, 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
				m_exclPanelSizer->AddStretchSpacer();
			}

			m_exclPanel->Show( true );

			active_pages = &( m_currentGroup->Pages[excl_idx] );
			active_headers = &( m_currentGroup->ExclusiveHeaderPages );
		}
		else
		{
			wxMessageBox("Exclusive page variable '" + m_currentGroup->ExclusivePageVar 
				+ wxString::Format("' has invalid value: %d.  Only %d input pages exist.", 
					(int)excl_idx, (int)m_currentGroup->Pages.size() ) );
		}
	}
	else if ( m_currentGroup->Pages.size() == 1 )
	{
		active_pages = &( m_currentGroup->Pages[0] );
	}
	else
	{
		wxMessageBox( "ui engine error: invalid page configuration on " + m_currentGroup->SideBarLabel );
		return;
	}
	
	// setup active display states	
	if ( active_headers ) LoadPageList( *active_headers, true );
	if ( active_pages ) LoadPageList( *active_pages, false );

	LayoutPage();

}

void CaseWindow::LayoutPage()
{
	int vsx, vsy;
	m_inputPageScrollWin->GetViewStart( &vsx, &vsy );

	
	int y = 0;
	int x = 0;
	
	size_t exclPanelPos = 0;

	wxSize available_size(0,0);
	for( size_t i=0;i<m_currentActivePages.size();i++ )
	{
		if ( m_currentActivePages[i]->Form != 0 )
		{
			wxSize sz = m_currentActivePages[i]->Form->GetSize();
			if( available_size.x < sz.x ) available_size.x = sz.x;
			available_size.y += sz.y;

			if ( m_currentActivePages[i]->HeaderPage )
				exclPanelPos = i+1;
		}
	}	

	// input pages are stacked upon one another
	for( size_t i=0;i<m_currentActivePages.size();i++ )
	{
		PageDisplayState &pds = *m_currentActivePages[i];

		
		if ( i==exclPanelPos && m_exclPanel->IsShown() )
		{
			wxSize excl_size( m_exclPanel->GetBestSize() );
			m_exclPanel->SetSize( 0, y, 
				available_size.x > 500 ? available_size.x : 500,
				excl_size.y );
			m_exclPanel->Layout();
			y += excl_size.y;
		}


		if( pds.CollapseCheck != 0 )
		{
			wxSize szbest = pds.CollapseCheck->GetBestSize();

			wxPoint curpos = pds.CollapseCheck->GetPosition();
			wxSize cursz = pds.CollapseCheck->GetClientSize();

			if( curpos.x != 0 || curpos.y != y )
				pds.CollapseCheck->SetPosition( wxPoint(0, y) );

			if( cursz.x != available_size.x+10 || cursz.y != szbest.y )
				pds.CollapseCheck->SetClientSize( available_size.x+10, szbest.y );

			y += szbest.y;
			if( x < szbest.x ) x = szbest.x;
		}

		if( pds.ActivePage != 0 )
		{
			wxPoint curpos = pds.ActivePage->GetPosition();
			if( curpos.x != 0 || curpos.y != y )
				pds.ActivePage->SetPosition( wxPoint(0, y) );

			pds.ActivePage->Show( true );

			wxSize sz = pds.ActivePage->GetClientSize();
			y += sz.y;
			if ( sz.x > x ) x = sz.x;
		}
	}
	
	m_inputPageScrollWin->SetScrollbars(1, 1, x, y);// , vsx, vsy);
	m_inputPageScrollWin->SetScrollRate(15,15);
}

void CaseWindow::UpdatePageListForConfiguration( const std::vector<PageInfo> &pages, ConfigInfo *cfg )
{
	for (size_t j=0;j<pages.size();j++ )
	{
		InputPageDataHash::iterator it = cfg->InputPages.find( pages[j].Name );
		if ( it != cfg->InputPages.end() )
			m_forms.Add( pages[j].Name, it->second->Form().Duplicate() );
		else
			wxMessageBox("Could not locate form data for " + pages[j].Name );			
	}
}

void CaseWindow::UpdateConfiguration()
{
	DetachCurrentInputPage();
	m_currentGroup = 0;
	m_inputPageList->ClearItems();

	ConfigInfo *cfg = m_case->GetConfiguration();
	if ( !cfg ) return;

	wxString Ts( SamApp::Config().Options( cfg->Technology ).ShortName );
	if ( Ts.IsEmpty() ) Ts = cfg->Technology;
	wxString Fs( SamApp::Config().Options( cfg->Financing ).ShortName );
	if ( Fs.IsEmpty() ) Fs = cfg->Financing;
	

	m_configLabel->SetLabel( Ts + ", " + Fs );
	
	// update current set of input pages
	m_pageGroups = cfg->InputPageGroups;

	// erase current set of forms, and rebuild the forms for this case
	m_forms.Clear();
	
	// update input page list (sidebar)
	for( size_t i=0;i<m_pageGroups.size();i++ )
	{
		InputPageGroup *group = m_pageGroups[i];

		for( size_t kk=0;kk<group->Pages.size();kk++ )
			UpdatePageListForConfiguration( group->Pages[kk], cfg );

		UpdatePageListForConfiguration( group->ExclusiveHeaderPages, cfg );

		m_inputPageList->Add( m_pageGroups[i]->SideBarLabel, i == m_pageGroups.size()-1, m_pageGroups[i]->HelpContext );
	}

	Layout();

}


void CaseWindow::UpdatePageNote()
{
	if ( m_pageNote == 0 ) return;

	// save page note to ID
	if (m_lastPageNoteId != "")
	{
		// check if the note text has changed
		wxString old_note = m_case->RetrieveNote(m_lastPageNoteId);
		if (old_note != m_pageNote->GetText())
			SamApp::Project().SetModified( true );

		m_case->SaveNote( m_lastPageNoteId, m_pageNote->GetText() );
		if (m_pageFlipper->GetSelection() == 0)
			m_inputPageList->Refresh();
	}

	// update ID
	m_lastPageNoteId = GetCurrentContext();

	// update text on page note
	wxString text = m_case->RetrieveNote( m_lastPageNoteId );
	m_pageNote->SetText(text);
	m_pageNote->Show( SamApp::Window()->GetCurrentCaseWindow() == this && !text.IsEmpty() );
}

void CaseWindow::ShowPageNote()
{
	m_pageNote->Show();
	m_pageNote->GetTextCtrl()->SetFocus();
}

void CaseWindow::SetPageNote( const wxString &note )
{
	m_pageNote->GetTextCtrl()->ChangeValue( note );
	UpdatePageNote();
}

bool CaseWindow::HasPageNote(const wxString &id)
{
	return !id.IsEmpty() && !m_case->RetrieveNote(id).IsEmpty();
}

void CaseWindow::OnSubNotebookPageChanged( wxNotebookEvent & )
{
	// common event handler for notebook page events to update the page note
	UpdatePageNote();
}

wxString CaseWindow::GetCurrentContext()
{
	wxString id = "about";
	wxString tech, fin;
	m_case->GetConfiguration( &tech, &fin );
	int page = m_pageFlipper->GetSelection();

	switch( page )
	{
	case 0: // inputs page
		if ( m_currentGroup ) id = m_currentGroup->HelpContext;
		else id = "inputs";
		break;
	case 1: // base case results
		id = m_baseCaseResults->GetCurrentContext();
		break;
	case 2: // parametrics
		id = "parametrics";
		break;
	case 3:
		id = "stochastic";
		break;
	case 4:
		id = "p50p90";
		break;
	case 5:
		id = "macros";
		break;
	default:
		id = "results";
		break;
	}

	return id;
}

/* ********* SAM Page Notes ************** */

BEGIN_EVENT_TABLE(PageNote, wxMiniFrame)
EVT_CLOSE( PageNote::OnHideClose )
END_EVENT_TABLE()

PageNote::PageNote(CaseWindow *cwin)
	: wxMiniFrame(cwin, -1, "Notes", wxDefaultPosition, wxScaleSize(300,450),
			  wxCLOSE_BOX|wxSYSTEM_MENU|wxCAPTION|wxRESIZE_BORDER)
{
	m_text = new wxTextCtrl(this,
		wxID_ANY, wxEmptyString, wxDefaultPosition ,wxDefaultSize,
		wxTE_MULTILINE|wxBORDER_NONE);
	m_text->SetFont( wxMetroTheme::Font( wxMT_LIGHT, 12 ));
	m_text->SetBackgroundColour( wxColour(255,255,180) );
}

void PageNote::SetText(const wxString &t)
{
	m_text->SetValue(t);
}

wxString PageNote::GetText()
{
	return m_text->GetValue();
}

wxTextCtrl *PageNote::GetTextCtrl()
{
	return m_text;
}

void PageNote::OnHideClose(wxCloseEvent &evt)
{
	evt.Veto();
	Hide();
}



enum {
  ID_txtSearch = wxID_HIGHEST+494,
  ID_tree,
  ID_btnUncheckAll,
  ID_btnExpandAll };

BEGIN_EVENT_TABLE( SelectVariableDialog, wxDialog )
	EVT_BUTTON(ID_btnUncheckAll, SelectVariableDialog::OnUncheckAll)
	EVT_BUTTON(ID_btnExpandAll, SelectVariableDialog::OnExpandAll)
	EVT_TEXT(ID_txtSearch, SelectVariableDialog::OnSearch )
	EVT_TREE_ITEM_ACTIVATED(ID_tree, SelectVariableDialog::OnTree)
END_EVENT_TABLE()

SelectVariableDialog::SelectVariableDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(500,500), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{	
	wxBoxSizer *search_sizer = new wxBoxSizer( wxHORIZONTAL );
	search_sizer->Add( new wxStaticText(this, wxID_ANY, "  Search: "), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	txtSearch = new wxExtTextCtrl(this, ID_txtSearch);
	search_sizer->Add( txtSearch, 1, wxALL|wxEXPAND, 4 );
	
	wxBoxSizer *button_sizer = new wxBoxSizer(wxHORIZONTAL);
	button_sizer->Add(  new wxButton(this, ID_btnExpandAll, "Expand All"), 0, wxALL|wxEXPAND, 4 );
	button_sizer->Add(  new wxButton(this, ID_btnUncheckAll, "Uncheck All"), 0, wxALL|wxEXPAND, 4 );
	button_sizer->AddStretchSpacer();
	button_sizer->Add( new wxButton(this, wxID_OK), 0, wxALL|wxEXPAND, 4  );
	button_sizer->Add( new wxButton(this, wxID_CANCEL), 0, wxALL|wxEXPAND, 4  );

	tree = new wxExtTreeCtrl(this, ID_tree);

	m_sizer = new wxBoxSizer(wxVERTICAL);
	m_sizer->Add( search_sizer, 0, wxALL|wxEXPAND, 0 );
	m_sizer->Add( tree, 1, wxALL|wxEXPAND, 4 );
	m_sizer->Add( button_sizer, 0, wxALL|wxEXPAND, 0 );

	SetSizer( m_sizer );
	SetEscapeId( wxID_CANCEL );
}

void SelectVariableDialog::OnSearch( wxCommandEvent & )
{
	wxString filter = txtSearch->GetValue().Lower();

	if (filter.IsEmpty())
	{
		for (size_t i=0;i<m_items.size();i++)
			m_items[i].shown = true;
	}
	else
	{
		for (size_t i=0;i<m_items.size();i++)
		{
			if (filter.Len() <= 2 && m_items[i].label.Left( filter.Len() ).Lower() == filter)
				m_items[i].shown = true;
			else if (m_items[i].label.Lower().Find( filter ) >= 0)
				m_items[i].shown = true;
			else if (m_items[i].name.Lower().Find( filter ) == 0)
				m_items[i].shown = true;
			else
				m_items[i].shown = false;
		}
	}

	UpdateTree();

	if ( !filter.IsEmpty() )
	{
		tree->Freeze();
		tree->ExpandAll();
		tree->EnsureVisible( m_root );
		tree->Thaw();
	}
	else
	{
		tree->Freeze();
		for (size_t i=0;i<m_items.size();i++)
		{
			if ( m_items[i].tree_id.IsOk() && m_items[i].checked )
				tree->Expand( tree->GetItemParent( m_items[i].tree_id ));
		}
		tree->EnsureVisible( m_root );
		tree->Thaw();
	}
}

void SelectVariableDialog::OnTree(wxTreeEvent &evt)
{
	wxTreeItemId item = evt.GetItem();	
	for (size_t i=0;i<m_items.size();i++)
		if (m_items[i].tree_id == item)
			m_items[i].checked = tree->IsChecked( m_items[i].tree_id );

	evt.Skip();
}

void SelectVariableDialog::ShowAllItems()
{
	tree->ExpandAll();
	tree->UnselectAll();
	tree->ScrollTo(this->m_root);
}

wxString SelectVariableDialog::PrettyPrintLabel(const wxString name, const VarInfo vi) {
	return PrettyPrintLabel(name, vi.Label, wxString::Format(wxT("%i"), vi.Type), vi.Units, vi.Group, false);
}

wxString SelectVariableDialog::PrettyPrintLabel(const wxString name, const wxString label, const wxString type, 
												const wxString units, const wxString group, bool ssc_variable) {
	wxString ppLabel = label;
	wxString un = units;
	un = un.Trim();
	ppLabel += " {'" + name + "'}";
	if (!un.IsEmpty()) ppLabel += " (" + un + ")";
	int ty = wxAtoi(type);
	wxString sty = "";
	if (ssc_variable)
	{
		if (ty == SSC_NUMBER) sty = "number";
		else if (ty == SSC_ARRAY) sty = "array";
		else if (ty == SSC_MATRIX) sty = "matrix";
		else if (ty == SSC_STRING) sty = "string";
		else if (ty == SSC_TABLE) sty = "table";
	}
	else
	{
		if (ty == VV_NUMBER) sty = "number";
		else if (ty == VV_ARRAY) sty = "array";
		else if (ty == VV_MATRIX) sty = "matrix";
		else if (ty == VV_STRING) sty = "string";
		else if (ty == VV_TABLE) sty = "table";
	}
	if (sty != "")
		ppLabel += " [" + sty + "]";

	if (!group.IsEmpty())
		ppLabel = group + "/" + ppLabel;
	else
		ppLabel = "Other/" + ppLabel;
	return ppLabel;
}

void SelectVariableDialog::SetItems(const wxArrayString &names, const wxArrayString &labels)
{
	if ( names.Count() != labels.Count() ) return;

	m_items.resize( names.Count() );
	for (size_t i=0;i<names.Count();i++)
	{
		m_items[i].name = names[i];
		m_items[i].label = labels[i];
		m_items[i].tree_id = 0;
		m_items[i].shown = true;
		m_items[i].checked = false;
	}

	UpdateTree();
	txtSearch->SetFocus();
}

void SelectVariableDialog::UpdateTree()
{
	tree->Freeze();
	tree->DeleteAllItems();

	m_root = tree->AddRoot("Available Variables",
		wxExtTreeCtrl::ICON_REMOVE,wxExtTreeCtrl::ICON_REMOVE);
	tree->SetItemBold(m_root);
	wxTreeItemId cur_parent;
	wxString cur_context;

	for (size_t i=0;i < m_items.size();i++)
	{
		m_items[i].tree_id.Unset();

		if ( !m_items[i].shown && !m_items[i].checked ) continue;

		wxString cxt;
		wxString lbl;
		int pos = m_items[i].label.Find('/');
		if (pos != wxNOT_FOUND)
		{
			cxt = m_items[i].label.Left(pos);
			lbl = m_items[i].label.Mid(pos+1);

			if (cur_context != cxt)
			{
				cur_context = cxt;
				cur_parent = tree->AppendItem(m_root, cur_context);
				tree->SetItemBold(cur_parent);
			}
		}
		
		if (lbl.IsEmpty())
			lbl = m_items[i].label;

		if (cur_parent.IsOk())
			m_items[i].tree_id = tree->AppendItem( cur_parent, lbl,wxExtTreeCtrl::ICON_CHECK_FALSE,-1 );
		else
			m_items[i].tree_id = tree->AppendItem( m_root, lbl, wxExtTreeCtrl::ICON_CHECK_FALSE, -1 );

		if ( m_items[i].checked )
			tree->Check( m_items[i].tree_id, true );
	}

	tree->Expand(m_root);
	tree->UnselectAll();
	tree->Thaw();
}

void SelectVariableDialog::SetCheckedNames(const wxArrayString &list)
{
	for (size_t i=0;i<m_items.size();i++)
	{
		m_items[i].checked = (list.Index( m_items[i].name ) >= 0);

		if (m_items[i].tree_id.IsOk())
		{
			tree->Check( m_items[i].tree_id, m_items[i].checked );
			if ( m_items[i].checked )
				tree->EnsureVisible( m_items[i].tree_id );
		}
	}

	if (m_root.IsOk())
		tree->EnsureVisible(m_root);
}

wxArrayString SelectVariableDialog::GetCheckedNames()
{
	wxArrayString list;
	for (size_t i=0;i<m_items.size();i++)
		if ( m_items[i].checked )
			list.Add( m_items[i].name );

	return list;
}
	
void SelectVariableDialog::OnExpandAll(wxCommandEvent &)
{
	tree->ExpandAll();
	if (m_root.IsOk())
		tree->EnsureVisible(m_root);
}

void SelectVariableDialog::OnUncheckAll(wxCommandEvent &)
{
	for (size_t i=0;i<m_items.size();i++)
		m_items[i].checked = false;
	UpdateTree();
}


bool SelectVariableDialog::Run( const wxString &title, 
	const wxArrayString &names, const wxArrayString &labels, wxArrayString &list,
	bool expand_all )
{
	SelectVariableDialog dlg( SamApp::Window(), title );
	dlg.SetItems( names, labels );
	dlg.SetCheckedNames( list );
	if (expand_all)
		dlg.ShowAllItems();

	if (dlg.ShowModal() == wxID_OK)
	{
		wxArrayString names = dlg.GetCheckedNames();
		
		// remove any from list
		int i=0;
		while (i<(int)list.Count())
		{
			if (names.Index( list[i] ) < 0)
				list.RemoveAt(i);
			else
				i++;
		}

		// append any new ones
		for (i=0;i<(int)names.Count();i++)
		{
			if (list.Index( names[i] ) < 0)
				list.Add( names[i] );
		}


		return true;
	}
	else
		return false;
}




BEGIN_EVENT_TABLE( VarSelectDialog, SelectVariableDialog )
	EVT_CHOICE( wxID_ANY, VarSelectDialog::OnConfig )
END_EVENT_TABLE()

VarSelectDialog::VarSelectDialog( wxWindow *parent, const wxString &title )
: SelectVariableDialog( parent, title )
{
	wxArrayString choices, tech( SamApp::Config().GetTechnologies() );
	for( size_t i=0;i<tech.size();i++ )
	{
		wxArrayString fin( SamApp::Config().GetFinancingForTech( tech[i] ));
		for( size_t k=0;k<fin.size();k++ )
			choices.Add( tech[i] + ", " + fin[k] );
	}
	m_cfglist = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, choices );
	m_sizer->Prepend( m_cfglist, 0, wxALL|wxEXPAND, 4 );

	m_cfglist->SetSelection( 0 );
	UpdateVariables();
}

void VarSelectDialog::UpdateVariables()
{
	wxString tech, fin, sel = m_cfglist->GetStringSelection();
	int pos = sel.Find( ',' );
	tech = sel.Mid( 0, pos );
	fin = sel.Mid( pos+2 );
	SetConfiguration( tech, fin );		
}

void VarSelectDialog::SetConfiguration( const wxString &tech, const wxString &fin )
{
	if ( ConfigInfo *ci = SamApp::Config().Find( tech, fin ) )
	{
		wxArrayString names, labels;
		for( VarInfoLookup::iterator it = ci->Variables.begin();
			it != ci->Variables.end();
			++it )
		{
			names.Add( it->first );
			wxString label = PrettyPrintLabel(it->first, *(it->second));

			labels.Add( label );
		}

		wxArrayString output_names, output_labels, output_units, output_groups;
		Simulation::ListAllOutputs( ci, &output_names, &output_labels, &output_units, &output_groups, nullptr, false );
		for( size_t i=0;i<output_names.size();i++ )
		{
			names.Add( output_names[i] );
			wxString label( output_labels[i] + " {'" + output_names[i] + "'}" );
			if ( !output_units[i].IsEmpty() )
				label += " (" + output_units[i] + ")";

			wxString G( output_groups[i] );
			if ( !G.IsEmpty() )
				G = " (" + G + ")";

			labels.Add( "@ Outputs" + G + "/" + label );
		}

		wxSortByLabels( names, labels );
		SetItems( names, labels );

		wxString cfgstr = tech + ", " + fin;
		if ( cfgstr != m_cfglist->GetStringSelection() )
			m_cfglist->SetStringSelection( cfgstr );
	}
}

void VarSelectDialog::OnConfig( wxCommandEvent &evt )
{
	if (evt.GetEventObject() == m_cfglist )
	{
		UpdateVariables();
	}
}





enum {
  ID_cmdUpdateValues = wxID_HIGHEST+495,
  ID_cmdMoveDown,
  ID_values,
  ID_cmdRemove,
  ID_cmdAddBefore,
  ID_cmdAddAfter,
  ID_cmdMoveUp,
  ID_numIncr,
  ID_numEnd,
  ID_numStart };

BEGIN_EVENT_TABLE( NumericRangeDialog, wxDialog )
	EVT_BUTTON( wxID_HELP, NumericRangeDialog::OnCommand )
	EVT_BUTTON( ID_cmdAddAfter, NumericRangeDialog::OnCommand)
	EVT_BUTTON( ID_cmdAddBefore, NumericRangeDialog::OnCommand)
	EVT_BUTTON( ID_cmdMoveUp, NumericRangeDialog::OnCommand)
	EVT_BUTTON( ID_cmdMoveDown, NumericRangeDialog::OnCommand)
	EVT_BUTTON( ID_cmdRemove, NumericRangeDialog::OnCommand)
	EVT_BUTTON( ID_cmdUpdateValues, NumericRangeDialog::OnCommand)
	EVT_NUMERIC( ID_numStart, NumericRangeDialog::OnCommand)
	EVT_NUMERIC( ID_numEnd, NumericRangeDialog::OnCommand)
	EVT_NUMERIC( ID_numIncr, NumericRangeDialog::OnCommand)
	EVT_LISTBOX_DCLICK( ID_values, NumericRangeDialog::OnCommand)
END_EVENT_TABLE()

NumericRangeDialog::NumericRangeDialog( wxWindow *parent, const wxString &title )
	 : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(500, 325), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
	m_intOnly = false;


	wxStaticBoxSizer *values_sizer = new wxStaticBoxSizer(wxVERTICAL, this, "Variable values");
	values_sizer->Add( m_values = new wxListBox( values_sizer->GetStaticBox(), ID_values ), 1, wxALL|wxEXPAND, 4 );

	wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );
	button_sizer->Add( new wxButton( values_sizer->GetStaticBox(), ID_cmdAddBefore, "Add before", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL, 2 );
	button_sizer->Add( new wxButton( values_sizer->GetStaticBox(), ID_cmdAddAfter, "Add after", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL, 2 );
	button_sizer->Add( new wxButton( values_sizer->GetStaticBox(), ID_cmdMoveUp, "Up", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL, 2 );
	button_sizer->Add( new wxButton( values_sizer->GetStaticBox(), ID_cmdMoveDown, "Down", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL, 2 );
	button_sizer->Add( new wxButton( values_sizer->GetStaticBox(), ID_cmdRemove, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL, 2 );

	values_sizer->Add( button_sizer, 0, wxALL, 2 );

	
	wxStaticBoxSizer *range_sizer_box = new wxStaticBoxSizer( wxVERTICAL, this, "Define range" );

	wxFlexGridSizer *range_sizer = new wxFlexGridSizer( 2 );
	range_sizer->Add( new wxStaticText( range_sizer_box->GetStaticBox(), wxID_ANY, "Start value:" ), 0, wxALL|wxALIGN_RIGHT, 4 );
	range_sizer->Add( m_numStart = new wxNumericCtrl( range_sizer_box->GetStaticBox(), ID_numStart, 0, wxNUMERIC_REAL ), 0, wxALL, 4 );
	range_sizer->Add( new wxStaticText( range_sizer_box->GetStaticBox(), wxID_ANY, "End value:" ), 0, wxALL|wxALIGN_RIGHT, 4 );
	range_sizer->Add( m_numEnd = new wxNumericCtrl( range_sizer_box->GetStaticBox(), ID_numEnd, 0, wxNUMERIC_REAL ), 0, wxALL, 4 );
	range_sizer->Add( new wxStaticText( range_sizer_box->GetStaticBox(), wxID_ANY, "Increment:" ), 0, wxALL|wxALIGN_RIGHT, 4 );
	range_sizer->Add( m_numIncr = new wxNumericCtrl( range_sizer_box->GetStaticBox(), ID_numIncr, 0, wxNUMERIC_REAL ), 0, wxALL, 4 );
	range_sizer->AddStretchSpacer(); 
	range_sizer->Add( new wxButton(range_sizer_box->GetStaticBox(), ID_cmdUpdateValues, "Update", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL, 4 );
	
	range_sizer_box->Add( range_sizer, 1, wxALL|wxEXPAND, 4 );
	range_sizer_box->Add( m_notification = new wxStaticText( range_sizer_box->GetStaticBox(), wxID_ANY, wxEmptyString ), 0, wxALL|wxALIGN_CENTER, 4 );
	
	wxBoxSizer *hor_sizer = new wxBoxSizer( wxHORIZONTAL );
	hor_sizer->Add( values_sizer, 1, wxALL|wxEXPAND, 4 );
	hor_sizer->Add( range_sizer_box, 0, wxALL, 4 );

	wxBoxSizer *main_sizer = new wxBoxSizer( wxVERTICAL );
	main_sizer->Add( hor_sizer, 1, wxALL|wxEXPAND, 10 );
	main_sizer->Add( CreateButtonSizer( wxOK|wxCANCEL|wxHELP ), 0, wxALL|wxEXPAND, 10 );
	SetSizerAndFit( main_sizer );
}

wxArrayString NumericRangeDialog::GetValues() 
{
	return m_values->GetStrings();
}

void NumericRangeDialog::SetValues(const wxArrayString &values, bool int_only)
{
	m_intOnly = int_only;

	m_values->Clear();
	m_values->Freeze();
	
	double min = 1e100, max=-1e100;

	for (int i=0;i<(int)values.Count();i++)
	{
		if (m_intOnly)
		{
			long x=0;
			values[i].ToLong(&x);
			m_values->Append( wxString::Format("%d", x ));
			if ( x < min)
				min = x;
			if (x > max)
				max = x;
		}
		else
		{
			double x=0;
			values[i].ToDouble(&x);
			m_values->Append( wxString::Format("%lg", x));
			if ( x < min)
				min = x;
			if (x > max)
				max = x;
		}
	}
	
	m_values->Thaw();

	m_numStart->SetValue(min);
	m_numEnd->SetValue(max);
	if (values.Count() > 1)
		m_numIncr->SetValue((max-min)/(double)(values.Count()-1));
	CheckRanges();
}

void NumericRangeDialog::OnCommand(wxCommandEvent &evt)
{
	int nsel = 0;
	switch(evt.GetId())
	{
	case wxID_HELP:
		SamApp::ShowHelp("edit_parametric_variables");
		break;

	case ID_values:
		{
			nsel = m_values->GetSelection();
			if (nsel < 0)
				return;

			wxString item = m_values->GetString(nsel);
			item = wxGetTextFromUser("Change value:", "Edit", item);
			if (item != wxEmptyString)
			{
				if (m_intOnly) m_values->SetString(nsel, wxString::Format("%d", (atoi(item.c_str()))));
				else m_values->SetString(nsel, wxString::Format("%lg",( atof( item.c_str() ) ) ));
			}

		}
		break;
	case ID_cmdAddAfter:
	case ID_cmdAddBefore:
		{
			nsel = m_values->GetSelection();
			if (nsel < 0)
				nsel = 0;

			wxString item = "1.0";
			if (nsel < (int) m_values->GetCount())
				item = m_values->GetString( nsel );

			wxString str = wxGetTextFromUser("Enter new value:", 
				evt.GetId() == ID_cmdAddAfter ? "Add Number After Selection" : "Add Number Before Selection",
				item);
			if (str != wxEmptyString)
			{
				int idxincr = (evt.GetId() == ID_cmdAddAfter && m_values->GetCount()>0 )? 1 : 0;
				
				if (m_intOnly) m_values->Insert(wxString::Format("%d", (atoi(str.c_str()))), nsel + idxincr);
				else m_values->Insert(wxString::Format("%lg", (atof(str.c_str()))), nsel + idxincr);
			}
		}

		break;

	case ID_cmdRemove:
		if ( (nsel=m_values->GetSelection()) >= 0)
		{
			m_values->Delete( m_values->GetSelection() );
			if (nsel >= (int)m_values->GetCount())
				nsel = m_values->GetCount() - 1;
			if (nsel >= 0)
				m_values->SetSelection(nsel);
		}
		break;
	case ID_cmdMoveUp:
		if (m_values->GetCount() >= 2)
		{
			int isel = m_values->GetSelection();
			if (isel >= 1)
			{
				wxString tmp = m_values->GetString( isel - 1 );
				m_values->SetString(isel - 1, m_values->GetString(isel) );
				m_values->SetString(isel, tmp);
				m_values->SetSelection( isel - 1 );
			}
		}
		break;

	case ID_cmdMoveDown:
		if (m_values->GetCount() >= 2)
		{
			int isel = m_values->GetSelection();
			if (isel <= (int)m_values->GetCount() - 2 && isel >= 0)
			{
				wxString tmp = m_values->GetString( isel + 1 );
				m_values->SetString(isel + 1, m_values->GetString(isel) );
				m_values->SetString(isel, tmp);
				m_values->SetSelection( isel + 1 );
			}
		}
		break;

	case ID_cmdUpdateValues:
	case ID_numStart:
	case ID_numEnd:
	case ID_numIncr:
		GenerateValues();
		break;
	}
}

bool NumericRangeDialog::CheckRanges()
{
	double start, end, incr;

	start = m_numStart->Value();
	end = m_numEnd->Value();
	incr = m_numIncr->Value();

	m_notification->SetLabel( wxEmptyString );

	if (incr == 0)
		m_notification->SetLabel("Increment is 0");

	if (end <= start && incr > 0)
		m_notification->SetLabel("End < start");

	if (start < end && incr < 0)
		m_notification->SetLabel("Start < end");
	
	m_notification->Refresh();

	return m_notification->GetLabel().IsEmpty();
}

void NumericRangeDialog::GenerateValues()
{
	if (!CheckRanges())
		return;

	double start, end, incr;

	start = m_numStart->Value();
	end = m_numEnd->Value();
	incr = m_numIncr->Value();

	double curval = start;
	wxString endvalstr = m_intOnly?
		wxString::Format("%d", (int) end ) :
		wxString::Format("%lg", end );

#define NMAXVALS 200

	wxArrayString vals;
	int nadded = 0;
	while ( (incr > 0 && curval <= end)
			|| (incr < 0 && curval >= end ) )
	{
		if (vals.Index( endvalstr ) >= 0)
			break;

		if (nadded > NMAXVALS)
			break;

		nadded++;

		if (incr > 0 && curval > 1.001*end)
			break;

		if (incr < 0 && curval < 0.999*end)
			break;

		vals.Add( m_intOnly?
			wxString::Format("%d", (int) curval ) :
			wxString::Format("%lg", curval ) );
			
		curval += incr;
	}

	if (vals.Index(endvalstr) < 0 && nadded <= NMAXVALS)
		vals.Add(endvalstr);


	m_values->Freeze();
	m_values->Clear();
	m_values->Append(vals);
	m_values->Thaw();
}
