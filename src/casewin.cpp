#include <wx/wx.h>
#include <wx/splitter.h>
#include <wx/simplebook.h>
#include <wx/statline.h>
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
	ID_SIMULATE, ID_RESULTSPAGE, ID_ADVANCED, ID_PARAMETRICS, ID_SENSITIVITY, ID_P50P90, ID_SCRIPTING,
	ID_COLLAPSE,ID_EXCL_BUTTON, ID_EXCL_OPTION, ID_EXCL_OPTION_MAX=ID_EXCL_OPTION+25,
	ID_PAGES, ID_BASECASE_PAGES };

BEGIN_EVENT_TABLE( CaseWindow, wxSplitterWindow )
	EVT_BUTTON( ID_SIMULATE, CaseWindow::OnCommand )
	EVT_BUTTON( ID_RESULTSPAGE, CaseWindow::OnCommand )
	EVT_BUTTON( ID_ADVANCED, CaseWindow::OnCommand )
	EVT_BUTTON( ID_PARAMETRICS, CaseWindow::OnCommand )
	EVT_BUTTON( ID_SENSITIVITY, CaseWindow::OnCommand )
	EVT_BUTTON( ID_P50P90, CaseWindow::OnCommand )
	EVT_BUTTON( ID_SCRIPTING, CaseWindow::OnCommand )
	EVT_MENU( ID_PARAMETRICS, CaseWindow::OnCommand )
	EVT_MENU( ID_SENSITIVITY, CaseWindow::OnCommand )
	EVT_MENU( ID_P50P90, CaseWindow::OnCommand )
	EVT_MENU( ID_SCRIPTING, CaseWindow::OnCommand )
	EVT_LISTBOX( ID_INPUTPAGELIST, CaseWindow::OnCommand )
	EVT_BUTTON( ID_EXCL_BUTTON, CaseWindow::OnCommand )
	EVT_CHECKBOX( ID_COLLAPSE, CaseWindow::OnCommand )
	EVT_MENU_RANGE( ID_EXCL_OPTION, ID_EXCL_OPTION_MAX, CaseWindow::OnCommand )

	EVT_NOTEBOOK_PAGE_CHANGED( ID_PAGES, CaseWindow::OnSubNotebookPageChanged )
	EVT_NOTEBOOK_PAGE_CHANGED( ID_BASECASE_PAGES, CaseWindow::OnSubNotebookPageChanged )
END_EVENT_TABLE()

CaseWindow::CaseWindow( wxWindow *parent, Case *c )
	: wxSplitterWindow( parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER | wxSP_LIVE_UPDATE ),
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
	for ( int i=0;i<10;i++ ) m_inputPageList->Add( wxString::Format("Input page %d", i+1) );

	m_simButton = new wxMetroButton( left_panel, ID_SIMULATE, "Simulate", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW );
	m_simButton->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 14) );
	m_resultsButton = new wxMetroButton( left_panel, ID_RESULTSPAGE, wxEmptyString, wxBITMAP_PNG_FROM_DATA( graph ) );

	wxBoxSizer *szhl = new wxBoxSizer( wxHORIZONTAL );
	szhl->Add( m_simButton, 1, wxALL|wxEXPAND, 0 );
	szhl->Add( m_resultsButton, 0, wxALL|wxEXPAND, 0 );
	
	wxFont lafont( *wxNORMAL_FONT );
	lafont.SetWeight( wxFONTWEIGHT_BOLD );
	m_configLabel = new wxStaticText( left_panel, wxID_ANY, "-technology-" );
	m_configLabel->SetBackgroundColour( laback );
	m_configLabel->SetForegroundColour( lafore );
	m_configLabel->SetFont( lafont );
	

	wxBoxSizer *szvl = new wxBoxSizer( wxVERTICAL );
	szvl->Add( m_configLabel, 0, wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER|wxTOP|wxBOTTOM, 3 );
	szvl->Add( m_inputPageList, 1, wxALL|wxEXPAND, 0 );
	szvl->Add( szhl, 0, wxALL|wxEXPAND, 0 );
	
	wxSizer *szsims = new wxGridSizer(2, 0, 0);
	szsims->Add( new wxMetroButton( left_panel, ID_PARAMETRICS, "Parametrics" ), 0, wxALL|wxEXPAND, 0 );
	szsims->Add( new wxMetroButton( left_panel, ID_SENSITIVITY, "Sensitivity" ), 0, wxALL|wxEXPAND, 0 );
	szsims->Add( new wxMetroButton( left_panel, ID_P50P90, "P50 / P90" ), 0, wxALL|wxEXPAND, 0 );
	szsims->Add( new wxMetroButton( left_panel, ID_SCRIPTING, "Monte Carlo" ), 0, wxALL|wxEXPAND, 0 );
	szvl->Add( szsims, 0, wxALL|wxEXPAND, 0 );
	

	left_panel->SetSizer( szvl );

	m_pageFlipper = new wxSimplebook( this, ID_PAGES, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );

	m_inputPagePanel = new wxPanel( m_pageFlipper );
	m_inputPagePanel->SetBackgroundColour( *wxWHITE );

	m_inputPageScrollWin = new wxScrolledWindow( m_inputPagePanel );
	m_inputPageScrollWin->SetBackgroundColour( *wxWHITE );
	
	m_exclPanel = new wxPanel( m_inputPagePanel );
	m_exclPageButton = new wxMetroButton( m_exclPanel, ID_EXCL_BUTTON, "Change...", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW );
	wxBoxSizer *excl_horiz = new wxBoxSizer( wxHORIZONTAL );
	excl_horiz->Add( m_exclPageButton, 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	excl_horiz->AddStretchSpacer();
	m_exclPanel->SetSizer( excl_horiz );

	wxBoxSizer *ip_sizer = new wxBoxSizer( wxVERTICAL );
	ip_sizer->Add( m_exclPanel, 0, wxALL|wxEXPAND, 0 );
	ip_sizer->Add( m_inputPageScrollWin, 1, wxALL|wxEXPAND, 0 );
	m_inputPagePanel->SetSizer( ip_sizer );
	
	m_pageFlipper->AddPage( m_inputPagePanel, "Input Pages", true );

	
	m_baseCaseResults = new ResultsViewer( m_pageFlipper, ID_BASECASE_PAGES );
	m_pageFlipper->AddPage( m_baseCaseResults, "Base Case" );
		
	m_parametrics = new ParametricViewer( m_pageFlipper, m_case );
	m_pageFlipper->AddPage( m_parametrics, "Parametric", false );


	m_pageFlipper->AddPage( new wxPanel( m_pageFlipper ), "Sensitivity", false );
	m_pageFlipper->AddPage( new wxPanel( m_pageFlipper ), "P50/P90", false );

	m_scriptCtrl = new wxLKScriptCtrl( m_pageFlipper, wxID_ANY );
	m_pageFlipper->AddPage( m_scriptCtrl, "Scripting", false );

	
	SetMinimumPaneSize( 50 );
	SplitVertically( left_panel, m_pageFlipper, 210 );
	
	
	m_pageNote = new PageNote( this );

	// load page note window geometry
	int nw_xrel, nw_yrel, nw_w, nw_h;
	nw_xrel = wxAtoi( m_case->GetProperty("NoteWindowXRel") );
	nw_yrel = wxAtoi( m_case->GetProperty("NoteWindowYRel") );
	nw_w = wxAtoi( m_case->GetProperty("NoteWindowWidth") );
	nw_h = wxAtoi( m_case->GetProperty("NoteWindowHeight") );
	
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

	m_pageNote->GetClientSize(&w,&h);
	m_case->SetProperty("NoteWindowXRel", wxString::Format("%d", x ));
	m_case->SetProperty("NoteWindowYRel", wxString::Format("%d",  y ));
	m_case->SetProperty("NoteWindowWidth", wxString::Format("%d", w ));
	m_case->SetProperty("NoteWindowHeight", wxString::Format("%d", h ));
}

bool CaseWindow::RunBaseCase( )
{
	Simulation &bcsim = m_case->BaseCase();
	m_inputPageList->Select( -1 );	
	bcsim.Clear();

	ExcelExchange &ex = m_case->ExcelExch();
	if ( ex.Enabled )
		ExcelExchange::RunExcelExchange( ex, m_case->Values(), &bcsim );

	if ( bcsim.Invoke() )
	{
		UpdateResults();
		m_pageFlipper->SetSelection( 1 );
		return true;
	}
	else
	{
		wxShowTextMessageDialog( wxJoin(bcsim.GetErrors(), '\n') );
		return false;
	}
}

void CaseWindow::UpdateResults()
{
	m_baseCaseResults->Setup( &m_case->BaseCase() );
}


void CaseWindow::GenerateReport( )
{


	// run base case automatically 
	if ( !RunBaseCase() )
	{
		wxMessageBox( "Base case simulation did not succeed.  Please check your inputs before creating a report");
		return;
	}

	wxString ct, cf;
	if ( ConfigInfo *ci = m_case->GetConfiguration() )
	{
		ct = ci->Technology;
		cf = ci->Financing;
	}
	else
	{
		wxMessageBox( "Internal error - invalid case configuration");
		return;
	}

	
	int total = 0;
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
			SamReportTemplate templ;
			if ( templ.Read( fp ))
			{
				total++;
				if ( templ.GetSpecificModelsOnly())
				{
					wxArrayString tt, tf;
					templ.GetModels( &tt, &tf );

					if (tt.Index(ct) != wxNOT_FOUND && tf.Index(cf) != wxNOT_FOUND)
						validfiles.Add( fp );
				}
				else
					validfiles.Add( fp );
			}

			has_more = dir.GetNext( &file );
		}
	}
	dir.Close();

	if ( total == 0 )
	{
		wxMessageBox("SAM could not find any report templates.\n\nPlease contact SAM user support at sam.support@nrel.gov for more information.");
		return;
	}


	if (validfiles.Count() == 0)
	{
		wxMessageBox( "SAM could not find any templates valid for the current technology and financing combination.\n\nPlease contact SAM user support at sam.support@nrel.gov for more information." );
		return;
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
		return;

	wxString casename = SamApp::Project().GetCaseName( m_case );
	wxString folder = wxPathOnly( SamApp::Window()->GetProjectFileName() );

	wxFileDialog fdlg( this, "Create PDF report for: " + casename, folder,
		casename + ".pdf", "Portable Document Format (*.pdf)|*.pdf", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );

	if ( fdlg.ShowModal() != wxID_OK )
		return;

	wxString pdffile = fdlg.GetPath();

	if (templ.Read( validfiles[index] ))
	{
		if (!templ.RenderPdf( pdffile, m_case ))
			wxMessageBox("Failed to write to selected PDF file:\n\n" + pdffile);
		else
			::wxLaunchDefaultBrowser( pdffile );
	}
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
		menu.Append( ID_SENSITIVITY, "Sensitivity" );
		menu.Append( ID_P50P90, "P50 / P90" );
		menu.Append( ID_SCRIPTING, "Scripting" );
		
		menu.Popup( this, pos, wxBOTTOM|wxRIGHT );
	}
	else if ( evt.GetId() == ID_PARAMETRICS )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 2 );
	}
	else if ( evt.GetId() == ID_SENSITIVITY )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 3 );
	}
	else if ( evt.GetId() == ID_P50P90 )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 4 );
	}
	else if ( evt.GetId() == ID_SCRIPTING )
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
			
			wxMenu menu;
			for( size_t i=0;i<m_currentGroup->Pages.size();i++)
				if ( m_currentGroup->Pages[i].size() > 0 )
					menu.AppendCheckItem( ID_EXCL_OPTION+i, m_currentGroup->Pages[i][0].Caption );

			menu.Check( ID_EXCL_OPTION+sel, true );

			PopupMenu( &menu );
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
	else if ( evt.GetId() >= ID_EXCL_OPTION && evt.GetId() < ID_EXCL_OPTION_MAX )
	{
		if ( 0 == m_currentGroup ) return;

		VarValue *vv = m_case->Values().Get( m_currentGroup->ExclusivePageVar );
		int sel = evt.GetId() - ID_EXCL_OPTION;
		if ( sel != vv->Integer() )
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
				ipage->DataExchange( obj, *vv, ActiveInputPage::VAR_TO_OBJ );


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
							}
							else
							{
								if( pds->ActivePage != 0 )
									pds->ActivePage->Destroy();

								pds->ActivePage = 0;
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

		// update side bar
		m_inputPageList->Refresh();

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

void CaseWindow::SetupActivePage()
{
	m_exclPanel->Show( false );

	if ( !m_currentGroup ) return;
	
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
			//m_exclPageLabel->SetLabel( m_currentGroup->Pages[excl_idx][0].Caption );
			m_exclPageButton->SetLabel( m_currentGroup->Pages[excl_idx][0].Caption );
			m_exclPanel->Layout();
			m_exclPanel->Show( true );
			active_pages = &( m_currentGroup->Pages[excl_idx] );
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

	if ( !active_pages ) return;

	for( size_t ii=0;ii<active_pages->size();ii++ )
	{
		PageInfo &pi = (*active_pages)[ii];

		PageDisplayState *pds = new PageDisplayState;

		// must register the PDS here so that the case knows 
		// about it when the ActiveInputPages are initialized.
		// this allows the 'on_load' callbacks to find objects via FindActiveObject
		m_currentActivePages.push_back( pds ); 


		pds->Form = m_forms.Lookup( pi.Name );
		if ( !pds->Form )
			wxMessageBox( "error locating form data " + pi.Name );

		pds->Collapsible = pi.Collapsible;

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

	LayoutPage();

	m_inputPagePanel->Layout();
}

void CaseWindow::LayoutPage()
{
	int vsx, vsy;
	m_inputPageScrollWin->GetViewStart( &vsx, &vsy );
	
	int y = 0;
	int x = 0;

	wxSize available_size(0,0);
	for( size_t i=0;i<m_currentActivePages.size();i++ )
	{
		if ( m_currentActivePages[i]->Form != 0 )
		{
			wxSize sz = m_currentActivePages[i]->Form->GetSize();
			if( available_size.x < sz.x ) available_size.x = sz.x;
			available_size.y += sz.y;
		}
	}

	// input pages are stacked upon one another
	for( size_t i=0;i<m_currentActivePages.size();i++ )
	{
		PageDisplayState &pds = *m_currentActivePages[i];

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
	
	m_inputPageScrollWin->SetScrollbars(1,1, x, y, vsx, vsy);
	m_inputPageScrollWin->SetScrollRate(15,15);
}

void CaseWindow::UpdateConfiguration()
{
	DetachCurrentInputPage();
	m_currentGroup = 0;
	m_inputPageList->ClearItems();

	ConfigInfo *cfg = m_case->GetConfiguration();
	if ( !cfg ) return;

	m_configLabel->SetLabel( cfg->Technology  + ", " + cfg->Financing );
	
	// update current set of input pages
	m_pageGroups = cfg->InputPageGroups;

	// erase current set of forms, and rebuild the forms for this case
	m_forms.Clear();
	
	// update input page list (sidebar)
	for( size_t i=0;i<m_pageGroups.size();i++ )
	{
		InputPageGroup *group = m_pageGroups[i];

		for( size_t kk=0;kk<group->Pages.size();kk++ )
		{
			std::vector<PageInfo> &pages = group->Pages[kk];
			for (size_t j=0;j<pages.size();j++ )
			{
				InputPageDataHash::iterator it = cfg->InputPages.find( pages[j].Name );
				if ( it != cfg->InputPages.end() )
					m_forms.Add( pages[j].Name, it->second->Form().Duplicate() );
				else
					wxMessageBox("Could not locate form data for " + pages[j].Name );			
			}
		}

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

bool CaseWindow::HasPageNote(const wxString &id)
{
	return !id.IsEmpty() && !m_case->RetrieveNote(id).IsEmpty();
}

void CaseWindow::OnSubNotebookPageChanged( wxNotebookEvent &evt )
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
		else id = "Inputs";
		break;
	case 1: // base case results
		id = "Base Case " + m_baseCaseResults->GetSelectionText();
		break;
	case 2: // parametrics
		id = "Parametrics";
		break;
	case 3:
		id = "Sensitivity";
		break;
	case 4:
		id = "P50/P90";
		break;
	case 5:
		id = "Scripting";
		break;
	default:
		id = "Results";
		break;
	}

	return id;
}

bool CaseWindow::ShowSelectVariableDialog( const wxString &title, 
	const wxArrayString &names, const wxArrayString &labels, wxArrayString &list,
	bool expand_all )
{
	SelectVariableDialog dlg(this, title);
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

/* ********* SAM Page Notes ************** */

BEGIN_EVENT_TABLE(PageNote, wxMiniFrame)
EVT_CLOSE( PageNote::OnHideClose )
END_EVENT_TABLE()

PageNote::PageNote(CaseWindow *cwin)
	: wxMiniFrame(cwin, -1, "Notes", wxDefaultPosition, wxDefaultSize,
			  wxCLOSE_BOX|wxSYSTEM_MENU|wxCAPTION/*|wxSTAY_ON_TOP*/|wxRESIZE_BORDER)
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
	 : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(500,500), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
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

	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	sizer->Add( search_sizer, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( tree, 1, wxALL|wxEXPAND, 4 );
	sizer->Add( button_sizer, 0, wxALL|wxEXPAND, 0 );

	SetSizer( sizer );
	SetEscapeId( wxID_CANCEL );
}

void SelectVariableDialog::OnSearch( wxCommandEvent & evt)
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
	
void SelectVariableDialog::OnExpandAll(wxCommandEvent &evt)
{
	tree->ExpandAll();
	if (m_root.IsOk())
		tree->EnsureVisible(m_root);
}

void SelectVariableDialog::OnUncheckAll(wxCommandEvent &evt)
{
	for (size_t i=0;i<m_items.size();i++)
		m_items[i].checked = false;
	UpdateTree();
}

