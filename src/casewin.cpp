#include <wx/wx.h>
#include <wx/splitter.h>
#include <wx/simplebook.h>
#include <wx/statline.h>

#include <wex/metro.h>
#include <wex/lkscript.h>
#include <wex/extgrid.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>

#include <wex/icons/cirplus.cpng>
#include <wex/icons/cirminus.cpng>

#include "basecase.h"
#include "main.h"
#include "case.h"
#include "casewin.h"
#include "ipagelist.h"

#include "../resource/graph.cpng"

class ActiveInputPage : public InputPageBase
{
	CaseWindow *m_cwin;
	Case *m_case;

public:
	ActiveInputPage( wxWindow *parent, wxUIFormData *ipdata, CaseWindow *cw )
		: m_cwin(cw), m_case(cw->GetCase()), InputPageBase( parent, ipdata, wxID_ANY )
	{		
		Initialize();
	}
	virtual ~ActiveInputPage() { /* nothing to do */ }
		
	virtual VarInfoLookup &GetVariables()	{ return m_case->Variables(); }
	virtual EqnFastLookup &GetEquations() { return m_case->Equations(); }
	virtual CallbackDatabase &GetCallbacks() { return SamApp::Callbacks(); }
	virtual VarTable &GetValues() { return m_case->Values(); }

	virtual void OnUserInputChanged( wxUIObject *obj )
	{
		// transfer the data from the UI object to the variable (DDX) 
		// then notify the case that the variable was changed
		// within the case, the calculations will be redone as needed
		// and then the casewindow will be notified by event that
		// other UI objects (calculated ones) need to be updated
		if( VarValue *vval = GetValues().Get( obj->GetName() ) )
		{
			if ( DataExchange( obj, *vval, OBJ_TO_VAR ) )
			{
				wxLogStatus( "Variable " + obj->GetName() + " changed by user interaction, case notified." );
				m_case->Recalculate( obj->GetName() );
			}
			else
				wxMessageBox("ActiveInputPage >> data exchange fail: " + obj->GetName() );
		}
	}

	virtual void OnVariableChanged( const wxString &varname )
	{
		wxLogStatus( "Variable " + varname + " changed programmatically, case notified." );		
		m_case->VariableChanged( varname );
			
	}
};




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
	ID_SIMULATE, ID_RESULTSPAGE, ID_COLLAPSE,
	ID_EXCL_BUTTON, ID_EXCL_OPTION, ID_EXCL_OPTION_MAX=ID_EXCL_OPTION+25 };

BEGIN_EVENT_TABLE( CaseWindow, wxSplitterWindow )
	EVT_BUTTON( ID_SIMULATE, CaseWindow::OnCommand )
	EVT_BUTTON( ID_RESULTSPAGE, CaseWindow::OnCommand )
	EVT_LISTBOX( ID_INPUTPAGELIST, CaseWindow::OnCommand )
	EVT_BUTTON( ID_EXCL_BUTTON, CaseWindow::OnCommand )
	EVT_CHECKBOX( ID_COLLAPSE, CaseWindow::OnCommand )
	EVT_MENU_RANGE( ID_EXCL_OPTION, ID_EXCL_OPTION_MAX, CaseWindow::OnCommand )
END_EVENT_TABLE()

CaseWindow::CaseWindow( wxWindow *parent, Case *c )
	: wxSplitterWindow( parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER | wxSP_LIVE_UPDATE ),
	m_case( c )
{
	m_case->AddListener( this );

	m_currentGroup = 0;

	wxPanel *left_panel = new wxPanel( this );
	left_panel->SetBackgroundColour( *wxWHITE );
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

	wxBoxSizer *szvl = new wxBoxSizer( wxVERTICAL );
	szvl->Add( m_inputPageList, 1, wxALL|wxEXPAND, 0 );
	szvl->Add( szhl, 0, wxALL|wxEXPAND, 0 );

	left_panel->SetSizer( szvl );

	m_pageFlipper = new wxSimplebook( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );

	m_inputPagePanel = new wxPanel( m_pageFlipper );
	m_inputPagePanel->SetBackgroundColour( *wxWHITE );

	m_inputPageScrollWin = new wxScrolledWindow( m_inputPagePanel );
	m_inputPageScrollWin->SetBackgroundColour( *wxWHITE );

	m_exclPanel = new wxPanel( m_inputPagePanel );
	m_exclPageLabel = new wxStaticText( m_exclPanel, wxID_ANY, wxEmptyString );	
	m_exclPageLabel->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 12 ));	
	m_exclPageButton = new wxMetroButton( m_exclPanel, ID_EXCL_BUTTON, "Change..." );
	wxBoxSizer *excl_horiz = new wxBoxSizer( wxHORIZONTAL );
	excl_horiz->Add( m_exclPageLabel, 1, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	excl_horiz->Add( m_exclPageButton, 0, wxALL, 1 );
	wxBoxSizer *excl_vert = new wxBoxSizer( wxVERTICAL );
	excl_vert->Add( excl_horiz, 0, wxALL|wxEXPAND, 1 );
	excl_vert->Add( new wxStaticLine( m_exclPanel ), 1, wxALL|wxEXPAND, 1 );
	m_exclPanel->SetSizer( excl_vert );

	wxBoxSizer *ip_sizer = new wxBoxSizer( wxVERTICAL );
	ip_sizer->Add( m_exclPanel, 0, wxALL|wxEXPAND, 0 );
	ip_sizer->Add( m_inputPageScrollWin, 1, wxALL|wxEXPAND, 0 );
	m_inputPagePanel->SetSizer( ip_sizer );

	m_resultsTab = new wxMetroNotebook( m_pageFlipper, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxMT_LIGHTTHEME );
	
	m_baseCase = new BaseCase( m_resultsTab, 0 );
	m_resultsTab->AddPage( m_baseCase, "Base Case", true );

	wxPanel *param_panel = new wxPanel( m_resultsTab );

	wxBoxSizer *par_sizer = new wxBoxSizer( wxHORIZONTAL );
	par_sizer->Add( new wxButton( param_panel, wxID_ANY, "Configure..."), 0, wxALL|wxEXPAND, 2 );
	par_sizer->Add( new wxButton( param_panel, wxID_ANY, "Run parametric simulation"), 0, wxALL|wxEXPAND, 2 );
	par_sizer->AddStretchSpacer();
	par_sizer->Add( new wxButton( param_panel, wxID_ANY, "Clear results"), 0, wxALL|wxEXPAND, 2 );
	
	wxExtGridCtrl *par_grid = new wxExtGridCtrl( param_panel, wxID_ANY );
	par_grid->CreateGrid( 7, 4 );

	wxString grid[8][5]= {
		{ "Tilt", "Input 2", "LCOEreal", "AnnOutput", "NPV" },
		{ "10", "1", "1",   "1", "-4" },
		{ "15", "2", "3.1", "2", "-3" },
		{ "20", "3", "4.1", "3", "-2" },
		{ "25", "4", "3.1", "4", "-1" },
		{ "30", "5", "2.1", "5", "-2" },
		{ "35", "6", "1.1", "6", "-3" },
		{ "40", "7", "1.1", "7", "-4" } };

	for( size_t r=1;r<8;r++ )
	{
		for( size_t c=1;c<5;c++ )
		{
			if ( r == 1 ) par_grid->SetColLabelValue( c-1, grid[0][c-1] );
			par_grid->SetCellValue( grid[r][c], r-1, c-1 );
			if ( c <= 2 ) par_grid->SetCellBackgroundColour( wxColour(244,244,210), r-1, c-1 );
		}
	}
	par_grid->AutoSizeColumns();

	wxPLPlotCtrl *par_plot = new wxPLPlotCtrl( param_panel, wxID_ANY );
	std::vector<wxRealPoint> bar_data;
	bar_data.push_back( wxRealPoint( 1, 3 ) );
	bar_data.push_back( wxRealPoint( 2, 2.9 ) );
	bar_data.push_back( wxRealPoint( 3, 3.2 ) );
	bar_data.push_back( wxRealPoint( 4, 3.7 ) );
	bar_data.push_back( wxRealPoint( 5, 2.2 ) );
	bar_data.push_back( wxRealPoint( 6, 1.7 ) );
	wxPLBarPlot *bar0, *bar1;
	par_plot->AddPlot( bar0 = new wxPLBarPlot( bar_data, "Var2 run^0", wxMetroTheme::Colour( wxMT_ACCENT ) ) );
	for( size_t i=0;i<bar_data.size();i++ ) bar_data[i].y *= 0.7 + ((double)i)/5;
	par_plot->AddPlot( bar1 = new wxPLBarPlot( bar_data, "Var1 run^0", wxMetroTheme::Colour( wxMT_FOREGROUND ) ) );
	par_plot->GetXAxis1()->SetWorld( 0, 7 );
	par_plot->GetYAxis1()->SetWorld( 0, 5 );
	std::vector<wxPLBarPlot*> bar_group;
	bar_group.push_back( bar0 );
	bar_group.push_back( bar1 );
	bar0->SetGroup( bar_group );
	bar1->SetGroup( bar_group );
	bar0->SetThickness( 30 );
	bar1->SetThickness( 30 );

	wxBoxSizer *par_vsizer = new wxBoxSizer( wxVERTICAL );
	par_vsizer->Add( par_sizer, 0, wxALL|wxEXPAND, 2 );
	par_vsizer->Add( par_grid, 1, wxALL|wxEXPAND, 0 );
	par_vsizer->Add( par_plot, 1, wxALL|wxEXPAND, 0 );

	param_panel->SetSizer( par_vsizer );

	m_resultsTab->AddPage( param_panel, "Parametric", false );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "Sensitivity", false );
	m_resultsTab->AddPage( new wxPanel( m_resultsTab ), "P50/P90", false );

	m_scriptCtrl = new wxLKScriptCtrl( m_resultsTab, wxID_ANY );
	m_resultsTab->AddPage( m_scriptCtrl, "Scripting", false );

	m_pageFlipper->AddPage( m_inputPagePanel, "Input Pages", true );
	m_pageFlipper->AddPage( m_resultsTab, "Output Pages", false );

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
}

CaseWindow::~CaseWindow()
{
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

void CaseWindow::OnCommand( wxCommandEvent &evt )
{
	if ( evt.GetId() == ID_SIMULATE )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 1 );
		m_resultsTab->SetSelection( 0 ); // show base case
		// run simulations for this case
	}
	else if (evt.GetId() == ID_RESULTSPAGE )
	{
		m_inputPageList->Select( -1 );
		m_pageFlipper->SetSelection( 1 );
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

		if( pds != 0 )
		{
			bool en = pds->CollapseCheck->GetValue();

			if( en )
			{
				if( pds->ActivePage == 0 ) 
					pds->ActivePage = new ActiveInputPage( m_inputPageScrollWin, pds->Form, this );
			}
			else
			{
				if( pds->ActivePage != 0 ) pds->ActivePage->Destroy();
				pds->ActivePage = 0;
			}

			m_case->Values().Set( pds->CollapsibleVar, VarValue( en ) );
			m_case->VariableChanged( pds->CollapsibleVar ); // this will re-layout the page
		}
	}
	else if ( evt.GetId() >= ID_EXCL_OPTION && evt.GetId() < ID_EXCL_OPTION_MAX )
	{
		if ( 0 == m_currentGroup ) return;

		VarValue *vv = m_case->Values().Get( m_currentGroup->ExclusivePageVar );
		int sel = evt.GetId() - ID_EXCL_OPTION;
		if ( sel != vv->Integer() )
		{
			vv->Set( sel );
			m_case->VariableChanged( m_currentGroup->ExclusivePageVar ); // this will redo the view
		}
	}
}

wxUIObject *CaseWindow::FindActiveObject( const wxString &name, InputPageBase **ipage )
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

void CaseWindow::OnCaseEvent( Case *c, CaseEvent &evt )
{
	if ( evt.GetType() == CaseEvent::VARS_CHANGED )
	{
		// update UI objects for the ones that changed
		wxArrayString &list = evt.GetVars();
		for( size_t i=0;i<list.size();i++ )
		{
			InputPageBase *ipage = 0;
			wxUIObject *obj = FindActiveObject( list[i], &ipage );
			VarValue *vv = m_case->Values().Get( list[i] );
			if ( ipage && obj && vv )
				ipage->DataExchange( obj, *vv, InputPageBase::VAR_TO_OBJ );


			// update views if the variable controls an
			// exclusive set of input pages or a collapsible pane
			if( VarInfo *info = SamApp::Variables().Lookup( list[i] ) )
			{
				if ( info->Flags & VF_COLLAPSIBLE_PANE )
				{
					// determine if this variable is in the current view
					for( size_t j=0;j<m_currentActivePages.size();j++ )
						if ( m_currentActivePages[j]->CollapsibleVar == list[i] )
							LayoutPage();
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

		SamApp::Window()->Project().SetModified( true );
	}
	else if ( evt.GetType() == CaseEvent::CONFIG_CHANGED )
	{
		wxString sel = m_inputPageList->GetStringSelection();
		UpdateConfiguration();
		SwitchToInputPage( sel );
		
		SamApp::Window()->Project().SetModified( true );
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

bool CaseWindow::SwitchToInputPage( const wxString &name )
{
	m_inputPagePanel->Show( false );

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

	m_inputPagePanel->Show( true );

	return true;
}

void CaseWindow::SetupActivePage()
{
	m_exclPanel->Show( false );

	if ( !m_currentGroup ) return;
	
	std::vector<ConfigDatabase::PageInfo> *active_pages = 0;
	
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
			m_exclPageLabel->SetLabel( m_currentGroup->Pages[excl_idx][0].Caption );
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
		ConfigDatabase::PageInfo &pi = (*active_pages)[ii];

		PageDisplayState *pds = new PageDisplayState;
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
			pds->ActivePage = new ActiveInputPage( m_inputPageScrollWin, pds->Form, this );

		m_currentActivePages.push_back( pds );
	}

	LayoutPage();
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
		wxSize sz = m_currentActivePages[i]->Form->GetSize();
		if( available_size.x < sz.x ) available_size.x = sz.x;
		available_size.y += sz.y;
	}

	// input pages are stacked upon one another
	for( size_t i=0;i<m_currentActivePages.size();i++ )
	{
		PageDisplayState &pds = *m_currentActivePages[i];

		if( pds.CollapseCheck != 0 )
		{
			wxSize szbest = pds.CollapseCheck->GetBestSize();
			pds.CollapseCheck->SetSize( 0, y, available_size.x+10, szbest.y );
			y += szbest.y;
			if( x < szbest.x ) x = szbest.x;
		}

		if( pds.ActivePage != 0 )
		{
			pds.ActivePage->SetPosition( wxPoint(0, y) );
			wxSize sz = pds.ActivePage->GetClientSize();
			y += sz.y;
			if ( sz.x > x ) x = sz.x;
		}
	}
	
	m_inputPagePanel->Layout();
	m_inputPageScrollWin->SetScrollbars(1,1, x, y, 0, 0);
	m_inputPageScrollWin->SetScrollRate(15,15);
	m_inputPageScrollWin->Scroll( vsx, vsy );
}

void CaseWindow::UpdateConfiguration()
{
	DetachCurrentInputPage();
	m_currentGroup = 0;
	m_inputPageList->ClearItems();

	wxString tech, fin;
	m_case->GetConfiguration( &tech, &fin );

	// update current set of input pages
	m_pageGroups = SamApp::Config().GetInputPages( tech, fin );

	// erase current set of forms, and rebuild the forms for this case
	m_forms.Clear();
	
	// update input page list (sidebar)
	for( size_t i=0;i<m_pageGroups.size();i++ )
	{
		ConfigDatabase::InputPageGroup *group = m_pageGroups[i];

		for( size_t kk=0;kk<group->Pages.size();kk++ )
		{
			std::vector<ConfigDatabase::PageInfo> &pages = group->Pages[kk];
			for (size_t j=0;j<pages.size();j++ )
			{
				if ( wxUIFormData *ui = SamApp::Forms().Lookup( pages[j].Name ) )
					m_forms.Add( pages[j].Name, ui->Duplicate() );
				else
					wxMessageBox("Could not locate form data for " + pages[j].Name );			
			}
		}

		m_inputPageList->Add( m_pageGroups[i]->SideBarLabel, i == m_pageGroups.size()-1, m_pageGroups[i]->HelpContext );
	}

}


void CaseWindow::UpdatePageNote()
{
	// save page note to ID
	if (m_lastPageNoteId != "")
	{
		// check if the note text has changed
		wxString old_note = m_case->RetrieveNote(m_lastPageNoteId);
		if (old_note != m_pageNote->GetText())
			SamApp::Window()->Project().SetModified( true );

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

wxString CaseWindow::GetCurrentContext()
{
	wxString id = "about";
	wxString tech, fin;
	m_case->GetConfiguration( &tech, &fin );
	int page = m_pageFlipper->GetSelection();

	if (page == 0)
	{
		if ( m_currentGroup ) id = m_currentGroup->HelpContext;
		else id = "Inputs";
	}
	else if (page == 1)
	{
		int respage = m_resultsTab->GetSelection();
		switch(respage)
		{
		case 0: id = "Base Case"; break;			
		case 1: id = "Parametrics"; break;
		case 2: 
			{
				if (fin=="Residential") 
					id="Cash Flow Residential";
				else if (fin=="Commercial") 
					id="Cash Flow Commercial";
				else if (fin=="Commercial PPA") 
					id="Cash Flow Commercial PPA";
				else if (fin=="Independent Power Producer") 
					id="Cash Flow Utility IPP";
				else if (fin=="Single Owner") 
					id="Cash Flow Utility SO";
				else if (fin=="All Equity Partnership Flip") 
					id="Cash Flow Utility AEPF";
				else if (fin=="Leveraged Partnership Flip") 
					id="Cash Flow Utility LPF";
				else if (fin=="Sale Leaseback") 
					id="Cash Flow Utility SL";
				else
					id = "Base Case Cashflow"; 
				break;
			}

		case 3: id = "Monte Carlo"; break;		
		case 4: id = "Scripting"; break;
		default: id = "Results";
		}
	}

	return id;
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
