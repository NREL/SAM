#include <wx/wx.h>
#include <wx/splitter.h>
#include <wx/simplebook.h>
#include <wex/metro.h>
#include <wex/lkscript.h>
#include <wex/extgrid.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>

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
		: InputPageBase( parent, ipdata, wxID_ANY )
	{
		m_cwin = cw;
		m_case = cw->GetCase();
		
		SetBackgroundColour( *wxWHITE );
		
		m_formData->Attach( this );
		SetClientSize( m_formData->GetSize() ); // resize self to specified form data
		
		Initialize();
	}
	virtual ~ActiveInputPage() { /* nothing to do */ }
		
	virtual VarInfoLookup &GetVariables()	{ return m_case->Variables(); }
	virtual EqnFastLookup &GetEquations() { return m_case->Equations(); }
	virtual CallbackDatabase &GetCallbacks() { return SamApp::Callbacks(); }
	virtual VarTable &GetValues() { return m_case->Values(); }

	virtual void OnInputChanged( wxUIObject *obj )
	{
		if( VarValue *vval = GetValues().Get( obj->GetName() ) )
		{
			// transfer the data from the UI object to the variable (DDX) 
			// then notify the case that the variable was changed
			// within the case, the calculations will be redone as needed
			// and then the casewindow will be notived by event that
			// other UI objects (calculated ones) need to be updated
			if ( DataExchange( obj, *vval, OBJ_TO_VAR ) )
				m_case->Changed( obj->GetName() );
			else
				wxMessageBox("ActveInputPage >> data exchange fail: " + obj->GetName() );
		}
	}
};



enum { ID_INPUTPAGELIST = wxID_HIGHEST + 142,
	ID_SIMULATE, ID_RESULTSPAGE,
	ID_EXCL_BUTTON };

BEGIN_EVENT_TABLE( CaseWindow, wxSplitterWindow )
	EVT_BUTTON( ID_SIMULATE, CaseWindow::OnCommand )
	EVT_BUTTON( ID_RESULTSPAGE, CaseWindow::OnCommand )
	EVT_LISTBOX( ID_INPUTPAGELIST, CaseWindow::OnCommand )
	EVT_BUTTON( ID_EXCL_BUTTON, CaseWindow::OnCommand )
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

	m_inputPageScrollWin = new wxScrolledWindow( m_pageFlipper );
	m_inputPageScrollWin->SetBackgroundColour( *wxWHITE );
	m_exclPageLabel = new wxStaticText( m_inputPageScrollWin, wxID_ANY, wxEmptyString );	
	wxFont font = m_exclPageLabel->GetFont();
	font.SetWeight( wxFONTWEIGHT_BOLD );
	font.SetPointSize( font.GetPointSize()+1 );
	m_exclPageLabel->SetFont(font);
	m_exclPageLabel->Show( false );
	
	m_exclPageButton = new wxButton( m_inputPageScrollWin, ID_EXCL_BUTTON, "Change..." );
	m_exclPageButton->Show( false );

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

	m_pageFlipper->AddPage( m_inputPageScrollWin, "Input Pages", true );
	m_pageFlipper->AddPage( m_resultsTab, "Output Pages", false );

	SplitVertically( left_panel, m_pageFlipper, 210 );

	UpdateConfiguration();

	
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
}

CaseWindow::~CaseWindow()
{
	// detach forms if any shown on input pages.
	DetachCurrentInputPage();

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

class PageOptionDialog : public wxDialog
{
	wxRadioBox *m_radio;
public:
	PageOptionDialog( wxWindow *parent, const wxString &title,
		const wxArrayString &items, int sel )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(400,400), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
	{
		m_radio = new wxRadioBox( this, wxID_ANY, "Options", 
			wxDefaultPosition, wxDefaultSize, 
			items, 1, wxRA_SPECIFY_COLS );
		m_radio->SetSelection( sel );

		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( m_radio, 1, wxALL|wxEXPAND, 5 );
		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 5);
		SetSizer(sizer);

		Fit();		
	}

	int GetSelection() { return m_radio->GetSelection(); }
};

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

			PageOptionDialog dlg( this, "Select option", m_currentGroup->Pages, vv->Integer() );
			wxPoint pt = m_exclPageButton->GetScreenPosition();
			wxSize sz = m_exclPageButton->GetSize();
			dlg.Move( pt.x - 20, pt.y + sz.y + 10 );

			if ( dlg.ShowModal() != wxID_OK ) return;
		
			int sel = dlg.GetSelection();
			if ( sel != vv->Integer() )
			{
				vv->Set( sel );
				m_case->Changed( m_currentGroup->ExclusivePageVar );
				OrganizeCurrentPages();
			}
		}
	}
}

wxUIObject *CaseWindow::FindActiveObject( const wxString &name, InputPageBase **ipage )
{
	for( size_t i=0;i<m_currentShownPages.size();i++ )
	{
		if ( wxUIObject *obj = m_currentShownPages[i]->Find( name ) )
		{
			if ( ipage ) *ipage = m_currentShownPages[i];
			return obj;
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
	for( size_t i=0;i<m_currentShownPages.size();i++ )
		m_currentShownPages[i]->Destroy();
	m_currentShownPages.clear();

	m_currentGroup = 0;
}

bool CaseWindow::SwitchToInputPage( const wxString &name )
{
	DetachCurrentInputPage();
	
	for( size_t i=0;i<m_pageGroups.size();i++ )
		if ( m_pageGroups[i]->Caption == name )
			m_currentGroup = m_pageGroups[i];

	if ( !m_currentGroup ) return false;

	for( size_t i=0;i<m_currentGroup->Pages.size();i++ )
		if ( wxUIFormData *form = m_currentForms.Lookup( m_currentGroup->Pages[i] ) )
			m_currentShownPages.push_back( new ActiveInputPage( m_inputPageScrollWin, form, this ) );

	OrganizeCurrentPages();
	UpdatePageNote();

	return true;
}

void CaseWindow::OrganizeCurrentPages()
{
	if ( !m_currentGroup ) return;
	
	int y = 0;
	int x = 0;

	if ( m_currentGroup->OrganizeAsExclusivePages )
	{
		VarValue *vv = m_case->Values().Get( m_currentGroup->ExclusivePageVar );
		if ( !vv )
		{
			wxMessageBox( "could not locate exclusive page variable " + m_currentGroup->ExclusivePageVar );
			return;
		}
		
		// switch between pages here using a flipper widget or something?
		for( size_t i=0;i<m_currentShownPages.size();i++ )
		{
			InputPageBase *page = m_currentShownPages[i];
			wxSize sz = page->GetClientSize();
			if ( i == vv->Integer() )
			{
				page->Show( true );
				m_exclPageLabel->SetLabel( page->GetName() );
				y = sz.y;
			}
			else
				page->Show( false );

			page->SetPosition( wxPoint(0,30) );
			if ( sz.x > x ) x = sz.x;
		}

		m_exclPageLabel->SetSize( 5, 5, 300, 23 );
		m_exclPageLabel->Show();
		m_exclPageLabel->Refresh();

		m_exclPageButton->SetSize( 305, 5, 120, 23 );
		m_exclPageButton->Show();

		y += 30;
	}
	else
	{
		// input pages are stacked upon one another
		for( size_t i=0;i<m_currentShownPages.size();i++ )
		{
			InputPageBase *page = m_currentShownPages[i];
			page->SetPosition( wxPoint(0, y) );
			wxSize sz = page->GetClientSize();
			y += sz.y;
			if ( sz.x > x ) x = sz.x;
		}
	}

	m_inputPageScrollWin->SetScrollbars(1,1, x, y, 0, 0);
	m_inputPageScrollWin->SetScrollRate(50,50);
	m_inputPageScrollWin->Scroll(0,0);
}

void CaseWindow::UpdateConfiguration()
{
	m_inputPageList->ClearItems();

	wxString tech, fin;
	m_case->GetConfiguration( &tech, &fin );

	// update current set of input pages
	m_pageGroups = SamApp::Config().GetInputPages( tech, fin );

	// erase current set of forms, and rebuild the forms for this case
	m_currentForms.Clear();
	
	// update input page list (sidebar)
	for( size_t i=0;i<m_pageGroups.size();i++ )
	{
		wxArrayString &pages = m_pageGroups[i]->Pages;
		for (size_t j=0;j<pages.size();j++ )
		{
			if ( wxUIFormData *ui = SamApp::Forms().Lookup( pages[j] ) )
				m_currentForms.Add( pages[j], ui->Duplicate() );
			else
				wxMessageBox("could not locate form data for " + pages[j] );			
		}

		m_inputPageList->Add( m_pageGroups[i]->Caption, i < m_pageGroups.size()-1, m_pageGroups[i]->HelpContext );
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
