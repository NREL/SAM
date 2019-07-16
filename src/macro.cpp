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

#include "macro.h"

class macro_vm : public lk::vm
{
	size_t m_counter;
	MacroEngine *m_me;
public:
	macro_vm( MacroEngine *me ) 
		: m_me(me)
	{
		m_counter = 0;
	}

	virtual bool on_run( int  )
	{
		// expression & (constant-1) is equivalent to expression % constant where 
		// constant is a power of two: so use bitwise operator for better performance
		// see https://en.wikipedia.org/wiki/Modulo_operation#Performance_issues 
		if ( 0 == (m_counter++ & 1023) ) {
			wxGetApp().Yield( true );
			return !m_me->IsStopFlagSet();
		}
		else return true;
	}
};

class my_vm : public lk::vm
{
	MacroEngine *m_me;
public:
	my_vm( MacroEngine *me ) : m_me(me) { }
	virtual bool on_run( const lk::srcpos_t & )
	{
		wxGetApp().Yield( true );
		return !m_me->IsStopFlagSet();
	}
};

MacroEngine::MacroEngine()
{
	m_stopFlag = false;
}

MacroEngine::~MacroEngine()
{
	// nothing to do
}

bool MacroEngine::IsStopFlagSet()
{
	return m_stopFlag;
}

static void fcall_out( lk::invoke_t &cxt )
{
	LK_DOC("out", "Output data to the console.", "(...):none");

	MacroEngine *lksc = (MacroEngine*)cxt.user_data();
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string();
	lksc->Output( output );
}

static void fcall_outln( lk::invoke_t &cxt )
{
	LK_DOC("outln", "Output data to the console with a newline.", "(...):none");
	
	MacroEngine *lksc = (MacroEngine*)cxt.user_data();
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string(); 
	output += '\n';
	lksc->Output( output );
}

extern lk::fcall_t *sam_functions();
extern lk::fcall_t *invoke_general_funcs();
extern lk::fcall_t *invoke_ssc_funcs();

bool MacroEngine::Run( const wxString &script, lk::vardata_t *args )
{	
	// first, erase any old output
	ClearOutput();
	
	// parse macro code
	lk::input_string p( script );
	lk::parser parse( p );	
	smart_ptr<lk::node_t>::ptr tree( parse.script() );
			
	int i=0;
	while ( i < parse.error_count() )
		Output( parse.error(i++) );
	
	if ( parse.token() != lk::lexer::END)
	{
		Output("Parsing did not reach end of input.\n");
		return false;
	}

	if ( parse.error_count() > 0 )
		return false;	
	
	// compile the code into VM instructions and load it
	macro_vm vm( this );
	lk::codegen cg;
	lk::bytecode bc;
	if ( cg.generate( tree.get() ) )
	{	
		cg.get( bc );
		vm.load( &bc );
	}
	else
	{
		Output("Error in code generation: " + cg.error() );
		return false;
	}
	
	
	// setup macro runtime environment
	lk::env_t env;	
	if ( args != 0 ) env.assign( "macro", args );
	else env.assign( "macro", new lk::vardata_t ); // assign null to macro variable

	env.register_funcs( lk::stdlib_basic() );
	env.register_funcs( lk::stdlib_sysio() );
	env.register_funcs( lk::stdlib_string() );
	env.register_funcs( lk::stdlib_math() );
	env.register_funcs( lk::stdlib_wxui() );
	env.register_funcs( wxLKPlotFunctions() );
	env.register_funcs( wxLKMiscFunctions() );
	env.register_funcs( wxLKFileFunctions() );
	env.register_funcs( invoke_general_funcs() );
	env.register_funcs( invoke_ssc_funcs() );
	env.register_funcs( sam_functions() );
	env.register_func( fcall_out, this );
	env.register_func( fcall_outln, this );
	
	// initialize VM environment
	vm.initialize( &env );

	// setup target parent windows for plots
	wxLKSetToplevelParentForPlots( SamApp::Window() );
	wxLKSetPlotTarget( NULL );

	// clear the stop flag
	m_stopFlag = false;
	
	// run it!
	if ( !vm.run( lk::vm::NORMAL ) )
	{
		Output("Macro did not finish.\n");
		Output( vm.error() );
		return false;
	}

	return true;
}

void MacroEngine::Stop()
{
	// first cancel any simulations that
	// might be running that were invoked 
	// from a script
	SamScriptWindow::CancelRunningSimulations();
	m_stopFlag = true;
}

class MacroOutputFrame;
static MacroOutputFrame *gs_macroOutputFrame = 0;
class MacroOutputFrame : public wxFrame
{
public:
	wxTextCtrl *txtoutput;
	MacroOutputFrame()
		: wxFrame( SamApp::Window(), wxID_ANY, "Macro Output", wxDefaultPosition, wxScaleSize( 700, 250 ) )
	{
		gs_macroOutputFrame = this;
		txtoutput = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxBORDER_NONE );
	}
	void OnClose( wxCloseEvent & )
	{
		wxGetApp().ScheduleForDestruction( this );
		gs_macroOutputFrame = 0;
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( MacroOutputFrame, wxFrame )
	EVT_CLOSE( MacroOutputFrame::OnClose )
END_EVENT_TABLE()

void MacroEngine::Output( const wxString &output )
{

	if ( gs_macroOutputFrame == 0 )
	{
		gs_macroOutputFrame = new MacroOutputFrame;
		gs_macroOutputFrame->Show();
	}

	gs_macroOutputFrame->txtoutput->AppendText( output );
}

void MacroEngine::ClearOutput()
{
	if ( gs_macroOutputFrame != 0 )
		gs_macroOutputFrame->txtoutput->Clear();
}

enum { ID_MACRO_LIST = wxID_HIGHEST+895, ID_HTML,
	ID_RUN_MACRO, ID_STOP_MACRO,
	ID_VIEW_CODE, ID_SAVE_UIDATA, ID_LOAD_UIDATA };

BEGIN_EVENT_TABLE( MacroPanel, wxSplitterWindow )
	EVT_LISTBOX( ID_MACRO_LIST, MacroPanel::OnCommand )
	EVT_HTML_LINK_CLICKED( ID_HTML, MacroPanel::OnHtmlLink )
	EVT_BUTTON( ID_RUN_MACRO, MacroPanel::OnCommand )
	EVT_BUTTON( ID_STOP_MACRO, MacroPanel::OnCommand )
	EVT_BUTTON( ID_VIEW_CODE, MacroPanel::OnCommand )
	EVT_BUTTON( ID_SAVE_UIDATA, MacroPanel::OnCommand )
	EVT_BUTTON( ID_LOAD_UIDATA, MacroPanel::OnCommand )
END_EVENT_TABLE()

MacroPanel::MacroPanel( wxWindow *parent, Case *cc )
	: wxSplitterWindow( parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER|wxSP_LIVE_UPDATE | wxSP_3DSASH ), m_case(cc)
{
	m_output = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxBORDER_NONE);
	
	wxSplitterWindow *vsplit = new wxSplitterWindow(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER | wxSP_LIVE_UPDATE | wxSP_3DSASH);
	m_leftPanel = new wxPanel(vsplit);
	m_rightPanel = new wxPanel(vsplit);
	m_listbox = new wxMetroListBox(m_leftPanel, ID_MACRO_LIST);
	wxBoxSizer *lvsizer = new wxBoxSizer(wxVERTICAL);
	lvsizer->Add(m_listbox, 10, wxALL | wxEXPAND, 0);
	m_leftPanel->SetSizer(lvsizer);


	m_rightPanel->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	m_html = new wxHtmlWindow( m_rightPanel, ID_HTML, wxDefaultPosition, wxDefaultSize, wxHW_DEFAULT_STYLE|wxBORDER_NONE );
	m_html->SetMinClientSize( wxScaleSize( 300, 300 ) );

	m_run = new wxMetroButton( m_rightPanel, ID_RUN_MACRO, "Run macro", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW );
	m_stop = new wxMetroButton( m_rightPanel, ID_STOP_MACRO, "Stop" );
	m_stop->Hide();

	m_code = new wxMetroButton( m_rightPanel, ID_VIEW_CODE, "View code");
	
	wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );
	button_sizer->Add( m_run, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
	button_sizer->Add( m_stop, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
	button_sizer->AddStretchSpacer();
	button_sizer->Add( m_code, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
	
	
	m_macroUI = new wxPanel( m_rightPanel );
	m_macroUI->SetBackgroundColour( wxColour(240,240,240) );
	m_macroUISizer = new wxFlexGridSizer( 2 );
	m_macroUISizer->AddGrowableCol( 1 );
	m_macroUI->SetSizer( m_macroUISizer );

	wxBoxSizer *hsizer = new wxBoxSizer( wxHORIZONTAL );
	hsizer->Add( m_html, 1, wxALL|wxEXPAND, 0 );
	hsizer->Add( m_macroUI, 0, wxALL|wxEXPAND, 0 );
	
	wxBoxSizer *vsizer = new wxBoxSizer( wxVERTICAL );
	vsizer->Add( button_sizer, 0, wxALL|wxEXPAND, 0 );
	vsizer->Add( hsizer, 3, wxALL|wxEXPAND, 0 );

	m_rightPanel->SetSizer( vsizer );

	vsplit->SplitVertically(m_leftPanel, m_rightPanel, (int)(250*wxGetScreenHDScale()));
	vsplit->SetMinimumPaneSize(100);

	SetMinimumPaneSize( 100 );
	SetSashGravity( 1.0 );
	SplitHorizontally( vsplit, m_output, -250 );	

	ConfigurationChanged();
}

void MacroPanel::UpdateHtml()
{
	ClearUI();

	if ( m_listbox->Count()  == 0 )
	{
		m_html->SetPage( "<html><body><h3>No macros available for this project.</h3></body></html>" );
		m_run->Hide();
		m_code->Hide();
		m_rightPanel->Layout();
		return;
	}

	m_curMacroPath.clear();
	int sel = m_listbox->GetSelection();
	if ( sel >= 0 && sel < (int)m_macroList.size() )
		m_curMacroPath = wxFileName(m_macroList[sel]).GetFullPath();;
	
	if ( m_curMacroPath.IsEmpty() )
	{
		m_html->SetPage( "<html><body><h3>Select a macro from the list.</h3></body></html>" );
		m_run->Hide();
		m_code->Hide();
		m_rightPanel->Layout();
		return;
	}

	wxFile fp( m_curMacroPath );
	if ( fp.IsOpened() )
	{
		wxString buf;
		fp.ReadAll( &buf );

		int pos1 = buf.Find("/*@");
		int pos2 = buf.Find("@*/");
		if ( pos1 >= 0 && pos2 >= 0 && pos2 > pos1+3 )
		{
			wxFileName fn(m_curMacroPath);
			fn.MakeAbsolute();	
			wxString page( buf.Mid( pos1+3, pos2-pos1-3 ) );
			page.Replace( "$MACROPATH", fn.GetPath( wxPATH_UNIX ) );
			wxString script( buf.Mid(pos2+3) );		
			m_html->SetPage( "<html><body>\n<h3>" + fn.GetName() + "</h3>\n" + page + "\n</body></html>");
			CreateUI( buf );
		}
		else
			m_html->SetPage( "<html><body><h3>No description available for:</h3><br><br>" + m_curMacroPath + "</body></html>" );
		
		m_run->Show();
		m_code->Show();
		m_rightPanel->Layout();
	}
	else
	{
		m_run->Hide();
		m_code->Hide();
		m_rightPanel->Layout();
		m_html->SetPage("<html><body><h3>Could not open macro:</h3><br><br>" + m_curMacroPath + "</body></html>" );
		return;
	}

}

void MacroPanel::ClearUI()
{
	m_ui.clear();
	m_macroUISizer->Clear( true );
	m_rightPanel->Layout();
}

enum{ ID_SELECT_VARS = wxID_HIGHEST + 941, ID_EDIT_META, ID_VARLIST };
class VarListSelector : public wxPanel
{
	bool m_enableMeta;
	wxString m_prompt;
	bool m_onlyNumbers;
	wxListBox *m_list;
	wxArrayString m_names;
	StringHash m_meta;

public:
	VarListSelector( wxWindow *parent, bool only_num=false, bool en_meta=false, const wxString &prompt=wxEmptyString )
	  : wxPanel( parent ), m_enableMeta(en_meta), m_prompt(prompt), m_onlyNumbers( only_num )
	{
		SetMinClientSize( wxScaleSize( 200, 130 ) );
		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( m_list = new wxListBox( this, ID_VARLIST, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_HSCROLL ), 1, wxALL|wxEXPAND, 0 );

		wxBoxSizer *vsizer = new wxBoxSizer( wxVERTICAL );
		vsizer->Add( new wxButton( this, ID_SELECT_VARS , en_meta ? "Select..." : "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND|wxALIGN_TOP, 2 );
		if ( en_meta )
			vsizer->Add( new wxButton( this, ID_EDIT_META, "Edit...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND|wxALIGN_TOP, 2 );
		
		sizer->Add( vsizer, 0, wxALL|wxALIGN_TOP, 0 );
		SetSizer( sizer );
	}
	wxArrayString &GetVars() { return m_names; }
	StringHash &GetMeta() { return m_meta; }
	bool IsMetaDataEnabled() { return m_enableMeta; }
	void OnSelect( wxCommandEvent & )
	{
		wxArrayString nn, ll;
		Case *cc = SamApp::Window()->GetCurrentCase();
		if ( !cc ) {
			wxMessageBox("Error: there must be an active case to select input variables." );
			return;
		}

		for( VarInfoLookup::iterator it = cc->Variables().begin();
			it != cc->Variables().end();
			++it )
		{
			VarInfo &vi = *(it->second);

			if ( !vi.Label.IsEmpty()
				&& !(vi.Flags & VF_INDICATOR) 
				&& !(vi.Flags & VF_CALCULATED) )
			{
				if ( vi.Type == VV_NUMBER && vi.IndexLabels.size() > 0 )
					continue;

				if ( m_onlyNumbers && vi.Type != VV_NUMBER )
					continue;

				wxString label;
				if ( !vi.Group.IsEmpty() )
					label = vi.Group + "/" + vi.Label;
				else
					label = vi.Label;
							
				nn.Add( it->first );
				ll.Add( label );
			}
		}

		// automatically remove any variables
		// that are 'stale', i.e. no longer in the
		// currently active case
		size_t i=0;
		int nremove = 0;
		while( i < m_names.size() )
		{
			if ( nn.Index( m_names[i] ) == wxNOT_FOUND )
			{
				m_names.RemoveAt( i );
				nremove++;
			}
			else
				i++;
		}

		wxSortByLabels( nn, ll );
		if ( SelectVariableDialog::Run( "Select input variables", nn, ll, m_names ) || nremove > 0)
			UpdateList();
	}

	
	void OnEditOrListBoxDClick(wxCommandEvent & )
	{
		int isel = m_list->GetSelection();
		if ( isel < 0 ) {
			wxMessageBox("Please select a variable to edit its data.");
			return;
		}
		wxString L(m_prompt);
		L.Replace( "@var", m_list->GetStringSelection() );
		L.Replace( "\\n", "\n" );
		m_meta[m_names[isel]] = wxGetTextFromUser( L, "Edit data for: " + m_list->GetStringSelection(), m_meta[m_names[isel]], this );
		UpdateList();
	}

	void UpdateList()
	{
		m_list->Clear();
		Case *cc = SamApp::Window()->GetCurrentCase();
		if ( !cc ) return;

		for( size_t i=0;i<m_names.size();i++ )
		{
			wxString L(cc->Variables().Label( m_names[i] ));
			if ( L.IsEmpty() ) L = "{ " + m_names[i] + " }";
			if ( !cc->Variables().Units( m_names[i]).IsEmpty() )
				L += " (" +cc->Variables().Units( m_names[i]) + ")";

			if ( m_enableMeta && !m_meta[m_names[i]].IsEmpty())
				L += ": " + m_meta[m_names[i]];

			m_list->Append( L );
		}

	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( VarListSelector, wxPanel )
	EVT_BUTTON( ID_SELECT_VARS, VarListSelector::OnSelect )
	EVT_LISTBOX_DCLICK( ID_VARLIST, VarListSelector::OnEditOrListBoxDClick )
	EVT_BUTTON( ID_EDIT_META, VarListSelector::OnEditOrListBoxDClick )
END_EVENT_TABLE()

class SearchListDialog : public wxDialog
{
	AFSearchListBox *m_slb;
public:
	SearchListDialog( wxWindow *parent, const wxString &title, const wxArrayString &list )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, 
			wxScaleSize(500, 400), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		m_slb = new AFSearchListBox( this, wxID_ANY );
		m_slb->Append( list );

		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL );
		sizer->Add( m_slb, 1, wxALL|wxEXPAND, 5 );
		sizer->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 5 );
		SetSizer( sizer );
	}

	void SetSelection( int sel )
	{
		m_slb->SetSelection( sel );
	}

	int GetSelection()
	{
		return m_slb->GetSelection();
	}
};


enum { ID_SELECT_OUTPUT = wxID_HIGHEST+439 };

class SVOutputCtrl : public wxPanel 
{
	wxTextCtrl *m_text;
	wxArrayString m_names, m_labels;
	wxString m_curName;
public:
	SVOutputCtrl( wxWindow *parent )
		: wxPanel( parent, wxID_ANY )
	{

		m_text = new wxTextCtrl( this, wxID_ANY, "<none selected>", wxDefaultPosition, wxDefaultSize, wxTE_READONLY|wxBORDER_NONE );
		m_text->SetForegroundColour( *wxBLUE );

		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( m_text, 1, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
		sizer->Add( new wxButton( this, ID_SELECT_OUTPUT, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
		SetSizer( sizer );
		
		if ( Case *cc = SamApp::Window()->GetCurrentCase() )
		{
			wxArrayString labels, units, groups;
			Simulation::ListAllOutputs( cc->GetConfiguration(), 
				&m_names, &labels, &units, &groups, nullptr, true );

			for( size_t i=0;i<m_names.size();i++ )
			{
				wxString L(labels[i] );
				if ( !units[i].IsEmpty() )
					L += " (" + units[i] + ")";
				
				m_labels.Add( L );
			}

			wxSortByLabels( m_names, m_labels );
		}

	}

	wxString GetOutputVar() {
		return m_curName;
	}

	bool SetOutputVar( const wxString &var ) {
		int isel = m_names.Index( var );
		if ( isel >= 0 && isel < (int)m_labels.size() )
		{
			m_curName = m_names[isel];
			m_text->ChangeValue( m_labels[isel] );
			return true;
		}
		else
			return false;
	}

	void OnSelect( wxCommandEvent & )
	{
		SearchListDialog sld( this, "Select Output Variable", m_labels );
		sld.CenterOnScreen();
		if ( !m_curName.IsEmpty() )
			sld.SetSelection( m_names.Index( m_curName ) );

		if ( sld.ShowModal() == wxID_OK )
		{
			int isel = sld.GetSelection();
			if ( isel >= 0 && isel < (int)m_names.size() )
			{
				m_curName = m_names[isel];
				m_text->ChangeValue( m_labels[isel] );
			}
		}
	}

	DECLARE_EVENT_TABLE();
};


BEGIN_EVENT_TABLE( SVOutputCtrl, wxPanel )
	EVT_BUTTON(ID_SELECT_OUTPUT, SVOutputCtrl::OnSelect )
END_EVENT_TABLE()



enum { ID_SELECT_FILENAME = wxID_HIGHEST + 449 };


FileNameInputCtrl::FileNameInputCtrl(wxWindow *parent, const wxString &_label, const wxString &_filename, const wxString &_filter)
				: wxPanel(parent, wxID_ANY), m_fileName(_filename), m_filter(_filter), m_label(_label)
{

	if (m_fileName == wxEmptyString)
	{
		m_text = new wxTextCtrl(this, wxID_ANY, "<none selected>", wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxBORDER_NONE);
		m_dir = "";
	}
	else
	{
		m_text = new wxTextCtrl(this, wxID_ANY, m_fileName, wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxBORDER_NONE);
		m_dir = wxPathOnly(m_fileName);
	}
	m_text->SetForegroundColour(*wxBLUE);

	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	sizer->Add(m_text, 1, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sizer->Add(new wxButton(this, ID_SELECT_FILENAME, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	SetSizer(sizer);

	wxArrayString as = wxSplit(m_filter, ',');
	wxString filter = wxEmptyString, fil_front = wxEmptyString, fil_back = wxEmptyString;
	if (as.Count() > 0)
	{
		filter = "Files (";
		for (size_t i = 0; i < as.Count(); i++)
		{
			fil_front += "*." + as[i].Lower();
			if (i < (as.Count() -1)) fil_front += ",";
			fil_back += "*." + as[i].Lower();
			if (i < (as.Count() - 1)) fil_back += ";";
		}
		filter += fil_front + ")|" + fil_back;
	}
	if (filter != wxEmptyString) m_filter = filter;

}

void FileNameInputCtrl::OnSelect(wxCommandEvent &)
{
	wxFileDialog dlg(this, m_label, m_dir, m_fileName, m_filter, wxFD_OPEN | wxFD_FILE_MUST_EXIST);
	if (dlg.ShowModal() == wxID_OK)
	{
		m_fileName = dlg.GetPath();
		m_dir = wxPathOnly(m_fileName);
		m_text->ChangeValue(m_fileName);
	}
}

BEGIN_EVENT_TABLE(FileNameInputCtrl, wxPanel)
EVT_BUTTON(ID_SELECT_FILENAME, FileNameInputCtrl::OnSelect)
END_EVENT_TABLE()

FolderNameInputCtrl::FolderNameInputCtrl(wxWindow *parent, const wxString &_label, const wxString &folderName)
	: wxPanel(parent, wxID_ANY), m_dir(folderName), m_label(_label)
{
	if (m_dir == wxEmptyString)
	{
		m_text = new wxTextCtrl(this, wxID_ANY, "<none selected>", wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxBORDER_NONE);
		m_dir = "";
	}
	else
	{
		m_text = new wxTextCtrl(this, wxID_ANY, m_dir, wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxBORDER_NONE);
		m_dir = wxPathOnly(m_dir);
	}
	m_text->SetForegroundColour(*wxBLUE);

	wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
	sizer->Add(m_text, 1, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	sizer->Add(new wxButton(this, ID_SELECT_FILENAME, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL | wxALIGN_CENTER_VERTICAL, 2);
	SetSizer(sizer);
}

void FolderNameInputCtrl::OnSelect(wxCommandEvent &)
{
	wxDirDialog dlg(this, m_label, m_dir);
	if (dlg.ShowModal() == wxID_OK)
	{
		m_dir = dlg.GetPath();
		m_text->ChangeValue(m_dir);
	}
}

BEGIN_EVENT_TABLE(FolderNameInputCtrl, wxPanel)
EVT_BUTTON(ID_SELECT_FILENAME, FolderNameInputCtrl::OnSelect)
END_EVENT_TABLE()


void MacroPanel::CreateUI( const wxString &buf )
{
	ClearUI();

	wxArrayString lines = wxStringTokenize(buf, "\n");

	bool show_save_load = false;

	for( size_t i=0;i<lines.size();i++ )
	{
		wxString line( lines[i] );
		if ( line.Left(3) != "//@" ) continue;
		
		StringHash tab;
		tab.Split( line.Mid(3).Trim().Trim(false), ';', '=' );

		if ( tab["show_save_load_buttons"].Lower() == "true" )
		{
			show_save_load = true;
			continue;
		}
		
		wxString type( tab["type"] ),
			name( tab["name"] ),
			label( tab["label"] ), 
			value( tab["value"] );

		if ( name.IsEmpty() )
			continue;
		
		wxWindow *win = 0;
		bool expand_win = false;

		if ( type == "text" )
		{
			win = new wxTextCtrl( m_macroUI, wxID_ANY, value );
		}
		else if ( type == "checkbox" )
		{
			wxCheckBox *cb = new wxCheckBox( m_macroUI, wxID_ANY, wxEmptyString );
			if ( value.Lower() == "true" || value == "1" ) cb->SetValue( true );
			win = cb;
		}
		else if ( type == "integer" )
		{
			win = new wxNumericCtrl( m_macroUI, wxID_ANY, wxAtof(value), wxNUMERIC_INTEGER );
		}
		else if ( type == "number" )
		{
			win = new wxNumericCtrl( m_macroUI, wxID_ANY, wxAtof(value), wxNUMERIC_REAL );
		}
		else if ( type == "combo" )
		{
			int sel = -1;
			if ( tab.find("sel") != tab.end() )
				sel = wxAtoi( tab["sel"] );

			wxChoice *cho = new wxChoice( m_macroUI, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxStringTokenize(value,",") );
			if ( sel >= 0 && sel < (int)cho->GetCount())
				cho->SetSelection( sel );

			win = cho;
		}
		else if ( type == "inputs" )
		{
			bool only_num = false;
			if ( tab.find("only") != tab.end() )
				if ( tab["only"].Lower() == "numbers" )
					only_num=true;

			bool en_meta = false;
			if ( tab.find("meta") != tab.end() )
				if ( tab["meta"] == "true" )
					en_meta = true;
			wxString prompt;
			if ( tab.find("prompt") != tab.end() )
				prompt = tab["prompt"];

			win = new VarListSelector( m_macroUI, only_num, en_meta, prompt );
			expand_win = true;
		}
		else if ( type == "svoutput" )
		{
			win = new SVOutputCtrl( m_macroUI );
			expand_win = true;
		}
		else if (type == "filename")
		{
			if (value.Find("|") != wxNOT_FOUND)
			{
				wxArrayString as = wxSplit(value, '|');
				if (as.Count() > 1)
					win = new FileNameInputCtrl(m_macroUI, label, as[0], as[1]);
				else
					win = new FileNameInputCtrl(m_macroUI, label, value);
			}
			else
				win = new FileNameInputCtrl(m_macroUI,label,value);
			expand_win = true;
		}
		else if (type == "folder")
		{
			if (value.Find("|") != wxNOT_FOUND)
			{
				wxArrayString as = wxSplit(value, '|');
				if (as.Count() > 1)
					win = new FolderNameInputCtrl(m_macroUI, label, as[0]);
				else
					win = new FolderNameInputCtrl(m_macroUI, label, value);
			}
			else
				win = new FolderNameInputCtrl(m_macroUI, label, value);
			expand_win = true;
		}

		if ( win != 0 )
		{
			if (label.IsEmpty() ) label = "<empty label>";
			label.Replace("\\n", "\n");
			ui_item x;
			x.name = name;
			x.label = new wxStaticText( m_macroUI, wxID_ANY, label );
			m_macroUISizer->Add( x.label, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
			x.window = win;
			int flags = wxALL|wxALIGN_CENTER_VERTICAL;
			if ( expand_win ) flags |= wxEXPAND;
			m_macroUISizer->Add( x.window, 0, flags, 5 );
			m_ui.push_back( x );
		}
	}

	if(  m_ui.size() > 0 && show_save_load )
	{
		m_macroUISizer->Add( new wxStaticText( m_macroUI, wxID_ANY, "Input values:" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
		wxBoxSizer *sz = new wxBoxSizer( wxHORIZONTAL );
		sz->Add( new wxButton( m_macroUI, ID_SAVE_UIDATA, "Save...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL, 2 );
		sz->Add( new wxButton( m_macroUI, ID_LOAD_UIDATA, "Open...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL, 2 );
		m_macroUISizer->Add( sz, 0, wxALL, 5 );
	}

	m_rightPanel->Layout();
}



lk::vardata_t *MacroPanel::GetUIArgs()
{
	if ( m_ui.size() == 0 )
		return 0;

	lk::vardata_t *tab = new lk::vardata_t;
	GetUIArgs( *tab );
	return tab;
}

void MacroPanel::GetUIArgs( lk::vardata_t &table )
{
	table.empty_hash();

	for( size_t i=0;i<m_ui.size();i++ )
	{
		ui_item &x = m_ui[i];
		lk::vardata_t &item = table.hash_item( x.name );
		
		// note: numeric ctrl must be tested before textctrl, b/c numeric extends textctrl
		if ( wxNumericCtrl *num = dynamic_cast<wxNumericCtrl*>( x.window ) )
			item.assign( num->Value() );
		else if ( wxTextCtrl *txt = dynamic_cast<wxTextCtrl*>( x.window ) )
			item.assign( txt->GetValue() );
		else if ( wxCheckBox *chk = dynamic_cast<wxCheckBox*>( x.window ) )
			item.assign( chk->GetValue() ? 1.0 : 0.0 );
		else if ( SVOutputCtrl *svo = dynamic_cast<SVOutputCtrl*>( x.window ) ) // SVOutput must be before wxCHoice since it extends it
			item.assign( svo->GetOutputVar() );
		else if ( wxChoice *cho = dynamic_cast<wxChoice*>( x.window ) )
			item.assign( cho->GetStringSelection() );
		else if ( VarListSelector *vls = dynamic_cast<VarListSelector*>(x.window) )
		{
			item.empty_vector();
			wxArrayString list( vls->GetVars() );
			for( size_t i=0;i<list.size();i++ )
			{
				if ( vls->IsMetaDataEnabled() )
				{
					lk::vardata_t x;
					x.empty_vector();
					x.vec_append( list[i] );
					x.vec_append( vls->GetMeta()[ list[i] ] );
					item.vec()->push_back( x );
				}
				else
					item.vec_append( list[i] );
			}
		}
		else if (FileNameInputCtrl *fni = dynamic_cast<FileNameInputCtrl*>(x.window))
			item.assign(fni->GetFileName());
		else if (FolderNameInputCtrl *fni = dynamic_cast<FolderNameInputCtrl*>(x.window))
			item.assign(fni->GetFolderName());
	}
}

MacroPanel::ui_item *MacroPanel::FindItem( const wxString &name )
{
	for( size_t i=0;i<m_ui.size();i++)
		 if ( m_ui[i].name == name )
			 return &m_ui[i];

	return 0;
}


int MacroPanel::SetUIArgs( lk::vardata_t &table )
{
	if ( table.deref().type() != lk::vardata_t::HASH ) return 0;
	int iset = 0;
	lk::varhash_t &vh = *table.deref().hash();
	for ( lk::varhash_t::iterator it = vh.begin();
		it != vh.end();
		++it )
	{
		if( ui_item *ui = FindItem( it->first ) )
		{
			lk::vardata_t &item = (*(it->second)).deref();
		
			// note: numeric ctrl must be tested before textctrl, b/c numeric extends textctrl
			if ( wxNumericCtrl *num = dynamic_cast<wxNumericCtrl*>( ui->window ) )
			{
				num->SetValue( item.as_number() );
				iset++;
			}
			else if ( wxTextCtrl *txt = dynamic_cast<wxTextCtrl*>( ui->window ) )
			{
				txt->ChangeValue( item.as_string() );
				iset++;
			}
			else if ( wxCheckBox *chk = dynamic_cast<wxCheckBox*>( ui->window ) )
			{
				chk->SetValue( item.as_number() != 0.0 );
				iset++;
			}
			else if ( SVOutputCtrl *svo = dynamic_cast<SVOutputCtrl*>( ui->window ) ) // SVOutput must be before wxCHoice since it extends it
			{
				if ( svo->SetOutputVar( item.as_string() ) )
					iset++;
			}
			else if ( wxChoice *cho = dynamic_cast<wxChoice*>( ui->window ) )
			{
				cho->SetStringSelection( item.as_string() );
				iset++;
			}
			else if ( VarListSelector *vls = dynamic_cast<VarListSelector*>(ui->window) )
			{
				wxArrayString &list = vls->GetVars();
				list.clear();
				if ( item.type() == lk::vardata_t::VECTOR )
				{
					for( size_t i=0;i<item.length();i++ )
					{
						lk::vardata_t *xx = item.index(i);
						if( vls->IsMetaDataEnabled() && xx->type() == lk::vardata_t::VECTOR && xx->length() == 2 )
						{
							wxString name( xx->index(0)->as_string() );
							list.Add( name );
							vls->GetMeta()[ name ] = xx->index(1)->as_string();
						}
						else
						{
							list.Add( xx->as_string() );
						}
					}

				}
				vls->UpdateList();
				iset++;
			}
		}
	}

	return iset;
}

static void WriteLKData( wxOutputStream &out, lk::vardata_t &data )
{
	wxDataOutputStream os(out);
	os.Write16( data.type() );
	switch( data.type() )
	{
	case lk::vardata_t::NUMBER:
		os.WriteDouble( data.as_number() );
		break;
	case lk::vardata_t::STRING:
		os.WriteString( data.as_string() );
		break;
	case lk::vardata_t::VECTOR:
		os.Write32( data.length() );
		for( size_t i=0;i<data.length();i++ )
			WriteLKData( out, *data.index(i) );
		break;
	case lk::vardata_t::HASH:
		os.Write32( data.hash()->size() );
		for( lk::varhash_t::iterator it = data.hash()->begin();
			it != data.hash()->end();
			++it )
		{
			os.WriteString( it->first );
			WriteLKData( out, *(it->second) );
		}
		break;
	case lk::vardata_t::NULLVAL:
		// nothing to write for a null value
		break;
	}
}

static bool ReadLKData( wxInputStream &in, lk::vardata_t &data )
{
	wxUint32 len, i;
	wxDataInputStream is(in);
	wxUint16 type = is.Read16();
	switch( type )
	{
	case lk::vardata_t::NUMBER:
		data.assign( is.ReadDouble() );
		return true;
	case lk::vardata_t::STRING:
		data.assign( is.ReadString() );
		return true;
	case lk::vardata_t::VECTOR:
		len = is.Read32();
		data.empty_vector();
		if( len > 0 )
		{
			bool ok = true;
			data.vec()->resize( len );
			for( i=0;i<len;i++ )
			{
				lk::vardata_t item;
				ok = ok && ReadLKData( in, (*data.vec())[i] );
			}
			if (!ok) return false;
		}
		return true;
	case lk::vardata_t::HASH:
		len = is.Read32();
		data.empty_hash();
		if ( len > 0 )
		{
			bool ok = true;
			for( i=0;i<len;i++ )
			{
				wxString key( is.ReadString() );
				ok = ok && ReadLKData( in, data.hash_item(key) );
			}
			if (!ok) return false;
		}
	case lk::vardata_t::NULLVAL:
		return true;
	default:
		return false;
	}
}


int MacroPanel::ReadUIData( const wxString &file )
{
	wxFFileInputStream in( file );
	if ( in.IsOk() )
	{
		wxDataInputStream is(in);
		wxUint16 code = is.Read16();
		is.Read8();

		lk::vardata_t lkvalue;
		if (!ReadLKData( in, lkvalue )) return 0;
		if (is.Read16() != code) return 0;		
		return SetUIArgs( lkvalue );
	}
	else
		return 0;
}

bool MacroPanel::WriteUIData( const wxString &file )
{
	wxFFileOutputStream out( file );
	if ( out.IsOk() )
	{
		wxDataOutputStream ds(out);
		ds.Write16( 0x3415 );
		ds.Write8( 1 );

		lk::vardata_t lkvalue;
		GetUIArgs( lkvalue );
		WriteLKData( out, lkvalue );

		ds.Write16( 0x3415 );
		return true;
	}
	else
		return false;
}

static void ListScripts( const wxString &path, wxArrayString &list )
{
	wxDir dir( path );
	wxString file;
	if ( dir.IsOpened() )
	{
		bool ok = dir.GetFirst( &file, "*.lk", wxDIR_FILES );
		while( ok )
		{
			list.Add( path + "/" + file );
			ok = dir.GetNext( &file );
		}
	}
}

wxArrayString MacroEngine::ListMacrosForConfiguration( const wxString &tech, const wxString &fin )
{	
	wxArrayString list;
	ListScripts(  SamApp::GetRuntimePath() + "/macros/" + tech + "_" + fin, list );
	ListScripts(  SamApp::GetRuntimePath() + "/macros/" + tech, list );
	ListScripts(  SamApp::GetRuntimePath() + "/macros/" + fin, list );
	ListScripts(  SamApp::GetRuntimePath() + "/macros", list );
	return list;
}


void MacroPanel::ConfigurationChanged()
{
	wxString tech, fin;
	m_case->GetConfiguration( &tech, &fin );

	m_macroList = MacroEngine::ListMacrosForConfiguration( tech, fin );
		
	m_listbox->Clear();
	for( size_t i=0;i<m_macroList.size();i++ )
		m_listbox->Add( wxFileName(m_macroList[i]).GetName() );
	
	m_listbox->Refresh();

	UpdateHtml();

	m_leftPanel->Layout();

}

void MacroPanel::OnCommand( wxCommandEvent &evt )
{	
	switch( evt.GetId() )
	{
	case ID_RUN_MACRO:
		{
			if ( m_curMacroPath.IsEmpty() )
			{
				wxMessageBox("No macro selected");
				return;
			}

			wxFile fp( m_curMacroPath );
			if ( fp.IsOpened() )
			{
				wxString buf;
				fp.ReadAll( &buf );

				m_run->Hide();
				m_stop->Show();
				m_rightPanel->Layout();
				wxYield();

				Run( buf, GetUIArgs() );
				
				m_run->Show();
				m_stop->Hide();
				m_rightPanel->Layout();
			}
			else
				wxMessageBox("Could not open macro file: " + m_curMacroPath );
		}
		break;
	case ID_STOP_MACRO:
		Stop();
		break;
	case ID_MACRO_LIST:
		UpdateHtml();
		break;
	case ID_VIEW_CODE:		
		if ( wxFileExists(m_curMacroPath) )
		{
			if ( wxLKScriptWindow *sw = SamScriptWindow::FindOpenFile( m_curMacroPath ) )
				sw->Raise();
			else if ( wxLKScriptWindow *sw = SamScriptWindow::CreateNewWindow() )
				sw->Load( m_curMacroPath );
		}
		break;
	case ID_SAVE_UIDATA:
	{
		wxFileDialog dlg( this, "Save macro data", wxEmptyString, wxEmptyString, "SAM Macro Data Files (*.macro)|*.macro", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
		if ( dlg.ShowModal() == wxID_OK )
			if ( !WriteUIData( dlg.GetPath() ) )
				wxMessageBox("Error writing macro data to file:\n\n" + dlg.GetPath() );
	}
		break;
	case ID_LOAD_UIDATA:
	{
		wxFileDialog dlg( this, "Load macro data", wxEmptyString, wxEmptyString, "SAM Macro Data Files (*.macro)|*.macro", wxFD_OPEN );
		if ( dlg.ShowModal() == wxID_OK )
		{
			int ivals = ReadUIData( dlg.GetPath() );
			if ( ivals == 0 )
			{
				wxMessageBox("Error loading macro data file:\n\n" + dlg.GetPath() );
			}
			else if ( ivals < (int)m_ui.size() )
			{
				wxMessageBox("Only some of the loaded macro data was applicable to the currently selected macro.");
			}
		}
	}
		break;
	}
}

void MacroPanel::OnHtmlLink( wxHtmlLinkEvent &evt )
{
	wxString url( evt.GetLinkInfo().GetHref() );
	wxLaunchDefaultBrowser( url );
}

void MacroPanel::Output( const wxString &text )
{
	m_output->AppendText( text );
}


void MacroPanel::ClearOutput()
{
	m_output->Clear();
}
