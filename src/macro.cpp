#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/html/htmlwin.h>
#include <wx/dir.h>
#include <wx/file.h>
#include <wx/tokenzr.h>

#include <wex/metro.h>
#include <wex/utils.h>
#include <wex/lkscript.h>

#include <lk_parse.h>
#include <lk_eval.h>
#include <lk_stdlib.h>

#include "script.h"
#include "main.h"
#include "macro.h"
#include "casewin.h"


class macro_eval : public lk::eval
{
	MacroEngine *m_me;
public:
	macro_eval( lk::node_t *tree, lk::env_t *env, MacroEngine *me ) 
		: lk::eval( tree, env ), m_me(me)
	{
	}

	virtual bool on_run( int line )
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
	lk::env_t env;
	
	if ( args != 0 )
		env.assign( "macro", args );
	else
		env.assign( "macro", new lk::vardata_t ); // assign null to macro variable

	env.register_funcs( lk::stdlib_basic(), this );
	env.register_funcs( lk::stdlib_string(), this );
	env.register_funcs( lk::stdlib_math(), this );
	env.register_funcs( lk::stdlib_wxui(), this );
	env.register_funcs( lk::stdlib_basic(), this );
	env.register_funcs( wxLKPlotFunctions(), this );
	env.register_funcs( wxLKHttpFunctions(), this );
	env.register_funcs( wxLKMiscFunctions(), this );
	env.register_funcs( invoke_general_funcs() );
	env.register_funcs( sam_functions() );
	env.register_funcs( invoke_ssc_funcs() );
	env.register_func( fcall_out, this );
	env.register_func( fcall_outln, this );


	ClearOutput();
	
	lk::input_string p( script );
	lk::parser parse( p );
	
	lk::node_t *tree = parse.script();
				
	wxYield();
	bool success = false;

	if ( parse.error_count() != 0 
		|| parse.token() != lk::lexer::END)
	{
		Output("Parsing did not reach end of input.\n");
	}
	else
	{
		wxLKSetToplevelParent( SamApp::Window() );
		wxLKSetPlotTarget( NULL );

		m_stopFlag = false;
		macro_eval e( tree, &env, this );
		success = e.run();

		if ( !success )
		{
			Output("Script did not finish.\n");
			for (size_t i=0;i<e.error_count();i++)
				Output( e.get_error(i) + "\n");
		}
	}
	
	int i=0;
	while ( i < parse.error_count() )
		Output( parse.error(i++) );

	if( tree != 0 )
		delete tree;

	return success;
}

void MacroEngine::Stop()
{
	m_stopFlag = true;
}

class MacroOutputFrame;
static MacroOutputFrame *gs_macroOutputFrame = 0;
class MacroOutputFrame : public wxFrame
{
public:
	wxTextCtrl *txtoutput;
	MacroOutputFrame()
		: wxFrame( SamApp::Window(), wxID_ANY, "Macro Output", wxDefaultPosition, wxSize( 700, 250 ) )
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
	ID_VIEW_CODE };

BEGIN_EVENT_TABLE( MacroPanel, wxSplitterWindow )
	EVT_LISTBOX( ID_MACRO_LIST, MacroPanel::OnCommand )
	EVT_HTML_LINK_CLICKED( ID_HTML, MacroPanel::OnHtmlLink )
	EVT_BUTTON( ID_RUN_MACRO, MacroPanel::OnCommand )
	EVT_BUTTON( ID_STOP_MACRO, MacroPanel::OnCommand )
	EVT_BUTTON( ID_VIEW_CODE, MacroPanel::OnCommand )
END_EVENT_TABLE()

MacroPanel::MacroPanel( wxWindow *parent, Case *cc )
	: wxSplitterWindow( parent, wxID_ANY ), m_case(cc)
{
	m_output = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxBORDER_NONE);
	
	wxSplitterWindow *vsplit = new wxSplitterWindow( this, wxID_ANY );
	
	m_listbox = new wxMetroListBox( vsplit, ID_MACRO_LIST );
	m_rightPanel = new wxPanel( vsplit );

	m_rightPanel->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	m_html = new wxHtmlWindow( m_rightPanel, ID_HTML, wxDefaultPosition, wxDefaultSize, wxHW_DEFAULT_STYLE|wxBORDER_NONE );

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
	m_macroUI->SetBackgroundColour( *wxWHITE );
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

	vsplit->SplitVertically( m_listbox, m_rightPanel, 250 );
	vsplit->SetMinimumPaneSize( 100 );

	SetMinimumPaneSize( 100 );
	SetSashGravity( 1.0 );
	SplitHorizontally( vsplit, m_output, -250 );	

	ConfigurationChanged();
}

void MacroPanel::ListScripts( const wxString &path, wxArrayString &list )
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

		int pos1 = buf.Find("/**");
		int pos2 = buf.Find("**/");
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

enum{ ID_SELECT_VARS = wxID_HIGHEST + 941 };
class VarListSelector : public wxPanel
{
	bool m_onlyNumbers;
	wxListBox *m_list;
	wxArrayString m_names;
public:
	VarListSelector( wxWindow *parent, bool only_num=false )
		: wxPanel( parent ), m_onlyNumbers( only_num )
	{
		SetInitialSize( wxSize( 180, 130 ) );
		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( m_list = new wxListBox( this, wxID_ANY ), 1, wxALL|wxEXPAND, 0 );
		sizer->Add( new wxButton( this, ID_SELECT_VARS , "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxALIGN_TOP, 2 );
		SetSizer( sizer );
	}
	wxArrayString GetVars() { return m_names; }
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
			m_list->Append( L );
		}
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( VarListSelector, wxPanel )
	EVT_BUTTON( ID_SELECT_VARS, VarListSelector::OnSelect )
END_EVENT_TABLE()

class SVOutputCtrl : public wxChoice 
{
	wxArrayString m_names;
public:
	SVOutputCtrl( wxWindow *parent )
		: wxChoice( parent, wxID_ANY )
	{
		if ( Case *cc = SamApp::Window()->GetCurrentCase() )
		{
			wxArrayString labels, units, groups;
			Simulation::ListAllOutputs( cc->GetConfiguration(), 
				&m_names, &labels, &units, &groups, true );

			for( size_t i=0;i<m_names.size();i++ )
			{
				wxString L(groups[i] );
				if ( L.IsEmpty() ) L = "Miscellaneous";
				L += "/" + labels[i];
				if ( !units[i].IsEmpty() )
					L += " (" + units[i] + ")";
				Append( L );
			}
		}

	}

	wxString GetOutputVar() {
		int sel = GetSelection();
		if ( sel >= 0  && sel < m_names.size() ) return m_names[sel];
		else return wxEmptyString;
	}
};

void MacroPanel::CreateUI( const wxString &buf )
{
	ClearUI();

	wxArrayString lines = wxStringTokenize(buf, "\n");

	for( size_t i=0;i<lines.size();i++ )
	{
		wxString line( lines[i] );
		if ( line.Left(3) != "//@" ) continue;

		StringHash tab;
		tab.Split( line.Mid(3).Trim().Trim(false), ';', '=' );
		
		wxString type( tab["type"] ),
			name( tab["name"] ),
			label( tab["label"] ), 
			value( tab["value"] );

		if ( name.IsEmpty() )
			continue;
		
		wxWindow *win = 0;

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
			win = new wxNumericCtrl( m_macroUI, wxID_ANY, wxAtof(value), wxNumericCtrl::INTEGER );
		}
		else if ( type == "number" )
		{
			win = new wxNumericCtrl( m_macroUI, wxID_ANY, wxAtof(value), wxNumericCtrl::REAL );
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
			win = new VarListSelector( m_macroUI, only_num );
		}
		else if ( type == "svoutput" )
		{
			win = new SVOutputCtrl( m_macroUI );
		}

		if ( win != 0 )
		{
			if (label.IsEmpty() ) label = "<empty label>";
			ui_item x;
			x.name = name;
			x.label = new wxStaticText( m_macroUI, wxID_ANY, label );
			m_macroUISizer->Add( x.label, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
			x.window = win;
			m_macroUISizer->Add( x.window, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
			m_ui.push_back( x );
		}
	}

	m_rightPanel->Layout();
}

lk::vardata_t *MacroPanel::GetUIArgs()
{
	if ( m_ui.size() == 0 )
		return 0;

	lk::vardata_t *tab = new lk::vardata_t;
	tab->empty_hash();

	for( size_t i=0;i<m_ui.size();i++ )
	{
		ui_item &x = m_ui[i];
		lk::vardata_t &item = tab->hash_item( x.name );
		
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
				item.vec_append( list[i] );
		}
	}

	return tab;
}



void MacroPanel::ConfigurationChanged()
{
	wxString tech, fin;
	m_case->GetConfiguration( &tech, &fin );

	m_macroList.clear();
	ListScripts(  SamApp::GetRuntimePath() + "/macros/" + tech + "_" + fin, m_macroList );
	ListScripts(  SamApp::GetRuntimePath() + "/macros/" + tech, m_macroList );
	ListScripts(  SamApp::GetRuntimePath() + "/macros/" + fin, m_macroList );
	ListScripts(  SamApp::GetRuntimePath() + "/macros", m_macroList );
		
	m_listbox->Clear();
	for( size_t i=0;i<m_macroList.size();i++ )
		m_listbox->Add( wxFileName(m_macroList[i]).GetName() );
	
	m_listbox->Refresh();

	UpdateHtml();
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
			if ( ScriptWindow *sw = ScriptWindow::FindOpenFile( m_curMacroPath ) )
				sw->Raise();
			else if ( ScriptWindow *sw = ScriptWindow::CreateNewWindow() )
				sw->Load( m_curMacroPath );
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
