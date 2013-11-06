#include <wx/splitter.h>
#include <wx/busyinfo.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/gbsizer.h>

#include <wex/exttext.h>
#include <wex/lkscript.h>
#include <wex/metro.h>
#include <wex/uiform.h>
#include <wex/utils.h>

#include <lk_lex.h>
#include <lk_parse.h>

#include "ide.h"
#include "main.h"
#include "equations.h"
#include "inputpage.h"

enum { ID_STARTUP_EDITOR = wxID_HIGHEST+124,
	ID_STARTUP_SAVE,
	ID_STARTUP_FIND,
	ID_STARTUP_HELP,
	ID_STARTUP_RESTART
};

BEGIN_EVENT_TABLE( StartupScriptPanel, wxPanel )
	EVT_BUTTON( ID_STARTUP_SAVE, StartupScriptPanel::OnCommand )
	EVT_BUTTON( ID_STARTUP_FIND, StartupScriptPanel::OnCommand )
	EVT_BUTTON( ID_STARTUP_HELP, StartupScriptPanel::OnCommand )
	EVT_BUTTON( ID_STARTUP_RESTART, StartupScriptPanel::OnCommand )
END_EVENT_TABLE()

extern lk::fcall_t *startup_funcs();

StartupScriptPanel::StartupScriptPanel( wxWindow *parent )	
	: wxPanel( parent )
{
	wxBoxSizer *sz_startup_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_SAVE, "Save"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_FIND, "Find..."), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_HELP, "Help"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_RESTART, "Restart SAM"), 0, wxALL|wxEXPAND, 2 );	
	m_scriptCtrl = new wxLKScriptCtrl( this, ID_STARTUP_EDITOR );
	m_scriptCtrl->RegisterLibrary( startup_funcs(), "Startup Functions", 0 );
	wxBoxSizer *sz_startup_main = new wxBoxSizer( wxVERTICAL );
	sz_startup_main->Add( sz_startup_tools, 0, wxALL|wxEXPAND, 2 );
	sz_startup_main->Add( m_scriptCtrl, 1, wxALL|wxEXPAND, 0 );
	
	SetSizer( sz_startup_main );

	if (!m_scriptCtrl->ReadAscii( SamApp::GetRuntimePath() + "/startup.lk" ) )
		wxMessageBox("Error loading startup.lk", "notice", wxOK, this);
}

void StartupScriptPanel::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_STARTUP_SAVE:
		{
			wxBusyInfo savemsg( "Writing startup.lk to disk" );
			wxYield();
			wxMilliSleep(300);
			if ( !m_scriptCtrl->WriteAscii( SamApp::GetRuntimePath() + "/startup.lk" ))
				wxMessageBox("Error writing startup.lk to disk" , "notice", wxOK, this);
			break;
		}
	case ID_STARTUP_FIND:
		m_scriptCtrl->ShowFindReplaceDialog();
		break;
	case ID_STARTUP_HELP:
		m_scriptCtrl->ShowHelpDialog( this );
		break;
	case ID_STARTUP_RESTART:
		{
			if ( m_scriptCtrl->IsModified() 
				&& wxYES == wxMessageBox( "startup.lk is modified. Save first?", "Query", wxYES_NO, this) )
				m_scriptCtrl->WriteAscii( SamApp::GetRuntimePath() + "/startup.lk" );
		
			wxBusyInfo restartmsg( "Restarting SAM... messages in debug output screen" );
				wxYield();
				wxMilliSleep(300);
			SamApp::Restart();
		}
		break;
	}
}


ExtendedFormData::ExtendedFormData( VarDatabase *vdb ) : m_vdb(vdb) { }
ExtendedFormData::~ExtendedFormData() { m_vdb = 0; }
bool ExtendedFormData::GetMetaData( const wxString &name,
		wxString *label, wxString *units, wxColour *colour )
{
	if ( !m_vdb ) return false;

	if ( VarInfo *vv = m_vdb->Lookup( name ) )
	{
		if (vv->Flags & VF_HIDE_LABELS) return false;

		*label = vv->Label;
		*units = vv->Units;
		if ( vv->Flags & VF_INDICATOR ) *colour = wxColour(90,90,90);
		else if ( vv->Flags & VF_CALCULATED ) *colour = *wxBLUE;
		else *colour = *wxBLACK;

		return true;
	}
	
	return false;
}


enum { 
	ID_FORM_EDITOR = wxID_HIGHEST + 231,
	ID_CALLBACK_EDITOR,
	ID_EQUATION_EDITOR,

	ID_FORM_LIST,
	ID_FORM_LIST_REFRESH,
	ID_FORM_ADD,
	ID_FORM_SAVE,
	ID_FORM_DELETE,
	ID_FORM_TEST,

	ID_VAR_SYNC,
	ID_VAR_ADD,
	ID_VAR_DELETE,

	ID_VAR_LIST,
	ID_VAR_NAME,
	ID_VAR_TYPE, 
	ID_VAR_LABEL,
	ID_VAR_UNITS,
	ID_VAR_GROUP,
	ID_VAR_INDEX_LABELS,
	ID_VAR_DEFAULT_VALUE,
	ID_VAR_FL_HIDELABELS,
	ID_VAR_FL_PARAMETRIC,
	ID_VAR_FL_INDICATOR,
	ID_VAR_FL_CALCULATED,

	ID_CALLBACK_FIND,
	ID_CALLBACK_CHECK,
	ID_CALLBACK_GOTO,
	ID_CALLBACK_HELP,

	ID_CALLBACK_GOTO_1,
	ID_CALLBACK_GOTO_100 = ID_CALLBACK_GOTO_1 + 100,

	ID_EQUATION_FIND,
	ID_EQUATION_SCAN,
	ID_EQUATION_HELP

};

BEGIN_EVENT_TABLE( UIEditorPanel, wxPanel )
	EVT_LISTBOX( ID_FORM_LIST, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_LIST_REFRESH, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_ADD, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_SAVE, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_DELETE, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_TEST, UIEditorPanel::OnFormTest )
	
	EVT_BUTTON( ID_VAR_SYNC, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_ADD, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_DELETE, UIEditorPanel::OnCommand )

	EVT_LISTBOX( ID_VAR_LIST, UIEditorPanel::OnCommand )
	EVT_TEXT_ENTER( ID_VAR_NAME, UIEditorPanel::OnCommand )
	EVT_TEXT_ENTER( ID_VAR_LABEL, UIEditorPanel::OnCommand )
	EVT_TEXT_ENTER( ID_VAR_UNITS, UIEditorPanel::OnCommand )

	EVT_BUTTON( ID_CALLBACK_FIND, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_CALLBACK_CHECK, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_CALLBACK_GOTO, UIEditorPanel::OnCommand )
	EVT_MENU_RANGE( ID_CALLBACK_GOTO_1, ID_CALLBACK_GOTO_100, UIEditorPanel::OnCallbackGoto )
	EVT_BUTTON( ID_CALLBACK_HELP, UIEditorPanel::OnCommand )

	EVT_BUTTON( ID_EQUATION_FIND, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_EQUATION_SCAN, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_EQUATION_HELP, UIEditorPanel::OnCommand )

	EVT_UIFORM_SELECT( ID_FORM_EDITOR, UIEditorPanel::OnFormSelectObject )
END_EVENT_TABLE()

UIEditorPanel::UIEditorPanel( wxWindow *parent )
	: wxPanel( parent ), m_formData( &m_varData )
{
	wxBoxSizer *sz_form_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_form_tools->Add( new wxButton( this, ID_FORM_LIST_REFRESH, "Refresh list"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_FORM_ADD, "Add..."), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_FORM_SAVE, "Save"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_FORM_DELETE, "Delete"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_FORM_TEST, "Test"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->AddStretchSpacer();	
	sz_form_tools->Add( new wxButton( this, ID_VAR_SYNC, "Sync vars"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_ADD, "Add var..."), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_DELETE, "Delete var"), 0, wxALL|wxEXPAND, 2 );
		
	m_uiPropEditor = new wxUIPropertyEditor( this, wxID_ANY );
	m_formList = new wxListBox( this, ID_FORM_LIST, wxDefaultPosition, wxSize(300, 300), 0, 0, wxLB_SINGLE|wxBORDER_NONE );
	
	wxBoxSizer *sz_form_left = new wxBoxSizer( wxVERTICAL );
	sz_form_left->Add( new wxStaticText( this, wxID_ANY, "UI Forms" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_form_left->Add( m_formList, 1, wxALL|wxEXPAND, 2 );
	sz_form_left->Add( new wxStaticText( this, wxID_ANY, "Object Properties" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_form_left->Add( m_uiPropEditor, 1, wxALL|wxEXPAND, 2 );

	
	m_varList = new wxListBox( this, ID_VAR_LIST, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_SINGLE|wxBORDER_NONE );
	m_varName = new wxExtTextCtrl( this, ID_VAR_NAME, wxEmptyString );
	wxString strtypes[] = { "Invalid", "Number", "Array", "Matrix", "String", "Table" };
	m_varType = new wxChoice( this, ID_VAR_TYPE, wxDefaultPosition, wxDefaultSize, 6, strtypes );
	m_varLabel = new wxExtTextCtrl( this, ID_VAR_LABEL, wxEmptyString );
	m_varUnits = new wxExtTextCtrl( this, ID_VAR_UNITS, wxEmptyString );
	m_varGroup = new wxExtTextCtrl( this, ID_VAR_GROUP, wxEmptyString );
	m_varIndexLabels = new wxExtTextCtrl( this, ID_VAR_INDEX_LABELS, wxEmptyString );
	m_varDefaultValue = new wxExtTextCtrl( this, ID_VAR_DEFAULT_VALUE, wxEmptyString );
	m_varFlagHideLabels = new wxCheckBox( this, ID_VAR_FL_HIDELABELS, "Hide labels?" );
	m_varFlagParametric = new wxCheckBox( this, ID_VAR_FL_PARAMETRIC, "Parametric?" );
	m_varFlagIndicator = new wxCheckBox( this, ID_VAR_FL_INDICATOR, "Indicator?" );
	m_varFlagCalculated = new wxCheckBox( this, ID_VAR_FL_CALCULATED, "Calculated?" );

	wxGridBagSizer *sz_var_fields = new wxGridBagSizer(1,1);
	sz_var_fields->SetFlexibleDirection( wxHORIZONTAL );
	
	sz_var_fields->Add( new wxStaticText( this, wxID_ANY, "Name:" ), wxGBPosition(0,0), wxDefaultSpan, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_var_fields->Add( m_varName, wxGBPosition(0, 1), wxGBSpan(1,2), wxALL|wxEXPAND, 2 );
	sz_var_fields->Add( m_varType, wxGBPosition(0, 3), wxDefaultSpan, wxALL|wxEXPAND, 2 );

	sz_var_fields->Add( new wxStaticText( this, wxID_ANY, "Label:" ), wxGBPosition(1,0), wxDefaultSpan, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_var_fields->Add( m_varLabel, wxGBPosition(1, 1), wxGBSpan(1,3), wxALL|wxEXPAND, 2  );

	sz_var_fields->Add( new wxStaticText( this, wxID_ANY, "Units:" ), wxGBPosition(2,0), wxDefaultSpan, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_var_fields->Add( m_varUnits, wxGBPosition(2, 1), wxGBSpan(1,3), wxALL|wxEXPAND, 2  );

	sz_var_fields->Add( new wxStaticText( this, wxID_ANY, "Group:" ), wxGBPosition(3,0), wxDefaultSpan, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_var_fields->Add( m_varGroup, wxGBPosition(3, 1), wxGBSpan(1,3), wxALL|wxEXPAND, 2  );

	sz_var_fields->Add( new wxStaticText( this, wxID_ANY, "Index Labels:" ), wxGBPosition(4,0), wxDefaultSpan, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_var_fields->Add( m_varIndexLabels, wxGBPosition(4, 1), wxGBSpan(1,3), wxALL|wxEXPAND, 2  );

	sz_var_fields->Add( new wxStaticText( this, wxID_ANY, "Default Value:" ), wxGBPosition(5,0), wxDefaultSpan, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_var_fields->Add( m_varDefaultValue, wxGBPosition(5, 1), wxGBSpan(1,3), wxALL|wxEXPAND, 2  );

	sz_var_fields->Add( m_varFlagHideLabels, wxGBPosition(6,0), wxGBSpan(1,1), wxALL|wxALIGN_CENTER_VERTICAL, 3  );
	sz_var_fields->Add( m_varFlagParametric, wxGBPosition(6,1), wxGBSpan(1,1), wxALL|wxALIGN_CENTER_VERTICAL, 3  );
	sz_var_fields->Add( m_varFlagIndicator, wxGBPosition(7,0), wxGBSpan(1,1), wxALL|wxALIGN_CENTER_VERTICAL, 3  );
	sz_var_fields->Add( m_varFlagCalculated, wxGBPosition(7,1), wxGBSpan(1,1), wxALL|wxALIGN_CENTER_VERTICAL, 3 );

	sz_var_fields->AddGrowableCol( 1 );

	wxBoxSizer *sz_var_main = new wxBoxSizer( wxVERTICAL );
	sz_var_main->Add( new wxStaticText( this, wxID_ANY, "Variables" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_var_main->Add( m_varList, 1, wxALL|wxEXPAND, 2 );
	sz_var_main->Add( sz_var_fields,1, wxALL|wxEXPAND, 0 );

	wxSplitterWindow *center_split = new wxSplitterWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE );
	m_uiFormEditor = new wxUIFormDesigner( center_split, ID_FORM_EDITOR );
	m_uiFormEditor->SetBackgroundColour( wxColour(120,120,120) );

	wxPanel *scripts_panel = new wxPanel( center_split );

	wxBoxSizer *sz_callback_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_callback_tools->Add( new wxStaticText( scripts_panel, wxID_ANY, "Callbacks"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_callback_tools->AddSpacer(5);
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_FIND, "Find..." ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_CHECK, "Check code" ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_GOTO, "Goto..." ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_HELP, "Help" ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->AddStretchSpacer();

	m_callbackScript = new wxLKScriptCtrl( scripts_panel, ID_CALLBACK_EDITOR );
	wxBoxSizer *sz_scripts_left = new wxBoxSizer( wxVERTICAL );
	sz_scripts_left->Add( sz_callback_tools, 0, wxALL|wxEXPAND, 2 );
	sz_scripts_left->Add( m_callbackScript, 1, wxALL|wxEXPAND, 0 );

	wxBoxSizer *sz_equation_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_equation_tools->Add( new wxStaticText( scripts_panel, wxID_ANY, "Equations"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_equation_tools->AddSpacer(5);
	sz_equation_tools->Add( new wxButton( scripts_panel, ID_EQUATION_FIND, "Find..." ), 0, wxALL|wxEXPAND, 2 );
	sz_equation_tools->Add( new wxButton( scripts_panel, ID_EQUATION_SCAN, "Scan" ), 0, wxALL|wxEXPAND, 2 );
	sz_equation_tools->Add( new wxButton( scripts_panel, ID_EQUATION_HELP, "Help" ), 0, wxALL|wxEXPAND, 2 );
	sz_equation_tools->AddStretchSpacer();

	m_equationScript = new wxLKScriptCtrl( scripts_panel, ID_EQUATION_EDITOR, wxDefaultPosition, wxDefaultSize,
		wxLK_STDLIB_BASIC | wxLK_STDLIB_STRING | wxLK_STDLIB_MATH );
	wxBoxSizer *sz_scripts_right = new wxBoxSizer( wxVERTICAL );
	sz_scripts_right->Add( sz_equation_tools, 0, wxALL|wxEXPAND, 2 );
	sz_scripts_right->Add( m_equationScript, 1, wxALL|wxEXPAND, 0 );

	wxBoxSizer *sz_scripts_main = new wxBoxSizer( wxHORIZONTAL );
	sz_scripts_main->Add( sz_scripts_left, 1, wxALL|wxEXPAND, 0 );
	sz_scripts_main->Add( sz_scripts_right, 1, wxALL|wxEXPAND, 0 );
	scripts_panel->SetSizer( sz_scripts_main );

	center_split->SplitHorizontally( m_uiFormEditor, scripts_panel, -400 );
	center_split->SetSashGravity( 1.0 );
	center_split->SetMinimumPaneSize( 20 );

	wxBoxSizer *sz_form_main = new wxBoxSizer( wxHORIZONTAL );
	sz_form_main->Add( sz_form_left, 0, wxALL|wxEXPAND, 0 );
	sz_form_main->Add( center_split, 1, wxALL|wxEXPAND, 4 );
	sz_form_main->Add( sz_var_main, 0, wxALL|wxEXPAND, 0 );

	m_uiFormEditor->SetPropertyEditor( m_uiPropEditor );
	m_uiFormEditor->SetFormData( &m_formData );

	m_curVar = 0;


	wxBoxSizer *sz_form_top = new wxBoxSizer( wxVERTICAL );
	sz_form_top->Add( sz_form_tools, 0, wxALL|wxEXPAND, 2 );
	sz_form_top->Add( sz_form_main, 1, wxALL|wxEXPAND, 0 );
	this->SetSizer( sz_form_top );

	LoadFormList();
	LoadVarList();
	VarInfoToForm( NULL );
}

void UIEditorPanel::LoadFormList( const wxString &sel )
{
	m_formList->Freeze();
	m_formList->Clear();

	wxDir dir( SamApp::GetRuntimePath() + "/ui" );
	if ( dir.IsOpened() )
	{
		wxString file;
		bool has_more = dir.GetFirst( &file, "*.ui", wxDIR_FILES  );
		while( has_more )
		{
			wxFileName fn( file );
			m_formList->Append( fn.GetName() );
			has_more = dir.GetNext( &file );
		}
	}


	m_formList->Thaw();
	
	if ( !sel.IsEmpty() )
		m_formList->SetStringSelection( sel );
}

void UIEditorPanel::OnCallbackGoto( wxCommandEvent &evt )
{
	int idx = evt.GetId() - ID_CALLBACK_GOTO_1;
	if ( idx >= 0 && idx < m_callbackGotoList.Count() )
	{
		int pos = m_callbackScript->FindText( 0, m_callbackScript->GetLength(), m_callbackGotoList[idx], 0 );
		if ( pos >= 0 )
		{
			m_callbackScript->SetSelection( pos, pos+m_callbackGotoList[idx].Len() );
		}
		else
		{
			wxString text = "\n" + m_callbackGotoList[idx] + "\n\tmsgbox('test');\n};\n";
			m_callbackScript->AppendText( text );
			m_callbackScript->SetSelection( m_callbackScript->GetLength()-text.Len()-1, text.Len() );
		}

		m_callbackScript->EnsureCaretVisible();
	}
}




class InputPageTestPanel : public InputPageBase
{
	
	VarDatabase m_vars;
	EqnDatabase m_eqns;
	CallbackDatabase m_call;
	VarTable m_vals;

public:
	InputPageTestPanel( wxWindow *parent, int id )
		: InputPageBase( parent, id ) { }
	virtual ~InputPageTestPanel() { /* nothing to do */ }

	bool Setup( const wxString &name )
	{
		bool ok = Load( name );
		ok = ok && m_vars.LoadFile( SamApp::GetRuntimePath() + "/ui/" + name + ".vars" );
		ok = ok && m_eqns.LoadFile( SamApp::GetRuntimePath() + "/ui/" + name + ".eqn" );
		ok = ok && m_call.LoadFile( SamApp::GetRuntimePath() + "/ui/" + name + ".cb" );

		// setup variable value table
		m_vals.clear();
		wxArrayString list = m_vars.ListAll();
		for( size_t i=0;i<list.size();i++ )
			m_vals.Set( list[i], m_vars.InternalDefaultValue( list[i] ) );
		
		return ok;
	}
	
	virtual VarDatabase &GetVariables()	{ return m_vars; }
	virtual EqnDatabase &GetEquations() { return m_eqns; }
	virtual CallbackDatabase &GetCallbacks() { return m_call; }
	virtual VarTable &GetVarTable() { return m_vals; }

	virtual void OnInputChanged( wxUIObject *obj )
	{
		if( VarValue *vval = m_vals.Get( obj->GetName() ) )
		{
			// store the value
			DataExchange( obj, *vval, OBJ_TO_VAR );
			EqnEvaluator eval( &m_vals, &m_eqns );
			size_t n = eval.Changed( obj->GetName() );
			if ( n > 0 )
			{
				wxArrayString list = eval.GetUpdated();
				for( size_t i=0;i<list.size();i++ )
				{
					VarValue *upd = m_vals.Get( list[i] );
					wxUIObject *obj = Find( list[i] );
					if ( upd && obj )
						DataExchange( obj, *upd, VAR_TO_OBJ );
				}

			}
		}

		wxLogStatus( "iptp: variable " + obj->GetName() + " changed and value stored" );
	}
};

void UIEditorPanel::OnFormTest( wxCommandEvent & )
{
	wxFrame *frame = new wxFrame( this, wxID_ANY, "Test: " + m_formName, wxDefaultPosition, wxSize( 500, 400 ) );
	InputPageTestPanel *ip = new InputPageTestPanel( frame, wxID_ANY );

	if (!ip->Load( m_formName ))
		wxMessageBox("test form load not successful", "notice", wxOK, this );

	frame->SetClientSize( ip->GetClientSize() );
	frame->Show();
}

void UIEditorPanel::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_FORM_LIST:
		if ( !m_formName.IsEmpty() )
		{
			SyncFormUIToDataBeforeWriting();
			WriteForm( m_formName );
		}

		if (!LoadForm( m_formList->GetStringSelection() ))
			wxMessageBox("error loading form: " + m_formList->GetStringSelection(), "notice", wxOK, this );
		break;
	case ID_FORM_LIST_REFRESH:
		LoadFormList();
		break;
	case ID_FORM_ADD:
		{
			wxString name = wxGetTextFromUser( "Enter new form name:", "query", wxEmptyString, this );
			if ( wxFileExists(SamApp::GetRuntimePath()  + "/ui/" + name + ".ui" ) )
			{
				wxMessageBox("that form already exists.", "notice", wxOK, this);
				return;
			}
			m_uiFormEditor->SetFormData( 0 ) ;
			m_formData.DeleteAll();
			m_varData.Clear();
			m_curVar = 0;
			m_callbackScript->SetText( wxEmptyString );
			m_equationScript->SetText( wxEmptyString );
			WriteForm( name );
			if ( !LoadForm( name ) )
				wxMessageBox("error loading form: " + name, "notice", wxOK, this );
			LoadFormList( name );
		}
		break;
	case ID_FORM_DELETE:
		{
			wxString form = m_formList->GetStringSelection();
			if ( wxYES == wxMessageBox( "really delete .ui/.vars/.cb/.eqn for: " + form + " ?  cannot undo!", "query", wxYES_NO, this ) )
			{
				wxString ff = SamApp::GetRuntimePath() + "/ui/" + form + ".ui";
				if ( wxFileExists( ff ) ) wxRemoveFile( ff );
				
				ff = SamApp::GetRuntimePath() + "/ui/" + form + ".vars";
				if ( wxFileExists( ff ) ) wxRemoveFile( ff );

				ff = SamApp::GetRuntimePath() + "/ui/" + form + ".cb";
				if ( wxFileExists( ff ) ) wxRemoveFile( ff );

				ff = SamApp::GetRuntimePath() + "/ui/" + form + ".eqn";
				if ( wxFileExists( ff ) ) wxRemoveFile( ff );

				m_formName.Clear();
				m_formData.DeleteAll();
				m_varData.Clear();
				m_curVar = 0;
				m_uiFormEditor->SetFormData( &m_formData );
				m_uiFormEditor->Refresh();
				m_callbackScript->Clear();
				m_equationScript->Clear();
				LoadFormList();
				LoadVarList();
				VarInfoToForm( NULL );
			}
		}
		break;
	case ID_FORM_SAVE:
		{
			wxBusyInfo info( "Saving form and variable data: " + m_formName );
			wxYield();
			wxMilliSleep( 300 );
			
			SyncFormUIToDataBeforeWriting();

			if (!WriteForm( m_formName ))
				wxMessageBox("error writing form: " + m_formName, "notice", wxOK, this );
		}
		break;
	case ID_VAR_SYNC:
		{
			std::vector<wxUIObject*> objects = m_formData.GetObjects();
			for( size_t i=0;i<objects.size();i++ )
			{
				wxString type = objects[i]->GetTypeName();
				wxString name = objects[i]->GetName();
				if ( m_varData.Lookup( name ) == 0 )
				{
					if ( type == "Numeric" )
						m_varData.Add( name, VV_NUMBER );
					else if ( type == "Choice" )
						m_varData.Add( name, VV_NUMBER );
					else if ( type == "ListBox" )
						m_varData.Add( name, VV_STRING );
					else if ( type == "RadioChoice" )
						m_varData.Add( name, VV_NUMBER );
					else if ( type == "TextEntry" )
						m_varData.Add( name, VV_STRING );
					else if ( type == "Slider" )
						m_varData.Add( name, VV_NUMBER );
					else if ( type == "CheckBox" )
						m_varData.Add( name, VV_NUMBER );

					// extend this list in the future for additional controls
				}
			}

			LoadVarList( m_varList->GetStringSelection() );
		}
		break;
	case ID_VAR_ADD:
		{
			wxString name = wxGetTextFromUser( "Enter new variable name", "query", wxEmptyString, this);
			if ( !name.IsEmpty() )
			{
				VarInfo *vv = m_varData.Lookup( name );
				if ( vv )
				{
					FormToVarInfo( m_curVar );
					VarInfoToForm( vv );
				}
				else
				{
					VarInfoToForm( m_varData.Add( name, VV_INVALID ) );
					LoadVarList( name );
				}
			}

		}
		break;
	case ID_VAR_DELETE:
		m_varData.Delete( m_varList->GetStringSelection() );
		m_varList->Delete( m_varList->GetSelection() );
		VarInfoToForm( NULL );
		break;
	case ID_VAR_LIST:
		FormToVarInfo( m_curVar ); // save the current var
		VarInfoToForm( m_varData.Lookup( m_varList->GetStringSelection() ) ); // switch to the new var
		break;
	case ID_VAR_NAME:
		if ( m_curVar )
		{
			if ( m_curVar->Name == m_varName->GetValue() )
				return;

			if ( m_varData.Lookup( m_varName->GetValue() ) )
			{
				wxMessageBox("that variable already exists: " + m_varName->GetValue(), "notice", wxOK, this );
				m_varName->ChangeValue( m_curVar->Name );
				m_varName->SelectAll();
				return;
			}

			if ( m_varData.Rename( m_curVar->Name, m_varName->GetValue() ) )
			{
				m_curVar->Name = m_varName->GetValue();
				LoadVarList( m_curVar->Name );
			}
		}
		break;
	case ID_VAR_LABEL:
	case ID_VAR_UNITS:
		m_uiFormEditor->Refresh();
		break;

	case ID_CALLBACK_FIND:
		m_callbackScript->ShowFindReplaceDialog();
		break;

	case ID_CALLBACK_CHECK:
		{
			lk::input_string in( m_callbackScript->GetText() );
			lk::parser parse( in );
			lk::node_t *root = parse.script();
			if ( parse.error_count() == 0 )
				wxMessageBox("callback script parsed ok.", "notice", wxOK, this );
			else
			{
				wxString text;
				for( size_t i=0;i<parse.error_count();i++ )
					text += parse.error( i ) + "\n";
				::wxShowTextMessageDialog( text, "errors", this );
			}

			if( root != 0 ) delete root;
		}
		break;

	case ID_CALLBACK_GOTO:
		{
			m_callbackGotoList.Clear();
			wxMenu menu;
			int id = ID_CALLBACK_GOTO_1;
			m_callbackGotoList.Add( "on_load{'" + m_formName + "'} = define() {" );
			menu.Append( id++, "form->on_load" );

			std::vector<wxUIObject*> objs = m_formData.GetObjects();
			for( size_t i=0;i<objs.size();i++ )
			{
				wxString type = objs[i]->GetTypeName();
				if ( type == "Button" || type == "CheckBox" || type == "RadioChoice" || type == "Choice" )
				{
					m_callbackGotoList.Add( "on_change{'" + objs[i]->GetName() + "'} = define() {" );
					menu.Append(id++, objs[i]->GetName() + "->on_change" );
				}
			}
			PopupMenu( &menu );
		}
		break;
	case ID_CALLBACK_HELP:
		m_callbackScript->ShowHelpDialog( this );
		break;
	case ID_EQUATION_FIND:
		m_equationScript->ShowFindReplaceDialog();
		break;
	case ID_EQUATION_HELP:
		m_equationScript->ShowHelpDialog( this );
		break;
	case ID_EQUATION_SCAN:
		{
			wxArrayString errors;
			EqnDatabase db;
			bool ok = db.LoadScript( m_equationScript->GetText(), &errors );

			wxString text;
			if ( ok )
			{
				std::vector<EqnDatabase::eqn_data*> list = db.GetEquations();
				text << list.size() <<  " equations resolved ok \n\n";
				for( size_t i=0;i<list.size();i++ )
				{
					text << "-------------------------------------------------------------------------------\n";
					text << "[" << i << "] outputs: " << wxJoin( list[i]->outputs, ';' ) << "\n";
					text << "[" << i << "] inputs: " << wxJoin( list[i]->inputs, ';' ) << "\n";
					wxString str;
					lk::pretty_print( str, list[i]->tree, 0 );
					text << str << "\n\n";
				}
			}
			else
			{
				text << errors.size() << " errors when scanning equations\n";
				text << wxJoin(errors, '\n');
			}
			
			wxShowTextMessageDialog( text, "equation scan", this, wxSize(800,700) );
		}
		break;
	}
}

void UIEditorPanel::SyncFormUIToDataBeforeWriting()
{
	if ( m_curVar ) FormToVarInfo( m_curVar ); // sync any updates to the var before writing
}

void UIEditorPanel::OnFormSelectObject( wxUIFormEvent &evt )
{
	if ( wxUIObject *obj = evt.GetUIObject() )
	{
		if ( VarInfo *vv = m_varData.Lookup( obj->GetName() ) )
		{
			FormToVarInfo( m_curVar ); // save the current var
			m_varList->SetStringSelection( vv->Name );
			m_curVar = vv;
			VarInfoToForm( m_curVar );
		}		
	}
	
}

void UIEditorPanel::FormToVarInfo( VarInfo *vv )
{
	if ( !vv ) return;

	vv->Name = m_varName->GetValue();
	vv->Type = m_varType->GetSelection();
	vv->Label = m_varLabel->GetValue();
	vv->Units = m_varUnits->GetValue();
	vv->Group = m_varGroup->GetValue();
	vv->IndexLabels = wxSplit( m_varIndexLabels->GetValue(), ',' );
	vv->Flags = 0;
	if( m_varFlagHideLabels->GetValue() ) vv->Flags |= VF_HIDE_LABELS;
	if( m_varFlagParametric->GetValue() ) vv->Flags |= VF_PARAMETRIC;
	if( m_varFlagIndicator->GetValue() ) vv->Flags |= VF_INDICATOR;
	if( m_varFlagCalculated->GetValue() ) vv->Flags |= VF_CALCULATED;

}

void UIEditorPanel::VarInfoToForm( VarInfo *vv )
{
	m_curVar = vv;

	if ( vv )
	{
		m_varName->ChangeValue( vv->Name );
		m_varType->SetSelection( vv->Type );
		m_varLabel->ChangeValue( vv->Label );
		m_varUnits->ChangeValue( vv->Units );
		m_varGroup->ChangeValue( vv->Group );
		m_varIndexLabels->ChangeValue( wxJoin( vv->IndexLabels, ',' ) );
		m_varFlagHideLabels->SetValue( vv->Flags & VF_HIDE_LABELS );
		m_varFlagParametric->SetValue( vv->Flags & VF_PARAMETRIC );
		m_varFlagIndicator->SetValue( vv->Flags & VF_INDICATOR );
		m_varFlagCalculated->SetValue( vv->Flags & VF_CALCULATED );
		
		// todo: default value
	}
	else
	{
		m_varName->ChangeValue( wxEmptyString );
		m_varType->SetSelection( 0 );
		m_varLabel->ChangeValue( wxEmptyString );
		m_varUnits->ChangeValue( wxEmptyString );
		m_varGroup->ChangeValue( wxEmptyString );
		m_varIndexLabels->ChangeValue( wxEmptyString );
		m_varFlagHideLabels->SetValue( false );
		m_varFlagParametric->SetValue( false );
		m_varFlagIndicator->SetValue( false );
		m_varFlagCalculated->SetValue( false );

		// todo: default value
	}
	
	m_varName->Enable( m_curVar != 0 );
	m_varType->Enable( m_curVar != 0 );
	m_varLabel->Enable( m_curVar != 0 );
	m_varUnits->Enable( m_curVar != 0 );
	m_varGroup->Enable( m_curVar != 0 );
	m_varIndexLabels->Enable( m_curVar != 0 );
	m_varDefaultValue->Enable( m_curVar != 0 );
	m_varFlagHideLabels->Enable( m_curVar != 0 );
	m_varFlagParametric->Enable( m_curVar != 0 );
	m_varFlagIndicator->Enable( m_curVar != 0 );
	m_varFlagCalculated->Enable( m_curVar != 0 );

}

void UIEditorPanel::LoadVarList( const wxString &sel )
{
	m_varList->Freeze();
	m_varList->Clear();
	wxArrayString list = m_varData.ListAll();
	list.Sort();
	m_varList->Append( list );
	m_varList->Thaw();
	if ( !sel.IsEmpty() ) m_varList->SetStringSelection( sel );
}

bool UIEditorPanel::WriteForm( const wxString &name )
{
	bool ok = true;

	wxFFileOutputStream fform( SamApp::GetRuntimePath() + "/ui/" + name + ".ui" );
	if ( fform.IsOk() )
		m_formData.Write( fform );
	else ok = false;

	wxFFileOutputStream fvars( SamApp::GetRuntimePath() + "/ui/" + name + ".var" );
	if ( fvars.IsOk() )
		m_varData.Write( fvars );
	else ok = false;

	ok = ok && m_callbackScript->WriteAscii( SamApp::GetRuntimePath() + "/ui/" + name + ".cb" );
	
	ok = ok && m_equationScript->WriteAscii( SamApp::GetRuntimePath() + "/ui/" + name + ".eqn" );

	return ok;
}

bool UIEditorPanel::LoadForm( const wxString &name )
{
	m_uiFormEditor->SetFormData( 0 );
	m_uiFormEditor->Refresh();
	m_formName.Clear();
	
	bool ok = true;

	wxString file = SamApp::GetRuntimePath() + "/ui/" + name + ".ui";

	if ( wxFileExists( file ) )
	{
		wxFFileInputStream fform( file );
		if ( fform.IsOk() && m_formData.Read( fform ) )
		{
			m_uiFormEditor->SetFormData( &m_formData );
			m_uiFormEditor->Refresh();
			m_formName = name;
		}
		else ok = false;
	}

	file = SamApp::GetRuntimePath() + "/ui/" + name + ".var";
	if ( wxFileExists( file ) )
	{
		wxFFileInputStream fvars( file );
		if ( fvars.IsOk() && m_varData.Read( fvars ) )
		{
			m_curVar = 0;
			LoadVarList();
			VarInfoToForm( NULL );
		}
		else ok = false;
	}

	file = SamApp::GetRuntimePath() + "/ui/" + name + ".cb";
	if ( wxFileExists( file ) )
		ok = ok && m_callbackScript->ReadAscii( file );

	file = SamApp::GetRuntimePath() + "/ui/" + name + ".eqn";
	if ( wxFileExists( file ) )
		ok = ok && m_equationScript->ReadAscii( file );

	return ok;
}



static IDEWindow *g_ideWin=0;

void ShowIDEWindow()
{
	if ( !g_ideWin )
		g_ideWin = new IDEWindow( SamApp::Window() );

	g_ideWin->Show();
	g_ideWin->Raise();
}

BEGIN_EVENT_TABLE( IDEWindow, wxFrame )
	EVT_CLOSE( IDEWindow::OnClose )
END_EVENT_TABLE()

IDEWindow::IDEWindow( wxWindow *parent )
	: wxFrame(parent, wxID_ANY, "SAM Development Environment", wxDefaultPosition, wxSize(1150,900) )
{
#ifdef __WXMSW__
	SetIcon( wxICON( appicon ) );
#endif	

	m_notebook = new wxMetroNotebook( this );

	m_startupPanel = new StartupScriptPanel( m_notebook );
	m_notebook->AddPage( m_startupPanel, "Startup" );

	m_uiPanel = new UIEditorPanel( m_notebook );
	m_notebook->AddPage( m_uiPanel, "User Interface" );

	m_notebook->AddPage( new wxPanel( m_notebook ), "Simulations" );
	m_notebook->AddPage( new wxPanel( m_notebook ), "Default Values" );
	m_notebook->AddPage( new wxPanel( m_notebook ), "Default Graphs" );
	m_notebook->AddPage( new wxPanel( m_notebook ), "Cash Flows" );
	m_notebook->AddPage( new wxPanel( m_notebook ), "Report Templates" );

	m_notebook->Refresh();
}

IDEWindow::~IDEWindow()
{
	g_ideWin = 0;
}

void IDEWindow::OnClose( wxCloseEvent &evt )
{
	Hide();
	evt.Veto();
}
	
