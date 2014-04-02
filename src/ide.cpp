#include <wx/splitter.h>
#include <wx/notebook.h>
#include <wx/busyinfo.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/gbsizer.h>

#include <wex/exttext.h>
#include <wex/lkscript.h>
#include <wex/metro.h>
#include <wex/uiform.h>
#include <wex/utils.h>
#include <wex/extgrid.h>
#include <wex/csv.h>

#include <lk_lex.h>
#include <lk_parse.h>

#include <ssc/sscapi.h>

#include "ide.h"
#include "main.h"
#include "equations.h"
#include "inputpage.h"
#include "invoke.h"
#include "widgets.h"

enum { ID_STARTUP_EDITOR = wxID_HIGHEST+124,
	ID_STARTUP_SAVE,
	ID_STARTUP_FIND,
	ID_STARTUP_HELP,
	ID_STARTUP_RESTART
};

BEGIN_EVENT_TABLE( ScriptPanel, wxPanel )
	EVT_BUTTON( ID_STARTUP_SAVE, ScriptPanel::OnCommand )
	EVT_BUTTON( ID_STARTUP_FIND, ScriptPanel::OnCommand )
	EVT_BUTTON( ID_STARTUP_HELP, ScriptPanel::OnCommand )
	EVT_BUTTON( ID_STARTUP_RESTART, ScriptPanel::OnCommand )
END_EVENT_TABLE()


ScriptPanel::ScriptPanel( wxWindow *parent, const wxString &script_file_name )	
	: wxPanel( parent ), m_fileName( script_file_name )
{
	wxBoxSizer *sz_startup_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_SAVE, "Save"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_FIND, "Find..."), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_HELP, "Help"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_STARTUP_RESTART, "Restart SAM"), 0, wxALL|wxEXPAND, 2 );	
	m_scriptCtrl = new wxLKScriptCtrl( this, ID_STARTUP_EDITOR );
	wxBoxSizer *sz_startup_main = new wxBoxSizer( wxVERTICAL );
	sz_startup_main->Add( sz_startup_tools, 0, wxALL|wxEXPAND, 2 );
	sz_startup_main->Add( m_scriptCtrl, 1, wxALL|wxEXPAND, 0 );
	
	SetSizer( sz_startup_main );
	
	m_scriptCtrl->RegisterLibrary( invoke_general_funcs(), "General Functions", 0 );

	if (!m_scriptCtrl->ReadAscii( SamApp::GetRuntimePath() + "/" + m_fileName ) )
		wxMessageBox("Error loading " + m_fileName, "notice", wxOK, this);
}

void ScriptPanel::AddLibrary( lk::fcall_t *lib, const wxString &name )
{
	m_scriptCtrl->RegisterLibrary( lib, name );
}

void ScriptPanel::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_STARTUP_SAVE:
		{
			wxBusyInfo savemsg( "Writing " + m_fileName + " to disk" );
			wxYield();
			wxMilliSleep(300);
			if ( !m_scriptCtrl->WriteAscii( SamApp::GetRuntimePath() + "/" + m_fileName ))
				wxMessageBox("Error writing " + m_fileName + "  to disk" , "notice", wxOK, this);
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
				&& wxYES == wxMessageBox( m_fileName + " is modified. Save first?", "Query", wxYES_NO, this) )
				m_scriptCtrl->WriteAscii( SamApp::GetRuntimePath() + "/" + m_fileName );
		
			wxBusyInfo restartmsg( "Restarting SAM... messages in debug output screen" );
				wxYield();
				wxMilliSleep(300);
			SamApp::Restart();
		}
		break;
	}
}

/*

enum { ID_SIMULATIONS_EDITOR = wxID_HIGHEST+495, ID_SIMULATIONS_LIST, 
	ID_SIMULATIONS_SAVE, ID_SIMULATIONS_NEW, ID_SIMULATIONS_FIND, ID_SIMULATIONS_HELP };

BEGIN_EVENT_TABLE( SimulationScriptPanel, wxPanel )
	EVT_CHOICE( ID_SIMULATIONS_LIST, SimulationScriptPanel::OnCommand )
	EVT_BUTTON( ID_SIMULATIONS_SAVE, SimulationScriptPanel::OnCommand )
	EVT_BUTTON( ID_SIMULATIONS_NEW, SimulationScriptPanel::OnCommand )
	EVT_BUTTON( ID_SIMULATIONS_FIND, SimulationScriptPanel::OnCommand )
	EVT_BUTTON( ID_SIMULATIONS_HELP, SimulationScriptPanel::OnCommand )
END_EVENT_TABLE()


extern lk::fcall_t *invoke_simulation_stubs();

SimulationScriptPanel::SimulationScriptPanel( wxWindow *parent )
	: wxPanel( parent )
{
	m_scriptCtrl = new wxLKScriptCtrl( this, ID_SIMULATIONS_EDITOR );
	m_scriptCtrl->RegisterLibrary( invoke_simulation_stubs(), "Simulation Functions", 0 );

	m_simList = new wxChoice( this, ID_SIMULATIONS_LIST );
	
	RefreshList();

	if ( m_simList->GetCount() > 0 ) 
	{
		m_simList->SetSelection( 0 );
		m_currentFile = SamApp::GetRuntimePath() + "/simulations/" + m_simList->GetStringSelection();
		m_scriptCtrl->ReadAscii( m_currentFile );
	}
		
	wxBoxSizer *sz_startup_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_startup_tools->Add( m_simList, 0, wxALL|wxEXPAND, 2 );	
	sz_startup_tools->Add( new wxButton( this, ID_SIMULATIONS_NEW, "New"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_SIMULATIONS_SAVE, "Save"), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_SIMULATIONS_FIND, "Find..."), 0, wxALL|wxEXPAND, 2 );
	sz_startup_tools->Add( new wxButton( this, ID_SIMULATIONS_HELP, "Help"), 0, wxALL|wxEXPAND, 2 );
	wxBoxSizer *sz_startup_main = new wxBoxSizer( wxVERTICAL );
	sz_startup_main->Add( sz_startup_tools, 0, wxALL|wxEXPAND, 2 );
	sz_startup_main->Add( m_scriptCtrl, 1, wxALL|wxEXPAND, 0 );
	
	SetSizer( sz_startup_main );
}

void SimulationScriptPanel::RefreshList()
{
	wxString sel = m_simList->GetStringSelection();
	m_simList->Clear();
	wxDir dir( SamApp::GetRuntimePath() + "/simulations" );
	if ( dir.IsOpened() )
	{
		wxString file;
		bool has_more = dir.GetFirst( &file, "*.sim", wxDIR_FILES  );
		while( has_more )
		{
			m_simList->Append( file );
			has_more = dir.GetNext( &file );
		}
	}

	m_simList->SetStringSelection( sel );
}

void SimulationScriptPanel::SaveCurrent()
{
	wxBusyInfo savemsg( "Writing to disk: " + wxFileName(m_currentFile).GetName() );
	wxYield();
	wxMilliSleep(100);
	if ( !m_scriptCtrl->WriteAscii( m_currentFile ))
		wxMessageBox("Error writing to disk:\n\n" + m_currentFile, "notice", wxOK, this);
}

void SimulationScriptPanel::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_SIMULATIONS_LIST:
		SaveCurrent();
		m_currentFile = SamApp::GetRuntimePath() + "/simulations/" + m_simList->GetStringSelection();
		m_scriptCtrl->ReadAscii( m_currentFile );		
		break;
	case ID_SIMULATIONS_SAVE:
		SaveCurrent();
		break;
	case ID_SIMULATIONS_FIND:
		m_scriptCtrl->ShowFindReplaceDialog();
		break;
	case ID_SIMULATIONS_HELP:
		m_scriptCtrl->ShowHelpDialog( this );
		break;
	case ID_SIMULATIONS_NEW:
	{
		wxString name = wxGetTextFromUser("Enter name of simulation file:", "query", wxEmptyString, this );
		if ( name.IsEmpty() ) return;

		wxString path = SamApp::GetRuntimePath() + "/simulations/" + name + ".sim";
		if ( wxFileExists( path ) )
		{
			wxMessageBox("that one already exists", "notice", wxOK, this);
			return;
		}

		wxFile ff( path, wxFile::write );
		if (ff.IsOpened() )
		{
			ff.Write( "on_simulate{'<<tech-or-financing-name>>'} = define() {\n\tlog('simulation info');\n\treturn true;\n};\n");
			ff.Close();

			SaveCurrent();
			RefreshList();
			m_simList->SetStringSelection( name + ".sim" );
			m_currentFile = path;
			m_scriptCtrl->ReadAscii( path );
		}
		else
			wxMessageBox("error creating new simulation file");
	}
		break;
	}
}
*/

enum { ID_SSC_MOD = wxID_HIGHEST, ID_APPLY_VTL, ID_APPLY_LAST_DOT };

class RemapDialog : public wxDialog
{
	UIEditorPanel *m_ui;
	wxExtGridCtrl *m_grid;
	wxChoice *m_sscMod;
	wxExtGridCtrl *m_sscVars;
	AFSearchListBox *m_vtlList;
	StringHash m_vtlMap;
public:
	RemapDialog( wxWindow *parent, const wxString &title, UIEditorPanel *ui )
		: wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(610, 550), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER ),
		m_ui( ui )
	{
		wxNotebook *notebook = new wxNotebook( this, wxID_ANY );

		m_grid = new wxExtGridCtrl( notebook, wxID_ANY );
		m_grid->CreateGrid( 10, 2 );
		m_grid->SetColLabelValue( 0, "Old name" );
		m_grid->SetColLabelValue( 1, "New name" );
		m_grid->SetColumnWidth( 0, 250 );
		m_grid->SetColumnWidth( 1, 250 );
		notebook->AddPage( m_grid, "Name Mapping" );

		wxPanel *panel_ssc = new wxPanel( notebook, wxID_ANY );
		m_sscMod = new wxChoice( panel_ssc, ID_SSC_MOD );
		
		int idx=0;
		while (const ssc_entry_t p_entry = ::ssc_module_entry(idx++))
			m_sscMod->Append( ::ssc_entry_name(p_entry) );
		
		m_sscVars = new wxExtGridCtrl( panel_ssc, wxID_ANY );
		m_sscVars->CreateGrid( 1, 1);

		wxBoxSizer *sizer_ssc = new wxBoxSizer( wxVERTICAL );
		sizer_ssc->Add( m_sscMod, 0, wxALL|wxEXPAND, 3 );
		sizer_ssc->Add( m_sscVars, 1, wxALL|wxEXPAND, 0 );
		panel_ssc->SetSizer( sizer_ssc );

		notebook->AddPage( panel_ssc, "SSC Modules" );
		
		wxBusyInfo info("loading vtl maps...");
		m_vtlList = new AFSearchListBox( notebook, wxID_ANY );
		wxArrayString files;
		wxDir::GetAllFiles( SamApp::GetRuntimePath() + "/vtl", &files, "*.csv" );
		for( size_t i=0;i<files.size();i++)
		{
			wxCSVData csv;
			csv.ReadFile( files[i] );
			for( size_t j=0;j<csv.NumRows();j++ )
				m_vtlMap[ csv(j,0) ] = csv(j,1);
		}

		files.Clear();
		for( StringHash::iterator it = m_vtlMap.begin();
			it != m_vtlMap.end();
			++it )
		{
			files.Add( it->first + " --> " + it->second );
		}
		m_vtlList->Append( files );
		
		notebook->AddPage( m_vtlList, "VTL" );

		wxBoxSizer *sizer_buttons = new wxBoxSizer( wxHORIZONTAL );
		sizer_buttons->Add( new wxButton( this, wxID_OPEN, "Read" ), 0, wxALL|wxEXPAND, 2 );
		sizer_buttons->Add( new wxButton( this, wxID_SAVE, "Write" ), 0, wxALL|wxEXPAND, 2 );
		sizer_buttons->Add( new wxButton( this, wxID_REFRESH, "Reload" ), 0, wxALL|wxEXPAND, 2 );
		sizer_buttons->Add( new wxButton( this, ID_APPLY_VTL, "Apply VTL"), 0, wxALL|wxEXPAND, 2 );
		sizer_buttons->Add( new wxButton( this, ID_APPLY_LAST_DOT, "Apply last dot"), 0, wxALL|wxEXPAND, 2 );
		sizer_buttons->AddStretchSpacer();
		sizer_buttons->Add( new wxButton( this, wxID_APPLY, "Apply changes" ), 0, wxALL|wxEXPAND, 2 );
		sizer_buttons->Add( new wxButton( this, wxID_CLOSE, "Close" ), 0, wxALL|wxEXPAND, 2 );

		wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );
		sizer_main->Add( notebook, 1, wxALL|wxEXPAND, 0 );
		sizer_main->Add( sizer_buttons, 0, wxALL|wxEXPAND, 2 );

		SetSizer( sizer_main );

		LoadVariables();
	}

	void LoadVariables( )
	{
		wxArrayString list = m_ui->GetVars()->ListAll();
		
		m_grid->ClearGrid();
		m_grid->ResizeGrid( list.size(), 2 );
		for( size_t i=0;i<list.size();i++ )
		{
			m_grid->SetCellValue( list[i], i, 0 );
			m_grid->SetCellBackgroundColour( *wxWHITE, i, 1 );
		}
	}

	void LoadSSCTable()
	{
		wxString cm_name = m_sscMod->GetStringSelection();
	
		ssc_module_t p_mod = ::ssc_module_create( (const char*)cm_name.c_str() );
		if ( p_mod == 0 )
		{
			wxMessageBox("Could not create a module of type: " + cm_name );
			return;
		}

		std::vector<wxArrayString> vartab;

		int idx=0;
		while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, idx++ ) )
		{
			int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
		

			wxArrayString row;

			switch( var_type )
			{
			case SSC_INPUT: row.Add("SSC_INPUT"); break;
			case SSC_OUTPUT: row.Add("SSC_OUTPUT"); break;
			case SSC_INOUT: row.Add("SSC_INOUT"); break;
			default: row.Add("<!unknown>"); break;
			}

			switch( data_type )
			{
			case SSC_STRING: row.Add("SSC_STRING"); break;
			case SSC_NUMBER: row.Add("SSC_NUMBER"); break;
			case SSC_ARRAY: row.Add("SSC_ARRAY"); break;
			case SSC_MATRIX: row.Add("SSC_MATRIX"); break;
			default: row.Add("<!unknown>"); break;
			}
			
			row.Add( ssc_info_name( p_inf ) );
			row.Add( ssc_info_label( p_inf ) );
			row.Add( ssc_info_units( p_inf ) );
			row.Add( ssc_info_meta( p_inf ) );
			row.Add( ssc_info_group( p_inf ) );
			row.Add( ssc_info_required( p_inf ) );
			row.Add( ssc_info_constraints( p_inf ) );

			vartab.push_back(row);
		}

		int nrows = (int)vartab.size();
		int ncols = 9;
		
		m_sscVars->Freeze();
		m_sscVars->ResizeGrid( nrows, ncols);
		m_sscVars->SetColLabelValue( 0, "TYPE" );
		m_sscVars->SetColLabelValue( 1, "DATA" );
		m_sscVars->SetColLabelValue( 2, "NAME" );
		m_sscVars->SetColLabelValue( 3, "LABEL" );
		m_sscVars->SetColLabelValue( 4, "UNITS" );
		m_sscVars->SetColLabelValue( 5, "META" );
		m_sscVars->SetColLabelValue( 6, "GROUP" );
		m_sscVars->SetColLabelValue( 7, "REQUIRE" );
		m_sscVars->SetColLabelValue( 8, "CONSTRAINT" );

		for (int r=0;r<nrows;r++)
			for (int c=0;c<ncols;c++)
				m_sscVars->SetCellValue( vartab[r][c], r, c );

		m_sscVars->AutoSizeColumns(false);
		m_sscVars->Thaw();

		::ssc_module_free( p_mod );
	}

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case ID_SSC_MOD:
			LoadSSCTable();
			break;
		case wxID_OPEN:
		{
			wxFileDialog dlg( this, "Read csv file", wxEmptyString, wxEmptyString, "CSV Files (*.csv)|*.csv", wxFD_OPEN );
			if ( dlg.ShowModal() != wxID_OK ) return;

			wxCSVData csv;
			if ( !csv.ReadFile( dlg.GetPath() ) )
			{
				wxMessageBox("failed to read csv file");
				return;
			}

			size_t nr = csv.NumRows();
			size_t nc = csv.NumCols();
			if ( nr == 0 ) nr = 1;
			if ( nc != 2 ) nc = 2;

			m_grid->ResizeGrid( nr,2 );
			for( size_t i=0;i<nr;i++)
			{
				m_grid->SetCellValue( i, 0, csv(i,0) );
				m_grid->SetCellValue( i, 1, csv(i,1) );
			}

		}
			break;
		case wxID_REFRESH:
			LoadVariables();
			break;
		case wxID_SAVE:
		{
			wxFileDialog dlg( this, "Write csv file", wxEmptyString, wxEmptyString, "CSV Files (*.csv)|*.csv", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
			if ( dlg.ShowModal() != wxID_OK ) return;

			wxCSVData csv;
			for( size_t i=0;i<m_grid->GetNumberRows();i++ )
			{
				csv(i,0) = m_grid->GetCellValue( i, 0 );
				csv(i,1) = m_grid->GetCellValue( i, 1 );
			}

			if ( !csv.WriteFile( dlg.GetPath() ))
				wxMessageBox("failed to write csv file");
		}
			break;
		case ID_APPLY_VTL:
			for( size_t i=0;i<(size_t)m_grid->GetNumberRows();i++)
			{
				wxString ssc = m_vtlMap[ m_grid->GetCellValue(i,0) ];
				if ( !ssc.IsEmpty() )
				{
					m_grid->SetCellValue(ssc,i,1);
					m_grid->SetCellBackgroundColour( wxColour(255,213,226), i,1 );
				}
			}
			break;
		case ID_APPLY_LAST_DOT:
			for( size_t i=0;i<(size_t)m_grid->GetNumberRows();i++)
			{
				if (m_grid->GetCellValue(i,1).IsEmpty())
				{
					wxString ssc = m_grid->GetCellValue(i,0);
					if ( !ssc.IsEmpty() )
					{
						size_t pos = ssc.find_last_of(".");
						if ( pos > 0 )
						{
							ssc = ssc.Mid( pos+1);
							m_grid->SetCellValue(ssc,i,1);
							m_grid->SetCellBackgroundColour( wxColour(255,213,226), i,1 );
						}
					}
				}
			}
			break;
		case wxID_APPLY:
		{
			m_ui->GetPropertyEditor()->SetObject( 0 );
			m_ui->VarInfoToForm( wxEmptyString );
			m_ui->LoadVarList( );

			wxUIFormData *form = m_ui->GetFormData();
			VarDatabase *vars = m_ui->GetVars();

			wxString result;
			wxString cb = m_ui->GetCallbacks();
			wxString eqn = m_ui->GetEquations();
			int n_cbreps = 0, n_eqnreps = 0;
			
			for( int i=0;i<m_grid->GetNumberRows();i++ )
			{
				wxString sold = m_grid->GetCellValue(i,0);
				wxString snew = m_grid->GetCellValue(i,1);
				
				if ( sold.IsEmpty() || snew.IsEmpty() ) continue;

				result += sold + " -> " + snew;

				// change name of object in form and variable table
				if ( wxUIObject *obj = form->Find( sold ) )
				{
					obj->SetName( snew );
					result += "   (form ok)";
				}

				if ( vars->Rename( sold, snew ) )
					result += "   (var ok)";

				n_cbreps += cb.Replace( "'" + sold + "'", "'" + snew + "'" );
				n_cbreps += cb.Replace( "\"" + sold + "\"", "'" + snew + "'" );
				n_cbreps += cb.Replace( "${" + sold + "}", "${" + snew + "}" );
				
				n_eqnreps += eqn.Replace( "'" + sold + "'", "'" + snew + "'" );
				n_eqnreps += eqn.Replace( "\"" + sold + "\"", "'" + snew + "'" );
				n_eqnreps += eqn.Replace( "${" + sold + "}", "${" + snew + "}" );

				result += "\n";
			}
			
			m_ui->SetCallbacks( cb );

			wxShowTextMessageDialog( wxString::Format("textual replacements\n\tcallbacks: %d equations: %d\n\n", n_cbreps, n_eqnreps )
				+ result, "Remapping result", this );
			
			LoadVariables();

			m_ui->LoadVarList();
			m_ui->GetDesigner()->Refresh();

			break;
		}
		case wxID_CLOSE:
			if ( IsModal() ) EndModal( wxID_CANCEL );
			else Close();
			break;
		}
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( RemapDialog, wxDialog )
	EVT_BUTTON( wxID_APPLY, RemapDialog::OnCommand )
	EVT_BUTTON( wxID_CLOSE, RemapDialog::OnCommand )
	EVT_BUTTON( wxID_SAVE, RemapDialog::OnCommand )
	EVT_BUTTON( wxID_OPEN, RemapDialog::OnCommand )
	EVT_CHOICE( ID_SSC_MOD, RemapDialog::OnCommand )
	EVT_BUTTON( ID_APPLY_VTL, RemapDialog::OnCommand )
	EVT_BUTTON( ID_APPLY_LAST_DOT, RemapDialog::OnCommand )
	EVT_BUTTON( wxID_REFRESH, RemapDialog::OnCommand )
END_EVENT_TABLE()

enum { 
	ID_FORM_EDITOR = wxID_HIGHEST + 231,
	ID_CALLBACK_EDITOR,
	ID_EQUATION_EDITOR,

	ID_FORM_LIST,
	ID_FORM_LIST_REFRESH,
	ID_FORM_ADD,
	ID_FORM_SAVE,
	ID_FORM_DELETE,
	

	ID_VAR_REMAP,
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
	ID_VAR_FL_LIBRARY,

	ID_CALLBACK_FIND,
	ID_CALLBACK_CHECK,
	ID_CALLBACK_GOTO,
	ID_CALLBACK_HELP,

	ID_CALLBACK_GOTO_1,
	ID_CALLBACK_GOTO_100 = ID_CALLBACK_GOTO_1 + 100,

	ID_EQUATION_FIND,
	ID_EQUATION_SCAN,
	ID_EQUATION_HELP,

	ID_VAR_CHECK_ALL,
	ID_VAR_CHECK_NONE,
	ID_VAR_CHECK_SEL,

	ID_VAR_STORE,
	ID_VAR_LOAD

};

BEGIN_EVENT_TABLE( UIEditorPanel, wxPanel )
	EVT_LISTBOX( ID_FORM_LIST, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_LIST_REFRESH, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_ADD, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_SAVE, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_FORM_DELETE, UIEditorPanel::OnCommand )
	
	EVT_BUTTON( ID_VAR_SYNC, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_REMAP, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_ADD, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_DELETE, UIEditorPanel::OnCommand )

	EVT_BUTTON( ID_VAR_CHECK_ALL, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_CHECK_NONE, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_CHECK_SEL, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_STORE, UIEditorPanel::OnCommand )
	EVT_BUTTON( ID_VAR_LOAD, UIEditorPanel::OnCommand )

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

static void fcall_technology_stub( lk::invoke_t &cxt )
{
	LK_DOC( "technology", "Return the current technology option name", "(void):string" );
}

static void fcall_financing_stub( lk::invoke_t &cxt )
{
	LK_DOC( "financing", "Return the current financing option name", "(void):string" );
}


static lk::fcall_t* invoke_equation_stubs()
{
	static const lk::fcall_t vec[] = {
		fcall_technology_stub,
		fcall_financing_stub,
		0 };
	return (lk::fcall_t*)vec;
}

UIEditorPanel::UIEditorPanel( wxWindow *parent )
	: wxPanel( parent ), m_exForm( &m_ipd.Variables() )
{
	wxBoxSizer *sz_form_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_form_tools->Add( new wxButton( this, ID_FORM_LIST_REFRESH, "Refresh list"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_FORM_ADD, "Add..."), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_FORM_SAVE, "Save"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_FORM_DELETE, "Delete"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->AddStretchSpacer();	
	sz_form_tools->Add( new wxButton( this, ID_VAR_REMAP, "Remap"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_SYNC, "Sync"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_ADD, "Add"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_DELETE, "Delete"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_CHECK_ALL, "Chk all"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_CHECK_NONE, "Chk none"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_CHECK_SEL, "Chk sel"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_STORE, "Store"), 0, wxALL|wxEXPAND, 2 );
	sz_form_tools->Add( new wxButton( this, ID_VAR_LOAD, "Load"), 0, wxALL|wxEXPAND, 2 );
		
	m_uiPropEditor = new wxUIPropertyEditor( this, wxID_ANY );
	m_formList = new wxListBox( this, ID_FORM_LIST, wxDefaultPosition, wxSize(300, 300), 0, 0, wxLB_SINGLE|wxBORDER_NONE );
	
	wxBoxSizer *sz_form_left = new wxBoxSizer( wxVERTICAL );
	sz_form_left->Add( new wxStaticText( this, wxID_ANY, "UI Forms" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_form_left->Add( m_formList, 1, wxALL|wxEXPAND, 2 );
	sz_form_left->Add( new wxStaticText( this, wxID_ANY, "Object Properties" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_form_left->Add( m_uiPropEditor, 1, wxALL|wxEXPAND, 2 );

	
	m_varList = new wxCheckListBox( this, ID_VAR_LIST, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_SINGLE|wxBORDER_NONE );
	m_varName = new wxExtTextCtrl( this, ID_VAR_NAME, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
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
	m_varFlagLibrary = new wxCheckBox( this, ID_VAR_FL_LIBRARY, "Library?" );


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
	sz_var_fields->Add( m_varFlagLibrary,  wxGBPosition(8,1), wxGBSpan(1,1), wxALL|wxALIGN_CENTER_VERTICAL, 3 );

	sz_var_fields->AddGrowableCol( 1 );

	wxBoxSizer *sz_var_main = new wxBoxSizer( wxVERTICAL );
	sz_var_main->Add( new wxStaticText( this, wxID_ANY, "Variables" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_var_main->Add( m_varList, 1, wxALL|wxEXPAND, 2 );
	sz_var_main->Add( sz_var_fields,1, wxALL|wxEXPAND, 0 );

	wxSplitterWindow *center_split = new wxSplitterWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE );
	m_uiFormEditor = new wxUIFormDesigner( center_split, ID_FORM_EDITOR );
	m_uiFormEditor->SetBackgroundColour( wxColour(120,120,120) );
	m_uiFormEditor->SetCopyBuffer( &m_uiCopyBuffer );

	wxPanel *scripts_panel = new wxPanel( center_split );

	wxBoxSizer *sz_callback_tools = new wxBoxSizer( wxHORIZONTAL );
	sz_callback_tools->Add( new wxStaticText( scripts_panel, wxID_ANY, "Callbacks"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_callback_tools->AddSpacer(5);
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_FIND, "Find..." ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_CHECK, "Check code" ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_GOTO, "Goto..." ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->Add( new wxButton( scripts_panel, ID_CALLBACK_HELP, "Help" ), 0, wxALL|wxEXPAND, 2 );
	sz_callback_tools->AddStretchSpacer();

	m_callbackScript = new wxLKScriptCtrl( scripts_panel, ID_CALLBACK_EDITOR ); // by default registers all LK stdlib functions
	m_callbackScript->RegisterLibrary( invoke_general_funcs(), "Callback UI Functions" );
	m_callbackScript->RegisterLibrary( invoke_casecallback_funcs(), "Callback UI Functions" );
	m_callbackScript->RegisterLibrary( invoke_uicallback_funcs(), "Callback UI Functions" );
	m_callbackScript->RegisterLibrary( invoke_ssc_funcs(), "SSC Functions" );
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
		wxLK_STDLIB_BASIC | wxLK_STDLIB_STRING | wxLK_STDLIB_MATH | wxLK_STDLIB_WXUI  );
	m_equationScript->RegisterLibrary( invoke_equation_stubs(), "Case Information", 0 );
	m_equationScript->RegisterLibrary( invoke_ssc_funcs(), "SSC Functions", 0 );
	m_equationScript->RegisterLibrary( invoke_equation_funcs(), "Additional Equation Functions", 0 );

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
	m_uiFormEditor->SetFormData( &m_exForm );


	wxBoxSizer *sz_form_top = new wxBoxSizer( wxVERTICAL );
	sz_form_top->Add( sz_form_tools, 0, wxALL|wxEXPAND, 2 );
	sz_form_top->Add( sz_form_main, 1, wxALL|wxEXPAND, 0 );
	this->SetSizer( sz_form_top );

	LoadFormList();
	LoadVarList();
	VarInfoToForm( wxEmptyString );
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



ExFormData::ExFormData( VarDatabase *vdb ) 
	: m_vdb(vdb) { }

ExFormData::~ExFormData()
{
	m_vdb = 0;
}

bool ExFormData::GetMetaData( const wxString &name,
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

void UIEditorPanel::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_FORM_LIST:
		if ( !m_formName.IsEmpty() )
		{
			SyncFormUIToDataBeforeWriting();
			Write( m_formName );
		}

		if (!Load( m_formList->GetStringSelection() ))
			wxMessageBox("error loading form: " + m_formList->GetStringSelection(), "notice", wxOK, this );
		break;
	case ID_FORM_LIST_REFRESH:
		LoadFormList();
		break;
	case ID_FORM_ADD:
		{
			wxString name = wxGetTextFromUser( "Enter new form name:", "query", wxEmptyString, this );
			if ( name.IsEmpty() ) return;

			if ( wxFileExists(SamApp::GetRuntimePath()  + "/ui/" + name + ".ui" ) )
			{
				wxMessageBox("that form already exists.", "notice", wxOK, this);
				return;
			}
			m_uiFormEditor->SetFormData( 0 ) ;
			m_exForm.DeleteAll();
			m_ipd.Clear();
			
			m_callbackScript->SetText( wxEmptyString );
			m_equationScript->SetText( wxEmptyString );
			Write( name );
			if ( !Load( name ) )
				wxMessageBox("error loading newly created form: " + name, "notice", wxOK, this );
			LoadFormList( name );
			VarInfoToForm( wxEmptyString );
		}
		break;
	case ID_FORM_DELETE:
		{
			wxString form = m_formList->GetStringSelection();
			if ( wxYES == wxMessageBox( "really delete .ui for: " + form + " ?  cannot undo!", "query", wxYES_NO, this ) )
			{
				wxString ff = SamApp::GetRuntimePath() + "/ui/" + form + ".ui";
				if ( wxFileExists( ff ) ) wxRemoveFile( ff );
				
				m_formName.Clear();
				m_exForm.DeleteAll();
				m_ipd.Clear();				
				m_uiFormEditor->SetFormData( &m_exForm );
				m_uiFormEditor->Refresh();
				m_callbackScript->Clear();
				m_equationScript->Clear();
				LoadFormList();
				LoadVarList();
				VarInfoToForm( wxEmptyString );
			}
		}
		break;
	case ID_FORM_SAVE:
		{
			wxBusyInfo info( "Saving form and variable data: " + m_formName );
			wxYield();
			wxMilliSleep( 300 );
			
			SyncFormUIToDataBeforeWriting();

			if (!Write( m_formName ))
				wxMessageBox("error writing form: " + m_formName, "notice", wxOK, this );
		}
		break;
	case ID_VAR_REMAP:
	{
		RemapDialog *rdlg = new RemapDialog( this, "Remap variable names", this );
		rdlg->Show();
	}
		break;
	case ID_VAR_SYNC:
		{
			std::vector<wxUIObject*> objects = m_exForm.GetObjects();
			for( size_t i=0;i<objects.size();i++ )
			{
				wxString type = objects[i]->GetTypeName();
				wxString name = objects[i]->GetName();
				if ( m_ipd.Variables().Lookup( name ) == 0 )
				{
					if ( type == "Numeric" )
						m_ipd.Variables().Create( name, VV_NUMBER );
					else if ( type == "Choice" )
						m_ipd.Variables().Create( name, VV_NUMBER );
					else if ( type == "ListBox" )
						m_ipd.Variables().Create( name, VV_STRING );
					else if ( type == "RadioChoice" )
						m_ipd.Variables().Create( name, VV_NUMBER );
					else if ( type == "TextEntry" )
						m_ipd.Variables().Create( name, VV_STRING );
					else if ( type == "Slider" )
						m_ipd.Variables().Create( name, VV_NUMBER );
					else if ( type == "CheckBox" )
						m_ipd.Variables().Create( name, VV_NUMBER );
					else if ( type == "RadioChoice" )
						m_ipd.Variables().Create( name, VV_NUMBER );
					else if ( type == "Slider" )
						m_ipd.Variables().Create( name, VV_NUMBER );
					else if ( type == "SchedNumeric" )
						m_ipd.Variables().Create( name, VV_ARRAY );
					else if ( type == "TOUSchedule" )
						m_ipd.Variables().Create( name, VV_STRING );
					else if ( type == "PTLayout" )
						m_ipd.Variables().Create( name, VV_TABLE );
					else if ( type == "MaterialProperties" )
						m_ipd.Variables().Create( name, VV_MATRIX );
					else if ( type == "TroughLoop" )
						m_ipd.Variables().Create( name, VV_ARRAY );
					else if ( type == "MonthlyFactor" )
						m_ipd.Variables().Create( name, VV_ARRAY );
					else if ( type == "SearchListBox" )
						m_ipd.Variables().Create( name, VV_STRING );
					else if ( type == "DataArray" )
						m_ipd.Variables().Create( name, VV_ARRAY );
					else if ( type == "DataMatrix" )
						m_ipd.Variables().Create( name, VV_MATRIX );
					else if ( type == "ShadingFactors" )
						m_ipd.Variables().Create( name, VV_TABLE );
					else if ( type == "ValueMatrix" )
						m_ipd.Variables().Create( name, VV_MATRIX );
					else if ( type == "MonthByHourFactors" )
						m_ipd.Variables().Create( name, VV_MATRIX );
					else if ( type == "HourlyFactor" )
						m_ipd.Variables().Create( name, VV_TABLE );
					else if (type == "DiurnalPeriod")
						m_ipd.Variables().Create(name, VV_MATRIX);

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
				VarInfo *vv = m_ipd.Variables().Lookup( name );
				if ( vv )
					FormToVarInfo( );
				else
					m_ipd.Variables().Create( name, VV_INVALID );

				LoadVarList( name );
				VarInfoToForm( name );
			}
		}
		break;
	case ID_VAR_DELETE:
		{
		// delete selected item
			m_ipd.Variables().Delete( m_varList->GetStringSelection() );
			m_varList->Delete( m_varList->GetSelection() );

		// delete any checked items
			wxArrayString checked;
			for( size_t i=0;i<m_varList->GetCount();i++ )
				if ( m_varList->IsChecked( i ) )
					checked.Add( m_varList->GetString( i ) );

			for( size_t i=0;i<checked.size();i++ )
			{
				m_varList->Delete( m_varList->FindString( checked[i] ) );
				m_ipd.Variables().Delete( checked[i] );
			}
			
			VarInfoToForm( wxEmptyString );
		}
		break;
	case ID_VAR_LIST:
		FormToVarInfo( ); // save the current var
		VarInfoToForm( m_varList->GetStringSelection() ); // switch to the new var
		break;
	case ID_VAR_NAME:
		if ( !m_curVarName.IsEmpty() )
		{
			wxString old_name = m_curVarName;
			wxString new_name = m_varName->GetValue();

			if ( m_ipd.Variables().Lookup( new_name ) )
			{
				wxMessageBox("that variable already exists: " + new_name, "notice", wxOK, this );
				m_varName->ChangeValue( old_name );
				m_varName->SelectAll();
				return;
			}

			if ( m_ipd.Variables().Rename( old_name, new_name ) )
			{
				m_curVarName = new_name;
				LoadVarList( new_name );
			}
			else
				wxMessageBox("failed to rename: " + old_name + " --> " + new_name );
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

			std::vector<wxUIObject*> objs = m_exForm.GetObjects();
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
				std::vector<EqnData*> list = db.GetEquations();
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
	case ID_VAR_CHECK_ALL:
	case ID_VAR_CHECK_NONE:
		for( size_t i=0;i<m_varList->GetCount();i++)
			m_varList->Check( i, evt.GetId() == ID_VAR_CHECK_ALL );
		break;
	case ID_VAR_CHECK_SEL:
		for( size_t i=0;i<m_varList->GetCount();i++)
			m_varList->Check( i, m_uiFormEditor->GetEditor()->IsSelected( m_varList->GetString( i ) ) );
		break;
	case ID_VAR_STORE:
		m_varCopyBuffer.clear();
		for( size_t i=0;i<m_varList->GetCount();i++ )
		{
			if ( !m_varList->IsChecked( i ) ) continue;

			if ( VarInfo *vi = m_ipd.Variables().Lookup( m_varList->GetString(i) ) )
				m_varCopyBuffer.Add( m_varList->GetString(i), new VarInfo( *vi ) );
		}
		break;
	case ID_VAR_LOAD:
		{
			wxArrayString err;

			for( VarInfoHash::iterator it = m_varCopyBuffer.begin();
				it != m_varCopyBuffer.end();
				++it )
			{
				if( m_ipd.Variables().Lookup( it->first ) == 0 )
					m_ipd.Variables().Add( it->first, new VarInfo( *(it->second)  ) );
				else
					err.Add( it->first );
			}

			VarInfoToForm( wxEmptyString );
			LoadVarList( m_varList->GetStringSelection() );

			wxShowTextMessageDialog( "Conflicts?\n" + wxJoin(err, '\n') + "\nLoaded:\n" + wxJoin( m_varCopyBuffer.ListAll(), '\n' ) );
		}
		break;
	}

}

void UIEditorPanel::SyncFormUIToDataBeforeWriting()
{
	FormToVarInfo( ); // sync any updates to the var before writing
}

void UIEditorPanel::OnFormSelectObject( wxUIFormEvent &evt )
{
	if ( wxUIObject *obj = evt.GetUIObject() )
	{
		wxString name = obj->GetName();
		if ( VarInfo *vv = m_ipd.Variables().Lookup( name ) )
		{
			FormToVarInfo( ); // save the current var
			m_varList->SetStringSelection( name );
			VarInfoToForm( name );
		}		
	}
	
}

void UIEditorPanel::FormToVarInfo( )
{
	VarInfo *vv = m_ipd.Variables().Lookup( m_curVarName );
	if ( !vv ) return;

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
	if( m_varFlagLibrary->GetValue() ) vv->Flags |= VF_LIBRARY;

	vv->DefaultValue.SetType( vv->Type );
	VarValue::Parse( vv->Type, m_varDefaultValue->GetValue(), vv->DefaultValue );
}

void UIEditorPanel::VarInfoToForm( const wxString &name )
{
	if ( VarInfo *vv = m_ipd.Variables().Lookup( name ) )
	{
		m_curVarName = name;

		m_varName->ChangeValue( m_curVarName );
		m_varType->SetSelection( vv->Type );
		m_varLabel->ChangeValue( vv->Label );
		m_varUnits->ChangeValue( vv->Units );
		m_varGroup->ChangeValue( vv->Group );
		m_varIndexLabels->ChangeValue( wxJoin( vv->IndexLabels, ',' ) );
		m_varFlagHideLabels->SetValue( vv->Flags & VF_HIDE_LABELS );
		m_varFlagParametric->SetValue( vv->Flags & VF_PARAMETRIC );
		m_varFlagIndicator->SetValue( vv->Flags & VF_INDICATOR );
		m_varFlagCalculated->SetValue( vv->Flags & VF_CALCULATED );
		m_varFlagLibrary->SetValue( vv->Flags & VF_LIBRARY );
		
		m_varDefaultValue->SetValue(vv->DefaultValue.AsString(';') );
	}
	else
	{
		m_curVarName.Clear(); // m_curVarName was not a valid variable

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
		m_varFlagLibrary->SetValue( false );
		m_varDefaultValue->SetValue( wxEmptyString );
	}

	bool en = !m_curVarName.IsEmpty();
	
	m_varName->Enable( en );
	m_varType->Enable( en );
	m_varLabel->Enable( en );
	m_varUnits->Enable( en );
	m_varGroup->Enable( en );
	m_varIndexLabels->Enable( en );
	m_varDefaultValue->Enable( en );
	m_varFlagHideLabels->Enable( en );
	m_varFlagParametric->Enable( en );
	m_varFlagIndicator->Enable( en );
	m_varFlagCalculated->Enable( en );
	m_varFlagLibrary->Enable( en );
}

void UIEditorPanel::LoadVarList( const wxString &sel )
{
	m_varList->Freeze();
	m_varList->Clear();
	wxArrayString list = m_ipd.Variables().ListAll();
	list.Sort();
	m_varList->Append( list );
	m_varList->Thaw();
	if ( !sel.IsEmpty() ) m_varList->SetStringSelection( sel );
}

bool UIEditorPanel::Write( const wxString &name )
{
	bool ok = true;
	m_ipd.Form().Copy( m_exForm );
	m_ipd.Form().SetName( name );
	// note: ipd.Variables() already up-to-date
	m_ipd.CbScript() = m_callbackScript->GetText();
	m_ipd.EqnScript() = m_equationScript->GetText();

	wxFFileOutputStream ff(  SamApp::GetRuntimePath() + "/ui/" + name + ".ui" );
	if ( ff.IsOk() )
		m_ipd.Write( ff );
	else ok = false;

	return ok;
}

bool UIEditorPanel::Load( const wxString &name )
{
	m_uiFormEditor->SetFormData( 0 );
	m_uiFormEditor->Refresh();
	m_formName.Clear();

	m_ipd.Clear();
	
	bool ok = true;

	wxString file = SamApp::GetRuntimePath() + "/ui/" + name + ".ui";

	if ( wxFileExists( file ) )
	{
		wxFFileInputStream ff( file );
		if ( ff.IsOk() && m_ipd.Read( ff ) )
		{
			m_ipd.Form().SetName( name );
			m_exForm.Copy( m_ipd.Form() );

			m_uiFormEditor->SetFormData( &m_exForm );
			m_uiFormEditor->Refresh();
			m_formName = name;
			LoadVarList();
			VarInfoToForm( wxEmptyString );

			m_callbackScript->SetText( m_ipd.CbScript() );
			m_equationScript->SetText( m_ipd.EqnScript() );
		}
		else ok = false;
	}

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

	m_startupPanel = new ScriptPanel( m_notebook, "startup.lk" );	
	m_startupPanel->AddLibrary( invoke_config_funcs(), "Config Functions" );
	m_notebook->AddPage( m_startupPanel, "Startup" );

	m_uiPanel = new UIEditorPanel( m_notebook );
	m_notebook->AddPage( m_uiPanel, "User Interface" );

	m_defaultsPanel = new DefaultsManager( m_notebook );
	m_notebook->AddPage( m_defaultsPanel, "Defaults" );

	m_metricsPanel = new ScriptPanel( m_notebook, "metrics.lk" );
	m_metricsPanel->AddLibrary( invoke_casecallback_funcs(), "Case callbacks" );
	m_metricsPanel->AddLibrary( invoke_resultscallback_funcs(), "Results callbacks" );
	m_notebook->AddPage( m_metricsPanel, "Metrics" );
	
	m_cashFlowPanel = new ScriptPanel( m_notebook, "cashflow.lk" );
	m_cashFlowPanel->AddLibrary(invoke_casecallback_funcs(), "Case callbacks");
	m_metricsPanel->AddLibrary( invoke_resultscallback_funcs(), "Results callbacks" );
	m_notebook->AddPage(m_cashFlowPanel, "Cashflows");

	m_autoGraphPanel = new ScriptPanel( m_notebook, "autographs.lk" );
	m_autoGraphPanel->AddLibrary( invoke_casecallback_funcs(), "Case callbacks" );
	m_metricsPanel->AddLibrary( invoke_resultscallback_funcs(), "Results callbacks" );
	m_notebook->AddPage( m_autoGraphPanel, "Autographs" );

	m_navigationPanel = new ScriptPanel( m_notebook, "navigation.lk" );
	m_navigationPanel->AddLibrary( invoke_casecallback_funcs(), "Case callbacks" );
	m_notebook->AddPage( m_navigationPanel, "Navigation" );

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
	
