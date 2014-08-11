#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/checklst.h>
#include <wx/wfstream.h>

#include "main.h"
#include "casewin.h"
#include "defmgr.h"


static wxString GetDefaultsFile( const wxString &t, const wxString &f )
{	
	return SamApp::GetRuntimePath() + "/defaults/" + t + "_" + f;
}

static wxString GetTypeStr( int type )
{	
static wxString strtypes[] = { "invalid", "number", "array", "matrix", "string", "table", "binary" };
	if ( type <= VV_BINARY && type >=0  )
		return strtypes[type];
	else
		return "unknown type";
}

enum { ID_QUERY = wxID_HIGHEST+392, ID_LOOKUP_VAR, ID_DELETE, ID_MODIFY };

BEGIN_EVENT_TABLE( DefaultsManager, wxPanel )
	EVT_BUTTON( ID_QUERY, DefaultsManager::OnQuery )
	EVT_BUTTON( ID_MODIFY, DefaultsManager::OnModify )
	EVT_BUTTON( ID_LOOKUP_VAR, DefaultsManager::OnLookupVar )
	EVT_BUTTON( ID_DELETE, DefaultsManager::OnDeleteVar )
END_EVENT_TABLE()


DefaultsManager::DefaultsManager( wxWindow *parent )
	: wxPanel( parent )
{
	m_varName = new wxTextCtrl( this, wxID_ANY, wxEmptyString );
	
	m_value = new wxTextCtrl( this, wxID_ANY, wxEmptyString );
	m_dataType = new wxChoice( this, wxID_ANY );
	m_dataType->Append( "Don't change type" );
	for( size_t i=1;i<=VV_BINARY;i++ )
		m_dataType->Append( "Change to " + GetTypeStr(i) );
	m_dataType->SetSelection( 0 );
	m_enableAdd = new wxCheckBox( this, wxID_ANY, "Add variable?" );
	
	m_output = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxBORDER_NONE );
	m_output->SetForegroundColour( *wxLIGHT_GREY );
	m_output->SetBackgroundColour( "NAVY" );
	m_output->SetFont( wxFont(11, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "consolas") );

	m_configList = new wxCheckListBox( this, wxID_ANY );

	// layout

	wxBoxSizer *button_sizer1 = new wxBoxSizer( wxHORIZONTAL );
	button_sizer1->Add( new wxStaticText( this, wxID_ANY, "Variable name:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	button_sizer1->Add( m_varName, 1, wxALL|wxEXPAND, 4 );
	button_sizer1->Add( new wxButton( this, ID_LOOKUP_VAR, "Lookup" ), 0, wxALL, 2 );
	button_sizer1->Add( new wxButton( this, ID_QUERY, "Query" ), 0, wxALL, 2 );	
	button_sizer1->Add( new wxButton( this, ID_DELETE, "Delete" ), 0, wxALL, 2 );	
	
	wxBoxSizer *button_sizer2 = new wxBoxSizer( wxHORIZONTAL );
	button_sizer2->Add( new wxStaticText( this, wxID_ANY, "Variable value (number, string, or binhex):"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	button_sizer2->Add( m_value, 1, wxALL|wxEXPAND, 4 );
	button_sizer2->Add( m_dataType, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
	button_sizer2->Add( m_enableAdd, 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	button_sizer2->Add( new wxButton(this, ID_MODIFY, "Modify"), 0, wxALL, 2 );

	wxBoxSizer *right_sizer = new wxBoxSizer( wxVERTICAL );
	right_sizer->Add( button_sizer1, 0, wxALL|wxEXPAND, 4 );
	right_sizer->Add( button_sizer2, 0, wxALL|wxEXPAND, 4 );
	right_sizer->Add( m_output, 1, wxALL|wxEXPAND, 0 );
	
	wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
	sizer->Add( m_configList, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( right_sizer, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );

	// initialize list

	int num_defaults = 0;
	wxArrayString tl = SamApp::Config().GetTechnologies();
	for (int i=0;i<(int)tl.Count();i++)
	{
		wxArrayString fl = SamApp::Config().GetFinancingForTech(tl[i]);
		for (int j=0;j<(int)fl.Count();j++)
		{
			wxString item = tl[i] + "," + fl[j];
			
			m_techList.Add( tl[i] );
			m_finList.Add( fl[j] );

			if (wxFileExists(GetDefaultsFile(tl[i], fl[j])))
				num_defaults++;
			else
				item = "(?)  " + item;

			m_configList->Append(item);
		}
	}

	m_output->AppendText( wxString::Format("%d total configurations, %d defaults files missing.\n", 
					(int) m_configList->GetCount(), (int) m_configList->GetCount()-num_defaults) );
}


void DefaultsManager::ClearLog()
{
	m_output->Clear();
}

void DefaultsManager::Log(const wxString &s)
{
	m_output->AppendText( s + "\n" );
}

void DefaultsManager::OnQuery(wxCommandEvent &evt)
{
	ClearLog();

	for (int i=0;i<(int)m_configList->GetCount();i++)
	{
		if (!m_configList->IsChecked(i)) continue;
		
		wxString file(GetDefaultsFile(m_techList[i], m_finList[i]));
		VarTable tab;
		if ( !tab.Read( file ))			
		{
			Log("file error: " + file);
			continue;
		}

		wxString name( m_varName->GetValue() );
		
		if ( VarValue *vv = tab.Get( name ) )
			Log("'" + name + "' in " + m_techList[i] + ", " + m_finList[i] + " (" + GetTypeStr( vv->Type() ) + ") = " + vv->AsString() );
	}
}

void DefaultsManager::OnModify( wxCommandEvent & )
{
	ClearLog();
	
	wxString value = m_value->GetValue();
	int datatype = m_dataType->GetSelection();

	for (int i=0;i<(int)m_configList->GetCount();i++)
	{
		if (!m_configList->IsChecked(i)) continue;
				
		wxString file(GetDefaultsFile(m_techList[i], m_finList[i]));
		VarTable tab;
		if ( !tab.Read( file ))			
		{
			Log("file read error: " + file);
			continue;
		}

		wxString name( m_varName->GetValue() );
		bool needs_write = false;
		if ( VarValue *vv = tab.Get( name ) )
		{
			if ( datatype > 0 && vv->Type() != datatype )
			{
				needs_write = true;
				vv->SetType( datatype );
				Log("Changed data type to " + GetTypeStr(datatype) + " for '" + name + "' in " + m_techList[i] + ", " + m_finList[i]);
			}

			if ( vv->Type() == VV_NUMBER 
				|| vv->Type() == VV_STRING 
				|| vv->Type() == VV_BINARY )
			{
				if ( VarValue::Parse( vv->Type(), value, *vv ) )
				{
					needs_write = true;
					Log("Set '" + name + "' in " + m_techList[i] + ", " + m_finList[i]  + " (" + GetTypeStr( vv->Type() ) + ") = " + vv->AsString() );
				}
				else
					Log("Error setting '" + name + "' in " + m_techList[i] + ", " + m_finList[i]  + " to " + value );
			}

		} 
		else if ( m_enableAdd->GetValue() )
		{
			VarValue *vv = tab.Create( name, datatype );
			VarValue::Parse( vv->Type(), value, *vv );
			Log("Created '" + name + "' in " + m_techList[i] + ", " + m_finList[i]  + " (" + GetTypeStr( vv->Type() ) + ") = " + vv->AsString() );
			needs_write = true;
		}

		if ( needs_write )
		{
			if ( !tab.Write( file ) )
				Log("file write error: " + file );
		}

	}
}

void DefaultsManager::OnDeleteVar(wxCommandEvent &)
{
	wxString name( m_varName->GetValue() );
	
	if (wxYES!=wxMessageBox("Are you sure you want to remove '" + name + "' from selected configurations?", "Query", wxYES_NO))
		return;

	ClearLog();

	for (int i=0;i<(int)m_configList->GetCount();i++)
	{
		if (!m_configList->IsChecked(i)) continue;
		wxString file = GetDefaultsFile(m_techList[i], m_finList[i]);
		
		VarTable tab;
		if ( !tab.Read( file ) )
		{
			Log("read error: " + file );
			continue;
		}

		if ( VarValue *vv = tab.Get( name ) )
		{
			tab.Delete( name );

			if (!tab.Write( file ) )
				Log("Error writing: " + file );
			else
				Log("Removed '" + name + "' from " + m_techList[i] + ", " + m_finList[i]);
		}
		else
			Log("Variable '" + name + "' not found in " + m_techList[i] + ", " + m_finList[i]);
	}
}


void DefaultsManager::OnLookupVar( wxCommandEvent &evt )
{
	VarSelectDialog dlg( this, "Select Input Variable" );
	if ( wxID_OK == dlg.ShowModal() )
		m_varName->ChangeValue( wxJoin(dlg.GetCheckedNames(),',') );
}
