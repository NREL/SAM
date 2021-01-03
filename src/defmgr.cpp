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

#include <wx/sizer.h>
#include <wx/textctrl.h>
#include <wx/checklst.h>
#include <wx/wfstream.h>

#include "widgets.h"
#include "main.h"
#include "casewin.h"
#include "defmgr.h"
 

static wxString GetDefaultsFile( const wxString &t, const wxString &f )
{	
#ifdef UI_BINARY
	return SamApp::GetRuntimePath() + "/defaults/" + t + "_" + f;
#elif defined(__LOAD_AS_JSON__)
	return SamApp::GetRuntimePath() + "/defaults/" + t + "_" + f + ".json";
#else
	return SamApp::GetRuntimePath() + "/defaults/" + t + "_" + f + ".txt";
#endif
}

static wxString GetTypeStr( int type )
{	
	if ( type <= VV_BINARY && type >=0  )
		return vv_strtypes[type];
	else
		return "unknown type";
}


enum{ __idfirst = wxID_HIGHEST+491,
	ID_TYPE, ID_ADD_FIELD, ID_REMOVE_FIELD, ID_CLEAR_TABLE, ID_EDIT_FIELD, ID_FIELDS, ID_VALUE, ID_MATRIX };

BEGIN_EVENT_TABLE( ValueEditor, wxPanel )
	EVT_CHOICE( ID_TYPE, ValueEditor::OnCommand )
	EVT_TEXT( ID_VALUE, ValueEditor::OnCommand )
	EVT_DATAMATRIX( ID_MATRIX, ValueEditor::OnCommand )
	EVT_LISTBOX_DCLICK( ID_FIELDS, ValueEditor::OnEditField )
	EVT_BUTTON( ID_ADD_FIELD, ValueEditor::OnCommand )
	EVT_BUTTON( ID_REMOVE_FIELD, ValueEditor::OnCommand )
	EVT_BUTTON( ID_EDIT_FIELD, ValueEditor::OnEditField )
	EVT_BUTTON( ID_CLEAR_TABLE, ValueEditor::OnCommand )
END_EVENT_TABLE()

ValueEditor::ValueEditor( wxWindow *parent )
	: wxPanel( parent )
{
	m_type = new wxChoice( this, ID_TYPE );
	for( size_t i=0;i<=VV_BINARY;i++ )
		m_type->Append( GetTypeStr(i) );


	m_text = new wxTextCtrl( this, ID_VALUE, wxEmptyString );
	m_matrix = new AFDataMatrixCtrl( this, ID_MATRIX, wxDefaultPosition, wxDefaultSize, true );
	m_matrix->ShadeR0C0( false );
	m_matrix->ShowRowLabels( true );

	m_fields = new wxListBox( this, ID_FIELDS );
	
	wxBoxSizer *sizer_top = new wxBoxSizer( wxHORIZONTAL );
	sizer_top->Add( new wxStaticText(this, wxID_ANY, "Data type:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_top->Add( m_type, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sizer_top->Add( m_valLabel = new wxStaticText(this, wxID_ANY, "Numeric, string, or hexbin value:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sizer_top->Add( m_text, 1, wxALL|wxALIGN_CENTER_VERTICAL, 3 );

	wxBoxSizer *sizer_tab_buttons = new wxBoxSizer( wxVERTICAL );
	sizer_tab_buttons->Add( m_tabLabel = new wxStaticText(this, wxID_ANY, "Table editing:"), 0, wxALL, 2 );
	sizer_tab_buttons->Add( m_addField = new wxButton( this, ID_ADD_FIELD, "Add..." ), 0, wxALL, 2 );
	sizer_tab_buttons->Add( m_removeField = new wxButton( this, ID_REMOVE_FIELD, "Remove" ), 0, wxALL, 2 );
	sizer_tab_buttons->Add( m_editField = new wxButton( this, ID_EDIT_FIELD, "Edit..." ), 0, wxALL, 2 );
	sizer_tab_buttons->Add( m_clearTable = new wxButton( this, ID_CLEAR_TABLE, "Clear" ), 0, wxALL, 2 );

	wxBoxSizer *sizer_tab = new wxBoxSizer( wxHORIZONTAL );
	sizer_tab->Add( sizer_tab_buttons, 0, wxALL, 3 );
	sizer_tab->Add( m_fields, 1, wxALL|wxEXPAND, 2 );

	wxBoxSizer *sizer_bot = new wxBoxSizer( wxHORIZONTAL );
	sizer_bot->Add( m_matrix, 1, wxALL|wxEXPAND, 2 );
	sizer_bot->Add( sizer_tab, 1, wxALL|wxEXPAND, 2 );
	
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( sizer_top, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( sizer_bot, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
	
	VarValue nil;
	Set( nil );
}

int ValueEditor::GetType() { return m_type->GetSelection(); }
void ValueEditor::Set( const VarValue &vv )
{
	m_val.Copy( vv );
	ValueToForm();
	UpdateFormUI();
}
void ValueEditor::ValueToForm()
{
	matrix_t<double> mat;
	m_type->SetSelection( m_val.Type() );
	switch( m_val.Type() )
	{
	case VV_NUMBER:
	case VV_STRING:
	case VV_BINARY:
		m_text->ChangeValue( m_val.AsString() );
		break;
	case VV_ARRAY:
		if ( m_val.Length() > 0 )
		{
			size_t nn;
			double *p = m_val.Array( &nn );
			mat.resize_fill( nn, 0.0f );
			for( size_t i=0;i<nn;i++ )
				mat.at(i) = p[i];
		}
		m_matrix->SetData( mat );
		break;
	case VV_MATRIX:
		m_matrix->SetData( m_val.Matrix() );
		break;
	case VV_TABLE:
		m_fields->Clear();
		for( VarTable::iterator it = m_val.Table().begin();
			it != m_val.Table().end();
			++ it )
		{
			m_fields->Append( it->first );
		}
		break;
	}
}


VarValue ValueEditor::Get()
{
	return m_val;
}

void ValueEditor::UpdateFormUI()
{
	int ty = GetType();
	m_text->Show( ty == VV_STRING || ty == VV_NUMBER || ty == VV_BINARY );
	m_valLabel->Show( ty == VV_STRING || ty == VV_NUMBER || ty == VV_BINARY );
	if ( ty == VV_STRING ) m_valLabel->SetLabel( "Enter string value: " );
	if ( ty == VV_NUMBER ) m_valLabel->SetLabel( "Enter numeric value: " );
	if ( ty == VV_BINARY ) m_valLabel->SetLabel( "Enter hexadecimal binary data: " );
	m_matrix->Show( ty == VV_ARRAY || ty == VV_MATRIX );
	m_tabLabel->Show( ty == VV_TABLE );
	m_fields->Show( ty == VV_TABLE );
	m_addField->Show( ty == VV_TABLE );
	m_removeField->Show( ty == VV_TABLE );
	m_editField->Show( ty == VV_TABLE );
	m_clearTable->Show( ty == VV_TABLE );
	Layout();
	Refresh();
}

void ValueEditor::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_TYPE:
		m_val.SetType( m_type->GetSelection() );
		UpdateFormUI();
		ValueToForm();
		break;
	case ID_VALUE:		
		if ( !VarValue::Parse( m_val.Type(), m_text->GetValue(), m_val ) )
			m_text->SetForegroundColour( *wxRED );
		else if ( m_text->GetForegroundColour() != *wxBLACK ) 
			m_text->SetForegroundColour( *wxBLACK );
		break;
	case ID_MATRIX:
		if ( m_val.Type() == VV_ARRAY )
		{
			matrix_t<double> mat;
			m_matrix->GetData( mat );
			int arrlen = mat.nrows();
			if ( arrlen > 0 )
			{
				double *arr = new double[arrlen];
				for( int i=0;i<arrlen;i++ )
					arr[i] = mat(i,0);

				m_val.Set( arr, arrlen );
				delete [] arr;
			}
			else
				m_val.Set( std::vector<double>() ); // empty array
		}
		else if ( m_val.Type() == VV_MATRIX )
		{
			matrix_t<double> mat;
			m_matrix->GetData( mat );
			m_val.Set( mat );
		}
		break;
	case ID_ADD_FIELD:
	{
		wxString name = wxGetTextFromUser( "Enter table field name:");
		if ( name.IsEmpty() ) return;
		if ( m_val.Table().Get( name ) != 0 ) return;
		m_val.Table().Create( name, VV_NUMBER );
		m_fields->Append( name );
	}
		break;
	case ID_REMOVE_FIELD:
	{
		wxString name = m_fields->GetStringSelection();
		if ( !name.IsEmpty() && m_val.Table().Get(name) != 0 )
		{
			m_val.Table().Delete( name );
			m_fields->Delete( m_fields->GetSelection() );
		}
	}
	case ID_CLEAR_TABLE:
		m_val.Table().clear();
		m_fields->Clear();
		break;
	}
}

void ValueEditor::OnEditField( wxCommandEvent & )
{
	wxString name = m_fields->GetStringSelection();
	if ( name.IsEmpty() ) return;
	
	VarValue *vv = m_val.Table().Get(name);
	if ( !vv )
	{
		wxMessageBox("Could not locate field in table: " + name );
		return;
	}
	
	wxDialog dlg( this, wxID_ANY, "Edit field: " + name, wxDefaultPosition, wxScaleSize(600,400), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER );
	ValueEditor *ve = new ValueEditor( &dlg );
	ve->Set( *vv );
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( ve, 1, wxALL|wxEXPAND, 0 );
	sizer->Add( dlg.CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 5 );
	dlg.SetSizer( sizer );
	if ( wxID_OK == dlg.ShowModal() )
		vv->Copy( ve->Get() );
}


enum { ID_QUERY = wxID_HIGHEST+392, ID_LOOKUP_VAR, ID_DELETE, ID_MODIFY, ID_LOAD, ID_CONFIGS, 
	ID_POPUP_first, ID_CHECK_ALL, ID_UNCHECK_ALL, ID_CHECK_SELECTED, ID_UNCHECK_SELECTED, ID_SAVE_TEXT, ID_SAVE_JSON,ID_SAVE_BINARY, ID_POPUP_last };

BEGIN_EVENT_TABLE( DefaultsManager, wxPanel )
	EVT_BUTTON( ID_QUERY, DefaultsManager::OnQuery )
	EVT_BUTTON( ID_MODIFY, DefaultsManager::OnModify )
	EVT_BUTTON( ID_LOAD, DefaultsManager::OnLoad )
	EVT_BUTTON( ID_LOOKUP_VAR, DefaultsManager::OnLookupVar )
	EVT_BUTTON( ID_DELETE, DefaultsManager::OnDeleteVar )
	EVT_MENU_RANGE( ID_POPUP_first, ID_POPUP_last, DefaultsManager::OnPopupMenu )
END_EVENT_TABLE()


DefaultsManager::DefaultsManager( wxWindow *parent )
	: wxPanel( parent )
{
	m_varName = new wxTextCtrl( this, wxID_ANY, wxEmptyString );
	
	m_value = new ValueEditor( this );

	m_changeType = new wxCheckBox( this, wxID_ANY, "Change data type?" );
	m_enableAdd = new wxCheckBox( this, wxID_ANY, "Add variable?" );
	
	m_output = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxBORDER_NONE );
	m_output->SetForegroundColour( *wxLIGHT_GREY );
	m_output->SetBackgroundColour( "NAVY" );
	m_output->SetFont( wxFont(11, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "consolas") );

	m_configList = new wxCheckListBox( this, ID_CONFIGS, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_EXTENDED );
	m_configList->Connect( ID_CONFIGS, wxEVT_RIGHT_UP,
		wxMouseEventHandler(DefaultsManager::OnListRightClick), NULL, this );

	// layout
	wxBoxSizer *button_sizer1 = new wxBoxSizer( wxHORIZONTAL );
	button_sizer1->Add( new wxStaticText( this, wxID_ANY, "Variable name:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4);
	button_sizer1->Add( m_varName, 1, wxALL|wxEXPAND, 4 );
	button_sizer1->Add( new wxButton( this, ID_LOOKUP_VAR, "Lookup" ), 0, wxALL, 2 );
	button_sizer1->Add( new wxButton( this, ID_QUERY, "Query" ), 0, wxALL, 2 );	
	button_sizer1->Add( new wxButton( this, ID_DELETE, "Delete" ), 0, wxALL, 2 );	
	button_sizer1->Add( m_changeType, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
	button_sizer1->Add( m_enableAdd, 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	button_sizer1->Add( new wxButton(this, ID_MODIFY, "Modify"), 0, wxALL, 2 );
	button_sizer1->Add( new wxButton(this, ID_LOAD, "Load"), 0, wxALL, 2 );
	
	wxBoxSizer *right_sizer = new wxBoxSizer( wxVERTICAL );
	right_sizer->Add( button_sizer1, 0, wxALL|wxEXPAND, 4 );
	right_sizer->Add( m_value, 1, wxALL|wxEXPAND, 0 );
	right_sizer->Add( m_output, 2, wxALL|wxEXPAND, 0 );
	
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

void DefaultsManager::OnPopupMenu( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_CHECK_ALL:
	case ID_UNCHECK_ALL:
		for( size_t i=0;i<m_configList->GetCount();i++ )
			m_configList->Check( i, evt.GetId() == ID_CHECK_ALL );
		break;
	case ID_CHECK_SELECTED:
	case ID_UNCHECK_SELECTED:
		for( size_t i=0;i<m_configList->GetCount();i++ )
			if ( m_configList->IsSelected(i) )
				m_configList->Check( i, evt.GetId() == ID_CHECK_SELECTED );
		break;
	case ID_SAVE_BINARY:
	case ID_SAVE_TEXT:
	case ID_SAVE_JSON:
		OnSaveAsType(evt);
		break;
	}
}

void DefaultsManager::OnSaveAsType(wxCommandEvent &evt)
{
	for (size_t i = 0; i < m_configList->GetCount(); i++)
		if (m_configList->IsChecked(i))
		{
			wxString file(GetDefaultsFile(m_techList[i], m_finList[i]));
			VarTable tab;
			bool success = true;
#ifdef UI_BINARY
			if (!tab.Read(file))
#elif defined(__LOAD_AS_JSON__)
			if (!tab.Read_JSON(file.ToStdString()))
#else
			if (!tab.Read_text(file))
#endif
			{
				Log("file read error: " + file);
				continue;
			}
			if (evt.GetId() == ID_SAVE_BINARY)
			{
				file = SamApp::GetRuntimePath() + "/defaults/" + m_techList[i] + "_" + m_finList[i];
				success = tab.Write(file);
			}
			else if (evt.GetId() == ID_SAVE_TEXT)
			{
				file = SamApp::GetRuntimePath() + "/defaults/" + m_techList[i] + "_" + m_finList[i] + ".txt";
				success = tab.Write_text(file);
			}
			else if (evt.GetId() == ID_SAVE_JSON)
			{
				file = SamApp::GetRuntimePath() + "/defaults/" + m_techList[i] + "_" + m_finList[i] + ".json";
				success = tab.Write_JSON(file.ToStdString());
			}
			else
				Log(wxString::Format("invalid event ID: %d", evt.GetId()));

			if (!success)
				Log("file write error: " + file);
		}
}

void DefaultsManager::OnListRightClick( wxMouseEvent & )
{
	wxMenu menu;
	menu.Append( ID_CHECK_SELECTED, "Check selected" );
	menu.Append( ID_UNCHECK_SELECTED, "Uncheck selected" );
	menu.AppendSeparator();
	menu.Append(ID_CHECK_ALL, "Check all");
	menu.Append(ID_UNCHECK_ALL, "Uncheck all");
	menu.AppendSeparator();
//	menu.Append(ID_SAVE_BINARY, "Save checked as binary");
//	menu.Append(ID_SAVE_TEXT, "Save checked as text");
	menu.Append(ID_SAVE_JSON, "Save checked");
	PopupMenu( &menu );
}

void DefaultsManager::ClearLog()
{
	m_output->Clear();
}

void DefaultsManager::Log(const wxString &s)
{
	m_output->AppendText( s + "\n" );
}

void DefaultsManager::OnQuery(wxCommandEvent &)
{
	ClearLog();

	for (int i=0;i<(int)m_configList->GetCount();i++)
	{
		if (!m_configList->IsChecked(i)) continue;
		
		wxString file(GetDefaultsFile(m_techList[i], m_finList[i]));
		VarTable tab;
#ifdef UI_BINARY
		if ( !tab.Read( file ))		
#elif defined(__LOAD_AS_JSON__)
		if (!tab.Read_JSON(file.ToStdString()))
#else
		if (!tab.Read_text(file))
#endif
		{
			Log("file error: " + file);
			continue;
		}

		wxString name( m_varName->GetValue() );
		
		if ( VarValue *vv = tab.Get( name ) )
			Log("'" + name + "' in " + m_techList[i] + ", " + m_finList[i] + " (" + GetTypeStr( vv->Type() ) + ") = " + vv->AsString() );
	}
}

void DefaultsManager::OnLoad( wxCommandEvent & )
{
	ClearLog();
	size_t i = 0;
	for (i = 0;i < m_configList->GetCount(); i++)
		if (m_configList->IsChecked(i)) break;

	if ( i == m_configList->GetCount() ) return;

	wxString file(GetDefaultsFile(m_techList[i], m_finList[i]));
	VarTable tab;
#ifdef UI_BINARY
	if (!tab.Read(file))
#elif defined(__LOAD_AS_JSON__)
	if (!tab.Read_JSON(file.ToStdString()))
#else
	if (!tab.Read_text(file))
#endif
	{
		Log("file read error: " + file);
		return;
	}

	wxString name( m_varName->GetValue() );		
	if( VarValue *vv = tab.Get( name ) )
	{
		m_value->Set( *vv );
		Log("Loaded '" + name + "' from " + m_techList[i] + ", " + m_finList[i]  + " (" + GetTypeStr( vv->Type() ) + ")" );
	}
	else
		Log("Variable '" + name + "' not found in " + m_techList[i] + ", " + m_finList[i]);
}

void DefaultsManager::OnModify( wxCommandEvent & )
{
	ClearLog();
	

	bool en_change = m_changeType->GetValue();
	int datatype = m_value->GetType();
	VarValue value = m_value->Get();

	for (int i=0;i<(int)m_configList->GetCount();i++)
	{
		if (!m_configList->IsChecked(i)) continue;
				
		wxString file(GetDefaultsFile(m_techList[i], m_finList[i]));
		VarTable tab;
#ifdef UI_BINARY
		if (!tab.Read(file))
#elif defined(__LOAD_AS_JSON__)
		if (!tab.Read_JSON(file.ToStdString()))
#else
		if (!tab.Read_text(file))
#endif
		{
			Log("file read error: " + file);
			continue;
		}

		wxString name( m_varName->GetValue() );
		bool needs_write = false;
		
		VarValue *vv = tab.Get( name );

		if ( !vv && m_enableAdd->GetValue() )
		{
			vv = tab.Create( name, datatype );
			Log("Created '" + name + "' in " + m_techList[i] + ", " + m_finList[i]  + " (" + GetTypeStr( vv->Type() ) + ") = " + vv->AsString() );
			needs_write = true;
		}


		if ( !vv ) 
			continue;

		if ( en_change && vv->Type() != datatype )
		{
			needs_write = true;
			vv->SetType( datatype );
			Log("Changed data type to " + GetTypeStr(datatype) + " for '" + name + "' in " + m_techList[i] + ", " + m_finList[i]);
		}

		if ( vv->Type() == value.Type() )
		{
			vv->Copy( value );
			needs_write = true;

			if ( vv->Type() == VV_NUMBER 
				|| vv->Type() == VV_STRING 
				|| vv->Type() == VV_MATRIX 
				|| vv->Type() == VV_TABLE 
				|| vv->Type() == VV_BINARY )
			{
				Log("Set '" + name + "' in " + m_techList[i] + ", " + m_finList[i]  + " (" + GetTypeStr( vv->Type() ) + ") = " + vv->AsString() );
			}
			else if ( vv->Type() == VV_ARRAY )
			{	
				size_t arrlen;
				double *arr = value.Array( & arrlen );
				wxString s("Set '" + name + "' in " + m_techList[i] + ", " + m_finList[i]  + " (" + GetTypeStr( vv->Type() ) + ") = ");
				if ( arrlen > 25 )
				{
					s += wxString::Format("[%d]: ", (int)arrlen);
					for( size_t j=0;j<10;j++ )
						s += wxString::Format("%g%c", (float)arr[j], j<9?',':' ');
					s += "...";
				}
				else
					s += vv->AsString();	
			
				Log("Set '" + name + "' in " + m_techList[i] + ", " + m_finList[i]  + " (" + GetTypeStr( vv->Type() ) + ") = " + s );

			}
		}

		if ( needs_write )
		{
#ifdef UI_BINARY
			if (!tab.Write(file))
#elif defined(__LOAD_AS_JSON__)
			if (!tab.Write_JSON(file.ToStdString()))
#else
			if (!tab.Write_text(file))
#endif
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
#ifdef UI_BINARY
		if ( !tab.Read( file ) )
#elif defined(__LOAD_AS_JSON__)
		if (!tab.Read_JSON(file.ToStdString()))
#else
		if (!tab.Read_text(file))
#endif
		{
			Log("read error: " + file );
			continue;
		}

		if ( tab.Get( name ) )
		{
			tab.Delete( name );

#ifdef UI_BINARY
			if (!tab.Write(file))
#elif defined(__LOAD_AS_JSON__)
			if (!tab.Write_JSON(file.ToStdString()))
#else
			if (!tab.Write_text(file))
#endif
				Log("Error writing: " + file );
			else
				Log("Deleted '" + name + "' from " + m_techList[i] + ", " + m_finList[i]);
		}
		else
			Log("Variable '" + name + "' not found in " + m_techList[i] + ", " + m_finList[i]);
	}
}


void DefaultsManager::OnLookupVar( wxCommandEvent & )
{
	VarSelectDialog dlg( this, "Select Input Variable" );
	if ( wxID_OK == dlg.ShowModal() )
		m_varName->ChangeValue( wxJoin(dlg.GetCheckedNames(),',') );
}
