#include <vector>
#include <algorithm>

#include <wx/wx.h>
#include <wx/filename.h>
#include <wx/dir.h>

#include <ssc/sscapi.h>

#include "variablegrid.h"
#include "object.h"
#include "main.h"

class VarManager {
public:
	VarManager() { }
	~VarManager() {
		DeleteAll();
	}

	void DeleteAll() {
		for( size_t i=0;i<m_libs.size();i++ )
			delete m_libs[i];
		m_libs.clear();
	}

	std::vector<VariableGrid*> m_libs;
};

static VarManager gs_libs;

VariableGrid *VariableGrid::Load(std::vector<Case*> &cases)
{
	VariableGrid *l = new VariableGrid;
	if ( l->Read( cases ) )
	{
		gs_libs.m_libs.push_back( l );
		return l;
	}
	else
	{
		delete l;
		return 0;
	}
}

VariableGrid *VariableGrid::Find( const wxString &name )
{
	for( size_t i=0;i<gs_libs.m_libs.size();i++ )
		if ( name == gs_libs.m_libs[i]->GetName() )
			return gs_libs.m_libs[i];
	return 0;
}

void VariableGrid::UnloadAll()
{
	gs_libs.DeleteAll();
}


VariableGrid::VariableGrid()
{
	m_startRow = 0;
}

bool VariableGrid::Read(std::vector<Case*> &cases)
{
/*	if ( !m_csv.ReadFile( file ) )
		return false;

	m_name = wxFileName(file).GetName();
*/
	return ScanData();

}


bool VariableGrid::ScanData()
{
	m_errors.clear();

	m_startRow = 2;
	/*
	while( m_startRow < m_csv.NumRows()
		&& !m_csv(m_startRow, 0).IsEmpty()
		&& m_csv(m_startRow, 0)[0] == '[' )
		m_startRow++;

	if ( m_startRow >= m_csv.NumRows() )
		return false;
*/
	size_t nvarlists = m_startRow-2;

	if ( nvarlists < 1 )
		return false;

	m_fields.clear();
	/*
	size_t ncol = m_csv.NumCols();
	for( size_t i=0;i<ncol;i++)
	{
		Field f;
		if ( i == 0 ) f.Name = wxT("Name");
		else
		{
			f.Name = m_csv( 0, i );
			f.Units = m_csv( 1, i );
		}

		for( size_t j=0;j<nvarlists;j++ )
			f.Variables.Add( m_csv( 2+j, i ) );

		f.DataIndex = i;

		m_fields.push_back( f );
	}
	*/
	return true;
}

wxString VariableGrid::GetName() const
{
	return m_name;
}

wxArrayString VariableGrid::ListEntries()
{
	wxArrayString list;
//	for( size_t i=m_startRow;i<m_csv.NumRows();i++ )
//		list.Add( m_csv(i,0) );
	return list;
}

size_t VariableGrid::NumEntries()
{
	return 0;// m_csv.NumRows() - m_startRow;
}

std::vector<VariableGrid::Field> &VariableGrid::GetFields()
{
	return m_fields;
}

int VariableGrid::GetFieldIndex( const wxString &name )
{
	for( size_t i=0;i<m_fields.size();i++ )
		if ( m_fields[i].Name.Lower() == name.Lower() )
			return i;

	return -1;
}

int VariableGrid::FindEntry( const wxString &name )
{
//	for( size_t i=m_startRow;i<m_csv.NumRows();i++ )
//		if ( m_csv(i,0) == name )
//			return (int)(i-m_startRow);

	return -1;
}

wxString VariableGrid::GetEntryValue( int entry, int field )
{
	return wxEmptyString; // m_csv(entry + m_startRow, field);
}

wxString VariableGrid::GetEntryName( int entry )
{
	return GetEntryValue( entry, 0 );
}

bool VariableGrid::ApplyEntry( int entry, int varindex, VarTable &tab, wxArrayString &changed )
{
	m_errors.Clear();

	if ( varindex < 0 || varindex >= m_startRow-2 )
	{
		m_errors.Add( wxString::Format("invalid varindex of %d", varindex ) );
		return false;
	}

	size_t row = m_startRow + (size_t)entry;
/*	if ( row >= m_csv.NumRows() || row < m_startRow )
	{
		m_errors.Add( wxString::Format("invalid entry %d (max %d)", entry, (int)(m_csv.NumRows()-m_startRow)) );
		return false;
	}
*/
	for( size_t i=1;i<m_fields.size();i++ )
	{
		Field &f = m_fields[i];

		wxString var;
		if ( varindex < f.Variables.size() )
			var = f.Variables[varindex];

		if ( var.IsEmpty() ) continue; // skip this variable if no name was found

		if ( VarValue *vv = tab.Get( var ) )
		{
//			if( VarValue::Parse( vv->Type(), m_csv(row,f.DataIndex), *vv ) )
//				changed.Add( var );
//			else
				m_errors.Add( "could not parse '" + var + "' to required data type" );
		}
		else
			m_errors.Add( "variable '" + var + "' not found in collection" );
	}

	return m_errors.Count() == 0;
}


VariableGridListView::VariableGridListView( VariableGridCtrl *parent, int id, const wxPoint &pos,
	const wxSize &size )
	: wxListView( parent, id, pos, size, wxLC_REPORT|wxLC_VIRTUAL|wxLC_SINGLE_SEL )
{
	m_variablegridctrl = parent;
}

wxString VariableGridListView::OnGetItemText( long item, long col ) const
{
	return m_variablegridctrl->GetCellValue(item, col);
}
/*
wxListItemAttr *VariableGridListView::OnGetItemAttr( long item ) const
{
	static wxListItemAttr even( *wxBLACK, *wxWHITE, *wxNORMAL_FONT ),
		odd( *wxBLACK, wxColour(245,245,245), *wxNORMAL_FONT );

	return (item%2==0) ? &even : &odd;
}
*/

enum { ID_LIST = wxID_HIGHEST+495, ID_FILTER, ID_TARGET, ID_REFRESH };

BEGIN_EVENT_TABLE( VariableGridCtrl, wxPanel )
	EVT_LIST_ITEM_SELECTED( ID_LIST, VariableGridCtrl::OnSelected )
	EVT_LIST_COL_CLICK( ID_LIST, VariableGridCtrl::OnColClick )
	EVT_TEXT( ID_FILTER, VariableGridCtrl::OnCommand )
	EVT_BUTTON( ID_REFRESH, VariableGridCtrl::OnCommand )
	EVT_CHOICE( ID_TARGET, VariableGridCtrl::OnCommand )
END_EVENT_TABLE()


VariableGridCtrl::VariableGridCtrl( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxPanel( parent, id, pos, size, wxTAB_TRAVERSAL )
{
	m_sendEvents = true;

	m_label = new wxStaticText( this, wxID_ANY, wxT("Search for:") );
	m_filter = new wxTextCtrl( this, ID_FILTER );
	m_target = new wxChoice( this, ID_TARGET );
	m_notify = new wxStaticText( this, wxID_ANY, wxEmptyString );
	m_notify->SetForegroundColour( *wxRED );
	m_list = new VariableGridListView( this, ID_LIST );

	wxBoxSizer *sz_horiz = new wxBoxSizer( wxHORIZONTAL );
	sz_horiz->Add( m_label, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	sz_horiz->Add( m_filter, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_horiz->Add( m_target, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_horiz->AddStretchSpacer();
	sz_horiz->Add( m_notify, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	//sz_horiz->Add( new wxButton( this, ID_REFRESH, wxT("Refresh list") ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );

	wxBoxSizer *sz_vert = new wxBoxSizer( wxVERTICAL );
	sz_vert->Add( sz_horiz, 0, wxALL|wxEXPAND, 0 );
	sz_vert->Add( m_list, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sz_vert );
}

VariableGridCtrl::~VariableGridCtrl()
{
	// nothing to do
}

bool VariableGridCtrl::SetEntrySelection( const wxString &entry )
{
	if ( VariableGrid *lib = VariableGrid::Find( m_variablegrid ) )
	{
		int entry_idx = lib->FindEntry( entry );
		if ( entry_idx >= 0 )
		{
			int to_sel = -1;
			// find in current view

			for( size_t i=0;i<m_view.size();i++ )
			{
				if ( m_view[i].index == entry_idx )
				{
					to_sel = i;
					break;
				}
			}

			if ( to_sel < 0 )
			{
				// selecting item that is not in the currently filtered view,
				// so we must add it to the view
				m_view.push_back( viewable(entry, entry_idx) );
				m_list->SetItemCount( m_view.size() );
				m_list->Refresh();

				to_sel = m_view.size()-1;
			}

			m_list->SetItemState( to_sel, wxLIST_STATE_SELECTED|wxLIST_STATE_FOCUSED,
				wxLIST_STATE_SELECTED|wxLIST_STATE_FOCUSED );
			m_list->EnsureVisible( to_sel );

			return true;
		}
	}

	return false;
}

wxString VariableGridCtrl::GetEntrySelection()
{
	long sel = m_list->GetFirstSelected();
	if ( sel >= 0 && sel < m_view.size() ) return m_view[ sel ].name;
	return wxEmptyString;
}

wxString VariableGridCtrl::GetCellValue( long item, long col )
{
	if ( VariableGrid *lib = VariableGrid::Find( m_variablegrid ) )
		if ( item < m_view.size() && col < m_fieldMap.size() )
			return lib->GetEntryValue( m_view[item].index, m_fieldMap[col] );

	return wxT("<inval>");
}

void VariableGridCtrl::SetVariableGrid( const wxString &name, const wxString &fields )
{
	m_variablegrid = name;

	if( VariableGrid *lib = VariableGrid::Find( m_variablegrid ) )
	{
		m_fields.Clear();
		m_fieldMap.clear();

		if ( fields == "*" )
		{
			std::vector<VariableGrid::Field> &ff = lib->GetFields();
			for( size_t i=0;i<ff.size();i++ )
			{
				m_fields.Add( ff[i].Name );
				m_fieldMap.push_back( i );
			}
		}
		else
		{
			wxArrayString fnames = wxSplit( fields, ',' );
			for( size_t i=0;i<fnames.size();i++ )
			{
				int idx = lib->GetFieldIndex( fnames[i] );
				if ( idx >= 0 )
				{
					m_fields.Add( fnames[i] );
					m_fieldMap.push_back( lib->GetFieldIndex( fnames[i] ) );
				}
			}
		}

		m_sortDir.resize( m_fieldMap.size(), true );
	}

	ReloadVariableGrid();

}

void VariableGridCtrl::ReloadVariableGrid()
{
	if( VariableGrid *lib = VariableGrid::Find( m_variablegrid ) )
	{
		wxString tarsel = m_target->GetStringSelection();
		m_target->Clear();

		m_list->ClearAll();
		for( size_t i=0;i<m_fields.size();i++ )
		{
			m_list->AppendColumn( m_fields[i] );
			m_target->Append( m_fields[i] );
		}

		if ( tarsel.IsEmpty()  && m_target->GetCount() > 0 )
			m_target->SetSelection( 0 );
		else
			m_target->SetStringSelection( tarsel );

		m_entries = lib->ListEntries();

		UpdateList();

		m_list->SetColumnWidth( 0, 350 );

	}
	else
		wxLogStatus( "VariableGridCtrl: could not find variablegrid " + m_variablegrid );
}

void VariableGridCtrl::UpdateList()
{
	m_sendEvents = false;

	wxString filter = m_filter->GetValue().Lower();
	wxString sel = GetEntrySelection();

	m_notify->SetLabel( wxEmptyString );
	m_view.clear();
	if ( m_entries.size() > 0 )
		m_view.reserve( m_entries.size() );

	size_t nmatches = 0;

	if( VariableGrid *lib = VariableGrid::Find( m_variablegrid ) )
	{
		size_t num_entries = lib->NumEntries();

		size_t target_field_idx = 0;
		int t_sel = m_target->GetSelection();
		if ( t_sel >= 0 && t_sel < m_fieldMap.size() )
			target_field_idx = m_fieldMap[t_sel];

		for (size_t i=0;i<num_entries;i++)
		{
			wxString target = lib->GetEntryValue( i, target_field_idx );

			if ( filter.IsEmpty()
				|| (filter.Len() <= 2 && target.Left( filter.Len() ).Lower() == filter)
				|| (target.Lower().Find( filter ) >= 0)
				)
			{
				m_view.push_back( viewable(m_entries[i], i) );
				nmatches++;
			}
			else if ( target == sel )
				m_view.push_back( viewable(m_entries[i], i) );
		}
	}

	if ( nmatches == 0 )
	{
		m_notify->SetLabel( "No matches. (selected item shown)" );
		Layout();
	}

	m_list->SetItemCount( m_view.size() );
	m_list->Refresh();
	SetEntrySelection( sel );

	m_sendEvents = true;
}

void VariableGridCtrl::OnSelected( wxListEvent &evt )
{
	if ( !m_sendEvents ) return;

	wxCommandEvent issue( wxEVT_LISTBOX, GetId() );
	issue.SetEventObject( this );
	issue.SetInt( evt.GetSelection() );
	ProcessEvent( issue );

}

bool VariableGridCtrl::viewable_compare::operator() ( const viewable &lhs, const viewable &rhs )
{
	if ( dir && col[lhs.index] < col[rhs.index] ) return true;
	else if ( !dir && col[lhs.index] > col[rhs.index] ) return true;
	else return false;
}

void VariableGridCtrl::OnColClick( wxListEvent &evt )
{
	wxBusyCursor wait;

	if( VariableGrid *lib = VariableGrid::Find( m_variablegrid ) )
	{
		m_sendEvents = false;

		wxString sel = GetEntrySelection();

		size_t field_idx = m_fieldMap[evt.GetColumn()];

		wxArrayString column;
		column.reserve( m_entries.size() );
		for( size_t i=0;i<m_entries.size();i++ )
			column.push_back( lib->GetEntryValue( i, field_idx ) );

		viewable_compare cc( column, ! m_sortDir[evt.GetColumn()] );
		std::stable_sort( m_view.begin(), m_view.end(), cc );

		m_sortDir[ evt.GetColumn() ] = ! m_sortDir[evt.GetColumn()];

		m_list->Refresh();
		SetEntrySelection( sel );

		m_sendEvents = true;
	}

}

void VariableGridCtrl::OnCommand( wxCommandEvent &evt )
{
	if( evt.GetId() == ID_FILTER || evt.GetId() == ID_TARGET )
		UpdateList();
}

void VariableGridCtrl::SetLabel( const wxString &text )
{
	m_label->SetLabel( text );
	Layout();
}


