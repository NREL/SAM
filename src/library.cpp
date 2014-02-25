#include <vector>
#include <algorithm>

#include <wx/wx.h>
#include <wx/filename.h>
#include <wx/dir.h>

#include <ssc/sscapi.h>

#include "library.h"
#include "object.h"
#include "main.h"

class LibManager {
public:
	LibManager() { }
	~LibManager() {
		DeleteAll();
	}
	
	void DeleteAll() {
		for( size_t i=0;i<m_libs.size();i++ )
			delete m_libs[i];
		m_libs.clear();
	}

	std::vector<Library*> m_libs;
};

static LibManager gs_libs;

Library *Library::Load( const wxString &file )
{
	Library *l = new Library;
	if ( l->Read( file ) )
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

Library *Library::Find( const wxString &name )
{
	for( size_t i=0;i<gs_libs.m_libs.size();i++ )
		if ( name == gs_libs.m_libs[i]->GetName() )
			return gs_libs.m_libs[i];
	return 0;
}

void Library::UnloadAll()
{
	gs_libs.DeleteAll();
}


Library::Library()
{
	m_startRow = 0;
}
	
bool Library::Read( const wxString &file )
{
	if ( !m_csv.ReadFile( file ) )
		return false;

	m_name = wxFileName(file).GetName();

	return ScanData();

}

bool Library::Read( const wxCSVData &data, const wxString &name )
{
	m_csv = data;
	m_name = name;
	return ScanData();
}

bool Library::ScanData()
{
	m_errors.clear();

	m_startRow = 2;
	while( m_startRow < m_csv.NumRows()
		&& !m_csv(m_startRow, 0).IsEmpty()
		&& m_csv(m_startRow, 0)[0] == '[' )
		m_startRow++;

	if ( m_startRow >= m_csv.NumRows() )
		return false;

	size_t nvarlists = m_startRow-2;

	if ( nvarlists < 1 )
		return false;
	
	m_fields.clear();
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

	return true;
}

wxString Library::GetName() const
{
	return m_name;
}

wxArrayString Library::ListEntries()
{
	wxArrayString list;
	for( size_t i=m_startRow;i<m_csv.NumRows();i++ )
		list.Add( m_csv(i,0) );
	return list;
}

size_t Library::NumEntries()
{
	return m_csv.NumRows()-m_startRow;
}

std::vector<Library::Field> &Library::GetFields()
{
	return m_fields;
}

int Library::GetFieldIndex( const wxString &name )
{
	for( size_t i=0;i<m_fields.size();i++ )
		if ( m_fields[i].Name.Lower() == name.Lower() )
			return i;
		
	return -1;
}

int Library::FindEntry( const wxString &name )
{
	for( size_t i=m_startRow;i<m_csv.NumRows();i++ )
		if ( m_csv(i,0) == name )
			return (int)(i-m_startRow);

	return -1;
}

wxString Library::GetEntryValue( int entry, int field )
{
	return m_csv( entry+m_startRow, field );
}

wxString Library::GetEntryName( int entry )
{
	return GetEntryValue( entry, 0 );
}

bool Library::ApplyEntry( int entry, int varindex, VarTable &tab, wxArrayString &changed )
{
	m_errors.Clear();

	if ( varindex < 0 || varindex >= m_startRow-2 )
	{
		m_errors.Add( wxString::Format("invalid varindex of %d", varindex ) );
		return false;
	}
	
	size_t row = m_startRow + (size_t)entry;
	if ( row >= m_csv.NumRows() || row < m_startRow )
	{
		m_errors.Add( wxString::Format("invalid entry %d (max %d)", entry, (int)(m_csv.NumRows()-m_startRow)) );
		return false;
	}

	for( size_t i=1;i<m_fields.size();i++ )
	{
		Field &f = m_fields[i];

		wxString var;
		if ( varindex < f.Variables.size() )
			var = f.Variables[varindex];

		if ( var.IsEmpty() ) continue; // skip this variable if no name was found

		if ( VarValue *vv = tab.Get( var ) )
		{
			if( VarValue::Parse( vv->Type(), m_csv(row,f.DataIndex), *vv ) )
				changed.Add( var );
			else
				m_errors.Add( "could not parse '" + var + "' to required data type" );
		}
		else
			m_errors.Add( "variable '" + var + "' not found in collection" );		
	}

	return m_errors.Count() == 0;
}


LibraryListView::LibraryListView( LibraryCtrl *parent, int id, const wxPoint &pos,
	const wxSize &size )
	: wxListView( parent, id, pos, size, wxLC_REPORT|wxLC_VIRTUAL|wxLC_SINGLE_SEL )
{
	m_libctrl = parent;
}

wxString LibraryListView::OnGetItemText( long item, long col ) const
{
	return m_libctrl->GetCellValue( item, col );
}
/*
wxListItemAttr *LibraryListView::OnGetItemAttr( long item ) const
{
	static wxListItemAttr even( *wxBLACK, *wxWHITE, *wxNORMAL_FONT ),
		odd( *wxBLACK, wxColour(245,245,245), *wxNORMAL_FONT );

	return (item%2==0) ? &even : &odd;
}
*/

enum { ID_LIST = wxID_HIGHEST+495, ID_FILTER, ID_TARGET, ID_REFRESH };

BEGIN_EVENT_TABLE( LibraryCtrl, wxPanel )
	EVT_LIST_ITEM_SELECTED( ID_LIST, LibraryCtrl::OnSelected )
	EVT_LIST_COL_CLICK( ID_LIST, LibraryCtrl::OnColClick )
	EVT_TEXT( ID_FILTER, LibraryCtrl::OnCommand )
	EVT_BUTTON( ID_REFRESH, LibraryCtrl::OnCommand )
	EVT_CHOICE( ID_TARGET, LibraryCtrl::OnCommand )
END_EVENT_TABLE()


LibraryCtrl::LibraryCtrl( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxPanel( parent, id, pos, size, wxTAB_TRAVERSAL )
{
	m_sendEvents = true;

	m_label = new wxStaticText( this, wxID_ANY, wxT("Search for:") );
	m_filter = new wxTextCtrl( this, ID_FILTER );
	m_target = new wxChoice( this, ID_TARGET );
	m_notify = new wxStaticText( this, wxID_ANY, wxEmptyString );
	m_notify->SetForegroundColour( *wxRED );
	m_list = new LibraryListView( this, ID_LIST );

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

LibraryCtrl::~LibraryCtrl()
{
	// nothing to do
}

bool LibraryCtrl::SetEntrySelection( const wxString &entry )
{
	if ( Library *lib = Library::Find( m_library ) )
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

wxString LibraryCtrl::GetEntrySelection()
{
	long sel = m_list->GetFirstSelected();
	if ( sel >= 0 && sel < m_view.size() ) return m_view[ sel ].name;
	return wxEmptyString;
}

wxString LibraryCtrl::GetCellValue( long item, long col )
{
	if ( Library *lib = Library::Find( m_library ) )
		if ( item < m_view.size() && col < m_fieldMap.size() )
			return lib->GetEntryValue( m_view[item].index, m_fieldMap[col] );

	return wxT("<inval>");
}

void LibraryCtrl::SetLibrary( const wxString &name, const wxString &fields )
{
	m_library = name;
	
	if( Library *lib = Library::Find( m_library ) )
	{
		m_fields.Clear();
		m_fieldMap.clear();	
		
		if ( fields == "*" )
		{
			std::vector<Library::Field> &ff = lib->GetFields();
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

	ReloadLibrary();

}

void LibraryCtrl::ReloadLibrary()
{
	if( Library *lib = Library::Find( m_library ) )
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
		wxLogStatus( "LibraryCtrl: could not find library " + m_library );
}

void LibraryCtrl::UpdateList()
{
	m_sendEvents = false;

	wxString filter = m_filter->GetValue().Lower();
	wxString sel = GetEntrySelection();
	
	m_notify->SetLabel( wxEmptyString );
	m_view.clear();
	if ( m_entries.size() > 0 )
		m_view.reserve( m_entries.size() );

	size_t nmatches = 0;

	if( Library *lib = Library::Find( m_library ) )
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

void LibraryCtrl::OnSelected( wxListEvent &evt )
{
	if ( !m_sendEvents ) return;

	wxCommandEvent issue( wxEVT_LISTBOX, GetId() );
	issue.SetEventObject( this );
	issue.SetInt( evt.GetSelection() );
	ProcessEvent( issue );
	
}

bool LibraryCtrl::viewable_compare::operator() ( const viewable &lhs, const viewable &rhs )
{
	if ( dir && col[lhs.index] < col[rhs.index] ) return true;
	else if ( !dir && col[lhs.index] > col[rhs.index] ) return true;
	else return false;
}

void LibraryCtrl::OnColClick( wxListEvent &evt )
{
	wxBusyCursor wait;

	if( Library *lib = Library::Find( m_library ) )
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

void LibraryCtrl::OnCommand( wxCommandEvent &evt )
{
	if( evt.GetId() == ID_FILTER || evt.GetId() == ID_TARGET )
		UpdateList();
}

void LibraryCtrl::SetLabel( const wxString &text )
{
	m_label->SetLabel( text );
	Layout();
}


bool ScanSolarResourceData( const wxString &db_file )
{
	wxString path = SamApp::GetRuntimePath() + "../solar_resource/";
	wxDir dir( path );
	if( !dir.IsOpened() ) return false;

	wxCSVData csv;
	csv(0,0) = "Name";
	csv(2,0) = "[0]";

	csv(0,1) = "City";
	csv(2,1) = "city";

	csv(0,2) = "State";
	csv(2,2) = "state";

	csv(0,3) = "Country";
	csv(2,3) = "country";

	csv(0,4) = "Latitude";
	csv(1,4) = "deg";
	csv(2,4) = "lat";

	csv(0,5) = "Longitude";
	csv(1,5) = "deg";
	csv(2,5) = "lon";

	csv(0,6) = "Time zone";
	csv(1,6) = "hour";
	csv(2,6) = "tz";

	csv(0,7) = "Elevation";
	csv(1,7) = "m";
	csv(2,7) = "elev";

	csv(0,8) = "Station ID";
	csv(2,8) = "station_id";

	csv(0,9) = "Source";
	csv(2,9) = "solar_data_source";

	csv(0,10) = "File name";
	csv(2,10) = "solar_data_file_name";

	int row = 3;
	wxString file;
	bool has_more = dir.GetFirst( &file, "*.csv", wxDIR_FILES );
	while( has_more )
	{
		// process file
		wxString wf = path + "/" + file;
		
		ssc_data_t pdata = ssc_data_create();
		ssc_data_set_string( pdata, "file_name", (const char*)wf.c_str() );
		ssc_data_set_number( pdata, "header_only", 1 );

		if ( const char *err = ssc_module_exec_simple_nothread( "wfreader", pdata ) )
		{
			wxLogStatus("error scanning '" + wf + "'");
			wxLogStatus("\t%s", err );
		}
		else
		{
			ssc_number_t val;
			const char *str;

			wxFileName ff(wf);
			ff.Normalize();

			csv(row,0) = ff.GetName();

			if ( str = ssc_data_get_string( pdata, "city" ) )
				csv(row,1) = wxString(str);

			if ( str = ssc_data_get_string( pdata, "state" ) )
				csv(row,2) = wxString(str);

			if ( str = ssc_data_get_string( pdata, "country" ) )
				csv(row,3) = wxString(str);
			
			if ( ssc_data_get_number( pdata, "lat", &val ) )
				csv(row,4) = wxString::Format("%g", val);
			
			if ( ssc_data_get_number( pdata, "lon", &val ) )
				csv(row,5) = wxString::Format("%g", val);
			
			if ( ssc_data_get_number( pdata, "tz", &val ) )
				csv(row,6) = wxString::Format("%g", val);
			
			if ( ssc_data_get_number( pdata, "elev", &val ) )
				csv(row,7) = wxString::Format("%g", val);

			if ( str = ssc_data_get_string( pdata, "location" ) )
				csv(row,8) = wxString(str);
			
			if ( str = ssc_data_get_string( pdata, "source" ) )
				csv(row,9) = wxString(str);
					
			csv(row,10) = ff.GetFullPath();

			row++;
		}

		ssc_data_free( pdata );

		has_more = dir.GetNext( &file );
	}

	return csv.WriteFile( db_file );
}

bool ScanWindResourceData( const wxString &db_file )
{
	wxString path = SamApp::GetRuntimePath() + "../wind_resource/";
	wxDir dir(path);
	if (!dir.IsOpened()) return false;

	wxCSVData csv;
	csv(0, 0) = "Name";
	csv(2, 0) = "[0]";

	csv(0, 1) = "City";
	csv(2, 1) = "wind_resource.city";

	csv(0, 2) = "State";
	csv(2, 2) = "wind_resource.state";

	csv(0, 3) = "Country";
	csv(2, 3) = "wind_resource.country";

	csv(0, 4) = "Latitude";
	csv(1, 4) = "deg";
	csv(2, 4) = "wind_resource.lat";

	csv(0, 5) = "Longitude";
	csv(1, 5) = "deg";
	csv(2, 5) = "wind_resource.lon";

	csv(0, 6) = "Location ID";
	csv(2, 6) = "wind_resource.location_id";

	csv(0, 7) = "Elevation";
	csv(1, 7) = "m";
	csv(2, 7) = "wind_resource.elev";

	csv(0, 8) = "Year";
	csv(2, 8) = "wind_resource.year";

	csv(0, 9) = "Description";
	csv(2, 9) = "wind_resource.description";

	csv(0, 10) = "File name";
	csv(2, 10) = "wind_resource.file";

	csv(0, 11) = "Closest Speed Measurement Ht";
	csv(2, 11) = "wind_resource.closest_speed_meas_ht";

	csv(0, 12) = "Closest Direction Measurement Ht";
	csv(2, 12) = "wind_resource.closest_dir_meas_ht";

	int row = 3;
	wxString file;
	bool has_more = dir.GetFirst(&file, "*.srw", wxDIR_FILES);
	while (has_more)
	{
		// process file
		wxString wf = path + "/" + file;

		ssc_data_t pdata = ssc_data_create();
		ssc_data_set_string(pdata, "file_name", (const char*)wf.c_str());
		ssc_data_set_number(pdata, "scan_header_only", 1);
		ssc_data_set_number(pdata, "requested_ht", 80.0);

		if (const char *err = ssc_module_exec_simple_nothread("wind_file_reader", pdata))
		{
			wxLogStatus("error scanning '" + wf + "'");
			wxLogStatus("\t%s", err);
		}
		else
		{
			ssc_number_t val;
			const char *str;

			wxFileName ff(wf);
			ff.Normalize();

			csv(row, 0) = ff.GetName();

			if (str = ssc_data_get_string(pdata, "city"))
				csv(row, 1) = wxString(str);

			if (str = ssc_data_get_string(pdata, "state"))
				csv(row, 2) = wxString(str);

			if (str = ssc_data_get_string(pdata, "country"))
				csv(row, 3) = wxString(str);

			if (ssc_data_get_number(pdata, "lat", &val))
				csv(row, 4) = wxString::Format("%g", val);

			if (ssc_data_get_number(pdata, "lon", &val))
				csv(row, 5) = wxString::Format("%g", val);

			if (str = ssc_data_get_string(pdata, "location_id"))
				csv(row, 6) = wxString(str);

			if (ssc_data_get_number(pdata, "elev", &val))
				csv(row, 7) = wxString::Format("%g", val);

			if (ssc_data_get_number(pdata, "year", &val))
				csv(row, 8) = wxString::Format("%g", val);

			if (str = ssc_data_get_string(pdata, "description"))
				csv(row, 9) = wxString(str);

			csv(row, 10) = ff.GetFullPath();

			if (ssc_data_get_number(pdata, "closest_speed_meas_ht", &val))
				csv(row, 11) = wxString::Format("%g", val);

			if (ssc_data_get_number(pdata, "closest_dir_meas_ht", &val))
				csv(row, 12) = wxString::Format("%g", val);

			row++;
		}

		ssc_data_free(pdata);

		has_more = dir.GetNext(&file);
	}

	return csv.WriteFile( db_file );
}
