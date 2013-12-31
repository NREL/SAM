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
	for( size_t i=1;i<ncol;i++)
	{
		Field f;
		f.Name = m_csv( 0, i );
		f.Units = m_csv( 1, i );
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

int Library::FindEntry( const wxString &name )
{
	for( size_t i=m_startRow;i<m_csv.NumRows();i++ )
		if ( m_csv(i,0) == name )
			return (int)(i-m_startRow);

	return -1;
}

wxString Library::GetEntryValue( int entry, int field )
{
	return m_csv( entry+m_startRow, field+1 );
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

	for( size_t i=0;i<m_fields.size();i++ )
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


bool ScanSolarResourceData()
{
	wxString path = SamApp::GetRuntimePath() + "../solar/";
	wxDir dir( path );
	if( !dir.IsOpened() ) return false;

	wxCSVData csv;
	csv(0,0) = "Name";
	csv(2,0) = "[0]";

	csv(0,1) = "City";
	csv(2,1) = "loc.city";

	csv(0,2) = "State";
	csv(2,2) = "loc.state";

	csv(0,3) = "Country";
	csv(2,3) = "loc.country";

	csv(0,4) = "Latitude";
	csv(1,4) = "deg";
	csv(2,4) = "loc.lat";

	csv(0,5) = "Longitude";
	csv(1,5) = "deg";
	csv(2,5) = "loc.lon";

	csv(0,6) = "Time zone";
	csv(1,6) = "hour";
	csv(2,6) = "loc.tz";

	csv(0,7) = "Elevation";
	csv(1,7) = "m";
	csv(2,7) = "loc.elev";

	csv(0,8) = "Station ID";
	csv(2,8) = "loc.id";

	csv(0,9) = "File name";
	csv(2,9) = "loc.file";

	int row = 3;
	wxString file;
	bool has_more = dir.GetFirst( &file, "*.csv", wxDIR_FILES );
	while( has_more )
	{
		// process file
		wxString wf = path + "/" + file;
		
		ssc_data_t pdata = ssc_data_create();
		ssc_data_set_string( pdata, "file_name", (const char*)wf.c_str() );
		ssc_data_set_number( pdata, "scan_header_only", 1 );

		if ( const char *err = ssc_module_exec_simple_nothread( "wfcsvread", pdata ) )
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
			
			csv(row,9) = ff.GetFullPath();

			row++;
		}

		ssc_data_free( pdata );

		has_more = dir.GetNext( &file );
	}
	
	csv.WriteFile( wxGetUserHome() + "/SolarResourceData.csv" );

	Library *lib = new Library;	
	if ( lib->Read( csv, "SolarResourceData" ) )
	{
		gs_libs.m_libs.push_back( lib );
		return true;
	}
	else
	{
		delete lib;
		return false;
	}
}