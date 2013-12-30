#include <wx/filename.h>

#include "library.h"
#include "object.h"

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
	m_dataFile = file;

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
