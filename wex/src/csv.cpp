
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/sstream.h>

#include "wex/csv.h"

wxCSVData::wxCSVData()
{
	m_nrows = m_ncols = 0;
	m_invalidated = false;
	m_errorLine = 0;
	m_sep = ',';
}

wxCSVData::wxCSVData( const wxCSVData &copy )
{
	Copy( copy );
}

wxCSVData::~wxCSVData()
{
	 // nothing to do
}

void wxCSVData::Copy( const wxCSVData &copy )
{
	if ( this != &copy )
	{
		m_invalidated = copy.m_invalidated;
		m_nrows = copy.m_nrows;
		m_ncols = copy.m_ncols;
		m_cells = copy.m_cells;
		m_sep = copy.m_sep;
		m_errorLine = 0;
	}
}

wxCSVData &wxCSVData::operator=( const wxCSVData &copy )
{
	Copy( copy );
	return *this;
}

void wxCSVData::Set( size_t r, size_t c, const wxString &val )
{
	// rows, cols remain valid
	if ( r+1 > m_nrows ) m_nrows = r+1;
	if ( c+1 > m_ncols ) m_ncols = c+1;
	m_cells[ Encode(r,c) ] = val;
}

wxString &wxCSVData::operator()(size_t r, size_t c)
{
	// rows, cols remain valid
	if ( r+1 > m_nrows ) m_nrows = r+1;
	if ( c+1 > m_ncols ) m_ncols = c+1;
	return m_cells[ Encode(r,c) ];
}

const wxString &wxCSVData::Get( size_t r, size_t c ) const
{
	cell_hash::const_iterator it = m_cells.find(Encode(r,c));
	if ( it != m_cells.end() ) return it->second;
	else return m_emptyStr;
}

const wxString &wxCSVData::operator()(size_t r, size_t c) const
{
	return Get(r,c);
}

size_t wxCSVData::NumCells() const
{
	return m_cells.size();
}

size_t wxCSVData::NumRows()
{
	if ( m_invalidated ) RecalculateDimensions();
	return m_nrows;
}

size_t wxCSVData::NumCols()
{
	if ( m_invalidated ) RecalculateDimensions();
	return m_ncols;
}


void wxCSVData::Clear()
{
	m_cells.clear();
	m_nrows = m_ncols = 0;
	m_invalidated = false;
}

void wxCSVData::Clear( size_t r, size_t c )
{
	cell_hash::iterator it = m_cells.find( Encode(r,c) );
	if( it != m_cells.end() )
	{
		m_cells.erase( it );
		m_invalidated = true;
	}
}
	
bool wxCSVData::Read( wxInputStream &in )
{
/*
TEST CSV FILE: (r/w)
"Cell 0,0","a""oran,ge",'e"e'

"Cell 1,0",b'prime,cellb,"Row 1,Col 3"
dd,"c""dprime","Cell 2,2",,,,bacd
"ans,b,z,d","x,""yz""","xx',' - "",""",,"a,,,b"

*/
	Clear(); // erase the csv data first

	wxTextInputStream txt(in);
	
	m_errorLine = 0;
	size_t max_row = 0;
	size_t max_col = 0;

	while ( !txt.GetInputStream().Eof() )
	{
		wxString buf = txt.ReadLine();
		size_t cur_col = 0;
		wxString::iterator it = buf.begin();
		while( it < buf.end() && *it != wxUniChar('\n') && *it != wxUniChar('\r') )
		{
		//	csvcell *cell = new csvcell;
		//	cell->r = max_row;
		//	cell->c = cur_col;
		//	cells.push_back( cell );

			wxString text;

			if( *it == '"' )
			{
				++it; // skip the initial quote

				// read a quoted cell
				while ( it < buf.end() )
				{
					if ( *it == '"' )
					{
						if ( it + 1 < buf.end() )
						{
							wxUniChar next = *(++it);
							--it;
							if ( next == '"')
							{
								text += '"';
								++it; 
								if ( it < buf.end() ) ++it;
							}
							else
							{
								++it; // skip current quote
								break; // end of quoted cell
							}
						}
						else
						{
							++it;
							break;
						}
					}
					else
					{
						text += *(it++);
					}
				}
			}
			else
			{
				// read a normal cell
				wxUniChar prev = 0;
				while ( it < buf.end() && *it != m_sep && *it != '\n' && *it != '\r' )
				{
					if ( *it == '"' && prev == '"' )
					{
						text += '"';
						prev = *it;
						++it;
					}
					else
					{
						text += *it;
						prev = *it;
						++it;
					}
				}
			}
			
			// store the cell
			Set( max_row, cur_col, text );

			if ( it >= buf.end() || *it == m_sep || *it == '\n' || *it == '\r' )
			{
				if ( it < buf.end() ) ++it; // skip over the comma
				cur_col++;
				if ( cur_col > max_col )
					max_col = cur_col;
			}
			else
			{
				// flag error on the first row with a formatting problem
				if (m_errorLine == 0)
					m_errorLine = -( (int)max_row+1 );
			}


		}

		max_row++;
	}

	return (0 == m_errorLine);

}

void wxCSVData::Write( wxOutputStream &out )
{
	wxTextOutputStream txt( out, wxEOL_UNIX );
	if ( m_invalidated ) RecalculateDimensions();

	for( size_t r=0;r<m_nrows; r++ )
	{
		for( size_t c=0;c<m_ncols;c++ )
		{
			wxString cell = Get(r,c);
			if ( cell.Find( wxUniChar(m_sep) ) >= 0 )
			{
				cell.Replace("\"", "\"\"");
				txt << '"' << cell << '"';
			}
			else txt << cell;

			if ( c < m_ncols-1 ) txt << ((wxChar)m_sep);
		}

		txt << endl;
	}
}

bool wxCSVData::ReadFile( const wxString &file )
{
	wxFFileInputStream in( file );
	if ( !in.IsOk() ) return false;
	return Read( in );
}

bool wxCSVData::WriteFile( const wxString &file )
{
	wxFFileOutputStream out( file );
	if ( !out.IsOk() ) return false;
	Write( out );
	return true;
}

bool wxCSVData::ReadString( const wxString &data )
{
	wxStringInputStream ss(data);
	return Read(ss);
}

wxString wxCSVData::WriteString()
{
	wxString buf;
	wxStringOutputStream ss( &buf );
	Write( ss );
	return buf;
}
	
wxUint64 wxCSVData::Encode( size_t r, size_t c ) const
{
	return ( ((wxUint64)r) << 32 ) | ((wxUint64)c);
}

void wxCSVData::Decode( wxUint64 idx, size_t *r, size_t *c ) const
{
	*r = (size_t)(idx >> 32);
	*c = (size_t)(idx & 0x00000000ffffffff);
}

void wxCSVData::RecalculateDimensions()
{
	m_nrows = m_ncols = 0;
	for( cell_hash::const_iterator it = m_cells.begin();
		it != m_cells.end();
		++it )
	{
		size_t r, c;
		Decode( it->first, &r, &c );
		if ( r+1 > m_nrows ) m_nrows = r+1;
		if ( c+1 > m_ncols ) m_ncols = c+1;
	}
	
	m_invalidated = false;
}
