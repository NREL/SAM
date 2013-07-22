#include <wx/datstrm.h>

#include "variable.h"

Variables::Variables()
{
	m_data = ::ssc_data_create();
	m_owned = true;
}

Variables::Variables( ssc_data_t src )
{
	m_data = src;
	m_owned = false;
}

Variables::~Variables()
{
	if ( m_owned ) ::ssc_data_free( m_data );
}

Object *Variables::Duplicate()
{
	Variables *v = new Variables;
	v->Copy( this );
	return v;
}

bool Variables::Copy( Object *obj )
{
	if ( Variables *v = dynamic_cast<Variables*>(obj) )
	{
		Copy( *v, true );
		return true;
	}
	else return false;
}

wxString Variables::GetTypeName()
{
	return "sam.variables";
}

Variables &Variables::operator=( const Variables &rhs )
{
	if ( m_owned ) ::ssc_data_free( m_data );
	m_data = rhs.Data();
	m_owned = false;
	return *this;
}

bool Variables::IsAssigned( const wxString &name ) const
{
	return Type(name) != INVALID;
}

int Variables::Type( const wxString &name ) const
{
	return ::ssc_data_query( m_data, name.ToUTF8() );
}

void Variables::Clear()
{
	::ssc_data_clear( m_data );
}

void Variables::Clear( const wxString &name )
{
	::ssc_data_unassign( m_data, name.ToUTF8() );
}

void Variables::Clear( const wxArrayString &names )
{
	for ( size_t i=0;i<names.Count(); i++ )
		Clear( names[i] );
}

void Variables::Copy( const Variables &rhs, bool erase_first )
{
	if ( erase_first ) Clear();

	wxArrayString list = rhs.List();
	for ( size_t i=0;i<list.Count(); i++ )
	{
		std::string name( list[i].ToUTF8() );
		int type = ssc_data_query( rhs.Data(), name.c_str() );
		switch( type )
		{
		case SSC_NUMBER:
			{
				ssc_number_t val;
				::ssc_data_get_number( rhs.Data(), name.c_str(), &val );
				::ssc_data_set_number( m_data, name.c_str(), val );
			}
			break;
		case SSC_ARRAY:
			{
				int len;
				ssc_number_t *p = ::ssc_data_get_array( rhs.Data(), name.c_str(), &len );
				if ( p != 0 ) ::ssc_data_set_array( m_data, name.c_str(), p, len );
			}
			break;
		case SSC_MATRIX:
			{
				int rows, cols;
				ssc_number_t *p = ::ssc_data_get_matrix( rhs.Data(), name.c_str(), &rows, &cols );
				if ( p != 0 ) ::ssc_data_set_matrix( m_data, name.c_str(), p, rows, cols );
			}
			break;
		case SSC_STRING:
			{
				const char *s = ::ssc_data_get_string( rhs.Data(), name.c_str() );
				if ( s != 0 ) ::ssc_data_set_string( m_data, name.c_str(), s );
			}
			break;
		case SSC_TABLE:
			{
				ssc_data_t table = ::ssc_data_get_table( rhs.Data(), name.c_str() );
				if ( table != 0 ) ::ssc_data_set_table( m_data, name.c_str(), table );
			}
			break;

		case SSC_INVALID:
		default:
			break;
		}
	}

}

wxArrayString Variables::List() const
{
	wxArrayString names;
	const char *utf8 = ::ssc_data_first( m_data );
	while ( utf8 != 0 )
	{
		names.Add( wxString::FromUTF8( utf8 ) );
		utf8 = ::ssc_data_next( m_data );
	}
	return names;
}

void Variables::Set( const wxString &name, int val )
{
	::ssc_data_set_number( m_data, name.ToUTF8(), (ssc_number_t) val );
}

void Variables::Set( const wxString &name, float val )
{
	::ssc_data_set_number( m_data, name.ToUTF8(), (ssc_number_t) val );
}

	
void Variables::Set( const wxString &name, float *vals, size_t n )
{
	if ( n < 1 ) return;
	
	ssc_number_t *p = new ssc_number_t[n];
	for (size_t i=0;i<n;i++)
		p[i] = (ssc_number_t)vals[i];

	::ssc_data_set_array( m_data, name.ToUTF8(), p, n );
	delete [] p;
}

void Variables::Set( const wxString &name, const std::vector<float> &vals )
{
	if ( vals.size() < 1 ) return;

	ssc_number_t *p = new ssc_number_t[vals.size()];
	for (size_t i=0;i<vals.size();i++)
		p[i] = (ssc_number_t)vals[i];

	::ssc_data_set_array( m_data, name.ToUTF8(), p, vals.size() );
	delete [] p;
}

	
void Variables::Set( const wxString &name, float **mat, size_t rows, size_t cols )
{
	if ( rows * cols < 1 ) return;

	ssc_number_t *p = new ssc_number_t[ rows * cols ];
	for ( size_t r=0;r<rows; r++ )
		for ( size_t c=0;c<cols;c++ )
			p[ r*cols+c ] = mat[r][c];

	::ssc_data_set_matrix( m_data, name.ToUTF8(), p, rows, cols );
	delete [] p;
}

void Variables::Set( const wxString &name, float *mat, size_t rows, size_t cols )
{
	size_t n = rows * cols;
	if ( n < 1 ) return;

	ssc_number_t *p = new ssc_number_t[ n ];
	for ( size_t i=0;i<n;i++ )
		p[i] = (ssc_number_t)mat[i];

	::ssc_data_set_matrix( m_data, name.ToUTF8(), p, rows, cols );
	delete [] p;
}

void Variables::Set( const wxString &name, const std::vector< std::vector<float> > &mat )
{
	size_t rows = mat.size();
	if ( rows < 1 ) return;
	size_t cols = mat[0].size();
	
	if ( rows * cols < 1 ) return;

	for ( size_t i=1;i<mat.size();i++ )
		if ( mat[i].size() != cols ) return;


	ssc_number_t *p = new ssc_number_t[ rows * cols ];
	for ( size_t r=0;r<rows; r++ )
		for ( size_t c=0;c<cols;c++ )
			p[ r*cols+c ] = mat[r][c];

	::ssc_data_set_matrix( m_data, name.ToUTF8(), p, rows, cols );
	delete [] p;
}


void Variables::Set( const wxString &name, const wxString &val )
{
	::ssc_data_set_string( m_data, name.ToUTF8(), val.ToUTF8() );
}

void Variables::Set( const wxString &name, const Variables &tab )
{
	::ssc_data_set_table( m_data, name.ToUTF8(), tab.Data() );
}


int Variables::Integer( const wxString &name )
{
	ssc_number_t ival = 0.0f;
	::ssc_data_get_number( m_data, name.ToUTF8(), &ival );
	return (int) ival;
}

float Variables::Value( const wxString &name )
{
	ssc_number_t fval = 0.0f;
	::ssc_data_get_number( m_data, name.ToUTF8(), &fval );
	return (float) fval;
}

std::vector<float> Variables::Array( const wxString &name )
{
	std::vector<float> vec;
	int len;
	ssc_number_t *p = ::ssc_data_get_array( m_data, name.ToUTF8(), &len );
	if ( p && len > 0 )
	{
		vec.resize( len );
		for ( int i=0;i<len;i++ )
			vec[i] = (float)p[i];
	}
	return vec;
}

std::vector< std::vector<float> > Variables::Matrix( const wxString &name )
{
	std::vector< std::vector<float> > mat;
	int rows, cols;
	ssc_number_t *p = ::ssc_data_get_matrix( m_data, name.ToUTF8(), &rows, &cols );
	if ( p && rows*cols > 0 )
	{
		mat.resize( rows );
		for ( int r=0;r<rows; r++ )
		{
			mat[r].resize( cols );
			for ( int c=0;c<cols; c++ )
				mat[r][c] = (float) p[ r*cols+c ];
		}
	}
	return mat;
}

wxString Variables::String( const wxString &name )
{
	const char *s = ::ssc_data_get_string( m_data, name.ToUTF8() );
	if ( s ) return wxString::FromUTF8( s );
	else return wxEmptyString;
}

Variables Variables::Table( const wxString &name )
{
	ssc_data_t tab = ::ssc_data_get_table( m_data, name.ToUTF8() );
	if ( tab != 0 ) return Variables( tab ); // creates an non-owned instance as a reference
	else return Variables(); // return empty
}


void Variables::Write( wxOutputStream &out )
{
	WriteTable( out, m_data );
}

bool Variables::Read( wxInputStream &in )
{
	Clear();
	return ReadTable( in, m_data );
}

void Variables::WriteTable( wxOutputStream &_o, ssc_data_t data )
{
	wxDataOutputStream out(_o);
	out.Write8( 0x9a );
	out.Write8( 1 );

	const char *name = ::ssc_data_first( data );
	
	while( name != 0 )
	{
		int type = ::ssc_data_query( data, name );
		if( type == SSC_INVALID ) continue;

		out.Write8( 0x1b ); // identifier
		out.Write8( (wxUint8)type );
		out.WriteString( wxString::FromUTF8( name ) );

		switch( type )
		{
		case SSC_NUMBER:
			{
				ssc_number_t val;
				::ssc_data_get_number( data, name, &val );
				out.WriteFloat( (float) val );
			}
			break;
		case SSC_ARRAY:
			{
				int len = 0;
				ssc_number_t *p = ::ssc_data_get_array( data, name, &len );
				
				out.Write32( len );
				for ( int i=0;i<len;i++ )
					out.WriteFloat( (float) p[i] );
			}
			break;
		case SSC_MATRIX:
			{
				int rows = 0, cols = 0;
				ssc_number_t *p = ::ssc_data_get_matrix( data, name, &rows, &cols );
				out.Write32( rows );
				out.Write32( cols );
				int n = rows*cols;
				for ( int i=0;i<n;i++ )
					out.WriteFloat( (float) p[i] );
			}
			break;
		case SSC_STRING:
			{
				const char *s = ::ssc_data_get_string( data, name );
				out.WriteString( wxString::FromUTF8(s) );
			}
			break;
		case SSC_TABLE:
			{
				ssc_data_t t = ::ssc_data_get_table( data, name );
				WriteTable( _o, t );
			}
			break;
		}

		name = ::ssc_data_next( data );
	}

	out.Write8( 0x9a );
}

bool Variables::ReadTable( wxInputStream &_i, ssc_data_t data )
{
	wxDataInputStream in(_i);
	
	wxUint8 start_code = in.Read8( );
	in.Read8(); // version

	::ssc_data_clear( data );

	wxUint8 code = in.Read8();
	while( code == 0x1b )
	{
		wxUint8 type = in.Read8();
		wxString name = in.ReadString();

		switch( type )
		{
		case SSC_NUMBER:
			::ssc_data_set_number( data, name.ToUTF8(), (ssc_number_t) in.ReadFloat() );
			break;
		case SSC_ARRAY:
			{
				int len = in.Read32();
				ssc_number_t *p = new ssc_number_t[len];
				for ( int i=0;i<len;i++ )
					p[i] = (ssc_number_t)in.ReadFloat();

				::ssc_data_set_array( data, name.ToUTF8(), p, len );
				delete [] p;
			}
			break;
		case SSC_MATRIX:
			{
				int rows = in.Read32();
				int cols = in.Read32();
				int n = rows*cols;
				ssc_number_t *p = new ssc_number_t[n];
				for ( int i=0;i<n;i++ )
					p[i] = (ssc_number_t)in.ReadFloat();
				::ssc_data_set_matrix( data, name.ToUTF8(), p, rows, cols );
				delete [] p;
			}
			break;
		case SSC_STRING:
			{
				wxString s = in.ReadString();
				::ssc_data_set_string( data, name.ToUTF8(), s.ToUTF8() );
			}
			break;
		case SSC_TABLE:
			{
				ssc_data_t tab = ::ssc_data_create();
				ReadTable( _i, tab );
				::ssc_data_set_table( data, name.ToUTF8(), tab ); // does a deep copy of tab, so free it next
				::ssc_data_free( tab );
			}
			break;
		}

		code = in.Read8();
	}

	return start_code == code;
}
