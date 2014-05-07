#include <algorithm>

#include <wx/datstrm.h>
#include <wx/wfstream.h>
#include <wx/ffile.h>
#include <wx/tokenzr.h>
#include <wx/log.h>

#include <lk_stdlib.h>
#include <lk_eval.h>

#include "variables.h"



VarTable::VarTable()
{
	/* nothing to do */
}

VarTable::VarTable( const VarTable &rhs )
{
	Copy( rhs );
}

VarTable::~VarTable()
{
	VarTable::clear();
}

VarTable &VarTable::operator=( const VarTable &rhs )
{
	Copy( rhs );
	return *this;
}

void VarTable::Copy( const VarTable &rhs )
{
	if ( this != &rhs )
	{
		VarTable::clear();
		for( const_iterator it = rhs.begin();
			it != rhs.end();
			++it )
			Set( it->first, *(it->second) );
	}
}

void VarTable::clear()
{
	for( iterator it = begin(); it != end(); ++it )
		delete it->second;

	VarTableBase::clear();
}

void VarTable::Delete( const wxString &name )
{
	iterator it = find(name);
	if ( it != end() )
	{
		delete it->second;
		erase( it );
	}
}

void VarTable::Delete( const wxArrayString &names )
{
	for( size_t i=0;i<names.size();i++ )
		Delete( names[i] );
}

wxArrayString VarTable::ListAll( std::vector<VarValue*> *vals )
{
	wxArrayString list;
	for( iterator it = begin(); it != end(); ++it )
	{
		list.Add( it->first );
		if ( vals ) vals->push_back( it->second );
	}
	return list;
}

VarValue *VarTable::Create( const wxString &name, int type )
{
	VarValue *vv = 0;
	iterator it = find( name );
	if ( it == end() )
	{
		vv = new VarValue;
		(*this)[ name ] = vv;
	}
	else
		vv = it->second;

	vv->SetType( type );

	return vv;
}

VarValue *VarTable::Set( const wxString &name, const VarValue &val )
{
	VarValue *vv = 0;
	iterator it = find( name );
	if ( it == end() )
	{
		vv = new VarValue(val);
		(*this)[ name ] = vv;
	}
	else
	{
		it->second->Copy(val);
		vv = it->second;
	}
	return vv;
}

VarValue *VarTable::Get( const wxString &name )
{
	iterator it = find( name );
	if ( it == end() ) return 0;
	else return it->second;
}

bool VarTable::Rename( const wxString &old_name, const wxString &new_name )
{
	iterator it = find( old_name );
	if ( it == end() ) return false;

	if ( find( new_name ) != end() ) return false;

	VarValue *vv = it->second;
	erase( it );
	
	(*this)[ new_name ] = vv;
	return true;
}

void VarTable::Write( wxOutputStream &_O, size_t maxdim )
{
	wxDataOutputStream out(_O);
	out.Write8( 0xf9 );
	out.Write8( 1 );

	if ( maxdim == 0 )
	{
		out.Write32( size() );
		for( iterator it = begin(); it != end(); ++it )
		{
			out.WriteString( it->first );
			it->second->Write( _O );
		}
	}
	else
	{
		wxArrayString names;
		std::vector<VarValue*> list;
		list.reserve( size() );

		for( iterator it = begin(); it != end(); ++ it )
		{
			VarValue &vv = *(it->second);
			if ( vv.Type() == VV_ARRAY 
				&& vv.Length() <= maxdim )
			{
				names.Add( it->first );
				list.push_back( it->second );
			}
			else if ( vv.Type() == VV_MATRIX
				&& vv.Matrix().nrows() <= maxdim
				&& vv.Matrix().ncols() <= maxdim )
			{
				names.Add( it->first );
				list.push_back( it->second );
			}
			else if ( vv.Type() != VV_MATRIX
				&& vv.Type() != VV_ARRAY )
			{
				names.Add( it->first );
				list.push_back( it->second );
			}
		}

		out.Write32( list.size() );
		for( size_t i=0;i<list.size();i++ )
		{
			out.WriteString( names[i] );
			list[i]->Write( _O );
		}
	}

	out.Write8( 0xf9 );
}

bool VarTable::Read( wxInputStream &_I )
{
	clear();
	
	wxDataInputStream in(_I);
	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	bool ok = true;
	size_t n = in.Read32();
	for ( size_t i=0;i<n;i++ )
	{
		wxString name = in.ReadString();
		VarValue *value = new VarValue;
		ok = ok && value->Read( _I );
		
		if ( find(name) == end() ) (*this)[name] = value;
		else delete value;
	}
	
	return in.Read8() == code;
}


VarValue VarValue::Invalid; // declaration

VarValue::VarValue()
{
	m_type = VV_INVALID;
}


VarValue::VarValue( const VarValue &vv )
{
	Copy( vv );
}

VarValue::VarValue( int i )
{
	m_type = VV_NUMBER;
	m_val = (float)i;
}

VarValue::VarValue( float f )
{
	m_type = VV_NUMBER;
	m_val = f;
}

VarValue::VarValue( bool b )
{
	m_type = VV_NUMBER;
	m_val = b ? 1 : 0;
}

VarValue::VarValue( const std::vector<float> &f )
{
	m_type = VV_ARRAY;
	if ( f.size() > 0 ) m_val.assign( &f[0], f.size() );
}

VarValue::VarValue( float *arr, size_t n )
{
	m_type = VV_ARRAY;
	m_val.assign( arr, n );
}

VarValue::VarValue( float *mat, size_t r, size_t c )
{
	m_type = VV_MATRIX;
	m_val.assign( mat, r, c );
}

VarValue::VarValue( const ::matrix_t<float> &m )
{
	m_type = VV_MATRIX;
	m_val = m;
}

VarValue::VarValue( const wxString &s )
{
	m_type = VV_STRING;
	m_str = s;
}

VarValue::VarValue( const VarTable &t )
{
	m_type = VV_TABLE;
	m_tab.Copy( t );
}

VarValue::~VarValue()
{
	// nothing to do 
}

VarValue &VarValue::operator=( const VarValue &rhs )
{
	Copy( rhs );
	return *this;
}


bool VarValue::ValueEqual( VarValue &rhs)
{
	bool equal = false;
	if (m_type == rhs.m_type)
	{
		std::vector<VarValue*> vv_vec1, vv_vec2;
		switch (m_type)
		{
		case VV_INVALID: // this and rhs are both invalid
			equal = true;
			break;
		case VV_NUMBER:
		case VV_ARRAY:
		case VV_MATRIX:
			equal = ((m_val.nrows() == rhs.m_val.nrows()) && (m_val.ncols() == rhs.m_val.ncols()));
			if (equal)
				for (size_t r = 0; r < m_val.nrows(); r++)
					for (size_t c = 0; c < m_val.ncols(); c++)
						if (equal)
							equal = equal && (m_val(r, c) == rhs.m_val(r, c));
						else
							break;
			break;
		case VV_TABLE: // not working correctly 
//			equal = (m_tab.size() == rhs.m_tab.size());
			if (equal)
				for (VarTable::iterator it1 = m_tab.begin(); it1 != m_tab.end(); ++it1)
					if (equal)
					{ 
						VarTable::iterator it2 = find(rhs.m_tab.begin(), rhs.m_tab.end(), *it1);
						if (it2 != rhs.m_tab.end())
							equal = equal && (it1->second->ValueEqual(*(it2->second)));
						else
						{
							equal = false;
							break;
						}
					}
					else
						break;
			break;
		case VV_STRING:
			equal = (m_str == rhs.m_str);
			break;
		}
	}
	return equal;
}


void VarValue::Copy( const VarValue &rhs )
{
	if ( this != &rhs )
	{
		m_type = rhs.m_type;
		m_str = rhs.m_str;
		m_val = rhs.m_val;
		m_tab = rhs.m_tab;
	}
}


void VarValue::Write( wxOutputStream &_O )
{
	wxDataOutputStream out( _O );

	out.Write8( 0xf2 );
	out.Write8( 1 );

	out.Write8( m_type );
	
	switch( m_type )
	{
	case VV_INVALID:
		break; // no data to be written
	case VV_NUMBER:
	case VV_ARRAY:
	case VV_MATRIX:
		out.Write32( m_val.nrows() );
		out.Write32( m_val.ncols() );
		for ( size_t r=0;r<m_val.nrows();r++ )
			for( size_t c=0;c<m_val.ncols();c++ )
				out.WriteFloat( m_val(r,c) );
		break;
	case VV_TABLE:
		m_tab.Write( _O );
		break;
	case VV_STRING:
		out.WriteString( m_str );
		break;
	}

	out.Write8( 0xf2 );
}

bool VarValue::Read( wxInputStream &_I )
{
	wxDataInputStream in( _I );

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	m_type = in.Read8();

	size_t nr, nc;
	switch( m_type )
	{
	case VV_INVALID:
		break;
	case VV_NUMBER:
	case VV_ARRAY:
	case VV_MATRIX:
		nr = in.Read32();
		nc = in.Read32();
		if ( nr*nc < 1 ) return false; // big error
		m_val.resize_fill( nr, nc, 0.0f );
		for ( size_t r=0;r<nr;r++ )
			for( size_t c=0;c<nc;c++ )
				m_val(r,c) = in.ReadFloat();
		break;
	case VV_TABLE:
		m_tab.Read( _I );
		break;
	case VV_STRING:
		m_str = in.ReadString();
		break;
	}

	return in.Read8() == code;
}
	
int VarValue::Type() const { return m_type; }
void VarValue::SetType( int ty ) { m_type = ty; }
void VarValue::Set( int val ) { m_type = VV_NUMBER; m_val = (float)val; }
void VarValue::Set( float val ) { m_type = VV_NUMBER; m_val = val; }
void VarValue::Set( double val ) { m_type = VV_NUMBER; m_val = val; }

void VarValue::Set( const std::vector<int> &ivec )
{
	m_type = VV_ARRAY;
	if ( ivec.size() > 0 ) 
	{
		m_val.resize_fill( ivec.size(), 0 );
		for( size_t i=0;i<ivec.size();i++ )
			m_val.at(i) = (float)ivec[i];
	}
	else
		m_val.clear();
}

void VarValue::Set( const std::vector<float> &fvec )
{
	m_type = VV_ARRAY;
	if( fvec.size() > 0 ) m_val.assign( &fvec[0], fvec.size() );
	else m_val.clear();
}

void VarValue::Set( float *val, size_t n ) { m_type = VV_ARRAY; m_val.assign( val, n ); }
void VarValue::Set( float *mat, size_t r, size_t c ) { m_type = VV_MATRIX; m_val.assign( mat, r, c ); }
void VarValue::Set( const ::matrix_t<float> &mat ) { m_type = VV_MATRIX; m_val = mat; }
void VarValue::Set( const wxString &str ) { m_type = VV_STRING; m_str = str; }
void VarValue::Set( const VarTable &tab ) { m_type = VV_TABLE; m_tab.Copy( tab ); }

int VarValue::Integer()
{
	if ( m_type == VV_NUMBER ) return (int)(float)m_val;
	else return 0;
}

bool VarValue::Boolean()
{
	if ( m_type == VV_NUMBER ) return Integer() != 0;
	else return 0;
}

float VarValue::Value()
{
	if ( m_type == VV_NUMBER) return (float)m_val;
	else return std::numeric_limits<float>::quiet_NaN();
}

size_t VarValue::Length()
{
	if ( m_type == VV_ARRAY ) return m_val.length();
	else return 0;
}

float *VarValue::Array( size_t *n )
{
	if ( m_type == VV_ARRAY )
	{
		if ( n != 0 ) *n = m_val.length();
		return m_val.data();
	}
	else
	{
		if ( n != 0 ) *n = 0;
		return 0;
	}
}

std::vector<float> VarValue::Array()
{
	if ( m_type == VV_ARRAY )
	{
		std::vector<float> vec( m_val.length(), 0.0f );
		for( size_t i=0;i<m_val.length();i++)
			vec[i] = m_val[i];
		return vec;
	}
	else
		return std::vector<float>();
}

std::vector<int> VarValue::IntegerArray()
{
	if ( m_type == VV_ARRAY )
	{
		std::vector<int> vec( m_val.length(), 0 );
		for( size_t i=0;i<m_val.length();i++)
			vec[i] = (int)m_val[i];
		return vec;
	}
	else
		return std::vector<int>();
}

matrix_t<float> &VarValue::Matrix()
{
	return m_val;
}

float *VarValue::Matrix( size_t *nr, size_t *nc )
{
	*nr = m_val.nrows();
	*nc = m_val.ncols();
	return m_val.data();
}

wxString VarValue::String()
{
	return m_str;
}

VarTable &VarValue::Table()
{
	return m_tab;
}

bool VarValue::Read( const lk::vardata_t &val, bool change_type )
{
	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		if ( Type() == VV_NUMBER || change_type ) Set( (float) val.as_number() );
		break;
	case lk::vardata_t::STRING:
		if ( Type() == VV_STRING || change_type ) Set( val.as_string() );
		break;
	case lk::vardata_t::VECTOR:
		{
			size_t dim1 = val.length(), dim2 = 0;
			for (size_t i=0;i<val.length();i++)
			{
				lk::vardata_t *row = val.index(i);
				if (row->type() == lk::vardata_t::VECTOR && row->length() > dim2 )
					dim2 = row->length();
			}

			if (dim2 == 0 && dim1 > 0)
			{
				float *vec = new float[dim1];
				for( size_t i=0;i<dim1;i++)
					vec[i] = (float)val.index(i)->as_number();


				if ( Type() == VV_ARRAY || change_type )
					Set( vec, dim1 );
				else if ( Type() == VV_MATRIX )
					Set( vec, dim1, 1 );

				delete [] vec;
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				if ( Type() == VV_MATRIX || change_type )
				{
					m_type = VV_MATRIX;
					m_val.resize_fill( dim1, dim2, 0.0f );

					for ( size_t i=0;i<dim1;i++)
					{
						for ( size_t j=0;j<dim2;j++ )
						{
							double x = 0;
							if ( val.index(i)->type() == lk::vardata_t::VECTOR
								&& j < val.index(i)->length() )
								x = (float)val.index(i)->index(j)->as_number();

							m_val.at(i,j) = x;
						}
					}
				}
			}
		}
		break;
	case lk::vardata_t::HASH:		
		{
			if ( Type() == VV_TABLE || change_type )
			{
				Set( VarTable() ); // switch to an empty table
				VarValue vv_inval;	
				lk::varhash_t &hash = *val.hash();
				for ( lk::varhash_t::iterator it = hash.begin();
					it != hash.end();
					++it )
				{
					VarValue *item = Table().Set( (*it).first, vv_inval );
					item->Read( *(*it).second );
				}
			}
		}
		break;
	}

	return true;
}

bool VarValue::Write( lk::vardata_t &val )
{
	switch( Type() )
	{
	case VV_NUMBER:
		val.assign( (double) Value() );
		break;
	case VV_STRING:
		val.assign( String() );
		break;
	case VV_ARRAY:
		{
			size_t n = 0;
			float *p = Array( &n );
			val.empty_vector();
			if ( n > 0 )
			{
				val.vec()->reserve( (size_t) n  );
				for (int i=0;i<n;i++)
					val.vec_append( (double)p[i] );
			}
		}
		break;
	case VV_MATRIX:
		{
			::matrix_t<float> &mat = Matrix();
			val.empty_vector();
			val.vec()->reserve( mat.nrows() );
			for (int i=0;i<mat.nrows();i++)
			{
				val.vec()->push_back( lk::vardata_t() );
				val.vec()->at(i).empty_vector();
				val.vec()->at(i).vec()->reserve( mat.ncols() );
				for (int j=0;j<mat.ncols();j++)
					val.vec()->at(i).vec_append( mat.at(i,j) );
			}
		}
		break;
	case VV_TABLE:
		{
			VarTable &tab = Table();
			for (VarTableBase::iterator it = tab.begin();
				it != tab.end();
				++ it )
			{
				lk::vardata_t &xvd = val.hash_item( it->first );
				it->second->Write( xvd );
			}
		}
		break;
	}

	return m_type != VV_INVALID;
}

bool VarValue::Parse( int type, const wxString &str, VarValue &value )
{
	switch(type)
	{
	case VV_STRING:
		{
			value.m_type = VV_STRING;
			value.m_str = str;
			return true;
		}
	case VV_NUMBER:
		{
			value.m_type = VV_NUMBER;
			value.m_val = wxAtof( str );
			return true;		
		}
	case VV_ARRAY:
		{
			wxArrayString tokens = wxStringTokenize(str," ,;|", wxTOKEN_STRTOK );
			value.m_type = VV_ARRAY;
			value.m_val.resize_fill( tokens.size(), 0.0 );
			for (size_t i=0; i<tokens.size(); i++)
					value.m_val[i] = wxAtof( tokens[i] );

			return true;
		}
	case VV_MATRIX:
		{
			wxArrayString rows = wxStringTokenize(str," []", wxTOKEN_STRTOK );
			if (rows.size() < 1) return false;
			wxArrayString cols = wxStringTokenize(rows[0], " ,;|", wxTOKEN_STRTOK );
			if (cols.size() < 1) return false;

			size_t nrows = rows.size();
			size_t ncols = cols.size();

			value.m_type = VV_MATRIX;
			value.m_val.resize_fill( nrows, ncols, 0.0 );
			
			for (size_t r=0; r < nrows; r++)
			{
				cols = wxStringTokenize(rows[r], " ,;|");
				for (size_t c=0; c<cols.size() && c<ncols; c++)
					value.m_val(r,c) = wxAtof( cols[c] );
			}
			return true;
		}
	case VV_TABLE:
		{
			value.m_type = VV_TABLE;
			VarTable vt;
			wxArrayString hash = wxStringTokenize(str, "|", wxTOKEN_STRTOK);
			if (hash.size() < 1) return false;
			for (size_t i = 0; i < hash.size(); i++)
			{
				wxArrayString name_value = wxStringTokenize(hash[i], "=", wxTOKEN_STRTOK);
				if (name_value.Count() != 2) return false;
				wxArrayString name_type = wxStringTokenize(name_value[0], ":", wxTOKEN_STRTOK);
				if (name_type.Count() != 2) return false;
				VarValue vv;
				if (!Parse(wxAtoi(name_type[1]), name_value[1], vv)) return false;
				if (!vt.Set(name_type[0], vv)) return false;
			}
			if (vt.size() != hash.size()) return false;
			value.m_tab = vt;
		return true;
		}
	}
	
	return false;
}

wxString VarValue::AsString( wxChar arrsep, wxChar tabsep )
{
	switch( m_type )
	{
	case VV_INVALID: return "<invalid>";
	case VV_STRING: return m_str;
	case VV_NUMBER: return wxString::Format("%g", (float)m_val );
	case VV_ARRAY:
	{
		wxString buf="";
		for( size_t i=0;i<m_val.length();i++ )
		{
			buf += wxString::Format("%g", (float)m_val[i]  );
			if ( i < m_val.length()-1 ) buf += arrsep;
		}
		return buf;
	}
	case VV_MATRIX:
	{
		wxString buf="";
		for( size_t r=0;r<m_val.nrows();r++ )
		{
			buf += '[';
			for( size_t c=0;c<m_val.ncols();c++ )
			{
				buf += wxString::Format("%g", (float)m_val.at(r,c)  );
				if ( c < m_val.ncols()-1 ) buf += arrsep;
			}
			buf += ']';
		}
		return buf;
	}
	case VV_TABLE:
	{
		wxString buf = "";
		size_t i = 0;
		for (VarTable::iterator it = m_tab.begin(); it != m_tab.end(); ++it)
		{
			buf += (it->first) + ":" + wxString::Format("%d",it->second->Type()) + "=" + it->second->AsString();
			if ( ++i < (m_tab.size())) buf += tabsep; 
		}

		return buf;
	}
	}

	return "<no value found>";
}

VarInfo::VarInfo() 
{
	Type = VV_INVALID;
	Flags = VF_NONE;
}

VarInfo::VarInfo( const VarInfo &copy )
{
	Type = copy.Type;
	Label = copy.Label;
	Units = copy.Units;
	Group = copy.Group;
	IndexLabels = copy.IndexLabels;
	Flags = copy.Flags;
	DefaultValue.Copy( copy.DefaultValue );
}

void VarInfo::Write( wxOutputStream &os )
{
	wxDataOutputStream out(os);
	out.Write8( 0xe1 );
//	out.Write8(2);
	out.Write8(3); // change to version 3 after wxString "UIObject" field added

	out.Write32( Type );
	out.WriteString( Label );
	out.WriteString( Units );
	out.WriteString( Group );
	out.WriteString( wxJoin(IndexLabels, '|') );
	out.Write32( Flags );
	DefaultValue.Write( os );
	out.WriteString(UIObject);

	out.Write8( 0xe1 );
}

bool VarInfo::Read( wxInputStream &is )
{
	wxDataInputStream in(is);
	wxUint8 code = in.Read8();
	int ver = in.Read8(); // ver

	if ( ver < 2 ) in.ReadString(); // formerly, name field

	Type = in.Read32();
	Label = in.ReadString();
	Units = in.ReadString();
	Group = in.ReadString();
	IndexLabels = wxSplit( in.ReadString(), '|' );
	Flags = in.Read32();
	bool valok = DefaultValue.Read( is );
	if (ver < 3) 
		UIObject = VUIOBJ_NONE; // wxUIObject associated with variable
	else
		UIObject = in.ReadString();
	wxUint8 lastcode = in.Read8();
	return  lastcode == code && valok;
}

VarDatabase::VarDatabase()
{
}

VarDatabase::~VarDatabase()
{
	clear();
}

bool VarDatabase::LoadFile( const wxString &file, const wxString &page )
{
	wxFFileInputStream ff( file );
	if ( !ff.IsOk() ) return false;
	return Read( ff, page );
}

void VarDatabase::Write( wxOutputStream &os )
{
	wxDataOutputStream out(os);
	out.Write8( 0xf8 );
	out.Write8( 1 );
	out.Write32( size() );
	for( VarInfoHash::iterator it = begin();
		it != end();
		++it )
	{
		out.WriteString( it->first );
		it->second->Write( os );
	}
	out.Write8( 0xf8 );
}

bool VarDatabase::Read( wxInputStream &is, const wxString &page )
{
	wxDataInputStream in(is);
	wxUint8 code = in.Read8();
	in.Read8();
	size_t n = in.Read32();
	bool ok = true;
	wxArrayString list;
	for( size_t i=0;i<n;i++ )
	{
		wxString name = in.ReadString();
		VarInfo *vv = 0;

		VarInfoHash::iterator it = find( name );
		if ( it != end() ) 
			vv = it->second;
		else 
			vv = new VarInfo;

		ok = ok && vv->Read( is );
		
		(*this)[name] = vv;
		if ( !page.IsEmpty() ) list.Add( name );
	}
	if ( !ok ) return false;

	return in.Read8() == code;
}


VarInfo *VarDatabase::Create( const wxString &name, int type,
	const wxString &label, const wxString &units,
	const wxString &group, const wxString &indexlabels, 
	unsigned long flags, const VarValue &defval, const wxString &uiobject )
{
	VarInfo *vv = new VarInfo;
	vv->Type = type;
	vv->Label = label;
	vv->Units = units;
	vv->Group = group;
	vv->IndexLabels = wxStringTokenize( indexlabels, "," );
	vv->Flags = flags;
	vv->DefaultValue = defval;
	vv->UIObject = uiobject;

	Add( name, vv );

	return vv;
}

bool VarDatabase::Delete( const wxString &name )
{
	VarInfoHash::iterator it = find( name );
	if ( it != end() )
	{
		delete it->second;
		erase( it );
		return true;
	}
	else return false;
}

bool VarDatabase::Rename( const wxString &old_name, const wxString &new_name )
{
	VarInfoHash::iterator it = find( old_name );
	if ( it == end() ) return false;

	if ( find( new_name ) != end() ) return false;

	VarInfo *vv = it->second;
	erase( it );
	
	(*this)[ new_name ] = vv;
	return true;
}

void VarDatabase::clear()
{
	// delete the variable info
	for ( VarInfoHash::iterator it = begin();
		it != end();
		++it )
		delete it->second;

	VarInfoHash::clear();
}




VarInfoLookup::VarInfoLookup()
{
	/* nothing to do */
}

VarInfoLookup::~VarInfoLookup()
{
	/* nothing to do */
}

bool VarInfoLookup::Add( const wxString &name, VarInfo *vv )
{
	if ( vv == 0 )
		return false;

	VarInfoHash::iterator it = find( name );
	if ( it == end() )
	{
		(*this)[name] = vv;
		return true;
	}
	else
		return false;

}

void VarInfoLookup::Add( VarInfoLookup *vil )
{
	for( VarInfoLookup::iterator it = vil->begin();
		it != vil->end();
		++it )
		Add( it->first, it->second );
}

wxArrayString VarInfoLookup::ListAll()
{
	wxArrayString list;
	for( VarInfoHash::iterator it = begin(); it != end(); ++it )
		list.Add( it->first );
	return list;
}

int VarInfoLookup::Type( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Type;
	else return VV_INVALID;
}

wxString VarInfoLookup::Label( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Label;
	else return "<not found>";
}

wxString VarInfoLookup::Group( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Group;
	else return wxEmptyString;
}

wxString VarInfoLookup::Units( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Units;
	else return wxEmptyString;
}

wxArrayString VarInfoLookup::IndexLabels( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->IndexLabels;
	else return wxArrayString();
}

unsigned long VarInfoLookup::Flags( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Flags;
	else return 0;
}

VarValue &VarInfoLookup::DefaultValue( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->DefaultValue;
	else return VarValue::Invalid;
}

VarInfo *VarInfoLookup::Lookup( const wxString &name )
{
	VarInfoHash::iterator it = find(name);
	if ( it == end() ) return 0;
	else return it->second;
}



VarTableScriptInterpreter::VarTableScriptInterpreter( lk::node_t *tree, lk::env_t *env, VarTable *vt )
	: lk::eval( tree, env ), m_vars( vt )
{
}

VarTableScriptInterpreter::~VarTableScriptInterpreter( ) { /* nothing to do */ }

bool VarTableScriptInterpreter::special_set( const lk_string &name, lk::vardata_t &val )
{
	bool ok = false;
	if ( VarValue *vv = m_vars->Get( name ) )
		ok = vv->Read( val );

//	wxLogStatus("vtsi->special_set( " + name + " ) " + wxString( ok?"ok":"fail") );
	return ok;
}

bool VarTableScriptInterpreter::special_get( const lk_string &name, lk::vardata_t &val )
{
	bool ok = false;
	if ( VarValue *vv = m_vars->Get( name ) )
		ok = vv->Write( val );
	
//	wxLogStatus("vtsi->special_get( " + name + " ) " + wxString( ok?"ok":"fail") );
	return ok;
}

