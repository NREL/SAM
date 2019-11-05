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

#include <cmath>
#include <numeric>
#include <algorithm>

#include <wx/datstrm.h>
#include <wx/wfstream.h>
#include <wx/ffile.h>
#include <wx/tokenzr.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/filename.h>
#include <wex/exttextstream.h>
#include <lk/stdlib.h>
#include <lk/eval.h>

#include "variables.h"

// can place in utility
// reference http://c-faq.com/fp/fpequal.html
#define Abs(x)    ((x) < 0 ? -(x) : (x))
#define Max(a, b) ((a) > (b) ? (a) : (b))
#define TOLERANCE (1.0e-6)

double RelDif(double a, double b)
{
	double c = Abs(a);
	double d = Abs(b);

	d = Max(c, d);

	return d == 0.0 ? 0.0 : Abs(a - b) / d;
}



wxString vv_strtypes[7] = { "invalid", "number", "array", "matrix", "string", "table", "binary" };

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

bool VarTable::Delete( const wxString &name )
{
	iterator it = find(name);
	if ( it != end() )
	{
		delete it->second;
		erase( it );
		return true;
	}
	else
		return false;
}

int VarTable::Delete( const wxArrayString &names )
{
	int ndel = 0;
	for( size_t i=0;i<names.size();i++ )
		if ( Delete( names[i] ) )
			ndel++;

	return ndel;
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

bool VarTable::Write( const wxString &file, size_t maxdim )
{
	wxFFileOutputStream out(file);
	if ( !out.IsOk() ) return false;
	Write( out, maxdim );
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

#ifdef _DEBUG
            if ( it->second->Type() == VV_BINARY )
			  {
				wxLogStatus("WRITE VV_BINARY(%s): %d bytes", (const char*)it->first.c_str(), (int)it->second->Binary().GetDataLen() );
			  }
#endif
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
#ifdef _DEBUG
            if ( list[i]->Type() == VV_BINARY )
			  {
				wxLogStatus("WRITE VV_BINARY(%s): %d bytes", (const char*)names[i].c_str(), (int)list[i]->Binary().GetDataLen() );
			  }
#endif
		}
	}

	out.Write8( 0xf9 );
}

bool VarTable::Read( const wxString &file )
{
	wxFFileInputStream in( file );
	if ( !in.IsOk() ) return false;
	else return Read( in );
}

bool VarTable::Read( wxInputStream &_I )
{
	clear();
	
	wxDataInputStream in(_I);
	wxUint8 code = in.Read8();
	in.Read8(); //ver

  bool ok = true;
	size_t n = in.Read32();
	for ( size_t i=0;i<n;i++ )
	{
		wxString name = in.ReadString();
		VarValue *value = new VarValue;
		ok = ok && value->Read( _I );

#ifdef _DEBUG
		if( value->Type() == VV_BINARY )
		  {
//			wxLogStatus("READ VV_BINARY(%s): %d bytes", (const char*)name.c_str(), (int)value->Binary().GetDataLen() );
		  }
#endif
		
		if ( find(name) == end() ) (*this)[name] = value;
		else delete value;
	}
	
	return in.Read8() == code;
}

bool VarTable::Write_text(const wxString &file, size_t maxdim)
{
	wxFFileOutputStream out(file);
	if (!out.IsOk()) return false;
	Write_text(out, maxdim);
	return true;
}


void VarTable::Write_text(wxOutputStream &_O, size_t maxdim)
{
	wxExtTextOutputStream out(_O, wxEOL_UNIX);
	out.Write8(1);
	out.PutChar('\n');
	wxArrayString names;
	VarValue *v;
	if (maxdim == 0)
	{
		out.Write32(size());
		out.PutChar('\n');
		// add sorting for consistent and comparable defaults 
		/*
		for (iterator it = begin(); it != end(); ++it)
		{
			out.WriteString(it->first);
			out.PutChar('\n');
			it->second->Write_text(_O);

			if (it->second->Type() == VV_BINARY)
			{
				wxLogStatus("WRITE VV_BINARY(%s): %d bytes", (const char*)it->first.c_str(), (int)it->second->Binary().GetDataLen());
			}
		}
		*/
		names = ListAll();
		names.Sort();
		for (size_t i = 0; i < names.Count(); i++)
		{
			v = Get(names[i]);
			if (v != NULL)
			{
				out.WriteString(names[i]);
				out.PutChar('\n');
				v->Write_text(_O);
#ifdef _DEBUG
				if (v->Type() == VV_BINARY)
				{
					wxLogStatus("WRITE VV_BINARY(%s): %d bytes", (const char*)names[i].c_str(), (int)v->Binary().GetDataLen());
				}
#endif
			}
		}
	}
	else
	{
// add sorting for consistent and comparable defaults 
//		wxArrayString names;
/*
		std::vector<VarValue*> list;
		list.reserve(size());

		for (iterator it = begin(); it != end(); ++it)
		{
			VarValue &vv = *(it->second);
			if (vv.Type() == VV_ARRAY
				&& vv.Length() <= maxdim)
			{
				names.Add(it->first);
				list.push_back(it->second);
			}
			else if (vv.Type() == VV_MATRIX
				&& vv.Matrix().nrows() <= maxdim
				&& vv.Matrix().ncols() <= maxdim)
			{
				names.Add(it->first);
				list.push_back(it->second);
			}
			else if (vv.Type() != VV_MATRIX
				&& vv.Type() != VV_ARRAY)
			{
				names.Add(it->first);
				list.push_back(it->second);
			}
		}

		out.Write32(list.size());
		out.PutChar('\n');
*/
		for (iterator it = begin(); it != end(); ++it)
		{
			VarValue &vv = *(it->second);
			if (vv.Type() == VV_ARRAY
				&& vv.Length() <= maxdim)
				names.Add(it->first);
			else if (vv.Type() == VV_MATRIX
				&& vv.Matrix().nrows() <= maxdim
				&& vv.Matrix().ncols() <= maxdim)
				names.Add(it->first);
			else if (vv.Type() != VV_MATRIX
				&& vv.Type() != VV_ARRAY)
				names.Add(it->first);
		}

		names.Sort();
		out.Write32(names.Count());
		out.PutChar('\n');

		for (size_t i = 0; i<names.Count(); i++)
		{
			v = Get(names[i]);
			if (v != NULL)
			{
				out.WriteString(names[i]);
				out.PutChar('\n');
				v->Write_text(_O);
#ifdef _DEBUG
				if (v->Type() == VV_BINARY)
				{
					wxLogStatus("WRITE VV_BINARY(%s): %d bytes", (const char*)names[i].c_str(), (int)v->Binary().GetDataLen());
				}
#endif
			}
		}
	}
}
bool VarTable::Read_text(const wxString &file)
{
	wxFFileInputStream in(file);
	if (!in.IsOk()) return false;
	else return Read_text(in);
}

bool VarTable::Read_text(wxInputStream &_I)
{
	clear();

	wxExtTextInputStream in(_I, "\n");
	in.Read8(); //ver

	bool ok = true;
	size_t n = in.Read32();
	for (size_t i = 0; i<n; i++)
	{
		wxString name = in.ReadWord();
		VarValue *value = new VarValue;
		ok = ok && value->Read_text(_I);

#ifdef _DEBUG
        if (value->Type() == VV_BINARY)
		{
//			wxLogStatus("READ VV_BINARY(%s): %d bytes", (const char*)name.c_str(), (int)value->Binary().GetDataLen());
		}
#endif

		if (find(name) == end()) (*this)[name] = value;
		else delete value;
	}

	return ok;
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

VarValue::VarValue( double f )
{
	m_type = VV_NUMBER;
	m_val = f;
}

VarValue::VarValue( bool b )
{
	m_type = VV_NUMBER;
	m_val = b ? 1 : 0;
}

VarValue::VarValue( const std::vector<double> &f )
{
	m_type = VV_ARRAY;
	if ( f.size() > 0 ) m_val.assign( &f[0], f.size() );
}

VarValue::VarValue( double *arr, size_t n )
{
	m_type = VV_ARRAY;
	m_val.assign( arr, n );
}

VarValue::VarValue( double *mat, size_t r, size_t c )
{
	m_type = VV_MATRIX;
	m_val.assign( mat, r, c );
}

VarValue::VarValue( const ::matrix_t<double> &m )
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

VarValue::VarValue( const wxMemoryBuffer &mb )
{
	m_type = VV_BINARY;
	m_bin = mb;
}

VarValue::~VarValue()
{
	// Nothing to do
}

VarValue &VarValue::operator=( const VarValue &rhs )
{
	Copy( rhs );
	return *this;
}


bool VarValue::ValueEqual( VarValue &rhs )
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
					  {
						if (equal)
//							equal = equal && (m_val(r, c) == rhs.m_val(r, c));
							equal = equal && (RelDif(m_val(r, c),rhs.m_val(r, c)) < TOLERANCE);
						else
							break;
					  }
			break;
		case VV_TABLE: // not working correctly 
			equal = (m_tab.size() == rhs.m_tab.size());
			if (equal)
				for (VarTable::iterator it1 = m_tab.begin(); it1 != m_tab.end(); ++it1)
				{
					if (VarValue *vv = rhs.m_tab.Get(it1->first))
						equal = equal && (it1->second->ValueEqual(*vv));
					else
					{
						equal = false;
						break;
					}
				}
			break;
		case VV_STRING:
			equal = (m_str == rhs.m_str);
			break;
		case VV_BINARY:
			if ( m_bin.GetDataLen() == rhs.m_bin.GetDataLen() )
			{
				equal = true;
				size_t n = m_bin.GetDataLen();
				char *b1 = (char*)m_bin.GetData();
				char *b2 = (char*)rhs.m_bin.GetData();
				for( size_t i=0;i<n;i++ )
				{
					if ( b1[i] != b2[i] )
					{
						equal = false;
						break;
					}
				}
			}
			else
				equal = false;
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
		// wxMemeoryBuffer is not a copy on write so causing issues bewtween cases, e.g. shade_scen_3d
		//		m_bin = rhs.m_bin;
		m_bin.Clear();
		m_bin.AppendData(rhs.m_bin.GetData(),rhs.m_bin.GetDataLen());
		// UI hints?
	}
}


void VarValue::Write( wxOutputStream &_O )
{
	wxDataOutputStream out(_O);

	out.Write8( 0xf2 );
//	out.Write8(1);
	out.Write8(2); // float to double

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
				out.WriteDouble(m_val(r, c));
//		out.WriteFloat(m_val(r, c));
		break;
	case VV_TABLE:
		m_tab.Write( _O );
		break;
	case VV_STRING:
		out.WriteString( m_str );
		break;
	case VV_BINARY:
		out.Write32( m_bin.GetDataLen() );
		_O.Write( m_bin.GetData(), m_bin.GetDataLen() );
		break;
	}

	out.Write8( 0xf2 );
}

bool VarValue::Read(wxInputStream &_I)
{
	wxDataInputStream in(_I);

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8(); // ver

	m_type = in.Read8();

	size_t nr, nc, len;
	switch (m_type)
	{
	case VV_INVALID:
		break;
	case VV_NUMBER:
	case VV_ARRAY:
	case VV_MATRIX:
		nr = in.Read32();
		nc = in.Read32();
		if (nr*nc < 1) return false; // big error
		m_val.resize_fill(nr, nc, 0.0f);
		for (size_t r = 0; r<nr; r++)
			for (size_t c = 0; c < nc; c++)
			{
				if (ver == 1)
					m_val(r, c) = in.ReadFloat();
				else
					m_val(r, c) = in.ReadDouble();
			}
		break;
	case VV_TABLE:
		m_tab.Read(_I);
		break;
	case VV_STRING:
		m_str = in.ReadString();
		break;
	case VV_BINARY:
		len = in.Read32();
		_I.Read(m_bin.GetWriteBuf(len), len);
		m_bin.UngetWriteBuf(len);
		break;
	}

	return in.Read8() == code;
}

void VarValue::Write_text(wxOutputStream &_O)
{
	wxExtTextOutputStream out(_O, wxEOL_UNIX);
	size_t n;
	wxString x;

	out.Write8(1);
	out.PutChar('\n');
	out.Write8(m_type);
	out.PutChar('\n');

	switch (m_type)
	{
	case VV_INVALID:
		break; // no data to be written
	case VV_NUMBER:
	case VV_ARRAY:
	case VV_MATRIX:
		out.Write32(m_val.nrows());
		out.PutChar('\n');
		out.Write32(m_val.ncols());
		out.PutChar('\n');
		for (size_t r = 0; r < m_val.nrows(); r++)
		{
			for (size_t c = 0; c < m_val.ncols(); c++)
			{
				out.WriteDouble(m_val(r, c));
				/*
								wxString sd;
				sd.Printf("%g", m_val(r, c));
				out.WriteString(sd);
void wxTextOutputStream::WriteDouble(double d)
{
	wxString str;

	str.Printf(wxT("%f"), d);
	WriteString(str);
}

*/
	//			out.PutChar('\n');
				if (m_val.nrows()*m_val.ncols() > 1) out.PutChar(' ');
			}
			if (m_val.nrows()*m_val.ncols() > 1) out.PutChar('\n');
		}
		out.PutChar('\n');
		break;
	case VV_TABLE:
		m_tab.Write_text(_O);
		break;
	case VV_STRING:
		x = m_str;
		if (wxFileName::Exists(x))
		{ // write filename only
			wxString fn, ext;
			wxFileName::SplitPath(x, NULL, &fn, &ext);
			x = fn + "." + ext;
		}
		x.Replace("\r", "");
		n = x.Len();
		out.Write32((wxUint32)n);
		if (n > 0)
		{
			out.PutChar('\n');
			for (size_t i = 0; i < n; i++)
			{
				out.PutChar(x[i]);
			}
		}
		out.PutChar('\n');
		break;
	case VV_BINARY:
		out.Write32(m_bin.GetDataLen());
		out.PutChar('\n');
		wxByte *p = (wxByte*)m_bin.GetData();
		for (size_t i = 0; i < m_bin.GetDataLen(); i++)
			out.Write(p[i]);
//		_O.Write(m_bin.GetData(), m_bin.GetDataLen());
//		out.PutChar('`');
		break;
	}

}

bool VarValue::Read_text(wxInputStream &_I)
{
	wxExtTextInputStream in(_I, "\n");
	size_t n;

	in.Read8(); // ver

	m_type = in.Read8();

	bool ok = true;

	size_t nr, nc, len;
	switch (m_type)
	{
	case VV_INVALID:
		break;
	case VV_NUMBER:
	case VV_ARRAY:
	case VV_MATRIX:
		nr = in.Read32();
		nc = in.Read32();
		if (nr*nc < 1) return false; // big error
		m_val.resize_fill(nr, nc, 0.0);
		if (nc*nr > 1)
		{
			for (size_t r = 0; r < nr; r++)
			{
				wxString x = in.ReadLine();
				wxArrayString ar = wxStringTokenize(x, ' ');
				if (nc != ar.Count()) return false;
				for (size_t c = 0; c < nc; c++)
				{
					double y;
					if (ar[c].ToDouble(&y))
						m_val(r, c) = y;
					else
						return false;
				}
			}
		}
		else
			m_val(0, 0) = in.ReadDouble();
		/*
		for (size_t r = 0; r < nr; r++)
			for (size_t c = 0; c < nc; c++)
			{
		//		m_val(r, c) = in.ReadDouble();
			}
			*/
		break;
	case VV_TABLE:
		ok = ok && m_tab.Read_text(_I);
		break;
	case VV_STRING:
		n = in.Read32();
		m_str.Clear();
		if (n > 0)
		{
			for (size_t i = 0; i < n; i++)
				m_str.Append(in.GetChar());
		}
		break;
	case VV_BINARY:
		len = in.Read32();
		m_bin.SetBufSize(len);
		m_bin.Clear();
//		char *p = (char*)m_bin.GetWriteBuf(len);
		for (size_t i = 0; i <len; i++)
			m_bin.AppendByte(in.GetChar());

//		_I.Read(m_bin.GetWriteBuf(len), len);
//		m_bin.UngetWriteBuf(len);
		break;
	}

	return ok;
//	return in.Read8() == code;
}

int VarValue::Type() const { return m_type; }
wxString VarValue::TypeAsString() const {
	switch (m_type) {
	case VV_INVALID: return wxString("invalid");
	case VV_NUMBER: return wxString("number");
	case VV_ARRAY: return wxString("array");
	case VV_MATRIX: return wxString("matrix");
	case VV_STRING: return wxString("string");
	case VV_BINARY: return wxString("binary");
	case VV_TABLE: return wxString("table");
	}
	return wxString();
}

void VarValue::ChangeType(int type) { m_type = (unsigned char)type; }
void VarValue::SetType( int ty ) { m_type = (unsigned char)ty; }
void VarValue::Set( int val ) { m_type = VV_NUMBER; m_val = (float)val; }
//void VarValue::Set( float val ) { m_type = VV_NUMBER; m_val = val; }
void VarValue::Set( double val ) { m_type = VV_NUMBER; m_val = val; }

void VarValue::Set( const std::vector<int> &ivec )
{
	m_type = VV_ARRAY;
	if ( ivec.size() > 0 ) 
	{
		m_val.resize_fill( ivec.size(), 0 );
		for( size_t i=0;i<ivec.size();i++ )
			m_val.at(i) = (double)ivec[i];
	}
	else
		m_val.clear();
}

void VarValue::Set( const std::vector<double> &fvec )
{
	m_type = VV_ARRAY;
	if( fvec.size() > 0 ) m_val.assign( &fvec[0], fvec.size() );
	else m_val.clear();
}

void VarValue::Set( double *val, size_t n ) { m_type = VV_ARRAY; m_val.assign( val, n ); }
void VarValue::Set(double *mat, size_t r, size_t c ) { m_type = VV_MATRIX; m_val.assign( mat, r, c ); }
void VarValue::Set( const ::matrix_t<double> &mat ) { m_type = VV_MATRIX; m_val = mat; }
void VarValue::Set( const wxString &str ) { m_type = VV_STRING; m_str = str; }
void VarValue::Set( const VarTable &tab ) { m_type = VV_TABLE; m_tab.Copy( tab ); }
void VarValue::Set( const wxMemoryBuffer &mb ) { m_type = VV_BINARY; m_bin = mb; }

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

double VarValue::Value()
{
	if ( m_type == VV_NUMBER || Length() == 1) return (double)m_val;
	else return std::numeric_limits<double>::quiet_NaN();
}

size_t VarValue::Length()
{
	if ( m_type == VV_ARRAY ) return m_val.length();
	else return 0;
}
size_t VarValue::Rows()
{
	if (m_type == VV_ARRAY || m_type == VV_MATRIX) return m_val.nrows();
	else if (m_type == VV_NUMBER) return 1;
	else return 0;
}
size_t VarValue::Columns()
{
	if (m_type == VV_ARRAY || m_type == VV_MATRIX) return m_val.ncols();
	else if (m_type == VV_NUMBER) return 1;
	else return 0;
}
double *VarValue::Array( size_t *n )
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

std::vector<double> VarValue::Array()
{
	if ( m_type == VV_ARRAY )
	{
		std::vector<double> vec( m_val.length(), 0.0 );
		for( size_t i=0;i<m_val.length();i++)
			vec[i] = m_val[i];
		return vec;
	}
	else
		return std::vector<double>();
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
 
matrix_t<double> &VarValue::Matrix()
{
	return m_val;
}

double *VarValue::Matrix( size_t *nr, size_t *nc )
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

wxMemoryBuffer &VarValue::Binary()
{
	return m_bin;
}


bool VarValue::Read( const lk::vardata_t &val, bool change_type )
{
	bool ok = false;
	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		if ( Type() == VV_NUMBER || change_type )
		{
			Set( (float) val.as_number() );
			ok = true;
		}
		break;
	case lk::vardata_t::STRING:
		if ( Type() == VV_STRING || change_type )
		{
			Set( val.as_string() );
			ok = true;
		}
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
				double *vec = new double[dim1];
				for( size_t i=0;i<dim1;i++)
					vec[i] = (double)val.index(i)->as_number();


				if ( Type() == VV_ARRAY || change_type )
				{
					Set( vec, dim1 );
					ok = true;
				}
				else if ( Type() == VV_MATRIX )
				{
					Set( vec, dim1, 1 );
					ok = true;
				}

				delete [] vec;
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				if ( Type() == VV_MATRIX || change_type )
				{
					m_type = VV_MATRIX;
					m_val.resize_fill( dim1, dim2, 0.0 );

					for ( size_t i=0;i<dim1;i++)
					{
						for ( size_t j=0;j<dim2;j++ )
						{
							double x = 0;
							if ( val.index(i)->type() == lk::vardata_t::VECTOR
								&& j < val.index(i)->length() )
								x = (double)val.index(i)->index(j)->as_number();

							m_val.at(i,j) = x;
						}
					}

					ok = true;
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
					item->Read( *(*it).second, true );
				}

				ok = true;
			}
		}
		break;
	}

	return ok;
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
			double *p = Array( &n );
			val.empty_vector();
			if ( n > 0 )
			{
				val.vec()->reserve( (size_t) n  );
				for (size_t i=0;i<n;i++)
					val.vec_append( (double)p[i] );
			}
		}
		break;
	case VV_MATRIX:
		{
			::matrix_t<double> &mat = Matrix();
			val.empty_vector();
			val.vec()->reserve( mat.nrows() );
			for (size_t i=0;i<mat.nrows();i++)
			{
				val.vec()->push_back( lk::vardata_t() );
				val.vec()->at(i).empty_vector();
				val.vec()->at(i).vec()->reserve( mat.ncols() );
				for (size_t j=0;j<mat.ncols();j++)
					val.vec()->at(i).vec_append( mat.at(i,j) );
			}
		}
		break;
	case VV_TABLE:
		{
			val.empty_hash();
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
	case VV_BINARY:
			val.assign( wxString::Format("binary<%d>", (int)m_bin.GetDataLen() ) );
		break;
	}

	return m_type != VV_INVALID;
}

static inline char nibble_to_hex( unsigned char nibble )
{
	return nibble <= 9 ? ( '0'+nibble ) : ( 'A' + nibble-10 );
}

static inline unsigned char hex_to_nibble( char hex )
{
	unsigned char bin = 0;
	if ( hex >= '0' && hex <= '9' ) bin = hex-'0';
	else if (hex >= 'A' && hex <= 'F') bin =  hex-'A'+10;
	else if (hex >= 'a' && hex <= 'f') bin =  hex-'a'+10;	
	return bin;
}

wxString bintohexstr( char *data, int len )
{
	wxString str( len*2, '0' );
	int i;
	wxString::iterator it = str.begin();
	for( i=0;i<len;i++ )
	{
		unsigned char byte = data[i];
		unsigned char upper = (byte >> 4);
		unsigned char lower = (byte & 0x0f);
		*it++ = nibble_to_hex(upper);
		*it++ = nibble_to_hex(lower);
	}
	return str;
}

void hexstrtobin( const wxString &str, char *data, int len )
{
	int idx = 0;
	wxString::const_iterator it = str.begin();
	while ( it != str.end() )
	{
		unsigned char upper = hex_to_nibble( *it++ );
		if ( it == str.end() ) return;

		unsigned char lower = hex_to_nibble( *it++ );
		if ( idx < len ) data[idx++] = (upper << 4) | lower;
	}
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
	case VV_BINARY:
		value.m_type = VV_BINARY;
		value.m_bin.Clear();
		if ( str.Len() > 0 )
		{
			int nbytes = str.Len()/2;
			if ( nbytes*2 != (int)str.Len() ) return false;
			char *data = (char*)value.m_bin.GetWriteBuf( nbytes );
			hexstrtobin( str, data, nbytes );
			value.m_bin.UngetWriteBuf( nbytes );
		}
		return true;
	}
	
	return false;
}

wxString VarValue::AsString( wxChar arrsep, wxChar tabsep )
{
	switch( m_type )
	{
	case VV_INVALID: return "<invalid>";
	case VV_STRING: return m_str;
	case VV_NUMBER:
	{
		if (std::isnan((float)m_val))
			return "NaN";
		else
			return wxString::Format("%g", (float)m_val);
	}
	case VV_ARRAY:
	{
		wxString buf="";
		for( size_t i=0;i<m_val.length();i++ )
		{
			buf += wxString::Format("%g", (float)m_val[i]  );
			buf += arrsep;
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
	case VV_BINARY:
		return bintohexstr( (char*)m_bin.GetData(), m_bin.GetDataLen() );
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

void VarInfo::Write(wxOutputStream &os)
{
	wxDataOutputStream out(os);
	out.Write8(0xe1);
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

	out.Write8(0xe1);
}


bool VarInfo::Read(wxInputStream &is)
{
	wxDataInputStream in(is);
	wxUint8 code = in.Read8();
	int ver = in.Read8(); // ver

	if (ver < 2) in.ReadString(); // formerly, name field

	Type = in.Read32();
	Label = in.ReadString();
	Units = in.ReadString();
	Group = in.ReadString();
	IndexLabels = wxSplit( in.ReadString(), '|' );
	Flags = in.Read32();
	bool valok = DefaultValue.Read(is);
	if (ver < 3)
		UIObject = VUIOBJ_NONE; // wxUIObject associated with variable
	else
		UIObject = in.ReadString();
	wxUint8 lastcode = in.Read8();
	return  lastcode == code && valok;
}

void VarInfo::Write_text(wxOutputStream &os)
{
	wxExtTextOutputStream out(os, wxEOL_UNIX);
	out.Write8(3); // change to version 3 after wxString "UIObject" field added
	out.PutChar('\n');
	out.Write32(Type);
	out.PutChar('\n');
	if (Label.Len() > 0)
		out.WriteString(Label);
	else 
		out.WriteString(" ");
	out.PutChar('\n');
	if (Units.Len() > 0)
		out.WriteString(Units);
	else
		out.WriteString(" ");
	out.PutChar('\n');
	if (Group.Len() > 0)
		out.WriteString(Group);
	else
		out.WriteString(" ");
	out.PutChar('\n');
	/* Handle multiline equations in IndexLabels
		e.g. PV system Design
		Numeric
		subarray1_nstrings
		3
		1
		Number of parallel strings 1

		PV System Design
		=${pv.array.strings_in_parallel}
		- ?${pv.subarray2.enable}[0|${pv.subarray2.num_strings}]
		- ?${pv.subarray3.enable}[0|${pv.subarray3.num_strings}]
		- ?${pv.subarray4.enable}[0|${pv.subarray4.num_strings}]
		9
		1
		1
		1
		1
		0.000000
	*/
	wxString x = "";
	if (IndexLabels.Count() > 0)
	{
		x = wxJoin(IndexLabels, '|');
	}
	size_t n = x.Len();
	out.Write32((wxUint32)n);
	if (n > 0)
	{
		out.PutChar('\n');
		for (size_t i = 0; i < n; i++)
		{
			out.PutChar(x[i]);
		}
	}
	out.PutChar('\n');
	out.Write32(Flags);
	out.PutChar('\n');
	DefaultValue.Write_text(os);
	if (UIObject.Len() > 0)
		out.WriteString(UIObject);
	else
		out.WriteString(" ");
	out.PutChar('\n');
}

bool VarInfo::Read_text(wxInputStream &is)
{
	wxExtTextInputStream in(is, "\n", wxConvAuto(wxFONTENCODING_UTF8));
	int ver = in.Read8(); // ver

	if (ver < 2) in.ReadWord(); // formerly, name field

	bool ok = true;

	Type = in.Read32();
	Label = in.ReadWord();
	Units = in.ReadWord();
	Group = in.ReadWord();
	size_t n = in.Read32();
	if (n > 0)
	{
		wxString x;
		for (size_t i = 0; i < n; i++)
			x.Append(in.GetChar());
		IndexLabels = wxSplit(x, '|');
	}

	Flags = in.Read32();
	ok = ok && DefaultValue.Read_text(is);
	if (ver < 3)
		UIObject = VUIOBJ_NONE; // wxUIObject associated with variable
	else
		UIObject = in.ReadWord();
	return  ok;
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

void VarDatabase::Write(wxOutputStream &os)
{
	wxDataOutputStream out(os);
	out.Write8(0xf8);
	out.Write8(1);
	out.Write32(size());
	for (VarInfoHash::iterator it = begin();
		it != end();
		++it)
	{
		out.WriteString(it->first);
		it->second->Write(os);
	}
	out.Write8(0xf8);
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

void VarDatabase::Write_text(wxOutputStream &os)
{
	wxExtTextOutputStream out(os, wxEOL_UNIX);
	out.PutChar('\n');
	out.Write32(size());
	out.PutChar('\n');
	// Sort for consistent order
	/*
	for (VarInfoHash::iterator it = begin();
		it != end();
		++it)
	{
		out.WriteString(it->first);
		out.PutChar('\n');
		it->second->Write_text(os);
	}
	*/
	VarInfo *v;
	wxArrayString as = ListAll();
	as.Sort();
	for (size_t i = 0; i < as.Count(); i++)
	{
		v = Lookup(as[i]);
		if (v != NULL)
		{
			out.WriteString(as[i]);
			out.PutChar('\n');
			v->Write_text(os);
		}
	}
}

bool VarDatabase::Read_text(wxInputStream &is, const wxString &page)
{
	wxExtTextInputStream in(is, "\n");
	size_t n = in.Read32();
	bool ok = true;
	wxArrayString list;
	for (size_t i = 0; i<n; i++)
	{
		wxString name = in.ReadWord();
		VarInfo *vv = 0;

		VarInfoHash::iterator it = find(name);
		if (it != end())
			vv = it->second;
		else
			vv = new VarInfo;

		ok = ok && vv->Read_text(is);

		(*this)[name] = vv;
		if (!page.IsEmpty()) list.Add(name);
	}
	return ok;
	
//	if (!ok) return false;
//	return in.Read8() == code;
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
	else return "<not found: '" + name + "'>";
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

wxString VarInfoLookup::LookupByLabel(const wxString &label)
{
	for (VarInfoHash::iterator it = begin(); it != end(); ++it)
		if (it->second->Label.IsSameAs(label, false))
			return it->first;
	return wxEmptyString;
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

