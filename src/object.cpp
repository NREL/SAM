/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/datstrm.h>
#include <wx/tokenzr.h>
#include "object.h"


static ObjectCollection g_objectTypes;

void ObjectTypes::Register( Object *dummy )
{
	if ( !dummy ) return;
	ObjectCollection::iterator it = g_objectTypes.find( dummy->GetTypeName() );
	if ( it != g_objectTypes.end() )
	{
		delete it->second;
		it->second = dummy;
	}
	else
		g_objectTypes[ dummy->GetTypeName() ] = dummy;
}

Object *ObjectTypes::Create( const wxString &type )
{
	if ( Object *o = g_objectTypes.Lookup( type ) )
		return o->Duplicate();
	else
		return 0;
}

wxArrayString ObjectTypes::AllTypes()
{
	return g_objectTypes.GetNames();
}

ObjectCollection::ObjectCollection()
{
}

ObjectCollection::ObjectCollection( const ObjectCollection &rhs )
{
	Copy( rhs );
}

ObjectCollection::~ObjectCollection()
{
	for ( iterator it = begin();
		it != end();
		++it )
		delete it->second;
}

void ObjectCollection::Copy( const ObjectCollection &rhs )
{
	Clear();
	for( ObjectCollection::const_iterator it = rhs.begin();
		it != rhs.end();
		++it )
		Add( it->first, it->second->Duplicate() );
}

void ObjectCollection::Clear()
{
	for ( iterator it = begin();
		it != end();
		++it )
		delete it->second;

	ObjectCollectionBase::clear();
}

void ObjectCollection::Add( const wxString &name, Object *obj )
{
	iterator it = find( name );
	if ( it != end() )
	{
		delete it->second;
		it->second = obj;		
	}
	else
		(*this)[name] = obj;
}

bool ObjectCollection::Rename( const wxString &old_name, const wxString &new_name )
{
	iterator it = find( old_name );
	if ( it == end() || Lookup( new_name ) != 0 ) return false;

	Object *obj = it->second;	

	erase( it );

	(*this)[new_name] = obj;

	return true;
}

bool ObjectCollection::Delete( const wxString &name )
{
	iterator it = find( name );
	if ( it != end() )
	{
		delete it->second;
		erase( it );
		return true;
	}

	return false;
}

Object *ObjectCollection::Lookup( const wxString &name )
{
	iterator it = find( name );
	if ( it != end() )
		return it->second;
	else
		return 0;
}

wxArrayString ObjectCollection::GetNames()
{
	wxArrayString list;
	for ( iterator it = begin();
		it != end();
		++it )
		list.Add( it->first );
	return list;
}
	
void ObjectCollection::Write( wxOutputStream &output )
{
	wxArrayString list;
	wxDataOutputStream out(output);

	out.Write16( 0x2f ); // identifier code
	out.Write16( 1 ); // data format version
	
	out.Write32( size() );
	wxArrayString objs = GetNames();
	objs.Sort();
	for ( size_t i=0;i<objs.Count();i++ )
	{
		Object *obj = Lookup( objs[i] );
		assert( obj != 0 );

		out.WriteString( objs[i] );
		out.WriteString( obj->GetTypeName() );
		obj->Write( output );
	}

	out.Write16( 0x2f ); // identifier code to finish
}

bool ObjectCollection::Read( wxInputStream &input )
{
	wxDataInputStream in( input );

	wxUint16 code = in.Read16();
	wxUint16 ver = in.Read16();

	size_t count = in.Read32();
	for ( size_t i=0;i<count;i++ )
	{
		wxString name = in.ReadString();
		wxString type = in.ReadString();

		Object *obj = ObjectTypes::Create( type );
		if ( obj != 0 
			&& obj->Read( input ) )
		{
			Add( name, obj );
		}
	}

	return ( in.Read16() == code );
}


StringHash::StringHash()
{
	// nothing to do
}

Object *StringHash::Duplicate()
{
	StringHash *s = new StringHash;
	s->Copy( this );
	return s;
}

bool StringHash::Copy( Object *obj )
{
	if ( StringHash *sh = dynamic_cast<StringHash*>(obj) )
	{
		*this = *sh;
		return true;
	}
	else return false;
}

wxString StringHash::GetTypeName()
{
	return "sam.stringhash";
}

void StringHash::Write( wxOutputStream &output )
{
	wxDataOutputStream out(output);
	out.Write8(0xb7);
	out.Write32( size() );
	for ( StringHash::iterator it = begin();
		it != end();
		++it )
	{
		out.WriteString( it->first );
		out.WriteString( it->second );
	}
	out.Write8(0xb7);
}

bool StringHash::Read( wxInputStream &input )
{
	wxDataInputStream in(input);
	wxUint8 code = in.Read8();
	size_t n = in.Read32();
	for (size_t i=0;i<n;i++)
	{
		wxString name = in.ReadString();
		wxString value = in.ReadString();
		(*this)[name] = value;
	}
	return code == in.Read8();
}

void StringHash::Split(const wxString &input, char sep, char eq)
{
	clear();
	wxArrayString items = wxStringTokenize(input,sep);
	for (int i=0;i<(int)items.Count();i++)
	{
		int eqpos = items[i].Index(eq);
		if (eqpos >= 0)
			(*this)[ items[i].Left(eqpos) ] = items[i].Mid(eqpos+1);
	}
}

bool StringHash::ReadKeyValueFile( const wxString &file, bool clear )
{
	if ( clear ) this->clear();

	wxFileInputStream infile( file );
	if (!infile.IsOk())
		return false;

	wxTextInputStream in(infile);
	while ( infile.CanRead() )
	{
		wxString line( in.ReadLine() );
		if (line.Trim(false).Trim() == "" || line.Left(1) == "'")
			continue;

		int pos = line.Find('=');
		if ( pos != wxNOT_FOUND )
		{
			wxString key = line.Mid(0, pos);
			wxString val = line.Mid(pos+1);
			if ( !key.IsEmpty() && !val.IsEmpty() )
				(*this)[ key ] = val;
		}
	}
	
	return true;
}
