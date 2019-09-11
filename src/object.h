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

#ifndef __object_h
#define __object_h


#include <vector>
#include <unordered_map>
using std::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'


#include <wx/hashmap.h>
#include <wx/stream.h>


class Object
{
public:
	virtual ~Object() { }

	virtual Object *Duplicate() = 0;
	virtual bool Copy( Object *obj ) = 0;
	virtual wxString GetTypeName() = 0;
	virtual void Write( wxOutputStream & ) = 0;
	virtual bool Read( wxInputStream & ) = 0;
};

class ObjectTypes
{
public:
	static void Register( Object *dummy );
	static Object *Create( const wxString &type );
	static wxArrayString AllTypes();
};

typedef unordered_map< wxString, Object*, wxStringHash, wxStringEqual > ObjectCollectionBase;
class ObjectCollection : public ObjectCollectionBase
{
public:
	ObjectCollection();
	ObjectCollection( const ObjectCollection &rhs );
	virtual ~ObjectCollection();
	
	void Copy( const ObjectCollection &rhs );
	void Clear();
	virtual void clear() { this->Clear(); }
	void Add( const wxString &name, Object * );
	bool Rename( const wxString &old_name, const wxString &new_name );
	bool Delete( const wxString &name );
	Object *Lookup( const wxString &name );
	wxArrayString GetNames();
	template<typename TY> std::vector<TY*> GetObjects( ) {
		std::vector<TY*> list;
		list.reserve( size() );
		for( iterator it = begin(); it!=end();++it )
			if ( TY *ocast = dynamic_cast<TY*>(it->second) )
				list.push_back( ocast );
		return list;
	}

	void Write( wxOutputStream &out );
	bool Read( wxInputStream &in ); // does NOT clear first
};

typedef unordered_map<wxString, wxString, wxStringHash, wxStringEqual> StringHashBase;
class StringHash : public StringHashBase,
	public Object
{
public:
	StringHash();

	virtual Object *Duplicate();
	virtual bool Copy( Object *obj );
	virtual wxString GetTypeName();
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );
	
	void Split(const wxString &input, char sep, char eq);
	bool ReadKeyValueFile( const wxString &file, bool clear = true );

};

#endif

