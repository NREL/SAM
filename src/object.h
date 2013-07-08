#ifndef __object_h
#define __object_h


#include <vector>

#ifdef _MSC_VER
#include <unordered_map>
using std::tr1::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'
#else
#include <tr1/unordered_map>
using std::tr1::unordered_map;
#endif

#include <wx/hashmap.h>
#include <wx/stream.h>


class Object
{
public:
	virtual ~Object();

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

class ObjectCollection : public unordered_map< wxString, Object*, wxStringHash, wxStringEqual >
{
public:
	ObjectCollection();
	virtual ~ObjectCollection();
	
	void Clear();
	virtual void clear() { this->Clear(); }
	void Add( const wxString &name, Object * );
	bool Delete( const wxString &name );
	Object *Lookup( const wxString &name );
	wxArrayString GetNames();
	
	void Write( wxOutputStream &out );
	bool Read( wxInputStream &in ); // does NOT clear first
};




#endif

