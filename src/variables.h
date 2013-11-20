#ifndef __variable_h
#define __variable_h

#include <vector>

#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/stream.h>

#include <lk_absyn.h>
#include <lk_env.h>

#include "object.h"

class VarValue;
class VarDatabase;

#define VV_INVALID 0
#define VV_NUMBER 1
#define VV_ARRAY 2
#define VV_MATRIX 3
#define VV_STRING 4
#define VV_TABLE 5

typedef unordered_map<wxString, VarValue*, wxStringHash, wxStringEqual> VarTableBase;

class VarTable : public VarTableBase
{
public:
	VarTable();
	~VarTable();
	
	void Delete( const wxString &name );
	void Delete( const wxArrayString &names );
	virtual void clear();
	wxArrayString ListAll( std::vector<VarValue*> *vals = 0 );
	VarValue *Create( const wxString &name, int type = VV_INVALID );
	VarValue *Set( const wxString &name, const VarValue &val );
	VarValue *Get( const wxString &name );

	void Copy( const VarTable &rhs );
	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
		
};

class VarValue
{
public:
	explicit VarValue();
	explicit VarValue( int i );
	explicit VarValue( float f );
	explicit VarValue( float *arr, size_t n );
	explicit VarValue( float *mat, size_t r, size_t c );
	explicit VarValue( const matrix_t<float> &m );
	explicit VarValue( const wxString &s );
	explicit VarValue( const VarTable &t );
	explicit VarValue( const VarValue &vv );
	virtual ~VarValue();

	VarValue &operator=( const VarValue &rhs );
	void Copy( const VarValue &rhs );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
	
	int Type();
	void SetType( int t );
	void Set( int val );
	void Set( float val );
	void Set( double val );
	void Set( float *val, size_t n );
	void Set( float *mat, size_t r, size_t c );
	void Set( const ::matrix_t<float> &mat );
	void Set( const wxString &str );
	void Set( const VarTable &tab );

	int Integer();
	float Value();
	float *Array( size_t *n );
	std::vector<float> Array();
	::matrix_t<float> &Matrix();
	wxString String();
	VarTable &Table();

	bool Read( const lk::vardata_t &val, bool change_type = false );
	bool Write( lk::vardata_t &val );

	static bool Parse( int type, const wxString &str, VarValue &val );
	wxString AsString();

private:
	unsigned char m_type;
	::matrix_t<float> m_val;
	wxString m_str;
	VarTable m_tab;
};

#define VF_NONE 0x00
#define VF_HIDE_LABELS 0x01
#define VF_PARAMETRIC 0x02
#define VF_INDICATOR 0x04
#define VF_CALCULATED 0x08

class VarInfo
{
public:
	VarInfo() { Type = VV_INVALID; Flags = VF_NONE; }

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

	wxString Name;
	int Type;
	wxString Label;
	wxString Units;
	wxString Group;
	wxArrayString IndexLabels;
	long Flags;
	VarValue DefaultValue;
};

typedef unordered_map<wxString, VarInfo*, wxStringHash, wxStringEqual> VarInfoHash;	

class VarInfoLookup : public VarInfoHash
{
public:
	VarInfoLookup();
	virtual ~VarInfoLookup();

	void Add( VarInfo *vv );
	void Add( VarInfoLookup *vil );
	
	VarInfo *Lookup( const wxString &name );
	wxArrayString ListAll();

	int Type( const wxString &name );
	wxString Label( const wxString &name );
	wxString Group( const wxString &name );
	wxString Units( const wxString &name );
	wxArrayString IndexLabels( const wxString &name );
	unsigned long Flags( const wxString &name );
	VarValue &DefaultValue( const wxString &name );

private:
	VarValue m_invVal;
};

class VarDatabase : public VarInfoLookup
{
public:
	VarDatabase();
	virtual ~VarDatabase();

	VarInfo *Add( const wxString &name, int type,
		const wxString &label = wxEmptyString, const wxString &units = wxEmptyString,
		const wxString &group = wxEmptyString, const wxString &indexlabels = wxEmptyString,
		unsigned long flags = VF_NONE, const VarValue &defval = VarValue() );
		
	bool Delete( const wxString &name );
	bool Rename( const wxString &old_name, const wxString &new_name );
	virtual void clear();	
	
	bool LoadFile( const wxString &file, const wxString &page = wxEmptyString );
	void Write( wxOutputStream & );
	bool Read( wxInputStream &, const wxString &page = wxEmptyString );

	wxArrayString GetVarsForPage( const wxString &page );

private:
	
	typedef unordered_map<wxString, wxArrayString, wxStringHash, wxStringEqual> StringArrayHash;	
	StringArrayHash m_pageCache;

};

class VarTableScriptEnvironment : public lk::env_t
{
private:
	VarTable *m_vars;
public:
	VarTableScriptEnvironment( VarTable *vt );
	virtual ~VarTableScriptEnvironment( );	
	virtual bool special_set( const lk_string &name, lk::vardata_t &val );
	virtual bool special_get( const lk_string &name, lk::vardata_t &val );
};

#endif
