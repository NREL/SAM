#ifndef __variable_h
#define __variable_h

#include <vector>

#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/stream.h>
#include <wx/buffer.h>

#include <lk_absyn.h>
#include <lk_eval.h>

#include "object.h"

class VarValue;
class VarDatabase;


#define VV_INVALID 0
#define VV_NUMBER 1
#define VV_ARRAY 2
#define VV_MATRIX 3
#define VV_STRING 4
#define VV_TABLE 5
#define VV_BINARY 6

extern wxString vv_strtypes[7];

typedef unordered_map<wxString, VarValue*, wxStringHash, wxStringEqual> VarTableBase;

class VarTable : public VarTableBase
{
public:
	VarTable();
	VarTable( const VarTable &rhs );
	~VarTable();

	VarTable &operator=( const VarTable &rhs );
	void Copy( const VarTable &rhs );
	
	bool Delete( const wxString &name );
	int Delete( const wxArrayString &names );
	virtual void clear();
	wxArrayString ListAll( std::vector<VarValue*> *vals = 0 );
	VarValue *Create( const wxString &name, int type = VV_INVALID );
	VarValue *Set( const wxString &name, const VarValue &val );
	VarValue *Get( const wxString &name );
	bool Rename( const wxString &old_name, const wxString &new_name );

	void Write( wxOutputStream &, size_t maxdim = 0 ); // MaxDim specifies the maximum allowable array or matrix dimension when writing.
	bool Write( const wxString &file, size_t maxdim= 0);
	bool Read( wxInputStream & );
	bool Read( const wxString &file );
		
};

class VarValue
{
public:
	VarValue();
	VarValue( const VarValue &vv );

	explicit VarValue( int i );
	explicit VarValue( bool b );
	explicit VarValue( float f );
	explicit VarValue( const std::vector<float> &f );
	explicit VarValue( const matrix_t<float> &m );
	explicit VarValue( const wxString &s );
	explicit VarValue( const VarTable &t );
	explicit VarValue( const wxMemoryBuffer &mb );

	VarValue( float *arr, size_t n );
	VarValue( float *mat, size_t r, size_t c );

	virtual ~VarValue();

	VarValue &operator=( const VarValue &rhs );
	bool ValueEqual( VarValue &rhs);
	void Copy( const VarValue &rhs );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
	
	int Type() const;
	void SetType( int t );
	void Set( int val );
	void Set( float val );
	void Set( double val );
	void Set( const std::vector<int> &ivec );
	void Set( const std::vector<float> &fvec );
	void Set( float *val, size_t n );
	void Set( float *mat, size_t r, size_t c );
	void Set( const ::matrix_t<float> &mat );
	void Set( const wxString &str );
	void Set( const VarTable &tab );
	void Set( const wxMemoryBuffer &mb );

	int Integer();
	bool Boolean();
	float Value();
	float *Array( size_t *n );
	std::vector<float> Array();
	size_t Length();
	std::vector<int> IntegerArray();
	::matrix_t<float> &Matrix();
	float *Matrix( size_t *nr, size_t *nc );
	wxString String();
	VarTable &Table();
	wxMemoryBuffer &Binary();

	bool Read( const lk::vardata_t &val, bool change_type = false );
	bool Write( lk::vardata_t &val );

	static bool Parse( int type, const wxString &str, VarValue &val );
	wxString AsString(wxChar arrsep = ';', wxChar tabsep = '|');

	static VarValue Invalid;
private:
	unsigned char m_type;
	::matrix_t<float> m_val;
	wxString m_str;
	VarTable m_tab;
	wxMemoryBuffer m_bin;
};

#define VF_NONE                0x00
#define VF_HIDE_LABELS         0x01
#define VF_PARAMETRIC          0x02
#define VF_INDICATOR           0x04
#define VF_CALCULATED          0x08
#define VF_LIBRARY             0x10
#define VF_EXCLUSIVE_PAGES     0x20
#define VF_COLLAPSIBLE_PANE    0x40

#define VUIOBJ_NONE		"Default"

class VarInfo
{
public:
	VarInfo();
	VarInfo( const VarInfo &copy );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

	int Type;
	wxString Label;
	wxString Units;
	wxString Group;
	wxArrayString IndexLabels;
	long Flags;
	VarValue DefaultValue;
	wxString UIObject; // typename of wxUIObject used for editing
};

typedef unordered_map<wxString, VarInfo*, wxStringHash, wxStringEqual> VarInfoHash;	

class VarInfoLookup : public VarInfoHash
{
public:
	VarInfoLookup();
	virtual ~VarInfoLookup();

	bool Add( const wxString &name, VarInfo *vv );
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
};

class VarDatabase : public VarInfoLookup
{
public:
	VarDatabase();
	virtual ~VarDatabase();

	VarInfo *Create( const wxString &name, int type,
		const wxString &label = wxEmptyString, const wxString &units = wxEmptyString,
		const wxString &group = wxEmptyString, const wxString &indexlabels = wxEmptyString,
		unsigned long flags = VF_NONE, const VarValue &defval = VarValue::Invalid, const wxString &uiobject = wxEmptyString);
			
	bool Delete( const wxString &name );
	bool Rename( const wxString &old_name, const wxString &new_name );
	virtual void clear();	
	
	bool LoadFile( const wxString &file, const wxString &page = wxEmptyString );
	void Write( wxOutputStream & );
	bool Read( wxInputStream &, const wxString &page = wxEmptyString );
	
private:
	
	typedef unordered_map<wxString, wxArrayString, wxStringHash, wxStringEqual> StringArrayHash;

};

class VarTableScriptInterpreter : public lk::eval
{
private:
	VarTable *m_vars;
public:
	VarTableScriptInterpreter( lk::node_t *tree, lk::env_t *env, VarTable *vt );
	virtual ~VarTableScriptInterpreter( );	
	virtual bool special_set( const lk_string &name, lk::vardata_t &val );
	virtual bool special_get( const lk_string &name, lk::vardata_t &val );
};

#endif
