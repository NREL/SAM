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

typedef unordered_map<wxString, VarValue*, wxStringHash, wxStringEqual> VarTableBase;

class VarTable : public VarTableBase
{
public:
	VarTable();
	~VarTable();

	virtual void clear();
	wxArrayString ListAll( std::vector<VarValue*> *vals = 0 );
	VarValue *Set( const wxString &name, const VarValue &val );
	VarValue *Get( const wxString &name );

	void Copy( const VarTable &rhs );
	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
		
};

#define VV_INVALID 0
#define VV_NUMBER 1
#define VV_ARRAY 2
#define VV_MATRIX 3
#define VV_STRING 4
#define VV_TABLE 5

class VarValue
{
public:
	VarValue();
	VarValue( int i );
	VarValue( float f );
	VarValue( float *arr, size_t n );
	VarValue( float *mat, size_t r, size_t c );
	VarValue( const matrix_t<float> &m );
	VarValue( const wxString &s );
	VarValue( const VarTable &t );
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


class VarDatabase
{
public:
	VarDatabase();
	~VarDatabase();

	void Add( const wxString &name, int type,
		const wxString &label, const wxString &units,
		const wxString &group, const wxString &indexlabels,
		unsigned long flags, const VarValue &defval );

	
	void Clear();	
	VarInfo *Lookup( const wxString &name );
	wxArrayString ListAll();
	int Type( const wxString &name );
	wxString Label( const wxString &name );
	wxString Group( const wxString &name );
	wxString Units( const wxString &name );
	wxArrayString IndexLabels( const wxString &name );
	unsigned long Flags( const wxString &name );
	VarValue &InternalDefaultValue( const wxString &name );

	bool AddEquation( const wxString &inputs, const wxString &outputs, const wxString &script, wxArrayString *errors = 0 );
	lk::node_t *GetEquation( const wxString &var, wxArrayString *inputs, wxArrayString *outputs );
	wxArrayString *GetAffectedVariables( const wxString &var );
	struct eqn_data { lk::node_t *tree; wxString script; wxArrayString inputs, outputs; };
	std::vector<eqn_data*> GetEquations() { return m_equations; }
	eqn_data *GetEquationData( const wxString &var );
	int GetEquationIndex( const wxString &var );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

private:
	VarValue m_invVal;

	typedef unordered_map<wxString, VarInfo*, wxStringHash, wxStringEqual> varinfo_hash_t;	
	varinfo_hash_t m_hash;

	typedef unordered_map< wxString, wxArrayString*, wxStringHash, wxStringEqual > arraystring_hash_t;
	arraystring_hash_t m_affected;
	std::vector<eqn_data*> m_equations;
	typedef unordered_map< wxString, eqn_data*, wxStringHash, wxStringEqual > eqndata_hash_t;
	eqndata_hash_t m_eqnLookup;
	typedef unordered_map< wxString, size_t, wxStringHash, wxStringEqual > eqnindex_hash_t;
	eqnindex_hash_t m_eqnIndices;
	

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

class VarEvaluator
{
private:
	static const int INVALID = 0;
	static const int OK = 1;

	VarTable *m_vars;
	VarDatabase *m_vdb;
	std::vector<VarDatabase::eqn_data*> m_eqns;
	std::vector<char> m_status;
	wxArrayString m_errors;
	wxArrayString m_updated;
	int Calculate( );
	size_t MarkAffectedEquations( const wxString &var );

public:
	VarEvaluator( VarTable *vars, VarDatabase *db );

	void Reset();
	int CalculateAll();
	int Changed( const wxString &var );
	wxArrayString GetErrors() { return m_errors; }
	wxArrayString GetUpdated() { return m_updated; }
};


#endif
