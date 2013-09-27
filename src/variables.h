#ifndef __variable_h
#define __variable_h

#include <vector>

#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/stream.h>
#include "object.h"

class VarValue;

class VarTable : public unordered_map<wxString, VarValue*, wxStringHash, wxStringEqual>
{
public:
	VarTable();
	~VarTable();

	virtual void clear();
	wxArrayString ListAll( std::vector<VarValue*> *vals = 0 );
	void Set( const wxString &name, const VarValue &val );
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

private:
	unsigned char m_type;
	::matrix_t<float> m_val;
	wxString m_str;
	VarTable m_tab;
};

#define VF_NONE 0x00
#define VF_HIDE_LABELS 0x01
#define VF_CALCULATED 0x02
#define VF_PARAMETRIC 0x04
#define VF_INDICATOR 0x08

class VarInfo
{
public:
	VarInfo() { Type = VV_INVALID; Flags = VF_NONE; }

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

private:
	unordered_map<wxString, VarInfo*, wxStringHash, wxStringEqual> m_hash;	
	VarValue m_invVal;
};


#endif
