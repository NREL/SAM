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

#ifndef __variable_h
#define __variable_h

#include <vector>
#include <unordered_map>

#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/stream.h>
#include <wx/buffer.h>
#include <wx/utils.h>

#include <wex/utils.h>

#include <lk/absyn.h>
#include <lk/eval.h>

#include <shared/lib_util.h>

#include <ssc/sscapi.h>
#include <ssc/../rapidjson/document.h>

#include "object.h"

// adds new entry to ssc_data from the value in vardata
bool assign_lkvar_to_sscdata(lk::vardata_t &val, const char *name, ssc_data_t p_dat);

// clears ssc_data and hashes all the variables entries into vardata's hash
bool lkhash_to_sscdata(lk::vardata_t &val, ssc_data_t table);

// clears ssc_data and hashes vardata under name
bool lkvar_to_sscdata(lk::vardata_t &val, const char *name, ssc_data_t p_dat);

// clears ssc_var and assigns the value from vardata
bool lkvar_to_sscvar(lk::vardata_t &val, ssc_var_t p_var);

// clears vardata and hashes all the variable entries in the ssc_data_t table
void sscdata_to_lkhash(ssc_data_t p_dat, lk::vardata_t &out);

// clears vardata and assigns the ssc_data to the ssc_data_t
void sscdata_to_lkvar(ssc_data_t p_dat, const char *name, lk::vardata_t &out);

void DoubleToJSONValue(rapidjson::Value& json_val);

// clears vardata and assigns the value from ssc_var_t
void sscvar_to_lkvar(ssc_var_t vd, lk::vardata_t &out);


class VarValue;
class VarDatabase;


#define VV_INVALID 0
#define VV_STRING 4
#define VV_NUMBER 1
#define VV_ARRAY 2
#define VV_MATRIX 3
#define VV_TABLE 5
#define VV_BINARY 6
#define VV_DATARR 7
#define VV_DATMAT 8

extern wxString vv_strtypes[9];

typedef unordered_map<wxString, VarValue*, wxStringHash, wxStringEqual> VarTableBase;

class VarTable : public VarTableBase
{
public:
	VarTable();
	VarTable( const VarTable &rhs );
	VarTable( ssc_data_t rhs);
	~VarTable();

	VarTable &operator=( const VarTable &rhs );
	void Copy( const VarTable &rhs );
    void Merge(const VarTable &rhs, bool overwrite_existing);

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

	void Write_text(wxOutputStream &, size_t maxdim = 0); // MaxDim specifies the maximum allowable array or matrix dimension when writing.
	bool Write_text(const wxString &file, size_t maxdim = 0);
	bool Read_text(wxInputStream &);
	bool Read_text(const wxString &file);

	void Write_JSON(rapidjson::Document&, const wxArrayString&, const wxArrayString&, const size_t maxdim = 0); // MaxDim specifies the maximum allowable array or matrix dimension when writing.
	bool Write_JSON(const std::string& file, const wxArrayString&, const wxArrayString&, const size_t maxdim = 0);
	bool Read_JSON(const rapidjson::Document&); 
	bool Read_JSON(const std::string& file);

    // returns a pointer to a ssc::var_table class that'll need to be freed using ssc_data_free
    bool AsSSCData(ssc_data_t p_dat);
};

class VarValue
{
public:
	VarValue();
	VarValue( const VarValue &vv );

	explicit VarValue( int i );
	explicit VarValue( bool b );
	explicit VarValue( double f );
	explicit VarValue( const std::vector<double> &f );
	explicit VarValue( const matrix_t<double> &m );
	explicit VarValue( const wxString &s );
	explicit VarValue( const VarTable &t );
	explicit VarValue( const wxMemoryBuffer &mb );
	explicit VarValue(ssc_var_t vd);

	VarValue( double *arr, size_t n );
	VarValue( double *mat, size_t r, size_t c );

	virtual ~VarValue();

	VarValue &operator=( const VarValue &rhs );
	bool ValueEqual( VarValue &rhs);
	void Copy( const VarValue &rhs );

	void Write(wxOutputStream &);
	bool Read(wxInputStream &);

	void Write_text(wxOutputStream &);
	bool Read_text(wxInputStream &);

	void Write_JSON(rapidjson::Document&, const wxString&, const wxArrayString&, const wxArrayString&);
	void Write_JSON_Constant(rapidjson::Document&, const wxString&, const wxArrayString&, const wxArrayString&, const double);

	bool Read_JSON(const rapidjson::Value&);

	// returns a pointer to a ssc::var_data class that'll need to be freed using ssc_var_free
    bool AsSSCVar(ssc_var_t p_var);

	int Type() const;
	wxString TypeAsString() const;
	void ChangeType(int type);
	void SetType( int t );
	void Set( int val );
//	void Set( float val );
	void Set( double val );
	void Set( const std::vector<int> &ivec );
	void Set( const std::vector<double> &fvec );
	void Set(double *val, size_t n);
	void Set(double *mat, size_t r, size_t c);
	void Set(const matrix_t<double> &mat);
	void Set( const wxString &str );
	void Set( const VarTable &tab );
	void Set( const wxMemoryBuffer &mb );

	int Integer();
	bool Boolean();
	double Value();
	double *Array( size_t *n );
	std::vector<double> Array();
	size_t Length();
	size_t Rows();
	size_t Columns();
	std::vector<int> IntegerArray();
	matrix_t<double> &Matrix();
	double *Matrix( size_t *nr, size_t *nc );
	wxString String();
	VarTable &Table();
	wxMemoryBuffer &Binary();
	std::vector<VarValue>& DataArray();
    std::vector<std::vector<VarValue>>& DataMatrix();


    bool Read( const lk::vardata_t &val, bool change_type = false );
	bool Write( lk::vardata_t &val );

	static bool Parse( int type, const wxString &str, VarValue &val );
	wxString AsString(wxChar arrsep = ';', wxChar tabsep = '|');

	static VarValue Invalid;
private:
	unsigned char m_type;
	matrix_t<double> m_val;
	wxString m_str;
	VarTable m_tab;
	wxMemoryBuffer m_bin;
	std::vector<VarValue> m_datarr;
    std::vector<std::vector<VarValue>> m_datmat;

};

#define VF_NONE                0x00
#define VF_HIDE_LABELS         0x01
#define VF_PARAMETRIC          0x02
#define VF_INDICATOR           0x04
#define VF_CALCULATED          0x08
#define VF_LIBRARY             0x10
#define VF_EXCLUSIVE_PAGES     0x20
#define VF_COLLAPSIBLE_PANE    0x40
#define VF_CHANGE_MODEL        0x80

#define VUIOBJ_NONE		"Default"

class VarInfo
{
public:
	VarInfo();
	VarInfo( const VarInfo &copy );

	void Write(wxOutputStream &);
	bool Read(wxInputStream &);

	void Write_text(wxOutputStream &);
	bool Read_text(wxInputStream &);

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
	wxString LookupByLabel(const wxString &label);
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

	void Write_text(wxOutputStream &);
	bool Read_text(wxInputStream &, const wxString &page = wxEmptyString);

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




struct ArraySize
{
	ArraySize(){ n_rows = n_cols = 0; }

	size_t n_rows;
	size_t n_cols;

	bool operator==(const ArraySize& x) const{
		return x.n_rows == n_rows && x.n_cols == n_cols;
	}
};
struct SortByRow
{
	bool operator()(ArraySize const  &a, ArraySize const &b)
	{
		return a.n_rows < b.n_rows;
	}
};
struct ArraySizeKey
{
	ArraySizeKey(){ n_rows = n_cols = key = 0; }
	ArraySizeKey(size_t rows, size_t cols){ n_rows = rows; n_cols = cols; }
	size_t n_rows;
	size_t n_cols;
	int key;

	bool operator==(const ArraySizeKey& x) const{
		return x.n_rows == n_rows && x.n_cols == n_cols && x.key == key;
	}
};
struct ArraySizeKeyCompare
{
	bool operator()(const ArraySizeKey& x, const ArraySizeKey& y)  const
    {

		if (x.n_rows < y.n_rows)
		{
			return true;
		}
		else if (x.n_rows == y.n_rows)
		{
			if (x.n_cols < y.n_cols)
			{
				return true;
			}
			else if (x.n_cols== y.n_cols)
			{
				return x.key < y.key;
			}
		}

		return false;

	}
};
#endif
