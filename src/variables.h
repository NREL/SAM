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
	void ChangeType(int type);
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
	size_t Rows(); 
	size_t Columns();
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
