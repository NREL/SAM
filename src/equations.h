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

#ifndef __equations_h
#define __equations_h

#include <vector>

#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/stream.h>

#include <lk/absyn.h>
#include <lk/lex.h>
#include <lk/env.h>

#include "object.h"
#include "variables.h"


struct EqnData
{
	lk::node_t *tree; wxArrayString inputs, outputs; bool result_is_output;
};

class EqnDatabase
{
public:
	EqnDatabase();
	virtual ~EqnDatabase();

	void Clear();
	bool LoadFile( const wxString &file, wxArrayString *errors = 0 );
	bool LoadScript( const wxString &text, wxArrayString *errors = 0 );
	bool Parse( lk::input_base &in, wxArrayString *errors = 0 );

	wxArrayString *GetAffectedVariables( const wxString &var );
	const std::vector<EqnData*> &GetEquations() { return m_equations; }
	
private:
	std::vector<lk::node_t*> m_trees;

	typedef unordered_map< wxString, wxArrayString*, wxStringHash, wxStringEqual > arraystring_hash_t;
	arraystring_hash_t m_affected;

	std::vector<EqnData*> m_equations;

	void ScanParseTree( lk::node_t *root, wxArrayString *inputs, wxArrayString *outputs, bool in_assign_lhs = false );
	bool AddEquation( const wxArrayString &inputs, const wxArrayString &outputs, lk::node_t *tree, bool result_is_output );

};

class EqnFastLookup 
{	
	std::vector<EqnDatabase*> m_dbs;
public:
	
	typedef unordered_map< wxString, EqnData*, wxStringHash, wxStringEqual > eqndata_hash_t;
	typedef unordered_map< wxString, size_t, wxStringHash, wxStringEqual > eqnindex_hash_t;
	typedef unordered_map< wxString, bool, wxStringHash, wxStringEqual > eqnmark_hash_t;

	EqnFastLookup();
	EqnFastLookup( EqnDatabase *db );

	void AddDatabase( EqnDatabase *db );

	void Add( EqnData *ed );
	void Add( const std::vector<EqnData*> &list );
	void Clear();
	
	size_t GetAffectedVariables( const wxString &var, wxArrayString &list, eqnmark_hash_t &ignore );
	lk::node_t *GetEquation( const wxString &var, wxArrayString *inputs, wxArrayString *outputs );
	std::vector<EqnData*> GetEquations() { return m_eqnList; }
	EqnData *GetEquationData( const wxString &var );
	int GetEquationIndex( const wxString &var );

private:
	
	std::vector<EqnData*> m_eqnList;
	eqndata_hash_t m_eqnLookup;
	eqnindex_hash_t m_eqnIndices;	
};


class EqnEvaluator
{
protected:
	static const int INVALID = 0;
	static const int OK = 1;


	VarTable &m_vars;
	EqnFastLookup &m_efl;
	std::vector<EqnData*> m_eqns;
	std::vector<char> m_status;
	wxArrayString m_errors;
	wxArrayString m_updated;
	int Calculate( );
	size_t MarkAffectedEquations( const wxString &var, EqnFastLookup::eqnmark_hash_t &affected );

public:
	EqnEvaluator( VarTable &vars, EqnFastLookup &efl );

	void Reset();
	virtual int CalculateAll();
	virtual int Changed( const wxArrayString &vars );
	wxArrayString &GetErrors() { return m_errors; }
	wxArrayString &GetUpdated() { return m_updated; }

	// setup any context-specific function calls here
	virtual void SetupEnvironment( lk::env_t &env );
};




#endif

