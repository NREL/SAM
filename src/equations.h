#ifndef __equations_h
#define __equations_h

#include <vector>

#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/stream.h>

#include <lk_absyn.h>
#include <lk_lex.h>
#include <lk_env.h>

#include "object.h"
#include "variables.h"

class EqnDatabase
{
public:
	EqnDatabase();
	virtual ~EqnDatabase();

	void Clear();
	bool LoadFile( const wxString &file, wxArrayString *errors = 0 );
	bool LoadScript( const wxString &text, wxArrayString *errors = 0 );
	bool Parse( lk::input_base &in, wxArrayString *errors = 0 );

	lk::node_t *GetEquation( const wxString &var, wxArrayString *inputs, wxArrayString *outputs );
	wxArrayString *GetAffectedVariables( const wxString &var );

	struct eqn_data { lk::node_t *tree; wxArrayString inputs, outputs; bool result_is_output; };
	
	std::vector<eqn_data*> GetEquations() { return m_equations; }
	eqn_data *GetEquationData( const wxString &var );
	int GetEquationIndex( const wxString &var );

private:
	std::vector<lk::node_t*> m_trees;

	void ScanParseTree( lk::node_t *root, wxArrayString *inputs, wxArrayString *outputs, bool in_assign_lhs = false );
	
	bool AddEquation( const wxArrayString &inputs, const wxArrayString &outputs, lk::node_t *tree, bool result_is_output );

	typedef unordered_map< wxString, wxArrayString*, wxStringHash, wxStringEqual > arraystring_hash_t;
	arraystring_hash_t m_affected;
	std::vector<eqn_data*> m_equations;
	typedef unordered_map< wxString, eqn_data*, wxStringHash, wxStringEqual > eqndata_hash_t;
	eqndata_hash_t m_eqnLookup;
	typedef unordered_map< wxString, size_t, wxStringHash, wxStringEqual > eqnindex_hash_t;
	eqnindex_hash_t m_eqnIndices;
};



class EqnEvaluator
{
private:
	static const int INVALID = 0;
	static const int OK = 1;

	VarTable *m_vars;
	EqnDatabase *m_edb;
	std::vector<EqnDatabase::eqn_data*> m_eqns;
	std::vector<char> m_status;
	wxArrayString m_errors;
	wxArrayString m_updated;
	int Calculate( );
	size_t MarkAffectedEquations( const wxString &var );

public:
	EqnEvaluator( VarTable *vars, EqnDatabase *db );

	void Reset();
	int CalculateAll();
	int Changed( const wxString &var );
	wxArrayString GetErrors() { return m_errors; }
	wxArrayString GetUpdated() { return m_updated; }
};




#endif

